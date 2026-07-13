/* Mock the native SSE lib. The fake must be defined INSIDE the factory
   (jest hoists jest.mock above imports and forbids out-of-scope refs); tests
   reach the captured instances through the imported default's static array. */
jest.mock("react-native-sse", () => {
  class FakeEventSource {
    static instances: FakeEventSource[] = [];
    url: string;
    options: unknown;
    listeners: Record<string, ((ev: unknown) => void)[]> = {};
    closed = false;
    removedAll = false;
    constructor(url: string, options: unknown) {
      this.url = url;
      this.options = options;
      FakeEventSource.instances.push(this);
    }
    addEventListener(type: string, cb: (ev: unknown) => void) {
      (this.listeners[type] ||= []).push(cb);
    }
    removeAllEventListeners() {
      this.listeners = {};
      this.removedAll = true;
    }
    close() {
      this.closed = true;
    }
    emit(type: string, ev: unknown) {
      (this.listeners[type] ?? []).forEach((cb) => cb(ev));
    }
  }
  return { __esModule: true, default: FakeEventSource };
});

import EventSource from "react-native-sse";
import {
  VisGatewayClient,
  GatewayEvent,
  gatewayConnectionMessage,
  gatewayErrorMessage,
  isGatewayConnectionMessage,
} from "./VisClient";

interface FakeES {
  url: string;
  closed: boolean;
  removedAll: boolean;
  emit(type: string, ev: unknown): void;
}
const Fake = EventSource as unknown as { instances: FakeES[] };

const jsonResponse = (status: number, body: unknown, statusText = "") => ({
  ok: status >= 200 && status < 300,
  status,
  statusText,
  text: async () => (body === undefined ? "" : JSON.stringify(body)),
});

const mockFetch = jest.fn();
beforeEach(() => {
  mockFetch.mockReset();
  Fake.instances = [];
  (global as unknown as { fetch: unknown }).fetch = mockFetch;
});

const client = (token?: string) =>
  new VisGatewayClient(
    token === undefined
      ? { gatewayUrl: "http://gw:7890/" }
      : { gatewayUrl: "http://gw:7890/", token },
  );

describe("request error unwrapping ([object Object] regression)", () => {
  it("reaches the nested {error:{message}} string", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(
        404,
        { error: { type: "not-found", message: "no such route" } },
        "Not Found",
      ),
    );
    await expect(client().listSessions()).rejects.toThrow("no such route");
  });

  it("NEVER surfaces the literal [object Object]", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(500, { error: { type: "boom", message: "kaboom" } }),
    );
    await expect(client().listSessions()).rejects.toThrow(
      /^(?!.*\[object Object\]).*$/,
    );
  });

  it("tolerates a plain-string error", async () => {
    mockFetch.mockResolvedValue(jsonResponse(400, { error: "bad input" }));
    await expect(client().listSessions()).rejects.toThrow("bad input");
  });

  it("tolerates a bare {message}", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(400, { message: "top level msg" }),
    );
    await expect(client().listSessions()).rejects.toThrow("top level msg");
  });

  it("falls back to status + statusText when the body carries no error", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(503, undefined, "Service Unavailable"),
    );
    await expect(client().listSessions()).rejects.toThrow(
      "503 Service Unavailable",
    );
  });

  it("turns React Native's generic network failure into gateway guidance", async () => {
    mockFetch.mockRejectedValue(new TypeError("Network request failed"));
    await expect(client().listSessions()).rejects.toThrow(
      "Cannot reach the Vis gateway at gw:7890.",
    );
    await expect(client().listSessions()).rejects.toThrow(
      "Check that the gateway is running",
    );
  });

  it("explains the localhost-on-phone trap and marks gateway connection errors", () => {
    const msg = gatewayConnectionMessage("http://127.0.0.1:7890");
    expect(msg).toContain("points at the phone itself");
    expect(isGatewayConnectionMessage(msg)).toBe(true);
    expect(isGatewayConnectionMessage("503 Service Unavailable")).toBe(false);
  });

  it("normalizes generic SSE error objects to gateway guidance", () => {
    expect(gatewayErrorMessage("http://gw:7890", { type: "error" })).toContain(
      "Cannot reach the Vis gateway at gw:7890",
    );
  });
});

describe("request success paths", () => {
  it("returns the parsed sessions", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(200, { sessions: [{ id: "a" }, { id: "b" }] }),
    );
    await expect(client().listSessions()).resolves.toEqual([
      { id: "a" },
      { id: "b" },
    ]);
  });

  it("defaults to [] when sessions is missing", async () => {
    mockFetch.mockResolvedValue(jsonResponse(200, {}));
    await expect(client().listSessions()).resolves.toEqual([]);
  });

  it("attaches a bearer token when configured", async () => {
    mockFetch.mockResolvedValue(jsonResponse(200, { sessions: [] }));
    await client("secret").listSessions();
    const init = mockFetch.mock.calls[0]![1] as { headers: Headers };
    expect(init.headers.get("Authorization")).toBe("Bearer secret");
  });

  it("trims the trailing slash off the gateway url", async () => {
    mockFetch.mockResolvedValue(jsonResponse(200, { sessions: [] }));
    await client().listSessions();
    expect(mockFetch.mock.calls[0]![0]).toBe("http://gw:7890/v1/sessions");
  });
});

describe("project endpoints", () => {
  it("listProjects asks for the cross-channel view", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(200, { projects: [{ id: "g1", name: "vis-core" }] }),
    );
    await expect(client().listProjects()).resolves.toEqual([
      { id: "g1", name: "vis-core" },
    ]);
    expect(mockFetch.mock.calls[0]![0]).toBe(
      "http://gw:7890/v1/projects?channel=all",
    );
  });

  it("assignProject PATCHes the session with project_id", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(200, { id: "s1", project_id: "g1" }),
    );
    await client().assignProject("s1", "g1");
    const [url, init] = mockFetch.mock.calls[0] as [
      string,
      { method: string; body: string },
    ];
    expect(url).toBe("http://gw:7890/v1/sessions/s1");
    expect(init.method).toBe("PATCH");
    expect(JSON.parse(init.body)).toEqual({ project_id: "g1" });
  });

  it("assignProject with null clears the project (project_id:null)", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(200, { id: "s1", project_id: null }),
    );
    await client().assignProject("s1", null);
    const init = mockFetch.mock.calls[0]![1] as { body: string };
    expect(JSON.parse(init.body)).toEqual({ project_id: null });
  });
});

describe("turnTrace", () => {
  it("GETs the turn trace route and unwraps iterations", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(200, {
        iterations: [{ position: 0, forms: [{ tool_name: "rg" }] }],
      }),
    );
    await expect(client().turnTrace("s1", "t9")).resolves.toEqual([
      { position: 0, forms: [{ tool_name: "rg" }] },
    ]);
    expect(mockFetch.mock.calls[0]![0]).toBe(
      "http://gw:7890/v1/sessions/s1/turns/t9/trace",
    );
  });

  it("defaults to [] when a (pre-trace) gateway omits iterations", async () => {
    mockFetch.mockResolvedValue(jsonResponse(200, {}));
    await expect(client().turnTrace("s1", "t9")).resolves.toEqual([]);
  });

  it("propagates a 404 (older gateway without the trace route)", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(404, { error: { message: "no such route" } }, "Not Found"),
    );
    await expect(client().turnTrace("s1", "t9")).rejects.toThrow(
      "no such route",
    );
  });
});

describe("canonical gateway feature endpoints", () => {
  it("loads the session context snapshot", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(200, { session_utilization: { saturation: 42 } }),
    );
    await expect(client().sessionContext("s1")).resolves.toEqual({
      session_utilization: { saturation: 42 },
    });
    expect(mockFetch.mock.calls[0]![0]).toBe(
      "http://gw:7890/v1/sessions/s1/context",
    );
  });

  it("queries the shared @-file suggestion endpoint", async () => {
    mockFetch.mockResolvedValue(jsonResponse(200, [{ name: "src/app.clj" }]));
    await expect(client().suggest("s1", "src/app")).resolves.toEqual([
      { name: "src/app.clj" },
    ]);
    expect(mockFetch.mock.calls[0]![0]).toBe(
      "http://gw:7890/v1/sessions/s1/suggest?kind=file&q=src%2Fapp",
    );
  });

  it("manages workspace filesystem roots through the gateway", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(200, { workspace: { root: "/repo" } }),
    );
    await client().addRoot("s1", "/tmp/root");
    expect(mockFetch.mock.calls[0]![0]).toBe(
      "http://gw:7890/v1/sessions/s1/workspace/roots",
    );
    expect(
      JSON.parse((mockFetch.mock.calls[0]![1] as { body: string }).body),
    ).toEqual({
      path: "/tmp/root",
    });

    mockFetch.mockResolvedValue(
      jsonResponse(200, { workspace: { root: "/repo" } }),
    );
    await client().removeRoot("s1", "/tmp/root");
    expect(mockFetch.mock.calls[1]![0]).toBe(
      "http://gw:7890/v1/sessions/s1/workspace/roots?path=%2Ftmp%2Froot",
    );
    expect((mockFetch.mock.calls[1]![1] as { method: string }).method).toBe(
      "DELETE",
    );
  });

  it("loads provider status and limits for the model picker", async () => {
    mockFetch.mockResolvedValueOnce(
      jsonResponse(200, { status: { configured: true } }),
    );
    mockFetch.mockResolvedValueOnce(
      jsonResponse(200, { report: { status: "ok", message: "fresh" } }),
    );
    await expect(client().providerStatus("anthropic")).resolves.toEqual({
      configured: true,
    });
    await expect(client().providerLimits("anthropic")).resolves.toEqual({
      status: "ok",
      message: "fresh",
    });
    expect(mockFetch.mock.calls[0]![0]).toBe(
      "http://gw:7890/v1/providers/anthropic/status",
    );
    expect(mockFetch.mock.calls[1]![0]).toBe(
      "http://gw:7890/v1/providers/anthropic/limits",
    );
  });
});

describe("streamEvents SSE dispatch", () => {
  const open = () => {
    const events: GatewayEvent[] = [];
    let opened = false;
    const errors: unknown[] = [];
    const close = client().streamEvents("s1", {
      onEvent: (e) => events.push(e),
      onOpen: () => (opened = true),
      onError: (e) => errors.push(e),
    });
    const es = Fake.instances.at(-1)!;
    return { events, errors, close, es, opened: () => opened };
  };

  it("opens against the session events endpoint with cursor", () => {
    const { es } = open();
    expect(es.url).toBe("http://gw:7890/v1/sessions/s1/events?cursor=0");
  });

  it("parses a typed data frame into onEvent", () => {
    const { events, es } = open();
    es.emit("iteration.completed", {
      data: JSON.stringify({
        type: "iteration.completed",
        turn_id: "t1",
        thinking: "full thought",
        assistant_prose: "full prose",
      }),
    });
    expect(events).toEqual([
      {
        type: "iteration.completed",
        turn_id: "t1",
        thinking: "full thought",
        assistant_prose: "full prose",
      },
    ]);
  });

  it("also dispatches untyped 'message' frames", () => {
    const { events, es } = open();
    es.emit("message", {
      data: JSON.stringify({ type: "turn.started", turn_id: "t1" }),
    });
    expect(events).toEqual([{ type: "turn.started", turn_id: "t1" }]);
  });

  it("skips heartbeat / comment frames with null data", () => {
    const { events, es } = open();
    es.emit("message", { data: null });
    expect(events).toHaveLength(0);
  });

  it("swallows an unparseable frame without throwing", () => {
    const { events, es } = open();
    expect(() => es.emit("message", { data: "{not json" })).not.toThrow();
    expect(events).toHaveLength(0);
  });

  it("fires onOpen and onError through", () => {
    const { es, errors, opened } = open();
    es.emit("open", {});
    es.emit("error", { type: "error" });
    expect(opened()).toBe(true);
    expect(errors).toHaveLength(1);
  });

  it("close() tears the connection down", () => {
    const { close, es } = open();
    close();
    expect(es.closed).toBe(true);
    expect(es.removedAll).toBe(true);
  });
});
