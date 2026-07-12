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
import { VisGatewayClient, GatewayEvent } from "./VisClient";

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
  text: async () => (body === undefined ? "" : JSON.stringify(body))
});

const mockFetch = jest.fn();
beforeEach(() => {
  mockFetch.mockReset();
  Fake.instances = [];
  (global as unknown as { fetch: unknown }).fetch = mockFetch;
});

const client = (token?: string) =>
  new VisGatewayClient(
    token === undefined ? { gatewayUrl: "http://gw:7890/" } : { gatewayUrl: "http://gw:7890/", token }
  );

describe("request error unwrapping ([object Object] regression)", () => {
  it("reaches the nested {error:{message}} string", async () => {
    mockFetch.mockResolvedValue(
      jsonResponse(404, { error: { type: "not-found", message: "no such route" } }, "Not Found")
    );
    await expect(client().listSessions()).rejects.toThrow("no such route");
  });

  it("NEVER surfaces the literal [object Object]", async () => {
    mockFetch.mockResolvedValue(jsonResponse(500, { error: { type: "boom", message: "kaboom" } }));
    await expect(client().listSessions()).rejects.toThrow(/^(?!.*\[object Object\]).*$/);
  });

  it("tolerates a plain-string error", async () => {
    mockFetch.mockResolvedValue(jsonResponse(400, { error: "bad input" }));
    await expect(client().listSessions()).rejects.toThrow("bad input");
  });

  it("tolerates a bare {message}", async () => {
    mockFetch.mockResolvedValue(jsonResponse(400, { message: "top level msg" }));
    await expect(client().listSessions()).rejects.toThrow("top level msg");
  });

  it("falls back to status + statusText when the body carries no error", async () => {
    mockFetch.mockResolvedValue(jsonResponse(503, undefined, "Service Unavailable"));
    await expect(client().listSessions()).rejects.toThrow("503 Service Unavailable");
  });
});

describe("request success paths", () => {
  it("returns the parsed sessions", async () => {
    mockFetch.mockResolvedValue(jsonResponse(200, { sessions: [{ id: "a" }, { id: "b" }] }));
    await expect(client().listSessions()).resolves.toEqual([{ id: "a" }, { id: "b" }]);
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

describe("group endpoints", () => {
  it("listGroups asks for the cross-channel view", async () => {
    mockFetch.mockResolvedValue(jsonResponse(200, { groups: [{ id: "g1", name: "vis-core" }] }));
    await expect(client().listGroups()).resolves.toEqual([{ id: "g1", name: "vis-core" }]);
    expect(mockFetch.mock.calls[0]![0]).toBe("http://gw:7890/v1/groups?channel=all");
  });

  it("assignGroup PATCHes the session with group_id", async () => {
    mockFetch.mockResolvedValue(jsonResponse(200, { id: "s1", group_id: "g1" }));
    await client().assignGroup("s1", "g1");
    const [url, init] = mockFetch.mock.calls[0] as [string, { method: string; body: string }];
    expect(url).toBe("http://gw:7890/v1/sessions/s1");
    expect(init.method).toBe("PATCH");
    expect(JSON.parse(init.body)).toEqual({ group_id: "g1" });
  });

  it("assignGroup with null ungroups (group_id:null)", async () => {
    mockFetch.mockResolvedValue(jsonResponse(200, { id: "s1", group_id: null }));
    await client().assignGroup("s1", null);
    const init = mockFetch.mock.calls[0]![1] as { body: string };
    expect(JSON.parse(init.body)).toEqual({ group_id: null });
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
      onError: (e) => errors.push(e)
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
    es.emit("content.delta", {
      data: JSON.stringify({ type: "content.delta", turn_id: "t1", text: "hi" })
    });
    expect(events).toEqual([{ type: "content.delta", turn_id: "t1", text: "hi" }]);
  });

  it("also dispatches untyped 'message' frames", () => {
    const { events, es } = open();
    es.emit("message", { data: JSON.stringify({ type: "turn.started", turn_id: "t1" }) });
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
