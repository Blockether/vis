import { fileURLToPath } from "node:url";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";

type McpTool = {
  name: string;
  description?: string;
  inputSchema?: Record<string, unknown>;
};

type McpTextContent = { type: "text"; text: string };
type McpImageContent = { type: "image"; data: string; mimeType: string };
type McpContent = McpTextContent | McpImageContent | Record<string, unknown>;
type McpCallResult = { content?: McpContent[]; isError?: boolean; [key: string]: unknown };

const serverPath = fileURLToPath(new URL("./node_modules/chiasmus/dist/mcp-server.js", import.meta.url));

function processEnv(): Record<string, string> {
  return Object.fromEntries(
    Object.entries(process.env).filter((entry): entry is [string, string] => typeof entry[1] === "string"),
  );
}

function renderUnknown(value: unknown): string {
  if (typeof value === "string") return value;
  try {
    return JSON.stringify(value, null, 2);
  } catch {
    return String(value);
  }
}

function toPiContent(result: McpCallResult) {
  const content = result.content ?? [];
  if (content.length === 0) {
    return [{ type: "text" as const, text: renderUnknown(result) }];
  }

  return content.map((part) => {
    if (part && part.type === "text" && typeof part.text === "string") {
      return { type: "text" as const, text: part.text };
    }
    if (
      part &&
      part.type === "image" &&
      typeof part.data === "string" &&
      typeof part.mimeType === "string"
    ) {
      return { type: "image" as const, data: part.data, mimeType: part.mimeType };
    }
    return { type: "text" as const, text: renderUnknown(part) };
  });
}

export default async function chiasmusExtension(pi: ExtensionAPI) {
  const transport = new StdioClientTransport({
    command: process.execPath,
    args: [serverPath],
    env: {
      ...processEnv(),
      CHIASMUS_HOME: process.env.CHIASMUS_HOME ?? `${process.cwd()}/.pi/chiasmus`,
    },
    stderr: "pipe",
  });
  transport.stderr?.resume();

  const client = new Client({ name: "pi-chiasmus", version: "0.1.0" });
  await client.connect(transport);

  const listed = await client.listTools();
  const tools = (listed.tools ?? []) as McpTool[];

  for (const tool of tools) {
    const schema = tool.inputSchema ?? { type: "object", properties: {} };

    pi.registerTool({
      name: tool.name,
      label: tool.name,
      description: tool.description ?? `Chiasmus MCP tool: ${tool.name}`,
      promptSnippet: tool.description ?? `Run Chiasmus tool ${tool.name}`,
      parameters: schema as any,
      async execute(_toolCallId, params) {
        try {
          const result = (await client.callTool({
            name: tool.name,
            arguments: params as Record<string, unknown>,
          })) as McpCallResult;

          return {
            content: toPiContent(result),
            details: result,
            isError: Boolean(result.isError),
          };
        } catch (error) {
          return {
            content: [{ type: "text", text: error instanceof Error ? error.stack ?? error.message : String(error) }],
            details: { error: error instanceof Error ? error.message : String(error) },
            isError: true,
          };
        }
      },
    });
  }

  pi.registerCommand("chiasmus-status", {
    description: "Show Chiasmus MCP bridge status",
    handler: async (_args, ctx) => {
      ctx.ui.notify(`Chiasmus loaded: ${tools.length} tools from ${serverPath}`, "info");
    },
  });

  pi.on("session_start", async (_event, ctx) => {
    ctx.ui.setStatus("chiasmus", `chiasmus: ${tools.length} tools`);
  });

  pi.on("session_shutdown", async () => {
    await client.close();
  });
}
