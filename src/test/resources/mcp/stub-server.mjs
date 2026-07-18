// A minimal, deterministic stub MCP server: newline-delimited JSON-RPC 2.0 over
// stdin/stdout. Used by McpClientLiveSuite to prove the real stdio wire (the rest
// of the suite uses scripted transports / a mock helper). One response line per
// request, echoing the id; `notifications/initialized` gets no reply.
let buf = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", (chunk) => {
  buf += chunk;
  let nl;
  while ((nl = buf.indexOf("\n")) >= 0) {
    const line = buf.slice(0, nl);
    buf = buf.slice(nl + 1);
    if (line.trim()) handle(line);
  }
});

function send(id, result) {
  process.stdout.write(JSON.stringify({ jsonrpc: "2.0", id, result }) + "\n");
}

function handle(line) {
  let req;
  try { req = JSON.parse(line); } catch (_) { return; }
  const id = req.id;
  const params = req.params || {};
  switch (req.method) {
    case "initialize":
      send(id, {
        protocolVersion: "2025-06-18",
        capabilities: { tools: {}, resources: {} },
        serverInfo: { name: "stub", version: "0.0.1" }
      });
      break;
    case "notifications/initialized":
      break; // a notification: no response
    case "tools/list":
      send(id, { tools: [
        { name: "echo", description: "echoes text", inputSchema: { type: "object", properties: { text: { type: "string" } }, required: ["text"] } },
        { name: "add", description: "adds a and b", inputSchema: { type: "object", properties: { a: { type: "number" }, b: { type: "number" } }, required: ["a", "b"] } }
      ] });
      break;
    case "tools/call": {
      const name = params.name;
      const args = params.arguments || {};
      if (name === "echo") send(id, { content: [{ type: "text", text: String(args.text) }], isError: false });
      else if (name === "add") send(id, { content: [{ type: "text", text: String(args.a + args.b) }], isError: false });
      else send(id, { content: [{ type: "text", text: "unknown tool: " + name }], isError: true });
      break;
    }
    case "resources/list":
      send(id, { resources: [{ uri: "stub://hello", name: "hello", mimeType: "text/plain" }] });
      break;
    case "resources/read":
      send(id, { contents: [{ uri: params.uri, mimeType: "text/plain", text: "hello world" }] });
      break;
    default:
      // Any other method with an id → method-not-found; id-less notifications ignored.
      if (id !== undefined && id !== null)
        process.stdout.write(JSON.stringify({ jsonrpc: "2.0", id, error: { code: -32601, message: "method not found" } }) + "\n");
  }
}
