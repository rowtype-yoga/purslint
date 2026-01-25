// LSP stdio FFI for JavaScript
import * as fs from 'fs';

// PureScript Maybe representation that works for both standard and optimized backends
// Standard: { value0: x } for Just, Nothing.value for Nothing
// Optimized: { tag: 1, _1: x } for Just, { tag: 0 } for Nothing
const mkJust = (x) => ({ tag: 1, _1: x, value0: x });
const mkNothing = { tag: 0 };

// Buffer for reading LSP messages
let buffer = '';

// Try to parse a complete LSP message from buffer
function tryParseMessage() {
  // Look for Content-Length header
  const headerMatch = buffer.match(/Content-Length:\s*(\d+)\r?\n\r?\n/);
  if (!headerMatch) return null;
  
  const headerEnd = headerMatch.index + headerMatch[0].length;
  const contentLength = parseInt(headerMatch[1], 10);
  
  // Check if we have the full content
  if (buffer.length < headerEnd + contentLength) return null;
  
  // Extract the message content
  const content = buffer.slice(headerEnd, headerEnd + contentLength);
  buffer = buffer.slice(headerEnd + contentLength);
  
  return content;
}

// Read a message synchronously (blocking)
// Returns Maybe String
export const readMessage = () => {
  // First check if we already have a complete message in buffer
  let msg = tryParseMessage();
  if (msg !== null) {
    return mkJust(msg);
  }
  
  // Need to read more data synchronously from stdin
  const fd = fs.openSync('/dev/stdin', 'r');
  
  try {
    while (true) {
      const chunk = Buffer.alloc(4096);
      const bytesRead = fs.readSync(fd, chunk, 0, 4096);
      
      if (bytesRead === 0) {
        // EOF
        return mkNothing;
      }
      
      buffer += chunk.toString('utf8', 0, bytesRead);
      
      msg = tryParseMessage();
      if (msg !== null) {
        return mkJust(msg);
      }
    }
  } catch (e) {
    return mkNothing;
  } finally {
    fs.closeSync(fd);
  }
};

// Write a JSON-RPC message to stdout
export const writeMessage = (msg) => () => {
  const content = msg;
  const header = `Content-Length: ${Buffer.byteLength(content)}\r\n\r\n`;
  process.stdout.write(header + content);
};

// Log a message to stderr (for debugging)
export const logMessage = (msg) => () => {
  process.stderr.write('[purelint-lsp] ' + msg + '\n');
};

// Parse JSON string to Foreign (returns Maybe Foreign)
export const parseJSON = (str) => {
  try {
    return mkJust(JSON.parse(str));
  } catch (e) {
    return mkNothing;
  }
};

// Stringify Foreign to JSON
export const stringifyJSON = (obj) => {
  return JSON.stringify(obj);
};
