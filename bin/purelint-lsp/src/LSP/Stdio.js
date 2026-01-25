// LSP stdio FFI for JavaScript
import * as fs from 'fs';
import * as $Maybe from '../Data.Maybe/index.js';

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
    return $Maybe.Just.create(msg);
  }
  
  // Need to read more data synchronously from stdin
  const fd = fs.openSync('/dev/stdin', 'r');
  
  try {
    while (true) {
      const chunk = Buffer.alloc(4096);
      const bytesRead = fs.readSync(fd, chunk, 0, 4096);
      
      if (bytesRead === 0) {
        // EOF
        return $Maybe.Nothing.value;
      }
      
      buffer += chunk.toString('utf8', 0, bytesRead);
      
      msg = tryParseMessage();
      if (msg !== null) {
        return $Maybe.Just.create(msg);
      }
    }
  } catch (e) {
    return $Maybe.Nothing.value;
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
    return $Maybe.Just.create(JSON.parse(str));
  } catch (e) {
    return $Maybe.Nothing.value;
  }
};

// Stringify Foreign to JSON
export const stringifyJSON = (obj) => {
  return JSON.stringify(obj);
};
