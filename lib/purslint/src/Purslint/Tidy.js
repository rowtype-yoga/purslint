import { execSync } from "child_process";

// Format a full PureScript module using purs-tidy CLI
// Returns the formatted module string
export const formatModule = (str) => () => {
  try {
    const result = execSync("purs-tidy format", {
      input: str,
      encoding: "utf8",
      stdio: ["pipe", "pipe", "pipe"],
      timeout: 5000,
    });
    return result;
  } catch (_e) {
    // If purs-tidy fails or isn't installed, return original
    return str;
  }
};

// Format a PureScript expression using purs-tidy CLI
// Wraps in a module, formats, and extracts the expression back
export const formatExpr = (str) => () => {
  try {
    // Wrap expression in a minimal module
    const moduleCode = `module T where\nx = ${str}\n`;
    
    // Shell out to purs-tidy
    const result = execSync("purs-tidy format", {
      input: moduleCode,
      encoding: "utf8",
      stdio: ["pipe", "pipe", "pipe"],
      timeout: 5000,
    });
    
    // Extract the expression after "x = "
    const lines = result.split("\n");
    const xLine = lines.findIndex((l) => l.startsWith("x = "));
    if (xLine === -1) return str;
    
    // Get everything after "x = " including continuation lines
    const exprLines = [];
    exprLines.push(lines[xLine].slice(4)); // Remove "x = "
    
    // Collect continuation lines until we hit another top-level declaration
    // or end of file. Keep relative indentation intact.
    for (let i = xLine + 1; i < lines.length; i++) {
      const line = lines[i];
      // Empty line or indented = still part of expression
      if (line === "" || line.startsWith(" ")) {
        exprLines.push(line);
      } else {
        break;
      }
    }
    
    // Trim trailing empty lines
    while (exprLines.length > 0 && exprLines[exprLines.length - 1] === "") {
      exprLines.pop();
    }
    
    return exprLines.join("\n");
  } catch (_e) {
    // If purs-tidy fails or isn't installed, return original
    return str;
  }
};
