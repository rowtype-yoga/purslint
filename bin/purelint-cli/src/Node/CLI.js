// Node.CLI FFI for JavaScript
export const argv = () => process.argv;
export const cwd = () => process.cwd();
export const exit = (code) => () => process.exit(code);
