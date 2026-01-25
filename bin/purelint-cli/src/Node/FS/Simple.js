// Node.FS.Simple FFI for JavaScript
import * as fs from 'fs';

export const readTextFile = (path) => () => fs.readFileSync(path, 'utf8');

export const writeTextFile = (path) => (content) => () => {
  fs.writeFileSync(path, content, 'utf8');
};

export const exists = (path) => () => fs.existsSync(path);

export const readdir = (path) => () => fs.readdirSync(path);

export const stat = (path) => () => {
  const s = fs.statSync(path);
  return {
    isFile: s.isFile(),
    isDirectory: s.isDirectory(),
    isSymbolicLink: s.isSymbolicLink(),
    size: s.size
  };
};
