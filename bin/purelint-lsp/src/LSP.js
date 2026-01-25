// Purelint.LSP FFI for JavaScript

// Create a null value
export const mkNull = (_) => null;

// Check if a key exists in an object
export const hasKey = (key) => (obj) => {
  return obj != null && typeof obj === 'object' && key in obj;
};

// Get a key from an object
export const getKey = (key) => (obj) => {
  return obj[key];
};
