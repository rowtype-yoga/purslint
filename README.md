# Purslint

<p align="center">
  <img src="vscode-extension/purslint.jpg" alt="Purslint logo" width="180" />
</p>
<p align="center">
  <video src="demo.mp4" width="480" controls playsinline></video>
</p>

A PureScript linter inspired by [hlint](https://github.com/ndmitchell/hlint), providing suggestions for cleaner, more idiomatic PureScript code.

## Install

### VS Code (Marketplace)

Install from the Visual Studio Marketplace:
[Purslint](https://marketplace.visualstudio.com/items?itemName=RowtypeYoga.purslint).

### CLI (npm)

```bash
npm install -g purslint

# CLI
purslint --help
```

## Usage

### CLI

```bash
# Lint a single file
./purslint src/Main.purs

# Lint multiple files
./purslint src/*.purs

# Lint with config file (optional)
./purslint --config purslint.json src/*.purs
```

### LSP (Language Server Protocol)

The LSP binary (`purslint-lsp`) provides IDE integration. It communicates via stdio using the standard LSP protocol.

#### Zed Editor

A Zed extension is available in the `zed-extension/` directory. To install:

1. Open Zed
2. Go to Extensions (Cmd+Shift+X)
3. Click "Install Dev Extension"
4. Select the `zed-extension/` directory

The extension will automatically use the `purslint-lsp` binary.

#### Neovim (nvim-lspconfig)

After building `purslint-lsp` (see build steps below), you can configure Neovim like this:

```lua
require("lspconfig").purslint = {
  default_config = {
    cmd = { "/path/to/purslint-lsp" },
    filetypes = { "purescript" },
    root_dir = require("lspconfig.util").root_pattern(".git", "spago.yaml", "package.json"),
  },
}

require("lspconfig").purslint.setup({})
```

If you prefer to run the bundled JS directly:

```lua
cmd = { "node", "/path/to/dist/purslint-lsp.js" }
```

## Features

Purslint currently ships with **81 lint rules**, covering common PureScript refactors and simplifications.

For a complete list, see `lib/purslint/src/Purslint/Rules/`.

## Configuration

Create a `purslint.json` file in your project root:

```json
{
  "disabledRules": ["LetToWhere", "EtaReduce"]
}
```

## Architecture

Purslint is written in PureScript and compiled to JavaScript via
`purs-backend-es`, then bundled for Node.js. This allows:

- Full access to the PureScript CST parser (`purescript-language-cst-parser`)
- Straightforward distribution via npm
- A single runtime across CLI and LSP

### Project Structure

```
purslint/
├── lib/purslint/src/       # Core library
│   └── Purslint/
│       ├── Rules/          # Lint rules
│       ├── Runner.purs     # Rule runner
│       ├── Types.purs      # Core types
│       └── Imports.purs    # Import checking
├── bin/
│   ├── purslint-cli/       # CLI application
│   └── purslint-lsp/       # LSP server
├── test/                   # Test suite
└── zed-extension/          # Zed editor extension
```

## Development

### Prerequisites

- [PureScript](https://www.purescript.org/) compiler (`purs`)
- [Spago](https://github.com/purescript/spago) package manager
- [Node.js](https://nodejs.org/) 18+

### Building

```bash
# Install dependencies
npm install

# Build and bundle CLI + LSP
npm run dist

# Run tests
npm test
```

### Publishing to npm

Publishing is handled by GitHub Actions. Add an `NPM_TOKEN` secret with an
automation or granular token that has publish rights for `purslint`, then
create a GitHub Release to trigger the workflow. You can also run it manually
via the workflow dispatch.

### Adding a New Rule

1. Create a new file in `lib/purslint/src/Purslint/Rules/`
2. Implement the rule using the CST traversal API
3. Export the rule from `Purslint.Rules`
4. Add tests in `test/Test/Main.purs`
5. Rebuild and test

### Key Dependencies

- `purescript-language-cst-parser` - PureScript CST parser
- `purescript-lsp` - LSP protocol implementation (Go FFI)
- `purs-backend-es` - PureScript to JavaScript backend
- `esbuild` - Bundler for CLI and LSP

## License

MIT
