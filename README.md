# PureLint

A PureScript linter inspired by [hlint](https://github.com/ndmitchell/hlint), providing suggestions for cleaner, more idiomatic PureScript code.

## Features

PureLint currently ships with **81 lint rules**, covering common PureScript refactors and simplifications.

For a complete list, see `lib/purslint/src/Purslint/Rules/`.

## Installation

### npm (recommended)

```bash
npm install -g purslint

# CLI
purelint --help
```

### Prerequisites

- [PureScript](https://www.purescript.org/) compiler (`purs`)
- [Spago](https://github.com/purescript/spago) package manager
- [Go](https://golang.org/) 1.19+
- [psgo](https://github.com/puregocore/psgo) - PureScript to Go compiler

### Building

```bash
# Install dependencies
spago install

# Build CLI
purs compile --codegen corefn 'lib/**/*.purs' 'bin/purslint-cli/**/*.purs' '.spago/**/*.purs'
psgo output/*/corefn.json
cp output/Main/Main.go main_cli.go
go build -o purelint main_cli.go

# Build LSP
purs compile --codegen corefn 'lib/**/*.purs' 'bin/purslint-lsp/**/*.purs' '.spago/**/*.purs'
psgo output/*/corefn.json
cp output/Main/Main.go main_lsp.go
go build -o purelint-lsp main_lsp.go

# Build and run tests
purs compile --codegen corefn 'lib/**/*.purs' 'test/**/*.purs' '.spago/**/*.purs'
psgo output/*/corefn.json
cp output/Test.Main/Test_Main.go test_main.go
go build -o test-runner test_main.go
./test-runner
```

## Usage

### CLI

```bash
# Lint a single file
./purelint src/Main.purs

# Lint multiple files
./purelint src/*.purs

# Lint with config file (optional)
./purelint --config purelint.json src/*.purs
```

### LSP (Language Server Protocol)

The LSP binary (`purelint-lsp`) provides IDE integration. It communicates via stdio using the standard LSP protocol.

#### Zed Editor

A Zed extension is available in the `zed-extension/` directory. To install:

1. Open Zed
2. Go to Extensions (Cmd+Shift+X)
3. Click "Install Dev Extension"
4. Select the `zed-extension/` directory

The extension will automatically use the `purelint-lsp` binary.

#### Neovim (nvim-lspconfig)

After building `purelint-lsp` (see build steps below), you can configure Neovim like this:

```lua
require("lspconfig").purelint = {
  default_config = {
    cmd = { "/path/to/purelint-lsp" },
    filetypes = { "purescript" },
    root_dir = require("lspconfig.util").root_pattern(".git", "spago.yaml", "package.json"),
  },
}

require("lspconfig").purelint.setup({})
```

If you prefer to run the bundled JS directly:

```lua
cmd = { "node", "/path/to/dist/purelint-lsp.js" }
```

## Configuration

Create a `purelint.json` file in your project root:

```json
{
  "disabledRules": ["LetToWhere", "EtaReduce"]
}
```

## Architecture

PureLint is written in PureScript and compiled to Go using `psgo` (purescript-native). This allows:

- Full access to the PureScript CST parser (`purescript-language-cst-parser`)
- Native binary performance
- Easy distribution without runtime dependencies

### Project Structure

```
purelint/
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

### Adding a New Rule

1. Create a new file in `lib/purslint/src/Purslint/Rules/`
2. Implement the rule using the CST traversal API
3. Export the rule from `Purslint.Rules`
4. Add tests in `test/Test/Main.purs`
5. Rebuild and test

### Key Dependencies

- `purescript-language-cst-parser` - PureScript CST parser
- `purescript-lsp` - LSP protocol implementation (Go FFI)

## License

MIT
