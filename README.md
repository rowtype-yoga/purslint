# PureLint

A PureScript linter inspired by [hlint](https://github.com/ndmitchell/hlint), providing suggestions for cleaner, more idiomatic PureScript code.

## Features

PureLint includes 10 lint rules:

| Rule | Description | Example |
|------|-------------|---------|
| `FmapId` | `map identity x` → `x` | Redundant identity mapping |
| `ConcatMap` | `join <<< map f` → `f =<< _` | Use bind instead of join+map |
| `MapFusion` | `map f <<< map g` → `map (f <<< g)` | Fuse consecutive maps |
| `UseTraverse` | `sequence <<< map f` → `traverse f` | Use traverse |
| `NotEqual` | `not (x == y)` → `x /= y` | Use not-equal operator |
| `UseGuard` | `if c then Just x else Nothing` → `guard` | Use guard from Control.MonadZero |
| `EtaReduce` | `\x -> f x` → `f` | Eta reduce lambdas |
| `EtaReduceDecl` | `f x = g x` → `f = g` | Eta reduce declarations |
| `RedundantBind` | `x >>= pure` → `x` | Remove redundant bind |
| `LetToWhere` | `let x = y in z` → `z where x = y` | Prefer where clauses |

## Installation

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
purs compile --codegen corefn 'lib/**/*.purs' 'bin/purelint-cli/**/*.purs' '.spago/**/*.purs'
psgo output/*/corefn.json
cp output/Main/Main.go main_cli.go
go build -o purelint main_cli.go

# Build LSP
purs compile --codegen corefn 'lib/**/*.purs' 'bin/purelint-lsp/**/*.purs' '.spago/**/*.purs'
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
├── lib/purelint/src/       # Core library
│   └── Purelint/
│       ├── Rules/          # Lint rules
│       ├── Runner.purs     # Rule runner
│       ├── Types.purs      # Core types
│       └── Imports.purs    # Import checking
├── bin/
│   ├── purelint-cli/       # CLI application
│   └── purelint-lsp/       # LSP server
├── test/                   # Test suite
└── zed-extension/          # Zed editor extension
```

## Development

### Adding a New Rule

1. Create a new file in `lib/purelint/src/Purelint/Rules/`
2. Implement the rule using the CST traversal API
3. Export the rule from `Purelint.Rules`
4. Add tests in `test/Test/Main.purs`
5. Rebuild and test

### Key Dependencies

- `purescript-language-cst-parser` - PureScript CST parser
- `purescript-lsp` - LSP protocol implementation (Go FFI)

## License

MIT
