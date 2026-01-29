import * as path from 'path';
import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
  const config = vscode.workspace.getConfiguration('purslint');
  
  if (!config.get<boolean>('enable', true)) {
    return;
  }

  // Get server path from config or use default
  let serverPath = config.get<string>('serverPath', '');
  if (!serverPath) {
    // Default to the purslint project location
    serverPath = '/Users/mark/Developer/purslint/dist/purslint-lsp.js';
  }

  const serverOptions: ServerOptions = {
    run: {
      module: serverPath,
      transport: TransportKind.stdio
    },
    debug: {
      module: serverPath,
      transport: TransportKind.stdio
    }
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'purescript' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/*.purs')
    }
  };

  client = new LanguageClient(
    'purslint',
    'Purslint',
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
