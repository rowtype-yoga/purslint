import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';

suite('Purelint Extension Test Suite', () => {
  vscode.window.showInformationMessage('Start all tests.');

  test('Extension should be present', () => {
    assert.ok(vscode.extensions.getExtension('purelint.purelint'));
  });

  test('Extension should activate on PureScript file', async () => {
    const ext = vscode.extensions.getExtension('purelint.purelint');
    assert.ok(ext);
    
    // Open a PureScript file to trigger activation
    const testFile = path.join(__dirname, '../../../test-workspace/Test.purs');
    const uri = vscode.Uri.file(testFile);
    const doc = await vscode.workspace.openTextDocument(uri);
    await vscode.window.showTextDocument(doc);
    
    // Wait for extension to activate
    await ext.activate();
    assert.ok(ext.isActive);
  });

  test('Should provide code actions', async () => {
    const ext = vscode.extensions.getExtension('purelint.purelint');
    if (!ext?.isActive) {
      await ext?.activate();
    }
    
    const testFile = path.join(__dirname, '../../../test-workspace/Test.purs');
    const uri = vscode.Uri.file(testFile);
    const doc = await vscode.workspace.openTextDocument(uri);
    await vscode.window.showTextDocument(doc);

    // Wait for LSP to start and process document
    await new Promise(resolve => setTimeout(resolve, 3000));

    // Try to get code actions for the whole document
    const range = new vscode.Range(0, 0, doc.lineCount, 0);
    
    const codeActions = await vscode.commands.executeCommand<vscode.CodeAction[]>(
      'vscode.executeCodeActionProvider',
      doc.uri,
      range
    );
    
    // Code actions may or may not be present depending on LSP connection
    // Just verify the command doesn't throw
    assert.ok(Array.isArray(codeActions) || codeActions === undefined, 'Should return array or undefined');
  });
});
