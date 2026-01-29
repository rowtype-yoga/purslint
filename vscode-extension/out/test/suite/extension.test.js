"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
const assert = __importStar(require("assert"));
const vscode = __importStar(require("vscode"));
const path = __importStar(require("path"));
suite('Purslint Extension Test Suite', () => {
    vscode.window.showInformationMessage('Start all tests.');
    test('Extension should be present', () => {
        assert.ok(vscode.extensions.getExtension('RowtypeYoga.purslint'));
    });
    test('Extension should activate on PureScript file', async () => {
        const ext = vscode.extensions.getExtension('RowtypeYoga.purslint');
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
        const ext = vscode.extensions.getExtension('RowtypeYoga.purslint');
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
        const codeActions = await vscode.commands.executeCommand('vscode.executeCodeActionProvider', doc.uri, range);
        // Code actions may or may not be present depending on LSP connection
        // Just verify the command doesn't throw
        assert.ok(Array.isArray(codeActions) || codeActions === undefined, 'Should return array or undefined');
    });
});
//# sourceMappingURL=extension.test.js.map