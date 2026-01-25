module Purelint.LSP where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Foreign (Foreign)
import LSP.Stdio as Stdio
import Purelint.Config (defaultConfig, filterRules)
import Purelint.Rule (Rule)
import Purelint.Rules.ConcatMap (concatMapRule)
import Purelint.Rules.EtaReduce (etaReduceRule)
import Purelint.Rules.EtaReduceDecl (etaReduceDeclRule)
import Purelint.Rules.FmapId (fmapIdRule)
import Purelint.Rules.LetToWhere (letToWhereRule)
import Purelint.Rules.MapFusion (mapFusionRule)
import Purelint.Rules.NotEqual (notEqualRule)
import Purelint.Rules.RedundantBind (redundantBindRule)
import Purelint.Rules.UseGuard (useGuardRule)
import Purelint.Rules.UseTraverse (useTraverseRule)
import Purelint.Runner (runRules)
import Purelint.Types (LintResult(..), LintWarning(..), Severity(..), SourceCode(..), Suggestion(..))
import Unsafe.Coerce (unsafeCoerce)

-- | All available rules
allRules :: Array Rule
allRules =
  [ useTraverseRule
  , mapFusionRule
  , fmapIdRule
  , notEqualRule
  , concatMapRule
  , useGuardRule
  , etaReduceRule
  , etaReduceDeclRule
  , redundantBindRule
  , letToWhereRule
  ]

-- | Get enabled rules
enabledRules :: Array Rule
enabledRules = filterRules allRules defaultConfig

-- | Convert lint warning to LSP diagnostic (with fix data embedded)
warningToDiagnostic :: LintWarning -> Foreign
warningToDiagnostic (LintWarning w) =
  let
    range =
      { start: { line: w.range.start.line, character: w.range.start.column }
      , end: { line: w.range.end.line, character: w.range.end.column }
      }
    base =
      { range: range
      , severity: severityToLSP w.severity
      , source: "purelint"
      , message: unwrap w.ruleId <> ": " <> unwrap w.message
      }
  in case w.suggestion of
    Nothing -> unsafeCoerce base
    Just (Suggestion sug) -> unsafeCoerce
      { range: range
      , severity: severityToLSP w.severity
      , source: "purelint"
      , message: unwrap w.ruleId <> ": " <> unwrap w.message
      , data:
          { hasFix: true
          , replacement: unwrap sug.replacement
          , description: unwrap sug.description
          }
      }

severityToLSP :: Severity -> Int
severityToLSP Error = 1
severityToLSP Warning = 2
severityToLSP Hint = 4

-- | Lint a document and return diagnostics
lintDocument :: String -> Array Foreign
lintDocument content =
  case runRules enabledRules (SourceCode content) of
    Left _ -> []
    Right (LintResult result) -> map warningToDiagnostic result.warnings

-- | Create a publishDiagnostics notification
mkPublishDiagnostics :: String -> Array Foreign -> String
mkPublishDiagnostics uri diagnostics =
  Stdio.stringifyJSON $ unsafeCoerce
    { jsonrpc: "2.0"
    , method: "textDocument/publishDiagnostics"
    , params:
        { uri: uri
        , diagnostics: diagnostics
        }
    }

-- | Create an initialize response
mkInitializeResponse :: Int -> String
mkInitializeResponse reqId =
  Stdio.stringifyJSON $ unsafeCoerce
    { jsonrpc: "2.0"
    , id: reqId
    , result:
        { capabilities:
            { textDocumentSync: 1  -- Full sync
            , codeActionProvider: true
            }
        , serverInfo:
            { name: "purelint-lsp"
            , version: "0.1.0"
            }
        }
    }

-- | Create a simple response (null result)
mkResponse :: Int -> String
mkResponse reqId =
  Stdio.stringifyJSON $ unsafeCoerce
    { jsonrpc: "2.0"
    , id: reqId
    , result: mkNull unit
    }

-- | Create code action response
mkCodeActionResponse :: Int -> Array Foreign -> String
mkCodeActionResponse reqId actions =
  Stdio.stringifyJSON $ unsafeCoerce
    { jsonrpc: "2.0"
    , id: reqId
    , result: actions
    }

foreign import mkNull :: forall a. a -> Foreign

-- | Helper to get a string field from Foreign
getField :: String -> Foreign -> Maybe String
getField key obj = 
  if hasKey key obj 
    then Just (unsafeCoerce (getKey key obj))
    else Nothing

getFieldInt :: String -> Foreign -> Maybe Int
getFieldInt key obj = 
  if hasKey key obj 
    then Just (unsafeCoerce (getKey key obj))
    else Nothing

getFieldObj :: String -> Foreign -> Maybe Foreign
getFieldObj key obj = 
  if hasKey key obj 
    then Just (getKey key obj)
    else Nothing

getFieldArr :: String -> Foreign -> Maybe (Array Foreign)
getFieldArr key obj = 
  if hasKey key obj 
    then Just (unsafeCoerce (getKey key obj))
    else Nothing

foreign import hasKey :: String -> Foreign -> Boolean
foreign import getKey :: String -> Foreign -> Foreign

-- | Handle a single message
handleMessage :: Foreign -> Effect Unit
handleMessage msg = do
  let method = fromMaybe "" (getField "method" msg)
  let reqId = fromMaybe 0 (getFieldInt "id" msg)
  
  Stdio.logMessage $ "Method: " <> method
  
  if method == "initialize" then do
    Stdio.logMessage "Handling initialize"
    Stdio.writeMessage $ mkInitializeResponse reqId
  else if method == "initialized" then do
    Stdio.logMessage "Client initialized"
  else if method == "textDocument/didOpen" then do
    handleDidOpen msg
  else if method == "textDocument/didChange" then do
    handleDidChange msg
  else if method == "textDocument/didClose" then do
    handleDidClose msg
  else if method == "textDocument/codeAction" then do
    handleCodeAction msg reqId
  else if method == "shutdown" then do
    Stdio.logMessage "Shutdown"
    Stdio.writeMessage $ mkResponse reqId
  else if method == "exit" then do
    Stdio.logMessage "Exit"
  else do
    Stdio.logMessage $ "Unknown method: " <> method

handleDidOpen :: Foreign -> Effect Unit
handleDidOpen msg = do
  Stdio.logMessage "Document opened"
  case getFieldObj "params" msg of
    Nothing -> pure unit
    Just params -> 
      case getFieldObj "textDocument" params of
        Nothing -> pure unit
        Just textDoc -> do
          let uri = fromMaybe "" (getField "uri" textDoc)
          let text = fromMaybe "" (getField "text" textDoc)
          let diagnostics = lintDocument text
          Stdio.writeMessage $ mkPublishDiagnostics uri diagnostics

handleDidChange :: Foreign -> Effect Unit
handleDidChange msg = do
  Stdio.logMessage "Document changed"
  case getFieldObj "params" msg of
    Nothing -> pure unit
    Just params -> do
      let uri = fromMaybe "" $ do
            textDoc <- getFieldObj "textDocument" params
            getField "uri" textDoc
      case getFieldArr "contentChanges" params of
        Nothing -> pure unit
        Just changes -> 
          case Array.head changes of
            Nothing -> pure unit
            Just change -> do
              let text = fromMaybe "" (getField "text" change)
              let diagnostics = lintDocument text
              Stdio.writeMessage $ mkPublishDiagnostics uri diagnostics

handleDidClose :: Foreign -> Effect Unit
handleDidClose msg = do
  Stdio.logMessage "Document closed"
  case getFieldObj "params" msg of
    Nothing -> pure unit
    Just params -> 
      case getFieldObj "textDocument" params of
        Nothing -> pure unit
        Just textDoc -> do
          let uri = fromMaybe "" (getField "uri" textDoc)
          Stdio.writeMessage $ mkPublishDiagnostics uri []

handleCodeAction :: Foreign -> Int -> Effect Unit
handleCodeAction msg reqId = do
  Stdio.logMessage "Code action requested"
  case getFieldObj "params" msg of
    Nothing -> Stdio.writeMessage $ mkCodeActionResponse reqId []
    Just params -> do
      -- Get the document URI
      let uri = fromMaybe "" $ do
            textDoc <- getFieldObj "textDocument" params
            getField "uri" textDoc
      
      -- Get diagnostics from the request context
      let diagnostics = fromMaybe [] $ do
            context <- getFieldObj "context" params
            getFieldArr "diagnostics" context
      
      -- Convert diagnostics to code actions
      let actions = Array.catMaybes $ map (diagnosticToCodeAction uri) diagnostics
      
      Stdio.logMessage $ "Returning " <> show (Array.length actions) <> " code actions"
      Stdio.writeMessage $ mkCodeActionResponse reqId actions

-- | Convert a diagnostic with fix data to a code action
diagnosticToCodeAction :: String -> Foreign -> Maybe Foreign
diagnosticToCodeAction uri diag = do
  dataField <- getFieldObj "data" diag
  if hasKey "replacement" dataField && hasKey "description" dataField
    then do
      replacement <- getField "replacement" dataField
      description <- getField "description" dataField
      range <- getFieldObj "range" diag
      pure $ mkCodeAction description uri range replacement diag
    else Nothing

-- | Create a code action with a workspace edit
mkCodeAction :: String -> String -> Foreign -> String -> Foreign -> Foreign
mkCodeAction title uri range newText diagnostic =
  unsafeCoerce
    { title: title
    , kind: "quickfix"
    , diagnostics: [diagnostic]
    , isPreferred: true
    , edit:
        { documentChanges:
            [ { textDocument: 
                  { uri: uri
                  , version: mkNull unit
                  }
              , edits: [{ range: range, newText: newText }]
              }
            ]
        }
    }

-- | Main loop
mainLoop :: Effect Unit
mainLoop = do
  msgStr <- Stdio.readMessage
  case msgStr of
    Nothing -> do
      Stdio.logMessage "No message, exiting"
    Just str -> do
      Stdio.logMessage $ "Got message: " <> str
      case Stdio.parseJSON str of
        Nothing -> do
          Stdio.logMessage "Failed to parse message"
          mainLoop
        Just msg -> do
          handleMessage msg
          -- Check if it was exit
          let method = fromMaybe "" (getField "method" msg)
          if method == "exit" 
            then pure unit
            else mainLoop

main :: Effect Unit
main = do
  Stdio.logMessage "purelint-lsp starting..."
  mainLoop
  Stdio.logMessage "purelint-lsp exiting"
