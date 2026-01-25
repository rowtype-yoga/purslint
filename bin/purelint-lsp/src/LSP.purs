module Purelint.LSP where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (Foreign)
import LSP.Stdio as Stdio
import Purelint.Config (Config, defaultConfig, filterRules)
import Purelint.Rule (Rule)
import Purelint.Rules.BooleanSimplify (booleanSimplifyRule)
import Purelint.Rules.CollapseLambdas (collapseLambdasRule)
import Purelint.Rules.ConcatMap (concatMapRule)
import Purelint.Rules.EtaReduce (etaReduceRule)
import Purelint.Rules.EtaReduceDecl (etaReduceDeclRule)
import Purelint.Rules.FmapId (fmapIdRule)
import Purelint.Rules.FunctorLaw (functorLawRule)
import Purelint.Rules.LetToWhere (letToWhereRule)
import Purelint.Rules.MapFusion (mapFusionRule)
import Purelint.Rules.MonoidIdentity (monoidIdentityRule)
import Purelint.Rules.NotEqual (notEqualRule)
import Purelint.Rules.RedundantBind (redundantBindRule)
import Purelint.Rules.RedundantFlip (redundantFlipRule)
import Purelint.Rules.RedundantIf (redundantIfRule)
import Purelint.Rules.RedundantNot (redundantNotRule)
import Purelint.Rules.UseAll (useAllRule)
import Purelint.Rules.UseAny (useAnyRule)
import Purelint.Rules.UseConst (useConstRule)
import Purelint.Rules.UseFold (useFoldRule)
import Purelint.Rules.UseFromMaybe (useFromMaybeRule)
import Purelint.Rules.UseFstSnd (useFstSndRule)
import Purelint.Rules.UseGuard (useGuardRule)
import Purelint.Rules.UseIsJust (useIsJustRule)
import Purelint.Rules.UseIsNothing (useIsNothingRule)
import Purelint.Rules.UseJoin (useJoinRule)
import Purelint.Rules.UseMinMax (useMinMaxRule)
import Purelint.Rules.UseNotElem (useNotElemRule)
import Purelint.Rules.UseNull (useNullRule)
import Purelint.Rules.UseSequence (useSequenceRule)
import Purelint.Rules.UseTraverse (useTraverseRule)
import Purelint.Rules.UseTraverseSequence (useTraverseSequenceRule)
import Purelint.Rules.UseUnless (useUnlessRule)
import Purelint.Rules.UseVoid (useVoidRule)
import Purelint.Rules.UseWhen (useWhenRule)
import Purelint.Rules.UseMapMaybe (useMapMaybeRule)
import Purelint.Rules.UseGuardMaybe (useGuardMaybeRule)
import Purelint.Rules.UseMaybeMap (useMaybeMapRule)
import Purelint.Rules.UseApplicative (useApplicativeRule)
import Purelint.Rules.UseBindFlip (useBindFlipRule)
import Purelint.Rules.UseFor (useForRule)
import Purelint.Rules.RedundantGuard (redundantGuardRule)
import Purelint.Rules.UseComparing (useComparingRule)
import Purelint.Rules.UseOn (useOnRule)
import Purelint.Rules.UseFindMap (useFindMapRule)
import Purelint.Rules.UseLastReverse (useLastReverseRule)
import Purelint.Rules.RedundantReverse (redundantReverseRule)
import Purelint.Rules.UseBreak (useBreakRule)
import Purelint.Rules.UseSpan (useSpanRule)
import Purelint.Rules.UseMinimumSort (useMinimumSortRule)
import Purelint.Rules.UseBimap (useBimapRule)
import Purelint.Rules.UseEitherMap (useEitherMapRule)
import Purelint.Rules.EvaluateFst (evaluateFstRule)
import Purelint.Rules.EvaluateConst (evaluateConstRule)
import Purelint.Rules.UseOr (useOrRule)
import Purelint.Rules.UseAnd (useAndRule)
import Purelint.Rules.UseFoldMapId (useFoldMapIdRule)
import Purelint.Rules.WhenNot (whenNotRule)
import Purelint.Rules.UnlessNot (unlessNotRule)
import Purelint.Rules.UseZip (useZipRule)
import Purelint.Rules.UseReplicate (useReplicateRule)
import Purelint.Rules.UseUncurry (useUncurryRule)
import Purelint.Rules.RedundantNegate (redundantNegateRule)
import Purelint.Rules.UseFromJust (useFromJustRule)
import Purelint.Rules.UseHead (useHeadRule)
import Purelint.Rules.RedundantId (redundantIdRule)
import Purelint.Rules.NothingBind (nothingBindRule)
import Purelint.Rules.UseElemIndex (useElemIndexRule)
import Purelint.Runner (runRules)
import Purelint.Types (LintResult(..), LintWarning(..), RuleId(..), Severity(..), SourceCode(..), Suggestion(..))
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
  , redundantIfRule
  , functorLawRule
  , useVoidRule
  , useJoinRule
  , useAnyRule
  , useAllRule
  , booleanSimplifyRule
  , collapseLambdasRule
  , useConstRule
  , useNullRule
  , useFromMaybeRule
  , useIsJustRule
  , useIsNothingRule
  , useWhenRule
  , useUnlessRule
  , redundantFlipRule
  , useNotElemRule
  , useMinMaxRule
  , monoidIdentityRule
  , useFoldRule
  , useSequenceRule
  , useTraverseSequenceRule
  , redundantNotRule
  , useFstSndRule
  , useMapMaybeRule
  , useGuardMaybeRule
  , useMaybeMapRule
  , useApplicativeRule
  , useBindFlipRule
  , useForRule
  , redundantGuardRule
  , useComparingRule
  , useOnRule
  , useFindMapRule
  -- New rules
  , useLastReverseRule
  , redundantReverseRule
  , useBreakRule
  , useSpanRule
  , useMinimumSortRule
  , useBimapRule
  , useEitherMapRule
  , evaluateFstRule
  , evaluateConstRule
  , useOrRule
  , useAndRule
  , useFoldMapIdRule
  , whenNotRule
  , unlessNotRule
  , useZipRule
  , useReplicateRule
  , useUncurryRule
  , redundantNegateRule
  , useFromJustRule
  , useHeadRule
  , redundantIdRule
  , nothingBindRule
  , useElemIndexRule
  ]

-- | Get enabled rules based on config
getEnabledRules :: Config -> Array Rule
getEnabledRules config = filterRules allRules config

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
          , ruleId: unwrap w.ruleId
          }
      }

severityToLSP :: Severity -> Int
severityToLSP Error = 1
severityToLSP Warning = 2
severityToLSP Hint = 4

-- | Lint a document and return diagnostics
lintDocument :: Config -> String -> Array Foreign
lintDocument config content =
  let rules = getEnabledRules config
  in case runRules rules (SourceCode content) of
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

-- | Create an initialize response with workspace configuration support
mkInitializeResponse :: Int -> String
mkInitializeResponse reqId =
  Stdio.stringifyJSON $ unsafeCoerce
    { jsonrpc: "2.0"
    , id: reqId
    , result:
        { capabilities:
            { textDocumentSync: 1  -- Full sync
            , codeActionProvider: true
            , workspace:
                { workspaceFolders:
                    { supported: true
                    , changeNotifications: true
                    }
                }
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

getFieldBool :: String -> Foreign -> Maybe Boolean
getFieldBool key obj = 
  if hasKey key obj 
    then Just (unsafeCoerce (getKey key obj))
    else Nothing

foreign import hasKey :: String -> Foreign -> Boolean
foreign import getKey :: String -> Foreign -> Foreign

-- | Parse settings to get disabled rules
-- | Expected format: { "purelint": { "disabledRules": ["RuleId1", "RuleId2"] } }
-- | Or: { "purelint.disabledRules": ["RuleId1", "RuleId2"] }
-- | Or: { "purelint.enabled": false } to disable all rules
parseSettings :: Foreign -> Config
parseSettings settings =
  -- Try to get purelint.enabled first
  case getFieldObj "purelint" settings of
    Just purelintObj ->
      -- Check if all rules are disabled
      case getFieldBool "enabled" purelintObj of
        Just false -> 
          { disabledRules: map (\r -> r.ruleId) allRules }  -- Disable all rules
        _ ->
          -- Get disabled rules list
          case getFieldArr "disabledRules" purelintObj of
            Just arr -> 
              { disabledRules: Array.mapMaybe parseRuleId arr }
            Nothing -> defaultConfig
    Nothing ->
      -- Try flat format: purelint.disabledRules
      case getFieldArr "purelint.disabledRules" settings of
        Just arr -> 
          { disabledRules: Array.mapMaybe parseRuleId arr }
        Nothing ->
          -- Try purelint.enabled
          case getFieldBool "purelint.enabled" settings of
            Just false -> 
              { disabledRules: map (\r -> r.ruleId) allRules }
            _ -> defaultConfig

  where
  parseRuleId :: Foreign -> Maybe RuleId
  parseRuleId f = 
    -- Try to coerce to string
    let s = unsafeCoerce f :: String
    in if s == "" then Nothing else Just (RuleId s)

-- | Handle a single message
handleMessage :: Ref Config -> Foreign -> Effect Unit
handleMessage configRef msg = do
  let method = fromMaybe "" (getField "method" msg)
  let reqId = fromMaybe 0 (getFieldInt "id" msg)
  
  Stdio.logMessage $ "Method: " <> method
  
  if method == "initialize" then do
    Stdio.logMessage "Handling initialize"
    -- Parse initial settings from initializationOptions if present
    case getFieldObj "params" msg of
      Just params -> 
        case getFieldObj "initializationOptions" params of
          Just options -> do
            let config = parseSettings options
            Ref.write config configRef
            Stdio.logMessage $ "Initial config loaded, disabled: " <> show (Array.length config.disabledRules)
          Nothing -> pure unit
      Nothing -> pure unit
    Stdio.writeMessage $ mkInitializeResponse reqId
  else if method == "initialized" then do
    Stdio.logMessage "Client initialized"
  else if method == "workspace/didChangeConfiguration" then do
    handleDidChangeConfiguration configRef msg
  else if method == "textDocument/didOpen" then do
    handleDidOpen configRef msg
  else if method == "textDocument/didChange" then do
    handleDidChange configRef msg
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

handleDidChangeConfiguration :: Ref Config -> Foreign -> Effect Unit
handleDidChangeConfiguration configRef msg = do
  Stdio.logMessage "Configuration changed"
  case getFieldObj "params" msg of
    Nothing -> pure unit
    Just params ->
      case getFieldObj "settings" params of
        Nothing -> pure unit
        Just settings -> do
          let config = parseSettings settings
          Ref.write config configRef
          Stdio.logMessage $ "Config updated, disabled rules: " <> show (Array.length config.disabledRules)

handleDidOpen :: Ref Config -> Foreign -> Effect Unit
handleDidOpen configRef msg = do
  Stdio.logMessage "Document opened"
  config <- Ref.read configRef
  case getFieldObj "params" msg of
    Nothing -> pure unit
    Just params -> 
      case getFieldObj "textDocument" params of
        Nothing -> pure unit
        Just textDoc -> do
          let uri = fromMaybe "" (getField "uri" textDoc)
          let text = fromMaybe "" (getField "text" textDoc)
          let diagnostics = lintDocument config text
          Stdio.writeMessage $ mkPublishDiagnostics uri diagnostics

handleDidChange :: Ref Config -> Foreign -> Effect Unit
handleDidChange configRef msg = do
  Stdio.logMessage "Document changed"
  config <- Ref.read configRef
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
              let diagnostics = lintDocument config text
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
      
      -- Convert diagnostics to code actions (each diagnostic may produce multiple actions)
      let actions = Array.concat $ map (diagnosticToCodeActions uri) diagnostics
      
      Stdio.logMessage $ "Returning " <> show (Array.length actions) <> " code actions"
      Stdio.writeMessage $ mkCodeActionResponse reqId actions

-- | Convert a diagnostic to code actions (quickfix + disable rule)
diagnosticToCodeActions :: String -> Foreign -> Array Foreign
diagnosticToCodeActions uri diag = 
  case getFieldObj "data" diag of
    Nothing -> []
    Just dataField ->
      let
        -- Try to create quickfix action
        quickfixAction = 
          if hasKey "replacement" dataField && hasKey "description" dataField
            then do
              replacement <- getField "replacement" dataField
              description <- getField "description" dataField
              range <- getFieldObj "range" diag
              pure $ mkCodeAction description uri range replacement diag
            else Nothing
        
        -- Try to create disable rule action  
        disableAction = do
          ruleId <- getField "ruleId" dataField
          pure $ mkDisableRuleAction ruleId diag
      in
        Array.catMaybes [quickfixAction, disableAction]

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

-- | Create a "Disable rule" code action (uses a command the client can execute)
mkDisableRuleAction :: String -> Foreign -> Foreign
mkDisableRuleAction ruleId diagnostic =
  unsafeCoerce
    { title: "Disable rule: " <> ruleId
    , kind: "quickfix"
    , diagnostics: [diagnostic]
    , isPreferred: false
    , command:
        { title: "Disable rule " <> ruleId
        , command: "purelint.disableRule"
        , arguments: [ruleId]
        }
    }

-- | Main loop
mainLoop :: Ref Config -> Effect Unit
mainLoop configRef = do
  msgStr <- Stdio.readMessage
  case msgStr of
    Nothing -> do
      Stdio.logMessage "No message, exiting"
    Just str -> do
      Stdio.logMessage $ "Got message: " <> str
      case Stdio.parseJSON str of
        Nothing -> do
          Stdio.logMessage "Failed to parse message"
          mainLoop configRef
        Just msg -> do
          handleMessage configRef msg
          -- Check if it was exit
          let method = fromMaybe "" (getField "method" msg)
          if method == "exit" 
            then pure unit
            else mainLoop configRef

main :: Effect Unit
main = do
  Stdio.logMessage "purelint-lsp starting..."
  configRef <- Ref.new defaultConfig
  mainLoop configRef
  Stdio.logMessage "purelint-lsp exiting"
