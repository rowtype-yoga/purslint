use zed_extension_api::{self as zed, Result};

struct PurelintExtension;

impl zed::Extension for PurelintExtension {
    fn new() -> Self {
        PurelintExtension
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &zed::LanguageServerId,
        _worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        Ok(zed::Command {
            command: "/Users/mark/Developer/purelint/purelint-lsp".to_string(),
            args: vec![],
            env: Default::default(),
        })
    }
}

zed::register_extension!(PurelintExtension);
