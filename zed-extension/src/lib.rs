use zed_extension_api::{self as zed, Result};

struct PurslintExtension;

impl zed::Extension for PurslintExtension {
    fn new() -> Self {
        PurslintExtension
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &zed::LanguageServerId,
        _worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        Ok(zed::Command {
            command: "purslint-lsp".to_string(),
            args: vec![],
            env: Default::default(),
        })
    }
}

zed::register_extension!(PurslintExtension);
