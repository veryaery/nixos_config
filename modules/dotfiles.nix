{ flakeRoot, ... } @ args:

{
    theme.enable = true;
    theme.src = flakeRoot + /dotfiles;
    theme.files = _: {
	".config/sway/config" = {};
        ".config/fish/config.fish" = {};
    };
    theme.postInstallScripts = {};
}
