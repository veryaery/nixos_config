pkgs:

# derivation -> derivation
themes:

pkgs.writeScriptBin "lstheme"
''
#!${pkgs.fish}/bin/fish

ls -1 ${themes}
''
