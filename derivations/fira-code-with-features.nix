pkgs:

{
	# features? :: [ string ]
    features ? []
}:

let
	inherit (builtins)
		concatStringsSep;

	version = "6.2";
	features' = concatStringsSep "," features;
in
pkgs.runCommandLocal "fira-code-features"
{
    src = pkgs.fetchurl {
        url = "https://github.com/tonsky/FiraCode/releases/download/${version}/Fira_Code_v${version}.zip";
        recursiveHash = true;
        sha256 = "sha256-BDdAvLUYncYJciMU5gDEmid7ru1xul0LLI8C3JwgN74=";
    };
	nativeBuildInputs = with pkgs; [
        unzip
		opentype-feature-freezer
        nerd-font-patcher
	];
}
''
unzip $src ttf/*

featdir=feat

mkdir $featdir

files=$(find ttf -type f)
for origfile in $files; do
    basename=$(basename $origfile)
    suffix=$(echo $basename | sed "s/^FiraCode//")
    featfile=$featdir/FiraCode-Features$suffix

    pyftfeatfreeze -f "${features'}" $origfile $featfile 
done

outdir=$out/share/fonts/truetype

mkdir -p $outdir

files=$(find $featdir -type f)
for featfile in $files; do
    nerd-font-patcher -out $outdir $featfile \
        --fontawesome --fontawesomeextension \
        --octicons \
        --powersymbols \
        --pomicons \
        --powerline \
        --powerlineextra \
        --material \
        --weather
done
''
