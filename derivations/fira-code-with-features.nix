pkgs:

{
    features ? []
}:

let
	inherit (builtins)
		concatStringsSep;

	version = "6.2";
	features' = concatStringsSep "," features;
in
pkgs.fetchurl {
	name = "fira-code-${version}";

	nativeBuildInputs = with pkgs; [
		unzip
		opentype-feature-freezer
	];

	url = "https://github.com/tonsky/FiraCode/releases/download/${version}/Fira_Code_v${version}.zip";
	downloadToTemp = true;
	recursiveHash = true;
	sha256 = "sha256-+reVUJRXsu6QkiDJyxmT/lfZEgio+FIU3NEsxp+541I=";
	
	postFetch = ''
		outFontDirPath=$out/share/fonts/truetype

		unzip -j $downloadedFile variable_ttf/FiraCode-VF.ttf

		mkdir -p $outFontDirPath
		pyftfeatfreeze -f "${features'}" FiraCode-VF.ttf $outFontDirPath/FiraCode-Features-VF.ttf
	'';
}