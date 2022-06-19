pkgs:

{
    features ? []
}:

let
	inherit (builtins)
		concatStringsSep;

	features' = concatStringsSep "," features;
in
pkgs.fetchurl {
	nativeBuildInputs = with pkgs; [
		unzip
		opentype-feature-freezer
	];

	url = "https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip";
	downloadToTemp = true;
	recursiveHash = true;
	sha256 = "sha256-+reVUJRXsu6QkiDJyxmT/lfZEgio+FIU3NEsxp+541I=";
	
	postFetch = ''
		fontDir=$out/share/fonts/truetype

		unzip -j $downloadedFile variable_ttf/FiraCode-VF.ttf

		mkdir -p $fontDir
		pyftfeatfreeze -f "${features'}" FiraCode-VF.ttf $fontDir/FiraCode-Features-VF.ttf
	'';
}