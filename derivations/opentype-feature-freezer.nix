pkgs:

let
	pname = "opentype-feature-freezer";
	version = "1.32.2";
in
pkgs.python3Packages.buildPythonApplication {
	inherit pname version;

	buildInputs = with pkgs.python3Packages; [
		fonttools
	];

	src = pkgs.python3Packages.fetchPypi {
		inherit pname version;

		sha256 = "sha256-zckzIL/uTi8UVUdvK1YY2C9HwNhlMvG2lnNmatzCtXM=";
	};
}