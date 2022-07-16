pkgs:

let
    version = "0.1.2";
in
pkgs.fetchFromGitHub {
    owner = "tanvirtin";
    repo = "vgit.nvim";
    rev = "v${version}";
    sha256 = "sha256-U5EzibmOv9Z01RUcvh4gB3zVQAC4tI8yVngdSzE7vmw=";
}
