pkgs:

let
    version = "22.12";
in
pkgs.stdenv.mkDerivation {
    inherit version;
    pname = "cardinal";

    src = pkgs.fetchFromGitHub {
        owner = "DISTRHO";
        repo = "cardinal";
        rev = version;
        sha256 = "sha256-ebKGsa6PIkUVUjaaXSkwl2CNGGEktSjg6x2rLT2AYag=";
        fetchSubmodules = true;
    };

    prePatch = ''
        patchShebangs dpf/utils/generate-ttl.sh
    '';

    makeFlags = [ "SYSDEPS=true" "PREFIX=$(out)" ];

    nativeBuildInputs = with pkgs; [ pkg-config cmake ];
    #                                           ^^^^^ Some random submodule depends on cmake.

    # Prevent nix from using cmake instead of GNU make which is Cardinal's build system.
    dontUseCmakeConfigure=true;

    buildInputs = with pkgs; [
        jansson
        libarchive
        libsamplerate
        speexdsp
        libGL
        mesa
        xorg.libX11
        xorg.libXcursor
        xorg.libXext
        xorg.xrandr
        python3
    ];
}
