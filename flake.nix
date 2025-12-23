{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
    ps-overlay.url = "github:thomashoneyman/purescript-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, mkSpagoDerivation, ps-overlay }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ mkSpagoDerivation.overlays.default
                       ps-overlay.overlays.default ];
        };
      in
        {
          packages.default =
            pkgs.mkSpagoDerivation {
              spagoYaml = ./spago.yaml;
              spagoLock = ./spago.lock;
              src = ./.;
              nativeBuildInputs = [ pkgs.purs-unstable pkgs.spago-unstable pkgs.esbuild ];
              version = "0.1.0";
              buildPhase = "spago bundle";
              installPhase = "mkdir $out; cp index.js $out";
              buildNodeModulesArgs = {
                npmRoot = ./.;
                nodejs = pkgs.nodejs;
              };
            };
        }
    );
}