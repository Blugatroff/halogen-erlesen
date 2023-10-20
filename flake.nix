{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, flake-utils, purescript-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ purescript-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "purs-shell";
            buildInputs = [
              pkgs.purs
              pkgs.spago-unstable
              pkgs.purescript-language-server
              pkgs.purs-backend-es
              pkgs.purs-tidy-bin.purs-tidy-0_10_0
              pkgs.nodejs_20
              pkgs.esbuild
              pkgs.just
              pkgs.nodePackages.purescript-psa
            ];
          shellHook = ''
            source <(spago --bash-completion-script `which spago`)
            source <(node --completion-bash)
            '';
          };
       };
     }
  );
}

