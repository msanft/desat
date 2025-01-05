{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            ocaml
            dune_3
            ocamlPackages.ocaml-lsp
            ocamlPackages.ocamlformat
            ocamlPackages.findlib
            ocamlPackages.alcotest
            ocamlPackages.menhir
          ];
        };
      }
    );
}
