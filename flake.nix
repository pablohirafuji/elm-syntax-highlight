{
  description = "Development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages =
            (with pkgs.elmPackages; [
              elm
              elm-test
              elm-format
            ]) ++ (with pkgs; [
              rnix-lsp
              nodejs_20
              nodePackages.uglify-js
            ]);
        };
      });
    };
}
