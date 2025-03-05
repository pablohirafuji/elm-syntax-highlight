{
  description = "Development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";

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
              nil
              nixpkgs-fmt
              nodejs_20
              nodePackages.uglify-js

              (writeScriptBin "run-demo" ''
                cd demo
                ${lib.getExe elmPackages.elm} reactor
              '')

              (writeScriptBin "build-demo" ''
                cd demo
                ${lib.getExe elmPackages.elm} make src/Main.elm --optimize --output build/index.js
                cp -f index.html build/index.html
                ${lib.getExe nodePackages.uglify-js} build/index.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | ${lib.getExe nodePackages.uglify-js} --mangle --output build/index.js
              '')

              (writeScriptBin "build-themes" ''
                cd demo/themes-page
                elm make Main.elm --output elm-themes.js
                node make-themes.js
                rm elm-themes.js
                cd ..
                mv -f themes-page/themes.html build
              '')

            ]);
        };
      });
    };
}
