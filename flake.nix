{
  description = "my blog";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-flake-tests.url = "github:antifuchs/nix-flake-tests";
  };

  outputs = { self, nixpkgs, flake-utils, nix-flake-tests }: flake-utils.lib.eachSystem [
    "x86_64-darwin"
    "x86_64-linux"
  ]
    (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        nativeBuildInputs = with pkgs; [ hugo font-awesome ];
      in
      rec {
        devShell = pkgs.mkShell {
          inherit nativeBuildInputs;
        };

        apps = {
          hugo = flake-utils.lib.mkApp {
            drv = pkgs.hugo;
          };

          repl =
            flake-utils.lib.mkApp {
              drv = pkgs.writeShellScriptBin "repl" ''
                confnix=$(mktemp)
                echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
                trap "rm $confnix" EXIT
                nix repl $confnix
              '';
            };
        };

        checks.versions = nix-flake-tests.lib.check {
          inherit pkgs;
          tests = {
            testMatch =
              let
                netlifyVersion = (builtins.fromTOML (builtins.readFile ./netlify.toml)).build.environment.HUGO_VERSION;
                pkgsVersion = pkgs.hugo.version;
              in
              {
                expected = netlifyVersion;
                expr = pkgsVersion;
              };
          };
        };
      });
}
