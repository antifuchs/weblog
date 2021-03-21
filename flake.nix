{
  description = "my blog";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-20.09";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
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

      # from https://gitlab.com/panakeia/flake-unit-tests/-/blob/master/flake.nix, emulate a test runner:
      packages.tests =
        let
          results = pkgs.lib.concatLists (map (path: pkgs.callPackage path { }) [ ./tests ]);
          resultToString = { name, expected, result }: ''
            ${name} failed: expected ${builtins.toString expected}, but got ${
              builtins.toString result
            }
          '';
        in
        if results != [ ] then
          builtins.throw (pkgs.lib.concatStringsSep "\n" (map resultToString results))
        else
          ""; # all tests passed

      checks =
        let
          runTestCommand = name: command:
            pkgs.runCommand name { } ''
              set -o errexit
              cd ${self}
              ${command}
              touch $out
            '';
        in
        {
          tests = runTestCommand "tests" "nix eval --raw .#tests";
        };
    });
}
