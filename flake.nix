{
  description = "my blog";

  outputs = inputs @ {
    self,
    flake-parts,
    nixpkgs,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.devshell.flakeModule
      ];

      systems = [
        "x86_64-darwin"
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
      ];
      perSystem = {
        config,
        pkgs,
        lib,
        final,
        ...
      }: {
        formatter = pkgs.alejandra;

        devshells.default = {
          commands = [
            {package = pkgs.hugo;}
            {
              name = "dev";
              help = "Run the hugo server";
              command = ''
                (sleep 1 ; open http://localhost:1313/) &
                exec hugo server --noHTTPCache --buildDrafts --buildFuture \"$@\"
              '';
            }
            {
              name = "new-post";
              help = "Create a new post with '[plain-title]'";
              command = ''
                set -eu -x
                date="$(date -Idate)"
                title_plain="$1"; shift
                title="$(echo "$title_plain" | tr ' A-Z' '-a-z')"

                hugo new content -k post "$@" content/post/"$date"-"$title".md
                "$EDITOR" content/post/"$date"-"$title".md
              '';
            }
          ];
          packages = with pkgs; [go git];
        };

        checks.versions = inputs.nix-flake-tests.lib.check {
          inherit pkgs;
          tests = {
            testMatch = let
              netlifyVersion = (builtins.fromTOML (builtins.readFile ./netlify.toml)).build.environment.HUGO_VERSION;
              pkgsVersion = pkgs.hugo.version;
            in {
              expected = netlifyVersion;
              expr = pkgsVersion;
            };
          };
        };
      };
    };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    devshell.url = "github:numtide/devshell";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nix-flake-tests.url = "github:antifuchs/nix-flake-tests";
  };
}
