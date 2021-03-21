{ hugo, lib }:
lib.runTests {

  testNetlifyVersion =
    let
      netlifyVersion = (builtins.fromTOML (builtins.readFile ../netlify.toml)).build.environment.HUGO_VERSION;
      pkgsVersion = hugo.version;
    in
    {
      expected = netlifyVersion;
      expr = pkgsVersion;
    };
}
