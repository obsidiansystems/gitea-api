{ # Pinned nixpkgs to use. We'll use the openapi-generator found within.
  pkgs ? (import ./reflex-platform {}).nixpkgs

  # The swagger file from which we're going to generate the gitea api bindings
, swaggerFile ? ./swagger.v1.json

  # The cabal version of the package will be the api version with this minor version appended
  # E.g., if minorVersion is "0" and the api version is "1.1.1", the cabal version will be "1.1.1.0"
  # Gitea seems to always use 3-part api version numbers. If it didn't we'd need to do something cleverer.
, minorVersion ? "0"
}:
let swaggerJson = builtins.fromJSON (builtins.readFile swaggerFile);
    # We generate the openapi-generate-cli config file so that it can include the generated cabal version number
    cfg = pkgs.writeText "config.json" ''
      { "cabalPackage":"gitea-api",
        "cabalVersion":"${swaggerJson.info.version + "." + minorVersion}",
        "baseModule":"Gitea"
      }
    '';

    # These stanzas wil be added to the cabal file
    srcRepoStanza = ''
      source-repository head
        type: git
        location: https://github.com/obsidiansystems/gitea-api
    '';
    cabalFileReplacements = {
      "Author Name Here" = "Obsidian Systems LLC";
      "author.name@email.com" = "maintainer@obsidian.systems";
      "YEAR - AUTHOR" = "2020 Obsidian Systems LLC";
      "UnspecifiedLicense" = "BSD3";
      "extra-source-files:" = "extra-source-files:\n    ChangeLog.md";
      "cabal-version:  >= 1.10" = ''
        cabal-version:  >= 1.10
        tested-with: GHC ==8.6.5
        license-file: LICENSE
      '';


      # Package version changes:
      "http-media >= 0.4 && < 0.8" = "http-media >= 0.4 && < 0.9";
      "http-client >=0.5 && <0.6" = "http-client >=0.5 && <0.7";

    };
in pkgs.stdenv.mkDerivation {
  name = "gitea-api";
  version = swaggerJson.info.version;
  src = ./.;
  buildPhase = "";
  # In this phase, we're going to run the generator and then do some substitutions in the resulting files
  installPhase = ''
    mkdir $out
    mkdir tmp
    ${pkgs.openapi-generator-cli}/bin/openapi-generator-cli generate -g haskell-http-client -o tmp -i $src/swagger.v1.json -c "${cfg}"
    substituteInPlace tmp/gitea-api.cabal \
    ${builtins.concatStringsSep " \\\n"
        (builtins.attrValues
          (builtins.mapAttrs (a: b: "  --replace \"${a}\" \"${b}\"") cabalFileReplacements))}
    cat >> tmp/gitea-api.cabal <<EOF

    ${srcRepoStanza}
    EOF
    cp -r tmp/* $out
  '';
  meta = {
    homepage = "https://try.gitea.io/api/swagger";
    description = "Generate haskell client bindings for the Gitea API";
    license = pkgs.lib.licenses.bsd3;
    platforms = pkgs.lib.platforms.all;
  };
}
