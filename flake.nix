{
  description = "HTTP server that manages verification requests to different tools from the Viper tool stack.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    sbt = {
      url = "github:zaninime/sbt-derivation";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, sbt }: let
    supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    forAllSystems = function: nixpkgs.lib.genAttrs supportedSystems
      (system: function (import nixpkgs { inherit system; }));
  in {
    packages = forAllSystems (pkgs: {
      default = sbt.lib.mkSbtDerivation {
        inherit pkgs;
        overrides = { sbt = pkgs.sbt.override { jre = pkgs.jdk11_headless; }; };
        pname = "viperserver";
        src = ./.;
        depsSha256 = "sha256-iJHPSJkxjFWVEmXFXX16OpqKOuSSE/qIxUpDfRbb/Fk=";
        version = "${self.tag or "${self.lastModifiedDate}.${self.shortRev or "dirty"}"}";
        buildInputs = with pkgs; [ z3 boogie ];
        depsWarmupCommand = "sbt update";
        buildPhase = "sbt assembly";
        installPhase = ''mkdir -p $out/server && mkdir -p $out/boogie/Binaries/ &&
          cp target/scala-2.13/viperserver.jar $out/server/viperserver.jar &&
          cp -r ${pkgs.z3} $out/z3 &&
          cp ${pkgs.boogie}/bin/boogie $out/boogie/Binaries/Boogie'';
        Z3_EXE = "${pkgs.z3}/bin/z3";
        BOOGIE_EXE = "${pkgs.boogie}/bin/boogie";
      };
    });
  };
}
