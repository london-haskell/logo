{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      occt = pkgs.opencascade-occt;
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          ghc
          cabal-install
          haskell-language-server
          ghcid
          pkg-config
          zlib
          freeglut
          occt
          f3d
          (writeShellApplication {
            name = "watch";
            text = ''
              ${lib.getExe process-compose} --no-server --config ${
                pkgs.writeText "process-compose.yaml" (builtins.toJSON {
                  processes = {
                    ghcid.command = "ghcid -W -r";
                    f3d.command = "f3d --watch logo.glb --light-intensity 4 --background-color black";
                  };
                })
              }
            '';
          })
        ];
        LIBRARY_PATH = "${occt}/lib";
        CPLUS_INCLUDE_PATH = "${occt}/include/opencascade";
        LD_LIBRARY_PATH = "${occt}/lib";
      };
    };
}
