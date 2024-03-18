{
  description = "yaaaaaaaaaaaaaaaaaaaaa";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system: let
      overlay-unstable = final: prev: {
        unstable = import inputs.nixpkgs-unstable {
          inherit system;
        };
      };
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [overlay-unstable];
      };
    in {
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          zig_0_10
          zls
        ];
        shellHook = ''
        '';
      };
    });
}
