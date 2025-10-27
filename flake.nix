{
  description = "streamly-text";

  inputs = {
    basepkgs.url = "git+ssh://git@github.com/composewell/streamly-packages?rev=85d7fcdbed9b06c1efec1e531086ea436a895b5d";
    nixpkgs.follows = "basepkgs/nixpkgs";
    nixpkgs-darwin.follows = "basepkgs/nixpkgs-darwin";
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    basepkgs.nixpack.mkOutputs {
      inherit nixpkgs nixpkgs-darwin basepkgs;
      name = "streamly-text-env";
      sources = import ./sources.nix;
      packages = import ./packages.nix;
    };
}
