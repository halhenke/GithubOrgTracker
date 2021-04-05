{
  description = "My First Flake";
  inputs = {
    # github example, also supported gitlab:
    nixpkgs.url = "github:Mic92/nixpkgs/master";
  };

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.hello;

  };
}
