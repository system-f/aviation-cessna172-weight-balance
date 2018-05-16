{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    papa = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "papa";
      rev = "97ef00aa45c70213a4f0ce348a2208e3f482a7e3";
      sha256 = "0qm0ay49wc0frxs6ipc10xyjj654b0wgk0b1hzm79qdlfp2yq0n5";
    };

    aviation-units = pkgs.fetchFromGitHub {
      owner = "data61";
      repo = "aviation-units";
      rev = "aee9b5c37946acfa28dc8ce96bf71643e01f28c7";
      sha256 = "0kyqn2mn5yvjngqzapqgidvpj5r664i5afkbj0di0hp2nj7f58ac";
    };

    aviation-weight-balance = pkgs.fetchFromGitHub {
      owner = "data61";
      repo = "aviation-weight-balance";
      rev = "00c65b36ff350b6577bf0f815bd902f820a4bbf0";
      sha256 = "0sxz247qwjmmkpwjc5221jsph3vbcpgjf93sdcqnnk7r8hlbxj9h";
    };

  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {
      aviation-units = import sources.aviation-units {};
      aviation-weight-balance = import sources.aviation-weight-balance {};
      parsers = pkgs.haskell.lib.dontCheck super.parsers;        
    };
  };

  aviation-cessna172-weight-balance = modifiedHaskellPackages.callPackage ./aviation-cessna172-weight-balance.nix {};

in

  aviation-cessna172-weight-balance

