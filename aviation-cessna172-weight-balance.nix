{ mkDerivation, aviation-units, aviation-weight-balance, base
, directory, doctest, filepath, lens, parsec, QuickCheck
, quickcheck-text, stdenv, template-haskell
}:
mkDerivation {
  pname = "aviation-cessna172-weight-balance";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aviation-units aviation-weight-balance base lens
  ];
  testHaskellDepends = [
    base directory doctest filepath parsec QuickCheck quickcheck-text
    template-haskell
  ];
  homepage = "https://github.com/data61/aviation-cessna172-weight-balance";
  description = "Weight and Balance for the Cessna 172 aircraft in aviation";
  license = "unknown";
}
