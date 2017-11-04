{ mkDerivation, base, doctest, Glob, hspec, QuickCheck, stdenv
, yaml
}:
mkDerivation {
  pname = "servant-auth";
  version = "0.2.7.0";
  sha256 = "0j0hfal64qjdbif9a6vy7dv4zn0pmkspn0pwafhsyh5adgqkrhxa";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest Glob hspec QuickCheck yaml ];
  homepage = "http://github.com/plow-technologies/servant-auth#readme";
  description = "Authentication combinators for servant";
  license = stdenv.lib.licenses.bsd3;
}
