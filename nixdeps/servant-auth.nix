{ mkDerivation, base, doctest, Glob, hspec, QuickCheck, stdenv
, yaml
}:
mkDerivation {
  pname = "servant-auth";
  version = "0.3.0.0";
  sha256 = "085xfhrim0y067yab3pwgiilk7zzdg8b7dz3i08f4cfd633an8km";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest Glob hspec QuickCheck yaml ];
  homepage = "http://github.com/plow-technologies/servant-auth#readme";
  description = "Authentication combinators for servant";
  license = stdenv.lib.licenses.bsd3;
}
