{ mkDerivation, acid-state, aeson-injector, base, bytestring
, containers, ghc-prim, monad-control, mtl, safe, safecopy
, servant-auth-token, servant-auth-token-api, servant-server
, stdenv, template-haskell, text, time, transformers
, transformers-base, uuid
}:
mkDerivation {
  pname = "servant-auth-token-acid";
  version = "0.5.0.0";
  sha256 = "1hvslg23l43k6wz6z84xcm3sv0lxgnvcsrx7z8493zyav9lnlx6h";
  libraryHaskellDepends = [
    acid-state aeson-injector base bytestring containers ghc-prim
    monad-control mtl safe safecopy servant-auth-token
    servant-auth-token-api servant-server template-haskell text time
    transformers transformers-base uuid
  ];
  doHaddock = false;
  homepage = "https://github.com/ncrashed/servant-auth-token#readme";
  description = "Acid-state backend for servant-auth-token server";
  license = stdenv.lib.licenses.bsd3;
}
