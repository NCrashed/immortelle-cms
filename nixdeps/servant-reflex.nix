{ mkDerivation, aeson, base, bytestring, case-insensitive
, containers, data-default, exceptions, fetchgit, ghcjs-dom
, http-api-data, http-media, jsaddle, mtl, network-uri, reflex
, reflex-dom, safe, scientific, servant, servant-auth, stdenv
, string-conversions, text, transformers
}:
mkDerivation {
  pname = "servant-reflex";
  version = "0.3.3";
  src = fetchgit {
    url = "https://github.com/imalsogreg/servant-reflex";
    sha256 = "06k40ri3r9jvjsn4ayzdsccsxwkzzd02nhcacbcnyxrzwmcdqdiz";
    rev = "4f77a91a35ddd89c0ac2ecefb1c1e115ad86c460";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers data-default exceptions
    ghcjs-dom http-api-data http-media jsaddle mtl network-uri reflex
    reflex-dom safe servant servant-auth string-conversions text
    transformers
  ];
  executableHaskellDepends = [
    aeson base reflex reflex-dom scientific servant text
  ];
  description = "Servant reflex API generator";
  license = stdenv.lib.licenses.bsd3;
}
