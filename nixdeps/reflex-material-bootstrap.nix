{ mkDerivation, aeson, base, bytestring, containers, data-default
, dependent-map, either, extra, fetchgit, ghcjs-base, ghcjs-dom
, lens, reflex, reflex-dom, safe, stdenv, text, time
}:
mkDerivation {
  pname = "reflex-material-bootstrap";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/hexresearch/reflex-material-bootstrap.git";
    sha256 = "1dpm9h0plaghb00w2ys76bzv12wskz784rv5kqlcp74znxq0wqkw";
    rev = "0dbf56c743f9739b4e1ec7af8a47cdd3062f85f7";
  };
  libraryHaskellDepends = [
    aeson base bytestring containers data-default dependent-map either
    extra ghcjs-base ghcjs-dom lens reflex reflex-dom safe text time
  ];
  homepage = "https://github.com/hexresearch/reports-manager";
  description = "Frontend library for reactive bootstrap with material skin";
  license = stdenv.lib.licenses.mit;
}
