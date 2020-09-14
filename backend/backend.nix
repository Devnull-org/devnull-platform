{ mkDerivation, aeson, base, hpack, hspec, hspec-wai
, hspec-wai-json, servant-server, stdenv, wai, wai-cors, warp
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base servant-server wai wai-cors warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base servant-server wai wai-cors warp
  ];
  testHaskellDepends = [
    aeson base hspec hspec-wai hspec-wai-json servant-server wai
    wai-cors warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/v0d1ch/backend#readme";
  license = stdenv.lib.licenses.bsd3;
}
