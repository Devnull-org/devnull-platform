{ mkDerivation, aeson, base, bytestring, containers, hspec
, hspec-wai, hspec-wai-json, mtl, opaleye, postgresql-simple
, product-profunctors, profunctors, servant-server, stdenv, text
, wai, wai-cors, warp
}:
mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers mtl opaleye postgresql-simple
    product-profunctors profunctors servant-server text wai wai-cors
    warp
  ];
  executableHaskellDepends = [
    aeson base opaleye product-profunctors servant-server wai wai-cors
    warp
  ];
  testHaskellDepends = [
    aeson base hspec hspec-wai hspec-wai-json servant-server wai
    wai-cors warp
  ];
  homepage = "https://github.com/v0d1ch/backend#readme";
  license = stdenv.lib.licenses.bsd3;
}
