{ stdenv, mkDerivation, sourceByRegex, base, haskell-src-exts, containers,
  text, s-cargot, parsec, transformers, hspec, temporary }:
mkDerivation {
  pname = "haskell-src-edit";
  version = "0.1.0.0";
  #  "src/Language" "src/Langauge/Haskell" "src/Language/Haskell/Edit"
  src = sourceByRegex ./. [
    "haskell-src-edit\.cabal$" "LICENSE" ".*\.hs$"
    "src"
    "src/Language"
    "src/Language/Haskell"
    "src/Language/Haskell/Edit"
    "app"
    "test" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base haskell-src-exts containers text s-cargot parsec
  ];
  executableHaskellDepends = [
    base text transformers
  ];
  testHaskellDepends = [
    base hspec temporary text
  ];
  homepage = "https://github.com/acowley/haskell-src-edit#readme";
  license = stdenv.lib.licenses.bsd3;
}
