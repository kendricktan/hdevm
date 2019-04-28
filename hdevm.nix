{ mkDerivation, Cabal, cabal-install, stdenv, base, bytestring
} : mkDerivation {
  pname = "hdevm";
  version = "0.0.1";
  src = ./.;
  buildTools = [
    cabal-install
  ];
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [
    base bytestring
  ];
  testHaskellDepends = [
    base
  ];
  homepage = "https://github.com/kendricktan/hdevm";
  description = "Tool to decompile EVM bytecode into AST";
  license = stdenv.lib.licenses.mit;
}
