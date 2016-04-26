{ mkDerivation, base, monads-tf, stdenv }:
mkDerivation {
  pname = "OperadDP";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base monads-tf ];
  homepage = "none";
  description = "Operad";
  license = stdenv.lib.licenses.bsd3;
}
