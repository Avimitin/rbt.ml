{ lib
, ocamlPackages
, stdenv
}:
stdenv.mkDerivation {
  name = "RedBlackTreeBench";

  src = with lib.fileset; toSource {
    root = ../../.;
    fileset = unions [
      ../../bin
      ../../lib
      ../../lib
      ../../test
      ../../dune-project
      ../../rbt.opam
    ];
  };

  buildInputs = with ocamlPackages; [
    benchmark
  ];

  nativeBuildInputs = with ocamlPackages; [
    findlib
    ocaml
    dune_3
  ];

  buildPhase = ''
    runHook preBuild

    dune build bin/main.exe

    runHook postBuild
  '';
}
