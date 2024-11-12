{ lib
, ocamlPackages
, stdenv
}:
stdenv.mkDerivation (finalAttrs: {
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

  checkInputs = with ocamlPackages; [
    ounit2
  ];

  buildPhase = ''
    runHook preBuild

    dune build bin/main.exe

    runHook postBuild
  '';

  passthru.devShell = finalAttrs.finalPackage.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [
      ocamlPackages.ocaml-lsp
      ocamlPackages.ocamlformat
      ocamlPackages.ounit2
    ];
  });
})
