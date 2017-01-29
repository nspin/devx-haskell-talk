with import <nixpkgs> {}; stdenv.mkDerivation {
  name = "env";
  buildInputs = with haskellPackages; [
    pandoc
    (ghcWithPackages (hp: [
      async
      attoparsec
      wreq
    ]))
  ];
}
