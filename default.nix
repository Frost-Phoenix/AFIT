{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
	buildInputs = with pkgs; [
		ocaml
		dune_3
		ocamlPackages.findlib
		ocamlPackages.alcotest
		ocamlPackages.junit
		ocamlPackages.junit_alcotest
		ocamlPackages.zarith

		## Lsp
		ocamlPackages.merlin
		ocamlPackages.ocaml-lsp
	];
}
