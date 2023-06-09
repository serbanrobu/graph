{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            bacon # Background rust code check
            cargo-audit
            cargo-expand
            cargo-tarpaulin # Code coverage reporting tool
            cargo-watch
            clang
            evcxr # Rust REPL
            llvmPackages.bintools
            nil # Nix language server
            nixpkgs-fmt # Nix formatter
            (rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override {
              extensions = [ "rust-analyzer" "rust-src" ];
            }))
            taplo # TOML language server
          ];
        };
      }
    );
}
