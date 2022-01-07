{
  	inputs = {
		nixpkgs.url = "nixpkgs/nixpkgs-unstable";
		rust-overlay.url = "github:oxalica/rust-overlay";
		utils.url = "github:numtide/flake-utils";
  	};

  	outputs = { self, nixpkgs, utils, rust-overlay, ... }:
	utils.lib.eachDefaultSystem (system: let
		overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
        	inherit system overlays;
        };
		rust-toolchain = pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override {
			extensions = [ "rust-src" "clippy" ];
		});

		llvmPkgs = pkgs.buildPackages.llvmPackages_11;
		mkShell = pkgs.mkShell.override { stdenv = llvmPkgs.stdenv; };
	in rec {
		# `nix develop`
		devShell = mkShell {
			nativeBuildInputs = with pkgs; [
				pkg-config
				cmake
				rust-toolchain
				cargo-edit
				gdb
				lldb
				vscode-extensions.vadimcn.vscode-lldb

				firejail
			];
			buildInputs = with pkgs; [
				llvmPkgs.bintools
				#stdenv.cc.cc.lib
				#lld
				
				x11
				xorg.libXcursor
				xorg.libXrandr
				xorg.libXi
				libxkbcommon
				vulkan-tools
				vulkan-headers
				vulkan-loader
				vulkan-validation-layers
				fontconfig
				freetype
				protobuf
			];
			hardeningDisable = [ "fortify" ];
			NIX_CFLAGS_LINK = "-fuse-ld=lld";
			#LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib}/lib64:$LD_LIBRARY_PATH"; # Fix can't find libstdc++.so.6
			#PKG_CONFIG_PATH = "${pkgs.libxkbcommon.dev}/lib/pkgconfig";
			
			LD_LIBRARY_PATH="${pkgs.vulkan-loader}/lib"; # Vulkan Fix
		};
	});
}