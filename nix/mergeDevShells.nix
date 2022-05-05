{ pkgs, ... }:

# Merge multiple devShells into one, such that entering the resulting
# dev shell puts one in the environment of all the input dev shells.
envs:
pkgs.mkShell (builtins.foldl'
  (a: b:
    # Standard shell attributes
    {
      buildInputs = a.buildInputs ++ b.buildInputs;
      nativeBuildInputs = a.nativeBuildInputs ++ b.nativeBuildInputs;
      propagatedBuildInputs = a.propagatedBuildInputs ++ b.propagatedBuildInputs;
      propagatedNativeBuildInputs = a.propagatedNativeBuildInputs ++ b.propagatedNativeBuildInputs;
      shellHook = a.shellHook + "\n" + b.shellHook;
    } //
    # Environment variables
    (
      let
        isUpperCase = s: pkgs.lib.strings.toUpper s == s;
        filterUpperCaseAttrs = attrs: pkgs.lib.attrsets.filterAttrs (n: _: isUpperCase n) attrs;
      in
      filterUpperCaseAttrs a // filterUpperCaseAttrs b
    )
  )
  (pkgs.mkShell { })
  envs)
