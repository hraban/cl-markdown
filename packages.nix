{ lib }:

final: prev: {
  cl-markdown = prev.lispDerivation (self: {
    lispSystem = "cl-markdown";
    src = ./.;
    lispDependencies =
      with final;
      (
        [
          asdf-system-connections
          anaphora
          final."cl-containers/with-asdf-system-connections"
          cl-ppcre
          dynamic-classes
          metabang-bind
          metatilities-base
        ]
        ++ lib.optionals (self.doCheck or false) [
          lift
          trivial-shell
        ]
      );
    meta.broken =
      # “There is no class named ABSTRACT-CONTAINER.”
      (final._lisp.name == "abcl")
      ||
        # > The function LIFT::GET-BACKTRACE-AS-STRING is undefined..
        ((self.doCheck or false) && (final._lisp.name == "ecl"));
  });
}
