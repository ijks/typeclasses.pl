# typeclasses.pl

A proof-of-concept Prolog implementation of Haskell's type class system, specifically instance resolution.
Comes with a parser for (a fragment of) Haskell type class and instance declarations.

## To Do

- [ ] Input checks:
  - [x] definition of classes
  - [x] presence of superclass instances
  - [ ] coherence
  - [x] the type in the head is `T a1 ... an` (no FlexibleInstances)
  - [x] Only type-variable arguments in contexts (no FlexibleContexts)
  - [ ] no unbound variables (no UndecidableInstances)
    - if FlexibleContexts but no UndecidableInstances, then constraints must
      be 'smaller' than the instance head.
  - [ ] anything else that could prevent termination?
- [ ] Qualified type identifiers
- [ ] Explicit instance dictionaries?

## Future Work

* Correctness.
  * Unit/property tests.
  * Prove against formal specs.
* Performance.
* Use in a real compiler (e.g GHC plugin).
  * Would likely highlight places where implementation is suboptimal (either performance or API).
* Configurable system for toggling language extensions or adding other checks.
* Techniques could be adapted for other FP language implementations in Prolog---e.g. hijacking unification for Hindley-Milner type systems.
  * But this would probably get tricky once rank-n types are required.
