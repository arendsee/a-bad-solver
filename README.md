[![experimental](http://badges.github.io/stability-badges/dist/experimental.svg)](http://github.com/badges/stability-badges)

# A toy logic solver

Since this is entirely a person project, and since I am a complete novice,
there is probably no reason for you to look at this repo.

The purpose of this repo is to implement my own logic solver. Eventually, I may
port this package Morloc to work as its type checker. To that end, my solver
will differ from normal ones in that generating interpretable error messages is
a priority.

I will start with a simple SAT solver. I may then move on to implementing
a more general SMT solver. As I go, I will play with case studies for solving
problems with a bias towards type systems.

## Inspiration

I started from the material Andrew Gibiansky blog [1]. This was used in the
implementation of the SAT solver.

I also want to write an SMT solver. [2] shows an example of the use of an SMT
solver in compiler optimization. This might be a good place to continue.

[1] Andrew Gibiansky (2015) Writing a SAT solver.
    http://andrew.gibiansky.com/blog/verification/writing-a-sat-solver/

[2] https://www.well-typed.com/blog/2014/12/simple-smt-solver/
