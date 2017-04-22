clpfd-smt
=========

This package contains libraries for finite domain constraint programming,
where an SMT solver is used to solve finite domain constraints.

Overview
--------

The operations to define constraints are collected in the library `XFD.FD`.
The actual FD solvers are defined in other libraries (e.g.,
`XFD.Solvers.SMT.Z3`). By importing the solver libraries,
one could use the interface of the library `XFD.FD` to specify
constraint problems and the solver operations (e.g., `solveFD`,
`solveFDAll`, `solveFDOne`) to solve the FD constraint problems
with the corresponding solver.

Note that this interface is almost identical to the interface
of the (Prolog-based) FD constraint solver library `CLP.FD`
distributed with PAKCS. For instance, to solve FD problems
with the SMT solver Z3, one has to import the library
`XFD.Solvers.SMT.Z3` instead of `CLP.FD` (provided that Z3 is installed).
Similarly, one can import the library `XFD.Solvers.SMT.CVC4`
to solve FD problems with the SMT solver CVC4.

The directory `examples` contains various examples to specify
FD constraint problems.

Note that these libraries require that the corresponding SMT/SAT solvers
are already installed. For instance, the executable `z3` must be
in your path when you want to use the Z3 solver for FD solving.
Alternatively, one could modify the solver configuration in the
corresponding Curry modules (e.g, the definition of `solverConfig`
in the module `XFD.Solvers.SMT.Z3`).


Documentation
-------------

The structure and implementation of these libraries
are described in the master's thesis

[Implementation of Constraint Solvers for Curry with SMT](https://www.informatik.uni-kiel.de/~mh/lehre/abschlussarbeiten/msc/hueser.pdf)
(in German, by Sven Hüser, CAU Kiel, March 2016)
