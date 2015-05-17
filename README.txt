Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
See the LICENSE.txt file at the top-level directory of this distribution.

Description
===========
One-dimensional wavepacket scattering program. Time dependence is calculated
by expanding on continuum eigenfunctions.

Dependencies
============
* gfortran (tested on gfortran 4.8.2)
* scons (tested on scons 2.3.0)

Compilation / Running
=====================
To compile and run:

   scons && ./scatter.x

To clean files:

   scons -c

Extensibility
=============
Different problems can be modeled by modifying ``params.f90`` accordingly.

Todo
====
* Documentation, especially on parameters
* Extension to both bound and continuum eigenstates
* Multiple example problems
* Specify output directory
