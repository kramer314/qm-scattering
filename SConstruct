# Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
# See the LICENSE.txt file at the top-level directory of this distribution.

env = Environment(LINK="gfortran", LINKFLAGS="-Og")
sources = ["progvars.f90", "params.f90", "output.f90", "wfunc_prop.f90", "numerics.f90", "setup.f90", "scatter.f90"]
objs = env.Program("scatter.x", sources)