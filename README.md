
# Numerical benchmarks for Julia, Python.

We try to avoid trivial and non-sens benchmarks (for people doing
numerics!) like fibonacci, sorting and so on.

We put ourself in the shoes of a typical Matlab or Matlab-like
programmer, writing quite short but numerically intensive
programs. 

 Are Python
and Julia easy to use and efficient? We compare them with a C++
optimized implementation.

The benchmark(s):

* Gaussian:  Gaussian elimination with partial pivoting (see README
  inside).
  
* :new: Weno: a classical computation in hyperbolic PDES (see README
  inside).
  
* :new: MicroBenchmarks: very simple benchmarks to show the importance
  of different programing styles. 


We will add other numerical significative benchmarks in the (near) future.

### Dependencies:

#### What you need to install:


- python3
- pip
- g++ (and/or clang++)
- lapack
- openblas
- cmake
- gnuplot

You can install them using your distribution tool (apt...).

- julia

:exclamation: Julia :exclamation: programs  need at least  version Version 0.6.2 (stable
version in March 2018; note 
that the version packaged with  Ubuntu 17-10  is older). Install the stable version from
[here](https://julialang.org). Note also that Julia is evolving, and it is
possible that the codes need some adaptation to run with later
versions of the language.


You also need:

- pythran
- scipy
- Numpy
- numba

to install them,  you can just do:

```
pip install pythran
```
and so on...

You can also install them from [conda](https://conda.io/docs/).
