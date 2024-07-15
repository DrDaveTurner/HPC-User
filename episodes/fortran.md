---
title: "The Fortran Language"
teaching: 15
exercises: 5
---

:::::::::::::::::::::::::::::::::::::: questions

- "What are the strengths and weaknesses of Fortran?"

::::::::::::::::::::::::::::::::::::::::::::::::


::::::::::::::::::::::::::::::::::::: objectives

- "Analyze the merits of Fortran"
- "Learn how to compile andd run a Fortran program"

::::::::::::::::::::::::::::::::::::::::::::::::

---

## Modern Fortran

Fortran is one of the oldest computer languages used for scientific computing.
One reason it is still heavily used is for historic reasons since there are
just so many lines of Fortran code out there that are hard to replace.
The good news is that this code base is extremely efficient.
The Fortran language has also continued to modernize adding much of the same
advanced functionality of C++.

Historically people used the f77 or Fortran 77 standard for a long time
(defined in 1977).  Modern Fortran has made great strides from this old
code in adding object oriented programming capabilities and a less stringent
form.
Fortran code is identified by the **.f** suffix or **.F** if there are
pre-processor commands embedded.  You may also see the **.f90**
suffix to denote that the code adheres to the fluid formatting of the
Fortran 90 standard defined in 1990.
Files with the **.mod** suffix are modules.

All these newer standards have added capabilities similar to C++
like dynamic memory allocation, object-oriented programming, and
operator overloading.
More recent work has been geared toward adding more parallel capabilities
like the **Coarray Fortran** parallel execution model and the
**Do concurrent** construct for identifying that a loop has no
interdependencies and is therefore capable of being parallelized.

The primary value of Fortran will always be its efficiency and the same 
access to all the scientific and mathematical packages shared with C/C++.
It is a column-major language like R and Matlab, and starts arrays
at one instead of zero just like both of those as well.
OpenMP and MPI packages likewise have full support for Fortran.

So Fortran is every bit as powerful and efficient as C/C++, but
it is slowly being taken over by C/C++ on large supercomputers.


### Language characteristics to avoid (gotchas)

While most memory in C/C++ is dynamically allocated, it is very
common to have Fortran arrays statically allocated to a given size
especially in older codes.
This memory comes from what internally is called the **stack** which
is a variable defined on each system.  In our cluster at 
Kansas State University the default stack size as seen by
doing **ulimit -a** is set to only a little over 8 MB while
data arrays can easily exceed a GB at times.
When you exceed the stack size, your job crashes with a segfault
that will not give you any useful information on what went wrong.
If you think the stack size may be an issue, you can include
a command **ulimit -s unlimited** before running your application
to remove the stack size limit entirely.


### Compiling Fortran code

Fortran is compiled with many of the same arguments and libraries used for
C/C++.
The Gnu version is **gfortran** and the Intel compiler is **ifort**.
When using the OpenMP multi-threading package you will add the
**-fopenmp** or **-openmp** flag respectively.
To compile a Fortran MPI code you will use **mpifort**.

While there are many compilation options for each of these,
you can general get by with **-O3** level 3 optimization.
I also strongly suggest always compiling with **-g**.
This creaates a symbol table so that if your code crashes you
at least get the line number where it failed.
Without this you get a pretty meaningless onslaught of information
that won't really give you a clue as to the problem.
Compiling with **-g** should not slow down your code as long as
you also use **-O3**, and the extra size of the executable should
not matter.

Compiling source code to get an executable is actually a two step
process where source code is compiled into binary object files which
are combined with any libraries needed in the linking stage.
For simple applications this may all be done in a single step.
For more complex codes involving many individual source files and
modules it is common to have a **Makefile** handle everything.
The **Makefile** provides the logic to compile only the parts of 
an application code base that have changed, then link everything
together to produce the executable.


> ## Practice compiling and running Fortran codes
> Try compiling the **dot_product_fortran.f90** code with the **gfortran**
> compiler, then try with **ifort** if you have access to it.
> Do the same with the optimized code **dot_product_fortran_opt.f90**
> to see the difference that the built-in **dot_product( x, y )** function
> can have.
> You can then compile the OpenMP version **dot_product_fortran_openmp.f90**
> and do a scaling study, and if you 
> are on a system with MPI installed then try compiling and running
> the MPI version **dot_product_fortran_mpi.f90** using **mpifort**.
> Once you have compiled these codes manually take a look at the
> **Makefile**.  This contains all the commands necessary to compile all the
> codes above with a single command **make all_fortran**.
>  > ## Solution and Analysis
> Each computer system may be set up differently with regard to what
> compilers are available and how they need to be accessed.
> You may need to contact your administrator for help if you are unsure
> whether you have the Intel compiler suite installed, and how
> to access an MPI package if avaiable.
> In my tests on a modern Intel system the raw Fortran code and optimized
> both took 0.14 seconds as did the OpenMP using 1 thread and the
> MPI version using 1 task.
> OpenMP using 4 threads took 0.063 seconds which is a little more than
> twice as fast and then performance flattened out for more threads.
> The MPI version using 4 tasks took 0.05 seconds which is slightly better
> than the OpenMP version, and 0.027 seconds for 8 tasks showing better
> scaling.
> {: .solution}
{: .challenge}

### Links for additional information

* [Fortran wiki](https://en.wikipedia.org/wiki/Fortran)
* [Fortran tutorials](https://fortran-lang.org/learn/)

::::::::::::::::::::::::::::::::::::: keypoints

- "Learn about the characteristics of modern Fortran"

::::::::::::::::::::::::::::::::::::::::::::::::

{% include links.md %}

