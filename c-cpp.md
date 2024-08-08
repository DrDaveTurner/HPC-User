---
title: "C and C++ Languages"
teaching: 15
exercises: 10
---

:::::::::::::::::::::::::::::::::::::: questions
- What are the strengths and weaknesses of programming in C and C++?
::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives
- Analyze the merits of the C/C++ language.
- Learn how to compile and run C/C++ programs.
- Try compiling and running OpenMP and MPI versions.
::::::::::::::::::::::::::::::::::::::::::::::::

## The C Programming Language

C is a very low level language that is extremely flexible and efficient.
It is the language used to program the Linux operating system and
the Python, Matlab, and much of the R languages.
But all this power comes at a price; it is more difficult to debug.

C files end in **.c** with header files ending in **.h**.
It is a row-major language meaning that a matrix is stored by rows
with elements of each row next to each other.
Arrays are numbered starting with zero same as with Python.

In C, variable types are less strict to allow for greater flexibility,
but this makes it more difficult for compilers to catch errors before run time.
Memory is dynamically allocated in a raw form and assigned with a pointer to the first
element, but there is little control after that on how the programmer 
accesses the memory.  If the program tries to write to memory past what is
allocated to that array, there is no protection to prevent it from happening.
So the programmer is responsible for a lot more since the compiler cannot
check much of the code and provide detailed warnings.
This is just the cost of the low level access and flexibility of C.

Part of the power of C as well as C++ and Fortran is the access to
massive numbers of highly-optimized libraries of routines that have been developed
over the past 60 years.
These involve scalar, vector, and matrix algorithms in the
BLAS (Basic Linear Algebra Subroutines) libraries, sparse matrix libraries,
Linear Algebra Package of LaPack and its multi-processor version
ScaLapack, FFT (Fast Fourier Analysis) routines, and many others.

**OpenMP** is the premier multi-threaded package available for
scientific computing.  There are other methods of doing multi-threaded
computing like **pThreads** that are just as efficient but harder to use.
**MPI** is likewise developed specifically for C/C++ and Fortran.
While other languages have stripped down versions implemented, none
can rival the rich set of functionality that the OpenMPI and MPICH
packages provide with the full MPI standard.

C doesn't have as much access to statistical package that have been developed
for R, nor the mathematical toolboxes of Matlab and the wide variety of
artificial intelligence toolkits of Python.
But it is unrivaled in power, performance and flexibility.

## The C++ Language

C++ is a super set of C, meaning that it starts with C and adds
functionality beyond.
Since C can be embedded with C++ code you get the best of both
worlds with access to the low level capabilities of C along
with the high level data structures and programming functionality of C++.
C++ files end with **.cpp** and use the same header files ending in **.h**.

It is an object-oriented language, with objects having data that
can be private (hidden) or public (exposed) along with definitions
of how that object is created and interacts with other objects.
This is good in a sense since much of the work in creating an object
is hidden from the programmer, but hiding this process also means
it is more difficult to track memory usage and computations, both
of which are very important in understanding performance issues.

In C++ you also have overloaded operators, meaning that a
multiplication sign can have different meaning
depending on the data types it is applied to.  If it is between 2
scalar variables then a scalar multiplication is done, while the same
operator between 2 matrices would do a matrix multiplication.
This is why C++ is great for programming other languages like R
since the programmer can define what each operator does and have
that be dependent on the variable types involved.

Much of what makes C++ so powerful is also what makes it more
difficult to work with where performance is concerned.
The ability to hide object initialization means that memory allocation
is also hidden.
Operator overloading also can obscure the computations being done, as a
multiplication between two variables as in the example code below
may represent a single floating-point
operation or a triply-nested loop if the operands are both matrices.
Memory and computations are simply less explicit in C++ by design,
and that can make it more difficult to identify where performance
issues may lie.

```cpp
C = A * B;
```

## C/C++ Compilers

Unlike interpreted languages like Python, R, and Matlab, you have
to compile C and C++ code into an executable before running.
The compiler analyzes the code and optimizes it in ways
that cannot be done on the fly with interpreted languages making
the resulting executable much more efficient.

The most common compilers are the Gnu compiler **gcc** with the
C++ version **g++** and the exceptional commercial Intel compilers 
**icc** and **icpc**.  There are many compiler options available
but you can't go wrong using **-g** to generate a symbol table 
which will provide a line number where the error occurred in case
of a crash and **-O3** for high-level optimization.
You compile in the OpenMP pragmas using **-fopenmp** for the Gnu
compiles and **-openmp** for the Intel compilers.


## Makefiles for compiling

Compilation is actually done in two stages although for single
file applications it is typically done in one step.
Source files get compiled into binary object files with a **.o**
suffix then all those are linked together with any optimization
libraries to produce the executable.

Large applications may divide the source code into many smaller
source files for organizational reasons.
A **Makefile** can be developed that has all the logical directions
to compile a single application with a single **make** command.
These dependencies have many advantages, such as speeding up the
compilation process by allowing only those source files that have
changed to be recompiled into new object files.
You will get a chance to examine a small makefile in the exercise
at the end of this lesson.

## Installing large software packages

To install any large software package you will need to read the
documentation and follow the directions.
Having said that, many well designed packages follow a similar
approach of running a **configure** script, then compiling the
package with **make** and installing it with **make install**.
The **configure** script usually requires at least a **--prefix=**
argument to tell it where to install the software.
While you will see many variations on this approach it is good to
at least understand this as a starting point.

* ./configure --prefix=/path/to/installation/directory
* make
* make install

:::::::::::::::::::::::::::::::::::::: challenge

## Practice compiling and running C codes

The most common compiler for C is the Gnu C Compiler **gcc**.
This may be available by default on your system, so try
**gcc --version** to check.  If not then you'll need to figure out
how to gain access to it.  You can also try the Intel C Compiler
**icc** if you have it available on your system.

Try compiling the **dot_product_c.c** file using 
**gcc -g -O3 -o dot_product_c dot_product_c.c** which tells the
compiler to use optimization level 3 and create the executable 
dot_product_c from the source code dot_product_c.c.
Once compiled you can run this using **./dot_product_c** or
submit a job to the batch queue.
Try also to compile with **icc** if it is available.

Next try to compile the OpenMP multi-threaded version.  You will
need to tell it to access the OpenMP library using a 
**-fopenmp** flag for the **gcc** compiler or **-openmp** for **icc**.
Try a few runs with different numbers of threads to get
comfortable with running on multiple cores.

If you have an MPI package installed, try compiling the
message-passing version using
**mpicc -g -O3 -o dot_product_c_mpi dot_product_c_mpi.c**
and running some tests with **mpirun -np 4 dot_product_c_mpi**
for example.

If you want more practice you may try running the matmult_c.c
code and the optimized version matmult_cblas.c.

:::::::::::::::::: solution

On a modern Intel system the raw scalar code ran in 0.14 seconds
as did the single-threaded OpenMP and single-task MPI runs.
The test on 4 threads took 0.06 seconds which is quite a bit off
the 4x speedup we are looking for.  This again is due to how little
work is being done during each pass through the loop compared to the
loop overhead.
The MPI test on 4 tasks is better at 0.047 seconds and is a little
faster at 0.034 seconds on 8 tasks since the parallelization is
done in a different manner.
How do your results compare to these?

:::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::::::::::::::


::::::::::::::::::::::::::::::::::::: keypoints
- Learn about the characteristics of C/C++
::::::::::::::::::::::::::::::::::::::::::::::::

### Links for additional information

* [C Tutorial](https://www.guru99.com/c-programming-tutorial.html)

