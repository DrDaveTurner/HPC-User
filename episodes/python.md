---
title: "The Python Language"
teaching: 25
exercises: 15
---

:::::::::::::::::::::::::::::::::::::: questions

- What are the strengths and weaknesses of programming with Python?

::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives

- Analyze the merits of programming in Python.
- Learn how to work with virtual environments.
- Learn about common performance-oriented libraries.

::::::::::::::::::::::::::::::::::::::::::::::::


### Python performance

Python is a high-level and extremely flexible language that has 
broad acceptance within the high-performance computing community and 
even more broadly with scientific and technical programmers.
However, it isn't a performance language in the same manner as 
the compiled languages C/C++ and Fortran.
It is an interpreted language that executes the code line by line
while compiled languages optimize larger blocks of code before
execution.
At the Department of Energy supercomputing center NERSC Python is
involved in 25% of the code bases for the complex applications,
but only accounts for 4% of the CPU time on their systems.
This means that Python has a high level control role
while the compiled languages are used to do the performance computations.

A matrix multiplication algorithm implemented with raw Python code is
more than a hundred times slower than raw C code that is compiled.
You can achieve decent performance with Python only when your
code uses the highly-optimized libraries, and fortunately there
is a rich set available such as **NumPy** and **SciPy** among
many others. 
Even these can be slower than their compiled language counterparts
though, as a **NumPy** matrix multiplication is still 50%-25% the
speed depending on the matrix size compared to a 
**BLAS** library **DGEMM** function available for C/C++/Fortran.

While Python is not intended to be a performance language, there
are very good options available for parallelization.
The **pymp** library is a multi-threaded package based on a 
stripped down version of the much more extensive **OpenMP** library 
available for the compiled languages.
**pymp** is actively developed and maintained and provides the basic 
functionality of OpenMP to the Python environment in an easy to use manner.
The **mpi4py** package likewise provides a stripped down MPI
environment for Python users.
While neither of these is anywhere near as complex nor complete
as their compiled language cousins, both provide the basics that
most programmers will need.

**Dask** is another option that extends Python in two ways.
If your data set is too large for the memory on one computer,
it provides the framework to use packages like **NumPy**, **Pandas**,
and Python iterators on data that is distributed across multiple
compute nodes in a cluster.
It also provides a dynamic task scheduler for parallelizing code.

**Numba** is designed to speed up raw Python code by compiling
blocks of code programmed as functions that are tagged by the 
programmer with **@jit(nopython=True,cache=True)**.
This uses the industry standard **LLVM** compiler library,
but the compilation is not being done
once ahead of the runtime as with C/C++ and Fortran, it is being
done at runtime in what is referred to as a __just-in-time__
or JIT manner.
How effective this approach can be will depend on how much the
compilation can be done in advance of when it is needed, and how
much optimization it can do on the fly, but there also is the
option to save the compiled functions so that they don't need
to be recompiled for subsequent runs.
There are also multi-threading options for parallelizing functions.

**Parsl** is a parallelization library for Python that very easily
allows that programmer to define functions as separate apps or
applications that can be run on different cores or compute nodes.
This approach is especially useful in expressing multi-step workflows.
The link below provides more information on this approach.

[Parsl python package github](https://github.com/Parsl/parsl)

**memory_profiler** is a Python package that allows easy profiling
of the memory usage of the code.  If run externally using the 
**mprof** command it reports the full memory usage of the executable
which can also be easily plotted.
You can also add **@profile** statements before functions to provide
line by line memory profiling to help identify exactly where in your
code your large memory allocations are occurring.
The link below provides a more detailed explanation with examples.

[Python memory-profiler package documentation](https://pypi.org/project/memory-profiler/)

### Python capabilities

For what Python lacks in raw performance as an interpreted language
it more than makes up for in its flexibility and easy of use.
It is common to develop code in an interactive environment like
a Jupyter Notebook which provides a very quick cycle from changing
code to testing it.
It is a high level language that is easy to read, write, and learn,
and with advantages like dynamic variable typing.
All this makes Python a high productivity language enabling
programmers to produce code more quickly than in many
lower level, more computationally efficient languages.

Where this language really shines is in the extensive selection of
software packages that have been developed for the Python
environment.
Some important examples are the TensorFlow and Scikit packages
for doing Artificial Intelligence and Machine Learning,
but Python is commonly used in fields as diverse as video processing,
data mining, game and language development, finance, and general
programming.


### Technical aspects

Python is a row-major language like C/C++ where 2-dimensional
arrays or matrices are stored by row with elements in each row
being next to each other in memory.
Python is also like C/C++ in that arrays are numbered starting
with 0.
Languages like Fortran, R, and Matlab are the opposite of
Python and C in both these respects.

Python is an interpreted language like R and Matlab.
It can be compiled but not in the same way as C/C++ and Fortran
where the compiler optimizes whole blocks of code.
The Python compiler simply packages the code up so
that it can run independent of Python or any installed packages.
This means the user running the 'compiled' python executable 
does not need access to the same version of Python nor any
virtual environment with the python packages installed since
everything is packaged up in the executable.
There will be an exercise at the end of this section
where you will be able to test this for yourself. 


### Advantages of using a virtual environment

If you are on your own laptop or desktop computer you can install
Python packages system wide if you would like.
On HPC systems there may be some Python packages installed as
loadable modules.
You can also install packages locally on your home directory
using the **--user** flag to **pip install**.

It is strongly advised however that you use a different virtual environment
for each new project.  While you may need to re-install some
common packages like **NumPy**, having a clean virtual environment
for each project helps avoid problems with conflicting versions of
dependency packages.
Quite often if users are having trouble installing Python software,
I'll advise them to start with a clean virtual environment and 
that fixes everything.

Creating a virtual environment is not very difficult.
Below is an example of creating one called **env-py-3.7.4**
where I labeled it after the version of Python being used.
Once activated the prompt will change to show that you are
working in the **env-py-3.7.4** environment.
You can install any packages you need, run your python code,
then deactivate the environment when you are done.
The dollar sign is the command prompt to illustrate how the prompt
changes while the virtual environment is active.

```bash
$ mkdir -p ~/virtualenvs
$ cd ~/virtualenvs
$ python -m venv --system-site-packages python-hpc-user
$ source ~/virtualenvs/python-hpc-user/bin/activate
(python-hpc-user$ pip install numpy scipy
(python-hpc-user)$ python python_code.py
(python-hpc-user)$ deactivate
$
```

### Compiling Python

Compiling Python code does not do the same type of code loop
analysis that compiling C/C++ or Fortran does, so there really
isn't any speedup involved.  It does create a self-contained
executable that no longer needs to have a matching Python version
or installed package virtual environment available.
It is therefore most useful when you are done developing the
code and want to distribute it as a binary.

Compiling Python code does vary depending on the operating
system you are on.  In Linux you can compile code by
adding a flag **-m py_compile** which will create an
executable in a **__pycache__** directory.

```bash
python -m py_compile python_code.py
mv  __pycache__/python_code.cpython-37.pyc  compiled_python_code.pyc
```

The executable **compiled_python_code.pyc** will then run on
systems where Python is not installed and you have no virtual
environment activated.


### Language characteristics to be aware of (gotchas)

One of Python's greatest strengths is that it is a simple 
high-level language that is interpreted and easy to use.
This is also its biggest disadvantage when it comes to performance.
Simply put, when you need performance you need to use the 
highly-optimized numeric libraries.
Unfortunately compiling Python code really doesn't help
when there are no optimized library codes available.

Virtual environments provide an excellent way to manage
the installation of software packages, and it is easy
to use the **pip install** command which installs the
desired package and any dependencies.
You do always need to have the active virtual environment
match the Python version number used to install all those
packages, so if you want to use a newer version of Python
you will have to reinstall all the packages too.

Python2 was deprecated on January 1 of 2020.  Everyone should
be programming in Python3 now, and these codes are not 
backwards compatible with Python2.  This means that if you
encounter any Python2 code you would either need to convert
the code to Python3 or find an old and deprecated version
of Python2 and hope that still works.  Basically if you run
into old python code at this point, expect to have some
errors to deal with.

Python uses a Global Interpreter Lock (GIL) to ensure that 
only one thread can control the interpreter at any given time.
This basically defines Python as being single-threaded which
simplifies everything internally.
The problem comes when a programmer wants to do multi-threading
to apply more power to solve a problem faster.  The
**pymp** package gets around the GIL by forking off separate
processes and therefore each essentially has its own GIL.
The **mpi4py** package for message-passing is not affected
since with MPI each task is a separate copy of the same
program.  In general, the GIL is something to be aware of
but packages like **pymp** have already done the work of getting
around the issue.


:::::::::::::::::::::::::::::::::::::: challenge

## Play with the Python dot product examples
Take a look at the Python versions of the dot product code
to see how they differ from the C and Fortran versions.
Also compare the **pymp** multi-threaded and **mpi4py**
message-passing versions to the scalar version to see what
changes were made.
If you haven't already, run the scalar code and do scaling
studies with the multi-threaded and message-passing versions
to see how they compare with versions written in  other languages.

:::::::::::::::::: solution

While the syntax is different, the code is basically the same in
each language.  Performance is much slower for raw Python code
than the compiled language.
From looking at the same code in various languages, which do you
think would be easiest to write from scratch?

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


:::::::::::::::::::::::::::::::::::::: challenge

## Compare the raw matrix multiply code to a **NumPy** version
Do a scaling study of the **matmult.py** code for various
matrix sizes of 10, 100, and 1000.  Compare this to the
**matmult_numpy.py** version to see how much of a difference the 
highly-tuned library function makes.

:::::::::::::::::: solution

If you have already run the optimized C version, you may notice
that the compiled code still beats the optimized **NumPy** routine
by 2-4 times in my tests.
Using the **NumPy** routine closes the performance gap enormously
but the C version is still better.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


:::::::::::::::::::::::::::::::::::::: challenge

## Optional Exercise - Try compiling a Python code
If you have time, try compiling one of the Python test codes 
supplied like **matmult_numpy.py**.  Then try running it without
an active virtual environment meaning that there is no **NumPy**
library installed.

:::::::::::::::::: solution

Once compiled you should be able to run this with a command like:
**./matmult.pyc 100**.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


:::::::::::::::::::::::::::::::::::::: challenge

## Optional Homework - Test the **Numba** version of the dot product code
Do a **pip install numba** then
time the **dot_product_numba.py** performance and compare to 
the raw code.
Does the performance change if you cache the compilation by
adding **cache=True**?
You will need to run twice so that the second time takes advantage
of the cached compilation code.

:::::::::::::::::: solution

Notice that in this code we needed to rewrite the 
computationally intensive loops into functions.
This does not take much effort but does disrupt the program flow
somewhat, but if it speeds up the runtime it is usually worth it.
Unfortunately in the case of the dot product the **Numba** version
takes 13 seconds compared to only 90 milliseconds for the raw code.
Adding **cache=True** to cache the compiled code does reduce the
runtime down to 3.7 seconds but this is still substantially worse
than the original code.
There are also warnings about lists being deprecated as input 
arguments in the near future. 
Switching our algorithm to use **NumPy** arrays
may be necessary but this also defeats the purpose of using **Numba**
since **NumPy** already has optimized routines for the algorithms
we are testing.  So in short this isn't a really good test
of the capabilities of **Numba**, but is a good example of how these
things don't always work as hoped, and it illustrates that **Numba**
does not support all aspects of Python.
Forcing the computational parts of an algorithm into functions is
also detracts from the flow of any program, and is definitely not 
what is considered as the Python way.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


:::::::::::::::::::::::::::::::::::::: challenge

## Optional Homework - Write a Pi calculation program using **Numba**
Write a Python version of the Pi calculation program and time
raw Python code versus **Numba** optimized.
If you want to have more fun try out **Numba** multi-threading
compared to **pymp** for this same algorithm.
This is a much fairer test of the capabilities of **Numba** since we
are not comparing it to optimized functions in **NumPy**.

:::::::::::::::::: solution

If you do this please contribute your code and timings.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::



::::::::::::::::::::::::::::::::::::: keypoints
- Learn about the characteristics of the Python language.
- When performance is important always use optimized libraries!!!
::::::::::::::::::::::::::::::::::::::::::::::::


### Links for additional information

* [Python tutorial](https://docs.python.org/3/tutorial/)
* [pymp multi-threading package](https://github.com/classner/pymp)
* [mpi4py on github](https://github.com/mpi4py/mpi4py)
* [mpi4py documentations](https://mpi4py.readthedocs.io/)
* [TensorFlow]()
* [Scikit]()
* [Dask](https://dask.org)
* [Numba](https://numba.pydata.org)
* [Parsl python package](https://github.com/Parsl/parsl)


