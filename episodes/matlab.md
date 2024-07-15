---
title: "The Matlab Language"
teaching: 20
exercises: 5
---

:::::::::::::::::::::::::::::::::::::: questions

- "What are the strengths and weaknesses of the Matlab language?"

::::::::::::::::::::::::::::::::::::::::::::::::


::::::::::::::::::::::::::::::::::::: objectives

- "Understand the performance and parallelization characteristics of Matlab"
- "Learn the practicallity of using the Matlab compiler on HPC systems"

::::::::::::::::::::::::::::::::::::::::::::::::


---

## Programming in the Matlab Language

Matlab is a commercial programming language designed for broad
use in science and mathematics.
The obvious drawback of it being commercial software is that it costs
money and has licensing restrictions when you use it.
The more positive aspect is that it is professionally developed and
maintained with lots of advanced science modules.

The core Matlab code is interpretted which limits its speed but there
are lots of highly optimized and parallelized low level operations that
often lead to very good performance.
Users can also parallelize code manually using **parfor** loops which
are easy to implement but difficult to make efficient.
Many modules are also programmed to run on NVIDIA GPUs for acceleration.

There is a free language **GNU Octave** that runs much of raw Matlab code.
Some of the more advanced features are not supported, but some like
parallelization simply have a different format.
Octave also has lots of user-contributed modules covering a broad
range of science and mathematics.


### Matlab Toolboxes

The Matlab core can be enhanced with a very wide variety of
add-on toolboxes.  Some are more generic while others are 
very specific to certain areas of science.
Some of the more common ones include:

* Parallel Computing Toolbox
* Simulink - Simulation and Model-Based Design
* Statistics and Machine Learning
* Curve Fitting
* Control Systems
* Signal Processing
* Mapping
* System Identification
* Deep Learning
* DSP System
* Datafeed
* Financial
* Image Processing
* Text Analytics
* Predictive Maintenance

There are many domain-specific toolboxes that include Bioinformatics,
Aerospace, Wavelets, and Econometrics to name a few.


### Commercial licensing restrictions and costs

Matlab has a base cost with toolboxes being extra.
There are annual and perpetual licenses with all pricing being
in U.S. dollars.

[click here for pricing](https://www.mathworks.com/pricing-licensing.html?prodcode=ML&intendeduse=student)

There are huge educational discounts with students paying under 
a hundred U.S. dollars for Matlab, Simulink, and 10 add-on toolboxes.
Teachers and researchers also get large discounts, but add-on toolboxes
can increase the cost quickly as the more common ones can run a few hundred
dollars and more specialized toolboxes even more.

Licenses for HPC clusters can be floating meaning that each license
can be checked out by an authorized user and used on a given number
of processors or nodes.
This does require a license server to be set up which can be done
on the HPC cluster or handled remotely.
Just be aware that with commercial software there is additional
setup required as well as costs.


### Using the Matlab compiler

When we talk about compiling for C/C++ and Fortran, we are talking
about analyzing a block of code like the body of a loop to optimize
the code so that it runs faster.
Matlab and Python both have compilers but neither does this.
In both cases the compiler packages up the code, interpretter,
and libraries into an executable that can be run independently.
The compmiled code therefore does not run any faster.

A programmer will always start by developing and running their code
in raw form which requires checking out a Matlab license.
If you need to share your Matlab code with others who do not have
access to a Matlab license, then you would want to compile the
code to package it into an executable that doesn't need Matlab to run.
In an HPC cluster context, you still want to develop your code in raw
Matlab but when it is time to run you want to again compile it
into an executable so that many copies can be run without needing
a separate Matlab license for each.
In this way, an HPC center only needs as many Matlab licenses as
they want to have simultaneous users developing code rather than
needing to support the number of jobs run.

#### Minor compiler mcc gotcha

When you run the Matlab compiler, it does check the license out
for a half hour at a time so if you just have a single license for
a cluster users may end up waiting a bit for access.


### Performance

Matlab is not a compiled language so raw code is much slower
than C/C++/Fortran.
It is however faster than Python and R as shown in the serial matrix
multiply of 1000x1000 matrices taking 5 seconds compared to
Python at 306 seconds.
The built-in routines that are optimized and parallelized are going to
be similar in the different languages, and in this case 
Matlab takes 0.7 seconds while the Numpy version in Python takes
0.13 seconds in the same test.

Matlab does have very strong integration with
optimized and parallelized library routines throughout its modules
which brings automatic efficiency and parallelization when available.


## Parallelization methods

The **Parallel Computing Toolbox** is the source of most of the parallel
computing capabilities in Matlab.  It provides the ability to program using
multiple cores, multiple nodes, and GPUs without explicitly
using CUDA or MPI.  Matlab also includes a wide variety of parallelized
numerical libraries at its core to automatically take advantage of the
hardware you allocate to your job.
The **Simulink** toolkit allows the user to set up and run multiple simulations
of a model in parallel.
The **Matlab Parallel Server** can also be used to run matrix calculations
that are too large to fit in the memory of a single computer.

### Parallelizing loops with **parfor**

When iterations of a for loop do not depend on each other,
the iterations can be spread across multiple processes 
within the same compute node in a distributed-memory manner,
multiple threads in a shared-memory approach, 
or they can be spread across multiple nodes.
All 3 appraoches are accomplished
by changing the for loop to a parfor loop.
Computations will be split across multiple cores whether
they are on the same compute node or multiple nodes.
This provides a very easy means of parallelizing code and 
the flexibility of running the same code in a variety of
parallel environments.

This flexibility comes with a prices though.
Distributed-memory approaches are often only efficient
when the flow of data between processes is careful controlled
which is not possible here.  It often requires much more work
to get the needed efficiency out of complex algorithms.
This type of automatic distributed-memory approach also results
in large data sets being redundantly copied to all processes
which can lead to extra execution time for the communications
and much extra memory usage.
So if you are working with a 1 GB size matrix and want to run
on a 128 core AMD system you would have to copy and redundantly
store 128 GB of data at the start.

The multi-threaded appraoch is designed to avoid this redundant
memory use by leaving read-only data sets in place rather than
copying them to each thread.  This holds much greater promise
cenceptually, but tests with a simple parallel matrix multiplication
are showing much slower times than expected. 
Even a parallel dot product which is trivially parallel takes
longer than the serial version so it is unclear how useful
even the multi-threaded parfor is in general.

So while Matlab provides an easy-to-use and flexible parallel programming
environment with **parfor**, it can suffer greatly when
dealing with large data sets and complex algorithms,
and doesn't even do all that well on simple algorithms.
From these tests so far I would recommend
using this approach mostly for trivially parallel algorithms 
and smaller data sets and testing very carefully.

**NOTE: Loop iterations are non-deterministic and 
indicies must be consecutive increasing integers,
and there is no nesting of loops allowed.**


## Octave

GNU Octave is a language that is largely compatible with Matlab,
but it is free and unlicensed.
It can be used to run many Matlab programs using **octave < matlab_code.m**
though many advanced features of Matlab may not be supported.
There are many add-on packages available for Octave but these are different
from those available for Matlab.

There is a **parallel** toolbox that is well developed.
This provides a local parallel execution environment similar
to the single-node multi-process capability of parpool.
There are also tools to work with clusters of computers,
but these are more similar to message-passing commands where
you manually send and receive data to and from remote processes
and manually initiate function evaluation.




> ## Test the performance of the Matlab matrix multiplication code.
> Test the performance of the **matmult.m** code for a matrix size of 
> 1000 and compare to other languages.
>  > ## Solution and Analysis
> For the 1000x1000 matrices, I measure 5 seconds for the serial code,
> 0.7 seconds for the built-in matrix multiply that uses low level
> optimized BLAS routines.  The parpool multi-process test takes
> 530 seconds which is understandably slow since it is doing the
> matrix multiplication in a distributed memory manner without
> explicitly programming it to do this efficiently.
> The multi-threaded parpool test measured in at > 510 seconds
> which is very disappointing since there should be no copying of the
> matrices at the beginning.  It isn't clear what is happening behind
> the scenes for this to be so slow.
> {: .solution}
{: .challenge}

> ## Test the performance of the Matlab dot product code.
> Test the performance of the **dot_product.m** code for an array size of 
> 100,000,000 and compare to other languages.
>  > ## Solution and Analysis
> I measure serial performance at 0.5 seconds with the built in optimized
> routine at 0.2 seconds on 8 cores for a modest speedup.
> The multi-core parfor loop on the same 8 cores takes 2.1 seconds while
> the multi-threaded parfor loop takes a disappointing 0.9 seconds which
> is still greater than the serial code.
> The overhead for using these methods is still much larger than the
> performance gain which indicates the parfor method should only really
> be used for very coarse grained algorithms.
> {: .solution}
{: .challenge}


> ## Homework:  Alter the **matmult.m** code to run on multiple nodes and multiple cores.
> Test on multiple compute nodes and compare performance to the serial and
> multi-threaded versions.
> If you want a real challenge try setting the code up to run on a cloud server.
> And an even bigger challenge would be to convert matmult.m to run on Octave's
> parallel computing environments.
>  > ## Solution and Analysis
> If you do develop code for this, let us know so we can consider including
> your work for others to see.
> {: .solution}
{: .challenge}



## Summary

The greatest value of Matlab is the very wide range of professionally
developed and maintained packages that are available in many areas
of science.
This comes at a financial cost that often limits how codes can be used,
but some of this can be circumvented in an HPC environment by using
the Matlab mcc compiler to create an executable that does not need 
a license to run on many compute nodes at once.

Matlab code itself is not that fast, but it uses highly-optimized
library routines seemlessly whenever possible.
Adding parallelism into a code manually is often as easy as changing
the for loop to a parfor loop, but flexibility and ease of use 
often do not produce efficient code.
These methods are probably only useful for trivially parallel algorithms.

Octave is a viable option to avoid the cost of Matlab, and has many
add-on packages of its own as well as parallel computing capabilities.
You should however only expect the more basic Matlab codes to run
with Octave, then you would need to choose to split off into the
Octave world itself.


### Links for additional information

* [Matlab documentation](https://www.mathworks.com/)
* [Octave website](https://octave.org/ )
* [Matlab Parallel Computing Toolbox](https://www.mathworks.com/products/parallel-computing.html)
* [Matlab Simulink](https://www.mathworks.com/products/simulink.html)
* [Matlab Toolboxes and Examples](https://www.mathworks.com/help/thingspeak/matlab-toolbox-access.html)

::::::::::::::::::::::::::::::::::::: keypoints

- "Learn about the characteristics of the Matlab language"

::::::::::::::::::::::::::::::::::::::::::::::::

{% include links.md %}

