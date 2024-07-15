---
title: "Multi-Threaded Programs"
teaching: 20
exercises: 10
---

:::::::::::::::::::::::::::::::::::::: questions
- What is the multi-threaded shared-memory programming model?
::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives
- Learn about multi-threaded computing and how to use it.
- What are the strengths and limitations of this approach?
::::::::::::::::::::::::::::::::::::::::::::::::


Most computer languages have the ability to do multi-threaded computing.
C/C++ and Fortran use the OpenMP package which is by far the most well
developed.
It uses pragma statements to control the parallelization of loops.
OpenMP is not only extremely efficient, but it also provides very advanced
features offering greater control on how the parallelization is to be done,
all without encumbering the programmer too much.
The **pymp** package for Python is a stripped down version of OpenMP supporting
just the basic functionality.  It is an actively developed project
and is very efficient and relatively easy to use as well.
R takes a very different approach in doing multi-threading using the
**mclapply()** function which is a multi-core replacement for the 
**lapply()** function.
This operates similarly to OpenMP and pymp but uses a very different syntax.
It is also not nearly as efficient and requires some non-default choices to
make it more efficient.
Matlab also uses a different syntax in its Parallel Computing Toolbox
where it uses a **parfor** command to do a parallel for loop.

All of these methods behave basically the same by forking extra threads
when called.
Each thread gets its own virtual memory space, but most large arrays are
not copied during the initialization stage.
If any thread writes to an array, only then is that array copied to that
thread's memory, and only the page of memory that has been changed.
This is called a **copy-on-write** method and is handled by the operating
system.
Forking is very efficient in this way, only doing the work it needs.
For Python, this gets around the Global Interface Lock which is designed
to protect python objects.
Unfortunately the Windows operating system does not have support for forking
so you cannot run multi-threaded Python codes like **pymp** on Windows.
(Modern Windows systems do have the Windows Subsystem for Linux (WSL).
Does WSL support forking???)

The figure below illustrates how multi-threading works on a
dot product between two vectors.  Since the program uses shared-memory,
the initialization is done entirely on the main thread of processor 1.
Then each of 4 threads in this example does a partial sum on the
vector elements it is responsible for, so all 4 threads are running
at the same time but operating on different parts of the data.
After they have all completed their parts, then the master thread
sums all for partial sums into the dot product and prints it out.

![Diagram of a shared-memory multi-threaded dot product](../fig/multi-threaded-dot-product-0.jpg )

### The multi-threaded dot product code

Let's go through the multi-threaded version of the dot product code
dot-product-threaded.py below to illustrate the changes that had to be
made to the code to parallelize it.
We should first note that we need to install the **pymp** package into
our virtual environment by doing **pip install pymp-pypi**.
Once that is installed, the **import pymp** line will bring those functions
into the code.

When we run the code we will want to set the number of threads for it to use.
In the code below, this is being set internally using the number of threads
passed in as a command line argument.
This is used to set the number of threads using the **pymp.config.num_threads**
variable.
The other method of setting the number of threads is to use the environmental
variable **PYMP_NUM_THREADS** externally.  For example, in your job script
you can have a line **export PYMP_NUM_THREADS=4** to tell the program to use
4 threads.

Right before the loop we must define our parallel environment with the
line **with pymp.Parallel( nthreads ) as p:** which
spins up the threads with the fork method.
Then the for loop range is changed so that each thread has a different range
for the elements of the loop that each thread is responsible for.

In the OpenMP multi-threading package used with C/C++ and Fortran, you
can use the same variable d_prod in the loop and just declare it as a 
variable to be used locally within each thread then globally summed at
the end, which is called a **reduction**.
This is very convenient and requires fewer changes to the code, but the
pymp package does not support this added function by choice opting for
the Python way of doing it more explicitly, so in our code we need
to create a shared array of partial sums and manually sum them together
at the end.  This is just as efficient computationally, it just takes a
little extra coding but is more explicit.

~~~
# Do the dot product between two vectors X and Y then print the result
# USAGE:  python dot_product_threaded.py 4       to run on 4 threads

import sys
import time
import pymp

   # Get and set nthreads from the command line

pymp.config.num_threads = int( sys.argv[1] )
nthreads = pymp.config.num_threads

N = 100000000      # Do a large enough test to reduce timing variance

x = [ float( i ) for i in range( N ) ]
y = [ float( i ) for i in range( N ) ]

   # Now initialize a very large dummy array to force X and Y out of all levels of cache
   #    so that our times are for pulling elements up from main memory.

dummy = [ 0.0 for i in range( 125000000 ) ]  # Initialize 1 GB of memory

   # Now we start our timer and do our calculation using multiple threads

t_start = time.perf_counter()

psum = pymp.shared.array( (nthreads,), dtype='float' )
for i in range( nthreads ):
   psum[i] = 0.0

d_prod = 0.0
with pymp.Parallel( nthreads ) as p:
   for i in p.range( N ):
      #d_prod += x[i] * y[i]
      psum[p.thread_num] += x[i] * y[i]

for i in range( nthreads ):     # Explicitly do the reduction across threads
   d_prod += psum[i]

t_elapsed = time.perf_counter() - t_start

   # The calculation is done and timer stopped so print out the answer

print('dot product = ', d_prod, 'took ', t_elapsed, ' seconds' );
print( 2.0*N*1.0e-9 / t_elapsed, ' Gflops (billion floating-point operations per second)')
print( 2.0*N*8.0/1.0e9, ' GB memory used' )
~~~
{: .language-python}

So parallelizing this program really only requires us to change around 11 lines
of code, and from that we get the benefit of being able to apply much greater
computing power.
We do have some control over how the parallelization works internally.
Using **p.range(N)** in our for loop will use static scheduling
where each thread is responsible for a pre-determined set of indices
at regular intervals as in the figure above.
If instead we use **p.xrange(N)** then dynamic scheduling will be used
where each index will be assigned to the next available thread.
This can be very useful if the amount of work in each pass through the loop
varies greatly.
Dynamic scheduling can produce much more effient results in cases where there
is a great load imbalance.

### Understanding what can cause inefficient scaling

A scaling study is designed to expose inefficiencies in a parallel code 
and to determine how many cores to use for a given problem size.
That last part is important to understand.
If there is too little work in each iteration of a loop, then loop overhead
can limit scaling.  Larger problem sets usually scale better.

A loop may be very scalable in itself, but if there is too much time spent
in the scalar part of the code like initialization, doing the reductions,
or doing input at the beginning and output at the end, then the entire code
may not scale well.
Load imbalance can also be a problem.
If the time it takes to pass through a loop varies, then using dynamic scheduling
is very important.

Shared arrays are an extremely important part of multi-threaded packages.
Since they do involve the copy-on-write mechanism, they can lead to 
inefficiency in the loop.
In general this is minimal but something to be aware of.

Multi-threading packages like OpenMP and pymp provide locking mechanisms and/or
methods of having part of a loop done by a single thread at a time.
This always leads to terrible scaling and should almost never be done.


> ## Scaling Study of the Multi-Threaded Dot Product Code
>
> Measure the execution time for the dot_product_threaded.py code
> for 1, 4, 8, and 16 cores.  If possible, use a job script
> requesting 16 cores and do all runs in the same job.
> Then calculate the speedup compared to the scalar (single-core)
> run to see how close to ideal the performance is.
>  > ## Solution and Analysis
> For this very simple problem, each thread can do its computations
> totally independently.  There is only a global sum of all the
> partial sums at the end, so we would expect the scaling to be
> close to ideal.
> In my measurements, I saw a 3.1x speedup on 4 cores, a 5.3x
> speedup on 8 cores, and a 7.8x speedup on 16 cores.
> For this problem, there just is so few computations being done
> in each loop iteration, only 2 floating-pont operations, that the
> loop overhead is preventing better scaling.
> A C version of this code using OpenMP for multi-threading runs
> 170 times faster, but likewise does not scale well due to the
> few computations being done in each pass through the loop.
> {: .solution}
{: .challenge}

::::::::::::::::::::::::::::::::::::: keypoints
- Multi-theaded computing is powerful and fairly easy to use but only works on one compute node.
- Understand key factors that can limit the efficient scaling of multi-threaded programs.
::::::::::::::::::::::::::::::::::::::::::::::::

### Links for additional information

* [github pymp](https://github.com/classner/pymp)
* [LLNL OpenMP tutorial](https://hpc-tutorials.llnl.gov/openmp/)
* [need link for mclapply()]()

