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
- Understand the strengths and limitations of this approach.

::::::::::::::::::::::::::::::::::::::::::::::::


Most computer languages have the ability to do multi-threaded computing.
C/C++ and Fortran use the OpenMP package which is by far the most 
extensive and well developed.
It uses pragma statements to control the parallelization of loops so 
that multiple compute cores work at the same time on different parts
of the data.
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
make it more perform better.
Matlab also uses a different syntax in its Parallel Computing Toolbox
where it uses a **parfor** command to do a parallel **for** loop.

All of these methods behave basically the same by forking, or splitting off,
extra compute threads when called.
Each thread gets its own virtual memory space, meaning most large arrays are
not copied during the initialization stage.
If any thread writes to an array, only then is that array copied to that
thread's memory, and only the page of memory (4096 Bytes) that has been changed.
This is called a **copy-on-write** method and is handled by the operating
system.
Forking is very efficient in this way, only doing the work it needs.
For Python, this gets around the Global Interface Lock which is designed
to protect python objects.
Unfortunately the Windows operating system itself does not have support for 
the **fork** function
so you cannot run multi-threaded Python codes like **pymp** on Windows,
at least from the Power Shell.
However, if you have the Windows Subsystem for Linux (WSL) installed this
provides a robust Linux system that bypasses Windows and its limitations
so **pymp** codes can be run in this manner.

The figure below illustrates how multi-threading works on a
dot product between two vectors.  Since the program uses shared-memory,
the initialization is done entirely on the main thread of processor 1.
Then each of 4 threads in this example does a partial sum on the
vector elements it is responsible for, so all 4 threads are running
at the same time but operating on different parts of the data.
After they have all completed their parts of the computation, the master thread
sums all the partial sums into the dot product and prints it out.

![Diagram of a shared-memory multi-threaded dot product](fig/multi-threaded-dot-product-0.jpg ){alt="Shared-memory multi-threaded dot product showing the memory layout of both vectors"}

### The multi-threaded dot product code

:::::::::::::::: group-tab

### Python

Let's go through the multi-threaded version of the dot product code
below to illustrate the changes that had to be made to the code to parallelize it.
The **pymp** package needs to be installed into
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
**pymp** package does not support this added function by choice opting for
the Python way of doing it more explicitly, so in our code we need
to create a shared array of partial sums and manually sum them together
at the end.  This is just as efficient computationally, it just takes a
little extra coding but is more explicit.

```python
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
```

### R

Let's go through the multi-threaded version of the dot product code
below to illustrate the changes that had to be made to the code to parallelize it.
In R we need to define a virtual cluster that will be used to spread the work
from the **for** loops across the CPU cores. This can be done in several ways
in R and the choice should come down to what works best for the problem you
are coding up.

We start by loading the library **parallel** in the 
first example code below to pull in the detectCores(), makeCluster(),
clusterExport(), and clusterApply() functions.
We next detect the number of cores accessible to the job
then define the cluster with **makeCluster()** spawning independent worker processes
to handle parts of the parallel processing.
For this **parallel** library we need the body of the loop to be put into a
function and any variables that need to be used inside this function must 
be exported using **clusterExport()** commands.
The **clusterApply()** command uses the cluster object, iteration range, 
and function name which then spawns multiple processes to execute the
function for the iteration loop, automatically splitting them across the 
cores in the virtual cluster on a single compute node.
At the end there is a **stopCluster()** statement
that cleans up the virtual cluster before the program ends.

This basic approach is simple and can be useful but also may be inefficient since 
the overhead for dividing the work between threads may be much greater
than the work done within each iteration, as is clearly the case in
our simple example where there is only a single multiplication for each
pass through the loop.
In the second part of this code, the loop is instead
divided over the number of threads with the function then manually splitting
the loop iterations internally.  This greatly limits the assignment-of-work 
overhead since only the initial assignment is needed and you will see
for yourself that the resulting performance is enormously better.

```R
# Dot product in R using a loop and a vector summation
# USAGE:  Rscript dot_product_multi_thread.R 100000 8   for 100,000 elements on 8 cores

library( parallel )

   # Get the vector size and nThreads from the command line

args <- commandArgs(TRUE)
if( length( args ) == 2 ) {
   n <- as.integer( args[1] )
   nThreads <- as.integer( args[2] )
} else {
   n <- 100000
   nThreads <- detectCores()
}

cl <- makeCluster( nThreads )


   # Allocate space for and initialize the arrays

x <- vector( "double", n )
y <- vector( "double", n )

for( i in 1:n )
{
   x[i] <- as.double(i)
   y[i] <- as.double(3*i)
}

   # Export variables needed within the functions

clusterExport( cl, "x" )
clusterExport( cl, "y" )
clusterExport( cl, "n" )
clusterExport( cl, "nThreads" )

   # Time a multi-threaded dot product even though it's inefficient

dot_product_function <- function( i ) {

   return( x[i] * y[i] )

}

dummy <- matrix( 1:125000000 )       # Clear the cache buffers before timing

t_start <- proc.time()[[3]]

dot_product_list <- clusterApply( cl, 1:n, dot_product_function )
dot_product <- sum( unlist(dot_product_list) )

t_end <- proc.time()[[3]]

print(sprintf("Threaded dot product by clusterApply took %6.3f seconds", t_end-t_start))
print(sprintf("dot_product = %.6e on %i threads for vector size %i", dot_product, nThreads, n ) )


   # Now try dividing the iterations manually between workers

dot_product_workers <- function( myThread ) {

   mySum <- 0.0
   for( i in seq( myThread, n, by = nThreads ) )
   {
      mySum <- mySum + x[i] * y[i]
   }
   return( mySum )

}

dummy <- matrix( 1:125000000 )       # Clear the cache buffers before timing

t_start <- proc.time()[[3]]

dot_product_list <- clusterApply( cl, 1:nThreads, dot_product_workers )
dot_product <- sum( unlist(dot_product_list) )

t_end <- proc.time()[[3]]

print(sprintf("Threaded dot product with nThreads workers took %6.3f seconds", t_end-t_start))
print(sprintf("dot_product = %.6e on %i threads for vector size %i", dot_product, nThreads, n ) )

stopCluster( cl )
```

This second multi-threaded example below uses the **foreach** and **doParallel** 
libraries.  This code similarly defines and initiates a virtual
cluster.  The **foreach** loop is similar to a **for** loop but you can
choose between different back ends.  A **%do%** back end would run the body
in scalar, while the **%dopar** will split the iterations across the cores
of the virtual cluster, and we will discuss later that there is a
**%doMPI%** back end that can split the work across cores on different
compute nodes.
While similar to the previous example, the **foreach** approach is cleaner
programming in that you don't have to create a separate function for the
body of the loop.  You also don't need to manually export variables since
the processes that are spawned inherit the environment of the parent process.
So we get more flexibility in the back ends as well as a more convenient 
programming approach.
You'll be asked to measure the performance of each approach in the
exercise below.

```R
# Dot product in R using a loop and a vector summation
# USAGE:  Rscript dot_product_threaded_dopar.R 100000 8     for 100,000 elements on 8 threads

library( foreach )
library( iterators )
library( parallel )
library( doParallel )

   # Get the vector size and nThreads from the command line

args <- commandArgs(TRUE)
if( length( args ) == 2 ) {
   n <- as.integer( args[1] )
   nThreads <- as.integer( args[2] )
} else {
   n <- 100000
   nThreads <- detectCores()
}

   # Initialize the vectors and our virtual cluster

x <- vector( "double", n )
y <- vector( "double", n )

for( i in 1:n )
{
   x[i] <- as.double(i)
   y[i] <- as.double(3*i)
}

cl <- makeCluster( nThreads )
registerDoParallel( cl, nThreads )

   # Time the multi-threaded dot product foreach loop
   #   This returns a vector of size 'n' that will need to be summed
   #   so it is very inefficient.

dummy <- matrix( 1:125000000 )       # Clear the cache buffers before timing

t_start <- proc.time()[[3]]

#dot_product_vector <- foreach( i = 1:n, .combine = c, mc.preschedule = TRUE ) %dopar% {
dot_product_vector <- foreach( i = 1:n, .combine = c ) %dopar% {

   x[i] * y[i]

}
dot_product <- sum( dot_product_vector )

t_end <- proc.time()[[3]]

print(sprintf("dopar dot product took %6.3f seconds", t_end-t_start))
print(sprintf("dot_product = %.6e on %i threads for vector size %i", dot_product, nThreads, n ) )

   # Now let's try a more complex but more efficient method where
   #    we manually divide the work between the threads.

dummy <- matrix( 1:125000000 )       # Clear the cache buffers before timing

t_start <- proc.time()[[3]]

dot_product_vector <- foreach( myThread = 1:nThreads, .combine = c ) %dopar% {

   psum <- 0.0
   for( j in seq( myThread, n, nThreads ) ) {
      psum <- psum + x[j] * y[j]
   }
   psum

}
dot_product <- sum( dot_product_vector )

t_end <- proc.time()[[3]]

print(sprintf("dopar dot product with nThreads workers took %6.3f seconds", t_end-t_start))
print(sprintf("dot_product = %.6e on %i threads for vector size %i", dot_product, nThreads, n ) )

stopCluster( cl )
```

### C

Let's go through the multi-threaded version of the dot product code
below to illustrate the changes that had to be made to the code to parallelize it.
In C/C++ we need a line **#include <omp.h>** to bring the OpenMP
headers, then the code needs to be compiled with the 
**-fopenmp** flag for **gcc** or the **-qopenmp** flag for **icc**.

When we run the code we will want to set the number of threads for it to use.
In the code below, this is being set internally using the number of threads
passed in as a command line argument.
This is used to set the number of threads using 
the **omp_set_num_threads()** function in C/C++.
The other method of setting the number of threads is to use the environmental
variable **OMP_NUM_THREADS** for C/C++/Fortran.  For example, in your job script
you can have a line **export OMP_NUM_THREADS=4** to tell the program to use
4 threads.

Right before the loop we must tell the compiler to parallelize the loop
using a **#pragma omp parallel for** statement, plus we need to indicate
that there is a summation reduction of **dprod** taking place where
the partial sums calculated by each thread get globally summed at the end.
Then the for loop range is changed so that each thread has a different range
for the elements of the loop that each thread is responsible for.

```c
// Dot product in C using OpenMP
// USAGE:  dot_product_openmp 4   to run with 4 cores

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <omp.h>

void main (int argc, char **argv)
{
   int i, N;
   double dprod, *X, *Y;
   double t_elapsed;
   struct timespec ts, tf;

      // Get the number of threads from the command line

   char *a = argv[1];
   int nthreads = atoi( a );

   N = 100000000;

      // Allocate space for the X and Y vectors

   X = malloc( N * sizeof(double) );
   Y = malloc( N * sizeof(double) );

      // Initialize the X and Y vectors

   for( i = 0; i < N; i++ ) {
      X[i] = (double) i;
      Y[i] = (double) i;
   }

      // Allocate and innitialize a dummy array to clear cache

   double *dummy = malloc( 125000000 * sizeof(double) );
   for( i = 0; i < 125000000; i++ ) { dummy[i] = 0.0; }


      // Now we start the timer and do our calculation

   clock_gettime(CLOCK_REALTIME, &ts);

   omp_set_num_threads( nthreads );

   dprod = 0.0;
#pragma omp parallel for reduction( +:dprod)
   for( i = 0; i < N; i++ ) {
      dprod += X[i] * Y[i];
   }

   clock_gettime(CLOCK_REALTIME, &tf);
   t_elapsed =  (double) ( tf.tv_sec - ts.tv_sec );
   t_elapsed += (double) (tf.tv_nsec - ts.tv_nsec) * 1e-9;

   printf("dot product = %e on %d threads took %lf seconds\n", dprod, nthreads, t_elapsed );
   printf("%lf Gflops (billion floating-point operations per second)\n",
          2.0*N*1.0e-9 / t_elapsed);
   printf( "%lf GB memory used\n", 2.0*N*8.0/1.0e9);
}
```

### Fortran

Let's go through the multi-threaded version of the dot product code
below to illustrate the changes that had to be made to the code to parallelize it.
In Fortran we need a line **USE omp_lib** to bring the OpenMP
functions, then the code needs to be compiled with the
**-fopenmp** flag for **gfortran** or the **-qopenmp** flag for **ifort**.

When we run the code we will want to set the number of threads for it to use.
In the code below, this is being set internally using the number of threads
passed in as a command line argument.
This is used to set the number of threads using
the **OMP_SET_NUM_THREADS()** subroutine in Fortran.
The other method of setting the number of threads is to use the environmental
variable **OMP_NUM_THREADS** for C/C++/Fortran.  For example, in your job script
you can have a line **export OMP_NUM_THREADS=4** to tell the program to use
4 threads.

Right before the loop we must tell the compiler to parallelize the loop
using a **!$OMP PARALLEL DO** statement, plus we need to indicate
that there is a summation reduction of **dprod** taking place where
the partial sums calculated by each thread get globally summed at the end.
Then the **DO** loop range is changed so that each thread has a different range
for the elements of the loop that each thread is responsible for.

```fortran
! Dot product in Fortran using OpenMP

PROGRAM dot_product_fortran_openmp
   USE omp_lib

   INTEGER :: i, n, nthreads
   CHARACTER(100) :: arg1

   DOUBLE PRECISION :: dprod, t_start, t_elapsed
   DOUBLE PRECISION, ALLOCATABLE :: x(:), y(:)
   DOUBLE PRECISION, ALLOCATABLE :: dummy(:)

      ! Dynamically allocate large arrays to avoid overflowing the stack

   n = 100000000
   ALLOCATE( x(n) )
   ALLOCATE( y(n) )
   ALLOCATE( dummy(125000000) )

      ! Set the number of threads from the command line argument

   CALL GET_COMMAND_ARGUMENT( 1, arg1 )
   READ( arg1, *) nthreads
   CALL OMP_SET_NUM_THREADS( nthreads )   ! Set the number of threads

      ! Initialize the vectors

   DO i = 1, n
      x(i) = i
      y(i) = i
   END DO

      ! Initialize a dummy array to clear cache

   DO i = 1, 125000000
      dummy(i) = 0.0
   END DO

      ! Now start the timer and do the calculations

t_start = OMP_GET_WTIME()

   dprod = 0.0
!$OMP PARALLEL DO REDUCTION(+:dprod)
   DO i = 1, n
      dprod = dprod + x(i) * y(i)
   END DO

t_elapsed = OMP_GET_WTIME() - t_start

   WRITE(*,*) "dot product = ", dprod, " took ", &
      t_elapsed, " seconds  on ", nthreads, " threads"

END PROGRAM dot_product_fortran_openmp

```

### Matlab

Not implemented yet.

::::::::::::::::::::::::::

So parallelizing this program really only requires us to change around 11 lines
of code, and from that we get the benefit of being able to apply much greater
computing power.
In Python for example we do have some control over how the parallelization works 
internally.  Using **p.range(N)** in our for loop will use static scheduling
where each thread is responsible for a pre-determined set of indices
at regular intervals as in the figure above.
If instead we use **p.xrange(N)** then dynamic scheduling will be used
where each index will be assigned to the next available thread.
This can be very useful if the amount of work in each pass through the loop
varies greatly.
Dynamic scheduling can produce much more efficient results in cases where there
is a great load imbalance.

### Understanding what can cause inefficient scaling

A scaling study is designed to expose inefficiencies in a parallel code 
and to determine how many cores to use for a given problem size.
That last part is important to understand.
If there is too little work in each iteration of a loop, then loop overhead
can limit scaling.
Calculations on larger data  sets usually scale better.

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

Multi-threading packages like **OpenMP** and **pymp** provide mechanisms
that force loops in the algorithm out of multi-threaded operation and back into 
single-threaded operation.
This always leads to terrible scaling and should almost never be used.

:::::::::::::::::::::::::::::::::::::: challenge

## Scaling Study of the Multi-Threaded Dot Product Code

:::::::::::::::: group-tab

### Python

Measure the execution time for the **dot_product_threaded.py** code
for 1, 4, 8, and 16 cores.  If possible, use a job script
requesting 16 cores and do all runs in the same job.
You can look at the job scripts like **sb.ddot_py** in the **code**
directory as an example but your job script will probably be
different.
Then calculate the speedup compared to the scalar (single-core)
run to see how close to ideal the performance is.

### R

Measure the performance of **dot_product_threaded.R** and
**dot_product_threaded_dopar.R** for a given number of threads
like 8 if you have that many cores available.  You should be able
to run both in a few minutes using 100,000 elements. 

If you have time, try running a scaling study using a job script
similar to **sb.ddot_R** in the **code** directory.
This will allow us to see how each code scales with the number of
cores used.
Then calculate the speedup compared to the scalar (single-core)
run to see how close to ideal the performance is.

### C

Measure the execution time for the **dot_product_c_openmp.c** code
for 1, 4, 8, and 16 cores.  If possible, use a job script
requesting 16 cores and do all runs in the same job.
You can look at the job scripts like **sb.ddot_c** in the **code**
directory as an example but your job script will probably be
different.
Then calculate the speedup compared to the scalar (single-core)
run to see how close to ideal the performance is.

### Fortran

Measure the execution time for the **dot_product_fortran_openmp.f90** code
for 1, 4, 8, and 16 cores.  If possible, use a job script
requesting 16 cores and do all runs in the same job.
You can look at the job scripts like **sb.ddot_c** in the **code**
directory as an example but your job script will probably be
different.
Then calculate the speedup compared to the scalar (single-core)
run to see how close to ideal the performance is.


### Matlab

Not implemented yet.

::::::::::::::::::::::::::


:::::::::::::::::: solution

:::::::::::::::: group-tab

### Python

For this very simple problem, each thread can do its computations
totally independently.  There is only a global sum of all the
partial sums at the end, so we would expect the scaling to be
close to ideal.
In my measurements, I saw a 3.1x speedup on 4 cores, a 5.3x
speedup on 8 cores, and a 7.8x speedup on 16 cores.
For this problem, there just are so few computations being done
in each loop iteration, only 2 floating-point operations, that the
loop overhead is preventing better scaling.
A C/C++ version of this code using OpenMP for multi-threading runs
170 times faster because it is a compiled language,
but likewise does not scale well due to the
few computations being done in each pass through the loop.

### R

For 8 cores on a modern Intel processor I get 4.7 seconds for the 
first loop and 33 ms for the second loop in **dot_product_threaded.R**.
Manually dividing the workload between our processes greatly reduces the
overhead compared to letting R handle it.  The extra programming we did
is an absolute necessity in this case since we don't have much work in
the body of the loop.  It may be less necessary in more realistic
applications but this illustrates that the difference in overhead is enormous.

For the **dot_product_threaded_dopar.R** code, I measure 18.6 seconds for the
first loop and 70 ms for the second, so again there is an enormous saving in
overhead by manually dividing the work among the threads to limit the 
overhead from R scheduling the iterations across the workers.

If you have time you can try increasing the workload greatly.
Comment out the first loop in each code since that would take too long,
then increase the number of elements by 100 to 10,000,000.
My scaling studies now show **dot_product_threaded.R** getting
a 3.65 times speedup on 4 cores, a 6.5 times speedup on 8 cores,
and a 9.3 times speedup on 16 cores.  These are now very reasonable.
However, for **dot_product_threaded_dopar.R** I still measure over
a second for a single core and the time increases as I add more cores,
so the overhead for this method is still dominating the computations
inside the loop.
You can also measure the performance difference using the matrix
multiplication codes if you wish.

The conclusion from all this is that while using a **foreach**
is simple and clean code, the **clusterApply()** approach or
**foreach** over the number of threads with manually splitting the
iterations internally provides much greater performance. If each 
iteration is doing enough calculations then the overhead may not
matter.

### C

For this very simple problem, each thread can do its computations
totally independently.  There is only a global sum of all the
partial sums at the end, so we would expect the scaling to be
close to ideal.
In my measurements, I saw a 2.3x speedup on 4 cores, a 3.3x
speedup on 8 cores, and a 3.9x speedup on 16 cores.
For this problem, there just are so few computations being done
in each loop iteration, only 2 floating-point operations, that the
loop overhead is preventing better scaling.

### Fortran

For this very simple problem, each thread can do its computations
totally independently.  There is only a global sum of all the
partial sums at the end, so we would expect the scaling to be
close to ideal.
In my measurements, I saw a 3.5x speedup on 4 cores, a 6.6x
speedup on 8 cores.

### Matlab

Not implemented yet.

::::::::::::::::::::::::::

:::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::::::::::::::


::::::::::::::::::::::::::::::::::::: keypoints
- Multi-threaded computing is powerful and fairly easy to use but only works on one compute node.
- Understand key factors that can limit the efficient scaling of multi-threaded programs.
::::::::::::::::::::::::::::::::::::::::::::::::

### Links for additional information

* [LLNL OpenMP tutorial](https://hpc-tutorials.llnl.gov/openmp/)
* [github pymp](https://github.com/classner/pymp)

