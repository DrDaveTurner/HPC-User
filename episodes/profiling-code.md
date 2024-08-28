---
title: "Profiling Code for Performance"
teaching: 20
exercises: 0
---

:::::::::::::::::::::::::::::::::::::: questions

- How to measure performance?
- How to measure memory usage?

::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives

- Learn the different methods available to measure time and memory usage
  externally and from within a program.
- Understand what parts of a program are important to time.
- Learn how to do a scaling study for multi-core and multi-node jobs.

::::::::::::::::::::::::::::::::::::::::::::::::


When we talk about the performance of a program, we are always interested
in how much time it takes to run, but in some cases we also need to know
how much memory the program uses if we are pushing the limits of our computer.
Even more than that, we often need to know how much time each part
of the code takes so that we know where to concentrate our efforts as
we try to improve the overall performance of the program.
So we need to be able to time the entire run, and also internally
each part of the code.
For parallel codes, we also need to know how efficiently they scale
as we increase the number of cores or compute nodes in order to determine
what resources to request.

## Timing a Program Externally

Linux has a **time** function that can proceed any command, so this
can be used to time the entire job externally even if we don't have
access to the source code.
This can be used to time a short test run in order to estimate the
runtime needed for the complete job.
When we get to talking about parallel computing, or using multiple
compute cores to run a single job, the **time** function will
prove useful for getting the execution time for a job as it
depends on the number of cores.
For example, it is common to test the same job on 1, 4, 8, 16,
and 32 cores to see how it scales with more cores to determine
what number of cores is most efficient to use.

Let's start with a few simple examples of using the **time** function
at the command prompt.
Try timing the **pwd** command which prints the current working directory.

```bash
time pwd
```
```output
/Users/daveturner

real	0m0.000s
user	0m0.000s
sys	0m0.000s
```

The first thing you see is the output of the **pwd** command, which in
this case is the directory **/Users/daveturner** on my Mac.
Then the **time** function prints its output, always as real time, which
is what we want, then user and system time which we can ignore.
This shows that the **pwd** command is faster than the clock can measure.
This is important to note that the **time** command isn't accurate to
less than 1 millisecond, so in general we should always make sure we are
measuring execution times that are greater than a second in general.

Below is another example where this time we are timing the **ls** function
which will list the files in the current directory.

```bash
time ls
```
```output
file1 file2 file3

real	0m0.110s
user	0m0.006s
sys	0m0.027s
```

When I do this on my Mac, I get a real time of just 0.006 seconds
because it's really fast to access the local hard disk.
The test above is from a very large cluster computer that has
a parallel file server with 1 Petabyte of storage 
(1 Petabyte is 1000 Terabytes, and each Terabyte is 1000 Gigabytes).
The performance for accessing large files on a parallel file server
is very good, but it does take longer to do small tasks like get the
contents of the current directory.
How does this compare to the system you are on?
On large HPC (High-Performance Computing) systems like this cluster,
the speed also depends on what file system you are testing.
You can usually access the local disk at /tmp, but your home
directory may be on another file system, and often there is
fast scratch space that is very high performance but only used
for programs when they are running.

Below we are going to use the **sleep** command to time an interval
of 5 seconds just to simulate what we might see in a real job.
In this case, even though the sleep function was supposed to go
5 seconds, there was some overhead or inaccuracy in the timnig 
routine or the length of the sleep.
This is one thing that you always need to be aware of when measuring
performance.
If you are measuring short things, you may want to measure multiple
times and take an average.

```bash
time sleep 5
```
```output
real	0m5.052s
user	0m0.001s
sys	0m0.003s
```

In addition to worrying about the clock accuracy, you also need to 
worry about interferrence from other jobs that may be running on
the same compute node you are on.
The best way to time a real job is to test it out on a completely
isolated computer.
If you are on an HPC system with a batch queue, you can always 
request an entire compute node and then just use part of the
node you requested, even a single compute core.
If this is not possible, then try to get the job as isolated
as you can.  Submitting a single-core request to a job queue
is one example of this, where you at least are sure that your
job is the only one on the cores that you requested.
Your job will still be sharing the memory bus and L3 cache
with other jobs.  Jobs usually don't effect each other much
from this, but they can.
If your job is using the network to communicate with other 
compute nodes, that might also be shared with other jobs running
on the same node.
The single largest factor to be aware of is that other jobs using
the same file server as you are can definitely affect the peroformance
of your job if your code is doing lots of IO (Input and Output).
On HPC systems, this can be true even if the other jobs are not on
the same compute node as your job.
If you want to isolate your job from others in this case, you will
need to do your IO to the local disk (usually /tmp).

## Timing Internal Parts of a Program

Every computer language has multiple clock functions that can
be used to time sections of the code.
The syntax is different in each language, but they all work about
the same, and there is always a very high precision function that
is accurate down to somewhere in the nanosecond range, though
I typically don't trust these for measuring less than a 
microsecond interval.
Each of these clock functions returns the current time, so 
to measure the time in an interval you need to store the
start time, do some calculations or IO, then get the end time
and subtract the two to get the time used for that part
of the code.

In Python since version 3.3, the best timer is in the **time** 
package.  To use it, you must start by importing the package.
In C/C++ the **clock_gettime()** function returns the current time
accurate to less than a microsecond. Accuracy of other timers like 
**SYSTEM CLOCK()** in Fortran 90 vary with the system and compiler.
Below are examples of using the clock functions to measure the
time it takes to do a loop, then measure the time it takes to
dump an array out to a file.  Some of these codes initialize a
dummy array to clear the values of **array** out of all levels
of cache before starting the timing.  This is just done because
we want the time to include retrieving the elements of **array**
from main memory and would not be done in a normal code.

:::::::::::::::: group-tab

### Python

```python
# timing_example.py - Example code showing how to put timing around an IO loop.

import time

N = 1000000
array = [ float(i) for i in range( N ) ]

t_start = time.perf_counter()

sum = 0.0
for i in range( N ):
   sum += array[i]

t_loop = time.perf_counter() - t_start
print("The loop took ", t_loop, " seconds")

t_start = time.perf_counter()

fd = open( "array.out", "w")
for i in range( N ):
   fd.write( str(array[i]) + "\n" )
fd.close()

t_output = time.perf_counter() - t_start
print("The output took ", t_output, " seconds")
```
### R

Not implemented yet.

### C

```c
// timing_example.c - Example code showing how to put timing around an IO loop

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

int main (int argc, char **argv)
{
   int i, N;
   double t_loop, t_sum, t_io, a_sum, *array;
   struct timespec ts, tf, ts_sum, tf_sum, ts_io, tf_io;
   FILE *fd;

      // Allocate space for the array and initialize it

   N = 1000000;
   array = malloc( N * sizeof(double) );

   for( i = 0; i < N; i++ ) {
      array[i] = (double) i;
   }

      // Put timing around our loop

   clock_gettime(CLOCK_REALTIME, &ts);

   a_sum = 0.0;
   for( i = 0; i < N; i++ ) {
      a_sum += array[i];
   }

   clock_gettime(CLOCK_REALTIME, &tf);
   t_loop =  (double) ( tf.tv_sec - ts.tv_sec );
   t_loop += (double) (tf.tv_nsec - ts.tv_nsec) * 1e-9;

   printf( "The loop took %lf seconds\n", t_loop );

      // Now time the file write

   clock_gettime(CLOCK_REALTIME, &ts_io);

   fd = fopen( "time_example.out", "w" );
   for( i = 0; i < N; i++ ) {
      fprintf( fd, "%lf\n", array[i] );
   }
   fclose( fd );

   clock_gettime(CLOCK_REALTIME, &tf_io);
   t_io =  (double) ( tf_io.tv_sec - ts_io.tv_sec );
   t_io += (double) (tf_io.tv_nsec - ts_io.tv_nsec) * 1e-9;

   printf( "The IO write took %lf seconds\n", t_io );
}
```

### Fortran

! timing_example.f90 - Example code showing how to put timing around an IO loop.

PROGRAM timing_example

   INTEGER :: i, N, c_start, c_stop, c_rate, cs_start, cs_stop
   INTEGER :: cio_start, cio_stop, fd
   DOUBLE PRECISION :: t_sum, t_loop, a_sum, t_io
   DOUBLE PRECISION, ALLOCATABLE :: array(:)
   DOUBLE PRECISION, ALLOCATABLE :: dummy(:)

      ! Allocate space for the array and initialize it

   N = 100000000;
   ALLOCATE( array(N) )

   DO i = 1, N
      array(i) = i
   END DO

      ! Initialize a dummy array to clear cache

   ALLOCATE( dummy(125000000) )
   DO i = 1, 125000000
      dummy(i) = 0.0
   END DO

      ! Put timing around our loop

   CALL SYSTEM_CLOCK( COUNT_RATE = c_rate )
   CALL SYSTEM_CLOCK( COUNT = c_start )

   a_sum = 0.0
   DO i = 1, N
      a_sum = a_sum + array(i)
   END DO

   CALL SYSTEM_CLOCK( COUNT = c_stop )
   t_loop = DBLE(c_stop - c_start) / c_rate

   WRITE(*,*) "a_sum = ", a_sum
   WRITE(*,*) "The loop took ", t_loop, " seconds "

!      Now time the file write

   CALL SYSTEM_CLOCK( COUNT = cio_start, COUNT_RATE = c_rate )

   open( fd, file = "timing_example.out" )
   DO i = 1, N
      write( fd, *) array(i)
   END DO

   close( fd )

   CALL SYSTEM_CLOCK( COUNT = cio_stop )
   t_io = DBLE(cio_stop - cio_start) / c_rate

   WRITE(*,*) "The IO write  took = ", t_io, " seconds "

END PROGRAM timing_example

### Matlab

Not implemented yet.

::::::::::::::::::::

Try running the **timing_example** code yourself for the language
you are working with.
These are codes you should have downloaded and unzipped
in your HPC system, and should be in the **code** directory.
Timing will be dependent on the language, but the values I see
are in the millisecond range.
Since both of these are above the nanosecond range, we can be confident
that the timing routine is accurately measuring each.

Let's see what we can learn by playing around with it some more.
When I run the python version preceeded by the linux **time** function, 
I see a real time significantly larger than the loop time and output time 
combined.
The initialization time is not measured but shouldn't be more than
the loop time.
What all this means is that there is some startup time for getting
the python program running and importing the **time** package, but
we may also be seeing the lack of accuracy of the external **time**
function when it comes to measuring things down in the millisecond range.
We do not see the same time discrepancy when running the C version.

Now lets change the program itself.
Sometimes we need to time a part of something that is in a larger
loop, so we need to sum the times together.
Try changing the timing so that it is inside the summation
loop instead of outside it to see what happens.
You can do this by uncommenting the timing and printing functions
in the code file.

:::::::::::::::: group-tab

### Python

```python
import time

N = 100
array = [ float(i) for i in range( N ) ]

t_start = time.perf_counter()

sum = 0.0
t_sum = 0.0
for i in range( N ):
   t0 = time.perf_counter()
   sum += array[i]
   t_sum += time.perf_counter() - t0

t_loop = time.perf_counter() - t_start

print("The sum took ", t_sum, " seconds")
print("The loop took ", t_loop, " seconds")

t_start = time.perf_counter()

fd = open( "array.out", "w")
for i in range( N ):
   fd.write( str(array[i]) + "\n" )
fd.close()

t_output = time.perf_counter() - t_start
print("The output took ", t_output, " seconds")
```
### R

Not implemented yet.

### C

```c
// timing_example.c - Example code showing how to put timing around an IO loop.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

int main (int argc, char **argv)
{
   int i, N;
   double t_loop, t_sum, t_io, a_sum, *array;
   struct timespec ts, tf, ts_sum, tf_sum, ts_io, tf_io;
   FILE *fd;

      // Allocate space for the array and initialize it

   N = 1000000;
   array = malloc( N * sizeof(double) );

   for( i = 0; i < N; i++ ) {
      array[i] = (double) i;
   }

      // Put timing around our loop

   clock_gettime(CLOCK_REALTIME, &ts);

   a_sum = 0.0;
   t_sum = 0.0;
   for( i = 0; i < N; i++ ) {
      clock_gettime(CLOCK_REALTIME, &ts_sum);
      a_sum += array[i];
      clock_gettime(CLOCK_REALTIME, &tf_sum);
      t_sum =  (double) ( tf_sum.tv_sec - ts_sum.tv_sec );
      t_sum += (double) (tf_sum.tv_nsec - ts_sum.tv_nsec) * 1e-9;
   }

   clock_gettime(CLOCK_REALTIME, &tf);
   t_loop =  (double) ( tf.tv_sec - ts.tv_sec );
   t_loop += (double) (tf.tv_nsec - ts.tv_nsec) * 1e-9;

   printf( "The sum took %lf seconds\n", t_sum );
   printf( "The loop took %lf seconds\n", t_loop );

      // Now time the file write

   clock_gettime(CLOCK_REALTIME, &ts_io);

   fd = fopen( "time_example.out", "w" );
   for( i = 0; i < N; i++ ) {
      fprintf( fd, "%lf\n", array[i] );
   }
   fclose( fd );

   clock_gettime(CLOCK_REALTIME, &tf_io);
   t_io =  (double) ( tf_io.tv_sec - ts_io.tv_sec );
   t_io += (double) (tf_io.tv_nsec - ts_io.tv_nsec) * 1e-9;

   printf( "The IO write took %lf seconds\n", t_io );
}
```

### Fortran

! timing_example.f90 - Example code showing how to put timing around an IO loop.

PROGRAM timing_example

   INTEGER :: i, N, c_start, c_stop, c_rate, cs_start, cs_stop
   INTEGER :: cio_start, cio_stop, fd
   DOUBLE PRECISION :: t_sum, t_loop, a_sum, t_io
   DOUBLE PRECISION, ALLOCATABLE :: array(:)
   DOUBLE PRECISION, ALLOCATABLE :: dummy(:)


      ! Allocate space for the array and initialize it

   N = 100000000;
   ALLOCATE( array(N) )

   DO i = 1, N
      array(i) = i
   END DO

      ! Initialize a dummy array to clear cache

   ALLOCATE( dummy(125000000) )
   DO i = 1, 125000000
      dummy(i) = 0.0
   END DO

      ! Put timing around our loop

   CALL SYSTEM_CLOCK( COUNT_RATE = c_rate )
   CALL SYSTEM_CLOCK( COUNT = c_start )

   t_sum = 0.0
   a_sum = 0.0
   DO i = 1, N
      CALL SYSTEM_CLOCK( COUNT = cs_start )
      a_sum = a_sum + array(i)
      CALL SYSTEM_CLOCK( COUNT = cs_stop )
      t_sum = t_sum + DBLE(cs_stop - cs_start) / c_rate
   END DO

   CALL SYSTEM_CLOCK( COUNT = c_stop )
   t_loop = DBLE(c_stop - c_start) / c_rate

   WRITE(*,*) "a_sum = ", a_sum
   WRITE(*,*) "The sum took ", t_sum, " seconds "
   WRITE(*,*) "The loop took ", t_loop, " seconds "

      ! Now time the file write

   CALL SYSTEM_CLOCK( COUNT = cio_start, COUNT_RATE = c_rate )

   open( fd, file = "timing_example.out" )
   DO i = 1, N
      write( fd, *) array(i)
   END DO

   close( fd )

   CALL SYSTEM_CLOCK( COUNT = cio_stop )
   t_io = DBLE(cio_stop - cio_start) / c_rate

   WRITE(*,*) "The IO write  took = ", t_io, " seconds "

END PROGRAM timing_example

### Matlab

Not implemented yet.

::::::::::::::::::::

In my computer, the **t_sum** time is only a bit larger than the 
**t_loop** time from before, but remember that this doesn't count
the loop overhead.
If we look at the **t_loop** time instead, in my computer it is more
than double what it was before.
When the clock routine is measuring very small intervals each time,
it can be intrusive in that it distorts the measurement by increasing
the runtime of the entire code.
It isn't surprising that this is intrusive since we are measuring the
time it takes to retrieve a single array element and do one addition.
The code is doing a subtraction and addition itself to calculate the
time interval, so it is probably more surprising that 
doing the timing in this way is not more intrusive.

## What to Time

The goal is to fully profile your code so that you understand
where all the time is being spent.
This means timnig each computational section where time is being
spent, usually the loops for example.
While simple print statements may not be important contributers to
the overall runtime of a code, any large input or output from files
may be.
When we start talking about parallel programs that use multiple
cores or even multiple compute nodes it will become important
to measure the time taken in communicating data to other cores
and other nodes.
Once you have a complete profile of where time is being spent,
then you can understand where to start in trying to optimize
your program to make it run faster.

## Measuring Parallel Job Scaling

When we get to talking about multi-processor jobs it will be very
important to understand how efficiently a job scales as we use
more processing cores.
For this we will do what is called a scaling study where we 
measure the performance of a typical job using different number
of processing cores.
We may for example run on 1 core, then 4, 8, 16, and 32 cores
to see how efficiently the job scales as we apply more processing
power.
This is done so that we can understand how many cores we can 
efficiently apply to that job.
If we get a 7 times speedup on 8 cores compared to 1, that less
than ideal but still very good.  If we then see a 9 times speedup
using 16 cores, then we'd probably want to stick with using
just 8 cores on that particular job. 
Do keep in mind that scaling is very dependent on the problem
size.
Larger problems will typically scale better to more cores, while
smaller problems may be limited to only a few cores.
The goal is to determine how many cores we can use with reasonable
efficiency.
The same kind of scaling study can also be used for multi-node
jobs, where we would test the performance on 1 node, 2, 4, and 8 nodes
for example.

Problem size is one factor that can affect the scaling efficiency.
For multi-node jobs, choosing the fastest networking options, and 
ensuring that all compute nodes are on the same network switch can
both increase the scaling efficiency.


## Tracking Memory Usage

When we think about high peformance, we mostly think about running
jobs faster.
For some programs, the memory usage may be the factor limiting what
types of science we can do.
At the very least, we need to know what memory to request when 
submitting jobs to a batch scheduler.

For simple jobs, like the matrix multiplication code in the next 
section, we can calculate the exact memory requirements.
If we are going to multiply two matrices of size NxN and put
the results in a third, then each matrix takes NxN x 8 Bytes if the
elements are 64-bit floats, so the total memory required would
be 3 x NxN * 8 Bytes.

For more complicated programs, often the best approach is to do
a short test run to measure the memory use before submitting the
full job.  This is especially true if you are submitting lots of
similar jobs.  If your job goes over the requested memory, it is
most often killed, so you want to over estimate the request 
somewhat, but if you request too much it can take a lot longer
to get your job scheduled and result in inefficient use of
the resources as you will be locking up memory that you are
not using.

Measuring the memory usage is more difficult than it should be
on most systems, and it depends on what tools you have available.
If you are on an HPC system with a batch scheduler like Slurm,
you can use the **sstat** command to find the current memory usage
for a running job and **seff** to find the maximum memory used
for a completed job, using the job ID number in both cases to choose
the job you are interested in.

```bash
seff 5072064
```
```output
Job ID: 5072064
Cluster: beocat
User/Group: daveturner/daveturner_users
State: FAILED (exit code 16)
Cores: 1
CPU Utilized: 00:01:07
CPU Efficiency: 97.10% of 00:01:09 core-walltime
Job Wall-clock time: 00:01:09
Memory Utilized: 2.26 GB
Memory Efficiency: 11.31% of 20.00 GB
```

The matrix multiplication job above used 10,000x10,000 matrices and
took 1 minute 9 seconds.
We see that in this case it used 2.26 GB of memory.
One thing to keep in mind is that 1 GB can be calculated in different
ways.  In this case, 1 kB is 1024 Bytes, 1 MB is 1024 x 1024 Bytes,
and 1 GB is 1024 x 1024 x 1024 Bytes.
So 3 matrices are 3 x NxN x 8 / (1024x1024x1024) GB = 2.235 GB, 
rounded up to 2.26 GB.
In general, if you estimate 1 GB as 10^9 Bytes that works fine.

```bash
sstat --format=MaxRSS 5072069
```
```output
MaxRSS
----------
  2289.50M
```

For a running job you can use the **sstat** command with the job ID number.
The **sstat** command will dump a lot of information out, so using
the **--format=MaxRSS** parameter provides just the real memory that we want.
In this case, it is again 2.29 GB.

Different HPC systems may have batch queue systems other than Slurm, but
all will have similar methods of getting the memory usage for running
and completed jobs.
If you are not running a job through a batch system, you can use
the **htop** command to find your process and look at the **Res** 
or resident memory.
This works best for single-core jobs as multi-core jobs may show memory
usage for each thread.

**Ganglia** is another option providing a web-based interface to look at
memory usage over time.
If you have access to Ganglia on your HPC system, it provides a node
view only rather than a job view, so it is only really useful if your
job is the only one running on a given compute node.
However, being able to see the memory use over time can be very helpful.


We will practice these approaches more in the upcoming modules.

::::::::::::::::::::::::::::::::::::: keypoints
- The **time** function can always be used externally to measure performance
  but has limitted accuracy of around 1 millisecond.
- Internally there are precise clock routines that can be used to measure 
  the performance of each part of a code.  These are different for each 
  programming language, but the use is always the same.
- Scaling studies can help determine how many cores or nodes we can efficiently
  use for a parallel job of a given problem size.
- Measuring memory use can be done from outside a program, but you may
  also be able to calculate total memory for simple programs.
::::::::::::::::::::::::::::::::::::::::::::::::

