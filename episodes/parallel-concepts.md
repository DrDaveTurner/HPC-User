---
title: "Parallel Computing Concepts"
teaching: 20
exercises: 10
---

:::::::::::::::::::::::::::::::::::::: questions
- Now that we can profile programs to find where the time is being spent,
  how do we speed the code up?
- What is parallel computing, and what are the underlying concpets that
  make it work?
::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives
- Learn different approaches to speeding up code.
::::::::::::::::::::::::::::::::::::::::::::::::


In the last chapter we learned how to profile a program to understand
where the time is being spent.
In thhis section, we will talk more about how to improve the
performance of different parts of a program.
This may involve steering clear of inefficient methods in some
computer languages, minimizing the operation count, using optimized
libraries, and applying multiple cores or even multiple nodes to
speed up the computational part.
Computations can sometimes be sped up enormously using accelerators
like GPUs (Graphic Processing Units) as well.
Reducing or eliminating IO can sometimes help, but ensuring that the
IO is done without costly locks, and to a file server that is not
overly used, can often help performance greatly as well.

## Optimizing Scalar Code

Programming in languages like C/C++ and Fortran produces fairly
efficient code in general because these are compiled languages.
In other words, you need to compile the code before executing it,
and the compilers do very intricate optimizations to ensure the
resulting executable is highly efficient.
Interpretive languages like Python, R, and Matlab do some compilation
on the fly.  They are therefore much less optimized, but more convenient
to run.  We have already learned the importance of using optimized 
library routines whenever possible, but this is especially true
for the interpretive languages.
Some languages also have certain methods that are convenient, but
very inefficient, and what to avoid and how to get around them will
be discussed in a later chapter.

One thing that can help in understanding performance is to know
how much it costs to perform different common math functions.
We can express the speed of a code in GFlops, or Giga (Billion)
Floating-point operations per second.
A floating-point operation involves two operands that are 
typically 64-bit floats.
When counting the Flops, we ignore integer arithmatic and comparisons
since those are very fast in relation to the floating-point operations.
Below is a table of the Flop cost for each operaton.
This can be thought of as for example how many operations does it take
to do a cosine function, since the cosine is done using a math library.

| Function / Operation  | Floating-point opeations |
|---------- |------|
| *  +  -     |   1  |
|    /      |   4  |
| square root   |   4  |
| sin()/cos()/tan()   |   6  |
| exponent()     |  14  |

One example of how this knowledge can help is if you have a large
loop where you are dividing by 4.0.  Instead, you reduce the operation
count by 3 if you multiply by 0.25.  This is not needed in compiled
languages like C/C++ or Fortran since the compiler does this optimization
for you, but it can help in the interpretive languages.
I have also used trigonometric identities to reduce the number of
trig functions in a program since they are very costly at 6 operations
each.


## Parallelizing Code

It is always good to optimize the scalar code first, but if you still
need your code to run faster than one option is to use more processing
power.
This means using more than one
computational core with each working on a different part of the data.
The goal is to get a speedup of N times if you are using N cores,
though often parallel programs will fall short of this ideal.

There are two main types of parallel programming.
The first is multi-core or shared-memory programming.
This is the easiest approach, with a single program running
but for computationally demanding sections like loops multiple
threads are used with typically one thread on each computational core.
In this way, the scalar part of the code does not need to be alterred
and we only need to put parallelizing commands before each loop
to tell the computer how to divide the problem up.
It is called **shared-memory** because all threads operate
on the data that is shared in main memory.
This is the easiest approach, is often very efficient, but has
the limitation that it needs to work only on a single compute node.
Multi-threading in C/C++ and Fortran use the **OpenMP** package,
Python uses a simplified version of this called **pymp**, and R
allows for this through the **mclapply()** function.

If you need even more computational power, or need access to more
memory than is on a single node, then you need to use 
multi-node computing.  This is also referred to as 
distributed-memory computing since each thread in this case
is its own separate but identical program, again just operating
on a different part of the data.
Distributed-memory programs can be run on a single compute node
when needed, but are designed to run on very large numbers
of nodes at the same time.  Some programs have been run on
millions of compute nodes, or some of the largest HPC systems
in the world which may cost more than $100 million.
C/C++ and Fortran codes use MPI, the Message-Passing Interface,
launching all the copies of the program to the different nodes
using the **mpirun** command, then each node shakes hands with the
others with the **MPI_Init()** function.
Each thread or task will operate on a different part of the data,
and when data needs to be exchanged the programmer can use
MPI commands like **MPI_Send()** and **MPI_Recv()**
to pass blocks of data to other threads.
This is a very powerful way to program, but it is definitely 
much more complex too.
Python has the **mpi4py** package which is a stripped down version
of MPI, and unfortunately you cannot do multi-node computing
with R.


You will not be taught how to program in these parallel languages
in this course, but you will be shown how to recognize each type
of parallel appraoch and how to work with each efficiently.


## Parallel Computing Concepts

The syntax for doing parallel processing is different for multi-threaded
and multi-node programming, and also can vary for each language, but 
handling multiple threads at the same time always involves some of the
same basic underlying concepts.

### Locks in Programs and File Systems

A multi-threaded program uses shared-memory where many threads may
want to access the same data at the same time.
If they are all reading the data, this is not a problem.
However, if even one thread wants to change the data by writing
to it while other threads may be reading it, this can lead to 
uncertain results depending on which threads does its read or write
first.
This uncertainty must therefore always be avoided, and often it is
handled by locking memory when a write occurs so that only that one
thread has access at that time.

The same thing can happen in parallel file servers.
If multiple threads, or even multiple programs, are reading the
same file or different files in the same directory then everything
is fine.
However, if one of those is writing to a file then a parallel file
server will lock the entire directory down to prevent other threads
from reading until the write is completed.
This is something that every user needs to be aware of.
It only applies to parallel file servers, so local disk (/tmp)
has no problems with this since there is only one controller for
the disk, while a parallel file server has many controlling nodes.
Some scratch space also can handle multiple writes to the same directory
without locking.
Since this can have severe impacts on performance, it is always good to
ask your system administrator if you don't know.
Ways around this include switching to a different file system like
/tmp while running the job, or putting the files in different directories.

### Barriers

Since distributed-memory programs involve multiple copies of the same
code, we commonly need to ensure that all are at the same point in
the code.
MPI uses barriers for this, an **MPI_Barrier()** function to be exact.
When this is encountered, each task will stop and wait until all tasks
reach that point in the code, they will communicate this to each other,
then continue on.  A common example of why you would need this would be
in debugging an MPI code where you want to identify where the code may
be failing.  If one task gets ahead of the other and errors out, it
may be that the root task will be at a different place in the code and
report that line where the job failed.

### Forks

All multi-threaded packages use some sort of a fork function.
When a loop is encountered and the root thread needs to spin up
multiple threads to do the work, it does so by doing a **fork()**
which duplicates the variables in the root thread.
This is done virtually which may be a bit confusing.
If every piece of data was copied it would increase the memory
usage enormously for runs on large numbers of cores,
so only the pointers to the arrays are copied.
If the data is only being read then all threads can read from
the original array.
If any thread writes to part of an array, then a unique copy
of that part of the array is made for that thread only.
So the fork process manages the memory usage behind the scenes
to minimize the redundant storage of data, but ensures that 
there is no need for a memory lock when a thread writes to that
data by making a copy instead.

### Dependencies in Loops

All those mechanisms discussed above may be used in implementing
a parallel computing package.
As a user, what we really need to know is when can a section of
a program be parallelized.
If you look at the loops where the most computational time is being
spent, what you need to determine is whether each pass through the
loop is independent of the others, or whether each pass is dependent
on the results of the previous iteration.
If each pass through a loop can be done indepently of the others, then
we can do them at the same time.
This is a simple statement, but it does sometimes take more thinking
to understand if there are any dependencies involved.
If you have any doubt, try writing down all the variables that are
needed as input for each iteration of the loop, then see if any
of those change throughout the loop.

If you have a program with nested loops, you may need to analyze
each loop level to see if it is parallelizable.
Parallelizing the outer loop means that there will be more computations
for each thread or task, which is called being more coarse grained.
This can lead to much higher efficiencies, but it is not always possible.
Often it is the inner loop that is easiest to parallelize, and this is
most often the case with multi-threaded parallelism.


## Using Accelerators like GPUs (Graphical Processing Units)

Some programs can be sped up by using a GPU as a computational 
accelerator.
A 32-bit GPU is the same as you would buy for a high-end gaming computer,
and can cost $1000-$1500.
These are great for accelerating 32-bit codes like classical molecular
dynamics simulations, and have custom hardware that is great for 
training neural networks.
The more expensive 64-bit GPUs are never intended for graphics at all.
They are custom designed for accelerators even though they are still
called GPUs.

Writing a program to run on a GPU is very difficult.
For NVIDIA GPUs, you use a programming language called CUDA.
There are many fewer codes optimized for AMD GPUs at this point.
They are programmed with Hip which can be compiled to run on either
AMD or NVIDIA GPUs.

Running a job with a GPU accelerator is not that difficult.
If your application can make use of one or more GPUs, there
will be directions on how to specify the number of GPUs.
If you are on an HPC system, you can request the number and
type of GPUs you want for your job.

## Optimizing Input and Output

The first thing to understand about IO (Input and Output) is that it can 
make a big difference
as to what type of a file system you are reading or writing to.
Local disk (usually /tmp) is temporary storage and has size restrictions,
and it isn't as fast as a parallel file server that stripes data across many
disks at the same time, but it is still sometimes the best to use if others
are heavily using the main file server and slowing it down.
As good as parallel file severs are, they also commonly need to lock
a directory if more than one file is being written at the same time.
Any locking can slow the performance of a code down emensely and should
be avoided if at all possible.
Many HPC systems may have fast scratch space which is temporary storage
but very large in size.  This is designed for use when you are running
your job, and may also not suffer from the same locking issues that some
parallel file servers can.

On our HPC system at Kansas State University, our fast scratch is
about ten times as fast as the parallel file server system that our
home directories are on.
So you would think that all you have to do is use fast scratch all the
time to make your IO ten times faster.
It actually is the case if you are streaming data in, by which we mean
reading in data in large chunks that do not need to be converted.
Files with large strings like genetic information falls into this 
category since the strings can be hundreds or thousands of characters
long and the representation in the file is the same as in the program.
Reading in array of floats or integers from a binary file also can go
as fast as the file server allows since the elements are stored in
binary in both the file and the program.

The problem comes when we want to store numbers for example in a 
text file so we can see them ourselves.
When we write them, or read them, the process goes slow since we
have to convert each number from its binary representation into a
text string before reading or writing.
With IO, it is that conversion process which is slow, so it doesn't
matter how fast the underlying file server is in these cases.
So if you want to speed up IO, think about streaming data in binary
form if possible, and if so then choose the fastest file server 
available.


:::::::::::::::::::::::::::::::::::::: challenge

## Scaling Study of the Distributed-Memory Dot Product Code
Measure the entire runtime for the dot_product_mpi.py code
for 1, 4, 8, and 16 cores if you are on an HPC system with
at least 2 compute nodes. 
You can try different combinations of nodes and cores for
each if you would like to see the effects of the network
(for the 4 cores, try 2 nodes 2 cores vs 4 nodes 1 core).

:::::::::::::::::: solution

In this code we initialize the vectors locally so there
is no communication involved.
The only communication is the global sum at the end, so
we expect the scaling to be close to ideal.
In many practical MPI codes, we would need to read data 
into one of the ranks, divide the data up and send it
out to each other node.
Real MPI applications also usually require communication
mixed in with the computational part in order to get data
where it needs to be.
All this communication can slow down the job, and this
usually gets worse as you spread a job over more cores,
and especially over more nodes.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


::::::::::::::::::::::::::::::::::::: keypoints
- What techniques  can be used to speed up scalar code?
- How to improve input and output?
- Learn about the difference between multi-core and multi-node programs.
- Understand the fundamentals of locks, barriers, and forks.
- Practice doing a scaling study.
::::::::::::::::::::::::::::::::::::::::::::::::

