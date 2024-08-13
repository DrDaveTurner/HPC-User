---
title: "The R Language"
teaching: 30
exercises: 15
---

:::::::::::::::::::::::::::::::::::::: questions
- What are the strengths and weaknesses of the R programming language?
::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives
- Analyze the merits of the R Language.
- Learn what to avoid in R when performance is important.
- Learn a little about how to parallelize R code.
::::::::::::::::::::::::::::::::::::::::::::::::

 
## Programming in the R Language

R is a high level programming language with an extremely rich
set of internal functions and add-on packages for statistical
analysis and other scientific programming.
It is an interpretive language and therefore the raw performance
is not great.  While there are ways to write R code that performs
better, it does take more work than in other languages due to the
many performance pitfalls inherent in the language, and often you
have to sift through the the many external packages to find ones that
work well for your needs.
This section will try to
identify those pitfalls and present higher-performing alternatives.

R code is identified with the **.R** ending.
External add-on packages are installed into the user's base directory under a
sub-directory also named **R**.  These packages can be easily installed
from the **CRAN** mirrors using commands like
**install.packages("data.table")** then referenced in the code
with a similar statement like **library(data.table)**.
The wide range of scientific and code packages available in R
and their ease of installation and use are its real strengths.

R programs can be run through the Linux command line interface (CLI),
submitted to batch queues, or run interactively from the 
CLI or the popular graphical user interface Rstudio. 
The package Rshiny also makes it easy to build interactive web
applications from R.

R is like Python in that both are interpretive languages and
both can easily be used interactively which is helpful for minimizing
the code development cycle.  The programmer is constantly changing
the code and immediately seeing the results.  This strength in
easing the development cycle is also one source of its weaknesses
in performance as interpretive languages only execute a single
line of code at a time while compiled languages take whole
blocks of code like the bodies of loops and highly optimize them
before execution time, but this is the same trade-off that many languages
face.

You can run R codes on multiple cores, but the parallel capabilities
of R are much more limited than the compiled languages C/C++ and Fortran
and even compared to Python.
The basic parallel model for R is that loops whether from **lapply()**
or **foreach** return an object that is either a list or a 
data frame with one entry for the results from  each pass through the loop.
If your code uses the loops in this manner then parallelizing them is
relatively easy as you just need to choose a back-end package to use,
tell it how many cores you want to use, then change the 
**lapply()** to an **mclapply()** or the **%do%** in the **foreach**
to a **%%dopar%**.

However, it is very common in scientific codes to want to instead have
all parallel tasks operate on a shared-memory object like the resulting
matrix in our matrix multiplication example.
People in the R community often refer to back-end methods that use 
a **fork** as shared-memory since the data structures can be referenced
in place in a shared manner as long as they are not written to.
This is quite different than what the HPC community refers to as
shared-memory, where all threads can write to a common data structure
and it is up to the programmer to ensure that threads don't obstruct
each other.

Our matrix multiplication example illustrates this difference as
the source matrices A and B can be shared in R since they are read-only,
saving great time when a back-end that uses a virtual-memory fork is used,
but the elements of the result matrix C needs to be filled in by each thread.
In R this is very difficult while it is common place in the compiled
languages C/C++/Fortran as well as in Python.
I have not found any example code yet to do this in R though I have seen
references that it can be done with a **bigmemory** package and a lot
of jumping through hoops that goes well beyond the scope of this course.
There are also some methods of altering the default matrix multiplication
package used in the **%*%** operation but these are very operating-system
dependent.

There is extensive support for running C/C++/Fortran code across
multiple nodes using the message-passing interface MPI, and Python has
a stripped down version of this with the **mpi4py** package.
There is a package for R called Rmpi that provides wrappers around
some of the common MPI functions, but it was developed up until 2014 and
looks to have only been patched every year or two since then.
Under Windows you must compile with Microsoft MPI, and there are some
limitations in functionality.
The doMPI back-end to **foreach** runs
on this Rmpi package, but it is not clear that either work or work well
due to the lack of current support so neither can be recommended at this
point.

There is a newer package called **pdbMPI** where the *pbd* stands for
Programming with Big Data.  This is an interface to the Message-Passing
Initiative that is the foundation for distributed parallel computing
in the HPC community, and also is a dependency for the 
**pdbDMAT** and **kazaam**
packages for working with matrices across multiple compute nodes.
These packages are much more recently developed and while still having
0.x version numbers they are actively managed and being used on
large supercomputer systems.
These provide true interfaces to MPI functions, not a back-end to
**mclapply()** or **doParallel**, so they are more difficult to use
but also much more powerful.


## Performance Pitfalls in R

While all languages take effort to optimize and parallelize
when performance becomes important, with R it is often more
about what aspects of the language need to be avoided that
are inhibiting performance.


### Profiling function

As with most languages there are many methods that can
be used to time sections of code in order to understand
where time is being spent.
For R the best options are to bracket the code of interest with
calls like **t_start <- proc.time()[[3]]** and 
**t_end <- proc.time()[[3]]** then take the difference.
The **proc.time()** function will provide the best clock
available and the **[[3]]** part takes the elapsed or real
time that we want.
The **system.time()** function can also be used which returns
the time taken to execute the function put in the parentheses.
This uses the same **proc.time()** clock but may provide
a more convenient method in some cases.

  Another common approach that should be avoided is to use
the **Sys.time()** function.
This similarly reports the time between the bracketed code,
but it by default auto-adjusts the units to the length of the
interval.  So if your code takes 59 seconds it will report
59, but if the same code takes 60 seconds it will auto-adjust
to minutes and report 1 instead.  You can and always should
manually specify the units if you choose to use this function.


### Dataframes and the rbind() function

Dataframes are a very valuable and integral part of the R language.
The results from loops or functions are often returned in the form of dataframes,
and the input and output of data is built more on dumping out
whole dataframes to files than the line-by-line approaches that
other languages use.
Dataframes in R are designed internally to be very flexible to
enable all of this, but this same design choice makes them
extremely inefficient from a computational view when working
with larger data sets.

The best example of this is the **rbind()** function which is 
used to build a dataframe.
It is very common to build a row of data using **cbind()**then use **rbind()**
to add the row to the dataframe table, but internally R must allocate
an entirely new area of memory and copy all the existing data
over as well as the new data.
This is because R is a column-major language so elements
in a column are stored next to each other.
If R was row-major then other alternatives would be present
like having an array pointing to each row in memory.

Having to recopy the entire dataframe each time a row is added
is an enormous performance penalty.
I was approached with an R code and asked to optimize and parallelize
it since it was going to take a month of runtime to complete.
We generated a test case that took one hour, and after commenting
out only the **rbind()** function the calculations took only
5 seconds.  All the rest of the time was spent copying the
dataframe data to newly allocated memory each time a row was added to the bottom.

If the code is building the dataframe just to dump it out to a file,
then one option is to simply print each row to file.  R isn't really
designed as well for this so it isn't always optimal, but often is
a huge improvement over the **rbind()** inefficiency.

A better option is to use the **data.table** package which is a
drop in alternative to a dataframe.  It is not as easy to use,
but is immensely more efficient than a dataframe since you can
pre-allocate the structure and insert values rather than having
to constantly rebuild the dataframe structure.
There will be an exercise at the end of this section that will
allow you to see the difference in the code and measure the 
performance of each approach.


## Parallelizing R code

As mentioned above, when a loop is parallelizable it is conceptually fairly 
easy to accomplish this.  The **lapply()** function has a
multi-core version **mclapply()** that spawns multiple
threads on a single compute node.
The **foreach** command can be parallelized by changing
the **%do%** to **%dopar%**.
Both of these commands are part of the core **parallel** library
in R, but to make them work you need to decide which of the
many back-end library packages to use.

Considerations include whether you may need to run on Windows OS
which does not support the **fork()** function that is the more
efficient way to implement the needed functionality, whether you
may need to run a job across multiple compute nodes, and
whether you need to use shared-memory in your threads to enable
working on a common data set.
Packages based on the **fork()** mechanism use virtual memory
rather than redundantly copying all data structures needed.
This can be enormously more efficient when dealing with large
data structures as each thread only gets a copy of the pages
in memory that it needs to alter.
In our matrix multiplication example, that means that the
two matrices that only need to be read never get copied to
each thread, and the for matrix that is being calculated 
only the parts that each thread is modifying get copied.
For this reason it is always recommended to use a back-end
library based on **fork()**, but Windows does not support this
functionality so you may need to consider other options 
that fully copy all data structures at the start or even a
socket-based cluster if you think your code might need to run
on Windows.
Another option would be to install the Windows Subsystem for Linux
(WSL) which supports the **fork)** function.

The basic parallelization model for both approaches is the same, to 
have each pass through the loop executed on different cores with
one line of data being returned for each iteration in the form of
a list or data frame.
When the goal is to instead operate on a common data set such as in
our matrix multiplication example, then shared-memory is needed.
There are not very many back-end packages that support this approach
even though it is a very common need.
In our matrix multiplication example code we use 
the **mcparallel** back-end and the **bigmemory** package.
These are designed to work on matrices, but would not work if you
for example wanted all threads to work on a different data structure
like a shared-memory data.table.


### mclapply() pitfalls

The **mclapply()** function is fairly straight forward to use
since you mostly need to supply the number of cores through
the **mc.cores=** argument.
There are options to tune the way the parallelization is done.
The **mc.preschedule=True** argument is the default, and this
means that the number of iterations is divided among the available
cores at the start.  This is highly recommended since if this is
turned off the system will fork a new process for each iteration,
do the work, then collapse that fork.  This can be incredibly 
inefficient since it means copying data structures many times over
so it should in general be avoided.
If you do try this, make sure to check the performance for both options
as this choice can drastically effect the efficiency
of the resulting parallel implementation.


:::::::::::::::::::::::::::::::::::::: challenge

## Compare raw and optimized performance of matmult.R
Run matmult_loops.R, matmult_foreach.R, and matmult_builtin.R 
for matrix sizes 100 and 1000 to compare
the performance of a raw loop to the built-in matrix
multiplication function.  Also compare these numbers
to other languages.

:::::::::::::::::: solution

I measured 500 seconds for the raw loop in R and
0.10 seconds for the optimized built-in matrix multiplication function.
Be aware that the built-in function may use all the cores it
has access to, so this may not be a fair comparison unless you submit
a batch scheduler job with only 1 core allocated.
Python for comparison took 300 seconds for raw loops and 0.13 seconds
for the **numpy** optimized routine, but a larger run for 
size 10,000 had **numpy** at 46 seconds compared to **R** at 72 seconds.
So in general, R and Python are similar in speed for both raw and
optimized code, but Python is a little faster.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


:::::::::::::::::::::::::::::::::::::: challenge

## Test the scaling of the rbind() function

Profile the run time for using rbind() as the number of rows in the data frame increases.
Time runs of **rbind.R** for 10, 100, 1000, and 10000 rows.

:::::::::::::::::: solution

While rbind() is convenient and works well for small data frames,
the time to add rows begins to increase exponentially for data frames
around 10,000 rows.  I measured 1 second for 1000 rows, 48 seconds for 10,000
rows, and 5100 seconds for 100,000 rows.
**rbind()** works well for small data frames, but it is very inefficient
when you scale up to larger data sets of over 10,000 rows.
This is because R copies the entire data frame over each time it adds
a new row.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


:::::::::::::::::::::::::::::::::::::: challenge

## Investigate the data table performance.
Test the **datatable.R** code for 100, 1000, and 10000 rows and compare to the rbind() results.

:::::::::::::::::: solution

For 1,000 rows I measured 0.12 seconds for a data table set()
compared to 5.4 seconds for a data table assignment and 1.1 second
for a data frame rbind().
For 10,000 rows the performance really starts to differ with
0.53 seconds for a data table set() compared to 50 seconds for a data
table assignment and 48 seconds for a data frame rbind().
For a large test of 100,000 rows the data table set() still only
took 5 seconds while the data table assignment took 485 seconds and
the data frame rbind() took 5100 seconds, or 1000x longer.
This again shows that while data frames can be convenient, when 
you scale up to larger sizes you have to use data tables and the
set() function.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


:::::::::::::::::::::::::::::::::::::: challenge

## Optional Homework - Test the IO performance in R.

Test the **fread.R** code to see the speedup of the **fread()** function from the optimized **data.table** package compared to the standard **read.csv()**.
Time runs of **fread.R** for 10,000 rows, 100,000 rows, and 1,000,000 rows.

:::::::::::::::::: solution

For 10,000 rows I saw similar results for each function, but for 
100,000 rows **fread()** was 10 times faster than **read.csv()**
and for 1,000,000 rows it was 100 times faster.
This is another example illustrating when to avoid the core
R functionality and use the external add-on packages to achieve performance
in your code.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


:::::::::::::::::::::::::::::::::::::: challenge

## Advanced Homework - Use the **pdbMPI** package to code and run a parallel Hello World program
For those who want a challenge, follow the **pdbMPI** link at the
end of this lesson an write, run, and test the Hello World program.

:::::::::::::::::: solution

I would love to have a pdbMPI-based matrix multiply code available
for people to look at and test if anyone finds one.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::


## Summary

R is a very powerful language because of the enormous set
of statistical and external scientific libraries that have been 
developed for it.
It can however be difficult to program in since much effort 
involves programming in supplemental packages rather than the
core language.
Users need to therefore know not only the core R language, but
be familiar with which external programming packages to use
when more performance or flexibility is needed, and this can
be an ever changing target.

It is often very challenging to get good performance
out of R code.
Elements of the core language like dataframes have inherent
performance and scaling problems.
Parallelization seems as easy as registering the desired
number of cores and changing the **%do%** in a **foreach** loop
to **%dopar%**, but setting up writable shared-memory is difficult 
to impossible.
Each back-end package has different capabilities and efficiencies
so it can be difficult to decide which approach is best.
While it is possible to achieve good performance with R code,
much of the work involves programming around the built-in
capabilities using optimized add-on libraries, and you have
to understand which of the many packages to utilize.
It is hoped that this section can at least steer people 
in the correct direction with some of these performance oriented
issues.

::::::::::::::::::::::::::::::::::::: keypoints
- Learn about the characteristics of the R language.
::::::::::::::::::::::::::::::::::::::::::::::::

### Links for additional information

* [R documentation](https://www.r-project.org/other-docs.html)
* [Foreach function](https://cran.r-project.org/web/packages/foreach/vignettes/foreach.html)
* [mclapply function](https://www.rdocumentation.org/packages/parallel/versions/3.4.0/topics/mclapply)
* [data.table package](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html)
* [Programming with Big Data - MPI package](https://github.com/snoweye/pbdMPI)
* [pdbDMAT package](https://github.com/RBigData/pbdDMAT)
* [Rstudio](https://www.rstudio.com)
* [Rshiny](https://shiny.rstudio.com)

