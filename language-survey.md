---
title: "Language Survey"
teaching: 5
exercises: 0
---

:::::::::::::::::::::::::::::::::::::: questions
- What are the strengths and weaknesses of each computer language?
::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives
- Understand the criteria we will use to evaluate each language.
::::::::::::::::::::::::::::::::::::::::::::::::


People choose a programming language for a given project for many reasons.
Sometimes they choose a language because it is the only one they know, or
because it is the only one their advisor knows.
Many languages are easier to learn and use, and some can be used interactively.

In the next few sections, we want to do a survey of some of the more
common languages being used in science so that we can compare and
contrast each.
When we first choose a language to use for a project, it is common to
only consider the capabilities of that language.
For example, R has great access to statistical analysis routines so
it is a great choice when those capabilities are needed.
R and Python both can be used interactively which appeals to many.
But when we start talking needing performance as well, then we have
to balance the capabilities that each language offers with the
need to get great performance.
This performance can come in the form of scalar or single-core 
performance, but also involves the ability to apply multiple cores
or multiple compute nodes.

Capability, ease of programming, performance, and parallelizability are all 
attributes that we will need to consider.
Capability refers to the routines each language has access to like
all the statistical functions in R, the wide variety of artificial
intelligence packages programmed in Python, and the mathematical
toolboxes in Matlab.
Usability means the ease of programming and the productivity of the
programmer.  A low level language like C is incredibly flexible and
efficient but is more difficult to program and debug so that program
development takes longer.
Performance is unimportant for simple calculations but everything
as we scale up to more complex and computationally costly runs.
This is why people may start a project in a less efficient language
and end up needing to switch languages when performance begins to
limit the science that can be done.
Parallelizability refers to how many compute cores we can apply
to a given job.  This again ultimately limits the size of the
science we can achieve.
We must understand how each language measures up for each of these
merits in order to choose an effective approach for each
project we are interested in.

* Capability - Access to the routines and data structures you need
* Usability - Ease of programming and productivity
* Performance - How fast is the final code going to run?
* Parallelizability - How many cores or compute nodes can be used?


Compute cycles on NERSC (National Energy Research Scientific Computing) 
supercomputers are dominated by the compiled languages C/C++ and Fortran.
Python is involved in one quarter of all jobs, but in a job control
role rather than a computational one.
When you run jobs on large $100 million supercomputers, you have to
choose your language for performance reasons even if that means 
putting extra effort into the programming.

In a university environment it is very common to have less
efficient languages supported for computations such as Python, R, and Matlab.
Even though these are far from efficient computationally, they
are typically easier to program and can provide greater functionality.
These factors are often more important in cases where you may have
a single programmer writing a custom code for a particular project.
So the choice of a language can depend on the circumstances which
may include factors like how long an application is expected to be
used versus how much effort it will take to be developed.

It is useful to know a little bit about each language
so you can decide which is best for a given project 
or even which languages you want to be proficient at for
your career.
The next sections will present the strengths and weaknesses of
many languages commonly used in scientific computing.
Some languages have common practices that are performance
bottlenecks that need to be avoided, so these will be discussed
and alternative approaches presented.

## Overview

* C/C++ and Python are row-major languages with arrays starting at 0.
* Fortran, R, and Matlab are column-major languages with arrays starting at 1.

* C/C++ and Fortran are compiled languages for high-performance.
* Python, R, and Matlab have some optimized libraries to help with performance.


::::::::::::::::::::::::::::::::::::: keypoints
- Performance is just one criteria we need to understand when choosing
   the best language for a given project.
::::::::::::::::::::::::::::::::::::::::::::::::

