---
title: "Introduction"
teaching: 5
exercises: 0
---

:::::::::::::::::::::::::::::::::::::: questions
- What should I expect to learn from the HPC User module?
::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives
- Set the basis for learning about High-Performance Computing in Science.
::::::::::::::::::::::::::::::::::::::::::::::::


## Overview

In doing computational science it is very common to start a project by
writing code on a personal computer.
Often as the project proceeds we find that we need more computer resources
to complete the science project.
This may come in the form of needing more processing power 
to complete the research in a reasonable time.
It may also mean needing more memory to be able to run larger calculations.
Or it may just mean needing to do a very large number of smaller jobs
that would overwhelm a single computer system.

In these cases where we need to seek out more computational resources,
we also need to start understanding the performance aspects of our code.
More power is not always the answer, sometimes writing more efficient
code can get the job done equally as well.

This HPC User lesson is aimed at scientists who need to use computers
to do calculations, and not at computer scientists or computer engineers
who need to be experts at programming in a High-Performance Computing
environment.
This lesson will be aimed at giving an overview of performance concepts
to provide a general understanding of how to operate in an HPC environment.

## Organization

The first few chapters concentrate on discussing performance issues
at the conceptual level with practical examples.
**These examples are currently given in Python but it is intended to
eventually have the user and instructor choose the language that the examples display
in to make it more appropriate to teach this to groups primarily 
interested in R, C/C++, Fortran, or Matlab too**.
As the lesson proceeds these same concepts will be used in different
ways and with examples in different computer languages to help drill them in.

The middle third of the lesson is a language survey.
Even though most scientists may work primarily in a single language,
it is important to understand the strengths and
weaknesses of alternative languages as well as their own favorite.

The last sections provide overviews of some more advanced topics
like working with GPUs to accelerate scientific codes.
It may be that some of this will be skipped by your instructor
due to time limitations but it is good to have these available
for reference purposes.

There are hands-on exercises throughout the lesson where you will
be asked to apply some of what you have learned.
There are also optional homework assignments available for those
who want to challenge themselves outside of the workshop.

Most sections also have website links at the end which provide
a means to seek out more information.

::::::::::::::::::::::::::::::::::::: keypoints
- The HPC User lesson will help to understand basic concepts affecting 
performance in programming.
::::::::::::::::::::::::::::::::::::::::::::::::


