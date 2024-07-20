---
title: "Introduction"
teaching: 5
exercises: 0
---

:::::::::::::::::::::::::::::::::::::: questions
- What should I expect to learn from these modules?
::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives
- Understand what to expect out of the HPC User lesson.
::::::::::::::::::::::::::::::::::::::::::::::::


**NOTE** - Examples are currently in Python.
There will eventually be the ability to choose which language the examples in
the concepts part of the lesson are shown in, 
plus at each example the user will be able to
click on a tab to see the examples in other languages.**

**Q?** - Do group-tags work across episodes or should I request global-tabs?

:::::::::::::::: group-tab

### Windows

Don't use CloudStrike

### MacOS

Doesn't use CloudStrike

### Linux

Doesn't use CloudStrike

::::::::::::::::::::

## Overview

In doing computational science it is very common to start a project by
writing code on a laptop or desktop computer.
Often as the project proceeds we find that we need more computer resources
to get all the science project done.
This may come in the form of needing more processing power to get a job
done in a reasonable time.
It may mean needing more memory to be able to run larger calculations.
Or it may just mean needing to do a very large number of smaller jobs
that would overwhelm a single computer.

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
interested in R, Matlab, or C/C++ too**.
As the lesson proceeds these same concepts will be used in different
ways and with examples in different computer languages to help drill them in.

The middle third of the lesson is a language survey.
Even though most scientists may work mainly in a single language,
it is important for everyone to understand the strengths and
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
- This lesson will help to understand basic concpets affecting performance in programming.
::::::::::::::::::::::::::::::::::::::::::::::::


