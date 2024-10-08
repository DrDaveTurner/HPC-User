---
title: "Array Jobs"
teaching: 10
exercises: 5
---

:::::::::::::::::::::::::::::::::::::: questions

- What do array jobs have to do with high-performance computing?

::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives

- Learn what an array job is in a batch scheduler.
- Understand what types of science can make use of array jobs.

::::::::::::::::::::::::::::::::::::::::::::::::

### Array jobs using Slurm

An array job is mostly just like a normal job script in that it
has arguments at the top prefaced with **#SBATCH** that tell the
scheduler what resources are being requested followed by a list of
commands to be executed at run time.
The difference is that array jobs have an extra allocation request
line like **#SBATCH --array=1-5** that tells the Slurm scheduler
to launch in this case 5 individual jobs identical in every way
except that each will have a different value for the environmental 
variable **$SLURM_ARRAY_TASK_ID**.
This variable can be used to make each run unique as part of a series
of related runs.
It might be used as an input argument to the code being run in order
to let the application determine what is different with each run.
It may also be used to choose a different input file from a list
to use when running the application.
This is very flexible and is entirely up to the programmer to decide
how to use it.

For the job below the 
**$SLURM_ARRAY_TASK_ID** variable will be set to 1, 3, or 5.
since the script specifies that the task ID starts at 1,
ends at 5, and steps by 2.

```bash
#!/bin/bash -l
#SBATCH --job-name=array_test
#SBATCH --time=0-0:1:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=1G
#SBATCH --array=1-5:2

hostname
echo "Hello from array task ID $SLURM_ARRAY_TASK_ID"
```


:::::::::::::::::::::::::::::::::::::: challenge

## Submit an array job
If you have access to an HPC system with Slurm installed,
submit the **sb.array_test** script from the **scripts**
sub-directory using **sbatch sb.array_test** then look at the output.
If your HPC system has a different scheduler you may need to 
figure out how to submit an array job yourself and will need
to write the job script from scratch.
If you want more of a challenge 
you can try writing a script to choose an input filename from
a list using **$SLURM_ARRAY_TASK_ID**.

:::::::::::::::::: solution
This will run 3 individual jobs that will show the
**$SLURM_ARRAY_TASK_ID** to be 1, 3, or 5.
:::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::::::::::::::


### Array jobs as parallel computing

Most of the time when we thing of parallel computing we think
of taking a single program and making it run faster by applying
more compute power to it in terms of more compute cores.
Array jobs allow us to do the same thing, but in this case we are
running many individual programs instead of just one.

One common area of science where we can make use of this is
to do what is called a parameter sweep.
You may have a set of parameters such as system temperature,
pressure, atom type, and lattice type (atomic arrangement)
where you need to run the same code on all these different
input parameters.
Array jobs allow you to do parameter sweeps like this in a very
convenient manner with a single job script.
This makes it easy to submit and manage.

Another common use is in doing statistical science.
For applications that use a random number sequence,
you may want to run the same simulation many times
using a different seed to determine how the results
vary statistically as the random number sequence changes.

### Programming habits to avoid

Many programmers write scripts to submit lots of individual jobs instead
of making use of the array jobs functionality.
While the result is basically the same, this method should be 
avoided in general.
Lots of individual jobs can clog up the queue making it difficult
for users to see where other jobs are, and can also affect
scheduling since batch systems have limits on how deep they
can look.
Array jobs avoid both of these issues and make it much easier
to manage the resulting jobs since canceling your array job
is for instance just the same as canceling an individual job.

The testing cycle is always more important when you are dealing
with large numbers of jobs.
One user submitted an array job for tens of thousands of tasks
that had a typo in the email address so when it ran it
spammed the ticket system with tens of thousands of bounced emails.
Always start by running a few typical jobs to nail down your
resource requests.
Running a test job with just a few array IDs will allow you to 
ensure that each job is using the **$SLURM_ARRAY_TASK_ID**
in the desired manner.
Then when you are confident that your script is working as intended
you are ready to submit the full array job.

::::::::::::::::::::::::::::::::::::: keypoints
- Array jobs provide an easy way submit and manage large numbers of similar jobs.
- Array jobs are another way to do parallel computing, but by running
lots of small jobs individually.
- Test your script carefully on a few array IDs before submitting the full job.
::::::::::::::::::::::::::::::::::::::::::::::::

