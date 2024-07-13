---
title: "High-Throughput Computing"
teaching: 15
exercises: 0
questions:
- "What is the difference between high-throughput computing and cluster computing?"
objectives:
- "Learn what types of jobs run well on HTC systems"
- "Understand how to submit jobs using OSG"
keypoints:
- "Explore the basics of HTC computing"
---

## High-Throughput Computing

A typical HPC system today is a cluster computer made of one or
more head nodes and many compute nodes tied together with a common
file server and having a batch scheduler to decide where jobs run.
Some of the largest supercomputers in the world are similar conceptually
to a cluster computer but have more custom designed circuit boards as
nodes and may be packaged up and racked in a different manner than
cluster systems that use more off-the-shelf components.

A High-Throughput Computer (HTC) system is designed to run very
large numbers of small jobs and is often made up of many HPC
systems seperated geographically.
In the U.S. the main example is the Open Science Grid (OSG)
which is open for use to anyone associated with a U.S. university
or national laboratory.
Time is given away freely on a first-come first-served basis,
and computer resources are donated by various institutions.
Since HTC is geared toward running large numbers of smaller jobs,
the jobs fit well into the spare nooks and crannies of most
HPC systems and these systems are set up to kill any OSG job
when the host institute needs those resources back.
In this way the HTC jobs run invisibly on the over 100 HPC sites
spread across the U.S. offerring free computing to any who
need to run small jobs while greatly enhancing the usage
of each HPC system.


### Large numbers of small jobs

One of the first examples of HTC was the SETI program
where individual PC users could donate their CPU cycles
to the project to searching through large quantities of
data to try to find signals that might common from 
intelligent life.  This problem is ideal for HTC since
it can be broken up into large numbers of small jobs, and
any job that did not return an answer in a given period was
just thrown away and rerun on another system.

OSG has the ability to run a wide variety of jobs including
large memory and GPU runs, but it is much more difficult than
running the simple small jobs that it was designed for.
The OSG guidelines for the typical job are 
1-8 cores, upto 40 GB memory and 10 GB IO, and upto 20 hours run-time.

The most important aspect of working with HTC systems is that the jobs
be self-contained as much as possible, and be able to run on 
any OS and use mainstream modules.
This is essential since each job may be run on a wide range of
HPC environments.
Executables that are dynamically linked can work if you request the
matching resources by specifying things like acceptable CPU architectures.
Statically linked executables will always work.
Containers take more effort to set up but are ideal for HTC since
all executables, modules, and environmental variables are set 
within the container.
Running on multiple nodes using MPI is possible but difficult
usually requiring containers with the MPI connections specified
externally.


### Using OSG

Users may submit jobs to the OSG queue through their
institute's portal if one exists or through the
OSG Connect submission service at the link below.
If using the OSG Connect portal you will need to request
access and arrange for a short Zoom meeting with one
of their support staff.
There are links to the OSG Consortium and support documentation
at the end of this lesson.


### Working with the HTCondor scheduler

Submitting jobs to the HTCondor for scheduling on any of the 
hundreds of remote systems available is similar to using any
scheduler, except for the notes above on the job not relying on
modules and libraries that may not be available or labeled the same
everywhere.
Below is an example job script to run the stand-alone executable 
**namd2**.
Note that the X64_64 CPU architecture is specified.
The script requests 1 GB of disk for this job and directs
that all files from the directory **input_files** be transferred
along with the job at the start, and all files in **output** be
transferred back at the end.
The **queue 1** command then specifies the number of these jobs
to submit.  If multiple jobs are submitted then the environmental
variable **$(Process)** will be used in the script to differentiate each
with that being between zero and the number of jobs specified minus one.

~~~
#!/bin/bash -l
 
output = osg.namd.out
error = osg.namd.error
log = osg.namd.log
 
# Requested resources
request_cpus = 8
request_memory = 8 GB
request_disk = 1 GB
requirements = Arch == "X86_64" && HAS_MODULES == True
 
transfer_input_files = input_files/         # Slash means all files in that directory
executable = namd2
arguments = +p8 test.0.namd
transfer_output_files = output
queue 1
~~~
{: .language-bash}

Most systems will have support for modules, but the **HAS_MODULES == True** 
requirement can mean some systems are not supported.
Most systems use RHEL7, so specifying that may also rule out use
of RHEL6 and RHEL8 systems.  In general, if you use mainstream OS and
modules then you should be fine.  Otherwise you likely will need
to use a container.

Once you have the script you can submit, monitor, and
control the job using commands like those below.

~~~
> condor_submit htc_job.sh                   # Submit the condor script to the queue
> condor_q                                   # Check on the status while in the queue
> condor_q netid                             # Check status of currently running jobs
> condor_q 121763                            # Check status of a particular job
> condor_history 121763                      # Check status of a job that is completed
> condor_history -long 121763                # Same but report more info
> condor_rm 121763                           # Remove the job number specified
> condor_rm daveturner                       # Remove all jobs for the given username
~~~
{: .language-bash}



## HTC Outside the U.S.

Need to add info about non-US HTC systems


> ## Homework: Get an OSG account and submit a test job.
> If you are in the U.S. and want a big challenge, request an OSG account and
> try submitting some small jobs.
{: .challenge}

## Summary

High-Throughput Computing is free in the U.S. using the Open Science Grid.
It is great for running large numbers of small jobs.
Using it for GPU jobs or when large memory or IO is needed is possible
but much more challenging.
Aside from that it is similar to running jobs using any other scheduler.


### Links for additional information

* [OSG Consortium](https://osg-htc.org)
* [OSG Connect](https://connect.osg-htc.org)
* [OSG support home](https://support.opensciencegrid.org/support/home)
* [OSG typical jobs](https://osg-htc.org/services/open_science_pool.html)
* [OSG typical jobs](https://support.opensciencegrid.org/support/solutions/articles/5000632058-is-the-open-science-grid-for-you-)
* [HTCondor scheduler youtube video](https://www.youtube.com/watch?v=oMAvxsFJaw4)


{% include links.md %}

