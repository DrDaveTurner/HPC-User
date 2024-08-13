---
title: "HPC Resources"
teaching: 15
exercises: 0
---

:::::::::::::::::::::::::::::::::::::: questions

- How do I get access to HPC resources?

::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives

- Understand what HPC resources are available in the U.S.
- Learn how to access these resources for free.

::::::::::::::::::::::::::::::::::::::::::::::::


## Getting Access to HPC Resources

It is very typical to start a project on a personal
computer only to find that you need more performance either
as computing power or memory.
The next step may be to run your code on a more powerful
workstation such as a departmental server.
But when you still need more performance where do you go?
Most scientists do not understand that there are many options
for getting more powerful computing resources, and that they
are commonly free for the asking.

If you are in a university or laboratory environment the first
place to look is your local supercomputing center if you have one.
While many provide priority access to the compute resources to those
that provide research funds, most also have some means of providing
significant resources to users that can't provide financial support.
Many university cluster computers work on the **condo** model where
scientific groups can purchase compute nodes that they have full priority
on, but in return for the university managing those compute nodes
anyone else is allowed to use them when idle.  This pay for priority
model works well in sharing computing resources between the haves
and the have nots.

In many states one of the larger universities may provide free
access to scientists in smaller universities in the state.
Kansas State University for example provides free access to anyone
associated with any university in the state.  Many other states
provide funding for a single supercomputing center that all in
the state can use.

### Remote resources

There are many national supercomputing centers in the U.S.
that receive federal funding from the National Science Foundation (NSF)
and Department of Energy (DoE).
These supercomputers can cost $10-$100 million or above and have capacities
now over 1 exa-flops (billion billion floating-point operations per second).
When these are funded, part of the compute cycles are designated for
access to scientists not directly associated with those institutions.

Account and allocation requests as well as access is handled through
user portals.  Most of these remote systems have startup accounts
available that may typically be in the range of 5,000 core-hours
(5000 hours on 1 core or 500 hours on 10 cores for example).
Getting access is as easy as knowing where and how to ask.

#### The ACCESS portal to remote NSF supercomputers

The ACCESS portal is formerly known as XSEDE.
The link below is where to go to request an account and allocation,
submit a proposal, and find documentation and support.
There are a very wide variety of supercomputing systems available
to users associated with a university or laboratory.
Small startup allocations usually only take a few minutes to apply
for and are typically approved within a few days.

Many universities have a **Campus Champion** with a larger allocation
on many systems designed to be shared across a campus.
When these allocations run out, they can simply request more but
they are intended to get users needing long-term support the
experience on a system so that they can submit a full proposal.
While startup requests are typically on a specific system and
may be around 5,000 core-hours, campus champions may be granted
~5 times as much plus access to GPU and large memory nodes.
If you are on a campus with a Campus Champion then you just need
to apply for an account then contact them to get access to the
actual allocations.

Follow the ACCESS link at the end of this lesson for a complete
list of supercomputing resources, but below is an example of
some of the systems available (last updated August of 2022).


### Bridges2 at the Pittsburgh Supercomputing Center (PSC)

**488 RM Regular Memory compute nodes**

* 2 x AMD EPYC 7742 --> 128 cores
* 256 GB memory (16 more nodes have 512 GB each)
* 3.84 TB NVMe SSD
* Mellanox HDR 200 Gbps network

**4 EM Extreme Memory compute nodes**

* 4 x Intel Cascade 8260M --> 96 cores
* 4 TB memory
* 7.68 TB NVMe SSD
* Mellanox HDR 200 Gbps network

**24 GPU compute nodes**

* 2 x Intel Gold Cascade 6248 --> 40 cores
* 8 x NVIDIA Tesla v100 32 GB sxm2 GPU cards
* 512 GB memory
* 7.68 TB NVMe SSD
* Mellanox HDR 200 Gbps network


### Expanse at the San Diego Supercomputing Center (SDSC)

**728 compute nodes**

* 2 x AMD EPYC 7742 --> 128 cores
* 256 GB memory

**4 Large Memory nodes**

* 4 x Intel Cascade 8260M --> 96 cores
* 2 TB memory

**52 GPU nodes**

* 2 x Intel Xeon 6248 --> 40 cores
* 4 x NVIDIA Tesla v100 GPU cards
* 384 GB memory

**Cluster-wide capabilities**

* 12 PetaByte Lustre file system
* 7 PetaByte CEPH object store
* 56 Gbps bi-directional HDR InfiniBand network


## National Energy Research Scientific Computing Center (NERSC)

While allocations for the NSF supercomputing centers are managed
through the ACCESS website, external access to the Department
of Energy (DoE) supercomputers are managed through NERSC with the 
website link at the end of this module.
Many DoE supercomputers can be accessed by scientists in universities
and laboratories, but the process is more involved.
Access is still free, but you typically need to write a proposal
for significant node-hours and fully justify that the science
you intend to do is important and fits within the DoE mission,
and demonstrate that your code will use the requested resources
efficiently.


## Open Science Grid (OSG)

If you need to run many smaller jobs, then High-Throughput
Computing (HTC) covered in the previous chapter may be ideal
for you.  OSG access is fairly easy for users in the U.S.
and provides virtually unlimited access.

## National Research Platform (NRP)

The NRP is a partnership of over 50 institutions led
by UC San Diego supported in large part by NSF.
The Nautilus system is a HyperCluster for running containerized
big data applications using kubernetes for the container management.
This is a distributed set of compute nodes similar to OSG but applications
must specifically be self-contained and are guaranteed isolated access
to the resources allocated.

While kubernetes is more difficult for the average user, this system
provides access to much more powerful computing including very high-end GPUs.
The compute nodes are mainly GPU-based and research is aimed at
Machine Learning codes.


## Cloud Computing

Running HPC jobs in the cloud is much more difficult and costly
than many people understand.
Most cloud computing vendors do give small amounts of access away
for free on a trial basis for those that want to experiment.
The amount is very minimal in the context of running an HPC job.

As of August of 2024,
Google Cloud Platform (GCP) offered $300 credit to new customers for example.
Amazon Web Services (AWS) offered several free trial plans with $750 credits.
Microsoft Azure offers a $200 credit for 30 days for eligible new customers.


## Summary

Many projects that start on a laptop or desktop system end up needing
larger resources.
Transitioning to an HPC system can be challenging, but the best news
is that HPC resources are often free for the asking.
You just need to know where and how to ask, and hopefully
this module will give you ideas on where to start.

::::::::::::::::::::::::::::::::::::: keypoints
- There are many HPC resources that are totally free for the asking.
::::::::::::::::::::::::::::::::::::::::::::::::

### Links for additional information

* [ACCESS Portal](https://access-ci.org)
* [NERSC Portal](https://nersc.gov/)
* [OSG Consortium](https://osg-htc.org)
* [NRP National Research Platform](https://nationalresearchplatform.org)
* [AWS Cloud Services](https://aws.amazon.com/free/)
* [GCP Services](https://cloud.google.com/free)
* [Microsoft Azure](https://azure.microsoft.com/en-us/offers/ms-azr-0044p/)


