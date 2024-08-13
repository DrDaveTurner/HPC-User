---
title: "Accelerating Scientific Computing with GPUs"
teaching: 15
exercises: 5
---

:::::::::::::::::::::::::::::::::::::: questions

- What differences are there in handling applications that can use a GPU?

::::::::::::::::::::::::::::::::::::::::::::::::

::::::::::::::::::::::::::::::::::::: objectives

- Learn a little about how GPUs can accelerate some scientific codes.
- Understand the basics of compiling and running GPU codes.

::::::::::::::::::::::::::::::::::::::::::::::::


## How to use a GPU to Accelerate Scientific Calculations?

We normally think of a Graphics Processing Unit or GPU in terms of
displaying graphics to a computer monitor.
GPU cards are designed with many streaming processors that are great
at performing lots of similar computations.
While this makes them ideal for doing graphics work, it also
makes them good for running some scientific codes.
GPU cards can accelerate some scientific codes by an order
of magnitude while costing much less than what the equivalent CPUs
would.

NVIDIA dominates the GPU accelerating market with both 
32-bit or consumer grade GPUs like the NVIDIA RTX 4090
that can cost around $1500, but they also produce 64-bit
Tesla cards that have no graphics ports at all and are 
only designed for scientific computing.  These include
the NVIDIA A100 which can cost as much as $11,000 each
or their newest H100 that can cost twice that.
Compute nodes can host 1-8 GPU cards each and in some
systems the high-end A100s and H100s may be linked together through
a fast NVLink connection.  Otherwise a multi-GPU run would
need to communication across the PCI bus.

AMD GPUs can also be used to accelerate scientific jobs
and are increasingly being found in the fastest supercomputers
in the world.
The code development is still behind that of the
NVIDIA CUDA community due to its much later entrance
into the scientific computing market.
AMD has a **Hip** compiler that is used to generate code
for the AMD GPUs, and it has the benefit of being able to
compile the same code for NVIDIA GPUs as well.
The AMD GPU line includes 32-bit GPUs such as the 
Radeon RX 6000 series and the 64-bit Radeon Instinct
MI100 cards with costs comparable to the NVIDIA line.


### Using GPUs for Machine Learning

Artificial Intelligence applications like Machine Learning can
be done on any CPU or GPU.
However, GPUs with tensor units can do them faster and less costly
since tensor units can do more operations per clock cycle, and the
lower precision of the results do not matter for Machine Learning
applications.
If your system is going to be primarily used for Machine Learning,
you'll want to look at the tensor units in the GPUs while the
memory will limit the size of the system you can work with.

AMD and Intel GPUs do not have tensor units.
NVIDIA offers tensor units in their 32-bit RTX GPUs 
providing an inexpensive means of accelerating machine learning algorithms.
The 32-bit NVIDIA RTX 3090 for example offers 328 Tensor cores 
along with 10496 CUDA cores with a maximum of 24 GB of memory.
The 64-bit NVIDIA A100 offers 432 Tensor cores along with 6912 CUDA cores
for high-end computing and 40 GB or 80 GB of memory.
These A100s can also be combined with the NVLink connection so that
an 8xA100 GPU cluster can look to the user like a single GPU
with 640 GB of memory.


### Profiling GPU code

It is more difficult to profile GPU programs since half of the
code is running on the CPU and half on the GPU.
From outside the program, if a user can ssh into the compute node
then running **nvidia-smi** provides a snapshot of the
GPU utilization and GPU memory usage.
In the job script the **nvprof** function can be used in place of the
**time** function to give profile information for various functions of
the job.


### Compiling and running GPU jobs

Depending on your system you may need to load a module to gain
access to the **nvcc** compiler for CUDA code.
**nvcc --help** can provide you with all the optional parameters,
but a basic compile is like **nvcc code.cu -o cuda_exec_name**.
You can then run the executable like any other except that there
must be a GPU present.
If you are on an HPC system with the Slurm scheduler, you can
request a single GPU by adding the **--gres=gpu:1** parameter.
You can also request a specific type of GPU, but this depends
on how each system is set up so you will need to refer to the
user documentations.  On our HPC cluster at Kansas State University
the request for one NVIDIA RTX 3090 would be
**--gres=gpu:geforce_rtx_3090:1**.

:::::::::::::::::::::::::::::::::::::: challenge

## Run a simple GPU job if you have access to an HPC system with GPUs
Compile and run the hello_from_gpu.cu CUDA program.
You may need to load a module to gain access to the **nvcc** compiler.
There is an **sb.hello_from_gpu** Slurm batch script to submit the job.

:::::::::::::::::: solution

The goal is just to start getting you comfortable with
compiling and submitting GPU jobs.
Use **nvcc hello_gpu.cu -o hello_from_gpu** to compile.
If you have Slurm on your HPC system you can submit the
job with **sbatch sb.hello_gpu** but you may need to add a partition.

:::::::::::::::::::::::::::
::::::::::::::::::::::::::::::::::::::::::::::::

## Summary

GPUs are more difficult to program, so for most scientists you will only
want to know how to run codes when GPUs are involved and not how
to program them yourself.
If you are fortunate enough to have an algorithm that someone has 
accelerated with a GPU, GPUs can often provide an order of magnitude
increase in speed over just using CPUs and greatly reduce the
cost of doing a calculation.

::::::::::::::::::::::::::::::::::::: keypoints
- Start to become comfortable with using GPUs on an HPC system.
::::::::::::::::::::::::::::::::::::::::::::::::

### Links for additional information

* [GPU Tensor Unit explanation](https://en.wikipedia.org/wiki/Tensor_Processing_Unit)
* [NVIDIA CUDA programming documentation](https://docs.nvidia.com/cuda/)
* [NVIDIA GPU specs](https://en.wikipedia.org/wiki/List_of_Nvidia_graphics_processing_units)
* [AMD Hip programming documentation](https://rocm.docs.amd.com/projects/HIP/en/latest/)
* [AMD GPU specs](https://en.wikipedia.org/wiki/List_of_AMD_graphics_processing_units)


