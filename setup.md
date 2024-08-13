---
title: Setup
---

## Data Sets

<!--
cd episodes
zip -r code.zip code
cp code.zip data
-->
Download the [code examples zip file](data/code.zip) and unzip it to your Desktop.

## Software Setup

::::::::::::::::::::::::::::::::::::::: discussion

### Details

Your instructor should provide you with information on how to access a
Linux system or Supercomputer.
You will need to use the directions below to access that system using
a terminal window so you can use the command line interface.
You will also need to transfer the code files to the Linux system, again
by following the directions for your operating system below.
Once the code files are copied over, you may edit them on the Linux side with
**vim** or **nano**, or if you are more comfortable you may edit the code files on
your personal computer and transfer them to the Linux system instead.

:::::::::::::::::::::::::::::::::::::::::::::::::::

:::::::::::::::: tab

### Windows

You may use PuTTY to ssh into a system for which your instructor should provide
access information.  In the long run, the MobaXTerm app can be installed and used
if desired.  It provides the same terminal access as PuTTY but also provides a
convenient file transfer capability.  This is especially useful if you are more
comfortable editing your files on the Windows side then transferring them over
to the Linux system.

### MacOS

Use **Terminal.app** or **iTerm2.app**.  You will ssh into the Linux system for
which your instructor should provide access information.  You can edit your files on 
the Linux system with **vim** or **nano**, or if you have a different editor
on your Mac system you may edit the code files there and **scp** them
over to the Linux system each time.

### Linux

Use  a Terminal window and ssh into the Linux system for which your instructor 
should provide access information.  You can edit your files on 
the Linux HPC system with **vim** or **nano**, or if you have a different editor
on your Linux personal computer you may edit the code files there and **scp** them
over to the Linux system each time.

::::::::::::::::::::::::::

### Programming Language for Example Code

Your instructor will direct you to click on the tab of a particular
programming language below.  This will select the language for all
example codes and diagrams for the first few chapters of this workshop.
As you go through each lesson, you may always click on the other tabs
to see how each code is implemented in the different programming
languages.

**NOTE** - Examples are currently only available in Python.

:::::::::::::::: group-tab

### Python

Show program examples and diagrams in sections 2-6 in Python.
You will need to set up a virtual environment on the Linux system that
your instructor provides access to.  The generic directions below should
usually work.

```bash
mkdir -p ~/virtualenvs
cd ~/virtualenvs
python -m ve3nv --system-site-packages python-hpc-user
source ~/virtualenvs/python-hpc-user/bin/activate
pip install numpy
pip install time
```

### R

Not implemented yet.

### C

Not implemented yet.

### Fortran

Not implemented yet.

### Matlab

Not implemented yet.

::::::::::::::::::::::::::


