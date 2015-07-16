#!/bin/csh
### script to run a serial job using one core on htc using queue windfall or standard

### beginning of line, three pound/cross-hatch characters indicate comment
### beginning of line #PBS indicates an active PBS command/directive
### use ###PBS and #PBS to deactivate and activate (respectively PBS lines without removing them from script

### Refer to UA Batch System website for system and queue specific limits (max values)
### Minimize resource requests (ncpus, mem, walltime, cputime, etc) to minimize queue wait delays

### Set the job name
#PBS -N test_mkl

### Request email when job begins and ends
#PBS -m bea

### Specify email address to use for notification.
#PBS -M jterwin@email.arizona.edu

### Specify the PI group for this job
### List of PI groups available to each user can be found with "va" command
#PBS -W group_list=yelle

### Set the queue for this job as windfall or standard (adjust ### and #)
#PBS -q standard
###PBS -q windfall

### Set the jobtype for this job (serial, small_mpi, small_smp, large_mpi, large_smp)
### jobtype=serial submits to htc and can be automatically moved to cluster and smp
### Type parameter determines initial queue placement and possible automatic queue moves
#PBS -l jobtype=serial

### Set the number of cores (cpus) and memory that will be used for this job
### When specifying memory request slightly less than 2GB memory per ncpus for standard node
### Some memory needs to be reserved for the Linux system processes
#PBS -l select=1:ncpus=1:mem=1gb

### Important!!! Include this line for your 1p job.
### Without it, the entire node, containing 12 core, will be allocated
#PBS -l place=pack:shared

### Specify "wallclock time" required for this job, hhh:mm:ss
#PBS -l walltime=01:00:00

### Specify total cpu time required for this job, hhh:mm:ss
### total cputime = walltime * ncpus
#PBS -l cput=01:00:00

### Load required modules/libraries if needed (blas example)
### Use "module avail" command to list all available modules
### NOTE: /usr/share/Modules/init/csh -CAPITAL M in Modules
source /usr/share/Modules/init/csh
module load intel

### set directory for job execution, ~netid = home directory path
cd $PBS_O_WORKDIR
ls

### run your executable program with begin and end date and time output
date
/usr/bin/time ./tmkl.x
date
