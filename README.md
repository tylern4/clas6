
[![Build Status](https://travis-ci.com/tylern4/clas6.svg?token=Hour3TGGb984zn33pgvC&branch=master)](https://travis-ci.com/tylern4/clas6)

  * [Running Locally](#running-locally)

  * [Running at JLab](#using-the-container-on-the-farm)
  
  

## Using the container on the farm

I have a prebuilt singularity container on the farm. There is also a slurm script availible which goes through all the steps needed to run a simulation from generator to user\_ana.

To download the scripts needed to run the simulation chain on the farm you can download them with:

```bash
wget https://raw.githubusercontent.com/tylern4/clas6/master/clas6_function_singularity.sh
wget https://raw.githubusercontent.com/tylern4/clas6/master/clas6_sim.sh
```

To run on the farm copy the script below and modify to load in the your input files. Then to run using slurm:

```bash
sbatch clas6_sim.sh
```

## Demo

[![asciicast](https://asciinema.org/a/ZMqJwxOmJ5PmIhiR6ISAvPaAM.png)](https://asciinema.org/a/ZMqJwxOmJ5PmIhiR6ISAvPaAM)

## clas6_sim.sh

```
#!/bin/bash

#INPUT Sequences file

# Name the job
#SBATCH -J clas6_sim

# Ask for 1 core (n) on 1 node (N)
#SBATCH -n 1
#SBATCH -N 1

# Make sure account is clas for clas6 sim
#SBATCH --account=clas

# Add time constraint to 2 days
#SBATCH --time=2-00

# Add 2GB memory constraint
#SBATCH --mem=2048M

# Place logfiles in a directory
# This directory should be in the same folder as the sim.sh script
#SBATCH --output=logfiles/sim_%A_%a.out
#SBATCH --error=logfiles/sim_%A_%a.err

# To run multiple jobs I use an array
# This will run 2 jobs
#SBATCH --array=1-2

# Make a few environment varialbes for directories
export JOB_DIR=${PWD}
export SCRATCH=/scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID}

########=========== Load Singularity to Farm Node ===========########
source clas6_function_singularity.sh

########=========== Setup Scartch Folders ===========########
cd ${SCRATCH}

#######============ Copy in configuration files =====#######
#************************* Modify this to get your input files in ****************************
cp-to-job ${JOB_DIR}/aao_rad.inp
cp-to-job ${JOB_DIR}/gsim.inp
cp-to-job ${JOB_DIR}/user_ana.tcl
#************************* Modify this to get your input files in ****************************

# starttime to time job
STARTTIME=$(date +%s)

########=========== Run Generator ===========########
#************************* Modify this for the generator you want ****************************
run-singularity-clas6 aao_rad < aao_rad.inp
#************************* Modify this for the generator you want ****************************

########=========== Run gsim ===========########
gsim_bat -nomcdata -ffread gsim.inp -mcin aao_rad.evt -bosout gsim.bos

########=========== Run gpp ===========########
#************************* Modify this for gpp configuration ****************************
gpp -ouncooked.bos -a2.35 -b2.35 -c2.35 -f0.97 -P0x1b -R23500 gsim.bos
#************************* Modify this for gpp configurtaion ****************************

########=========== Run user_ana ===========########
user_ana -t user_ana.tcl

#************************* Modify this for your output file preferences ****************************
########=========== Run h10maker ===========########
run-singularity-clas6 h10maker -rpm cooked.bos all.root
########=========== Copy all the files to Work for output ===========########
cp -r ${SCRATCH}/all.root ${JOB_DIR}/outputs/sim_${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}.root
#************************* Modify this for your output file preferences ****************************

# endtime to time job
ENDTIME=$(date +%s)
# Print time for host in seconds
echo "Time for $HOSTNAME: $(($ENDTIME-$STARTTIME))"
```

## Running Locally

In order to run locally there are a few things you need to have ready.

  1) Download [docker]()
  
  2) A local copy of the parms folder \[`/group/clas/parms`\] [a](a-local-copy-of-the-parms-folder)
  
  3) A local copy of the clasdb.jlab.org [b](a-local-copy-of-the-clasdb.jlab.org)
  
### A local copy of the parms folder


### A local copy of the clasdb.jlab.org

```
docker run --name clasdb -p 3306:3306 -e MYSQL_USER=root -e MYSQL_ALLOW_EMPTY_PASSWORD=yes -d tylern4/clas6db:latest
```

### Running the clas6 container

```
docker run --link clasdb:clasdb -v/path/to/parms:/group/clas/parms -v$PWD:/work --rm -it tylern4/clas6:latest
```

