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