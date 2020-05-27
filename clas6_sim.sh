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
# This will run 1000 jobs
#SBATCH --array=1-1000

########=========== Load Singularity to Farm Node ===========########
source /etc/profile.d/modules.sh
module load singularity/3.4.0

########=========== Setup Scartch Folders ===========########
mkdir -p $HOME/.recsis
touch $HOME/.recsis/recseq.ini
mkdir -p /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID}
cd /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID}

#************************* Modify this to get your input files in ****************************
cp ${HOME}/${CODE}/aao_rad.inp .
cp ${HOME}/${CODE}/gsim.inp .
cp ${HOME}/${CODE}/user_ana.tcl .
#************************* Modify this to get your input files in ****************************


STARTTIME=$(date +%s)

#************************* Modify this for the generator you want ****************************

########=========== Run Generator ===========########
echo "============ aao_rad ============"
singularity exec \
-B /u/group:/group \
-B /lustre:/lustre \
-B /w/work:/work \
-B /lustre/expphy/volatile:/volatile \
-B /u/home:/home \
--pwd /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID} \
/work/clas/clase1/tylern/clas6.img aao_rad < aao_rad.inp
echo "============ aao_rad ============"

#************************* Modify this for the generator you want ****************************

########=========== Run gsim ===========########
echo "============ gsim_bat ============"
singularity exec \
-B /u/group:/group \
-B /lustre:/lustre \
-B /w/work:/work \
-B /lustre/expphy/volatile:/volatile \
-B /u/home:/home \
--pwd /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID} \
/work/clas/clase1/tylern/clas6.img gsim_bat -nomcdata -ffread gsim.inp -mcin aao_rad.evt -bosout gsim.bos
cp gsim.bos gsim_no_gpp.bos
echo "============ gsim_bat ============"

########=========== Run gpp ===========########
echo "============ gpp ============"
singularity exec \
-B /u/group:/group \
-B /lustre:/lustre \
-B /w/work:/work \
-B /lustre/expphy/volatile:/volatile \
-B /u/home:/home \
--pwd /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID} \
/work/clas/clase1/tylern/clas6.img gpp -ouncooked.bos -a2.35 -b2.35 -c2.35 -f0.97 -P0x1b -R23500 gsim.bos
echo "============ gpp ============"

########=========== Run user_ana ===========########
echo "============ user_ana ============"
singularity exec \
-B /u/group:/group \
-B /lustre:/lustre \
-B /w/work:/work \
-B /lustre/expphy/volatile:/volatile \
-B /u/home:/home \
-B $HOME/.recsis:/recsis \
--pwd /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID} \
/work/clas/clase1/tylern/clas6.img user_ana -t user_ana.tcl
echo "============ user_ana ============"

#************************* Modify this for your output file preferences ****************************

########=========== Run h10maker ===========########
echo "============ h10maker ============"
singularity exec --pwd /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID} \
/work/clas/clase1/tylern/clas6.img h10maker -rpm cooked.bos all.root
echo "============ h10maker ============"

########=========== Copy all the files to Work for output ===========########
cp -r /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID}/all.root /work/clas/clase1/${USER}/simulations/sim_${SLURM_JOB_ID}_${SLURM_ARRAY_TASK_ID}.root

#************************* Modify this for your output file preferences ****************************

ENDTIME=$(date +%s)
echo "Time for $HOSTNAME: $(($ENDTIME-$STARTTIME))"
