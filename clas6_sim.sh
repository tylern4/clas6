#!/bin/bash

#INPUT Sequences file

#SBATCH -J clas6_sim
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --output=logfiles/sim_%A_%a.out
#SBATCH --error=logfiles/sim_%A_%a.err

########=========== Setup Folder and Copy in Input Files ===========########
mkdir -p $HOME/.recsis
touch $HOME/.recsis/recseq.ini
mkdir -p /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID}
cd /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID}

#************************* Modify this to get your input files in ****************************
cp ${HOME}/${CODE}/aao_rad.inp .
cp ${HOME}/${CODE}/gsim.inp .
cp ${HOME}/${CODE}/user_ana.tcl .
#************************* Modify this to get your input files in ****************************

########=========== Run Generator ===========########
STARTTIME=$(date +%s)
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

ENDTIME=$(date +%s)
echo "Time for $HOSTNAME: $(($ENDTIME-$STARTTIME))"

########=========== Copy all the files to Work for output ===========########
cp -r /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID} /work/clas/clase1/${USER}/simulations
