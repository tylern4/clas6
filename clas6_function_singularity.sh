## To be sourced on the farm
source /etc/profile.d/modules.sh

# Clean out all old modules
module purge
# Load singularity
module load singularity

echoerr() { printf "%s\n" "$*" >&1; printf "%s\n" "$*" >&2; }

cp-to-job() {
    mkdir -p /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID}
    cp -r "$@" /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID}/
}

run-singularity-clas6() {
    if [ -z "$SLURM_JOB_ID" ]
    then
        singularity exec \
        -B /u/group:/group \
        -B /lustre:/lustre \
        -B /w/work:/work \
        -B /lustre/expphy/volatile:/volatile \
        -B /u/home:/home \
        -B $HOME/.recsis:/recsis \
        --pwd $PWD \
        /work/clas/clase1/tylern/clas6.img "$@"
    else
        singularity exec \
        -B /u/group:/group \
        -B /lustre:/lustre \
        -B /w/work:/work \
        -B /lustre/expphy/volatile:/volatile \
        -B /u/home:/home \
        -B $HOME/.recsis:/recsis \
        --pwd /scratch/${USER}/${SLURM_JOB_ID}/${SLURM_ARRAY_TASK_ID} \
        /work/clas/clase1/tylern/clas6.img "$@"
    fi
}


gsim_bat() {
    echoerr "============ gsim_bat ============"
    run-singularity-clas6 gsim_bat "$@"
    echoerr "============ gsim_bat ============"
}

gpp() {
    echoerr "============ gpp ============"
    run-singularity-clas6 gpp "$@"
    echoerr "============ gpp ============"
}


user_ana() {
    echoerr "============ user_ana ============"
    run-singularity-clas6 user_ana "$@"
    echoerr "============ user_ana ============"
}