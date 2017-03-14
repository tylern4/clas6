#
# init file for RECSIS
#

#  load in  Tcl procedures, first check to see if the user is working
#  from the stand recsis_proc.tcl or has their own

if { [file exists ../tcl/recsis_proc.tcl] } {
    puts "Using local version of recsis_proc.tcl in ../tcl\n";
    source ../tcl/recsis_proc.tcl
} else {
    puts "Using standard version of recsis_proc.tcl in $env(RECSIS)/tcl\n";
    source $env(RECSIS)/tcl/recsis_proc.tcl
}

# define packages

turnon ALL
turnoff cc
# define input and output file

inputfile "$env(CLAS_DATA)/clas_sector6_000781.b00, \
           $env(CLAS_DATA)/clas_sector6_000781.b01"
#inputfile $env(CLAS_DATA)/clas000725.a00
#outputfile morejunk.evt

#set nevt_to_skip 100

# egn tcl parameters

set anal_opt 2;
set touch_id 2;

# drift chamber tcl parameters

set trk_patt(1) 4;

# tell FPACK not to stop if it thinks you are running out of time

fpack "timestop -999999999"

# do not send events to event display

set lscat $false;
set ldisplay_all $false;

# tell recsis to pause or go

pause

#  define the prompt for the user...they can change it later if 
#  they don't like it

setc rec_prompt "[exec whoami]_recsis> "

# turn off shared memory option, this can only be toggled at the beginning
# of a job....for now global section only works on LINUX so you should
# not try to invoke it.

global_section on

# define some new histograms, these are examples of a 1-dim, 2-dim, n-dim
# histograms defined interactively...to view them type "dump 200" any time
# during execution or try to use the share memory (global section) by
# using the preceding command.  You can define new histograms at any time
# during the execution, as long as the variable is in a BOS bank.

book 200 "sc 5 ADCL" 100 0. 5000.
fill 200  SC.5.ADCL

book 201 "sc 5 TDCL vs TDCR" 100 0. 5000. 100 0. 5000.
fill 201  SC.5.TDCL SC.5.TDCR

bookn 202 "SC bank" ID TDCL TDCR ADCL ADCR
filln 202 SC.5.ID SC.5.TDCL SC.5.TDCR SC.5.ADCL SC.5.ADCR

# that all there is to it....









