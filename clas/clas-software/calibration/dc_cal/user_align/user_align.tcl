#
# init file for RECSIS
#


#  load in  Tcl procedures, first check to see if the user is working
#  from the stand recsis_proc.tcl or has their own

if { [file exists ../../tcl/recsis_proc.tcl] } {
    puts "Using local version of recsis_proc.tcl in ../../tcl\n";
    source ../../tcl/recsis_proc.tcl
} else {
    puts "Using standard version of recsis_proc.tcl in $env(RECSIS)/tcl\n";
    source $env(RECSIS)/tcl/recsis_proc.tcl
}

# define packages
 
turnoff ALL
global_section off


# set ltagger_do and lst_do = -1 for photon running
set ltagger_do  0; 
set lst_do      0;
set lrf_do     -1;
set lcall_do   -1;
set ltof_do    -1;
set lcc_do     -1;
set legn_do    -1;
set lec1_do    -1;
set llac_do    -1;
set ltrk_do    -1;
set lusr1_do   -1;
set lusr0_do   -1;
set lhbid_do   -1;
set lseb_do     0;
 
inputfile clas_008935.A00
setc chist_filename all_layer_fit.hbook
outputfile all_layer_fit.out
setc log_file_name all_layer_fit.log
#set trk_magtyp 5;
set trk_print(1) 1;
 
#level of analysis 0: raw  2: hbt 4: tbt 
set trk_level 4;
 
# tbt stuff realistic curve for drift time to drift distance.
set dc_xvst_choice 2;
 
#for ALIGNMENT, we want to be able to modify the SIGMA's for the track fitting
# by default, these are all = 1.
# Default running conditions
set dc_mult_Sigma(1) 1.
set dc_mult_Sigma(2) 1.
set dc_mult_Sigma(3) 1.
set dc_mult_Sigma(4) 1.
set dc_mult_Sigma(5) 1.
set dc_mult_Sigma(6) 1.
 
# Fit to AXIAL layers only strongly
#set dc_mult_Sigma(1) 100.
#set dc_mult_Sigma(2) 1.
#set dc_mult_Sigma(3) 1.
#set dc_mult_Sigma(4) 100.
#set dc_mult_Sigma(5) 1.
#set dc_mult_Sigma(6) 100.
 
# Fit to Region 1 Axial layers only strongly
#set dc_mult_Sigma(1) 100.
#set dc_mult_Sigma(2) 1.
#set dc_mult_Sigma(3) 4000.
#set dc_mult_Sigma(4) 4000.
#set dc_mult_Sigma(5) 7000.
#set dc_mult_Sigma(6) 7000.
 
# Fit to Region 1+2 Axial layers only strongly
#set dc_mult_Sigma(1) 100.
#set dc_mult_Sigma(2) 1.
#set dc_mult_Sigma(3) 1.
#set dc_mult_Sigma(4) 100.
#set dc_mult_Sigma(5) 7000.
#set dc_mult_Sigma(6) 7000.

# DO VERTEX-Constrained FIT -- empty target runs only!!!
#  align_vert_cut = 0 don't change target wall
#                   1 change target wall to either upstm or dwnstm,
#                     depending upon which has more tracks around. 
##set align_vert_cut 1;

#  trk_VXconstr = 0  no vertex constraint on track
#                 1 or 3   constraint due to thin beam size
#                 2 or 3   constraint due to thin target (transverse to beam)
#  
#set trk_VXconstr   2;

#  target_wall  =   length (thickness) of target along Z (beam 
#                   direction)
#
#set target_wall   0.5;
#
#  beam_spot =  NOT USED
#
#set beam_spot     0.5;

#
#  dstmwall , upstmwall  = Z(cm) location of the downstream and 
#                          upstream target walls
#                          determine location from MVRT banks beforehand
#set dstmwall  2.;
#set upstmwall -2.;

#  dstmlen, upstmlen  = distance in Z from the target-wall
#                       center a track's vertex can
#                       be in order to count as a track from that 
#                       wall. (cm)
#   which wall is determined by a VOTE of all the found tracks from the
#   previous running of tracking on this file.
#set dstmlen       0.5;
#set upstmlen      0.5;

#
#     be sure to set the TARGET POSITIONS appropriately!!!
#set TargetPos(1)   0.099920;
#set TargetPos(2)  -0.580300;

# tell FPACK not to stop if it thinks you are running out of time
fpack "timestop -999999999"
 
# do not send events to event display
set lscat $false;
set ldisplay_all $false;
 
# tell recsis to pause or go
#set nevt_to_skip 10000

go 300000

setc rec_prompt "align_recsis> "

exit_pend
