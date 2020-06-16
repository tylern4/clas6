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

set ltrk_do    -1;
set legn_do    -1;
set lcc_do     -1;
set ltof_do    -1;
set lst_do     0;
set ltime_do   0;
set ltagger_do 0;
set lec1_do 0;
set lusr0_do   0;
set lusr1_do   -1;
set lseb_do -1;

set ltrk_h_do    -1;
set legn_h_do    -1;
set lcc_h_do     -1;
set ltof_h_do    -1;
set lst_h_do     0;
set ltime_h_do   0;
set ltagger_h_do 0;
set lec1_h_do    0;
set lusr0_h_do   0;
set lusr1_h_do   0;
set lseb_h_do    -1;

inputfile $env(CLAS_DATA)/clas_sector6_000781.b01
outputfile /scratch/roudot/junk.evt

# clasbanks

#setc ddl_file ../bankdefs/clasbanks.ddl;

# Tracking tcl setting

#histo and ntuple level 3:raw hbt tbt 2:hbt tbt 1:hbt
set trk_ntuple 1
set trk_histo 1

#level of analysis 0: raw  2: hbt 4: tbt 
set trk_patt(1) 2

# tbt stuff realistic curve for drift time to drift distance.
set dc_xvst_choice 1

#for TBT test
#set trk_zcut(5) 50
#set trk_zcut(6) 30


# EGN Tcl setting (from stepan)

set lpart_do -1;
set part_his_out 0;
set m2_ecel_cut 1000.;
set m3_ecel_cut 10000.;
set trkec_match 5.;
set EcFitEdge_strip_threshold 0.001; 
set EcFitEdge_peak_threshold 0.003; 
set EcFitEdge_hit_threshold 0.010; 
#
set anal_opt 2;
set touch_id 2;
set def_geom -1;
set def_adc 0;
set def_atten 0;
set whole_surf 16.; 
set inner_surf 1.; 
set outer_surf 28.; 
#
set test_ec 0;
set ltest  0;
set test_sect 6;


# tell FPACK not to stop if it thinks you are running out of time

fpack "timestop -999999999"

# do not send events to event display
set lscat $false;
set ldisplay_all $false;

# tell recsis to pause or go
go 1

#  define the prompt for the user...they can change it later if 
#  they don't like it

setc rec_prompt "[exec whoami]_recsis> "

