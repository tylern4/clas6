#!/usr/local/bin/perl
# $Id : $
#        R A Thompson
#        University of Pittsburgh
#        Sept 22, 1998
#

# Purpose:  To convert some of the information in the offline database to data files
#           that could then be displayed by a plottting program.
# Input:    Selection criteria & a dbm database.
# Output:   An ascii data file.

# ------------ setup defaults -----------------
$UseEnergyCut = 0;
$UseFieldCut = 0;
$FieldRange = 100.0;        # in A
$EnergyRange = 0.2;         # in GeV
$Scan = 0;                  # scan mode is where you just print out contents of DB
$GoodRun = 0;
$PhotonBeam = 1;
$ElectronBeam = 0;
$BeamTypeCut = $ElectronBeam;

$ParmDir = $ENV{"CLAS_PARMS"};
$MapDir = "$ParmDir/Maps";
$Map = "$MapDir/RUN_CONTROL.map";

#----------------- parse command line -----------------
for (@ARGV) {
    if(/^-h/)      {
	&PrintUsage;
        exit;
    }
    if(/^-E(.+)/)       {                    # beam energy
	$UseEnergyCut = 1;
	$EnergyCut = $1;
    }      
    if(/^-T(.+)/) {                         # fraction of full torus field
	$UseFieldCut = 1;
	$FieldCut = $1;
    }   
    if(/^-e/) { $BeamTypeCut = $ElectronBeam}      # electron beam
    if(/^-g/) { $BeamTypeCut = $PhotonBeam};
    if(/^-S/) { $Scan = 1; }
    if(/^-r(.+)/) { $MinRun = $1;} 
    if(/^-R(.+)/) { $MaxRun = $1;} 
    if(/[^-]/)    { $entry = $_; }
}
#print "entry $entry\n";

#----------------- figure out run number ----------------- 
$RunNo = $entry;
$RunNo =~ s/^.*clas_//;            # get rid of prefix
$RunNo =~ s/\.A.+//;               # get rid of suffix
#print "run number: $RunNo\n";
if(($RunNo < $MinRun) || ($RunNo > $MaxRun)){
  exit;
}
#----------------- read Map -----------------
open(M,"get_map_float -m$Map -scurrents -itorus -t$RunNo -l1 |")  || die "Could not access the Map\n";
while (<M>) {
    chop;
    $Field = $_;
    #print"Field $Field\n";
}
open(M,"get_map_int -m$Map -sbeam -itype -t$RunNo -l1 |")  || die "Could not access the Map\n";
while (<M>) {
    chop;
    $BeamType = $_;
    #print "BeamType $BeamType\n";
}
open(M,"get_map_float -m$Map -sbeam -ienergy -t$RunNo -l1 |")  || die "Could not access the Map\n";
while (<M>) {
    chop;
    $Energy = $_;
    $Energy = $Energy/1000.0;
    #print "Energy $Energy\n";
}


#----------------- test Map parameters ----------------- 
if ($BeamType == $BeamTypeCut    ){ 
    $GoodRun = 1; 
}
else { 
    $GoodRun = 0; }

if ($UseFieldCut == 1) {
    if ($GoodRun == 1                       &&
	$Field  > ($FieldCut - $FieldRange) &&
	$Field  < ($FieldCut + $FieldRange)    ) {
	$GoodRun = 1;
    }
    else { $GoodRun = 0; }
}
if($UseEnergyCut == 1) {
    if (($GoodRun == 1)                         &&
	($Energy > ($EnergyCut - $EnergyRange)) && 
	($Energy < ($EnergyCut + $EnergyRange))    ) {
	$GoodRun = 1;
    }
    else { $GoodRun = 0; }
}

if($GoodRun == 1) { 
#  this is where you write the data file    
  print "Run: $RunNo\r";

  # open the output files
  open(SummaryFile, ">>Summary.dat") || die "Holy crap, couldn't open Summary.dat\n";
  open(DeadWires, ">>DeadWires.dat") || die "Holy crap, couldn't open DeadWires.dat\n";
  
  # initialize the dead wire counters.
  $DeadWires_S1 = 0;
  $DeadWires_S2 = 0;
  $DeadWires_S3 = 0;
  $DeadWires_S4 = 0;
  $DeadWires_S5 = 0;
  $DeadWires_S6 = 0;
    
  #----------------- read database -----------------
  if ($Scan==1) { print "database file: $entry\n"; }
  dbmopen(%ENTRY, $entry, undef);
  foreach $key (keys %ENTRY) {
    if ($Scan==1) { print "$key has a value of $ENTRY{$key}\n"; }
    if ($key eq "# of events written"){
      $eventswritten=$ENTRY{$key};
    }
    if ($key eq "# of events"){
      $eventsread=$ENTRY{$key};
    }
    if ($key eq "total charge"){
      $Charge=$ENTRY{$key};
      #print "Charge: $Charge\n";
    }
    if ($key eq "part_e"){
      $NumE=$ENTRY{$key};
    }
    if ($key eq "part_p"){
      $NumP=$ENTRY{$key};
    }
    if ($key eq "part_ep"){
      $NumEP=$ENTRY{$key};
    }
    # ----- get the pdu dead wires ------
    if (($key eq "pdu_s1r1_dead") || ($key eq "pdu_s1r2_dead") || ($key eq "pdu_s1r3_dead")){
      $DeadWires_S1 += $ENTRY{$key};
    }
    if (($key eq "pdu_s2r1_dead") || ($key eq "pdu_s2r2_dead") || ($key eq "pdu_s2r3_dead")){
      $DeadWires_S2 += $ENTRY{$key};
    }
    if (($key eq "pdu_s3r1_dead") || ($key eq "pdu_s3r2_dead") || ($key eq "pdu_s3r3_dead")){
      $DeadWires_S3 += $ENTRY{$key};
    }
    if (($key eq "pdu_s4r1_dead") || ($key eq "pdu_s4r2_dead") || ($key eq "pdu_s4r3_dead")){
      $DeadWires_S4 += $ENTRY{$key};
    }
    if (($key eq "pdu_s5r1_dead") || ($key eq "pdu_s5r2_dead") || ($key eq "pdu_s5r3_dead")){
      $DeadWires_S5 += $ENTRY{$key};
    }
    if (($key eq "pdu_s6r1_dead") || ($key eq "pdu_s6r2_dead") || ($key eq "pdu_s6r3_dead")){
      $DeadWires_S6 += $ENTRY{$key};
    }
  }
  if ($eventsread > 0){
    $ratio=($eventswritten/$eventsread);
  }
  else {
    $ratio = 0.0;
  }
  if ($Charge > 0.0){
    $FracE = $NumE/$Charge;
    $FracP = $NumP/$Charge;
    $FracEP = $NumEP/$Charge;
  }
  else {
    $FracE = 0.0;
    $FracP = 0.0;
    $FracEP = 0.0;
  }
  
  print SummaryFile "$RunNo   $FracE   $FracEP\n";
  print DeadWires "$RunNo  $DeadWires_S1  $DeadWires_S2  $DeadWires_S3  $DeadWires_S4  $DeadWires_S  $DeadWires_S5  $DeadWires_S6\n";

  #----------------- TESTS -----------------
  dbmclose(%ENTRY);
}
sub PrintUsage {
    print "usage: IsGoodRun [-options]
     -h       print this Help
     -E#      beam Energy (GeV) to match to within 0.2 GeV.
     -T#      Torus current (A) to match to within 100 A.
     -e       Electron beam (default)
     -g       photon beam (Gamma)
     -O#      minimum # of events written Out
     -I#      minimum # of events read In
     -F#      minimum Fraction of events written out
     -Q#      minimum integrated charge
     -S       Scan the database\n"
}
