#!/usr/local/bin/perl
# $Id : $
#        R A Thompson
#        University of Pittsburgh
#        July 29, 1998
#

# ------------ setup defaults -----------------
$UseEnergyCut = 0;
$UseFieldCut = 0;
$FieldRange = 100.0;        # in A
$EnergyRange = 0.2;         # in GeV
$NumWrittenCut = 0;
$NumReadCut = 0;
$FracWrittenCut = 0.0;
$ChargeCut = 0.0;
$Scan = 0;
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
    if(/^-E(.+)/)      {                    # beam energy
	$UseEnergyCut = 1;
	$EnergyCut = $1;
    }      
    if(/^-T(.+)/) {                         # fraction of full torus field
	$UseFieldCut = 1;
	$FieldCut = $1;
    }   
    if(/^-e/) { $BeamTypeCut = $ElectronBeam}      # electron beam
    if(/^-g/) { $BeamTypeCut = $PhotonBeam};
    if(/^-O(.+)/) { $NumWrittenCut = $1;}   # number of events written
    if(/^-I(.+)/) { $NumReadCut = $1;}      # number of events processed
    if(/^-F(.+)/) { $FracWrittenCut = $1;}  # fraction of events written
    if(/^-Q(.+)/) { $ChargeCut = $1;}       # integrated charge
    if(/^-S/) { $Scan = 1; }
    if(/[^-]/)    { $entry = $_; }
}

#----------------- figure out run number ----------------- 
$RunNo = $entry;
$RunNo =~ s/^.*clas_//;            # get rid of prefix
$RunNo =~ s/\.A.+//;               # get rid of suffix

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
    }
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

#----------------- TESTS -----------------
if ($eventsread>0){
    #----------------- test database parameters -----------------
    $ratio=($eventswritten/$eventsread);
    if ($eventswritten >  $NumWrittenCut   &&
	$eventsread    >  $NumReadCut      &&
	$ratio         >= $FracWrittenCut  &&
	$Charge        >= $ChargeCut          ) { 
	$GoodRun = 1; }
    else { $GoodRun = 0; }
   #----------------- test Map parameters ----------------- 
    if ($GoodRun == 1             &&
	$BeamType == $BeamTypeCut    ){ 
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
} 

if($GoodRun == 1) { 
    $DataFile = $entry;
    $DataFile =~ s/^.*clas_//;
    printf"$DataFile\n"; 
}
dbmclose(%ENTRY);


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
