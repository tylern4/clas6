#!/usr/local/bin/perl
# $Id : $
#        R A Thompson
#        University of Pittsburgh
#        July 29, 1998
#

# ------------ setup defaults -----------------
$UseEnergyCut = 0;
$EnergyCut = 0.0;
$EnergyRange = 0.2;         # in GeV
$UseFieldCut = 0;
$FieldCut = 0.0;
$FieldRange = 100.0;        # in A

$NumWrittenCut = 0;
$NumReadCut = 0;
$FracWrittenCut = 0.0;

$ChargeCut = 0.0;
$Beam = "e";

$Pass = 1;

$GoodRun = 0;

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
    if(/^-e/) { $Beam = "e"}                # electron beam
    if(/^-g/) { $Beam = "g"};               # photon beam
    if(/^-O(.+)/) { $NumWrittenCut = $1;}   # number of events written
    if(/^-I(.+)/) { $NumReadCut = $1;}      # number of events processed
    if(/^-F(.+)/) { $FracWrittenCut = $1;}  # fraction of events written
    if(/^-Q(.+)/) { $ChargeCut = $1;}       # integrated charge
    if(/^-P(.+)/) { $Pass = $1;}            # analysis pass
}

print"Be patient.\n";

$Switches = "-$Beam -O$NumWrittenCut -I$NumReadCut -F$FracWrittenCut -Q$ChargeCut";
if($UseEnergyCut == 1 ){
    $Switches = "$Switches -E$EnergyCut";
}
if($UseFieldCut == 1 ){
    $Switches = "$Switches -T$FieldCut";
}
#print "switches $Switches\n";

$DBPath = "/group/clasdev/clas/prod-1-$Pass/database";
reset 'a-z';

open(F,"ls -1 $DBPath/clas*db |")|| die "Could not execute ls command\n";
while(<F>){
    chop;
    push(@files,($_));
#    print "file $_\n";
}
foreach $file (@files){
    $_=$file;
    s/\.db$/\ /;
#   print $_ , "\n";	
    $cmd="IsGoodRun.pl $Switches $_";
#    print "cmd $cmd\n";
    system($cmd);
}
print "\n";

sub PrintUsage {
    print "usage: GoodRuns [-options]
     -h       print this Help
     -E#      beam Energy (GeV) to match to within 0.2 GeV.
     -T#      Torus current (A) to match to within 100 A.
     -e       Electron beam (default)
     -g       photon beam (Gamma)
     -O#      minimum # of events written Out
     -I#      minimum # of events read In
     -F#      minimum Fraction of events written out
     -Q#      minimum integrated charge (microamps)
     -P#      analysis pass\n"
}
