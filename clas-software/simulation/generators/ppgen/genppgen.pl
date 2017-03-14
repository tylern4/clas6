#! /user/bin/env perl

# This is a script to generate events using ppgen and taking three arguments. 
# The arguments are as follows:
# genppgen.pl factor flux_file out_directory
# Note: the "factor" is what you want to multiply the number of events in the 
#       flux file by, ex: .000000001 for large numbers of events

$percentage = $ARGV[0];
$flux_filename = $ARGV[1];
$out_directory = $ARGV[2];

open (FILE, $flux_filename);
while (<FILE>){
    chomp;
    ($photon, $nevents) = split(" ");
    $ngenerate = $nevents*$percentage;
    $rounded = sprintf "%.0f",$ngenerate;

    system("/group/clas/builds/test/trunk/build/bin/ppgen -M24 -t2.4 -j1 -P$photon -G -m$rounded > $out_directory/pkk.$photon.gamp");

    $total_events += $nevents;
    $total_generated += $rounded;

}
close (FILE);

print "Total Number of Events: $total_events\n";
print "Total Number Generated: $total_generated\n";

exit;

