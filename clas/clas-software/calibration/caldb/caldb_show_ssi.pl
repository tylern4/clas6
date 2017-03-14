#! /usr/bin/env perl

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
                          \$dbh);

ShowSSI($dbh, $system, $subsystem, $item, \@ssi_ray);

for $i (0..$#ssi_ray) {
    print "$ssi_ray[$i][0]";
    for $j (1..3) {
	print " $ssi_ray[$i][$j]";
    }
    print "\n";
}
exit 0;

sub ProcessCommandLine {

    $user = "clasuser";
    $password = "";
    $database = "calib";
                          
    if ($help) {
        &PrintUsage();  
        exit 0;
    }
    if (!$hostname){
        $hostname = "clasdb.jlab.org";
    }
    if (!$system) {
	if ($s) {$system = $s;}
    }
    if (!$subsystem) {
	if ($ss) {$subsystem = $ss;}
    }
    if (!$item) {
	if ($i) {$item = $i;}
    }
}

sub PrintUsage {
    print <<EOM;
purpose: show system/subsystem/item combinations and corresponding item ID's
usage:
caldb_show_ssi.pl [system|s=<system>] [subsystem|ss=<subsystem>] \\
    [item|i=<item>] [hostname=<hostname of db server>] [help=<non-zero>]

example:
caldb_show_ssi.pl s=DC_DOCA ss=t_max
EOM
    return;
}

# end of perl script
