#!/usr/bin/env perl
#
# Dumps calibration constants given system, subsystem, item, constant
# set id and (optionally) the run index table name.
#
# $Id: caldb_show_constants_id.pl,v 2.4 2001/06/15 20:01:19 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ReadConstantsByIndex($dbh, $system, $subsystem, $item, $constantSetId, \@itemvalue);

# display the results

$ncol = $#itemvalue; # index of last array item

$index = $itemvalue[0];
$author = $itemvalue[1];
$time = $itemvalue[2];
$source_min = $itemvalue[3];
$source_max = $itemvalue[4];
$comment = $itemvalue[5];

if (!$quiet) { 
    print "$index $author $time $source_min $source_max\n";
    print "$comment\n";
}

for ($i = 6; $i <= $ncol; $i++) {
    print "$itemvalue[$i]\n";
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
    if (!$system) {
	if ($s) {
	    $system = $s;
	} else {
	    &PrintUsage();
	    die "error: system not defined\n";
	}
    }
    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	} else {
	    &PrintUsage();
	    die "error: subsystem not defined\n";
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	} else {
	    &PrintUsage();
	    die "error: item not defined\n";
	}
    }
    if (!$constantSetId) {
	if ($id) {
	    $constantSetId = $id;
	} else {
	    &PrintUsage();
	    die "error: constantSetId not defined\n";
	}
    }
    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
    if (!$quiet) {
	if ($q) {
	    $quiet = $q;
	}
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_show_constants_id.pl s=<system> ss=<subsystem> i=<item> \\
    id=<constant set ID> [hostname=<hostname of db server>] \\
    [q=<non-zero to suppress some info>] [help=<non-zero>]

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  id or constantSetId for constant set ID
  q or quiet for quiet mode

example:
caldb_show_constants_id.pl s=DC_DOCA ss=t_max i=Sector6 id=253
EOM
    return;
}

# end of perl script
