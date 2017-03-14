#!/usr/bin/env perl
#
# caldb_copy_run.pl
#
# $Id: caldb_copy_run.pl,v 2.14 2002/09/26 20:16:14 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

if ($nowrite) {
    print "info: nowrite specified, no linking will be done.\n";
}
if ($quiet) {
    $verbose = 0;
} else {
    $verbose = 1;
}
$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowSSI($dbh, $system, $subsystem, $item, \@itemArray);

if ($#itemArray == -1) {
    die "error: no items match system $system, subsystem $subsystem,"
	. " item $item\n";
}

# loop over all subsystems and items

for ($iitem = 0; $iitem <= $#itemArray; $iitem++) {
    $system = $itemArray[$iitem][0];
    $subsystem = $itemArray[$iitem][1];
    $item = $itemArray[$iitem][2];
    $itemId = $itemArray[$iitem][3];
    $comment = "Copied from run $runSource of $runIndexSource as of $time.";
    $status = ShowConstantSetId($dbh, $itemId, $runSource, $runIndexSource,
				$time, \$constantSetId);
    #print "debug: constantSetId = $constantSetId\n";
    if (!$constantSetId) {
	print "warning: no constants found linked to run $runSource"
	    . " for system $system, subsystem $subsystem, item $item"
		. " in $runIndexSource\n";
    } else {
	if ($nowrite) {
	    $phrase = "would have been";
	    $status = 0;
	    $runIndexId = "not assigned (nowrite enabled)";
	} else {
	    $phrase = "now are";
	    $status = LinkConstantSet($dbh, $system, $subsystem, $item,
				      $runIndexDest, $runMin, $runMax,
				      $constantSetId, $comment, \$runIndexId);
	}
	if ($status == 0) {
	    if (!$quiet) {
		print "info: in $runIndexDest, runs $runMin-$runMax"
		    . " for $system, $subsystem, $item "
			. $phrase
			    . " linked with constant set ID $constantSetId,"
				. " runIndexId is $runIndexId\n";
	    }
	} else {
	    print "warning: no assignment made for runs $runMin-$runMax"
		. " for $system, $subsystem, $item\n";
	}
    }
}

exit 0;

sub ProcessCommandLine {

    $database = "calib";
    $password="";
    
    if ($help) {
	&PrintUsage();
	exit 0;
    }
    if (!$system) {
	if ($s) {
	    $system = $s;
	} else {
	    print "info: system not specified, all systems will be copied\n";
	}
    }
    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	} else {
	    print "info: subsystem not specified,"
		. " all subsystems will be copied\n";
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	} else {
	    print "info: item not specified, all items will be copied\n";
	}
    }
    if (!$runSource) {
	if ($r) {
	    $runSource = $r;
	} else {
	    &PrintUsage();
	    die "error: runSource not defined\n";
	}
    }
    if (!$runMin) {
        if ($min) {
            $runMin = $min;
        } else {
            &PrintUsage();
            die "error: runMin not defined\n";
        }
    }
    if (!$runMax) {
        if ($max) {
            $runMax = $max;
        } else {
            &PrintUsage();
            die "error: runMax not defined\n";
        }
    }
    if ($runMax < $runMin) {
	die "error: $runMax < $runMin \n";
    }
    if (!$runIndexSource) {
	if ($ris) {
	    $runIndexSource = $ris;
	} else {
	    $runIndexSource = "RunIndex";
	}
    }
    if (!$runIndexDest) {
	if ($rid) {
	    $runIndexDest = $rid;
	} else {
            &PrintUsage();
            die "error: runIndexDest not defined\n";
	}
    }
    if ($runIndexDest =~ /^calib_user./) {
	$user = 'clasuser';
    } else {
	$user = $ENV{USER};
    }
    
    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
    if (!$time){
	if ($t) {
	    $time = $t;
	} else {
	    $time = '2037-1-1';
	}
    }
    if (!$quiet){
	if ($q) {
	    $quiet = $q;
	}
    }
}


sub PrintUsage {
    print <<EOM;
function: Links constants to a range of runs (runMin to runMax). Constants
          chosen are those assigned to the specified source run number.
note: use nowrite=1 to practice using this script
usage:
caldb_copy_run.pl  min=<run min> max=<run max> r=<runSource> \\
    rid=<destination run index table> \\
    [s=<system>] [ss=<subsystem>] [i=<item>] \\
    [ris=<source run index table>] \\
    [t=<time of validity>] \\
    [hostname=<hostname of db server>] [nowrite=<non-zero>] \\
    [help=<non-zero>] [q=<non-zero to suppress some info>]
 

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  r or runSource for reference run number
  min or runMin
  max or runMax
  ris or runIndexSource for source run index table name
  rid or runIndexDest for destination run index table name
  t or time for time of validity
  q or quiet for suppression of extra information

example:
caldb_copy_run.pl s=DC_DOCA ss=xvst_params i=SL1 ris=RunIndex \\
    rid=calib_user.RunIndexJunk r=28000 min=30000 max=40000
EOM
    return;
}

# end of perl script
