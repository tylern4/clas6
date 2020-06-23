#!/usr/bin/env perl
#
# Copies effective run ranges from one run index table to
# another. Requires a specific choice of system, subsystem, item and
# run range. Run range begin must match the beginning of a current
# effective range. Same for run range end.
#
# 9/19/01 add runMin runMax to runranges for compatibility H.A.
#
# $Id: caldb_copy_ranges.pl,v 1.11 2007/03/16 21:42:19 hovanes Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

# set connection variables

$database = 'calib';
$password = '';

# connect to database

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);
if (!$nowrite && !$noprompt) {
    print "note: use nowrite=1 to practice using this script\n";
}
#
# print a summary of the requested actions
#
if ($allruns) {
    print "info: Copying all run ranges\n";
} else {
    print "info: Copying all run ranges from $runMin to $runMax\n";
}
print "info: on host $hostname\n";
#print "info: for system $system\n";
if ( $system eq 'allsystems' ) {
  print "info: all systems\n";
} else {
  print "info: for system $system\n";
}
if ($subsystem eq 'allsubsystems') {
    print "info: all subsystems\n";
} else {
    print "info: subsystem $subsystem\n";
}
if ($item eq 'allitems') {
    print "info: all items\n";
} else {
    print "info: item $item\n";
}
if ($runIndexSrc =~ /\./) {
    $ris_print = $runIndexSrc;
} else {
    $ris_print = 'calib.' . $runIndexSrc;
}
print "info: from run index table $ris_print\n";
if ($time eq '2037-01-01') {
    print "info: as of the present time\n";
} else {
    print "info: as of $time\n";
}
if ($runIndexDest =~ /\./) {
    $rid_print = $runIndexDest;
} else {
    $rid_print = 'calib.' . $runIndexDest;
}
print "info: to run index table $rid_print\n";
if ($nowrite) {
    print "info: writing is disabled\n";
} else {
    print "info: writing is enabled\n";
}
#
# If writing is enabled prompt for are-you-sure answer
#
if (!$nowrite && !$noprompt) {
    print "Are you sure you want to do this? (yes/no) ";
    $response = <>;
    chomp $response;
    if ($response ne 'yes') {
	print "Exiting per user request.\n";
	exit 0;
    }
}

if( $system eq 'allsystems' ) {
  $subsystemshow = '';
  $itemshow = '';
  $status = ShowSystems( $dbh, $subsystemshow, $itemshow, , \@system_ary );
} else {
  @system_ary = ();
  $system_ary[0] = $system;
}
foreach $is ( 0..$#system_ary ) {
  $system_this = $system_ary[$is];
  print "info: system = $system_this\n";
  if ($subsystem eq 'allsubsystems') {
    $itemshow = '';
    $status = ShowSubsystems($dbh, $system_this, $itemshow, \@subsystem_ary);
  } else {
    @subsystem_ary = ();
    $subsystem_ary[0] = $subsystem;
  }
  foreach $iss (0 .. $#subsystem_ary) {
    $subsystem_this = $subsystem_ary[$iss];
    print "info: subsystem = $subsystem_this\n";
    if ($item eq 'allitems') {
      $status = ShowItems($dbh, $system_this, $subsystem_this, \@item_ary);
    } else {
      @item_ary = ();
      $item_ary[0] = $item;
    }
    foreach $ii (0 .. $#item_ary) {
      $item_this = $item_ary[$ii];
      print "info: item = $item_this\n";
      CopyRangesItem($system_this, $subsystem_this, $item_this);
    }
  }
}

sub CopyRangesItem {

    my ($system, $subsystem, $item) = @_;

# get the run ranges from the source index

    $status = ShowRunRanges($dbh, $system, $subsystem, $item, $runIndexSrc,
			    $time, $runMin, $runMax, \@runRangeInfo);

# make the links in the destination run index for all run ranges in
# the source index that are in the specified range.

    foreach $ir (0 .. $#runRangeInfo) {
	$runMinThis = $runRangeInfo[$ir][0];
	if ($runMinThis < $runMin) {$runMinThis = $runMin;}
	$runMaxThis = $runRangeInfo[$ir][1];
	if ($runMaxThis > $runMax) {$runMaxThis = $runMax;}
	$itemValueId = $runRangeInfo[$ir][2];
	$comment = "Link copied from $runIndexSrc as of $time.";
	if (!$nowrite) {
	    $status = LinkConstantSet($dbh, $system, $subsystem, $item,
				      $runIndexDest, $runMinThis, $runMaxThis,
				      $itemValueId, $comment, \$runIndexId);
	    print "info: linking runs $runMinThis-$runMaxThis"
		. " to constant set $itemValueId, runIndexId = $runIndexId\n";
	} else {
	    print "info: would have linked runs $runMinThis-$runMaxThis"
		. " to constant set $itemValueId\n";
	}
    }
}

exit 0;

sub ProcessCommandLine {

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
    if ( $system eq 'allsystems' &&  $subsystem ne 'allsubsystems' ) {
      die "error: all systems selected," 
	. " all susbsystems must be selected also\n";
    }

    if ($subsystem eq 'allsubsystems' && $item ne 'allitems') {
	die "error: all subsystems selected,"
	    . " all items must be selected also\n";
    }
    if (!$runIndexDest) {
        if ($rid) {
            $runIndexDest = $rid;
        } else {
	    &PrintUsage();
            die "error: runIndexDest not defined\n";
        }
    }
    if (!$runIndexSrc) {
        if ($ris) {
            $runIndexSrc = $ris;
        } else {
	    &PrintUsage();
            die "error: runIndexSrc not defined\n";
        }
    }
    if (!$runMin) {
        if ($min) {
            $runMin = $min;
	}
    }
    if (!$runMax) {
        if ($max) {
            $runMax = $max;
        }
    }
    if ($allruns) {
	if ($runMin) {
            &PrintUsage();
            die "error: allruns and runMin defined\n";
	}
	if ($runMax) {
            &PrintUsage();
            die "error: allruns and runMax defined\n";
	}
	$runMin = -1000000;
	$runMax = 1000000; 
    } else {
	if (!$runMin) {
            &PrintUsage();
            die "error: runMin not defined\n";
        }
	if (!$runMax) {
            &PrintUsage();
            die "error: runMax not defined\n";
	}
	if ($runMin > $runMax) {
	    die "error: runMin greater than runMax.\n";
	}
    }
    if (!$time) {
	$time = '2037-01-01';
    }
    if (!$user) {
	if ($runIndexDest eq 'calib.RunIndex' || $runIndexDest eq 'RunIndex') {
	    $user = $ENV{USER};
	} else {
	    $user = 'clasuser';
	}
    }

    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
}

sub PrintUsage {
    print <<EOM;
purpose: Copy a set of run ranges from one run index to another. Do so for one
         item, all items in a subsystem, or all items in a system.

usage:
caldb_copy_ranges.pl s=<system name | 'allsystems'> ss=<subsystem name | 'allsubsystems'>  \\
    i=<item name | 'allitems'> \\
    [min=<run min> max=<run max> | allruns=<non-zero>] \\
    ris=<source run index table name> \\
    rid=<destination run index table name> [time=<time strobe>] \\
    [user=<MySQL user name>] [hostname=<hostname of db server>] \\
    [nowrite=<non-zero>] [noprompt=<non-zero>] [help=<non-zero>]

note: use nowrite=1 to practice using this script

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  min or runMin
  max or runMax
  ris or runIndexSrc for source run index table name
  rid or runIndexDest for destination run index table name

example:
caldb_copy_ranges.pl s=DC_DOCA ss=t_max i=Sector1 ris=RunIndex \\
  rid=calib_user.RunIndexMarkI min=23602 max=23682
EOM
    return;
}
