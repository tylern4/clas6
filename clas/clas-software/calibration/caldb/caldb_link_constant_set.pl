#!/usr/bin/env perl
#
# Makes the correspondence between a particular set of constants and a
# particular run range for a specified item and version of the
# database.
#
# $Id: caldb_link_constant_set.pl,v 1.10 2002/09/24 15:53:17 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

# get the password

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = LinkConstantSet($dbh, $system, $subsystem, $item, $runIndexTable,
			  $runMin, $runMax, $constantSetId, $comment,
			  \$runIndexId);

if ($status != 0) {
    die "error: system $system, subsystem $subsystem, item $item does not exist\n";
}

print "runIndexId = $runIndexId\n";

exit 0;

sub ProcessCommandLine {

    $database = 'calib';
    $password ="";

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
    if (!$runIndexTable) {
        if ($it) {
            $runIndexTable = $it;
        } else {
	    &PrintUsage();
	    die "error: runIndexTable not defined\n";
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
    if (!$constantSetId) {
	if ($id) {
	    $constantSetId = $id;
	} else {
	    &PrintUsage();
	    die "error: constantSetId not defined\n";
	}
    }
    if (!$comment) {
	if ($c) {
	    $comment = $c;
	} else {
	    &PrintUsage();
	    die "error: comment not defined\n";
	}
    }

    if (!$user) {
	if ($runIndexTable =~ /^calib_user./) {
	    $user = 'clasuser';
	} else {
	    $user = $ENV{USER};
	}
    }

    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_link_constant_set.pl s=<system> ss=<subsystem> i=<item> min=<run min> \\
    max=<run max> id=<item value id number> c=<comment> \\
    it=<run index table name> [user=<MySQL user name>] \\
    [hostname=<hostname of db server>] [help=<non-zero>]

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  min or runMin
  max or runMax
  id or constantSetId
  c or comment
  it or runIndexTable for run index table name

example:
caldb_link_constant_set.pl s=mom_corr ss=theta_func i=sector6 min=100 \\
    max=200 id=3 c="just a link example" it=calib_user.RunIndexJunk
EOM
    return;
}
