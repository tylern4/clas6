#!/usr/bin/env perl
#
# Shows changes to a run index table for selected items, going back
# to a selected point in time.
#
# $Id: caldb_show_changes.pl,v 2.4 2001/12/20 14:37:13 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

# set connection variables

if (!$hostname) {
    $hostname = 'clasdb.jlab.org';
}
$user = 'clasuser';

$database = 'calib';
#
# print a summary of the requested actions
#
if (!$quiet) {
    print "connecting to host $hostname\n";
    if (!$system) {
	print "examining entries from all systems\n";
    } else {
	print "examining entries from system $system\n";
    }
    if (!$subsystem) {
	print "all subsystems\n";
    } else {
	print "subsystem $subsystem\n";
    }
    if (!$item) {
	print "all items\n";
    } else {
	print "item $item\n";
    }
    print "from run index table $runIndexTable\n";
    if ($officer eq '%') {
	print "entered by any officer\n";
    } else {
	print "entered by officer $officer\n";
    }
    print "going back to $time\n";
}
#
# connect to database
#
$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowSSI($dbh, $system, $subsystem, $item, \@ssi_ary);


foreach $ii (0 .. $#ssi_ary) {
    $system_this = $ssi_ary[$ii][0];
    $subsystem_this = $ssi_ary[$ii][1];
    $item_this = $ssi_ary[$ii][2];
    if (!($skip_run_control && $system_this eq 'RUN_CONTROL')) {
	@change = ();
	ShowChangesIndex($dbh, $system_this, $subsystem_this, $item_this,
			 $runIndexTable, $officer, $time, \@change);
	for $ic (0 .. $#change) {
	    if ($ic == 0) {
		print "system = $system_this, subsystem = $subsystem_this, item = $item_this\n";
	    }
	    $change_this = $change[$ic];
	    @cray = split("\n", $change_this->{comment});
	    if (!$fullComment && $#cray > 2) {
		$ncray = 2;
	    } else {
		$ncray = $#cray;
	    }
	    DayTime($change_this->{time}, \$date_this, \$time_this);
	    print "    runs $change_this->{minRun}-$change_this->{maxRun},"
		. " set $change_this->{itemValueId},"
		    . " linked $date_this $time_this"
			. " by $change_this->{officer}\n";
	    print "        comment: $cray[0]\n";
	    for $icom (1 .. $ncray) {
		print "                 $cray[$icom]\n";
	    }
	    push(@id_delete, $change_this->{RunIndexId});
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
	}
    }
    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	}
    }
    if (!$officer) {
	if ($o) {
	    $officer = $o;
	} else {
	    $officer = '%';
	}
    }
    if (!$time) {
	if ($t) {
	    $time = $t;
	} else {
	    PrintUsage();
	    die "error: time not defined\n";
	}
    }
    if (!$runIndexTable) {
        if ($it) {
            $runIndexTable = $it;
        } else {
            $runIndexTable = "RunIndex";
        }
    }
}

sub PrintUsage {
    print <<EOM;
purpose: Shows changes to a run index back to a specified time for
         specified items entered by a specified officer.

usage:
caldb_show_changes.pl time|t=<time to go back to> [s|system=<system name>] \\
    [subsystem|ss=<subsystem name>] [item|i=<item name> \\
    [officer|o=<officer name>] [runIndexTable|it=<run index table name>]  \\
    [fullComment=<non-zero>] [quiet=<non-zero>] \\
    [hostname=<hostname of db server>] [help=<non-zero>]

example:
caldb_show_changes.pl t=2001/12/1 s=DC_DOCA ss=t_max i=Sector6
EOM
    return;
}
