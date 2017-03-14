#!/usr/bin/env perl
#
# Deletes rows from a run index table for selected items, going back
# to a selected point in time.
#
# $Id: caldb_delete_changes.pl,v 2.2 2002/01/09 22:02:55 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

# set connection variables

if ($nodelete || $runIndexTable =~ /^calib_user/) {
    if (!$hostname) {
	$hostname = 'clasdb.jlab.org';
    }
    $user = $ENV{USER};
} else {
    if ($hostname) {
	die "error: cannot specify hostname when deleting from a non-clas_user database.\n";
    } else {
	$hostname = 'localhost';
	$user = 'dbmanager';
    }
}

$database = 'calib';

#
# print a summary of the requested actions
#
if (!$nodelete) {
    print "note: use nodelete=1 to practice using this script\n";
}
print "info: connecting to host $hostname\n";
print "info: as user $user\n";
print "info: deleting entries from system $system\n";
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
print "info: from run index table $runIndexTable\n";
print "info: entered by user $author\n";
print "info: going back to $time\n";
if ($nodelete) {
    print "info: deletion is disabled\n";
} else {
    print "info: deletion is enabled\n";
}
#
# If deletion is enabled prompt for are-you-sure answer
#
if (!$nodelete) {
    print "Is this what you want to do? (yes/no) ";
    $response = <>;
    chomp $response;
    if ($response ne 'yes') {
	print "Exiting per user request.\n";
	exit 0;
    }
}

if ($user eq 'dbmanager') {
#   get the password
    print "password: ";
    system("stty -echo");
    chop($password=<STDIN>);
    print "\n";
    system("stty echo");
}

# connect to database

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

print "info: searching for entries to delete\n";

if ($subsystem eq 'allsubsystems') {
    $itemshow = '';
    $status = ShowSubsystems($dbh, $system, $itemshow, \@subsystem_ary);
} else {
    @subsystem_ary = ();
    $subsystem_ary[0] = $subsystem;
}
@id_delete = ();
foreach $iss (0 .. $#subsystem_ary) {
    $subsystem_this = $subsystem_ary[$iss];
    print "info: subsystem = $subsystem_this\n";
    if ($item eq 'allitems') {
	$status = ShowItems($dbh, $system, $subsystem_this, \@item_ary);
    } else {
	@item_ary = ();
	$item_ary[0] = $item;
    }
    foreach $ii (0 .. $#item_ary) {
	$item_this = $item_ary[$ii];
	print "info: item = $item_this\n";
	@change = ();
	ShowChangesIndex($dbh, $system, $subsystem_this, $item_this,
			 $runIndexTable, $author, $time, \@change);
	for $ic (0 .. $#change) {
	    $change_this = $change[$ic];
	    @cray = split("\n", $change_this->{comment});
	    if ($#cray > 2) {
		$ncray = 2;
	    } else {
		$ncray = $#cray;
	    }
	    DayTime($change_this->{time}, \$date_this, \$time_this);
	    print "info: runs $change_this->{minRun}-$change_this->{maxRun},"
		. " set $change_this->{itemValueId},"
		    . " linked $date_this $time_this"
			. " by $change_this->{officer}\n";
	    print "      comment: $cray[0]\n";
	    for $icom (1 .. $ncray) {
		print "               $cray[$icom]\n";
	    }
	    push(@id_delete, $change_this->{RunIndexId});
	}
    }
}

if (!$nodelete) {

    print "Are you sure you want to delete these entries? (yes/no) ";
    $response = <>;
    chomp $response;
    if ($response ne 'yes') {
	print "Exiting per user request.\n";
	exit 0;
    }

    for $id (0 .. $#id_delete) {
	print "info: deleting id=$id_delete[$id]\n";
	DeleteRunIndexRow($dbh, $runIndexTable, $id_delete[$id]);
    }
    
} else {

    print "info: deletion disabled, no action taken\n"

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
    if ($subsystem eq 'allsubsystems' && $item ne 'allitems') {
	die "error: all subsystems selected,"
	    . " all items must be selected also\n";
    }
    if (!$author) {
	if ($a) {
	    $author = $a;
	} else {
	    PrintUsage();
	    die "error: author not defined\n";
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
	    PrintUsage();
	    die "error: runIndexTable not defined\n";
        }
    }
}

sub PrintUsage {
    print <<EOM;

purpose: Delete run index entries back to a specified time for
         specified items entered by a specified officer.

usage:
caldb_delete_changes.pl s=<system name> \\
    ss=<subsystem name | 'allsubsystems'> i=<item name | 'allitems'> \\
    a=<author name> t=<time to go back to> it=<run index table name> \\
    [hostname=<hostname of db server>] [nodelete=<non-zero>] \\
    [help=<non-zero>]

note: use nodelete=1 to practice using this script

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  a or author for author name
  t or time for time to go back to
  it or runIndexTable for run index table name

example:
caldb_delete_changes.pl s=SC_CALIBRATIONS ss=delta_T i=paddle2paddle \\
    it=calib_user.RunIndexJunk a=vipuli t=2001/10/5 hostname=claspc2
EOM
    return;
}
