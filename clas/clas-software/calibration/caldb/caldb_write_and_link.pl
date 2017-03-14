#!/usr/bin/env perl
#
# $Id: caldb_write_and_link.pl,v 2.14 2003/05/07 17:55:28 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

# get the password

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowItemInfo($dbh, $system, $subsystem, $item, \%itemInfo);
#print "status = $status\n";
if ($status != 0) {
    die "error: system $system, subsystem $subsystem, item $item does not exist";
}

ReadConstantsFromFile();
# check the file content is reasonable (expected number of rows)
$rows_in_file=$#constants+1;

if ($itemInfo{type} eq 'char') {
    print "it's a char\n";
    print "rows = $rows_in_file\n";
} elsif ($itemInfo{length} != $rows_in_file) {
    die "error: invalid inputFile $inputFile $rows_in_file rows (expected $itemInfo{length})\n";
}

$status = WriteConstantSet($dbh, $system, $subsystem, $item, $sourceRunMin,
			   $sourceRunMax, $commentConstants, \$itemValueId,
			   @constants);

$status = LinkConstantSet($dbh, $system, $subsystem, $item, $runIndexTable,
			  $runMin, $runMax, $itemValueId, $commentIndex,
			  \$runIndexId);

print "Data from inputFile $inputFile loaded to $hostname $runIndexTable (runIndexId = $runIndexId, itemValueId = $itemValueId)\n";

exit 0;

sub ReadConstantsFromFile {

    open (FILE, $inputFile);

    while (<FILE>) {
	chop;   
	push (@constants, $_);
    }

    close (FILE);

}

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
    if (!$commentIndex) {
	if ($ci) {
	    $commentIndex = $ci;
	} else {
	    &PrintUsage();
	    die "error: comment for the run index not defined\n";
	}
    }
    if (!$inputFile) {
        if ($f) {
            $inputFile = $f;
        } else { 
            &PrintUsage();
            die "error: inputFile not defined\n";
        }
    }
    if (!$sourceRunMin) {
        if ($srmin) {
            $sourceRunMin = $srmin;
        } else {
            $sourceRunMin = 'NULL';
        }
    }
    if (!$sourceRunMax) {
        if ($srmax) {
            $sourceRunMax = $srmax;
        } else {
            $sourceRunMax = 'NULL';
        }
    }
    if (!$commentConstants) {
        if ($cc) {
            $commentConstants = $cc;
        } else {
            $commentConstants = $commentIndex;
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
      -T $inputFile or die "error!!!: inputFile $inputFile is missing\n";
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_write_and_link.pl s=<system> ss=<subsystem> i=<item> min=<run min> \\
    max=<run max> ci=<comment for run index> f=<file with constants> \\
    it=<run index table name> \\
    [srmin=<source run min>] [srmax=<source run max>] \\
    [cc=<comment for constant set table>] \\ 
    [user=<MySQL user name>] \\
    [hostname=<hostname of db server>] [help=<non-zero>]

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  min or runMin
  max or runMax
  ci or commentIndex 
  srmin or sourceRunMin
  srmax or sourceRunMax
  cc or commentConstants
  f or inputFile
  it or runIndexTable for run index table name

example:
caldb_write_and_link.pl s=mom_corr ss=theta_func i=sector6 min=1000 \\
    max=2000 srmin=1400 srmax=1450 f=const_file.dat \\
    it=calib_user.RunIndexJunk \\
    ci='linking new constants' cc='creating new constants' \\
    hostname=claspc2.jlab.org
EOM
    return;
}
