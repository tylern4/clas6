#!/usr/bin/env perl
#
# $Id: caldb_write_constant_set.pl,v 1.10 2003/05/07 17:55:28 marki Exp $
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

open (FILE, $inputFile);

while (<FILE>) {
    chop;
    push (@constants, $_);
}

# get the length of the item
$status = ShowItemInfo($dbh, $system, $subsystem, $item, \%itemInfo);

if ($status == 1) {
    die "error: system $system, subsystem $subsystem, item $item does not exist\n";
}

# check the file content is reasonable (expected number of rows)
$rows_in_file=$#constants+1;

print "type = $itemInfo{type}\n";
if ($itemInfo{type} eq 'char') {
    print "it's a char\n";
    print "rows = $rows_in_file\n";
} elsif ($itemInfo{length} != $rows_in_file) {
    die "error: $inputFile has $rows_in_file rows, $itemInfo{length} expected\n";
}


$status = WriteConstantSet($dbh, $system, $subsystem, $item,
			$sourceRunMin, $sourceRunMax, $comment, \$itemValueId, @constants);

print "Data from inputFile $inputFile loaded to $hostname (itemValueId = $itemValueId)\n";

exit 0;

sub ProcessCommandLine {

    $user = 'clasuser';
    $password = '';
    $database = 'calib';

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
    if (!$comment) {
	if ($c) {
	    $comment = $c;
	} else {
	    &PrintUsage();
	    die "error: comment not defined\n";
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
    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_write_constant_set.pl s=<system> ss=<subsystem> i=<item> \\
    c=<comment> [srmin=<source run min>] [srmax=<source run max>]  \\
    f=<file with constants> [hostname=<hostname of db server>] \\
    [help=<non-zero>]

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  srmin or sourceRunMin
  srmax or sourceRunMax
  c or comment
  f or inputFile

example:
caldb_write_constant_set.pl s=mom_corr ss=theta_func i=sector6 \\
    srmin=1000 srmax=2000 c="just an example" f=const_file.dat \\
    hostname=claspc2.jlab.org
EOM
    return;
}


