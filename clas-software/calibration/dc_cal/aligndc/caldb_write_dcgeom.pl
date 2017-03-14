#!/usr/bin/env perl
#
# Script hacked from one by Mark Ito for CalDB (S. Morrow 5/27/02)
#
# $Id: caldb_write_dcgeom.pl,v 1.2 2005/12/14 06:30:50 bellis Exp $
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
    chomp;
#    if (/DC align sec:. reg:. \(xyz\): ([eE0-9\-\.]+) ([eE0-9\-\.]+) ([eE0-9\-\.]+)/) {
    if (/DC align sec:. reg:. \(xyz\):  ([eE0-9\-\.]+)  ([eE0-9\-\.]+)  ([eE0-9\-\.]+)/) {
      push (@xalign, $1);
      push (@yalign, $2);
      push (@zalign, $3);
  }
#    elsif (/DC rot sec:. reg:. \(xyz\): ([eE0-9\-\.]+) ([eE0-9\-\.]+) ([eE0-9\-\.]+)/) {
    elsif (/DC rot sec:. reg:. \(xyz\):  ([eE0-9\-\.]+)  ([eE0-9\-\.]+)  ([eE0-9\-\.]+)/) {
      push (@xrot, $1);
      push (@yrot, $2);
      push (@zrot, $3);
  }
}
    print "@xalign \n";
    print "@yalign \n";
    print "@zalign \n";
    print "@xrot \n";
    print "@yrot \n";
    print "@zrot \n";

close FILE;

# get the length of the item
$status = ShowItemInfo($dbh, $system, $subsystem, $item, \%itemInfo);

# check the file content is reasonable (expected number of rows)
$rows_in_file=($#xalign+1)+($#xrot+1)+3;
#$rows_in_file=($#align+1)+($#rot+1)+3;

(39 eq $rows_in_file) or die "error!!!: invalid inputFile $inputFile $rows_in_file rows (expected 39)\n";

$commentIndex=$comment;

#WRITE EACH CONSTANT SET FOR X,Y,Z ALIGN & ROT TO THE CALDB

$status = WriteConstantSet($dbh, DC_GEOM, dc, xalign,
$sourceRunMin, $sourceRunMax, $comment, \$itemValueId, @xalign);
$status = LinkConstantSet($dbh, DC_GEOM, dc, xalign, 
			  $runIndexTable,
			  $sourceRunMin, $sourceRunMax, 
			  $itemValueId, $commentIndex,
			  \$runIndexId);

$status = WriteConstantSet($dbh, DC_GEOM, dc, yalign,
$sourceRunMin, $sourceRunMax, $comment, \$itemValueId, @yalign);
$status = LinkConstantSet($dbh, DC_GEOM, dc, yalign, 
			  $runIndexTable,
			  $sourceRunMin, $sourceRunMax, 
			  $itemValueId, $commentIndex,
			  \$runIndexId);

$status = WriteConstantSet($dbh, DC_GEOM, dc, zalign,
$sourceRunMin, $sourceRunMax, $comment, \$itemValueId, @zalign);
$status = LinkConstantSet($dbh, DC_GEOM, dc, zalign, 
			  $runIndexTable,
			  $sourceRunMin, $sourceRunMax, 
			  $itemValueId, $commentIndex,
			  \$runIndexId);

$status = WriteConstantSet($dbh, DC_GEOM, dc, xrot,
$sourceRunMin, $sourceRunMax, $comment, \$itemValueId, @xrot);
$status = LinkConstantSet($dbh, DC_GEOM, dc, xrot, 
			  $runIndexTable,
			  $sourceRunMin, $sourceRunMax, 
			  $itemValueId, $commentIndex,
			  \$runIndexId);

$status = WriteConstantSet($dbh, DC_GEOM, dc, yrot,
$sourceRunMin, $sourceRunMax, $comment, \$itemValueId, @yrot);
$status = LinkConstantSet($dbh, DC_GEOM, dc, yrot, 
			  $runIndexTable,
			  $sourceRunMin, $sourceRunMax, 
			  $itemValueId, $commentIndex,
			  \$runIndexId);

$status = WriteConstantSet($dbh, DC_GEOM, dc, zrot,
$sourceRunMin, $sourceRunMax, $comment, \$itemValueId, @zrot);
$status = LinkConstantSet($dbh, DC_GEOM, dc, zrot, 
			  $runIndexTable,
			  $sourceRunMin, $sourceRunMax, 
			  $itemValueId, $commentIndex,
			  \$runIndexId);


print "Data from inputFile $inputFile loaded to $hostname, \nrun index table = $runIndexTable (itemValueId = $itemValueId)\n\n";

exit 0;

sub ProcessCommandLine {
    $user = 'clasuser';
    $password = '';
    $database = 'calib';

    if ($help) {
	&PrintUsage();
	exit 0;
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
    if (!$runIndexTable) {
	if ($it) {
	    $runIndexTable = $it;
	} else {
	    $runIndexTable = "RunIndex";
	    if ($verbose) {print "using runIndexTable = $runIndexTable\n";}
	}
    }
     if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_write_dcgeom.pl \\
    c=<comment> f=<file with constants> \\
    [srmin=<source run min>] [srmax=<source run max>]  \\
    [it=<run index table>]  \\
    [hostname=<hostname of db server>] [help=<non-zero>]

alternate flag names:
  srmin or sourceRunMin
  srmax or sourceRunMax
  c or comment
  f or inputFile
  it or runIndexTable
example:
caldb_write_dcgeom.pl \\
    srmin=1000 srmax=2000 c="just an example" f=const_file.dat \\
    it=calib_user.RunIndexe1_6 hostname=claspc2.jlab.org
EOM
    return;
}


