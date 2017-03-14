#!/usr/bin/env perl
#
# caldb_import_set.pl
# tacking an existing set of calibration constants from
# a directory with text files and writing to a
# specified RunIndex and applying to user specified  
# validity range and RunIndex table. The set of systems, subsystems 
# and items could be defined from the command line
#
# H.Avakian 03/20/02
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);
$status = ShowSSI($dbh, $system, $subsystem, $item, \@itemArrayRef);


# loop over all subsystems and items
for ($iitem = 0; $iitem <= $#itemArrayRef; $iitem++) {
 $system=$itemArrayRef[$iitem][0];
 $subsystem=$itemArrayRef[$iitem][1];
 $item=$itemArrayRef[$iitem][2];
if (!$quiet){
  print "Now importing $system $subsystem $item of $runIndexDest from $dirSource\n";
}

$inputFile=$dirSource.'/'.$system.'/'.$subsystem.'_'.$item;
$status = ShowItemInfo($dbh, $system, $subsystem, $item, \%itemInfo);

ReadConstantsFromFile();
# check the file content is reasonable (expected number of rows)
$rows_in_file=$#constants+1;

 if ($itemInfo{type} eq "char") {
     print "skip char field $system, $subsystem, $item (not tested!!!)\n";
 }else{
  ($itemInfo{length} eq $rows_in_file) or die "error!!!: invalid inputFile $inputFile $rows_in_file rows (expected $itemInfo{length})\n";
  print "@constants \n";
$status = WriteConstantSet($dbh, $system, $subsystem, $item, $sourceRunMin,
			   $sourceRunMin, $commentIndex, \$itemValueId,
			   @constants);

$status = LinkConstantSet($dbh, $system, $subsystem, $item, $runIndexDest,
			  $runMin, $runMax, $itemValueId, $commentIndex,
			  \$runIndexId);

print "Data from inputFile $inputFile loaded to $hostname $runIndexDest (runIndexId = $runIndexId, itemValueId = $itemValueId)\n";
}

}
exit 0;


#
sub ReadConstantsFromFile {

    open (FILE, $inputFile);
    $#constants=-1;
    while (<FILE>) {
	chop;   
	push (@constants, $_);
    }

    close (FILE);

}

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
	    print "warning:system not defined\n  copy all systems\n";
#	    &PrintUsage();
#	    die "error: system not defined\n";
	}
    }
    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	} else {
	    print "warning:subsystem not defined\n      linking all subsystem for $system\n";
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	} else {
  print "warning:item not defined\n        linking all items  for $system  $subsystem\n";
	}
    }
    if (!$sourceRunMin) {
        if ($srmin) {
            $sourceRunMin = $srmin;
        } else {
            &PrintUsage();
	    die "error: Source run not defined\n";
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
    if (!$commentIndex) {
	if ($ci) {
	    $commentIndex = $ci;
	} else {
	    &PrintUsage();
	    die "error: comment for the run index not defined\n";
	}
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
    $dirSource='./caldb_export'.$sourceRunMin;
    if (-d $dirSource) {  
  if (!$quiet) {
         print "Reading constants from dir $dirSource \n";
         print "using runIndexTable = $runIndexDest from $hostname\n";
       }
    } else {          
         &PrintUsage();
         die "error: Source dir $dirSource not defined\n";
    }
}


sub PrintUsage {
    print <<EOM;
function: Import constants from a given directory caldb_export<srmin>.

usage:
caldb_import_run.pl   srmin=<runSource> \\
    [s=<system>] [ss=<subsystem>] [i=<item>] \\
    [rid=<destination run index table>] \\
    [t=<time of validity>] \\
    [hostname=<hostname of db server>]  \\
    [help=<non-zero>] [q=<non-zero to suppress some info>]
 

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  r or runSource for reference run number
  rid or runIndexDest destination run index table name
  q or quiet for suppression of extra information
  srmin or sourceRunMin source run (defines the directory for input)
example:
caldb_import_set.pl s=DC_DOCA ss=xvst_params i=SL1 rid=calib_user.RunIndextestmap srmin=28000 min=40000 max=40001 ci='test' \\
EOM
    return;
}

# end of perl script
