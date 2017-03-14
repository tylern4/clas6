#!/usr/bin/env perl
#
# Copies effective run ranges from one run index table to
# another. 
# If system, subsystem, item not choosen copies all items of all systems
# Given by minR-maxR will be checked to be in limits for a certain
# run period
# 
# updated by H. Avagyan 11/5
########################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
##use lib ("/home/avakian/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

# get the password

# connect to database

$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

# timelimit
$timeLimit=($time ? $time : '2037-1-1');
# &checkRunPeriod($dbh,$minR,$maxR,\$runP);

if(!$runP) { die  "inconsistent entry: check range with runinfo! \n";}
else { print "  $runIndexTable ($runP)  insert $v to $system $subsystem $item \n";}

# make sure the destination RunIndex exist
&CreateRunIndex($rid,\$status);


# get the list of all subsystems/items to copy
$status = ShowSSI($dbh, $system, $subsystem, $item, \@itemArrayRef);

# loop over all subsystems/items

for ($iitem = 0; $iitem <= $#itemArrayRef; $iitem++) {
    $system=$itemArrayRef[$iitem][0];
    $subsystem=$itemArrayRef[$iitem][1];
    $item=$itemArrayRef[$iitem][2];
    if(!$quiet) {print " copy: system-> $system subs-> $subsystem "
		     ."item-> $item from $ris to $rid on $hostname\n";}
#
#   get the run ranges from the source index 
#
    $status = ShowRunRanges($dbh, $system, $subsystem, $item, 
              $runIndexSrc,$time,$minR,$maxR, \@runRangeInfo);

# make the links in the destination run index for all run ranges in
# the source index that are in the specified range.

  foreach $ir (0 .. $#runRangeInfo) {
     $runMinThis = $runRangeInfo[$ir][0];
     $runMaxThis = $runRangeInfo[$ir][1];
     if (($runMinThis > $maxR) || ($runMaxThis < $minR)) {
	if($debug) {print " Skip range:  [$runMinThis $runMaxThis] \n";}
     } else {
	$itemValueId = $runRangeInfo[$ir][2];
	$comment = "Link $runIndexSrc  ($minR-$maxR) timelimit $timeLimit ";
	if($debug) {
           print "debug mode: \n LinkConstantSet($dbh, $system, $subsystem," 
                . "$item,$runIndexDest, $runMinThis, $runMaxThis,"
		. "$itemValueId, $comment, \$runIndexId) \n";
         } else {  
# if strict defined keep exact limits
	   if($keep != 1){
	     if ($runMinThis < $minR) { $runMinThis=$minR;}
	     if ($runMaxThis > $maxR) { $runMaxThis=$maxR;}
	   }
#
           $status = LinkConstantSet($dbh, $system, $subsystem, $item,
				  $runIndexDest, $runMinThis, $runMaxThis,
				  $itemValueId, $comment, \$runIndexId);
	   if($verbose) {print "info: linking runs $runMinThis-$runMaxThis"
	   . " to constant set $itemValueId, runIndexId = $runIndexId\n";}
         }
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
	    print "\n warning:system not defined\n  linking all systems for $system\n";
	}
    }
    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	} else {
	    print "\n warning:subsystem not defined\n      linking all subsystem for $system\n";
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	} else {
  print "\n warning:item not defined\n        linking all items  for $system  $subsystem\n";
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
    if (!$runIndexDest) {
        if ($rid) {
            $runIndexDest = $rid;
        } else {
	    &PrintUsage();
            die "error: runIndexDest not defined\n";                 
        }
    }

    if (!$minR) {
	    &PrintUsage();
            die "error: minR not defined\n";                 
    }
    if (!$maxR) {
	    &PrintUsage();
            die "error: maxR not defined\n";                 
    }

    if (!time) {
	$time = '2037-01-01';
    }

     if ($runIndexDest =~ /^calib_user./) {
           $user = 'clasuser';
	} else {
	   $user = $ENV{USER};
      }


    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
    if (!$database){
	$database = "calib";
    }
    print "\n ---  Copying $ris to $rid on $hostname --- \n";
}

sub CreateRunIndex {
    my($rid,$status) = @_;
 $sql  ="CREATE TABLE IF NOT EXISTS $rid (";
 $sql .=" RunIndexId int(11) NOT NULL auto_increment, ";
 $sql .=" minRun int(11) default NULL,";
 $sql .=" maxRun int(11) default NULL,";
 $sql .=" itemId int(11) NOT NULL default '0',";
 $sql .=" itemValueId int(11) NOT NULL default '0',";
 $sql .=" officer varchar(31) default NULL,";
 $sql .=" time timestamp(14) NOT NULL,";
 $sql .=" comment text,";
 $sql .=" PRIMARY KEY (RunIndexId),";
 $sql .=" KEY ItemMinMax (itemId,minRun,maxRun) "; 
 $sql .=") TYPE=MyISAM";
 $$status=$dbh->do($sql);
    print " CreateRunIndex: status of create -> $$status \n";
}


sub PrintUsage {
    print <<EOM;
usage:
runIndex2runIndex.pl [s=<system>] [ss=<subsystem>] [i=<item>] \\
    ris=<source run index table name> \\
    rid=<destination run index table name> \\
    minR=<run min>] [maxR=<run max>  \\
    [time=<time strobe>] \\
    [user=<MySQL user name>] [hostname=<hostname of db server>] \\
    [help=<non-zero>] [debug=1] 

alternate flag names:
  s or system for system name
  ss or subsystem for subsystem name
  i or item for item name
  minR
  maxR
  ris or runIndexSrc for source run index table name
  rid or runIndexDest for destination run index table name
  debug=1 prints only the commands without executing them.
  keep=1 keep the existing range, else enforce exact equation of run range to [minRun,maxRun]

example:
runIndex2runIndex.pl s=DC_DOCA ss=t_max i=Sector1 ris=calib_user.RunIndexe1_6 \\
  rid=calib_user.RunIndexe1_6DC minR=30300 maxR=30500
EOM
    return;
}
