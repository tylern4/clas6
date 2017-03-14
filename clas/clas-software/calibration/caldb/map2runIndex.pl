#!/usr/bin/env perl 
#
# map2runIndex modified version of map_item2db.pl
# copy a Map file (or its subsystem or item) to the calib
# database. Contains a run period check to avoid complications with typos.
# $Id: map2runIndex.pl,v 2.4 2001/11/05 13:32:52 avakian Exp $
#####################################################################
#
use lib ("/home/avakian/caldb");
use CaldbUtil;
#
# get command line parameters
#
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book
ProcessCommandLine();
#
# connect to MySQL server
#
$database = "calib";

$verbose = 1;


if (! -f "$ENV{'CLAS_BIN'}/scan_map") {
    die "\$CLAS_BIN/scan_map not found.(try setenv CLAS_BIN /group/clas/builds/release-2-5/bin/LinuxRH6 ) Exiting\n";
}

ConnectToServer ($hostname, $user, $password, $database, $verbose, \$dbh);
#
# Scan the map files
# 
&checkRunPeriod($dbh,$minR,$maxR,\$runP);
if(!$runP) { die  "\n\n dubious entry: please check the $minR - $maxR range with period runinfo! \n";}

# extended warning
print "\n\n Will attempt to connect to server at $hostname as user $user,\n";
print "  and copy the $mapfile to  $runIndexDest ($runP) \n";
print "  with limits $minR - $maxR.\n";
print "  Is that ok? (yes or no): ";
$answer = <STDIN>;
chomp $answer;
if ($answer ne 'yes') {
    die "You did not answer \"yes\". Exiting.\n";
}


$author  = $user;
$officer = $user;
$comment = "copied from $mapfile for $runP";

# get the system name

print "Working on map file $mapfile\n";
@field = split(/\//, $mapfile);
@subfield = split(/\.map/, $field[$#field]);
$system = $subfield[0];

# check for the scan_map program

# scan the Map file
    
open (SCAN, "$ENV{'CLAS_BIN'}/scan_map -t $mapfile |");
$line = <SCAN>;
$line = <SCAN>;
while ($line = <SCAN>) {
# get the subsystem information

    chop $line;
    @field = split(/\s+/, $line);
    $keyword = $field[1];
    if ($keyword eq "Subsystem:") {
	$subsystem = $field[2];
	$subsystem =~ s/,//;
	$nitems = $field[4];
        if ($debug) {print "  Working on subsystem $subsystem with $nitems items.\n";}

# loop over all items for this subsystem

	for ($ii = 0; $ii < $nitems; $ii++) {
	    &PROCESS_ITEM();
	}
    } else {
	print "bad line:\n";
	print "$line \n";
	exit(1);
    }
}

exit;

sub PROCESS_ITEM {

# get the item information

    $line = <SCAN>;
    chop $line;
    @field = split(/:/, $line);
    @field1 = split(/,/, $field[1]);
    $item = $field1[0];
    $item =~ s/^\s+//;
    @field1 = split(/,/, $field[2]);
    $length_map = $field1[0];
    $length_map =~ s/^\s+//;
    @field1 = split(/,/, $field[3]);
    $type = $field1[0];
    $type =~ s/^\s+//;
    @field1 = split(/,/, $field[4]);
    $nsets = $field1[0];
    $nsets =~ s/^\s+//;
    $line = <SCAN>;
    chop $line;
    if( ($i && ($item ne $i)) || ($ss && ($ss ne $subsystem)) ) 
      {if ($debug) {print " Skip this item ($subsystem $item) \n";}} 
    else {
	if($debug) {print " $subsystem   Item: $item, Length: $length_map, Type: $type, Sets: $nsets\n";}
      @run = split(/\s+<--\s+/, $line);
      if ($debug) {print "    \@run = @run\n";}

      $length_db = $length_map;
      if ($length_map == 6912) {
	 $type_db = $type . 'blob';
      } else {
	$type_db = $type;
      }

      for ($s = 1; $s <= $nsets; $s++) {
	$maxRun = $run[$s-1]-1;
	$minRun = $run[$s];
	if ($s == 1){$maxRun = 1000000;}
	if ($minRun == 'UNDEF') {$minRun = 0;}

	if( ($maxR < $minRun) || ($minR > $maxRun) ) {
	if ($debug) {print " Skip set=$s minRun=$minRun maxRun=$maxRun  ($minR,$maxR) \n";}
	 }else {  
#                  cut to unPeriod limits
           if( $maxR < $maxRun ) { $maxRun=$maxR;}
	   if( $minR > $minRun ) { $minRun=$minR;}
	   if ($debug)  {print "Writing->$hostname $runIndexDest   set=$s minRun=$minRun maxRun=$maxRun   [$minR - $maxR] \n";}
          &PROCESS_SET();
        }
      }
   }
}

sub PROCESS_SET {
    $index = 0;
    @value = ();
    if ($type eq "float") {
	$command = "$ENV{'CLAS_BIN'}/get_map_float -m$mapfile -s$subsystem -i$item -t$run[$s] -l$length_map";
	if ($debug) {print "command = $command\n";}
	open (GET, "$command |");
	while ($line = <GET>) { 
	    chop $line;
	    $value[$index] = $line;
	    if ($debug) {print "        value[$index] = $value[$index]\n";}
	    $index++;
	} 
	
    } elsif ($type eq "int") {
	$command = "$ENV{'CLAS_BIN'}/get_map_int -m$mapfile -s$subsystem -i$item -t$run[$s] -l$length_map";
	if ($debug) {print "command = $command\n";}
	open (GET, "$command |");
	while ($line = <GET>) {
	    chop $line;
	    $value[$index] = $line;
	    if ($debug) {print "value[index] = $value[$index]\n";}
	    $index++;
	} 
	
    }  elsif ($type eq "char") {
	$command = "$ENV{'CLAS_BIN'}/get_map_char -m$mapfile -s$subsystem -i$item -t$run[$s] -l$length_map";
	if ($debug) {print "command = $command\n";}
	open (GET, "$command |");
	while ($line = <GET>) {
	    $value[$index] = "\'".$line;
	    if ($debug) {print "value[$index] = $value[$index]\n";}
	    $index++;
          if ($item =~ m/comment/) {last;} 
	}
	chop $value[$index - 1]; # chop extra newline that get_map_char
                                 # "tosses in"
	 $value[$index-1] .="\'";
    }  else {
	die "Unknown type $type found. Exiting";
    }
    
    $constant_comment = "from Map file $mapfile";

    WriteConstantSet($dbh, $system, $subsystem, $item, $minRun,
		     $maxRun, $constant_comment, \$itemValueId, @value);
    if($debug) {print "WriteConstantSet($dbh, $system, $subsystem, $item, $minRun,
		     $maxRun, $constant_comment,  @value)\n ";}

    $index_comment = $constant_comment;
    
    LinkConstantSet($dbh, $system, $subsystem, $item, $runIndexDest, $minRun,
		    $maxRun, $itemValueId, $index_comment, $runIndexIdRef);
    if($verbose) {print "LinkConstantSet($dbh, $system, $subsystem, $item, $runIndexDest, $minRun,
		    $maxRun, $itemValueId, $index_comment, $runIndexIdRef)\n ";}


}

sub ProcessCommandLine {
    if (!(defined $hostname)) {
	$hostname = 'clasdb.jlab.org';
    }

    if (!(defined $mapfile)) {
	PrintUsage();
	die "\n error: mapfile not defined, exiting\n";
    }

    if (!$i) {
	print "\n warning: item not defined: copying all items\n";
    }

    if (!$ss) {
	print "\n warning: subsystem not defined: copying all subsystems\n";
    }

    if (!$minR) {
	die "\n error: minR not defined, exiting\n";
    }
    if (!$maxR or ($maxR < $minR) ) {
	die "\n error: maxR not defined, exiting\n";
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

    if (defined $help) {
	PrintUsage();
	exit 0;
    }
}

sub PrintUsage {
    print <<EOM;
usage:
 map2runIndex.pl mapfile=<name of map file> minR=<min run from map> maxR=<maximum run from map> rid=<destination RunIndex> [ss=<subsystem>] [i=<item>] [debug=<non-zero>] [help-<non-zero>]
[hostname=<hostname of MySQL server>]

copy particular item (all if not specified) of \\
subsystem (all if not specified) \\
of the map to corresponding system in the caldb from minimum \\
run (0 if not specified)\\
to maximum run (1000000 if not specified)

flag names:
  ss  for subsystem name
  i   for item name
  minR minimum run number for validity range
  maxR maximum run number for validity range
  rid or runIndexDest for destination run index table name


example:
 map2runIndex.pl mapfile=DC_DOCA.map  ss=t_max i=Sector6 minR=30300 maxR=30500 \\
                rid=calib_user.RunIndexe1_6DC \\
EOM
    return;
}

# end of perl script
