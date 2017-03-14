#!/usr/bin/env perl 
#
# $Id: mapfile2db.pl,v 2.9 2003/09/30 15:30:01 marki Exp $
#####################################################################
#
use lib ("$ENV{CLAS_TOOLS}/caldb");
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
if ($runIndex =~ /^calib_user\./) {
    $user = 'clasuser';
} else {
    $user = $ENV{USER};
}
$verbose = 1;

print "connecting to server at $hostname as user $user\n";
print "writing to run index table $runIndex\n";
print "from Map file $mapfile\n";
print "subsystem = $subsystem\n";
print "item = $item\n";
if ($allruns) {
    print "all runs will be copied\n";
} else {
    print "Map entries from $runMin to $runMax inclusive will be copied\n";
}
if ($nowrite) {
    print "writing is disabled\n";
} else {
    print "writing is enabled\n";
}
#
# prompt for confirmation
#
print "  Is that ok? (yes or no): ";
$answer = <STDIN>;
chomp $answer;
if ($answer ne 'yes') {
    die "Exiting per user request.\n";
}

ConnectToServer ($hostname, $user, $password, $database, $verbose, \$dbh);
#
# Scan the map files
# 
$author  = $user;
$comment = 'copied from Map';
$officer = $user;

# get the system name

$mapfile;
print "Working on map file $mapfile\n";
@field = split(/\//, $mapfile);
@subfield = split(/\.map/, $field[$#field]);
$system = $subfield[0];

# check for the scan_map program

if (! -f "$ENV{'CLAS_BIN'}/scan_map") {
    die "\$CLAS_BIN/scan_map not found. Exiting\n";
}

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
	$subsystem_this = $field[2];
	$subsystem_this =~ s/,//;
	if ($subsystem_this eq $subsystem || $subsystem eq 'allsubsystems') {
	    $nitems = $field[4];
	    print "  Working on subsystem $subsystem_this with $nitems items.\n";
	    
# loop over all items for this subsystem

	    for ($i = 0; $i < $nitems; $i++) {
		&PROCESS_ITEM();
	    }
	}
    }
}

if ($nowrite) {
    print "Writing disabled, no changes made\n";
}

exit 0;

sub PROCESS_ITEM {

# get the item information

    $line = <SCAN>;
    chop $line;
    @field = split(/:/, $line);
    @field1 = split(/,/, $field[1]);
    $item_this = $field1[0];
    $item_this =~ s/^\s+//;
    if ($debug) {print "item_this = /$item_this/\n";}
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
    @run = split(/\s+<--\s+/, $line);
    if ($debug) {print "    \@run = @run\n";}
	
    $length_db = $length_map;
    if ($length_map == 6912) {
	$type_db = $type . 'blob';
    } else {
	$type_db = $type;
    }

    if ($item_this eq $item || $item eq 'allitems') {
	print "    Item: $item_this, Length: $length_map, Type: $type,"
	    . " Sets: $nsets\n";
	for ($s = 1; $s <= $nsets; $s++) {
	    $runMax_map = $run[$s-1]-1;
	    $runMin_map = $run[$s];
	    if ($s == 1){$runMax_map = 1000000;}
	    if ($runMin_map == 'UNDEF') {$runMin_map = 0;}
	    if (($runMin_map >= $runMin && $runMin_map <= $runMax)
		|| ($runMax_map >= $runMin && $runMax_map <= $runMax)
		|| ($runMin_map <= $runMin && $runMax_map >= $runMax)) {
		if ($runMin_map < $runMin) {
		    $runMin_db = $runMin;
		} else {
		    $runMin_db = $runMin_map;
		}
		if ($runMax_map > $runMax) {
		    $runMax_db = $runMax;
		} else {
		    $runMax_db = $runMax_map;
		}
		print "      set=$s Map range: [$runMin_map,$runMax_map],"
		    . " CalDB range: [$runMin_db,$runMax_db]\n";
		if (!$nowrite) {
		    &PROCESS_SET();
		}
	    }
	}
    }
}

sub PROCESS_SET {
    $index = 0;
    @value = ();
    if ($type eq "float") {
	$command = "$ENV{'CLAS_BIN'}/get_map_float -m$mapfile -s$subsystem_this -i$item_this -t$run[$s] -l$length_map";
	if ($debug) {print "command = $command\n";}
	open (GET, "$command |");
	while ($line = <GET>) { 
	    chop $line;
	    $value[$index] = $line;
	    if ($debug) {print "        value[$index] = $value[$index]\n";}
	    $index++;
	} 
	
    } elsif ($type eq "int") {
	$command = "$ENV{'CLAS_BIN'}/get_map_int -m$mapfile -s$subsystem_this -i$item_this -t$run[$s] -l$length_map";
	if ($debug) {print "command = $command\n";}
	open (GET, "$command |");
	while ($line = <GET>) {
	    chop $line;
	    $value[$index] = $line;
	    if ($debug) {print "value[index] = $value[$index]\n";}
	    $index++;
	} 
	
    }  elsif ($type eq "char") {
	$command = "$ENV{'CLAS_BIN'}/get_map_char -m$mapfile -s$subsystem_this -i$item_this -t$run[$s] -l$length_map";
	if ($debug) {print "command = $command\n";}
	open (GET, "$command |");
	$string = '';
	while ($line = <GET>) {
	    $string .= $line;
	    if ($debug) {print "$line\n";}
	}
	chop $string; # get rid of extra newline that get_map_char
                  # "tosses in"
	$string =~ s/'/\\'/g; # escape any single quotes
	# set the zeroth value to the catenation of the lines read in with single quotes
	$value[0] = "'" . $string . "'";
	if ($debug) {print "value[0] = $value[0]\n";}
    }  else {
	die "Unknown type $type found. Exiting";
    }
    
    $constant_comment = "from Map file $mapfile";

	WriteConstantSet($dbh, $system, $subsystem_this, $item_this,
			 $runMin_map, $runMax_map, $constant_comment,
			 \$itemValueId, @value);
	
	$index_comment = $constant_comment;
	
	LinkConstantSet($dbh, $system, $subsystem_this, $item_this, $runIndex,
			$runMin_db, $runMax_db, $itemValueId,
			$index_comment, $runIndexIdRef);

}

sub ProcessCommandLine {
    if (defined $help) {
	PrintUsage();
	exit 0;
    }
    if (!(defined $mapfile)) {
	if ($m) {
	    $mapfile = $m;
	} else {
	    PrintUsage();
	    die "ERROR: mapfile not defined, exiting\n";
	}
    }
    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	} else {
	    PrintUsage();
	    die "ERROR: no subsystem defined\n";
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	} else {
	    PrintUsage();
	    die "ERROR: no item defined\n";
	}
    }
    if (!(defined $hostname)) {
	$hostname = 'clasdb.jlab.org';
    }
    if (!(defined $runIndex)) {
	if ($ri) {
	    $runIndex = $ri;
	} else {
	    $runIndex = 'RunIndex';
	}
    }
    if (!defined($runMin)) {
	if (defined($min)) {
	    $runMin = $min;
	}
    }
    if (!defined($runMax)) {
	if (defined($max)) {
	    $runMax = $max;
	}
    }
    if ($allruns) { # copy all runs
	if (defined($runMin) || defined($runMax)) {
	    PrintUsage();
	    die "ERROR: runMin or runMax cannot be defined with allruns\n";
	} else {
	    $runMin = -1000000;
	    $runMax = 1000000;
	}
    } else {
	if (!defined($runMin) && !defined($runMax)) {
	    PrintUsage();
	    die "ERROR: either allruns or minRun/maxRun must be defined\n";
	} elsif (!defined($runMin) || !defined($runMax)) {
	    PrintUsage();
	    die "ERROR: both runMin and runMax must be defined\n";
	} else {
	    if ($runMax < $runMin) {
		PrintUsage();
		die "ERROR: runMin must be less than or equal to runMax\n";
	    }
	}
    }
}

sub PrintUsage {
    print <<EOM;
Purpose: copy constants from a single Map file into the CalDB.

Note: use nowrite=1 to practice using the script.

Usage:
mapfile2db.pl mapfile|m=<name of map file> \\
    subsystem|ss=<subsystem name|allsubsystems> item|i=<item name|allitems> \\
    [allruns=<non-zero>}] [runMin|min=<minimum run number>] \\
    [runMax|max=<maximum run number>] \\
    [runIndex|ri=<name of target run index>] \\
    [hostname=<hostname of MySQL server>] [nowrite=<non-zero>] \\
    [help-<non-zero>]

Example:
mapfile2db.pl mapfile=/group/clas/parms/Maps/DC_DOCA.map \\
    subsystem=t_max item=Sector6 runIndex=calib_user.RunIndexJunk \\
    min=2063 max=8099
EOM
    return;
}

# end of perl script
