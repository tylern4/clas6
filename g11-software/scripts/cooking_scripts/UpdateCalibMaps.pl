#!/usr/local/bin/perl

# Author     : Simeon McAleer
# Institution: Flortida State University
# Date       : July 27, 1999

######## Must be run on linux machine ###############
#
#Usage: UpdateCalibMaps.pl [-h][-t#][-f<string>][-d<dbdir>][-pdu<string>]
#        -h              Print this help message.
#        -f<string>      Intermediate file name needed for put_map command
#                        (default = alibaba).
#        -d<dbdir>       Path to database directory.
#        -pdu<string>    PDU wire file including path to use for DC_STATUS. 
#        -t#             Run number to use as time stamp for DC_STATUS.
#        -P[0x#]         Choose maps to update.
#                0x1     Update Geometry map with x,y, and z beam positions.
#                0x2     Update Sync map with number of synchronization bits.
#                0x4     Update DC_STATUS map from pdu wire file. 
#
#                        If option -P0x4 is selected a pdu_wire file must
#                        be identified with the -pdu command.
#
########### Define the variables for processing############ 

$PROCESS_BEAM                 = 0x1;
$PROCESS_SYNC                 = 0x2;
$PROCESS_DC                   = 0x4;

$clas_parms = $ENV{"CLAS_PARMS"};

# Needlessly Initialize all variables
$beam = 0;
$sync   = 0;
$dc = 0;
$run_to_update = "";
@run_list = ();
$pdu_dir = "";
$file_name = "alibaba";
############### End initialization

# Parse through command lines

foreach (@ARGV) {
    if (/^-P/) {
	$ProcessBit = $_;
	$ProcessBit =~ s/^-P//;
	$ProcessBit = hex $ProcessBit;
	print "Processing 0x$ProcessBit.\n";
    } elsif (/^-d/) {
	$dbdir = $_;
	$dbdir =~ s/^-d//;
	print "$0: Database directory is $dbdir.\n";
    } elsif (/^-f/) {
	$file_name = $_;
	$file_name =~ s/^-f//;
	print "$0: Using intermediate file called $file_name.\n";
    } elsif (/^-pdu/) {
	$pdu_wire_file = $_;
	$pdu_wire_file =~ s/^-pdu//;
	print "$0: Pdu file to use is $pdu_wire_file.\n";
    } elsif (/^-t/) {
	$run_to_update = $_;
	$run_to_update =~ s/^-t//;
	print "$0: Going to update DC_Status map at time $run_to_update.\n";
    } elsif (/^-h/) {
	&PrintUsage;
	exit(0);
    } elsif (/^-/) {
	print "Option $_ not understood.\n";
	&PrintUsage;
	exit(0);
    }
}

if ($ProcessBit & $PROCESS_BEAM) {
    $beam = 1;
}
if ($ProcessBit & $PROCESS_SYNC) {
    $sync = 1;
}
if ($ProcessBit & $PROCESS_DC) {
    $dc = 1;
}

###### Start Processing ####

if ($sync || $beam) {

    $lscmd = "ls -1 $dbdir/*.db |";

    open(LS, $lscmd) || die "\n$0: $!\n";

    while (<LS>) {
	$run = $_;
	print $run;
	chop($run);
	$run =~ s/\.db$//;
	dbmopen(%RUN, "$run", 0644) || warn "$0: Couldn't open $run\n";
	$run_num = $run;
	$run_num =~ s/$dbdir\/+clas_0*//;
	($run_num =~ s/\.A\d\d.*$//) || next ;
	
	if ($run_num != $run_list[$#run_list]) {
	    push(@run_list, $run_num);
	}
	if ($beam) {
	    $BEAMX{$run_num} += $RUN{"Beam x position"} * $RUN{"3 track events"};
	    $BEAMY{$run_num} += $RUN{"Beam y position"} * $RUN{"3 track events"};
	    $BEAMZ{$run_num} += $RUN{"Beam z position"} * $RUN{"3 track events"};
	    $BEAM3EVTS{$run_num} += $RUN{"3 track events"};
	}
	if ($sync) {
	    $SYNCTOT{$run_num} += $RUN{"Total Sync Events"};
	    $SYNCBAD{$run_num} += $RUN{"# of Bad Sync Events"};
	}
	dbmclose(%RUN);
    }
    
    close(LS);
}

if ($sync) { 
    &UpdateSyncMap;
}

if ($beam) {
    &UpdateBeamMap;
}

if($dc) {
    &UpdateDCSTATUSMap;
}

sub UpdateSyncMap {

    if (-e "$clas_parms/Maps/SYNC.map" && -w "$clas_parms/Maps/SYNC.map") {
	foreach $run (@run_list) {

# Update SYNC map with total sync events

	    &RemoveMapArr("$clas_parms/Maps/SYNC.map", "sync", "total", $run);
	    &PutMapItem("$clas_parms/Maps/SYNC.map", "sync", "total", $run, "int", $SYNCTOT{$run}, $file_name);

# Update SYNC map with bad sync events
	    &RemoveMapArr("$clas_parms/Maps/SYNC.map", "sync", "bad", $run);
	    &PutMapItem("$clas_parms/Maps/SYNC.map", "sync", "bad", $run, "int", $SYNCBAD{$run}, $file_name);
	}

    } else {
	print "Couldn't do anything with the map\n";
    }
}


sub UpdateBeamMap {
 
   if (-e "$clas_parms/Maps/GEOMETRY.map" && -w "$clas_parms/Maps/GEOMETRY.map") {
	foreach $run (@run_list) {
	    &RemoveMapArr("$clas_parms/Maps/GEOMETRY.map", "beam", "position", $run);
	    $x_pos =  $BEAMX{$run_num} / $BEAM3EVTS{$run_num};
	    $y_pos =  $BEAMY{$run_num} / $BEAM3EVTS{$run_num};
	    $z_pos =  $BEAMZ{$run_num} / $BEAM3EVTS{$run_num};
	    $position = "$x_pos $y_pos $z_pos";
	    &PutMapItem("$clas_parms/Maps/SYNC.map", "beam", "position", $run, "float", $position, $file_name);
	}
    } else {
	print "Couldn't do anything with the map\n";
    }	
}

sub UpdateDCSTATUSMap{
#
# Initializes a set of wires as those that do not exist.
# Reads a user specified pdu_wire file which lists the dead and hot wires.
# Places the status of each drfit chamber wire into the DC_STATUS.map
#

# Initialize standard definitions from /packages/include/status.h
    $DC_DNE = "-2";
    $DC_BAD = "1";
    $DC_HOT = "100";

    $LAYERS = "36";
    $WIRESPERLAYER = "192";
    

# Initialize all of the wires as good

    for ($i = 1; $i < 6913; $i++) {
	$sector1_wires[$i] = 0;
	$sector2_wires[$i] = 0;
	$sector3_wires[$i] = 0;
	$sector4_wires[$i] = 0;
	$sector5_wires[$i] = 0;
	$sector6_wires[$i] = 0;
    }

# Initialize the drift chamber wire limits taken from /packages/utilities/pdu/pduInit.c

    $WireLimits{"l1 min"} = $WireLimits{"l7 min"} = "4";
    $WireLimits{"l2 min"} = $WireLimits{"l3 min"} = $WireLimits{"l8 min"} = $WireLimits{"l9 min"} = "3";
    $WireLimits{"l4 min"} = $WireLimits{"l10 min"} = $WireLimits{"l11 min"} = "2";
    $WireLimits{"l12 min"} = "1";

    for ($L = 1; $L < 6; $L++) {
	$WireLimits{"l$L max"} = "130";
    }
    for ($L = 7; $L < 10; $L++) {
	$WireLimits{"l$L max"} = "142";
    }
    $WireLimits{"l10 max"} = "126";
    $WireLimits{"l11 max"} = "121";
    $WireLimits{"l12 max"} = "120";

    $WireLimits{"l5 min"} = $WireLimits{"l5 max"} = $WireLimits{"l6 min"} = $WireLimits{"l6 max"} = "0";

    for ($L = 13; $L < 25; $L++) {
	$WireLimits{"l$L min"} = "1";
    }
    
    $WireLimits{"l13 max"} = "184";
    $WireLimits{"l14 max"} = "185";
    $WireLimits{"l15 max"} = "186";
    $WireLimits{"l16 max"} = "187";
    $WireLimits{"l17 max"} = "188";
    $WireLimits{"l18 max"} = $WireLimits{"l19 max"} = $WireLimits{"l20 max"} = "189";
    $WireLimits{"l21 max"} = "190";
    $WireLimits{"l22 max"} = "191";
    $WireLimits{"l23 max"} = $WireLimits{"l24 max"} = "192";

    for ($L = 25; $L < 37; $L++) {
	$WireLimits{"l$L min"} = "1";
	$WireLimits{"l$L max"} = "192";
    }

# Flag wires past DC wire limits as DNE

    for ($L = 1; $L <= $LAYERS; $L++) {
	for ($W = 1; $W < $WireLimits{"l$L min"}; $W++) {
	    $wire = ($L - 1) * $WIRESPERLAYER + $W;
	    $sector1_wires[$wire] = $DC_DNE;
	    $sector2_wires[$wire] = $DC_DNE;
	    $sector3_wires[$wire] = $DC_DNE;
	    $sector4_wires[$wire] = $DC_DNE;
	    $sector5_wires[$wire] = $DC_DNE;
	    $sector6_wires[$wire] = $DC_DNE;
	}

	for ($W = $WireLimits{"l$L max"} + 1; $W <= $WIRESPERLAYER; $W++) {
	    $wire = ($L - 1) * $WIRESPERLAYER + $W;
	    $sector1_wires[$wire] = $DC_DNE;
	    $sector2_wires[$wire] = $DC_DNE;
	    $sector3_wires[$wire] = $DC_DNE;
	    $sector4_wires[$wire] = $DC_DNE;
	    $sector5_wires[$wire] = $DC_DNE;
	    $sector6_wires[$wire] = $DC_DNE;
	}
    }

    for ($L = 5; $L <=6; $L++) { 
	for ($W = 1; $W <= $WIRESPERLAYER; $W++) {
	    $wire = ($L - 1) * $WIRESPERLAYER + $W;
	    $sector1_wires[$wire] = $DC_DNE;
	    $sector2_wires[$wire] = $DC_DNE;
	    $sector3_wires[$wire] = $DC_DNE;
	    $sector4_wires[$wire] = $DC_DNE;
	    $sector5_wires[$wire] = $DC_DNE;
	    $sector6_wires[$wire] = $DC_DNE;
	}
    }

# Get dead wires from pdu wire file

    if (-e "$pdu_wire_file" && -r "$pdu_wire_file") {
	open(WIRE, "$pdu_wire_file");
	while (<WIRE>) {
	    if ($_ =~ /^(\d)\s+(\d+)\s+(\d+)\s+(\d+)/) {
		$region = $1;
		$layer = $2;
		$wire_in_region = $3;
		$status = $4;
		if ($status == 1) {
		    if ($region == 1) {
			$wire = ($layer - 1) * $WIRESPERLAYER + $wire_in_region;
			$sector1_wires[$wire] = $status;
		    }
		    if ($region == 2) {
			$wire = ($layer - 1) * $WIRESPERLAYER + $wire_in_region;
			$sector2_wires[$wire] = $status;
		    }
		    if ($region == 3) {
			$wire = ($layer - 1) * $WIRESPERLAYER + $wire_in_region;
			$sector3_wires[$wire] = $status;
		    }
		    if ($region == 4) {
			$wire = ($layer - 1) * $WIRESPERLAYER + $wire_in_region;
			$sector4_wires[$wire] = $status;
		    }
		    if ($region == 5) {
			$wire = ($layer - 1) * $WIRESPERLAYER + $wire_in_region;
			$sector5_wires[$wire] = $status;
		    }
		    if ($region == 6) {
			$wire = ($layer - 1) * $WIRESPERLAYER + $wire_in_region;
			$sector6_wires[$wire] = $status;
		    }
		}
	    }
	}
    }

    $sector1 = join(" ", @sector1_wires);
    $sector2 = join(" ", @sector2_wires);
    $sector3 = join(" ", @sector3_wires);
    $sector4 = join(" ", @sector4_wires);
    $sector5 = join(" ", @sector5_wires);
    $sector6 = join(" ", @sector6_wires);
    
    if (-e "$clas_parms/Maps/DC_STATUS.map" && -w "$clas_parms/Maps/DC_STATUS.map") {
	&RemoveMapArr("$clas_parms/Maps/DC_STATUS.map", "sector1", "status", $run_to_update);
	&RemoveMapArr("$clas_parms/Maps/DC_STATUS.map", "sector2", "status", $run_to_update);
	&RemoveMapArr("$clas_parms/Maps/DC_STATUS.map", "sector3", "status", $run_to_update);
	&RemoveMapArr("$clas_parms/Maps/DC_STATUS.map", "sector4", "status", $run_to_update);
	&RemoveMapArr("$clas_parms/Maps/DC_STATUS.map", "sector5", "status", $run_to_update);
	&RemoveMapArr("$clas_parms/Maps/DC_STATUS.map", "sector6", "status", $run_to_update);
	PutMapItem("$clas_parms/Maps/DC_STATUS.map", "sector1", "status", $run_to_update, "int", $sector1, $file_name);
	PutMapItem("$clas_parms/Maps/DC_STATUS.map", "sector2", "status", $run_to_update, "int", $sector2, $file_name);
	PutMapItem("$clas_parms/Maps/DC_STATUS.map", "sector3", "status", $run_to_update, "int", $sector3, $file_name);
	PutMapItem("$clas_parms/Maps/DC_STATUS.map", "sector4", "status", $run_to_update, "int", $sector4, $file_name);
	PutMapItem("$clas_parms/Maps/DC_STATUS.map", "sector5", "status", $run_to_update, "int", $sector5, $file_name);
	PutMapItem("$clas_parms/Maps/DC_STATUS.map", "sector6", "status", $run_to_update, "int", $sector6, $file_name);
    } else {
	print "Couldn't do anything with the map\n";
    }	

}


sub RemoveMapArr {
#
# Wrapper for rem_map_arr
#
 
    local($map) = $_[0];
    local($system) = $_[1];
    local($item) = $_[2];
    local($time) = $_[3];

    $rm_cmd = "$ENV{'CLAS_BIN'}/rem_map_arr -m$map -s$system -i$item -t$time ";
    print "$0: Performing $rm_cmd. \n";
    system($rm_cmd);
}

sub PutMapItem {
#
# Wrapper for put_map
#
    local($map) = $_[0];
    local($system) = $_[1];
    local($item) = $_[2];
    local($time) = $_[3];
    local($type) = $_[4];
    local($values) = $_[5];
    local($file) = $_[6];

    @val_list = split(/\s+/,$values);

    if (-e $file) {
	die "\n$0: Intermediate file $file already exists. Try a new file name with the -f flag.\n";
    } else {
	open(JUNK, ">$file") || die "\n$0: Couldn't open $file\n";
    }

    foreach $value (@val_list) {
	print JUNK "$value\n";
    }

    close(JUNK);

    $put_cmd = "$ENV{'CLAS_BIN'}/put_map_$type -m$map -s$system -i$item -t$time < $file";
    print "$0: Performing $put_cmd. \n";
    system($put_cmd);

    system("rm $file") && die "\n$0: Couldn't remove the file $file\n";

}   


sub PrintUsage {

    print "\nUsage: $0 [-h][-t#][-f<string>][-d<dbdir>][-pdu<string>]\n";
    print "\t-h \t\tPrint this help message.\n";
    print "\t-f<string>\tIntermediate file name needed for put_map command\n\t\t\t(default = alibaba).\n";
    print "\t-d<dbdir> \tPath to database directory.\n";
    print "\t-pdu<string> \tPDU wire file including path to use for DC_STATUS. \n";
    print "\t-t# \t\tRun number to use as time stamp for DC_STATUS.\n";
    print "\t-P[0x#]  \tChoose maps to update.\n";
    print "\t\t0x1 \tUpdate Geometry map with x,y, and z beam positions.\n";
    print "\t\t0x2 \tUpdate Sync map with number of synchronization bits.\n";
    print "\t\t0x4 \tUpdate DC_STATUS map from pdu wire file. \n";
    print "\n\t\t\tIf option -P0x4 is selected a pdu_wire file must\n";
    print "\t\t\tbe identified with the -pdu command.\n\n";

}
