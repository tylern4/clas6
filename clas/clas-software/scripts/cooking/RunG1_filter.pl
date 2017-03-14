#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunG1_filter;
}

sub RunG1_filter {

# Runs the filtering program g_filter. It parses the
# output for important database information.
#
# Usage: Rung1_filter($files, $Outpath, $db_file, $basename)
#
# Inputs: $files          = List of files to be processed
#         $Outpath        = Path where the ouput files should be copied
#         $db_file        = Database file including path
#         $basename       = Basename for output files
    
    require "ctime.pl";
    $perl_lib = "$ENV{'CLAS_SCRIPTS'}";
    require "$perl_lib/MoveFile.pl";
    
    local($t) = time();
    local($start_time) = &ctime($t);
    local($end_time);

    local($files)   = $_[0];
    local($OutPath) = $_[1];
    local($db_file) = $_[2];
    local($basename) = $_[3];

    local($os) = $ENV{"OSNAME"};
    local($clas_bin) = $ENV{"CLAS_BIN"};

    if ($clas_bin eq "") {
	$clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
    }

    $g1filter_cmd = "$clas_bin/g1_filter -i -R1 -ofilter$basename $files";

    print "\n$0 EXECUTE: $g1filter_cmd at $start_time\n";

    dbmopen(%RUN,$db_file, 0644) || print "$0: Could not open $db_file for offline db\n";

    $RUN{"g1_filter command"} = $g1filter_cmd;

    open(FILT, "$g1filter_cmd 2>&1 |") || warn "DB: Could not open $g1filter_cmd.\n";

    while (<FILT>) {
	print $_;
	if ($_ =~ /\# of events written:\s+(\d+)/) {
	    $RUN{"K filtered events"} = $1;
	    $RUN{"Kmiss filtered events"} = $2;
	    $RUN{"KPE filtered events"} = $3;
	    $RUN{"Phys filtered events"} = $4;
	    $RUN{"Nmiss filtered events"} = $5;
	    $RUN{"Pimiss filtered events"} = $6;
	    $RUN{"Eta filtered events"} = $7;
	    $RUN{"PKpKm filtered events"} = $8;
	}
	
	if ($_ =~ /hadron mass squared cut\(s\)/) {
	    $line = <FILT>;
	    print $line;
	    if ($line =~ /^Lower/) {
		$line =~ /\s+.*:\s+(\d+.\d+)/;
		$RUN{"g1filter hadron mass min cut"} = $1;
	    }
	    $line = <FILT>;
	    print $line;
	    if ($line =~ /^Upper:/) {
		$line =~ /\s+.*:\s+(\d+.\d+)/;
		$RUN{"g1filter hadron mass max cut"} = $1;
	    }
	}
	
    }

    close(FILT);

    dbmclose(%RUN);

    $filterpath="$OutPath/K";
    CheckDir("$filterpath");
    MoveFile("filter*.K", $filterpath, "g1filtered K file", $db_file);
    system("rm filter*.K");

    $filterpath="$OutPath/Kmiss";
    CheckDir("$filterpath");
    MoveFile("filter*.Kmiss", $filterpath, "g1filtered Kmiss file", $db_file);
    system("rm filter*.Kmiss");

    $filterpath="$OutPath/KPE";
    CheckDir("$filterpath");
    MoveFile("filter*.KPE", $filterpath, "g1filtered KPE file", $db_file);
    system("rm filter*.KPE");

    $filterpath="$OutPath/Phys";
    CheckDir("$filterpath");
    MoveFile("filter*.Phys", $filterpath, "g1filtered Phys file", $db_file);
    system("rm filter*.Phys");

    $filterpath="$OutPath/Nmiss";
    CheckDir("$filterpath");
    MoveFile("filter*.Nmiss", $filterpath, "g1filtered Nmiss file", $db_file);
    system("rm filter*.Nmiss");

    $filterpath="$OutPath/Pimiss";
    CheckDir("$filterpath");
    MoveFile("filter*.Pimiss", $filterpath,"g1filtered Pimiss file", $db_file);
    system("rm filter*.Pimiss");

    $filterpath="$OutPath/Eta";
    CheckDir("$filterpath");
    MoveFile("filter*.Eta", $filterpath, "g1filtered Eta file", $db_file);
    system("rm filter*.Eta");

    $filterpath="$OutPath/PKpKm";
    CheckDir("$filterpath");
    MoveFile("filter*.PKpKm", $filterpath, "g1filtered PKpKm file", $db_file);
    system("rm filter*.PKpKm");

#   take care of the leftovers
    CheckDir("$filterpath");
    $filterpath="$OutPath";
    MoveFile("filter*", $OutPath, "g1filtered file", $db_file);

    $t = time();
    $end_time = &ctime($t);

    print "\n$0 FINISHED: $g1filter_cmd at $end_time\n";

    print "\n\n";

}

1;
