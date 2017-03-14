#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunPDU;
}

sub RunPDU {

# Runs the monitoring program pdu. It parses the output
# for problems and important information for the offline 
# database
#
# Usage: RunPDU($files, $Outpath, $db_file, $basename, $db_mysql)
#
# Inputs: $files          = List of files to be processed
#         $Outpath        = Path where the ouput file should be copied
#         $db_file        = Database file including path
#         $basename       = Basename for output files
#         $db_mysql      = Flag to use the MYSQL db instead of the perl db.
    
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
    local($db_msyql) = $_[4];

    local($os) = $ENV{"OSNAME"};
    local($clas_bin) = $ENV{"CLAS_BIN"};

    if ($clas_bin eq "") {
	$clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
    }

    $pdu_cmd = "$clas_bin/pdu -b -n60000 -o$basename $files";

    print  "\n$0 EXECUTE: $pdu_cmd at $start_time\n";

    if (!$db_mysql) {
# Open the PERL database
      dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";
    
      $RUN{"pdu command"} = $pdu_cmd;
    }

    open(PDU, "$pdu_cmd 2>&1 |") || warn "$0: Could not open $pdu_cmd\n";

    while (<PDU>) {
	print $_;
	if (/^pdu: detected pathologies for run/) {
	    $junk = <PDU>;
	    $sector = 1;
	    $region = 1;
	    while (<PDU>) {
		print $_;
		chop;
		$_ =~ s/^\s+//;
		@line = split(/\s+/, $_);
		$dead = "pdu_s".$sector."r".$region."_dead";
		if (!$db_mysql) {
		  $RUN{"$dead"} = $line[2];
		}
		$hot = "pdu_s".$sector."r".$region."_hot";
		if (!$db_mysql) {
		  $RUN{"$hot"} = $line[4];
		}
		if ($region * $sector == 18) {
		    last;
		}
		$region += 1;
		if ($region == 4) {
		    $sector += 1;
		    $region = 1;
		}
	    }
	}
	if (/^ \*\*\* Stop reading /) {
	    print "\n ** $0: Problem with pdu.\n";
	    $error = 1;
	    last;
	}       
    }

    close(PDU);

    if (!$db_mysql) {
      dbmclose(%RUN);
    }
    if ($error) {
	print "\n$0: No files moved due to problem with pdu. \n";
    } else {
	MoveFile("pdu_*.hbk", $OutPath, "pdu histo file", $db_file);
	MoveFile("pdu_*.out", $OutPath, "pdu out file ", $db_file);
	MoveFile("pdu_wire_*", $OutPath, "pdu wire file", $db_file);
    }

    $t = time();
    $end_time = &ctime($t);

    print "\n$0 FINISHED: $pdu_cmd at $end_time\n";

}

1;
