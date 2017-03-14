#!/apps/bin/perl

use lib ("$ENV{CLAS_TOOLS}/perl");
use Csql;

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunPid_mon;
}

sub RunPid_mon {

# Runs the monitoring program pid_mon. It parses the output
# text file for important database information.
#
# Usage: RunPid_mon($files, $Outpath, $db_file, $basename, $seb, $db_mysql)
#
# Inputs: $files          = List of files to be processed
#         $Outpath        = Path where the ouput file should be copied
#         $db_file        = Database file including path
#         $basename       = Basename for output files
#         $seb            = Flag for whether you want SEB particle counts
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
    local($seb) = $_[4];
    local($db_mysql) = $_[5];

    local($os) = $ENV{"OSNAME"};
    local($clas_bin) = $ENV{"CLAS_BIN"};

    if ($clas_bin eq "") {
	$clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
    }

    $pid_mon_cmd = "$clas_bin/pid_mon -d -b -opid$basename.hbook $files > pid$basename.txt";

    print  "\n$0 EXECUTE: $pid_mon_cmd at $start_time\n";

    system($pid_mon_cmd);

#MYSQL DATABASE INSERTION:

    if ($db_mysql) {
       
       if (-r "pid$basename.txt") { 
	open(PID_TXT, "pid$basename.txt") || print "$0: Could not open pid$basename.txt\n";
      }
    
      while (<PID_TXT>) {

        #Inserting values for HB pos:
       
	$newstring = "\\s*"."HB pos" . ":\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)";
	if (@fields = ($_ =~ /$newstring/)){
	    $i = 0;
	    foreach $f (@fields){
		$i++;
		@row = (
                          {
                             name => "HBpos_s" . $i,
		             value => $f
                             }
			);
                UpdateRow(@row);
	    }
	}

        #Inserting values for HB neg:
       
	$newstring = "\\s*"."HB neg" . ":\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)";
	if (@fields = ($_ =~ /$newstring/)){
	    $i = 0;
	    foreach $f (@fields){
		$i++;
		@row = (
                          {
                             name => "HBneg_s" . $i,
		             value => $f
                             }
			);
                UpdateRow(@row);
	    }
	}

        #Inserting values for TB pos:
       
	$newstring = "\\s*"."TB pos" . ":\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)";
	if (@fields = ($_ =~ /$newstring/)){
	    $i = 0;
	    foreach $f (@fields){
		$i++;
		@row = (
                          {
                             name => "TBpos_s" . $i,
		             value => $f
                             }
			);
                UpdateRow(@row);
	    }
	}
        #Inserting values for TB neg:
       
	$newstring = "\\s*"."TB neg" . ":\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)";
	if (@fields = ($_ =~ /$newstring/)){
	    $i = 0;
	    foreach $f (@fields){
		$i++;
		@row = (
                          {
                             name => "TBneg_s" . $i,
		             value => $f
                             }
			);
                UpdateRow(@row);
	    }
	}

        #Inserting values for Chi2 pos:
       
	$newstring = "\\s*"."Chi2 pos" . ":\\s*(\\w+.\\w+)\\s*(\\w+.\\w+)\\s*(\\w+.\\w+)\\s*(\\w+.\\w+)\\s*(\\w+.\\w+)\\s*(\\w+.\\w+)";
	if (@fields = ($_ =~ /$newstring/)){
	    $i = 0;
	    foreach $f (@fields){
		$i++;
		@row = (
                          {
                             name => "Chi2pos_s" . $i,
		             value => $f
                             }
			);
                UpdateRow(@row);
	    }
	}

        #Inserting values for Chi2 neg:
       
	$newstring = "\\s*"."Chi2 neg" . ":\\s*(\\w+.\\w+)\\s*(\\w+.\\w+)\\s*(\\w+.\\w+)\\s*(\\w+.\\w+)\\s*(\\w+.\\w+)\\s*(\\w+.\\w+)";
	if (@fields = ($_ =~ /$newstring/)){
	    $i = 0;
	    foreach $f (@fields){
		$i++;
		@row = (
                          {
                             name => "Chi2neg_s" . $i,
		             value => $f
                             }
			);
                UpdateRow(@row);
	    }
	}
	    
        #Inserting values for the rest:

	sectorfieldSQL($_, "Unknown");
	sectorfieldSQL($_, "Gamma");
        sectorfieldSQL($_, "Positron");
	sectorfieldSQL($_, "Electron");
	sectorfieldSQL($_, "PiPlus");
	sectorfieldSQL($_, "PiMinus");
	sectorfieldSQL($_, "Proton");
	sectorfieldSQL($_, "KPlus");
	sectorfieldSQL($_, "KMinus");
	sectorfieldSQL($_, "Deuteron");
	sectorfieldSQL($_, "Neutron");
	
	partfieldSQL($_, "Unknown", 0);
	partfieldSQL($_, "Gamma", 1);
        partfieldSQL($_, "Positron", 2);
	partfieldSQL($_, "Electron", 3);
	partfieldSQL($_, "PiPlus", 8);
	partfieldSQL($_, "PiMinus", 9);
	partfieldSQL($_, "Proton", 14);
	partfieldSQL($_, "KPlus", 11);
	partfieldSQL($_, "KMinus", 12);
	partfieldSQL($_, "Deuteron", 45);
	partfieldSQL($_, "Neutron", 13);
    }

      close(PID_TXT);

   }
      

#PERL DATABASE INSERTION:

    if(!$db_mysql){

      dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";

      $RUN{"pid PART command"} = $pid_mon_cmd;

      if (-r "pid$basename.txt") { 
	open(PID_TXT, "pid$basename.txt") || print "$0: Could not open pid$basename.txt\n";
      }
    
      while (<PID_TXT>) {
	sectorfield($_, "HB pos");
	sectorfield($_, "HB neg");
	sectorfield($_, "TB neg");
	sectorfield($_, "TB pos");
	sectorfield($_, "Chi2 pos");
	sectorfield($_, "Chi2 neg");
	sectorfield($_, "Unknown");
	sectorfield($_, "Gamma");
        sectorfield($_, "Positron");
	sectorfield($_, "Electron");
	sectorfield($_, "PiPlus");
	sectorfield($_, "PiMinus");
	sectorfield($_, "Proton");
	sectorfield($_, "KPlus");
	sectorfield($_, "KMinus");
	sectorfield($_, "Deuteron");
	sectorfield($_, "Neutron");
	
	partfield($_, "Unknown", 0);
	partfield($_, "Gamma", 1);
        partfield($_, "Positron", 2);
	partfield($_, "Electron", 3);
	partfield($_, "PiPlus", 8);
	partfield($_, "PiMinus", 9);
	partfield($_, "Proton", 14);
	partfield($_, "KPlus", 11);
	partfield($_, "KMinus", 12);
	partfield($_, "Deuteron", 45);
	partfield($_, "Neutron", 13);
      }

      close(PID_TXT);

      dbmclose(%RUN);
    }

    MoveFile("pid*txt", $OutPath, "pid_mon text file", $db_file);
    MoveFile("pid*hbook", $OutPath, "pid_mon hbook file", $db_file);

#PERL DATABASE INSERTION:

    if ($seb) {
    
	$pid_mon_seb_cmd = "$clas_bin/pid_mon -Pf -oseb$basename.hbook -S -b $files > seb$basename.txt";

	print  "\n$0 EXECUTE: $pid_mon_seb_cmd at $start_time\n";

	system($pid_mon_seb_cmd);

	if(!$db_mysql){

	  dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";

	  $RUN{"pid SEB command"} = $pid_mon_seb_cmd;

	  if (-r "seb$basename.txt") { 
	    open(PID_TXT, "seb$basename.txt") || print  "$0: Could not open seb$basename.txt\n";
	  }
	
	  while (<PID_TXT>) {

            sectorfield($_, "HB pos", $seb);
	    sectorfield($_, "HB neg", $seb);
	    sectorfield($_, "TB neg", $seb);
	    sectorfield($_, "TB pos", $seb);
	    sectorfield($_, "Chi2 pos", $seb);
	    sectorfield($_, "Chi2 neg", $seb);
	    sectorfield($_, "Unknown", $seb);
	    sectorfield($_, "Gamma", $seb);
            sectorfield($_, "Positron", $seb);
	    sectorfield($_, "Electron", $seb);
	    sectorfield($_, "PiPlus", $seb);
	    sectorfield($_, "PiMinus", $seb);
	    sectorfield($_, "Proton", $seb);
	    sectorfield($_, "KPlus", $seb);
	    sectorfield($_, "KMinus", $seb);
	    sectorfield($_, "Deuteron", $seb);
	    sectorfield($_, "Neutron", $seb);

	    partfield($_, "Unknown", 0, $seb);
	    partfield($_, "Gamma", 1, $seb);
            partfield($_, "Positron", 2, $seb);
	    partfield($_, "Electron", 3, $seb);
	    partfield($_, "PiPlus", 8, $seb);
	    partfield($_, "PiMinus", 9, $seb);
	    partfield($_, "Proton", 14, $seb);
	    partfield($_, "KPlus", 11, $seb);
	    partfield($_, "KMinus", 12, $seb);
	    partfield($_, "Deuteron", 45, $seb);
	    partfield($_, "Neutron", 13, $seb);
	  }

	  close(PID_TXT);

	  dbmclose(%RUN);
	}

    MoveFile("seb*txt", $OutPath, "seb_mon text file", $db_file);
    MoveFile("seb*hbook", $OutPath, "seb_mon hbook file", $db_file);

    }    

    $t = time();
    $end_time = &ctime($t);

    print "\n$0 FINISHED: $pid_mon_cmd at $end_time\n";

    print "\n\n";


    sub sectorfieldSQL{
	
	local($check) = $_[0];
	local($string) = $_[1];
	local($seb) = $_[2];
	local($newstring);
	$newstring = "\\s*". $string . ":\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)";
	if (@fields = ($check =~ /$newstring/)){
	    $i = 0;
	    foreach $f (@fields){
		$i++;
		@row = (
                          {
                             name => $string . "_s" . $i,
		             value => $f
                             }
			);
                UpdateRow(@row);
	    }
	}
    }

    sub partfieldSQL{
    
	local($check) = $_[0];
	local($string) = $_[1];
	local($number) = $_[2];
	local($seb) = $_[3];
	local($newstring);
	
	$newstring = "\\s*" . $string . "\\s*" . $number . "\\s*(\\w+)\\s*(\\w+.\\w+)";
	if ($check =~ /$newstring/){
	    @row = (
                     {
                        name => "n_" . $string,
	                value => $1
                        },
		     {
                        name => "qtrk_" . $string,
	                value => $2
                        }
		    );
	    UpdateRow(@row);
	}
    }

     sub sectorfield{
	
	local($check) = $_[0];
	local($string) = $_[1];
	local($seb) = $_[2];
	local($newstring);
	$newstring = "\\s*". $string . ":\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)\\s*(\\w+)";
	if (@fields = ($check =~ /$newstring/)){
	    $i = 0;
	    foreach $f (@fields){
		$i++;
		$key = $string . "_s" . $i;
		if($seb) { $key = "seb_" . $key  }
		$RUN{$key} = $f;
	    }
	}
    }

    sub partfield{
    
	local($check) = $_[0];
	local($string) = $_[1];
	local($number) = $_[2];
	local($seb) = $_[3];
	local($newstring);
	
	$newstring = "\\s*" . $string . "\\s*" . $number . "\\s*(\\w+)\\s*(\\w+.\\w+)";
	if ($check =~ /$newstring/){
	    $key = "n_" . $string;
	    if($seb) { $key = "seb_" . $key }
	    $RUN{$key} = $1;
	    $key = "qtrk_" . $string;
	    if($seb) { $key = "seb_" . $key }
	    $RUN{$key} = $2;
	    print $key  . " " . $2 . "\n";
	}
    }

}

1;
