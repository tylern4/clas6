#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        August 23, 1999

CONFIG:{
    package RunKK_filter;
}

sub RunKK_filter {

# Runs the filtering program KK_filter. It parses the output
# text file for important database information.
#
# Usage: RunKK_filter($files, $Outpath, $db_file, $basename, $db_mysql)
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
    local($db_mysql) = $_[4];

    local($os) = $ENV{"OSNAME"};
    local($clas_bin) = $ENV{"CLAS_BIN"};

    if ($clas_bin eq "") {
	$clas_bin = "/group/clasdev/clas/src/prod-1-9/bin/$os/";
    }

    $kk_filt_cmd = "$clas_bin/KK_filter -okk_$basename.filt $files > kk_filt_$basename.txt";

    print  "\n$0 EXECUTE: $kk_filt_cmd at $start_time\n";

    system($kk_filt_cmd);

    if (!$db_mysql) {
      dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";

      $RUN{"KK_filter  command"} = $kk_filt_cmd;

      if (-r "kk_filt_$basename.txt") { 
	open(KK_TXT, "kk_filt_$basename.txt") || print  "$0: Could not open kk_filt_$basename.txt\n";
      }
    
      while (<KK_TXT>) {
	
      }
    
      close(KK_TXT);

      dbmclose(%RUN);
    }

    MoveFile("kk_filt_*txt", $OutPath, "KK_filter  txt file", $db_file);
    MoveFile("kk*filt", $OutPath, "KK_filter output  file", $db_file); 

    $t = time();
    $end_time = &ctime($t);

    print "\n$0 FINISHED: $kk_filt_cmd at $end_time\n";

    print "\n\n";

}

1;
