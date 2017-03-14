#!/apps/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# orig: Simeon McAleer
#        Florida State University
#        March 5, 1999
# adapted: C. Marchand, Nov 23, 1999

CONFIG:{
    package RuntFilter;
}

sub RuntFilter {

# Runs the filtering program g_filter. It parses the
# output for important database information.
#
# Usage: RuntFilter($files, $Outpath, $progress_file, $db_file, $basename)
#
# Inputs: $files          = List of files to be processed
#         $Outpath        = Path where the ouput file should be copied
#         $progess_file   = Progress file including path
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
    local($progress_file) = $_[2];
    local($db_file) = $_[3];
    local($basename) = $_[4];

    local($os) = $ENV{"OSNAME"};
    local($clas_bin) = $ENV{"CLAS_BIN"};

    if ($clas_bin eq "") {
        $clas_bin = "/group/clasdev/clas/src/prod-1-9/bin/$os/";
    }

    open(L, ">> $progress_file") || warn "$0; Could not open log file: $progress_file.\n";

    $gfilter_cmd = "$clas_bin/tFilter -i -T0x1 -R1 -q1 -c2 -p2 -T0x4 -R1 -c2 $files";

    print "\n$0 EXECUTE: $gfilter_cmd\n";
    print L "\n$0 EXECUTE: $gfilter_cmd at $start_time\n";

    dbmopen(%RUN,$db_file, 0644) || print L "$0: Could not open $db_file for offline db\n";

    close(L);

    $RUN{"g_filter command"} = $gfilter_cmd;

    open(FILT, "$gfilter_cmd 2>&1 |") || warn "DB: Could not open $gfilter_cmd.\n";

    while (<FILT>) {
        print $_;
#        if ($_ =~ /\# of events written:\s+(\d+)/) {
#            $RUN{"93031 filtered events"} = $1;
#        }
        if ($_ =~ /Wrote\s+(\d+)\s+events on unit 30/) {
            $RUN{"99005 filtered events"} = $1;
        }
        elsif ($_ =~ /Wrote\s+(\d+)\s+events on unit 31/) {
            $RUN{"93031 filtered events"} = $1;
        }
    }

    close(FILT);

    dbmclose(%RUN);

    rename("trigFilter0",$basename . ".93031");
    rename("trigFilter1",$basename . ".99005");

## Rename the trigFilter* ##

    MoveFile("run*93031", "$OutPath/93031/", "g93031 file", $db_file);
    MoveFile("run*99005", "$OutPath/99005/", "g99005 file", $db_file);

    open(L, ">> $progress_file") || warn "$0; Could not open log file: $progress_file.\n";

    $t = time();
    $end_time = &ctime($t);

    print "\n$0 FINISHED: $gfilter_cmd at $end_time\n";
    print L "\n$0 FINISHED: $gfilter_cmd at $end_time\n";

    close(L);

    print "\n\n";





}

1;
