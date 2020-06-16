#!/usr/local/bin/perl

require "ctime.pl";     

$outdir = `pwd`;
$file = `ls -1 clas_*.A*`;
chop($file);
die"\n No input data file is found, will terminate now\!" if !$file;


# Run sync/trip and gflux
&GetFlux($file,$outdir);

####################################################################################





####################################################################################
sub GetFlux{

    local($file,$outdir) = @_;

    print "GetFlux -I- Input file name: $file \n";


    $basename = $file;
    $basename =~ s/clas_0//;
    $basename =~ s/A/a/;
    $fnum = $basename;
    $fnum =~  s/\d+\Wa*//;


# first do sync

    $sync_cmd = "sync -c -t -s10 $file";
    $syn_opt = " -c -t -s10 $file";
    print "\nGetFlux -I-: sync command: $sync_cmd \n";
 
    $t = time();
    $start_time = &ctime($t);
    print  "\nGetFlux -I-: $start_time EXECUTE: $sync_cmd\n";
    system($sync_cmd);
 
#    exit(0);
 
    $t = time();
    $end_time = &ctime($t);
    print "\nGetFlux -I-: $end_time FINISHED: $sync_cmd\n";
 
# copy trip files
 
    system("ls -l *.trip");
        print "$copy_cmd \n";
    system("$copy_cmd");
    system("rm -f *.sync");

# check trip file
    system("check_trip.sh");	 
# now do gflux
 
    $gflux_cmd = "gflux -b -c -F100000 -T -f$fnum -t$file.trip -ogflux$basename.hbook $file";
 
    $t = time();
    $start_time = &ctime($t);
 
    print  "\nGetFlux -I-: $start_time EXECUTE: $gflux_cmd\n";
 
    system($gflux_cmd);
 
    $t = time();
    $end_time = &ctime($t);
    print "\nGetFlux -I-: $end_time FINISHED: $gflux_cmd\n";
 
    return;
}

