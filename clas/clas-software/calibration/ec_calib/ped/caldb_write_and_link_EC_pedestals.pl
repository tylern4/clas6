#!/usr/bin/env perl 
#
# Put the pedestals calibration constant sets to the calibration 
# database CalDB, system :EC_CALIB EC_PEDESTALS and link to the RunIndex table
# 
# H.Avakian 
#############################################################################################

use lib ("$ENV{CLAS_TOOLS}/caldb");
use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();
            
# get the password
            
if ($user ne 'clasuser') {
    print "password: ";
    system("stty -echo");
    chop($password=<STDIN>);
    print "\n";
    system("stty echo");
} else {
    $password = '';
}

#using split function to separate each value that belongs to the same column
#into seperated arrays


$counter_status = ConnectToServer($hostname, $user, $password, $database, $verbose,
                          \$dbh);
            
ReadConstantsFromFile();
$i = 0;
while ($i < @array) { 
 push(@inneru,$array[$i+2]);
 push(@innerv,$array[$i+6]);
 push(@innerw,$array[$i+10]);
 push(@outeru,$array[$i+14]);
 push(@outerv,$array[$i+18]);
 push(@outerw,$array[$i+22]);
 $i=$i+24;
}

# putting constant sets of system: EC_CALIB, subsystem: EC_PEDESTALS, into CalDB 
 print "ped:\n";

$status = WriteConstantSet($dbh, $system, 'EC_PEDESTALS', 'InnerU',$sourceRunMin,
                                   $sourceRunMax,$comment_value, \$itemValueId,@inneru);

print "ped. itemValueId = $itemValueId\n";
 
$status = LinkConstantSet($dbh, $system, 'EC_PEDESTALS', 'InnerU' , $runIndexTable,
                                  $runMin, $runMax, $itemValueId, $comment_index,
                                  \$runIndexId);
 
print "ped. runIndexId = $runIndexId\n";
$status = WriteConstantSet($dbh, $system, 'EC_PEDESTALS', 'InnerV',$sourceRunMin,
                                   $sourceRunMax,$comment_value, \$itemValueId,@innerv);

print "ped. itemValueId = $itemValueId\n";
 
$status = LinkConstantSet($dbh, $system, 'EC_PEDESTALS', 'InnerV' , $runIndexTable,
                                  $runMin, $runMax, $itemValueId, $comment_index,
                                  \$runIndexId);
 
print "ped. runIndexId = $runIndexId\n";
$status = WriteConstantSet($dbh, $system, 'EC_PEDESTALS', 'InnerW',$sourceRunMin,
                                   $sourceRunMax,$comment_value, \$itemValueId,@innerw);

print "ped. itemValueId = $itemValueId\n";
 
$status = LinkConstantSet($dbh, $system, 'EC_PEDESTALS', 'InnerW' , $runIndexTable,
                                  $runMin, $runMax, $itemValueId, $comment_index,
                                  \$runIndexId);
 
print "ped. runIndexId = $runIndexId\n";
$status = WriteConstantSet($dbh, $system, 'EC_PEDESTALS', 'OuterU',$sourceRunMin,
                                   $sourceRunMax,$comment_value, \$itemValueId,@outeru);

print "ped. itemValueId = $itemValueId\n";
 
$status = LinkConstantSet($dbh, $system, 'EC_PEDESTALS', 'OuterU' , $runIndexTable,
                                  $runMin, $runMax, $itemValueId, $comment_index,
                                  \$runIndexId);
 
print "ped. runIndexId = $runIndexId\n";
$status = WriteConstantSet($dbh, $system, 'EC_PEDESTALS', 'OuterV',$sourceRunMin,
                                   $sourceRunMax,$comment_value, \$itemValueId,@outerv);

print "ped. itemValueId = $itemValueId\n";
 
$status = LinkConstantSet($dbh, $system, 'EC_PEDESTALS', 'OuterV' , $runIndexTable,
                                  $runMin, $runMax, $itemValueId, $comment_index,
                                  \$runIndexId);
 
print "ped. runIndexId = $runIndexId\n";
$status = WriteConstantSet($dbh, $system, 'EC_PEDESTALS', 'OuterW',$sourceRunMin,
                                   $sourceRunMax,$comment_value, \$itemValueId,@outerw);

print "ped. itemValueId = $itemValueId\n";
 
$status = LinkConstantSet($dbh, $system, 'EC_PEDESTALS', 'OuterW' , $runIndexTable,
                                  $runMin, $runMax, $itemValueId, $comment_index,
                                  \$runIndexId);
 
print "ped. runIndexId = $runIndexId\n";

exit 0;

sub ReadConstantsFromFile {
    open (FILE, $inputFile);
    while (<FILE>) {
        chop;
        @buffer = split(/  */,$_);
        if($#buffer >0 && !/sector/i && !/1 2 3 4 5 6/) { push(@array,grep(($_ || $_ eq '0'),@buffer));}
    }
    close (FILE);
}

sub ProcessCommandLine {
            
    $database = 'calib';
    $system = 'EC_CALIB';

    if (!$runMin) {
        if ($min) {
            $runMin = $min;
        } else {
            &PrintUsage();
            die "error: runMin not defined\n";
        }
    }
    if (!$runMax) {
        if ($max) {
            $runMax = $max;
        } else {
            &PrintUsage();
            die "error: runMax not defined\n";
        }
    }
    if (!$sourceRunMin) {
        if ($srmin) {
            $sourceRunMin = $srmin;
        } else {
            $sourceRunMin = $runMin;
        }
    }
    if (!$sourceRunMax) { 
        if ($srmax) {
            $sourceRunMax = $srmax;
        } else {
            $sourceRunMax = $runMax;
        }
    }    
    if (!$runIndexTable) {
        if ($it) {
            $runIndexTable = $it;
        } else {
            $runIndexTable = "RunIndex";
            print "using runIndexTable = $runIndexTable\n";
        }
    }
    if (!$comment_index) {
        if ($ci) {   
            $comment_index = $ci;  
        } else {
            &PrintUsage();
            die "error: comment for the run index not defined\n";
        }
    }

    if (!$comment_value) {
        if ($cv) {
            $comment_value = $cv;
        } else {
            $comment_value = $comment_index;
        }
    }

    if (!$inputFile) {
        if ($f) {
            $inputFile = $f;
        } else {
            &PrintUsage();
            die "error: inputFile not defined\n";
        }
      -T $inputFile or die "error!!!: inputFile $inputFile is missing\n";
    }
    if (!$user) {
        if ($runIndexTable =~ /^calib_user./) {
            $user = 'clasuser';
        } else {
            $user = $ENV{USER};
        } 
    }
    if (!$hostname){
        $hostname = "clasdb.jlab.org";
    }
}

sub PrintUsage {
    print <<EOM;
usage:
caldb_write_and_link_pedestals.pl  f=<file with constants> ci=<comment for run index>\
    min=<run min> max=<run max> [srmin=<source run min>] [srmax=<source runmax>]\
    [cv=<comment for item value table>] [it=<run index table name>] [user=<MySQL user name>] \
    [hostname=<hostname of db server>]

alternate flag names:
  f or inputFile
  min or runMin 
  max or runMax
  ci or comment_index
  srmin or sourceRunMin
  srmax or sourceRunMax
  cv or comment_value
  it or runIndexTable for run index table name
            
example:
caldb_write_and_link_pedestals.pl f=const_file.dat \\
min=1000 max=2000 srmin=1400 srmax=1450 \\
ci='write and link pedestals' cv='creating new constants'\\
user=khuengo hostname=claspc24

EOM
    return;
}    
