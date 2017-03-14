#!/usr/bin/env perl
#
# squeeze the RUN_CONTROL subsystem item
# (optionally) the run index table name with a new 
# validity range defined from runMin runMax
#
########################################################################

##use lib ("$ENV{CLAS_TOOLS}/caldb");
use lib ("/home/avakian/caldb");

use CaldbUtil;

# get command line parameters
eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift; # see camel book

&ProcessCommandLine();

unless ($job eq "zip") { $status = ConnectTopc10Server(\$dbh10);}

print "caldb: $hostname, $user, $password, $database $runIndexTable \n";
$status = ConnectToServer($hostname, $user, $password, $database, $verbose,
			  \$dbh);

$status = ShowSSI($dbh, $system, $subsystem, $item, \@itemArrayRef);

# loop over all subsystems and items

for ($iitem = 0; $iitem <= $#itemArrayRef; $iitem++) {
       $itemid=$itemArrayRef[$iitem][3];
       $item=$itemArrayRef[$iitem][2];
       $subsystem=$itemArrayRef[$iitem][1];
       $sitem=$subsystem."_".$item;
   if (!$quiet ) {print "PROCESS $job : $subsystem $item : $sitem : itemid=$itemid \n";}  
  for ($runi = $runMin; $runi <= $runMax; $runi++) {
       $status = ShowItemRunIndexinfo($dbh, $itemArrayRef[$iitem][0], 
                              $itemArrayRef[$iitem][1],$itemArrayRef[$iitem][2], 
                              $runi,$runIndexTable, $time, \@itemIndexref);
       if ($#itemIndexref > 0) {     # there is an entry in caldb
           $valuidin=$itemIndexref[1];
           $runindexin=$itemIndexref[2];

           $status = ReadConstantsByIndex($dbh, $system, $subsystem, $item, 
           $valuidin, \@itemvalue);
           if($#itemvalue > 0 ){  # there is a valid record in valueID table
                $valueout=$itemvalue[6];
            }else{
                $valueout="missing valueID";
	    }
       } else {
           print "WARNING! no entry in  $runIndexTable run $runi\n";
           $valuidin=0;
           $runindexin=0;
           $valueout="missing runindex";
       }

       unless ($job eq "zip"){
         &Getpc10System($sitem,\$pc10table,\$pc10field);
         $status =&Getpc10Constants($dbh10,$pc10table,$pc10field,$runi,\$pc10value); 
         $differ=$valueout-$pc10value;
           if(abs($differ) > $diff) {print "caldb vs claspc10: run=$runi caldb-> $valueout ($subsystem,$item) claspc10-> $pc10cons  ($pc10table,$pc10field) diff=$differ =$pc10value runindex=$runindexin valueid=$valuidin \n";}
       }


       if ($job eq "zip") {
         if ($runi > $runMin  ){
           $status=&Caldbzip($runindexin,$valuidin,$valueout,\$runindexBAS,\$valuidBAS,\$valueBAS);
         } else { 
           $status=&CaldbzipRunMin($runindexin,$valuidin,$valueout,\$runindexBAS,\$valuidBAS,\$valueBAS);
         }
       } elsif ($job eq "update") {
          if ($pc10value =~ m/missing/) { #missing in claspc10
	      if (!$quiet ) {print "Entry for $runi missing in claspc10!!!  \n";}
          } else { 
           $status=&Caldbupdate($runIndexTable,$sitem,$runindexin,$valuidin,$valueout,$differ,$pc10value);
          }
       }
        


   }   ### run-i loop
  }   ### item-i loop

exit 0;

sub Caldbupdate {
 my($runIndexTable,$sitem,$runindexin,$valuidin,$valueout,$differ,$pc10value) = @_;

 $caldbtable="RUN_CONTROL_".$sitem;

 if($valueout =~ m/missing/ ) {    # no record in caldb 
      $status=$dbh->do("INSERT $caldbtable SET  time=NOW()+0,comment='updated from claspc10',author='$ENV{USER}',minRunSource=$runi,maxRunSource=$runi,v_0001=$pc10value  ");
      $newitemvalueid=$dbh->{'mysql_insertid'};
      $dbh->do("INSERT $runIndexTable SET  time=NOW()+0,comment='updated from claspc10',officer='$ENV{USER}',minRun=$runi,maxRun=$runi,itemId=$itemid,itemValueId=$newitemvalueid");
     if (!$quiet ) {print " No entry in caldb adding $pc10value in $caldbtable for run $runi from claspc10 \n";}  
      return  $dbh->{'mysql_insertid'};
  } else {
     if( abs($differ) > $diff) {
      $dbh->do("UPDATE $caldbtable SET  time=NOW()+0,comment='updated from claspc10',author='$ENV{USER}',v_0001=$pc10value  WHERE itemValueId=$valuidin");
     if (!$quiet ) {print " abs($differ)> $diff updating value to $pc10value in $caldbtable  for run $runi \n";}  
  }
 }
}


sub CaldbzipRunMin {
 my($runindexin,$valuidin,$valueout,$runindexBAS,$valuidBAS,$valueBAS) = @_;

	   if( $valueout =~ m/$missing/  ) { #   there is no entry
               print "valueout missing ($valueout)!  \n";
               die " not ready  yet with response \n";   
           } else {                         # there is a value
	       $$valuidBAS=  $valuidin;
               $$runindexBAS=$runindexin;
               $$valueBAS=   $valueout;
               ## update the record
               $dbh->do("UPDATE $runIndexTable SET  time=NOW()+0 WHERE runindexId=$runindexin");
           } 

}
sub Caldbzip {
 my($runindexin,$valuidin,$valueout,$runindexBAS,$valuidBAS,$valueBAS) = @_;
       $diffzip=abs($valueout - $$valueBAS);
	   if ($diffzip <= $diff ) { # value is the same within diff
             if($runindexin != $$runindexBAS ) {     # dummy record delete it
              $dbh->do("DELETE from $runIndexTable WHERE runindexId=$runindexin");
              $dbh->do("UPDATE $runIndexTable SET  time=NOW()+0,maxRun=$runi,comment='updated from script',officer='$ENV{USER}' WHERE runindexID=$$runindexBAS");
         if (!$quiet ) {print "Caldbzip: Diff $diffzip <= $diff  deleteing record runindexId=$runindexin and updating $runIndexTable runindexId=$$runindexBAS for run $runi \n";}     
             } 
	   } else {                                      # value is not the same within diff
              if( $valueout =~ m/missing/ ) {   ## run was missing extend maxRun           
              $dbh->do("UPDATE $runIndexTable SET  time=NOW()+0,maxRun=$runi,comment='updated from script',officer='$ENV{USER}' WHERE runindexID=$$runindexBAS");                
		  if (!$quiet ) {print "Caldbzip: update $runIndexTable runindexID=$$runindexBAS for missing entry run $runi \n";}
               } else {   ## run with different value, update BASic values
#print " ures? $valuidin $runindexin $valueout $$valueBAS \n";
          	       $$valuidinBAS=$valuidin;
                       $$runindexBAS=$runindexin;
                       $$valueBAS=$valueout;   
               }          
	  }
}
sub Getpc10Constants {
 my($dbhpc10, $pc10table, $pc10field, $run,$pc10valueref) = @_;
    $pc10sql  = "SELECT $pc10field  from  $pc10table Where run=$run";
    &DoSel($dbhpc10,$pc10sql, \$sthpc10);
    @resultpc10 = $sthpc10->fetchrow_array;
    $pc10cons=@resultpc10[0];
       if($#resultpc10 ) {$pc10cons="missing entry";} 
       if($pc10field eq "run_type") {$pc10cons=&GetRuntype($pc10cons);}
       if($pc10field eq "target") {$pc10cons=&GetTargtype($pc10cons);}
       $$pc10valueref=$pc10cons;
    return $#resultpc10;
}

sub Getpc10System{
my ($item,$tablepc10,$fieldpc10) =  @_;
#
#    correspondence between caldb an online
#    subsystem_item     table         field
#
    %pc10link=(
    "Faraday_cup_Q_all"  =>  "run_log_end  fcup",
    "Faraday_cup_Q_active"=> "run_log_end  fcup_active",
    "Faraday_cup_Q_live"  => "run_log_end  fcup_live",
    "beam_energy"  => "run_log_begin      beam_energy",
    "beam_type"    => "run_log_comment    run_type",
    "beampol_value" => "run_log_polar     beam_polarization",
    "currents_torus"=> "run_log_begin     torus_current",
    "currents_minitorus"=> "run_log_begin minitorus_current",
    "currents_ptm"=> "run_log_polar       pt_current",
    "currents_tagger"=> "run_log_begin    tagger_current",
    "currents_pair_spec"=> "run_log_begin minitorus_current",
    "target_type"=> "run_log_comment        target"
	       );
($$tablepc10,$$fieldpc10)=($pc10link{$item} =~ m/(\w+)\s*(\w+)/);

if(!$$fieldpc10) { die "Error! not supported item $item  \n";}

}

sub ConnectTopc10Server{
my ($dbhref) =  @_;
 $adatabase = "clas_online";
 $auser = "clas_offline";
 $apassword = "";
 $ahostname = "claspc10";
print "pc10Server: $ahostname, $auser, $apassword, $adatabase\n";
ConnectToServer($ahostname, $auser, $apassword, $adatabase, $verbose,\$dbhre);
$$dbhref=$dbhre;
}
##########
sub GetRuntype {
    my($line) = @_;
    print "line=$line";
if ($line =~ /electron$/) {
    $runtype = 0;
} elsif ($line =~ /photon$/) {
    $runtype = 1;
} elsif ($line =~ /cosmic$/) {
    $runtype = -1;
} elsif ($line =~ /pedestal$/) {
    $runtype = -2;
} else {
    $runtype =-5;
}
    print "GetRuntype: runtype=$runtype\n";
    return ($runtype);
}
sub GetTargtype {
    my($line) = @_;
    print "line=$line";
if ($line =~ m/H2_full/) {
    $targtype = 1;
} elsif ($line =~ /D2_full/) {
    $targtype = 2;
} elsif ($line =~ /C12/) {
    $targtype = 3;
} elsif ($line =~ /NH3/) {
    $targtype = 4;
} elsif ($line =~ /ND3/) {
    $targtype = 5;
} elsif ($line =~ /empty/) {
    $targtype = 0;
} else {
    $targtype =-1;
}
    print "GetTargtype: targtype=$targtype\n";
    return ($targtype);
}

sub ProcessCommandLine {

    $user = "clasuser";
    $password = "";
    $database = "calib";
    $system="RUN_CONTROL";
    if ($help) {
	&PrintUsage();
	exit 0;
    }
    if (!$subsystem) {
	if ($ss) {
	    $subsystem = $ss;
	} else {
	    print "warning:RUN_CONTROL subsystem not defined\n      linking all subsystem for $system\n";
	}
    }
    if (!$item) {
	if ($i) {
	    $item = $i;
	} else {
  print "warning:RUN_CONTROL item not defined\n        linking all items  for $system  $subsystem\n";
	}
    }
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
    if (!$runIndexTable) {
	if ($it) {
	    $runIndexTable = $it;
	} else {
	    $runIndexTable = "RunIndex";
	}
    }
	    if ($verbose) {print "using runIndexTable = $runIndexTable\n";}
    if (!$hostname){
	$hostname = "clasdb.jlab.org";
    }
    if (!$job){
	$job = "monitor";
    }
    if (!$time){
	if ($t) {
	    $time = $t;
	}
    }
    if (!$time){  $time = '2037-01-01'}

    if (!$quiet){
	if ($q) {
	    $quiet = $q;
	}
    }
    if (!$diff){
	if ($d) {
	    $diff = $d;
	}
    $diff=0;
    }
}


sub PrintUsage {
    print <<EOM;
function: This script is linking an old run as new to extend its validity range \\
          to cover from runMin to runMax.

usage:
caldb_upd_runcontrol.pl ss=<subsystem> i=<item>  \\
    min=<run min> max=<run max>  [it=<run index table>] [t=<time of validity>] \\
    [hostname=<hostname of db server>] [q=<non-zero to suppress some info>] \\
    [help=<non-zero>]
 

alternate flag names:
  ss or subsystem for subsystem name
  i or item for item name
  r or runold for reference run number
  min or runMin
  max or runMax
  it or runIndexTable for run index table name
  t or time for time of validity
  q or quiet for suppression of extra information

example:
caldb_upd_runcontrol.pl  ss=beam i=energy  min=27358 max=27360
EOM
    return;
}

##########

sub DoSel {
    my($dbh,$sql_cmd,$sthref) = @_;
    $$sthref = $dbh -> prepare($sql_cmd)
	or die "Can't prepare $sql_cmd: $dbh-errstr\n";
    $rv = $$sthref -> execute
	or die "Can't execute the query: $sth-> errstr\n";
}

# end of perl script
