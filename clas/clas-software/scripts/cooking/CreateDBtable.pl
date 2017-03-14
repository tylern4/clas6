#!/apps/bin/perl

use lib ("$ENV{CLAS_TOOLS}/perl");
use Csql;

#This script needs to be run prior to Runjob for all run groups wishing to use
#the MySql database and its web interface to store pass0/1 cooking information.
#It's purpose is to create a table of entries (defined according to the wishes
#of the run group) in this database. 

# Usage: RunJob [-h] +env
#-------------------------
#       -h                          Print this help message.
#       +env <string>    File name w/path of environment variable text file

# Start of CreateDBtable.pl code

if ($ARGV[0] eq "+h" || $ARGV[0] eq "-h") {
    &PrintUsage;
    exit(0);
}

# Parse through command line options

for ($i = 0; $i <= $#ARGV; $i++) {
     $arg = $ARGV[$i];
     if ($arg =~ /^\+(\w*)/) {
	 if ($1 eq "h") {
	     &PrintUsage;
	     exit(0);
	 }
         elsif ($1 =~ /^env/) {
	     $i++;
	     $env_src_file = $ARGV[$i];
	 }
     }
 }

# Parse through environmental variable template file

print "\n$0: Going to use file $env_src_file to set environment variables.\n\n";

open(ENV, "$env_src_file") || die "\n$0: Could not open environment variable text template.\n";

while(<ENV>) {
    if ($_ =~ /^#/) {
	next;
    } elsif ($_ =~ /\s*(\S+)\s+(\S+)/) {
	$ENV{$1} = $2;
    }
}

# Get some environmental variables

@env_vars = ("CSQL_DBHOST","CSQL_DB","CSQL_USER","CSQL_TABLE");

# Get rid of possible extraneous ending / marks

foreach (@env_vars) {
  if ($ENV{"$_"} =~ /\/$/) {
    $ENV{"$_"} =~ s /\/$//;
  }
}

$hostname = $ENV{"CSQL_DBHOST"};
$database = $ENV{"CSQL_DB"};
$user = $ENV{"CSQL_USER"};
$table_name = $ENV{"CSQL_TABLE"};

########################################
# Table specification for MySql database
########################################
@table_spec = (

	        {
		   name => "startTime",
		   type => "datetime"
		   },
                {
		   name => "beamEnergy",
		   type => "float"
		   },
	        {
		   name => "torusCurrent",
		   type => "float"
		   },
                {
		   name => "miniTorusCurrent",
		   type => "int"
		   },
                {
		   name => "taggerCurrent",
		   type => "float"
		   },
                {
		   name => "a1cStartTime",
		   type => "datetime"
		   },
                {
		   name => "eventsReadByA1c",
		   type => "int"
		   },
                {
		   name => "eventsWrittenByA1c",
		   type => "int"
		   },
                {
		   name => "a1cEndTime",
		   type => "datetime"
		   },
                {
		   name => "hbtPerEvt_s1",
		   type => "float"
		   },
                {
		   name => "hbtPerEvt_s2",
		   type => "float"
		   },
                {
		   name => "hbtPerEvt_s3",
		   type => "float"
		   }, 
                {
		   name => "hbtPerEvt_s4",
		   type => "float"
		   }, 
                {
		   name => "hbtPerEvt_s5",
		   type => "float"
		   },
                {
		   name => "hbtPerEvt_s6",
		   type => "float"
		   },
                {
		   name => "tbtPerEvt_s1",
		   type => "float"
		   },
                {
		   name => "tbtPerEvt_s2",
		   type => "float"
		   },
                {
		   name => "tbtPerEvt_s3",
		   type => "float"
		   }, 
                {
		   name => "tbtPerEvt_s4",
		   type => "float"
		   }, 
                {
		   name => "tbtPerEvt_s5",
		   type => "float"
		   },
                {
		   name => "tbtPerEvt_s6",
		   type => "float"
		   },
                {
		   name => "tbtPerHbt_s1",
		   type => "float"
		   },
                {
		   name => "tbtPerHbt_s2",
		   type => "float"
		   },
                {
		   name => "tbtPerHbt_s3",
		   type => "float"
		   }, 
                {
		   name => "tbtPerHbt_s4",
		   type => "float"
		   }, 
                {
		   name => "tbtPerHbt_s5",
		   type => "float"
		   },
                {
		   name => "tbtPerHbt_s6",
		   type => "float"
		   },
                {
		   name => "nhitsPerTbt_s1",
		   type => "float"
		   },
                {
		   name => "nhitsPerTbt_s2",
		   type => "float"
		   },
                {
		   name => "nhitsPerTbt_s3",
		   type => "float"
		   }, 
                {
		   name => "nhitsPerTbt_s4",
		   type => "float"
		   }, 
                {
		   name => "nhitsPerTbt_s5",
		   type => "float"
		   },
                {
		   name => "nhitsPerTbt_s6",
		   type => "float"
		   },
	        {  
		   name => "rms_s1_sl1",
		   type => "float"
		   },
                {  
		   name => "rms_s1_sl2",
		   type => "float"
		   },
                {  
		   name => "rms_s1_sl3",
		   type => "float"
		   },
                {  
		   name => "rms_s1_sl4",
		   type => "float"
		   },
                {  
		   name => "rms_s1_sl5",
		   type => "float"
		   },
                {  
		   name => "rms_s1_sl6",
		   type => "float"
		   },
                {  
		   name => "rms_s2_sl1",
		   type => "float"
		   },
                {  
		   name => "rms_s2_sl2",
		   type => "float"
		   },
                {  
		   name => "rms_s2_sl3",
		   type => "float"
		   },
                {  
		   name => "rms_s2_sl4",
		   type => "float"
		   },
                {  
		   name => "rms_s2_sl5",
		   type => "float"
		   },
                {  
		   name => "rms_s2_sl6",
		   type => "float"
		   },
                {  
		   name => "rms_s3_sl1",
		   type => "float"
		   },
                {  
		   name => "rms_s3_sl2",
		   type => "float"
		   },
                {  
		   name => "rms_s3_sl3",
		   type => "float"
		   },
                {  
		   name => "rms_s3_sl4",
		   type => "float"
		   },
                {  
		   name => "rms_s3_sl5",
		   type => "float"
		   },
                {  
		   name => "rms_s3_sl6",
		   type => "float"
		   },
                {  
		   name => "rms_s4_sl1",
		   type => "float"
		   },
                {  
		   name => "rms_s4_sl2",
		   type => "float"
		   },
                {  
		   name => "rms_s4_sl3",
		   type => "float"
		   },
                {  
		   name => "rms_s4_sl4",
		   type => "float"
		   },
                {  
		   name => "rms_s4_sl5",
		   type => "float"
		   },
                {  
		   name => "rms_s4_sl6",
		   type => "float"
		   },
                {  
		   name => "rms_s5_sl1",
		   type => "float"
		   },
                {  
		   name => "rms_s5_sl2",
		   type => "float"
		   },
                {  
		   name => "rms_s5_sl3",
		   type => "float"
		   },
                {  
		   name => "rms_s5_sl4",
		   type => "float"
		   },
                {  
		   name => "rms_s5_sl5",
		   type => "float"
		   },
                {  
		   name => "rms_s5_sl6",
		   type => "float"
		   },
                {  
		   name => "rms_s6_sl1",
		   type => "float"
		   },
                {  
		   name => "rms_s6_sl2",
		   type => "float"
		   },
                {  
		   name => "rms_s6_sl3",
		   type => "float"
		   },
                {  
		   name => "rms_s6_sl4",
		   type => "float"
		   },
                {  
		   name => "rms_s6_sl5",
		   type => "float"
		   },
                {  
		   name => "rms_s6_sl6",
		   type => "float"
		   },
                 {  
		   name => "flat_s1_sl1",
		   type => "float"
		   },
                {  
		   name => "flat_s1_sl2",
		   type => "float"
		   },
                {  
		   name => "flat_s1_sl3",
		   type => "float"
		   },
                {  
		   name => "flat_s1_sl4",
		   type => "float"
		   },
                {  
		   name => "flat_s1_sl5",
		   type => "float"
		   },
                {  
		   name => "flat_s1_sl6",
		   type => "float"
		   },
                {  
		   name => "flat_s2_sl1",
		   type => "float"
		   },
                {  
		   name => "flat_s2_sl2",
		   type => "float"
		   },
                {  
		   name => "flat_s2_sl3",
		   type => "float"
		   },
                {  
		   name => "flat_s2_sl4",
		   type => "float"
		   },
                {  
		   name => "flat_s2_sl5",
		   type => "float"
		   },
                {  
		   name => "flat_s2_sl6",
		   type => "float"
		   },
                {  
		   name => "flat_s3_sl1",
		   type => "float"
		   },
                {  
		   name => "flat_s3_sl2",
		   type => "float"
		   },
                {  
		   name => "flat_s3_sl3",
		   type => "float"
		   },
                {  
		   name => "flat_s3_sl4",
		   type => "float"
		   },
                {  
		   name => "flat_s3_sl5",
		   type => "float"
		   },
                {  
		   name => "flat_s3_sl6",
		   type => "float"
		   },
                {  
		   name => "flat_s4_sl1",
		   type => "float"
		   },
                {  
		   name => "flat_s4_sl2",
		   type => "float"
		   },
                {  
		   name => "flat_s4_sl3",
		   type => "float"
		   },
                {  
		   name => "flat_s4_sl4",
		   type => "float"
		   },
                {  
		   name => "flat_s4_sl5",
		   type => "float"
		   },
                {  
		   name => "flat_s4_sl6",
		   type => "float"
		   },
                {  
		   name => "flat_s5_sl1",
		   type => "float"
		   },
                {  
		   name => "flat_s5_sl2",
		   type => "float"
		   },
                {  
		   name => "flat_s5_sl3",
		   type => "float"
		   },
                {  
		   name => "flat_s5_sl4",
		   type => "float"
		   },
                {  
		   name => "flat_s5_sl5",
		   type => "float"
		   },
                {  
		   name => "flat_s5_sl6",
		   type => "float"
		   },
                {  
		   name => "flat_s6_sl1",
		   type => "float"
		   },
                {  
		   name => "flat_s6_sl2",
		   type => "float"
		   },
                {  
		   name => "flat_s6_sl3",
		   type => "float"
		   },
                {  
		   name => "flat_s6_sl4",
		   type => "float"
		   },
                {  
		   name => "flat_s6_sl5",
		   type => "float"
		   },
                {  
		   name => "flat_s6_sl6",
		   type => "float"
		   },
                {  
		   name => "beamPosX",
		   type => "float"
		   },
                {  
		   name => "beamPosY",
		   type => "float"
		   },
                {  
		   name => "beamPosZ",
		   type => "float"
		   },
                {  
		   name => "beamFitSigmaX",
		   type => "float"
		   },
                {  
		   name => "beamFitSigmaY",
		   type => "float"
		   },
                {  
		   name => "beamFitSigmaZ",
		   type => "float"
		   },
                {  
		   name => "beamFitChi2X",
		   type => "float"
		   },
                {  
		   name => "beamFitChi2Y",
		   type => "float"
		   },
                {  
		   name => "beamFitChi2Z",
		   type => "float"
		   },
                {  
		   name => "HBpos_s1",
		   type => "int"
		   },
                {  
		   name => "HBpos_s2",
		   type => "int"
		   },
                {  
		   name => "HBpos_s3",
		   type => "int"
		   },
                {  
		   name => "HBpos_s4",
		   type => "int"
		   },
                {  
		   name => "HBpos_s5",
		   type => "int"
		   },
                {  
		   name => "HBpos_s6",
		   type => "int"
		   },
                {  
		   name => "HBneg_s1",
		   type => "int"
		   },
                {  
		   name => "HBneg_s2",
		   type => "int"
		   },
                {  
		   name => "HBneg_s3",
		   type => "int"
		   },
                {  
		   name => "HBneg_s4",
		   type => "int"
		   },
                {  
		   name => "HBneg_s5",
		   type => "int"
		   },
                {  
		   name => "HBneg_s6",
		   type => "int"
		   },
                {  
		   name => "TBpos_s1",
		   type => "int"
		   },
                {  
		   name => "TBpos_s2",
		   type => "int"
		   },
                {  
		   name => "TBpos_s3",
		   type => "int"
		   },
                {  
		   name => "TBpos_s4",
		   type => "int"
		   },
                {  
		   name => "TBpos_s5",
		   type => "int"
		   },
                {  
		   name => "TBpos_s6",
		   type => "int"
		   },
                {  
		   name => "TBneg_s1",
		   type => "int"
		   },
                {  
		   name => "TBneg_s2",
		   type => "int"
		   },
                {  
		   name => "TBneg_s3",
		   type => "int"
		   },
                {  
		   name => "TBneg_s4",
		   type => "int"
		   },
                {  
		   name => "TBneg_s5",
		   type => "int"
		   },
                {  
		   name => "TBneg_s6",
		   type => "int"
		   },
                {  
		   name => "Chi2pos_s1",
		   type => "float"
		   },
                {  
		   name => "Chi2pos_s2",
		   type => "float"
		   },
                {  
		   name => "Chi2pos_s3",
		   type => "float"
		   },
                {  
		   name => "Chi2pos_s4",
		   type => "float"
		   },
                {  
		   name => "Chi2pos_s5",
		   type => "float"
		   },
                {  
		   name => "Chi2pos_s6",
		   type => "float"
		   },
                {  
		   name => "Chi2neg_s1",
		   type => "float"
		   },
                {  
		   name => "Chi2neg_s2",
		   type => "float"
		   },
                {  
		   name => "Chi2neg_s3",
		   type => "float"
		   },
                {  
		   name => "Chi2neg_s4",
		   type => "float"
		   },
                {  
		   name => "Chi2neg_s5",
		   type => "float"
		   },
                {  
		   name => "Chi2neg_s6",
		   type => "float"
		   },
                {  
		   name => "Unknown_s1",
		   type => "int"
		   },
                {  
		   name => "Unknown_s2",
		   type => "int"
		   },
                {  
		   name => "Unknown_s3",
		   type => "int"
		   },
                {  
		   name => "Unknown_s4",
		   type => "int"
		   },
                {  
		   name => "Unknown_s5",
		   type => "int"
		   },
                {  
		   name => "Unknown_s6",
		   type => "int"
		   }, 
                {  
		   name => "Gamma_s1",
		   type => "int"
		   },
                {  
		   name => "Gamma_s2",
		   type => "int"
		   },
                {  
		   name => "Gamma_s3",
		   type => "int"
		   },
                {  
		   name => "Gamma_s4",
		   type => "int"
		   },
                {  
		   name => "Gamma_s5",
		   type => "int"
		   },
                {  
		   name => "Gamma_s6",
		   type => "int"
		   }, 
                {  
		   name => "Positron_s1",
		   type => "int"
		   },
                {  
		   name => "Positron_s2",
		   type => "int"
		   },
                {  
		   name => "Positron_s3",
		   type => "int"
		   },
                {  
		   name => "Positron_s4",
		   type => "int"
		   },
                {  
		   name => "Positron_s5",
		   type => "int"
		   },
                {  
		   name => "Positron_s6",
		   type => "int"
		   }, 
                {  
		   name => "Electron_s1",
		   type => "int"
		   },
                {  
		   name => "Electron_s2",
		   type => "int"
		   },
                {  
		   name => "Electron_s3",
		   type => "int"
		   },
                {  
		   name => "Electron_s4",
		   type => "int"
		   },
                {  
		   name => "Electron_s5",
		   type => "int"
		   },
                {  
		   name => "Electron_s6",
		   type => "int"
		   }, 
                {  
		   name => "PiPlus_s1",
		   type => "int"
		   },
                {  
		   name => "PiPlus_s2",
		   type => "int"
		   },
                {  
		   name => "PiPlus_s3",
		   type => "int"
		   },
                {  
		   name => "PiPlus_s4",
		   type => "int"
		   },
                {  
		   name => "PiPlus_s5",
		   type => "int"
		   },
                {  
		   name => "PiPlus_s6",
		   type => "int"
		   }, 
                {  
		   name => "PiMinus_s1",
		   type => "int"
		   },
                {  
		   name => "PiMinus_s2",
		   type => "int"
		   },
                {  
		   name => "PiMinus_s3",
		   type => "int"
		   },
                {  
		   name => "PiMinus_s4",
		   type => "int"
		   },
                {  
		   name => "PiMinus_s5",
		   type => "int"
		   },
                {  
		   name => "PiMinus_s6",
		   type => "int"
		   }, 
                {  
		   name => "Proton_s1",
		   type => "int"
		   },
                {  
		   name => "Proton_s2",
		   type => "int"
		   },
                {  
		   name => "Proton_s3",
		   type => "int"
		   },
                {  
		   name => "Proton_s4",
		   type => "int"
		   },
                {  
		   name => "Proton_s5",
		   type => "int"
		   },
                {  
		   name => "Proton_s6",
		   type => "int"
		   },
                 {  
		   name => "KPlus_s1",
		   type => "int"
		   },
                {  
		   name => "KPlus_s2",
		   type => "int"
		   },
                {  
		   name => "KPlus_s3",
		   type => "int"
		   },
                {  
		   name => "KPlus_s4",
		   type => "int"
		   },
                {  
		   name => "KPlus_s5",
		   type => "int"
		   },
                {  
		   name => "KPlus_s6",
		   type => "int"
		   },
                 {  
		   name => "KMinus_s1",
		   type => "int"
		   },
                {  
		   name => "KMinus_s2",
		   type => "int"
		   },
                {  
		   name => "KMinus_s3",
		   type => "int"
		   },
                {  
		   name => "KMinus_s4",
		   type => "int"
		   },
                {  
		   name => "KMinus_s5",
		   type => "int"
		   },
                {  
		   name => "KMinus_s6",
		   type => "int"
		   },
                {  
		   name => "Deuteron_s1",
		   type => "int"
		   },
                {  
		   name => "Deuteron_s2",
		   type => "int"
		   },
                {  
		   name => "Deuteron_s3",
		   type => "int"
		   },
                {  
		   name => "Deuteron_s4",
		   type => "int"
		   },
                {  
		   name => "Deuteron_s5",
		   type => "int"
		   },
                {  
		   name => "Deuteron_s6",
		   type => "int"
		   },
                {  
		   name => "Neutron_s1",
		   type => "int"
		   },
                {  
		   name => "Neutron_s2",
		   type => "int"
		   },
                {  
		   name => "Neutron_s3",
		   type => "int"
		   },
                {  
		   name => "Neutron_s4",
		   type => "int"
		   },
                {  
		   name => "Neutron_s5",
		   type => "int"
		   },
                {  
		   name => "Neutron_s6",
		   type => "int"
		   },
                {  
		   name => "n_Unknown",
		   type => "int"
		   },
                {  
		   name => "n_Gamma",
		   type => "int"
		   },
                {  
		   name => "n_Positron",
		   type => "int"
		   },
                {  
		   name => "n_Electron",
		   type => "int"
		   },
                {  
		   name => "n_PiPlus",
		   type => "int"
		   },
                {  
		   name => "n_PiMinus",
		   type => "int"
		   },
                {  
		   name => "n_Proton",
		   type => "int"
		   },
                {  
		   name => "n_KPlus",
		   type => "int"
		   },
                {  
		   name => "n_KMinus",
		   type => "int"
		   },
                {  
		   name => "n_Deuteron",
		   type => "int"
		   },
                {  
		   name => "n_Neutron",
		   type => "int"
		   },
                {  
		   name => "qtrk_Unknown",
		   type => "float"
		   },
                {  
		   name => "qtrk_Gamma",
		   type => "float"
		   },
                {  
		   name => "qtrk_Positron",
		   type => "float"
		   },
                {  
		   name => "qtrk_Electron",
		   type => "float"
		   },
                {  
		   name => "qtrk_PiPlus",
		   type => "float"
		   },
                {  
		   name => "qtrk_PiMinus",
		   type => "float"
		   },
                {  
		   name => "qtrk_Proton",
		   type => "float"
		   },
                {  
		   name => "qtrk_KPlus",
		   type => "float"
		   },
                {  
		   name => "qtrk_KMinus",
		   type => "float"
		   },
                {  
		   name => "qtrk_Deuteron",
		   type => "float"
		   },
                {  
		   name => "qtrk_Neutron",
		   type => "float"
		   },
                {
		   name => "endTime",
		   type => "datetime"
		   } 
              ); 


############################################
#NOW CREATE THE TABLE IN THE MYSQL DATABASE
############################################

ConnectToServer($hostname, $user, $database);

CreateTable($table_name, @table_spec);

DisconnectFromServer();

exit;

##############################

sub PrintUsage {
    print "\nUsage: $0 [-h] +env\n";
    print "\t-h\t\t Print this help message.\n";
    print "\t+env <string> \t File name w/path of environment variable text file.\n";
}
