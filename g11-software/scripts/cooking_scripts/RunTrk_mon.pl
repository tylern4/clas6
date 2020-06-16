#!/apps/bin/perl

use lib ("$ENV{CLAS_TOOLS}/perl");
use Csql;

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        March 5, 1999

CONFIG:{
    package RunTrk_mon;
}

sub RunTrk_mon {

# Runs the monitoring program trk_mon. It parses the output
# text file for important database information.
#
# Usage: RunTrk_mon($files, $Outpath, $db_file, $basename, $run_type, $db_mysql)
#
# Inputs: $files          = List of files to be processed
#         $Outpath        = Path where the ouput file should be copied
#         $db_file        = Database file including path
#         $basename       = Basename for output files
#         $run_type       = Type of beam (electron or photon)
#         $db_mysql       = Flag to use the MYSQL db instead of the perl db
    
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
    local($run_type) = $_[4];
    local($db_mysql) = $_[5];

    local($os) = $ENV{"OSNAME"};
    local($clas_bin) = $ENV{"CLAS_BIN"};

    if ($clas_bin eq "") {
	$clas_bin = "/group/clasdev/clas/src/prod-1-5/bin/$os/";
    }

    $trk_mon_cmd = "$clas_bin/trk_mon -N -PROTON -Cchisq.lt.2.0 -A1l-28.0 -A1u-20.0 -A2l-12.0 -A2u-2.0 -A3l-8.0 -A3u-2.0 -otrkm$basename.hbook ";

#    if($run_type == "photon") {
#      $trk_mon_cmd .= " -cm0 ";
#    }

    $trk_mon_cmd .= "$files > trkm$basename.txt";

    print  "\n$0 EXECUTE: $trk_mon_cmd at $start_time\n";

    system($trk_mon_cmd);

#MYSQL DATABASE INSERTION:

if ($db_mysql) {
      
      if (-r "trkm$basename.txt") { 
	open(TRK_TXT, "trkm$basename.txt") || print  "$0: Could not open trkmd$basename.txt\n";
      }
    
      while (<TRK_TXT>) {
	if ($trk_flag == 1){
	  if(/\s*(\d+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)/){
	    
            @row = (
                        {
                           name => "hbtPerEvt_s" . $1,
                           value => $2
			   },
                         {
                           name => "tbtPerEvt_s" . $1,
                           value => $3
			   },
                         {
                           name => "tbtPerHbt_s" . $1,
                           value => $4
			   },
                         {
                           name => "nhitsPerTbt_s" . $1,
                           value => $5
			   }
		    );

	    UpdateRow(@row);

	}
      }
	if ($trk_flag == 2){
	  if (@fields = /\s*(\d+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)/){
	    $i = 1;
	    foreach $f (@fields){
	      if ($i > 1){
		$sl = $i-1;
                @row = (
                         {
                           name => "rms_s" . @fields[0]. "_sl" . $sl,
		           value =>  $f
			   }
                        );
		UpdateRow(@row);
	      }
	      $i++;
	    }
	  }
	}
	if ($trk_flag == 3){
	  if (@fields = /\s*(\d+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)/){
	    $i = 1;
	    foreach $f (@fields){
	      if ($i > 1){
		$sl = $i-1;
		@row = (
			 {
                           name => "flat_s" . @fields[0] . "_sl" . $sl,
		           value => $f
			   }
                        );
                UpdateRow(@row);
	      }
	      $i++;
	    }
	  }
	}
	if (/hbt/){
	  $trk_flag = 1;
	}
	if (/SL_RMS/){
	  $trk_flag = 2;
	}
	if (/SL_DOCA_FLATNESS/){
	  $trk_flag = 3;
	}
	if (/number of 3 track events:(\s*)(\d+)/) {
	  $RUN{"3 track events"} = $2;
	}
	if (/beam position     :(\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)/) {
	    @row = (
		    {
			name => "beamPosX",
			value => $2
			},
		    {
			name => "beamPosY",
			value => $4
			},
		    {
			name => "beamPosZ",
			value => $6
			}
		    );
	    UpdateRow(@row);
	}
	if (/beam fit sigma    :(\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)/) {
            @row = (
		    {
			name => "beamFitSigmaX",
			value => $2
			},
                    {
			name => "beamFitSigmaY",
			value => $4
			},
                    {
			name => "beamFitSigmaZ",
			value => $6
			}
                    );
	    UpdateRow(@row);
	}
	if (/beam fit chi2\/dof :(\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)/) {
             @row = (
		     {
			 name => "beamFitChi2X",
			 value => $2
			 },
		     {
			 name => "beamFitChi2Y",
			 value => $4
			 },
		     {
			 name => "beamFitChi2Z",
			 value => $6
			 }
		     );
	     UpdateRow(@row);
	}
      }
    
      close(TRK_TXT);

  }

#PERL DATABASE INSERTION:

else {
      dbmopen(%RUN,$db_file, 0644) || print  "$0: Could not open $db_file for offline db\n";

      $RUN{"trk_mon command"} = $trk_mon_cmd;

      if (-r "trkm$basename.txt") { 
	open(TRK_TXT, "trkm$basename.txt") || print  "$0: Could not open trkmd$basename.txt\n";
      }
    
      while (<TRK_TXT>) {
	if ($trk_flag == 1){
	  if(/\s*(\d+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)/){
	    $key = "hbt/evt_" . $1;
	    $RUN{$key} = $2;
	    $key = "tbt/evt_" . $1;
	    $RUN{$key} = $3;
	    $key = "tbt/hbt_" . $1;
	    $RUN{$key} = $4;
	    $key = "nhits/trk_" . $1;
	    $RUN{$key} = $5;
	  }
	}
	if ($trk_flag == 2){
	  if (@fields = /\s*(\d+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)/){
	    $i = 1;
	    foreach $f (@fields){
	      if ($i > 1){
		$sl = $i-1;
		$key = "rms_s" . @fields[0]. "_sl" . $sl;
		$RUN{$key} = $f;
	      }
	      $i++;
	    }
	  }
	}
	if ($trk_flag == 3){
	  if (@fields = /\s*(\d+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)/){
	    $i = 1;
	    foreach $f (@fields){
	      if ($i > 1){
		$sl = $i-1;
		$key = "flat_s" . @fields[0] . "_sl" . $sl;
		$RUN{$key} = $f;
	      }
	      $i++;
	    }
	  }
	}
	if (/hbt/){
	  $trk_flag = 1;
	}
	if (/SL_RMS/){
	  $trk_flag = 2;
	}
	if (/SL_DOCA_FLATNESS/){
	  $trk_flag = 3;
	}
	if (/number of 3 track events:(\s*)(\d+)/) {
	  $RUN{"3 track events"} = $2;
	}
	if (/beam position     :(\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)/) {
	  $RUN{"Beam x position"} = $2;
	  $RUN{"Beam y position"} = $4;
	  $RUN{"Beam z position"} = $6;
	}
	if (/beam fit sigma    :(\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)/) {
	  $RUN{"Beam fit sigma x"} = $2;
	  $RUN{"Beam fit sigma y"} = $4;
	  $RUN{"Beam fit sigma z"} = $6;
	}
	if (/beam fit chi2\/dof :(\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)(\s*,\s*)(-?\d+.\d+)/) {
	  $RUN{"Beam fit chi2 x"} = $2;
	  $RUN{"Beam fit chi2 y"} = $4;
	  $RUN{"Beam fit chi2 z"} = $6;
	}
      }
    
      close(TRK_TXT);

      dbmclose(%RUN);
  }

    MoveFile("trkm*txt", $OutPath, "trk_mon txt file", $db_file);
    MoveFile("trkm*hbook", $OutPath, "trk_mon histo file", $db_file); 

    $t = time();
    $end_time = &ctime($t);

    print "\n$0 FINISHED: $trk_mon_cmd at $end_time\n";

    print "\n\n";

}

1;
