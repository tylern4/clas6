#! /usr/local/bin/perl

@arr = ();
for (@ARGV) {

    if(/^-d(.+)/)      { $dbfile = $1;}

    else {
        push(@arr,($_));
    }
}

# undef - remember
dbmopen(%RUN, $dbfile, 0644) || print L "Could not open database for offline\n";

for (@arr){
    if (-r $_){
	open(INFILE, $_);
	print "opening file: " . $_ . "\n";
	while (<INFILE>){
	    if ($trk_flag == 1){
		if(/\s*(\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)/){
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
		if (@fields = /\s*(\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)/){
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
		if (@fields = /\s*(\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)\s*(\w+.\w+)/){
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
	}
    }
}


dbmclose(%RUN);


