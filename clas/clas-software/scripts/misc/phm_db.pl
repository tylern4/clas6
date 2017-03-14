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
	    if ($evt_flag){
		if(@values = /\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)\s*(\w+)/){
		    $i = 0;
		    foreach $f (@names){
			$RUN{$f} = @values[$i];
			$i++;
		    }
		}
	    }
	    if (@values = /pi0 p\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/){
		$i = 1;
		foreach $f (@values){
		    $key = "pi0p_s" . $i;  
		    print $key . " " . $f . "\n";
		    $RUN{$key} = $f;
		    $i++;
		}
	    }
	    if (@values = /pi0 p \(f\)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/){
		$i = 1;
		foreach $f (@values){
		    $key = "pi0p(f)_s" . $i; 
		    print $key . " " . $f . "\n";
		    $RUN{$key} = $f;
		    $i++;
		}
	    }
	    if (@values = /pi\+ n\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/){
		$i = 1;
		foreach $f (@values){
		    $key = "pi+n_s" . $i;  
		    print $key . " " . $f . "\n";
		    $RUN{$key} = $f;
		    $i++;
		}
	    }
	    if (@values = /pi\+ n \(f\)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)/){
		$i = 1;
		foreach $f (@values){
		    $key = "pi+n(f)_s" . $i;  
		    print $key . " " . $f . "\n";
		    $RUN{$key} = $f;
		    $i++;
		}
	    }
	    if (@names = /\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)\s*(\S+)/){
		if (@names[0] =~ /Nevt/) {
		    $evt_flag = 1;
		}
	    }
	}
    }
}


dbmclose(%RUN);


