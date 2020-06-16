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

$total = 0.0;
for (@arr){
    if (-r $_){
	open(INFILE, $_);
	print "opening file: " . $_ . "\n";
	while (<INFILE>){
	    if ($go_ps == 1){
		$total = $total + $_;
	    }
	    if (/Production PS trues/){
		$go_ps = 1;
	    }
	    if (/Production PS accidentals/){
		$go_ps = 0;
	    }
	}
    }
}

print "integrated_PS_scalers: ". $total. "\n";

$RUN{"integrated_PS_scalers"} = $total;

dbmclose(%RUN);
