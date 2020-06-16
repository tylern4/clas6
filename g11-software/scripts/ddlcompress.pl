#!/usr/local/bin/perl

#
# $Id : $
#        D. P. Weygand
#



while (<>) {
    if (/formatBank\(\"(\w+\s*)\",\s*\"([\w,]+)\"\);/) {
	print "formatBank\(\"$1\",\"";
	$k = &compress($2);
	print "\"\)\;\n";
	print "if\(verbose\) fprintf\(out,\"$1 bank defined with format: $k\\n\"\)\;\n";
	<>;
    }

    else {
	print;
    }

    
}

sub compress {
    local $st,$k,$n,$last,$delim,$ret;
    $delim = "";
    $last = "";
    $st = $_[0];
#   print "\nst: $st\n";
    @i = split(/[, \"]/,$st);
    $n = 0;
    $ret = "";

    foreach $i (@i) {
#	print "\nlast: $last i: $i delim: $delim n: $n\n";
	if ($i eq $last) {
	    $n++;
	}
	else {
	    if (length($last)) {
		if ($n > 1) {
		    print "$delim$n$last";
		    $ret = $ret .  "$delim$n$last";
		}
		else {
		    print "$delim$last";
		    $ret = $ret . "$delim$last";
		}
		$last = $i;
		$n = 1;
		$delim = ",";
	    }
	    else {
		$n = 1;
		$last = $i;
	    }
	}
    }

    if ($n > 1) {
	print "$delim$n$last";
	$ret = $ret . "$delim$n$last";
    }
    elsif ($n == 1) {
	print "$delim$last";
	$ret = $ret . "$delim$last";
    }
    
    else {
	print "$delim$last";
	$ret = $ret . "$delim$last";
    }
 	    
    $ret;
	    
	
}


