#! /usr/bin/env perl

$oldmap = $ARGV[0];
shift;


$map = $ARGV[0];

$cmd = "scan_map -t $map |";

open(A,$cmd);

while (<A>) {

	if (/^Map:\s+(\S+)/) {
		$mapfile = $1;
		print "MAP: $mapfile\n";
	}

	elsif (/Subsystem:\s+(\S+),\s+nitems:\s*(\d+)/) {
		$sub = $1;
		$nitems = $2;
	}

	elsif (/Item:\s+(\S+),\s+length:\s+(\d+),\s+type:\s+(\S+),\s+narray:(\d+)/) {
		$item = $1;
		$length = $2;
		$type = $3;
		$narr = $4;
		print "Item: $item $length $type $narr\n";
		$_ = <A>;
		@fields = split;
		foreach $field (@fields) {
			$_ = $field;
			if (/INF/) {
				;
			}
			elsif (/(\d+)/) {
#				print "EXAMINE RUN $1 $sub $item $length $type $narr\n";
				$run = $1;
				if ($type eq "float") {
				  &examineFloat($oldmap,$mapfile,$run,$sub,$item,$length,$narr);
				}
				elsif ($type eq "int") {
				  &examineInt($oldmap,$mapfile,$run,$sub,$item,$length,$narr);
				}
				


			}
		}
		print "\n";
	}

}

		

sub examineFloat {
  local($omap,$map,$r,$subs,$xitem,$len,$n) = @_;
  $cmdX = "get_map_float -m$map -s$subs -i$xitem -t$r -l$len |";
  $cmdX2 = "get_map_float -m$omap -s$subs -i$xitem -t$r -l$len |";
 print "$cmdX\n";
#  print "$cmdX2\n\n";
  open(X,$cmdX);
  open(X2,$cmdX2);
  while (<X>) {
    chop;
    $new = $_;
    $_ = <X2>;
    chop;
    if ($_ != $new) {
      print "$subs,$xitem,$r:\t$new != $_\n";
    }
  }
  
}
sub examineInt {
  local($omap,$map,$r,$subs,$xitem,$len,$n) = @_;
  $cmdX = "get_map_int -m$map -s$subs -i$xitem -t$r -l$len |";
  $cmdX2 = "get_map_int -m$omap -s$subs -i$xitem -t$r -l$len |";
 print "$cmdX\n";
#  print "$cmdX2\n\n";
  open(X,$cmdX);
  open(X2,$cmdX2);
  while (<X>) {
    chop;
    $new = $_;
    $_ = <X2>;
    chop;
    if ($_ != $new) {
      print "$subs,$xitem,$r:\t$new != $_\n";
    }
  }
  
}
