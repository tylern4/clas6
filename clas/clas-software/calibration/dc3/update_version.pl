#!/apps/bin/perl
#
# This script just reads in the Makefile and increments the build
# number by one before re-writing it.
#

open(FILE,"Makefile");

@body=();

while (<FILE>) {
	if (/^VER_BUILD/) {
		@arr=split(/=/,$_);
		$ver=$arr[1]+1;
		push(@body,"VER_BUILD = $ver\n");
#		print "VER_BUILD = $ver\n";
	}else{
		push(@body,$_);
#		print $_;
	}
}
close(FILE);

open(FILE,">Makefile");

print FILE @body;
close(FILE);

