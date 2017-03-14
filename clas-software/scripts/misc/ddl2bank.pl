#!/usr/local/bin/perl

#
# $Id : $
#        S. McAleer,  J. Manak
#        Florida State University
#        December 10, 1997
#

# This script reads in a ddl file and writes to STDOUT a configure_banks.c file
# with print statements containing the bank names and their format


# Print include information
print "#include <string.h>\n";
print "#include <stdio.h>\n";
print "#include <stdlib.h>\n";
print "#include <errno.h>\n";
print "#include <ntypes.h>\n";
print "#include <bostypes.h>\n";
print "#include <utility.h>\n";
print "\n";

# Print function header
print "int configure_banks(FILE *out, int verbose){\n\n";

$bank = 0;

while (<>) {
    if ($_ =~/^\s*!/) {
	next;
    }
    $_ =~ s/^\s+//;
    @line = split(/\s+/, $_);
    if ($line[0] =~ /TABLE/) {
	$bank_name = $line[1];
	@bank = split(//,$bank_name);
	if ($#bank < 3) {
	    for ($i = $#bank; $i < 3; $i++) {
		$bank_name = $bank_name." ";
	    }
	} elsif ($#bank > 3) {
	    $bank_name = substr($bank_name, 0, 4);
	}
	if ($line[2] =~ /!/) {
	    $bank = 1;
	} else {
	    $format = $line[2];
	}
	next;
    }
    if ($bank == 1) {
	$var = $line[2];
	$var =~ s/i/I/;
	$var =~ s/f/F/;
	$format = $format.$var.",";
    }
    if ($line[0] =~/^END$/) {
	if ($bank == 1) {
	    chop($format);
	    chop($format);
	}
	$bank = 0;
	print "  formatBank(\"$bank_name\", \"$format\");\n";
	print "  if(verbose) fprintf(out, \"$bank_name  bank defined with format: $format\\n\");\n\n";
	$format = "";
    }
}

print "\n }\n";

