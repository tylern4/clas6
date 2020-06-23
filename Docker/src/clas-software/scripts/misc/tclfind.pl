#!/usr/local/bin/perl
open (LINKVAR, "find . -name \"*.F\" |");
while (<LINKVAR>) {
    #print;
    chop;
    open (GREP, "grep -i tcl_linkvar $_ |");
    while (<GREP>) {
	#print;
	@field = split(/'/);
	$varname = $field[1];
	print "puts \"$varname=\$$varname\"\n";
    }
}
