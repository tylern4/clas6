#!/usr/local/bin/perl -w
$count=0;

#extract information from stub files listed in arguments
foreach $name (@ARGV)
{
    open(STUBFILE,$name); #opens the next stub file
    @a=<STUBFILE>; #puts each line of stubfile in array
    print "opening $name   size: $a[10]"; 
#now access 11th line (c-style counting)
    $count += $a[10];
}
print "total size=$count bytes\n";
