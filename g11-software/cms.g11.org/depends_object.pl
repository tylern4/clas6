#!/usr/bin/env perl
$dependsdir = $ARGV[0];
$libdir = $ARGV[1];
if ($#ARGV == 2) {
    $add_debug = $ARGV[2];
} else {
    $add_debug = '';
}
$firstline = <STDIN>;
split(/\./, $firstline);
$filename_no_ext = $_[0];
split(/:/, $firstline);
$dependencies = $_[1];
print "$dependsdir/$filename_no_ext$add_debug.dd"
    . " $libdir/$filename_no_ext$add_debug.o: $dependencies";
while (<STDIN>) {
    print;
}
# end of perl script
