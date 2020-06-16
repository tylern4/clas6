#!/usr/bin/env perl
$libname = <>;
chop $libname;
$depdir = <>;
chop $depdir;
$filebase = <>;
chop $filebase;
$add_debug = <>;
chop $add_debug;
$_ = <>;
split/:/;
print "$libname($filebase.o) $depdir/$filebase$add_debug.d: @_[1]";
while (<>) {
    print;
}
