#! /usr/local/bin/perl

$pack = $ENV{"CLAS_PACK"};
$n = 100;

open(A,"< $pack/include/bosddl.h");

while(<A>) {
	if (/clas(\S+)_t/) {
		print "#define GROUP_$1 $n /* clas$1_t (see bosddl.h) */\n";
		$n += 10;
	}
}
