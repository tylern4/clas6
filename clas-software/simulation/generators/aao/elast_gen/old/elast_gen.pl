#!/usr/local/bin/perl
#
$pgm = 'elast_gen';
$nevents = 20000;
$OS      = $ENV{"OSNAME"};
$TOP_DIR = $ENV{"TOP_DIR"};
$workdir = '/work/clas/disk1/kjoo/elast_gen';
chdir("$workdir");

$outfile1 = '1.6-elast-50000evts';
$outfile2 = '2.4-elast-50000evts';
$outfile3 = '4.0-elast-50000evts';

$cmd = "$TOP_DIR/bin/$OS/$pgm -M$nevents -o$outfile1 -f0.6 -g3 -tl4.0 -tr0.4 -vx.082 -vy-.449 -vz-.139 -E1.645 -emn0.3 -smn18. -smx52. -p0 -c0.2";
print  "cmd: $cmd\n";
system $cmd;

$cmd = "$TOP_DIR/bin/$OS/$pgm -M$nevents -o$outfile2 -f0.6 -g3 -tl4.0 -tr0.4 -vx.082 -vy-.449 -vz-.139 -E2.445 -emn0.3 -smn18. -smx52. -p0 -c0.2";
print  "cmd: $cmd\n";
system $cmd;

$cmd = "$TOP_DIR/bin/$OS/$pgm -M$nevents -o$outfile3 -f0.6 -g3 -tl4.0 -tr0.4 -vx.082 -vy-.449 -vz-.139 -E4.045 -emn0.3 -smn18. -smx52. -p0 -c0.2";
print  "cmd: $cmd\n";
system $cmd;

