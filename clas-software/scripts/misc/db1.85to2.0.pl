#!/usr/local/bin/perl

if ($ARGV[0] eq "-h" || $#ARGV<1) { 
    &PrintUsage;
    exit(0);
}else{
    $dbin=$ARGV[0];
    $dbout=$ARGV[1];
}
if($dbin eq $dbout){
    &PrintUsage;
    exit(0);
}

@dblist=glob("$ARGV[0]/*.db");
foreach $dbinfile(@dblist){
    $dboutfile = $dbinfile;
    $dboutfile =~ s/$dbin/$dbout/; 
    $dump_cmd="db_dump185 -f$dboutfile.temp $dbinfile";
    $load_cmd="db_load -f$dboutfile.temp $dboutfile";

    system("$dump_cmd");
    system("$load_cmd");
    system("rm $dboutfile.temp");
    print "$dbinfile converted to $dboutfile\n";
}

sub PrintUsage {
    print "\nUsage: $0 dbin dbout \n";
    print "\tconverts all .db file in dbin directory from berkley db vs1.85\n";
    print "\tinto berkely .db vs2.0 and stores the new files in dbout\n";
    print "\tdbin and dbout must be different\n";
}
