#! /usr/local/bin/perl
# usage:  kill_all file
# kills all farm jobs in job-list file.  
# note: assumes that first line is content free
#       assumes that first word is a process number
# example:
# jlabs1> jobstat > jobstat.list
# jlabs1> kill_all.pl jobstat.list
#
# note that this reverses the list so that the example above kills 
# running jobs last 

$line=<>;

while($line=<>){
    @word=split(/ /,$line);
    push(@list,$word[0]);
}

@rlist=reverse(@list);
foreach $i (@rlist){
    print "killing: ";
    print $i;
    print "\n";
    system "jkill $i";
}


