#!/usr/local/bin/perl

# This is a perl library to be used for the CLAS cooking scripts
#
# by: Simeon McAleer
#        Florida State University
#        September 29, 1998

CONFIG:{
    package CheckDir;
}

sub CheckDir{

# Searches for the existence of the directories needed by ProcessRun and RunJob
# If those directories exist the routine exits. If they don't exist CreateDirs creates them
# Usage
#    CheckDir(dir)
#                     dir = directory to check

  local($dir) = $_[0];

  print "\n Checking structure of $dir.\n";
    
  if (!(-e $dir)) {
    print "Directory $dir does not exist. Will try to make it.\n";
    mkdir("$dir", 0777) || die "Could not create $dir. Please mkdir $dir and try again.\n";  
    print "Succeeded in making $dir.\n";
  }
  
  if (!(-r $dir) || !(-w $dir)) {
    print "The permissions on $dir are not properly set. Will try to correct them.\n";
    chmod("$dir", 0777) || die "Could not chmod $dir. Please chmod $dir and try again.\n";
    print "Succeeded in changing premissions on $dir\n";
  }
  
}
  
1;
