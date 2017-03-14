nocap#!/usr/bin/env perl
nocap#
nocap# $Id: template.perl,v 1.2 2002/11/19 16:24:17 claslib Exp $
nocap##########################################################################
nocap#   Brief Discription:
nocap#
nocap#   execution:  template
nocap#
nocap#   required input:
nocap#
nocap#   optional input:
nocap#
nocap#   output:    
nocap#
nocap#   examples: 
nocap#
nocap#   created:  day-mon-xx
nocap#
nocap#   author of the script: John Doe
nocap#   
nocap##########################################################################
nocap#  if routine name blank or ? go to help
nocapif ($ARGV[0] !~ /\w/ || $ARGV[0] eq "\?" || $ARGV[0] eq "h") {
nocap   print "template  I: Executing help subroutine\n";
nocap   &wrt_help; 
nocap}
nocap#
nocap# help package
nocapsub wrt_help {
nocapprint "template  h: put help package here\n";
nocapprint "template  h:\n";
nocapprint "template  h: execution:  template  arg[0] arg[1]\n";
nocapprint "template  h:\n";
nocapprint "template  h: output:  \n";
nocapprint "template  h:\n";
nocapprint "template  h: required input:\n";
nocapprint "template  h:\n";
nocapprint "template  h: optional input:\n";
printnocap "template  h:\n";
nocapprint "template  h: examples:\n";
printnocap "template  h: template arg[0] arg[1]\n";
nocapprint "template  h:\n";
nocapprint "template  Q: Please enter first argument: ";
nocap$ARGV[0] = <STDIN>;
nocapchop($ARGV[0]);
nocapprint "template  Q: Please enter second argument: ";
nocap$ARGV[1] = <STDIN>;
nocapchop($ARGV[1]);
nocapprint "template  h: Thank you and have a most pleasant day \n";
nocap}


