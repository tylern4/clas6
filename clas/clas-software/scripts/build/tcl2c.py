#! /usr/bin/env python

import os, fileinput, sys

from optparse import OptionParser
parser = OptionParser(
    usage="usage: %prog -i infile.tcl -o outfile_tcl.c -f tcl_procs.h",
    version="%prog 1.0")

parser.add_option("-o", "--outfile", dest="outfile",
    help="output file", metavar="string")
parser.add_option("-i", "--infile", dest="infile",
    help="input file", metavar="string")
parser.add_option("-f", "--headerfile", dest="headerfile",
    help="header file", metavar="string")
(options, args) = parser.parse_args()

#print "==== tcl2c.py =================="
#print "input file:  " + options.infile
#print "output file: " + options.outfile
#print "header file: " + options.headerfile
#print "================================"

base_tcl_fname = os.path.basename(options.infile)
base_c_fname = os.path.basename(options.outfile)

proc_name = base_tcl_fname[:-4]+"_tcl_proc"

fout = open(options.outfile, 'w')
fout.write("\n\nchar " + proc_name + "[]=\n")
for line in fileinput.input(options.infile):
    line = line.replace("\\\n","\\\\")
    line = line.replace('"','\\"')
    fout.write("\"" + line[:-1] + "\\n\"\n")
fout.write("\n;\n\n")
fout.close()

os.popen('touch ' + options.headerfile)
header_line = "extern char " + proc_name + "[];\n"
headerfile = open(options.headerfile, 'r+')
if header_line not in headerfile.readlines():
    headerfile.write(header_line)
headerfile.close()
