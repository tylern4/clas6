#!/usr/bin/env perl
# set return status to OK
$status = 0;

$clasbanks_h = $ARGV[0];
$clasbanks_cc = $ARGV[1];
#print "output: " . $clasbanks_h . " and " . $clasbanks_cc . "\n";

open (HEADER, ">".$clasbanks_h);
# open the source file and start writing to it
open (SOURCE, ">".$clasbanks_cc);
print SOURCE "#include \<stddef.h\>\n";
print SOURCE "#include \<string.h\>\n";
print SOURCE "#include \"clasbanks.h\"\n";
print SOURCE "extern \"C\" {\n";
print SOURCE "#include \"clas_common.h\"\n";
print SOURCE "}\n";
print SOURCE "\n";
print SOURCE "int get_bos_index(int array_key, char *bankname, int bank_index);\n";
print SOURCE "\n";

foreach $file_ddl (@ARGV) {
    if ($file_ddl !~ "clasbanks.ddl"
    	&& $file_ddl !~ $clasbanks_h
    	&& $file_ddl !~ $clasbanks_cc
    	&& $file_ddl !~ "aaaa.ddl"
        && $file_ddl !~ "tspr.ddl"
        && $file_ddl !~ "tsrg.ddl"
        && $file_ddl !~ "zzzz.ddl") {
#        print "working on $file_ddl\n";
# open the ddl file
        open (DDL, $file_ddl);
        $ncol = 0;
        $array = "";
        while ($line = <DDL>) {
            chop $line;
# remove leading spaces
            while (substr($line, 0, 1) eq " ") {
                $line = substr($line, 1);
            }
# check for a comment
            if (substr($line, 0, 1) ne "!") { # this is probably not a comment
                @field = split(/\s+/, $line);
                if (substr($field[0], 0, 1) ne "!") { # un-comment-like
                    if ($field[0] eq "TABLE") {
#                       print "table found\n";
                        $bankname = $field[1];
                        $banktype = $field[2];
                    } elsif ($field[0] eq "END") {
#                       print "end of table\n";
                    } elsif ($field[0] eq "ARRAY") {
                        $array = $field[1];
                    } elsif ($field[0] eq "") {
#                       print "blank line\n";
                    } else {
                        $col[$ncol] = $line;
                        $ncol++;
                    }
                }
            }
        }
# close the ddl file
        close(DDL);
        print "$bankname ";
#       for ($icol = 0; $icol < $ncol; $icol++) {
#           print "$icol $col[$icol]\n";
#       }
#
# write this bank's info to the header file
#
# declare the row overlay structure
        print HEADER "struct $bankname\_row {\n";
        for ($icol = 0; $icol < $ncol; $icol++) {
            $line = $col[$icol];
            @field = split(/\s+/, $line);
            if ($field[2] eq "I" || $field[2] eq "i") {
                if ($banktype eq "B16") {
                    print HEADER "  short $field[1];\n";
                } else {
                    print HEADER "  int $field[1];\n";
                }
            } elsif ($field[2] eq "F" || $field[2] eq "f") {
                print HEADER "  float $field[1];\n";
            } elsif ($field[2] eq "A" || $field[2] eq "a") {
                print HEADER "  char $field[1]\[4\];\n";
            } else {
                print "unrecognized field making structure: /$field[0]/$field[1]/$field[2]/$field[3]/\n";
                $status = 1;
            }
        }
        print HEADER "};\n";
        print HEADER "\n";
# declare the class
        print HEADER "class $bankname {\n";
        print HEADER "private:\n";
        print HEADER "  int nrows;\n";
        print HEADER "  struct $bankname\_row *row_ptr;\n";
        print HEADER "public:\n";
        print HEADER "  $bankname(void) {\n";
        print HEADER "    return;\n";
        print HEADER "  }\n";
        print HEADER "  $bankname(int bank_index);\n";
        print HEADER "  int get_nrows(void) {\n";
        print HEADER "    return(nrows);\n";
        print HEADER "  }\n";
        for ($icol = 0; $icol < $ncol; $icol++) {
            $line = $col[$icol];
            @field = split(/\s+/, $line);
            if ($field[2] eq "I" || $field[2] eq "i") {
                if ($banktype eq "B16") {
                    print HEADER "  short get_$field[1](int row) {\n";
                } else {
                    print HEADER "  int get_$field[1](int row) {\n";
                }
            } elsif ($field[2] eq "F" || $field[2] eq "f") {
                print HEADER "  float get_$field[1](int row) {\n";
            } elsif ($field[2] eq "A" || $field[2] eq "a") {
                print HEADER "  char *get_$field[1](int row) {\n";
            } else {
                print "unrecognized field declaring get, /$field[0]/$field[1]/$field[2]/$field[3]/\n";
                $status = 1;
            }
            print HEADER "    return ((row_ptr+row)->$field[1]);\n";
            print HEADER "  }\n";
        }
        print HEADER "};\n";
        print HEADER "\n";
#
# figure out which array to go to
#
        if ($array eq "JW" || $array eq "jw") {
            $array_key = 1; # wcs common
        } else {
            $array_key = 0; # bcs common, the default
        }
#
# write this bank's info to the source file
#
        print SOURCE "\n";
# construct the constructor
        print SOURCE "$bankname\:\:$bankname", "(int bank_index) {\n";
        print SOURCE "  int bos_ind, ind;\n";
        print SOURCE "  char bankname[4];\n";
        print SOURCE "  strcpy(bankname, \"$bankname\");\n";
        print SOURCE "  bos_ind = get_bos_index($array_key, bankname, bank_index);\n";
        print SOURCE "  if (bos_ind) {\n";
# change from FORTRAN indexing to C indexing
        print SOURCE "    ind = bos_ind - 1;\n";
        if ($array_key) {
            print SOURCE "    nrows = wcs_.jw[ind - 4];\n";
            print SOURCE "    row_ptr = ($bankname\_row *)(&wcs_.jw[ind + 1]);\n";
        } else {
            print SOURCE "    nrows = bcs_.iw[ind - 4];\n";
            print SOURCE "    row_ptr = ($bankname\_row *)(&bcs_.iw[ind + 1]);\n";
        }
        print SOURCE "  } else {\n";
        print SOURCE "    nrows = 0;\n";
        print SOURCE "    row_ptr = NULL;\n";
        print SOURCE "  }\n";
        print SOURCE "}\n";
    }
}
# close the header file
close (HEADER);
# close the source file
close (SOURCE);
print "\n";
print "clasbanks.pl exiting with status = $status\n";
exit $status;
