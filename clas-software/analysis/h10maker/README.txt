This program is a cannibalized nt10maker program.  The approach used
at JLab to date is to

1. Use BOS files for Fortran processing of data via common utilities.

2. Convert these BOS files into PAW hbook files via nt10maker.

3. Convert the hbook files into ROOT files via h2root.

Recently however, there has been a problem with this approach.  The
BOS files contain good data, but nt10maker stopped working properly.
The PAW library is a middle man, since virtually everyone works with
ROOT now.  The only challenge is to link to both Fortran and C++ code.
This is trivial, although for some reason it hasn't been done very
often.  This program is the first implementation I know of to do this.

The design of the program is as follows:

* The critical BOS sections of the nt10maker code are kept in tact,
  but provided to C++ via header files.

* The common blocks are simply global structs, so C++ headers are
  provided for these as well.

* The fill_* functions are kept intact, since they are simply
  accessing a single event of BOS data.

* Various functions are given for working with and initializing the
  branches.

======================================================================
NOTES:

* There is a common problem causing segmentation violations whenever I
  copy code from various nt10maker code.  The source is the ierr
  variable being passed or not passed to the Fortran routines.  I
  don't need the ierr variable because I use routines which return
  integers, or simply don't care about the error condition because it
  wasn't used by the routines in any case.  However, when copying
  routines, this causes errors because Fortran assumes an integer will
  be allocated in memory, and C++ assumes there isn't any need, so at
  run time the memory isn't available.
