**************  tag_calib  *******************************
*  Status 2008 (g9a):  
*  program tagraw should produce "st_time - tag_time" plots but that
*    needs some tracking ... (to be added later)
*  Note that left-right offsets can be off by multiples of 2.004 nsec (=RF cycles)
*    histograms "ttag" and "tpho" show such that offset ... offsets for Tleft & Tright
*    have to be adjusted by hand! 
*
**********************************************************

To get things running:

1.) Check out the content of this CVS directory

2.) Make sure that your CLAS environment is set up correctly.
     - Pay particular attention to the RunIndex, if you use a private one.
     - Use the LATEST libraries, the older one might not recognize TGTL banks.

3.) Type "make" to compile the program tagraw

4.) Run "tagraw <file-1> <file-2> ... <file-n>" on raw bos files.
     - Use files with low current if available.
     - A million trigger should do the job.
     - Cooked files can be used as well if TGTL/R banks are present.

4.a) or run "tagraw -h" for options
 
5.) Start "root <tagraw.result-from-step-4.root>" 

6.) Check RF slopes using ".x rf_check.C"  (in particular check for background hits)

7.) Get left-right offsets and slopes with ".x tagslope"
    Check histograms! (You might need several raw bos files to get the statistics)
    - Click on the yellow shaded surveillance histograms to see the
    underlaying fit.

8.) If satisfied, click on the checkin button.

9.) When left-right slopes and offsets are checked in, rerun "tagraw" and
    type ".x ci_adjust.C"
    to get the tag_t.ci offsets 
    (plots for single counters and checkin option using the Menu list (right mouse in pad)
     or clicking on the upper result histogram)

10.) To get E-counter offsets
    type ".x edt_fit.C"
    (plots for single counters and checkin option using the Menu list (right mouse in pad)
     or clicking on the upper result histogram)

