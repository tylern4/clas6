c      include file USER_CONTROL
c
c_begin_doc
c  RCS ID string
c  $Id: user_control.inc,v 1.2 2002/01/18 23:44:15 feuerbac Exp $
c
c  Documentation for include file USER_CONTROL
c
c  Purpose:  holds user control variables
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Nov 21 10:11:57 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file USER_CONTROL:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIUSER_CONTROL
      DATA CIUSER_CONTROL/
     1'$rcsId$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in USER_CONTROL common block
c  --------------------------------------------
c
c_end_var
c  the following is an example of a runtime controlable variable
c  look in user_tcl_init.F to see how to link this variable to a
c  tcl variable
c
       INTEGER lmy_string
       parameter (lmy_string = 10)

       integer my_integer
       REAL*8  my_real     ! tcl doesn't know about single precision reals
       real*8  beam_spot,dstmwall,dstmlen,upstmwall,upstmlen,target_wall

       logical align_chuck
       integer align_vert_cut
       integer align_photon_run

       CHARACTER*10 my_string	
       CHARACTER*132 outbanknames(10)	
       
       COMMON/USER_CONTROL/
     $      outbanknames,
     $      beam_spot,dstmwall,dstmlen,
     $      upstmwall,upstmlen,target_wall,
     $      align_vert_cut,align_chuck,align_photon_run
c----6----------------------------------------------------------------72




