      SUBROUTINE USER_TCL_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: user_tcl_init.F,v 1.2 2002/01/18 23:44:16 feuerbac Exp $
c
c  Documentation for subroutine USER_TCL_INIT
c
c  Purpose:  USER routine for initializing runtime variables
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Fri Dec  1 17:04:06 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "user_control.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: user_tcl_init.F,v 1.2 2002/01/18 23:44:16 feuerbac Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='USER_TCL_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, I
      INTEGER ITCL_INT, ITCL_STRING, ITCL_INT_READ, ITCL_DOUBLE, LSTRING
      DATA ITCL_INT, ITCL_STRING, ITCL_INT_READ, ITCL_DOUBLE, LSTRING
     $     /1,4,129,2, 132/
      Character*32 outbos
      
c_end_var
c
c  executable code for routine USER_TCL_INIT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
c  The below call LINKS the fortran variable to the tcl variable, to 
c  interactive change the value of `my_variable' either:
c     A) type  set my_variable 10  at the prompt or in your INITializing script
c     B) setup the communication shell and : SendMsg "set my_variable 10"
c
      DO i=1,10
         outbanknames(i)='junk'
         write(outbos,'(A,I1,A)') 'outbanknames(',i,')'
         call tcl_linkchar(outbos(1:15),outbanknames(i),
     $        itcl_string,lstring)
      ENDDO

      align_vert_cut = 0
      call tcl_linkvar('align_vert_cut',align_vert_cut,itcl_int)

      beam_spot = 5.
      call tcl_linkvar('beam_spot',beam_spot,itcl_double)

      dstmwall = 8.688
      call tcl_linkvar('dstmwall',dstmwall,itcl_double)

      dstmlen = 2.5
      call tcl_linkvar('dstmlen',dstmlen,itcl_double)

      upstmwall = -9.410
      call tcl_linkvar('upstmwall',upstmwall,itcl_double)

      upstmlen = 2.5
      call tcl_linkvar('upstmlen',upstmlen,itcl_double)

      target_wall = 0.3
      call tcl_linkvar('target_wall',target_wall,itcl_double)

      align_photon_run = 0
      call tcl_linkvar('align_photon_run',align_photon_run,itcl_int)
c
      RETURN
      END
c
c------------------------------------------------------------------------------




