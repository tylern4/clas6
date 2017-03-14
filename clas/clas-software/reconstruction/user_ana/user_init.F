      SUBROUTINE USER_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: user_init.F,v 1.14 2008/09/15 20:59:46 goetz Exp $
c
c  Documentation for subroutine USER_INIT
c
c  Purpose:  USER routine called ONCE at initialization time
c  --------  This routine using the pacjkge tccl variable allow the booking
c            of histograms.
c
c            The user must also edit the subroutine user_last.F to insure the
c            HBOOK file is closed properly. ...No guarantees implied....
c
c  Input Parameters: NONE
c  ----------------
c
c  Output Parameters: NONE
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Francois ROUDOT      Created:  Wed May 28 16:00:00 EDT 1997
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
#include "bcs.inc"
#include "clasmdl.inc"
#include "run_control.inc"
#include "histtcl.inc"
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
      PARAMETER ( CFILE = '$RCSfile: user_init.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.14 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/09/15 20:59:46 $')
      PARAMETER ( CAUTHO = '$Author: goetz $')
      DATA CRCSID/
     1'$Id: user_init.F,v 1.14 2008/09/15 20:59:46 goetz Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='USER_INIT')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LNBLNK    ! cern routine (string length without blanks)
      INTEGER NWRITE
      INTEGER mbank, ind, i, istat, nrf

c_end_var
c
c  executable code for routine USER_INIT:

c  -------------------------------------
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Histograming and Analysis pakage initialization'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c  First toggle on log file, you can set this to false if your tired
c   of all the REnnnn.LOG files.

      CALL SET_LOG(.TRUE.)

c  Register my USER package with job database....
      CALL REVINI('USER',CREVIS,CDATE)

      if(LNBLNK(eventtagfile).GT.0) then
       call reslun('user_init', let, istat)
       if (istat .eq. 1) then
         OPEN (unit=LET,file=eventtagfile,STATUS='UNKNOWN')
       endif
      endif
c Initialize c_sql if tcl is set
      if(LMySQL)then
         call init_csql_clas(nccrun,runfile,'CSQL')
      endif
c changed back to top directory
c -------------  Call the booking stuff for all packages ----------------
      IF ((LUSR0_DO .OR. LUSR1_DO).and.(LUSR0_H_DO .OR. LUSR1_H_DO)) THEN
        CALL USER_BOOK
        CALL RECMES(CRNAME,'I','USER Histo package initialized')
      ENDIF
c
      IF (LSEB_H_DO) THEN
       CALL SEB_BOOK
       CALL RECMES(CRNAME,'I','Seb Histo package initialized')
      ENDIF
c
      IF (LEGN_H_DO) THEN
       CALL EC_BOOK
       CALL RECMES(CRNAME,'I','EC Histo package initialized')
      ENDIF
c
      IF (LTRK_H_DO) THEN
       CALL DCTRK_BOOK
       CALL RECMES(CRNAME,'I','TRK Histo package initialized')
      ENDIF
c
      IF (LTOF_H_DO) THEN
         CALL SC_BOOK
         CALL RECMES(CRNAME,'I','TOF Histo package initialized')
      ENDIF
c
      IF (LEC1_H_DO) THEN
         CALL EC1_BOOK
         CALL RECMES(CRNAME,'I','EC1 Histo package initialized')
      ENDIF
c
      IF (LCC_H_DO) THEN
         CALL CC_BOOK
         CALL RECMES(CRNAME,'I','CC Histo package initialized')
      ENDIF
c
      IF (LIC_H_DO) THEN
         CALL IC_BOOK
         CALL RECMES(CRNAME,'I','IC Histo package initialized')
      ENDIF
c
      IF (LTAGGER_H_DO) THEN
       CALL TAG_BOOK(.true.,.true.,.true.,.true.)
       CALL RECMES(CRNAME,'I','Tagger Histo package initialized')
      ENDIF
cStep
      IF (LALL_NT_DO) THEN
       CALL ALL_NT_BOOK
       CALL RECMES(CRNAME,'I','Booking N-tupls for all banks.')
      ELSE
         IF (LIC_DO.AND.LICHB_NT_DO)THEN
            call ic_book
         ENDIF
         IF (LMCTK_NT_DO) THEN
            CALL HCDIR('//PAWC',' ')
            CALL HCDIR('//LUN1',' ')
            CALL HBNT(19,'MC_out',' ')
            CALL HBNAME(19,' ',0,'$clear')
            CALL BOOK_MC_NT(19)
            CALL RECMES(CRNAME,'I','Booking N-tupls for MCTK bank.')
         ENDIF
      ENDIF

      RETURN
      END
c
c------------------------------------------------------------------------------






