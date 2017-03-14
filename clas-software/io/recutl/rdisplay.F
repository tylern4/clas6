      SUBROUTINE RDISPLAY
c
c_begin_doc
c  RCS ID string
c  $Id: rdisplay.F,v 1.3 2000/11/10 21:45:15 boiarino Exp $
c
c  Documentation for subroutine RDISPLAY
c
c  Purpose: selects and packs those BANKS needed for the event DISPLAY
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Mon Mar  4 15:10:19 EST 1996
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
#include "bcs.inc"
#include "bnkfmt.inc"
#include "scat_control.inc"
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
      PARAMETER (CFILE = '$RCSfile: rdisplay.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/11/10 21:45:15 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: rdisplay.F,v 1.3 2000/11/10 21:45:15 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='RDISPLAY')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER MAMIND, scat_inithandle, LENOCC
c      EXTERNAL MAMIND, scat_inithandle, LENOCC
      INTEGER  MAMIND, LENOCC
      EXTERNAL MAMIND, LENOCC
c
      INTEGER I, NAMI, IND, IRET, NFAIL
      CHARACTER*132  msg
      CHARACTER*132 CTMP
      logical linit
      data linit/.true./
c_end_var

c  executable code for routine RDISPLAY:
c----6----------------------------------------------------------------72
c
c
        IF (.NOT. LSCAT .OR. SCAT_HANDLE(1) .LT. -1) THEN
         NFAIL = NFAIL + 1
         IF (NFAIL .LT. 10) CALL RECMES(CRNAME,'I',
     1      'Communications to EventStore NOT enabled, cannot send
     2       events')
         return
        endif
c
c
        DO 100 I=1,MIN(NBANK_DISPLAY,MAX_BANK)
           NAMI = MAMIND(IW,CBANK_DISPLAY(I))
           IND = IW(NAMI)
 10        IF (IND .EQ. 0) GOTO 100
               CTMP = CBANK_FORMAT(I)
           CALL SCAT_PACK(CBANK_DISPLAY(I), CTMP(1:LENOCC(CTMP)),
     1                        IW(IND-2), IW(IND-4), IW(IND-5),
     2                        IW(IND), IW(IND+1), IRET)
           IND = IW(IND-1)
           GOTO 10
 100    CONTINUE

        msg(1:8) = 'create  '
        msg(9:132) = scat_name
c        write(msg,29)scat_name(1:lenocc(scat_name))
 29     format('create  ',a120)
        CALL SCAT_SENDEVTNR(SCAT_HANDLE(1), msg, iret)
cc        write(*,*) "scat_sendevtnr iret=", iret
        if (linit) then
         CALL SCAT_PACKFORGET
        endif
        linit = .false.
        RETURN
        END
c
c------------------------------------------------------------------------------





