
C======================================================================
      SUBROUTINE trk_stati(stat_option)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints No. of events left after each cut.
C-
C-
C-   Inputs  :  stat_option : =1 Statistics at End-of-Run
C-                            =2 Statistics after each Event
C-   Outputs :
C-
C-   Calls              : none
C-
C-   Created  JUL-20-1992  Billy Leung
C-	       Aug.1997    F.Klein
C-
C-   Called by trk_evnt_tbt , trk_erun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*  I/O variables
*  -------------
      INTEGER  stat_option
*
*  Include Files
*  -------------
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_stati.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.6 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/08/21 17:44:41 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_stati.F,v 1.6 1998/08/21 17:44:41 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_STATI')
      PARAMETER (CRAUTH='Franz Klein')

      INTEGER type, dtype
      REAL    xcut
      LOGICAL ack
      CHARACTER*30 caption

      COMMON/ErrBook/ type(10), dtype(10), xcut(10), ack(10), caption(10)
*
*  Local User defined variables:
*  -----------------------------
*     
      INTEGER  i
      REAL     fraction, xevents, xlinks

*  Report analysis failures

      if( stat_option.LT.1 ) return
      if(type(1).GT.0) then
        xevents=FLOAT(type(1))/100.
      else
        xevents=0.01
      endif
      if(type(3).GT.0) then
        xlinks=FLOAT(type(3))/100.
      else
        xlinks=0.01
      endif
      if( stat_option.GT.1 )then
       call RecMes(CRNAME,'L',' Cut#  Description               Value     Passed     %       Yes/No')
      else
       call RecMes(CRNAME,'L',' Cut#  Description               Value     Passed     %')
      endif
      DO i = 1,10
        IF (ack(i)) THEN
	  fraction=FLOAT(type(i))/xevents
	  if( stat_option.GT.1 )then
	    write(CRMESS,'(I5,A2,A30,F6.1,I10,F8.1,I10)')i,'  ',caption(i),
     &			 xcut(i), type(i), fraction, dtype(i)
            dtype(i) = 0
          ELSE
	    if(i.LT.3) then
	      write(CRMESS,'(I5,A2,A30,F6.1,I10,F8.1)')i,'  ',caption(i),
     &			 xcut(i), type(i), fraction
	    else
	      write(CRMESS,'(I5,A2,A30,F6.1,I10,2F8.1)')i,'  ',caption(i),
     &			 xcut(i), type(i), fraction, FLOAT(type(i))/xlinks
	    endif
          ENDIF
	  call RecMes(CRNAME,'L',CRMESS)
        ENDIF
      ENDDO
      call RecMes(CRNAME,'L','             ')
      END

