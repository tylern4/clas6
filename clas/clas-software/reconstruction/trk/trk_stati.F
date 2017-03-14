
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
      PARAMETER ( CREVIS = '$Revision: 1.8 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/09/01 20:25:45 $')
      PARAMETER ( CAUTHO = '$Author: lcsmith $')
      DATA CRCSID/
     1'$Id: trk_stati.F,v 1.8 2004/09/01 20:25:45 lcsmith Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_STATI')
      PARAMETER (CRAUTH='Franz Klein')

      INTEGER type, dtype, ddtype
      REAL    xcut
      LOGICAL ack
      CHARACTER*30 caption

      COMMON/ErrBook/ type(3,6,10), dtype(10), ddtype(10), xcut(10), ack(10), caption(10)
*
*  Local User defined variables:
*  -----------------------------
*     
      INTEGER  i,s
      REAL     fraction, xevents, xlinks

*  Report analysis failures

      if( stat_option.LT.1 ) return

      if( stat_option.eq.3) then
      
      call RecMes(CRNAME,'L',' Cut#        Description            Value ')
      
      do i=1,9
        if (ack(i)) then
	write(CRMESS,'(I5,A2,A30,F6.1,I10,F8.1)') i,'  ',caption(i), xcut(i) 
        call RecMes(CRNAME,'L',CRMESS)
        endif
      enddo
          
      call RecMes(CRNAME,'L','.  S1    S2    S3    S4    S5    S6')
      
      do i = 1,9
        if (ack(i)) then
          if(i.eq.1) then
	  write(CRMESS,'(i1,6i6,6f5.2)') 
     &    i,(type(1,s,i),s=1,6),
     &    (float(type(1,s,i))/max(0.01,float(type(1,s,1))),s=1,6)
          elseif(i.eq.2) then
	  write(CRMESS,'(i1,6i6,6f5.2)') 
     &    i,(type(1,s,i),s=1,6),
     &    (float(type(1,s,i))/max(0.01,float(type(1,s,1))/6.),s=1,6)
	  elseif(i.le.4) then
	  write(CRMESS,'(i1,6i6,6f5.2,6f5.2)') 
     &    i,(type(1,s,i),s=1,6),
     &    (float(type(1,s,i))/max(0.01,float(type(1,s,2))),s=1,6),
     &    (float(type(1,s,i))/max(0.01,float(type(1,s,3))),s=1,6) 
          else
	  write(CRMESS,'(i1,6i6,6f5.2,6f5.2,6f5.2)') 
     &    i,(type(1,s,i),s=1,6),
     &    (float(type(1,s,i))/max(0.01,float(type(1,s,2))),s=1,6),
     &    (float(type(1,s,i))/max(0.01,float(type(1,s,3))),s=1,6), 
     &    (float(type(1,s,i))/max(0.01,float(type(1,s,5))),s=1,6)          
	  endif
        endif
	call RecMes(CRNAME,'L',CRMESS)
      enddo
      
      do i = 5,9
        if (ack(i)) then
	  write(CRMESS,'(i1,6i6,6f5.2)') 
     &    i,(type(2,s,i),s=1,6),
     &    (float(type(2,s,i))/max(0.01,float(type(2,s,5))),s=1,6)   
        endif       
	call RecMes(CRNAME,'-',CRMESS)
      enddo
      
      do i = 5,9
        if (ack(i)) then
	  write(CRMESS,'(i1,6i6,6f5.2)') 
     &    i,(type(3,s,i),s=1,6),
     &    (float(type(3,s,i))/max(0.01,float(type(3,s,5))),s=1,6)   
        endif       
	call RecMes(CRNAME,'+',CRMESS)
      enddo
      
      call RecMes(CRNAME,'L','             ')

      elseif(stat_option.lt.3) then  ! Old style printout (default)
        
      if(ddtype(1).GT.0) then
        xevents=FLOAT(ddtype(1))/100.
      else
        xevents=0.01
      endif

      if(ddtype(3).GT.0) then
        xlinks=FLOAT(ddtype(3))/100.
      else
        xlinks=0.01
      endif

      if( stat_option.eq.2 )then
       call RecMes(CRNAME,'L',' Cut#  Description               Value     Passed     %       Yes/No')
      else
       call RecMes(CRNAME,'L',' Cut#  Description               Value     Passed     %')
      endif

      DO i = 1,10
        IF (ack(i)) THEN
          fraction=FLOAT(ddtype(i))/xevents
          if( stat_option.eq.2 )then
            write(CRMESS,'(I5,A2,A30,F6.1,I10,F8.1,I10)')i,'  ',caption(i),
     &                   xcut(i), dtype(i), fraction, dtype(i)
            dtype(i) = 0
          ELSE
            if(i.LT.3) then
              write(CRMESS,'(I5,A2,A30,F6.1,I10,F8.1)')i,'  ',caption(i),
     &                   xcut(i), ddtype(i), fraction
            else
              write(CRMESS,'(I5,A2,A30,F6.1,I10,2F8.1)')i,'  ',caption(i),
     &                   xcut(i), ddtype(i), fraction, FLOAT(ddtype(i))/xlinks
            endif
          ENDIF
          call RecMes(CRNAME,'L',CRMESS)
        ENDIF
      ENDDO
      call RecMes(CRNAME,'L','             ')

      endif


      END

