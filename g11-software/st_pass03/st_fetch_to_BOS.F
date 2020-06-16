      SUBROUTINE ST_fetch_to_BOS(OK, TRK_LEVEL)
c
c_begin_doc
c  RCS ID string
c  $Id: st_fetch_to_BOS.F,v 1.11 2004/06/22 00:13:44 devita Exp $
c
c  Documentation for subroutine ST_fetch_to_BOS
c
c  Purpose: 
c  --------
c
c  Input Parameters:  TRK_LEVEL
c  ----------------
c
c  Output Parameters:  OK
c  -----------------
c
c  Notes: this sucks
c  ------
c
c  Author: 
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
#include "st_general.PAR"
#include "stconvert.CMN"
#include "stfit.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      integer trk_level
c      
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: st_fetch_to_BOS.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.11 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2004/06/22 00:13:44 $')
      PARAMETER (CAUTHO= '$Author: devita $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ST_fetch_to_BOS')
      PARAMETER (CRAUTH='Francois Roudot')
      INTEGER NWRITE
c
c  Local st defined variables:
c  ----------------------------
      logical ok
      integer idx,i,ncount, bankno    
      integer MBANK
      real rf_corr_time
     
c_end_var
c
c  executable code for routine ST_fetch_to_BOS:
c  ------------------------------------
c
c   
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='ST_FETCH_TO_BOS CALLED'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
      idx = MBANK(iw,StFit_BOS_name,TRK_LEVEL,StFit_BOS_size,stnfit)
      OK= idx.GT.0
      If(.NOT.OK) Then
         CRMESS= ' '
c     WRITE(CRMESS,'(a)') 'unable to open "'//StFit_BOS_name//'" BOS bank'
c     call RECMES(CRNAME,'F',CRMESS)
         RETURN
      endif
      do i=1,StNfit
         idx=idx+1
         if(StType.eq.0) then
            IW(idx)=StFit_sector(i)
         else
            IW(idx)=StFit_sector(i)*100+StFit_paddle(i)*10+StFit_where(i)
         endif
         idx=idx+1
         IW(idx)=TrkNum(i)
         idx=idx+1
         RW(idx)=StFit_pos_indep_time(i) 
         idx=idx+1
         RW(idx)=StFit_length(i)
         idx=idx+1
         RW(idx)=StFit_Position(i)
         idx=idx+1
         IW(idx)=StrStatus(i)
c         idx=idx+1
c         IW(idx)=StFit_Energy(i)
      enddo
      
      RETURN
      END
c
c------------------------------------------------------------------------------








































