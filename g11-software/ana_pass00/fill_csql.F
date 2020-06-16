      SUBROUTINE FILL_CSQL()
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_csql.F,v 1.3 2002/03/27 13:07:27 stepanya Exp $
c     
c     Documentation for subroutine FILL_CSQL
c     
c     Purpose: Fills CSQL bos bank for C_SQL package (MySQL DB).
c     --------
c     
c     Input Parameters:  NON
c     ----------------
c     
c     Output Parameters:  Error flag
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:    S. Stepanyan     Created:  Fri Mar  7 09:43:25 EST 1997
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
c     BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "wcs.inc"
#include "user_control.inc"
#include "csql.inc"
#include "clasmdl.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_csql.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/03/27 13:07:27 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1     '$Id: fill_csql.F,v 1.3 2002/03/27 13:07:27 stepanya Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_CSQL')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE
      integer ind,i
      INTEGER mamind, mbank, ibmpr
      external mamind,mbank
c     _end_var
c     
c     executable code for routine FILL_PART_NT:
c---- 6----------------------------------------------------------------72
c     
c     
      ibmpr=MAMIND(JW,'BMPR')
      ind = JW(ibmpr)
      IF(ind.NE.0) THEN
         Q_t=rjw(ind + 1)*1.E+06
         Q_t_l=rjw(ind+ 2)*1.E+06
         T_l_T=rjw(ind + 3)
         I_FC=rjw(ind+4)*1.E+09
      ENDIF
c     
      ind=0
      ind=mbank(iw,'CSQL',0,21,1)
      CALL MKFMT(IW,'CSQL','2I,4F,15I')
c     
      totcpu=0.0
      do i=1,14
         totcpu=totcpu+cpusum(i)
      enddo
      if (ind .ne. 0) then
         iw(ind+1)=nevent
         iw(ind+2)=tevent_seen
         rw(ind+3)=totcpu
         rw(ind+4)=Q_t
         rw(ind+5)=q_t_l
         rw(ind+6)=t_l_t
         rw(ind+7)=i_fc
         do i=1,6
            iw(ind+i+7)=nelect(i)
         enddo
         iw(ind+14)=nhbttrk
         iw(ind+15)=ntbttrk
         iw(ind+16)=nprot
         iw(ind+17)=npip
         iw(ind+18)=ndeut
         iw(ind+19)=nphoton
         iw(ind+20)=nelectp
         iw(ind+21)=nelectn
      endif
      RETURN
      END
c------------------------------------------------------------------------------




