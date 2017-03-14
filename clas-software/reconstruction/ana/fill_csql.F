      SUBROUTINE FILL_CSQL()
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_csql.F,v 1.5 2009/03/31 20:54:57 clasg9 Exp $
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
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2009/03/31 20:54:57 $')
      PARAMETER (CAUTHO = '$Author: clasg9 $')
      DATA CRCSID/   
     1     '$Id: fill_csql.F,v 1.5 2009/03/31 20:54:57 clasg9 Exp $'   
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
      INTEGER mamind, mbank, mlink, ibmpr, icsql
      external mamind,mbank, mlink
      integer lenocc
      external lenocc
      data nwrite,ibmpr,icsql/0,0,0/
c     _end_var
c     
c     executable code for routine FILL_PART_NT:
c---- 6----------------------------------------------------------------72
c     
c     
      if(ibmpr.eq.0) ibmpr=MAMIND(JW,'BMPR')
      ind = JW(ibmpr)
      IF(ind.NE.0) THEN
         Q_t=rjw(ind + 1)*1.E+06
         Q_t_l=rjw(ind+ 2)*1.E+06
         T_l_T=rjw(ind + 3)
         I_FC=rjw(ind+4)*1.E+09
      ENDIF
c     
      if(nwrite.eq.0) then
         nwrite=nwrite+1
         ng_norm=0
         ind=mbank(jw,'CSQL',0,50,1)
         CALL MKFMT(jW,'CSQL','2I,3F,45I')
      else
         if(icsql.eq.0) icsql=mamind(jw,'CSQL')
         ind=jw(icsql)
      endif
c     
      totcpu=0.0
      do i=1,14
         totcpu=totcpu+cpusum(i)
      enddo
      if (ind .ne. 0) then
         jw(ind+1)=nevent
         jw(ind+2)=tevent_seen
         jw(ind+3)=totcpu
         rjw(ind+4)=t_l_t
         rjw(ind+5)=i_fc

         jw(ind+6)=npipsect(1)
         jw(ind+7)=npipsect(2)
         jw(ind+8)=npipsect(3)
         jw(ind+9)=npipsect(4)
         jw(ind+10)=npipsect(5)
         jw(ind+11)=npipsect(6)

         jw(ind+12)=nprotsect(1)
         jw(ind+13)=nprotsect(2)
         jw(ind+14)=nprotsect(3)
         jw(ind+15)=nprotsect(4)
         jw(ind+16)=nprotsect(5)
         jw(ind+17)=nprotsect(6)

         jw(ind+18)=npimsect(1)
         jw(ind+19)=npimsect(2)
         jw(ind+20)=npimsect(3)
         jw(ind+21)=npimsect(4)
         jw(ind+22)=npimsect(5)
         jw(ind+23)=npimsect(6)

         jw(ind+24)=nhbsect(1)
         jw(ind+25)=nhbsect(2)
         jw(ind+26)=nhbsect(3)
         jw(ind+27)=nhbsect(4)
         jw(ind+28)=nhbsect(5)
         jw(ind+29)=nhbsect(6)

         jw(ind+30)=ntbsect(1)
         jw(ind+31)=ntbsect(2)
         jw(ind+32)=ntbsect(3)
         jw(ind+33)=ntbsect(4)
         jw(ind+34)=ntbsect(5)
         jw(ind+35)=ntbsect(6)

         jw(ind+36)=nphotsect(1)
         jw(ind+37)=nphotsect(2)
         jw(ind+38)=nphotsect(3)
         jw(ind+39)=nphotsect(4)
         jw(ind+40)=nphotsect(5)
         jw(ind+41)=nphotsect(6)

         jw(ind+42)=nhbttrk
         jw(ind+43)=ntbttrk
         jw(ind+44)=nprot
         jw(ind+45)=npip
         jw(ind+46)=npim
         jw(ind+47)=nphoton
         jw(ind+48)=nkaonp
         jw(ind+49)=nkaonm
         jw(ind+50)=ngamma
      endif
      RETURN
      END
c------------------------------------------------------------------------------
