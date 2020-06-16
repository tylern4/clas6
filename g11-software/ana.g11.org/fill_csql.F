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
      integer ind,i,istat,ln1,ln2,ln3,ln4,ltrip
      integer d1,d2
      INTEGER mamind, mbank, ibmpr
      external mamind,mbank
      integer lenocc
      external lenocc
      data ng_norm/0/
c     _end_var
c     
c     executable code for routine FILL_PART_NT:
c---- 6----------------------------------------------------------------72
c     
c     
c++ addition done on 9/29/04 to get number of normalized photons from glux output
      if(lnorm) then
         call reslun('norm_file1', ln1, istat)
         if(istat.eq.1) then 
            ng_norm=0
            i=0
            open(UNIT=ln1,FILE=normfile1(1:lenocc(normfile1)),STATUS='OLD',ERR=101)
            do while(.true.)
               i=i+1
               read(ln1,*,END=102,ERR=102) nphot1(i),err_nphot1(i)
               ng_norm=ng_norm+int(nphot1(i)/1000)
               nphot1(i)    =    nphot1(i)/1000000
               err_nphot1(i)=err_nphot1(i)/1000000
            enddo
 101        continue
 102        continue
            print *,'Ngamma from glux = ',ng_norm
            close(ln1)
         endif
         call reslun('norm_file2', ln2, istat)
         if(istat.eq.1) then 
            i=0
            open(UNIT=ln2,FILE=normfile2(1:lenocc(normfile2)),STATUS='OLD',ERR=201)
            do while(.true.)
               i=i+1
               read(ln2,*,END=202,ERR=202) nphot2(i),err_nphot2(i)
               nphot2(i)    =    nphot2(i)/1000000
               err_nphot2(i)=err_nphot2(i)/1000000
            enddo
 201        continue
 202        continue
            close(ln2)
         endif
         call reslun('norm_file3', ln3, istat)
         if(istat.eq.1) then 
            i=0
            open(UNIT=ln3,FILE=normfile3(1:lenocc(normfile3)),STATUS='OLD',ERR=301)
            do while(.true.)
               i=i+1
               read(ln3,*,END=302,ERR=302) nphot3(i),err_nphot3(i)
               nphot3(i)    =    nphot3(i)/1000000
               err_nphot3(i)=err_nphot3(i)/1000000
            enddo
 301        continue
 302        continue
            close(ln3)
         endif
         call reslun('norm_file4', ln4, istat)
         if(istat.eq.1) then 
            i=0
            open(UNIT=ln4,FILE=normfile4(1:lenocc(normfile4)),STATUS='OLD',ERR=401)
            do while(.true.)
               i=i+1
               read(ln4,*,END=402,ERR=402) nphot4(i),err_nphot4(i)
               nphot4(i)    =    nphot4(i)/1000000
               err_nphot4(i)=err_nphot4(i)/1000000
            enddo
 401        continue
 402        continue
            close(ln4)
         endif
         call reslun('trip_file', ltrip, istat)
         if(istat.eq.1) then 
            i=0
            ntrip=0
            open(UNIT=ltrip,FILE=tripfile(1:lenocc(tripfile)),STATUS='OLD',ERR=501)
            do while(.true.)
               i=i+1
               read(ltrip,*,END=502,ERR=502) d1,d2,trip_flag(i),trip_min(i),trip_max(i)
            enddo
 501        continue
 502        continue
            ntrip=i-1
            print *,ntrip
            close(ltrip)
         endif
      endif

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
      ind=mbank(iw,'CSQL',0,23,1)
      CALL MKFMT(IW,'CSQL','2I,4F,17I')
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
            iw(ind+i+7)=npipsect(i)
         enddo
         iw(ind+14)=nhbttrk
         iw(ind+15)=ntbttrk
         iw(ind+16)=nprot
         iw(ind+17)=npip
         iw(ind+18)=ndeut
         iw(ind+19)=nphoton
         iw(ind+20)=npipp
         iw(ind+21)=npipn
         iw(ind+22)=ngamma
         iw(ind+23)=ng_norm
c         print *,'aaaaaaaa', ngamma
      endif
      RETURN
      END
c------------------------------------------------------------------------------




