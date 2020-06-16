      SUBROUTINE USER_ERUN
c
c_begin_doc
c  RCS ID string
c  $Id: user_erun.F,v 1.8 2002/03/26 14:05:54 claseg1 Exp $
c
c  Documentation for subroutine USER_ERUN
c
c  Purpose: USER supplied routine called AT the END of EVERY RUN
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Wed Mar 15 14:01:16 EST 1995
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
#include "histtcl.inc"
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
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: user_erun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.8 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2002/03/26 14:05:54 $')
      PARAMETER (CAUTHO= '$Author: claseg1 $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='USER_ERUN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, iret, icycle, hid, i, j, k
      INTEGER ind, nami, mamind, ncol, lenocc
      REAL gpar(3),egpar(3),chi2,calb(90)
      INTEGER IQUEST
      COMMON/QUEST/IQUEST(100)
      REAL statv(2),fmin,fmax,hstati
      external hstati
      INTEGER imin,imax
c_end_var
c
c  executable code for routine USER_ERUN:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c  Perform TAGGER fill histograms
      IF (LTAGGER_H_DO) CALL TAG_FILL_ERUN(.true.,.true.,.true.,.true.)
      IF(LMON_HIST)THEN
c
         call hcdir('//LUN2',' ')
         call hcdir('//PAWC',' ')
         call hcdir('SEB',' ')
         hid=3102
         do j=1,2
            statv(j)=hstati(hid,j,'',0)
         enddo
         fmin=statv(1)-1.5*statv(2)
         fmax=statv(1)+1.5*statv(2)
         call hxi(hid,fmin,imin)
         call hxi(hid,fmax,imax)
         iquest(11)=imin
         iquest(12)=imax
         call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
         calb(1)=gpar(2)
         calb(2)=gpar(3)
         hid=6110
         call hfithn(hid,'G','Q',0,gpar,' ',' ',' ',egpar,chi2)
         calb(3)=gpar(3)
         hid=6120
         call hfithn(hid,'G','Q',0,gpar,' ',' ',' ',egpar,chi2)
         calb(4)=gpar(3)
         hid=6130
         call hfithn(hid,'G','Q',0,gpar,' ',' ',' ',egpar,chi2)
         calb(5)=gpar(3)
         hid=6600
         call hfithn(hid,'G','Q',0,gpar,' ',' ',' ',egpar,chi2)
         calb(6)=gpar(3)
         call hcdir('//PAWC',' ')
         call hcdir('TBTPR',' ')
         do i=1,6
            hid=i*1000+42
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            fmin=statv(1)-1.5*statv(2)
            fmax=statv(1)+1.5*statv(2)
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
            calb( 6+i)=gpar(2)
            calb(12+i)=gpar(3)
            do k=1,6
               hid=i*1000+k*100+42
               do j=1,2
                  statv(j)=hstati(hid,j,'',0)
               enddo
               fmin=statv(1)-1.5*statv(2)
               fmax=statv(1)+1.5*statv(2)
               call hxi(hid,fmin,imin)
               call hxi(hid,fmax,imax)
               iquest(11)=imin
               iquest(12)=imax
               call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
               calb( 6+12*i+k)=gpar(2)
               calb(12+12*i+k)=gpar(3)
            enddo
         enddo
         print *,calb
c
         call hcdir('//PAWC',' ')
         call hcdir('//LUN2',' ')
         CALL HRPUT(0,' ','T')
         CALL HREND('LUN2')
         close(LHM)
         CALL RESLUN(CRNAME,-LHM,IRET)
      ENDIF
c
      if(LMySQL)Then
         nami=mamind(iw,'CSQL')
         ind=0
         if(nami.gt.0)ind=IW(nami)
         if(ind.gt.0)then
            call set_group('CSQL',IW(ind+1))
            call set_group('CALB',calb(1))
            call write_def_table('SYSTCSQLCALB')
            call drop_mon_table
         endif
      endif
c
      if(let.GT.0) then
         close(let)
         CALL RESLUN(CRNAME,-LET,IRET)
      endif
      RETURN
      END
c
c------------------------------------------------------------------------------














