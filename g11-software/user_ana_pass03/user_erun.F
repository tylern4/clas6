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
      REAL gpar(3),egpar(3),ggpar(6),eggpar(6),chi2,calb(103)
      REAL pmin(6),pmax(6),step(6)
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
         do j=1,2
            statv(j)=hstati(hid,j,'',0)
         enddo
         fmin=statv(1)-1.*statv(2)
         fmax=statv(1)+1.*statv(2)
         call hxi(hid,fmin,imin)
         call hxi(hid,fmax,imax)
         iquest(11)=imin
         iquest(12)=imax
         call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
         calb(3)=gpar(2)
         calb(4)=gpar(3)

         hid=6140
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
         calb(5)=gpar(2)
         calb(6)=gpar(3)

         hid=6150
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
         calb(7)=gpar(2)
         calb(8)=gpar(3)

         hid=7140
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
         calb(9)=gpar(2)
         calb(10)=gpar(3)

         hid=7150
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
         calb(11)=gpar(2)
         calb(12)=gpar(3)

         hid=6600
         call hfithn(hid,'G','Q',0,gpar,' ',' ',' ',egpar,chi2)
         calb(13)=gpar(3)

         hid=6051
         call hfithn(hid,'G','Q',0,gpar,' ',' ',' ',egpar,chi2)
         calb(14)=gpar(2)
         calb(16)=gpar(3)

         hid=6052
         call hfithn(hid,'G','Q',0,gpar,' ',' ',' ',egpar,chi2)
         calb(15)=gpar(2)
         calb(17)=gpar(3)

         hid=10001
c         do j=1,2
c            statv(j)=hstati(hid,j,'',0)
c         enddo
c         fmin=statv(1)-1.5*statv(2)
c         fmax=statv(1)+1.5*statv(2)
c         call hxi(hid,fmin,imin)
c         call hxi(hid,fmax,imax)
c         iquest(11)=imin
c         iquest(12)=imax
	 ggpar(1)=10000.
         ggpar(2)=0.02   
         ggpar(3)=0.02
         ggpar(4)=0. 
         ggpar(5)=0.  
         ggpar(6)=0. 
         call hfithn(hid,'G+P2','Q',0,ggpar,' ',' ',' ',eggpar,chi2)
         calb(18)=ggpar(2)


         hid=10003
c         do j=1,2
c            statv(j)=hstati(hid,j,'',0)
c         enddo
c         fmin=statv(1)-1.5*statv(2)
c         fmax=statv(1)+1.5*statv(2)
c         call hxi(hid,fmin,imin)
c         call hxi(hid,fmax,imax)
c         iquest(11)=imin
c         iquest(12)=imax
         ggpar(1)=10000.
         ggpar(2)=0.94
         ggpar(3)=0.01
         ggpar(4)=0.
         ggpar(5)=0.
         ggpar(6)=0.
         call hfithn(hid,'G+P2','Q',0,ggpar,' ',' ',' ',eggpar,chi2)
         calb(19)=ggpar(2)

         call hcdir('//PAWC',' ')
         call hcdir('TBTPR',' ')
         do i=1,6
c            hid=i*1000+42
	     hid=i*1000+45
c            do j=1,2
c               statv(j)=hstati(hid,j,'',0)
c            enddo
c            fmin=statv(1)-1.5*statv(2)
c            fmax=statv(1)+1.5*statv(2)
c            call hxi(hid,fmin,imin)
c            call hxi(hid,fmax,imax)
c            iquest(11)=imin
c            iquest(12)=imax
c            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
c            calb( 6+i)=gpar(2)
c            calb(12+i)=gpar(3)
c            do k=1,6
c               hid=i*1000+k*100+42
c               do j=1,2
c                  statv(j)=hstati(hid,j,'',0)
c               enddo
c               fmin=statv(1)-1.5*statv(2)
c               fmax=statv(1)+1.5*statv(2)
c               call hxi(hid,fmin,imin)
c               call hxi(hid,fmax,imax)
c               iquest(11)=imin
c               iquest(12)=imax
c               call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
c               calb( 6+12*i+k)=gpar(2)
c               calb(12+12*i+k)=gpar(3)
c            enddo
c         enddo
            step(1)=1
            step(2)=0.0001
            step(3)=0.0001
            step(4)=1
            step(5)=0.0001
            step(6)=0.0001
            pmin(1)=0
            pmin(2)=-0.05
            pmin(3)=0.01
            pmin(4)=0.
            pmin(5)=-0.1
            pmin(6)=0.05
            pmax(1)=1000000
            pmax(2)=0.05
            pmax(3)=0.05
            pmax(4)=500000
            pmax(5)=0.1
            pmax(6)=0.5

            ggpar(1)=70000
            ggpar(2)=0.
            ggpar(3)=0.02
            ggpar(4)=10000
            ggpar(5)=0.
            ggpar(6)=0.1            
            call hfithn(hid,'G+G','QB',0,ggpar,step,pmin,pmax,eggpar,chi2)
            calb(19+i)=(ggpar(1)*ggpar(2)*ggpar(3)+ggpar(4)*ggpar(5)*ggpar(6))
     &                        /(ggpar(1)*ggpar(3)+ggpar(4)*ggpar(6))*10000
            calb(25+i)=ggpar(3)*10000
            do k=1,6
c               hid=i*1000+k*100+42
               hid=i*1000+k*100+45
               ggpar(1)=70000
               ggpar(2)=0.
               ggpar(3)=0.02
               ggpar(4)=10000
               ggpar(5)=0.
               ggpar(6)=0.1
               call hfithn(hid,'G+G','QB',0,ggpar,step,pmin,pmax,eggpar,chi2)
               calb(19+12*i+k)=(ggpar(1)*ggpar(2)*ggpar(3)+ggpar(4)*ggpar(5)*ggpar(6))
     &                        /(ggpar(1)*ggpar(3)+ggpar(4)*ggpar(6))*10000
               calb(25+12*i+k)=ggpar(3)*10000
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














