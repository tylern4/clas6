      SUBROUTINE USER_ERUN
c
c_begin_doc
c  RCS ID string
c  $Id: user_erun.F,v 1.10 2009/03/31 20:56:55 clasg9 Exp $
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
#include "wcs.inc"
#include "histtcl.inc"
#include "user_control.inc"
#include "csql.inc"

      INTEGER IQUEST
      COMMON/QUEST/IQUEST(100)
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
      PARAMETER (CREVIS= '$Revision: 1.10 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2009/03/31 20:56:55 $')
      PARAMETER (CAUTHO= '$Author: clasg9 $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='USER_ERUN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      REAL PI 
      INTEGER NWRITE, iret, icycle, hid, i, j, k
      INTEGER ind, nami, mamind, ncol, lenocc, hsum
      REAL gpar(3),egpar(3),ggpar(6),eggpar(6),gggpar(9),egggpar(9)
      REAL gpar7(7), egpar7(7)
      REAL pmin(6),pmax(6),step(6)

      INTEGER NLEN_CALB
      PARAMETER (NLEN_CALB=282)   !<< adjust this
      REAL chi2,calb(NLEN_CALB)

      REAL statv(2),fmin,fmax,hstati
      external hstati
      INTEGER imin,imax
      CHARACTER*80 htit
      INTEGER nchx, nchy, nwt, lcaddr
      REAL  xmi, xma, ymi, yma 
      REAL maxloc, FindHistMax
      REAL dtstpaddle(24,200)
      INTEGER sliyhid
c_end_var
c
c  executable code for routine USER_ERUN:
c  -------------------------------------
c
      PI = 2.0*ACOS(0.0)
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c  Perform TAGGER fill histograms
      IF (LTAGGER_H_DO) CALL TAG_FILL_ERUN(.true.,.true.,.true.,.true.)
      IF(LMON_HIST)THEN

c reset calb entries
         do j=1,NLEN_CALB
            calb(j)=0
         enddo
c
         call hcdir('//LUN2',' ')
         call hcdir('//PAWC',' ')
         call hcdir('SEB',' ')
c     RF offset
         hid=3102
         if(hsum(hid).gt.100) then
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            gpar7(1)=4000
            gpar7(2)=FindHistMax(hid)
            gpar7(3)=statv(2)
            fmin = gpar7(2)-1.0
            fmax = gpar7(2)+1.0
            gpar7(4)=0.0
            gpar7(5)=0.0
            gpar7(6)=0.0
            gpar7(7)=0.0
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax 
            call hfithn(hid,'G+P3','RQ',0,gpar7,' ',' ',' ',egpar7,chi2)
            calb(1)=gpar7(2)
            calb(2)=abs(gpar7(3))
         endif

c     dt prot TOF
         hid=6110
         if(hsum(hid).gt.100) then
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            maxloc = FindHistMax(hid)
            fmin=maxloc-0.4
            fmax=maxloc+0.4
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            gpar(1)=1000.0
            gpar(2)=maxloc
            gpar(3)=0.25
            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
            calb(5)=gpar(2)
            calb(6)=abs(gpar(3))
         endif

c     dt pi+ TOF
         hid=6140
         if(hsum(hid).gt.100) then
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            maxloc = FindHistMax(hid)
            fmin=maxloc-0.4
            fmax=maxloc+0.4
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            gpar(1)=1000.0
            gpar(2)=maxloc
            gpar(3)=0.25
            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
            calb(7)=gpar(2)
            calb(8)=abs(gpar(3))
         endif

c     dt pi- TOF
         hid=6150
         if(hsum(hid).gt.100) then
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            maxloc = FindHistMax(hid)
            fmin=maxloc-0.4
            fmax=maxloc+0.4
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            gpar(1)=1500.0
            gpar(2)=maxloc
            gpar(3)=0.2
            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
            calb(9)=gpar(2)
            calb(10)=abs(gpar(3))
         endif

c     dt_st prot
         hid=7110
         if(hsum(hid).gt.100) then
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            maxloc = FindHistMax(hid)
            fmin=maxloc-1.1
            fmax=maxloc+1.1
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            gpar(1)=1000.0
            gpar(2)=maxloc
            gpar(3)=0.4
            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
            calb(11)=gpar(2)
            calb(12)=abs(gpar(3))
         endif

c     dt_st pi+
         hid=7140
         if(hsum(hid).gt.100) then
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            maxloc = FindHistMax(hid)
            fmin=maxloc-1.1
            fmax=maxloc+1.1
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            gpar(1)=300.0
            gpar(2)=maxloc
            gpar(3)=0.5
            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
            calb(13)=gpar(2)
            calb(14)=abs(gpar(3))
         endif

c     dt_st pi-
         hid=7150
         if(hsum(hid).gt.100) then
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            maxloc = FindHistMax(hid)
            fmin=maxloc-1.1
            fmax=maxloc+1.1
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            gpar(1)=600.0
            gpar(2)=maxloc
            gpar(3)=0.5
            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
            calb(15)=gpar(2)
            calb(16)=abs(gpar(3))
         endif

C     dt_st per paddle proton
         do i=1,24
            hid=711100+i
            if(hsum(hid).gt.100) then
               do j=1,2
                  statv(j)=hstati(hid,j,'',0)
               enddo
               maxloc = FindHistMax(hid)
               fmin=maxloc-1.1
               fmax=maxloc+1.1
               call hxi(hid,fmin,imin)
               call hxi(hid,fmax,imax)
               iquest(11)=imin
               iquest(12)=imax
               gpar(1)=100.0
               gpar(2)=maxloc
               gpar(3)=0.4
               call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
               calb(16+i)=gpar(2)
               calb(40+i)=abs(gpar(3))
            endif
         enddo

C     dt_st per paddle pi+
         do i=1,24
            hid=714100+i
            if(hsum(hid).gt.100) then
               do j=1,2
                  statv(j)=hstati(hid,j,'',0)
               enddo
               maxloc = FindHistMax(hid)
               fmin=maxloc-1.1
               fmax=maxloc+1.1
               call hxi(hid,fmin,imin)
               call hxi(hid,fmax,imax)
               iquest(11)=imin
               iquest(12)=imax
               gpar(1)=100.0
               gpar(2)=maxloc
               gpar(3)=0.4
               call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
               calb(64+i)=gpar(2)
               calb(88+i)=abs(gpar(3))
            endif
         enddo

            
C     dt_st per paddle pi-
         do i=1,24
            hid=715100+i
            if(hsum(hid).gt.100) then
               do j=1,2
                  statv(j)=hstati(hid,j,'',0)
               enddo
               maxloc = FindHistMax(hid)
               fmin=maxloc-1.1
               fmax=maxloc+1.1
               call hxi(hid,fmin,imin)
               call hxi(hid,fmax,imax)
               iquest(11)=imin
               iquest(12)=imax
               gpar(1)=100.0
               gpar(2)=maxloc
               gpar(3)=0.4
               call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
               calb(112+i)=gpar(2)
               calb(136+i)=abs(gpar(3))
            endif
         enddo

         
         
c     dt_ec prot
         hid=7210
         if(hsum(hid).gt.100) then
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            maxloc = FindHistMax(hid)
            fmin=maxloc-1.1
            fmax=maxloc+1.1
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            gpar(1)=500.0
            gpar(2)=maxloc
            gpar(3)=0.5
            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
            calb(161)=gpar(2)
            calb(162)=abs(gpar(3))
         endif
         
c     dt_ec pi+
         hid=7240
         if(hsum(hid).gt.100) then
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            maxloc = FindHistMax(hid)
            fmin=maxloc-1.1
            fmax=maxloc+1.1
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            gpar(1)=300.0
            gpar(2)=maxloc
            gpar(3)=0.5
            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
            calb(163)=gpar(2)
            calb(164)=abs(gpar(3))
         endif

c     dt_ec pi-
         hid=7250
         if(hsum(hid).gt.100) then
            do j=1,2
               statv(j)=hstati(hid,j,'',0)
            enddo
            maxloc = FindHistMax(hid)
            fmin=maxloc-1.1
            fmax=maxloc+1.1
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            gpar(1)=300.0
            gpar(2)=maxloc
            gpar(3)=0.5
            call hfithn(hid,'G','RQ',0,gpar,' ',' ',' ',egpar,chi2)
            calb(165)=gpar(2)
            calb(166)=abs(gpar(3))
         endif


c     ECt-SCt for pions
         hid=6600
         if(hsum(hid).gt.100) then
            ggpar(1)=4000.
            ggpar(2)=FindHistMax(hid)   
            ggpar(3)=0.6
            ggpar(4)=0. 
            ggpar(5)=0.  
            ggpar(6)=0. 
            fmin=-4.
            fmax= 4.
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            call hfithn(hid,'G+P2','RQ',0,ggpar,' ',' ',' ',eggpar,chi2)
            calb(167)=ggpar(2)
            calb(168)=abs(ggpar(3))
         endif

c     Beta for neutrals
         hid=6620
         if(hsum(hid).gt.100) then
            ggpar(1)=600.
            ggpar(2)=1.0   
            ggpar(3)=0.01
            ggpar(4)=0. 
            ggpar(5)=0.  
            ggpar(6)=0. 
            fmin=0.65
            fmax=1.1
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            call hfithn(hid,'G+P2','RQ',0,ggpar,' ',' ',' ',eggpar,chi2)
            calb(169)=ggpar(2)
            calb(170)=abs(ggpar(3))
         endif

c     2-photon mass in EC
         hid=6640
         if(hsum(hid).gt.100) then
            ggpar(1)=600.
            ggpar(2)=0.14   
            ggpar(3)=0.01
            ggpar(4)=0. 
            ggpar(5)=0.  
            ggpar(6)=0. 
            fmin=0.04
            fmax=0.3
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            call HGIVE( hid, htit, nchx, xmi, xma, nchy, ymi, yma, nwt, lcaddr )
            call hfithn(hid,'G+P2','RQ',0,ggpar,' ',' ',' ',eggpar,chi2)
            calb(171)=ggpar(2)
            calb(172)=abs(ggpar(3)) 
            if ( xma .GT. xmi )  then 
               calb(173)=ggpar(1)*abs(ggpar(3)) * sqrt(2.*PI) * nchx / ( xma-xmi )
            endif
         endif

c     MVRT X
         hid=6051
         if(hsum(hid).gt.100) then
            call hfithn(hid,'G','Q',0,gpar,' ',' ',' ',egpar,chi2)
            calb(174)=gpar(2)
            calb(176)=abs(gpar(3))
         endif

c     MVRT Y
         hid=6052
         if(hsum(hid).gt.100) then
            call hfithn(hid,'G','Q',0,gpar,' ',' ',' ',egpar,chi2)
            calb(175)=gpar(2)
            calb(177)=abs(gpar(3))
         endif

c     TOF Mass pion
         hid=6100
         if(hsum(hid).gt.100) then
            ggpar(1)=5000.
            ggpar(2)=0.140
            ggpar(3)=0.1
            ggpar(4)=0. 
            ggpar(5)=0.  
            ggpar(6)=0. 
            fmin=0.0
            fmax=0.4
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            call HGIVE( hid, htit, nchx, xmi, xma, nchy, ymi, yma, nwt, lcaddr )
            call hfithn(hid,'G+P2','RQ',0,ggpar,' ',' ',' ',eggpar,chi2)
            calb(178)=ggpar(2)
            calb(179)=abs(ggpar(3))
            if ( xma .GT. xmi )  then 
               calb(182)=ggpar(1)*abs(ggpar(3)) * sqrt(2.*PI) * nchx / ( xma-xmi )
            endif
         endif

c     TOF Mass proton
         hid=6100
         if(hsum(hid).gt.100) then
            ggpar(1)=7000.
            ggpar(2)=0.938
            ggpar(3)=0.1
            ggpar(4)=0. 
            ggpar(5)=0.  
            ggpar(6)=0. 
            fmin=0.7
            fmax=1.5
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            call HGIVE( hid, htit, nchx, xmi, xma, nchy, ymi, yma, nwt, lcaddr )
            call hfithn(hid,'G+P2','RQ',0,ggpar,' ',' ',' ',eggpar,chi2)
            calb(180)=ggpar(2)
            calb(181)=abs(ggpar(3))
            if ( xma .GT. xmi )  then 
               calb(183)=ggpar(1)*abs(ggpar(3)) * sqrt(2.*PI) * nchx / ( xma-xmi )
            endif
         endif


c     MM ppi+
         hid=10002
         if(hsum(hid).gt.100) then
            ggpar(1)=60.
            ggpar(2)=0.140
            ggpar(3)=0.02
            ggpar(4)=0. 
            ggpar(5)=0.  
            ggpar(6)=0. 
            fmin=0.0
            fmax=0.35
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            call HGIVE( hid, htit, nchx, xmi, xma, nchy, ymi, yma, nwt, lcaddr )
            call hfithn(hid,'G+P2','RQ',0,ggpar,' ',' ',' ',eggpar,chi2)
            calb(184)=ggpar(2)
            calb(185)=abs(ggpar(3))
            if ( xma .GT. xmi )  then 
               calb(186)=ggpar(1)*abs(ggpar(3)) * sqrt(2.*PI) * nchx / ( xma-xmi )
            endif
         endif

c     MM ppi+pi-
         hid=10003
         if(hsum(hid).gt.100) then
            ggpar(1)=200.
            ggpar(2)=0.140
            ggpar(3)=0.02
            ggpar(4)=0.
            ggpar(5)=0.
            ggpar(6)=0.
            fmin=0.0
            fmax=0.4
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            call HGIVE( hid, htit, nchx, xmi, xma, nchy, ymi, yma, nwt, lcaddr )
            call hfithn(hid,'G+P2','RQ',0,ggpar,' ',' ',' ',eggpar,chi2)
            calb(187)=ggpar(2)
            calb(188)=abs(ggpar(3))
            if ( xma .GT. xmi )  then 
               calb(189)=ggpar(1)*abs(ggpar(3)) * sqrt(2.*PI) * nchx / ( xma-xmi )
            endif
         endif

c     MM K+
         hid=10011
         if(hsum(hid).gt.100) then
            gggpar(1)=50.
            gggpar(2)=1.1
            gggpar(3)=0.01
            gggpar(4)=20.
            gggpar(5)=1.2
            gggpar(6)=0.01
            gggpar(7)=0.
            gggpar(8)=0.
            gggpar(9)=0.
            fmin=1.
            fmax=1.3
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            call HGIVE( hid, htit, nchx, xmi, xma, nchy, ymi, yma, nwt, lcaddr )
            call hfithn(hid,'G+G+P2','RQ',0,gggpar,' ',' ',' ',egggpar,chi2)
            calb(190)=gggpar(2)
            calb(191)=abs(gggpar(3))
            calb(192)=gggpar(5)
            calb(193)=abs(gggpar(6))
            if ( xma .GT. xmi )  then          
               calb(194)=ggpar(1)*abs(ggpar(3)) * sqrt(2.*PI) * nchx / ( xma-xmi )
               calb(195)=ggpar(4)*abs(ggpar(6)) * sqrt(2.*PI) * nchx / ( xma-xmi )
            endif
         endif
         
c     MM pi+
         hid=10007
         if(hsum(hid).gt.100) then
            ggpar(1)=60.
            ggpar(2)=0.939   
            ggpar(3)=0.02
            ggpar(4)=0. 
            ggpar(5)=0.  
            ggpar(6)=0. 
            fmin=0.8
            fmax=1.05
            call hxi(hid,fmin,imin)
            call hxi(hid,fmax,imax)
            iquest(11)=imin
            iquest(12)=imax
            call HGIVE( hid, htit, nchx, xmi, xma, nchy, ymi, yma, nwt, lcaddr )
            call hfithn(hid,'G+P2','RQ',0, ggpar,' ',' ',' ',eggpar,chi2)
            calb(196)=ggpar(2)
            calb(197)=abs(ggpar(3))
            if ( xma .GT. xmi )  then 
               calb(198)=ggpar(1)*abs(ggpar(3)) * sqrt(2.*PI) * nchx / ( xma-xmi )
            endif
         endif





c Time from tagger
         call hcdir('//PAWC',' ')
         call hcdir('TAG',' ')
         hid=1007
         if(hsum(hid).gt.100) then
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
            calb(3)=gpar(2)
            calb(4)=abs(gpar(3))
         endif

c     DC residuals without cuts
         call hcdir('//PAWC',' ')
         call hcdir('TBTPR',' ')
         do i=1,6
	    hid=i*1000+45
            if(hsum(hid).gt.100) then
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
               
               ggpar(1)=50000
               ggpar(2)=0.
               ggpar(3)=0.02
               ggpar(4)=10000
               ggpar(5)=0.
               ggpar(6)=0.1            
               call hfithn(hid,'G+G','QB',0,ggpar,step,pmin,pmax,eggpar,chi2)
c     calb(48+i)=(abs(ggpar(1)*ggpar(3))*ggpar(2)+abs(ggpar(4)*ggpar(6))*ggpar(5))
c     &                        /(abs(ggpar(1)*ggpar(3))+abs(ggpar(4)*ggpar(6)))*10000
c     calb(54+i)=(abs(ggpar(1)*ggpar(3))*abs(ggpar(3))+abs(ggpar(4)*ggpar(6))*abs(ggpar(6)))
c     &                        /(abs(ggpar(1)*ggpar(3))+abs(ggpar(4)*ggpar(6)))*10000
            endif
            do k=1,6
               hid=i*1000+k*100+45
               if(hsum(hid).gt.100) then
                  ggpar(1)=3000
                  ggpar(2)=0.
                  ggpar(3)=0.02
                  ggpar(4)=1000
                  ggpar(5)=0.
                  ggpar(6)=0.1
                  call hfithn(hid,'G+G','QB',0,ggpar,step,pmin,pmax,eggpar,chi2)
c     calb(40+12*i+k)=(abs(ggpar(1)*ggpar(3))*ggpar(2)+abs(ggpar(4)*ggpar(6))*ggpar(5))
c     &                        /(abs(ggpar(1)*ggpar(3))+abs(ggpar(4)*ggpar(6)))*10000
c     calb(46+12*i+k)=(abs(ggpar(1)*ggpar(3))*abs(ggpar(3))+abs(ggpar(4)*ggpar(6))*abs(ggpar(6)))
c     &                        /(abs(ggpar(1)*ggpar(3))+abs(ggpar(4)*ggpar(6)))*10000
               endif
            enddo
         enddo

c     DC residuals with cuts
         do i=1,6
	    hid=i*1000+46
            if(hsum(hid).gt.100) then
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
               
               ggpar(1)=50000
               ggpar(2)=0.
               ggpar(3)=0.02
               ggpar(4)=10000
               ggpar(5)=0.
               ggpar(6)=0.1            
               call hfithn(hid,'G+G','QB',0,ggpar,step,pmin,pmax,eggpar,chi2)
               calb(198+i)=(abs(ggpar(1)*ggpar(3))*ggpar(2)+abs(ggpar(4)*ggpar(6))*ggpar(5))
     &              /(abs(ggpar(1)*ggpar(3))+abs(ggpar(4)*ggpar(6)))*10000
               calb(204+i)=(abs(ggpar(1)*ggpar(3))*abs(ggpar(3))+abs(ggpar(4)*ggpar(6))*abs(ggpar(6)))
     &              /(abs(ggpar(1)*ggpar(3))+abs(ggpar(4)*ggpar(6)))*10000
            endif
            do k=1,6
               hid=i*1000+k*100+46
               if(hsum(hid).gt.100) then
                  ggpar(1)=3000
                  ggpar(2)=0.
                  ggpar(3)=0.02
                  ggpar(4)=1000
                  ggpar(5)=0.
                  ggpar(6)=0.1
                  call hfithn(hid,'G+G','QB',0,ggpar,step,pmin,pmax,eggpar,chi2)
                  calb(198+12*i+k)=(abs(ggpar(1)*ggpar(3))*ggpar(2)+abs(ggpar(4)*ggpar(6))*ggpar(5))
     &                 /(abs(ggpar(1)*ggpar(3))+abs(ggpar(4)*ggpar(6)))*10000
                  calb(204+12*i+k)=(abs(ggpar(1)*ggpar(3))*abs(ggpar(3))+abs(ggpar(4)*ggpar(6))*abs(ggpar(6)))
     &                 /(abs(ggpar(1)*ggpar(3))+abs(ggpar(4)*ggpar(6)))*10000
               endif
            enddo
         enddo
         print *,calb


c
c-- filling histogram wiht normalization information
         if(Lnorm) then
            call hcdir('//PAWC',' ')
            call hbook1(71,'Normalized Photons("x#10^6!) vs. T counter',61,0.,61., 0.0)
            call hpak(71,nphot1)
            call hpake(71,err_nphot1)
            call hbook1(72,'Normalized Photons("x#10^6!) vs. Eid ',767,0.,767., 0.0)
            call hpak(72,nphot2)
            call hpake(72,err_nphot2)
            call hbook1(73,'Normalized Photons("x#10^6!) vs. Energy ',225,0.5,5.0, 0.0)
            call hpak(73,nphot3)
            call hpake(73,err_nphot3)
            call hbook1(74,'Normalized Photons("x#10^6!) vs. Energy ',90,0.5,5.0, 0.0)
            call hpak(74,nphot4)
            call hpake(74,err_nphot4)



            call hcdir('//LUN1',' ')
            CALL HROUT(71,ICYCLE,'T')
            CALL HROUT(72,ICYCLE,'T')
            CALL HROUT(73,ICYCLE,'T')
            CALL HROUT(74,ICYCLE,'T')
            if ( LSEB_NTN_DO ) then 
               call hcdir('//PAWC',' ')
               call hcdir('//LUN3',' ')
               CALL HROUT(71,ICYCLE,'T')
               CALL HROUT(72,ICYCLE,'T')
               CALL HROUT(73,ICYCLE,'T')               
               CALL HROUT(74,ICYCLE,'T')
            endif
        endif
         

         call hcdir('//PAWC',' ')
         call hcdir('//LUN2',' ')
         CALL HRPUT(0,' ','T')
         CALL HREND('LUN2')
         close(LHM)
         CALL RESLUN(CRNAME,-LHM,IRET)
      ENDIF

      IF(LSEB_NTN_DO) then
         call hcdir('//PAWC',' ')
         call hcdir('//LUN3',' ')
         CALL HROUT(21,ICYCLE,'T')
         CALL HREND('LUN3')
         close(LHST21)
         CALL RESLUN(CRNAME,-LHST21,IRET)
      ENDIF
c
      if(LMySQL)Then
         write (*,*) ">>>>about to do csql 0"	
         nami=mamind(jw,'CSQL')
	 write (*,*) "nami ", nami
         ind=0
         if(nami.gt.0)ind=JW(nami)
	 write (*,*) "ind ", ind, "JW(nami) ", JW(nami) 
         
         if(ind.gt.0)then
         write (*,*) ">>>>about to do csql 1"	
            call set_group('CSQL',JW(ind+1))
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

C
C     Returns the X-value where the histogram maximum is located
C
      real function FindHistMax( hid )

      implicit none

      integer hid
      real ycont( 10000 ), tmp
      CHARACTER*128 htit
      integer nchx, nchy, nwt, lcaddr
      real  xmi, xma, ymi, yma 
      integer maxloc
      integer MAXRZE

      call HGIVE( hid, htit, nchx, xmi, xma, nchy, ymi, yma, nwt, lcaddr )
      call HUNPAK( hid, ycont, ' ', 0 )

      maxloc = MAXRZE( ycont, nchx )

      call HIX( hid, maxloc, tmp )
      FindHistMax = tmp

      return
      end











