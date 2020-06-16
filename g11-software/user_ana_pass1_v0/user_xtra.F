      SUBROUTINE USER_XTRA(err)
c
c_begin_doc
c  RCS ID string
c  $Id: user_xtra.F,v 1.5 2003/09/05 15:33:56 ungaro Exp $
c
c  Documentation for subroutine USER_XTRA
c
c  Purpose:
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
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
c  Author:   Arne Freyberger      Created:  Wed Mar 15 14:05:36 EST 1995
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
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      integer err(10)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: user_xtra.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.5 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2003/09/05 15:33:56 $')
      PARAMETER ( CAUTHO = '$Author: ungaro $')
      DATA CRCSID/
     1'$Id: user_xtra.F,v 1.5 2003/09/05 15:33:56 ungaro Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='USER_XTRA')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE,ntbtrack,ix,npos,nneg,nkp,npip,npim,nkm,nk0
      INTEGER ipip(30),ipim(30),ip,im
      real p1x,p1y,p1z,p2x,p2y,p2z,px,py,pz,e,m,mpi
      INTEGER nami, ind, mamind
      EXTERNAL mamind
      data mpi/0.1395/
c_end_var
c
c  executable code for routine USER_XTRA:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

cccc writing out all the events (pass0)
c      nami = mamind(IW,'HEAD')
c      ind = IW(nami)
c      if(ind.gt.0)then
c         if(iw(ind+5).lt.10) err(1)=0
c      endif

ccc for PASS1
c   check number of positive and negative tracks
      nami = mamind(IW,'TBTR')
      ntbtrack=0
      npos=0
      nneg=0
      if(nami.gt.0)then
         ind = IW(nami)
         if (ind .ne. 0) then
            ntbtrack=iw(ind-4)
            do ix=1,iw(ind-4)
               if(iw(ind+7+iw(ind-5)*(ix-1)).gt.0)npos=npos+1
               if(iw(ind+7+iw(ind-5)*(ix-1)).lt.0)nneg=nneg+1
            enddo
         endif
      endif

c     look for a Kaon candidates
      nkp=0
      nkm=0
      nk0=0

      npip=0
      npim=0
      nami = mamind(IW,'EVNT')
      if(nami.gt.0)then
         ind = IW(nami)
         if (ind .ne. 0) then
            do ix=1,iw(ind-4)
               if(iw(ind+18+iw(ind-5)*(ix-1)).gt.0) then
                  if(iw(ind+1+iw(ind-5)*(ix-1)).eq.321) nkp=nkp+1
                  if(iw(ind+1+iw(ind-5)*(ix-1)).eq.-321)nkm=nkm+1
                  if(iw(ind+1+iw(ind-5)*(ix-1)).eq.211) then 
                     npip=npip+1
                     ipip(npip)=ix
                  endif
                  if(iw(ind+1+iw(ind-5)*(ix-1)).eq.-211) then
                     npim=npim+1
                     ipim(npim)=ix
                  endif
               endif
            enddo
            do ip=1,npip
               do im=1,npim
                  p1x=rw(ind+2+iw(ind-5)*(ipip(ip)-1))*rw(ind+6+iw(ind-5)*(ipip(ip)-1))
                  p1y=rw(ind+2+iw(ind-5)*(ipip(ip)-1))*rw(ind+7+iw(ind-5)*(ipip(ip)-1))
                  p1z=rw(ind+2+iw(ind-5)*(ipip(ip)-1))*rw(ind+8+iw(ind-5)*(ipip(ip)-1))
                  p2x=rw(ind+2+iw(ind-5)*(ipim(im)-1))*rw(ind+6+iw(ind-5)*(ipim(im)-1))
                  p2y=rw(ind+2+iw(ind-5)*(ipim(im)-1))*rw(ind+7+iw(ind-5)*(ipim(im)-1))
                  p2z=rw(ind+2+iw(ind-5)*(ipim(im)-1))*rw(ind+8+iw(ind-5)*(ipim(im)-1))
                  px=p1x+p2x
                  py=p1y+p2y
                  pz=p1z+p2z
                  e=sqrt(p1x**2+p1y**2+p1z**2+mpi**2)+sqrt(p2x**2+p2y**2+p2z**2+mpi**2)
                  m=sqrt(e**2-px**2-py**2-pz**2)
                  if(abs(m-0.5).lt.0.1) nk0=nk0+1
               enddo
            enddo
         endif
      endif
      npip=0
      npim=0
      nami = mamind(IW,'PART')
      if(nami.gt.0)then
         ind = IW(nami)
         if (ind .ne. 0) then
            do ix=1,iw(ind-4)
               if(iw(ind+1+iw(ind-5)*(ix-1)).eq.11)nkp=nkp+1
               if(iw(ind+1+iw(ind-5)*(ix-1)).eq.12)nkm=nkm+1
               if(iw(ind+1+iw(ind-5)*(ix-1)).eq.8) then 
                  npip=npip+1
                  ipip(npip)=ix
               endif
               if(iw(ind+1+iw(ind-5)*(ix-1)).eq.9)then
                  npim=npim+1
                  ipim(npim)=ix
               endif
            enddo
            do ip=1,npip
               do im=1,npim
                  p1x=rw(ind+6+iw(ind-5)*(ipip(ip)-1))
                  p1y=rw(ind+7+iw(ind-5)*(ipip(ip)-1))
                  p1z=rw(ind+8+iw(ind-5)*(ipip(ip)-1))
                  p2x=rw(ind+6+iw(ind-5)*(ipim(im)-1))
                  p2y=rw(ind+7+iw(ind-5)*(ipim(im)-1))
                  p2z=rw(ind+8+iw(ind-5)*(ipim(im)-1))
                  px=p1x+p2x
                  py=p1y+p2y
                  pz=p1z+p2z
                  e=sqrt(p1x**2+p1y**2+p1z**2+mpi**2)+sqrt(p2x**2+p2y**2+p2z**2+mpi**2)
                  m=sqrt(e**2-px**2-py**2-pz**2)
                  if(abs(m-0.5).lt.0.1) nk0=nk0+1
               enddo
            enddo
         endif
      endif
      npip=0
      npim=0
      nami = mamind(IW,'GPID')
      if(nami.gt.0)then
         ind = IW(nami)
         if (ind .ne. 0) then
            do ix=1,iw(ind-4)
               if(iw(ind+1+iw(ind-5)*(ix-1)).eq.11)nkp=nkp+1
               if(iw(ind+1+iw(ind-5)*(ix-1)).eq.12)nkm=nkm+1
               if(iw(ind+1+iw(ind-5)*(ix-1)).eq.8)then 
                  npip=npip+1
                  ipip(npip)=ix
               endif
               if(iw(ind+1+iw(ind-5)*(ix-1)).eq.9)then
                  npim=npim+1
                  ipim(npim)=ix
               endif
            enddo
            do ip=1,npip
               do im=1,npim
                  p1x=rw(ind+6+iw(ind-5)*(ipip(ip)-1))
                  p1y=rw(ind+7+iw(ind-5)*(ipip(ip)-1))
                  p1z=rw(ind+8+iw(ind-5)*(ipip(ip)-1))
                  p2x=rw(ind+6+iw(ind-5)*(ipim(im)-1))
                  p2y=rw(ind+7+iw(ind-5)*(ipim(im)-1))
                  p2z=rw(ind+8+iw(ind-5)*(ipim(im)-1))
                  px=p1x+p2x
                  py=p1y+p2y
                  pz=p1z+p2z
                  e=sqrt(p1x**2+p1y**2+p1z**2+mpi**2)+sqrt(p2x**2+p2y**2+p2z**2+mpi**2)
                  m=sqrt(e**2-px**2-py**2-pz**2)
                  if(abs(m-0.5).lt.0.1) nk0=nk0+1
               enddo
            enddo
         endif
      endif

      if(ntbtrack.gt.0)           err(1)=0
      if(npos.ge.2.and.nneg.ge.1) err(2)=0
      if((nkp.gt.0.and.(npos.ge.2.OR.nneg.ge.1)).OR.
     &   (nkm.gt.0.and.npos.gt.0))  err(3)=0
      if(nkp.gt.0.OR.nkm.gt.0.OR.nk0.gt.0)  err(4)=0


c For MC events
      nami = mamind(IW,'HEAD')
      ind = IW(nami)
      if(ind.gt.0)then
         if(IW(ind+5).lt.0) err(1)=0
      endif      

      
c
      RETURN
      END
c
c------------------------------------------------------------------------------




