      SUBROUTINE FILL_GCPB_NT(ierr)
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_gcpb_nt.F,v 1.3 2006/07/18 03:57:30 bonus Exp $
c     
c     Documentation for subroutine FILL_ICHB_NT
c     
c     Purpose:
c     --------
c     
c     Input Parameters: ierr - integer - returnes 1 if there was(were) 
c     hit(s) in IC
c     
c     Output Parameters:  Fill's areas in ICHB_NT common block.
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:   S. Stepanyan      Created:  Fri Dec 13 00:42:15 EST 1996
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     22.11.2005. Changed by V. Tvaskis for adding new variables from BoNuS (VS2) 
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
#include "gcpb_nt.inc"
#include "bcs.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
      INTEGER ierr
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_gcpb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/07/18 03:57:30 $')
      PARAMETER (CAUTHO = '$Author: bonus $')
      DATA CRCSID/   
     1     '$Id: fill_gcpb_nt.F,v 1.3 2006/07/18 03:57:30 bonus Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_GCPB_NT')
      PARAMETER (CRAUTH='whoistheauthor')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE
      INTEGER  IGCPB, ind, indx, ix, nd, ncol, i
      DATA IGCPB/0/
      INTEGER MAMIND
      EXTERNAL MAMIND
      INTEGER  loop_hit
      INTEGER  nhit_sav
      INTEGER  nhits,kkk
      INTEGER  iii,kk
      REAL sss
c     _end_var
c     
c     executable code for routine FILL_ECHB_NT:
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='Filling Ntuple by GCPB'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c      CALL BPRNT(IW,'GCPB')
c
      gcpart=0
      ind=0
      
      
c      write(*,*)'----------1---------'
      
      IF(IGCPB.eq.0)IGCPB=MAMIND(IW,'GCPB')

      if(IGCPB.gt.0)ind=IW(IGCPB)

      If(ind.NE.0) Then
c      write(*,*)'----------2---------'

         ncol=iw(ind-5)
         nd=iw(ind)
         i=0

         do ix=1,nd,ncol
	 kk=kk+1
            indx=ind+ix-1
            i=i+1 
            pid(i)=iw(indx+1)
            x(i)=rw(indx+2)
            y(i)=rw(indx+3)	    
	    z(i)=rw(indx+4)
            dedx(i)=rw(indx+5)
            px(i)=rw(indx+6)
            py(i)=rw(indx+7)
            pz(i)=rw(indx+8)
            p_tot(i)=rw(indx+9)
            x2(i)=rw(indx+10)
            theta(i)=rw(indx+11)
            charge(i)=rw(indx+12)
            dca(i)=rw(indx+13)
            index(i)=rw(indx+14)
            phi(i)=rw(indx+15)
            vtl(i)=rw(indx+16)     
            sdist(i)=rw(indx+17)   
            edist(i)=rw(indx+18)   
            npts(i)=iw(indx+19) 
	    r_0(i)=rw(indx+20)
	    fiterr(i)=iw(indx+21)
	    tothits(i)=iw(indx+22)
	    npd_track(i)=iw(indx+23)
	    npd_event(i)=iw(indx+24)
	    bonus_bits(i)=iw(indx+25)
	    q_tot(i)=rw(indx+26)
c	     write(*,*)'FILL GCPB------->', theta(i),z(i),i,ix,nd,ncol,pid(i),kk 
         enddo
         gcpart=i
         ierr=i
      else
c         print *,'No GCPB bank'
      endif
 222   format(F12.6)
c
 99   RETURN
      END
c
c------------------------------------------------------------------------------




