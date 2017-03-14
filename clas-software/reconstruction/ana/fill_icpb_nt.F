      SUBROUTINE FILL_ICPB_NT(ierr)
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_icpb_nt.F,v 1.4 2006/01/18 23:10:18 stepanya Exp $
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
#include "icpb_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: fill_icpb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/01/18 23:10:18 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1     '$Id: fill_icpb_nt.F,v 1.4 2006/01/18 23:10:18 stepanya Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_ECHB_NT')
      PARAMETER (CRAUTH='whoistheauthor')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE
      INTEGER  IICPB, ind, indx, ix, nd, ncol, i
      DATA IICPB/0/
      INTEGER MAMIND
      EXTERNAL MAMIND
      INTEGER  loop_hit
      INTEGER  nhit_sav
      INTEGER  nhits
c     _end_var
c     
c     executable code for routine FILL_ECHB_NT:
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='Filling Ntuple by ICPB'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
      icpart=0
      ind=0
      IF(IICPB.eq.0)IICPB=MAMIND(IW,'ICPB')
      if(IICPB.gt.0)ind=IW(IICPB)
      If(ind.NE.0) Then
         ncol=iw(ind-5)
         nd=iw(ind)
         i=0
         do ix=1,nd,ncol
            indx=ind+ix-1
            i=i+1
            etc(i)=rw(indx+1)
            ecc(i)=rw(indx+2)
            tc(i)=rw(indx+3)
            tn(i)=rw(indx+4)
            xc(i)=rw(indx+5)
            yc(i)=rw(indx+6)
            zc(i)=rw(indx+7)
            m2c(i)=rw(indx+8)
            m3c(i)=rw(indx+9)
            statc(i)=iw(indx+10)
         enddo
         icpart=i
         ierr=i
      else
c         print *,'No ICPB bank'
      endif
c
 99   RETURN
      END
c
c------------------------------------------------------------------------------




