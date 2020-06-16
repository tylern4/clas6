      SUBROUTINE FILL_ICHB_NT(ierr)
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_ichb_nt.F,v 1.3 2003/12/08 22:30:55 stepanya Exp $
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
#include "seb_ntn.inc"
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
      PARAMETER (CFILE = '$RCSfile: fill_ichb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2003/12/08 22:30:55 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1     '$Id: fill_ichb_nt.F,v 1.3 2003/12/08 22:30:55 stepanya Exp $'   
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
      INTEGER  IICHB, ind, indx, ix, nd, ncol, i
      DATA IICHB/0/
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
         CRMESS='Filling Ntuple by ECHB'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
      ic_part=0
      ind=0
      IF(IICHB.eq.0)IICHB=MAMIND(IW,'ICHB')
      if(IICHB.gt.0)ind=IW(IICHB)
      If(ind.NE.0) Then
         ncol=iw(ind-5)
         nd=iw(ind)
         i=0
         do ix=1,nd,ncol
            indx=ind+ix-1
            i=i+1
            et(i)=rw(indx+1)
            egl(i)=rw(indx+2)
            time(i)=rw(indx+3)
            tgl(i)=rw(indx+4)
            ich_x(i)=rw(indx+5)
            ich_y(i)=rw(indx+6)
            ich_z(i)=rw(indx+7)
            ich_xgl(i)=rw(indx+8)
            ich_ygl(i)=rw(indx+9)
            ich_zgl(i)=rw(indx+10)
            ic_m2(i)=rw(indx+11)
            ic_m3(i)=rw(indx+12)
            ic_m4(i)=rw(indx+13)
            ic_stat(i)=iw(indx+14)
         enddo
         ic_part=i
         ierr=i
      else
c         print *,'No ICHB bank'
      endif
c
      if(ic_part.gt.0) then
c         ierr=0
      endif
 99   RETURN
      END
c
c------------------------------------------------------------------------------




