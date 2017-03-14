      SUBROUTINE FILL_ICHB_NT(ierr)
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_ichb_nt.F,v 1.7 2008/03/16 04:33:09 fklein Exp $
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
#include "ichb_nt.inc"
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
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:09 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1     '$Id: fill_ichb_nt.F,v 1.7 2008/03/16 04:33:09 fklein Exp $'   
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
      INTEGER  IICHB, ind, indx, nd, ncol, nrow, i
      DATA IICHB/0/
      INTEGER MAMIND
      EXTERNAL MAMIND
      INTEGER  loop_hit
      INTEGER  nhit_sav
      INTEGER  nhits
c     _end_var
c     
c     executable code for routine FILL_ICHB_NT:
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
      ierr=0
      IF(IICHB.eq.0)IICHB=MAMIND(IW,'ICHB')
      if(IICHB.gt.0)ind=IW(IICHB)
      If(ind.NE.0) Then
         ncol=iw(ind-5)
         nrow=iw(ind-4)
         if(nrow.gt.maxparts_ichb) nrow=maxparts_ichb
         nd=iw(ind)
         do i=1,nrow
            indx=ind+(i-1)*ncol
            et(i)=rw(indx+1)
            egl(i)=rw(indx+2)
            time(i)=rw(indx+3)
            time_next(i)=rw(indx+4)
            ich_x(i)=rw(indx+5)
            ich_y(i)=rw(indx+6)
            ich_z(i)=rw(indx+7)
            ich_xgl(i)=rw(indx+8)
            ich_ygl(i)=rw(indx+9)
            ich_xwid(i)=rw(indx+10)
            ich_ywid(i)=rw(indx+11)
            ich_xm3(i)=rw(indx+12)
            ich_ym3(i)=rw(indx+13)
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




