      SUBROUTINE FILL_SHHB_NT(ierr)
c     
c     _begin_doc
c     RCS ID string
c     $Id: fill_shhb_nt.F,v 1.1 2009/03/31 19:00:04 myurov Exp $
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
#include "bcs.inc"
#include "shhb_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: fill_shhb_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2009/03/31 19:00:04 $')
      PARAMETER (CAUTHO = '$Author: myurov $')
      DATA CRCSID/   
     1     '$Id: fill_shhb_nt.F,v 1.1 2009/03/31 19:00:04 myurov Exp $'      
     2     /    
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_SHHB_NT')
      PARAMETER (CRAUTH='whoistheauthor')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE
      INTEGER  ISHHB, ind, indx, ix, nd, ncol, nrow, i,ITGBI
      DATA ISHHB,ITGBI/0,0/
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
         CRMESS='Filling Ntuple by SHHB'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
      IF(ITGBI.eq.0)ITGBI=MAMIND(IW,'TGBI')
      if(ITGBI.gt.0 .AND. IW(ITGBI).GT.0) then
         ind=IW(ITGBI)
      endif
c
      shh_part=0
      ind=0
      IF(ISHHB.eq.0)ISHHB=MAMIND(IW,'SHHB')
      if(ISHHB.gt.0)ind=IW(ISHHB)
      If(ind.NE.0) Then
         ncol=iw(ind-5)         
         nrow=iw(ind-4)
         if(nrow.gt.maxparts_shhb) nrow=maxparts_shhb
         nd=iw(ind)
         do i=1,nrow
            indx=ind+(i-1)*ncol
            shh_id(i)=iw(indx+1)            
            shh_x(i)=rw(indx+2)
            shh_y(i)=rw(indx+3)
            shh_z(i)=rw(indx+4)
            shh_nphe(i)=rw(indx+5)            
            shh_time(i)=rw(indx+6)
            shh_stat(i)=iw(indx+7)
c            print *,'bank',shh_id(i)," ",shh_x(i)," ",shh_y(i),
c     +      " ",shh_z(i)," ",shh_nphe(i)," ",shh_time(i)," ",shh_stat(i)
         enddo
c         shh_part=i
         shh_part=nrow
c         print *,'Hiiits SHHB bank',shh_part," ",nrow
         ierr=i
      else
c         print *,'No SHHB bank'
      endif
c
 99   RETURN
      END
c
c------------------------------------------------------------------------------




