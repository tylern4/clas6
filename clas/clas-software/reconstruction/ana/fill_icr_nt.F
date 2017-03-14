      SUBROUTINE FILL_ICR_NT(NTID)
c
c_begin_doc
c  RCS ID string
c  $Id: fill_icr_nt.F,v 1.8 2008/06/06 21:52:24 pasyuk Exp $
c
c  Documentation for subroutine FILL_ICR_NT.F
c
c  Purpose:
c  --------
c
c  Input Parameters: Non (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:Non  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:  S. Stepanyan       Created:  Fri Dec 20 11:12:59 EST 2003
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
#include "clasmdl.inc"
#include "icr_nt.inc"
#include "ichit.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_icr_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.8 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/06/06 21:52:24 $')
      PARAMETER (CAUTHO = '$Author: pasyuk $')
      DATA CRCSID/   
     1'$Id: fill_icr_nt.F,v 1.8 2008/06/06 21:52:24 pasyuk Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_ICR_NT.F')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE, NTID, i, j, k, myshift
c_end_var
c
c  executable code for routine FILL_SCRW_NT.F:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling a N-tuple for IC hits'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
       Do i=1,nhit
         icix(i)=ixhit(i)
         iciy(i)=iyhit(i)
         ict(i)=Ticm(i)
         ice(i)=Eicm(i)
         icstat(i)=0
         icfrac(i)=0
         do j=1,ngroup
            do k=1,nmember(j)
               if(groupmember(j,k).eq.i) then
                  icstat(i)=j+100*icstat(i)
                  myshift=(j-1)*1000
                  myshift=max(1,myshift)
                  icfrac(i)=icfrac(i)+myshift*int(fre(j,k)*1000.)
               endif
            enddo
         enddo
      EndDo
      nichit=nhit
      RETURN
      END
c
c------------------------------------------------------------------------------




