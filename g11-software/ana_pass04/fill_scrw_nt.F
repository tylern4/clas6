      SUBROUTINE FILL_SCRW_NT(ierr)
c
c_begin_doc
c  RCS ID string
c  $Id: fill_scrw_nt.F,v 1.3 1997/12/29 17:56:34 stepanya Exp $
c
c  Documentation for subroutine FILL_SCRW_NT
c
c  Purpose:
c  --------
c
c  Input Parameters: ierr - integer - returnes 1 if there was(were) 
c                     hit(s) in SC
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   S. Stepanya      Created:  Fri Dec 20 11:13:40 EST 1996
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
#include "scrw_nt.inc"
#include "evnt_general.inc"
#include "bcs.inc"
#include "wcs.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      INTEGER ierr
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: fill_scrw_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/12/29 17:56:34 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: fill_scrw_nt.F,v 1.3 1997/12/29 17:56:34 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FILL_SCRW_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER MLINK
      EXTERNAL MLINK
      INTEGER NWRITE
      INTEGER s, entr, ihit, id
      INTEGER ind,nrow,ncol
c_end_var
c
c  executable code for routine FILL_SCRW_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Fills a N-tuple for SC hits'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      entr = 0
      nschit =  0
      
      DO s=1,MAXsector
         ind = MLINK(IW,'SCR ',s)
         IF ( ind.NE.0 ) THEN
            nrow = iw(ind-4)
            ncol = iw(ind-5) 
            if(nrow.ge.20) nrow=20 ! maximum number of hits per sector
            Do ihit = 1, nrow
               if(entr.lt.20)then  
                  entr = entr + 1
                  scsect (entr) = s
                  schid (entr) = ihit
                  scpid (entr) = iw(ind+1)
                  sce (entr)   = rw(ind+2) 
                  sct (entr)   = rw(ind+3) 
                  scx (entr)   = rw(ind+4) 
                  scy (entr)   = rw(ind+5) 
                  scz (entr)   = rw(ind+6)
                  scdy (entr)  = rw(ind+8) 
                  scstat(entr) = iw(ind+10) 
                  scde (entr)  = rw(ind+11) 
                  scdt (entr)  = rw(ind+12)  
               endif
            EndDo
         ENDIF
      ENDDO
      nschit = entr
      IF(nschit.gt.0)THEN
        ierr=0
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------------------




