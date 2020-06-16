      SUBROUTINE FILL_SC1_NT
c
c_begin_doc
c  RCS ID string
c  $Id: fill_sc1_nt.F,v 1.1 2001/12/08 02:41:49 stepanya Exp $
c
c  Documentation for subroutine FILL_SC1_NT
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
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
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EDT 1997
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
#include "sc1_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: fill_sc1_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/12/08 02:41:49 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: fill_sc1_nt.F,v 1.1 2001/12/08 02:41:49 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='SC_FILL')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER NAMISC1, IND, MAMIND, ND, SC1_COL, sector, indx, in
      EXTERNAL MAMIND
      integer hit, hid
c_end_var
c
c  executable code for routine SC_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='Filling TOF Histograms, this message written once'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
      IF(NAMISC1.EQ.0) NAMISC1=MAMIND(IW,'SC1 ')
      IND=NAMISC1+1
      hit=0
 13   IND=IW(IND-1)
      IF(IND.NE.0) THEN
         ND = IW(IND)
         SC1_COL=IW(IND-5)
         sector=IW(ind-2)
         hid=sector*100
         if (sector .gt. 6 .or. sector .lt. 1) then
            call recmes(crname,'w','Bad Sector ID')
            goto 13
         endif
c
         DO in = 1,ND,SC1_COL
            indx = ind + in -1
            hit=hit+1
            if(hit.lt.40)then
               sc1sectpd(hit)=100*sector+iw(indx+1)
               sctl(hit)=rw(indx+2)
               scel(hit)=rw(indx+3)
               sctr(hit)=rw(indx+2)
               scer(hit)=rw(indx+3)
            endif
         ENDDO
c
         GO TO 13
      ENDIF
c     return to the top directory and exit
      call hcdir('//PAWC',' ')
      
      RETURN
      END
c
c------------------------------------------------------------------------------




