      FUNCTION GET_XXTR(ilnk)
c
c_begin_doc
c  RCS ID string
c  $Id: get_xxtr.F,v 1.2 1999/02/24 02:50:20 boiarino Exp $
c
c  Documentation for subroutine GET_XXTR
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
c  Author:   James Mueller      Created:  Tue Apr 14 11:04:57 EDT 1998
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
      integer ilnk
      integer get_xxtr
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: get_xxtr.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/02/24 02:50:20 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: get_xxtr.F,v 1.2 1999/02/24 02:50:20 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*200  CRMESS
      PARAMETER (CRNAME='GET_XXTR')
      PARAMETER (CRAUTH='James Mueller')
c
c  Local User defined variables:
c  -----------------------------
      integer mamind
      integer n_hbtr,l_hbtr,ncol,ntracks,index
c_end_var
c
c  executable code for routine GET_XXTR:
c----6----------------------------------------------------------------72
c
      index = 0
      IF(n_hbtr .le. 0) n_hbtr = MAMIND(iw,'HBTR')
      l_hbtr = IW(n_hbtr)
      IF(l_hbtr .GT. 0) THEN
         ncol = IW(l_hbtr-5)
         ntracks = IW(l_hbtr-4)
         if(ilnk.gt.ntracks.or.ilnk.le.0)then
            CRMESS='Requested track # > size of bank'
            CALL RECMES(CRNAME,'I',CRMESS)
         ELSE
            index = l_hbtr + (ilnk-1)*ncol
         ENDIF
      ELSE
         CRMESS='No HBTR bank found'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
c
      get_xxtr = index
      RETURN
      END
c
c------------------------------------------------------------------------------




