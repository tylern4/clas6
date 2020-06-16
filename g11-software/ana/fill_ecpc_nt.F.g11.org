      SUBROUTINE FILL_ECPC_NT(ID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_ecpo_nt.F,v 1.1 1998/08/21 21:33:01 stepanya Exp $
c
c  Documentation for subroutine BOOK_ECPO_NT
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
c  Author:    S. Stepanyan     Created:  Fri Aug 14 00:41:53 EST 1998
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
#include "ecpc_nt.inc"

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
      PARAMETER (CFILE = '$RCSfile: book_ecpo_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/08/21 21:33:01 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: book_ecpo_nt.F,v 1.1 1998/08/21 21:33:01 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_ECPO_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER ID
      integer iecpc,ierr,ind,ncol,idd,nd,i,ix,indx
      INTEGER namind
      external namind  

      data iecpc/0/
        
c_end_var
c
c  executable code for routine BOOK_ECPO_NT:
c----6----------------------------------------------------------------72
c

        
      IF (IECPC.eq.0) IECPC=NAMIND('ECPC')
     
      ierr      = 0
      ind       = 0
      ecpc_nhit = 0
     
      ind = 0
      i   = 0
     
      if (IECPC.gt.0) then
        ind = IW(IECPC)
      else
        return
      endif
     
99    if (ind.ne.0) then
        ncol            = iw(ind-5)
        idd             = iw(ind-2)
        nd              = iw(ind)
        do ix=1,nd,ncol
          indx          = ind+ix-1
          i             = i+1
          ecpc_hit(i)   = idd
          ecpc_id(i)    = iw(indx+1)
          ecpc_tdc(i)   = min(4095,max(0,rw(indx+2)))
          ecpc_adc(i)   = min(4095,max(0,rw(indx+3)))
          ecpc_nhit     = i
        enddo
      else
        return
      endif
     
      ind               = iw(ind-1)
      go to 99
     
      end
