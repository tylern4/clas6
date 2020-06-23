      SUBROUTINE CC_BOS_FILL
c
c_begin_doc
c  RCS ID string
c  $Id: cc_bos_fill.F,v 1.5 1999/03/09 18:30:50 boiarino Exp $
c
c  Documentation for subroutine CC_BOS_FILL
c
c  Purpose: Filling CC reconstruction bank CCRC
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
c  Author:   Alexander Vlassov    Created:  Fri Jul 26 10:54:14 EDT 1996
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
#include "bcs.inc"
#include "cc_rc.inc"
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
      PARAMETER (CFILE = '$RCSfile: cc_bos_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 18:30:50 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: cc_bos_fill.F,v 1.5 1999/03/09 18:30:50 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*300  CRMESS
      PARAMETER (CRNAME='CC_BOS_FILL')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
c
      integer i, k, icc
c
      integer mbank
c_end_var
c
c  executable code for routine CC_BOS_FILL:
c----6----------------------------------------------------------------72
c
c  filling BOS bank 'CCRC'
c
      if(nrclust.ge.1) then     
c
        icc = mbank(iw,'CCRC',0,NCOL_CCRC,nrclust)
c
        do i = 1 , nrclust
c        
          if(i.gt.maxcrec) then
            write(CRMESS,*) ' More than ',maxcrec,' clusters in CC !!!'
            call recmes(CRNAME,'I',CRMESS)
            go to 10       !  exit from the loop
          end if 
c
          k = icc + NCOL_CCRC*(i-1)
          iw( k +  1 )  =  nrsect(i)
          iw( k +  2 )  =  nrsegm(i)
          iw( k +  3 )  =  nrsegm_p(i)
          iw( k +  4 )  =  nrsegm_m(i)
          iw( k +  5 )  =  nrphe(i)
          iw( k +  6 )  =  nrtime(i)
          iw( k +  7 )  =  nrthet(i)
          iw( k +  8 )  =  nrdthet(i)
          iw( k +  9 )  =  nrphy(i)
          iw( k + 10 )  =  nriec(i)
          iw( k + 11 )  =  nrdiec(i)
          iw( k + 12 )  =  nrstat(i)
c
        end do
   10   continue
      end if
c
c
      RETURN
      END
c
c-----------------------------------------------------------------------



