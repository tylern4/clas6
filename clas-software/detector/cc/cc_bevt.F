      SUBROUTINE CC_BEVT
c
c_begin_doc
c  RCS ID string
c  $Id: cc_bevt.F,v 1.10 2005/11/23 21:01:09 vlassov Exp $
c
c  Documentation for subroutine CC_BEVT
c
c  Purpose:   Reading and unpacking the CC bank for following
c  --------   reconstraction procedure
c
c   Input Parameters :    none
c  ----------------
c
c   Output Parameters:    /cc_revt/ filling
c  -----------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov      Created:  Fri Jun 21 18:16:11 EDT 1996
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
c BOS common block 
#include "bcs.inc"
c   CC reconstraction common block
#include "cc_revt.inc"
c 
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
      PARAMETER (CFILE = '$RCSfile: cc_bevt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.10 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2005/11/23 21:01:09 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/   
     1'$Id: cc_bevt.F,v 1.10 2005/11/23 21:01:09 vlassov Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='CC_BEVT')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IFIRST
      INTEGER ind, kword, i, k, name_i, NCOL, NROW, NSECT
      integer ievent,irun,itrig
      integer MAMIND
c_end_var
      data IFIRST /0/
c
c  executable code for routine USER_BEVT:
c
 900  format(a10,i10)
c----6----------------------------------------------------------------72
c
      n_hits_CC = 0
c
      name_i = MAMIND(IW,'CC  ')
      if(name_i.eq.0) then
        CRMESS= ' NO CC - bank registered'
        CALL RECMES(CRNAME,'W',CRMESS)
        return
      end if
c
      ind = iw(name_i)
c
    1 CONTINUE   ! Cycle on "hits" in CC 
c
      IF(ind.NE.0) THEN
c
        NSECT = iw(ind - 2)
        kword = iw(ind)
        NCOL = iw(ind - 5)
        NROW = iw(ind - 4)
c
        k = 2*ind
        do i = 1, NROW
c
          n_hits_CC =  n_hits_CC + 1
          n_SECT_CC(n_hits_CC) = NSECT
          n_CC_hit (n_hits_CC) = iw16(k + 1)
          I_TDC_CC (n_hits_CC) = iw16(k + 2)
          I_ADC_CC (n_hits_CC) = iw16(k + 3)
c
          k = k + 3
        end do
c
      ELSE
        go to 10
      END IF
c      
      ind = iw(ind-1)
      go to 1
c
   10 continue
c     ========> some other actions
c
c=========>   CC_HRB block filling (CC hits bank )
c
c     This should work only for GEANT - made files:
c     =============================================
      call rernev(irun,ievent,itrig)
      if(irun.le.100) then
        call CC_HRBF
      end if
c
c=========>
      RETURN
      END
c
c------------------------------------------------------------------------
