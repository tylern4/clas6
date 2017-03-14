      SUBROUTINE CC_HRBF
c
c_begin_doc
c  RCS ID string
c  $Id: cc_hrbf.F,v 1.3 1999/03/09 18:30:52 boiarino Exp $
c
c  Documentation for subroutine CC_HRBF
c
c  Purpose: Filling CC_HRB common block from CCH  - bank
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
c  Author:   Alexander Vlassov    Created:  Mon Jul  1 13:23:13 EDT 1996
c  -------
c
c  Major revisions: Alexander Vlassov 04/16/98 - CCH structure was
c  changed.
c  ----------------
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
#include "cc_hrb.inc"
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
      PARAMETER (CFILE = '$RCSfile: cc_hrbf.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 18:30:52 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: cc_hrbf.F,v 1.3 1999/03/09 18:30:52 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*256  CRMESS
      PARAMETER (CRNAME='CC_HRBF')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER IFIRST,NCOL,NROW
      INTEGER ind, i, k, name_i
      integer MAMIND
c_end_var
      data IFIRST /0/
c
c  executable code for routine CC_HRBF:
c----6----------------------------------------------------------------72
c
      IF(IFIRST.eq.0) then
        name_i = MAMIND(IW,'CCH ')
        if(name_i.eq.0) then
          write(CRMESS,*) ' NO CCH  - bank registered' 
          CALL RECMES(CRNAME,'E',CRMESS)
          return
        end if
        IFIRST = 1
      END IF
      ind = iw(name_i)
c
      jhtrk = 0
c
    1 CONTINUE   ! Cycle on "hits" in CCH
c
      IF(ind.NE.0) THEN
c
        NCOL  = iw(ind - 5)
        NROW  = iw(ind - 4)
c
        do i = 1,NROW
c
          k = ind + NCOL*(i-1)
c
          if(i.gt.mhtrk) go to 20  ! too many tracks
c
          jhtrk = jhtrk + 1
          itrk_num(jhtrk) = iw(k + 1)
          idh_prt (jhtrk) = iw(k + 2)
          ihitrk  (jhtrk) = iw(k + 3)
          itrsect (jhtrk) = iw(k + 4)
          itrsegm (jhtrk) = iw(k + 5)
          ph_tot  (jhtrk) = rw(k + 6)
          call ucopy(rw(k + 7),xenter(1,jhtrk),3)
          call ucopy(rw(k +10),xexit (1,jhtrk),3)
c
        end do
c
      ELSE
        IFIRST = 0
        go to 30
c
      END IF
      ind = iw(ind-1)
      go to 1
c
   20 continue
c     ========> too many tracks in CC
c
      write(CRMESS,*) ' Too many tracks in CC  !!!',NROW,' >', mhtrk,
     * ' NCOL,NROW', NCOL,NROW
      CALL RECMES(CRNAME,'W',CRMESS)
c
   30 continue
c
      RETURN
      END




