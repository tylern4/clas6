      SUBROUTINE USER_BEVT
c
c_begin_doc
c  RCS ID string
c  $Id: user_bevt.F,v 1.2 2002/05/27 23:17:23 sam Exp $
c
c  Documentation for subroutine USER_BEVT
c
c  Purpose:  USER routine called BEFORE any normal EVENT processing
c  --------
c
c  Input Parameters:  
c  ----------------
c
c  Output Parameters: 
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Thu Mar  7 20:34:19 EST 1996
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
#include "user_control.inc"
#include "trktcl_var.inc"
#include "clas_offsets.inc"
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
      PARAMETER (CFILE = '$RCSfile: user_bevt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/05/27 23:17:23 $')
      PARAMETER (CAUTHO = '$Author: sam $')
      DATA CRCSID/   
     1'$Id: user_bevt.F,v 1.2 2002/05/27 23:17:23 sam Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*400  CRMESS
      PARAMETER (CRNAME='USER_BEVT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      logical first/.true./
      integer i,last
      integer ind_bank(50)/50*0/
      character*4 name_bank(50)/'EC01','ECHB','ECPI', ! ec
     $     'CC01',                                    ! cc
     $     'SC1 ','SCR ',                             ! sc
     $     'HEVT','EVNT','DCPB','SCPB','CCPB','UNUS','EVHB', !seb 1
     $     'HBID',                                    ! hbid
     $     'TBTR','TBLA','TBER','TDPL',               ! tbt
     $     'TBID','PART',                             ! tbt pid
     $     30*'    '/
      integer nbanks/20/
      character*200 mylist
      character*200 tmplist
c those variables are not needed now.

      integer namind
      external namind

      integer tbtr/-1/
      integer ind
      integer ndown, nup, ncol, ind_row
c_end_var
c
c  executable code for routine USER_BEVT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

      align_chuck = .FALSE.
      if (align_vert_cut.eq.0) goto 300  ! don't do vertex cut
c look through the tracks for one with a vertex at the target window


      ndown = 0
      nup = 0
      if (tbtr.le.0) tbtr=namind('TBTR')
      if (tbtr.gt.0) then
         if (iw(tbtr).gt.0) then
            ind = iw(tbtr)
            ncol = iw(ind-5)
            do i=1,iw(ind-4)
               ind_row = ind+(i-1)*ncol
               if (abs(rw(ind_row+tbtr_z)-dstmwall).LT.dstmlen) then
                  ndown = ndown+1
               else if (abs(rw(ind_row+tbtr_z)-upstmwall).LT.upstmlen) then
                  nup = nup + 1
               endif
            enddo
         endif
      endif
      if (nup.gt.0 .or. ndown.gt.0) then
         if (nup.gt.ndown) then
            dpar_TargetLen = target_wall
            dpar_TargetPos(3) = upstmwall
         else
            dpar_TargetLen = target_wall
            dpar_TargetPos(3) = dstmwall
         endif
         align_chuck = .FALSE.
      else
         align_chuck = .TRUE.
      endif

 300  continue

      if (first) then
         first=.false.
         do i = 1,200
            mylist(i:i) = '.'
         enddo
         do i = 1,nbanks
            tmplist = mylist
            last = index(tmplist,'.')
            if (last.LT.2) then
               mylist = name_bank(i) // tmplist(last+4:)
            else
               mylist=tmplist(1:last-1)//name_bank(i)//tmplist(last+4:)
            endif
         enddo
         last = index(mylist,'.')
         write(crmess,'(A,A)')'Dropping the bank list: ',
     $          mylist(1:last-1)
         call recmes(crname,'I',crmess)
      endif

      call bdrop(iw,mylist(1:last-1))
c      call bprnt(iw,mylist(1:last-1))

      RETURN
      END
c
c------------------------------------------------------------------------------

