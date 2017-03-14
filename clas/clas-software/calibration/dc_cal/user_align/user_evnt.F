      SUBROUTINE USER_EVNT
c
c_begin_doc
c  RCS ID string
c  $Id: user_evnt.F,v 1.3 2002/05/27 23:17:23 sam Exp $
c
c  Documentation for subroutine USER_EVNT
c
c  Purpose:  USER supplied EVENT processing routine, called once per event
c  --------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Wed Mar 15 14:01:23 EST 1995
c  -------
c
c  Major revisions:
c  ----------------
c  7/3/97 - added a call to user_evnt_c the C version of user_evnt which
c           is able to access the c_bos_io library of BOS utilities - JM
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
#include "bnkfmt.inc"
#include "run_control.inc"
#include "user_control.inc"
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
      PARAMETER ( CFILE = '$RCSfile: user_evnt.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.3 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2002/05/27 23:17:23 $')
      PARAMETER ( CAUTHO = '$Author: sam $')
      DATA CRCSID/
     1'$Id: user_evnt.F,v 1.3 2002/05/27 23:17:23 sam Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='USER_EVNT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer namind
      external namind
      integer tbtrind/0/
      INTEGER I,SECT(10),ind,thissect
      integer nrows,ncol,opsect
      logical keep
      
      integer ns,npart,ierr,err(10),itype
      integer LENOCC
      
      character*132 droplist
      data droplist/
     $     'DC1 DHCLECHBECPCECPIECPOHBLAHDPLSCRCSCR '/


c_end_var
c
c  executable code for routine USER_EVNT:
c  -------------------------------------
c
c  The following is just an example of how to fill histograms, including
c  changing PAWC directories.
c

      
      if (align_chuck) goto 999
      call vertex_tbtr


c  Calling FSEQW tells FPACK to write this event out
c  comment it out or place in an if-then if you wish to be
c  a bit more sophisticated then merely writing every event out
       if (tbtrind.le.0) tbtrind = namind('TBTR')

c       if (tbtrind.le.0) print*, 'tbtrind was .le. 0'
c       print*, 'tbtrind = ', tbtrind, ' / namind(TBTR) = ', namind('TBTR'),
c     &         ' / iw(tbtrind) = ', iw(tbtrind)

c  to be written out, the event must have tracks in opposite sectors.
       keep = .false.

       do i=1,6
          sect(i) = 0
       enddo
       if (tbtrind.gt.0.and.iw(tbtrind).gt.0) then   ! TBT succeeded
          keep = (align_photon_run.EQ.1)      ! FOR PHOTONS, expect NOT 2 tracks
          ind = iw(tbtrind)
          nrows = iw(ind-4)
          ncol = iw(ind-5)
          i = 0
c   a fortran DO UNTIL keep or i >= nrows
 10       continue
            thissect = iw(ind+i*ncol+tbtr_itr_sec)/100
            opsect = mod(thissect+2,6)+1
            sect(thissect) = 1
            if (sect(opsect).eq.1) keep = .true.
         i = i + 1
          if (keep.or.i.ge.nrows) then
             if (keep) then

c     If we are keeping the event, save the info on it.
c     change to your subdirectory
                call hcdir('//LUN1/ALIGN',' ')
                call hcdir('//PAWC/ALIGN',' ')
                call align_fill

c     change back to top directory
       
                call hcdir('//PAWC',' ')

                call bdrop(iw,droplist)
c                CALL FSEQW('CLASOUTPUT')
             endif
          else
             goto 10
          endif
       endif

       do i=1,10
          err(i)=1
       enddo
       
       DO i=1,10
          IF(outbanknames(i).ne.'junk')THEN
             ns=i+1
             if(outbanknames(i).eq.'all')Then
                call fwbos(iw,ns,
     $               CBANK_WRITE(1:LENOCC(CBANK_WRITE)),ierr)
             Elseif(lnblnk(outbanknames(i)).gt.0) then
c               if(err(i).eq.0)
                call fwbos(iw,ns,outbanknames(i),ierr)
             Endif
          ENDIF
       ENDDO

 999   continue
       RETURN
       END
c
c----------------------------------------------------------------------




