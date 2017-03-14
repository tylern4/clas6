      SUBROUTINE USER_EVNT
c
c_begin_doc
c  RCS ID string
c  $Id: user_evnt.F,v 1.10 1999/11/01 21:55:34 freyberg Exp $
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
      PARAMETER ( CREVIS = '$Revision: 1.10 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/11/01 21:55:34 $')
      PARAMETER ( CAUTHO = '$Author: freyberg $')
      DATA CRCSID/
     1'$Id: user_evnt.F,v 1.10 1999/11/01 21:55:34 freyberg Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH, CRMESS
      PARAMETER (CRNAME='USER_EVNT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer namind
      external namind

      integer nami, ind, ns, npart,ierr,err(10), i,itype
       INTEGER LENOCC

c_end_var
c
c  executable code for routine USER_EVNT:
c  -------------------------------------
c
c  The following is just an example of how to fill histograms, including
c  changing PAWC directories.
c
       nami = namind('HEAD')

c now get the location of the HBTR bank

       ind = IW(nami)

c now get the momentum (ind+3) ... remember this is a REAL so you
c need use the RW array (IW for integers, IW16 for short-integers)
      
       if (ind .ne. 0) then
          itype = iw(ind+5)
       else
          call recmes(crname,'F','Record without a header!!!')
       endif

       if (itype .lt. 1 .or. itype .gt. 9) then
          write (crmess,*)'Made it into user_evnt with event type',itype
          call recmes(crname,'D',crmess)
       endif

c  Call the C version of user_evnt that can use the c_bos_io library - JM
      call user_evnt_c

      ns = ns + 1
      call hfill(10,float(ns),0.0,1.0)

c change to your subdirectory

      call hcdir('//PAWC/MYDIR',' ')

       call hfill(29,2.0*ns,0.0,1.0)
       call hfill(10,1000.0-ns,0.0,1.0)

c get information from the HBTR bank (Hit Based Tracking Results)
c The third item is the track momentum

c  first get the pointer to the location of the HBTR bank

       nami = namind('HBTR')

c now get the location of the HBTR bank

       ind = IW(nami)

c now get the momentum (ind+3) ... remember this is a REAL so you
c need use the RW array (IW for integers, IW16 for short-integers)
      
       if (ind .ne. 0) then
         call hfill(30, rw(ind+3),0.0,1.0)
       endif


c change back to top directory
       
       call hcdir('//PAWC',' ')

c  Calling FSEQW tells FPACK to write this event out
c  comment it out or place in an if-then if you wish to be
c  a bit more sophisticated then merely writing every event out

       do i=1,10
          err(i)=1
       enddo
C
C
C
      DO i=1,10
        IF(outbanknames(i).ne.'junk')THEN
          ns=i+1
          If(outbanknames(i).eq.'all')Then
            call fwbos(iw,ns,CBANK_WRITE(1:LENOCC(CBANK_WRITE)),ierr)
          Else
            if(err(i).eq.0)call fwbos(iw,ns,outbanknames(i),ierr)
          Endif
        ENDIF
      ENDDO

       RETURN
       END
c
c----------------------------------------------------------------------




