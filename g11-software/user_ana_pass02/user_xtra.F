      SUBROUTINE USER_XTRA(err)
c
c_begin_doc
c  RCS ID string
c  $Id: user_xtra.F,v 1.5 2003/09/05 15:33:56 ungaro Exp $
c
c  Documentation for subroutine USER_XTRA
c
c  Purpose:
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
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
c  Author:   Arne Freyberger      Created:  Wed Mar 15 14:05:36 EST 1995
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
      integer err(10)
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: user_xtra.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.5 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2003/09/05 15:33:56 $')
      PARAMETER ( CAUTHO = '$Author: ungaro $')
      DATA CRCSID/
     1'$Id: user_xtra.F,v 1.5 2003/09/05 15:33:56 ungaro Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='USER_XTRA')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER nami, ind, mamind
      EXTERNAL mamind
c_end_var
c
c  executable code for routine USER_XTRA:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      nami = mamind(IW,'HEVT')
      ind = IW(nami)
      if (ind .ne. 0) then
         if(iw(ind+5)/100.gt.0)then
            nami=mamind(IW,'EVNT')
            ind=IW(nami)
            if(ind.gt.0)then
               if(IW(ind+4).lt.0) err(1)=0
               if(IW(ind+12).gt.0.and.IW(ind+14).gt.0.and.IW(ind+15).gt.0)then
                  nami=mamind(IW,'DCPB')
                  ind=IW(nami)
                  if(IW(ind+13).gt.0)err(2)=0
               endif
            endif
         endif  
      endif
ccc writing out all the events
      nami = mamind(IW,'HEAD')
      ind = IW(nami)
      if(ind.gt.0)then
         if(iw(ind+5).lt.10) err(1)=0
      endif
      
c
      RETURN
      END
c
c------------------------------------------------------------------------------




