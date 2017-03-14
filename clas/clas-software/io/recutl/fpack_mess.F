      SUBROUTINE FPACK_MESS
c
c_begin_doc
c  RCS ID string
c  $Id: fpack_mess.F,v 1.1.1.1 1997/03/28 20:58:23 marki Exp $
c
c  Documentation for subroutine FPACK_MESS
c
c  Purpose:  probes FPACK message buffers and routes FPACK messages to 
c  --------  RECMES
c
c  Input Parameters: none
c  ----------------
c
c  Output Parameters: none
c  -----------------
c
c  Other routines:  call RECMES and uses CERNLIB string routines
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Mon Jul  1 14:58:14 EDT 1996
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
      PARAMETER (CFILE = '$RCSfile: fpack_mess.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:58:23 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: fpack_mess.F,v 1.1.1.1 1997/03/28 20:58:23 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FPACK_MESS')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER LENOCC
      EXTERNAL LENOCC

      integer iend
      CHARACTER*256 CTEMP
c      CHARACTER*256 FPACK_MESS, CTEMP, FPACK_ERR
c      DATA FPACK_MESS(1:14)/'FPACK message:'/ 
c      DATA FPACK_ERR(1:12)/'FPACK ERROR:'/ 
c_end_var
c
c  executable code for routine FPACK_MESS:
c----6----------------------------------------------------------------72
c
        call FERMES(ctemp,0)
        iend = lenocc(ctemp(1:255))
        if (iend .gt. 0) then
         call recmes(crname,'i',ctemp(1:iend))
        endif

        call FERMES(ctemp,1)
        iend = lenocc(ctemp(1:255))
        if (iend .gt. 0) then
         call recmes(crname,'w',ctemp(1:iend))
        endif
      RETURN
      END
c
c------------------------------------------------------------------------------




