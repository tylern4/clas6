      PROGRAM RECSIS
c
c_begin_doc
c  RCS ID string
c  $Id: recsis.F,v 1.2 1998/05/05 19:20:45 marki Exp $
c
c  Documentation for PROGRAM RECSIS
c
c  Purpose:   Provide framework for REConstruction and analySIS code on CLAS
c  --------   No bells and No whistles
c
c  Calling Sequence:  Main program 
c  ----------------
c
c  Input Parameters:  
c  ----------------
c
c  Output Parameters:  
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
c  Author:   Arne Freyberger      Created:  Wed Feb  8 13:20:54 EST 1995
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
c#include "bcs.inc"
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
       PARAMETER ( CFILE = '$RCSfile: recsis.F,v $')
       PARAMETER ( CREVIS = '$Revision: 1.2 $')
       PARAMETER ( CSTATE = '$State: Exp $')
       PARAMETER ( CDATE = '$Date: 1998/05/05 19:20:45 $')
       PARAMETER ( CAUTHO = '$Author: marki $')
       DATA CRCSID/
     1'$Id: recsis.F,v 1.2 1998/05/05 19:20:45 marki Exp $'
     3/
c  Module information:
       CHARACTER*(*)  CRNAME, CRAUTH
       PARAMETER (CRNAME='RECSIS')
       PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER LTIN, LTOUT, LLOG, LHST, LTMP, IOERR
c
       EXTERNAL RE_UNITS_BD ! forces linking of re_units.inc's block data
                            ! routine.
c_end_var
c
c  executable code for routine RECSIS:
c----6----------------------------------------------------------------72
c
      call recsis_main
c
      STOP
      END

c   below is the subroutine `NEVER_GETS_CALLED' and it should under no
c   circumstances be CALLED....its use is to FORCE linking of subroutines
c   from the RECSIS library, and not get the default versions in RECUTL
c
c
       SUBROUTINE NEVER_GETS_CALLED
c
c
      INTEGER IRUN, IEV, IREC
c
      CALL RERNEV(IRUN,IEV,IREC)
      CALL RFATAL
      CALL RERROR
c
c
      RETURN
      END



