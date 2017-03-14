      SUBROUTINE USER_BRUN
c
c_begin_doc
c  RCS ID string
c  $Id: user_brun.F,v 1.1 2001/08/30 16:51:47 feuerbac Exp $
c
c  Documentation for subroutine USER_BRUN
c
c  Purpose: USER supplied routined called at the BEGINNING of EVERY run
c  --------
c
c  Calling Sequence:
c  ----------------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters:  NONE
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
c  Author:   Arne Freyberger      Created:  Wed Mar 15 14:01:12 EST 1995
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
#include "wcs.inc"
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
      CHARACTER*(*)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO 
      PARAMETER (CFILE=  '$RCSfile: user_brun.F,v $')
      PARAMETER (CREVIS= '$Revision: 1.1 $')
      PARAMETER (CSTATE= '$State: Exp $')
      PARAMETER (CDATE=  '$Date: 2001/08/30 16:51:47 $')
      PARAMETER (CAUTHO= '$Author: feuerbac $')
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='USER_BRUN')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
      integer namind
      external namind
      integer nlink
      external nlink

      integer ind

      INTEGER NWRITE
      integer namidcgw
      integer runno
      data namidcgw/0/
      
c_end_var
c
c  executable code for routine USER_BRUN:
c  -------------------------------------
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='This is a DUMMY routine, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c     namidcgw = NAMIND('DCGW')

      if (namidcgw.le.0 .OR. iw(namidcgw).le.0) then !no DCGW bank
         ind = nlink('HEAD',0)
         runno = iw(ind+HEAD_nrun)
         call dc_make_dgeo(runno,1)
         call dc_geom_banks
      endif  ! I need the DCGW banks

c      call wirebos
      
      RETURN
      END
c
c------------------------------------------------------------------------------




