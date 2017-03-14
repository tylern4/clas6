      REAL FUNCTION SC_ERR(id,s,ax)
c
c_begin_doc
c  RCS ID string
c  $Id: sc_err.F,v 1.4 1997/06/06 14:45:03 stepanya Exp $
c
c  Documentation for function SC_ERR
c
c  Purpose:
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
c  Author:   S.Stepanyan      Created:  Mon Feb 24 20:10:25 EST 1997
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
#include "evnt_general.inc"
#include "schits.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
      integer id,s,ax     
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: sc_err.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/06/06 14:45:03 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: sc_err.F,v 1.4 1997/06/06 14:45:03 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='SC_ERR')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine SC_ERR:
c----6----------------------------------------------------------------72
c
      GO TO (10, 20, 30), ax
   10 continue
      sc_err = sqrt (
     &               ScFit_paddle(sc_dix,id,S)**2 
     +       +       ScFit_paddle(sc_djx,id,S)**2
     +       +       ScFit_paddle(sc_dkx,id,S)**2)
      RETURN
   20 continue
      sc_err = sqrt (
     &               ScFit_paddle(sc_diy,id,S)**2 
     +       +       ScFit_paddle(sc_djy,id,S)**2
     +       +       ScFit_paddle(sc_dky,id,S)**2)
      RETURN
   30 continue
      sc_err = sqrt (
     &ScFit_paddle(sc_diz,id,S)**2 + ScFit_paddle(sc_dkz,id,S)**2)
      RETURN
      END
c
c------------------------------------------------------------------------------




