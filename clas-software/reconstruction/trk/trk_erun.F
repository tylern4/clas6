      Subroutine trk_erun()
*_begin_doc
*  RCS ID string
*  $Id: trk_erun.F,v 1.6 2004/09/01 20:27:09 lcsmith Exp $
*
*  Documentation for subroutine TRK_ERUN
*
*  Purpose:  This routine will allow RecSis to terminate cleanly the needed 
*  --------  portion of sda. This routine is called once at the end
*            of the job.
*
*  Input Parameters: NONE
*  ----------------
*
*  Output Parameters: NONE
*  -----------------
*
*  Other routines:
*  ---------------
*
*  Notes:
*  ------
*
*  Author:         Curtis A. Meyer 
*  -------         Carnegie Mellon University
*
*  Creation Date:  22 March, 1996
*  --------------
*
*  Major revisions: Aug 1997  F.Klein  --  some statistics on tracking
*  ----------------
*     
*
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files :
*  ---------------------
#include "trktcl_var.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_erun.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.6 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2004/09/01 20:27:09 $')
      PARAMETER ( CAUTHO = '$Author: lcsmith $')
      DATA CRCSID/
     1'$Id: trk_erun.F,v 1.6 2004/09/01 20:27:09 lcsmith Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_ERUN')
      PARAMETER (CRAUTH='Curtis A. Meyer')
*
*  Local User defined variables:
*  -----------------------------
*     
*
      Call RecMes(CRNAME,'I',' TRK Package recognizes EOR')

      CALL trk_stati(ipar_trk_statistics)

      END
