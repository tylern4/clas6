      Subroutine trk_erun()
*_begin_doc
*  RCS ID string
*  $Id: trk_erun.F,v 1.5 1997/11/22 22:31:28 fklein Exp $
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
      PARAMETER ( CREVIS = '$Revision: 1.5 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1997/11/22 22:31:28 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_erun.F,v 1.5 1997/11/22 22:31:28 fklein Exp $
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

      if( ipar_trk_statistics.EQ.1 )then
        CALL trk_stati(1)
      endif

      END
