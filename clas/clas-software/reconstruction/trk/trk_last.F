      Subroutine trk_last()
*_begin_doc
*  RCS ID string
*  $Id: trk_last.F,v 1.5 1998/11/20 14:05:27 fklein Exp $
*
*  Documentation for subroutine TRK_LAST
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
*  Major revisions:
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
      PARAMETER ( CFILE = '$RCSfile: trk_last.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.5 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/11/20 14:05:27 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_last.F,v 1.5 1998/11/20 14:05:27 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_LAST')
      PARAMETER (CRAUTH='Curtis A. Meyer')
*
*  Local User defined variables:
*  -----------------------------
*
      Call RecMes(CRNAME,'I',' Dc Package cleanly terminated. ')
*
* info only if trk_statistics=2 (statistics for each event)
*
      if( ipar_trk_statistics.GT.1 )then
        CALL trk_stati(1)
      endif

      Return
      End
