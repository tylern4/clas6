      subroutine trk_reg1_init
*_begin_doc
*  RCS ID string
*  $Id: trk_reg1_init.F,v 1.2 1998/11/02 17:23:41 farhi Exp $
*
*_end_doc
*
      IMPLICIT NONE
*
*_begin_inc
*  include files :
*  ---------------------
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
      PARAMETER ( CFILE = '$RCSfile: trk_reg1_init.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/11/02 17:23:41 $')
      PARAMETER ( CAUTHO = '$Author: farhi $')
      DATA CRCSID/
     1'$Id: trk_reg1_init.F,v 1.2 1998/11/02 17:23:41 farhi Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_REG1_INIT')
      PARAMETER (CRAUTH='Laurent Farhi')
*
*  Local User defined variables:
*  -----------------------------
*
C
C Local variables
C
      integer i
      COMMON/REG1STAT/stat_array
      INTEGER stat_array(20)

      DO i=1,20
        stat_array(i) = 0
      enddo

      CRMESS = 'Initializing MINUIT, calling MNINIT.'
      CALL RECMES(CRNAME,'I',CRMESS)
      CALL MNINIT(5,6,6)

      return
      end
