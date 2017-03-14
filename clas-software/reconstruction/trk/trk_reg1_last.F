      SUBROUTINE TRK_REG1_LAST
c
c_begin_doc
c  RCS ID string
c  $Id: trk_reg1_last.F,v 1.2 1998/12/09 15:49:23 farhi Exp $
c
c  Documentation for subroutine TRK_REG1_LAST.F
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
c  Author:   L. Farhi      Created:  Tue Nov  3 14:04:06 MET 1998
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
      PARAMETER (CFILE = '$RCSfile: trk_reg1_last.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/12/09 15:49:23 $')
      PARAMETER (CAUTHO = '$Author: farhi $')
      DATA CRCSID/   
     1'$Id: trk_reg1_last.F,v 1.2 1998/12/09 15:49:23 farhi Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRK_REG1_LAST.F')
      PARAMETER (CRAUTH='L. Farhi')
c
c  Local User defined variables:
c  -----------------------------
      integer i,sum
      COMMON/REG1STAT/stat_array
      INTEGER stat_array(20)
c_end_var
c
c  executable code for routine TRK_REG1_LAST.F:
c----6----------------------------------------------------------------72
c
      sum = stat_array(10)+stat_array(11)+stat_array(12)+stat_array(13)
      call recmes(crname,'I','Region1 tracking status :')
      DO i=10,13
        write(crmess,
     &       '(''status '',i1,'' : '',i6,'' tracks, '',f5.1,''%'')')
     &       i-10,stat_array(i),100.*stat_array(i)/sum
        call recmes(crname,'I',crmess)
      ENDDO

      RETURN
      END
c
c------------------------------------------------------------------------------




