      SUBROUTINE EC1_INIT
c
c_begin_doc
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
#include "ec1_control.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec1_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1.1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/03/28 20:57:58 $')
      PARAMETER (CAUTHO = '$Author: marki $')
      DATA CRCSID/   
     1'$Id: ec1_init.F,v 1.1.1.1 1997/03/28 20:57:58 marki Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC1_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  -----------------------------
c
c_end_var
c
c  executable code for routine EC_INIT:
c----6----------------------------------------------------------------72
c
c  register the package with the database
c
      CALL REVINI('EC1R',CREVIS,CDATE)
c
      CALL EC1DBA_INIT
c
      CALL RECMES(CRNAME,'N','|')
      CALL RECMES(CRNAME,'I','LAC init start-----')
      CALL RECMES(CRNAME,'N','|')
20    FORMAT('| For this run Ec1_threshold =',F8.4)
      WRITE(CRMESS,20)Ec1_threshold
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,22)ec1stenmin_in
 22   FORMAT('|              Ec1StackEnMin =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)

      WRITE(CRMESS,21)Ec1_reco_type
21    FORMAT('|              Ec1_reco_type =',I4)
      CALL RECMES(CRNAME,'N',CRMESS)
      CALL RECMES(CRNAME,'N','|')
      CALL RECMES(CRNAME,'I','LAC package initialized-----')
      RETURN
      END
c
c------------------------------------------------------------------------------




