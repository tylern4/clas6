      SUBROUTINE BOOK_ECPC_NT(ID)
c
c_begin_doc
c  RCS ID string
c  $Id: book_ecpo_nt.F,v 1.1 1998/08/21 21:33:01 stepanya Exp $
c
c  Documentation for subroutine BOOK_ECPO_NT
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
c  Author:    S. Stepanyan     Created:  Fri Aug 14 00:41:53 EST 1998
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
#include "ecpc_nt.inc"
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
      PARAMETER (CFILE = '$RCSfile: book_ecpo_nt.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.1 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/08/21 21:33:01 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: book_ecpo_nt.F,v 1.1 1998/08/21 21:33:01 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='BOOK_ECPO_NT')
      PARAMETER (CRAUTH='Stepan Stepanyan')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ID
c_end_var
c
c  executable code for routine BOOK_ECPO_NT:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Booking a N-tuple for EC enegy calibration info'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
C Ntuple block for ECPC calibration bank linking ECHB hits to raw data
      call hbname(ID,'ecpc',ecpc_nhit,
     +'ecpc_nhit[0,256]:I,ecpc_hit(ecpc_nhit)[101,636]:I,'//
     +'ecpc_id(ecpc_nhit)[101,636]:I,ecpc_tdc(ecpc_nhit)[0,4095]:I,'//
     +'ecpc_adc(ecpc_nhit)[0,4095]:I')
    
      end


