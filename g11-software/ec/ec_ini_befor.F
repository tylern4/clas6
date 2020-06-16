      SUBROUTINE EC_INI_BEFOR
c
c_begin_doc
c  RCS ID string
c  $Id: ec_ini_befor.F,v 1.3 1997/05/19 15:01:07 qin Exp $
c
c  Documentation for subroutine EC_INI_BEFOR
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
c  Author:         Created:  Fri Aug  9 10:52:47 EDT 1996
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
#include "Ec_general.PAR"
#include "EcEvu.CMN"
#include "EcStrip.CMN"
#include "EcFit_general.CMN"
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
      PARAMETER (CFILE = '$RCSfile: ec_ini_befor.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/05/19 15:01:07 $')
      PARAMETER (CAUTHO = '$Author: qin $')
      DATA CRCSID/   
     1'$Id: ec_ini_befor.F,v 1.3 1997/05/19 15:01:07 qin Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_INI_BEFOR')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER NWRITE
      INTEGER S
      INTEGER L
      INTEGER AX
c_end_var
c
c  executable code for routine EC_INI_BEFOR:
c----6----------------------------------------------------------------72
c
      DO S = 1, MAXsectors
        EcFit_Nhit(whole,S)=0
        DO L = inner, outer
          EcFit_Nhit(L,S)=0
          DO AX = Ec_u, Ec_w
            EcEvu_Nreported(AX,L,S)=0
            Ec_StripN(AX,L,S)=0
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END
c
c------------------------------------------------------------------------------




