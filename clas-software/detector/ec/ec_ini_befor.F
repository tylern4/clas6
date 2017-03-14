      SUBROUTINE EC_INI_BEFOR
c     
c     _begin_doc
c     RCS ID string
c     $Id: ec_ini_befor.F,v 1.5 2008/03/16 04:33:15 fklein Exp $
c     
c     Documentation for subroutine EC_INI_BEFOR
c     
c     Purpose:
c     --------
c     
c     Input Parameters:  (Name - Type - Meaning)
c     ----------------
c     
c     Output Parameters:  (Name - Type - Meaning)
c     -----------------
c     
c     Other routines:
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:         Created:  Fri Aug  9 10:52:47 EDT 1996
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
#include "Ec_general.PAR"
#include "EcEvu.CMN"
#include "EcStrip.CMN"
#include "EcFit_general.CMN"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_ini_befor.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.5 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:15 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1     '$Id: ec_ini_befor.F,v 1.5 2008/03/16 04:33:15 fklein Exp $'   
     2     /   
c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_INI_BEFOR')
      PARAMETER (CRAUTH=' ')
c
c  Local User defined variables:
c  -----------------------------
c      INTEGER NWRITE
      INTEGER S, stripID
      INTEGER L
      INTEGER AX
c     _end_var
c     
c     executable code for routine EC_INI_BEFOR:
c---- 6----------------------------------------------------------------72
c     
      DO S = 1, MAXsectors
        EcFit_Nhit(whole,S)=0
        DO L = inner, cover
          EcFit_Nhit(L,S)=0
          DO AX = Ec_u, Ec_w
            EcEvu_Nreported(AX,L,S)=0
            Ec_StripN(AX,L,S)=0
            do stripid=1,maxstrips
              Ec_strip(ENERGY,stripID,ax,l,s)=0.0
            enddo
          ENDDO
        ENDDO
      ENDDO
      RETURN
      END
c
c------------------------------------------------------------------------------




