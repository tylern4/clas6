CDECK  ID>, CCVIEW.
C
      SUBROUTINE CCVIEW
C     =================
c
c_begin_doc
c  RCS ID string
c  $Id: ccview.F,v 1.2 1997/04/24 15:38:14 holtrop Exp $
c
c  Documentation for subroutine CCVIEW
c
c  Purpose: Prepaire QUIP - command for Cerenkov
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
C-
C-   Created    17-JUN-1994   Alexander V. Vlassov
C-   Modified   21-AUG-1995   Alexander V. Vlassov
C-
C----------------------------------------------------------------------
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "cccom.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccview.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:14 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccview.F,v 1.2 1997/04/24 15:38:14 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
c      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccview')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
C-----------------------------------------------
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine CCVIEW
c------------------------------------------------------------------------------
 
      CALL KUNWG(   8)
      CALL KUCMD(' ','CC','C')
      GUID(  1)='Cherenkov light ray tracing'
      CALL KUGUID('CC',GUID,  1,'S')
 
      CALL KUCMD('CC',' ','SW')
 
      CALL KUNWG(   8)
      CALL KUCMD(' ','EVENT','C')
      GUID(  1)='SHOW Cherenkov ray tracing'
      CALL KUGUID('EVENT',GUID,  1,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('EVENT','SXY',' SCALE factor','RO','S')
      CALL KUPVAL('EVENT','SXY',0,0.1,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('EVENT','X0','Shift on x','RO','S')
      CALL KUPVAL('EVENT','X0',0,2.0,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('EVENT','Y0','Shift on y','RO','S')
      CALL KUPVAL('EVENT','Y0',0,-32.0,' ','D')
      GUID(  1)='SHOW the picture of Cherenkov ray tracin'//
     +'g'
      CALL KUGUID('EVENT',GUID,  1,'S')
      CALL KUACT('EVENT',CCDRAW)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
