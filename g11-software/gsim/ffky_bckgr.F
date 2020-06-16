      SUBROUTINE FFKY_BCKGR
c
c_begin_doc
c  RCS ID string
c  $Id: ffky_bckgr.F,v 1.3 1998/06/08 19:29:31 fklein Exp $
c
c  Documentation for subroutine FFKY_BCKGR
c
c  Purpose:
c  --------
C  ffkey definitions for clas_bckgr.F
c
c  Other routines:
c  ---------------
c
c  Supports clas_bckgr.F
c
c  Notes:
c  ------
c
c  Author:   Maurik Holtrop      Created:  Wed Jan 15 12:18:30 EST 1997
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

#include "ffpar_bckgr.inc"

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
      PARAMETER (CFILE = '$RCSfile: ffky_bckgr.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1998/06/08 19:29:31 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: ffky_bckgr.F,v 1.3 1998/06/08 19:29:31 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='FFKY_BCKGR')
      PARAMETER (CRAUTH='Maurik Holtrop')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine FFKY_BCKGR:
c----6----------------------------------------------------------------72
c
c  Set the luminocity to 1e-32 , low background.
C  Set target to hydrogen.

      ntarget = 1
      call ffkey('NTARGET',ntarget,1,'INTEGER')

      olumen = 1.E-32
      call ffkey('LUMEN',olumen,1,'REAL')

      zelem = 1.
      call ffkey('ZTAR',zelem,1,'REAL')

      atom = 1.
      call ffkey('ATAR',atom,1,'REAL')

      rho = 7.E-3
      call ffkey('RHOT',rho,1,'REAL')

      tleng = 7.6
      call ffkey('TLENG',tleng,1,'REAL')

      timedc = 250.E-9
      call ffkey('TIMDC',timedc,1,'REAL')

      nincel=100000000
      call ffkey('NINCE',nincel,1,'INTEGER')

      RETURN
      END
c
c------------------------------------------------------------------------------




