      SUBROUTINE EID_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: eid_init.F,v 1.2 2000/12/13 22:38:22 fklein Exp $
c
c  Documentation for subroutine EID_INIT
c
c  Purpose:  Initialization for fast electron identification package
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
c  Author:   Alexander Vlassov    Created:  Mon Nov  4 14:21:08 EST 1996
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
#include "eidcontr.inc"
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
      PARAMETER (CFILE = '$RCSfile: eid_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2000/12/13 22:38:22 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/   
     1'$Id: eid_init.F,v 1.2 2000/12/13 22:38:22 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EID_INIT')
      PARAMETER (CRAUTH='Alexander Vlassov')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine EID_INIT:
c----6----------------------------------------------------------------72
c
      CRMESS=' EID_init was called !!!'
      CALL RECMES(CRNAME,'I',CRMESS)
c
c     Initialization of some constants for the
c           FAST ELECTRON IDENTIFICATION  (eid0)
c     ==========================================
c
      CCecthr =  150    !  EC deposited energy threshold (in MeV)
      CCscthr =   50    !  10 * SC deposited energy threshold (in MeV)
      CCscecy =   30    !  Possible difference in Y (Sector) coord.(cm)
      CCscecz =   15    !  Possible difference in Z (Sector) coord.(cm)
      CCicoor =   35    !  Possible deviation in I - coord in EC (cm)
      CCscpn  =   30    !  10 * Possible deviation in SC paddle #

c
      RETURN
      END
c
c-----------------------------------------------------------------------




