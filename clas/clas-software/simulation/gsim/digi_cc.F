      SUBROUTINE DIGI_CC
c
c_begin_doc
c  RCS ID string
c  $Id: digi_cc.F,v 1.2 1997/04/24 15:38:22 holtrop Exp $
c
c  Documentation for subroutine DIGI_CC
c
CA)========================================(begin of short description)
C    Digitization of the CC - counters
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods : make and store digi for CC :
C
C    Created:   25-May-1995   Alexander Vlassov
C-   Modified   21-AUG-1995   Alexander V. Vlassov
C
CC)-------------------------------------------(end of long description)
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
      PARAMETER (CFILE = '$RCSfile: digi_cc.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:22 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: digi_cc.F,v 1.2 1997/04/24 15:38:22 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='digi_cc')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c
c  Local User defined variables:
c  -----------------------------
CD)----------------------------------(declare common blocks, variables)
c_end_var
c
c  executable code for routine DIGI_CC
c------------------------------------------------------------------------------
      call ccdigi
      call ccout
      return
      end
