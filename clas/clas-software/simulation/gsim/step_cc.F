      SUBROUTINE STEP_CC
c     ==================

c_begin_doc
c  RCS ID string
c  $Id: step_cc.F,v 1.2 1997/04/24 15:38:54 holtrop Exp $

c  Documentation for subroutine STEP_CC

CA)========================================(begin of short description)
C     defines hits for CC - counters
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods : make and store hits for CC :
C     This is a dummy routine, it simply calls CCSTEP
C
C    Created:   25-May-1995   Alexander V. Vlassov
C-   Modified   21-AUG-1995   Alexander V. Vlassov
C
C----------------------------------------------------------------------

c_end_doc

      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
c_end_inc

c_begin_var
c  input/output variables:
c  -----------------------

c  Local pre-defined variables:
c  ---------------------------
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c  RCS information:                                                       rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: step_cc.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:54 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: step_cc.F,v 1.2 1997/04/24 15:38:54 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      PARAMETER (CRNAME='step_cc')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs

c  Local User defined variables:
c  -----------------------------
CC)-------------------------------------------(end of long description)
c_end_var

c  executable code for routine STEP_CC
c------------------------------------------------------------------------------

      if(nohits(cc))return

      call ccstep

      return
      end
