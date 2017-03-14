CDECK  ID>, CCOUT.
      SUBROUTINE CCOUT
C     ================
c
c_begin_doc
c  RCS ID string
c  $Id: ccout.F,v 1.2 1997/04/24 15:38:10 holtrop Exp $
c
c  Documentation for subroutine CCOUT
c
CA)========================================(begin of short description)
C End event routine for Cherenkov counters
CB)-----------------------------------------(begin of long description)
C
C    Created:   28-Jun-1995   Alexander Vlassov
C-   Modified   18-AUG-1995   Alexander V. Vlassov
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
C
#include "gcflag.inc"
#include "ccph.inc"
C
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
      PARAMETER (CFILE = '$RCSfile: ccout.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:10 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: ccout.F,v 1.2 1997/04/24 15:38:10 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccout')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
c
c  Local User defined variables:
c  -----------------------------
        integer ISEED1,ISEED2
c_end_var
c
c  executable code for routine CCOUT
c----------------------------------------------------------------------------
c
      jtro = jtr
      igo = ig
      jtr = 0
      ig = 0
C
      if(mod(ievent,1000).eq.0) then
c
        call grndmq(ISEED1,ISEED2,0,'G')
        write(CRMESS,*) '***  Event  ***',ievent,'  ISEED1,ISEED2 :',
     &  ISEED1,ISEED2,'  ***'
        call recmes(CRNAME,'I',CRMESS)
c
      end if
C
      return
      END
