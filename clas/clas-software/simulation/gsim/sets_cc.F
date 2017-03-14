      SUBROUTINE SETS_CC
C     ==================
c
c_begin_doc
c  RCS ID string
c  $Id: sets_cc.F,v 1.2 1997/04/24 15:38:52 holtrop Exp $
c
c  Documentation for subroutine SETS_CC
c
CA)========================================(begin of short description)
c    Define HITS and DIGI for cc - package
CB)-----------------------------------------(begin of long description)
C
C
C    Side effects (other variables/banks modified, etc.)
C
C    Created:   10-OCT-1993   M.Kossov
C    Modified:  02-MAY-1994   A.Vlassov
c    Modified:  19-AUG-1994   A.Vlassov
c    Modified:  25-MAY-1995   Alexander V. Vlassov
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
#include "lcgeom.inc"
#include "gcsets.inc"
#include "cccom.inc"
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
      PARAMETER (CFILE = '$RCSfile: sets_cc.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.2 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/04/24 15:38:52 $')
      PARAMETER (CAUTHO = '$Author: holtrop $')
      DATA CRCSID/ 
     1'$Id: sets_cc.F,v 1.2 1997/04/24 15:38:52 holtrop Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='sets_cc')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
CD)----------------------------------(declare common blocks, variables)
c
c  Local User defined variables:
c  -----------------------------
C
      integer idtype_cc,get_idtype
C---- Working arrays ---------------------------------------------------
C
      INTEGER  nbitsh(5),nbitsd(3)
      REAL orig(5),fact(5)
c
      CHARACTER*4 namesh(5),namesd(3)
C
      DATA namesh/'Xhit','Yhit','Zhit','CCNB','Time'/
      DATA nbitsh/  5*32 /
      DATA namesd /'CCSN','IADC','ITDC'/
      DATA nbitsd /  16  ,  16  ,  16  /
      DATA orig  / 3*500.0, 0., 0. /
      DATA fact  / 3*10., 1.0, 1000. /
C
c_end_var
c
c  executable code for routine SETS_CC
c------------------------------------------------------------------------------
C define hits and digitization
C
      write(CRMESS,*) ' BEGINNING of sets_cc REACHED'
      call recmes(CRNAME,'I',CRMESS)
C

C>>>>>>>>>>>>>>>>>>>>>>>>>>
      idtype_cc = get_idtype()
      CALL GSDETV('CC  ','CCVO',idtype_cc,500,100,ISET,IDET)
      CALL GSDETH('CC  ','CCVO',5,namesh,nbitsh,orig,fact)
      CALL GSDETD('CC  ','CCVO',3,namesd,nbitsd)
C>>>>>>>>>>>>>>>>>>>>>>>>>>
C
C  Printing of sets parameters.
*      CALL GPSETS('CC  ','CCVO')
C>>>>>>>>>>>>>>>>>>>>>>>>>>
C
      return
      end
