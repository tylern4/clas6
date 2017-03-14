      SUBROUTINE INIT_CC
c     ==================
c
c_begin_doc
c  RCS ID string
c  $Id: init_cc.F,v 1.4 2004/02/12 21:54:23 vlassov Exp $
c
c  Documentation for subroutine INIT_CC
c
CA)========================================(begin of short description)
C    Initialization of the Cherenkov detector package
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods :  Initialize some variables
c                           Reading calibration constants
c                           and filling  common /CCCALB/ (cccom.inc)
C    Created  25-MAY-1995  Alexander Vlassov
C-   Modified   21-AUG-1995   Alexander V. Vlassov
C-   Modified   12-MAR-2003   Alexander V. Vlassov 
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
#include "ffpar_cc.inc"
#include "gsimpar.inc"
#include "ccph.inc"
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
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs
c                                                                        
c  RCS information:                                                      
c                                                                        
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: init_cc.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2004/02/12 21:54:23 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/ 
     1'$Id: init_cc.F,v 1.4 2004/02/12 21:54:23 vlassov Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='init_cc')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                        
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs
c
CD)----------------------------------(declare common blocks, variables)
C
c
c  Local User defined variables:
c  -----------------------------
c
      character*80 filename
      INTEGER LUNC,ires,isect,i,IRUN
      data IRUN /1/ ! GEANT simulations only !
c
c_end_var
c
c  executable code for routine INIT_CC
c------------------------------------------------------------------------
C
      jtr = 0
      ig = 0
c
c     Reading calibration constants. (From a database now)
c
      call cc_read_map(IRUN, opheamp,'CC_CALIB','photoE','amplitude')
      call cc_read_map(irun, ccpedes,'CC_CALIB','pedestals','mean')
      call cc_read_map(irun, acctime,'CC_CALIB','TDC','T0')
      call cc_read_map(irun, xcct2ch,'CC_CALIB','TDC','T1')
c
c     ========>  END of file reading cycle
c
      write(CRMESS,*) ' CC calibration data have been readed'
      call recmes(CRNAME,'I',CRMESS)
c
      return      
      end
