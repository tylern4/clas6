      SUBROUTINE EC_INIT
c
c_begin_doc
c  RCS ID string
c  $Id: ec_init.F,v 1.3 1997/05/29 17:52:58 stepanya Exp $
c
c  Documentation for subroutine EC_INIT
c
c  Purpose:   initialize all 6 sectors for the egn code
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters: none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Arne Freyberger      Created:  Tue Nov 28 15:39:01 EST 1995
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
#include "ec_control.inc"
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
      PARAMETER (CFILE = '$RCSfile: ec_init.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1997/05/29 17:52:58 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/   
     1'$Id: ec_init.F,v 1.3 1997/05/29 17:52:58 stepanya Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC_INIT')
      PARAMETER (CRAUTH='Arne Freyberger')
c
c  Local User defined variables:
c  -----------------------------
c_end_var
c
c  executable code for routine EC_INIT:
c----6----------------------------------------------------------------72
c
c  register the package with the database
c
      CALL REVINI('ECRC',CREVIS,CDATE)
*
      CALL RECMES(CRNAME,'I','EGN package initialized-----')
      CALL RECMES(CRNAME,'N','|')
      WRITE(CRMESS,20)EcFitEdge_strip_threshold
20    FORMAT('| For this run EcFitEdge_strip_threshold=',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,21)EcFitEdge_peak_threshold
21    FORMAT('|              EcFitEdge_peak_threshold =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,22)EcFitEdge_hit_threshold
22    FORMAT('|              EcFitEdge_hit_threshold  =',F8.4)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,23)Def_Atten
23    FORMAT('|              Use default Attenuation Length = ',L1)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,31)Def_ADC
31    FORMAT('|              Use the default constans for ADC = ',L1)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,32)Def_TDC
32    FORMAT('|              Use the default constans for TDC = ',L1)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,34)ec_ln_weights
34    FORMAT('|              Use the logarithmic weights for peaks = ',L1)
      CALL RECMES(CRNAME,'N',CRMESS)
      IF ( anal_opt.EQ.1 ) THEN
        WRITE(CRMESS,24)
24    FORMAT('|              Standard analyses flow is used.')
      ELSE
        WRITE(CRMESS,25)
25    FORMAT('|              Modified analyses flow is used.')
      ENDIF
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,26)touch_id-1
26    FORMAT('|              Gap between followed strips in peak = ',I4)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,27)Def_geom
27    FORMAT('|              Use the Ideal Geometry Settings = ',L1)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,28)whole_surf
28    FORMAT('|              Main  Plane - Front Face of Layer=',F6.2)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,29)inner_surf
29    FORMAT('|              Inner Plane - Front Face of Layer=',F6.2)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,30)outer_surf
30    FORMAT('|              Outer Plane - Front Face of Layer=',F6.2)
      CALL RECMES(CRNAME,'N',CRMESS)
      WRITE(CRMESS,33)ec_match
33    FORMAT('|              EC hit match parameter =',F6.2)
      CALL RECMES(CRNAME,'N',CRMESS)
*
      CALL RECMES(CRNAME,'N','|')
      CALL RECMES(CRNAME,'I','---- EGN package initialized ----')
      RETURN
      END
c
c------------------------------------------------------------------------------




