	SUBROUTINE BM_READ_TRIG
c
c_begin_doc
c  RCS ID string
c  $Id: bm_read_trig.F,v 1.13 2002/03/11 21:59:06 hovanes Exp $
c
c  Documentation for subroutine BM_READ_TRIG
c
c  Purpose:  Read bem pars from TRGS bank processing routine  --------
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Hovanes Egiyan      Created:  Fri Sep 26 14:01:23 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
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
#include "bcs.inc"
#include "wcs.inc"
#include "clas_offsets.inc"
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
      PARAMETER ( CFILE = '$RCSfile: bm_read_trig.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.13 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2002/03/11 21:59:06 $')
      PARAMETER ( CAUTHO = '$Author: hovanes $')
      DATA CRCSID/
     1'$Id: bm_read_trig.F,v 1.13 2002/03/11 21:59:06 hovanes Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='BM_READ_TRIG')
      PARAMETER (CRAUTH='Hovanes Egiyan')
c
C-
#include "bm_pars.inc"
C-
      integer mamind, mbank
      external mamind, mbank

      integer NamI, Ind_trgs, IndB, IndH
      integer indx
      integer Run_Numb
C-
C-  Number of counts per coulomb for FCUP and frequency of the CLOCK
C-
      real FCUP_CONST, CLOCK_CONST, ELEC_CHARGE
      data FCUP_CONST / 1.0E+10 /
      real bm_get_fcup_scale
c      parameter ( FCUP_CONST = 1.0E+10 )
      parameter ( CLOCK_CONST = 1.0E+04 )
      parameter ( ELEC_CHARGE = 1.60217733E-19 )
C-    
      logical  lfirst
      data  lfirst /.TRUE./

      integer 	CLOCK_UG_1_0, CLOCK_G2_1_0 
	data CLOCK_UG_1_0 / 0 /
	data CLOCK_G2_1_0 / 0 /
      real Q_TOT_0, Q_TOT_LV_0 
      	data Q_TOT_0 / 0. /
      	data Q_TOT_LV_0 / 0. /
      real TIME, TIME_0 
      	data TIME_0 / 0. /  
C-
      character*100 CRMESS  
C-
c_end_var
c
c  executable code for routine BM_READ_TRIG:
c  -------------------------------------
c
C-
C-  Check for TRGS bank and get the name index
C-
      NamI = MamInd(IW,'TRGS')
      Ind_trgs = IW(NamI)
C-      
      NamI = MamInd(IW,'HEAD')
      IndH = IW(NamI)
C-
      if (lfirst) then
         ix = 0
         ispevt = 0
	 if ( IndH .NE. 0 ) then  
	    Run_Numb =   IW( IndH + HEAD_NRUN )
	    FCUP_CONST = bm_get_fcup_scale( Run_Numb ) * 1.0E+09 
	 else
	    CRMESS = 'Could not open HEAD bank : CONTINUE'
	    call RECMES( CRNAME, 'W', CRMESS )
	    return	    
	 endif
         lfirst = .FALSE.
      endif
         

      if ( Ind_trgs .NE. 0 ) then
C-
C-
C-  Create a new bank BMPR
C-
       IndB = MBANK(JW,'BMPR', 0, 4, 1)
       if ( IndB .EQ. 0) then
        CRMESS = 'Could not open BMPR bank : CONTINUE'
        call RECMES( CRNAME, 'W', CRMESS )
	return
       endif      
C-
C--  Special Event with TRGS bank
C-       
       if ( ix .GE. ixMax ) then
        ix = 1
        Loop = .TRUE.
       else
        ix = ix+1
       endif       
C-
       CLOCK_UG_1  =   IW(Ind_trgs +  TRGS_CLOCK_UG)
       FCUP_UG_1   =   IW(Ind_trgs +  TRGS_FCUP_UG)
       MASTER_OR   =   IW(Ind_trgs +  TRGS_MOR_PS)
       PC          =   IW(Ind_trgs + TRGS_PC)
       PS          =   IW(Ind_trgs + TRGS_PS)
       TAC         =   IW(Ind_trgs + TRGS_TAC)       
       CLOCK_UG_2  =   IW(Ind_trgs + TRGS_CLOCK_UG_2)
       FCUP_UG_2   =   IW(Ind_trgs + TRGS_FCUP_UG_2)
C-
       CLOCK_G1_1  =   IW(Ind_trgs + TRGS_CLOCK_G1)
       FCUP_G1_1   =   IW(Ind_trgs + TRGS_FCUP_G1)
       CLOCK_G1_2  =   IW(Ind_trgs + TRGS_CLOCK_G1_2)
       FCUP_G1_2   =   IW(Ind_trgs + TRGS_FCUP_G1_2)
C              
       CLOCK_G2_1  =   IW(Ind_trgs + TRGS_CLOCK_G2)
       FCUP_G2_1   =   IW(Ind_trgs + TRGS_FCUP_G2)  
       CLOCK_G2_2  =   IW(Ind_trgs + TRGS_CLOCK_G2_2)
       FCUP_G2_2   =   IW(Ind_trgs + TRGS_FCUP_G2_2)  
       do indx=1, 12
	TRIG_SCALER_UG(indx) = IW(Ind_trgs+TRGS_TRIG1_UG+indx-1)
       enddo
       TRIG_OR_UG  =   IW(Ind_trgs + TRGS_TRIG_OR_UG) 
       
C-
       Q_TOT(ix) = Float(FCUP_UG_1) / FCUP_CONST
       Q_TOT_LV(ix) = Float(FCUP_G2_1) / FCUP_CONST
C-      TL_TU(ix) = Float( CLOCK_G2_1 ) / CLOCK_UG_1
       TIME = Float(CLOCK_UG_1) / CLOCK_CONST
c       TIME = IW(IndH+4)
C-       
C-  Check for decrease in Time and Charge (Reset)
C-
       if ( Q_TOT(ix) .LT. Q_TOT_0 .OR.
     |      TIME .LT. TIME_0 ) then
        CRMESS= ' TOTAL CHARGE OR TIME DECREASED WITH TIME : CONTINUE '
        call RECMES( CRNAME, 'W', CRMESS )
       endif
C-
       if ( iSpEvt. GT. 0 )  then 
	if ( ( TIME - TIME_0 ) .GT. 1.0E-10 ) then 
         CURRENT(ix) = ( Q_TOT(ix)-Q_TOT_0 ) / ( TIME - TIME_0 )
         TL_TU(ix) = Float( CLOCK_G2_1 - CLOCK_G2_1_0 ) /
     &               Float( CLOCK_UG_1 - CLOCK_UG_1_0 ) 
	else
       	 CRMESS = ' Ungated clock not updating '
       	 call RECMES( CRNAME, 'W', CRMESS )	
         CURRENT(ix) = 0.
         TL_TU(ix) = 0. 	 
        endif
       else
        Loop = .FALSE.
        CURRENT(ix) = 0.
        TL_TU(ix) = 0. 
       endif
C-
       RJW(IndB+BMPR_Q_TOT) =  Q_TOT(ix)           
       RJW(IndB+BMPR_Q_TOT_LV) =  Q_TOT_LV(ix)
       RJW(IndB+BMPR_TL_TU) =  TL_TU(ix) 
       RJW(IndB+BMPR_CURRENT) =  CURRENT(ix)
C-
c       write(6,*)'Second trgs',indb, Q_TOT(ix), Q_TOT_LV(ix), TL_TU(ix), CURRENT(ix)    
c       write(6,*) RJW(IndB+BMPR_Q_TOT),RJW(IndB+BMPR_Q_TOT_LV), RJW(IndB+BMPR_CURRENT) 
C-
	CLOCK_G2_1_0 = CLOCK_G2_1
	CLOCK_UG_1_0 = CLOCK_UG_1
       	TIME_0 = TIME
       	Q_TOT_0 = Q_TOT(ix)
       	Q_TOT_LV_0 = Q_TOT_LV(ix)
C-
       	iSpEvt = iSpEvt + 1
C-   
       	if (iSpEvt .LE. 5 ) then    
       	 CRMESS = 'Beam parameters read'
         call RECMES( CRNAME, 'I', CRMESS )
        endif   
C-
      else
C-
C-   Bank TRGS not found (might not be a type 0 event)
C-      
       CRMESS = ' TRGS bank not found :  Skipping'
       call RECMES( CRNAME, 'W', CRMESS )
C-       
      endif
C-
      RETURN
      END
c
c----------------------------------------------------------------------




