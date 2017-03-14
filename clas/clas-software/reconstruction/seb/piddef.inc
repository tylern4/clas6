c      include file PIDDEF
c
c_begin_doc
c  RCS ID string
c  $Id: piddef.inc,v 1.13 2009/04/08 19:20:30 stepanya Exp $
c
c  Documentation for include file PIDDEF
c
c  Purpose:
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:   Stepan      Created:  Thu Apr  3 21:54:26 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file SC_INF:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIPIDDEF
      DATA CIPIDDEF/
     1'$Id: piddef.inc,v 1.13 2009/04/08 19:20:30 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in PIDDEF common block
c  --------------------------------------------
c
        REAL RF1SLOP             ! Slop parameters for RF TDC (1)
        REAL RF2SLOP             ! Slop parameters for RF TDC (2)
        REAL DTRF
        PARAMETER(DTRF=400.)
        REAL RF_struct
        PARAMETER(RF_struct=2.004)
        REAL RF_1G2
        REAL RF_1L2
        REAL OR_DIFF, RF_DELTA
        REAL RFCOR, gRF
        REAL RF_high1, RF_low1, RF_high2, RF_low2
        REAL RF1_high1, RF1_low1, RF1_high2, RF1_low2
        REAL RF2_high1, RF2_low1, RF2_high2, RF2_low2
        REAL TEL
        REAL F10,F11,F12,F20,F21,F22
        REAL RF1DL, RF1DH, RF2DL, RF2DH
        REAL RFpcorr(100)
C----------------------------------------------------------------
        REAL  RFG_low(4),RFG_HIGH(4)
        REAL  RFG_PARMS(4,4)
        INTEGER RF_STATUS
C----------------------------------------------------------------
        REAL beam_position(3),target_position(3)
C----------------------------------------------------------------
        COMMON/NEWRFFUNC/RFG_LOW,RFG_HIGH,RFG_PARMS,RF_STATUS,RFpcorr

c_end_var
c
      COMMON/PIDDEF/RF1SLOP,RF2SLOP,OR_DIFF,RFCOR,RF_DELTA,TEL,RF_1G2,RF_1L2,
     +RF_high1,RF_low1,RF_high2,RF_low2,RF1_high1,RF1_low1,RF1_high2,RF1_low2,
     +RF2_high1, RF2_low1, RF2_high2, RF2_low2,gRF,RF1DL,RF1DH,RF2DL,RF2DH,
     +F10,F11,F12,F20,F21,F22,beam_position,target_position
c----6----------------------------------------------------------------72




