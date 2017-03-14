      REAL FUNCTION modrf(XDUMMY)
      REAL Xdummy,
     +T_id ,TDC_L,TDC_R,E_id,TDC_E,RF,
     +TMEAN,TRF,PC       
*
      LOGICAL         CHAIN
      CHARACTER*128   CFILE
	integer decrement

*
      COMMON /PAWCHN/ CHAIN, NCHEVT, ICHEVT
      COMMON /PAWCHC/ CFILE
*
      COMMON/PAWIDN/IDNEVT,OBS(13),
     +T_id    ,TDC_L   ,TDC_R   ,E_id    ,TDC_E   ,RF,
     +TMEAN   ,TRF     ,PC       
*
      decrement = int( xdummy/2.004 )
      if (xdummy.lt.0) decrement=decrement -1
      
      modrf = xdummy - 2.004*decrement
      
      END
