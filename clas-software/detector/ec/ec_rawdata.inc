***************************** ec_rawdata.inc ****************************
*
*     Used for EC calibration
*
*     W. Brooks, August 1998

*     This include file defines common blocks used to store the raw data
*     after pedestal subtraction.

*     The index to these arrays is an encoded version of sector (1-6), 
*     layer (1-6) and strip (1-36). The encoding/decoding is provided by 
*     the subroutine "ec_tube_id".
*     

      real ec_raw_adcs(maxstrips,ec_u:ec_w,whole:cover,maxsectors)
      real ec_raw_tdcs(maxstrips,ec_u:ec_w,whole:cover,maxsectors)

      common / ec_raw / ec_raw_adcs, ec_raw_tdcs

*****************************************************************************
