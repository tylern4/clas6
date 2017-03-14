c      include file CCECHB
c
c_begin_doc
c  RCS ID string
c  $Id: ccechb.inc,v 1.1.1.1 1997/03/28 20:49:50 marki Exp $
c
c  Documentation for include file CCECHB
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
c  Author:   Alexander Vlassov    Created:  Fri Aug  2 11:01:58 EDT 1996
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
c  variables for include file CCECHB:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICCECHB
      DATA CICCECHB/
     1'$Id: ccechb.inc,v 1.1.1.1 1997/03/28 20:49:50 marki Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in CCECHB common block
c  --------------------------------------------
c
      integer mectrk,   ! Max number of tracks
     &        jectrk,   ! Number of tracks in the event
     &        jecsec,   ! Sector # 
     &        jeclay    ! Layer  #
c
      real ec_e,      ! Enegry found
     &     ec_de,     ! Error on the energy found
     &     ec_time,   ! EC time found 
     &     ec_dtime,  ! Error on the time found
     &     ec_ihit ,  ! Sector i - coordinate
     &     ec_dihit,  ! Error on Sector i - coordinate
     &     ec_r ,     ! (x,y,z) of hit
     &     ec_dr      ! d(x,y,z) of hit

      parameter (  mectrk = 100)       !  Number of tracks
c
c_end_var
c
       COMMON/CCECHB/ jectrk,  jecsec(mectrk),   jeclay(mectrk),
     &          ec_e(mectrk),   ec_de(mectrk),  ec_time(mectrk),   
     &      ec_dtime(mectrk), ec_ihit(mectrk), ec_dihit(mectrk),
     &        ec_r(3,mectrk), ec_dr(3,mectrk)
c
c----6----------------------------------------------------------------72




