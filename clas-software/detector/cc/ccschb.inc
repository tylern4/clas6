c      include file CCSCHB
c
c_begin_doc
c  RCS ID string 
c  $Id: ccschb.inc,v 1.3 1998/04/17 19:35:30 vlassov Exp $
c
c  Documentation for include file CCSCHB
c
c  Purpose:  Stored SC info from SCRX bank
c  --------
c
c  Author:   Alexander Vlassov    Created:  Fri Aug 16 10:29:59 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c_begin_var
c  variables for include file CCSCHB:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICCSCHB
      DATA CICCSCHB/
     1'$Id: ccschb.inc,v 1.3 1998/04/17 19:35:30 vlassov Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in CCSCHB common block
c  --------------------------------------------
c
c
      integer msctrk,   ! Max number of tracks (pairs)
     &        jsctrk,   ! Number of tracks (pairs) in the event
     &        jscsec,   ! Sector # 
     &        jscpad,   ! Paddle  #
     &        jscstat   ! Status word
c
      real sc_e,      ! Enegry found
     &     sc_de,     ! Error on the energy found
     &     sc_time,   ! SC time found 
     &     sc_dtime,  ! Error on the time found
     &     sc_r ,     ! (x,y,z) of hit
     &     sc_dr      ! d(x,y,z) of hit
      parameter (  msctrk = 50)       !  Number of tracks
c
c_end_var
c
       COMMON/CCSCHB/ jsctrk,  jscsec(msctrk),   jscpad(msctrk),
     &      jscstat(msctrk),
     &          sc_e(msctrk),   sc_de(msctrk),  sc_time(msctrk),   
     &      sc_dtime(msctrk),  sc_r(3,msctrk),  sc_dr(3,msctrk)
c
c----6----------------------------------------------------------------72




