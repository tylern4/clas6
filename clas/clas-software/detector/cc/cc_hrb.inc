c      include file CC_HRB
c
c_begin_doc
c  RCS ID string
c  $Id: cc_hrb.inc,v 1.2 1998/04/17 19:35:28 vlassov Exp $
c
c  Documentation for include file CC_HRB
c
c  Purpose:  Stored information fron CCH bank
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:   Alexander Vlassov    Created:  Mon Jul  1 13:15:54 EDT 1996
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c_begin_var
c  variables for include file CC_HRB:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICC_HRB
      DATA CICC_HRB/
     1'$Id: cc_hrb.inc,v 1.2 1998/04/17 19:35:28 vlassov Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in CC_HRB common block
c  --------------------------------------------
c
      integer mhtrk,   ! Max number of tracks
     &        jhtrk,   ! Number of tracks in the event
     &     itrk_num,   ! # track in the event (1000*istak + itrack ) 
     &      idh_prt,   ! Particle ID
     &      ihitrk,    ! Number of photoelectrons produced by track
     &      itrsect,   ! Sector #
     &      itrsegm    ! Segment #
c
      real xenter,     ! Track entering point coordinates in Sector RS
     &      xexit,     ! Track exit point coordinates in Sector system
     &     ph_tot      ! Particle momentum

      parameter ( mhtrk  = 50)       !  Number of tracks
      integer mbos_cch
      parameter (mbos_cch = 5000 )   !  Max number of words in hits
c
c ======================================================================
c
      common /CCHIT/ jhtrk, itrk_num(mhtrk), idh_prt(mhtrk),
     &                ihitrk(mhtrk), itrsect(mhtrk), itrsegm(mhtrk),
     &                ph_tot(mhtrk), xenter(3,mhtrk), xexit(3,mhtrk)
c
c ======================================================================
      integer jhit_cc
      real  xhit_cc(mbos_cch)
      equivalence(xhit_cc,jhit_cc)
c
      common /CCJHIT/jhit_cc(mbos_cch)
c ======================================================================
