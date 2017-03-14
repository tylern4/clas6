c      include file CCTBTB
c
c_begin_doc
c  RCS ID string
c  $Id: cctbtb.inc,v 1.1 1998/05/08 20:28:06 vlassov Exp $
c
c  Documentation for include file CCTBTB
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
c  Author:  Alexander Vlassov Created:  Fri Aug  2 11:01:58 EDT 1996
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
c  variables for include file CCTBTB:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICCTBTB
      DATA CICCTBTB/
     1'$Id: cctbtb.inc,v 1.1 1998/05/08 20:28:06 vlassov Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in CCTBTB common block
c  --------------------------------------------
c
      integer mtbtrk,   ! Max number of tracks
     &        jtbtrk,   ! Number of tracks in the event
     &        jtbsec    ! Sector # 
c
      real tb_mom,      ! Particle momentum
     &     tb_charge,   ! Particle charge
     &     tb_vert,     ! Vertex point (x,y,z)  in SECTOR r.s.
     &     tb_ccl,      ! Lenght to CC "plane"
     &     tb_ccpnt,    ! CC matching point (x,y,z) in SECTOR r.s.
     &     tb_ccdir,    ! CC matching direction (x,y,z) in SECTOR r.s.
     &     tb_scl,      ! Lenght to SC "plane"
     &     tb_scpnt,    ! SC matching point (x,y,z) in SECTOR r.s.
     &     tb_scdir,    ! SC matching direction (x,y,z) in SECTOR r.s.
     &     tb_ecl,      ! Lenght to EC "plane"
     &     tb_ecpnt,    ! EC matching point (x,y,z) in SECTOR r.s.
     &     tb_ecdir     ! EC matching direction (x,y,z) in SECTOR r.s.

      parameter (  mtbtrk = 20)       !  Number of tracks
c
c_end_var
c
       COMMON/CCTBTB/ jtbtrk,  jtbsec(mtbtrk),   tb_mom(3,mtbtrk),
     &     tb_charge(mtbtrk),  tb_vert(3,mtbtrk),   
     &     tb_ccl(mtbtrk), tb_ccpnt(3,mtbtrk), tb_ccdir(3,mtbtrk),
     &     tb_scl(mtbtrk), tb_scpnt(3,mtbtrk), tb_scdir(3,mtbtrk),
     &     tb_ecl(mtbtrk), tb_ecpnt(3,mtbtrk), tb_ecdir(3,mtbtrk)
c
c----6----------------------------------------------------------------72




