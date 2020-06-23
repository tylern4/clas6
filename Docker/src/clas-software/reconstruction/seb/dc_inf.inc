c      include file DC_INF
c
c_begin_doc
c  RCS ID string
c  $Id: dc_inf.inc,v 1.4 1997/06/05 19:46:33 stepanya Exp $
c
c  Documentation for include file DC_INF
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
c  Author:         Created:  Thu Apr  3 21:54:15 EST 1997
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
c  variables for include file DC_INF:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIDC_INF
      DATA CIDC_INF/
     1'$Id: dc_inf.inc,v 1.4 1997/06/05 19:46:33 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in DC_INF common block
c  --------------------------------------------
c
        REAL DCP          ! Reconstructed track momentum (GeV/c)
        REAL DCM          ! Reconstructed mass after TBT
        INTEGER DCQ       ! Reconstructed track charge (+1,0,-1 )
        REAL DCCX         ! X direction cosine at the origin
        REAL DCCY         ! Y direction cosine at the origin
        REAL DCCZ         ! Z direction cosine at the origin
        REAL DCVERTX      ! X coordinate of vertex (cm)
        REAL DCVERTY      ! Y coordinate of vertex (cm)
        REAL DCVERTZ      ! Z coordinate of vertex (cm)
        REAL DCCHI2       ! Chisquare of track fitting
        INTEGER DCPART    ! Particle row index for DC track 
        INTEGER DCSECTOR  ! DC track sector
        INTEGER DCITRK    ! DC track ID
        INTEGER  DCSTATUS ! Status word (Not yet defined)
c_end_var
c
      COMMON/DC_INF/DCP(MaxParticles),DCM(MaxParticles),DCCX(MaxParticles),
     &DCCY(MaxParticles),DCCZ(MaxParticles),DCVERTX(MaxParticles),
     &DCVERTY(MaxParticles),DCVERTZ(MaxParticles),DCCHI2(MaxParticles),
     &DCQ(MaxParticles),DCSECTOR(MaxParticles),DCITRK(MaxParticles),
     &DCPART(MaxParticles),DCSTATUS(MaxParticles)
c----6----------------------------------------------------------------72




