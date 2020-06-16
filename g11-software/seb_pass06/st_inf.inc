c      include file ST_INF
c
c_begin_doc
c
c  Documentation for include file ST_INF
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
c  Author:  B. Asavapibhop        Created:  Wed Jan 14  EST 1997
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
c  variables for include file ST_INF:
c  -----------------------------------
c
c----6----------------------------------------------------------------72
c  
c  Variables contained in ST_INF common block
c  --------------------------------------------
c
        REAL STTIME             ! Time of ST hit (ns)
        REAL STR_TRK            ! Trek lenght to the ST plane (cm)
        REAL VZPOS		! vertex positon along z axis
        INTEGER STTrkno		! track number from HBTR
        INTEGER STSECTOR        ! ST hit sector
        INTEGER STIHIT          ! ST hit ID
        INTEGER STPART          ! Particle row number for ST hit
        INTEGER STSTATUS        ! Status word 
c				!  1 = 1 track in each pair
c				!  2 = 2 tracks in different side of given pair
c				!  3 = 2 tracks in same side of given pair
	INTEGER STID		! ST hit paddle
c_end_var
c
      COMMON/ST_INF/STTIME(MaxParticles),
     &STR_TRK(MaxParticles),STTrkno(MaxParticles),STSECTOR(MaxParticles),
     &STIHIT(MaxParticles),STPART(MaxParticles),STSTATUS(MaxParticles),
     &VZPOS(MaxParticles),STID(MaxParticles)
    
c----6----------------------------------------------------------------72




