c      include file PHOTON
c
c_begin_doc
c
c  Documentation for include file PHOTON
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
c  Author:  B.Asavapibhop       Created:  Fri Apr 10 1998
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
c  variables for include file PHOTON:
c----6----------------------------------------------------------------72
c  
c  Variables contained in PHOTON common block
c  --------------------------------------------
          
      LOGICAL YES_PHOTON	! flag for photon run	
      REAL ZPHOTON(MaxParticles)
      
      REAL dist_radi2tgt
      PARAMETER (dist_radi2tgt=2198.)  ! distance from radiator to target(cm)
c_end_var
c
      COMMON/PHOTON_STUFF/ZPHOTON,YES_PHOTON
c----6----------------------------------------------------------------72




