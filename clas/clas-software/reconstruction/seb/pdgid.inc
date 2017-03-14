c      include file PDGID
c
c_begin_doc
c  RCS ID string
c  $Id: pdgid.inc,v 1.5 1998/04/30 20:21:23 stepanya Exp $
c
c  Documentation for include file PDGID
c
c  Purpose: To keep PDG ID's and masses
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:   S. Stepanyan      Created:  Sun Mar 23 19:39:29 EST 1997
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
c  variables for include file PDGID:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIPDGID
      DATA CIPDGID/
     1'$Id: pdgid.inc,v 1.5 1998/04/30 20:21:23 stepanya Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in PDGID common block
c  --------------------------------------------
c
c_end_var
c
c Particle Data Group ID's
      INTEGER p_id(10) 
      INTEGER electron, positron, photon 
      INTEGER pip, pim
      INTEGER kp, km
      INTEGER proton, neutron 
      INTEGER deuteron, hel3, hel4
      PARAMETER (electron=11)
      PARAMETER (positron=-11)
      PARAMETER (photon=22)
      PARAMETER (pip=211)
      PARAMETER (pim=-211)
      PARAMETER (kp=321)
      PARAMETER (km=-321)
      PARAMETER (proton=2212)
      PARAMETER (neutron=2112)
      PARAMETER (deuteron=45)  ! Note - this is a GEANT ID's
      PARAMETER (hel3=49)  ! Note - this is a GEANT ID's
      PARAMETER (hel4=47)  ! Note - this is a GEANT ID's
      DATA p_id/photon,positron,pip,kp,proton,neutron
     &,deuteron,hel3,hel4,0/
c
c- Masses of particles
      REAL p_mass(10)
      REAL melectron, mphoton
      REAL mpion
      REAL mkaon
      REAL mproton,mneutron
      REAL mdeuteron,mhel3,mhel4
      PARAMETER (melectron=0.0005)
      PARAMETER (mphoton=0.0)
      PARAMETER (mpion=0.1396)
      PARAMETER (mkaon=0.494)
      PARAMETER (mproton=0.9383)
      PARAMETER (mneutron=0.9396)
      PARAMETER (mdeuteron=1.8756)  
      PARAMETER (mhel3=2.8092)  
      PARAMETER (mhel4=3.7274) 
      DATA p_mass/mphoton,melectron,mpion,mkaon,mproton,mneutron
     &,mdeuteron,mhel3,mhel4,0./
c----6----------------------------------------------------------------72




