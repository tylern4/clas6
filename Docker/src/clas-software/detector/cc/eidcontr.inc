c      include file EIDCONTR
c
c_begin_doc
c  RCS ID string
c  $Id: eidcontr.inc,v 1.2 2000/12/13 22:38:22 fklein Exp $
c
c  Documentation for include file EIDCONTR
c
c  Purpose:  To store control variables (tcl variables for eid0)
c  --------
c
c  Author:   Alexander Vlassov   Created:  Fri Nov  1 14:23:48 EST 1996
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c_begin_var
c  variables for include file EIDCONTR:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIEIDCONTR
      DATA CIEIDCONTR/
     1'$Id: eidcontr.inc,v 1.2 2000/12/13 22:38:22 fklein Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in EIDCONTR common block
c  --------------------------------------------
c
      integer maxeid0
      parameter (maxeid0 = 20)
c
      integer CCecthr,   !  EC deposited energy threshold (in MeV)
     &        CCscthr,   !  SC deposited energy threshold (in 0.1 MeV)
     &        CCscecy,   !  Possible deviation in Y (Sector) coord.
c                        !  in EC and SC detectors (cm)
     &        CCscecz,   !  Possible deviation in Y (Sector) coord.
c                        !  in EC and SC detectors (cm)
     &        CCicoor,   !  Possible deviation in I - coord in EC (cm)
     &        CCscpn     !  10 * Possible deviation in SC paddle #
c
      integer jeidsec,   !  Sector # for electron candidate
     &        jCC_eid,   !  Pointer to the hit in CCRC bank
     &        jEC_eid,   !  Pointer to the hit in ECHB bank
     &        jSC_eid    !  Pointer to the hit in SCRX bank
c_end_var
c
       COMMON /EIDCONTR/ CCecthr, CCscthr, CCscecy, CCscecz, CCicoor, CCscpn
       COMMON /CCEID0/  jeidsec(maxeid0), jCC_eid(maxeid0),
     &  jEC_eid(maxeid0), jSC_eid(maxeid0)   
c----6----------------------------------------------------------------72




