      SUBROUTINE TRK_2_ST(itr,npart_st,npart,sector)
c     
c     _begin_doc
c     
c     Documentation for subroutine TRK_2_ST
c     
c     Purpose:  Finding a hit that matches with given track.
c     --------  In case of success fills structure recorded as a STST.
c     
c     Input Parameters:  itr      - integer - treck number
c     ----------------   npart_st - integer - counter for STST records
c     npart    - integer - counter for particle record
c     sector   - treck sector number  
c     
c     Output Parameters: npart_st - integer - In case of success +1
c     -----------------
c     
c     Other routines:    
c     ---------------
c     
c     Notes:
c     ------
c     
c     Author:   B. Asavapibhop    Created:  Wed Jan 14 09:33:54 EST 1998
c     -------
c     
c     Major revisions:
c     ----------------
c     
c     
c     _end_doc
c     
      IMPLICIT NONE
      SAVE
c     
c     _begin_inc
c     include files :
c     ---------------------
#include "evnt_general.inc"
#include "evnt_par.inc"
#include "echits.inc"
#include "hbt_planes.inc"
#include "particle_out.inc"
#include "st_inf.inc"
#include "sthits.inc"
c     _end_inc
c     
c     _begin_var
c     input/output variables:
c     -----------------------
      INTEGER itr,sector,npart_st,npart
c     
c     Local pre-defined variables:
c     ---------------------------
c     RCS information: 

c     Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='TRK_2_ST')
      PARAMETER (CRAUTH='Burin Asavapibhop')
c     
c     Local User defined variables:
c     -----------------------------
      INTEGER NWRITE,IRUN,IEVNT,IREC
      INTEGER sthit, nps, nprt, i
      LOGICAL OK
c     _end_var
c     
c     executable code for routine TRK_2_ST:
c---- 6----------------------------------------------------------------72
c     
      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='This routine matches hits in ST with given track.'
         CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF
      
c     - Loop over ST hits in the sector which match tracks 

      OK=.false.
      sthit =  0
      DO sthit = 1, StFit_Nhits(sector)
         OK=.false.
         if(sttype.eq.0) then
            If (trackid(itr).eq.stfit_trkno(sthit,sector) )OK=.true.
         else
            OK=itr_sect(itr).eq.sector.and.
     &         trackid(itr).eq.mod(stfit_trkno(sthit,sector),100)
         endif
         If (OK) Then    
            npart_st = npart_st + 1
            partstst(npart+1) = npart_st
c     - Fill the ST information structure.
            stpart(npart_st)    = npart+1
            stihit(npart_st)    = sthit
            stsector(npart_st)  = sector
            str_trk(npart_st)   = stfit_length(sthit,sector)
            sttrkno(npart_st)   = stfit_trkno(sthit,sector)
            sttime(npart_st)    = StFit_time(sthit,sector)
            ststatus(npart_st)  = stfit_status(sthit,sector)
            vzpos(npart_st)     = trkp(3,1,itr)
            stid(npart_st)      = stfit_pd(sthit,sector)
         EndIf
         
      ENDDO

      RETURN
      END
c
c------------------------------------------------------------------------------




