      SUBROUTINE TAG_SCALER
c
c_begin_doc
c  RCS ID string
c  $Id: tag_scaler.F,v 1.6 2008/03/16 04:33:24 fklein Exp $
c
c  Documentation for subroutine TAG_SCALER
c
c  Purpose:  adds the tgs bos bank input to the RTSL, G1SL, G2SL, G3SL, G4SL BOS bank
c  --------
c		This rerouting takes into account the hardware wiring of scalers and 
c		gates. Each bank can have a different gate. inside each bank the gating
c		is the same. one over 2 channel in each bank is unused, don't
c		worry, it is done on purpose.
c
c  Input Parameters:  NONE
c  ----------------
c
c  Output Parameters: NONE
c  -----------------
c
c  Called from:
c  ------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Eric Anciant	created feb,16th 1998
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "wcs.inc"
#include "tag_general.PAR"
#include "tagtcl.CMN"
#include "tagwarnings.CMN"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID 
      PARAMETER ( CFILE = '$RCSfile: tag_scaler.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.6 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/03/16 04:33:24 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: tag_scaler.F,v 1.6 2008/03/16 04:33:24 fklein Exp $
     3'/
c  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      PARAMETER (CRNAME='TAG_SCALER')
      PARAMETER (CRAUTH='Eric Anciant')
c
c  Local Tag defined variables:
c  ---------------------------
       integer nwrite,size,ind,ind1,i
       integer MLINK
       character*132 crmess
c_end_var
c
c  executable code for routine TAG_SCALER:
c  -------------------------------------
c
       crmess = '  '
c
c      we skip the 2 first scaler events. They have no TGS bank in it anyway
c
       IF (NWRITE .LT. 2) THEN
         NWRITE = NWRITE + 1
c         crmess = 'processing of the first SCALER EVENT '
c         CALL RECMES(CRNAME,'I',CRMESS)
         return
       ENDIF

c
c ---- Process scaler
c
       ind=MLINK(iw,ScalerData_BOS_name,0)

       if (ind.eq.0) then
         wits = wits + 1
         if (mod(wits-1,Tagger_warning).eq.0) then
           write(crmess,10) wits
10         format ('TGS data corrupted or absent occured ',I5,' times')
           CALL RECMES(CRNAME,'W',CRMESS)
         endif
         return
       endif
c
c      TGS bank contains so far 208 numbers. only 192 of them are used by the photon normalisation
c      here we just expect to have at least the required number such that we won't have to deal with
c      changes in the near future  
c
       size = IW(ind)
       if(size.lt.ScalerData_BOS_size) then
         CRMESS= 'TGS BOS bank does not contain all the required channels - Skipping'
         call RECMES(CRNAME,'W',CRMESS)
         return
       endif

c
c      adds to the RTSL BOS bank the ungated T counters
c
       ind1 = MLINK(JW,'RTSL',0)
       if(ind1.EQ.0) then
          call recmes(crname,'F','Begin Run routines were not called!!!')
       endif

       do i=1,64
         JW(ind1+i) = JW(ind1+i) + IW(ind+i)
       enddo
c
c      adds to the G1SL BOS bank the set 1 odd gated T counters
c      Remember that scalers 65-80 and 97-112 are odd T-counters
c
       ind1 = MLINK(JW,'G1SL',0)

       do i=1,16
         JW(ind1+2*i-1) = JW(ind1+2*i-1) + IW(ind+i+64)
         JW(ind1+2*i-1+32) = JW(ind1+2*i-1+32) + IW(ind+i+96)
       enddo
c
c      adds to the G2SL BOS bank the set 1 even gated T counters
c      Remember that scalers 81-96 and 113-128 are even T-counters
c
       ind1 = MLINK(JW,'G2SL',0)

       do i=1,16
         JW(ind1+2*i) = JW(ind1+2*i) + IW(ind+i+80)
         JW(ind1+2*i+32) = JW(ind1+2*i+32) + IW(ind+i+112)
       enddo
c
c      adds to the G3SL BOS bank the set 2 odd gated T counters
c      Remember that 129-144 and 161-176 are odd T-counters
c
       ind1 = MLINK(JW,'G3SL',0)

       do i=1,16
         JW(ind1+2*i-1) = JW(ind1+2*i-1) + IW(ind+i+128)
         JW(ind1+2*i-1+32) = JW(ind1+2*i-1+32) + IW(ind+i+160)
       enddo
c
c      adds to the G4SL BOS bank the set 2 even gated T counters
c      Remember that 145-160 and 177-192 are even T-counters
c
       ind1 = MLINK(JW,'G4SL',0)

       do i=1,16
         JW(ind1+2*i) = JW(ind1+2*i) + IW(ind+i+144)
         JW(ind1+2*i+32) = JW(ind1+2*i+32) + IW(ind+i+176)
       enddo
c
     

       RETURN
       END
c
c----------------------------------------------------------------------





