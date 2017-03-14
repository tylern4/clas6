C======================================================================
      SUBROUTINE trk_delete(ntracks,nhbt,level)
*_begin_doc
*  RCS ID string
*  $Id: trk_delete.F,v 1.2 1999/03/31 16:01:48 fklein Exp $
*
*  Documentation for subroutine TRK_DELETE
*
*  Purpose: eliminate tracks that have become subsets of other tracks  
*  -------   
*
*  Input Parameters:
*  ----------------
*     nhbt=number of hit based tracks 
*     level=trk_level to be checked
*
*  Output Parameters:
*                     ntracks    = #tracks that survived the weedout procedure
*           
*  -----------------
*  (copy from TRK_DCAM.F (J.Mueller))
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files :
*  ---------------------
*
#include "bcs.inc"
#include "anapatt.inc"
#include "dc_tcl.inc"
#include "trktcl_var.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
      INTEGER   ntracks, nhbt
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_delete.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1999/03/31 16:01:48 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_delete.F,v 1.2 1999/03/31 16:01:48 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_DELETE')
      PARAMETER (CRAUTH='Simon Taylor')
*
*  Local User defined variables:
*  -----------------------------
      INTEGER nlay
      PARAMETER (nlay = 6)
      INTEGER i,k, il, ilmin,is,icl,isg,ilnk,isgc, la,lab,lamin
      INTEGER improve, nsgc, n_seg, isec,Level, itr_new,isgb
      INTEGER ilnk2,isec2,nshare,nsum,nsum2,icl2
      INTEGER time_hits(NSGMX),time_hits_max,ihit
      INTEGER j
      LOGICAL lsum,lsum2,lshare
      integer nsuper /6/
      
      ntr_link=nhbt
 
C
C Now loop over all the still good tracks and delete those which
C Have become subsets of others due to losing SL during ambiguity resolution
C (reset the level passed to HBT
C

      IF(ntracks.GT.1)THEN
       DO ilnk=1,nhbt-1
        IF(itr_level(ilnk).EQ.Level)THEN
	  isec = itr_sect(ilnk)
          DO ilnk2=ilnk+1,nhbt
	    isec2 = itr_sect(ilnk2)
            IF(isec.EQ.isec2.AND.itr_level(ilnk2).EQ.Level)THEN
              nshare = 0
              nsum   = 0
              nsum2  = 0
              DO is=1,nsuper

                 ilmin=(is-1)*nlay
                 lsum = .FALSE.
                 lsum2 = .FALSE.
                 lshare = .FALSE.
                 do la=1,nlay
                    il = la + ilmin
                    if(trk(1,il,ilnk).GT.0)then
                       lsum = .TRUE.
                       if(abs(trk(1,il,ilnk)-trk(1,il,ilnk2)).LT.3.)then
                          lshare = .TRUE.
                          lsum2 = .TRUE.
                       endif
                    endif
                    if(trk(1,il,ilnk2).GT.0)lsum2 = .TRUE.
                 enddo                       
                 if(lsum)nsum = nsum +1
                 if(lsum2)nsum2 = nsum2 +1
                 if(lshare)nshare = nshare +1
              ENDDO
              IF(nsum.GT.nsum2.AND.nsum2.eq.nshare)THEN
                itr_level(ilnk2) = level-1
              ELSEIF(nsum.LT.nsum2.AND.nsum.eq.nshare)THEN
                itr_level(ilnk)  = level-1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
       ENDDO
      ENDIF
C
C Now count the tracks again.
C
      ntracks = 0
      DO ilnk = 1,nhbt
        IF(itr_level(ilnk).EQ.Level)ntracks = ntracks + 1
      enddo

      RETURN
      END
