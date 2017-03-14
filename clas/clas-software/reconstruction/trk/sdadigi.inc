C Results of the DC digitalization
C                                                   Mod F.R. Nov.1996
C                                                   Mod C.A.Meyer 3.1997
C
C  dc_ndig(ilayer,isect) - number of hits (digits) in a given layer & sector
C  dc_digi(1,idx,ilayer,isect) - wire number
C  dc_digi(2,idx,ilayer,isect) - TDC
C
C---C.A.Meyer: added the following array to "point" to the
C---row in the DC0 bank that contains this hit.
C
C  pt_digi(idx,ilayer,isect) - Layer number in the DC0 bank
C
C  Ttrig   - Trigger Time [ns]
C
C ndx      - maximum number of hits in a given layer & sector
C
C nlay_max - maximum number of layers: DC  nlay_max = 36
C
C nsect  - maximum number of sectors:  nsect = 6
C
      INTEGER    ndx
      PARAMETER (ndx = 192)
C
      INTEGER dc_digi,dc_ndig
      REAL Ttrig
*
*--c.a.meyer march 1997
*
      INTEGER pt_digi
C
      COMMON/sdadigi/ dc_digi(2,ndx,36,6) , dc_ndig(36,6), 
     &                Ttrig , pt_digi(ndx,36,6)
*--
C
      save  /sdadigi/
C












