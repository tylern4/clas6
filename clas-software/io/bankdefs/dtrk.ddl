!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
 TABLE  DTRK     ! create write display delete ! Drift chamber tracks
                 ! CED display bank of drift chamber tracks
                 ! record number = sector# + 16*(track number) +
                 !                          512*(particle ID)
                 !
                 ! X,Y,Z are the XYZ positions of a track in the sector
                 ! coordinate system,  one (XYZ) coordinate per tracking
                 ! layer, plus one coordinate for the CC SC EC.
                 !
                 ! Each track gets its own DTRK bank (one bank/track)
!
!   ATTributes:
!   -----------
!COL ATT-name FMT Min    Max   ! Comments
!
  1  X        F   -999999.  999999. !
  2  Y        F   -999999.  999999. !
  3  Z        F   -999999.  999999. !
!
 END TABLE
!
