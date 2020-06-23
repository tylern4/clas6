      subroutine get_lac_plane
*
      IMPLICIT NONE
      
      SAVE
      include "ntpl_com.inc"

*  local variablies

      integer j
      integer track_num,plane_num,sec_pln

      ntrk_lac = 0
      DO j=1,2
        x_lac(j) = 0.
        y_lac(j) = 0.
        z_lac(j) = 0.
        lac_track(j) = 0
      END DO
      DO j=1,nHDPL
        track_num = TRK_PLN_HDPL(j)/100
        plane_num = TRK_PLN_HDPL(j) - track_num*100
        if(plane_num .eq. 46 .and.  ! plane=46 corrisponds to LAC
     &     TLEN_HDPL(j).ne.0) then  ! lenght of the track <> 0
                                    ! get track's intersection with LAC
          sec_pln = sectHDPL (j)
          if (sec_pln .eq.1 .or. sec_pln .eq. 2) then
            ntrk_lac = ntrk_lac+1
c  remember track number comming to LAC
            lac_track(sec_pln) = track_num
            x_lac(sec_pln) = X_HDPL(j)
            y_lac(sec_pln) = Y_HDPL(j)
            z_lac(sec_pln) = Z_HDPL(j)
            trk_len(sec_pln) = TLEN_HDPL(j)
          end if
        end if
      END DO
      return
      end
      
