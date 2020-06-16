!-----------------------------------------------------------------------
!    BANKname   BANKtype                    ! Comments
 TABLE  SCPS  ! create write display delete ! Sc plane geometry in SS
!
!   ATTributes:
!   -----------
!COL ATT-name  FMT  Min      Max        ! Comments
!---------------------------------------------------
1   ID          I    0       0xFFFF     ! Plane #(1-6):1-23,24-34,35-39,40-42,43-46,47-48
2   FIRSTPNUM   I    0       0xFFFF     ! The first paddle number in the plane
3   LASTPNUM    I    0       0xFFFF     ! The last paddle number in the plane
4   DISTANCE    F  -999999.9 999999.9   ! distance to the center of the plane
5   BEGALONSHFT F  -999999.9 999999.9   ! X-shift of Begin Plane poin along id of SC
6   ENDALONSHFT F  -999999.9 999999.9   ! X-shift of End of Plane poin along id of SC
7   LENGTHSHFT  F  -999999.9 999999.9   ! Y-shift of Plane SYS along the SC length
8   ALONGIDX   F  -999999.9 999999.9   ! XP.x direction for width in Sector System
9   ALONGIDY   F  -999999.9 999999.9   ! XP.y direction for width in Sector System
10  ALONGIDZ   F  -999999.9 999999.9   ! XP.z direction for width in Sector System
11  SLENGTHX   F  -999999.9 999999.9   ! YP.x direction for length in Sector System
12  SLENGTHY   F  -999999.9 999999.9   ! YP.y direction for length in Sector System
13  SLENGTHZ   F  -999999.9 999999.9   ! YP.z direction for length in Sector System
14  UNORMALX   F  -999999.9 999999.9   ! ZP.x direction for thickness in Sector System
15  UNORMALY   F  -999999.9 999999.9   ! ZP.y direction for thickness in Sector System
16  UNORMALZ   F  -999999.9 999999.9   ! ZP.z direction for thickness in Sector System
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
 END TABLE
!
