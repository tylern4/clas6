c  dcgeom.inc

c  $Id: dcgeom.inc,v 1.3 1999/07/30 19:53:52 feuerbac Exp $

      character*132 cidcgeom
      data cidcgeom/
     1'$Id: dcgeom.inc,v 1.3 1999/07/30 19:53:52 feuerbac Exp $'
     1/


c  dcgeom.inc

c  DC volume names
      character*4 cdet_dcr(6),cdet_dcs(6),cdet_dcw(6),cdet_dch(3)
      character*4 cdet_str(2),cdet_sts(2),cdet_stw(2)
      data cdet_dcr  /'DC1A','DC1A','DC2','DC2','DC3','DC3'/
      data cdet_dcs  /'DS1A','DS2A','DS3','DS4','DS5','DS6'/
      data cdet_dcw  /'DW1A','DW2A','DW3','DW4','DW5','DW6'/
      data cdet_dch  /'DC1A','DC2','DC3'/
      data cdet_str  /'DC1B','DC1B'/
      data cdet_sts  /'DS1B','DS2B'/
      data cdet_stw  /'DW1B','DW2B'/


c  number of wire planes per superlayer
      integer ndc_wire_sl(6)
      data ndc_wire_sl / 4,5*6 /



C Drift Chamber Geometry Data base (for one sector)
C                     Bogdan Niczyporuk, March 1991
C Modified by P.Degtyarenko (add ch_plane,ch_dlen,ch_nwpln), September 1992
C
C  ch_r(ilayer)        -  Radius of a given layer
C  ch_d(ilayer)        -  Distance of a given layer to the origin of coord.
C  ch_phimin(ilayer)   -  Phi angle of the 1-st sense wire
C  ch_dphi(ilayer)     -  Angle between adjacent sense wires
C  ch_dlen(ilayer)     -  Distance between adjacent sense wires (along surface)
C  ch_nwire(ilayer)    -  Number of wires in a given layer
C  ch_nwmin(ilayer)    -  First wire# in a given layer
C  ch_nwmax(ilayer)    -  Last wire#  in a given layer
C  ch_ast(ilayer)      -  Stereo angle of a wire (6 degrees)
C  ch_dmax(ilayer)     -  Maximum drift distance (ca radius of cell)
C  ch_nwpln(ilayer)    -  Last wire in cylindrical part
C  ch_plx(ilayer)      -  x-edge between cylindrical and plane parts
C  ch_plane(ilayer)    -  Phi angle to the plane (from x axis to normal to pln)
C                      (Equal to phi of the last wire - if the plane is absent)
C
C  ch_offset(1,ilayer) |    
C  ch_offset(2,ilayer) |-> X0,Y0,Z0 offsets in Lab system (center of curvature)
C  ch_offset(3,ilayer) |     
C
C  ch_ortvec(1,ilayer) |    
C  ch_ortvec(2,ilayer) |-> ort normal to the plane, from coord. origin to plane
C  ch_ortvec(3,ilayer) |     
C
C  ch_middir(1,ilayer) |    
C  ch_middir(2,ilayer) |-> Unit vector (Vx,Vy,Vz) perpendicular to the middle
C  ch_middir(3,ilayer) |   plane (defines mid-plane orientation in Lab frame)
C  
C  ch_wpmid(1,iwire,ilayer) |
C  ch_wpmid(2,iwire,ilayer) |-> Wire position in the middle plane
C  ch_wpmid(3,iwire,ilayer) |
C
C  ch_wpend(1,iwire,ilayer) |
C  ch_wpend(2,iwire,ilayer) |-> Wire position in the "+Z-endplate" plane
C  ch_wpend(3,iwire,ilayer) |
C
C  ch_wpend(4,iwire,ilayer) |
C  ch_wpend(5,iwire,ilayer) |-> Wire position in the "-Z-endplate" plane
C  ch_wpend(6,iwire,ilayer) |
C
C  ch_wdir(1,iwire,ilayer)  |
C  ch_wdir(2,iwire,ilayer)  |-> Wire direction (unit vector)
C  ch_wdir(3,iwire,ilayer)  |
C
C  ch_spacz(ireg)   |-> Distance along Z-direction from endplate to the 
C                   |   middle-of-the-coil plane (for regions 1,2 & 3)
C
C  ch_xk            |   Kossov center for plane part of Reg.I
C

	integer nch,kch,ndx,nlay,nsup
      PARAMETER	(NCH=36)
      PARAMETER (kch=nch+12)          ! Number of layers (with guards) in a sector
      PARAMETER (ndx = 30)            ! Maximum number of hits per layer (CH)
      PARAMETER (nlay = 6)            ! Max. number of layers/superlayer
      PARAMETER (nsup = 6)            ! number of super-layers 

C
c      COMMON/dcgeom/ch_r(kch),ch_phimin(kch),ch_dphi(kch),ch_nwire(kch)
c     1      ,ch_ast(kch),ch_dmax(kch),ch_offset(3,kch),ch_middir(3,kch)
c     2      ,ch_wpmid(3,194,kch),ch_wpend(6,194,kch),ch_wdir(3,194,kch)
c     3      ,ch_plane(kch),ch_dlen(kch),ch_nwpln(kch),ch_d(kch)
c     4      ,ch_plx(kch),ch_ortvec(3,kch), ch_spacz(3), ch_nwmin(nch)
c     5      ,ch_nwmax(nch)

c      REAL ch_r, ch_phimin, ch_dphi, ch_dlen, ch_nwire, ch_ast, ch_dmax,
c     1     ch_offset, ch_middir, ch_wpmid, ch_wpend, ch_wdir, ch_plane,
c     2     ch_d, ch_plx, ch_ortvec, ch_spacz, ch_nwmin, ch_nwmax
c
c      INTEGER ch_nwpln
