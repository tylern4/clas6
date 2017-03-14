***************** begin: ecgeom **********************************
*- length of a side- (axis,layer,sector)
      real Ec_edge_L(Ec_u:Ec_w, whole:cover, 1:MAXsectors)
      real EcBsc_d(whole:cover)       ! depth of whole, inner and outer
      real EcBsc_H(whole:cover)      !
      real EcBsc_H1(whole:cover)      !
      real EcBsc_H2(whole:cover)      !
*
      real EcBsc_r,EcBsc_a            ! distance and angle
*      
      real EcBsc_phi(1:MAXsectors)   ! Phi angle of each sector 
      real EcBsc_x0a(1:MAXsectors)   ! X of origin of local coordinate system  
      real EcBsc_y0a(1:MAXsectors)   ! Y of origin of local coordinate system 
      real EcBsc_z0a(1:MAXsectors)   ! Z of origin of local coordinate system
*      
      real rotm(1:MAXsectors,3,3)     ! rotation matrix
*
* 
      COMMON/ecgeom/EcBsc_r,EcBsc_a,EcBsc_phi,EcBsc_d,
     &  EcBsc_H,EcBsc_H1,EcBsc_H2,Ec_edge_L,
     &  EcBsc_x0a,EcBsc_y0a,EcBsc_z0a,rotm
*************************** end: ecgeom.inc **********************************
