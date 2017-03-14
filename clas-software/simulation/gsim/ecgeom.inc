c  ecgeom.inc

c  $Id: ecgeom.inc,v 1.2 1997/04/24 15:38:24 holtrop Exp $

      character*132 ciecgeom
      data ciecgeom/
     1'$Id: ecgeom.inc,v 1.2 1997/04/24 15:38:24 holtrop Exp $'
     1/


c  this is param_ec, extracted from geec.f by E.Wolin, W&M, 15-nov-94
*
*   LCEC...Keep block containing parameters and variables for the
*   shower counter. Variables are initialized in GEEC or ECSET.
*   PARAMETERS: NVEC        : Number of volume descriptors
*               NHEC        : Number of elements per hit
*               NDEC        : Number of elements per digitisation
*               NSTRIP      : Number of scintillator strips per layer
*               NMOD        : Number of modules per sector
*               NFRONT      : Number of modules in front part
*               NLAYERS     : Number of layers in calorimeter
* ---------------------------------------------------------------------------

C ****  Define parameters for the active area of calorimeter

	real value_of_TT,radian,degree,centimeter
	parameter (value_of_TT= 3.141592653589793)	!value of pi
	parameter (radian= 1.00)
	parameter (degree= value_of_TT/180.*radian)
	parameter (centimeter= 1.00)
c
        integer nvec,nhec,ndec,nmod,nfront,nstrip,nlayer
        parameter      (nvec=4,nhec=7, ndec=2)
        parameter  ( nmod    = 13 )
        parameter  ( nfront  =  5 )
        parameter  ( nstrip  = 36 )
	integer nviews
        parameter  ( nlayer  = 39, nviews = 3)
c
	real Ec_basic_R,Ec_basic_angle,Ec_basic_Uin
	real Ec_basic_in_hght,Ec_basic_out_hght
	real Ec_basic_in_wdth,Ec_basic_out_wdth
        real cov1th, cov2th, cov3th
        real pbthi, scthi 
        real att_length(6, nlayer, nstrip) 
        
        common /ecgeom/ Ec_basic_R,Ec_basic_angle,Ec_basic_Uin, 
     +                  Ec_basic_in_hght,Ec_basic_out_hght,
     +                  Ec_basic_in_wdth,Ec_basic_out_wdth,
     +                  cov1th, cov2th, cov3th, pbthi, scthi,
     +                  att_length

