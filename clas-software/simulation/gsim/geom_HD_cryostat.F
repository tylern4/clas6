c Date: Thu, 20 Oct 2005 15:17:18 -0400 (EDT)
c From: Sam Hoblit <hoblit@bnl.gov>


	subroutine geom_hd_cryostat(itmed)

c	integer*4, public :: target_medium 			! the medium used to fill ice target
c	real*4, public :: target_length				! the actual length of target medium
c	real*4, public :: target_offset				! the z-offset of the target cell
c	real*4, public :: target_center				! the z position of the center of target medium

	!***********************************************************************************

	implicit none
	integer itmed
	integer hd_tmed(40)                   !array for tracking media indices

	call create_media(hd_tmed)		! specify all media used in simulation
	call create_cryostat(itmed,hd_tmed)	! the ice target

!	call grfile(1,'sasygeom.rz','NO')			! write geom file

	return
	end 

	!***********************************************************************************

	subroutine create_media(hd_tmed)

!>>>	use sasymc_field

	implicit none
	integer hd_tmed(40)

	integer nmat
	parameter (nmat = 28)	! # materials to create

! nlmat < 0 indicates the WMAT proportions are by atoms
! nlmat > 0 indicates the WMAT proportions are by mass
     
	integer nlmat(nmat) 
	data nlmat /-2,		! Nai
     1		-2,		! Plastic scintillator
     1          -3,		! Mylar
     1		-1,		! Argon
     1	  	-1,		! Nickel
     1		-1,		! Helium
     1		 5,		! Pb Glass
     1		 4,		! Paper
     1		 4,		! low density paper for NB packing
     1          -4,		! Foam
     1		-2,		! teflon
     1		-2,		! HD ice
     1		-1,		! Niobium
     1		-1,		! Titanium
     1		 5,		! Glass
     1		-2,		! Aerogel 30
     1		-2,		! Aerogel 50
     1		-4,		! kapton
     1		-3,		! lucite
     1		-3,		! kel-f
     1		 3,		! HD ice & wires
     1		 2,		! H2 ice & wires
     1		 2,		! D2 ice & wires
     1		-1,		! Silver
     1		-2,		! CH2
     1		-1,		! Carbon
     1		-1,		! low-density Carbon
     1		-2/		! NbTi superconductor

! the dens array gives the density of the material in g/cm**3

	real dens(nmat) 
	data dens /3.67,		! NaI
     1	        1.032,			! Plastic scintillator
     1		1.39,			! Mylar
     1		1.7837E-03,		! Argon
     1		8.902,			! Nickel
     1		1.785E-04,		! Helium
     1		3.85,			! Pb Glass
     1		1.0,			! Paper
     1		0.57,			! low density paper (measured 11-02)
     1		0.075,			! Foam
     1		2.2,			! teflon
     1  	0.14,			! HD ice
     1		8.57,			! Niobium
     1		4.54,			! Titanium
     1		2.5,			! Glass
     1		0.11,			! Aerogel 30
     1		0.18,			! Aerogel 50
     1		1.42,			! kapton
     1		1.2,			! lucite
     1		2.12,			! kel-f
     1		0.143,			! HD ice & wires
     1		0.12,			! H2 ice & wires
     1		0.23,			! D2 ice & wires
     1		10.5,			! Silver
     1		0.9393,			! CH2
     1		1.7939,			! carbon
     1		0.35878,		! low-density carbon
     1		6.0/			! NbTi

! the a array gives the atomic weights of the constituents of the material

	real a(5,nmat) 
	data a /23.0, 126.9, 0.0, 0.0, 0.0,	        ! NaI
     1		12.01, 1.01, 0.0, 0.0, 0.0,		! Plastic scintillator
     1		12.01, 1.01, 16.0, 0.0,	0.0,		! Mylar
     1		39.95, 0.0, 0.0, 0.0, 0.0,		! Argon
     1		58.69, 0.0, 0.0, 0.0, 0.0,		! Nickel
     1 		4.0026, 0.0, 0.0, 0.0, 0.0,		! Helium
     1		16.0, 22.9898, 28.086, 39.102, 207.19,	! Pb Glass
     1		12.01, 1.01, 16.0, 14.0067, 0.0,	! Paper
     1		12.01, 1.01, 16.0, 14.0067, 0.0,	! low density paper
     1		12.01, 1.01, 14.01, 16.0, 0.0,		! Foam  (low density C2H2)
     1		12.01, 18.998, 0.0, 0.0, 0.0,		! Teflon
     1		1.01, 2.01, 0.0, 0.0, 0.0,		! HD Ice
     1		92.906, 0.0, 0.0, 0.0, 0.0,		! Niobium
     1		47.88, 0.0, 0.0, 0.0, 0.0,		! Titanium
     1		28.086, 15.999, 10.811, 22.99, 26.981,	! Glass
     1		28.086, 15.999, 0.0, 0.0, 0.0,		! Aerogel 30
     1		28.086, 15.999, 0.0, 0.0, 0.0,		! Aerogel 50
     1		12.01, 1.01, 14.01, 16., 0.,		! kapton
     1		12.01, 1.01, 16.0, 0.0, 0.0,		! lucite
     1		12.01, 18.9984, 35.4527, 0.0, 0.0,	! kel-f
     1		1.01, 2.01, 26.982, 0.0, 0.0,		! HD ice & wires
     1		1.01, 26.982, 0.0, 0.0, 0.0,		! H2 ice & wires
     1		2.01, 26.982, 0.0, 0.0, 0.0,		! D2 ice & wires
     1		107.87, 0.0, 0.0, 0.0, 0.0,		! Silver
     1		12.01, 1.01, 0.0, 0.0, 0.0,		! CH2
     1		12.01, 0.0, 0.0, 0.0, 0.0,		! carbon
     1		12.01, 0.0, 0.0, 0.0, 0.0,		! carbon
     1		92.906,47.88, 0.0, 0.0, 0.0/		! NbTi

! the z array gives the atomic numbers of the constituents

	real  z(5,nmat) 
	data z /11., 53., 0.0, 0.0, 0.0,                ! NaI
     1		6. , 1., 0.0, 0.0, 0.0,			! Plastic scintillator
     1		6., 1., 8., 0.0, 0.0,			! Mylar
     1		18., 0.0, 0.0, 0.0, 0.0,		! Argon
     1		28., 0.0, 0.0, 0.0, 0.0,		! Nickel
     1		2., 0.0, 0.0, 0.0, 0.0,			! Helium
     1		8., 11., 14., 19., 82.,			! Pb Glass
     1		6., 1., 8., 7., 0.,			! Paper
     1		6., 1., 8., 7., 0.,			! low density paper
     1		6., 1., 7., 8., 0.,			! Foam  Rohacel
     1		6., 9., 0.0, 0.0, 0.0,			! Teflon
     1		1., 1., 0.0, 0.0, 0.0,			! HD Ice
     1		41., 0.0, 0.0, 0.0, 0.0,		! Niobium
     1		22., 0.0, 0.0, 0.0, 0.0,		! Titanium
     1		14., 8., 5., 11., 13.,			! Glass
     1		14., 8., 0.0, 0.0, 0.0,			! Aerogel 30
     1		14., 8., 0.0, 0.0, 0.0,			! Aerogel 50
     1		6., 1., 7., 8., 0.,			! kapton
     1		6., 1., 8., 0.0, 0.0,			! lucite
     1		6., 9., 17., 0.0, 0.0,			! kel-f
     1		1., 1., 13., 0.0, 0.0,			! HD ice & wires
     1		1., 13., 0.0, 0.0, 0.0,			! H2 ice & wires
     1		1., 13., 0.0, 0.0, 0.0,			! D2 ice & wires
     1		47., 0.0, 0.0, 0.0, 0.0,		! Silver
     1		6. , 1., 0.0, 0.0, 0.0,			! CH2
     1		6. , 0.0, 0.0, 0.0, 0.0,		! carbon
     1		6. , 0.0, 0.0, 0.0, 0.0,		! carbon
     1		41., 22., 0.0, 0.0, 0.0/		! NbTi

! the WMAT array contains the proportions of elements in the mixture

	real wmat(5,nmat) 
	data wmat /1., 1., 0.0, 0.0, 0.0,	        ! NaI
     1		1., 1.1, 0.0, 0.0, 0.0,			! Plastic scintillator
     1		5., 4., 2., 0.0, 0.0,			! Mylar
     1		1., 0.0, 0.0, 0.0, 0.0,			! Argon
     1		1., 0.0, 0.0, 0.0, 0.0,			! Nickel
     1		1., 0.0, 0.0, 0.0, 0.0,			! Helium
     1 		0.27, 0.01, 0.20, 0.04, 0.48,		! Pb Glass
     1		0.51, 0.06, 0.42, 0.01, 0.0,		! Paper
     1		0.51, 0.06, 0.42, 0.01, 0.0,		! low density paper
     1		9., 13., 1., 2., 0.,			! Foam  (rohacell)
     1		1., 2., 0.0, 0.0, 0.0,			! Teflon
     1		1., 1., 0.0, 0.0, 0.0,			! HD Ice
     1		1., 0.0, 0.0, 0.0, 0.0,			! Niobium
     1		1., 0.0, 0.0, 0.0, 0.0,			! Titanium
     1		0.2005, 0.7075, 0.0596, 0.0204, 0.012,	! Glass
     1		1., 2., 0.0, 0.0, 0.0,			! Aerogel 30
     1		1., 2., 0.0, 0.0, 0.0,			! Aerogel 50
     1		11., 5., 1., 2., 0.,			! kapton
     1		5., 8., 2., 0.0, 0.0,			! lucite
     1		2., 3., 1., 0.0, 0.0,			! kel-f
     1		0.272, 0.545, 0.183, 0.0, 0.0,		! HD ice & wires
     1		0.736, 0.264, 0.0, 0.0, 0.0,		! H2 ice & wires
     1		0.89, 0.11, 0.0, 0.0, 0.0,		! D2 ice & wires
     1		1., 0.0, 0.0, 0.0, 0.0,			! Silver
     1		1., 2., 0.0, 0.0, 0.0,			! CH2
     1		1., 0.0, 0.0, 0.0, 0.0,			! carbon
     1		1., 0.0, 0.0, 0.0, 0.0,			! carbon
     1		1., 1., 0.0, 0.0, 0.0/			! NbTi

! namat contains the names of the various materials (fk: numbers relative to comon offset)

	character*20 namat(nmat) 
	data namat /    'Sodium Iodide (NaI) ',         ! 17
     1			'Pilot B scintillator',		! 18
     1			'Mylar',			! 19
     1			'Argon',			! 20
     1			'Nickel',			! 21
     1			'Helium gas',			! 22
     1			'Lead Glass',			! 23
     1			'Paper',			! 24
     1			'Low density paper',		! 25
     1			'Foam',				! 26  Rohacell
     1			'Teflon',			! 27
     1			'HD Ice',			! 28
     1			'Niobium',			! 29
     1			'Titanium',			! 30
     1			'Glass',			! 31
     1			'Aerogel 30',			! 32
     1			'Aerogel 50',			! 33
     1			'Kapton',			! 34
     1			'Lucite',			! 35
     1			'KEL-F',			! 36
     1			'HD ice & wires',		! 37  density = 1.15 HD density
     1			'H2 ice & wires',		! 38
     1			'D2 ice & wires',		! 39
     1			'Silver',			! 40
     1			'CH2',				! 41
     1			'Carbon',			! 42
     1			'Low density Carbon',		! 43
     1			'Niobium-Titanium'/		! 44

	integer nmed
	parameter (nmed = 36)      ! number of media we will use in simulation

! the following are parameters used when defining the media we will use
! in the simulation. These are usually not used, as we let geant determine
! these constants automatically for each material by leaving IGAUTO = 1 (default).
! if IGAUTO in /GCTRAK/ is set to 0, then the following parameters are used.

	real amax, dmax, emax, smin
	data amax /1.0/	   ! max. angle deviation in bfield in 1 step (degrees)
	data dmax /0.02/   ! maximum step size (cm)
	data emax /0.01/   ! maximum fractional E loss in 1 step
	data smin /0.0005/ ! minimum value for maximum step (cm)

	integer field_trak
	real bfield_max
	data field_trak, bfield_max / 0, 0 /         !fk: for now
! the names we'll assign to the media we use in simulation

	character*20 named(nmed) 
	data named /    'Atmosphere (AIR)',	! 1
     1			'Sodium Iodide (NAI)',			! 2
     1			'Scintillator',				! 3
     1			'Aluminum',				! 4
     1			'Liquid Hydrogen',			! 5
     1			'Liquid Deuterium',			! 6
     1			'Mylar',				! 7
     1			'Argon',				! 8
     1			'Nickel',				! 9
     1			'Helium gas',				! 10
     1			'Lead Glass',				! 11
     1			'Paper',				! 12
     1			'Low density Paper',			! 13
     1			'Foam (C2H2) Low den',			! 14
     1			'Teflon',				! 15
     1			'HD ice',				! 16
     1			'Niobium',				! 17
     1			'Titanium',				! 18
     1			'Glass',				! 19
     1			'Vacuum',				! 20
     1			'Copper',				! 21
     1			'Liquid Helium',			! 22
     1			'Aerogel 30',				! 23
     1			'Aerogel 50',				! 24
     1			'Kapton',				! 25
     1			'Lucite',				! 26
     1			'Iron',					! 27
     1			'KEL-F',				! 28
     1			'HD ice & wires',			! 29
     1			'H2 ice & wires',			! 30
     1			'D2 ice & wires',			! 31
     1			'Silver',				! 32
     1			'CH2',					! 33
     1			'Carbon',				! 34
     1			'Low density Carbon',			! 35
     1			'Niobium-Titanium'/			! 36

! the material numbers of our media (fk: these numbers relative to common offset)

	integer*4 medmat(nmed) 
	data medmat /15,17,18, 9, 1, 2,19,20,21,22,23,24,25,26,27,28,29,
     1	       30,31,16,11, 3,32,33,34,35,10,36,37,38,39,40,41,42,43,44/

! flags for sensitive media (1 = sensitive)

	integer*4 ismed(nmed) 
	data ismed  / 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0,
     1          0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/

! and finally the tracking precision for our media

	real*4 epsil_med(nmed) 
	data epsil_med /0.03,			! air
     1			0.03,			! NaI
     1			0.03,			! plastic scint
     1			0.03,			! aluminum
     1			0.03,			! LH2
     1 			0.03,			! LD2
     1			0.03,			! mylar
     1			0.001,			! argon
     1			0.001,			! Nickel
     1			0.001,			! Helium gas
     1			0.1,			! Pb Glass
     1			0.1,			! paper
     1			0.1,			! Hexel
     1			0.1,			! foam
     1			0.1,			! teflon
     1			0.1,			! HD ice
     1			0.1,			! niobium
     1			0.1,			! titanium
     1			0.1,			! glass
     1			0.1,			! vacuum
     1			0.1,			! copper
     1			0.1,			! liquid He
     1			0.1,			! aerogel 30
     1			0.001,			! aerogel 50
     1  		0.001,			! kapton
     1			0.001,			! lucite
     1			0.001,			! iron
     1			0.001,			! Kel-F
     1			0.1,			! HD ice & wires
     1			0.1,			! H2 ice & wires
     1			0.1,			! D2 ice & wires
     1			0.01,			! silver
     1			0.1,			! CH2
     1			0.1,			! carbon
     1			0.1,			! low-dens carbon
     1			0.1/			! NbTi


	integer get_mate, get_tmed
	integer i, hd_imat(40), im

! make everything as mixtures so geant will calculate radiation lengths
	do i=1,40
	   hd_tmed(i)=0
	   hd_imat(i)=0
	enddo
	
	do i=1,nmat
	   hd_imat(i)=get_mate()
	   call gsmixt(hd_imat(i),namat(i),a(1,i), z(1,i), dens(i),nlmat(i),wmat(1,i))
	enddo

	do i = 1,nmed
	   im=medmat(i)-16
	   if(im.le.0) then              ! geant standard media
	      hd_tmed(i)=get_tmed()
	      call gstmed(hd_tmed(i),named(i),medmat(i),ismed(i),field_trak,bfield_max,
     &             amax,dmax,emax,epsil_med(i),smin,0,0)
	   elseif(hd_imat(im).gt.0) then
	      hd_tmed(i)=get_tmed()
	      call gstmed(hd_tmed(i),named(i),hd_imat(im),ismed(i),field_trak,bfield_max,
     &             amax,dmax,emax,epsil_med(i),smin,0,0)
	   endif
	end do

	return
	end 


	!***********************************************************************************

	subroutine create_cryostat(itmed,hd_tmed)

	implicit none
#include "ffpar_tg.inc"
	integer itmed		! the medium the user wants for target
	integer hd_tmed(40)
	real len_ibc 		! half-length of vacuum tube (cm)
	real rad_ibc 		! radius of IBC vacuum       (cm)
	data len_ibc, rad_ibc / 19.07032, 4.01 /

	real len_mag		! half-length of SC magnet from Healy's drawing
				! with cooling contractions (w/o = 12.766) (cm)
	data  len_mag / 12.386 /

	real shape(3)
	integer i

	! all the volumes defined in the target will have a common
	! position relative to the IBC. When we insert a target,
	! only the target cell itself is inserted into the IBC; all the
	! other volumes - the NMR coil, the magnet coil, the 80K shield,
	! are attached to the IBC. The nominal center position of the target
	! and the IBC are (0,0,0), which is defined to be the center of
	! the crystal box. If the actual position of the IBC is not 0
	! then the IBC center positions must be changed in the create_ibc routine.
	! The target medium, it's relative position to the IBC, and the length
	! of the target medium are passed parameters. The target_medium number
	! is saved as a global for reference in other routines, like source.

	! create the vacuum inside the IBC.
	! this volume 'TGVC' serves as mother for
	! everything down inside the IBC

	shape(1) = 0.0					! inner radius
	shape(2) = rad_ibc				! outer radius
	shape(3) = len_ibc				! half-length
	call gsvolu('TGVC','TUBE', hd_tmed(20), shape, 3, i)	! vacuum chamber inside IBC
	call gspos ('TGVC',1,'BEAM',tg_pos_ff(1),tg_pos_ff(2),tg_pos_ff(3),0,'ONLY')	! position IBC vacuum relative to BEAM pipe volume

	write(6,*)' ***********************************************'
	write(6,*)' HDics crystat: TGVC defined'

	! now create the IBC and contents

	call create_IBC(rad_ibc,len_ibc,hd_tmed)	! vacuum chamber & heat shields

	call create_superconducting_coil(len_mag,hd_tmed)	! SC coil and Al holder
	call create_nmr_form(len_mag,hd_tmed)			! KEL-F NMR form & Al support
	call create_target_cell(itmed,hd_tmed)			! HD ice target cell

	return
	end

	!-----------------------------------------------------------

	subroutine create_IBC(rad_ibc,len_ibc,hd_tmed)

	implicit none

	real rad_ibc, len_ibc
	integer hd_tmed(40)

! heat shield: half-length, outer radius, thickness
	real len_20k, rad_20k, thick_20k            ! 20k heat shield
	real len_77k, rad_77k, thick_77k            ! 77k heat shield
			! 20k shield inner radius	5=mm thick - Wei 10/5/05
	data len_20k, rad_20k, thick_20k / 14.2844, 3.40 , 0.05 /
	data len_77k, rad_77k, thick_77k / 15.1014, 3.591, 0.10 /

	real end_cap_hafthk     ! half-thickness of endcaps (half 0.3 mils) (cm)
	data end_cap_hafthk / 0.000381 /
	real super_thk 		! thickness of superinsulation with backing (cm)
	data super_thk      / 0.001787 /
	real thin_s_thk 	! thickness of non-backed SI (cm)
	data thin_s_thk     / 0.0006787 /
				! both thickness deduced from weighed samples
				! and using the density of mylar of 1.39 g/cm^3

	real z,shape(30)

	integer i
			! this routine makes outer IBC tube & heat shields.
	! the main outer vacuum tube of IBC

	shape(1) = rad_ibc - 0.1		! inner radius
	shape(2) = rad_ibc			! outer radius
	shape(3) = len_ibc			! half length
	call gsvolu('IBC1','TUBE', hd_tmed(4), shape, 3, i)		! aluminum
	call gspos ('IBC1',1,'TGVC',0.0,0.0,0.0,0,'ONLY')

! polycone for vacuum mylar window (from Dennis Healy's drawing)

	shape(1) = 0.0				! starting phi
	shape(2) = 360.0			! delta phi
	shape(3) = 9				! # of z plane cuts

	shape(4) =-3.2034			! value of first z cut
	shape(5) = 0.0				! inner radius at cut
	shape(6) = 0.0				! outer radius (same as target_radius)

	shape(7) = -3.1738
	shape(8) = 0.0
	shape(9) = 0.6796

	shape(10) = -3.0947
	shape(11) = 0.0
	shape(12) = 1.2969

	shape(13) = -2.9377
	shape(14) = 0.0
	shape(15) = 2.0182

	shape(16) = -2.8128
	shape(17) = 0.0
	shape(18) = 2.5719

	shape(19) = -2.4011
	shape(20) = 0.0
	shape(21) = 3.2583

	shape(22) = -1.7488
	shape(23) = 0.0
	shape(24) = 3.7224

	shape(25) = -0.9652
	shape(26) = 0.0
	shape(27) = rad_ibc - 0.1

	shape(28) = 0.0
	shape(29) = 0.0
	shape(30) = rad_ibc - 0.1

	call gsvolu('HCAP', 'PCON', hd_tmed(7), shape, 30, i)		! mylar
	call gspos ('HCAP',1,'TGVC',0.0,0.0,len_ibc,0,'ONLY')	! place at end of IBC

	! polycone for air leaving 4 mils of mylar for end cap (0.01 cm)

	shape(1) = 0.0						! starting phi
	shape(2) = 360.0					! delta phi
	shape(3) = 9						! # of z plane cuts

	shape(4) =-3.1934					! value of first z cut
	shape(5) = 0.0						! inner radius at cut
	shape(6) = 0.0						! outer radius (same as target_radius)

	shape(7) = -3.1485
	shape(8) = 0.0
	shape(9) = 0.6774

	shape(10) = -3.0851
	shape(11) = 0.0
	shape(12) = 1.2940

	shape(13) = -2.9132
	shape(14) = 0.0
	shape(15) = 2.0116

	shape(16) = -2.8044
	shape(17) = 0.0
	shape(18) = 2.5665

	shape(19) = -2.3972
	shape(20) = 0.0
	shape(21) = 3.2491

	shape(22) = -1.7474
	shape(23) = 0.0
	shape(24) = 3.71249

	shape(25) = -0.9652
	shape(26) = 0.0
	shape(27) = 3.90

	shape(28) = 0.0
	shape(29) = 0.0
	shape(30) = 3.90

	call gsvolu('ACAP', 'PCON', hd_tmed(1), shape, 30, i)		! put air cap
	call gspos ('ACAP',1,'HCAP',0.0,0.0,0.0,0,'ONLY')	! inside mylar cap

! Al ring at the downstream end of vacuum chamber

	shape(1) = 3.90 - 0.16					! inner radius
	shape(2) = 3.90						! outer radius
	shape(3) = 0.4585					! half length
	z = -0.4585
	call gsvolu('IBC2','TUBE', hd_tmed(4), shape, 3, i)		! aluminum
	call gspos ('IBC2',1,'ACAP',0.0,0.0,z,0,'ONLY')

	! the 20K Al shield

	shape(1) = rad_20k - 0.05
	shape(2) = rad_20k					! outer radius
	shape(3) = len_20k					! half length
	call gsvolu('ALF3','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('ALF3',1,'TGVC',0.0,0.0,0.0,0,'ONLY')

! the super insulation - 10 layers

	shape(1) = rad_20k					! inner radius
	shape(2) = rad_20k + 10.0*super_thk			! outer radius
	shape(3) = len_20k					! half length
	call gsvolu('SUP1','TUBE', hd_tmed(7), shape, 3, i)		! mylar
	call gspos ('SUP1',1,'TGVC',0.0,0.0,0.0,0,'ONLY')

! 20k AL endcap

	shape(1) = 0.0						! inner radius
	shape(2) = rad_20k					! outer radius
	shape(3) = end_cap_hafthk 				! half length - 3 mils thick
	z = len_20k + end_cap_hafthk 				! position
	call gsvolu('ALF4','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('ALF4',1,'TGVC',0.0,0.0,z,0,'ONLY')

! non-backed super insulation end cap - 10 layers

	shape(1) = 0.0						! inner radius
	shape(2) = rad_20k + 10.0*super_thk			! outer radius
	shape(3) = 10.0*thin_s_thk/2.0				! half length
	z = z + end_cap_hafthk + shape(3)			! end cap
	call gsvolu('SUP2','TUBE', hd_tmed(7), shape, 3, i)		! mylar
	call gspos ('SUP2',1,'TGVC',0.0,0.0,z,0,'ONLY')

! the 77K Al shield

	shape(1) = rad_77k - 0.1				! inner radius
	shape(2) = rad_77k					! outer radius
	shape(3) = len_77k					! half length
	call gsvolu('ALF1','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('ALF1',1,'TGVC',0.0,0.0,0.0,0,'ONLY')

! the super insulation - 10 layers

	shape(1) = rad_77k					! inner radius
	shape(2) = rad_77k + 10.0*super_thk			! outer radius
	shape(3) = len_77k					! half length
	call gsvolu('SUP3','TUBE', hd_tmed(7), shape, 3, i)		! mylar
	call gspos ('SUP3',1,'TGVC',0.0,0.0,0.0,0,'ONLY')

! 77k Al endcap

	shape(1) = 0.0						! inner radius
	shape(2) = rad_77k					! outer radius
	shape(3) = end_cap_hafthk				! half length - 8 mils thick
	z = len_77k + end_cap_hafthk				! position
	call gsvolu('ALF2','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('ALF2',1,'TGVC',0.0,0.0,z,0,'ONLY')

! non-backed super insulation end cap - 10 layers

	shape(1) = 0.0						! inner radius
	shape(2) = rad_77k + 10.0*super_thk			! outer radius
	shape(3) = 10.0*thin_s_thk/2.0				! half length
	z = z + end_cap_hafthk + shape(3)			! end cap
	call gsvolu('SUP4','TUBE', hd_tmed(7), shape, 3, i)		! mylar
	call gspos ('SUP4',1,'TGVC',0.0,0.0,z,0,'ONLY')

	return
	end

	!-----------------------------------------------------------

	subroutine create_superconducting_coil(len_mag,hd_tmed)

	implicit none

	real len_mag
	integer hd_tmed(40)

	real z, shape(3)
	integer i

	! first the inner Al mandrell

	shape(1) = 2.64						! inner radius
	shape(2) = 2.74						! outer radius
	shape(3) = len_mag					! half length
	call gsvolu('COAL','TUBE', hd_tmed(4), shape, 3, i)		! Aluminum
	call gspos ('COAL',1,'TGVC',0.0,0.0,0.0,0,'ONLY')

	! make the coil. It's a mix of Copper, Niobium/Titanium superconductor
	! wire diameter is 0.0175" = 0.016" cu/nbti + 0.0015" formvar
	! get equivalent mass of Copper and NbTi as 2 layers. Ratio of Copper
	! to NbTi is 1.34:1.0. There are 4 layers of wire.
	! area of 0.016" dia wire = pi*(0.4064/2.0)**2 = 0.12971 mm*2
	! spread this over base of 1 diameter of wire = 0.12971/0.4445 = 0.29182 mm/layer
	! 4 layers = 1.16731 mm of copper-NbTi mixture.
	! (1.34/2.34)*1.16731 = 0.6685 mm Copper
	! 1.16731 - 0.6685    = 0.4988 mm NbTi.

	shape(1) = 2.74						! inner radius
	shape(2) = 2.74 + 0.06685				! outer radius
	call gsvolu('CONC','TUBE', hd_tmed(21), shape, 3, i)		! copper
	call gspos ('CONC',1,'TGVC',0.0,0.0,0.0,0,'ONLY')

	shape(1) = shape(2)					! inner radius
	shape(2) = shape(2) + 0.04988				! outer radius
	call gsvolu('CONB','TUBE', hd_tmed(36), shape, 3, i)		! NbTi superconductor
	call gspos ('CONB',1,'TGVC',0.0,0.0,0.0,0,'ONLY')

	! outer Al shell over superconductor

	shape(1) = 2.95						! inner radius
	shape(2) = 3.05						! outer radius
	call gsvolu('CUCL','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('CUCL',1,'TGVC',0.0,0.0,0.0,0,'ONLY')

	! Al joint at end

	shape(1) = 2.64						! inner radius
	shape(2) = 3.05						! outer radius
	shape(3) = 0.135					! half length
	z = len_mag + 0.135					! at end of superconductor
	call gsvolu('COE1','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('COE1',1,'TGVC',0.0,0.0, z,0,'ONLY')

	! Al end tube

	shape(1) = 2.85						! inner radius
	shape(2) = 3.05						! outer radius
	shape(3) = 0.56						! half length
	z = z + 0.135 + 0.56
	call gsvolu('COE2','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('COE2',1,'TGVC',0.0,0.0, z,0,'ONLY')

	! and the Al endcap

	shape(1) = 0.0						! inner radius
	shape(2) = 3.05						! outer radius
	shape(3) = 0.001					! half thickness -- check this!!!!
	z = z + 0.56 + 0.001
	call gsvolu('CONS','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('CONS',1,'TGVC',0.0,0.0, z,0,'ONLY')

	! the weld bump - just use 1mm thick cylinder
	! width and position taken from drawing (roughly)

	shape(1) = 3.05
	shape(2) = 3.15
	shape(3) = 0.40
	z = z - 0.001 - 0.635
	call gsvolu('WELD','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('WELD',1,'TGVC',0.0,0.0, z,0,'ONLY')

	return
	end

	!-----------------------------------------------------------

	subroutine create_nmr_form(len_mag,hd_tmed)

	! this routine creates the KEL-F form that holds the NMR wires
	! and the Aluminum shell that holds it and slides into the Al SC magnet frame

	implicit none
	real len_mag
	integer hd_tmed(40)

	real sind               !external
	real ag_rad		! half thickness (radius) of Ag in NMR wire
	data ag_rad / 0.0125 /
	real tefw_thk		! (cm) thickness of teflon on NMR wire
	data tefw_thk / 0.0025 /
	real wire_rad 		! (cm) total thickness of wire
	real len_coil 		! (cm) edge of SC frame where Al shell stops

	! all dimensions for NMR Kel-F and ALuminum were taken from
	! the spare frame and then corrected for shrinkage when cool.

	real x,y,z,zk,dd
	real shape(3)
	integer i

	wire_rad = ag_rad + tefw_thk	! (cm) total thickness of wire
	len_coil = len_mag + 0.135	! (cm) edge of SC frame where Al shell stops
	! the Al can that slides into the magnet

	shape(1) = 2.58				! inner radius (0.6 mm thick)
	shape(2) = 2.64				! outer radius
	shape(3) = 3.127			! half length
	z = len_coil - 3.127			! at end of transition
	call gsvolu('NMR9','TUBE', hd_tmed(4), shape, 3, i)			! Al
	call gspos ('NMR9',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! brass sleeve that keeps NMR wires out of beam
	! length (z) of this sleeve is guessed from photo

	shape(1) = 2.3				! from Andy's drawing
	shape(2) = 2.3 + 0.00762		! 3 mils thick
	shape(3) = 2.8				! from Andy's drawing
	z = len_coil - 6.254 + 2.8		! ~ at upstream edge
	call gsvolu('BRSL','TUBE', hd_tmed(21), shape, 3, i)		! use copper
	call gspos ('BRSL',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! NMR wires and teflon heat-shrink between brass and Al

	shape(1) = 0.07				! from Andy's drawing
	shape(2) = 0.0762			! 
	shape(3) = 2.91				! from Andy's drawing
	dd = (2.58 + 2.30762)/2.0
	x = sind(45.0)*dd
	z = len_coil - 6.254 + 2.91		! ~ at upstream edge
	call gsvolu('HEAT','TUBE', hd_tmed(15), shape, 3, i)		! use teflon
	call gspos ('HEAT',1,'TGVC',x,-x,z,0,'ONLY')		! -45 degrees
	call gspos ('HEAT',2,'TGVC',-x,-x,z,0,'ONLY')		! 235 degrees

	shape(1) = 0.0				! inner radius
	shape(2) = wire_rad			! outer radius
	shape(3) = 2.60				! half length
	call gsvolu('WLR1','TUBE', hd_tmed(15), shape, 3, i)		! teflon coating

	shape(2) = ag_rad					! outer radius
	call gsvolu('WLR2','TUBE', hd_tmed(32), shape, 3, i)		! silver wire
	call gspos ('WLR2',1,'WLR1',0.0,0.0,0.0,0,'ONLY')
	call gspos ('WLR1',1,'TGVC', x-wire_rad,-x,z,0,'ONLY')
	call gspos ('WLR1',2,'TGVC', x+wire_rad,-x,z,0,'ONLY')
	call gspos ('WLR1',3,'TGVC',-x-wire_rad,-x,z,0,'ONLY')
	call gspos ('WLR1',4,'TGVC',-x+wire_rad,-x,z,0,'ONLY')

	! small Al ridge at end of holder that stops the Al insert
	! (NMR9) when inserting into superconducting magnet

	shape(1) = 2.58				! inner radius
	shape(2) = 2.68				! outer radius ( total thick 1 mm)
	shape(3) = 0.08				! half length
	z = len_coil + 0.08			! at end of transition
	call gsvolu('NMRA','TUBE', hd_tmed(28), shape, 3, i)		! KEL-F
	call gspos ('NMRA',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! transition from Al inside KEL-F to Al that
	! fits into the magnet form

	shape(1) = 2.34				! inner radius (0.6 mm thick)
	shape(2) = 2.64				! outer radius
	shape(3) = 0.05				! half length
	z = len_coil - 6.254 - 0.05		! at transition
	call gsvolu('NMR8','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('NMR8',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! the inner Al can that the KEL-F cylinder connects to

	shape(1) = 2.34				! inner radius
	shape(2) = 2.44				! outer radius (2" diameter)
	shape(3) = 1.33				! half length
	z = z - 0.05 - 1.33			! end of transition
	call gsvolu('NMR7','TUBE', hd_tmed(4), shape, 3, i)		! Al
	call gspos ('NMR7',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! downstream KEL-F cynlinder that attaches to Al shell

	shape(1) = 2.44				! inner radius
	shape(2) = 2.54				! outer radius (2" diameter)
	shape(3) = 1.33				! half length
	call gsvolu('NMR6','TUBE', hd_tmed(28), shape, 3, i)		! KEL-F
	call gspos ('NMR6',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! downstream transition to larger radius

	shape(1) = 1.45				! inner radius (same as main coil holder)
	shape(2) = 2.54				! outer radius (2" diameter)
	shape(3) = 0.1				! half length
	z = z - 1.33 - 0.1
	call gsvolu('NMR5','TUBE', hd_tmed(28), shape, 3, i)		! KEL-F
	call gspos ('NMR5',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! the main KEL-F coil

	shape(1) = 1.45				! inner radius
	shape(2) = 1.76				! outer radius
	shape(3) = 3.28				! half length
	zk = z - 0.1 - 3.28
	call gsvolu('NMR1','TUBE', hd_tmed(28), shape, 3, i)		! KEL-F
	call gspos ('NMR1',1,'TGVC',0.0,0.0,zk,0,'ONLY')

	write(6,*) ' kel-f center z = ', zk

	z = -0.15				! downstream thick band 3mm longer

	! center cut-out

	shape(1) = 1.57				! inner radius
	shape(2) = 1.76				! outer radius
	shape(3) = 2.349			! half length
	call gsvolu('NMR2','TUBE', hd_tmed(20), shape, 3, i)		! vacuum
	call gspos ('NMR2',1,'NMR1',0.0,0.0,z,0,'MANY')		! overlaps with NMR3 and NMR4

	! thin cut-outs for NMR wires.
	! 4 cutouts running length of Kel-F form

	shape(1) = 0.125			! half width (depth of cut)
	shape(2) = 0.08				! half height (width of cut)
	shape(3) = 3.13				! half length (long cuts at 0, 180)
	call gsvolu('NMR3','BOX ', hd_tmed(20), shape, 3, i)

	shape(1) = 0.08				! half width (width of cut)
	shape(2) = 0.125			! half height (dpeth of cut)
	shape(3) = 2.66				! half length (short cuts at 90, 270)
	call gsvolu('NMR4','BOX ', hd_tmed(20), shape, 3, i)

	x = 1.76 - 0.125           	! radial offset of cutout
	call gspos ('NMR3',1,'NMR1', x,0.0,z,0,'ONLY')
	call gspos ('NMR3',2,'NMR1',-x,0.0,z,0,'ONLY')
	call gspos ('NMR4',1,'NMR1',0.0, x,z,0,'ONLY')
	call gspos ('NMR4',2,'NMR1',0.0,-x,z,0,'ONLY')

	! the following makes silver wires with teflon coating

	shape(1) = 0.0				! inner radius
	shape(2) = wire_rad			! outer radius
	shape(3) = 2.60				! half length
	call gsvolu('NMRW','TUBE', hd_tmed(15), shape, 3, i)	! teflon coating

	shape(2) = ag_rad				! outer radius
	call gsvolu('NMRS','TUBE', hd_tmed(32), shape, 3, i)	! silver wire
	call gspos ('NMRS',1,'NMRW',0.0,0.0,0.0,0,'ONLY')

	! now stick these along outside in KELF cutouts
	! at 0 & 180 we have 2 wires, 90 & 270 have 4
	! for symmetry I put these at middle of cutout
	! but this probably doesn't matter too much

	call gspos ('NMRW',1,'NMR3', 0.0, -wire_rad, 0.0,0,'ONLY')	! 0 & 180
	call gspos ('NMRW',2,'NMR3', 0.0,  wire_rad, 0.0,0,'ONLY')

	call gspos ('NMRW',3,'NMR4', -3.0*wire_rad, 0.0, 0.0,0,'ONLY')	! 90 & 270
	call gspos ('NMRW',4,'NMR4', -1.0*wire_rad, 0.0, 0.0,0,'ONLY')
	call gspos ('NMRW',5,'NMR4',  1.0*wire_rad, 0.0, 0.0,0,'ONLY')
	call gspos ('NMRW',6,'NMR4',  3.0*wire_rad, 0.0, 0.0,0,'ONLY')

	! now put the wires wrapped around the ends of the KEL-F form
	! these are approx by tubes

	shape(1) = 1.76					! inner radius
	shape(2) = 1.76 + 2.0*wire_rad			! outer radius
	shape(3) = wire_rad				! half length
	call gsvolu('NMRX','TUBE',hd_tmed(15), shape, 3, i)	! teflon

	shape(1) = shape(1) + tefw_thk			! remove teflon thickness
	shape(2) = shape(2) - tefw_thk			! remove teflon thickness
	shape(3) = ag_rad				! remove teflon thickness
	call gsvolu('NMRY','TUBE',hd_tmed(32), shape, 3, i)		! silver
	call gspos ('NMRY',1,'NMRX',0.0,0.0,0.0,0,'ONLY')

	! put 3 wires at each end

	z = 2.65 + wire_rad
	call gspos ('NMRX',1,'TGVC',0.0,0.0,zk+z,0,'ONLY')
	call gspos ('NMRX',2,'TGVC',0.0,0.0,zk-z,0,'ONLY')

	z = 3.13 + wire_rad
	call gspos ('NMRX',3,'TGVC',0.0,0.0,zk+z,0,'ONLY')
	call gspos ('NMRX',4,'TGVC',0.0,0.0,zk-z,0,'ONLY')

	z = z + 2.0*wire_rad
	call gspos ('NMRX',5,'TGVC',0.0,0.0,zk+z,0,'ONLY')
	call gspos ('NMRX',6,'TGVC',0.0,0.0,zk-z,0,'ONLY')

	! add outer wrapping of teflon tape

	shape(1) = 1.76					! inner radius
	shape(2) = 1.76 + 0.04				! outer radius
	shape(3) = 3.28					! half length
	call gsvolu('NMRC','TUBE', hd_tmed(15), shape, 3, i)
	call gspos ('NMRC',1,'TGVC',0.0,0.0,zk,0,'ONLY')

	return
	end 

	!-----------------------------------------------------------

	subroutine create_target_cell(itmed,hd_tmed)

	implicit none
	integer hd_tmed(40), itmed

#include "ffpar_tg.inc"

	real tg_length, tg_zpos
	data tg_length / 5.0/           ! default
	data tg_zpos   / 0.0 /          ! relative to cryostat
	real ice_rad 			! (cm) radius of actual ice target
	data ice_rad / 1.26 /
	real klf_thk 			! (cm) thickness of KEL-F around ice target
	data klf_thk / 0.062 /		! (cm) thickness of KEL-F around ice target

	real z, shape(5)
	integer i

	! this routine defines the target & it's KEL-F cell
	! place all target cell volumes in the target vacuum with
	! additional z offset specified by user. Medium in target
	! cell and target medium length also user specified.

	if( tg_type_ff.eq.-1) then                      ! user defined
	   tg_length=tg_user_ff(1)
	endif
	shape(1) = 0.0					! inner radius
	shape(2) = ice_rad				! outer radius
	shape(3) = tg_length/2.0			! half-length
	z = tg_zpos	

	if(itmed.GE.29.and.itmed.LE.31) then
	call gsvolu('TRG1','TUBE', hd_tmed(itmed), shape, 3, i)
	else
	call gsvolu('TRG1','TUBE', hd_tmed(20), shape, 3, i)  !vacuum
	endif
	call gspos ('TRG1',1,'TGVC',0.0,0.0,z,0,'ONLY')
	tgpar_save(1) =tg_length
	tgpar_save(2) =ice_rad
	tgpar_save(6) =-tg_length/2
	tgpar_save(7) =tg_pos_ff(3)+tg_zpos +tgpar_save(6)
	tgpar_save(8) =tg_pos_ff(3)+tg_zpos +tgpar_save(6)
	tgpar_save(9) =tg_pos_ff(3)+tg_zpos +tgpar_save(7)
	tgpar_save(10)=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2) +ice_rad

	write(6,*)' HDice target: radius=',ice_rad,'; length=',tg_length
	if(itmed.eq.29) write(6,*)' target material: HD ice & wires'
	if(itmed.eq.30) write(6,*)' target material: H2 ice & wires'
	if(itmed.eq.31) write(6,*)' target material: D2 ice & wires'
	if(itmed.eq.20) write(6,*)' no target material: (vacuum)'
	write(6,*)' ***********************************************'

	! the downstream KEL-F cap on target

	shape(1) = 0.0					! inner radius
	shape(2) = ice_rad				! outer radius
	shape(3) = klf_thk/2.0				! half length
	z = tg_zpos + 2.5 + klf_thk/2.0		! position at end of target
	call gsvolu('TRG2','TUBE', hd_tmed(28), shape, 3, i)
	call gspos ('TRG2',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! the KEL-F tube around the ice target

	shape(1) = ice_rad				! inner radius
	shape(2) = ice_rad + klf_thk			! outer radius
	shape(3) = 2.95					! half length
	z = tg_zpos + 2.5 - 2.95			! align edge to downstream end of target cell
	call gsvolu('TRG3','TUBE', hd_tmed(28), shape, 3, i)
	call gspos ('TRG3',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! the KEL-F upstream outer conical transition

	shape(1) = 0.1					! half length of cone
	shape(2) = 1.485				! inner radius at -dz
	shape(3) = 1.485 + klf_thk			! outer radius at -dz
	shape(4) = ice_rad				! inner radius at +dz
	shape(5) = ice_rad + klf_thk			! outer radius at +dz
	z = z - 2.95 - 0.1				! center position of cone
	call gsvolu('TRG4','CONE', hd_tmed(28), shape, 5, i)
	call gspos ('TRG4',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! the outer upstream KEL-F tube

	shape(1) = 1.485				! inner radius
	shape(2) = 1.485 + klf_thk			! outer radius
	shape(3) = 1.265				! half length
	z = z - 0.1 - 1.265				! at upsteam up of target
	call gsvolu('TRG5','TUBE', hd_tmed(28), shape, 3, i)
	call gspos ('TRG5',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! the inner upstream KEL-F tube

	shape(1) = ice_rad				! inner radius
	shape(2) = ice_rad + klf_thk			! outer radius
	shape(3) = 1.15					! half length
	z = z - 1.265 + 1.15				! align edge at upstream end of tube
	call gsvolu('TRG6','TUBE', hd_tmed(28), shape, 3, i)
	call gspos ('TRG6',1,'TGVC',0.0,0.0,z,0,'ONLY')

	! the inner KEL-F upstream conical cap

	shape(1) = 0.6					! half length of cone
	shape(2) = ice_rad				! inner radius at -dz
	shape(3) = ice_rad + klf_thk			! outer radius at -dz
	shape(4) = 0.15					! inner radius at +dz
	shape(5) = 0.15 + klf_thk			! outer radius at +dz
	z = z + 1.15 + 0.6				! center position of cone
	call gsvolu('TRG7','CONE', hd_tmed(28), shape, 5, i)
	call gspos ('TRG7',1,'TGVC',0.0,0.0,z,0,'ONLY')

	return
	end 


