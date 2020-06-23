c	  This file defines parameters used in the ec timing calibration programs

      integer maxpoints
      parameter (maxpoints=20000)

      integer max_particles     !maximum number of particles program can handle
      parameter (max_particles=20000)

      integer tube
      parameter (tube = 36)     ! Number of tubes for which to allocate memory

      integer view
      parameter (view = 3)     ! Number of views for which to allocate memory

      integer sector
      parameter (sector = 6)    ! Number of sectors for which to allocate memory

      integer layer
      parameter (layer = 2)     ! Number of Layers (inner and outer) for memory

*     In the following, save one tdc, adc, etc for each event
      real tdc(maxpoints,tube,view,layer,sector),
     $   adc(maxpoints,tube,view,layer,sector),
     $   t_sc(maxpoints,tube,view,layer,sector),
     $	 beta(maxpoints,tube,view,layer,sector),
     $   distance(maxpoints,tube,view,layer,sector),
     $   volume(maxpoints,tube,view,layer,sector),
     $   factor(maxpoints,tube,view,layer,sector),
     $   dplane(maxpoints,tube,view,layer,sector),
     $   i_event
      real EcCal_Ech(sector,layer,view,tube),EcCal_Eo(sector,layer,view,tube),EcCal_atten(sector,layer,view,tube), EcCal_Tch(sector,layer,view,tube)
      integer tdc_input       ! If 1, will force the tdc slope to the one already in the database (default = 0)

      integer npts(tube,view,layer,sector),pdl_sc(maxpoints,tube,view,layer,sector)
 
      common/input_data/npts,tdc,adc,t_sc,beta,distance,volume,factor,dplane,EcCal_Ech,EcCal_Eo,EcCal_atten,EcCal_Tch,i_event,pdl_sc

      integer ec_tdc
      parameter (ec_tdc = 1)
      
      integer sc_time
      parameter (sc_time = 2)

      integer ec_layer
      parameter (ec_layer = 3)

      integer ec_tube_id
      parameter (ec_tube_id = 4)

      integer rf_1
      parameter (rf_1 = 5)

      integer rf_2
      parameter (rf_2 = 6)

      integer distance_to_pmt
      parameter (distance_to_pmt = 7)

      integer distance_to_target
      parameter (distance_to_target = 8)

      integer ec_adc
      parameter (ec_adc = 9)

      integer ec_x
      parameter (ec_x = 10)

      integer ec_y
      parameter (ec_y = 11)

      integer ec_z
      parameter (ec_z = 12)

      real ecscdist(2)          !  perpendicular distance between ec and sc,
      data ecscdist /0.6,1.1/  ! in ns, inner and outer

*     The distance between the back of the sc scintillator and the front
*     of the first calorimeter layer is 5.079", from Pete Hemler.
*

      logical first
      data first /.true./
      
      integer nwpawc
      parameter (nwpawc=20000000)

      real h
      common /pawc/ h(nwpawc)

*     define the unit vector for the particle
*     which defines its direction on impact

      real direction(3)

*     define perpendicular thicknesses, inner and outer layers, centimeters
      real perp_thickness(2)
      data perp_thickness /18.,33./

*     Velocity of light in scintillator, cm/ns
      real velocity
      parameter (velocity=18.1)


















