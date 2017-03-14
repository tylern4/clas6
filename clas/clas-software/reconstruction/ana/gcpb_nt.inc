C-
      Integer Maxparticles
      parameter(maxparticles=20)
        INTEGER gcpart              		! Nuber of GC particles 
        INTEGER pid(Maxparticles)   		! Particle ID
        REAL X(Maxparticles)        		! X coordinate of the hit
        REAL Y(Maxparticles)        		! Y coordinate of the hit
        REAL Z(Maxparticles)        		! Z coordinate of the hit
        REAL dedx(Maxparticles)    		! Energy
        REAL pX(Maxparticles)       		! X component of the momentum
        REAL pY(Maxparticles)       		! Y component of the momentum
        REAL pZ(Maxparticles)       		! Z component of the momentum
        REAL p_tot(Maxparticles)   		! Total momentum
        REAL x2(Maxparticles)       		! X2
        REAL theta(Maxparticles)   		! Theta
        REAL charge(Maxparticles)   		! Charge in Track
        REAL dca(Maxparticles)      		! dca  
        INTEGER index(Maxparticles) 		! index 
        REAL phi(Maxparticles)      		! Phi  
        REAL vtl(Maxparticles)      		! Visible Track Length mm 
        REAL sdist(Maxparticles)    		! Distance from Cathod mm
        REAL edist(Maxparticles)    		! Distance from First GEM mm
        INTEGER npts(Maxparticles)  		! Number of Points in Tracks  
	REAL r_0(Maxparticles)      		! Radius of Curviture
	INTEGER fiterr(Maxparticles)		! Helix fit Error
	INTEGER tothits(Maxparticles)		! Number of Hits
	INTEGER npd_track(Maxparticles)		! Number of Pads per Track
	INTEGER npd_event(Maxparticles)		! Number of Pads per Event
	INTEGER bonus_bits(Maxparticles)	! Bonus Bits	 
	REAL q_tot(Maxparticles)		! Total Charge in RTPC
		
        COMMON/gcpb_nt/gcpart,pid,x,y,z,dedx,px,py,pz,p_tot,x2,theta,
     +	     charge,dca,index,phi,vtl,sdist,edist,npts,r_0,
     +       fiterr,tothits,npd_track,npd_event,bonus_bits,q_tot
