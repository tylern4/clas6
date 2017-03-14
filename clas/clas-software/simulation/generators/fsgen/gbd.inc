	  REAL tecm,amass(18)
	  integer np,kgenev
	  COMMON/genin/np,tecm,amass,kgenev
	  REAL pcm(5,18)
	  REAL wt
	  COMMON/genout/pcm,wt
C     
C  Input to the Genbod.
C     
C     np              =       the same as a npart
C     tecm            =       total CM energy
C     amass           =       masses of outgoing particles
C     kgenev
C  and output from   
C     pcm(1,i)        =       px of outgoing particle
C     pcm(2,i)        =       py
C     pcm(3,i)        =       pz
C     pcm(4,i)        =       energy
C     pcm(5,i)        =       momenta
C     wt              =       weight of the event
