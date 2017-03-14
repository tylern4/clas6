      subroutine processevt

      implicit none

#include "bcs.inc"
#include "wcs.inc"
#include "ffpar.inc"
#include "processevt.inc"


      integer i_part,i_mctk,i,id
      real    Pe,phi,theta,test
      integer mlink,pdg_to_geant

#define PART_NCOL iw(i_part-5)
#define PART_NROW iw(i_part-4)
#define PART_PID(i)    iw(i_part+ PART_NCOL*(i-1)+1)
#define PART_X(i)      rw(i_part+ PART_NCOL*(i-1)+2)
#define PART_Y(i)      rw(i_part+ PART_NCOL*(i-1)+3)
#define PART_Z(i)      rw(i_part+ PART_NCOL*(i-1)+4)
#define PART_E(i)      rw(i_part+ PART_NCOL*(i-1)+5)
#define PART_PX(i)     rw(i_part+ PART_NCOL*(i-1)+6)
#define PART_PY(i)     rw(i_part+ PART_NCOL*(i-1)+7)
#define PART_PZ(i)     rw(i_part+ PART_NCOL*(i-1)+8)
#define PART_Q(i)      rw(i_part+ PART_NCOL*(i-1)+9)
#define PART_TRKID(i)  iw(i_part+ PART_NCOL*(i-1)+10)
#define PART_QPID(i)   rw(i_part+ PART_NCOL*(i-1)+11)
#define PART_QTRK(i)   rw(i_part+ PART_NCOL*(i-1)+12)
#define PART_FLAGS(i)  iw(i_part+ PART_NCOL*(i-1)+13)

#define MCTK_CX(i)     rw(i_mctk+11*(i-1)+1)
#define MCTK_CY(i)     rw(i_mctk+11*(i-1)+2)
#define MCTK_CZ(i)     rw(i_mctk+11*(i-1)+3)
#define MCTK_PID(i)    iw(i_mctk+11*(i-1)+7)


C Get Jpart reference from BOS

      i_part = mlink(iw,'PART',0)
      i_mctk = mlink(iw,'MCTK',0)

C  If the first particle is an electron and is in a region of
C  zero acceptance, do not process the event through GSIM.  The
C  input banks are still included in the output file.
      
      if(i_part.gt.0) then

	 if((noacc_ff(1).ne.0.or.noacc_ff(2).ne.0.or.
     1       noacc_ff(3).ne.0).and.PART_PID(1).eq.3) then

	    Pe = sqrt(PART_PX(1)**2+PART_PY(1)**2+PART_PZ(1)**2)
	    phi=180/3.14159*atan2(PART_PY(1),PART_PX(1))
	    theta=180/3.14159*acos(PART_PZ(1)/Pe)                  
	    
 10	    if(phi.gt.30) then
	       phi=phi-60
	       goto 10
	    endif
	    if(phi.lt.-30) then 
	       phi=phi+60
	       goto 10
	    endif
	    test = noacc_ff(2)*phi**2 + noacc_ff(1)
	    if(theta.lt.test.and.(theta.lt.noacc_ff(3).or.
     1         noacc_ff(3).eq.0)) process_evt = .false.
	 endif

      elseif(i_mctk.gt.0) then

	 id=pdg_to_geant(MCTK_PID(1))
         if((noacc_ff(1).ne.0.or.noacc_ff(2).ne.0.or.
     1       noacc_ff(3).ne.0).and.id.eq.3) then
	 
	    phi=180/3.14159*atan2(MCTK_CY(1),MCTK_CX(1))
	    theta=180/3.14159*acos(MCTK_CZ(1))                  
	    
 20	    if(phi.gt.30) then
	       phi=phi-60
	       goto 20
	    endif
	    if(phi.lt.-30) then 
	       phi=phi+60
	       goto 20
	    endif
	    test = noacc_ff(2)*phi**2 + noacc_ff(1)
	    if(theta.lt.test.and.(theta.lt.noacc_ff(3).or.
     1         noacc_ff(3).eq.0)) process_evt = .false.
	 endif


      endif

      return
      end


