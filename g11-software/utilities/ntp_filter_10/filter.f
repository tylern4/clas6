c*************************************************
	SUBROUTINE filter(OK)
c*************************************************
	implicit none
	include "ntpl_goa.inc"
        INCLUDE "ntpl_structure.inc"
	logical OK,cuts(100)		      	
	real degrad
	real v_x1,v_y1,v_z1,v_x2,v_y2,v_z2,v_x,v_y,v_z,dd,dd1
	real vv_x,vv_y,vv_z
	real pkmx,pkmy,pkmz,pkmt
	real ppx,ppy,ppz,ppt
	real pkx,pky,pkz,pkt
	real pphx,pphy,pphz,ppht
	real plx,ply,plz,plt
	real pzpx,pzpy,pzpz,pzpt
	real pkx_old,pky_old,pkz_old,pkt_old
	real pkmx_mix,pkmy_mix,pkmz_mix,pkmt_mix
	real plx_mix,ply_mix,plz_mix,plt_mix
	real pzpx_mix,pzpy_mix,pzpz_mix,pzpt_mix
	real v_cm(0:7,3),  m_cm(0:7,4)
	real psi_cm(3)
	integer jj,i
	real pout(3),pin(3),vertex(3),pmass,dpk1,dpk2,dpn1,dpn2
	data degrad/0.0174533/
	
	OK = .FALSE.




c+ 63% reduction (0.7Mb)
	nprt=0
	ntbid=0
	nschit=0

c+ save pointer to TBT banks bewfore dropping DCPB
	do i=1,gpart
	   if(q(i).ne.0.AND.dc(i).gt.0) then
	      dc(i)=dc_trk(dc(i))
	      if(dc(i).lt.0) dc(i)=0
	   endif
	enddo

c+ 49%   (~0.5Mb)    
	dc_part=0
	ec_part=0
c+ 

c	cuts(1)=gpart.gt.1
	
	cuts(90)=.true.
c+ Filtering events
          if(
     %      cuts(90)     
     %      )  OK = .true.
	return
	end      	
	      	
	      	
	      	
	      	
      subroutine minimum_distance(x1,y1,z1,a1,b1,c1
     &                           ,x2,y2,z2,a2,b2,c2
     &                           ,vx1,vy1,vz1
     &                           ,vx2,vy2,vz2
     &                           ,vx,vy,vz,dmin)

      implicit none
      real x1,y1,z1,a1,b1,c1
      real x2,y2,z2,a2,b2,c2
      real vx1,vy1,vz1
      real vx2,vy2,vz2
      real vx,vy,vz,dmin
      
      real x3,y3,z3,a3,b3,c3,a0,b0,c0,d0,e0,f0,t1,t2,dist
      


**** define two lines in the space as l1=(x1,y1,z1)+(a1,b1,c1)*t
****                                  l2=(x2,y2,z2)+(a2,b2,c2)*t
**** define common perpendicular (a3,b3,c3) as vector product of the two versors
          a3= b1*c2-b2*c1
          b3=-a1*c2+a2*c1
          c3= a1*b2-a2*b1
          dist=sqrt(a3**2+b3**2+c3**2)
          a3=a3/dist
          b3=b3/dist
          c3=c3/dist
**** define vector between given points
          x3=x1-x2
          y3=y1-y2
          z3=z1-z2
**** define shortest distance
          dmin=abs(x3*a3+y3*b3+z3*c3)
**** find t1 and t2 values for closest points on the two lines
**** i.e. solve an equation system as
**** a0*t1+b0*t2+c0=0        e0*a0*t1+e0*b0*t2+e0*c0=0    e0*a0*t1-b0*d0*t1+e0*c0-b0*f0=0
**** d0*t1+e0*t2+f0=0        b0*d0*t1+b0*e0*t2+b0*f0=0
          a0=a1/a3-c1/c3
          b0=-a2/a3+c2/c3
          c0=(x1-x2)/a3-(z1-z2)/c3
          d0=a1/a3-b1/b3
          e0=-a2/a3+b2/b3
          f0=(x1-x2)/a3-(y1-y2)/b3
          t1=-(c0*e0-f0*b0)/(a0*e0-d0*b0)
          t2=-(c0*d0-f0*a0)/(b0*d0-e0*a0)

          vx1=x1+a1*t1
          vy1=y1+b1*t1
          vz1=z1+c1*t1

          vx2=x2+a2*t2
          vy2=y2+b2*t2
          vz2=z2+c2*t2
          
          vx=(vx1+vx2)/2.
          vy=(vy1+vy2)/2.
          vz=(vz1+vz2)/2.

          return
          end