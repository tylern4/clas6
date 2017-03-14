      subroutine read_sf_file(rfile,iunit)

      implicit none

      include 'mpintp.inc'
      include 'spp.inc'

      character rfile*(*)
      integer iunit

      integer jj,j1,j2,nvar3,k,mpp,meth,nn
      real dumvar1,dumvar2,dumvar3,dumvar4
      character dum
      real ww,qq,wtmp
      
      real sff(62)
  
      if (iunit.eq.0) return
      
      method_spline   = 2
      method_helicity = 1
      
      open(unit=iunit,file=rfile,form='formatted',status='old')
     
      j1=1
      j2=0
      wtmp=0.
      nn=nvar1*nvar2
      
      do jj=1,nn
      
        read(iunit,*,end=999,err=1000) dum,ww,dum,qq
        
        if(ww.gt.wtmp) then
          j2=j2+1
          nv2=j2
        else
          j1=j1+1
          j2=1
          nv1=j1
        endif
        
        wtmp=ww
        var1(j1)=qq
        var2(j2)=ww
          
c SL+      
         read(iunit,'(a)',err=1000) dum
         read(iunit,*,err=1000) sff(1), sff(2),  
     &                          sff(3), sff(4),
     &                          sff(5), sff(6) 
         read(iunit,*,err=1000) sff(7),sff(8),
     &                          sff(9),sff(10),
     &                          sff(11),sff(12)
C SL-
         read(iunit,'(a)',err=1000) dum
         read(iunit,*,err=1000) dumvar1,dumvar2,
     &                          sff(13),sff(14),
     &                          sff(15),sff(16) 
         read(iunit,*,err=1000) sff(17),sff(18),
     &                          sff(19),sff(20),
     &                          sff(21),sff(22)
C EL+
         read(iunit,'(a)',err=1000) dum
         read(iunit,*,err=1000) sff(23),sff(24),
     &                          sff(25),sff(26),
     &                          sff(27),sff(28) 
         read(iunit,*,err=1000) sff(29),sff(30),
     &                          sff(31),sff(32),
     &                          sff(33),sff(34)
C EL-
         read(iunit,'(a)',err=1000) dum
         read(iunit,*,err=1000) dumvar1,dumvar2,
     &                          dumvar3,dumvar4,
     &                          sff(35),sff(36) 
         read(iunit,*,err=1000) sff(37),sff(38),
     &                          sff(39),sff(40),
     &                          sff(41),sff(42)
C ML+
         read(iunit,'(a)',err=1000) dum
         read(iunit,*,err=1000) dumvar1,dumvar2,
     &                          sff(43),sff(44),
     &                          sff(45),sff(46) 
         read(iunit,*,err=1000) sff(47),sff(48),
     &                          sff(49),sff(50),
     &                          sff(51),sff(52)
C ML-
         read(iunit,'(a)',err=1000) dum
         read(iunit,*,err=1000) dumvar1,dumvar2,
     &                          sff(53),sff(54),
     &                          sff(55),sff(56)
         read(iunit,*,err=1000) sff(57),sff(58),
     &                          sff(59),sff(60),
     &                          sff(61),sff(62)
     
        do mpp=1,max_mp
           sf(j1,j2,mpp) = sff(mpp)
        enddo
         
      enddo
     
999   close(iunit)

       print *, 'SF(Q2,W)=',nv1,nv2
       print *, 'W(MIN,MAX)=',var2(1),var2(nv2)
       print *, 'Q2(MIN,MAX)=',var1(1),var1(nv1)

C Now construct natural cubic splines in the 2nd dimension (VAR2) and
C calculate the 2nd derivatives wrt. VAR2 (2nd entry of each D2SF) of
C each structure function (SF). This is done only once, so do it here.

      if(method_spline.eq.1) then
      
        do k=1,max_mp
          call splie2(k,var2,sf,nv1,nv2,max_mp,d2sf)
        enddo
        
      elseif(method_spline.eq.2) then

        do j1=1,nv1                   ! Q2
          var(j1) = var1(j1)
        enddo
        
        do j2= nv1+1,nv1+nv2      ! W
          var(j2) = var2(j2-nv1)
        enddo
            
      endif
      
      return
      
 1000 print *, 'ERROR: End of file'
 
      return
      end
      