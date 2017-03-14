      program fout_map
c
      integer i,j,svls,ll,jj,kk,ii,mm
      real final(11,36,3,2,6)
      character*17 ftadc(6)
      character*16 ftch(6)
      character*15 ft0(6)
      character*17 ftrms(6)
      character*16 ftd1(6)
      character*16 ftd2(6)
      data ftadc/'EC_TadcInnerU.dat','EC_TadcInnerV.dat',
     +'EC_TadcInnerW.dat','EC_TadcOuterU.dat',
     +'EC_TadcOuterV.dat','EC_TadcOuterW.dat'/
      data ftch/'EC_TchInnerU.dat','EC_TchInnerV.dat',
     +'EC_TchInnerW.dat','EC_TchOuterU.dat',
     +'EC_TchOuterV.dat','EC_TchOuterW.dat'/
      data ft0/'EC_ToInnerU.dat','EC_ToInnerV.dat',
     +'EC_ToInnerW.dat','EC_ToOuterU.dat',
     +'EC_ToOuterV.dat','EC_ToOuterW.dat'/
      data ftd1/'EC_dT1InnerU.dat','EC_dT1InnerV.dat',
     +'EC_dT1InnerW.dat','EC_dT1OuterU.dat',
     +'EC_dT1OuterV.dat','EC_dT1OuterW.dat'/
      data ftd2/'EC_dT2InnerU.dat','EC_dT2InnerV.dat',
     +'EC_dT2InnerW.dat','EC_dT2OuterU.dat',
     +'EC_dT2OuterV.dat','EC_dT2OuterW.dat'/
      data ftrms/'EC_TrmsInnerU.dat','EC_TrmsInnerV.dat',
     +'EC_TrmsInnerW.dat','EC_TrmsOuterU.dat',
     +'EC_TrmsOuterV.dat','EC_TrmsOuterW.dat'/
c
      open(11,file='final.out',status='old')
c
      do ll=1,6
        do jj=1,3
          do kk=1,2
            do ii=1,36
              read(11,*)svls,
     $           (final(mm,ii,jj,kk,ll),mm=1,11)
              
            enddo
          enddo
        enddo
      enddo
      close(11)
c
      do i=1,3
         do j=1,2
            n=i+(j-1)*3
            open(21,file=ftadc(n),status='unknown')
            open(22,file=ftch(n),status='unknown')
            open(23,file=ft0(n),status='unknown')
            open(26,file=ftrms(n),status='unknown')
            open(24,file=ftd1(n),status='unknown')
            open(25,file=ftd2(n),status='unknown')
            do ll=1,6
               do ii=1,36
                  write(21,*)final(3,ii,i,j,ll)
                  write(22,*)final(1,ii,i,j,ll)/1000.
                  write(23,*)final(2,ii,i,j,ll)
                  write(24,*)final(4,ii,i,j,ll)
                  write(25,*)final(5,ii,i,j,ll)
                  write(26,*)final(11,ii,i,j,ll)
               enddo
            enddo
            close(21)
            close(22)
            close(23)
            close(24)
            close(25)
            close(26)
         enddo
      enddo
      end
