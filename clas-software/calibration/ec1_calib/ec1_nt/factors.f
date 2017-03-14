      SUBROUTINE factors(OutFile,pedest)

      IMPLICIT none
c
c calibration constatnts      
c
      real *4 A_l , A_r , da_l , da_r ,cT_l ,cT_r , dT_l , dT_r
      common /calib_const/  A_l(2,4,40), A_r(2,4,40),
     &                     da_l(2,4,40),da_r(2,4,40),
     &                     cT_l(2,4,40),cT_r(2,4,40),
     &                     dT_l(2,4,40),dT_r(2,4,40)

      INTEGER*4 pedest(2,2,1088)

      REAL      *4  Mpos(41,4,2)
      REAL      *4  dMpos(41,4,2)
      REAL      *4  Mean(4,2)
      INTEGER   *4  i,unit,id_1
      INTEGER   *4  ind,sect,ilayer,nstack
      CHARACTER *20 txt(40,4,2)
      CHARACTER *12 lay_txt(4)
      REAL      *4  delta(40,4,2),d_tot(40,4,2),delta0(4,2)
      REAL      *4  calib_e     
      CHARACTER *80 OutFile, fact_txt, fact_dat,filename
      DATA          lay_txt/'Inner Long','Inner Short','Outer Long','Outer Short'/
      unit=4

      fact_txt = OutFile(1:index(OutFile,' ')-1)//'_fact.txt'
      open (unit,file=fact_txt,status='unknown', err=20)
c      
      DO sect=1,2
        DO ilayer=1,4
          call hunpak(100+10*sect+ilayer,Mpos(1,ilayer,sect),'HIST',0)
          call hunpke(100+10*sect+ilayer,dMpos(1,ilayer,sect),'HIST',0)
      
      call IZERO(Mean,8) 
      ind  = 0
      IF(ilayer .eq. 1 .or. ilayer .eq. 3) nstack = 24
      IF(ilayer .eq. 2 .or. ilayer .eq. 4) nstack = 40

        DO i=1,nstack
          mean(ilayer,sect) = mean(ilayer,sect) + Mpos(i,ilayer,sect)
c ind here is a number of stacks with non zero signals
          IF (Mpos(i,ilayer,sect).gt.0.) ind = ind+1
        END DO
        mean(ilayer,sect) = mean(ilayer,sect)/ind
c
c  shift of mean values from calibration level of 27.55 Mev       
c  new calib. level 8*1.5*2.2=26.4 Mev (energy loses in SC ~2.2 MeV/cm)
c
        calib_e = 26.4*0.78
c
        delta0(ilayer,sect) = (calib_e - mean(ilayer,sect))/mean(ilayer,sect)
        
        IF(abs(delta0(ilayer,sect)).lt.0.02) delta0(ilayer,sect) = 0.
        
        DO i=1,nstack
          txt(i,ilayer,sect)='   OK '
          IF (Mpos(i,ilayer,sect).gt.0.) then
            delta(i,ilayer,sect) = (mean(ilayer,sect) - Mpos(i,ilayer,sect))
     &                             /Mpos(i,ilayer,sect)
            d_tot(i,ilayer,sect) = delta(i,ilayer,sect) + delta0(ilayer,sect) 
     &                           + delta(i,ilayer,sect) * delta0(ilayer,sect)
            IF(abs(delta(i,ilayer,sect)).gt.0.05) txt(i,ilayer,sect)=' *Must be changed!*'
          ELSE
            delta(i,ilayer,sect) = 0.
            d_tot(i,ilayer,sect) = 0.
            txt  (i,ilayer,sect) = ' Nothing to say...'
          END IF
        END DO
        
        write (4,*) ' Sector ',sect,' Layer ',ilayer
        write (4,*) lay_txt(ilayer),', Mean =',mean(ilayer,sect)
     &             ,',  delta0 =',delta0(ilayer,sect)
        write (4,'(i4,3f10.2,5x,a20,f10.2)')
     &        (i,Mpos (i,ilayer,sect),dMpos(i,ilayer,sect)
     &          ,delta(i,ilayer,sect),txt  (i,ilayer,sect)
     &          ,d_tot(i,ilayer,sect),i=1,nstack)

        END DO
      END DO
      close (unit)
c      unit=5
      fact_dat = OutFile(1:index(OutFile,' ')-1)//'_fact.dat'
      open (unit,file=fact_dat,status='unknown', err=20)
      write (unit,*) ' Sector  Layer  PMT  Mean  RMS/mean  D(mean)  D(500)'
c  write ec1.calib.dat
c      filename='ec1calib.dat.new'
      filename='ec1calib.'//OutFile(1:index(OutFile,' ')-1)
      open (unit=22,file=filename,status='unknown')
      do sect=1,2
        do ilayer=1,4
          if (ilayer .eq. 1 .or. ilayer .eq. 3) nstack = 24
          if (ilayer .eq. 2 .or. ilayer .eq. 4) nstack = 40
          do i=1,nstack

c           A_l(sect,ilayer,i)= A_l(sect,ilayer,i)*(1+d_tot(i,ilayer,sect))
c           A_r(sect,ilayer,i)= A_r(sect,ilayer,i)*(1+d_tot(i,ilayer,sect))

	    id_1  = 256*ilayer+i
            write (22,'(3i4,2i8,8f9.4)') sect,ilayer,i,pedest(sect,1,ID_1),
     &                                                 pedest(sect,2,ID_1),
     &                                           A_l (sect,ilayer,i),
     &                                           A_r (sect,ilayer,i),
     &                                           dA_l(sect,ilayer,i),
     &                                           dA_r(sect,ilayer,i),
     &                                           cT_l(sect,ilayer,i),
     &                                           cT_r(sect,ilayer,i),
     &                                           dT_l(sect,ilayer,i),
     &                                           dT_r(sect,ilayer,i) 

        write (unit,'(3i6,3f11.3,5x,f11.3)')
     &        sect,ilayer,i,Mpos (i,ilayer,sect),dMpos(i,ilayer,sect)
     &          ,delta(i,ilayer,sect)
     &          ,d_tot(i,ilayer,sect)
          
          end do
        end do
      end do
      close (unit)
      RETURN
 20   print *,'ERROR while opening file ',fact_txt,' or ',fact_dat
      RETURN
      END
