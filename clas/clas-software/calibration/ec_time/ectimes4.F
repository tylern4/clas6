      program ectimes4

      implicit none
#include "bcs.inc"
      integer iread,iwrite,ipunch,i,j,k,l
      common /my_units/ iread,iwrite,ipunch
      data iread/10/, iwrite/20/, ipunch/30/
      
#include "ectimes2.inc"
      integer current_sector,in_out,uvw,current_strip
      common /current/ current_sector,in_out,uvw,current_strip
      common /final_params/ final
      real final(15,36,3,2,6)
      integer ii,jj,kk,ll,mm,icycle,istat
      integer read_input,file2proc,maxproc,file2check
      external read_input
      real dummy
C     
      call bos(iw,700000)
c     Write out results:
      call  hlimit(nwpawc)
      call hropen(2,'PLOTS','plots.hbook','N',1024,istat)
c     write(6,*)'istat=',istat
      if(istat.ne.0)then
         print*,'Error in opening histogram file - istat = ',istat
      endif
      do j=1,2
         do k=1,3
c     call hbook2(150+(j-1)*10+k,'',50,0.,2000,217,0.,217.,0.)
         enddo
      enddo      
      open(unit=99,file='datafiles',status='unknown',err=999)
      print*,'File is opened ...'
      tdc_input = 0      ! Default
      read(unit=99,fmt='(4I10)')maxproc,file2proc,file2check,tdc_input
      if(tdc_input.gt.0) write(6,*)'Using EC tdc slopes from calibration database'
      if(file2proc.gt.0)then
         write(6,*)'Only ',maxproc,' from each file will be processed'
c     go to 333
         if(read_input(file2proc,maxproc).le.0) then ! read input from BOSfiles
            print *, 'No input data at all ====='
            stop
         endif
         
         current_sector = 1
         in_out = 1
         uvw = 1
         current_strip = 25
         
c     Fit data for each stack:
         do l=1,6
            do j=1,3
               do k=1,2
                  do i=1,36            
                     current_sector=l
                     in_out=k
                     uvw=j
                     current_strip=i
                     if(npts(current_strip,uvw,in_out,current_sector).ge.50)then
                        call create_cards(ecCal_Tch(current_sector,in_out,uvw,current_strip), tdc_input)
                        call do_minuit
                     endif
                  enddo
               enddo
            enddo
         enddo
c     
         open(11,file=
     $        'final.out',
     $        status='unknown')
         
         do ll=1,6
            do jj=1,3
               do kk=1,2
                  do ii=1,36
                     if((final(3,ii,jj,kk,ll) .ge. 0.0) .or. (final(3,ii,jj,kk,ll) .le. 0.0)) then
                       write(11,200)ll*10000+jj*1000+kk*100+ii,
     $                      (final(mm,ii,jj,kk,ll),mm=1,11)
                     else
                       write(11,200)ll*10000+jj*1000+kk*100+ii,
     $                      (0.00000E+00,mm=1,11)
                     endif
                  enddo
               enddo
            enddo
         enddo
         close(11)
 200     format(1X,I5,11(1X,E12.5))
         do ll=1,6
            do kk=1,2
               do jj=1,3
                  do ii=1,36
                     write(87,*)ll,kk,jj,ii,final(1,ii,jj,kk,ll),final(9,ii,jj,kk,ll),
     +                    final(7,ii,jj,kk,ll),final(11,ii,jj,kk,ll),final(12,ii,jj,kk,ll)
                  enddo
               enddo
            enddo
         enddo
      else
         write(6,*)'No calibration. Will do time reconstruction based on the existing final.out'
         close(99) 
      endif
      if(file2check.gt.0)then
 333     call rebook
         call retime(dummy)
      endif
c     
      call hcdir('//PLOTS',' ')
      call hrout(0,icycle,'T')
c     
      close(2)
      close(10)
      close(20)
      close(30)
 999  continue
      call good_bye

      end
      
************************************************************
      integer function read_input(file2proc,maxproc)

      implicit none
      save

      character*100 filename(50)
cstep      common /what_file/ filename

      real u,v,w,xi,yi
      real u_exit,v_exit,w_exit,xi_exit,yi_exit

      character*10 char_var(64)
      real pi
      data pi/3.14159265/
      integer maxsectstrip
      data maxsectstrip/5/
      logical tomany

      integer current_sector,in_out,uvw,current_strip
c     
#include "ntuple_commons.inc"
#include "ectimes2.inc"
#include "bcs.inc"
      
      integer istat
      integer ii,iii,jj,jjj,ihits
      integer i_particle
      integer npt,maxproc,file2proc
      integer max_adc(view,layer)
      integer max_adc_id(view,layer)
      integer nami, ind
      real temp,pu,pv,pw
      real thickness,temp2
      real r_exit(3),rx,ry,rz,dr
      real exit_distance,chnumb
      real perp(sector,3)
      real find_distance,pu_exit,pv_exit,pw_exit
      character*99 rmap
      integer NFILES
      integer ifile
      integer handle,count,count1,leng,first_read
      integer length,npch(2,3)
      integer bosread
      external bosread
      integer mamind
      external mamind

c     This include file defines a number of constants
#include "initializations.inc"
c     
      print*,'Initializing ...'
      NFILES=0
      read_input=0
c      open(unit=99,file='datafiles',status='unknown',err=999)
c      print*,'File is opened ...'
c      read(unit=99,fmt='(2I10)',err=998,end=998)maxproc,file2proc
 100  read(unit=99,fmt='(A)',err=998,end=998) filename(NFILES+1)
      write(6,*)'File ',NFILES+1,' is ',filename(NFILES+1)
      NFILES=NFILES+1
      if(nfiles.lt.file2proc)goto 100
 998  close(99)
      if(NFILES.eq.0) goto 999
      write(6,*)NFILES,' files will be processed...'

c     The array 'npts' counts useful data points for each
c     tube, view, layer, and sector
      do ii = 1, tube
         do iii = 1, view
            do jj = 1, layer
               do jjj = 1, sector
                  npts(ii,iii,jj,jjj)= 0
                  if(ii.eq.1.and.iii.eq.1)
     +                 call hbook2(300000+jjj*10+jj,'',3,1.,4.,20,0.,20.,0.)
               enddo
            enddo
         enddo
      enddo
      call hbook2(999997,'',36,1.,37.,36,1.,37.,0.)
      call hbook2(999998,'',36,1.,37.,36,1.,37.,0.)
      call hbook2(999999,'',36,1.,37.,36,1.,37.,0.)

C     endif                     ! end of initialization
      count=0
      count1=0
      first_read=0
C.... Loop over NFILES to read in
      do 10 ifile=1,NFILES
         count=0
         count1=0
         leng=length(filename(ifile))
         write(6,*)'Input file is  ',filename(ifile)(1:leng)
         istat = -1
         CALL BOSOPEN(filename(ifile)(1:leng),'r',Handle)
         read_input=read_input+1
         write(6,*) 'Reading data file - ', filename(ifile)(1:leng)
 11      continue
         istat=BOSREAD(Handle,IW,'E')
         if(istat.ne.0) then
            if(istat.eq.-1)go to 12 ! EOF, to next file
            write(6,*)'File ',filename(ifile)(1:leng),' corrupted!'
            goto 12
         else
            if(count.gt.maxproc)go to 12
            count=count+1
            count1=count1+1
            if(count1.eq.10000)then
               count1=0
               write(6,*)count,'  events read'
            endif
            call fill_seb_nt
         endif


         if(first_read.eq.0) then
            first_read = 1
            nami=mamind(iw,'HEAD')
            ind=0
            if(nami.gt.0)ind=IW(nami)
            if(ind.gt.0)then
               call revinm('CLAS_PARMS','Maps/EC_CALIB.map',rmap)
               write(6,*)'Reading in tdc constants from map for run #', iw(ind+2)
               call ecreadtdcs(iw(ind+2),rmap)
            endif
         endif


c     Select the desired particle type:
         if(ecpc_nhit.lt.1.or.ecpc_nhit.gt.300.or.gpart.eq.0)go to 2 ! abandon entire event
         if(dc(1).gt.0.and.dc_stat(dc(1)).gt.0)then
         do i_particle = 1, gpart ! loop over geometrically matched particles
            if(ec(i_particle).le.0.or.ec(i_particle).gt.gpart) go to 1 ! skip to next particle
            if(id(i_particle).ne.11.and.abs(id(i_particle)).ne.211.and.id(i_particle).ne.2212)go to 1 ! skip to next particle
            if(sc(i_particle).eq.0.or.sc(i_particle).gt.gpart) go to 1 ! skip to next particle
            if(sc_stat(sc(i_particle)).ne.33.and.p(i_particle).lt.0.7)go to 1
            if(dc(i_particle).le.0.or.dc(i_particle).gt.gpart) go to 1 ! skip to next particle
            if(dc_stat(dc(i_particle)).le.0)go to 1 ! skip non time based tracks
            if(dc_sect(dc(i_particle)).ne.ec_sect(ec(i_particle))) go to 1
c
c            if(dc_sect(dc(i_particle)).eq.2.and.(sc_pd(sc(i_particle)).eq.2)) go to 1
c            if(dc_sect(dc(i_particle)).eq.6.and.(sc_pd(sc(i_particle)).eq.2)) go to 1
c            if(dc_sect(dc(i_particle)).eq.3.and.(sc_pd(sc(i_particle)).eq.11)) go to 1
c            if(dc_sect(dc(i_particle)).eq.5.and.(sc_pd(sc(i_particle)).eq.16)) go to 1
c     At this point we've found the desired particle, geometrically matched to 
c     the desired detectors. Now find calorimeter hit corresponding to this particle.
c     Require one and only one calorimeter hit in this particle's sector.
c     write(6,*)'ec_part= ',ec_part
            ihits = 0
            do iii = 1,ec_part  ! count ec hits in this sector
               if(ec_sect(iii).eq.ec_sect(ec(i_particle)))ihits=ihits+1
            enddo
            if(ihits.ne.1)go to 1 ! skip to next particle
c     print *,'EC hits ',ihits
c     At this point, the desired particle has been identified and it
c     is associated with a calorimeter hit in a sector which has a 
c     single calorimeter hit. Now look at the ecpc information.

c     Initialize the following arrays:
            do ii = 1, view
               do jj = 1, layer
                  max_adc(ii,jj) = 0 ! Only consider adcs with non-zero values
                  max_adc_id(ii,jj) = 0
               enddo
            enddo

c     Loop over the contents of ecpc, hoping to find one strip in each view 
c     (for inner and outer) which has the maximal adc value.
c     write(6,*)'ecpc_nhit= ',ecpc_nhit
            do jj=1,2
               do jjj=1,3
                  npch(jj,jjj)=0
               enddo
            enddo
            tomany=.false.
c     
            do iii = 1,ecpc_nhit
               
c     Decode the ecpc hit information to find out the sector and layer of the hit.
c     The existence of only one hit in this sector is tacitly assumed here.
               current_sector = ecpc_hit(iii)/100
               
c     Check to see if this hit is in the right sector:
               if(current_sector.ne.ec_sect(ec(i_particle)))go to 3 ! skip to next ecpc hit
c     Check for non-zero tdc:
               if(ecpc_tdc(iii).lt.1)go to 3 ! skip to next ecpc hit
               
               in_out = (ecpc_hit(iii)-current_sector*100)/10 ! find layer
               
               uvw = ecpc_id(iii)/100 ! find view
               
               current_strip = ecpc_id(iii)-100*uvw ! find stack number
               npch(in_out,uvw)=npch(in_out,uvw)+1
               tomany=npch(in_out,uvw).gt.maxsectstrip
               if(max(max_adc(uvw,in_out),ecpc_adc(iii)).gt.
     $              max_adc(uvw,in_out))then ! found a new candidate
                  max_adc_id(uvw,in_out)=iii ! Save id of max adc
                  max_adc(uvw,in_out)=ecpc_adc(iii)
               endif

 3             continue
            enddo
c     
            current_sector = ec_sect(ec(i_particle)) ! The correct sector for the remaining part of this routine
            do jj=1,2
               do jjj=1,3
                  call hf2(300000+current_sector*10+jj,float(jjj),float(npch(jj,jjj)),1.)
               enddo
            enddo
c     
            if(tomany)go to 1
c     Now loop over the views and layers, and save adc's, tdc's etc.
            do in_out = 1, layer
               do uvw = 1, view
                  if(max_adc_id(uvw,in_out).eq.0)go to 4 ! no data, go to next ecpc hit
                  current_strip = ecpc_id(max_adc_id(uvw,in_out))-100*uvw ! find stack number
                  
                  if (npts(current_strip,uvw,in_out,current_sector)
     $                 .eq.maxpoints)go to 4 ! already have enough points, go to next ecpc hit
                  npts(current_strip,uvw,in_out,current_sector) = ! increment counter
     $                 npts(current_strip,uvw,in_out,current_sector)+1
                  npt = npts(current_strip,uvw,in_out,current_sector) ! shorthand notation
c     print *,'Nentr ',npt
                  tdc(npt,current_strip,uvw,in_out,current_sector)
     $                 = float(ecpc_tdc(max_adc_id(uvw,in_out))) ! Save this tdc value

                  adc(npt,current_strip,uvw,in_out,current_sector)
     $                 = float(ecpc_adc(max_adc_id(uvw,in_out))) ! Save this adc value
                  
                  t_sc(npt,current_strip,uvw,in_out,current_sector) 
     $                 = sc_t(sc(i_particle)) ! Save the sc time

                  pdl_sc(npt,current_strip,uvw,in_out,current_sector) 
     $                 = sc_pd(sc(i_particle)) ! Save the sc time
                  
                  beta(npt,current_strip,uvw,in_out,current_sector)
     $                 = b(i_particle)
                  chnumb=(current_sector-1)*36+current_strip
c
c                  if(uvw.eq.1)then
c     Fixed a bug that was causing worse resolutions for V and W strips.
c     Now doing all the following calculations for each view (U, V, W) 
c     instead of doing them only for the U view.  (H.-S. Jo  March 2006)

C     - Get the direction for tracking. Note the directions are in the sector 1.
                  direction(1)=dc_cxsc(dc(i_particle))*cos(pi*(current_sector-1)/3.)
     -                 -dc_cysc(dc(i_particle))*sin(pi*(current_sector-1)/3.)      
                  direction(2)=dc_cysc(dc(i_particle))*cos(pi*(current_sector-1)/3.)
     +                 +dc_cxsc(dc(i_particle))*sin(pi*(current_sector-1)/3.)      
                  direction(3)=dc_czsc(dc(i_particle))
                     
c     Calculate the thickness traversed through the calorimeter layer
c     Again direction vector in the defined in the sector 1, so perp as well.
                  temp=direction(1)*perp(current_sector,1) ! dot product here
                  temp=temp+direction(2)*perp(current_sector,2) ! dot product
                  temp=temp+direction(3)*perp(current_sector,3) ! dot product
c                                    temp2=direction(1)*perp(current_sector,1)
c                                    temp2=temp2 + direction(2)*perp(current_sector,2)
c                                    temp2=temp2 + direction(3)*perp(current_sector,3)
c                                    write(6,*)'temp2 = ', temp2
                     
                  thickness = perp_thickness(in_out)/temp ! Thickness in cm

c     Calculate the point of at the layer:
                  dr=(thickness-perp_thickness(1)/temp)
                  rx=ech_x(ec(i_particle))+direction(1)*dr
                  ry=ech_y(ec(i_particle))+direction(2)*dr
                  rz=ech_z(ec(i_particle))+direction(3)*dr
                  dplane(npt,current_strip,uvw,in_out,current_sector)=ec_r(ec(i_particle))+dr
     -                 -sc_r(sc(i_particle))
c     Calculate the vector to the exit point of the stack:
                  r_exit(1)=ech_x(ec(i_particle))+direction(1)*thickness
                  r_exit(2)=ech_y(ec(i_particle))+direction(2)*thickness
                  r_exit(3)=ech_z(ec(i_particle))+direction(3)*thickness       
                     
c     In the following, calculate local coordinates from the global coordinates, assuming
c     the ideal geometry. A better way would be to have the local coordinates directly 
c     from the data file, in case some day we use the survey-corrected geometry.
                     
c     Calculate the local coordinates of the entrance point:
                     
                  call ec_xyz_duvw(rx,ry,rz,u,v,w,xi,yi,in_out)
c     print *,rx,ry,rz,u,v,w,xi,yi,in_out,current_sector
                     
c     Calculate the distances to the phototube edge 
c     for the entrance point and the exit point.
                  call Ec_ij_path(yi,xi,pu,pv,pw,in_out,current_sector)

                  if(uvw.eq.1)then
                     distance(npt,current_strip,uvw,in_out,current_sector) = pu
                  elseif(uvw.eq.2)then
                     distance(npt,current_strip,uvw,in_out,current_sector) = pv
                  elseif(uvw.eq.3)then
                     distance(npt,current_strip,uvw,in_out,current_sector) = pw
                  endif
c
                  
c     Now calculate the "volume" quantity
                  
c     I believe the calculation of the volume term is flawed, because
c     for tracks calculated to come from the target, the distance to the
c     readout edge is sometime greater for the innermost layer compared to the 
c     outermost layer, while it should always be less, by contruction. Need to 
c     fix this as soon as the volume term is actually used. WB, Dec 1998.              
c     Calculate the local coordinates of the exit point:
                  call ec_xyz_duvw(r_exit(1),r_exit(2),r_exit(3),
     $                 u_exit,v_exit,w_exit,xi_exit,yi_exit,in_out)
                     
                  call Ec_ij_path(yi_exit,xi_exit,pu_exit,pv_exit,pw_exit,in_out,current_sector)

                  if(uvw.eq.1)then
                     volume(npt,current_strip,uvw,in_out,current_sector)
     $                 =-thickness/30.+
     $                 (distance(npt,current_strip,uvw,in_out,current_sector)-
     $                 pu_exit)/velocity
                  endif
                  factor(npt,current_strip,uvw,in_out,current_sector) 
     $                    = temp ! save this to adjust TOF time

                  if(uvw.eq.2)then
                     volume(npt,current_strip,uvw,in_out,current_sector)
     $                 =-thickness/30.+
     $                 (distance(npt,current_strip,uvw,in_out,current_sector)-
     $                 pv_exit)/velocity
                  elseif(uvw.eq.3)then
                     volume(npt,current_strip,uvw,in_out,current_sector)
     $                 =-thickness/30.+
     $                 (distance(npt,current_strip,uvw,in_out,current_sector)-
     $                 pw_exit)/velocity
                  endif

 4                continue
c               print *,distance(npt,current_strip,uvw,in_out,current_sector),uvw,
c     +npt,current_strip,in_out,current_sector,evntid
               enddo
            enddo               ! end of loops over ecpc max-adc hits
            
c     call hcdir('//INFILE',' ')
            
 1          continue
         enddo                  ! end of loop over identified particles in this event
         endif
 2       call BDROP(IW,'E')
         call BGARB(IW)
         go to 11               ! end of loop over all events      
 12      call BDROP(IW,'E')
         call BGARB(IW)
         call BOSCLOSE(Handle,0)
         write(6,*)'Finished reading data file - ',ifile,filename(ifile)(1:leng)
 10   continue
 999  return
      end 
************************************************************
      integer function length(string)
      implicit none
      save
      character*100 string
      integer Ilen
      do 510,Ilen = LEN(string),1,-1
         if(string(Ilen:Ilen).ne.' ') goto 520
 510  continue
 520  length=Ilen
      end
************************************************************
      
      subroutine create_cards(tdcslope, tdc_input)
      
      implicit none
      real tdcslope
      integer tdc_input

*     Create the card file for minuit.
      
      open (unit=16, file='minuit.input', status='unknown')
 
      write(16,101) 'ectimes2'
 101  format(a)
 
c     The order of the parameters is as follows:

c     parameter number  
c     parameter name    
c     start value 
c     starting step size or expected uncertainty
c     lower bound (optional)
c     upper bound (optional)
c
c     Example:
c      write(16,102) '1', 'a',  0.1, .01
c      write(16,102) '2', 'b',  0.1, .01
 
      if(tdc_input.eq.1) then
        write(16,113) '1', 'a', tdcslope, 0.005, tdcslope - 0.01, tdcslope + 0.01
        write(16,102) '2', 'b', -57.,  3.0
        write(16,102) '3', 'c', -35.,  3.0
        write(16,113) '4', 'd',  0.,  0.0001, -0.001, 0.001
        write(16,113) '5', 'e',  0., 0.000001, -0.00001, 0.00001
      else
        write(16,102) '1', 'a',  98.1,   0.1
c       above was 50 for fastbus
c        write(16,113) '1', 'a',  98.1,   0.1, 97.9, 98.4
        write(16,102) '2', 'b', -40.,  3.0
c       above was 10 for fastbus
        write(16,102) '3', 'c', -40.,  3.0
        write(16,102) '4', 'd',  0.,  0.1
        write(16,102) '5', 'e',  0., 0.1
c        write(16,113) '4', 'd',  0.,  0.0001, -0.001, 0.001
c        write(16,113) '5', 'e',  0., 0.000001, -0.00001, 0.00001
        write(16,102) '4', 'd',  0.,  0.1
        write(16,102) '5', 'e',  0., 0.1
      endif

102   format(a,t11,a10,t21,f6.1,t31,f6.1)
104   format(a,t11,a10,t21,f12.8,t31,f12.8)
112   format(a,t11,a10,t21,f6.1,t31,f6.1,t41,f6.1,t51,f6.1)
113   format(a,t11,a10,t21,f12.8,t31,f12.8,t41,f12.8,t51,f12.8)
 
      write(16,103) '          '
103   format(a)
 
c      write(16,103) 'fix 4'

      write(16,*) 'set printout -1'
      write(16,105) 'minimize',10000., 1.

c      write(16,105) 'release 3'

      write(16,103) 'end return'
105   format(a,t21,f9.1,t31,f5.1)
C105   format(a,t11,f,t21,f,t31,f,t41,f,t51,f)
 
      close (unit=16)
 
!     minuit card file was created
 
      return
      end
 
************************************************************

      subroutine do_minuit
      
      implicit none
      INTEGER IQUEST
      COMMON/QUEST/IQUEST(100)

      common /my_units/ iread,iwrite,ipunch
      integer iread,iwrite,ipunch
      common /current/ current_sector,in_out,uvw,current_strip
      integer current_sector,in_out,uvw,current_strip
      integer itemp,mpar,i,imin,imax
      common /numpar/ mpar
      common /final_params/ final
      real final(15,36,3,2,6)
      real temp1,temp2,fmin,fmax,statv(2), hstati
      external fcn, hstati
      external ectime
      external hcdir
      real par(3),sigpar(3),chi2

#include "ectimes2.inc"

!     open the input, output and punch files for minuit
 
      open (unit=iread, file='minuit.input',  status='old')
      open (unit=iwrite, file='minuit.output', status='unknown')
      open (unit=ipunch, file='minuit.punch',  status='unknown')
 
!     invoke minuit
 
      call mintio(iread,iwrite,ipunch)

C      call mninit
      call minuit(fcn,ectime)

      close (unit=10)
      close (unit=20)
c     close (unit=30)


      itemp = current_sector*10000
      itemp = itemp + uvw*1000
      itemp = itemp + in_out*100
      itemp = itemp + current_strip

c     Set initial guess for gaussian fit based on previously obtained
c     mean, rms, and area.
      temp1 = final(mpar+4,current_strip,uvw,in_out,current_sector)
      temp2 = final(mpar+2,current_strip,uvw,in_out,current_sector)

      par(1)= temp1/(2.35*temp2)
      par(2)= final(mpar+1,current_strip,uvw,in_out,current_sector)
      par(3)= final(mpar+2,current_strip,uvw,in_out,current_sector)

c     Fit the resulting distribution to estimate the error and mean
      call hcdir('//PLOTS',' ')
c      call hfithn(itemp,'g','F',3,par,0,0,0,sigpar,chi2)
      do i=1,2
         statv(i)=hstati(itemp,i,'',0)
      enddo
      fmin=statv(1)-3.*statv(2)
      fmax=statv(1)+3.*statv(2)
      call hxi(itemp,fmin,imin)
      call hxi(itemp,fmax,imax)
      iquest(11)=imin
      iquest(12)=imax
      call hfithn(itemp,'g','F',0,par,0,0,0,sigpar,chi2)
      final(2,current_strip,uvw,in_out,current_sector)=
     $final(2,current_strip,uvw,in_out,current_sector)
      if(final(1,current_strip,uvw,in_out,current_sector).le.-1000.or.
     $final(1,current_strip,uvw,in_out,current_sector).gt.1000)then
         do i=1,8
            final(i,current_strip,uvw,in_out,current_sector)=0.
         enddo
      endif
c     Save mean and centroid from Gaussian fit
      final(mpar+5,current_strip,uvw,in_out,current_sector)=par(2)
      final(mpar+6,current_strip,uvw,in_out,current_sector)=par(3)
      final(mpar+7,current_strip,uvw,in_out,current_sector)=par(3)
      call hf2(999998,float(18*(in_out - 1) + 6*(uvw - 1) + current_sector),float(current_strip),par(2))
      call hf2(999999,float(18*(in_out - 1) + 6*(uvw - 1) + current_sector),float(current_strip),par(3))

c      print*,'PAR = ',par
c      print*,'SIGPAR = ',sigpar
 
      return
      end
 
************************************************************

      subroutine fcn(npar,gradients,f_value,params,iflag,ectime)

      implicit none
      save

      common /current/ current_sector,in_out,uvw,current_strip
      integer current_sector,in_out,uvw,current_strip
      common /final_params/ final
      real final(15,36,3,2,6)
c     In 'final' the order is: #params+more, strip id, uvw, in_out, sector

#include "ntuple_commons.inc"
#include "ectimes2.inc"     
 
      integer iread,iwrite,ipunch
      integer icycle
      integer npt
      double precision chisquare
      integer npar,iflag,i,mpar
      double precision err,temp,ectime
      double precision f_value,gradients,params(*)
      real tdc_avg, adc_avg, inv_sqrt_adc_avg, dist_avg, vol_avg
      external hcdir
      common /numpar/ mpar

      integer itemp
      real diff,diff1
      real t_plot
      real par(3),sigpar(3),chi2
      real mean,rms,avg,cutoff
      data cutoff /5.0/ ! ignore any data this far beyond mean (ns)
      
      dimension gradients(*)
c      dimension params(*)
      
      common /my_units/ iread, iwrite, ipunch
      external ectime

*     iflag -> task to do
*     
*     1 -> initialize data, constants, etc . . .
*     2 -> calculate gradients (g) and function value (f) at point x
*     3 -> minuit is finished. minimum point (or best) is found
*     4 -> calculate function value (f) at point x
*     5 -> reserved
*     
*     n.b.  the "function value at x" is the chisquare value of the fit
*     using the parameter values in x.  we are using minuit to
*     minimize the chisquare.  
c      print *,'Flag ',iflag
      go to (10, 20, 30, 40, 50) iflag
      
c     ********** iflag = 1 **********
      
 10   continue                  ! initialize
      print*,'Beginning minuit fit'
      npt = npts(current_strip,uvw,in_out,current_sector) ! shorthand
      mpar = npar               ! save this for the later Gaussian fit
c     Force the current tdc slope if desired (set in datafiles):
c      if(tdc_input.eq.1) params(1) = ecCal_Tch(current_sector,in_out,uvw,current_strip)
      print *,npt,npar,params(1)
      print *,'mpar, npar = ',mpar,', ',npar
      return
      
c     ********** iflag = 2 **********
      
 20   continue                  ! gradient and f at x
      
      return
      
c     ********** iflag = 3 **********
      
 30   continue                  ! minuit is finished
      
*     Write out data to be plotted:

      open(14,file='sc_vs_ec',status='unknown')
      open(15,file='diff_vs_x',status='unknown')
      open(16,file='diff_vs_a',status='unknown')
      open(17,file='diff_vs_t',status='unknown')
      open(18,file='diff_vs_v',status='unknown')
      open(19,file='vol_vs_dist',status='unknown')
      open(20,file='t_vs_x',status='unknown')
      open(21,file='d_vs_x',status='unknown')
      open(22,file='d_vs_a',status='unknown')
      open(23,file='d_vs_t',status='unknown')
      open(24,file='d_vs_v',status='unknown')
      open(25,file='tdc_vs_x',status='unknown')
      open(26,file='tdc_vs_sc',status='unknown')

      PRINT*,npt,' data points used'
      tdc_avg = 0.
      adc_avg = 0.
      inv_sqrt_adc_avg = 0.
      dist_avg = 0.
      vol_avg = 0.
      mean = 0.
      rms = 0.
      avg = 0.

      itemp = current_sector*10000
      itemp = itemp + uvw*1000
      itemp = itemp + in_out*100
      itemp = itemp + current_strip
      call hcdir('//PLOTS',' ')
      call hbook1(itemp,' ',300,-15.,15.,0.)
c      if(current_sector.eq.4)call hbook2(itemp+60000,' ',100,-5.,5.,25,0.,25.,0.)
c      if(current_sector.eq.4)call hbook2(itemp+60000,' ',100,-5.,5.,50,0.,400.,0.)

c Modified by Joseph Donnelly
      do i = 1, npt

        temp =
     $     ectime(npar,params,
     $     dble(tdc(i,current_strip,uvw,in_out,current_sector)),
     $     dble(adc(i,current_strip,uvw,in_out,current_sector)),
     $     dble(distance(i,current_strip,uvw,in_out,current_sector)),
     $     dble(volume(i,current_strip,uvw,in_out,current_sector)),
     $     dble(velocity))
c
        diff = t_sc(i,current_strip,uvw,in_out,current_sector)+
     $     ecscdist(in_out)/
     &     beta(i,current_strip,uvw,in_out,current_sector)/
     &     factor(i,current_strip,uvw,in_out,current_sector)-
     $     sngl(temp)
        
        call hf1(itemp,diff,1.)
c        if(current_sector.eq.4)call hf2(itemp+60000,diff,float(pdl_sc(i,current_strip,uvw,in_out,current_sector)),1.)
c        if(current_sector.eq.4)call hf2(itemp+60000,diff,distance(i,current_strip,uvw,in_out,current_sector),1.)

        if(abs(diff).lt.cutoff)then ! ignore outliers
          mean = mean + diff
          rms = rms + diff**2
          avg = avg + 1.0
        endif

        write(14,*)temp,t_sc(i,current_strip,uvw,in_out,current_sector),current_strip,uvw,in_out,current_sector
        write(15,*)distance(i,current_strip,uvw,in_out,current_sector),diff,uvw,in_out
        write(16,*)adc(i,current_strip,uvw,in_out,current_sector),diff,uvw,in_out
        write(17,*)t_sc(i,current_strip,uvw,in_out,current_sector),diff,uvw,in_out
        write(18,*)volume(i,current_strip,uvw,in_out,current_sector),diff
        write(19,*)distance(i,current_strip,uvw,in_out,current_sector),
     $     volume(i,current_strip,uvw,in_out,current_sector)
        write(20,*)distance(i,current_strip,uvw,in_out,current_sector),temp
        write(25,*)distance(i,current_strip,uvw,in_out,current_sector),
     $     tdc(i,current_strip,uvw,in_out,current_sector)
        write(26,*)t_sc(i,current_strip,uvw,in_out,current_sector),
     $     tdc(i,current_strip,uvw,in_out,current_sector)
              
        tdc_avg = tdc_avg+tdc(i,current_strip,uvw,in_out,current_sector)
        adc_avg = adc_avg+adc(i,current_strip,uvw,in_out,current_sector)
        inv_sqrt_adc_avg =inv_sqrt_adc_avg + 
     &       1./sqrt(adc(i,current_strip,uvw,in_out,current_sector))
        dist_avg = dist_avg + 
     &       distance(i,current_strip,uvw,in_out,current_sector)
        vol_avg = vol_avg + 
     &       volume(i,current_strip,uvw,in_out,current_sector)

      enddo

c     Calculate mean and rms
      mean = mean/avg
      rms = rms/avg
      rms = sqrt(abs(rms-mean**2))
      print*,'mean,rms = ',mean,rms

c     Calculate the average values for following plots
      tdc_avg = tdc_avg/float(npt)
      adc_avg = adc_avg/float(npt)
      inv_sqrt_adc_avg = inv_sqrt_adc_avg/float(npt)
      dist_avg = dist_avg/float(npt)
      vol_avg = vol_avg/float(npt)      
      
      do i = 1, npt
        
c     In the following, write out diagnostic plots to show
c     the functional dependence of each variable

c     First calculate diff1 with all parameters operative: 

        temp =
     $     ectime(npar,params,
     $     dble(tdc(i,current_strip,uvw,in_out,current_sector)),
     $     dble(adc(i,current_strip,uvw,in_out,current_sector)),
     $     dble(distance(i,current_strip,uvw,in_out,current_sector)),
     $     dble(volume(i,current_strip,uvw,in_out,current_sector)),
     $     dble(velocity))

        diff1 = t_sc(i,current_strip,uvw,in_out,current_sector)+
     $       ecscdist(in_out)/
     &     beta(i,current_strip,uvw,in_out,current_sector)/
     &       factor(i,current_strip,uvw,in_out,current_sector)-
     $       sngl(temp)

c     Next, replace variables with their averages, plot the change
        temp =
     $       ectime(npar,params,
     $       dble(tdc_avg),
     $       dble(adc(i,current_strip,uvw,in_out,current_sector)),
     $       dble(distance(i,current_strip,uvw,in_out,current_sector)),
     $       dble(volume(i,current_strip,uvw,in_out,current_sector)),
     $       dble(velocity))

        diff = t_sc(i,current_strip,uvw,in_out,current_sector)+
     $       ecscdist(in_out)/
     &     beta(i,current_strip,uvw,in_out,current_sector)/
     &       factor(i,current_strip,uvw,in_out,current_sector)-
     $       sngl(temp)
        write(23,*) tdc(i,current_strip,uvw,in_out,current_sector),
     &       diff1-diff

        temp =
     $     ectime(npar,params,
     $     dble(tdc(i,current_strip,uvw,in_out,current_sector)),
     $     dble(adc_avg),
     $     dble(distance(i,current_strip,uvw,in_out,current_sector)),
     $     dble(volume(i,current_strip,uvw,in_out,current_sector)),
     $     dble(velocity))

        diff = t_sc(i,current_strip,uvw,in_out,current_sector)+
     $       ecscdist(in_out)/
     &     beta(i,current_strip,uvw,in_out,current_sector)/
     &       factor(i,current_strip,uvw,in_out,current_sector)-
     $       sngl(temp)
        write(22,*) adc(i,current_strip,uvw,in_out,current_sector),
     &       diff1-diff


        temp =
     $       ectime(npar,params,
     $       dble(tdc(i,current_strip,uvw,in_out,current_sector)),
     $       dble(adc(i,current_strip,uvw,in_out,current_sector)),
     $       dble(dist_avg),
     $       dble(volume(i,current_strip,uvw,in_out,current_sector)),
     $       dble(velocity))

        diff = t_sc(i,current_strip,uvw,in_out,current_sector)+
     $       ecscdist(in_out)/
     &     beta(i,current_strip,uvw,in_out,current_sector)/
     &       factor(i,current_strip,uvw,in_out,current_sector)-
     $       sngl(temp)
        write(21,*)distance(i,current_strip,uvw,in_out,current_sector),
     &       diff1-diff

        temp =
     $       ectime(npar,params,
     $       dble(tdc(i,current_strip,uvw,in_out,current_sector)),
     $       dble(adc(i,current_strip,uvw,in_out,current_sector)),
     $       dble(distance(i,current_strip,uvw,in_out,current_sector)),
     $       dble(vol_avg),
     $       dble(velocity))

        diff = t_sc(i,current_strip,uvw,in_out,current_sector)+
     $       ecscdist(in_out)/
     &     beta(i,current_strip,uvw,in_out,current_sector)/
     &       factor(i,current_strip,uvw,in_out,current_sector)-
     $       sngl(temp)
        write(24,*) volume(i,current_strip,uvw,in_out,current_sector),
     &       diff1-diff

      enddo

      close(14)
      close(15)
      close(16)
      close(17)
      close(18)
      close(19)
      close(20)
      close(21)
      close(22)
      close(23)
      close(24)
      close(25)
      close(26)

c      calculate final chisquare

      chisquare = 0.0d0
      avg = 0.

      do i = 1, npt
       err=1.d0/
     &   dsqrt(dble(adc(i,current_strip,uvw,in_out,current_sector))) ! sqrt adc as point-by-point error        
c        err = err*dabs(dble(volume(i,current_strip,uvw,in_out,current_sector))) ! possible future use
c        err = err/dble(distance(i,current_strip,uvw,in_out,current_sector))) ! possible future use

c     Check to see how far off this point is: 
        temp =
     $     ectime(npar,params,
     $     dble(tdc(i,current_strip,uvw,in_out,current_sector)),
     $     dble(adc(i,current_strip,uvw,in_out,current_sector)),
     $     dble(distance(i,current_strip,uvw,in_out,current_sector)),
     $     dble(volume(i,current_strip,uvw,in_out,current_sector)),
     $     dble(velocity))

        diff1 = t_sc(i,current_strip,uvw,in_out,current_sector)+
     $     ecscdist(in_out)/
     &     beta(i,current_strip,uvw,in_out,current_sector)/
     &       factor(i,current_strip,uvw,in_out,current_sector)-
     $     sngl(temp)

c Joseph end of Modification

        temp = 
     $     ((
     $     dble(t_sc(i,current_strip,uvw,in_out,current_sector))+
     $     dble(ecscdist(in_out)/
     &     beta(i,current_strip,uvw,in_out,current_sector)/
     &     factor(i,current_strip,uvw,in_out,current_sector))-
     $     ectime(npar,params,
     $        dble(tdc(i,current_strip,uvw,in_out,current_sector)),
     $        dble(adc(i,current_strip,uvw,in_out,current_sector)),
     $        dble(distance(i,current_strip,uvw,in_out,current_sector)),
     $        dble(volume(i,current_strip,uvw,in_out,current_sector)),
     $        dble(velocity)))/err)**2

c     Apply a cutoff to the points included in chisquare
        if(abs(diff1).lt.cutoff)then
          chisquare = chisquare + temp
          avg = avg + 1.0
        endif
      end do

c     chisquared per degree of freedom:
      chisquare = chisquare/(dble(avg)-dble(npar))

c     Save the fit parameters:
      do i=1,npar
       final(i,current_strip,uvw,in_out,current_sector)=sngl(params(i))
      enddo

c     Make final adjustment to offset term, based on final parameters:

      final(2,current_strip,uvw,in_out,current_sector)=
     $      final(2,current_strip,uvw,in_out,current_sector)+mean

c     Save information about quality of calibration:

      final(npar+1,current_strip,uvw,in_out,current_sector)=mean
      final(npar+2,current_strip,uvw,in_out,current_sector)=rms
      final(npar+3,current_strip,uvw,in_out,current_sector)=
     $   (sngl(chisquare)*inv_sqrt_adc_avg**2)/rms**2
      final(npar+4,current_strip,uvw,in_out,current_sector)=avg

      print*,'stack,uvw,in_out,sector =',
     ^     current_strip,uvw,in_out,current_sector
      print*,'Chi-squared per d. o. f. = ',chisquare
      print*,'Mod Chi-squared per d. o. f. = ',chisquare/dble(adc_avg)
      print*,'Estimated Chi-squared per d. o. f. = ',
     $     final(npar+3,current_strip,uvw,in_out,current_sector)
      print*,'Used ',avg,' events out of ',npt
      print *,current_strip,uvw,in_out,current_sector,final(1,current_strip,uvw,in_out,current_sector)
      call hf2(999997,float(18*(in_out - 1) + 6*(uvw - 1) + current_sector),float(current_strip),final(npar+3,current_strip,uvw,in_out,current_sector))
      return
 
c     ********** iflag = 4 **********
 
40    continue   ! f at x
 
!     We are returning the value of chisquare since this is what we want
!     minimize.

      chisquare = 0.0d0
      do i = 1, npt
        err = 1.d0/
     &   dsqrt(dble(adc(i,current_strip,uvw,in_out,current_sector))) ! take sqrt adc as point-by-point error
c        err = err*dabs(dble(volume(i,current_strip,uvw,in_out,current_sector))) !possible future use
c        err = err/(dble(distance(i,current_strip,uvw,in_out,current_sector))) !possible future use
        temp = 
     $     ((
     $     dble(t_sc(i,current_strip,uvw,in_out,current_sector))+
     $     dble(ecscdist(in_out)/
     &     beta(i,current_strip,uvw,in_out,current_sector)/
     &       factor(i,current_strip,uvw,in_out,current_sector))- ! use 1 ns for perp distance between EC and TOF
     $     ectime(npar,params,
     $     dble(tdc(i,current_strip,uvw,in_out,current_sector)),
     $     dble(adc(i,current_strip,uvw,in_out,current_sector)),
     $     dble(distance(i,current_strip,uvw,in_out,current_sector)),
     $     dble(volume(i,current_strip,uvw,in_out,current_sector)),
     $     dble(velocity)))/err)**2
        
        chisquare = chisquare + temp
      end do
      
      f_value = chisquare
      return
      
c     ********** iflag = 5 **********
      
 50   continue                  ! reserved
      return
      
      end

************************************************************
      
      double precision function ectime(npar,params,tdc,adc,distance,
     &     volume,velocity)

      implicit none
      save
 
      integer npar
      double precision params(*)
      double precision tdc,adc,distance,volume,velocity
      double precision a,b,c,d,e,f,g,h


      a=params(1)
      b=params(2)
      c=params(3)
      d=params(4)
      e=params(5)
c      f=params(6)

      ectime = a*(tdc/1.d3)
     $+b
     $+c/dsqrt(dabs(adc))
     $+d*dabs(distance)**2
     $+e*dabs(distance)**3
     $-distance/velocity
      return
      end
 
 
************************************************************
 
      subroutine good_bye
      
      implicit none

c      type *, ' '
      print *, 'Minuit has finished'
c      type *, ' '
c      type *, 'minuit cards -> minuit_example.cards created'
c      type *, 'data points to fit -> data_to_fit.dat created'
c      type *, 'minuit ouput -> minuit_example.output created'
      
      return
      end


C
C==================

      SUBROUTINE Ec_ij_path(di,dj,pu,pv,pw,layer,sector)
      
      real di,dj,pu,pv,pw,Lu,Lv,Lw,H,H1,du,dv,dw
      real Lui,Lvi,Lwi,H1i
      data Lui,Lvi,Lwi,H1i/418.9633,381.8558,418.9633,182.974/
      integer layer,sector 
      real dlu,dlv,dlw,dh1
      data dlu,dlv,dlw,dh1/15.019,15.689,15.019,6.5562/ 
c      data dlu,dlv,dlw,dh1/27.04,24.64,27.04,11.8/ 
*
      Lu=Lui+float(layer-1)*dlu
      Lv=Lvi+float(layer-1)*dlv
      Lw=Lwi+float(layer-1)*dlw
      H=sqrt(Lu**2-Lv**2/4.)              !assume Lu=Lw      
      H1=H1i+float(layer-1)*dh1
*
      du=(di/H +H1/H)*Lu                  !calculate u,v,w of (i,j)
      dv=Lv-H1/2./H*Lv-dj-Lv/2./H*di
      dw=Lw/Lv*dj-Lw/2./H*di-H1/2./H*Lw+Lw
*
      pu=du/Lu*Lv-(Lv-dv)                  ! path
      pv=dv/Lv*Lw-(Lw-dw)
      pw=dw/Lw*Lu-(Lu-du)      
* 
      RETURN
      END

C==================

      subroutine ecreadtdcs(irun,rmap)
#include "ectimes2.inc"
      character*(*) rmap
      character*99 ritems(6),ritem
      data ritems/'InnerU','InnerV','InnerW','OuterU','OuterV','OuterW'/
      integer ley,orient,ls,lst
      real value216(216)
c     
c     
      do ley=1,2
         do orient=1,3
            ritem(1:6)=ritems(orient+(ley-1)*3)
            call map_get_float(rmap,'EC_Tch',ritem,216,value216,irun,iflag)
            do ls=1,6
               do lst=1,36
                  ecCal_Tch(ls,ley,orient,lst)=1000*value216(lst+(ls-1)*36)
               enddo
            enddo
         enddo
      enddo
c     
      return
      end
