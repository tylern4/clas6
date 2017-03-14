      subroutine retime(dummy)
c     Calculates EC hit time.
      implicit none
      real dummy
      save
#include "ntuple_commons.inc"
#include "ectimes2.inc"
#include "bcs.inc"

      integer bosread
      external bosread
      integer mamind
      external mamind
      integer istat, hid, maxproc
      integer iii,ihits,sect,io
      integer ip,read_input
      integer nlc, first_read, btype, ind, nami
      integer NFILES,count
      integer ifile,iflag,Handle,ecsect
      real timel(2), timea, thetaec, drtheta
      real ew, ews, bet, drc, path, dtv,dtrv,doff,rfoffec
      data rfoffec/0.9/
      real perp(sector,3),trtime, sc_ec_t
      character*100 filename(50)
      character*99 rmap
c     step      common /what_file/ filename
      common /final_params/ final
      real final(15,36,3,2,6)
      integer count1,length,leng,file2proc,file2check
      character*10 char_var(64)
      real pi,dtime,r2d
      data pi,r2d/3.14159265,57.29578/
#include "initializations.inc"
c     
      call readpar
c     
      read_input=0
      NFILES=0
      open(unit=99,file='datafiles',status='unknown')
      read(unit=99,fmt='(3I10)',err=998,end=998)maxproc, file2proc,file2check
      write(6,*)'Only ',file2check,' file will be used'
 100  read(unit=99,fmt='(A)',err=998,end=998) filename(NFILES+1)
      NFILES=NFILES+1
      if(file2check.gt.nfiles)goto 100
 998  close(99)
C.... Loop over NFILES to read in
      do 10 ifile=1,NFILES
         count=0
         count1=0
         leng=length(filename(ifile))
         write(6,*)'Input file is  ',filename(ifile)(1:leng)
         istat = 0
         call BOSOPEN(filename(ifile)(1:leng),'r',Handle)
         read_input=read_input+1
         write(6,*) 'Reading data file - ', filename(ifile)(1:leng)
         first_read=0
         istat = 0
 11      istat=BOSREAD(Handle,IW,'E')
         if(istat.ne.0)then
            if(istat.eq.-1)goto 12 ! EOF, next file
            write(6,*)'File ',filename(ifile)(1:leng),' currupted!'
            goto 12
         else
            if(first_read.eq.0)then
c     get beam type from the RUNC bank (0:electron, 1:photon)  
               nami=mamind(iw,'HEAD')
               ind=0
               if(nami.gt.0)ind=IW(nami)
               if(ind.gt.0)then
                  call revinm('CLAS_PARMS','Maps/RUN_CONTROL.map',rmap)
                  call map_get_int(rmap,'beam', 'type',1,btype,iw(ind+2),iflag)
                  write(6,*)'Beam type =',btype,' for run ',iw(ind+2),' from run',iflag
                  first_read=1
                  call revinm('CLAS_PARMS','Maps/EC_CALIB.map',rmap)
                  call ecreadmap(iw(ind+2),rmap,1)
                  call ecreadmap(iw(ind+2),rmap,2)
                  call ecreadmap(iw(ind+2),rmap,3)
               endif
            endif
c     
            if(count.gt.maxproc) go to 12
            count=count+1
            count1=count1+1
            call fill_seb_nt
            if(count1.eq.10000)then
               count1=0
               write(6,*)count,' events read'
            endif
         endif
c     Read in the variables for each event:
         call hcdir('//PAWC',' ')
         if(ecpc_nhit.lt.1.or.ecpc_nhit.gt.300.or.gpart.eq.0)go to 1
         if(dc(1).gt.0.and.dc_stat(dc(1)).gt.0)then
         do ip = 1, gpart       ! loop over geometrically matched particles
            ihits = 0
            do iii = 1,ec_part  ! count ec hits in this sector
               if(ec_sect(iii).eq.ec_sect(ec(ip)))ihits=ihits+1
            enddo
            if(ihits.ne.1) go to 1
            sect=ec_sect(ec(ip))
            hid=1000*sect
c     if(ec_sect(ec(ip)).ne.6)go to 1
            if(btype.eq.0.and.ip.eq.1)then
c     abandon entire event in case of electron beam if bad trigg.electron
               if(dc(1).le.0.or.sc(1).le.0.or.ec(ip).le.0.or.cc(1).le.0)go to 1 
               if(sc_stat(sc(ip)).ne.33.or.dc_stat(dc(1)).le.0)goto 1
               if(nphe(cc(1)).lt.30.or.etot(ec(1))/p(1).lt.0.2)goto 1
               trtime=sc_t(sc(1))-sc_r(sc(1))/29.979
               call hf2(100+sect,trtime-tr_time,float(sc_pd(sc(1))),1.)
               direction(1)=dc_cxsc(dc(ip))*cos(pi*(sect-1)/3.)
     -              -dc_cysc(dc(ip))*sin(pi*(sect-1)/3.)      
               direction(2)=dc_cysc(dc(ip))*cos(pi*(sect-1)/3.)
     +              +dc_cxsc(dc(ip))*sin(pi*(sect-1)/3.)      
               direction(3)=dc_czsc(dc(ip))
               drc=perp(sect,1)*direction(1)+perp(sect,2)*direction(2)+perp(sect,3)*direction(3)
               sc_ec_t=sc_t(sc(1))+perp_thickness(1)/drc/29.979
               CALL HITTIME(timel,direction,sc_ec_t,1,iflag)
               if(iflag.gt.0)then
c     print *,evntid,sc_t(sc(1)),sc_ec_t,perp_thickness(1),perp_thickness(2),drc
                  nlc=0
                  timea=0.
                  ews=0.
                  do io=1,2
                     hid=hid+(io-1)*100
                     if(io.eq.1)ew=ec_ei(ec(1))
                     if(io.eq.2)ew=ec_eo(ec(1))
                     if(timel(io).gt.0.)then
                        timel(io)=timel(io)-(perp_thickness(io)/drc-perp_thickness(1)/drc)/29.979
                        call hf2(hid+2,timel(io)-sc_ec_t,float(sc_pd(sc(1))),1.)
                        call hf1(hid+1,timel(io)-sc_ec_t,1.)
                        dtime=timel(io)-ec_r(ec(1))/29.9792458
c     Modified here by Joseph Donnelly
c     call hf1(700,timel(io)-ec_r(20)/299792458-tr_time,1.)
c     call hf1(701,mod(timel(io)-ec_r(20)/299792458-tr_time,2.004),1.)
                        if(io.eq.1)then
                           call hf1(800,dtime-tr_time,1.)
                           call hf1(801,mod(dtime-rf_time1+100*2.004,2.004),1.)
                        elseif(io.eq.2)then
                           call hf1(900,dtime-tr_time,1.)
                           call hf1(901,mod(dtime-rf_time1+100*2.004,2.004),1.)
                        endif
c     End of modification by Joseph Donnelly
                        nlc=nlc+1
                        timea=timea+timel(io)*ew
                        ews=ews+ew

                     endif
                  enddo

                  if(ews.gt.0.)timea=timea/ews
                  call hf2(hid+102,timea-sc_ec_t,float(sc_pd(sc(1))),1.)
                  call hf1(hid+101,timea-sc_ec_t,1.)
               endif
            endif
c
            if(btype.ne.0.or.ip.gt.1)then
c     Select the desired particle type:
c     
               if(q(ip).ne.0.and.sc(ip).gt.0.and.dc(ip).gt.0.and.ec(ip).gt.0.)then ! charged particle
                  if(dc_stat(dc(ip)).gt.0.and.ihits.eq.1.and.sc_stat(sc(ip)).eq.33)then
                     if(dc_sect(dc(ip)).eq.ec_sect(ec(ip)))then 
                        direction(1)=dc_cxsc(dc(ip))*cos(pi*(sect-1)/3.)
     -                       -dc_cysc(dc(ip))*sin(pi*(sect-1)/3.)      
                        direction(2)=dc_cysc(dc(ip))*cos(pi*(sect-1)/3.)
     +                       +dc_cxsc(dc(ip))*sin(pi*(sect-1)/3.)      
                        direction(3)=dc_czsc(dc(ip))
                        drc=perp(sect,1)*direction(1)+perp(sect,2)*direction(2)+perp(sect,3)*direction(3)
                        sc_ec_t=sc_t(sc(ip))+perp_thickness(1)/drc/29.979/b(ip)
c     sc_ec_t=sc_t(sc(1))+(ec_r(ec(ip))-sc_r(sc(ip)))/29.979/b(ip)
                        CALL HITTIME(timel,direction,sc_ec_t,ip,iflag)
                        if(iflag.gt.0)then
                           nlc=0
                           timea=0.
                           ews=0.
                           do io=1,2
                              hid=hid+(io-1)*100
                              if(io.eq.1)ew=ec_ei(ec(ip))
                              if(io.eq.2)ew=ec_eo(ec(ip))
                              if(timel(io).gt.0..and.ew.gt.0.01)then
                                 timel(io)=timel(io)-(perp_thickness(io)/drc-perp_thickness(1)/drc)/29.979/b(ip)
                                 if(q(ip).gt.0)call hf1(hid+3,timel(io)-sc_ec_t,1.)
                                 if(q(ip).lt.0)call hf1(hid+4,timel(io)-sc_ec_t,1.)
                                 if(id(ip).eq.2212 .and. p(ip).gt.0.8) call hf1(hid+7,timel(io)-sc_ec_t,1.)
                                 if(id(ip).eq.2212 .and. p(ip).lt.0.8) call hf1(hid+8,timel(io)-sc_ec_t,1.)
                                 call hf1(hid+1,timel(io)-sc_ec_t,1.)
                                 if(btype.ne.0)call hf2(hid+2,timel(io)-sc_ec_t,float(sc_pd(sc(ip))),1.)
                                 nlc=nlc+1
                                 timea=timea+timel(io)*ew
                                 ews=ews+ew
                              endif
                           enddo
                           if(ews.gt.0.)then
                              timea=timea/ews
                              if(q(ip).gt.0)call hf1(hid+103,timea-sc_ec_t,1.)
                              if(id(ip).eq.2212 .and. p(ip).gt.0.8) call hf1(hid+107,timea-sc_ec_t,1.)
                              if(id(ip).eq.2212 .and. p(ip).lt.0.8) call hf1(hid+108,timea-sc_ec_t,1.)
                              if(q(ip).lt.0)call hf1(hid+104,timea-sc_ec_t,1.)
                              call hf1(hid+101,timea-sc_ec_t,1.)
                           endif
                           if(btype.ne.0)call hf2(hid+102,timea-sc_ec_t,float(sc_pd(sc(ip))),1.)
                        endif
                     else
                        write(6,*)'For hadronron EC and DC sectors are not the same',dc_sect(dc(ip)),ec_sect(ec(ip))
                     endif
                  endif
               elseif(q(ip).eq.0)then
                  if(id(ip).ne.0.and.ec(ip).gt.0)then
                     path=sqrt((ech_x(ec(ip))-vx(1))**2
     +                    +     (ech_y(ec(ip))-vy(1))**2
     +                    +     (ech_z(ec(ip))-vz(1))**2)
                     direction(1)=(ech_x(ec(ip))-vx(1))/path
                     direction(2)=(ech_y(ec(ip))-vy(1))/path
                     direction(3)=(ech_z(ec(ip))-vz(1))/path
                     drc=perp(sect,1)*direction(1)+perp(sect,2)*direction(2)+perp(sect,3)*direction(3)
                     thetaec=acos(direction(3))*r2d
                     drtheta=acos(drc)*r2d
                     ecsect=ec_sect(ec(ip))
                     sc_ec_t=0.
                     CALL HITTIME(timel,direction,sc_ec_t,ip,iflag)
                     if(iflag.gt.0)then
                        nlc=0
                        timea=0.
                        ews=0.
                        do io=1,2
                           hid=hid+(io-1)*100
                           if(io.eq.1)ew=ec_ei(ec(ip))
                           if(io.eq.2)ew=ec_eo(ec(ip))
                           if(timel(io).gt.0.)then
c     Commented out by Joseph Donnelly   timel(io)=timel(io)-(perp_thickness(io)/drc-perp_thickness(1)/drc)/29.979/b(ip)
                              call hf1(hid+5,timel(io)-tr_time,1.)

                              bet=path/(timel(io)-tr_time)/29.979
                              call hf2(hid+6,bet,path,1.)

c     Modified by Joseph Donnelly
c     call hf1(702,timel(io)-ec_r(20)/299792458-tr_time,1.)
c     call hf1(703,mod(timel(io)-ec_r(20)/299792458-tr_time,2.004),1.)
                              if(bet.gt.0.95.and.bet.lt.1.05.and.ec_ei(ec(ip))+ec_ei(ec(ip)).gt.0.5)then
                                 dtv=timel(io)-path/29.9792458-tr_time
                                 dtrv=timel(io)-path/29.9792458-rf_time1+100*2.004-rfoffec
                                 doff=mod(dtrv,2.004)-1.002
                                 call hf1((io-1)*100+802,dtv,1.)
                                 call hf2((io-1)*100+803,doff,drtheta,1.)
                                 call hf2((io-1)*100+803+ecsect*10,doff,drtheta,1.)
                              endif
c     End of Modification
                              nlc=nlc+1
                              timea=timea+timel(io)*ew
                              ews=ews+ew
                           endif
                        enddo
                        if(ews.gt.0.)timea=timea/ews
c     print *,evntid,timea,timel(1),ec_ei(ec(ip)),timel(2),ec_eo(ec(ip))
                        bet=path/(timea-tr_time)/29.979
                        call hf2(hid+106,bet,path,1.)
                        call hf1(hid+105,timea-tr_time,1.)
                     endif
                  endif
               endif
 2             continue
            endif
         enddo                  !loop over particles
         endif
c     endif                  !there is a good electron
 1       call bdrop(iw,'E')
         call bgarb(iw)
         goto 11                ! loop over events
c     
 12      call bdrop(iw,'E')
         call bgarb(iw)
         call BOSCLOSE(Handle,0)
 10   continue
      end
      
c     
c--------------------------------------------
c
      subroutine hittime(timel,direction,ecsct,ip,flag)
      implicit none
#include "ntuple_commons.inc"
#include "ectimes2.inc"
      save
      common /final_params/ final
      real final(15,36,3,2,6)
      real pi
      data pi/3.14159265/
c      
      integer sect,current_sector,itemp
      integer ii,iii,ip,jj,in_out,uvw,current_strip
      integer npar,flagl(2),flag
      data npar/5/
      integer max_adc(view,layer)
      integer max_adc_id(view,layer)
      real htdc,hadc,volm,vadcl(2),sigmf,pu,pv,pw
      real temp,dr,rx,ry,rz
      real timel(2), find_distance, ect,distnc
      double precision param(5),ectime
      character*10 char_var(64)
      real perp(sector,3),u,v,w,xi,yi,ecsct
      real dtime(6,3,2,36),emax(6,3,2,36)
c     Initialize the following arrays:
#include "initializations.inc"
      do ii = 1, view
         do jj = 1, layer
            max_adc(ii,jj) = 0  ! Only consider adcs with non-zero values
            max_adc_id(ii,jj) = 0
         enddo
      enddo
      sect=ec_sect(ec(ip))
c     Loop over the contents of ecpc, hoping to find one strip in each view 
c     (for inner and outer) which has the maximal adc value.
      do iii = 1,ecpc_nhit
            
c     Decode the ecpc hit information to find out the sector and layer of the hit.
c     The existence of only one hit in this sector is tacitly assumed here.
         current_sector = ecpc_hit(iii)/100
c     Check to see if this hit is in the right sector:
         if(current_sector.ne.sect)go to 3 ! skip to next ecpc hit
c     Check for non-zero tdc:
         if(ecpc_tdc(iii).lt.1)go to 3 ! skip to next ecpc hit
         in_out = (ecpc_hit(iii)-current_sector*100)/10 ! find layer
         uvw = ecpc_id(iii)/100 ! find view
         current_strip = ecpc_id(iii)-100*uvw ! find stack number
         if(max(max_adc(uvw,in_out),ecpc_adc(iii)).gt.
     $        max_adc(uvw,in_out))then ! found a new candidate
            max_adc_id(uvw,in_out)=iii ! Save id of max adc
            max_adc(uvw,in_out)=ecpc_adc(iii)
         endif
 3       continue
      enddo
      current_sector = sect
      do in_out = 1, layer
         timel(in_out)=0.
         vadcl(in_out)=0.
         flagl(in_out)=0
         do uvw = 1, view
            if(max_adc_id(uvw,in_out).eq.0)go to 4 ! no data, go to next ecpc hit
            current_strip = ecpc_id(max_adc_id(uvw,in_out))-100*uvw ! find stack number
            htdc = float(ecpc_tdc(max_adc_id(uvw,in_out))) ! Save this tdc value
            hadc = float(ecpc_adc(max_adc_id(uvw,in_out))) ! Save this adc value

c1            call hf2(540+current_sector,float((uvw-1)*72+(in_out-1)*36+current_strip),htdc,1.)
c1            call hf2(550+current_sector,float((uvw-1)*72+(in_out-1)*36+current_strip),hadc,1.)

C     - Get the direction for tracking for charged particles q=1, and from ec_hit for q=0.
c     In the following, calculate local coordinates from the global coordinates, assuming
c     the ideal geometry. A better way would be to have the local coordinates directly 
c     from the data file, in case some day we use the survey-corrected geometry.
c     Calculate the local coordinates of the entrance point:
c     Calculate the thickness traversed through the calorimeter layer
c     Again direction vector in the defined in the sector 1, so perp as well.
            if(uvw.eq.1)then
               temp=direction(1)*perp(current_sector,1) ! dot product here
               temp=temp+direction(2)*perp(current_sector,2) ! dot product
               temp=temp+direction(3)*perp(current_sector,3) ! dot product
c     Calculate the point of at the layer:
               dr=(perp_thickness(in_out)/temp-perp_thickness(1)/temp)
               rx=ech_x(ec(ip))+direction(1)*dr
               ry=ech_y(ec(ip))+direction(2)*dr
               rz=ech_z(ec(ip))+direction(3)*dr
               call ec_xyz_duvw(rx,ry,rz,u,v,w,xi,yi,in_out)
c     Calculate the distances to the phototube edge 
c     for the entrance point and the exit point.
               call Ec_ij_path(yi,xi,pu,pv,pw,in_out,current_sector)
               distnc = pu
            endif
            if(uvw.eq.2)distnc = pv
            if(uvw.eq.3)distnc = pw
c            print *,distnc,uvw,in_out,current_sector,evntid
            volm =0.0
            do ii=1,5
               param(ii)=final(ii,current_strip,uvw,in_out,current_sector)
            enddo 
            if(param(1).le.1.)go to 4      ! abandon zero param strips
            sigmf=abs(final(11,current_strip,uvw,in_out,current_sector))
            if(sigmf.lt.1.0E-5)go to 4     ! abandon zero sigma strips
            ect=sngl(
     +           ectime(npar,param,dble(htdc),dble(hadc),dble(distnc),dble(volm),dble(velocity)))
            timel(in_out)=timel(in_out)+ect*sqrt(hadc)/sigmf

            vadcl(in_out)=vadcl(in_out)+sqrt(hadc)/sigmf
            flagl(in_out)=flagl(in_out)+1
            dtime(current_sector,uvw,in_out,current_strip)=ect-ecsct-dr/29.979
            emax(current_sector,in_out,uvw,current_strip)=hadc*
     *           EcCal_Ech(current_sector,in_out,uvw,current_strip)*
     *           exp(distnc/EcCal_atten(current_sector,in_out,uvw,current_strip))
 4          continue
         enddo
         if(flagl(in_out).eq.3.and.vadcl(in_out).gt.0.)then
            timel(in_out)=timel(in_out)/vadcl(in_out)
            flag=3
            if(ip.eq.1)then
            do uvw = 1, view
               itemp = current_sector*10000
               itemp = itemp + uvw*1000
               itemp = itemp + in_out*100
               itemp = itemp + current_strip
c               call hf1(itemp,emax(current_sector,in_out,uvw,current_strip)/p(1),1.)
            enddo
         endif
         else
            timel(in_out)=0.
            flag=0
         endif
      enddo                     ! end of loops over ecpc max-adc hits
      end
c----------------------------------------
      subroutine rebook
c
      integer i,j,hid
c
      call hcdir('//PLOTS',' ')
c
c Modified by Joseph Donnelly
c            call hbook1(700,'timel-tof-tr_time',400,-10.,10.,0.)
c            call hbook1(701,'mod(timel-tof-tr_time,2.004)',400,-10.,10.,0.)
c            call hbook1(702,'time-tof-tr_time for neutrals',400,-10.,10.,0.)
c            call hbook1(703,'mod(timel-tof-tr_time,2.004) for neutrals',400,-10.,10.,0.)
            call hbook1(800,'timel-tof-tr_time',200,-10.,10.,0.)
            call hbook1(801,'mod(timel-tof-tr_time,2.004)',150,-1.5,1.5,0.)
            call hbook1(802,'time-tof-tr_time for neutrals',200,-20.,80.,0.)
            call hbook2(803,'tv for neutrals',150,-1.5,1.5,25,0.,25.,0.)
            call hbook1(900,'timel-tof-tr_time',200,-10.,10.,0.)
            call hbook1(901,'mod(timel-tof-tr_time,2.004)',150,-1.5,1.5,0.)
            call hbook1(902,'time-tof-tr_time for neutrals',200,-20.,80.,0.)
            call hbook2(903,'tv for neutrals',150,-1.5,1.5,25,0.,25.,0.)
c End of Modification
      DO i = 1,6
         call hbook2(100+i,'rf',100,-2.5,2.5,25,0.,25.,0.)
            call hbook2(903+i*10,'tv for neutrals',150,-1.5,1.5,25,0.,25.,0.)
            call hbook2(803+i*10,'tv for neutrals',150,-1.5,1.5,25,0.,25.,0.)
         Do j=1,3
            hid=1000*i+(j-1)*100
            call hbook1(hid+1,'ECt-SCt',400,-10.,10.,0.)
            call hbook2(hid+2,'ECt-SCt el',200,-10.,10.,25,0.,25.,0.)
            call hbook1(hid+3,'ECt-SCt h+',400,-10.,10.,0.)
            call hbook1(hid+4,'ECt-SCt h-',400,-10.,10.,0.)
            call hbook1(hid+5,'ECt-TRt n[g]',400,0.,40.,0.)
            call hbook2(hid+6,'[b] for neutrals',300,0.,1.5,100,500.,700.,0.)
            call hbook1(hid+7,'ECt-SCt fast prot',400,-10.,10.,0.)
            call hbook1(hid+8,'ECt-SCt slow prot',400,-10.,10.,0.)
         EndDo
      ENDDO
c
      do l=1,6
         do j=1,3
            do k=1,2
               do i=1,36            
                  itemp = l*10000
                  itemp = itemp + j*1000
                  itemp = itemp + k*100
                  itemp = itemp + i
                  call hcdir('//PLOTS',' ')
c                  call hbook1(itemp,' ',200,0.,1.,0.)
               enddo
            enddo
         enddo
      enddo
c
      return
      end

      subroutine readpar
      common /final_params/ final
      real final(15,36,3,2,6)
      open(11,file=
     $   'final.out',
     $   status='unknown')
      do ll=1,6
        do jj=1,3
          do kk=1,2
            do ii=1,36
              read(11,200,end=100)ij,
     $           (final(mm,ii,jj,kk,ll),mm=1,11)
            enddo
          enddo
        enddo
      enddo
 100  continue
 200  format(1X,I5,11(1X,E12.5))
      return
      end
c     
      subroutine ecreadmap(irun,rmap,ifl)
#include "ectimes2.inc"
      character*(*) rmap
      character*99 ritems(6),ritem
      data ritems/'InnerU','InnerV','InnerW','OuterU','OuterV','OuterW'/
      integer ifl,ley,orient,ls,lst
      real value216(216)
c     
c     
      do ley=1,2
         do orient=1,3
            ritem(1:6)=ritems(orient+(ley-1)*3)
            if(ifl.eq.1)then
               call map_get_float(rmap,'EC_GAIN',ritem,216,value216,irun,iflag)
            elseif(ifl.eq.2)then
               call  map_get_float(rmap,'EC_PEDESTALS',ritem,216,value216,irun,iflag)
            elseif(ifl.eq.3)then
               call map_get_float(rmap,'EC_ATTEN',ritem,216,value216,irun,iflag)
            endif
            do ls=1,6
               do lst=1,36
                  if(ifl.eq.1)then
                     ecCal_Ech(ls,ley,orient,lst)=value216(lst+(ls-1)*36)
                  elseif(ifl.eq.2)then
                     ecCal_Eo(ls,ley,orient,lst)=value216(lst+(ls-1)*36)
                  elseif(ifl.eq.3)then
                     EcCal_atten(ls,ley,orient,lst)=value216(lst+(ls-1)*36)
                  endif
               enddo
            enddo
         enddo
      enddo
c     
      return
      end
      
