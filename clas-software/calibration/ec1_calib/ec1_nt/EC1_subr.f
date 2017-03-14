c-----------------------------------------------------------------------

      SUBROUTINE init_par(Nfiles,Fname,OutFile,runno)
      
      IMPLICIT NONE
      include "ntpl_com.inc"
      CHARACTER *80 Fname(10), OutFile
      CHARACTER *1 rep
      INTEGER*4     nfiles,i,runno

 1    write (*,'(" Enter NUMBER of files you wish to analyze (<10): ",$)')
      READ *, Nfiles
      if (Nfiles.ge.10) then
        print *, " **ERROR** The MAX number of files to proceed = 9"
	goto 1
      end if 
      write (*,'(" Enter INPUT file names one by one line: ")')
      do i=1,Nfiles
        READ  (*,'(a80)') Fname(i)
        Fname(i) = Fname(i)(1:index(Fname(i),' ')-1)
      end do
      do i=1,Nfiles
        open (unit=1,file=Fname(i),status='old',err=2)
        goto 3
  2     continue
        print *,'Input file ',Fname(i),' does NOT exist'
        goto 1
  3     continue
        close (1)
      end do
        write (*,'(" Input files are: ")')
      do i=1,Nfiles
        write (*,'(bna)') Fname(i)
      end do

      write (*,'(" Enter RUN NUMBER you are going to analyze: ",$)')
      READ  *, runno

      write (*,'(" Would you like to use DEFAULT settings (Recomended) ? (y,n): ",$)')
      read (*,'(a1)') rep
      if (rep .eq. 'n' .OR. rep .eq. 'N') then
        write (*,'(" Enter mean value of Atten.Length ")' )
        write (*,'("(0 or negative means read from $CLAS_PARMS/ec1atten.dat) :",$)')
        READ  *, attlen
        if (attlen.gt.0) then
          print *,' Attenuation Length was replaced with constant value ',attlen,
     &        ' for all stacks'
        else
      
          print *,' Reading Attenuation Length from $CLAS_PARM/ec1atten.dat'
        end if
        write (*,'(" Enter OUTPUT file name: ",$)')
        READ  (*,'(a80)') OutFile
c      OutFile_h = OutFile(1:index(OutFile,' ')-1)//'.hbook'
        write (*,'(" Enter MAX number of STRIPS in one LAYER involved in one event ")')
        write (*,'("(default 2): ",$)')
        READ  *, Npmt
        print *, ' Npmt = ',Npmt
        write (*,'("shift between crosses in INNER and OUTER (0 - no shift, 1 - up to 1)")')
        write (*,'("(default 1): ",$)')
        READ  *, ishift
        print *, ' ishift = ',ishift
        write (*,'(" Enter number of events you want to proceed (0 - all file): ",$)')
        READ  *, n_events
        if(n_events.le.0) then
          n_events=-1
          print *,'Allora, facciamo tutto'
        else
          print *,'Allora, facciamo solo ',n_events,' events'
        end if
      else ! default settings
c Using fixed Atten. Length as in RECSIS.
        attlen = 380
        OutFile = 'EC1_calib'
	Npmt = 1
	ishift = 1
	n_events=-1   ! proceed all events in all files
      end if	
      return
      end 


c------------------------------------------------------------------------        
      SUBROUTINE read_ped(pedest,spar_ped)
      
      IMPLICIT NONE  
      INTEGER ID_P(30,0:63), Sector_P(30,0:63)  
      INTEGER isect, typ, nchan, slot, side, i, layer,strip
      INTEGER dummy
      INTEGER pedest(2,2,1088), ped(30,0:63)
      INTEGER unit
      INTEGER spar_ped
      CHARACTER*4 name
      CHARACTER*120 file_roc,file_ped

      character*(*) home
      parameter (home  = 'HOME')

      data ID_P/1920*0/

	call revinm(home,'../vvsap/PARMS/ROC19.tab',file_roc)
	call revinm(home,'../vvsap/PARMS/LAC.spar.dat',file_ped)
        
        unit = 4
c        file_roc = '~vvsap/PARMS/ROC19.tab'
c        file_ped = '~vvsap/PARMS/LAC.spar.dat'
c         file_roc = '/home/vvsap/PARMS/ROC19.tab'
c         file_ped = '/home/vvsap/PARMS/LAC.spar.dat'
        
        open (unit,file=file_roc,status='old', err=20)
        
       read(unit,*) name
       read(unit,*) name
       read(unit,*) name   ! skip first 3 header lines
       
       DO while (.true.)
         read(unit,*,end=10) name,isect,layer,strip,dummy,dummy,dummy,dummy
     &          ,slot,typ,nchan
         if(name.eq.'EC1 ' .and. typ.eq.1881) then ! select ADC board for EC1
           ID_P     (slot,nchan)=256*layer+strip
           Sector_P (slot,nchan)=isect
         end if
       ENDDO
 10    continue
       close(unit)
      
       open (unit,file=file_ped,status='old', err=21)
      
       do i=1,512
         read(unit,*) slot,nchan,ped(slot,nchan)
       end do

       side = 0                       ! init side
       do slot=1,30                   ! cycle around slots
         if (ID_P(slot,0).ne.0) then  ! slot not empty
           side=mod(side+2,2)+1       ! change side (1-left , 2-right)
           do i=0,63
c
c     added substruction of spar-ped channels to make usiable LAC.spar 
c       
             pedest(sector_P(slot,i),side,ID_P(slot,i))=ped(slot,i)-spar_ped
           end do
         end if
       end do
       
       close(unit)
        
        return
 20     print *,' cannot open file ',file_roc
        stop
 21     print *,' cannot open file ',file_ped
        stop
        end

c------------------------------------------------------------------------        
      SUBROUTINE prnt_ped(runno,pedest)
      
      IMPLICIT NONE
      CHARACTER*1 a
      CHARACTER*14 file_calib
      INTEGER isect, ilayer,istrip, nstrip, id_i, runno
      INTEGER pedest(2,2,1088)
      INCLUDE  "ntpl_com.inc"

c control output of pedestals
      Print *,' Pedestals for RUN#',runno
      Print *,' press return '
      read * 
c      write (file_calib,'(a6,BZI6,a4)') 'calib_',runno,'.dat'
      file_calib = 'calib_dat.curr'
      print *,' Control datafile of calib. constants used for this run', file_calib
      open (4,file=file_calib,status='unknown', err=20)
       do isect=1,2
         do ilayer=1,4
           if(ilayer.eq.1.or.ilayer.eq.3) nstrip=24
           if(ilayer.eq.2.or.ilayer.eq.4) nstrip=40
           do istrip=1,nstrip
             ID_i=256*ilayer+istrip
             write (4,'(5i4,8f8.3)') isect,ilayer,istrip,
     &             pedest(isect,1,ID_i),pedest(isect,2,ID_i)
     &            ,A_l (isect,ilayer,istrip) 
     &            ,A_r (isect,ilayer,istrip)
     &            ,dA_l(isect,ilayer,istrip) 
     &            ,dA_r(isect,ilayer,istrip)
     &            ,cT_l(isect,ilayer,istrip)
     &            ,cT_r(isect,ilayer,istrip)
     &            ,dT_l(isect,ilayer,istrip)
     &            ,dT_r(isect,ilayer,istrip)
           end do
         end do
       end do
       close (4)
       return
 20    print *,' cannot open ', file_calib
       stop
       end

c------------------------------------------------------------------------        
      SUBROUTINE book_ntpl(OutFile)
      
      IMPLICIT NONE  
      include "ntpl_com.inc"
      integer*4 istat,lrecl
      CHARACTER *80 OutFile, OutFile_h
      CHARACTER *8 NtHead(37)
      data lrecl/8191/
      data NtHead/'Nevt','Sector','File','id_xi','id_yi','id_xo','id_yo',
     &           'tot_in','tot_out',
     &           'sum_xi','sum_yi','sum_xo','sum_yo','Nhit',
     &           'T_xi','T_yi','T_xo','T_yo',
     &           'dT_xi','dT_yi','dT_xo','dT_yo',
     &           'RF_1','RF_2','P_trk','charge',
     &           'x_lac','y_lac','z_lac','trk_len',
     &           'cx_tr','cy_tr','cz_tr',
     &           'hix','hiy','hox','hoy'/
	
      call hlimit(128*maxpages)
c to increase max number of records in output hbook file
      IQUEST(10) = 65000            

      OutFile_h = OutFile(1:index(OutFile,' ')-1)//'.hbook'
      write (*,'(" Output file name is   : ",$)')
      write (*,'(bna)') OutFile_h
      call hropen(2,'ESCA',OutFile_h,'NQ',lrecl,istat)
      call hbook2(10,'MIPs spot tot(Sect. 1)',100,15.,100.,100,15.,100.,0.)
      call hbook2(20,'MIPs spot tot(Sect. 2)',100,15.,100.,100,15.,100.,0.)
      call hbprof(111,'In long (Sect. 1)',24,1.,25.,0.,10000.,' ')
      call hbprof(112,'In short (Sect. 1)',40,1.,41.,0.,10000.,' ')
      call hbprof(113,'Out long (Sect. 1)',24,1.,25.,0.,10000.,' ')
      call hbprof(114,'Out short (Sect. 1)',40,1.,41.,0.,10000.,' ')
      call hbprof(121,'In long (Sect. 2)',24,1.,25.,0.,10000.,' ')
      call hbprof(122,'In short (Sect. 2)',40,1.,41.,0.,10000.,' ')
      call hbprof(123,'Out long (Sect. 2)',24,1.,25.,0.,10000.,' ')
      call hbprof(124,'Out short (Sect. 2)',40,1.,41.,0.,10000.,' ')
      call hbookn(200,'Ntuple',37,'//ESCA',100000,NtHead)
     
      return
      end

      SUBROUTINE IZERO(IA,N)
      INTEGER IA(N)
      DO i=1,n
        IA(i) = 0
      END DO
      RETURN
      END
      
