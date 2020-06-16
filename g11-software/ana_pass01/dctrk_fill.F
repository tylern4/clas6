      subroutine dctrk_fill
c
c	filling histograms for DC and TRK monitoring
c

      IMPLICIT NONE

#include "bcs.inc"
#include "sdabgrid.inc"
	
      logical  gelec, gcc
        
      real pi
      parameter (pi = 3.14159)

      INTEGER MAXTRACK
      PARAMETER (MAXTRACK = 8)
      integer TrackIndexH, TrackIndex
      INTEGER MAXLAYER
      PARAMETER (MAXLAYER = 36)
      real momcut_e(2), momcut_p(2)
      real betacut_e(2), betacut_p(2)
      real chi2max
      
      
      integer ITBLA, JTBLA ,LTBLA, ITBTR, LTBTR, ITBID, LTBID
      integer MAMIND, NTBLA, NTBTR, NTBID, NHEVT, LHEVT
      external mamind
      integer NEVNT, LEVNT, IEVNT, EID, CCSTAT, DCSTAT, SCSTAT
      integer idc,sect,hid,nwc,ind
      data NTBLA, NTBTR  /0, 0/
	      
    
      integer NRow, NCol, Status, NumTracks
      integer NumTracks_e, TrackIndex_e
      integer NumTracks_p, TrackIndex_p

      integer i, k, J, Track, Sector, Layer, Region, HistID, isec
      integer NHits(6)
      integer GoodTrackHit, GoodWire

      real TDC, Mom, Vert(3), DirCos(3), Chi2, Charge, phi, beta
      real ResDOCA, FDOCA, CDOCA
      real zdircs, moment, invmass
      parameter (GoodTrackHit = 0)

      logical electron
      gelec=.false.
      gcc=.false.
     	
      TrackIndex = 0 
      momcut_e(1) = 0.1
      momcut_e(2) = 7.5
      momcut_p(1) = 0.1
      momcut_p(2) = 7.5
      betacut_e(1)=0.
      betacut_e(2)=1.2
      betacut_p(1)=0.6
      betacut_p(2)=1.
      chi2max=10.
      
c  Read info from TBLA and TBTR bank and fill histograms in TBT directory

c  Set up Namind pointers to the desired banks

      if (NTBTR.le.0) NTBTR = MAMIND(IW,'TBTR')
      if (NTBLA.le.0) NTBLA = MAMIND(IW,'TBLA')
      if (NTBID.le.0) NTBID = MAMIND(IW,'TBID')
      if (NEVNT.le.0) NEVNT = MAMIND(IW,'EVNT')
      if (NHEVT.le.0) NHEVT = MAMIND(IW,'HEVT')

c  Get the addresses of the TBLA and TBTR banks

      LTBLA = 0
      LTBTR = 0
      LTBID = 0
      LEVNT = 0
      LHEVT = 0      
c  
      if (NTBTR.gt.0)LTBLA = IW(NTBLA)
      if (NTBLA.gt.0)LTBTR = IW(NTBTR)
      if (NTBID.gt.0)LTBID = IW(NTBID)
      if (NEVNT.gt.0)LEVNT = IW(NEVNT)
      if (NHEVT.gt.0)LHEVT = IW(NHEVT)      
  
c

      call hcdir('//PAWC',' ')
      call hcdir('//PAWC/DC',' ')
      idc = mamind(IW,'DC0 ')
      if(idc.gt.0)then
        ind=idc+1
11     ind=iw(ind-1) 
        if(ind.ne.0)then 
          sect=iw(ind-2)
          nwc=nwc+iw(ind)
          hid=1000+sect
          call hf1(hid,float(iw(ind)),1.)
          if(LHEVT.gt.0)then
             if(iw(LHEVT).gt.0)call hf1(hid+10,float(iw(ind)),1.)
          endif
          goto 11
        endif
        if(nwc.gt.0)call hf1(1000,float(nwc),1.)
      endif

    
c  Loop the EVNT bank to see if there is an electron in the event
      IF (gelec) then
      electron = .false.

      if (LEVNT.le.0.or.lhevt.le.0.or.ltbtr.le.0.or.ltbla.le.0) return
      do I = 1,IW(LEVNT - 4)
       IEVNT = LEVNT + (I-1)*IW(LEVNT - 5)
       EID = IW(IEVNT+1)
       DCSTAT = IW(IEVNT+12)
       CCSTAT = IW(IEVNT+13)
       SCSTAT = IW(IEVNT+14)

       if (EID.eq.11) then
        invmass = 0.0
        if (.not.gcc) then
	 electron = .true.
        elseif (gcc.and.CCSTAT.gt.0.and.SCSTAT.gt.0) then
	 electron = .true.
	endif 
       endif
      enddo

      ELSE 
      electron = .true.
      ENDIF
      
      if (.not.electron) return
      

      TrackIndex_e = 0 
      TrackIndex_p = 0 

      if (LTBLA.le.0) Goto 2000
      if (LTBTR.le.0) Then
       ITBTR = 0
      else
       ITBTR = LTBTR
      endif

      ITBLA = LTBLA

1000  NRow   = IW(ITBLA - 4)
      NCol   = IW(ITBLA - 5)
      Sector = IW(ITBLA - 2)

      NumTracks_e = 0
      NumTracks_p = 0

c  Each TBLA bank has 34 entries per track

      NumTracks = NRow/34

      JTBLA = ITBLA

      do Track = 1, NumTracks

       TrackIndex = TrackIndex + 1

c  fill histograms for track indexed information

       if ((ITBTR.gt.0).and.(TrackIndex.le.IW(LTBTR-4))) then

        ITBTR = LTBTR + (TrackIndex-1) * IW(LTBTR-5)

        Vert(3)   = RW(ITBTR+3)

        DirCos(1) = RW(ITBTR+4)
        DirCos(2) = RW(ITBTR+5)
        DirCos(3) = RW(ITBTR+6)
        

        mom = sqrt(DirCos(1)**2+DirCos(2)**2+DirCos(3)**2)

        DirCos(1) = DirCos(1)/mom
        DirCos(2) = DirCos(2)/mom
        DirCos(3) = DirCos(3)/mom
 
        Chi2      = RW(ITBTR+8)
        Charge    = RW(ITBTR+7)

        if (LEVNT.gt.0) then
      
         do I = 1,IW(LEVNT - 4)
          IEVNT = LEVNT + (I-1)*IW(LEVNT - 5)
          EID = IW(IEVNT+1)
          CCSTAT = IW(IEVNT+13)
          DCSTAT = IW(IEVNT+12)
	  if (TrackIndex.eq.DCSTAT) then
	   beta = RW(IEVNT+5)
          endif
         enddo
        endif

        if (DirCos(1).gt.0.0) then
         phi = atan(DirCos(2)/DirCos(1))*180./pi
        elseif (DirCos(1).lt.0.0) then
         if (DirCos(2).ge.0.0) then
          phi = atan(DirCos(2)/DirCos(1))*180./pi+180.
         else
          phi = atan(DirCos(2)/DirCos(1))*180./pi-180.
         endif
        else
         if (DirCos(2).ge.0.0) then
          phi = 90.
         else
          phi = -90.
         endif 
        endif
       
       

c  cuts
	
        if (charge.eq.-1.) then
         if ( (beta.ge.betacut_e(1).and.beta.le.betacut_e(2)) .and.
     1        (mom.ge.momcut_e(1).and.mom.le.momcut_e(2)) .and.
     1         Chi2.le.chi2max ) then
            
          call hcdir('//PAWC/TBTEL',' ')
         else
              goto 3000
         endif

        elseif (charge.eq.+1.) then
         if ( (beta.ge.betacut_p(1).and.beta.le.betacut_p(2)) .and.
     1        (mom.ge.momcut_p(1).and.mom.le.momcut_p(2)) .and.
     1         Chi2.le.chi2max ) then
          call hcdir('//PAWC/TBTPR',' ')
         else 
              goto 3000
         endif
	 
	else

        endif
        
        HistID = 81 + Sector*100 + 7000
        call hfill(HistID,Chi2,         0.,1.)
        HistID = 7001
        call hfill(HistID,Mom,          0.,1.)
        HistID = 7002
        call hfill(HistID,phi,          0.,1.)
        HistID = 7003
        call hfill(HistID,Vert(3),      0.,1.)
 
        HistID = 7004
        call hfill(HistID,Chi2,         0.,1.)

       endif



c  fill histograms for layer indexed information

       do J = 1,34

        Status   = IW(JTBLA + 10)

        if (status .eq. GoodTrackHit) then

         GoodWire = IW(JTBLA + 11)
         TDC      = RW(JTBLA + 12)
         FDOCA    = RW(JTBLA + 16)
         CDOCA    = RW(JTBLA + 17)
         ResDOCA   = abs(FDOCA) - abs(CDOCA)
	 

c  count hits in each superlayer in the sector

         if (J.le.4) then
            Region = 1
            Layer = J
            
         elseif (J.le.10) then
            Region = 2 
            Layer = J + 2
            
         elseif (J.le.16) then
            Region=3
            Layer = J+2
         elseif (J.le.22) then
            Region=4
            Layer = J+2
            
         elseif (J.le.28) then
            Region = 5
            Layer = J+2
         elseif (J.le.34) then
            Region = 6 
            Layer = J+2
         endif

	
         HistID = 32 + Sector*100 + 7000
         call hfill(HistID,float(GoodWire),float(Layer),1.)
        
       
	 
         HistID = 41 + Sector*100 + Region*1000 
         call hfill(HistID,TDC,0.,1.)

         HistID = 42 + Sector*100 + Region*1000 
         call hfill(HistID,ResDOCA,0.,1.)
         HistID = 42 + Region*1000 
         call hfill(HistID,ResDOCA,0.,1.)
         HistID = 43 + Sector*100 + Region*1000
         call hfill(HistID,TDC,ResDOCA,1.)
         HistID = 44 + Sector*100 + Region*1000
         call hfill(HistID,FDOCA,ResDOCA,1.)
   
        endif

        JTBLA = JTBLA + NCol

       enddo
  
  	
3000   continue

       call hcdir('//PAWC',' ')

      enddo
      
      ITBLA = IW(ITBLA-1)
      if (ITBLA.gt.0) goto 1000    

2000  continue

      return
      end
