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

      real momcut_e(2), momcut_p(2)
      real betacut_e(2), betacut_p(2)
      real chi2max
      
      integer ITBTR, LTBTR, ITBLA, LTBLA_sec(6)
      integer LEVNT, IEVNT, EID, CCSTAT, DCSTAT, SCSTAT
      integer MAMIND, NTBLA, NTBTR, NDC0, NEVNT
      data NTBLA,NTBTR,NEVNT,NDC0  /0, 0, 0, 0/
	      
      integer NRow, NCol, Ncol_TBLA, ndata, Status
      integer ntrks_per_sector(6)

      integer sect,ind, idsec, itrk, itrk_sec
      integer i, k, J, Track, Sector, Layer, Superlayer, HistID, isec
      integer NHits(6)
      integer GoodTrackHit, GoodWire
      integer wire,myent,id
      integer*2 myent2(2)
      equivalence (myent,myent2(1))

      real TDC, Mom, Vert(3), p(3), Chi2, Charge, phi, theta, beta
      real ResDOCA, FDOCA, CDOCA, langle
      parameter (GoodTrackHit = 0)

      logical m2cut,lacut,bcut1,bcut2
      real m2, invmass
      logical electron
      gelec=.false.
      gcc=.false.
     	
      momcut_e(1) = 0.2
      momcut_e(2) = 4.5
      momcut_p(1) = 0.2
      momcut_p(2) = 4.5
      betacut_e(1)=0.2
      betacut_e(2)=1.2
      betacut_p(1)=0.2
      betacut_p(2)=1.2
      chi2max=50.
      do sect=1,6
        LTBLA_sec(sect)=0
        ntrks_per_sector(sect)=0
      enddo

c  Read info from TBLA and TBTR bank and fill histograms in TBT directory

c  Set up Namind pointers to the desired banks

      if (NTBTR.le.0) NTBTR = MAMIND(IW,'TBTR')
      if (NTBLA.le.0) NTBLA = MAMIND(IW,'TBLA')
      if (NEVNT.le.0) NEVNT = MAMIND(IW,'EVNT')
      if (NDC0 .le.0) NDC0  = mamind(IW,'DC0 ')

c  Get the addresses of the TBLA and TBTR banks

      LTBTR = IW(NTBTR)
      LEVNT = IW(NEVNT)
      ind=NTBLA+1
      do while (iw(ind-1).gt.0)
         ind=iw(ind-1)
         sect=iw(ind-2)
         ncol_tbla=iw(ind-5)
         LTBLA_SEC(sect)=ind
         ntrks_per_sector(sect)=iw(ind-4)/34 !34 rows per track
      enddo
c

      call hcdir('//PAWC/DC',' ')
      ind=NDC0+1
11    ind=iw(ind-1) 
      if(ind.ne.0)then 
         sect=iw(ind-2)
         histID=1000+sect
         call hf1(histID,float(iw(ind)),1.)
         if(ntrks_per_sector(sect).gt.0)call hf1(histID+10,float(iw(ind)),1.)
         do i=1,iw(ind)
            myent=iw(ind+i)
            id=myent2(1)
            wire=mod(id,256)
            layer=id/256
            call hfill(histID+20,float(wire),float(layer),1.)
         enddo
         goto 11
      endif
      call hcdir('//PAWC',' ')

c break if no tracking banks

      if (LTBTR.le.0.or.iw(NTBLA).le.0) return


c  Loop the EVNT bank to see if there is an electron in the event

      electron = .not.gelec

      IF (gelec) then
         if (LEVNT.le.0) return
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
         if (.not.electron) return
      ENDIF
      
cfk sorry I don't understand the logic of the previous code
c
c  start with TBTR bank

      nrow=IW(LTBTR-4)
      ncol=IW(LTBTR-5)
      
      do itrk=1,nrow

         ITBTR=LTBTR+(itrk-1)*ncol

         do i=1,3
            vert(i)=RW(ITBTR+i)
            p(i)   =RW(ITBTR+i+3)
         enddo
         mom  = sqrt(p(1)**2+p(2)**2+p(3)**2)
         phi  = atan2(p(2),p(1))
         theta= acos(p(3)/mom)
         Charge= RW(ITBTR+7)
         Chi2  = RW(ITBTR+8)
c  idsec=100*sector+trkno_in_sector
         idsec = IW(ITBTR+9)
         itrk_sec=mod(idsec,100)
         sector  =idsec/100

         if(itrk_sec.LT.1 .OR. sector.lt.1 .or. sector.gt.6) goto 3000

c get beta and part.id
         beta=0.01
         if (LEVNT.gt.0) then
            do I = 1,IW(LEVNT - 4)
               IEVNT = LEVNT + (I-1)*IW(LEVNT - 5)
               DCSTAT = IW(IEVNT+12)
               if (itrk.eq.DCSTAT) then
                  beta = RW(IEVNT+5)
                  EID  = IW(IEVNT+1)
                  CCSTAT=IW(IEVNT+13)
               endif
            enddo
         endif
c  cuts
         if(chi2.gt.chi2max) goto 3000
         if (charge.lt.0.) then
            if( electron .and.
     1           (beta.ge.betacut_e(1).and.beta.le.betacut_e(2)) .and.
     1           (mom.ge.momcut_e(1).and.mom.le.momcut_e(2)) )then
               
               call hcdir('//PAWC/TBTEL',' ')
            else
               goto 3000
            endif
            
         elseif (charge.gt.0) then
            if ( (beta.ge.betacut_p(1).and.beta.le.betacut_p(2)) .and.
     1           (mom.ge.momcut_p(1).and.mom.le.momcut_p(2)) )then
               call hcdir('//PAWC/TBTPR',' ')
            else 
               goto 3000
            endif
         endif
         call hfill(7004,            Chi2, 0.,1.)
         call hfill(7081+sector*100, Chi2, 0.,1.)
         call hfill(7001,            Mom,  0.,1.)
         call hfill(7001+sector*100, Mom,  0.,1.)
         call hfill(7002,            phi,  0.,1.)
         call hfill(7003,         Vert(3), 0.,1.)

c  fill histograms for layer indexed information

         if(LTBLA_sec(sector).EQ.0) goto 3000
         ndata=iw(LTBLA_sec(sector))
         ITBLA = LTBLA_sec(sector)+(itrk_sec-1)*ncol_tbla*34

c  check whether tbla entries for this track was really saved
         if(itrk_sec*ncol_tbla*34.GT.ndata) goto 3000

         do J = 1,34
            Status   = IW(ITBLA + 10)

            if (status .eq. GoodTrackHit) then

               GoodWire = IW(ITBLA + 11)
               TDC      = RW(ITBLA + 12)
               langle   = RW(ITBLA + 13)
               FDOCA    = RW(ITBLA + 16)
               CDOCA    = RW(ITBLA + 17)
               ResDOCA   = abs(FDOCA) - abs(CDOCA)
               langle=langle*180./3.14156 

c  count hits in each superlayer in the sector

               if (J.le.4) then
                  Layer = J
               else 
                  Layer = J+2
               endif
               Superlayer=(Layer+5)/6

               HistID = 32 + Sector*100 + 7000
               call hfill(HistID,float(GoodWire),float(Layer),1.)
        
               HistID = 41 + Sector*100 + Superlayer*1000 
               call hfill(HistID,TDC,0.,1.)
c               HistID = 42 + Sector*100 + Superlayer*1000 
c               call hfill(HistID,ResDOCA,0.,1.)
c               HistID = 42 + Superlayer*1000 
c               call hfill(HistID,ResDOCA,0.,1.)
               HistID = 43 + Sector*100 + Superlayer*1000
               call hfill(HistID,TDC,ResDOCA,1.)
               HistID = 44 + Sector*100 + Superlayer*1000
               call hfill(HistID,FDOCA,ResDOCA,1.)
   
               m2cut =.false.
               lacut =.false.
               bcut1  =.false.
               bcut2  =.false.
               if(charge.gt.0) then
                  m2=mom**2*(1/beta**2-1)
                  m2cut=(m2.gt.0.7**2.AND.M2.lt.1.13**2)
                  bcut1 = beta.lt.1..AND.beta.gt.0.4
                  bcut2 = beta.lt.1.
c                 m2cut =.true.
c                 bcut1 =.true.
c                 bcut2 =.true.            
               else
                  m2cut =.true.
                  bcut1 =.true.
                  bcut2 =.true.
               endif
cfk cuts made only sense for electrons -
cfk change them for protons and reverse meaning of xy45 and xy46
               if(Superlayer.lt.3) then
                  lacut=langle.gt.-29.AND.langle.lt.-1
               elseif(Superlayer.gt.4) then
                  lacut=langle.gt.-10.AND.langle.lt.-1
               else
                  lacut=langle.gt.-15.AND.langle.lt.-1
               endif
c         if(m2cut.AND.lacut.AND.bcut1.AND.chi2.lt.2.5) then
               if( .TRUE. ) then
                  HistID = 45 + Superlayer*1000 
                  call hfill(HistID,ResDOCA,0.,1.)
                  HistID = 45 + Sector*100 + Superlayer*1000 
                  call hfill(HistID,ResDOCA,0.,1.)
               endif
               if(m2cut.AND.lacut.AND.bcut2.AND.chi2.lt.2.5) then
                  HistID = 46 + Superlayer*1000 
                  call hfill(HistID,ResDOCA,0.,1.)
                  HistID = 46 + Sector*100 + Superlayer*1000 
                  call hfill(HistID,ResDOCA,0.,1.)
               endif

            endif

            ITBLA = ITBLA + NCol_TBLA
         enddo                                  !loop over layers
  
  	
 3000    continue

         call hcdir('//PAWC',' ')

      enddo                                     !loop over tracks in TBTR
      
      return
      end
