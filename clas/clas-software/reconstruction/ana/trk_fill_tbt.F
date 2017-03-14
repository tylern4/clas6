      SUBROUTINE trk_fill_tbt
c
c_begin_doc 
c  RCS ID string
c  $Id: trk_fill_tbt.F,v 1.4 1999/03/09 17:40:43 boiarino Exp $
c
c  Documentation for subroutine trk_fill_tbt
c
c  Purpose: Histogramming of tbt data.  These histograms are booked in trk_init.
c  --------
c
c  Input Parameters:  none
c  ----------------
c
c  Output Parameters:  none
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:    Called from trk_fill
c  ------
c
c  Author:   Francois Roudot      Created:  Thu May 29 16:00:00 EDT 1997
c  -------   
c
c  Major revisions:  Franz Klein  Nov.1997:  changes due to new tracking banks
c  ----------------
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
c#include "sdakeys.inc"
#include "sdadigi.inc"
#include "trktcl_var.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: trk_fill_tbt.F,v $')
 1    PARAMETER (CREVIS = '$Revision: 1.4 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 17:40:43 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: trk_fill_tbt.F,v 1.4 1999/03/09 17:40:43 boiarino Exp $'   
     2/   
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='trk_fill_tbt')
      PARAMETER (CRAUTH='Francois H. Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
 
      Integer  ITBLA,JTBLA,ITBTR
      Integer  LTBLA,LTBTR

      integer  MAMIND
      Integer  NTBLA,NTBTR
      Save     NTBLA,NTBTR
      Data     NTBLA,NTBTR  / 0,0 /

      integer NRow,NCol

      integer J,                              ! real layer number 1-34
     &        i, k,                           ! general purpose counters
     &        Track, NumTracks,HTrack,        ! index TBLA,#/sector,index TBTR
     &        NHits(6), NumHits(3,2),SumHits, ! num of hits per Region,SuperLayer
     &        GoodTrackHit,
     &        Sector, Layer, Region,
     &        GoodWire,SynthWire, TDC,
     &        Status,
     &        HistID,
     &        WirInLay(10,3)               ! the last real hit per track,region

      real    Momentum,
     &        Vert(3), Mom(3), Chi2,
     &        Charge,
     &        FitDOCA, LocAng

      real trk_TBLA_info(7)
      real trk_TBTR_info(19) 
      integer irun,ievnt,irec

      parameter (GoodTrackHit = 0)

c_end_var
c
c  executable code for routine trk_fill_tbt:
c----6----------------------------------------------------------------
	


       if (ipar_trk_histo.ge.1.or.ipar_trk_ntuple.ge.1) then

         CALL RERNEV(irun,ievnt,irec)                  
         call hcdir('tbt',' ')

*        Set up Mamind pointers to the desired banks.
         If ( NTBTR .ge. 0 ) NTBTR = MAMIND(IW,'TBTR')
         If ( NTBLA .ge. 0 ) NTBLA = MAMIND(IW,'TBLA')

*        Get the addresses of the "first" TBLA and TBTR banks.
         LTBLA = IW(NTBLA)
         LTBTR = IW(NTBTR)

         If ( LTBLA .le. 0 ) Goto 2000          ! if no TBLA bank, then bag it
         If ( LTBTR .le. 0 ) Then
            ITBTR = 0
         Else
            ITBTR = LTBTR
         Endif

c        fill number of tracks per event
         HistID = 30 + 7700
         call hfill(HistID,float(IW(lTBTR-4)),0.,1.)

         ITBLA = LTBLA
         NumTracks = 0
         HTrack = 0

 500     NRow   = iw(ITBLA - 4)
         NCol   = iw(ITBLA - 5)
         Sector = iw(ITBLA - 2)

c        Each TBLA bank has 34 entries per track, one per "real" layer
         NumTracks = Nrow/34

c        fill number of tracks per sector
         HistID = 30 + Sector*100 + 7000
         call hfill(HistID,float(NumTracks),0.,1.)

         JTBLA = ITBLA
         Do 1000 Track = 1,NumTracks
            HTrack = HTrack + 1
crat        GoodWire contains the wire number of the _real_ hit from the Jth layer that was associated with the track.  SynthWire contains the wire number of the hit that recsis had to synthesize (ie invent) in order to have one hit per layer. 

            do i=1,6
               Nhits(i) = 0
            end do
            do j=1,3
               do i=1,2
                  NumHits(j,i) = 0
               end do
            end do

c------------------------------------------------------------
crat        histo & ntuple quantities that are layer indexed
c------------------------------------------------------------
            Do 800 J = 1,34                ! loop over the 34 real hits
               Status   = iw(JTBLA + 10)
crat           fill most histograms only for the real hits: 
               if (status .eq. GoodTrackHit) then

                  GoodWire      = iw(JTBLA + 11)
                  TDC           = rw(JTBLA + 12)
                  FitDOCA       = rw(JTBLA + 16)
                  LocAng        = rw(JTBLA + 13)

crat              perform hit counting:
cyun 9/9/97	correct the region1 layer #


                  if ( J .le. 10) then
                     Region = 1
cyun                 Layer = J
                     WirInLay(Track,Region) = Goodwire
                     if (J.le.4) then                                ! SSL
			Layer = J
                        NumHits(Region,1) = NumHits(Region,1) + 1
                     elseif (J.ge.5) then                            ! ASL
			Layer = J + 2
                        NumHits(Region,2) = NumHits(Region,2) + 1
                     end if
                  endif

                  If ((J .ge. 11).and.(J.le.22)) then
                     Region = 2
                     Layer = J+2
                     WirInLay(Track,Region) = Goodwire
                     if (J.ge.17) then                                ! SSL
                        NumHits(Region,1) = NumHits(Region,1) + 1
                     else                                             ! ASL
                        NumHits(Region,2) = NumHits(Region,2) + 1
                     end if
                  endif
		  
                  If ( J .ge. 23) then
                     Region = 3
                     Layer = J+2
                     WirInLay(Track,Region) = Goodwire
                     if (J.ge.29) then                                ! SSL
                        NumHits(Region,1) = NumHits(Region,1) + 1
                     else                                             ! ASL
                        NumHits(Region,2) = NumHits(Region,2) + 1
                     end if
                  endif


cyun 9/9/97

crat              fill histograms if needed                  
                  if ( ipar_trk_histo.ge. 1) then
                     HistID = 32 + Sector*100 + 7000
                     call hfill(HistID,float(GoodWire),float(Layer),1.)
                     HistID = 34 + Sector*100 + Region*1000
                     call hfill(HistID,float(TDC),0.,1.)
                     HistID = 44 + Sector*100 + Region*1000
                     call hfill(HistID,FitDOCA,0.,1.)
                     LocAng = LocAng*180/3.1416
                     HistID = 45 + Sector*100 + 7000
                     call hfill(HistID,LocAng,float(Layer),1.)
                     HistID = 46 + Sector*100 + Region*1000
                     call hfill(HistID, float(TDC), FitDOCA, 1.)
                  end if

crat              fill  ntuples if needed
                  if(ipar_trk_ntuple.ge.1) then
                     trk_TBLA_info(1) = float(ievnt)                  ! event number
                     trk_TBLA_info(2) = float(Sector)                 ! sector number
                     trk_TBLA_info(3) = float(Layer)                  ! layer number
                     trk_TBLA_info(4) = float(GoodWire)               ! wire number
                     trk_TBLA_info(5) = FitDOCA    ! tbt fitted distance of 
                                                   ! closest approach
                     trk_TBLA_info(6) = TDC                    ! raw tdc
                     trk_TBLA_info(7) = LocAng                 ! local angle
                     call hfn(111,trk_TBLA_info)
                  endif                                    

               else                                   ! the hit was synthesized
crat              If the hit was synthesized, its wire number is negative in the TBLA bank.
                  SynthWire    = -iw(JTBLA+ 11)
                  HistID = 33 + Sector*100 + 7000
                  if (ipar_trk_histo.ge.1) call hfill(HistID,float(SynthWire),float(Layer),1.)

               endIf
               
               

               JTBLA = JTBLA + NCol

 800        Continue                             ! end of loop over real layers
        
crat        sum up the number of real hits
            do k=1,3
               do i=1,2
                  NHits(Sector) = NHits(Sector) + NumHits(k,i)
               end do
            end do
c------------------------------------------------------------
crat        histo & ntuple quantities that are track indexed
c------------------------------------------------------------

c           fill number of hits per track
            if (ipar_trk_histo.ge. 1) then
               SumHits = 0
               do i=1,6
                  SumHits = SumHits + NHits(i)
               end do
               HistID = 31 + 7700
               call hfill(HistID,float(SumHits),0.,1.)
               HistID = 31 + Sector*100 + 7000
               call hfill(HistID,float(NHits(Sector)),0.,1.)
            end if            

            if ((ITBTR.gt.0).and.(HTrack.ge.Iw(LTBTR-4))) then
               ITBTR = LTBTR + (HTrack-1) * Iw(LTBTR-5)
               mom(1) = RW(ITBTR+4)
               mom(2) = RW(ITBTR+5)
               mom(3) = RW(ITBTR+6)
               Momentum  = sqrt(rw(iTBTR+4)**2+rw(iTBTR+5)**2+rw(iTBTR+6)**2)
               if (Momentum.ne.0.0) then
                mom(1) = mom(1)/Momentum
                mom(2) = mom(2)/Momentum
                mom(3) = mom(3)/Momentum
	       endif
	       
               Vert(1)   = RW(ITBTR+1)
               Vert(2)   = RW(ITBTR+2)
               Vert(3)   = RW(ITBTR+3)
               Chi2      = RW(ITBTR+8)
               Charge    = RW(ITBTR+7)
               
crat           fill histograms if requested
               if (ipar_trk_histo.ge. 1) then
                  call hfill(7735,Chi2,         0.,1.)
                  call hfill(7736,Momentum,     0.,1.)
                  call hfill(7737,Vert(1),      0.,1.)
                  call hfill(7738,Vert(2),      0.,1.)
                  call hfill(7739,Vert(3),      0.,1.)
                  call hfill(7740,mom(1),       0.,1.)
                  call hfill(7741,mom(2),       0.,1.)
                  call hfill(7742,mom(3),       0.,1.)
c                 modif for aix charge is define real no float needed
                  call hfill(7743,Charge,0.,1.)
               end if

crat           fill ntuples if requested
               if (ipar_trk_ntuple.ge. 1) then
                  trk_TBTR_info(1) = float(Sector)
                  do k=1,3                               ! loop over region
                     do i=1,2                            ! loop over super layer
                        trk_TBTR_info(1 + 2*(k-1) + i) = NumHits(k,i)
                     end do
                  end do
                  do i=1,3
                     trk_TBTR_info(i+7) = Vert(i)
                     trk_TBTR_info(i+10) = mom(i)
                     trk_TBTR_info(i+13) = WirInLay(Track,i)
                  end do
                  trk_TBTR_info(17)  = Chi2
                  trk_TBTR_info(18)  = Momentum
                  trk_TBTR_info(19)  = Charge

                  call hfn(112,trk_TBTR_info)
               end if

            end if

 1000    continue

         ITBLA = IW(ITBLA-1)
         If ( ITBLA .gt. 0 ) goto 500


 2000    continue

         IF (NWRITE .LT. 1) THEN
            NWRITE = NWRITE + 1
            CRMESS='Time based TRK histograms are being filled '
            CALL RECMES(CRNAME,'I',CRMESS)
         ENDIF
         call hcdir('//PAWC/TRK',' ')
      endif

      RETURN
      END
c
c------------------------------------------------------------------------------








