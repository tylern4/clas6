      SUBROUTINE Ec_store_bos(ok)
c*************************************************************************
c* purpose : store info to EcEvu from BOS bank for each event
c* called by Ec_evnb
c* output ok(Ec_MAXsector): T if there is data in the sector
c*                          F if no data in the sector
c* 2/10/95   Z.Li
c*
c*************************************************************************   
      IMPLICIT NONE
      SAVE
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_store_bos.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/04/11 00:56:59 $')
      PARAMETER (CAUTHO = '$Author: lcsmith $')
      DATA CRCSID/ 
     1'$Id: ec_store_bos.F,v 1.7 1999/04/11 00:56:59 lcsmith Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ec_store_bos')                                   
      PARAMETER (CRAUTH='Zhujun Li')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
#include "bcs.inc"
#include "Ec_general.PAR"
#include "EcEvu.CMN"
* external functions
      integer MAMIND  !1 bos
*
* subroutine parameter
c      integer ierr
c* local varibles
      logical ok( MAXsectors)
      integer nd,nd2,ind,il,id,i,j,sector,nami,mth,iloc(MAXstrips)
      integer N_UI,N_VI,N_WI,N_UO,N_VO,N_WO
      integer idUI( MAXstrips),idVI( MAXstrips),idWI( MAXstrips)
      integer idUO( MAXstrips),idVO( MAXstrips),idWO( MAXstrips)
      real aUI( MAXstrips),aVI( MAXstrips),aWI( MAXstrips)
      real aUO( MAXstrips),aVO( MAXstrips),aWO( MAXstrips)
      real tUI( MAXstrips),tVI( MAXstrips),tWI( MAXstrips)
      real tUO( MAXstrips),tVO( MAXstrips),tWO( MAXstrips)
      data nami/0/
      integer*2 I16(2000)            ! 16 bits
      integer*4 I32(1000)            ! 32 bits
      integer*4 ichk
      equivalence (I16(1), I32(1))
* temp
      integer itest
*
c        call FSEQR('BOSINPUT',ierr)    ! get from input
c        OKrd=ierr.ge.0
*
c        IF(.not.OKrd) print *,' BOS open: OKrd=',OKrd
c        IF(OKrd) THEN

         If (nami.eq.0) nami = MAMIND(IW,'EC  ') !return name-index for NAME (EC)
          ind=nami+1               ! initialize ind
          do sector=1,MAXsectors
          ok(sector)=.false.
          enddo
30        continue
          ind = IW (ind-1)         !next index
          If(ind.eq.0) go to 99
          sector=IW(ind-2)
          if(sector.lt.1.or.sector.gt.MAXsectors) go to 30
          nd=    IW(ind)           ! # of data word
          if(nd.eq.0) go to 30
          itest=nd/3*3
          do j=1,nd
            I32(j)=IW(ind+j)       ! bos stored in 32 bit
          end do
c* unpack and store
          if(itest.lt.nd) nd2=2*nd-1
          if(itest.eq.nd) nd2=2*nd
          N_UI=0
          N_UO=0
          N_VI=0
          N_VO=0
          N_WI=0
          N_WO=0
          do 31, i=1,nd2,3
            ichk = i16(i)
             id = mod (ichk, 256) ! strip #
             il = ichk/256  ! UVWUVW (1-6)
            if(il.lt.1.or.il.gt.6) go to 31
            if(il.eq.1) then
              N_UI=N_UI+1
              idUI(N_UI)=id
              tUI(N_UI)=float(I16(i+1))
              aUI(N_UI)=float(I16(i+2))
            endif
            if(il.eq.2) then
              N_VI=N_VI+1
              idVI(N_VI)=id
              tVI(N_VI)=float(I16(i+1))
              aVI(N_VI)=float(I16(i+2))
            endif
            if(il.eq.3) then
              N_WI=N_WI+1
              idWI(N_WI)=id
              tWI(N_WI)=float(I16(i+1))
              aWI(N_WI)=float(I16(i+2))
            endif
            if(il.eq.4) then
              N_UO=N_UO+1
              idUO(N_UO)=id
              tUO(N_UO)=float(I16(i+1))
              aUO(N_UO)=float(I16(i+2))
            endif
            if(il.eq.5) then
              N_VO=N_VO+1
              idVO(N_VO)=id
              tVO(N_VO)=float(I16(i+1))
              aVO(N_VO)=float(I16(i+2))
            endif
            if(il.eq.6) then
              N_WO=N_WO+1
              idWO(N_WO)=id
              tWO(N_WO)=float(I16(i+1))
              aWO(N_WO)=float(I16(i+2))
            endif
c            print *, sector,il,id,float(i16(i+1)),float(i16(i+2))
31        continue
          call EcGus_int_sort(N_UI,idUI,iloc)  !sort by increasing id#
          EcEvu_Nreported(Ec_u, inner,sector)=N_UI
          do mth=1,N_UI
            id=idUI(iloc(mth))
c            print *, id,tui(iloc(mth))
            EcEvu_reportedID(mth,Ec_u, inner,sector)=id
            EcEvu_strip( ADC,id, Ec_u, inner,sector)=aUI(iloc(mth))
            EcEvu_strip( TDC,id, Ec_u, inner,sector)=tUI(iloc(mth))
          enddo
*
          call EcGus_int_sort(N_VI,idVI,iloc)  !sort by increasing id#
          EcEvu_Nreported(Ec_v, inner,sector)=N_VI
          do mth=1,N_VI
            id=idVi(iloc(mth))
c            print *, id,tvi(iloc(mth))
            EcEvu_reportedID(mth, Ec_v, inner,sector)=id
            EcEvu_strip( ADC,id, Ec_v, inner,sector)=aVI(iloc(mth))
            EcEvu_strip( TDC,id, Ec_v, inner,sector)=tVI(iloc(mth))
          enddo
c*
          call EcGus_int_sort(N_WI,idWI,iloc)  !sort by increasing id#
          EcEvu_Nreported( Ec_w, inner,sector)=N_WI
          do mth=1,N_WI
            id=idWI(iloc(mth))
c            print *, id,twi(iloc(mth))          
            EcEvu_reportedID(mth, Ec_w, inner,sector)=id
            EcEvu_strip( ADC,id, Ec_w, inner,sector)=aWI(iloc(mth))
            EcEvu_strip( TDC,id, Ec_w, inner,sector)=tWI(iloc(mth))
          enddo
c*
          call EcGus_int_sort(N_UO,idUO,iloc)  !sort by increasing id#
          EcEvu_Nreported( Ec_u, outer,sector)=N_UO
          do mth=1,N_UO
            id=idUO(iloc(mth))
c            print *, id,tuo(iloc(mth))
            EcEvu_reportedID(mth, Ec_u, outer,sector)=id
            EcEvu_strip( ADC,id, Ec_u, outer,sector)=aUO(iloc(mth))
            EcEvu_strip( TDC,id, Ec_u, outer,sector)=tUO(iloc(mth))
          enddo
c*
          call EcGus_int_sort(N_VO,idVO,iloc)  !sort by increasing id#
          EcEvu_Nreported( Ec_v, outer,sector)=N_VO
          do mth=1,N_VO
            id=idVO(iloc(mth))
c            print *, id,tvo(iloc(mth))
            EcEvu_reportedID(mth, Ec_v, outer,sector)=id
            EcEvu_strip( ADC,id, Ec_v, outer,sector)=aVO(iloc(mth))
            EcEvu_strip( TDC,id, Ec_v, outer,sector)=tVO(iloc(mth))
          enddo
c*
          call EcGus_int_sort(N_WO,idWO,iloc)  !sort by increasing id#
          EcEvu_Nreported( Ec_w, outer,sector)=N_WO
          do mth=1,N_WO
            id=idWO(iloc(mth))
c            print *, id,two(iloc(mth))
            EcEvu_reportedID(mth, Ec_w, outer,sector)=id
            EcEvu_strip( ADC,id, Ec_w, outer,sector)=aWO(iloc(mth))
            EcEvu_strip( TDC,id, Ec_w, outer,sector)=tWO(iloc(mth))
          enddo
          ok(sector)=.true.
c* go to next sector
          go to 30
c           print *, ' '
c          EndIf
99    return
      end
