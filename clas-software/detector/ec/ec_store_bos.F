      SUBROUTINE Ec_store_bos(ok)
c*************************************************************************
c     * purpose : store info to EcEvu from BOS bank for each event
c     * called by Ec_evnb
c     * output ok(Ec_MAXsector): T if there is data in the sector
c     *                          F if no data in the sector
c     * 2/10/95   Z.Li
c     *
c     * Modified by H.S. Jo - Nov 2005
c     * to work with the new multihit pipeline TDC (ECT BOS bank)
c     * using a database status flag for old-new TDC (TDCstat)
c*************************************************************************
      IMPLICIT NONE
      SAVE
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     rcs
c     RCS information:                                                       rcs
c     rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_store_bos.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.12 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:16 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/ 
     1     '$Id: ec_store_bos.F,v 1.12 2008/03/16 04:33:16 fklein Exp $'
     2     / 
c     Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ec_store_bos')                                   
      PARAMETER (CRAUTH='Zhujun Li')
c     rcs
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     
#include "bcs.inc"
#include "Ec_general.PAR"
#include "EcEvu.CMN"
#include "EcCal.CMN"
*     external functions
      integer MAMIND            !1 bos
*     
*     subroutine parameter
c     integer ierr
c     * local varibles
      logical ok( MAXsectors),preshower
      integer nd,nd2,ind,il,id,i,j,sector,nami,mth,iloc(MAXstrips)
      integer nd_ect,nd2_ect,ind_ect,il_ect,id_ect,sector_ect,nami_ect,i_ect
      integer sectorflag,repl, nami_ec2,ind_ec2
      integer axis,layer
      real tdcstatus
      integer N_UI,N_VI,N_WI,N_UO,N_VO,N_WO
      integer idUI( MAXstrips),idVI( MAXstrips),idWI( MAXstrips)
      integer idUO( MAXstrips),idVO( MAXstrips),idWO( MAXstrips)
      real aUI( MAXstrips),aVI( MAXstrips),aWI( MAXstrips)
      real aUO( MAXstrips),aVO( MAXstrips),aWO( MAXstrips)
      real tUI( MAXstrips),tVI( MAXstrips),tWI( MAXstrips)
      real tUO( MAXstrips),tVO( MAXstrips),tWO( MAXstrips)
      data nami/0/
      data nami_ect/0/
      data sector_ect/0/
      data sectorflag/0/
      data repl/0/
      integer*2 I16(2000)       ! 16 bits
      integer*4 I32(1000)       ! 32 bits
      integer*4 ichk
      equivalence (I16(1), I32(1))
      integer*2 J16(2000)       ! 16 bits
      integer*4 J32(1000)       ! 32 bits
      integer*4 ichk_ect
      equivalence (J16(1), J32(1))
*     temp
      integer itest
*     
c     call FSEQR('BOSINPUT',ierr)    ! get from input
c     OKrd=ierr.ge.0
*     
c     IF(.not.OKrd) print *,' BOS open: OKrd=',OKrd
c     IF(OKrd) THEN
      
      If (nami.eq.0) nami = MAMIND(IW,'EC  ') !return name-index for NAME (EC)
      ind=nami+1                ! initialize ind
c     new: read ECT bank (new multihit pipeline TDC)
      If (nami_ect.eq.0) nami_ect = MAMIND(IW,'ECT ') !return name-index for NAME (ECT)
c     new: for pre-shower
      If (nami_ec2.eq.0) nami_ec2 = MAMIND(IW,'EC2 ') !return name-index for NAME (ECT)
      ind_ec2=nami_ec2+1        ! initialize ind_ec2
      preshower=.false.
c     
      do sector=1,MAXsectors
         ok(sector)=.false.
      enddo
 30   continue
      ind = IW (ind-1)          !next index
      If(ind.eq.0) then
         If(ind_ec2.gt.0.and..not.preshower)then
            ind=ind_ec2
            preshower=.true.
            go to 30
         else
            go to 99
         endif
      endif
      sector=IW(ind-2)
      if(sector.lt.1.or.sector.gt.MAXsectors) go to 30
      nd=    IW(ind)            ! # of data word
      if(nd.eq.0) go to 30
      itest=nd/3*3
      do j=1,nd
         I32(j)=IW(ind+j)       ! bos stored in 32 bit
      end do
c     * unpack and store
      if(itest.lt.nd) nd2=2*nd-1
      if(itest.eq.nd) nd2=2*nd
      if(.not.preshower)then
         sectorflag=0
         nd2_ect=0
         ind_ect=nami_ect+1     ! initialize ind_ect
 32      continue
         ind_ect = IW (ind_ect-1) !next index
         if(ind_ect.gt.0)then
            sector_ect=IW(ind_ect-2)
c     check if sector'ECT'=sector'EC'
            if(sector_ect.eq.sector) sectorflag=1
            if((sector_ect.ne.0).and.(sector_ect.ne.sector).and.(sector_ect.ne.MAXsectors)) go to 32
            if (sectorflag.eq.1) then
               nd_ect = IW(ind_ect) ! # of data word
               if(nd_ect.eq.0) go to 32
               do j=1,nd_ect
                  J32(j)=IW(ind_ect+j) ! bos stored in 32 bit
               end do
               nd2_ect=2*nd_ect
            endif
         endif
      endif
      N_UI=0
      N_UO=0
      N_VI=0
      N_VO=0
      N_WI=0
      N_WO=0
      do 31, i=1,nd2,3
         ichk = i16(i)
         id = mod (ichk, 256)   ! strip #
         il = ichk/256          ! UVWUVW (1-6)
         if((il.eq.1).or.(il.eq.4)) axis = Ec_u
         if((il.eq.2).or.(il.eq.5)) axis = Ec_v
         if((il.eq.3).or.(il.eq.6)) axis = Ec_w
         if ((il.ge.1).and.(il.lt.4)) layer = inner
         if ((il.ge.4).and.(il.lt.7)) layer = outer
c     get the value of the database status flag 'TDCstat' (old-new TDC)   
         tdcstatus = EcCal_TDCstat(id,axis,layer,sector)
         repl=0
         if(il.eq.1) then
            N_UI=N_UI+1
            idUI(N_UI)=id
            tUI(N_UI)=float(I16(i+1))
            aUI(N_UI)=float(I16(i+2))
            if ((sectorflag.eq.1).and.(tdcstatus.eq.1)) then
               do i_ect=1,nd2_ect,2
                  ichk_ect = j16(i_ect)
                  id_ect = mod (ichk_ect, 256) ! strip #
                  il_ect = ichk_ect/256 ! UVWUVW (1-6)
                  if ((il_ect.eq.il).and.(id_ect.eq.id).and.(repl.eq.0)) then
                     if(float(J16(i_ect+1)).eq.0.) then
                        tUI(N_UI)=float(J16(i_ect+1))
                     else
                        tUI(N_UI)=5700-float(J16(i_ect+1))
                     endif
                     repl=1
                  endif
               enddo
            endif
         endif
         if(il.eq.2) then
            N_VI=N_VI+1
            idVI(N_VI)=id
            tVI(N_VI)=float(I16(i+1))
            aVI(N_VI)=float(I16(i+2))
            if ((sectorflag.eq.1).and.(tdcstatus.eq.1)) then
               do i_ect=1,nd2_ect,2
                  ichk_ect = j16(i_ect)
                  id_ect = mod (ichk_ect, 256) ! strip #
                  il_ect = ichk_ect/256 ! UVWUVW (1-6)
                  if ((il_ect.eq.il).and.(id_ect.eq.id).and.(repl.eq.0)) then
                     if(float(J16(i_ect+1)).eq.0.) then
                        tVI(N_VI)=float(J16(i_ect+1))
                     else
                        tVI(N_VI)=5700-float(J16(i_ect+1))
                     endif
                     repl=1
                  endif
               enddo
            endif
         endif
         if(il.eq.3) then
            N_WI=N_WI+1
            idWI(N_WI)=id
            tWI(N_WI)=float(I16(i+1))
            aWI(N_WI)=float(I16(i+2))
            if ((sectorflag.eq.1).and.(tdcstatus.eq.1)) then
               do i_ect=1,nd2_ect,2
                  ichk_ect = j16(i_ect)
                  id_ect = mod (ichk_ect, 256) ! strip #
                  il_ect = ichk_ect/256 ! UVWUVW (1-6)
                  if ((il_ect.eq.il).and.(id_ect.eq.id).and.(repl.eq.0)) then
                     if(float(J16(i_ect+1)).eq.0.) then
                        tWI(N_WI)=float(J16(i_ect+1))
                     else
                        tWI(N_WI)=5700-float(J16(i_ect+1))
                     endif
                     repl=1
                  endif
               enddo
            endif
         endif
         if(il.eq.4) then
            N_UO=N_UO+1
            idUO(N_UO)=id
            tUO(N_UO)=float(I16(i+1))
            aUO(N_UO)=float(I16(i+2))
            if ((sectorflag.eq.1).and.(tdcstatus.eq.1)) then
               do i_ect=1,nd2_ect,2
                  ichk_ect = j16(i_ect)
                  id_ect = mod (ichk_ect, 256) ! strip #
                  il_ect = ichk_ect/256 ! UVWUVW (1-6)
                  if ((il_ect.eq.il).and.(id_ect.eq.id).and.(repl.eq.0)) then
                     if(float(J16(i_ect+1)).eq.0.) then
                        tUO(N_UO)=float(J16(i_ect+1))
                     else
                        tUO(N_UO)=5700-float(J16(i_ect+1))
                     endif
                     repl=1
                  endif
               enddo
            endif
         endif
         if(il.eq.5) then
            N_VO=N_VO+1
            idVO(N_VO)=id
            tVO(N_VO)=float(I16(i+1))
            aVO(N_VO)=float(I16(i+2))
            if ((sectorflag.eq.1).and.(tdcstatus.eq.1)) then
               do i_ect=1,nd2_ect,2
                  ichk_ect = j16(i_ect)
                  id_ect = mod (ichk_ect, 256) ! strip #
                  il_ect = ichk_ect/256 ! UVWUVW (1-6)
                  if ((il_ect.eq.il).and.(id_ect.eq.id).and.(repl.eq.0)) then
                     if(float(J16(i_ect+1)).eq.0.) then
                        tVO(N_VO)=float(J16(i_ect+1))
                     else
                        tVO(N_VO)=5700-float(J16(i_ect+1))
                     endif
                     repl=1
                  endif
               enddo
            endif
         endif
         if(il.eq.6) then
            N_WO=N_WO+1
            idWO(N_WO)=id
            tWO(N_WO)=float(I16(i+1))
            aWO(N_WO)=float(I16(i+2))
            if ((sectorflag.eq.1).and.(tdcstatus.eq.1)) then
               do i_ect=1,nd2_ect,2
                  ichk_ect = j16(i_ect)
                  id_ect = mod (ichk_ect, 256) ! strip #
                  il_ect = ichk_ect/256 ! UVWUVW (1-6)
                  if ((il_ect.eq.il).and.(id_ect.eq.id).and.(repl.eq.0)) then
                     if(float(J16(i_ect+1)).eq.0.) then
                        tWO(N_WO)=float(J16(i_ect+1))
                     else
                        tWO(N_WO)=5700-float(J16(i_ect+1))
                     endif
                     repl=1
                  endif
               enddo
            endif
         endif
c         print *, sector,il,id,float(i16(i+1)),float(i16(i+2))
 31   continue
c     
      if(.NOT.preshower)then
         call EcGus_int_sort(N_UI,idUI,iloc) !sort by increasing id#
         EcEvu_Nreported(Ec_u, inner,sector)=N_UI
         do mth=1,N_UI
            id=idUI(iloc(mth))
c         print *, 'U-inner ',id,tui(iloc(mth)),aUI(iloc(mth))
            EcEvu_reportedID(mth,Ec_u, inner,sector)=id
            EcEvu_strip( ADC,id, Ec_u, inner,sector)=aUI(iloc(mth))
            EcEvu_strip( TDC,id, Ec_u, inner,sector)=tUI(iloc(mth))
         enddo
*     
         call EcGus_int_sort(N_VI,idVI,iloc) !sort by increasing id#
         EcEvu_Nreported(Ec_v, inner,sector)=N_VI
         do mth=1,N_VI
            id=idVi(iloc(mth))
c        print *,'V-inner ', id,tvi(iloc(mth)),aVI(iloc(mth))
            EcEvu_reportedID(mth, Ec_v, inner,sector)=id
            EcEvu_strip( ADC,id, Ec_v, inner,sector)=aVI(iloc(mth))
            EcEvu_strip( TDC,id, Ec_v, inner,sector)=tVI(iloc(mth))
         enddo
c     *
         call EcGus_int_sort(N_WI,idWI,iloc) !sort by increasing id#
         EcEvu_Nreported( Ec_w, inner,sector)=N_WI
         do mth=1,N_WI
            id=idWI(iloc(mth))
c        print *, 'W-inner ',id,twi(iloc(mth)),aWI(iloc(mth))     
            EcEvu_reportedID(mth, Ec_w, inner,sector)=id
            EcEvu_strip( ADC,id, Ec_w, inner,sector)=aWI(iloc(mth))
            EcEvu_strip( TDC,id, Ec_w, inner,sector)=tWI(iloc(mth))
         enddo
c     *
         call EcGus_int_sort(N_UO,idUO,iloc) !sort by increasing id#
         EcEvu_Nreported( Ec_u, outer,sector)=N_UO
         do mth=1,N_UO
            id=idUO(iloc(mth))
c        print *,'U-outter ', id,tuo(iloc(mth)),aUO(iloc(mth))
            EcEvu_reportedID(mth, Ec_u, outer,sector)=id
            EcEvu_strip( ADC,id, Ec_u, outer,sector)=aUO(iloc(mth))
            EcEvu_strip( TDC,id, Ec_u, outer,sector)=tUO(iloc(mth))
         enddo
c     *
         call EcGus_int_sort(N_VO,idVO,iloc) !sort by increasing id#
         EcEvu_Nreported( Ec_v, outer,sector)=N_VO
         do mth=1,N_VO
            id=idVO(iloc(mth))
c        print *,'V-outter ', id,tvo(iloc(mth)),aVO(iloc(mth))
            EcEvu_reportedID(mth, Ec_v, outer,sector)=id
            EcEvu_strip( ADC,id, Ec_v, outer,sector)=aVO(iloc(mth))
            EcEvu_strip( TDC,id, Ec_v, outer,sector)=tVO(iloc(mth))
         enddo
c     *
         call EcGus_int_sort(N_WO,idWO,iloc) !sort by increasing id#
         EcEvu_Nreported( Ec_w, outer,sector)=N_WO
         do mth=1,N_WO
            id=idWO(iloc(mth))
c        print *,'W-outter ', id,two(iloc(mth)),aWO(iloc(mth))
            EcEvu_reportedID(mth, Ec_w, outer,sector)=id
            EcEvu_strip( ADC,id, Ec_w, outer,sector)=aWO(iloc(mth))
            EcEvu_strip( TDC,id, Ec_w, outer,sector)=tWO(iloc(mth))
         enddo
         ok(sector)=.true.
c     * go to the next sector
         go to 30
      else
c     Pre-shower ADCs and TDCs
c         print *,'Preshower ',N_UI,N_VI,N_WI
         call EcGus_int_sort(N_UI,idUI,iloc) !sort by increasing id#
         EcEvu_Nreported(Ec_u, cover,sector)=N_UI
         do mth=1,N_UI
            id=idUI(iloc(mth))
c      print *, 'U-cover ',id,tui(iloc(mth)),aUI(iloc(mth))
            EcEvu_reportedID(mth,Ec_u, cover,sector)=id
            EcEvu_strip( ADC,id, Ec_u, cover,sector)=aUI(iloc(mth))
            EcEvu_strip( TDC,id, Ec_u, cover,sector)=tUI(iloc(mth))
         enddo
*     
         call EcGus_int_sort(N_VI,idVI,iloc) !sort by increasing id#
         EcEvu_Nreported(Ec_v, cover,sector)=N_VI
         do mth=1,N_VI
            id=idVi(iloc(mth))
c     print *,'V-cover ', id,tvi(iloc(mth)),aVI(iloc(mth))
            EcEvu_reportedID(mth, Ec_v, cover,sector)=id
            EcEvu_strip( ADC,id, Ec_v, cover,sector)=aVI(iloc(mth))
            EcEvu_strip( TDC,id, Ec_v, cover,sector)=tVI(iloc(mth))
         enddo
c     *
         call EcGus_int_sort(N_WI,idWI,iloc) !sort by increasing id#
         EcEvu_Nreported( Ec_w, cover,sector)=N_WI
         do mth=1,N_WI
            id=idWI(iloc(mth))
c     print *, 'W-cover ',id,twi(iloc(mth)),aWI(iloc(mth))     
            EcEvu_reportedID(mth, Ec_w, cover,sector)=id
            EcEvu_strip( ADC,id, Ec_w, cover,sector)=aWI(iloc(mth))
            EcEvu_strip( TDC,id, Ec_w, cover,sector)=tWI(iloc(mth))
         enddo
         ok(sector)=.true.
c         if( EcEvu_Nreported(Ec_u, cover,sector).eq.1.and. 
c     +        EcEvu_Nreported(Ec_u, inner,sector).eq.1)then
c           call hf2(10001,float(EcEvu_reportedID(1,Ec_u, cover,sector)),
c     +           float(EcEvu_reportedID(1,Ec_u, inner,sector)),1.)
c        endif
c        if( EcEvu_Nreported(Ec_v, cover,sector).eq.1.and. 
c     +       EcEvu_Nreported(Ec_v, inner,sector).eq.1)then
c           call hf2(10002,float(EcEvu_reportedID(1,Ec_v, cover,sector)),
c     +          float(EcEvu_reportedID(1,Ec_v, inner,sector)),1.)
c        endif
c        if( EcEvu_Nreported(Ec_w, cover,sector).eq.1.and. 
c     +       EcEvu_Nreported(Ec_w, inner,sector).eq.1)then
c           call hf2(10003,float(EcEvu_reportedID(1,Ec_w, cover,sector)),
c     +          float(EcEvu_reportedID(1,Ec_w, inner,sector)),1.)
c        endif
c     * go to the next sector
         go to 30
c     *         
      endif
c
 99   continue
      return
      end
