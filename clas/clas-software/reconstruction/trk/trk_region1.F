      subroutine trk_region1
      implicit none
#include "bcs.inc"
#include "wcs.inc"

      integer isec,status

      call BDROP(iw,'RGLK')
      do isec=1,6
        call region1(isec,status)
      enddo

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine region1(isec,status)
      IMPLICIT NONE
*_begin_doc
*  RCS ID string
*  $Id: trk_region1.F,v 1.13 2005/11/09 20:40:56 stepanya Exp $
*
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
c-               Find tracks in region 1 and sector isec
c-               for clusters which have not been used by HBT and TBT yet.
c-               IOW, tracks found in region 2 or 3 won't be processed here
c-               (condition on bpatt(2)).
c-               It makes a fit (MINUIT, not that slow) with a straight
c-               line closest to wires hit (w/ valid TDC info).
c-   Assumptions:Tracks reconstructed are assumed to intersect the beam
c-               axis Z (i.e. X=Y=0, sets one degree of freedom)
c-               Beta is arbitrarily set to 0.7 (for drift distance calculation).
c-   BOS banks:  It uses     BOS banks DC0 (to get TDC infos),
c-                                     DHCL (clusters of wires),
c-                                     HEVT (start time),
c-                                     DCGW, DCGM (drift chamber geometry).
c-               and creates BOS bank  RGLK (link direction and position).
C-
C-   Inputs  : isec = sector
C-   Outputs : status (number of tracks found, zero = sth wrong or 0 track)
C-   Controls: dream on !! Well, if you insist : 
c-             MAX_DPOS,MAX_DSLOPE,MAX_KI2 to compare cluster pos n slope.
c-             MIN_WIRE_PER_SL, MINUP, MAXITER for MINUIT setup.
C-
C-   Library belongs: libtrk
C-
C-   Calls: DC_TIME_TO_DIST(isec,ilayer,iwire,beta,alpha,tdrift
C-                          ,B_abs,B_para,ddrift,sigddrift) ! see trk/trk_dtime.F
C-
C-   Created 12-JUI-1998 by Laurent Farhi
C-           FRANCE WINS THE SOCCER WORLD CUP'98 AGAINST BRAZIL !!!!!!!!
C-
*_end_doc
*
      INTEGER isec,status
*
*_begin_inc
*  include files :
*  ---------------------
*
#include "bcs.inc"
#include "wcs.inc"
#include "clas_offsets.inc"
#include "dc_cal_tdly.inc"
#include "dc_tcl.inc"
#include "trktcl_var.inc"
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_region1.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.13 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2005/11/09 20:40:56 $')
      PARAMETER ( CAUTHO = '$Author: stepanya $')
      DATA CRCSID/
     1'$Id: trk_region1.F,v 1.13 2005/11/09 20:40:56 stepanya Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='REGION1')
      PARAMETER (CRAUTH='Laurent Farhi')
*
*  Local User defined variables:
*  -----------------------------
*
C
C Local variables
C
      BYTE  bpatt(16)
      INTEGER ind_dhcl,ncol_dhcl,icol,nclusters,icluster
      INTEGER ind_hevt,ind_dc
      INTEGER isl,icl,ilayer,bit,iwire,itdc
      INTEGER ihit,i0,i1,i2,sigma1,sigma2,sum2,den
      INTEGER ipatt,iwir0

      INTEGER iregion,stat_array(20)
      REAL start_time
      COMMON/REG1STAT/stat_array

      real chi2,theta0,phi0,rtheta,rphi
      double precision dbx,dby,dbz,dbrx,dbry,dbrz
      integer REG1_STATUS_LINK

      integer icl1,icl2
      real pos1,pos2,slope1,slope2,ki21,ki22

      integer ilink,jlink,index,jndex,nw
      logical badlink
      real beta,tPRd,tFLd,tdrift,ddrift,sigddrift,alpha,b_abs,b_para

      integer ind_rglk,nlink

      integer mamind,mbank
      real pi
      double precision dpi

c  arrays for DC0
      INTEGER n_words
      INTEGER*4 III_32
      INTEGER*2 III_16(2)
      EQUIVALENCE (III_16(1),III_32)

c  arrays for wires
      INTEGER wire_tdc(36,192)  ! init to 0
      INTEGER wire_cl_id(36,192) ! init to 0
      INTEGER max_w_in_sl
      PARAMETER (MAX_W_IN_SL=192*6) ! max # of wires hit in one SL
      INTEGER wire_layer(6,MAX_W_IN_SL)
      INTEGER wire_number(6,MAX_W_IN_SL)
      INTEGER n_hit_in_sl(6)    ! init to 0
      COMMON/WIRES/wire_tdc,wire_cl_id,
     &     wire_layer,wire_number,n_hit_in_sl

c  Defines the regions to look at.
      INTEGER MIN_REGION,MAX_REGION

c  parameter for cluster pos and slope
      LOGICAL USE_DHCL
      REAL MAX_DPOS,MAX_DSLOPE,MAX_KI2
      parameter (max_dpos=20.)
      parameter (max_dslope=1.)
      parameter (max_ki2=3.)

c  arrays for clusters in SL
      INTEGER MAXCL,MIN_CL_SIZE,MAX_CL_SIZE
      PARAMETER (MAXCL=20)
      PARAMETER (MIN_CL_SIZE=2)
      PARAMETER (MAX_CL_SIZE=50)
      INTEGER nocl(6)           ! # of clusters in SL, init to 0
      INTEGER n_hit_in_cl(6,MAXCL) ! cluster size (# of wires)
      INTEGER w_in_cl(6,MAXCL,MAX_CL_SIZE,6)
      COMMON/CLUSTERS/nocl,n_hit_in_cl,w_in_cl
      REAL cluster_slope(6,MAXCL),cluster_pos(6,MAXCL),
     &     cluster_ki2(6,MAXCL)

c  arrays for links in region, local to the main loop on regions
      INTEGER MAX_NOLINK
      PARAMETER (MAX_NOLINK=30)
      INTEGER NOLINK            ! # of links in region, init to 0
      INTEGER LINK_IN_REGION(MAX_NOLINK,2)
      COMMON/LINKS/LINK_IN_REGION

c  parameter for MINUIT fit
      REAL MINUP
      parameter (MINUP=100.)
      INTEGER MIN_WIRE_PER_SL,MAXITER
      parameter (MIN_WIRE_PER_SL=2)
      parameter (MAXITER=5000)

c  variables for MINUIT fit
      integer sector_fit,region_fit,link_fit,nbpoints_fit,nbparam_fit
      COMMON/REG1FIT/sector_fit,region_fit,link_fit,nbpoints_fit,nbparam_fit

      INTEGER NPRM(3)
      DOUBLE PRECISION VSTRT(3),STP(3)
      CHARACTER*5 PNAM(3)
      DATA NPRM / 1     , 2     , 3    / ! parameter #
      DATA PNAM /'THETA','PHI'  ,'Z'   / ! parameter name
      DATA VSTRT/ 90.   , 0.    , 0.   / ! starting value
      DATA STP  / 5.0   , 2.0   , 1.   / ! step

      INTEGER IERFLG,IPARM
      DOUBLE PRECISION ZERO,ARGLIS(10)
      DOUBLE PRECISION FMIN,FEDM,ERRDERF
      INTEGER NPARI,NPARX,ISTAT
      EXTERNAL CHI2REG

c  variables for fit result
      INTEGER MAX_LINK_FOUND
      PARAMETER (MAX_LINK_FOUND=30)
      INTEGER N_LINK_FOUND(3)
      INTEGER FILTER_LINK_CL1(MAX_LINK_FOUND) ! local to the main loop on regions
      INTEGER FILTER_LINK_CL2(MAX_LINK_FOUND)
      REAL    FILTER_LINK_KI2(MAX_LINK_FOUND)

c  arrays for fit result (and bank RGLK)
      INTEGER FILTER_LIST(3,MAX_LINK_FOUND)
      REAL LINK_X(3,MAX_LINK_FOUND)
      REAL LINK_Y(3,MAX_LINK_FOUND)
      REAL LINK_Z(3,MAX_LINK_FOUND)
      REAL LINK_T0(3,MAX_LINK_FOUND)
      REAL LINK_P0(3,MAX_LINK_FOUND)
      REAL LINK_RT(3,MAX_LINK_FOUND)
      REAL LINK_RP(3,MAX_LINK_FOUND)
      REAL LINK_CHI2(3,MAX_LINK_FOUND)
      INTEGER LINK_STATUS(3,MAX_LINK_FOUND)

      LOGICAL Lgfrst
      SAVE    Lgfrst
      DATA    Lgfrst/.true./

      USE_DHCL=.FALSE.
      IF (IPAR_REG1_USE_DHCL.NE.0) USE_DHCL=.TRUE.
      min_region = ipar_reg1_min
      max_region = ipar_reg1_max
      if (min_region.lt.1.or.min_region.gt.3) min_region=1
      if (max_region.lt.1.or.max_region.gt.3) max_region=3

      If (Lgfrst) Then
        Lgfrst = .false.
        write(crmess,
     & '(''Region 1 clusters processing for region(s) '''//
     &       ',i1,'' to '',i1,''.'')') min_region,max_region
       Call RecMes(CRNAME,'I',crmess)
       if (.not.USE_DHCL) then
         Call RecMes(CRNAME,'I','Rebuild clusters from DC0')
       else
         Call RecMes(CRNAME,'I','Rebuild clusters from DHCL')
       endif
      Endif

      pi = acos(-1.)
      dpi = dacos(-1.d+00)

C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
C  First, DC0 informations are read from scratch.
C  1/ Read DC0, and get wires hit in wire_tdc and wire_cl_id
      CALL vzero(wire_tdc,  36*192) ! init. for each sector
      CALL vzero(wire_cl_id,36*192) ! init. for each sector
      do isl=1,6
        n_hit_in_sl(isl) = 0    ! number of wires hit in sector
      enddo

      ind_dc = MAMIND(iw,'DC0 ')+1
 1    Continue
      
      ind_dc = IW(ind_dc-1)
      if (ind_dc.eq.0) then
c        call recmes(crname,'F','Error in DC0 Bos Bank')
        return
      endif

      If( IW(ind_dc-2).ne.isec ) GO TO 1
      n_words = IW(ind_dc)
      If( n_words .EQ. 0 ) return
c      If( ind_dc .EQ. 0 ) return   ! End 'DC0 ' of this event

      Do jndex = 1, n_words
        III_32 = IW(ind_dc+jndex)
        ILAYER = III_16(1)/256      !  layer # 1-36
        IWIRE = III_16(1) - ILAYER*256 !  wire # for soft
        iTDC = III_16(2)        !  TDC
        iSL = (ilayer-1)/6 + 1

        if (ilayer.ge.1.and.ilayer.le.36.and.
     &       iwire.ge.1.and.iwire.le.192) then
          if(itdc.gt.0) then
            wire_cl_id(ILAYER,IWIRE) = -1
            wire_tdc(ILAYER,IWIRE) = iTDC
            if (n_hit_in_sl(isl).lt.MAX_W_IN_SL) then
              n_hit_in_sl(isl) = n_hit_in_sl(isl) + 1   ! number of wires hit
              wire_layer(isl,n_hit_in_sl(isl)) = ILAYER ! wires in sector, 1 to 36
              wire_number(isl,n_hit_in_sl(isl))= IWIRE ! 1 to 192
c              print *,'Sector ',isec,', l',ilayer,', w',iwire
            else
              call recmes(crname,'W'
     &           ,'**** NUMBER OF WIRES LIMITED BY ARRAY SIZE ****')
            endif
          else
            wire_cl_id(ILAYER,IWIRE) = 0
            wire_tdc(ILAYER,IWIRE) = 0
          endif
        endif
      enddo
      
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
C 2/ Make clusters, either from DHCL or directly from DC0
      do isl=1,6
        nocl(isl) = 0           ! # of clusters in superlayer isl
      enddo

      IF (USE_DHCL) THEN        ! use DHCL to make clusters
C  Read DHCL
C  Be aware DHCL definition interchanges rows and columns in ana_segm.F :
C       ind_dhcl = MBANK(iw,'DHCL',isec,isum,16)
C  where should be 
C       ind_dhcl = MBANK(iw,'DHCL',isec,16,isum)
      ind_dhcl = MAMIND(iw,'DHCL')
      ind_dhcl = IW(IND_DHCL)
      if (ind_dhcl.eq.0) then
c        call recmes(crname,'W','Could not find DHCL bank')
        return
      endif

      do while (IW(IND_DHCL-2).ne.isec.and.iw(ind_dhcl-1).gt.0)
        ind_dhcl = IW(IND_DHCL-1)
      enddo
      if (iw(ind_dhcl-2).ne.isec) then
c        call recmes(crname,'W','No DHCL bank found.')
        return
      endif
      ncol_dhcl=IW(ind_dhcl-5)
      nclusters=(4*IW(ind_dhcl))/ncol_dhcl
c      print *,'DHCL, isec=',isec,', nclusters=',nclusters,
c     &     ', ncol_dhcl=',ncol_dhcl

      do icluster=1,nclusters   ! # of clusters in DHCL
        do icol=1,ncol_dhcl
          bpatt(icol)=IW08(4*ind_dhcl+(icluster-1)*ncol_dhcl+icol)
        enddo
        isl=bpatt(1)            ! superlayer
c        print *,'-> SL=',isl,', track ',BPATT(2),':'
c     &       ,(bpatt(icol),icol=5,ncol_dhcl)

        if (isl.ge.(2*min_region-1).and.isl.le.(2*max_region)) then
          
        IF (BPATT(2).EQ.0) THEN ! SELECTS ONLY UNUSED CLUSTERS BY TRK
c        IF (BPATT(2).GE.0) THEN ! SELECTS ALL CLUSTERS
C ----------- ADD CLUSTER
          if (nocl(isl).lt.MAXCL) then
            nocl(isl) = nocl(isl) + 1
          else
            call recmes(crname,'W'
     &           ,'**** NUMBER OF CLUSTERS LIMITED BY ARRAY SIZE ****')
          endif

          ihit = 0
          do ilayer = 1,6       ! loop on layers of cluster
            iwir0 = bpatt(3+2*ilayer) ! first wire#
            if (iwir0.lt.0) iwir0=iwir0+256
            ipatt = bpatt(4+2*ilayer) ! bit pattern
            if (ipatt.lt.0) ipatt=ipatt+256
            bit   = 0
            do while (ipatt.ne.0) ! decode bit pattern
              if ((ipatt-(ipatt/2)*2).ne.0) then ! wire is hit
                iwire = iwir0 + bit
                if (iwire.ge.1.and.iwire.le.192) then
                  if (ihit.lt.MAX_CL_SIZE) then
                    ihit = ihit + 1
                  else
                    call recmes(crname,'W'
     &                   ,'**** NUMBER OF HITS LIMITED BY ARRAY SIZE ****')
                  endif
                  
                  w_in_cl(isl,nocl(isl),ihit,1) = ilayer + 6*(isl-1)
                  w_in_cl(isl,nocl(isl),ihit,2) = iwire
                  w_in_cl(isl,nocl(isl),ihit,3) = 
     &                 wire_tdc(6*(isl-1)+ilayer,iwire)
                endif
              endif
              bit = bit + 1
              ipatt = ipatt/2
            enddo               ! while (ipatt.ne.0)
          enddo                 ! ilayer
          
          N_HIT_IN_CL(isl,nocl(isl)) = ihit

        endif                   ! (bpatt(2).eq.0)
        endif                   ! 1 <= isl <= 6
      enddo                     ! icluster

      ELSE                      ! DOES NOT USE DHCL
C Use home made, more general cluster definition, from DC0

        do isl=(2*min_region-1),(2*max_region)
          call make_cluster(isl)
        enddo

      ENDIF                     ! USE_DHCL

c      do isl=1,6
c        do icl=1,nocl(isl)
c          print *,'cluster in sector ',isec,', SL ',isl
c          do ihit = 1,N_HIT_IN_CL(isl,icl)
c            print *,'l',w_in_cl(isl,icl,ihit,1),
c     &           ', w ',w_in_cl(isl,icl,ihit,2),
c     &           ', tdc ',w_in_cl(isl,icl,ihit,3)
c          enddo
c        enddo
c      enddo

C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
C 3/ Get (kinda) slope and position of clusters for each SL.
c Position is defined in wire numbering unit, average wire number in layer.
c Slope is defined in wire/layer, ie difference of average wire number
C between 2 consecutive layers.
C Line in coordinates : X = wire numbers, Y = layer number
C           y = slope*(layer-1) + pos(=average_wire_number_in_layer_one)

c Loop on clusters found.
      do isl=1,6
        do icl=1,nocl(isl)
          i0 = 0
          i1 = 0
          i2 = 0
          sigma1 = 0
          sigma2 = 0
          sum2 = 0
          do ihit=1,N_HIT_IN_CL(isl,icl)
            ilayer = w_in_cl(isl,icl,ihit,1)-6*(isl-1) ! between 1 and 6
            iwire  = w_in_cl(isl,icl,ihit,2)
            i0 = i0 + 1
            i1 = i1 + ilayer
            i2 = i2 + ilayer*ilayer
            
            sigma1 = sigma1 + iwire
            sigma2 = sigma2 + ilayer*iwire
            sum2   = sum2 + iwire*iwire
          enddo
          den = i0*i2 - i1*i1
          if (den.ne.0.and.i0.ge.2) then
            cluster_slope(isl,icl) =1.*(i0*sigma2 - i1*sigma1)/den
            cluster_pos(isl,icl)   =1.*(i2*sigma1 - i1*sigma2)/den
            cluster_pos(isl,icl)   = cluster_pos(isl,icl)
     &           + cluster_slope(isl,icl) ! pos on layer 1 of SL
            cluster_ki2(isl,icl)   = 1.*sum2
     &           - 2.*cluster_slope(isl,icl)*sigma2
     &           - 2.*cluster_pos(isl,icl)*sigma1
     &           + cluster_slope(isl,icl)*cluster_slope(isl,icl)*i2
     &           + 2.*cluster_slope(isl,icl)*cluster_pos(isl,icl)*i1
     &           + cluster_pos(isl,icl)*cluster_pos(isl,icl)*i0
            if (i0.gt.2) cluster_ki2(isl,icl) = cluster_ki2(isl,icl)/(i0-2)
            if (i0.eq.2) cluster_ki2(isl,icl) = 0
c            print *,'Sector ',isec,', SL ',isl,', cluster '
c     &           ,icl,' : pos = ',cluster_pos(isl,icl)
c     &           ,', slope = ',cluster_slope(isl,icl),', ki2 = '
c     &           ,cluster_ki2(isl,icl)
            
          else
            cluster_slope(isl,icl)=-1000.
            cluster_pos(isl,icl)  =-1000. ! will make no link
            cluster_ki2(isl,icl)  = 1000.
            N_HIT_IN_CL(isl,icl) = 0 ! cluster rejected
c            print *,'     FAILED'
          endif                 ! (den.ne.0.and.i0.gt.2)
        enddo                   ! icl
      enddo                     ! isl
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
C 4/ GET t0, time at the target form bank HEVT
      ind_hevt = MAMIND(iw,'TRKS')
      ind_hevt = IW(ind_hevt)
      if (ind_hevt.eq.0) then
        return
      endif
      start_time = RW(ind_hevt+3)
CCCCCCCCCCCC

C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
C LOOP ON REGION - LOOP ON REGION - LOOP ON REGION - LOOP ON REGION
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
      do iregion=min_region,max_region ! loop on region
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
        nlink = 0               ! # of links w/ successful fit in region
        n_link_found(iregion) = 0        ! # of links w/ successful fit in region
        NOLINK=0                ! initialization, # of links of 2 clusters

C --------LINK REGION
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
C 5/ Find links inside superlayers (links axial and stereo)
c Compares positions and slopes in both axial and stereo.
        do icl1=1,nocl(2*iregion-1)
          pos1=cluster_pos(2*iregion-1,icl1)
          slope1=cluster_slope(2*iregion-1,icl1)
          ki21=cluster_ki2(2*iregion-1,icl1)
          pos1=pos1 + 6*slope1  ! position on the external plane of SL

          do icl2=1,nocl(2*iregion)
c            PRINT *,'TRY LINK '//
c     &           ' = cluster ',icl1,' in SL ',2*iregion-1,
c     &           ' + cluster ',icl2,' in SL ',2*iregion
            pos2=cluster_pos(2*iregion  ,icl2)
            slope2=cluster_slope(2*iregion  ,icl2)
            ki22=cluster_ki2(2*iregion  ,icl2)

            if (abs(pos2-pos1).le.MAX_DPOS.and.
     &           abs(slope2-slope1).le.MAX_DSLOPE.and.
     &           ki21.le.MAX_KI2.and.ki22.le.MAX_KI2.and.
     &           N_HIT_IN_CL(2*iregion-1,icl1).ne.0.and.
     &           N_HIT_IN_CL(2*iregion  ,icl2).ne.0
     &           ) then             
c found a link between cluster icl1 in SL 2*iregion-1 and
c                      cluster icl2 in SL 2*iregion
              if (NOLINK.lt.MAX_NOLINK) then
                NOLINK = NOLINK + 1
              else
c                call recmes(crname,'W'
c     &               ,'**** NUMBER OF LINKS LIMITED BY ARRAY SIZE ****')
              endif
              LINK_IN_REGION(nolink,1)=icl1
              LINK_IN_REGION(nolink,2)=icl2
c              print *,'OK'
            else
c              print *,'REJECTED (delta pos or dir or ki2)'  
            endif               ! on diff. pos, slope, and on ki2
          enddo                 ! on icl2
        enddo                   ! on icl1

C ---------------------------------------------------------------
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
C BEGIN LOOP ON LINKS IN REGION
C THE BIG MAIN LOOP - THE BIG MAIN LOOP - THE BIG MAIN LOOP - THE 
        do ilink=1,NOLINK ! THE BIG MAIN LOOP
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
c          print *,' LINK in sector ',isec,', region ',iregion
c          icl1 = LINK_IN_REGION(ilink,1)
c          icl2 = LINK_IN_REGION(ilink,2)
c          print *,' sl ',2*iregion-1,
c     &         ', pos ',cluster_pos(2*iregion-1,icl1),
c     &         ', slope ',cluster_slope(2*iregion-1,icl1),
c     &         ', ki2 ',cluster_ki2(2*iregion-1,icl1)
c          print *,' sl ',2*iregion,
c     &         ', pos ',cluster_pos(2*iregion  ,icl2),
c     &         ', slope ',cluster_slope(2*iregion  ,icl2),
c     &         ', ki2 ',cluster_ki2(2*iregion  ,icl2)
          
C ---------------------------------------------------------------

          badlink = .false.
          do jndex=1,2          ! SL in region
            icl = LINK_IN_REGION(ilink,jndex)
            isl = 2*(iregion-1) + jndex
            nw = 0
c            print *,'>>>>> Sector ',isec,', link ',ilink,
c     &           ', SL ',isl,', cluster ',icl,' :'
c 6/ GET DRIFT DISTANCE (call DC_TIME_TO_DIST)
            do index=1,N_HIT_IN_CL(isl,icl)

              ilayer = W_in_CL(isl,icl,index,1)
              iwire  = W_in_CL(isl,icl,index,2)
              itdc   = W_in_CL(isl,icl,index,3)
              
c              ilayer = 6*(isl-1) + ilayer

              tPRd = 0.         ! time propagation along wire
              tFLd = 0.         ! time of flight between start and wire signal

              tdrift= t0(iwire,ilayer,isec) -
     &             (start_time + tPRd + tFLd) - itdc/DC_Tslope ! in ns
              tdrift= tdrift - dc_reg_pulser_time(iregion)
C Does t0 array depend on the trigger used ?
              W_in_CL(isl,icl,index,4) =int(1000.*tdrift) ! in ps
              
              alpha = atan(cluster_slope(isl,icl)) ! local 'angle', rad
              
              if (tdrift.GT.-100.and.tdrift.lt.3000
     &             .and.itdc.gt.0) then
c-----------------------------------------------------
c The field is supposd to be 0
c (track bending "can be neglected" within a region).
c Could be modified for reconstruction in region 2.
                b_abs = 0.0
                b_para = 0.0

c use a standard arbitrary beta (since unknown)
c Could be modified in case the TOF that matches with this track is known.
                beta  = 0.7
                ddrift = 0.
c-----------------------------------------------------
                call DC_TIME_TO_DIST(isec,ilayer,iwire,beta,alpha,tdrift
     &               ,B_abs,B_para,ddrift,sigddrift) ! see also trk/trk_dtime.F           
c-----------------------------------------------------
c                ddrift=DC_TIME_TO_DISTANCE
c     &               (ilayer,iwire,alpha,abs(tdrift))
c  The 2 next array elements depend on the track (start_time)
                nw = nw + 1     ! # of good wires
                W_in_CL(isl,icl,index,5) =int(10000.*ddrift) ! in um
                W_in_CL(isl,icl,index,6) =int(10000.*sigddrift)
c                print *,'   ilayer ,',ilayer,', wire ',iwire,
c     &               ', TDC=',itdc,', ddrift=',ddrift,'+/-',sigddrift
              else
                W_in_CL(isl,icl,index,5) = 0
                W_in_CL(isl,icl,index,6) = -1 ! <0, discard wire
c                print *,'   ilayer ,',ilayer,', wire ',iwire,
c     &               ', TDC=',itdc,'    Discarded.'
              endif
c             W_in_CL(isl,icl,index,5) in micrometer
            enddo               ! index ; hits in cluster of SL
            if (nw.lt.MIN_WIRE_PER_SL) badlink=.true. ! less than 2 wires hit in SL, link discarded
          enddo                 ! jndex=1,2 ; SL in region
C ---------------------------------------------------------------
          if (badlink) then
            stat_array(10) = stat_array(10) + 1
            goto 999            ! link discarded
          endif
C ---- FIT LINK

C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
C 7/ Make a fit with a straight line to minimize distance to wires hit.
          region_fit = iregion
          link_fit   = ilink
          sector_fit = isec

          ARGLIS(1)=-1.D+00
c          ARGLIS(1)=2.D+00
          CALL MNEXCM(CHI2REG,'SET PRI',ARGLIS,1,IERFLG,0)
          CALL MNEXCM(CHI2REG,'SET NOW',ARGLIS,0,IERFLG,0)

          CALL MNSETI('Fitting straight track into region')

          NPRM(1)=1
          NPRM(2)=2
          NPRM(3)=3
c Direction of the link. Track is crossing the beam axis
          PNAM(1)='THETA'
          PNAM(2)='PHI'
c Intersection position of track with beam axis
          PNAM(3)='Z'
c direction of the link along Z axis
          VSTRT(1) = 90.D+00
          VSTRT(2) = 0.D+00
          VSTRT(3) = 0.D+00
          STP(1) =  5.D+00
          STP(2) =  2.D+00
          STP(3) =  5.D+00
c    3 free parameters

          DO IPARM=1,3
            IERFLG=0
            ZERO = 0.D+00
c            print *,NPRM(IPARM),PNAM(IPARM),VSTRT(IPARM),STP(IPARM),ZERO
c     &           ,ZERO,IERFLG 
            CALL MNPARM(NPRM(IPARM),PNAM(IPARM),VSTRT(IPARM),STP(IPARM
     &           ),ZERO,ZERO,IERFLG)
c            IF (IERFLG.NE.0) THEN
c              print *,'Unable to define parameter ',IPARM
c          STOP
c            ENDIF
          ENDDO

          ARGLIS(1)=MINUP       ! UP.
          CALL MNEXCM(CHI2REG,'SET ERR',ARGLIS,1,IERFLG,0)

c      ARGLIS(1)=1.D-7
c      CALL MNEXCM(CHI2REG,'SET EPS',ARGLIS,1,IERFLG)

c      ARGLIS(1)=1.D+00
c      ARGLIS(2)=4.D+00
c      CALL MNEXCM(CHI2REG,'FIX',ARGLIS,2,IERFLG,0)

          ARGLIS(1)=1.D+00
          CALL MNEXCM(CHI2REG,'CALL FCN',ARGLIS,1,IERFLG,0)

          CALL MNSTAT(FMIN,FEDM,ERRDERF,NPARI,NPARX,ISTAT)
          nbparam_fit = NPARI

          if (nbpoints_fit.lt.nbparam_fit) then
            REG1_STATUS_LINK=0
c            print *,'Too few points ',nbpoints_fit,'<',nbparam_fit
            stat_array(10) = stat_array(10) + 1
            goto 999
          endif

          ARGLIS(1)=1.d+00*MAXITER   ! max number of iterations
          ARGLIS(2)=0.1D+00
          CALL MNEXCM(CHI2REG,'MIGRAD',ARGLIS,2,IERFLG,0)
          CALL MNSTAT(FMIN,FEDM,ERRDERF,NPARI,NPARX,ISTAT)

          ARGLIS(1)=3.D+00
          CALL MNEXCM(CHI2REG,'CALL FCN',ARGLIS,1,IERFLG,0)

          DO IPARM=1,3
            ZERO = 0.D+00
            CALL MNPOUT(NPRM(IPARM),PNAM(IPARM),VSTRT(IPARM),STP(IPARM
     &           ),ZERO,ZERO,IERFLG)
c            IF (IERFLG.LT.0) THEN
c              print *,'Unable to read parameter ',IPARM
c          STOP
c            ENDIF
          ENDDO

          CALL MNEXCM(CHI2REG,'RETURN',ARGLIS,0,IERFLG,0)

c          print *,'   >>>>> Link ',
c     &         ' in sector ',isec,', region ', iregion
          stat_array(10+istat) = stat_array(10+istat) + 1
c          print *,stat_array(10),stat_array(11),
c     &         stat_array(12),stat_array(13)
          REG1_STATUS_LINK=istat
          if (istat.lt.3) then
c            print *,'      MINUIT fit FAILED with status ',istat
c            write(crmess,'(''MINUIT fit FAILED /w status '',i1,'//
c     &           ''' ('',f5.1,'' %)'')')
c     &           istat,
c     &           100.*(stat_array(11)+stat_array(12))/
c     &           (stat_array(11)+stat_array(12)+stat_array(13))
c            call recmes(CRNAME,'W',CRMESS)
            GOTO 999              ! if fitting has failed
          else
c            print *,'      Fit successful, ki2=',FMIN,', error=',FEDM
c            print *,'theta, phi, z',1.*VSTRT(1),1.*VSTRT(2),1.*VSTRT(3)
          endif

          chi2 = 1.*FMIN

C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
C 8/ Gets direction and position of links.
c in SCS
          theta0 = 1.*VSTRT(1)
          do while (theta0.gt.0.)
            theta0 = theta0 - 360.
          enddo
          do while (theta0.lt.0.)
            theta0 = theta0 + 360.
          enddo

          phi0 = 1.*VSTRT(2)
          do while (phi0.gt.0.)
            phi0 = phi0 - 360.
          enddo
          do while (phi0.lt.0.)
            phi0 = phi0 + 360.
          enddo

C   IN SECTOR COORDINATES SYSTEM
          dbx = 0.d+00
          dby = 0.d+00
          dbz = VSTRT(3)        ! cm
          dbrx = dsin(dpi*VSTRT(1)/180.d+00)*dcos(dpi*VSTRT(2)/180.d+00)
          dbry = dsin(dpi*VSTRT(1)/180.d+00)*dsin(dpi*VSTRT(2)/180.d+00)
          dbrz = dcos(dpi*VSTRT(1)/180.d+00)
c-
          isl = 2*iregion       ! 2nd SL of region
          ilayer = 6*isl - 5    ! first plane of SL
          iwire = int(cluster_pos(isl,link_in_region(ilink,2)))
          if (iwire.lt.1) iwire = 1
          if (iwire.gt.192) iwire = 192
c   Gets intersection of line w/ region 1.
          call reginter(isec,ilayer,iwire,
     &         dbx,dby,dbz,dbrx,dbry,dbrz,istat)

          if (istat.eq.0) then
            REG1_STATUS_LINK=0
c            print *,'      Intersection w/ region 1 failed'
            goto 999
          endif

          if ((dbx*dbrx+dby*dbry+dbz*dbrz).lt.0.d+00) then ! oriented inward
            dbrx=-dbrx
            dbry=-dbry
            dbrz=-dbrz            ! change orientation to outward
          endif

          theta0 = 1.*datan(dsqrt(dbx**2+dby**2)/dbz)*180./pi
          if (theta0.lt.0.) theta0 = 180. + theta0 ! bz<0
          phi0   = 1.*datan(dby/dbx)*180./pi
          if (dbx.lt.0.d+00) phi0 = phi0 + 180.
          if (phi0.lt.0.) phi0 = phi0 + 360.
          
          rtheta = 1.*datan(dsqrt(dbrx**2+dbry**2)/dbrz)*180./pi
          if (rtheta.lt.0.) rtheta = 180. + rtheta ! brz<0
          rphi   = 1.*datan(dbry/dbrx)*180./pi
          if (dbrx.lt.0.d+00) rphi = rphi + 180.
          if (rphi.lt.0.) rphi = rphi + 360.

c          print *,'Intersection ',1.*dbx,1.*dby,1.*dbz,' cm (SCS)'
c          print *,'       Theta = ',theta0,', Phi = ',phi0,' deg (SCS)'
c          print *,'Direction    ',1.*dbrx,1.*dbry,1.*dbrz,' (SCS)'
c          print *,'       Theta = ',rtheta,', Phi = ',rphi,' deg (SCS)'
          
c ******* Go to CLAS coordinate system **************
          call sector2clas(isec,dbx,dby,dbz)
          call sector2clas(isec,dbrx,dbry,dbrz)
          phi0 = phi0 + (isec-1)*60.
          rphi = rphi + (isec-1)*60.
c ***************************************************
          if (phi0.gt.360.) phi0 = phi0 - 360.
          if (rphi.gt.360.) rphi = rphi - 360.
c          print *,'Intersection ',1.*dbx,1.*dby,1.*dbz,' cm (HCS)'
c          print *,'       Theta = ',theta0,', Phi = ',phi0,' deg (HCS)'
c          print *,'Direction    ',1.*dbrx,1.*dbry,1.*dbrz,' (HCS)'
c          print *,'       Theta = ',rtheta,', Phi = ',rphi,' deg (HCS)'
          
C ---------------------------------------------------------------          
c          print *,isec,iregion,theta0,phi0,rtheta,rphi
          
          if (nlink.lt.MAX_LINK_FOUND) then
            nlink = nlink + 1
          else
            call recmes(crname,'W'
     &           ,'**** NUMBER OF TRACKS LIMITED BY ARRAY SIZE ****')
          endif

          FILTER_LIST(iregion,nlink) = nlink
c ilink = loop index
          FILTER_LINK_CL1(nlink) = LINK_IN_REGION(ilink,1)
          FILTER_LINK_CL2(nlink) = LINK_IN_REGION(ilink,2)
          FILTER_LINK_KI2(nlink) = 1.*chi2

c position of link in region :
          LINK_X(iregion,nlink)=1.*dbx 
          LINK_Y(iregion,nlink)=1.*dby
          LINK_Z(iregion,nlink)=1.*dbz
c position in polar angle
          LINK_T0(iregion,nlink)=1.*theta0
          LINK_P0(iregion,nlink)=1.*phi0
c direction in polar coordinates
          LINK_RT(iregion,nlink)=1.*rtheta 
          LINK_RP(iregion,nlink)=1.*rphi
c chi2 fit
          LINK_CHI2(iregion,nlink)=1.*chi2
c status (integer)
          LINK_STATUS(iregion,nlink)=REG1_STATUS_LINK

C ----------------------------------------------------------
 999      continue
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
        enddo                   ! ilink, MAIN LOOP on links in region
C END OF
C THE BIG MAIN LOOP - THE BIG MAIN LOOP - THE BIG MAIN LOOP - THE
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^

        n_link_found(iregion) = nlink
        do ilink = 1,nlink
          if (FILTER_LIST(iregion,ilink).ne.0) then
            do jlink = ilink+1,nlink
              if ( FILTER_LINK_CL1(ilink).eq.
     &             FILTER_LINK_CL1(jlink)    .or.
     &             FILTER_LINK_CL2(ilink).eq.
     &             FILTER_LINK_CL2(jlink)        ) then
c  Discard the link with highest ki2
                if ( FILTER_LINK_KI2(ilink).lt.
     &               FILTER_LINK_KI2(jlink)    ) then
                  FILTER_LIST(iregion,jlink) = 0
                else
                  FILTER_LIST(iregion,ilink) = 0
                endif
              endif
            enddo
          endif
        enddo
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
      enddo                     ! iregion
C END OF
C LOOP ON REGION - LOOP ON REGION - LOOP ON REGION - LOOP ON REGION
C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^

C v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
c 9/ Bank RGLK is filled.
      nlink = 0
      do iregion=min_region,max_region
        nlink = nlink + n_link_found(iregion)
      enddo
      if (nlink.gt.0) then
        ind_rglk = MBANK(iw,'RGLK',isec,10,nlink)
c        print *,'Fill RGLK, sector ',isec
      endif

      do iregion=min_region,max_region
        do ilink=1,n_link_found(iregion)
            IW(ind_rglk+1)=iregion
            RW(ind_rglk+2)=1.*LINK_X(iregion,ilink)
            RW(ind_rglk+3)=1.*LINK_Y(iregion,ilink)
            RW(ind_rglk+4)=1.*LINK_Z(iregion,ilink)
            RW(ind_rglk+5)=1.*LINK_T0(iregion,ilink)
            RW(ind_rglk+6)=1.*LINK_P0(iregion,ilink)
            RW(ind_rglk+7)=1.*LINK_RT(iregion,ilink)
            RW(ind_rglk+8)=1.*LINK_RP(iregion,ilink)
            RW(ind_rglk+9)=1.*LINK_CHI2(iregion,ilink)
            if (FILTER_LIST(iregion,ilink).ne.0) then
              IW(ind_rglk+10)=LINK_STATUS(iregion,ilink)
            else
              IW(ind_rglk+10)=0
            endif
c        print *,IW(ind_rglk+1)
c     &       ,RW(ind_rglk+2),RW(ind_rglk+3),RW(ind_rglk+4)
c     &       ,RW(ind_rglk+5),RW(ind_rglk+6)
c     &       ,RW(ind_rglk+7),RW(ind_rglk+8)
c     &       ,RW(ind_rglk+9),IW(ind_rglk+10)
            ind_rglk = ind_rglk + 10
        enddo
      enddo

      status = nlink            ! # of links in sector
      
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C    END MAIN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE CHI2REG(NPAR,GIN,CHI2,X,IFLAG)
C  "FCN" function for MINUIT.
C  MINUIT will minimize CHI2.
C  4 PARAMETERS X(4)
      IMPLICIT NONE
#include "bcs.inc"
#include "wcs.inc"
#include "clas_offsets.inc"

      INTEGER NPAR,IFLAG        ! # of params, internal MINUIT variable
      DOUBLE PRECISION GIN(*),X(*) ! first derivative, parameters to fit with.
      DOUBLE PRECISION CHI2     ! output

C Fit by a straight line the track corresponding to a link in a
C region.
C Minimizes the square distance from the track to wires fired.
C Track parameters : a,b,c,ra,rb,rc, in CLAS coordinate system.
C a,b,c in cm

      INTEGER nami_dcgw,ind_dcgw,ncol_dcgw,ipt_gw

c  arrays for clusters in SL
      INTEGER MAXCL,MAX_CL_SIZE
      PARAMETER (MAXCL=20)
      PARAMETER (MAX_CL_SIZE=50)
      INTEGER nocl(6)
      INTEGER n_hit_in_cl(6,MAXCL)
      INTEGER w_in_cl(6,MAXCL,MAX_CL_SIZE,6)
      COMMON/CLUSTERS/nocl,n_hit_in_cl,w_in_cl

c  arrays for links in region, local to the main loop on regions
      INTEGER MAX_NOLINK
      PARAMETER (MAX_NOLINK=30)
      INTEGER LINK_IN_REGION(MAX_NOLINK,2)
      COMMON/LINKS/LINK_IN_REGION

      integer sector_fit,region_fit,link_fit,nbpoints_fit,nbparam_fit
      COMMON/REG1FIT/sector_fit,region_fit,link_fit,nbpoints_fit,nbparam_fit

      integer cl(2)
      double precision dist_to_wire
      double precision distance2lines
      double precision dist_measured,cellsize
      double precision bx,by,bz,brx,bry,brz

      integer index,icl,ihit,isl,npoint
      integer iisec,iiregion,iilink,iilayer,iiwire
      real x0,y0,z0,rx,ry,rz
      double precision dx0,dy0,dz0,drx,dry,drz

      double precision zero,one,dpi
      data zero,one/0.d+00,1.d+00/

      integer mamind

      dpi = dacos(-1.d+00)

c      print *,'Appel CHI2REG'

      iilink = link_fit
      iiregion = region_fit
      iisec = sector_fit

C In SECTOR coordinates.
C Compute square distance to wires fired.

      nami_dcgw = mamind(jw,'DCGW')
      ind_dcgw = jw(nami_dcgw)
      if (ind_dcgw.le.0) then
c         CALL RECMES(CRNAME,'E','DCGW Bank does not exist!')
         stop
      else
         ncol_dcgw = JW(ind_dcgw-5) ! number of columns of dcgw bank
         do while (jw(ind_dcgw-2).lt.iisec.
     &        AND.jw(ind_dcgw-1).gt.0)
           ind_dcgw = jw(ind_dcgw-1)
         enddo
c         if (jw(ind_dcgw-2).ne.iisec) then
c           print *,iisec
c         endif
       endif

      npoint = 0
      chi2 = 0.d+00

c      if (dabs(x(2)).gt.35.d+00) then
c        chi2 = chi2 + dexp(dabs(x(2))-30.d+00)
c        npoint = npoint + 1
c        return
c      endif

c bx,by,bz    : one point of the track
c brx,bry,brz : direction of the track
c coordinates in CLAS Coordinate System
c (Y,Z) = horizontal plane
      bx  = 0.d+00
      by  = 0.d+00
      bz  = x(3)                ! intersection point along Z, cm
      brx = dsin(dpi*x(1)/180.d+00)*dcos(dpi*x(2)/180.d+00)
      bry = dsin(dpi*x(1)/180.d+00)*dsin(dpi*x(2)/180.d+00)
      brz = dcos(dpi*x(1)/180.d+00)

c      if (brx.lt.0.d+00) then
c        brx = -brx
c        bry = -bry
c        brz = -brz
c      endif

      iilayer = 12*iiregion - 5
      cl(1) = LINK_IN_REGION(iilink,1) ! cluster in first SL
      cl(2) = LINK_IN_REGION(iilink,2) ! cluster in   2nd SL

C  The track goes close to the beam axis
c      npoint = npoint + 1
c      chi2 = chi2 + distance2lines(0.d+00,0.d+00,0.d+00,0.d+00,0.d+00,1.d+00,
c     &     bx,by,bz,brx,bry,brz)/5.d+00 ! 5 cm
c      print *,bx,by,bz,brx,bry,brz,chi2

      do index=1,2
        icl=cl(index)
        isl=2*(iiregion-1)+index ! SL
        do ihit=1,N_HIT_IN_CL(isl,icl)
          iilayer = W_in_CL(isl,icl,ihit,1)
          iiwire  = W_in_CL(isl,icl,ihit,2)
          if (W_in_CL(isl,icl,ihit,6).ge.0) then

c Get wire equation parameters
c                          ----------------------
c Position of wires are IN | SECTOR COORDINATES | in DCGW
c                          ----------------------
            ipt_gw = ind_dcgw + ((iilayer-1)*192+iiwire-1)*ncol_dcgw
            x0 = RJW(ipt_gw+DCGW_X_MID)
            y0 = RJW(ipt_gw+DCGW_Y_MID)
            z0 = RJW(ipt_gw+DCGW_Z_MID)
            rx = RJW(ipt_gw+DCGW_X_DIR)
            ry = RJW(ipt_gw+DCGW_Y_DIR)
            rz = RJW(ipt_gw+DCGW_Z_DIR)
c          print *,iilayer,iiwire,x0,y0,z0,rx,ry,rz
            
c          call sector_to_clas(iisec,x0,y0,z0)
c          call sector_to_clas(iisec,rx,ry,rz)
            
            dx0 = dble(x0)
            dy0 = dble(y0)
            dz0 = dble(z0)
            drx = dble(rx)
            dry = dble(ry)
            drz = dble(rz)
            
            dist_to_wire = distance2lines(dx0,dy0,dz0,drx,dry,drz,
     &           bx,by,bz,brx,bry,brz)
c                      = distance from track to wire
            
            dist_measured   = 1.d-4*W_in_CL(isl,icl,ihit,5)
c            dist_measured   = 0.d+00
c                         = drift-distance in cm
            cellsize = 1.d-4*W_in_CL(isl,icl,ihit,6) ! sigma, in cm
            
            npoint = npoint + 1
            chi2 = chi2 +  ((dist_to_wire-dist_measured)/cellsize)**2
            
c          print *,dist_to_wire,chi2
          endif

        enddo                   ! ihit=1,N_HIT_IN_CL(isl,icl)
      enddo                     ! index=1,2

      nbpoints_fit = npoint
      if (nbpoints_fit.gt.nbparam_fit) then
        chi2 = chi2/(nbpoints_fit-nbparam_fit) ! # of independant parameters
      endif

C Aim : minimize chi2 using MIMUIT,
C         free parameters : a,b,c,ra,rb,rc IN SCS REFERENTIAL
C         2 constraints   : (a,b,c) in a surface (e.g. first layer of region)
C                           (ra,rb,rc) cosinus director
C         => 3 independant parameters : by,bz,ry,rz
C             (0,0,bz) intersection of the track with beam axis Z
C             (1,bry,brz) direction of the track
C                           IN SCS REFERENTIAL
C            The track in almost never parallel to (Y,Z)

c      chi2 = dble(bx*bx + by*by + bz*bz + brx*brx + bry*bry + brz*brz)

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine reginter(isec,ipl,iwire,a,b,c,ra,rb,rc,status)
      implicit none
#include "bcs.inc"
#include "wcs.inc"
#include "clas_offsets.inc"

C EVERYTHING CONSIDERED IN SECTOR COORDINATE SYSTEM.
C Local variables :
c line definition : point M, vector u
      double precision M(3)     ! a point of the line
      double precision u(3)     ! vector director of the line
c plane definition : point O, distance d, vector n
c plane tangent to sphere of center o and radius d.
c n towards exterior.
      double precision Op(3)    ! point in space
      double precision dp       ! distance from O to plane
      double precision np(3)    ! normal vector to plane

c cylinder definition : point O, vector n, radius d
      double precision Oc(3)    ! point in space
      double precision dc       ! radius of cylinder
      double precision nc(3)    ! vector colinear to the axis of the cylinder

c Output parameters :
c intersection point I
      double precision Ic(2,3),Ip(3)
      logical failc,failp,in_plane_section
      integer  status
c status = 0 : no intersection
c status = 1 : no intersection inside sector
c status = 2 : intersection inside sector
      integer isec,ipl,iwire,max_cylinder
      double precision xwire,zwire
      double precision a,b,c,ra,rb,rc,pi,d1,d2

      integer mamind
      integer nami_dcgm,ind_dcgm,ncol_dcgm,ipt_gm
      integer nami_dcgw,ind_dcgw,ncol_dcgw,ipt_gw

C EVERYTHING CONSIDERED IN SECTOR COORDINATE SYSTEM.

      if (nami_dcgm.le.0) nami_dcgm = mamind(jw,'DCGM')
      ind_dcgm = jw(nami_dcgm)
      do while (jw(ind_dcgm-2).lt.isec.
     &     AND.jw(ind_dcgm-1).gt.0)
        ind_dcgm = jw(ind_dcgm-1)
      enddo
      if (jw(ind_dcgm-2).ne.isec) then
c        call recmes(crname,'W','Error in DCGM Bank')
        return
      endif
      ncol_dcgm = JW(ind_dcgm-5) ! number of columns of dcgm bank
      ipt_gm = ind_dcgm + (ipl-1)*ncol_dcgm

      if (nami_dcgw.le.0) nami_dcgw = mamind(jw,'DCGW')
      ind_dcgw = jw(nami_dcgw)
      do while (jw(ind_dcgw-2).lt.isec
     &     .AND.jw(ind_dcgw-1).gt.0)
        ind_dcgw = jw(ind_dcgw-1)
      enddo
      if (jw(ind_dcgw-2).ne.isec) then
c        call recmes(crname,'W','Error in DCGW Bank')
        return
      endif
      ncol_dcgw = JW(ind_dcgw-5) ! number of columns of dcgw bank

      pi=dacos(-1.d+00)

c  (a,b,c) in horizontal plane
c      call clas_to_sector(isec,a,b,c) ! wire positions are defined in sector
c      call clas_to_sector(isec,ra,rb,rc)

      status=2

      ipt_gw = ind_dcgw + ((ipl-1)*192+iwire-1)*ncol_dcgw
      xwire = dble(RJW(ipt_gw+DCGW_X_MID))
      zwire = dble(RJW(ipt_gw+DCGW_Z_MID))

      M(1) = a
      M(2) = b
      M(3) = c
      u(1) = ra
      u(2) = rb
      u(3) = rc
c      if (print_out.ge.1) print *,' Track ',M,u

c      if (print_out.ge.1) print *,'max_cylinder',max_cylinder
c      max_cylinder = 10000

      Oc(1)=dble(RJW(ipt_gm+DCGM_X_CURVE))
      Oc(2)=dble(RJW(ipt_gm+DCGM_Y_CURVE))
      Oc(3)=dble(RJW(ipt_gm+DCGM_Z_CURVE))
      dc=dble(RJW(ipt_gm+DCGM_R_CURVE))
      nc(1)=dble(RJW(ipt_gm+DCGM_X_NMID))
      nc(2)=dble(RJW(ipt_gm+DCGM_Y_NMID))
      nc(3)=dble(RJW(ipt_gm+DCGM_Z_NMID))
      call interLineCylinder(M,u,Oc,dc,nc,Ic,failc)
c        if (print_out.ge.1) print *,' Cylinder ',Oc,dc,nc

      if (failc) then
        status = 0
        return
      endif

      a = Ic(1,1)
      b = Ic(1,2)
      c = Ic(1,3)
      d1 = (xwire-Ic(1,1))**2 + (zwire-Ic(1,3))**2
      d2 = (xwire-Ic(2,1))**2 + (zwire-Ic(2,3))**2
      
      if (a.lt.0.d+00.or.dabs(datan(b/a)).gt.pi/6.d+00
     &     .or.d2.lt.d1) then
        a = Ic(2,1)
        b = Ic(2,2)
        c = Ic(2,3)
      endif
c
      if (a.lt.0.d+00.or.dabs(datan(b/a)).gt.pi/6.d+00) then
        status = 0
        return
      endif

c      print *,isec,ipl,a,atan(b/a)*180./pi,fail

c Attention : In Sector Coord. Sys., a and b are not equivalent.

      max_cylinder = JW(ipt_gm+DCGM_MAX_CYLW)
      ipt_gw = ind_dcgw + ((ipl-1)*192+max_cylinder-1)*ncol_dcgw
      in_plane_section = 
     &     (ipl.le.4.and.
     &     1.*c.lt.RJW(ipt_gw+DCGW_Z_MID))

      if (in_plane_section) then ! planar section of region 1
        Op(1)=0.D+00
        Op(2)=0.D+00
        Op(3)=0.D+00
        dp=dble(RJW(ipt_gm+DCGM_P_DIST))
        np(1)=dble(RJW(ipt_gm+DCGM_X_NORM))
        np(2)=dble(RJW(ipt_gm+DCGM_Y_NORM))
        np(3)=dble(RJW(ipt_gm+DCGM_Z_NORM))
        call interLinePlane(M,u,Op,dp,np,Ip,failp)
c        if (print_out.ge.1) print *,' Plane',Op,dp,np

        if (failp) then
          status = 0
          return
        endif

        a = Ip(1)
        b = Ip(2)
        c = Ip(3)
        
        if (a.lt.0.d+00.or.dabs(datan(b/a)).gt.pi/6.d+00) then
          status = 0
          return
        endif
c        print *,isec,ipl,a,atan(b/a)*180./pi,fail
      endif

c      call sector_to_clas(isec,a,b,c)

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine interLineCylinder(M,u,O,d,n,I,fail)
C finds the intersection of a line with a cylinder.

      IMPLICIT NONE

c Input parameters :
c line definition : point M, vector u
      double precision M(3)                 ! a point of the line
      double precision u(3)                 ! vector director of the line
c cylinder definition : point O, vector n, radius d
      double precision O(3)                 ! point in space
      double precision d                    ! radius of cylinder
      double precision n(3)                 ! vector colinear to the axis of the cylinder

c Output parameters :
c intersection point I
      double precision I(2,3)   ! 2 intersection points
      logical fail

c Local variables :
      double precision normn,normu,scal,Mprime(3),uprime(3)
      double precision a,b,c,delta,k1,k2

      fail = .false.

      normn = dsqrt(n(1)*n(1) + n(2)*n(2) + n(3)*n(3))

c M' : projection of M, parallely to n, on plane containing O
c      and perpendicular to n.
C u' : projection of u, same projection, same plane


      scal = (M(1)-O(1))*n(1) + (M(2)-O(2))*n(2) + (M(3)-O(3))*n(3)
      Mprime(1) = M(1) - scal*n(1)/(normn*normn)
      Mprime(2) = M(2) - scal*n(2)/(normn*normn)
      Mprime(3) = M(3) - scal*n(3)/(normn*normn)

      scal = u(1)*n(1) + u(2)*n(2) + u(3)*n(3)
      uprime(1) = u(1) - scal*n(1)/(normn*normn)
      uprime(2) = u(2) - scal*n(2)/(normn*normn)
      uprime(3) = u(3) - scal*n(3)/(normn*normn)
      normu = dsqrt(uprime(1)*uprime(1) 
     &           + uprime(2)*uprime(2) 
     &           + uprime(3)*uprime(3))

      a =     normu*normu

      b =    (Mprime(1) - O(1))*uprime(1)
     &     + (Mprime(2) - O(2))*uprime(2)
     &     + (Mprime(3) - O(3))*uprime(3)

      c =    (Mprime(1) - O(1))*(Mprime(1) - O(1))
     &     + (Mprime(2) - O(2))*(Mprime(2) - O(2))
     &     + (Mprime(3) - O(3))*(Mprime(3) - O(3))
     &     - d*d

      delta = b*b - a*c
      if (delta.lt.0) then
        fail = .true.           ! track does not intersect the cylinder
        return
      endif

c     M inside cylinder <=> c<0
c           => delta > b*b

c 2 intersections : k = -b +/- sqrt(delta)
      k1 = (-b - dsqrt(delta))/a
      k2 = (-b + dsqrt(delta))/a
c ; a is positive (normu**2) ; c/a = k1*k2 ; b/a = -(k1+k2)

      if (
     &     c.le.0..or.          ! k2 > 0 > k1
     &     (c.gt.0..and.b.ge.0.)) ! 0 > k2 > k1
     &     then
        I(1,1) = M(1) + k2*u(1)
        I(1,2) = M(2) + k2*u(2)
        I(1,3) = M(3) + k2*u(3)
        
        I(2,1) = M(1) + k1*u(1)
        I(2,2) = M(2) + k1*u(2)
        I(2,3) = M(3) + k1*u(3)
      else                      ! k2 > k1 > 0
        I(1,1) = M(1) + k1*u(1)
        I(1,2) = M(2) + k1*u(2)
        I(1,3) = M(3) + k1*u(3)
        
        I(2,1) = M(1) + k2*u(1)
        I(2,2) = M(2) + k2*u(2)
        I(2,3) = M(3) + k2*u(3)
      endif
        
c     take the hit in the direction of u (if one) or
c     the closest intersection to M (smallest value of |k|)
c 1/  M outside (c>0) and b<0 => k>0
c 2/  M outside (c>0) and b>0 => k<0

c  If vector u is changed in -u, both solutions are exchanged.

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine interLinePlane(M,u,O,d,n,I,fail)
C finds the intersection of a line with a plane.

      IMPLICIT NONE

c Input parameters :
c line definition : point M, vector u
      double precision M(3)                 ! a point of the line
      double precision u(3)                 ! vector director of the line
c plane definition : point O, distance d, vector n
c plane tangent to sphere of center o and radius d.
c n towards exterior.
      double precision O(3)                 ! point in space
      double precision d                    ! distance from O to plane
      double precision n(3)                 ! normal vector to plane

c Output parameters :
c intersection point I
      double precision I(3)
      logical fail

c Local variables :
      double precision normu,normn,uscaln,mm

      fail = .false.

      normu = dsqrt(u(1)*u(1) + u(2)*u(2) + u(3)*u(3))
      normn = dsqrt(n(1)*n(1) + n(2)*n(2) + n(3)*n(3))
      uscaln = u(1)*n(1) + u(2)*n(2) + u(3)*n(3)
      if (uscaln.eq.0.d+00) then
        fail = .true.
        return
      endif

      mm = ((M(1)-O(1))*n(1)) + ((M(2)-O(2))*n(2)) + ((M(3)-O(3))*n(3))
      mm = mm/normn

      I(1) = M(1) + (d-mm)*normn/uscaln*u(1)
      I(2) = M(2) + (d-mm)*normn/uscaln*u(2)
      I(3) = M(3) + (d-mm)*normn/uscaln*u(3)

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      FUNCTION distance2lines(
     &     bx1,by1,bz1,ax1,ay1,az1,
     &     bx2,by2,bz2,ax2,ay2,az2)
      implicit none

C Compute distance between 2 lines in space which equations are
c     | x1(t1) = ax1*t1 + bx1               | x2(t2) = ax2*t2 + bx2
c  D1 | y1(t1) = ay1*t1 + by1 ;          D2 | y2(t2) = ay2*t2 + by2
c     | z1(t1) = az1*t1 + bz1               | z2(t2) = az2*t2 + bz2
c
c     | ax1                                 | ax2
c  k1 | ay1                   ;          k2 | ay2
c     | az1                                 | az2
c
c  k1 and k2 not necessarly of length 1

      double precision distance2lines
      double precision ax1,ay1,az1,bx1,by1,bz1,
     &     ax2,ay2,az2,bx2,by2,bz2

      double precision kx,ky,kz,normk

c Vector k is perpendicular to k1 and k2
c We suppose k1 and k2 are not colinear

      kx = ay1*az2 - az1*ay2
      ky = az1*ax2 - ax1*az2
      kz = ax1*ay2 - ay1*ax2

      normk = dsqrt(kx*kx + ky*ky + kz*kz)

      if (normk.eq.0.d+00) then
c needs to determine k by another method.
c this never happens in case of tracks through DC
        distance2lines = 0.d+00
        return
      endif
      
      distance2lines=dabs(kx*(bx1-bx2) + ky*(by1-by2) + kz*(bz1-bz2))
     &                   /normk

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine sector2clas(isec,x,y,z)
C Converts from sector coordinate system to CLAS coordinate system.

      IMPLICIT NONE

      integer isec              ! sector #
      double precision x,y,z
      double precision tx,ty,tz

      double precision c,s

      c = dcos((isec-1)*dacos(-1.d+00)/3.d+00)
      s = dsin((isec-1)*dacos(-1.d+00)/3.d+00)

      tx = x*c - y*s
      ty = y*c + x*s
      tz = z

      x = tx
      y = ty
      z = tz

      return
      end
c                          ----------------------
c Position of wires are IN | SECTOR COORDINATES |
!-----------------------------------------------------------------------
!       BANKname BANKtype      ! Comments
! TABLE  DCGW   ! create display delete ! DC geometry info by wire - in sector coordinate system
!
!   ATTributes:
!   -----------
!COL ATT-name FMT   Min    Max          ! Comments
!  1  x_mid     F  -1000.0  1000.0          ! x at midplane of wire(cm)
!  2  y_mid     F  -1000.0  1000.0          ! y at midplane of wire(cm)
!  3  z_mid     F  -1000.0  1000.0          ! z at midplane of wire(cm)
!  4  x_dir     F  -1.0000  1.0000          ! x direction cosine along wire (cm)
!  5  y_dir     F  -1.0000  1.0000          ! y direction cosine along wire (cm)
!  6  z_dir     F  -1.0000  1.0000          ! z direction cosine along wire (cm)
!  7  w_len     F   0.0     1000.0          ! wire length from midplane to amplifier (cm)
!  8  w_len_hv  F   0.0     1000.0          ! wire length from midplane to HV (cm)
!
!    RELations:
!    ----------
!COL RELname  RELtype INTbank  ! Comments
!                     (COL)
!
! END TABLE
!

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE MAKE_CLUSTER(ISL)
      IMPLICIT NONE

*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='REGION1')
      PARAMETER (CRAUTH='Laurent Farhi')

c  Local variables
      INTEGER isl,minl,maxl,parity
      INTEGER i,j,neighbour,ihit
      INTEGER il,iw,iln,iwn,itdc,itdcn
      INTEGER cl1,cl2,clt

c  arrays for wires
      INTEGER wire_tdc(36,192)  ! init to 0
      INTEGER wire_cl_id(36,192) ! init to 0
      INTEGER max_w_in_sl
      PARAMETER (MAX_W_IN_SL=192*6) ! max # of wires hit in one SL
      INTEGER wire_layer(6,MAX_W_IN_SL)
      INTEGER wire_number(6,MAX_W_IN_SL)
      INTEGER n_hit_in_sl(6)    ! init to 0
      COMMON/WIRES/wire_tdc,wire_cl_id,
     &     wire_layer,wire_number,n_hit_in_sl

c  arrays for clusters in SL
      INTEGER MAXCL,MIN_CL_SIZE,MAX_CL_SIZE
      PARAMETER (MAXCL=20)
      PARAMETER (MIN_CL_SIZE=2)
      PARAMETER (MAX_CL_SIZE=50)
      INTEGER nocl(6)           ! # of clusters in SL, init to 0
      INTEGER n_hit_in_cl(6,MAXCL) ! cluster size (# of wires)
      INTEGER w_in_cl(6,MAXCL,MAX_CL_SIZE,6)
      COMMON/CLUSTERS/nocl,n_hit_in_cl,w_in_cl

c  neighbor definition
      INTEGER Nneighb
      parameter (Nneighb = 12)
      INTEGER dec_l(2,Nneighb),dec_w(2,Nneighb)
c
C   min and max wires' numbers for all layers.
C   layers 5 and 6 in axial superlayer in REGION 1 do not exist in nature -
C   - they are dummy.

c   Setup superlayer :
C
      MINL=6*ISL-5
      MAXL=6*ISL
      IF (ISL.eq.1) MAXL=4

c Define pattern for neighbour wires to n.
c Any pattern must be SYMETRIC WITH RESPECT TO CENTRAL WIRE.

C For central wire in EVEN layer (2, 4, 6)
c             / \ / \ / \
c            |n+1| n |n-1|      layer l+2
C             \ / \ / \ /
c              |n+1| n |        layer l+1
c             / \5/ \4/ \
c            |n+1| n |n-1|      layer l EVEN
c             \6/ \ / \3/
c              |n+1| n |        layer l-1
c             / \1/ \2/ \
c            |n+1| n |n-1|      layer l-2
c             \ / \ / \ /

      dec_l(2, 1)=-1
      dec_l(2, 2)=-1
      dec_l(2, 3)= 0
      dec_l(2, 4)= 1
      dec_l(2, 5)= 1
      dec_l(2, 6)= 0
c
      dec_l(2, 7)=-2
      dec_l(2, 8)=-2
      dec_l(2, 9)=-2
      dec_l(2,10)= 2
      dec_l(2,11)= 2
      dec_l(2,12)= 2

      dec_w(2, 1)= 1
      dec_w(2, 2)= 0
      dec_w(2, 3)=-1
      dec_w(2, 4)= 0
      dec_w(2, 5)= 1
      dec_w(2, 6)= 1

      dec_w(2, 7)= 1
      dec_w(2, 8)= 0
      dec_w(2, 9)=-1
      dec_w(2,10)=-1
      dec_w(2,11)= 0
      dec_w(2,12)= 1
C For central wire in ODD layer (1, 3, 5)
c             / \ / \ / \
c            |n+1| n |n-1|      layer l+2
C             \ / \ / \ /
c              | n |n-1|        layer l+1
c             / \5/ \4/ \
c            |n+1| n |n-1|      layer l ODD
c             \6/ \ / \3/
c              | n |n-1|        layer l-1
c             / \1/ \2/ \
c            |n+1| n |n-1|      layer l-2
c             \ / \ / \ /

      dec_l(1, 1)=-1
      dec_l(1, 2)=-1
      dec_l(1, 3)= 0
      dec_l(1, 4)= 1
      dec_l(1, 5)= 1
      dec_l(1, 6)= 0
c
      dec_l(1, 7)=-2
      dec_l(1, 8)=-2
      dec_l(1, 9)=-2
      dec_l(1,10)= 2
      dec_l(1,11)= 2
      dec_l(1,12)= 2

      dec_w(1, 1)= 0
      dec_w(1, 2)=-1
      dec_w(1, 3)=-1
      dec_w(1, 4)=-1
      dec_w(1, 5)= 0
      dec_w(1, 6)= 1

      dec_w(1, 7)= 1
      dec_w(1, 8)= 0
      dec_w(1, 9)=-1
      dec_w(1,10)=-1
      dec_w(1,11)= 0
      dec_w(1,12)= 1
C-----------------------------------------------------------

      NOCL(isl) = 0             ! cluster index initialization

      do j = 1,N_HIT_in_SL(isl)
        IL = wire_layer(isl,j)      ! LAYER NUMBER
        IW = wire_number(isl,j)     ! WIRE NUMBER
        ITDC = wire_tdc(IL,IW)
        PARITY = 2 - IL + ((IL/2)*2) ! PARITY = 2(IL even) or 1(IL odd)

c        if (print_out.ge.2)
c     &       print *,'CENTER ',IL,IW,Wire_cl_id(IL,IW
c     &       )

c        IF (Wire_cl_id(IL,IW).LT.0) THEN
C   Wire_cl_id < 0 => this wire has not been inspected yet
C   Wire_cl_id > 0 => Wire_cl_id is the cluster number to which this wire is assigned
        DO NEIGHBOUR = 1,Nneighb
          ILN = IL+dec_l(PARITY,NEIGHBOUR)
          IWN = IW+dec_w(PARITY,NEIGHBOUR)
          IF (ILN.ge.minl.and.ILN.le.maxl.and.
     &         IWN.ge.1.and.IWN.le.192) THEN
            IF (Wire_cl_id(ILN,IWN).NE.0) THEN

              IF(Wire_cl_id(IL,IW).LT.0.and. ! base wire
     &             Wire_cl_id(ILN,IWN).LT.0) ! neighbour wire
     &             THEN
C none of base or beighbour wire belong to a cluster (Wire_cl_id<0) : create
C a new cluster of 2 wires.
                IF (NOCL(isl).LT.MAXCL) THEN
                  NOCL(isl) = NOCL(isl) + 1 ! new cluster
                  Wire_cl_id(IL ,IW ) = NOCL(isl)
                  Wire_cl_id(ILN,IWN) = NOCL(isl)
                  
                  IF (2.LE.MAX_CL_SIZE) THEN
                    N_HIT_in_CL(isl,NOCL(isl))=2
                    W_in_CL(isl,NOCL(isl),1,1) = IL
                    W_in_CL(isl,NOCL(isl),1,2) = IW
                    W_in_CL(isl,NOCL(isl),1,3) = ITDC
                    W_in_CL(isl,NOCL(isl),2,1) = ILN
                    W_in_CL(isl,NOCL(isl),2,2) = IWN
                    W_in_CL(isl,NOCL(isl),2,3) = 
     &                   Wire_tdc(ILN,IWN)

c                    if (print_out.ge.2) call print_cluster(isec,isl,nocl(isec,isl))
                  ENDIF
                else
                  call recmes(crname,'W'
     &                 ,'**** NUMBER OF CLUSTERS LIMITED BY ARRAY SIZE ****')
                ENDIF

              ELSEIF(Wire_cl_id(IL,IW).GT.0.and.
     &               Wire_cl_id(ILN,IWN).LT.0)
     &               THEN
C The base wire belongs to a cluster already.
C Add the neighbour wire to this cluster.
                CL1 = Wire_cl_id(IL,IW) ! cluster ID
                Wire_cl_id(ILN,IWN) = CL1
c                if (print_out.ge.2) call print_cluster(isl,cl1)

                IF (N_HIT_in_CL(isl,CL1).LT.MAX_CL_SIZE) THEN
                  N_HIT_in_CL(isl,CL1)=N_HIT_in_CL(isl,CL1)+1
                  W_in_CL(isl,CL1,N_HIT_in_CL(isl,CL1),1)=ILN
                  W_in_CL(isl,CL1,N_HIT_in_CL(isl,CL1),2)=IWN
                  W_in_CL(isl,CL1,N_HIT_in_CL(isl,CL1),3)=
     &                Wire_tdc(ILN,IWN)
                ENDIF
c                if (print_out.ge.2) print *,'+ ',ILN,IWN,' ='
c                if (print_out.ge.2) call print_cluster(isl,cl1)

              ELSEIF(Wire_cl_id(IL,IW).LT.0.and.
     &               Wire_cl_id(ILN,IWN).GT.0)
     &               THEN
c The base wire is not part of a cluster but the neighbour wire already
C is. The base wire is included to this cluster.
                CL1 = Wire_cl_id(ILN,IWN) ! cluster ID
                Wire_cl_id(IL,IW) = CL1
c                if (print_out.ge.2) call print_cluster(isl,cl1)

                IF (N_HIT_in_CL(isl,CL1).LT.MAX_CL_SIZE) THEN
                  N_HIT_in_CL(isl,CL1)=N_HIT_in_CL(isl,CL1)+1
                  W_in_CL(isl,CL1,N_HIT_in_CL(isl,CL1),1)=IL
                  W_in_CL(isl,CL1,N_HIT_in_CL(isl,CL1),2)=IW
                  W_in_CL(isl,CL1,N_HIT_in_CL(isl,CL1),3)=ITDC
                ENDIF
c                if (print_out.ge.2) print *,'+ ',IL,IW,' ='
c                if (print_out.ge.2) call print_cluster(isl,cl1)

              ELSEIF(Wire_cl_id(IL,IW).GT.0.and.
     &               Wire_cl_id(ILN,IWN).GT.0)
     &               THEN
C Both base and cluster wires belong to 2 different clusters : merge
C them into one.
                CL1 = Wire_cl_id(IL,IW)
                CL2 = Wire_cl_id(ILN,IWN)
                IF (CL1.NE.CL2) THEN
c - MERGES THE 2 CLUSTERS CL1 & CL2 ----------------------
c                if (print_out.ge.2) call print_cluster(isl,cl1)
c                if (print_out.ge.2) print *,' + '
c                if (print_out.ge.2) call print_cluster(isl,cl2)
                  IF((N_HIT_in_CL(isl,CL1)+N_HIT_in_CL(isl,CL2
     &                 )).LE.MAX_CL_SIZE)THEN
                    IF (CL1.GT.CL2) THEN ! Keep the lowest cluster ID value.
                      CLT=CL1
                      CL1=CL2
                      CL2=CLT
                    ENDIF
                    
                    do I=1,N_HIT_in_CL(isl,CL2)
                      ILN=W_in_CL(isl,CL2,I,1)
                      IWN=W_in_CL(isl,CL2,I,2)
                      ITDCN=W_in_CL(isl,CL2,I,3)
                      Wire_cl_id(ILN,IWN) = CL1
                      
                      ihit=N_HIT_in_CL(isl,CL1)+I
                      W_in_CL(isl,CL1,ihit,1)=ILN
                      W_in_CL(isl,CL1,ihit,2)=IWN
                      W_in_CL(isl,CL1,ihit,3)=ITDCN
                    enddo
                    
                    N_HIT_in_CL(isl,CL1) = 
     &                   N_HIT_in_CL(isl,CL1) +
     &                   N_HIT_in_CL(isl,CL2)
                    N_HIT_in_CL(isl,CL2) = 0 ! Discard cluster CL2
                  ENDIF
C --------------------------------------------------------
c                if (print_out.ge.2) print *,' = '
c                if (print_out.ge.2) call print_cluster(isl,cl1)
                ENDIF           ! (CL1.NE.CL2)
              ENDIF             ! ON CL1 AND CL2 SIGN
            ENDIF               ! (Wire_cl_id(ILN,IWN).NE.0)
          ENDIF                 ! check IN and IWN boudaries
        ENDDO                   ! NEIGHBOUR = 1,6
c        ENDIF                   ! (Wire_cl_id(IL,IW).LT.0)
      enddo                     ! do j = 1,N_HIT_in_SL

      do CL1=1,NOCL(isl)
        if (N_HIT_in_CL(isl,CL1).LT.MIN_CL_SIZE) then
          N_HIT_in_CL(isl,CL1)=0 ! discard cluster
c keeps cluster with size >= MIN_CL_SIZE
        endif
      enddo

      RETURN
      END
