CDECK  ID>, CCSTEP.
      SUBROUTINE CCSTEP
c
c_begin_doc
c  RCS ID string
c  $Id: ccstep.F,v 1.9 2009/04/08 07:18:56 vlassov Exp $
c
c  Documentation for subroutine CCSTEP
c
CA)========================================(begin of short description)
C    calculates hits for CC - counters
CB)-----------------------------------------(begin of long description)
C
C    Purpose and methods : make and store hits for CC :
C
C    Called by:
C
C    Input arguments : None
C    Input/output arguments : None
C    Output arguments : None
C
C    Side effects (other variables/banks modified, etc.):
C        common /ccph/ is modified (see ccph.inc) ;
C        HITS are stored in ZEBRA structure.
C
C    Created:   25-Jan-1994   Alexander Vlassov
C    Modified   21-FEB-1995   Eugeni Doroshkevich
C              Add COMMON/INPART/ to store CC start point in BOS
C-   Modified   21-AUG-1995   Alexander V. Vlassov
C
C
CC)-------------------------------------------(end of long description)
C----------------------------------------------------------------------
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
#include "gconsp.inc"
#include "gctrak.inc"
#include "gcunit.inc"
#include "gcsets.inc"
#include "gckine.inc"
#include "gcking.inc"
#include "gcvolu.inc"
#include "gctmed.inc"
#include "gcflag.inc"
#include "ccp.inc"
#include "ccph.inc"
#include "bcs.inc"
#include "ccdig.inc"
#include "cccom.inc"
C
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
C----------------------------------------------------------------------
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*(132)  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ccstep.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.9 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2009/04/08 07:18:56 $')
      PARAMETER (CAUTHO = '$Author: vlassov $')
      DATA CRCSID/ 
     1'$Id: ccstep.F,v 1.9 2009/04/08 07:18:56 vlassov Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ccstep')                                   
      PARAMETER (CRAUTH='Alexander Vlassov')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
CD)----------------------------------(declare common blocks, variables)
C
c
c  Local User defined variables:
c  -----------------------------
C  External references:


#ifdef Linux 
#ifndef OSF1
      real acosd, atan2d
      external acosd, atan2d
#endif
#endif

#ifdef USEGNU 
      real acosd, atan2d
      external acosd, atan2d
#endif
      real vmod,vdist
      integer geant_to_pdg
      external vmod,vdist,geant_to_pdg
c
      logical new_track
c
      integer i, k, ncc, jtrnew
      integer MSTEP, MTRSTP
      integer itra_old, istak_old, ivert_old, ipart_old, ich_old
      integer jhit, nvo, ng, INI_PART, IFIRST,ng_rnd(1)
c
      REAL*4 dloc(3), cloc(3)
      REAL*4 s, teta, phi
      real*4 dr2, xloc_in(3), xloc_out(3), sv_in, sv_out, v_imp
      real*4 ARA, DS, DC
      real*4 GMMIN, GFACT, veloc
      real*4 pvert_old(3)
      real*4 akvel, a_ng(1), a, x_sub(3), x_tmp(3), xrad(3), dir(3)
      real*4 O1(3), O2(3), a1, a2, s_plus, x_out(3), d(3)
      real*4 GAMLEN,sura,cura
      real*4 andef
      real*4 grn(2),vsmall
      data vsmall /0.000001/
c
      integer  ihit
      real  hitsh(5)
c
C-----------------------------------------------------------------------
c
C translate volume names for all levels from INTEGER to CHARACTER*4
C
      CHARACTER*4 volnam(15)
      EQUIVALENCE (names(1),volnam(1))
      CHARACTER*4 partna(3),casena
      EQUIVALENCE (napart(1),partna(1))
      EQUIVALENCE (kcase,casena)
C
      data GMMIN /18.09827552/       ! C4F10 gas
      data GAMLEN,sura,cura /2.29,0.055253883,0.998472337/     ! C4F10
      data andef /1.00153/            !  Refraction index of C_4F_10
      data IFIRST,INI_PART/1 , 9 /    ! pi-
c
CE)===============================================(end of declarations)
c_end_var
c
c  executable code for routine CCSTEP
c-----------------------------------------------------------------------
C
c
      IF(nlevel.lt.2) return           
c     ======================>                 NOT CC region
      IF(volnam(nlevel-1).NE.'CC  ') return
c     ======================================>    CC region only
      IF(volnam(nlevel).NE.'CCVO') return
c     ======================================>    CC region only
C**********************************************************************
c
c    Remember the type of initial particle
c
      if(IFIRST.eq.1) then
        INI_PART = ipart
        IFIRST   = 0
      end if
c
      IF(abs(charge).lt.vsmall) go to 999
C     =========================> WE ARE INTERESTED IN charged particles.
C
      if(abs(amass).lt.vsmall) then
        CRMESS = 'CHARGED particle with ZERO mass ?!'
        call recmes(CRNAME,'W',CRMESS)
        veloc = 1.0
        gfact = big
      else
        veloc = vect(7)/sqrt( vect(7)*vect(7) + amass*amass )
        GFACT = sqrt(amass*amass + vect(7)*vect(7))/amass
      end if
C
      ncc = number(nlevel)
      if(jtr.eq.0) then      !  ==== First entry in the event
c
        s = pvert(3)/vmod(pvert,3)
        if(abs(s).gt.1.) then
          teta = 0.
          phi =  0.
        else
          teta = acosd(s)
          a1   = pvert(1)/vmod(pvert,3)
          a2   = pvert(2)/vmod(pvert,3)
          phi  = atan2d(a2,a1)
        end if
c
c      teta, phi - angles of the initial particle in the target
c
        v0teta = teta
        v0phi  = phi
c
        in_part = ipart               ! Ini. part. type
        nsc_in = number(nlevel - 2)   ! Ini. part. Sector  #
        ncc_in = number(nlevel)       ! Ini. part. Segment #
        call ucopy(vect,xtr_in,3)     ! Ini. part. entry point
        call ucopy(vect(4),amom_in,3) ! Ini. part. momentum
      end if
C
      IF(GFACT.le.GMMIN) go to 999
C     ======================> WE ARE INTERESTED IN FAST particles ONLY!
      IF(JTR.GE.MJTR)  go to 999
C     ======================> THERE ARE too MANY tracks in event!
C
*     ======================>      IF(ISTAK.NE.0)   go to 999
C     ======================> WE ARE INTERESTED in DIRECT TRACKS!
C
      if(        itra .eq.  itra_old
     &  .and.   istak .eq. istak_old
     &  .and.   ivert .eq. ivert_old
     &  .and.   ipart .eq. ipart_old
     &  .and.     ncc .eq.   ich_old
     &  .and. abs(pvert(1)-pvert_old(1)).lt.vsmall
     &  .and. abs(pvert(2)-pvert_old(2)).lt.vsmall
     &  .and. abs(pvert(3)-pvert_old(3)).lt.vsmall ) then
        new_track = .false.
      else
        new_track = .true.
      end if
c
 
      IF( (inwvol.EQ.1) .and. new_track ) THEN
C
C *** First step in the volume  ****
C
        if(mtrstp.ge.mstp) then
c       ========================> TOO many steps
          go to 999
        end if
c
        itra_old   = itra
        istak_old  = istak
        ipart_old  = ipart
        ivert_old  = ivert
        ich_old    = ncc
        call ucopy(pvert,pvert_old,3)
c
        call ccdefpar(ncc)
        dr2 = (y12(ncc) + y21(ncc))/2.
C
C get of local coordinates current point
C
        CALL GMTOD(vect(1),cloc(1),1)
        CALL GMTOD(vect(4),dloc(1),2)
C
        xloc_out(1) = cloc(1)
        xloc_out(2) = cloc(3) + dr2
        xloc_out(3) = -cloc(2)
        sv_out      = clight*tofg
        v_imp       = vect(7)
        call ucopy(xloc_out,xloc_in,3)
C
C remember of local coordinates
C
        jtrnew  = jtr + 1    ! new track begins for /ph/
        if(jtrnew.gt.mjtr) then
          write(CRMESS,*) 'More than',mjtr,
     *    '  tracks! New track skipped.'
          call recmes(CRNAME,'I',CRMESS)
c
          call ucopy(xloc_out,xloc_in,3)
          sv_in = sv_out
          return
        end if
c
        mstep   = 1
        mtrstp  = 1
C
C       Initial point for CCH bank
C
        call ucopy(vect(1),xenter(1,jtrnew),3) 
        idh_prt(jtrnew)  = geant_to_pdg(ipart) ! particle number
        itrk_num(jtrnew) = 1000*istak + itra
        ph_tot(jtrnew)   = vect(7)
        itrsect(jtrnew)  = number(nlevel - 2)  ! Ini. part. Sector  #
        itrsegm(jtrnew)  = number(nlevel)      ! Ini. part. Segment #
        ihitrk(jtrnew)   = 0
C
C *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*==*=*=*=*=*=*=*=*=
C
      ELSE
C     ==============> ANY other step
C
C       Last point for CCH bank
C
        IF(inwvol.GE.2) THEN
          call ucopy(vect(1),xexit(1,jtrnew),3)
        END IF
C
        if(mtrstp.ge.mstp) then
c       ========================> TOO many tracks
          go to 999
        end if
c
        CALL GMTOD(vect(1),cloc(1),1)
C
        xloc_out(1) = cloc(1)
        xloc_out(2) = cloc(3) + dr2
        xloc_out(3) = -cloc(2)
        sv_out      = clight*tofg
C
        s = vdist(xloc_out,xloc_in,3)
        if(s.lt.0.01) then
          go to 999
        end if
c
c       ============= NON ZERO first path =========
c
        mstep = mstep + 1
 
********************************************************************
        if(mstep.eq.2) then
C
          jtr = jtrnew              ! new track begins for /ccph/
          jhtrk = jtrnew 
          ihitrk (jtr)    = 0 
*
          call ucopy(xloc_out,rtr(1,1,jtr),3)
          nvch (jtr)   = number(nlevel)
          nvsc (jtr)   = number(nlevel - 2)
          vteta(jtr)   = teta
          vphi (jtr)   = phi
          vpmom(1,jtr) = v_imp
          vlen (jtr)   = sv_out
C
        end if
c
********************************************************************
c
c      Storing the particle trajectory
c
        if(jtr.le.0)then
c           write(6,*)' ERROR - CCSTEP.F: jtr < 0 '
           return
        endif


        if(mstep.eq.2) then
c
          mtrstp = 2
          nstp(jtr) = mtrstp
          call ucopy(xloc_out,rtr(1,mtrstp,jtr),3)
          vpmom(mtrstp,jtr) = vect(7)
c
        else if(inwvol.GE.2) then
c
          mtrstp = mtrstp + 1
          nstp(jtr) = mtrstp
          call ucopy(xloc_out,rtr(1,mtrstp,jtr),3)
          vpmom(mtrstp,jtr) = vect(7)
c
        else
c
          s = vdist(xloc_out,rtr(1,mtrstp,jtr),3)
          if(s.ge.stpmin) then
c
            mtrstp = mtrstp + 1
            nstp(jtr) = mtrstp
            call ucopy(xloc_out,rtr(1,mtrstp,jtr),3)
            vpmom(mtrstp,jtr) = vect(7)
c
          end if
c
        end if
c
C *********************************************************************
c
c    ************   RAY TRACING   *************
c
C *********************************************************************
c
c   number of GAMMA's:
c
        ng = 0      ! No photons
        akvel = ( (andef*veloc)*(andef*veloc) - 1.0 ) /
     &          (   veloc*veloc*(andef*andef - 1.0) )
        if(akvel.le.0.01) go to 100  !  NO photons on this step
c
        s = vdist(xloc_in,xloc_out,3)
        A_NG(1) = s * akvel / GAMLEN
        call GPOISS(A_NG,NG_RND,1)    ! Poisson distribution for ng
*        CALL POISSN(A_NG,NG,K)    ! Poisson distribution for ng
        NG = NG_RND(1)
c
c       ========   Number of Gammas is calculated
c
        if(ng.ge.1) then
c
c    Cycle on the Number fo GAMMA's
c **************************************************************
C
          call vsub(xloc_out,xloc_in,x_sub,3) ! vector of step
c
          do k = 1,ng
c
            if(ig.ge.mig) then
              go to 10               ! WE have enough photons !
            end if
c
            ig = ig + 1
            nref(ig) = 0
            ihnc(ig) = 0
            ihsc(ig) = number(nlevel - 2)
            njtr(ig) = jtr
c
            call  grndm(grn,2)
            a = grn(1)
            call vscale(x_sub,a,x_tmp,3)
            call vadd(xloc_in,x_tmp,xrad,3)
            tdc (ig) = sv_in + a*s
c
c   ====>   xrad(1-3) is the local GAMMA radiation point.
c
c   now we are to determin the direction of GAMMA
c
            CALL CCORTBAS(x_sub,1.,O1,O2,I)
*   may be it is easy to use vector algebra functions?
            call vunit(O1,O1,3)
            call vunit(O2,O2,3)
 

            ARA=TWOPI*grn(2)
            DS = sin(ARA)
            DC = cos(ARA)
 
            cura = 1./(andef*veloc)
            sura = sqrt((andef*veloc)*(andef*veloc) - 1.)
     &             /    (andef*veloc)
            a1 = ds*sura*s
            a2 = dc*sura*s
 
            call vscale(O1,a1,O1,3)
            call vscale(O2,a2,O2,3)
            call vscale(x_sub,cura,dir,3)
            call vadd(dir,O1,dir,3)
            call vadd(dir,O2,dir,3)
            call vunit(dir,dir,3)
c
C  =====>  Now  dir - unit vector of direction.
c
            call CCVPHO(xrad,dir,ncc,jhit,nvo,s_plus,x_out,d)
c           =================================================
c           The task of this subroutine is to trace GAMMA
c           from input point (xrad,dir) up to the point of
c           exit (x_out,d). In case if this point is not on the
c           PMT window jhit=0;
c              else jhit=1 - left   PMT, jhit=2 - right  PMT.
c                   s_plus - the lentgh of GAMMA's parth from
c                            initial point to PMT (cm)
c                   nvo = # of CC - hit (it can differ from ncc)
c           ====================================================
c
            if(jhit.eq.1) then
              tdc (ig) = tdc(ig) + s_plus
              ihnc(ig) = nvo
            else if(jhit.eq.2) then
              tdc (ig) = tdc(ig) + s_plus
              ihnc(ig) = -nvo
            else
              tdc (ig) = 0.
              ihnc(ig) = 0
            end if
C
            if(ihnc(ig).ne.0) then     
C
C store hits
C ( iset and idet we have now in /GCSETS/, because we are in xxSTEP )
C
C------ CC number 1-36 odd /even : left - right
              hitsh(4) = 2*nvo - jhit + 1
*
              call ucopy(vect,hitsh,3)
              hitsh(5) = CM_TO_CH*tdc(ig)
C*****        ********************************************
              CALL GSAHIT(ISET,IDET,ITRA,NUMBV,HITSH,IHIT)
C*****        ********************************************
*
c     For CCH bank :
*
              if(icchit.ge.mcchit) then
                write(CRMESS,*) 'More than',mcchit,
     *          '  hits in CC ! New hit skipped.'
                call recmes(CRNAME,'I',CRMESS)
c
              else
c
                icchit = icchit + 1
                itrnum (icchit) = jtr
                iccsect(icchit) = ihsc(ig)
                iccsegm(icchit) = ihnc(ig)            
                icctdc (icchit) = CM_TO_CH*tdc(ig) + 0.5
                ihitrk (jtr)    = ihitrk (jtr) + 1 
*           
              end if
            end if 
c
          end do     ! end of Gamma cycle
        end if
   10   continue
  100   continue
c
        call ucopy(xloc_out,xloc_in,3)
        sv_in = sv_out
c
      END IF
C
  999 CONTINUE
c
      RETURN
      END





