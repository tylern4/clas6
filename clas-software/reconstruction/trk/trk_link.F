
C======================================================================
      SUBROUTINE trk_link(isec,ifail)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find possible trajectories by matching
C-                         permutations of found clusters from 6
C-                         super-layers to the link dictionary.
C-
C-   Inputs  : isec  - the sector to be analyzed
C-             ifail - control flag / number of links found
C-
C-   Controls: ipar_trk_minlinks   - minimum number of segments for good link.
C-
C-
C-   Library belongs    : trklib.a
C-
C-   Calls              : recmes,rernev
C-
C-   Created   JUNE-11-1997 James Mueller
C-   Based on ANA_LINK by Bogdan Niczyporuk
C-
C-   Called by TRK_EVNT_HBT
C-
C This code started out as a modification of ana_link to allow for links
C of less than 6 super layers.  It ended up as a major rewrite.  Because
C it is so different, I have renamed it as has happened to other code.
C A side benefit is that this code doesn't have 12-nested-do-loops.
C The code is designed to reproduce the results of ana_link when 'ipar_trk_minlinks=6'.
C We require 2 axial and 2 stereo segments, in order to provide decent
C 3-d resolution.
C
C I recomended using ipar_trk_minlinks=5 or 6.  In studies on run 3166, I found
C that ipar_trk_minlinks= 5 produced ~3 times more found tracks, and almost all of
C them look good with CED.  Time based tracking should be able to reject
C the others.  
C A short test job with ipar_trk_minlinks=4, yielded very few extra good tracks, 
C and a lot more junk.  At best, recsis will run a lot slower, with minimal
C efficiency increase.  At worst, bad tracks will cause you to hit the
C maximum number of links, hence losing real tracks.
C Of course your mileage may vary, depending on how well the drift chambers
C were operating during your favorite runs.
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*  I/O variables
*  -------------
      INTEGER isec,ifail
*
*  Include Files
*  -------------
#include "analink.inc"
#include "anapatt.inc"
#include "trktcl_var.inc"
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_link.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.18 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/05/07 00:27:04 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: trk_link.F,v 1.18 2008/05/07 00:27:04 fklein Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_LINK')
      PARAMETER (CRAUTH='Jim Mueller')
C
C Local variables
      INTEGER MAX_TRKS_TEMP
      PARAMETER (MAX_TRKS_TEMP=2*MAX_TRACKS)
      BYTE ntest1(4), ntest2(4), ltest1(4), ltest2(4), nlink4(4)
      INTEGER mtest1, mtest2, mylink1, mylink2
      EQUIVALENCE (mtest1,ntest1(1)), (mtest2,ntest2(1))
      EQUIVALENCE (ltest1(1),mylink1),(ltest2(1),mylink2)
      INTEGER mylink3,mylink4, n1, n2, n3, n4, iq, scid
      INTEGER*2 nlink3(2)
      EQUIVALENCE (nlink3(1),mylink3)
      EQUIVALENCE (nlink4(1),mylink4)

      INTEGER igo, it, ir, k, n_old(2),nold_min, i, isum, isum2, ip
      INTEGER cl, isg, icl(6,5), ik(6,5),nmat_cl(6), imat
      INTEGER sl, sl2, istsl, iaxsl, nstereo, naxial, nsgm_lnk
      INTEGER k1, isg1, icl1,isl
      INTEGER k1old(nsgmx,2)
C      INTEGER ktest1, ktest2
C      REAL pold, pnew
      LOGICAL lmatch(6),lmatch_bigger(6)
      integer im, im2, imhigh, imatch(4:6)
      integer imt(4:6,MAX_TRKS_TEMP),imr(4:6,MAX_TRKS_TEMP),imp(4:6,MAX_TRKS_TEMP)
      integer imcl(4:6,6,MAX_TRKS_TEMP),imk(4:6,6,MAX_TRKS_TEMP)
      integer slst(3)
      data slst /1,4,6/
      integer slax(3)
      data slax /2,3,5/
      
      integer iwire,iwire_min,iwire_max,k1min,k1max,itr
      real fwire1(3),fwire2(3)
      integer irun,ievnt,irec
      real tht0,phi0,z0,d0,DEGRAD
      data DEGRAD /0.017453/

C --------------------------- TRK_LINK begins
C
C Loops over clusters in SL1.
C Since this provides the index for the link patterns, a cluster in SL 1 is 
C required.  This would be the next thing to change.
C
      ifail = 0
      IF(ntr_link.GE.MAX_TRACKS) GOTO 999
      n_old(1) = 0
      n_old(2) = 0
      igo   = 0
      call vzero (icl, 6*5)
      do nsgm_lnk=4,6
         imatch(nsgm_lnk) = 0
      enddo

      if(ipar_trk_print(2).ge.2) then
        print *,' trk_link: Sector',isec,'      found clusters:'
        do isl=1,6
          print *,' sly',isl,('    #',i,':',
     & (segm(3,clust(isg1,i,isl,isec),isl,isec),isg1=1,nsegmc(i,isl,isec)),
     &   i=1,nclust(isl,isec))
        enddo
      endif

      do 90 isl=1,2
         IF (nclust(isl,isec).lt.1) GOTO 90
         DO 100 icl1 = 1,nclust(isl,isec)
            nold_min = n_old(isl)+1
C
C Loop over track segments SL1 clusters.
C Use wire hit in layer 2 as index for link dictionary
C
            IF (nsegmc(icl1,isl,isec).LT.1) go to 100
            DO 22 isg1 = 1,nsegmc(icl1,isl,isec)+2
               IF(isg1.le.nsegmc(icl1,isl,isec)) THEN
                  k1 = clust(isg1,icl1,isl,isec)
                  iwire = IABS(segm(3,k1,isl,isec))
                  if(iwire.lt.iwire_min) then
                     iwire_min = iwire
                     k1min=k1
                  endif
                  if(iwire.gt.iwire_max) then
                     iwire_max = iwire
                     k1max=k1
                  endif
               ELSEIF (isg1.eq.nsegmc(icl1,isl,isec)+1) THEN
                  iwire = iwire_min - 1
                  k1 = k1min
               ELSE
                  iwire = iwire_max + 1
                  k1 = k1max
               ENDIF
               ntest2(1) = (iwire-1)/2+1
               do i = nold_min,n_old(isl)
                  IF (ntest2(1).eq.k1old(i,isl)) go to 22
               enddo
               n_old(isl) = n_old(isl) + 1
               k1old(n_old(isl),isl) = ntest2(1)
               nmat_cl(isl) = 1
               icl(isl,1) = icl1
               ik(isl,1)  = k1
               ir = ntest2(1)
               if (ir.le.0) go to 22

               if(ipar_trk_print(2).eq.4) then
                print *,' *** trk_link: pattern for isl,ir',isl,ir,lnk(ir,3-isl)
                 do it=1,lnk(ir,3-isl),4
                  if(lnk(ir,3-isl)-it.eq.2) then
            print '(3(1x,2(1x,Z8)))',(link1(i,ir,3-isl),link2(i,ir,3-isl),i=it,it+2)
                  elseif(lnk(ir,3-isl)-it.eq.1) then
            print '(2(1x,2(1x,Z8)))',(link1(i,ir,3-isl),link2(i,ir,3-isl),i=it,it+1)
                  elseif(lnk(ir,3-isl).eq.it) then
            print '(1x,2(1x,Z8))',link1(it,ir,3-isl),link2(it,ir,3-isl)
                  else
            print '(4(1x,2(1x,Z8)))',(link1(i,ir,3-isl),link2(i,ir,3-isl),i=it,it+3)
                  endif
                 enddo
               endif
C     
C     Scanning big dictionary
C     
               DO 110 it = 1,lnk(ir,3-isl)
                  mylink1=link1(it,ir,3-isl) !get access to dict: ltest1,2
                  mylink2=link2(it,ir,3-isl)
                  fwire1(1)=float(ltest1(1)*2)-0.5
                  fwire2(1)=float(ltest2(1)*2)-0.5
                  do istsl=2,3
                     fwire1(istsl)=float(ltest1(istsl)*3)-1.0
                     fwire2(istsl)=float(ltest2(istsl)*3)-1.0
                  enddo
C     
C     Loop through stereo SLs, looking for a match at least two SLs.
C     If we don't find a match in a SL, save a -wire number given by the pattern.
C     This will give us something to pass to later routines that expect something
C     in every SL.
C     
                  IF (isl.eq.1)then
                     nstereo = 1
                  else
                     do i = 1,n_old(1)
                        IF (ltest2(1).eq.k1old(i,1)) go to 110
                     enddo
                     nstereo = 0
                     nmat_cl(1) = 0
                     ik(1,1) = -ltest2(1)*2
                     icl(1,1) = 0
                  endif
                  DO istsl = 2,3
                     sl = slst(istsl)
                     nmat_cl(sl) = 0
                     icl(sl,1)  = 0
                     ik(sl,1)  = 0
                     DO cl = 1,nclust(sl,isec)
                        DO isg = 1,nsegmc(cl,sl,isec)
                           k = clust(isg,cl,sl,isec)
                           iwire = IABS(segm(3,k,sl,isec))
                           if(abs(fwire2(istsl)-iwire).le.2.0.and.
     +                       nmat_cl(sl).lt.5)then
                              nmat_cl(sl) = nmat_cl(sl)+1
                              icl(sl,nmat_cl(sl)) = cl
                              ik(sl,nmat_cl(sl))  = k
                              go to 101
                           endif
                        ENDDO
 101                    CONTINUE
                     ENDDO
                     if(nmat_cl(sl).gt.0)then
                        nstereo = nstereo + 1
                     else
                        if(sl.le.2) then
                           ik(sl,1) = -ltest2(istsl)*2
                        else
                           ik(sl,1) = -ltest2(istsl)*3+1
                        endif
                     endif
                  ENDDO
C     
C     Nada. We're outa here.
C     
                  IF (nstereo.GE.2) THEN
C     
C     Loop through axial SLs, again looking for a match in at least two.
C     
                     naxial = 0
                     if (isl.eq.2) naxial=1
                     DO iaxsl = naxial+1,3
                        sl = slax(iaxsl)
                        nmat_cl(sl) = 0
                        icl(sl,1) = 0
                        ik(sl,1)  = 0
                        DO cl = 1,nclust(sl,isec)
                           DO isg = 1,nsegmc(cl,sl,isec)
                              k = clust(isg,cl,sl,isec)
                              iwire = IABS(segm(3,k,sl,isec))
                              if(abs(fwire1(iaxsl)-iwire).le.2.0.and.
     +                          nmat_cl(sl).lt.5)then
                                 nmat_cl(sl) = nmat_cl(sl)+1
                                 icl(sl,nmat_cl(sl)) = cl
                                 ik(sl,nmat_cl(sl))  = k
                                 go to 102
                              endif
                           ENDDO
 102                       CONTINUE
                        ENDDO
                        if(nmat_cl(sl).gt.0)then
                           naxial = naxial + 1
                        else
                           if(sl.le.2) then
                              ik(sl,1) = -ltest1(iaxsl)*2
                           else
                              ik(sl,1) = -ltest1(iaxsl)*3+1
                           endif
                        endif
                     ENDDO
C     
C     Do we have enough segments for this pattern?
C     
                     nsgm_lnk = naxial+nstereo
                     IF (nsgm_lnk .GE. ipar_trk_minlinks) THEN
C     
C     Is this an already found cluster pattern?
C     If not, increment number of matches and save relevant quantities for later.
C     Note, that that these are grouped by number of segments found.
C     
                        im = imatch(nsgm_lnk)
                        do i=1,im
                           isum = 0
                           do sl=1,6
                              if((icl(sl,1).eq.imcl(nsgm_lnk,sl,i)))then
                                 isum = isum + 1
                              endif
                           enddo
                           if (isum.eq.6) then
                              go to 103
                           endif
                        enddo
                        IF (im.lt.MAX_TRKS_TEMP) then
                           im = im + 1
                           imatch(nsgm_lnk) = im
                           imt(nsgm_lnk,im) = it
                           imr(nsgm_lnk,im) = ir
                           imp(nsgm_lnk,im) = 3-isl
c fjk: keep superlayer info
c                           mylink3=link3(it,ir,3-isl)
c                           imp(nsgm_lnk,im)  = nlink3(2)
                           DO sl = 1,6
                              imcl(nsgm_lnk,sl,im) = icl(sl,1)
                              imk(nsgm_lnk,sl,im) = ik(sl,1)
                           ENDDO
C
C deal with rare cases where more than one cluster matched this pattern in a 
C superlayer. if so add in all the possible permutations.
C
                           do sl2=1,6
                            if(nmat_cl(sl2).gt.1)then
                             imhigh = im
                             do imat=2,nmat_cl(sl2)
                              do i=imatch(nsgm_lnk),imhigh
                               if(im.lt.MAX_TRKS_TEMP) then
                                im = im + 1
                                imt(nsgm_lnk,im) = imt(nsgm_lnk,i)
                                imr(nsgm_lnk,im) = imr(nsgm_lnk,i)
                                imp(nsgm_lnk,im) = imp(nsgm_lnk,i)
                                DO sl = 1,6
                                 if(sl2.eq.sl)then
                                  imcl(nsgm_lnk,sl,im)= icl(sl,imat)
                                  imk(nsgm_lnk,sl,im) = ik(sl,imat)
                                 else
                                  imcl(nsgm_lnk,sl,im)=imcl(nsgm_lnk,sl,i)
                                  imk(nsgm_lnk,sl,im) =imk(nsgm_lnk,sl,i)
                                 endif
                                ENDDO
C
C make sure we aren't adding somthing that we found before.
C
                                do im2=1,imatch(nsgm_lnk)-1
                                 isum = 0
                                 do sl=1,6
        if((imcl(nsgm_lnk,sl,im).eq.imcl(nsgm_lnk,sl,im2)))isum=isum + 1
                                 enddo
                                 if (isum.eq.6) then
                                  im = im - 1
                                  go to 104
                                 endif
                                enddo
C
 104                            continue
C
                               endif
                              enddo
                             enddo
                            endif
                           enddo
                           imatch(nsgm_lnk) = im
C
                        ENDIF
                     ENDIF
 103                 Continue
                  ENDIF
 110           CONTINUE
 22         CONTINUE            ! End of loop over tracks in SL1 cluster
 100     CONTINUE               ! End of loop over clusters in SL1
 90   CONTINUE
C     
C     Catagorize matches with preference.
C     Take all the links found for this SL1 cluster and decide which ones
C     we want to keep.
C     
      do 130 nsgm_lnk=6,ipar_trk_minlinks,-1
         if(imatch(nsgm_lnk).le.0)goto 130
         DO 120 im = 1,imatch(nsgm_lnk)
            
            IF(nsgm_lnk.LT.6)THEN
               DO sl = 1,6
                  lmatch_bigger(sl) = .FALSE.
               ENDDO
               DO itr=1,ntr_link
                  if(itr_sect(itr).EQ.isec) THEN
                     isum  = 0
                     isum2 = 0
                     DO sl = 1,6
                        lmatch(sl) = .FALSE.
                        IF(lnk_clust(sl,itr).EQ.imcl(nsgm_lnk,sl,im)) THEN
                           isum = isum+1
                           IF(imcl(nsgm_lnk,sl,im).GT.0) lmatch(sl) = .TRUE.
                        ENDIF
                        IF(lnk_clust(sl,itr).GT.0) isum2 = isum2 + 1
                     ENDDO
                     IF(isum.ge.nsgm_lnk) GOTO 120
                     IF(isum2.gt.nsgm_lnk) THEN
                        DO sl = 1,6
                           lmatch_bigger(sl)=lmatch_bigger(sl).OR.lmatch(sl)
                        ENDDO
                     ENDIF
                  ENDIF
               ENDDO
               isum = 0
               DO sl = 1,6
                  IF(lmatch_bigger(sl)) isum = isum + 1
               ENDDO
               IF(isum.ge.nsgm_lnk) GOTO 120
            ENDIF
C
C     Franz moved this up here, so I copied it back to my original position.  
C     But I'll leave this here, so the othere doesn't get moved back.
C            
            IF(ntr_link.GT.MAX_TRACKS.OR.ifail.GT.MAX_TRACKS/2) THEN
               write(CRMESS,'(A,I3,A)')' *** ntr_link >',MAX_TRACKS,
     &              ' How in hell did we get here? ***'
               CALL RECMES('ANA_LINK','I',CRMESS)
               CALL RERNEV(irun,ievnt,irec)
               ntr_link = MAX_TRACKS
               ifail = 1 
               GO TO 999
            ENDIF
            
            ifail    = ifail    + 1 ! No of links/sector
            ntr_link = ntr_link + 1 ! No of links/detector
            itr_level(ntr_link) = TRK_LEVEL_linking
            itr_sect(ntr_link)  = isec
            it = imt(nsgm_lnk,im)
            ir = imr(nsgm_lnk,im) 
            ip = imp(nsgm_lnk,im) 
            do i=1,9
               trkp(i,1,ntr_link)=0     ! initialize
            enddo
C     Saving the segment id's for the matched link
C     Saving the cluster id's for the matched link
            DO sl = 1,6
               lnk_segm(sl,ntr_link) = imk(nsgm_lnk,sl,im)
               if(imk(nsgm_lnk,sl,im).GT.0) then
                  lnk_clust(sl,ntr_link) = imcl(nsgm_lnk,sl,im)
               else
                  lnk_clust(sl,ntr_link) = imk(nsgm_lnk,sl,im)
               endif
            ENDDO
C     
C     Saving momentum of matched pattern
C     
            mylink3=link3(it,ir,ip)
            trkp(7,1,ntr_link) =IAND(nlink3(2),'FFFF'X)/100.

            if (ipar_trk_print(2).EQ.2) then
              if(ifail.EQ.1)write(6,10)
              if(ntr_link.GT.0) then
                  write(6,11)ntr_link,(lnk_clust(sl,ntr_link),sl=1,6),
     1                 trkp(7,1,ntr_link)
               endif
            endif
c
c  other track parameters:
c
            if(dtyp.eq.-1 .AND. ipar_trk_prlink_param.GT.0) then
              mylink4=link4(it,ir,ip)
              n1=IAND(nlink4(1),255)
              iq=n1/16
              if(iq.ne.0) then            ! charge info ok -> initialize averages
                d0=mod(n1,16) -8.            ! in cm
                n2=IAND(nlink4(2),255)
                z0=n2*2-128.               ! in cm.
                n3=IAND(nlink4(3),255)
                phi0=n3*degrad             ! in rad (phi_sda ~ theta_scs)
                n4=IAND(nlink4(4),255)
                tht0=(120.-n4)*degrad      ! in rad (theta_sda ~ -phi_scs)
                if(n3.ge.ipar_trk_prlink_param) then
                  if(iq.eq.1) then    ! if charge info=3: pos.& neg. possible
                    trkp(9,1,ntr_link)=-1. 
                  elseif(iq.eq.2) then
                    trkp(9,1,ntr_link)=1. 
                  endif
                  trkp(1,1,ntr_link)=z0 ! z-vertex
                  trkp(3,1,ntr_link)=d0 ! y-vertex (transverse offset)
                  trkp(4,1,ntr_link)=sin(tht0)*cos(phi0)
                  trkp(5,1,ntr_link)=sin(tht0)*sin(phi0)
                  trkp(6,1,ntr_link)=cos(tht0)
                  scid=ISHFT(link1(it,ir,ip),-24)
                  if(scid.ge.1 .and. scid.le.57) trkp(8,1,ntr_link)=scid
                  if(ipar_trk_print(2).ge.2) then
              print *,' start parameters itr=',ntr_link,'  Sector',isec,'   #',nlink3(1)
              print '(A,2(2x,Z8),A,4F6.0,I4)','lnk=',mylink3,mylink4,
     &                 '  tht0,phi0,z0,d0,scid',tht0/degrad,phi0/degrad,z0,d0,scid 
              print '(A,3f8.3,3f8.4,f8.3,f4.0)',' trkp(i): ',(trkp(i,1,ntr_link),i=1,7),
     &              trkp(9,1,ntr_link)
                  endif
                endif
              endif
            endif
C     
            IF(ntr_link.EQ.MAX_TRACKS.OR.ifail.EQ.MAX_TRACKS/2) THEN
C     
C     If we have reached the maximum number of allowed links/sector, bail out.
C     
               write(CRMESS,'(A,I3,A)')' *** ntr_link =',MAX_TRACKS,
     &              'SUBSEQUENT TRACKS WILL BE TRUNCATED ***'
               CALL RECMES('ANA_LINK','W',CRMESS)
               CALL RERNEV(irun,ievnt,irec)
               ntr_link = MAX_TRACKS
               ifail = 1 
               GO TO 999
            ENDIF
C     
C     
 120     CONTINUE
 130  continue
      
 999  if (ifail.ne.0) call ana_statb(3,isec,0.)
 
      CONTINUE
*     
 10   Format("   Link Seg1 Seg2 Seg3 Seg4 Seg5 Seg6   mom") 
 11   Format(1x,7i5,f6.3)
      
      RETURN
      END
