C======================================================================
      SUBROUTINE ana_link(isec,nlinks)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find possible trajectories by matching
C-                         permutations of found clusters from 6
C-                         super-layers to the link dictionary.
C-
C-   Inputs  : isec  - the sector to be analyzed
C-             nlinks - #linked clusters (segments)
C-
C-   Controls: ipar_trk_make_prlink  - on (> 0) for creating or updating the
C-                         link dictionary.
C-             ipar_trk_minlinks  - min. #segments (superlayers) for accepted link
C-             ^^^^^ up to now the variable is still zcut(2)  (fk,Aug.97)
C-
C-   Library belongs    : trklib.a
C-
C-   Calls              : recmes,rernev
C-
C-   Created  AUG-1997  F.Klein
C-   based on ANA_LINK by Bogdan Niczyporuk
C-
C-   Called by TRK_EVNT_HBT
C-
C-   Modification of ANA_LINK to allow track finding (pattern recognition)
C-   with less than 6 segments (all superlayers) matched.
C-   Method:
C-   Combinations of track segments are compared with 'roads' (i.e. combinations
C-   of segments of simulated tracks): starting with DC-Region 2 or Reg.3
C-   1. test whether segments in all axial superlayers match & collect 
C-      appropriate segments in the stereo superlayers;
C-   2. for the segments which were not matched to tracks: test whether the
C-      segments in all stereo superlayers match & collect segments in axial sly.
C-   preliminary version!
C----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
*  I/O variables
*  -------------
      INTEGER isec,nlinks
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
      PARAMETER ( CFILE = '$RCSfile: ana_link.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.5 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/02/05 19:40:22 $')
      PARAMETER ( CAUTHO = '$Author: manak $')
      DATA CRCSID/
     1'$Id: ana_link.F,v 1.5 1998/02/05 19:40:22 manak Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='ANA_LINK')
      PARAMETER (CRAUTH='Franz Klein')

C
C Local variables

      BYTE mbtest1(4), mbtest2(4)
      INTEGER*2 mbtest3(2)
      INTEGER mytest1, mytest2, mytest3
      EQUIVALENCE (mytest1,mbtest1(1)), (mytest2,mbtest2(1))
      EQUIVALENCE (mytest3,mbtest3(1))

      BYTE mblink1(4), mblink2(4)
      INTEGER*2 mblink3(2)
      INTEGER mylink1, mylink2, mylink3
      EQUIVALENCE (mylink1,mblink1(1)), (mylink2,mblink2(1))
      EQUIVALENCE (mylink3,mblink3(1))
C
      INTEGER it, ir, i, k, n, n_old
      INTEGER icl1, icl2, icl3
      INTEGER icl11, icl12, icl13
      INTEGER isg1, isg2, isg3
      INTEGER isg11, isg12, isg13
      INTEGER k1, k2, k3
      INTEGER k11, k12, k13
      INTEGER link_old(nsgmx)
      INTEGER cltest1(2,max_lnk)
      BYTE bcltest1(4,2,max_lnk)
      EQUIVALENCE (bcltest1(1,1,1),cltest1(1,1))
      INTEGER ktest1(2)
      BYTE bktest1(4,2)
      EQUIVALENCE (bktest1(1,1),ktest1(1))

      REAL pold, pnew
      integer nptr, mypatt, max_it,  kfac(3)
      integer min_sly, isl1, isl2, isl3, isl11, isl12, isl13
      integer mcl11, mcl12, mcl13, istax, istax2, n_klnk
      integer my_clk(3,max_lnk), my_icl(3,max_lnk), patt_lnk(max_lnk)
      integer my_tln(max_lnk), my_rln(max_lnk), my_mom(max_lnk)
      integer nsorted(6)
      data nsorted /12,6,10,4,8,2/	!estimate the quality of tracks depending
					!on the superlayers where segments were found
      
C
C ANA_LINK begins
C
C
C Loops over all combinations of clusters in superlayers
      nlinks = 0
      isl2=3		!start in superlayer# 3 or 5
			! if no track found, try sly#4,6
      min_sly=ipar_trk_minlinks-3	!only superlayers where not started are count
      n_klnk=0

10    continue
      isl3=isl2+2	!take only axial or stereo superlayers
      isl1=5-isl2	!i.e.: SL2&3&5 or SL1&4&6
      isl11=3-isl1	!the other group of Superlayers: stereo/axial
      isl12=7-isl2
      isl13=isl12+2
      istax=isl2/2	   	!istax=1: only axial superlayers
      istax2=3-istax		!istax=2: stereo superlayers
      n_old=0

C loops over segments in clusters:

      DO 11 icl2 = 1,nclust(isl2,isec)
       IF(nsegmc(icl2,isl2,isec).GT.ipar_trk_maxsegm) GO TO 11
       bktest1(2,istax)=icl2
       DO 12 icl3 = 1,nclust(isl3,isec)
        IF(nsegmc(icl3,isl3,isec).GT.ipar_trk_maxsegm) GO TO 12
        bktest1(3,istax)=icl3
        DO 13 icl1 = 1,nclust(isl1,isec)
         IF(nsegmc(icl1,isl1,isec).GT.ipar_trk_maxsegm) GO TO 13
         bktest1(1,istax)=icl1
         bktest1(4,istax)=0
         DO 21 isg2 = 1,nsegmc(icl2,isl2,isec)
          k2 = clust(isg2,icl2,isl2,isec)
	  if(k2.LE.0) goto 21
          mbtest1(2) =(IABS(segm(3,k2,isl2,isec))-1)/3+1
	  mbtest1(4) =0
          DO 22 isg3 = 1,nsegmc(icl3,isl3,isec)
           k3 = clust(isg3,icl3,isl3,isec)
	   if(k3.LE.0) goto 22
           mbtest1(3) =(IABS(segm(3,k3,isl3,isec))-1)/3+1
           DO 23 isg1 = 1,nsegmc(icl1,isl1,isec)
            k1 = clust(isg1,icl1,isl1,isec)
	    if(k1.LE.0) goto 23
            mbtest1(1) =(IABS(segm(3,k1,isl1,isec))-1)/2+1

C Save the link if not an exact one has be compared before
	    if(n_old.GT.0) then
             do k = 1,n_old
              if(link_old(k).EQ.mytest1) goto 23
             enddo 
            endif
            if(n_old.LT.nsgmx) then
             n_old = n_old +1
             link_old(n_old) = mytest1
            endif
     
C Scan the big dictionary
            nptr=0
            call vzero(patt_lnk(1),max_lnk)
            call vzero(my_mom(1),max_lnk)
            call vzero(my_clk(1,1),3*max_lnk)
            call vzero(my_icl(1,1),3*max_lnk)
            call vzero(my_rln(1),max_lnk)
            call vzero(my_tln(1),max_lnk)
            ir=mbtest1(2)
	    if(ipar_trk_make_prlink.EQ.1) then
             max_it=1
            else
	     max_it=lnk(ir,istax)
            endif
cc	write(6,'(A,3I4,1X,Z8)')' test1:istax,max_it,ir',max_it,ir,mytest1
            do 30 it=1,max_it
	     if(ipar_trk_make_prlink.EQ.1) then
               mylink1=mytest1
             else
               mylink1=link1(it,ir,istax)	
               mylink2=link2(it,ir,istax)	
               mylink3=link3(it,ir,istax)
             endif
	     if(mylink1.NE.mytest1) goto 30
             ktest1(istax2)=0
	     mcl12=nclust(isl12,isec)
	     if(mcl12.eq.0) then
              mcl12=1
              bktest1(2,istax2)='FF'X
             endif
	     mcl13=nclust(isl13,isec)
	     if(mcl13.eq.0) then
              mcl13=1
              bktest1(3,istax2)='FF'X
             endif
	     mcl11=nclust(isl11,isec)
	     if(mcl11.eq.0) then
	      mcl11=1
              bktest1(1,istax2)='FF'X
             endif
             DO 31 icl12 = 1,mcl12
              if(bktest1(2,istax2).EQ.0) bktest1(2,istax2)=icl12
	      if(nsegmc(icl12,isl12,isec).GT.ipar_trk_maxsegm) then 
               nsegmc(icl12,isl12,isec)=ipar_trk_maxsegm
	      elseif(nsegmc(icl12,isl12,isec).EQ.0) then
               nsegmc(icl12,isl12,isec)=1 
              endif
              DO 31 icl13 = 1,mcl13
c               if(bktest1(3,istax2).EQ.0) bktest1(3,istax2)=icl13
	       if(nsegmc(icl13,isl13,isec).GT.ipar_trk_maxsegm) then 
                nsegmc(icl13,isl13,isec)=ipar_trk_maxsegm
	       elseif(nsegmc(icl13,isl13,isec).EQ.0) then
                nsegmc(icl13,isl13,isec)=1 
               endif
               DO 31 icl11 = 1,mcl11
                if(bktest1(1,istax2).EQ.0) bktest1(1,istax2)=icl11
	        if(nsegmc(icl11,isl11,isec).GT.ipar_trk_maxsegm) then 
                 nsegmc(icl11,isl11,isec)=ipar_trk_maxsegm
	        elseif(nsegmc(icl11,isl11,isec).EQ.0) then
                 nsegmc(icl11,isl11,isec)=1 
                endif
c  track in clusters found?
                do i=1,n_klnk
                 if(ktest1(istax).EQ.cltest1(istax,i).AND.
     &              ktest1(istax2).EQ.cltest1(istax2,i))then
                  goto 31
                 endif
                enddo
                DO 32 isg12 = 1,nsegmc(icl12,isl12,isec)
                 k12 = clust(isg12,icl12,isl12,isec)
                 if(k12.GT.0) then
                  mbtest2(2) =(IABS(segm(3,k12,isl2,isec))-1)/3+1
                 else
                  mbtest2(2)='FF'x
	         endif
	         mbtest2(4)=0
                 DO 32 isg13 = 1,nsegmc(icl13,isl13,isec)
                  k13 = clust(isg13,icl13,isl13,isec)
	          if(k13.GT.0) then
                   mbtest2(3) =(IABS(segm(3,k13,isl13,isec))-1)/3+1
                  else
                   mbtest2(3)='FF'x
	          endif
                  DO 32 isg11 = 1,nsegmc(icl11,isl11,isec)
                   k11 = clust(isg11,icl11,isl11,isec)
	           if(k11.GT.0) then
                    mbtest2(1) =(IABS(segm(3,k11,isl11,isec))-1)/2+1
                   else
                    mbtest2(1)='FF'x
	           endif

                   if(ipar_trk_make_prlink.EQ.1) mylink2=mytest2
                   if(mytest2.EQ.mylink2) then
                    nptr=-1			 !.true.
	            my_tln(1)=it
	            my_rln(1)=ir
		    n=mblink3(2)
                    my_mom(1)=n
                    my_clk(1,1)=k11
                    my_clk(2,1)=k12
                    my_clk(3,1)=k13
                    my_icl(1,1)=icl11
                    my_icl(2,1)=icl12
                    my_icl(3,1)=icl13
                    goto 40
                   else
                    mypatt=0
                    n=0
                    kfac(1)=-1
                    kfac(2)=-1
                    kfac(3)=-1
                    do k=1,3
                     if(mbtest2(k).EQ.mblink2(k)) then
                      n=n+1
                      mypatt=IBSET(mypatt,k)
                      kfac(k)=1
                     endif
                    enddo
                    if(n.GE.min_sly .AND. nptr.LT.max_lnk) then
                     nptr=nptr+1
		     patt_lnk(nptr)=mypatt
	             my_tln(nptr)=it
	             my_rln(nptr)=ir
	             n=mblink3(2)
	             my_mom(nptr)=n
                     my_clk(1,nptr)=k11*kfac(1)
                     my_clk(2,nptr)=k12*kfac(2)
                     my_clk(3,nptr)=k13*kfac(3)
                     do i=1,3
                      if(kfac(i).LT.0) then
                       if(i.EQ.1) then
                        my_clk(i,nptr)=-mblink2(i)*2
                       else 
                        my_clk(i,nptr)=-mblink2(i)*3+1
                       endif
                      endif
                     enddo
                     my_icl(1,nptr)=icl11*kfac(1)
                     my_icl(2,nptr)=icl12*kfac(2)
                     my_icl(3,nptr)=icl13*kfac(3)
                    endif
                   endif
32              continue		!do isg11,isg12,isg13
31           continue			!do icl11,icl12,icl13
30          continue			!do it=1,lnk(ir,istax) resp. max_it

40	    continue
	    if(nptr.EQ.0) then		!track candidate?
C
C Store new link i.e new road (if training selected)
             if(ipar_trk_make_prlink.GT.0) then
              if(max_it.LT.max_lnk) then
               mbtest3(1) = 1
               mbtest3(2) = INT(100.*trkp(7,1,1))
               lnk(ir,istax) = lnk(ir,istax) +1
               link1(lnk(ir,istax),ir,istax) = mytest1
               link2(lnk(ir,istax),ir,istax) = mytest2
               link3(lnk(ir,istax),ir,istax) = mytest3
              endif
             endif
             goto 23
            endif
            if(nptr.LT.0) then
	     if(istax.EQ.2) goto 23
	     n=1
             patt_lnk(1)=14
	     goto 41
	    endif
            do i=1,6
	     do k=1,nptr
              if(patt_lnk(k).EQ.nsorted(i)) then
               n=k
               goto 41
              endif
             enddo
            enddo

C Store info for successful link
41          continue
            IF(ntr_link.GE.MAX_TRACKS) THEN 
              write(CRMESS,'(A,I3,A)') ' *** ntr_link >',MAX_TRACKS,' ***'
              call RecMes(CRNAME,'W',CRMESS)
             return
            else
             nlinks   = nlinks   + 1  ! No of links/sector
             ntr_link = ntr_link + 1  ! No of links/detector
             itr_level(ntr_link) = 1
             itr_sect(ntr_link)  = isec

	     if(my_mom(n).LT.5) my_mom(n)=5     !as start value for prfit
             trkp(7,1,ntr_link) =my_mom(n)/100.

C Saving the segment id's for the matched link
             lnk_segm(isl1,ntr_link) = k1
             lnk_segm(isl2,ntr_link) = k2
             lnk_segm(isl3,ntr_link) = k3
             lnk_segm(isl11,ntr_link) = my_clk(1,n)
             lnk_segm(isl12,ntr_link) = my_clk(2,n)
             lnk_segm(isl13,ntr_link) = my_clk(3,n)
C Saving the cluster id's for the matched link
             lnk_clust(isl1,ntr_link) = icl1
             lnk_clust(isl2,ntr_link) = icl2
             lnk_clust(isl3,ntr_link) = icl3
             lnk_clust(isl11,ntr_link) = my_icl(1,n)
             lnk_clust(isl12,ntr_link) = my_icl(2,n)
             lnk_clust(isl13,ntr_link) = my_icl(3,n)

             n_klnk=n_klnk+1
	     bcltest1(1,istax,n_klnk)=icl1
	     bcltest1(2,istax,n_klnk)=icl2
	     bcltest1(3,istax,n_klnk)=icl3
	     bcltest1(4,istax,n_klnk)=0
             if(nclust(isl11,isec).EQ.0) then
              bcltest1(1,istax2,n_klnk)='FF'X
             else
              bcltest1(1,istax2,n_klnk)=IABS(my_icl(1,n))
             endif
             if(nclust(isl12,isec).EQ.0) then
              bcltest1(2,istax2,n_klnk)='FF'X
             else
              bcltest1(2,istax2,n_klnk)=IABS(my_icl(2,n))
             endif
             if(nclust(isl13,isec).EQ.0) then
              bcltest1(3,istax2,n_klnk)='FF'X
             else
              bcltest1(3,istax2,n_klnk)=IABS(my_icl(3,n))
             endif
	     bcltest1(4,istax2,n_klnk)=0
            endif

C Update <p> for old link i.e. established road (if training selected)
            i=my_tln(n)
	    k=my_rln(n)
            mytest1=link1(i,k,istax)
            mytest2=link2(i,k,istax)
            mytest3=link3(i,k,istax)
            if(ipar_trk_make_prlink.GT.0) then
             n = mbtest3(1)
             pold = mbtest3(2)
             pnew = (pold*FLOAT(n) + 100.*trkp(7,1,1))/FLOAT(n + 1)
             mbtest3(1) = mbtest3(1) + 1
             mbtest3(2) = pnew
	     write(6,*)' new template for ir,n,pold,pnew',k,n,pold,pnew
             link3(i,k,istax)=mytest3
            endif
23         continue
22        continue
21       continue
13      continue
12     continue
11    continue
      if(istax.EQ.1) then
       isl2=4
       goto 10
      endif
      end

