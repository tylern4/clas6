C======================================================================
      SUBROUTINE trk_lramb(isec,trkno,beta,vtime,ntbt_seg,tbt_seg,success)
*_begin_doc
*  RCS ID string
*  $Id: trk_lramb.F,v 1.2 1998/10/15 14:31:48 staylor Exp $
*
*  Documentation for subroutine TRK_LRAMB
*
*  Purpose:  This routine calulate the left-right ambiguity and determine the 
*  -------   distance of closest aproach
*
*  Input Parameters:
*  -----------------
*     isec= Sector containing the track candidate
*     trkno= index to the hit based track 
*     beta= speed of particle in units of c
*     vtime= vertex time of the event
*     ntbt_seg= array containing
*     tbt_seg=array
*
*  Output Parameters:
*  ------------------
*     success= integer variable (0 or 1) indicating whether or not the l/r 
*              ambiguity resolution worked.
*  
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files :
*  ---------------------
*
#include "bcs.inc"
#include "anapatt.inc"
#include "dc_tcl.inc"
#include "trktcl_var.inc"
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
      INTEGER  isec, trkno, success
      real beta,vtime
      integer ntbt_seg(6),tbt_seg(12,30,6)
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: trk_lramb.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.2 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/10/15 14:31:48 $')
      PARAMETER ( CAUTHO = '$Author: staylor $')
      DATA CRCSID/
     1'$Id: trk_lramb.F,v 1.2 1998/10/15 14:31:48 staylor Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_DCAM')
      PARAMETER (CRAUTH='Francois H. ROUDOT')
*
*  Local User defined variables:
*  -----------------------------
C-working array:  xdigi(1,la)  - hit wire# "iw"
C-                xdigi(2,la)  - Raw TDC [counts]
C-                xdigi(3,la)  - Drift Time Td [ns] to the hit wire#
C-                xdigi(4,la)  - Drift Distance (measured) Dm [cm]
C-                xdigi(5,la)  - Sigma of Drift Distance [cm]
*
      INTEGER nlay
      PARAMETER (nlay = 6)
      INTEGER i,k, il, ilmin,is,icl,isg,ilnk,isgc, la,lab,lamin
      INTEGER improve, nsgc, n_seg, Level, itr_new,isgb
      INTEGER time_hits(NSGMX),time_hits_max,ihit
      REAL alfa, slopD, slopDbest, chib, sigm, cut
      REAL rltrk,rl2trk,sl2,r2bys2,r1bys2,invsl2,weight_slope 
      REAL xdigi(5,nlay,NSGMX), xdigib(5,nlay)
      INTEGER j

      INTEGER nsuper 
      DATA nsuper /6/          

      INTEGER LRflag
      LRflag = 0
      
      Level = TRK_LEVEL_lramb
      success=0

      trkp(8,1,trkno)=beta
      itr_sect(trkno)=isec
CCC   
C Resolve the L-R ambiguity
      n_seg   = 0
      itr_new = 1
      
      DO 60 is = 1,nsuper
         isgb    = 0
         improve = 0
         ilmin = (is-1)*nlay
         alfa = trk(10,ilmin+3,trkno)

         r2bys2 = 0.0
         r1bys2 = 0.0
         invsl2 = 0.0
         DO la = 1,nlay
            il = ilmin+la
            IF(is.NE.1 .OR. la.LE.4 )THEN
               rltrk = trk(8,il,trkno)-trk(8,ilmin+3,trkno)
               rl2trk = rltrk**2
               sl2 = trk(5,il,trkno)**2
               IF(sl2.GT.0)THEN
                  r2bys2 = r2bys2 + rl2trk/sl2
                  r1bys2 = r1bys2 + rltrk/sl2
                  invsl2 = invsl2 + 1.0/sl2
               ENDIF
            ENDIF
         ENDDO
         IF(invsl2.GT.0)THEN
            weight_slope = r2bys2 - r1bys2**2/invsl2
         ELSE
            weight_slope = 1.0
         ENDIF
C     
         nsgc = min(ntbt_seg(is),NSGMX)
         time_hits_max = 0

C     Loop over segments in a linked cluster
         DO 40 isg = 1,nsgc
            time_hits(isg) = 0
            
C     Store {iw,tdc} in "xdigi" array
            k = 1
            DO la = 1,nlay
               xdigi(1,la,isg) = tbt_seg(k,  isg,is) ! wire id         
               xdigi(2,la,isg) = tbt_seg(k+1,isg,is) ! raw tdc
               k = k + 2
               xdigi(3,la,isg)=0. !drift time
               xdigi(4,la,isg)=0. !drift distance
               xdigi(5,la,isg)=0. !sigma(drift distance)
            ENDDO
C Get drift time and the derived drift distance (using a funcion or a straight line (cf dc tcl variables)
            CALL trk_dtime(is,trkno,0,vtime,beta, xdigi(1,1,isg))
C
C How many hits pass t-to-d conversion
C
            DO la = 1,nlay
               if(xdigi(1,la,isg).gt.0)time_hits(isg)=time_hits(isg)+1
            ENDDO
            time_hits_max = max(time_hits(isg),time_hits_max)
            
 40      CONTINUE
         IF(time_hits_max.LT.3)GO TO 52
C
C The segments were sorted before we called trk_dtime, but since we may have
C reduced the number of hits on some segment, we better resort.
C Also, don't bother checking smaller segments if we have a good enough one
C
          DO 49 ihit = time_hits_max,3,-1
             IF(LRflag.EQ.1) GO TO 49
             DO 50 isgc = 1,nsgc
                IF(time_hits(isgc).NE.ihit) GO TO 50
                
C Resolve L-R ambiguity
                CALL ana_lramb_c(isec,is,xdigi(1,1,isgc),alfa,weight_slope,trkno,LRflag,ipar_trk_print(4))

                DO la = 1,nlay
                   xdigib(1,la) = xdigi(1,la,isgc)
                   xdigib(2,la) = xdigi(2,la,isgc)
                   xdigib(3,la) = xdigi(3,la,isgc)
                   xdigib(4,la) = xdigi(4,la,isgc)
                   xdigib(5,la) = xdigi(5,la,isgc)
                ENDDO
                isgb = isg
 50          CONTINUE           ! loop over segments in a SuperLayer
 49       CONTINUE 
          dca_segm(is,trkno) = isgb

C     Improve LR_amb by allowing to disable one layer in a track segment
          IF(LRflag.EQ.0) THEN
             DO 51 la = 1,nlay
                IF(xdigib(1,la).LT.0.) GO TO 51
                xdigib(1,la) = - xdigib(1,la) ! disable the layer temp
                CALL ana_lramb_c(isec,is,xdigib,alfa,weight_slope,trkno,LRflag,ipar_trk_print(4))

                xdigib(1,la) = ABS(xdigib(1,la)) ! re-enable the layer
 51          CONTINUE
          ENDIF                
 52       CONTINUE


C Check if L-R Ambiguity was successfully resolved
C If not, we have a crap cluster.  Trash it.
C (We might want to come back and deal with 2-hit segments in SL1 later.)

          IF(LRflag.EQ.0) THEN
             lnk_clust(is,trkno) = 0
             DO la = 1,nlay
                il = la + ilmin
                trk(12,il,trkno)= 1. ! Status notOK
             ENDDO
          ELSE  
             LRflag = 0
             n_seg = n_seg + 1

C     Store info in "trk( , , )" array needed for final fit (with DT)

             DO la = 1,nlay
               il = la + ilmin
               trk(3,il,trkno) = xdigib(3,la) ! Td [ns]
               trk(6,il,trkno) = beta ! Velocity = beta [c]
               trk(11,il,trkno) = 10*isec + Level ! Analysis Level & Sect
               trk(12,il,trkno)= 0. ! Status OK
               IF(trk(1,il,trkno).LE.0.) trk(12,il,trkno) =1. ! Status notOK
            ENDDO
         ENDIF
 60   CONTINUE                  ! End of loop over the superlayers

C Count tracks with resolved L-R Ambiguity in six SuperLayers
        IF(n_seg.GE.ipar_trk_minlramb) THEN
           success=1
           itr_level(trkno) = Level
        ENDIF

      RETURN
      END








