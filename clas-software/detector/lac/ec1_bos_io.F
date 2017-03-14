      SUBROUTINE Ec1_bos_io(ok)
c*************************************************************************
c* purpose : store info to EcEvu from BOS bank for each event
c* called by Ec1_evnt
c* output ok(Ec1_MAXblocks): T if there is data in LAEC
c*                           F if no data 
c*************************************************************************   
      IMPLICIT NONE
      SAVE
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c                                                                         rcs
c  RCS information:                                                       rcs
c                                                                         rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec1_bos_io.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.16 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2001/11/10 04:51:12 $')
      PARAMETER (CAUTHO = '$Author: avakian $')
      DATA CRCSID/ 
     1'$Id: ec1_bos_io.F,v 1.16 2001/11/10 04:51:12 avakian Exp $'
     2/ 
c  Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ec1_bos_io')                                   
      PARAMETER (CRAUTH='avakian')
c                                                                         rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c
#include "bcs.inc"
#include "ec1_control.inc"
#include "ec1general.PAR"
#include "ec1_calibr.inc"
#include "ec1_pedstl.inc"
#include "wcs.inc"
* external functions
* local varibles
      logical ok
      integer iblockold  ! bos
      integer MLINK,MBANK  ! bos
      REAL XD(3),XM(3),einsh,einlo,eoutsh,eoutlo
*
      call BDROP(IW,'EC1R')
      if(ec1Maxclust.gt.MaxClust) ec1Maxclust=MaxClust ! not more
      
      ok=.false.
      do j=1,2                  !	/* start loop on EC1/2*/
ccc   ind=MLINK(jw,"EC1P",j)     
ccc   print *, '---ec1p-',ind,rjw(ind+1),rjw(ind+2),rjw(ind+3),rjw(ind+4)
         ind=MLINK(iw,'EC1 ',j)
C     /*  cluster stuff */
         if(ind.gt.0) then      !		/* check the index in  "EC1 " */
            ok=.true.
            nw=iw(ind)
            ind=ind*2
            idig=(2*nw+1)/5
c     
            call VZERO(adc0,320) !  
            call VZERO(tdc0,320) !
            pmtfired=0

            do i=1,idig         !; i++) /* start loop on hits */
c     call ec1_getstrip(iw16(ind+1),iblock,istack)
c     call C routine
               if(iw16(ind+1).lt.256) goto 999 ! garbage in BOS
               call ec1_getblock(iw16(ind+1),iblock,istack)
               IF( (iblock.gt.4).or.(iblock.lt.1).or.
     1              (istack.lt.1).or.(istack.gt.(24+16*mod(iblock+1,2)))) THEN
                  
                  WRITE(CRMESS,11) iblock,istack
 11               FORMAT('Wrong block number, block,stack=',I6,I6)
                  CALL RECMES(CRNAME,'F',CRMESS)
                  return
               ENDIF
               
               iblockold=iblock-(-1)**(mod(iblock,2))
               
               adc0(istack,iblockold,1)=ec1_cala1l(istack,iblock,j)
     1              *(iw16(ind+3)-ec1_pedl(istack,iblock,j))
     2              +ec1_cala2l(istack,iblock,j)
               adc0(istack,iblockold,2)=ec1_cala1r(istack,iblock,j)
     1              *(iw16(ind+5)-ec1_pedr(istack,iblock,j))
     2              +ec1_cala2r(istack,iblock,j)
               tdc0(istack,iblockold,1)=ec1_calt1l(istack,iblock,j)*iw16(ind+2)
     1              +ec1_calt2l(istack,iblock,j)
     2         -ec1_caltw_al(istack,iblock,j)*exp((iw16(ind+3)
     3         -ec1_pedl(istack,iblock,j))/ec1_caltw_bl(istack,iblock,j))
               tdc0(istack,iblockold,2)=ec1_calt1r(istack,iblock,j)*iw16(ind+4)
     1              +ec1_calt2r(istack,iblock,j)
     2         -ec1_caltw_ar(istack,iblock,j)*exp((iw16(ind+5)
     3         -ec1_pedr(istack,iblock,j))/ec1_caltw_br(istack,iblock,j))
 999           continue
               ind=ind+5
            enddo
C     I-block
            nlines=40
            nblock=0
            call clusindex(pmtfired,ncluster1,nlines,nblock,adc0,
     +           indi1,depin1l,depin1r,stackm1l,stackm1r)
C     II-block
            nlines=24
            nblock=1
            call clusindex(pmtfired,ncluster2,nlines,nblock,adc0,
     +           indi2,depin2l,depin2r,stackm2l,stackm2r)
            
C     III-block
            nlines=40
            nblock=2
            call clusindex(pmtfired,ncluster3,nlines,nblock,adc0,
     +           indi3,depin3l,depin3r,stackm3l,stackm3r)
C     IV-block
            nlines=24
            nblock=3
            call clusindex(pmtfired,ncluster4,nlines,nblock,adc0,
     +           indi4,depin4l,depin4r,stackm4l,stackm4r)
C     number of clusters in I and II is the same
            if((ncluster1.gt.0).and.(ncluster1.eq.ncluster2)) then
               call ec1_maxi(ncluster1,indi1,indadc1)
               do i=1,ncluster2
                  ivtdc2(i)=tdc0(indi2(i),2,1)-tdc0(indi2(i),2,2)
               enddo
               call ec1_maxi(ncluster2,ivtdc2,indtdc2)
               ec1_stat_in=1000
            elseif (ncluster1.eq.ncluster2-1) then
               call ec1_maxi(ncluster1,indi1,indadc1)
               do i=1,ncluster2
                  ivtdc2(i)=tdc0(indi2(i),2,1)-tdc0(indi2(i),2,2)
               enddo
               call ec1_maxi(ncluster2,ivtdc2,indtdc2)
               call ec1_sametdc(ncluster2,ivtdc2,indtdc2,ltdc1,ltdc2)
               if((ltdc1*ltdc2).gt.0) then
C     
C     overlapped stacks resolved
                  do i=ncluster2,ltdc2+1,-1
                     indadc1(i)=indadc1(i-1)
                  enddo
                  indadc1(ltdc2)=indadc1(ltdc1)
                  tdepl=depin2l(indtdc2(ltdc1))+depin2l(indtdc2(ltdc2))
                  tdepr=depin2r(indtdc2(ltdc1))+depin2r(indtdc2(ltdc2))
                  
                  do i=1,ncluster2
                     if((i.eq.ltdc1).or.(i.eq.ltdc2)) then 
                        deptempl(i) =depin2l(indtdc2(i))*depin1l(indadc1(i))/tdepl
                        deptempr(i) =depin2r(indtdc2(i))*depin1r(indadc1(i))/tdepr
                     else
                        deptempl(i) =depin1l(indadc1(i))
                        deptempr(i) =depin1r(indadc1(i))
                     endif
                  enddo
                  ec1_stat_in= 100
               else
C     
C     single stack candidate ?neutron
C     
                  ec1_stat_in= 10
               endif
               
            elseif (ncluster1.eq.ncluster2+1) then
               call ec1_maxi(ncluster2,indi2,indadc2)
               do i=1,ncluster1
                  ivtdc1(i)=tdc0(indi1(i),1,1)-tdc0(indi1(i),1,2)
               enddo
               call ec1_maxi(ncluster1,ivtdc1,indtdc1)
               call ec1_sametdc(ncluster1,ivtdc1,indtdc1,ltdc1,ltdc2)
               if((ltdc1*ltdc2).gt.0) then
C     
C     overlapped stacks resolved
                  do i=ncluster1,ltdc2+1,-1
                     indadc2(i)=indadc2(i-1)
                  enddo
                  indadc2(ltdc2)=indadc2(ltdc1)
                  tdepl=depin1l(indtdc1(ltdc1))+depin1l(indtdc1(ltdc2))
                  tdepr=depin1r(indtdc1(ltdc1))+depin1r(indtdc1(ltdc2))
                  
                  do i=1,ncluster1
                     if((i.eq.ltdc1).or.(i.eq.ltdc2)) then 
                        deptempl(i) =depin1l(indtdc1(i))*depin2l(indadc2(i))/tdepl
                        deptempr(i) =depin1r(indtdc1(i))*depin2r(indadc2(i))/tdepr
                     else
                        deptempl(i) =depin2l(indadc2(i))
                        deptempr(i) =depin2r(indadc2(i))
                     endif
                  enddo
                  
                  ec1_stat_in= 100
               else
C     
C     single stack candidate ?neutron
C     
                  ec1_stat_in= 10
               endif
c     
            else
C     
C     case with Ncl1 >> Ncl2 or Ncl1 << Ncl2 not implemented yet
C     
               ec1_stat_in=0
            endif
C     
C     number of clusters in III and IV is the same
            if((ncluster3.gt.0).and.(ncluster3.eq.ncluster4)) then
               call ec1_maxi(ncluster3,indi3,indadc3)
               do i=1,ncluster4
                  ivtdc4(i)=tdc0(indi4(i),4,1)-tdc0(indi4(i),4,2)
               enddo
               call ec1_maxi(ncluster4,ivtdc4,indtdc4)
               ec1_stat_out=1000
            elseif (ncluster3.gt.0.or.ncluster4.gt.0) then
               ec1_stat_out=10
            else
               ec1_stat_out=0
            endif
            ec1_stat_rec=ec1_stat_out+ec1_stat_in
C     writing the identified clusters.
C     
            if(ec1_stat_in.ge.100) then ! n1~=n2
               Nclust=max(ncluster1,ncluster2)
               ind = MBANK(IW,'EC1R',j,Npar,Nclust)
               IF(ind.LE.0) THEN
                  WRITE(CRMESS,10) ind
 10               FORMAT('Error in MBANK, ind=',I6)
                  CALL RECMES(CRNAME,'F',CRMESS)
                  return
               ENDIF
c     
               do i=1,Nclust
                  index = ind + (i-1)*Npar 
                  if(ncluster1.le.ncluster2)   then 
                     ix_in=indadc1(i)
                     iy_in=indtdc2(i)
                     if(ncluster1.eq.ncluster2)   then
                        RW(index +29) = depin1l(ix_in)*0.001 !MeV 2 GeV         
                        RW(index +30) = depin1r(ix_in)*0.001 !MeV 2 GeV           
                     else
                        RW(index +29) = deptempl(i)*0.001 !MeV 2 GeV           
                        RW(index +30) = deptempr(i) *0.001 !MeV 2 GeV            
                     endif
                     RW(index +31) = depin2l(iy_in)*0.001 !MeV 2 GeV
                     RW(index +32) = depin2r(iy_in)*0.001 !MeV 2 GeV
                  else
                     ix_in=indtdc1(i)
                     iy_in=indadc2(i)
                     RW(index +29) = depin1l(ix_in)*0.001 !MeV 2 GeV         
                     RW(index +30) = depin1r(ix_in) *0.001 !MeV 2 GeV          
                     RW(index +31) = deptempl(i) *0.001 !MeV 2 GeV            
                     RW(index +32) = deptempr(i)*0.001 !MeV 2 GeV            
                  endif
                  RW(index +17) = stackm1l(ix_in)             
                  RW(index +18) = stackm1r(ix_in)            
                  RW(index +19) = stackm2l(iy_in)          
                  RW(index +20) = stackm2r(iy_in)          
                  ix_1 =indi1(ix_in)             
                  iy_2 =indi2(iy_in)          
c     
c     get the local position
                  if((RW(index +29)+RW(index +30)).gt.0.0 
     +              .and.(RW(index +31)+RW(index +32)).gt.0.0) then
                     
                     xd(1)=(ix_1-20.5+(RW(index+17)*RW(index +29)+RW(index +18)*RW(index +30))/
     +                    (RW(index +29)+RW(index +30)))*10.21 ! the short strip length
                     xd(2)=(iy_2-12.5+(RW(index+19)*RW(index +31)+RW(index +20)*RW(index +32))/
     +                    (RW(index +31)+RW(index +32)))*9.23 ! the long strip length
                     xd(3)=-17.95 ! in EC1P -17.95
                     call EC1ROT(XD,XM,j)
                     RW(index +13) = xm(1)           
                     RW(index +14) = xm(2)
                     RW(index +48)=  xm(3)        
                  else
                     RW(index +13) = 0.0           
                     RW(index +14) = 0.0
                     RW(index +48)=  0.0        
                  endif
c     
                  IW(index +25) =indi1(ix_in)           
                  IW(index +26) =indi2(iy_in)            
                  RW(index +37) =(tdc0(ix_1,1,1)+tdc0(ix_1,1,2))/2
                  RW(index +38) =tdc0(ix_1,1,1)-tdc0(ix_1,1,2)
                  RW(index +39) =(tdc0(iy_2,2,1)+tdc0(iy_2,2,2))/2
                  RW(index +40) =tdc0(iy_2,2,1)-tdc0(iy_2,2,2)
C     looking for clusters in outer part
                  if(ncluster3.gt.0) then
                     call ec1_findpair(iupd1,ix_1,ncluster3,indshrt,indi3)
                     if(iupd1.gt.0) then
                        ix_out=indshrt(iupd1)
                        ix_3=indi3(ix_out)
                        IW(index +27) = ix_3             
                        RW(index +21) = stackm3l(ix_out) ! x_out_l
                        RW(index +22) = stackm3r(ix_out) ! x_out_r
                        RW(index +33) = depin3l(ix_out)*0.001 !MeV 2 GeV
                        RW(index +34) = depin3r(ix_out)*0.001 !MeV 2 GeV
                        RW(index +41) =(tdc0(ix_3,3,1)+tdc0(ix_3,3,2))/2 
                        RW(index +42) =tdc0(ix_3,3,1)-tdc0(ix_3,3,2)
                     endif
                  endif
                  if(ncluster4.gt.0) then
                     call ec1_findpair(iupd2,iy_2,ncluster4,indlong,indi4)
                     if(iupd2.gt.0) then
                        iy_out=indlong(iupd2)
                        iy_4=indi4(iy_out)
                        IW(index +28) =iy_4            
                        RW(index +23) = stackm4l(iy_out)
                        RW(index +24) = stackm4r(iy_out)
                        RW(index +35) = depin4l(iy_out)*0.001 !MeV 2 GeV
                        RW(index +36) = depin4r(iy_out)*0.001 !MeV 2 GeV
                        RW(index +43) =(tdc0(iy_4,4,1)+tdc0(iy_4,4,2))/2
                        RW(index +44) =tdc0(iy_4,4,1)-tdc0(iy_4,4,2)
                     endif
                  endif
c     
                  if(iupd2.gt.0.and.iupd1.gt.0
     +                 .and.(RW(index +33)+RW(index +34)).gt.0.0
     +                 .and.(RW(index +35)+RW(index +36)).gt.0.0) then
                     xd(1)=(ix_3-20.5+(RW(index+21)*RW(index +33)+RW(index +22)*RW(index +34))/
     +                    (RW(index +33)+RW(index +34)))*10.48 ! the short strip length
                     xd(2)=(iy_4-12.5+(RW(index+23)*RW(index +35)+RW(index +24)*RW(index +36))/
     +                    (RW(index +35)+RW(index +36)))*9.48 ! the long strip length
                     xd(3)=5.95 ! in EC1P -17.95
                     call EC1ROT(XD,XM,j)
                     RW(index +15) = xm(1)           
                     RW(index +16) = xm(2)
                     RW(index +49)=  xm(3)        
                  else
                     RW(index +15) = 0.0           
                     RW(index +16) = 0.0
                     RW(index +49)=  0.0        
                  endif
c     
      
c     
c     constans hardcoded. must be changed
                  ec1_alphashdo=1.0+0.0001*(12.5-iy_2)**2
                  ec1_alphalodo=1.0+0.0002*(20.5-ix_1)**2
                  einsh=RW(index +29)*RW(index +30)
                  if(einsh.lt.0.)einsh=0.
                  einlo=RW(index +31)*RW(index +32)
                  if(einlo.lt.0.)einlo=0.
                  eoutsh=RW(index +33)*RW(index +34)
                  if(eoutsh.lt.0.)eoutsh=0.
                  eoutlo=RW(index +35)*RW(index +36)
                  if(eoutlo.lt.0.)eoutlo=0.
                  RW(index +11)=sqrt(einsh)/0.758/ec1_alphashdo
     +                 +               sqrt(einlo)/0.598/ec1_alphalodo
                  RW(index +1)=sqrt(eoutsh)/0.746/ec1_alphashdo
     +                 +               sqrt(eoutlo)/0.583/ec1_alphalodo
     +                 +               RW(index +11)
                  RW(index +11)=RW(index +11)
                  RW(index +1)=RW(index +1)
                  RW(index +2)= 0.03*RW(index +1) ! to be defined
                  RW(index +3)= MIN(RW(index +37),RW(index +39))
                  RW(index +4)= 0.0 ! to be defined
                  RW(index +5)=RW(index +13) 
                  RW(index +6)=RW(index +14)
                  RW(index +7)=RW(index +48)
                  RW(index +8)= 2.5
                  RW(index +9)= 2.5
                  RW(index +10)= 2.5 ! to be defined
                  RW(index +12)= MIN(RW(index +37),RW(index +39))
                  
                  IW(index +45) = j
                  IW(index +46) = 1000*ncluster1+100*ncluster2+10*ncluster3+ncluster4
                  IW(index +47) = pmtfired  
                  if((iupd1*iupd2).gt.0) then
                     IW(index +50) = 0 ! istat
                  else
                     IW(index +50) = 1 ! istat
                  endif
               enddo
            else     
C     mixed case
C     there is something to look for in more detailed analysis (neutron?)
c               if(Ec1_reco_type.eq.2.and.ec1_stat_out.ge.10) then ! neutron in outer?
               if(ec1_stat_out.eq.1000) then ! neutron in outer?
                  nclust=1
                  ind = MBANK(IW,'EC1R',j,Npar,Nclust)
                  IF(ind.LE.0) THEN
                     WRITE(CRMESS,10) ind
                     CALL RECMES(CRNAME,'F',CRMESS)
                     return
                  ENDIF
                  
                  index=ind
                        iy_out=1  ! code valid for 1 cluster in outer block
                        ix_out=1
                        ix_3=indi3(ix_out)
                        IW(index +27) = ix_3             
                        RW(index +21) = stackm3l(ix_out) ! x_out_l
                        RW(index +22) = stackm3r(ix_out) ! x_out_r
                        RW(index +33) = depin3l(ix_out)*0.001 !MeV 2 GeV
                        RW(index +34) = depin3r(ix_out)*0.001 !MeV 2 GeV
                        RW(index +41) =(tdc0(ix_3,3,1)+tdc0(ix_3,3,2))/2 
                        RW(index +42) =tdc0(ix_3,3,1)-tdc0(ix_3,3,2)

                        iy_4=indi4(iy_out)
                        IW(index +28) =iy_4            
                        RW(index +23) = stackm4l(iy_out)
                        RW(index +24) = stackm4r(iy_out)
                        RW(index +35) = depin4l(iy_out)*0.001 !MeV 2 GeV
                        RW(index +36) = depin4r(iy_out)*0.001 !MeV 2 GeV
                        RW(index +43) =(tdc0(iy_4,4,1)+tdc0(iy_4,4,2))/2
                        RW(index +44) =tdc0(iy_4,4,1)-tdc0(iy_4,4,2)

                  if((RW(index +33)+RW(index +34)).gt.0.0
     +                 .and.(RW(index +35)+RW(index +36)).gt.0.0) then
                     xd(1)=(ix_3-20.5+(RW(index+21)*RW(index +33)+RW(index +22)*RW(index +34))/
     +                    (RW(index +33)+RW(index +34)))*10.48 ! the short strip length
                     xd(2)=(iy_4-12.5+(RW(index+23)*RW(index +35)+RW(index +24)*RW(index +36))/
     +                    (RW(index +35)+RW(index +36)))*9.48 ! the long strip length
                     xd(3)=5.95 ! in EC1P -17.95
                     call EC1ROT(XD,XM,j)
                     RW(index +15) = xm(1)           
                     RW(index +16) = xm(2)
                     RW(index +49)=  xm(3)        
                  else
                     RW(index +15) = 0.0           
                     RW(index +16) = 0.0
                     RW(index +49)=  0.0        
                  endif

                  ec1_alphashdo=1.0+0.0001*(12.5-iy_4)**2
                  ec1_alphalodo=1.0+0.0002*(20.5-ix_3)**2
                  eoutsh=RW(index +33)*RW(index +34)
                  if(eoutsh.lt.0.)eoutsh=0.
                  eoutlo=RW(index +35)*RW(index +36)
                  if(eoutlo.lt.0.)eoutlo=0.
                  RW(index +11)=0.0
                  RW(index +1)=sqrt(eoutsh)/0.746/ec1_alphashdo
     +                 +               sqrt(eoutlo)/0.583/ec1_alphalodo
                  RW(index +2)= 0.03*RW(index +1) ! to be defined
                  RW(index +3)= MIN(RW(index +41),RW(index +43))
                  RW(index +4)= 0.0 ! to be defined
                  RW(index +5)=RW(index +15) 
                  RW(index +6)=RW(index +16)
                  RW(index +7)=RW(index +49)
                  RW(index +8)= 2.5
                  RW(index +9)= 2.5
                  RW(index +10)= 2.5 ! to be defined
                  RW(index +12)= 0.0
                  
                  IW(index +45) = j
                  IW(index +46) = 1000*ncluster1+100*ncluster2+10*ncluster3+ncluster4
                  IW(index +47) = pmtfired  

                  IW(index +50) = 2
               endif            ! ec1_reco_type
            endif
C     if(Ec1_reco_type.eq.1) then ! only defined clusters
C     endif     ! ec1_reco_type
C     reading second LAC block
         endif
      enddo
      return
      end
      
