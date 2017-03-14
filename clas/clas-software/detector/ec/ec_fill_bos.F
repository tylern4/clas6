      SUBROUTINE EC_FILL_BOS(LOK)
*----------------------------------------------------------------------
*     
*     Purpose and Methods : fill BOS bank ECRB
*     
*     Inputs : LOK - if this sector active
*     
*     Created  16-JUN-1996   Serguei Boiarinov from Kevin's ecfit_hits_showers
*     Modified: September 1998, W. Brooks - indented code, added ECPC bank
*     Modified: May-11-1999, Ky Kim - moved "Timing Part" into ec_time.F which calculates
*               the EC hit time (Commented out by "CTime").
*     
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     rcs
c     RCS information:                                                       rcs
c     rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: ec_fill_bos.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.21 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2008/03/16 04:33:14 $')
      PARAMETER (CAUTHO = '$Author: fklein $')
      DATA CRCSID/ 
     1   '$Id: ec_fill_bos.F,v 1.21 2008/03/16 04:33:14 fklein Exp $'
     2   / 
c     Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ec_fill_bos')                                   
      PARAMETER (CRAUTH='Serguei Boiarinov')
c     rcs
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     
#include "bcs.inc"
#include "Ec_general.PAR"
#include "EcFit_general.CMN"
#include "EcFit_edge.CMN"
#include "ec_control.inc"
#include "ec_peak.inc"
#include "ec_strip.inc"
#include "ec_rawdata.inc"
c     
      logical LOK(6)
      integer s,n,ind,index,mbank,bank_number,axis1_3
c     step
      INTEGER axis,nrow_c
      INTEGER indd,indo
      INTEGER indexd,indexo
      INTEGER Nparsd
      INTEGER Layer,sct
      INTEGER Hit,ist
      INTEGER Npars
      INTEGER Nrows
      INTEGER Nparso
      INTEGER Nrowso
      INTEGER halfstrip, two2ist
      INTEGER ncol_c            ! number of columns in ECPC bank
      PARAMETER (Npars=38)
      PARAMETER (Nparsd=9)
      PARAMETER (Nparso=7)
c      PARAMETER (halfstrip=54)
      PARAMETER (halfstrip=18)
      PARAMETER (ncol_c=3)      ! 3 columns: strip id, tdc, and adc
      INTEGER sector_layer
      INTEGER Nhit_hit

      REAL EC_TIME              ! function subprogram which calculates EC hit time

*----------------------------------------------------------------------
      
c     step ==> create ECHB and ECPi banks
      Nrows=0
      Nrowso=0
      n=0
      DO s=1,MAXsectors         ! sector loop
        IF(LOK(s)) THEN
          do Layer=whole,cover
            Nrows=Nrows+EcFit_Nhit(Layer,s)
            if(Layer.ne.whole)Nrowso=Nrowso+EcFit_Nhit(Layer,s)
          enddo
        ENDIF
      ENDDO
      ind = MBANK(IW,'ECHB',0,Npars,Nrows)
      indd = MBANK(IW,'ECPI',0,Nparsd,Nrows)
      indo = MBANK(IW,'ECPO',0,Nparso,Nrowso)
      IF(ind.LE.0) THEN
        WRITE(CRMESS,10) ind
 10     FORMAT('Error in MBANK, ind=',I6)
        CALL RECMES(CRNAME,'F',CRMESS)
      ENDIF
      IF(indd.LE.0) THEN
        WRITE(CRMESS,10) indd
        CALL RECMES(CRNAME,'F',CRMESS)
      ENDIF
      IF(indo.LE.0) THEN
        WRITE(CRMESS,10) indo
        CALL RECMES(CRNAME,'F',CRMESS)
      ENDIF
      n = 0
      indexo = indo 
      DO sct=1,maxsectors
        IF(LOK(sct))then
          Do layer=whole,cover
            If(ecfit_nhit(layer,sct).gt.0)Then
              sector_layer=sct*100+layer
              do Hit=1,EcFit_Nhit(Layer,sct)
                Nhit_hit=EcFit_Nhit(Layer,sct)*100+hit
                index = ind + n*Npars
                IW(index + 1) = sector_layer ! sector number
                RW(index + 2) = EcFit_hit(ENERGY,Hit,Layer,sct) ! 
                RW(index + 3) = EcFit_hit(WIDTH,Hit,Layer,sct) ! 
                RW(index + 4) = EcFit_hit(TIME,Hit,Layer,sct) ! 
                RW(index + 6) = EcFit_hit(Ec_i,Hit,Layer,sct) ! 
                RW(index + 7) = EcFit_hit(Ec_j,Hit,Layer,sct) ! 
                RW(index + 8) = EcFit_hit(Ec_di,Hit,Layer,sct) ! 
                RW(index + 9) = EcFit_hit(Ec_dj,Hit,Layer,sct) ! 
                RW(index +10) = EcFit_hit(Ec_x,Hit,Layer,sct) ! 
                RW(index +11) = EcFit_hit(Ec_y,Hit,Layer,sct) ! 
                RW(index +12) = EcFit_hit(Ec_z,Hit,Layer,sct) ! 
                RW(index +13) = EcFit_hit(Ec_dx,Hit,Layer,sct) ! 
                RW(index +14) = EcFit_hit(Ec_dy,Hit,Layer,sct) ! 
                RW(index +15) = EcFit_hit(Ec_dz,Hit,Layer,sct) ! 
                RW(index +16) = Ec_peak(mom2,Hit,Ec_U,Layer,sct) ! 
                RW(index +17) = Ec_peak(mom2,Hit,Ec_V,Layer,sct) ! 
                RW(index +18) = Ec_peak(mom2,Hit,Ec_W,Layer,sct) ! 
                RW(index +19) = Ec_peak(mom3,Hit,Ec_U,Layer,sct) ! 
                RW(index +20) = Ec_peak(mom3,Hit,Ec_V,Layer,sct) ! 
                RW(index +21) = Ec_peak(mom3,Hit,Ec_W,Layer,sct) !
                RW(index +22) = Ec_peak(mom4,Hit,Ec_U,Layer,sct) ! 
                RW(index +23) = Ec_peak(mom4,Hit,Ec_V,Layer,sct) ! 
                RW(index +24) = Ec_peak(mom4,Hit,Ec_W,Layer,sct) !
                RW(index +25) = Ec_peak(dist,Hit,Ec_U,Layer,sct) ! 
                RW(index +26) = Ec_peak(dist,Hit,Ec_V,Layer,sct) ! 
                RW(index +27) = Ec_peak(dist,Hit,Ec_W,Layer,sct) !
                RW(index +28) = ppath(Hit,Ec_U,Layer,sct) !
                RW(index +29) = ppath(Hit,Ec_V,Layer,sct) !
                RW(index +30) = ppath(Hit,Ec_W,Layer,sct) !
                IW(index +31) = nstripc(hit,Ec_U,Layer,sct) 
                IW(index +32) = nstripc(hit,Ec_V,Layer,sct) 
                IW(index +33) = nstripc(hit,Ec_W,Layer,sct) 
                IF ( layer.eq.whole ) THEN
                  IW(index +34) = matched(layer,inner,hit,sct)
                  RW(index +35) = c2match(layer,inner,hit,sct)
                  IW(index +36) = matched(layer,outer,hit,sct)
                  RW(index +37) = c2match(layer,outer,hit,sct)
                ELSE IF ( layer.eq.inner ) THEN
                  IW(index +34) = matched(layer,whole,hit,sct)
                  RW(index +35) = c2match(layer,whole,hit,sct)
                  IW(index +36) = matched(layer,outer,hit,sct)
                  RW(index +37) = c2match(layer,outer,hit,sct)
                ELSE IF ( layer.eq.outer ) THEN
                  IW(index +34) = matched(layer,whole,hit,sct)
                  RW(index +35) = c2match(layer,whole,hit,sct)
                  IW(index +36) = matched(layer,inner,hit,sct)
                  RW(index +37) = c2match(layer,inner,hit,sct)
                END IF
                IW(index +38) = Nhit_hit ! status
cStep If some one wonts west his time try this.
c                if(.NOT.Time_def)then
CTime     Now fill the hit time by calling "ec_time" function routine
CTime     No timing error is currently available which would be useful to isolate
CTime     a neutron hit from a photon hit (or something else) based on the hit time
CTime     even though the error estimation is expected to be so complicated.
c                   RW(index + 4)=ec_time(layer,hit,sct)
c                endif
cStep
* Fill ECPI and ECPO banks
                indexd = indd + n*Nparsd
                IW(indexd + 1) = sct ! sector number
                IW(indexd + 2) = Layer ! Layer number
                IW(indexd + 3) = Nhit_hit ! number of hits and hit ID
                RW(indexd + 4) = ifec(Hit,Layer,sct) ! 
                RW(indexd + 5) = jfec(Hit,Layer,sct) ! 
                RW(indexd + 6) = difec(Hit,Layer,sct) ! 
                RW(indexd + 7) = djfec(Hit,Layer,sct) ! 
                RW(indexd + 8) = EcFit_hit(width,Hit,Layer,sct) ! 
                RW(indexd + 9) = EcFit_hit(ENERGY,Hit,Layer,sct) ! 
c     -
                IF ( layer.ne.whole ) THEN
                  IW(indexo+7)=1000*sct+100*(Layer-9)+hit
                  IW(indexo+1)=0
                  IW(indexo+2)=0
                  IW(indexo+3)=0
                  IW(indexo+4)=0
                  IW(indexo+5)=0
                  IW(indexo+6)=0
                  Do ist=1,MAXecstrips
                    if(ist.le.halfstrip)then
                      two2ist=2**(ist-1)
                      IW(indexo+1)=IW(indexo+1)+INT(stripc(ist,hit,
     $                   Ec_U,Layer,sct)*two2ist)
                      IW(indexo+3)=IW(indexo+2)+INT(stripc(ist,hit,
     $                   Ec_V,Layer,sct)*two2ist)
                      IW(indexo+5)=IW(indexo+3)+INT(stripc(ist,hit,
     $                   Ec_W,Layer,sct)*two2ist)
                    else 
                      two2ist=2**(ist-19)
                      IW(indexo+2)=IW(indexo+2)+INT(stripc(ist,hit,
     $                   Ec_U,Layer,sct)*two2ist)
                      IW(indexo+4)=IW(indexo+4)+INT(stripc(ist,hit,
     $                   Ec_V,Layer,sct)*two2ist)
                      IW(indexo+6)=IW(indexo+6)+INT(stripc(ist,hit,
     $                   Ec_W,Layer,sct)*two2ist)
                    endif
                  EndDo
                  indexo = indexo + Nparso
                ENDIF
*     
                n = n + 1
              enddo             ! end of loop over hits
            endif 
          enddo
        ENDIF 
      ENDDO
            
*     Fill the EC Particle Calibration bank
*
*     This bank has the hit id (sector*100+layer*10+hit) as its bank number.
*     In this context, 'layer' is equal to 1 if inner and 2 if outer.

      do sct=1,maxsectors
        if (LOK(sct)) then            
          do layer=inner,outer  ! just do inner and outer
            do hit=1,EcFit_Nhit(Layer,sct)
              bank_number = sct*100 + (layer-inner+1)*10 + hit                
              nrow_c = ntstripc(hit,layer,sct) ! total number of strips in the hit
              index = MBANK(IW,'ECPC',bank_number,ncol_c,nrow_c) ! make the bank
              do axis=Ec_U,Ec_W
                axis1_3 = axis - Ec_U + 1 ! ranges from 1 to 3
                do ist=1,maxecstrips
                  if (stripc(ist,hit,axis,layer,sct).gt.0.9) then ! this strip was in the hit
     
                    IW(index + 1) =  axis1_3 * 100 + ist ! extended strip ID
                    
                    if(ec_raw_tdcs(ist,axis,layer,sct).lt.-100.)then ! set these to -100
                      RW(index + 2) = -100.
c upper limit for dTDC was changed from 4096 to 9000 (for multihit pipeline TDCs)
                    elseif(ec_raw_tdcs(ist,axis,layer,sct).gt.9000.)then 
                      RW(index + 2) = 9000.
                    else
                      RW(index + 2) = ec_raw_tdcs(ist,axis,layer,sct) ! value is ok, store it
                    endif
                    
                    if(ec_raw_adcs(ist,axis,layer,sct).lt.-100.)then ! set these to -100
                      RW(index + 3) = -100.
                    elseif(ec_raw_adcs(ist,axis,layer,sct).gt.9999.)then ! set these to 10000
                      RW(index + 3) = 10000.
                    else        ! value is ok, store it
                      RW(index + 3) = ec_raw_adcs(ist,axis,layer,sct)
                    endif
                    
                    index = index + 3 ! ready for next strip
                    
                  endif
                enddo
              enddo
            enddo
          enddo
        endif
      enddo
*     
c     Serguei      ENDIF
*     
      RETURN
      END










