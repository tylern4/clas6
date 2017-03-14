      SUBROUTINE EcCal_Strip(sector)
*----------------------------------------------------------------------
*     -
*     -  Purpose and Methods : change TDC,ADC to energy and time
*     -                         EcEvu-->EcStrip
*     -
*     -  Inputs: sector       - CLAS sector (1-6)
*     -
*     -
*     -  Created  24-April-1995   Z.Li
*     - 
*     -  Modified 27-January-1998 W. Brooks - flag bad tdcs
*     -  Modified September-1998  W. Brooks - fill ec_raw_adcs and ec_raw_tdcs
*     -  Updated  May 1999        Ky Kim    - for the new EC timing
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     rcs
c     RCS information:                                                       rcs
c     rcs
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: eccal_strip.F,v $') 
      PARAMETER (CREVIS = '$Revision: 1.11 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2006/06/23 20:01:07 $')
      PARAMETER (CAUTHO = '$Author: stepanya $')
      DATA CRCSID/ 
     1     '$Id: eccal_strip.F,v 1.11 2006/06/23 20:01:07 stepanya Exp $'
     2     / 
c     Module information: 
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='eccal_strip')                                   
      PARAMETER (CRAUTH='Zhujun Li')
c     rcs
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rcs rsc rcs rcs rcs
c     
#include "Ec_general.PAR"
#include "EcStrip.CMN"
#include "EcEvu.CMN"
#include "EcCal.CMN"
#include "ec_strip.inc"
#include "clasmdl.inc"
#include "ec_rawdata.inc"
#include "ec_control.inc"
*     
      integer sector
*     
      integer id,axis,layer,s,ns,idf,nsf
      real dADC,dTDC
      INTEGER Nreported
      INTEGER zerostrips
*     
*----------------------------------------------------------------------
*     
      s=sector
      DO layer= INNER,Cover
         ntstripc(0,layer,s) = 0
         esum(layer)=0.
         Do axis= Ec_U,Ec_W
c            print *,s,layer,axis,EcEvu_Nreported(axis,layer,s)
            nstripc(0,axis,layer,s)=EcEvu_Nreported(axis,layer,s)
            ntstripc(0,layer,s)=ntstripc(0,layer,s)
     +           +nstripc(0,axis,layer,s)
            Ec_StripN(axis,layer,s)=EcEvu_Nreported(axis,layer,s)         
            do id=1,Ec_StripN(axis,layer,s)
               ns=EcEvu_reportedID(id,axis,layer,s)
               Ec_StripID(id,axis,layer,s)=ns
               dADC= EcEvu_strip(ADC,ns,axis,layer,s)
               ec_raw_adcs(ns,axis,layer,s)=dADC-EcCal_Eo(ns,axis,layer,s) !save adc minus pedestal
               Ec_Strip(energy,ns,axis,layer,s) ! convert to energy
     &              =(dADC-EcCal_Eo(ns,axis,layer,s))
     &              *EcCal_Ech(ns,axis,layer,s)
               if(Ec_Strip(energy,ns,axis,layer,s).lt.0.)
     &              Ec_Strip(energy,ns,axis,layer,s)=0.
*     
               dTDC= EcEvu_strip(TDC,ns,axis,layer,s)
               ec_raw_tdcs(ns,axis,layer,s)=dTDC !save raw tdc value
               if(dTDC.gt.0.0.and.dTDC.lt.9000.0.and.ec_raw_adcs(ns,axis,layer,s).gt.0)then !filter out bad tdcs
c     upper limit for dTDC was changed from 4096 to 9000
c     because of the new multihit pipeline TDC --- H.S. Jo November 2005
                  Ec_Strip(time,ns,axis,layer,s) ! convert to time
     &                 =dTDC*EcCal_Tch(ns,axis,layer,s)
     &                 +EcCal_To(ns,axis,layer,s)
     &                 +EcCal_Tadc(ns,axis,layer,s)/sqrt(ec_raw_adcs(ns,axis,layer,s))
               else
                  Ec_Strip(time,ns,axis,layer,s)=-999. !flag these as bad
               endif
               esum(layer)=esum(layer)+Ec_Strip(energy,ns,axis,layer,s)
            enddo
*     
         EndDo
c         print *,'E-sum',nevent,layer,esum(layer)
      ENDDO
c     *step------- Added one more layer befor Inner and outer, EC as a whole.
c     c
      Do axis= Ec_U,Ec_W
         Nreported=0
         IF(Ec_StripN(axis,inner,s)+Ec_StripN(axis,outer,s).GT.0)THEN
            do zerostrips=1,MAXstrips
               inner_strips(zerostrips,axis,s)=0
               outer_strips(zerostrips,axis,s)=0
            enddo
            do id=1,Ec_StripN(axis,inner,s)
               ns=Ec_StripID(id,axis,inner,s)
               inner_strips(ns,axis,s)=1
            enddo
            do id=1,Ec_StripN(axis,outer,s)
               ns=Ec_StripID(id,axis,outer,s)
               outer_strips(ns,axis,s)=1
            enddo
            do id=1,MAXecstrips
               IF(inner_strips(id,axis,s)+
     &              outer_strips(id,axis,s).GT.0)THEN
                  Nreported=Nreported+1
                  Ec_StripID(Nreported, axis, whole,s)=id
                  Ec_strip( energy,id, axis, whole,s)=
     &                 Ec_strip( energy,id, axis, inner,s)*
     &                 inner_strips(id,axis,s)+
     &                 Ec_strip( energy,id, axis, outer,s)*
     &                 outer_strips(id,axis,s)
               else
                  Ec_strip( energy,id, axis, whole,s)= 0.0
               ENDIF
            enddo
         ENDIF
         Ec_stripN( axis, whole, s)=Nreported
      ENDDO
c     c
c     *step
      RETURN
      END
