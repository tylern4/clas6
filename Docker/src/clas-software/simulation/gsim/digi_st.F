      subroutine digi_st

c_begin_doc

c  Documentation for subroutine digi_st

c  $Id: digi_st.F,v 1.8 2002/07/17 16:30:54 fklein Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Digitization of the scintillator                          *
c     *                                                                *
c     *    ==>Called by : user_digi                                    *
c     *       Author    T.Henkes  ********                             *
c     *       Modified  September 18, 1986   Kersten                   *
c     *       Modified  October   15, 1987   M.Guckes                  *
c     *                                                                *
c     *                                                                *
c     *          MODIFIED by M. Guidal March 93                        *
c     *          ==============================                        *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 17-oct-95
c     Elliott Wolin, College of William and Mary, 6-dec-94
c     Burin Asavapibhop, U. of Massachusetts at Amherst, 20-nov-97 
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: digi_st.F,v $')
      parameter (crevis= '$Revision: 1.8 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2002/07/17 16:30:54 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'DIGI_st')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: digi_st.F,v 1.8 2002/07/17 16:30:54 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gconst.inc"
#include "gcunit.inc"
#include "stsets.inc"
#include "lcgeom.inc"
#include "gcsets.inc"
#include "stgeom.inc"
#include "ffpar_st.inc"
c_end_inc

c  local variables:
c  ----------------
      integer nhmax
      parameter (nhmax=100)

      integer numbv2(nvol_st,nhmax),itrak(nhmax),numvs(nvol_st)
      integer kdigi(ndigi_st,nhmax)
      real hits(nhits_st,nhmax)
      
      real Length, refr_ind, conv_fac,l
      data Length,refr_ind,conv_fac/107.4, 1.58, 0.047/
      
      character*2 where
#ifdef Linux
#ifndef OSF1
        real sind, cosd, tand
        external sind, cosd, tand
#endif
#endif

#ifdef USEGNU
        real sind, cosd, tand
        external sind, cosd, tand
#endif

c  PEDES : Pedestal
c  SIGN  : Sigma for the gaussian noise distribution [GeV]
c  THRE  : Threshold [GeV]
c  FACT  : Factor for digitisation
      real  pedes,  sign,  thre,  fact
      data  pedes,  sign,  thre,  fact
     1    /    0.,0.3e-6,1.0e-6, 1.0e7/

      real x0,z0,vprop
      data x0,z0 /-70.0,48.0/

c  the following was automatically generated...ejw, W&M
	real coef1,coef2,deltae
	integer idig,ih,imodul,isect,nhits,ip,ipair
	real tprop1,tprop2,xh,yh,zh
c_end_var


c  executable code:
c  ----------------
           
c  loop over the ST pairs(3 pairs)
      do ipair=1,nsect/2

c  get hits
	numvs(1)=ipair
	
	do ip=1,2
	   numvs(2)=0
	   isect = (ipair-1)*2+ip
	   
   	   if (ip.eq.1) then
              call gfhits('ST  ','ST1 ',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
 	   else 
              call gfhits('ST  ','ST2 ',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
           endif

           if (nhits.gt.0) then
              if (nhits.gt.nhmax) then
                 write (chmail,1100) isect, imodul, nhmax, nhits
                 call recmes(crname,'I',chmail)
                 nhits=nhmax
              end if
           
c  Loop over the hits in one crystal.
              deltae=0.
              do ih=1,nhits
                 deltae=deltae+hits(6,ih)
                 hits(3,ih)=hits(3,ih)-st_zoff_ff
                 if(hits(3,ih).gt.19.9+2.) then
                   l = 19.9*2. + (hits(3,ih)-(19.9+2.))/cosd(30.)
                   where = 'n'
                 else
                   l = 19.9 + hits(3,ih) - 2.
                   where = 'l'
                 endif  
                 vprop = 30./refr_ind
                 tprop1=l/vprop
                 tprop2=(Length-l)/vprop

c there is no adc information, need to add later 4/24/98
                 coef1=1.
                 coef2=5.
		 
		 if(isect.eq.1.or.isect.eq.3.or.isect.eq.5) then                
                    kdigi(2,ih)=(hits(5,ih)+tprop1)/conv_fac
                    kdigi(4,ih)=(hits(5,ih)+tprop2)/conv_fac
                    kdigi(1,ih)=deltae*100000*coef1
                    kdigi(3,ih)=deltae*100000*coef2
                 else
                    kdigi(2,ih)=(hits(5,ih)+tprop2)/conv_fac
                    kdigi(4,ih)=(hits(5,ih)+tprop1)/conv_fac
                    kdigi(1,ih)=deltae*100000*coef1
                    kdigi(3,ih)=deltae*100000*coef2
                 endif   
                 

              enddo
	

c  Store digis
              do ih=1,nhits
                 if (kdigi(1,ih).ne.0)then
 	            if (ip.eq.1) then	
                       call gsdigi(set_st,det_st1,itrak,nhits,numvs,
     1                      kdigi(1,ih),idig)
                    else  
                       call gsdigi(set_st,det_st2,itrak,nhits,numvs,
     1                      kdigi(1,ih),idig)
                    endif 
                    
                    if (idig.eq.0) then
                       write (chmail,1200) imodul, isect
                       call recmes(crname,'I',chmail)
                    endif

                 endif
              enddo
              
c              call gpdigi('ST  ','*')

           endif
        
        enddo  

      enddo
   
      return


 1100 FORMAT ('?STCDIGI ERROR : Hits overflow for scintillator '
     1       ,'no. ',I3,' in sector ',I2,'. Only ',I4,' of ',I4,' hits '
     2       ,'could not be processed.')
 1200 FORMAT ('?STCDIGI ERROR : Digitization for scintillator '
     1       ,'no. ',I3,' in sector ',I2,' could not be stored.')

      end

c-------------------------------------------------------------------------------
