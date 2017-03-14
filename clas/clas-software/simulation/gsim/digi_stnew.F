      subroutine digi_stnew

c_begin_doc

c  Documentation for subroutine digi_st

c  $Id: digi_stnew.F,v 1.4 2005/11/18 01:15:15 fklein Exp $

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
      parameter (cfile=  '$RCSfile: digi_stnew.F,v $')
      parameter (crevis= '$Revision: 1.4 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2005/11/18 01:15:15 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'DIGI_st')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: digi_stnew.F,v 1.4 2005/11/18 01:15:15 fklein Exp $
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
      data Length,refr_ind,conv_fac/107.4, 1.58, 0.050/
      
      integer paddle_nhits,jj,kk
      real    paddle_hits(nhits_st,nhmax),leg_to_nose_z

     

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
      data x0,z0,vprop/-70.0,48.0,14.5/

c  the following was automatically generated...ejw, W&M
	real coef1,coef2,deltae
	integer idig,ih,imodul,isect,nhits,ip,ipair
	real tprop1,tprop2,xh,yh,zh
c_end_var


c  executable code:
c  ----------------
           
c  loop over the ST pairs(3 pairs)
c      do ipair=1,nsect/2
      do ipair=1,nsect

c  get hits
	numvs(1)=ipair
	
c	do ip=1,2
	do ip=1,4
	   numvs(2)=0
c	   isect = (ipair-1)*2+ip
	   isect = ipair
	   
c   	   if (ip.eq.1) then
c              call gfhits('ST  ','ST1 ',nvol_st,nhits_st,nhmax,
c     1             0,numvs,itrak,numbv2,hits,nhits)
c 	   else 
c              call gfhits('ST  ','ST2 ',nvol_st,nhits_st,nhmax,
c     1             0,numvs,itrak,numbv2,hits,nhits)
c           endif

           paddle_nhits=0
   	   if (ip.eq.1) then
              call gfhits('ST  ','LEG1',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
                   do jj=1,nhits
                     do kk=1,6
                        paddle_hits(kk,jj+paddle_nhits)=hits(kk,jj)
                      enddo
                   enddo
                  paddle_nhits=paddle_nhits+nhits
              call gfhits('ST  ','NOS1',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
                   do jj=1,nhits
                     do kk=1,6
                        paddle_hits(kk,jj+paddle_nhits)=hits(kk,jj)
                      enddo
                   enddo
                  paddle_nhits=paddle_nhits+nhits              
 	   else if (ip.eq.2) then
              call gfhits('ST  ','LEG2',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
                   do jj=1,nhits
                     do kk=1,6
                        paddle_hits(kk,jj+paddle_nhits)=hits(kk,jj)
                      enddo
                   enddo
                  paddle_nhits=paddle_nhits+nhits
              call gfhits('ST  ','NOS2',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
                   do jj=1,nhits
                     do kk=1,6
                        paddle_hits(kk,jj+paddle_nhits)=hits(kk,jj)
                      enddo
                   enddo
                  paddle_nhits=paddle_nhits+nhits
              call gfhits('ST  ','NNO2',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
                   do jj=1,nhits
                     do kk=1,6
                        paddle_hits(kk,jj+paddle_nhits)=hits(kk,jj)
                      enddo
                   enddo
                  paddle_nhits=paddle_nhits+nhits
 	   else if (ip.eq.3) then
              call gfhits('ST  ','LEG3',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
                   do jj=1,nhits
                     do kk=1,6
                        paddle_hits(kk,jj+paddle_nhits)=hits(kk,jj)
                      enddo
                   enddo
                  paddle_nhits=paddle_nhits+nhits

              call gfhits('ST  ','NOS3',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
                   do jj=1,nhits
                     do kk=1,6
                        paddle_hits(kk,jj+paddle_nhits)=hits(kk,jj)
                      enddo
                   enddo
                  paddle_nhits=paddle_nhits+nhits

              call gfhits('ST  ','NNO3',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
                   do jj=1,nhits
                     do kk=1,6
                        paddle_hits(kk,jj+paddle_nhits)=hits(kk,jj)
                      enddo
                   enddo
                  paddle_nhits=paddle_nhits+nhits
 	   else if (ip.eq.4) then
              call gfhits('ST  ','LEG4',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
                   do jj=1,nhits
                     do kk=1,6
                        paddle_hits(kk,jj+paddle_nhits)=hits(kk,jj)
                      enddo
                   enddo
                  paddle_nhits=paddle_nhits+nhits
              call gfhits('ST  ','NOS4',nvol_st,nhits_st,nhmax,
     1             0,numvs,itrak,numbv2,hits,nhits)
                   do jj=1,nhits
                     do kk=1,6
                        paddle_hits(kk,jj+paddle_nhits)=hits(kk,jj)
                      enddo
                   enddo
                  paddle_nhits=paddle_nhits+nhits

           endif

           if (paddle_nhits.gt.0) then
              if (paddle_nhits.gt.nhmax) then
                 write (chmail,1100) isect, imodul, nhmax, paddle_nhits
                 call recmes(crname,'I',chmail)
                 paddle_nhits=nhmax
              end if
           
c  Loop over the hits in one crystal.
              deltae=0.
              do ih=1,paddle_nhits
                 deltae=deltae+paddle_hits(6,ih)
              
                leg_to_nose_z=leg_parms(3)+(zoffset+st_zoff_ff)

                 if(paddle_hits(3,ih).gt.leg_to_nose_z) then
                   l =leg_parms(3)*2. +(paddle_hits(3,ih)-leg_to_nose_z)/cosd(45.)
                   where = 'n'
                 else
                   l = paddle_hits(3,ih)-(zoffset+st_zoff_ff)+leg_parms(3)
                   where = 'l'
                 endif  
c                 vprop = 22.91/refr_ind
                 tprop1=l/vprop
c       print *,leg_parms(3),zoffset,st_zoff_ff,paddle_hits(3,ih),tprop1

c there is no adc information, need to add later 4/24/98
                 coef1=1.
                 coef2=5.
		 
                 kdigi(2,ih)=(paddle_hits(5,ih)+tprop1)/conv_fac
                 kdigi(4,ih)=0.
                 kdigi(1,ih)=deltae*100000*coef2
                 kdigi(3,ih)=0.
              enddo
	

c  Store digis
c              do ih=1,nhits
c                 if (kdigi(1,ih).ne.0)then
c 	            if (ip.eq.1) then	
c                       call gsdigi(set_st,det_st1,itrak,nhits,numvs,
c     1                      kdigi(1,ih),idig)
c                    else  
c                       call gsdigi(set_st,det_st2,itrak,nhits,numvs,
c     1                      kdigi(1,ih),idig)
c                    endif 
c                    
c                    if (idig.eq.0) then
c                       write (chmail,1200) imodul, isect
c                       call recmes(crname,'I',chmail)
c                    endif
c
c                 endif
c              enddo

c+ The digitalized hits are stored in the leg of the corresponding paddles
c+ (it is not possible to disintangle different part of the same paddle)               
              do ih=1,paddle_nhits
                 if (kdigi(1,ih).ne.0)then
 	            if (ip.eq.1) then	
                       call gsdigi(set_st,det_stp11,itrak,nhits,numvs,
     1                      kdigi(1,ih),idig)
                    else if (ip.eq.2) then 
                       call gsdigi(set_st,det_stp21,itrak,nhits,numvs,
     1                      kdigi(1,ih),idig)
                    else if (ip.eq.3) then 
                       call gsdigi(set_st,det_stp31,itrak,nhits,numvs,
     1                      kdigi(1,ih),idig)
                    else if (ip.eq.4) then 
                       call gsdigi(set_st,det_stp41,itrak,nhits,numvs,
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
