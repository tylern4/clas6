      subroutine sets_stnew

c_begin_doc

c  Documentation for subroutine sets_st

c  $Id: sets_stnew.F,v 1.2 2009/08/14 22:17:59 fklein Exp $

c  Purpose:
c  --------

c     ******************************************************************
c     *                                                                *
c     *      Define the scintillator as a sensitive set,               *
c     *      define the hit and digitization parameters.               *
c     *                                                                *
c     *    ==>Called by : user_geom                                        *
c     *       Author    M.Guckes       Oct.  87                        *
c     *                                                                *
c     *                                                                *
c     *          MODIFIED by M. Guidal March 93                        *
c     *          ==============================                        *
c     ******************************************************************

c  Major revisions:
c  ----------------
c     Elliott Wolin, College of William and Mary, 6-dec-94

c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: sets_stnew.F,v $')
      parameter (crevis= '$Revision: 1.2 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/08/14 22:17:59 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'sets_st')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: sets_stnew.F,v 1.2 2009/08/14 22:17:59 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "lcgeom.inc"
#include "stsets.inc"
c_end_inc

c  local variables:
c  ----------------

c  for data hits,digi
c fk 8/09: add offset for time of flight because neg. values are not allowed

      character*4 namesv(nvol_st), namesh(nhits_st), namesd(ndigi_st) 
      integer   nbitsv(nvol_st),nbitsh(nhits_st),nbitsd(ndigi_st)
      real orig(nhits_st),fact(nhits_st)

c      data namesv /'ST  ', 'ST1 '/
      data namesv /'ST  ', 'STP1'/
c      data nbitsv /    2,     1/
      data nbitsv /    3,     1/
      data namesh /'X   ','Y   ','Z   ','PINC','TOF ','ELOS'/

      data nbitsh /     0,     0,     0,    0,     0,     0/
      data orig   /  500.,  500.,  500.,    0.,  100.,    0./
      data fact   /  100.,  100.,  100.,  1.e5,  1.e5,  1.e5/

       data namesd /'ADC1','TDC1','ADC2','TDC2'/
       data nbitsd /0,0,0,0/


c  for mc hits
      character*4 nameshh(nhits_sth)
      real origh(nhits_sth),facth(nhits_sth)
      integer nbitshh(nhits_sth)
      data nameshh/'X','Y','Z','CX','CY','CZ','PTOT','STAK','PART','TOF'/
      data origh  /500.,500.,500.,1.,1.,1.,0.,0.,0.,100./
      data facth  /100.,100.,100.,100.,100.,100.,1.e4,1.,1.,1.e5/
      data nbitshh /nhits_sth*0/

      integer get_idtype
c_end_var


c  executable code:
c  ----------------

c  use same idtype for all ST sets
      idtype_st=get_idtype()

c  define sets,digi for data
      idtype_st=get_idtype()
c      call gsdet('ST  ','ST1 ',nvol_st,namesv,nbitsv,
c     1     idtype_st,1000,1000,set_st,det_st1)
c      call gsdeth('ST  ','ST1 ',nhits_st,namesh,nbitsh,orig,fact)
c      call gsdetd('ST  ','ST1 ',ndigi_st,namesd,nbitsd)
c      namesv(2)='ST2 '
c      call gsdet('ST  ','ST2 ',nvol_st,namesv,nbitsv,
c     1     idtype_st,1000,1000,set_st,det_st2)
c      call gsdeth('ST  ','ST2 ',nhits_st,namesh,nbitsh,orig,fact)
c      call gsdetd('ST  ','ST2 ',ndigi_st,namesd,nbitsd)
       namesv(2)='STP1'
      call gsdet('ST  ','LEG1',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_stp11)
      call gsdeth('ST  ','LEG1',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','LEG1',ndigi_st,namesd,nbitsd)
      call gsdet('ST  ','NOS1',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_stp12)
      call gsdeth('ST  ','NOS1',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','NOS1',ndigi_st,namesd,nbitsd)

       namesv(2)='STP2'
      call gsdet('ST  ','LEG2',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_stp21)
      call gsdeth('ST  ','LEG2',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','LEG2',ndigi_st,namesd,nbitsd)
      call gsdet('ST  ','NOS2',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_stp22)
      call gsdeth('ST  ','NOS2',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','NOS2',ndigi_st,namesd,nbitsd)
      call gsdet('ST  ','NNO2',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_stp23)
      call gsdeth('ST  ','NNO2',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','NNO2',ndigi_st,namesd,nbitsd)


       namesv(2)='STP3'
      call gsdet('ST  ','LEG3',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_stp31)
      call gsdeth('ST  ','LEG3',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','LEG3',ndigi_st,namesd,nbitsd)
      call gsdet('ST  ','NOS3',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_stp32)
      call gsdeth('ST  ','NOS3',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','NOS3',ndigi_st,namesd,nbitsd)
      call gsdet('ST  ','NNO3',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_stp33)
      call gsdeth('ST  ','NNO3',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','NNO3',ndigi_st,namesd,nbitsd)

       namesv(2)='STP4'
      call gsdet('ST  ','LEG4',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_stp41)
      call gsdeth('ST  ','LEG4',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','LEG4',ndigi_st,namesd,nbitsd)
      call gsdet('ST  ','NOS4',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_stp42)
      call gsdeth('ST  ','NOS4',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','NOS4',ndigi_st,namesd,nbitsd)

     
           
c  define sets for MC hits using detector alias
c      call gsdeta('ST  ','ST1 ','STH1',1000,0,det_sth1)
c      call gsdeth('ST  ','STH1',nhits_sth,nameshh,nbitshh,origh,facth)
c      call gsdeta('ST  ','ST2 ','STH2',1000,0,det_sth2)
c      call gsdeth('ST  ','STH2',nhits_sth,nameshh,nbitshh,origh,facth)

      call gsdeta('ST  ','LEG1','LEH1',1000,0,det_stph11)
      call gsdeth('ST  ','LEH1',nhits_sth,nameshh,nbitshh,origh,facth)
      call gsdeta('ST  ','NOS1','NOH1',1000,0,det_stph12)
      call gsdeth('ST  ','NOH1',nhits_sth,nameshh,nbitshh,origh,facth)
      
      call gsdeta('ST  ','LEG2','LEH2',1000,0,det_stph21)
      call gsdeth('ST  ','LEH2',nhits_sth,nameshh,nbitshh,origh,facth)
      call gsdeta('ST  ','NOS2','NOH2',1000,0,det_stph22)
      call gsdeth('ST  ','NOH2',nhits_sth,nameshh,nbitshh,origh,facth)
      call gsdeta('ST  ','NNO2','NNH2',1000,0,det_stph23)
      call gsdeth('ST  ','NNH2',nhits_sth,nameshh,nbitshh,origh,facth)

      call gsdeta('ST  ','LEG3','LEH3',1000,0,det_stph31)
      call gsdeth('ST  ','LEH3',nhits_sth,nameshh,nbitshh,origh,facth)
      call gsdeta('ST  ','NOS3','NOH3',1000,0,det_stph32)
      call gsdeth('ST  ','NOH3',nhits_sth,nameshh,nbitshh,origh,facth)
      call gsdeta('ST  ','NNO3','NNH3',1000,0,det_stph33)
      call gsdeth('ST  ','NNH3',nhits_sth,nameshh,nbitshh,origh,facth)

      call gsdeta('ST  ','LEG4','LEH4',1000,0,det_stph41)
      call gsdeth('ST  ','LEH4',nhits_sth,nameshh,nbitshh,origh,facth)
      call gsdeta('ST  ','NOS4','NOH4',1000,0,det_stph42)
      call gsdeth('ST  ','NOH4',nhits_sth,nameshh,nbitshh,origh,facth)
      
      return
      end

c---------------------------------------------------------------------------------


