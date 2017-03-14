      subroutine sets_st

c_begin_doc

c  Documentation for subroutine sets_st

c  $Id: sets_st.F,v 1.3 1998/04/23 18:41:51 burin Exp $

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
      parameter (cfile=  '$RCSfile: sets_st.F,v $')
      parameter (crevis= '$Revision: 1.3 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 1998/04/23 18:41:51 $')
      parameter (cautho= '$Author: burin $')
      parameter (crname= 'sets_st')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: sets_st.F,v 1.3 1998/04/23 18:41:51 burin Exp $
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
      character*4 namesv(nvol_st), namesh(nhits_st), namesd(ndigi_st) 
      integer   nbitsv(nvol_st),nbitsh(nhits_st),nbitsd(ndigi_st)
      real orig(nhits_st),fact(nhits_st)

      data namesv /'ST  ', 'ST1 '/
      data nbitsv /    2,     1/
      data namesh /'X   ','Y   ','Z   ','PINC','TOF ','ELOS'/

      data nbitsh /     0,     0,     0,    0,     0,     0/
      data orig   /  500.,  500.,  500.,    0.,    0.,    0./
      data fact   /  100.,  100.,  100.,  1.e5,  1.e5,  1.e5/

      data namesd /'ADC1','TDC1','ADC2','TDC2'/
      data nbitsd /0,0,0,0/


c  for mc hits
      character*4 nameshh(nhits_sth)
      real origh(nhits_sth),facth(nhits_sth)
      integer nbitshh(nhits_sth)
      data nameshh/'X','Y','Z','CX','CY','CZ','PTOT','STAK','PART','TOF'/
      data origh  /500.,500.,500.,1.,1.,1.,0.,0.,0.,0./
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
      namesv(2)='ST1 '
      call gsdet('ST  ','ST1 ',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_st1)
      call gsdeth('ST  ','ST1 ',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','ST1 ',ndigi_st,namesd,nbitsd)
     
      namesv(2)='ST2 '
      call gsdet('ST  ','ST2 ',nvol_st,namesv,nbitsv,
     1     idtype_st,1000,1000,set_st,det_st2)
      call gsdeth('ST  ','ST2 ',nhits_st,namesh,nbitsh,orig,fact)
      call gsdetd('ST  ','ST2 ',ndigi_st,namesd,nbitsd)
           
c  define sets for MC hits using detector alias
      call gsdeta('ST  ','ST1 ','STH1',1000,0,det_sth1)
      call gsdeth('ST  ','STH1',nhits_sth,nameshh,nbitshh,origh,facth)
      
      call gsdeta('ST  ','ST2 ','STH2',1000,0,det_sth2)
      call gsdeth('ST  ','STH2',nhits_sth,nameshh,nbitshh,origh,facth)
      
      return
      end

c---------------------------------------------------------------------------------


