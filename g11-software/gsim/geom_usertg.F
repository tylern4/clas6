
      subroutine geom_usertg(qbeam,itg_mate)
  
c_begin_doc

c  Documentation for subroutine geom_usertg
c  
c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *                 User defined target geometry                   *
c     *                                                                *
c     *                                                                *
c     ******************************************************************
c
c      simple routine to define a cylindrical target 
c
c  Major revisions:
c  ----------------
c
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid,crmess
      parameter (cfile=  '$RCSfile: geom_usertg.F,v $')
      parameter (crevis= '$Revision: 1.1 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2001/04/23 17:53:17 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'GEOM_USERTG')
      parameter (crauth= 'fklein')
      data crcsid/'
     1$Id: geom_usertg.F,v 1.1 2001/04/23 17:53:17 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "gcbank.inc"
#include "gcnum.inc"
#include "ffpar_tg.inc"
#include "ffpar_mg.inc"
#include "clmate.inc"
#include "cltmed.inc"
c_end_inc

*
*  input parameter
*  ---------------

      integer  qbeam      !=0 for photon beam;  =-1 for electron beam
      integer  itg_mate   !target material number
*
*  external functions
*  ------------------

      integer  get_tmed, lnblnk, iucomp

*  local variables:
*  ----------------

      integer ivol                      !volume#
      integer i,istrlen
      real    zpos,ztg,xytg             !position of volume
      real    tgpar(3),parm(3),pars(3)  !arrays for volume definitions
      integer imat2,imat3               !material# for cell wall and superins.
      data    imat2,imat3 /0,0/
      integer tmed_tg           !track.medium for target 
      integer tmed_wall         !track.medium for cell wall
      integer tmed_ins          !track.medium for superinsulation layers
      real    tmaxfd, stmax, deemax, epsil, stmin    !track.media cuts
      real    aa,zz,dens,radl,absl                   !material constants

      character*4   ch_mvol                  !name of mother volume
      character*20  ctg_mate, cmat2, cmat3   !material names
      integer  h_cellflag(3)
      data h_cellflag /4HCELL,4HOFF ,4HINS /
      real  eltg_zoff
      data  eltg_zoff / -5.06 /

c_end_var


c  executable code:
c  ----------------
      if(qbeam.EQ.0) then
        call recmes(crname,'I','Define user geometry for photon target...')
      elseif(qbeam.EQ.-1) then
        call recmes(crname,'I','Define user geometry for electron target...')
      else
        call recmes(CRNAME,'W','Beam type not defined -> NO TARGET!')
        return
      endif
*
******************************
*  define geometry parameters
******************************
*
*  check whether solid/foil target or liquid target

      call GFMATE(itg_mate,ctg_mate,aa,zz,dens,radl,absl,parm,3)
      if(aa.eq.-1) then
        call recmes(CRNAME,'W','Target material not defined -> NO TARGET!')
        return
      endif
      if(ctg_mate(1:3).NE.'LIQ') then
        if(ctg_mate(1:3).EQ.'AIR' .OR. ctg_mate(1:6).EQ.'HELIUM' .OR. 
     &           ctg_mate(1:6).EQ.'VACUUM') then
          if(tg2_mate_ff.NE.0) itg_mate=0                 !invalid for 2.target
          if(tg_user_ff(3).LE.0.0) tg_user_ff(3)=0.0150   !default=150mu
        else
          if(tg2_mate_ff.NE.0) itg_mate=0                 !invalid for 2.target
          tg_user_ff(3)=0.0
        endif
      endif
*
*  parameters for target tube
      if(tg2_mate_ff.NE.0) then
        xytg=sqrt(tg2_pos_ff(1)**2+tg2_pos_ff(2)**2)
        ztg =tg2_pos_ff(3)
        if(tg_user_ff(1).LE.0.0) tg_user_ff(1)=0.0200    !200mu solid target
        if(tg_user_ff(2).LE.0.0) tg_user_ff(2)=1.0       !1cm diameter
      else
        if(tg_user_ff(1).LE.0.0 .OR. tg_user_ff(2).LE.0.0) then
          call recmes(CRNAME,'W','Geometry parameters fo user target invalid!')
          return
        endif
        xytg=sqrt(tg_pos_ff(1)**2+tg_pos_ff(2)**2)
        ztg =tg_pos_ff(3)
      endif
      tgpar(1)=0.0                               !inner radius
      tgpar(2)=tg_user_ff(2)/2.                  !outer radius
      tgpar(3)=tg_user_ff(1)/2.                  !half length
*
*  check whether TARG volume exists
*  if so: - stop defining a target geometry (except in case of 2.target)
*         - second target must be outside the primary target

      call glook('TARG', IQ(jvolum+1), NVOLUM, ivol)

      if(ivol.GT.0) then
        if(tg2_mate_ff.EQ.0) then
          call recmes(CRNAME,'W','TARG volume already defined !')
          return
        else
*  second target?
          if(tgpar_save(1).GT.0.0) then                    !check position:
            if(ztg-tgpar(3).GE.tgpar_save(7) .AND. 
     &         ztg+tgpar(3).LE.tgpar_save(7)+tgpar_save(1)) then
              if(xytg+tgpar(2).LT.tgpar_save(2)) then      !inside 1.target 
                if(ztg.GE.0.0) then
                  ztg=tgpar_save(6)+tgpar_save(1)+tgpar(3) !move to end of tg.cell
                else
                  ztg=tgpar_save(6)-tgpar(3)               !move to end of tg.cell
                endif
                tg2_pos_ff(3)=ztg
              endif
            endif
          endif 
        endif

*  primary target not defined:  second target as primary target

      elseif(tg2_mate_ff.NE.0) then
        tg_mate_ff=tg2_mate_ff
        tg2_mate_ff=0
        do i=1,3
          tg_pos_ff(i)=tg2_pos_ff(i)
        enddo

*  user defined geometry for primary target:
      else

        if(tg_user_ff(3).GT.0.0) then            !target wall thickness
          imat2=tgpar_save(5)
          if(imat2.LT.0) then                    !default material for cell wall
            if(qbeam.EQ.-1) then
              imat2=imate_aluminum
            else
              imat2=imate_mylar
            endif
          endif
        else
          imat2=0
        endif
        imat3=IFIX(tg_ins_ff(1))                 !superinsulation material
        if(imat3.GT.0) then
          if(tg_ins_ff(2).EQ.-1.0) then
            tg_ins_ff(2)=0.03                    !default thickness of superins.
            tg_ins_ff(3)=tg_ins_ff(2)
          elseif(tg_ins_ff(2).LE.0.0) then
            imat3=0
          endif
          if(tg_ins_ff(5).EQ.-1.0) then
            tg_ins_ff(5)=3.0                     !default dist. cell - superins.
            tg_ins_ff(6)=tg_ins_ff(5)
          elseif(tg_ins_ff(5).LE.0.0) then
            imat3=0
          endif
        endif
        i=IUCOMP(tg_cell_ff,h_cellflag,3)
        if(i.GT.0) then
          if(i.GT.1) imat2=0
          if(i.LT.2) imat3=0
        endif
      endif
*
***************************
*  define tracking media
***************************
*
        TMAXFD =  0.1           !max. angular deviation in one step [degree]
        STMAX  =  2.0           !max. step  [cm]
        DEEMAX =  0.01          !max. energy loss per step  [0;1]
        EPSIL  =  0.0001        !boundary crossing precision
        STMIN  =  0.01          !min. step  [cm]
*
*  target material:
*
      if(itg_mate.EQ.imate_vacuum) then
         tmed_tg=clas_med_vac
      elseif(itg_mate.GT.0) then
         tmed_tg=get_tmed()
         CALL GSTMED( tmed_tg,ctg_mate,itg_mate, 
     &             1, init_ifield,init_fieldm,tmaxfd,
     &             stmax, deemax, epsil, stmin, 0 , 0 )
      else
         write(CRMESS,'(3A)') 'Invalid target material for second target:',
     &                         ctg_mate,' -> no second target defined'
         call recmes(CRNAME,'W',CRMESS)
         return
      endif
*
*  target walls:
*  
      if(imat2.GT.0) then
        call GFMATE(imat2,cmat2,aa,zz,dens,radl,absl,parm,3)
        if(aa.gt.0) then
          tmed_wall=get_tmed()
          CALL GSTMED( tmed_wall,cmat2,imat2, 
     &             1, init_ifield,init_fieldm,tmaxfd,
     &             stmax, deemax, epsil, stmin, 0 , 0 )
        else
          imat2=0
        endif
      endif
*
*  superinsulation
*
      if(imat3.GT.0) then
        call GFMATE(imat3,cmat3,aa,zz,dens,radl,absl,parm,3)
        if(aa.gt.0) then
          tmed_ins=get_tmed()
          CALL GSTMED( tmed_ins,cmat3,imat3, 
     &             1, init_ifield,init_fieldm,tmaxfd,
     &             stmax, deemax, epsil, stmin, 0 , 0 )
        else
          imat3=0
        endif
      endif

      istrlen=LNBLNK(ctg_mate)
      write(crmess,'(3A,F8.3,A)') 'set cylindrical Target  (material=',
     &            ctg_mate(1:istrlen),')  at z-pos.:',ztg+tgpar_save(12),'cm'
      call recmes(crname,'I',crmess)
      if(imat2.GT.0) then
        istrlen=LNBLNK(cmat2)
        write(crmess,'(2(A,F7.3),A,F6.4,3A)') '   Target length:',
     &        tg_user_ff(1),'cm;  radius:',tgpar(2),'cm;  cell thickness:',
     &        tg_user_ff(3),' (',cmat2(1:istrlen),')'
      else
        write(crmess,'(2(A,F7.3),A)') '   Target length:',tg_user_ff(1),
     &        'cm;  radius:',tgpar(2),'cm'
      endif
      call recmes(crname,'I',crmess)
*  
************************
*  geometry definitions
************************
*
*  (a) mother volume (vacuum)
*  --------------------------
*  target for photon beam is positioned in 'BEAM' (beam pipe)
*
*  target for electron beam is positioned in 'FOIL'
*  - until 'FOIL' volumes are not cleaned up, put target into 'GAS2' volume
*    which is positioned at z=5.06 (so: add an artificial target offset)
*
      if(qbeam.EQ.0) then

         call GLOOK('BEAM', IQ(jvolum+1), NVOLUM, ivol)
         if(ivol.GT.0) then
           ch_mvol='BEAM'
         else
           ch_mvol='CLAS'
         endif

      else

         call GLOOK('FOIL', IQ(jvolum+1), NVOLUM, ivol)
         if(ivol.GT.0) then
           ch_mvol='GAS2'
           ztg=ztg+eltg_zoff
         else
           ch_mvol='CLAS'
         endif

      endif

      if(tg2_mate_ff.EQ.0) then
        parm(1)=0.
        parm(2)=tgpar(2) + 4.
        parm(3)=tgpar(3) + 5.
        call gsvolu('TG  ','TUBE',clas_med_vac,parm,3,ivol)
        call gspos('TG  ',1,ch_mvol,tg_pos_ff(1),tg_pos_ff(2),ztg,0,'MANY')
      endif
*
*  (b) target cell: 
*  ----------------
*
      if(tg2_mate_ff.EQ.0) then

        call gsvolu('TARG','TUBE',tmed_tg,tgpar,3,ivol)
        call gspos('TARG',1,'TG  ',0.,0.,0.,0,'ONLY')
        tgpar_save(1)=tgpar(3)*2.0
        tgpar_save(2)=tgpar(2)
        tgpar_save(6)=-tgpar(3)
        tgpar_save(7)=ztg-tgpar(3)

      else

        call gsvolu('TG2 ','TUBE',tmed_tg,tgpar,3,ivol)
        call gspos('TG2 ',1,ch_mvol,tg2_pos_ff(1),tg2_pos_ff(2),ztg,0,'ONLY')
        if(tgpar_save(8) .GT.ztg-tgpar(3))   tgpar_save(8) =ztg-tgpar(3)
        if(tgpar_save(9) .LT.ztg+tgpar(3))   tgpar_save(9) =ztg+tgpar(3)
        if(tgpar_save(10).LT.xytg+tgpar(2))  tgpar_save(10)=xytg+tgpar(2)

*  stop here for volume definitions of second target
        return

      endif
*
*  (c) cell walls: 
*  ---------------
*
      if(imat2.GT.0) then
        parm(1)=tgpar(2)
        parm(2)=tgpar(2)+tg_user_ff(3)
        parm(3)=tgpar(3)
        call gsvolu('TGCI','TUBE',tmed_wall,parm,3,ivol)
        parm(1)=0
        parm(2)=tgpar(2)+tg_user_ff(3)
        parm(3)=tg_user_ff(3)/2.
        call gsvolu('TGCO','TUBE',tmed_wall,parm,3,ivol)
        zpos=tgpar(3)+parm(3)
        call gspos('TGCI',1,'TG  ',0.,0.,0.,0,'ONLY')
        call gspos('TGCO',1,'TG  ',0.,0.,zpos,0,'ONLY')
        call gspos('TGCO',2,'TG  ',0.,0.,-zpos,0,'ONLY')
        tgpar_save(3)=tg_user_ff(3)
        tgpar_save(4)=tg_user_ff(3)
        tgpar_save(6)=tgpar_save(6)-tg_user_ff(3)
        tgpar_save(7)=tgpar_save(7)-tg_user_ff(3)
      endif
      tgpar_save(8) =tgpar_save(8)-tg_user_ff(3)
      tgpar_save(9) =tgpar_save(9)+tg_user_ff(3)
      tgpar_save(10)=xytg+tgpar(2)+tg_user_ff(3)
*
*  (d) superinsulation
*  -------------------
*
      if(imat3.GT.0) then
         parm(1)=tgpar(2)+tg_ins_ff(5)
         parm(2)=parm(1) +tg_ins_ff(2)
         if(tg_ins_ff(6).LE.0.0) then
            parm(3)=tgpar(3)+tg_ins_ff(5)
         else
            parm(3)=tgpar(3)+tg_ins_ff(6)
         endif
         call gsvolu('TGSC','TUBE',tmed_ins,parm,3,ivol)
         call gspos('TGSC',1,'TG  ',0.,0.,0.,0,'ONLY')
         tgpar_save(8) =ztg-parm(3)
         tgpar_save(9) =ztg+parm(3)
         tgpar_save(10)=xytg+parm(2)
         if(tg_ins_ff(3).GT.0.0) then
            pars(1)=0.0
            pars(2)=parm(2)
            pars(3)=tg_ins_ff(3)
            call gsvolu('TGSA','TUBE',tmed_ins,pars,3,ivol)
            call gspos('TGSA',1,'TG  ',0.,0.,parm(3),0,'ONLY')
            call gspos('TGSA',2,'TG  ',0.,0.,-parm(3),0,'MANY')
         endif
      endif

      end

c-----------------------------------------------------------------------------
