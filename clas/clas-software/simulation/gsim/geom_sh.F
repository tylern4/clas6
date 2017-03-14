      subroutine geom_sh

c_begin_doc

c  Documentation for subroutine geom_sh

c  $Id: geom_sh.F,v 1.1 2009/12/22 20:14:23 natasha Exp $

c  Purpose:
c  --------
c     ******************************************************************
c     *                                                                *
c     *      Defines the geometry of the scintillator hodoscope        *
c     *                                                                *
c     *                                                                *
c     *      Author: N.Dashyan   July, 2009                            * 
c     *                                                                *
c     *                                                                *
c     *                                                                *
c     ******************************************************************


c  include files:
c  --------------
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "gcunit.inc"
#include "gcflag.inc"
#include "gconst.inc"
#include "gclist.inc"
#include "gcbank.inc"
#include "clmate.inc"
#include "cltmed.inc" 
#include "gcnum.inc" 
#include "ectmed.inc"
#include "ffpar_ic.inc"       
c_end_inc
        integer pix_1, pix_2, pix_3, pix_4
        integer get_rotm
        real sh_pos(3)
	dimension shbox(3),pixbox(3),pixtrap(11),tiobox(3),shldbox(3)
c        
	data shbox/15.2, 15.2, 0.5/     !mother volume (vacuum) 
        data pixbox/1.9, 1.9, 0.4/      !single scintillator pixel(dens.=1.035)
        data tiobox/15.2, 15.2, 0.05/   !titanium dioxid layer (dens.=4.23) 
c
        data pixtrap/0.4,0.,0.,1.344,0.,2.687,0.,1.344,0.,2.687,0./        
	call gsvolu('SH  ','BOX ', clas_med_vac, shbox, 3, ivol1)
        call gsatt('SH  ','COLO',4)
        call gsvolu('SPIX','BOX ', ec_med_scint, pixbox,  3, ivol2)
        call gsatt('SPIX','COLO',2)
        call gsvolu('TITO','BOX ', clas_med_tio2, tiobox,  3, ivol3)
        call gsatt('TITO','COLO',3)
	call gsvolu('SPI2', 'TRAP',clas_med_vac, pixtrap ,11, ivol5)
        call gsatt('SPI2','COLO',1)
c
        print*,'ic_front(3)====',ic_front(3)
c
        call gspos('SH  ', 1,'CLAS', 0., 0., -11.03 , 0,'ONLY' )
        call gspos('TITO', 1,'SH  ', 0., 0., - 0.45 , 0,'ONLY' )
        call gspos('TITO', 2,'SH  ', 0., 0.,   0.45 , 0,'ONLY' )

ccc       call gspos('spix', #, 'SH  ', -13.3,  13,3, 0., 0, 'MANY')
	call gspos('SPIX', 1, 'SH  ', -13.3,   9.5, 0., 0, 'MANY')
	call gspos('SPIX', 2, 'SH  ', -13.3,   5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 3, 'SH  ', -13.3,   1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 4, 'SH  ', -13.3,  -1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 5, 'SH  ', -13.3,  -5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 6, 'SH  ', -13.3,  -9.5, 0., 0, 'MANY')
ccc	  call gspos('spix', #, 'SH  ', -13.3, -13.3, 0., 0, 'MANY')

        call gspos('SPIX',  7, 'SH  ', -9.5,  13.3, 0., 0, 'MANY')
	call gspos('SPIX',  8, 'SH  ', -9.5,   9.5, 0., 0, 'ONLY')
	call gspos('SPIX',  9, 'SH  ', -9.5,   5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 10, 'SH  ', -9.5,   1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 11, 'SH  ', -9.5,  -1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 12, 'SH  ', -9.5,  -5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 13, 'SH  ', -9.5,  -9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 14, 'SH  ', -9.5, -13.3, 0., 0, 'MANY')

        call gspos('SPIX', 15, 'SH  ', -5.7,  13.3, 0., 0, 'ONLY')
	call gspos('SPIX', 16, 'SH  ', -5.7,   9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 17, 'SH  ', -5.7,   5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 18, 'SH  ', -5.7,   1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 19, 'SH  ', -5.7,  -1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 20, 'SH  ', -5.7,  -5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 21, 'SH  ', -5.7,  -9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 22, 'SH  ', -5.7, -13.3, 0., 0, 'ONLY')

        call gspos('SPIX', 23, 'SH  ', -1.9,  13.3, 0., 0, 'ONLY')
	call gspos('SPIX', 24, 'SH  ', -1.9,   9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 25, 'SH  ', -1.9,   5.7, 0., 0, 'ONLY')
ccc	   call gspos('spix', #, 'SH  ', -1.9,   1.9, 0., 0, 'ONLY')
ccc	   call gspos('spix', #, 'SH  ', -1.9,  -1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 26, 'SH  ', -1.9,  -5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 27, 'SH  ', -1.9,  -9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 28, 'SH  ', -1.9, -13.3, 0., 0, 'ONLY')

        call gspos('SPIX', 29, 'SH  ',  1.9,  13.3, 0., 0, 'ONLY')
	call gspos('SPIX', 30, 'SH  ',  1.9,   9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 31, 'SH  ',  1.9,   5.7, 0., 0, 'ONLY')
ccc	   call gspos('spix', #, 'SH  ',  1.9,   1.9, 0., 0, 'ONLY')
ccc	   call gspos('spix', #, 'SH  ',  1.9,  -1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 32, 'SH  ',  1.9,  -5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 33, 'SH  ',  1.9,  -9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 34, 'SH  ',  1.9, -13.3, 0., 0, 'ONLY')

        call gspos('SPIX', 35, 'SH  ',  5.7,  13.3, 0., 0, 'ONLY')
	call gspos('SPIX', 36, 'SH  ',  5.7,   9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 37, 'SH  ',  5.7,   5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 38, 'SH  ',  5.7,   1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 39, 'SH  ',  5.7,  -1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 40, 'SH  ',  5.7,  -5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 41, 'SH  ',  5.7,  -9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 42, 'SH  ',  5.7, -13.3, 0., 0, 'ONLY')

        call gspos('SPIX', 43, 'SH  ',  9.5,  13.3, 0., 0, 'MANY')
	call gspos('SPIX', 44, 'SH  ',  9.5,   9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 45, 'SH  ',  9.5,   5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 46, 'SH  ',  9.5,   1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 47, 'SH  ',  9.5,  -1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 48, 'SH  ',  9.5,  -5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 49, 'SH  ',  9.5,  -9.5, 0., 0, 'ONLY')
	call gspos('SPIX', 50, 'SH  ',  9.5, -13.3, 0., 0, 'MANY')

ccc        call gspos('spix', #, 'SH  ',  13.3,  13,3, 0., 0, 'MANY')
	call gspos('SPIX', 51, 'SH  ',  13.3,   9.5, 0., 0, 'MANY')
	call gspos('SPIX', 52, 'SH  ',  13.3,   5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 53, 'SH  ',  13.3,   1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 54, 'SH  ',  13.3,  -1.9, 0., 0, 'ONLY')
	call gspos('SPIX', 55, 'SH  ',  13.3,  -5.7, 0., 0, 'ONLY')
	call gspos('SPIX', 56, 'SH  ',  13.3,  -9.5, 0., 0, 'MANY')
ccc        call gspos('spix', 64, 'SH  ',  13.3, -13.3, 0., 0, 'MANY')
          pix_1 = get_rotm()
c          call gsrotm(pix_1,90.,0.,90.,90.,0.,0.) ! unit matrix
           call gsrotm(pix_1,90.,0.,90.,90.,0., 45.)      
c
       call gspos('SPI2', 1, 'SH  ', -13.3, -9.5,  0., pix_1, 'ONLY')
       call gspos('SPI2', 2, 'SH  ', -9.5, -13.3,  0., pix_1, 'ONLY')

         pix_2 = get_rotm()
         call gsrotm(pix_2,90., 0., 90., 90., 0., - 45.)

       call gspos('SPI2', 3, 'SH  ', 9.5,  -13.3, 0., pix_2, 'ONLY')
       call gspos('SPI2', 4, 'SH  ', 13.3, -9.5, 0., pix_2, 'ONLY')

         pix_3 = get_rotm()
         call gsrotm(pix_3,90., 0., 90., 90., 0.,  135.)

       call gspos('SPI2', 5, 'SH  ', -13.3,  9.5, 0., pix_3, 'ONLY')
       call gspos('SPI2', 6, 'SH  ',  -9.5, 13.3, 0., pix_3, 'ONLY')

         pix_4 = get_rotm()
         call gsrotm(pix_4,90., 0., 90., 90., 0., -135.)

       call gspos('SPI2', 7, 'SH  ',  9.5, 13.3, 0., pix_4, 'ONLY')
       call gspos('SPI2', 8, 'SH  ', 13.3,  9.5, 0., pix_4, 'ONLY')


       CALL GSORD('SH  ',3)
        CALL GPVOLU(0)               ! PRINTOUT VOLUE PARAMETERS
C
C----------------------------------------------------------------
C
        CALL GGCLOS
C
      return
      end

c-------------------------------------------------------------------------------
