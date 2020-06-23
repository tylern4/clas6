      SUBROUTINE ALIGN_FILL
c
c_begin_doc
c  RCS ID string
c  $Id: align_fill.F,v 1.3 2002/05/27 23:17:23 sam Exp $
c
c  Documentation for subroutine ALIGN_FILL
c
c  Purpose:
c  --------
c
c  Input Parameters:  (Name - Type - Meaning)
c  ----------------
c
c  Output Parameters:  (Name - Type - Meaning)
c  -----------------
c
c  Other routines:
c  ---------------
c
c  Notes:
c  ------
c
c  Author:   Robert Feuerbach      Created:  Mon Jan 19 11:32:10 EST 1998
c  -------
c
c  Major revisions:
c  ----------------
c     
c
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
c BOS common block  uncomment the next line for BOS include file
#include "bcs.inc"
#include "wcs.inc"
#include "clas_offsets.inc"
#include "user_align.inc"
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$RCSfile: align_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.3 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 2002/05/27 23:17:23 $')
      PARAMETER (CAUTHO = '$Author: sam $')
      DATA CRCSID/   
     1'$Id: align_fill.F,v 1.3 2002/05/27 23:17:23 sam Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='ALIGN_FILL')
      PARAMETER (CRAUTH='Robert Feuerbach')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      integer NAMIND
      external NAMIND
      integer NLINK
      external NLINK

      integer MAX_WIRE
      parameter (MAX_WIRE = 192)
      integer ind, sect,sec
      integer namitbtr, namihead, namitbla, indtbtr, indhead, indtbla
      integer i,ncol,ntbtr, nrows
      real p(3),d(3),wdir(3),fdoca,cdoca,small
      real loLo,loHi,hiLo,hiHi
      real cossec(6), sinsec(6),pi
      integer trcks(6),lay,tck
      integer ind_dcgw(6), ncol_dcgw, WireInd, check
      integer nami_dcgw/0/
      INTEGER GWind, baseind, layr, wir
      REAL DirX, DirY, DirZ
      GWind(sect,layr,wir) = ind_dcgw(sect) +
     $     ((layr-1)*MAX_WIRE+wir-1)*ncol_dcgw
      DirX(baseind) = RJW(baseind+DCGW_x_dir)
      DirY(baseind) = RJW(baseind+DCGW_y_dir)
      DirZ(baseind) = RJW(baseind+DCGW_z_dir)

      data small/1.0E-03/
c_end_var
c
c  executable code for routine ALIGN_FILL:
c----6----------------------------------------------------------------72
c

      IF (NWRITE .LT. 1) THEN
         NWRITE = NWRITE + 1
         CRMESS='This is a DUMMY routine, this message written once'
         CALL RECMES(CRNAME,'I',CRMESS)

         pi = acos(-1.)
         do sect = 1,6
            cossec(sect) = cos( (sect-1.)*pi/3.)
            sinsec(sect) = sin( (sect-1.)*pi/3.)
         enddo
         loLo =  08.*pi/180.
         loHi =  20.*pi/180.
         hiLo =  40.*pi/180.
         hiHi =  65.*pi/180.
      ENDIF
      
c     prepare for wire direction info.
      
c     get the index of the bosbank we need
      if (nami_dcgw.le.0) nami_dcgw = NAMIND('DCGW')
      
      check = 0
      ind = nami_dcgw + 1
      do while (jw(ind-1).gt.0)
         ind = jw(ind-1)
         ind_dcgw(jw(ind-2)) = ind
         check = check+1
      enddo
      if (check.ne.6) call recmes(crname,'W','Error getting DCGW bank')
      
      ncol_dcgw = jw(ind_dcgw(check)-5)
      
c     fill TBLA info from HEAD and TBLA bank
      if (namihead.le.0) namihead = NAMIND('HEAD')
      if (namitbla.le.0) namitbla = NAMIND('TBLA')
      if (namitbtr.le.0) namitbtr = NAMIND('TBTR')
      
      
      if (namihead.gt.0) then
         indhead = iw(namihead)
         Evnt = iw(indhead+head_nevent) ! EVENT number
      else
         call recmes(crname,'W','No HEAD bank.')
      endif
      
      ntbtr = 0
      do i = 1,6
         trcks(i) = 0
      enddo
      
      if (namitbtr.gt.0) then   ! only 1 TBTR bank, with
                                ! the rows in the same order as
                                ! entries in the TBLA bank.
         indtbtr = iw(namitbtr)
         if (indtbtr.gt.0) then
            nrows = iw(indtbtr-4)
            if (nrows .gt. 10) nrows = 10
            ncol = iw(indtbtr-5)
            do i=0,nrows-1
               ind = indtbtr+i*ncol
               ntbtr = ntbtr + 1
               sector(ntbtr) = iw(ind+tbtr_itr_sec)/100
               sec = sector(ntbtr)
               chi2(ntbtr) = rw(ind+tbtr_chi2)
               x0(ntbtr) = rw(ind+tbtr_x)*cossec(sec)+rw(ind+tbtr_y)*sinsec(sec)
               y0(ntbtr) = rw(ind+tbtr_y)*cossec(sec)-rw(ind+tbtr_x)*sinsec(sec)
               z0(ntbtr) = rw(ind+tbtr_z)
               p(1) = rw(ind+tbtr_px)*cossec(sec)+rw(ind+tbtr_py)*sinsec(sec)
               p(2) = rw(ind+tbtr_py)*cossec(sec)-rw(ind+tbtr_px)*sinsec(sec)
               p(3) = rw(ind+tbtr_pz)
               theta(ntbtr) = atan2(sqrt(p(1)**2+p(2)**2),p(3))
               phi(ntbtr) = atan2(p(2),p(1))
               trcks(sec) = trcks(sec)+1
            enddo
         endif
      endif
      NTrack = ntbtr            ! number of tracks in event

      tck = 0
c for each sector that has a track (by TBTR), get the TBLA info.
      do sect = 1,6
         if (trcks(sect).gt.0) then
            indtbla = nlink('TBLA',sect)
            if (indtbla.le.0) then
               call recmes(crname,'W','Missing TBLA bank.')
            else
               ncol = iw(indtbla-5)
               do i = 0,(trcks(sect)*34)-1    ! for each layer
                  ind = indtbla+i*ncol
                  lay = MOD(iw(ind+tbla_trk_pln),100)-3
                  if (lay .EQ. 1) then
                     tck = tck + 1              ! track counter
                     if (tck .GT. Ntrack) then
                        write(crmess,'(A,i2)') 'tck (> ntbtr) = ',tck
                        call recmes(crname,'W',crmess)
                        goto 999
                     endif
                  endif
                  opstrcks(tck) = trcks(MOD(sector(tck)+2,6)+1)
                  layer(lay,tck) = lay
                  wire(lay,tck) = iw(ind+tbla_wire)
                  alpha(lay,tck) = rw(ind+tbla_alpha) ! ALPHA
                  fdoca = rw(ind+tbla_fitdoca) ! FITDOCA
                  fitdoca(lay,tck) = fdoca
                  cdoca = rw(ind+tbla_calcdoca) ! CALCDOCA
                  calcdoca(lay,tck) = cdoca
                  sigma(lay,tck) = rw(ind+tbla_sgdoca)  ! SIGMA DOCA
                  WireInd = GWInd(sector(tck),lay,wire(lay,tck))
                  wdir(1) = DirX(WireInd)
                  wdir(2) = DirY(WireInd)
                  wdir(3) = DirZ(WireInd)
c                  p(1) = rw(ind+tbla_cx)
c                  p(2) = rw(ind+tbla_cy)
c                  p(3) = rw(ind+tbla_cz)
c disabled calculation of vector doca
c                  p(1) = 1.
c                  p(2) = 0.
c                  p(3) = 0.
                  call CROSS(wdir,p,d)
                  call VUNIT(d,d,3)
                  if (abs(d(1)).LT.small) then
                     dx(lay,tck) = 0.
                  else
                     dx(lay,tck) = (cdoca-fdoca)/d(1) ! deltaX
                  endif
                  if (abs(d(2)).LT.small) then
                     dy(lay,tck) = 0.
                  else
                     dy(lay,tck) = (cdoca-fdoca)/d(2) ! deltaY
                  endif
                  if (abs(d(3)).LT.small) then
                     dz(lay,tck) = 0.
                  else
                     dz(lay,tck) = (cdoca-fdoca)/d(3) ! deltaZ
                  endif
                  dtime(lay,tck) = rw(ind+tbla_dtime) ! dtime
                  if (iw(ind+tbla_stat).ne.0) then
                     wire(lay,tck) = - abs(wire(lay,tck))
                  endif
                  x(lay,tck) = rw(ind+tbla_x)   ! x position
                  y(lay,tck) = rw(ind+tbla_y)   ! y position
                  z(lay,tck) = rw(ind+tbla_z)   ! z position
                  if (wire(lay,tck) .gt. 0.) then
                     call hfill(1000+sect,fdoca-cdoca,float(lay),1.)
                     if (theta(tck).gt.loLo .AND. theta(tck).lt.loHi) then
                        call hfill(1100+sect,fdoca-cdoca,float(lay),1.)
                     else if (theta(tck).gt.hiLo .AND. theta(tck).lt.hiHi) then
                        call hfill(1200+sect,fdoca-cdoca,float(lay),1.)
                     endif
                  endif
               enddo
            endif
         endif
      enddo
 998  if (tck.gt.0) then
         call hfnt(nt_align_id)
      endif
 999  RETURN
      END
c
c------------------------------------------------------------------------------




