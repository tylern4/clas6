      subroutine bos_mcev

c_begin_doc

c  Documentation for subroutine bos_mcev

c  $Id: bos_mcev.F,v 1.7 2009/03/14 20:27:52 fklein Exp $

c  Purpose:
c  --------
c	Fills MCEV banks with event information
c
c  Author:
c  -------
c     Elliott Wolin, College of William and Mary, 24-aug-95

c  Major revisions: 
c  ----------------
c     F.Klein 12-98: save secondaries in MCTK(#1),MCVX(#1)
c                    save conditions are controlled via
c                    GEANT common/GCLIST/ LSAVE(20) array
c                     (i.e. FFread Card: SAVE )
c
c_end_doc


      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid, crmess
      parameter (cfile=  '$RCSfile: bos_mcev.F,v $')
      parameter (crevis= '$Revision: 1.7 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2009/03/14 20:27:52 $')
      parameter (cautho= '$Author: fklein $')
      parameter (crname= 'BOS_MCEV')
      parameter (crauth= 'Elliott Wolin')
      data crcsid/'
     1$Id: bos_mcev.F,v 1.7 2009/03/14 20:27:52 fklein Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
#include "gcbank.inc"
#include "gcnum.inc"
#include "gclist.inc"
#include "gctrak.inc"
#include "gcflag.inc"
c_end_inc

c  local variables:
c   ----------------
c  definition  of reaction processes (i.e. flags in MCTK bank)
c   or other causes for stopping a particle (like flag=29:'endless loop')
c  (cf. geant common block: GCTRAK/NAMEC, GCTPOL/NAME1
c  flag=2:'MULS',  3:'LOSS',  4:'FIEL',  5:'DCAY',  6:'PAIR',  7:'COMP',
c       8:'PHOT'   9:'BREM', 10:'DRAY', 11:'ANNI', 12:'HADR', 13:'ECOH',
c      14:'EVAP', 15:'FISS', 16:'ABSO', 17:'ANNH', 18:'CAPT', 19:'EINC',
c      20:'INHE', 21:'MUNU', 22:'TOFM', 23:'PFIS', 24:'SCUT', 25:'RAYL',
c      26:'PARA', 27:'PRED', 28:'LOOP', 29:'NULL', 30:'STOP'
c      31:'LABS', 32:'LREF', 33:'SMAX', 34:'SCOR', 35:'CKOV', 36:'REFL',
c      37:'REFR', 38:'SYNC', 39:'STRA'
c
c
      integer IUCOMP, geant_to_pdg                     ! external functions
      integer MBANK, MLINK

      integer JK, JKU, JP, JV, JTM                     ! pointer to ZEBRA banks

      integer mcev,mctk,mcvx,mctk0,mcvx0,part0         ! pointer to BOS banks
      integer mctk_ncol,mcvx_ncol, nrow0, ind, indp
      data mctk_ncol/11/ , mcvx_ncol /5/

      real    xsave(20)                    ! access to floats in LSAVE array
      equivalence (xsave(1),lsave(1))
      integer mylsave(10)                  ! local copy of LSAVE array
      integer h_elmg(3)
      data h_elmg /4HPHOT,4HELEC,4HELMG/

      integer MAX_DCAYLIST
      parameter (MAX_DCAYLIST=6)
      integer dcay_list(MAX_DCAYLIST)        !list of decay/absorption processes 
                                             !(cf. /GCTRAK/NAMEC)
      data dcay_list /5, 6, 11, 16, 17, 18/

      real Emin_hadr, Emin_muon, Emin_elmg
      data Emin_hadr /0.1/   ! min. energy: 100 MeV (=> keep all hadrons)
      data Emin_muon /0.1/   ! min. energy: 100 MeV (=> keep all)
      data Emin_elmg /0.05/  ! min. energy: 50 MeV 

      logical flg_all,flg_allp,flg_elmg(3),flg_muon,flg_hadr,flg_dcay
      integer max_level, level, dtype

      integer MAX_TOSAVE, MAX_PRIMTRK, MAX_SECVX
      parameter (MAX_TOSAVE=200, MAX_PRIMTRK=20, MAX_SECVX=50)
      integer ntosave, itr_tosave(MAX_TOSAVE)
      integer nvx_tosave(MAX_TOSAVE), nvx2_tosave(MAX_TOSAVE)
      integer nvxtosave, ivx_vxtosave(MAX_TOSAVE)
      integer nprimtrk, itr_prim(MAX_PRIMTRK), itr_bos(MAX_PRIMTRK)
      integer nsecvx, ivx_sec(MAX_SECVX), ivx_arr(MAX_SECVX)

      integer j,k,ip,id,itr, ivx, nv, nt, ntb, nvx
      integer ipar, ivol, inum, imate, imed, isec, idpart
      real    Etot, ptot, xpar
      logical myflg, lfirst
      data lfirst /.TRUE./
c_end_var


c  executable code:
c  ----------------

      mcev=mbank(iw,'MCEV',0,2,1)
      iw(mcev+ 1) = iq(jhead+3)         !1st random number seed
      iw(mcev+ 2) = iq(jhead+4)         !2nd random number seed

c
c  no secondaries to save?
      if(NSAVE.EQ.0) return
      if(NVERTX.LE.1 .OR. NTRACK.LE.1) return
c
c  look up the LSAVE array
c  first check whether the parameters were changed interactively
      if(.NOT.(batch .OR. lfirst)) then
         ip=0
         do j=1,min(NSAVE,10)
            if(mylsave(j).NE.lsave(j)) ip=ip+1
         enddo
         if(ip.GT.0) lfirst=.TRUE.
      endif

      if(lfirst) then
         flg_all = IUCOMP(4HALL ,LSAVE,20).GT.0
         flg_allp= IUCOMP(4HALLP,LSAVE,20).GT.0 .OR. flg_all
c
c  specific physics processes ?

         flg_dcay=IUCOMP(4HDCAY,LSAVE,20).GT.0 
c
c  max. level for secondaries specified?
         max_level = 100
         ip=IUCOMP(4HLEVL,LSAVE,20)
         if(ip.LE.0 .AND. ip.GE.NSAVE) goto 51
         if(lsave(ip+1).LT.1) goto 51
         if(lsave(ip+1).LE.max_level) then
            max_level = lsave(ip+1)
         elseif(lsave(ip+1).LT.1.E16) then                   !ascii string 
            read(lsave(ip+1),'(I4)',err=51) ipar
            if(ipar.GE.1 .AND. ipar.LE.max_level) max_level=ipar
         endif
 51      continue
c
c  specific particle types ?

         myflg = .FALSE.
         ip = IUCOMP(4HHADR,LSAVE,20)
         flg_hadr= ip.GT.0 .OR. flg_allp
         if(ip.LE.0) goto 55
         myflg=.TRUE.
         if(ip.GE.NSAVE) goto 55
         if(xsave(ip+1).GT.1E-5 .AND. xsave(ip+1).LT.10.) then
            Emin_hadr = xsave(ip+1)
         elseif(lsave(ip+1).GT.1E8 .AND. lsave(ip+1).LT.1E16) then
            read(lsave(ip+1),*,err=55) xpar
            if(xpar.GE.1E-5 .AND. xpar.LT.10.) Emin_hadr=xpar
         endif
 55      continue

         ip = IUCOMP(4HMUON,LSAVE,20)
         flg_muon= ip.GT.0 .OR. flg_allp
         if(ip.LE.0) goto 57
         myflg=.TRUE.
         if(ip.GE.NSAVE) goto 57
         if(xsave(ip+1).GE.1E-5 .AND. xsave(ip+1).LT.10.) then
            Emin_muon = xsave(ip+1)
         elseif(lsave(ip+1).GT.1E8 .AND. lsave(ip+1).LT.1E16) then
            read(lsave(ip+1),*,err=57) xpar
            if(xpar.GE.1E-5 .AND. xpar.LT.10.) Emin_muon=xpar
         endif
 57      continue

         flg_elmg(3)=flg_allp
         do j=3,1,-1
            ip = IUCOMP(h_elmg(j),LSAVE,20)
            flg_elmg(j)= ip.GT.0 .OR. flg_elmg(3)
            if(ip.LE.0) goto 59
            myflg=.TRUE.
            if(ip.GE.NSAVE) goto 59
            if(xsave(ip+1).GE.1E-5 .AND. xsave(ip+1).LT.10.) then
               Emin_elmg = xsave(ip+1)
            elseif(lsave(ip+1).GT.1E8 .AND. lsave(ip+1).LT.1E12) then
               read(lsave(ip+1),*,err=59) xpar
               if(xpar.GE.1E-5 .AND. xpar.LT.10.) Emin_elmg=xpar
            endif
 59         continue
         enddo
c
c  if no particle type specified, discard part. type
         if(.NOT.(flg_allp.OR.myflg)) then
            flg_allp=.TRUE.
            flg_muon=.TRUE.
            flg_hadr=.TRUE.
            do j=1,3
               flg_elmg(j)=.TRUE.
            enddo
         endif
c
c  write log-file info
c
         write(crmess,'(A,I3)')
     &' Save parameters for secondaries up to level ',max_level
         call recmes(crname,'I',crmess)
         write(crmess,'(A)') 'save '
         ip=6
         if(flg_allp) then
         write(crmess(ip:ip+13),'(A)') 'all particles '
            ip=20
         else
            if(flg_hadr) then
               write(crmess(ip:ip+7),'(A)') 'hadrons '
               ip=ip+8
            endif
            if(flg_muon) then
               write(crmess(ip:ip+5),'(A)') 'muons '
               ip=ip+6
            endif
            if(flg_elmg(1)) then
               write(crmess(ip:ip+7),'(A)') 'photons '
               ip=ip+8
            endif
            if(flg_elmg(2)) then
               write(crmess(ip:ip+9),'(A)') 'electrons '
               ip=ip+10
            endif
         endif
         if(flg_dcay) then
            write(crmess(ip:ip+27),'(A)') ' from decay/absor. processes'
         endif
         call recmes(crname,'I',crmess)
         write(crmess,'(A)')' min. energies [GeV]:'
         ip=22
         if(flg_hadr) then
            write(crmess(ip:ip+15),'(A8,F8.5)') '  (hadr)',Emin_hadr
            ip=ip+16
         endif
         if(flg_muon) then
            write(crmess(ip:ip+15),'(A8,F8.5)') '  (muon)',Emin_muon
            ip=ip+16
         endif
         if(flg_elmg(1).OR.flg_elmg(2)) then
            write(crmess(ip:ip+15),'(A8,F8.5)') '  (elmg)',Emin_elmg
         endif
         call recmes(crname,'I',crmess)

         do j=1,10
            mylsave(j)=lsave(j)
         enddo
         lfirst=.FALSE.
      endif                               !start values for parameters
c
c  loop over 'primary vertices' (assuming that all primaries are stored first!)
      nrow0 = 0
      mctk0 = mlink(iw,'MCTK',0)
      if(mctk0.GT.0) then
         nrow0 = IW(mctk0-4)
      else
c was part bank used in generator? copy to MCTK#0
         part0=mlink(iw,'PART',0)
         if(part0.gt.0 .and. iw(part0).gt.0) then
            nrow0 = IW(part0-4)
            mctk0 = mbank(iw,'MCTK',0,MCTK_NCOL,nrow0)
            mcvx0 = mbank(iw,'MCVX',0,MCVX_NCOL,nrow0)
            if(mctk0.gt.0.and.mcvx0.gt.0) then
            do j=1,nrow0
               indp= part0+(j-1)*iw(part0-5)
               ind = mctk0+(j-1)*MCTK_NCOL
               ptot = sqrt(rw(indp+6)**2+rw(indp+7)**2+rw(indp+8)**2)
               rw(ind+1) = rw(indp+6)/ptot
               rw(ind+2) = rw(indp+7)/ptot
               rw(ind+3) = rw(indp+8)/ptot
               rw(ind+4) = ptot
               rw(ind+5) = sqrt(rw(indp+5)**2-ptot**2)
               rw(ind+6) = rw(indp+9)
               iw(ind+7) = geant_to_pdg(iw(indp+1))
               iw(ind+8) = iw(indp+13)
               if(j.eq.1) then
                  iw(ind+10) = 1
               else
                  iw(ind+9) = j
               endif
c bad style but PART bank has different vertex for each track
               ind = mcvx0+(j-1)*MCVX_NCOL
               rw(ind+1) = rw(indp+2)
               rw(ind+2) = rw(indp+3)
               rw(ind+3) = rw(indp+4)
            enddo  
            endif
         endif
      endif

      nprimtrk =0 
      do ivx=1,NVERTX
         JV = LQ(JVERTX-ivx)
         if(JV.LE.0)  goto 100
         if(Q(JV+5)+Q(JV+6).GE.1.) goto 100        !not primary vertex
         nt = Q(JV+7)
         do j=1,nt
            if(nprimtrk.LT.MAX_PRIMTRK) then
               nprimtrk=nprimtrk+1
               itr_prim(nprimtrk)=Q(JV+7+j)
            endif
         enddo
      enddo
c
c  get ending vertex (and other sec. vertices) for primary tracks 
 100  continue
      nsecvx    = 0                         ! sec.vertices on this level
      do itr=1,nprimtrk
         JK = LQ(JKINE-itr_prim(itr))
         if(JK.GT.0) then
            nv = Q(JK+7)
            do j=1,nv
               if(nsecvx.LT.MAX_SECVX) then
                  nsecvx=nsecvx+1
                  ivx_sec(nsecvx)=Q(JK+7+j)
               endif
            enddo
            nt = itr
            if(IQ(JK-2).GT.0) then
               JKU=LQ(JK-1)
               if(JKU.GT.0 .AND. IQ(JKU-1).GT.0) then
                  nt = Q(JKU+1)
                  if(nt.LT.1 .OR. nt.GT.nrow0) nt=itr
               endif
            endif
            itr_bos(itr) = nt
         endif
      enddo

      level     =0
      ntosave   =0                         ! count the tracks we want to save
      nvxtosave =0
c
c  loop over secondaries on each level of secondaries

      do while (level.LT.max_level .AND. nsecvx.GT.0)
         level = level +1                  ! next level
         nvx = nsecvx                      ! save vertex array
         do j=1,nvx
            ivx_arr(j)=ivx_sec(j)
         enddo
         nsecvx =0                         ! sec.vertices on this level

c  get track info on this level
c  we skip tracks that have the user flag set (=not connected to a vertex)

         do id = 1,nvx
            ivx = ivx_arr(id)
            if(ivx.LE.0 .OR. ivx.GT.NVERTX) goto 150
            JV = LQ(JVERTX-ivx)
            if(JV.LE.0) goto 150
            ntb= Q(JV+5)                         ! incoming 'beam' track
            nt = Q(JV+7)                         ! connected tracks (outgoing)

c  loop over secondary particles connected to this vertex
            myflg=.TRUE.                                 !to be saved
            do j=1,nt
               if(ntosave.GE.MAX_TOSAVE)  goto 150       !end loops
               itr=Q(JV+7+j)
               if(itr.LE.1 .OR. itr.GT.NTRACK)  goto 120
               JK=LQ(JKINE-itr)
               if(JK.LE.0)       goto 120
               if(IQ(JK-2).LE.0) goto 120            !no links
               JKU=LQ(JK-1)
               if(JKU.LE.0)       goto 120           !no subbank (reaction info)
               if(IQ(JKU-1).LT.5) goto 120           !no entries in subbank
c  skip Neutrinos and geantinos
               Etot   = Q(JK+4)
               idpart = Q(JK+5)
               if(idpart.LE.0 .OR. idpart.EQ.4 .OR. idpart.EQ.48) goto 120 
c  skip hadrons or leptons/photons if not selected or energy below cut
               if(idpart.LT.4) then
                  if(.NOT.flg_elmg(idpart)) goto 120 
                  if(Etot.LT.Emin_elmg) goto 120
               elseif(idpart.LT.7) then
                  if(.NOT.flg_muon)        goto 120 
                  if(Etot.LT.Emin_muon) goto 120
               elseif(idpart.LT.50) then
                  if(.NOT.flg_hadr)        goto 120 
                  if(Etot.LT.Emin_hadr) goto 120
               else
                  if(.NOT.(flg_allp.OR.flg_all)) goto 120 
               endif
c  skip all particles that are produced by reactions other than 
c  the specified one  (here only: 'DCAY')
               dtype= 0
               ipar = Q(JKU+5)
               do k=1,MAX_DCAYLIST
                  if(ipar.EQ.dcay_list(k)) dtype=k
               enddo
               if(flg_dcay .AND. dtype.EQ.0) goto 120
c
c  save the track and vertex (if not yet done)
               if(myflg) then
                  nvxtosave = nvxtosave+1
                  ivx_vxtosave(nvxtosave) = ivx
                  myflg=.FALSE.
               endif
               ntosave = ntosave+1
               itr_tosave(ntosave) = itr
               nvx_tosave(ntosave) = nvxtosave
               nvx2_tosave(ntosave)= 0
c
c  get sec. vertices for next level
               nv=Q(JK+7)
               do k=1,nv
                  if(nsecvx.LT.MAX_SECVX) then
                     nsecvx=nsecvx+1
                     ivx_sec(nsecvx)=Q(JK+7+k)
                  endif
               enddo
c
c  ending vertex (decay of parent particle)?
               if(ntb.LE.0) goto 120
c  for primaries: add ending vertex to MCTK(#0) bank
               if(level.EQ.1) then
                  if(mctk0.LE.0) goto 120
                  ip = ntb 
                  do k=1,nprimtrk
                     if(itr_prim(k).EQ.ntb) ip = itr_bos(k)
                  enddo
                  ind=mctk0+(ip-1)*MCTK_NCOL 
                  IW(ind+10)=nvxtosave
c  for secondaries: loop through secondaries we got so far
               else
                  do k=1,ntosave-1
                     if(itr_tosave(k).EQ.ntb) then
                        nvx2_tosave(k) = nvxtosave
                     endif
                  enddo
               endif
 120           continue
            enddo
c  break if no more space to save secondaries
         enddo
 150     continue
         if(ntosave.GE.MAX_TOSAVE) level=max_level
      enddo

      if(ntosave.EQ.0) return
c
c  create BOS banks
c
      mctk=mbank(iw,'MCTK',1,MCTK_NCOL,ntosave)
      mcvx=mbank(iw,'MCVX',1,MCVX_NCOL,nvxtosave)
c
c  save track parameters
c
      do j=1,ntosave
         ind=mctk+(j-1)*MCTK_NCOL
         itr=itr_tosave(j)
c  get links
         JK=LQ(JKINE-itr)
         JKU=LQ(JK-1)
c  get entries:
c  particle properties
         idpart = Q(JK+5)
         JP = LQ(JPART - idpart)
         rw(ind+5) = Q(JP+7)
         rw(ind+6) = Q(JP+8)
         iw(ind+7) = geant_to_pdg(idpart)
c  momentum
         Etot = Q(JK+4)
         ptot = SQRT(Etot*Etot - RW(ind+5)*RW(ind+5))
         if(ptot.LT.1E-9) ptot=1.
         do k=1,3
            rw(ind+k)=Q(JK+k)/ptot
         enddo
         rw(ind+4) = ptot
c  connected vertices and parent track
         ipar = Q(JKU+5)
         iw(ind+8) = ipar
         iw(ind+9) = nvx_tosave(j) 
         iw(ind+10)= nvx2_tosave(j)
         id=0
         ip= Q(JKU+1)
         do k=1,nprimtrk
            if(itr_prim(k).EQ.ip) id = -itr_bos(k)
         enddo
         if(id.EQ.0) then
            do k=1,ntosave
               if(itr_tosave(k).EQ.ip) id = k
            enddo
         endif
         iw(ind+11) = id
      enddo
c
c  save vertices
c
      do j=1,nvxtosave
         ind=mcvx+(j-1)*MCVX_NCOL
         ivx=ivx_vxtosave(j)
c  get links and volume info from incoming track
         JV=LQ(JVERTX-ivx)
         ivol=0
         if(Q(JV+7).GT.0) then
            nt = Q(JV+8)
            JK=LQ(JKINE-nt)
            if(IQ(JK-2).GT.0) then
               JKU=LQ(JK-1)
               if(JKU.GT.0 .AND. IQ(JKU-1).GE.5) then
                  imed = Q(JKU+2)
                  imate= 0
c  get material number from JTMED bank:
                  if(imed.GT.0 .AND. imed.LE.NTMED) then
                     JTM = LQ(JTMED - imed)
                     if(JTM.GT.0) imate=Q(JTM+6)
                  endif
c  volume in subbank = detector# *10 + sector#
                  ivol = Q(JKU+3)
                  isec = MOD(ivol,10)
                  if(ivol.GE.100) ivol=ivol/10
                  inum = Q(JKU+4)
               endif
            endif
         endif
c  vertex flag definition:        [d=digits] 
c  track_medium#(3d), material#(2d),sector#(1d),detector#(1d),volume#(2d)
         iw(ind+5)=(imed*100+imate)*10000+(isec*10+ivol/10)*100+MOD(inum,100)
c  position and flight time
         do k=1,4
            rw(ind+k) = Q(JV+k)
         enddo
      enddo

      return
      end

c------------------------------------------------------------------------
