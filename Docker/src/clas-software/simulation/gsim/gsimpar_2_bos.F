      subroutine gsimpar_2_bos

c_begin_doc
c  Documentation for subroutine gsimpar_2_bos
c  $Id: gsimpar_2_bos.F,v 1.12 2011/03/31 19:52:57 fxgirod Exp $
c
c  Purpose:   write input parameters into GPAR BOSbank (record_no.=0)
c  --------
c  
c  Notes:  for a simple check of the bank entries set bprint=1
c  ------  
c
c  Author:   F.Klein (JLab)  June 98
c  -------
c  Modification:  reading addresses (as done before) seems to have
c                 problems on AIX esp. for debug version,
c                 therefore each variable is read from the corresponding
c                 common block (without using the short form in 'cfread.inc')
c_end_doc

      implicit none
      character*(*) cfile,crevis,cstate,cdate,cautho,crname,crauth
      character*132 crcsid
      parameter (cfile=  '$RCSfile: gsimpar_2_bos.F,v $')
      parameter (crevis= '$Revision: 1.12 $')
      parameter (cstate= '$State: Exp $')
      parameter (cdate=  '$Date: 2011/03/31 19:52:57 $')
      parameter (cautho= '$Author: fxgirod $')
      parameter (crname= 'gsimpar_2_bos')
      parameter (crauth= 'Franz Klein')
      data crcsid/'
     1$Id: gsimpar_2_bos.F,v 1.12 2011/03/31 19:52:57 fxgirod Exp $
     1 '/
      save


c  include files:
c  --------------
#include "bcs.inc"
#include "wcs.inc"
#include "gsimpar.inc"
#include "gsimcontrol.inc"
#include "cvs_tagname.inc"
#include "ffpar.inc"
#include "ffpar_bckgr.inc"
#include "ffpar_cc.inc"
#include "ffpar_dc.inc"
#include "ffpar_ec.inc"
#include "ffpar_ec1.inc"
#include "ffpar_foil.inc"
#include "ffpar_mg.inc"
#include "ffpar_ptg.inc"
#include "ffpar_tg.inc"
#include "ffpar_sc.inc"
#include "ffpar_st.inc"
#include "gcphys.inc"
#include "gccuts.inc"
#include "gcflag.inc"
#include "gckine.inc"
#include "gclist.inc"
#include "gcparm.inc"
#include "gcstak.inc"
#include "gctrak.inc"
#include "gcscan.inc"
#include "gcunit.inc"
#include "gcopti.inc"
#include "gctmed.inc"
#include "gcrz.inc"
c_end_inc
c     using KERNLIB common:
      INTEGER  Nd,Ne,Nf,Ng,num,idummy
      COMMON/slate/ Nd,Ne,Nf,Ng,num(2),idummy(34)
c
c  local variables:
c  ----------------
      INTEGER  MBANK, ncol_gpar, nrow_gpar, ind, MAMIND
      DATA  ncol_gpar /7/

      INTEGER  LNBLNK, STATF, ICFIND
      INTEGER  istat, info_arr(12), ierr, nd1
      INTEGER  i,j, iflag, Eflag, ind_gp, mykine, mtest(5)
      INTEGER  str_len, str_cnt, lunit,icnt
      CHARACTER*132  filename, pgm_name, crmess
      CHARACTER*64   my_name
      CHARACTER*24   ctime_acc, ctime_crea
      CHARACTER*20   my_string
      CHARACTER*4    cval,cunit
      INTEGER  ival
      REAL  CODE_LOG_KEY, CODE_ASCII_KEY
      PARAMETER ( CODE_LOG_KEY=-1111.0 , CODE_ASCII_KEY=1111.0 )
      REAL  CODE_CREATION_TIME, CODE_ACCESS_TIME
      PARAMETER ( CODE_CREATION_TIME=1114.0 , CODE_ACCESS_TIME=1112.0 )

      INTEGER  nhead_ffpar, nclas_ffpar, nbckgr_ffpar, ncc_ffpar
      INTEGER  ndc_ffpar, nec_ffpar, nec1_ffpar, nfoil_ffpar, ntg_ffpar
      INTEGER  nmg_ffpar, nptg_ffpar, nsc_ffpar, nst_ffpar, ngeant_ffpar

      INTEGER  bprint
      DATA  bprint /0/
c_end_var


c  executable code:
c  ----------------
      nhead_ffpar  = 10                    !pgm-name,exec.time,etc.
      nclas_ffpar  = 12+9*max_detector     !general gsim control parameters
      nmg_ffpar    = 6                     !parameters for magnet definition
      ntg_ffpar    = 4                     !parameters for cryotarget
      if(tg_type_ff.GT.0) then
         ntg_ffpar = ntg_ffpar +4          !predefined target parameters
      elseif(tg_type_ff.LT.0) then
         ntg_ffpar = ntg_ffpar +6          !user defined target parameters
      endif
      nptg_ffpar   = 6                     !parameters for polar.target
      ncc_ffpar    = 5                     !parameters for CC
      ndc_ffpar    = 12                    !parameters for DC
      nec_ffpar    = 8                     !parameters for EC
      nec1_ffpar   = 11                    !parameters for EC1
      nsc_ffpar    = 5                     !parameters for SC
      nst_ffpar    = 5                     !parameters for ST
      nfoil_ffpar  = 7                     !parameters for 'foil'
      nbckgr_ffpar = 8                     !parameters for background gener.
      ngeant_ffpar = 52                    !parameters from GEANT ffread cards
c
c  first entries in GPAR: full executable name (incl.path), cvs tag name,
c                         creation date, execution time, OS type, user name
      nrow_gpar=nhead_ffpar+ngeant_ffpar+
     &          nclas_ffpar+nbckgr_ffpar+ncc_ffpar+ndc_ffpar+
     &          nec_ffpar+nec1_ffpar+nfoil_ffpar+nmg_ffpar+ntg_ffpar+
     &          nptg_ffpar+nsc_ffpar+nst_ffpar
c
c
      ind_gp=0
      Eflag=0
      if(Beam_energy_ff(1).GT.0.0) then
         nrow_gpar=nrow_gpar+1
         Eflag=1
      endif
c
c  get input filename for LUND, MCIN, LIZ input formats
c
      iflag=999
      mykine=ikine
      if(ABS(ikine).GT.10) then
         nrow_gpar = nrow_gpar+1
         if(ikine.LE.0)then
            mykine = ikine+10
         else
            mykine = ikine-10
         endif
      endif

      if (mykine.EQ.1) then
         call get_param(gsim_mcin_flag,gsim_mcin_env,filename,iflag)
         if(iflag.LT.0) filename=gsim_mcin_name
C
C  check whether GPAR bank exists for event generator
C
         call reslun(crname,lunit,ierr)
         if(ierr.NE.-1) then
            write(cunit,'(I4)')lunit
            str_len=LNBLNK(filename)
c  open file
            call fparm(' OPEN MCTST UNIT='//cunit//
     1    ' FILE="'//filename(1:str_len)//'" ACTION=READ STATUS=OLD')
c  select only GPAR bank 
c  and modify the record# if record#=0
            icnt=0
            do while (icnt.LT.3 .AND. Eflag.NE.-1)
               call frbos(jw,lunit,'E',ierr)
               if(ierr.EQ.-1) goto 8
               ind=mamind(JW,'GPAR')
               if(ind.EQ.0) goto 8
               ind=JW(ind)
               if(ind.EQ.0) goto 8
               ind_gp = ind
               call UCTOH('BEAM ENERGY ',mtest,4,12)
               nd1 = JW(ind)-1
               do j=ind,ind+nd1,JW(ind-5)
                  istat=0
                  do i=1,3
                     if(JW(j+2+i).EQ.mtest(i)) istat=istat+1
                  enddo
                  if(istat.EQ.3 .AND. RJW(j+1).GT.0.1) then
                     Beam_energy_ff(1)=RJW(j+1)
                     Eflag=-1
                     goto 8
                  endif
               enddo
 8             continue
               icnt=icnt+1
               if(ind_gp.GT.0) then
                  icnt=3
               else
                  call bdrop(jw,'E')
                  call bgarb(jw)
               endif
            enddo
            call fparm('CLOSE MCTST')
            call reslun(crname,-lunit,ierr)
         endif
      elseif (mykine.EQ.2) then
         call get_param(gsim_lizin_flag,gsim_lizin_env,filename,iflag)
         if(iflag.LT.0) filename=gsim_lizin_name
      elseif (mykine.EQ.5) then
         call get_param(gsim_lundin_flag,gsim_lundin_env,filename,iflag)
         if(iflag.LT.0) filename=gsim_lundin_name
      endif

      if(iflag.LT.999) then
         nrow_gpar=nrow_gpar+5
      endif
c
c  was there a GPAR bank in the input file?
c
      if(ind_gp.GT.0) then
         ind=MBANK(IW,'GPAR',1,jw(ind_gp-5),jw(ind_gp-4))
         if(ind.GT.0) then
            call ucopy(jw(ind_gp+1),iw(ind+1),jw(ind_gp))
         endif
         call bdrop(jw,'E')
         call bgarb(jw)
      endif
c
c  write these parameters into BOS bank
c
      ind=MBANK(IW,'GPAR',0,ncol_gpar,nrow_gpar)
      if(ind.LE.0) then
         call recmes(crname,'W','no space in BOS array for GPAR bank') 
         return
      endif
c
c  (1) program name (incl. path), cvs tag name,
c                creation time, execution time, OS name, user name
c
      call getarg(0,pgm_name)
      str_len=LNBLNK(pgm_name)
c
c  get full path if first chars="..",".", or program in local directory
      if (pgm_name(1:1).EQ.'.' .OR. 
     &              ICFIND('/',pgm_name,1,str_len).GT.str_len) then
         call getenvf('PWD',my_name)
         if (Nd.GT.1) then
            pgm_name=my_name(1:Nd)//'/'//pgm_name(1:str_len)
            str_len=LNBLNK(pgm_name)
         endif
      endif
      str_cnt=1
      do j=1,5
         RW(ind+1)=CODE_ASCII_KEY
         write(cval,'(A3,I1)') 'PGM',j
         call UCTOH(cval,ival,4,4)
         IW(ind+2)=ival
         call UCTOH(pgm_name(str_cnt:str_cnt+19),IW(ind+3),4,20)
         str_cnt=str_cnt+20
         ind=ind+ncol_gpar
      enddo
c
c  STATF: info(1:3): user,group,other; (4)=type; (5:6)=uid,gid; (7)=length
c          info(8): last access; info(9): last mod. info(10):create
c
      ctime_crea = '                    '
      ctime_acc  = '                    '
      istat = STATF(pgm_name(1:str_len),info_arr)
      if (istat.EQ.0) then
         call ctimef(info_arr(8),ctime_acc)
         call ctimef(info_arr(10),ctime_crea)
      endif

      RW(ind+1)=CODE_ASCII_KEY
      call UCTOH('TAG ',IW(ind+2),4,4)
      my_string=cvs_tagname
      call UCTOH(my_string,IW(ind+3),4,20)
      ind=ind+ncol_gpar

      RW(ind+1)=CODE_CREATION_TIME
      IW(ind+2)=info_arr(10)
      call UCTOH(ctime_crea(5:24),IW(ind+3),4,20)
      ind=ind+ncol_gpar

      RW(ind+1)=CODE_ACCESS_TIME
      IW(ind+2)=info_arr(8)
      call UCTOH(ctime_acc(5:24),IW(ind+3),4,20)
      ind=ind+ncol_gpar

      my_name='                                     '
      call gethostname(my_name,ival,64)
      RW(ind+1)=CODE_ASCII_KEY
      call UCTOH('HOST',IW(ind+2),4,4)
      call UCTOH(my_name,IW(ind+3),4,20)
      ind=ind+ncol_gpar

      my_name='                                      '
      call getenvf('LOGNAME',my_name)
      if (Nd.EQ.0) call getenvf('USER',my_name)
      RW(ind+1)=CODE_ASCII_KEY
      call UCTOH('USER',IW(ind+2),4,4)
      call UCTOH(my_name,IW(ind+3),4,20)
      ind=ind+ncol_gpar

c
c  (2) input filename for LUND, MCIN, LIZ input formats
c
      if(ABS(ikine).GT.10) then
         RW(ind+1)=CODE_LOG_KEY
         IW(ind+2)=-1
         call UCTOH('BACKGROUND          ',IW(ind+3),4,20)
         ind=ind+ncol_gpar
      endif
      if (mykine.EQ.1) then
         cval = 'MCIN'
      elseif (mykine.EQ.2) then
         cval = 'LIZ '
      elseif (mykine.EQ.5) then
         cval = 'LUND'
      endif
      if (iflag.LT.999) then
         str_len=1
         do j=1,5
            RW(ind+1)=CODE_ASCII_KEY
            if(j.GT.1) then
               write(cval(4:4),'(I1)') j
            endif
            call UCTOH(cval,ival,4,4)
            IW(ind+2)=ival
            call UCTOH(filename(str_len:str_len+19),IW(ind+3),4,20)
            str_len=str_len+20
            ind=ind+ncol_gpar
         enddo
      endif
c
c  (3) GSIM parameters  (cf. ffky_xx.F)
c
c  clas_ffky.F:
c
      do i=1,max_detector
         my_string = 'NOGEOM ('//det_name(i)//')'
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = CODE_LOG_KEY
         if (nogeom(i)) then
            IW(ind+2)=-1
         else
            IW(ind+2)=0
         endif
         ind=ind+ncol_gpar
      enddo
      do i=1,max_detector
         my_string = 'NOMATE ('//det_name(i)//')'
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = CODE_LOG_KEY
         if (nomate(i)) then
            IW(ind+2)=-1
         else
            IW(ind+2)=0
         endif
         ind=ind+ncol_gpar
      enddo
      do i=1,max_detector
         my_string = 'NOSEC ('//det_name(i)//')'
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = CODE_LOG_KEY
         if (nosec(i)) then
            IW(ind+2)=-1
         else
            IW(ind+2)=0
         endif
         ind=ind+ncol_gpar
      enddo
      do i=1,max_detector
         my_string = 'NOHITS ('//det_name(i)//')'
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = CODE_LOG_KEY
         if (nohits(i)) then
            IW(ind+2)=-1
         else
            IW(ind+2)=0
         endif
         ind=ind+ncol_gpar
      enddo
      do i=1,max_detector
         my_string = 'NODIGI ('//det_name(i)//')'
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = CODE_LOG_KEY
         if (nodigi(i)) then
            IW(ind+2)=-1
         else
            IW(ind+2)=0
         endif
         ind=ind+ncol_gpar
      enddo
      do i=1,max_detector
         my_string = 'NOBOSOUT ('//det_name(i)//')'
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = CODE_LOG_KEY
         if (nobosout(i)) then
            IW(ind+2)=-1
         else
            IW(ind+2)=0
         endif
         ind=ind+ncol_gpar
      enddo
      do i=1,max_detector
         my_string = 'NODATA ('//det_name(i)//')'
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = CODE_LOG_KEY
         if (nodata(i)) then
            IW(ind+2)=-1
         else
            IW(ind+2)=0
         endif
         ind=ind+ncol_gpar
      enddo
      do i=1,max_detector
         my_string = 'NOMCDATA ('//det_name(i)//')'
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = CODE_LOG_KEY
         if (nomcdata(i)) then
            IW(ind+2)=-1
         else
            IW(ind+2)=0
         endif
         ind=ind+ncol_gpar
      enddo
      do i=1,max_detector
         my_string = 'FASTCODE ('//det_name(i)//')'
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = CODE_LOG_KEY
         if (fast_code(i)) then
            IW(ind+2)=-1
         else
            IW(ind+2)=0
         endif
         ind=ind+ncol_gpar
      enddo

      call UCTOH('RAWBANKSIN          ',IW(ind+3),4,20)
      RW(ind+1)=CODE_LOG_KEY
      if (rawbanks_in) then
         IW(ind+2)=-1
      else
         IW(ind+2)=0
      endif
      ind=ind+ncol_gpar
      call UCTOH('HELIUMONLY          ',IW(ind+3),4,20)
      RW(ind+1)=CODE_LOG_KEY
      if (helium_only) then
         IW(ind+2)=-1
      else
         IW(ind+2)=0
      endif
      ind=ind+ncol_gpar
      call UCTOH('IFLGK               ',IW(ind+3),4,20)
      RW(ind+1)=0.0
      IW(ind+2)=iflgk_ff
      ind=ind+ncol_gpar
      call UCTOH('NSTEPMAX            ',IW(ind+3),4,20)
      RW(ind+1)=0.0
      IW(ind+2)=nstepmax_ff
      ind=ind+ncol_gpar
      call UCTOH('SLENGMAX            ',IW(ind+3),4,20)
      RW(ind+1)=slengmax_ff
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('RMAXCYLRAD          ',IW(ind+3),4,20)
      RW(ind+1)=rmax_ff
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('ZMIN                ',IW(ind+3),4,20)
      RW(ind+1)=zmin_ff
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('ZMAX                ',IW(ind+3),4,20)
      RW(ind+1)=zmax_ff
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('NOFIELD             ',IW(ind+3),4,20)
      RW(ind+1)=CODE_LOG_KEY
      if(nofield) then
         IW(ind+2)=-1
      else
         IW(ind+2)=0
      endif
      ind=ind+ncol_gpar
      if(Eflag.GT.0) then
         call UCTOH('BEAM ENERGY         ',IW(ind+3),4,20)
         RW(ind+1)=Beam_energy_ff(1)
         IW(ind+2)=0
         ind=ind+ncol_gpar
      endif
      call UCTOH('BEAM POSITION(X)    ',IW(ind+3),4,20)
      RW(ind+1)=Beam_offset_ff(1)
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('BEAM POSITION(Y)    ',IW(ind+3),4,20)
      RW(ind+1)=Beam_offset_ff(2)
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('BEAM SIGMA          ',IW(ind+3),4,20)
      RW(ind+1)=Beam_sigma_ff
      IW(ind+2)=0
      ind=ind+ncol_gpar
c
c  ffky_mg.F
c
      call UCTOH('MAGTYPE             ',IW(ind+3),4,20)
      RW(ind+1)=0
      IW(ind+2)=magtype
      ind=ind+ncol_gpar
      call UCTOH('TORUS_FIELD         ',IW(ind+3),4,20)
      RW(ind+1)=torus_field
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('MAGSCALE_TORUS      ',IW(ind+3),4,20)
      RW(ind+1)=magscale(1)
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('MAGSCALE_MINITORUS  ',IW(ind+3),4,20)
      RW(ind+1)=magscale(2)
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('IFIELD              ',IW(ind+3),4,20)
      RW(ind+1)=0
      IW(ind+2)=init_ifield
      ind=ind+ncol_gpar
      call UCTOH('FIELDMAX            ',IW(ind+3),4,20)
      RW(ind+1)=init_fieldm
      IW(ind+2)=0
      ind=ind+ncol_gpar
c
c  ffky_ptg.F:
c
      call UCTOH('PTG_SCALE(1)        ',IW(ind+3),4,20)
      RW(ind+1)=ptg_scale
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('PTG_ANGLE(2)        ',IW(ind+3),4,20)
      RW(ind+1)=ptg_angle(1)
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('PTG_ANGLE(3)        ',IW(ind+3),4,20)
      RW(ind+1)=ptg_angle(2)
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('PTG_ANGLE           ',IW(ind+3),4,20)
      RW(ind+1)=ptg_angle(3)
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('PTG_IFIELD          ',IW(ind+3),4,20)
      RW(ind+1)=0
      IW(ind+2)=ptg_ifield
      ind=ind+ncol_gpar
      call UCTOH('PTG_FIELDMAX        ',IW(ind+3),4,20)
      RW(ind+1)=ptg_fieldm
      IW(ind+2)=0
      ind=ind+ncol_gpar
c
c  ffky_tg.F:
c
      call UCTOH('TG_TYPE             ',IW(ind+3),4,20)
      RW(ind+1)=0.
      IW(ind+2)=tg_type_ff
      ind=ind+ncol_gpar
      call UCTOH('TG_MATERIAL         ',IW(ind+3),4,20)
      if(tg_mate_ff.GE.-1 .AND. tg_mate_ff.LT.100) then
         RW(ind+1)=0.
         IW(ind+2)=tg_mate_ff
      else
         RW(ind+1)=CODE_ASCII_KEY
         IW(ind+2)=tg_mate_ff
      endif
      ind=ind+ncol_gpar
      call UCTOH('TG_CELLFLAG         ',IW(ind+3),4,20)
      if(tg_cell_ff.GE.-1 .AND. tg_cell_ff.LE.2) then
         RW(ind+1)=0.
         IW(ind+2)=tg_cell_ff
      else
         RW(ind+1)=CODE_ASCII_KEY
         IW(ind+2)=tg_cell_ff
      endif
      ind=ind+ncol_gpar
      call UCTOH('TG_POSITION(Z)      ',IW(ind+3),4,20)
      RW(ind+1)=tg_pos_ff(3)
      IW(ind+2)=0
      ind=ind+ncol_gpar
      if(tg_type_ff.NE.0) then
         call UCTOH('TG_LENGTH           ',IW(ind+3),4,20)
         RW(ind+1)=tg_user_ff(1)
         IW(ind+2)=0
         ind=ind+ncol_gpar
         call UCTOH('TG_DIAMETER         ',IW(ind+3),4,20)
         RW(ind+1)=tg_user_ff(2)
         IW(ind+2)=0
         ind=ind+ncol_gpar
         call UCTOH('TG_CELL_THICKNESS   ',IW(ind+3),4,20)
         RW(ind+1)=tg_user_ff(3)
         IW(ind+2)=0
         ind=ind+ncol_gpar
         call UCTOH('TG_SUPPORT_TYPE     ',IW(ind+3),4,20)
         RW(ind+1)=tg_user_ff(4)
         IW(ind+2)=0
         ind=ind+ncol_gpar
c   get additional user defined target geometry values
         if(tg_type_ff.EQ.-1) then
            call UCTOH('TG_USER_FF(5)    ',IW(ind+3),4,20)
            RW(ind+1)=tg_user_ff(5)
            IW(ind+2)=0
            ind=ind+ncol_gpar
            call UCTOH('TG_USER_FF(6)    ',IW(ind+3),4,20)
            RW(ind+1)=tg_user_ff(6)
            IW(ind+2)=0
            ind=ind+ncol_gpar
         endif
      endif
c
c  ffky_cc.F:
c
      do i=1,5
         write(my_string,'(A8,I1,A)') 'CC_CUTS(',i,')          '
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1)=cc_cuts(i)
         IW(ind+2)=0
         ind=ind+ncol_gpar
      enddo
c
c  ffky_dc.F:
c
      do i=1,5
         write(my_string,'(A8,I1,A)') 'DC_CUTS(',i,')          '
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1)=dc_cuts(i)
         IW(ind+2)=0
         ind=ind+ncol_gpar
      enddo
      call UCTOH('DRIFT2TIME          ',IW(ind+3),4,20)
      RW(ind+1)=dc_dist_to_time
      IW(ind+2)=0
      ind=ind+ncol_gpar
      do i=1,6
         write(my_string,'(A12,I1,A)') 'DC_RESOLUTION (SL',i,')'
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1)=dc_resolution(i)
         IW(ind+2)=0
         ind=ind+ncol_gpar
      enddo
c
c  ffky_ec.F:
c
      do i=1,5
         write(my_string,'(A8,I1,A)') 'EC_CUTS(',i,')          '
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1)=ec_cuts(i)
         IW(ind+2)=0
         ind=ind+ncol_gpar
      enddo
      call UCTOH('EC_ATLEN            ',IW(ind+3),4,20)
      RW(ind+1)=atlen
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('EC_TDC2CHANNEL      ',IW(ind+3),4,20)
      RW(ind+1)=0.0
      IW(ind+2)=ec_tdc_to_channel
      ind=ind+ncol_gpar
      call UCTOH('EC_POISS            ',IW(ind+3),4,20)
      RW(ind+1)=poiss
      IW(ind+2)=0
      ind=ind+ncol_gpar
c
c  ffky_ec1.F:
c
      do i=1,5
         write(my_string,'(A9,I1,A)') 'EC1_CUTS(',i,')         '
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1)=ec1_cuts(i)
         IW(ind+2)=0
         ind=ind+ncol_gpar
      enddo
      call UCTOH('EC1_ATTENUATION     ',IW(ind+3),4,20)
      RW(ind+1)=0.0
      IW(ind+2)=iatten
      ind=ind+ncol_gpar
      call UCTOH('EC1_THRESHOLD       ',IW(ind+3),4,20)
      RW(ind+1)=0.0
      IW(ind+2)=ithreshold
      ind=ind+ncol_gpar
      call UCTOH('EC1_DELAY           ',IW(ind+3),4,20)
      RW(ind+1)=delay
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('EC1_PHOTOELEC       ',IW(ind+3),4,20)
      RW(ind+1)=photoel
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('EC1_CHARGE2ADC      ',IW(ind+3),4,20)
      RW(ind+1)=charge_to_adc
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('EC1_REFLECT         ',IW(ind+3),4,20)
      RW(ind+1)=reflect
      IW(ind+2)=0
      ind=ind+ncol_gpar
c
c  ffky_sc.F:
c
      do i=1,5
         write(my_string,'(A8,I1,A)') 'SC_CUTS(',i,')          '
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1)=sc_cuts(i)
         IW(ind+2)=0
         ind=ind+ncol_gpar
      enddo
c
c  ffky_st.F:
c
      do i=1,5
         write(my_string,'(A8,I1,A)') 'ST_CUTS(',i,')          '
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1)=st_cuts(i)
         IW(ind+2)=0
         ind=ind+ncol_gpar
      enddo
c
c  ffky_foil.F:
c
      do i=1,7
         write(my_string,'(A13,I1,A)') 'FOIL_BAFFLES(',i,')     '
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1)=foil_baffles(i)
         IW(ind+2)=0
         ind=ind+ncol_gpar
      enddo
c
c  ffky_bckgr.F
      call UCTOH('BCKGR_NTARGET       ',IW(ind+3),4,20)
      RW(ind+1)=0.0
      IW(ind+2)=ntarget
      ind=ind+ncol_gpar
      call UCTOH('BCKGR_LUMEN         ',IW(ind+3),4,20)
      RW(ind+1)=olumen
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('BCKGR_ZTAR          ',IW(ind+3),4,20)
      RW(ind+1)=zelem
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('BCKGR_ATAR          ',IW(ind+3),4,20)
      RW(ind+1)=atom
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('BCKGR_RHOT          ',IW(ind+3),4,20)
      RW(ind+1)=rho
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('BCKGR_TLENG         ',IW(ind+3),4,20)
      RW(ind+1)=tleng
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('BCKGR_TIMEDC        ',IW(ind+3),4,20)
      RW(ind+1)=timedc
      IW(ind+2)=0
      ind=ind+ncol_gpar
      call UCTOH('BCKGR_NINCEL        ',IW(ind+3),4,20)
      RW(ind+1)=0.0
      IW(ind+2)=nincel
      ind=ind+ncol_gpar
c
c  (4) GEANT parameters  (cf. gffgo.F)
c
c  run control, opti flag, random numbers
      my_string='IDRUN               '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = idrun
      ind=ind+ncol_gpar
      my_string='IDEVT               '         ! first user event number
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = idevt
      ind=ind+ncol_gpar   
      my_string='NEVENT (TRIG)       '         ! total number of events to process
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = nevent
      ind=ind+ncol_gpar
      my_string='IOPTIM              '         ! automatic optimisation of geom.settings
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = ioptim
      ind=ind+ncol_gpar
      my_string='IGAUTO              '         ! automatic computation of DEEMAX,STMIN,TMAXFD,STEMAX
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = igauto
      ind=ind+ncol_gpar
      my_string='ISTACK ORDERING     '         ! stack ordering flag (SORD)
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = istord
      ind=ind+ncol_gpar
      do i=1,2
         write(my_string,'(A5,I1,A)') 'RNDM(',i,')             '
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = 0.0
         IW(ind+2) = nrndm(i)
         ind=ind+ncol_gpar
      enddo
      my_string='IKINE               '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = ikine
      ind=ind+ncol_gpar
      do i=1,10
         write(my_string,'(A6,I2.2,A)')'PKINE(',i,')           '
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = pkine(i)
         IW(ind+2) = 0
         ind=ind+ncol_gpar
      enddo
c
c  process flags
      my_string='IANNIHILATION       '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = ianni
      ind=ind+ncol_gpar
      my_string='IBREMSSTRAHLUNG     '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = ibrem
      ind=ind+ncol_gpar
      my_string='ITCKOV PHOTON GENER '          !Cerenkov photon generation
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = itckov
      ind=ind+ncol_gpar
      my_string='ICOMPTONSCATTERING  '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = icomp
      ind=ind+ncol_gpar
      my_string='IDECAY              '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = idcay
      ind=ind+ncol_gpar
      my_string='IDELTA RAY          '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = idray
      ind=ind+ncol_gpar
      my_string='IHADRONIC PROCESSES '         !hadronic process
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = ihadr
      ind=ind+ncol_gpar
      my_string='ILABSORP.(CER.LIGHT)'         !Cerenkov light absorption
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = ilabs
      ind=ind+ncol_gpar
      my_string='ILOSS (ENERGY)      '          !energy loss
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = iloss
      ind=ind+ncol_gpar
      my_string='IMULSCATTERING      '          !multiple scattering
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = imuls
      ind=ind+ncol_gpar
      my_string='IMU NUCLEAR INTERACT'          !muon nuclear interaction
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = imunu
      ind=ind+ncol_gpar
      my_string='IPAIRPRODUCTION     '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = ipair
      ind=ind+ncol_gpar
      my_string='IPHOTONFISSION      '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = ipfis
      ind=ind+ncol_gpar
      my_string='IPHOTOEFFECT        '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = iphot
      ind=ind+ncol_gpar
      my_string='IRAYLEIGH           '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = irayl
      ind=ind+ncol_gpar
      my_string='IENERGYFLUCT. MODEL '         !energy fluctuation model
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = istra
      ind=ind+ncol_gpar
      my_string='ISYNCHR. RADIAT.    '         !synchrotron radiation generation
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = 0.0
      IW(ind+2) = isync
      ind=ind+ncol_gpar
c
c  cut parameters
c
      my_string='CUTGAMMA            '         !kin.energy cuts:
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = cutgam
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      my_string='CUTELEC             '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = cutele
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      my_string='CUTNEUTRALS         '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = cutneu
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      my_string='CUTHADRONS          '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = cuthad
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      my_string='CUTMUONS            '
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = cutmuo
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      my_string='BCUTELEC (BREMSSTR.)'         ! electron bremsstrahlung
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = bcute
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      my_string='BCUTMUON (BREMSSTR.)'         ! muon bremsstrahlung
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = bcutm
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      my_string='DCUTELEC (DELTA RAY)'         ! delta-rays by electrons
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = dcute
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      my_string='DCUTMUON (DELTA RAY)'         ! delta-rays by muons
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = dcutm
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      my_string='PPCUTM  (PAIR PROD.)'         ! energy cut for direct pair prod.
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = ppcutm
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      my_string='TOFMAX              '         ! max. time of flight in seconds
      call UCTOH(my_string,IW(ind+3),4,20)
      RW(ind+1) = tofmax
      IW(ind+2) = 0
      ind=ind+ncol_gpar
      do i=1,5
         write(my_string,'(A6,I1,A)')'GCUTS(',i,')            '  ! user cuts
         call UCTOH(my_string,IW(ind+3),4,20)
         RW(ind+1) = gcuts(i)
         IW(ind+2) = 0
         ind=ind+ncol_gpar
      enddo
      call blist(IW,'E+','GPAR')
c
c  check the entries  (bprint!=0)
c
      if(bprint.EQ.0) RETURN

      ind=MAMIND(IW,'GPAR')
      ind=IW(ind)
      ncol_gpar=IW(ind-5)
      nrow_gpar=IW(ind-4)
      write(6,'(A,3I10)')' BOSbank "GPAR": index/ncol/nrow',
     &                                  ind,ncol_gpar,nrow_gpar
      do j=1,nrow_gpar
         if(RW(ind+1).EQ.CODE_ASCII_KEY) then
            write(6,'(2X,A9,9X,A4,3X,5A4)')'ASCCI KEY',(IW(ind+i),i=2,7)
         elseif(RW(ind+1).EQ.CODE_CREATION_TIME) then
            write(6,'(2X,A12,I10,1X,5A4)')'CREAT. TIME ',(IW(ind+i),i=2,7)
         elseif(RW(ind+1).EQ.CODE_ACCESS_TIME) then
            write(6,'(2X,A12,I10,1X,5A4)')'ACCESS TIME ',(IW(ind+i),i=2,7)
         elseif(RW(ind+1).EQ.CODE_LOG_KEY) then
            write(6,'(2X,A8,I10,5X,5A4)') 'LOG.VAR.',(IW(ind+i),i=2,7)
         else
            write(6,'(F14.8,I10,1X,5A4)') RW(ind+1),(IW(ind+i),i=2,7)
         endif
         ind=ind+ncol_gpar
      enddo

      end

