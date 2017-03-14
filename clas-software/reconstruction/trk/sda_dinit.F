      SUBROUTINE sda_dinit
*_begin_doc
*  RCS ID string
*  $Id: sda_dinit.F,v 1.23 2008/04/21 00:52:02 fklein Exp $
*
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads initial dictionary (templets).
C-
C-   Inputs  : prlink file...? 
C-   Outputs : ???
C-   Controls: ???
C-
C-   Library belongs: libsda.a
C-
C-   Calls: sda_flags
C-
C-   Created   16-MAR-1995   Bogdan Niczyporuk
C-   Modified  27-MAR-1997   Curtis A. Meyer
C-             Use tcl-controlled variables to pick up the name and
C-             environment of the prlink file.
C-   Modified  24-JUN-1997   Richard A Thompson
C-             Added segment road width determination.
C-             Width(num_lay,edge) is an array that contains the width of the 
C-             segment road upstream (Width(lay,1)) and downstream 
C-             (Width(lay,2)).  This is used in ana_segm to scan for segment
C-             candidates. 
C-   Modified  AUG-1997  F.Klein
C-             preliminary: reorganize the road directory in order to start
C-             with Region 2 (SL3&SL4). As I only produced new dictionaries
C-             for 25%I_torus & 75%I_mini, this might be a way to use 'elder'
C-             dictionaries which all were organized depending on SL1.
C-             In order to improve the linking procedure by 1) matching all
C-             axial superlayers & collecting matching entries in stereo
C-             sly.s and 2) matching all stereo sly.s & collecting
C-             matching entries in axial sly.s (in the case that the hit
C-             combination doesn't have segments in all axial sly.s),
C-             the road dictionary is reorganized twice: wrt. SL3 & SL4. 
C-   Modified  Jan-1998 J. J. Manak
C-             does not look up prlink file name anymore - that is now done in 
C-             a seperate routine - simplified the logic a bit - program just quits
C-             if it cannot find the prlink file it needs.
C-   Modified  AUG-1999  F.Klein
C-             Read dictionary via FPACK routines, not BOS,
C-             allows to increase the array size.
C-    Called by trk_brun
C-
*_end_doc
*
      Implicit None
      Save
*
*
*_begin_inc
*  include files :
*  ---------------------
*
#include "analink.inc"
*--c.a.meyer march 1997
#include "trktcl_var.inc"
*--
*
*_end_inc
*
*_begin_var
*  input/output variables:
*  -----------------------
*
*  Local pre-defined variables:
*  ---------------------------
*  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER ( CFILE = '$RCSfile: sda_dinit.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.23 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 2008/04/21 00:52:02 $')
      PARAMETER ( CAUTHO = '$Author: fklein $')
      DATA CRCSID/
     1'$Id: sda_dinit.F,v 1.23 2008/04/21 00:52:02 fklein Exp $
     3cmeyer'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='SDA_DINIT')
      PARAMETER (CRAUTH='Curtis A. Meyer')
*
*  Local User defined variables:
*  -----------------------------
*
C External Functions
      Integer  lenocc
C
C Local variables
      Integer  Ilun, Iret
      Logical  Lexist
      Character*7 Bopen
*
      INTEGER i, j, nroads, ierr, md1, md2, md3, md1old
      integer nrw1, nrw2, nrw3, nrw4
      integer ir, ir2, i1, i2, it, m1,m2
      integer ilay, iseg, negcnt
      integer myflg              !flag to decide whether 2nd record can be used
      REAL    pscale,pcorr
      CHARACTER*256 fname
      CHARACTER*60  locname
      CHARACTER*24 utimes
C
      PARAMETER (md1 = 1+6*max_npat+2+ns2+max_npat)
      PARAMETER (md2 = 2*ns2*max_lnk)
      PARAMETER (md3 = ns2*max_lnk)
      PARAMETER (md1old = 675)
C
      INTEGER d1(md1), d2(md2), d3(md3), d4(md3)
      EQUIVALENCE (d1(1),npat)

      CHARACTER*8 kname,hname,format
      INTEGER num,numra,numrb,ncol,nrow,nch,icl
      INTEGER mylink1, mylink2, mylink3
      INTEGER*2 mlink3(2)
      BYTE    mblink1(4), mblink2(4)
      EQUIVALENCE (mblink1(1),mylink1),(mblink2(1),mylink2),(mlink3(1),mylink3)

C ipar_trk_make_prlink = 0   Read only (no update)
C ipar_trk_make_prlink = 1   Write only (1-st time: generate new templets)
C ipar_trk_make_prlink = 2   Both Read & Write (read and write updated templets)

      call vzero(ipat(1,1),6*max_npat)
      call vzero(lnk(1,1),2*ns2)
      call vzero(link1(1,1,1),2*ns2*max_lnk)
      call vzero(link2(1,1,1),2*ns2*max_lnk)
      call vzero(link3(1,1,1),2*ns2*max_lnk)
      call vzero(link4(1,1,1),2*ns2*max_lnk)
      IF(ipar_trk_make_prlink.EQ.1) THEN
        npat = 1
      ENDIF
C
C ipar_trk_make_prlink: 0 or 2 is Read-Only or Read-Write (Update)

      IF(ipar_trk_make_prlink.EQ.0 .OR. ipar_trk_make_prlink.EQ.2) THEN
        nrw1=0
        nrw2=0
        nrw3=0
        nrw4=0
        locname=spar_prlink_name

        call revinm(spar_prlink_loc,locname,fname)
        call RecMes(CRNAME,'I','PRLINK File: '//fname)
        INQUIRE(FILE=fname, IOSTAT=iret, EXIST=lexist)
        if(.NOT.lexist) then
           call RecMes(CRNAME,'F','no PRLINK-File found -> Program stops!!')
        endif
        call ResLun(CRNAME,Ilun,Iret)
        Write(Bopen,'(A5,I2.2)')'UNIT=',Ilun
        call fparm(' OPEN PRLINK ' // Bopen // ' FILE="' // fname // '" ')
        call frname('PRLINK')
        call frkey(kname,numra,numrb,icl,ierr)
        if(ierr.ne.0) then
           write(CRMESS,'(A,I6)')' Error in FRKEY =',ierr
           call recmes(CRNAME,'W',crmess)
        endif
        do i=1,3
           CALL frhdr(hname,num,ncol,nrow,format,nch,ierr)
           if(ierr.ne.0) then
              write(CRMESS,'(A,I6)')' Error in FRHDR =',ierr
              call recmes(CRNAME,'W',crmess)
           endif
           ierr=-1
           if(hname(1:4).EQ.'LSEG') then
             ierr=0
             if(ncol.GT.md1) ierr=md1
             call frdat(nrw1,d1,md1)
           elseif(hname(1:4).EQ.'LAPS') then
             ierr=0
             if(ncol.GT.md2) ierr=md2
             call frdat(nrw2,d2,md2)
           elseif(hname(1:4).EQ.'LMOM') then
             ierr=0
             if(ncol.GT.md3) ierr=md3
             call frdat(nrw3,d3,md3)
           endif
c
c  if number_of_roads (links) in dictionary > array limit 
c  we have to stop, otherwise the arrays are screwed up
           if(ierr.NE.0) then
             write(CRMESS,'(2A,2(A,I6))')' Mismatch: data ',hname,
     &            'in PRLINK:',ncol,
     &             '  required:',ierr
             call recmes(CRNAME,'F',crmess)
           endif
        enddo
c 2nd record?
        myflg=0
        i=0
        call frkey(kname,numra,numrb,icl,ierr)
        if(ierr.eq.0) then
          do while (ierr.ne.-1 .and. i.lt.15) 
            i=i+1
            CALL frhdr(hname,num,ncol,nrow,format,nch,ierr)
            if(ierr.gt.0) then
              write(CRMESS,'(A,I6)')' Error in FRHDR =',ierr
              call recmes(CRNAME,'W',crmess)
            elseif(ierr.eq.0 .and. hname(1:4).EQ.'LPS1') then
              call frdat(nrw4,d4,md3)
              myflg=-1
              if(ncol.GT.md3) then
                write(CRMESS,'(2A,2(A,I6))')' Mismatch: data ',hname,
     &            'in PRLINK (2nd record):',ncol,
     &             '  required:',md3
                call recmes(CRNAME,'W',crmess)
                myflg=1
              endif
            endif
          enddo
        endif
        call fparm('CLOSE PRLINK')
        Close(Ilun)
        Call ResLun('Sda_Dinit',-Ilun,Iret)

        write(CRMESS,'(2A)')' Read PRLINK file:',fname(1:lenocc(fname))
        call recmes(CRNAME,'L',crmess)
c
c  If road file has old length, shift some elements up to take into account
c     the new length of the lpat array and fill in default values for minsl
        if(nrw1.eq.md1old)then
           do j=nrw1,602,-1
              d1(j+6*(max_npat-100))=d1(j)
           enddo
           do j=1,npat
              minsl(j)=0
           enddo
        else
c  assume that minsl array is not part of the fpack record if myflg=-1 (p_scale)
          dtyp=d1(nrw1-ns2-1)
          utime=d1(nrw1-ns2)
          if(dtyp.gt.0 .and. dtyp.lt.16 .and. 
     &                         utime.ge.500 .and. utime.le.4000) then
            myflg=myflg*2
            do j=1,ns2
              i1=nrw1-ns2+j
              lnk(j,1)=d1(i1)
            enddo
          else
            do j=npat,ns2+1,-1
              minsl(j)=minsl(j-ns2)
            enddo
            do j=1,ns2
              minsl(j)=lnk(j,2)
            enddo
          endif
        endif
c
c  linking wrt. track segments in Superlayer3 & Superlayer4

        if (ipar_trk_startlink.GT.3) then
	  ipar_trk_startlink=2		!there are only 3 DC-Regions!
	endif
        if(ipar_trk_print(1).EQ.1) then
          nroads = 0
          do j = 1,ns2
             nroads = nroads + lnk(j,1)
          enddo
          write(6,*)nroads,' Links in SL1 ; start_link=',ipar_trk_startlink
          do i1=1,ns2-15,16
             write(6,*)(lnk(i2,1),i2=i1,i1+15)
          enddo
        endif

        pscale=1.0
        if(myflg.ne.0 .and. ipar_torus_current.ne.0) then
           if(iabs(myflg).ne.2) utime=3400                    ! default
           pscale=iabs(ipar_torus_current)/float(utime)
           write(crmess,'(2(A,I6),A,F7.3)')' road map type:',dtyp,
     &             ', I_torus',utime,'A => p_scale=',pscale
           call recmes(crname,'L',crmess)
        endif
        if(myflg.lt.0) dtyp=-1
        call vzero(lnk(1,1),2*ns2)
        i1=0
        negcnt=0
c       i2=ns2*max_lnk
        i2=nrw3
        do ir=1,ns2
          do it=1,nrw3/ns2
            i1=i1+1
            i2=i2+1
            mylink1=d2(i1)
            mylink2=d2(i2)
            mylink3=d3(i1)
            if(mylink1.lt.0.or.mylink2.lt.0) then
c              print *,'it',it,mblink1(4),mblink2(4)
              mblink1(4)=0
              mblink2(4)=0
              negcnt=negcnt+1
            endif
            if(myflg.ne.0) then 
              pcorr=mlink3(2)
              mlink3(2)=INT(pcorr*pscale)
            endif
            do ir2=1,ns2
c  rewrite the dictionary wrt. SL3 or SL5:
              if (mblink1(ipar_trk_startlink).EQ.ir2.AND.lnk(ir2,1).LT.max_lnk) then
                lnk(ir2,1)=lnk(ir2,1)+1
                link1(lnk(ir2,1),ir2,1)=mylink1
                link2(lnk(ir2,1),ir2,1)=mylink2
                link3(lnk(ir2,1),ir2,1)=mylink3
                if(myflg.lt.0) link4(lnk(ir2,1),ir2,1)=d4(i1)
              endif
c  rewrite the dictionary wrt. SL4 or SL6:
              if (mblink2(ipar_trk_startlink).EQ.ir2.AND.lnk(ir2,2).LT.max_lnk) then
                lnk(ir2,2)=lnk(ir2,2)+1
                link1(lnk(ir2,2),ir2,2)=mylink1
                link2(lnk(ir2,2),ir2,2)=mylink2
                link3(lnk(ir2,2),ir2,2)=mylink3
                if(myflg.lt.0) link4(lnk(ir2,2),ir2,2)=d4(i1)
              endif
            enddo
          enddo
        enddo

        if(ipar_trk_print(1).EQ.1) then
           write(6,*)' removed SC info from',negcnt,' links'
          nroads = 0
          do j = 1,ns2
            nroads = nroads + lnk(j,1)
          enddo
          if(ipar_trk_startlink.EQ.1) then
            write(6,*)nroads,' New Links in SL',ipar_trk_startlink*2
          else
            write(6,*)nroads,' New Links in SL',ipar_trk_startlink*2-1
          endif
          do i1=1,ns2-15,16
            write(6,*)(lnk(i2,1),i2=i1,i1+15)
          enddo
          nroads = 0
          do j = 1,ns2
            nroads = nroads + lnk(j,2)
          enddo
          if(ipar_trk_startlink.EQ.1) then
            write(6,*)nroads,' New Links in SL',ipar_trk_startlink
          else
            write(6,*)nroads,' New Links in SL',ipar_trk_startlink*2
          endif
          do i1=1,ns2-15,16
            write(6,*)(lnk(i2,2),i2=i1,i1+15)
          enddo
        endif
        if(myflg.lt.0) then
           call recmes(crname,'I',
     & 'Read new road map including start parameters: q, d0, z0, theta0, phi0')
        endif

C check segment patterns 
        nroads=npat
        do j=nroads,1,-1
           i1=0
           m1=0
           do i=1,6
              i1=i1+iabs(ipat(i,j))
              if(iabs(ipat(i,j)).gt.m1) m1=iabs(ipat(i,j))
           enddo
           i1=i1-m1
           if(i1.lt.2) i1=2
           if(m1.gt.2*(i1)) then
              do it=j+1,npat
                 do i=1,6
                   ipat(i,it-1)=ipat(i,it)
                enddo
             enddo
             npat=npat-1
          endif
       enddo

C Update ipat_flg3, ipat_flg4
        CALL sda_flags(ipat,ipat_flg3,npat, 6,3)
        CALL sda_flags(ipat,ipat_flg4,npat, 6,4)

crat  get the segment road widths.  Initiliaze to zero first.
      do ilay=1,6
         Width(ilay,1)=0
         Width(ilay,2)=0
      end do
      do ilay=1,6
         do iseg = 1,npat
            Width(ilay,1) = Min( Width(ilay,1), ipat(ilay,iseg)) 
            Width(ilay,2) = Max( Width(ilay,2), ipat(ilay,iseg))
         end do
         Width(ilay,1) = -Width(ilay,1)
      end do
C Print the segment templates
       IF(ipar_trk_print(1).EQ.1) THEN
          WRITE(6,1)
          DO j = 1,npat
            WRITE(6,2) j,(ipat(i,j),i=1,6), ipat_flg3(j), ipat_flg4(j),minsl(j)
          ENDDO
	  write(6,'(A,12(I3,A2))') ' Width:',
     &			(width(i,1)*(-1),' <',width(i,2),' ;',i=1,6)
cc          nroads = 0
cc          DO j = 1,ns2
cc            nroads = nroads + lnk(j,1)
cc          ENDDO
cc          WRITE(6,3) nroads
cc          WRITE(6,4) (lnk(j,1),j=1,ns2)
cc          WRITE(6,*)
        ENDIF
      ENDIF

 1    FORMAT(/' ipat     la1  la2  la3  la4  la5  la6     flg3 flg4')
 2    FORMAT (I5,3X,6I5,3X,3I5)
 3    FORMAT(/'  Nroads =',I8)
 4    FORMAT (8I5)

 999  CONTINUE
      RETURN
      END

