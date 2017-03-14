	subroutine TRKbosfill
*
      IMPLICIT NONE
      
      SAVE
      include "bcs.inc"
*
      include "clas_offsets.inc"
      include "clas_index.inc"
      include "ntpl_com.inc"
*
      integer nrow,j
* Local var
      REAL *4  Xl,Yl,CXl,CYl
      INTEGER*4 iCALL, iTRGS, iHBTR, iTBTR
     &         ,indCALL, indTRGS, indHBTR, indTBTR
     &         ,indHDPL, iHDPL, nrowHDPL 
     &         ,indTDPL, iTDPL, nrowTDPL,sectorHDPL,sectorTDPL
     &         ,nCALL, idCALL

      INTEGER*4 namind
c      LOGICAL   firsttime,eof
c      DATA      firsttime/.TRUE./

c  get links to all needed banks 

         indCALL  = namind('CALL')
         indTRGS  = namind('TRGS')
         indHBTR  = namind('HBTR')
         indTBTR  = namind('TBTR')
         
c       print *,iHEAD, iBMPR,iTRGS,iHBTR,iTBTR

c
c get CALL bank
c
       iCALL = IW16(indCALL*2)
       RF1   = 0
       RF2   = 0
       if ( iCALL .ne. 0 ) then
         nCALL    = IW(iCALL -4)
         nrow     = IW(iCALL -5)
         DO j=1,nCALL
           IDcall  = iw16(2*iCALL+CALL_ID+nrow*(j-1))
           if (IDcall.eq.6) then
             RF1   = iw16(2*iCALL+CALL_TDC+nrow*(j-1))
           else if (IDcall.eq.7) then
             RF2   = iw16(2*iCALL+CALL_TDC+nrow*(j-1))
           end if
         END DO
c         print *,' CALL ',nevent,nCALL,nrow,RF1,RF2
c         read *
       else
       end if
c
c get TRGS bank       
c
       iTRGS = IW(indTRGS)
       if ( iTRGS .ne. 0 ) then
c         TRGS_yes = .true.
         clock_ug = iw(iTRGS+TRGS_CLOCK_UG)
         fcup_ug  = iw(iTRGS+TRGS_FCUP_UG)
         clock_g1 = iw(iTRGS+TRGS_CLOCK_G1)
         fcup_g1  = iw(iTRGS+TRGS_FCUP_G1)
         clock_g2 = iw(iTRGS+TRGS_CLOCK_G2)
         fcup_g2  = iw(iTRGS+TRGS_FCUP_G2)
	 
c         print *,' TRGS ',event_num,event_clas,clock_ug,fcup_ug,clock_g2,fcup_g2
       else
c         TRGS_yes = .false.
       end if

c
c get HBTR bank       
c
       iHBTR = IW(indHBTR)
       if ( iHBTR .ne. 0 ) then
         nHBTR    = IW(iHBTR -4)
         nrow     = IW(iHBTR -5)
c	 print * , 'nHBTR=',nHBTR
         do j=1,nHBTR
           x_hbtr (j)   = rw(iHBTR+HBTR_X      +nrow*(j-1))
           y_hbtr (j)   = rw(iHBTR+HBTR_Y      +nrow*(j-1))
           z_hbtr (j)   = rw(iHBTR+HBTR_Z      +nrow*(j-1))
           px_hbtr(j)   = rw(iHBTR+HBTR_PX     +nrow*(j-1))
           py_hbtr(j)   = rw(iHBTR+HBTR_PY     +nrow*(j-1))
           pz_hbtr(j)   = rw(iHBTR+HBTR_PZ     +nrow*(j-1))
           q_hbtr (j)   = rw(iHBTR+HBTR_Q      +nrow*(j-1))
           chi2_hbtr(j) = rw(iHBTR+HBTR_CHI2   +nrow*(j-1))
           itr_hbtr (j) = iw(iHBTR+HBTR_ITR_SEC+nrow*(j-1))
         end do
       else
       end if
c
c get TBTR bank
c
       iTBTR = IW(indTBTR)
       if ( iTBTR .ne. 0 ) then
         nTBTR   = IW(iTBTR -4)
         nrow    = IW(iTBTR -5)
         DO j=1,nTBTR
           x_tbtr  (j)  = rw(iTBTR+TBTR_X           +nrow*(j-1))
           y_tbtr  (j)  = rw(iTBTR+TBTR_Y           +nrow*(j-1))
           z_tbtr  (j)  = rw(iTBTR+TBTR_Z           +nrow*(j-1))
           px_tbtr (j)  = rw(iTBTR+TBTR_PX          +nrow*(j-1))
           py_tbtr (j)  = rw(iTBTR+TBTR_PY          +nrow*(j-1))
           pz_tbtr (j)  = rw(iTBTR+TBTR_PZ          +nrow*(j-1))
           q_tbtr   (j) = rw(iTBTR+TBTR_Q           +nrow*(j-1))
           chi2_tbtr(j) = rw(iTBTR+TBTR_CHI2        +nrow*(j-1))
           itr_tbtr (j) = iw(iTBTR+TBTR_ITR_SEC     +nrow*(j-1))
           itr_hbt_tbtr(j) = iw(iTBTR+TBTR_ITR_HBT  +nrow*(j-1))
         end DO
       else
       end if
       
C--------------------------------------------------------------------
c  get links to HDPL banks (different banks for different sectors)
C--------------------------------------------------------------------
       indHDPL = namind('HDPL')
       if (indHDPL.ne.0) then 
         iHDPL   = indHDPL + 1
         nHDPL = 0
332       continue 
         iHDPL = IW (iHDPL-1)                        ! next index (sector)
         If (iHDPL.EQ.0) goto 992
C--------------------------------------------------------------------
C  track sector number 
C--------------------------------------------------------------------
         sectorHDPL = IW(iHDPL -2)
C--------------------------------------------------------------------
C  number of tracks (record blocks) in this bank (sector)
C--------------------------------------------------------------------
         nrowHDPL   = IW(iHDPL -4) ! number of plains (=10)
C--------------------------------------------------------------------
C  Plane #10 (TRK_PLN_HDPL=46) corrisponds to LAC
         DO j=1,nrowHDPL
           nHDPL = nHDPL +1
 	 if(nHDPL.gt.200) then 
	   print *,'nHDPL: too many tracks (>20)! Event',nevent, ' skipped'
           goto 992
	 end if
	   sectHDPL (nHDPL) = sectorHDPL 	
           TRK_PLN_HDPL (nHDPL) = iw(iHDPL+HDPL_TRK_PLN+8*(j-1))
           Xl               = rw(iHDPL+HDPL_X   +8*(j-1))
           Yl               = rw(iHDPL+HDPL_Y   +8*(j-1))
           Z_HDPL   (nHDPL) = rw(iHDPL+HDPL_Z   +8*(j-1))
           CXl              = rw(iHDPL+HDPL_CX  +8*(j-1))
           CYl              = rw(iHDPL+HDPL_CY  +8*(j-1))
           CZ_HDPL  (nHDPL) = rw(iHDPL+HDPL_CZ  +8*(j-1))
           TLEN_HDPL(nHDPL) = rw(iHDPL+HDPL_TLEN+8*(j-1))
C--------------------------------------------------------------------
C	Rotation to CLAS coordinate system
C--------------------------------------------------------------------
c	   X_HDPL(nHDPL) = Xl *cos((sectorHDPL-1)*3.1416/3.) -
c     >		           Yl *sin((sectorHDPL-1)*3.1416/3.)	
c	   Y_HDPL(nHDPL) = Xl *sin((sectorHDPL-1)*3.1416/3.) +
c     >	       	           Yl *cos((sectorHDPL-1)*3.1416/3.)	
c	   CX_HDPL(nHDPL)= CXl*cos((sectorHDPL-1)*3.1416/3.) -
c     >		           CYl*sin((sectorHDPL-1)*3.1416/3.)	
c	   CY_HDPL(nHDPL)= CXl*sin((sectorHDPL-1)*3.1416/3.) +
c     >	       	           CYl*cos((sectorHDPL-1)*3.1416/3.)	
	   X_HDPL(nHDPL) = Xl
	   Y_HDPL(nHDPL) = Yl
	   CX_HDPL(nHDPL)= CXl
	   CY_HDPL(nHDPL)= CYl        
         ENDDO         ! next track
         goto 332      ! next sector
C--------------------------------------------------------------------
992       continue                           ! end HDPL section
      else
      end if       
C--------------------------------------------------------------------
c  get links to TDPL banks (different banks for different sectors)
C--------------------------------------------------------------------
       indTDPL = namind('TDPL')
       if (indTDPL.ne.0) then 
         iTDPL   = indTDPL + 1
         nTDPL = 0
333       continue 
         iTDPL = IW (iTDPL-1)                        ! next index
         If (iTDPL.EQ.0) goto 993
C--------------------------------------------------------------------
C  track sector number 
C--------------------------------------------------------------------
         sectorTDPL = IW(iTDPL -2)
C--------------------------------------------------------------------
C  number of tracks (record blocks) in this bank (sector)
C--------------------------------------------------------------------
         nrowTDPL   = IW(iTDPL -4)
C--------------------------------------------------------------------
         DO j=1,nrowTDPL
           nTDPL = nTDPL +1
c	   print *,'nTDPL',nTDPL
           sectTDPL (nTDPL) = sectorTDPL 	
           TRK_PLN_TDPL (nTDPL) = iw(iTDPL+TDPL_TRK_PLN+8*(j-1))
           Xl               = rw(iTDPL+TDPL_X   +8*(j-1))
           Yl               = rw(iTDPL+TDPL_Y   +8*(j-1))
           Z_TDPL   (nTDPL) = rw(iTDPL+TDPL_Z   +8*(j-1))
           CXl              = rw(iTDPL+TDPL_CX  +8*(j-1))
           CYl              = rw(iTDPL+TDPL_CY  +8*(j-1))
           CZ_TDPL  (nTDPL) = rw(iTDPL+TDPL_CZ  +8*(j-1))
           TLEN_TDPL(nTDPL) = rw(iTDPL+TDPL_TLEN+8*(j-1))

C--------------------------------------------------------------------
C	Rotation to CLAS coordinate system
C--------------------------------------------------------------------
c	   X_TDPL(nTDPL) = Xl *cos((sectorTDPL-1)*3.1416/3.) -
c     >		           Yl *sin((sectorTDPL-1)*3.1416/3.)	
c	   Y_TDPL(nTDPL) = Xl *sin((sectorTDPL-1)*3.1416/3.) +
c     >	       	           Yl *cos((sectorTDPL-1)*3.1416/3.)	
c	   CX_TDPL(nTDPL)= CXl*cos((sectorTDPL-1)*3.1416/3.) -
c     >		           CYl*sin((sectorTDPL-1)*3.1416/3.)	
c	   CY_TDPL(nTDPL)= CXl*sin((sectorTDPL-1)*3.1416/3.) +
c     >	       	           CYl*cos((sectorTDPL-1)*3.1416/3.)	

	   X_TDPL(nTDPL) = Xl
	   Y_TDPL(nTDPL) = Yl
	   CX_TDPL(nTDPL)= CXl
	   CY_TDPL(nTDPL)= CYl
         ENDDO         ! next track
         goto 333      ! next sector
C--------------------------------------------------------------------
993       continue                           ! end TDPL section
      else
      end if
      
      return 
      end    
