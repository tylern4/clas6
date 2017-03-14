      SUBROUTINE EC1_FILL
c
c_begin_doc
c  RCS ID string
c  $Id: ec1_fill.F,v 1.7 1999/03/09 17:40:36 boiarino Exp $
c
c  Documentation for subroutine EC1_FILL
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
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EDT 1997
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
#include "ec1_pedstl.inc"
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
      PARAMETER (CFILE = '$RCSfile: ec1_fill.F,v $')
      PARAMETER (CREVIS = '$Revision: 1.7 $')
      PARAMETER (CSTATE = '$State: Exp $')
      PARAMETER (CDATE = '$Date: 1999/03/09 17:40:36 $')
      PARAMETER (CAUTHO = '$Author: boiarino $')
      DATA CRCSID/   
     1'$Id: ec1_fill.F,v 1.7 1999/03/09 17:40:36 boiarino Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='EC1_FILL')
      PARAMETER (CRAUTH='Francois Roudot')
c
c  Local User defined variables:
c  -----------------------------
      INTEGER NWRITE
      INTEGER ind,nami,mamind,Nrec,k,IDhist,j
      INTEGER id,layer,strip,sector,tdcL,adcL,tdcR,adcR
      INTEGER layer_ped
      INTEGER nxi,nxo,nyi,nyo
      INTEGER ixi,ixo,iyi,iyo
      INTEGER idXi(24),idXo(24),idYi(40),idYo(40)
      INTEGER tdcL_Xi(24),adcL_Xi(24),tdcR_Xi(24),adcR_Xi(24)
      INTEGER tdcL_Yi(40),adcL_Yi(40),tdcR_Yi(40),adcR_Yi(40)
      INTEGER tdcL_Xo(24),adcL_Xo(24),tdcR_Xo(24),adcR_Xo(24)
      INTEGER tdcL_Yo(40),adcL_Yo(40),tdcR_Yo(40),adcR_Yo(40)
      REAL weight
      DATA nami/0/
c_end_var
c
c  executable code for routine EC1_FILL:
c----6----------------------------------------------------------------72
c
      IF (NWRITE .LT. 1) THEN
        NWRITE = NWRITE + 1
        CRMESS='Filling LAC Histograms, this message written once'
        CALL RECMES(CRNAME,'I',CRMESS)
      ENDIF

c  First change PAWC directories
      call hcdir('//PAWC/EC1',' ')

c  Do your HFILLS below this line

c get reference to EC1 
c       If (nami.eq.0) 
       nami = mamind(iw,'EC1 ')
       ind = IW(nami)

 1101 CONTINUE
       nxi = 0
       nyi = 0
       nxo = 0
       nyo = 0

      IF(ind.le.0) goto 100
      
      Nrec=IWROW(ind)      
      sector = IW(ind-2)
      k = 2*ind             ! k is pointed before data words for iw16 array
      DO j = 1,Nrec
        k=k+1
        ID    = IW16(k)
        layer = ID/256
c---- convert layer numbering to one used by Harut
        if(layer.eq.1) layer_ped=2
        if(layer.eq.2) layer_ped=1
        if(layer.eq.3) layer_ped=4
        if(layer.eq.4) layer_ped=3
        strip = mod(ID,256)
        tdcL = IW16(k+1)
        adcL = IW16(k+2)-ec1_pedl(strip,layer_ped,sector)
        tdcR = IW16(k+3)
        adcR = IW16(k+4)-ec1_pedr(strip,layer_ped,sector)

        IDhist = sector*100+layer*10
        if (adcL.gt.0) call hfill(IDhist+1,1.*adcL,1.*strip,1.)
        if (adcR.gt.0) call hfill(IDhist+2,1.*adcR,1.*strip,1.)
        if (tdcL.gt.0) call hfill(IDhist+201,1.*tdcL,1.*strip,1.)
        if (tdcR.gt.0) call hfill(IDhist+202,1.*tdcR,1.*strip,1.)
c----------------------------------------------------------------------
c select side       
              if (id.gt.256 .AND. id.le.256+24) then
c check coinsidence beetwen left and right PMT
                if (adcl.gt.0 .and. adcr.gt.0) then
                  nxi = nxi + 1
                  idXi   (nxi) = id-256                        ! inner X
                  tdcL_Xi(nxi) = tdcl
                  adcL_Xi(nxi) = adcl
                  tdcR_Xi(nxi) = tdcr
                  adcR_Xi(nxi) = adcr
                end if
              else if (id.gt. 512 .AND. id.le.512+40) then
                if (adcl.gt.0 .and. adcr.gt.0) then
                  nyi = nyi + 1
                  idYi   (nyi) = id-512                        ! inner Y
                  tdcL_Yi(nyi) = tdcl
                  adcL_Yi(nyi) = adcl
                  tdcR_Yi(nyi) = tdcr
                  adcR_Yi(nyi) = adcr
                end if
              else if (id.gt.768 .AND. id.le.768+24) then
                if (adcl.gt.0 .and. adcr.gt.0) then
                  nxo = nxo + 1
                  idXo   (nxo) = id-768                        ! outer X
                  tdcL_Xo(nxo) = tdcl
                  adcL_Xo(nxo) = adcl
                  tdcR_Xo(nxo) = tdcr
                  adcR_Xo(nxo) = adcr
                end if
              else if (id.gt.1024 .AND. id.le.1024+40) then
                if (adcl.gt.0 .and. adcr.gt.0) then
                  nyo = nyo + 1
                  idYo   (nyo) = id-1024                       ! outer Y
                  tdcL_Yo(nyo) = tdcl
                  adcL_Yo(nyo) = adcl
                  tdcR_Yo(nyo) = tdcr
                  adcR_Yo(nyo) = adcr
                end if
              else 
                print *,'something wrong...',id,tdcl,adcl,tdcr,adcr
              end if
c-----------------------------------------------------------------------

        k=k+4
      ENDDO    ! one sector done

c-----------------------------------------------------------------------
c fill histograms:
         IDhist=sector*1000
         
         if (nxi*nyi.ne.0) then
           do ixi=1,nxi
              do iyi=1,nyi
                 call hfill(IDhist+1,1.*idXi(ixi),1.*idYi(iyi),1.) 
                 weight = sqrt(1.*adcL_Xi(ixi)*adcR_Xi(ixi))+
     &                    sqrt(1.*adcL_Yi(iyi)*adcR_Yi(iyi)) 
                 call hfill(IDhist+101,1.*idXi(ixi),1.*idYi(iyi),weight) 
              enddo
           enddo
         endif
         
         if (nxo*nyo.ne.0) then
           do ixo=1,nxo
              do iyo=1,nyo
                 call hfill(IDhist+2,1.*idXo(ixo),1.*idYo(iyo),1.)
                 weight=1.*sqrt(1.*adcL_Xo(ixo)*adcR_Xo(ixo))+
     &                     sqrt(1.*adcL_Yo(iyo)*adcR_Yo(iyo)) 
                 call hfill(IDhist+102,1.*idXo(ixo),1.*idYo(iyo),weight) 
              enddo
           enddo
         endif
c-----------------------------------------------------------------------

      ind=IW(ind-1)
      GOTO 1101        ! next sector
100   continue 


c  return to the top directory and exit
      call hcdir('//PAWC',' ')

      RETURN
      END
c
c------------------------------------------------------------------------------




