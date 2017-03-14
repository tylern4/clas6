        subroutine stread_from_bos(OK)
c
c_begin_doc
c  RCS ID string
c  $Id: stread_from_bos.F,v 1.19 2008/03/16 04:33:24 fklein Exp $
c
c  Documentation for subroutine Stread_from_bos
c
c  Purpose: 
c  --------
c
c       Reads raw data from ST BOS bank and fills the StFastbus_data
c       common.
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
c  Author: S.Taylor         Created:  Sat jun 11 14:07:15 EST 1997
c  -------
c
c  Major revisions:
c  ----------------    
c      - new format for ST bank   :Burin Mar98
c_end_doc
c
      IMPLICIT NONE
      SAVE
c
c_begin_inc
c  include files :
c  ---------------------
* 
c BOS common block  uncomment the next line for BOS include file
*      
#include "bcs.inc"
#include "st_general.PAR"
#include "sthit.CMN"
#include "stcal.CMN"
*
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
*
*-OUTPUT
      LOGICAL OK                !success or failure
c
c  Local pre-defined variables:
c  ---------------------------
c  RCS information: 
      CHARACTER*132  CFILE, CREVIS, CSTATE, CDATE, CAUTHO, CRCSID
      PARAMETER (CFILE = '$rcsRCSfile$')
      PARAMETER (CREVIS = '$rcsRevision$')
      PARAMETER (CSTATE = '$rcsState$')
      PARAMETER (CDATE = '$rcsDate$')
      PARAMETER (CAUTHO = '$rcsAuthor$')
      DATA CRCSID/   
     1'$Id: stread_from_bos.F,v 1.19 2008/03/16 04:33:24 fklein Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='STread_from_BOS')
      PARAMETER (CRAUTH='S.Taylor ')
c
c  Local User defined variables:
c  ---------------
c
c  Notes:
c  ------
c
c  -----------------------------
c        
        integer size,size4,ifirst,ptemp
        integer pair_id,i,j,ndata,j_adc,j_tdc,j_pd
        data ifirst/0/
	integer idx,idx0,idx1
	integer name_i,name_i1,name_i0,mamind
        integer sec(St_max_pair),pd(St_max_pair),tdc(St_max_pair),adc(St_max_pair),dat(St_max_pair) 
	
c_end_var
c
c
c  executable code for routine Stread_from_BOS:
c----6----------------------------------------------------------------72
*      
      StNHits=0
      ptemp = 0
      do i=1,St_max_pair
         dat(i)=0
         tdc(i)=0
         adc(i)=0
         sec(i)=0
         pd(i)=0
      enddo   

      if (ifirst.eq.0) then
         if (StType.eq.0) then
            name_i = mamind(iw,'ST  ')
         else
            name_i1 = mamind(iw,'STN1')
            name_i0 = mamind(iw,'STN0')
         endif
         if (name_i.eq.0.AND.name_i0.eq.0.AND.name_i1.eq.0) then
            CRMESS= 'NO ST OR STN BANK!'
            call RECMES(CRNAME,'W',CRMESS)
            return
         endif
c         ifirst = 1
      endif
      
*** if ST banks was found then create pairs
      if(StType.eq.0) then
         idx = iw(name_i)
         OK=idx.ne.0.
         if(.not.OK) then
c     CRMESS= 'NO DATA FOR START COUNTER!'
c     call RECMES(CRNAME,'W',CRMESS)
            return
         endif
         size4=IW(idx)          !size in [I*4] words
         size= 2*size4          !size in [I*2] words
*     
*-BOS only "knows" fullwords; odd number of half words were rounded up
*-(the remaining halfword is wasted)
*
         IF( MOD(size-1,StData_BOS_size).EQ.0 ) THEN
            size= size-1        !BOS was forced to round up
         ENDIF
      
         OK= size.GE.StData_BOS_size .and. MOD(size,StData_BOS_size).EQ.0
         IF(.NOT.OK) THEN
c          crmess= ' malformed bank : '//StData_BOS_name
c          call RECMES(CRNAME,'W',CRMESS)
            RETURN
         ENDIF 	
	
         NDATA = size/StData_BOS_size
         do i = 1, NDATA
            j=(i-1)*StData_BOS_size
            sec(i) = iw16(2*idx+j+1)
            tdc(sec(i)) = iw16(2*idx+j+2)
            adc(sec(i)) = iw16(2*idx+j+3)
            if(tdc(sec(i)).lt.4095.and.tdc(sec(i)).gt.0) then
               dat(sec(i)) = 1
            endif
         enddo
         do i = 1, ST_max_pair
            if(dat(i).eq.1) then
               pair_id = (i-1)/2 + 1
               if(ptemp.ne.pair_id) StNHits = StNHits + 1
               if(mod(i,2).eq.0) then
                  StFastbus_data(pair_id,St_TDC2) = tdc(i)
                  StFastbus_data(pair_id,St_ADC2) = adc(i)
               else
                  StFastbus_data(pair_id,St_TDC1) = tdc(i)
                  StFastbus_data(pair_id,St_ADC1) = adc(i)
               endif
               ptemp = pair_id
               StFoundHit(StNHits)=pair_id
               StSecHit(StNHits)=i
            endif	
         enddo
      elseif(StType.eq.1) then
*** if STN0-1 banks was found then match TDC and ADC
         idx0 = iw(name_i0)
         idx1 = iw(name_i1)
         OK=idx0.ne.0.AND.idx1.ne.0
         if(.not.OK) return
cc         size4=IW(idx1)          !size in [I*4] words
cc         size= 2*size4           !size in [I*2] words
*-BOS only "knows" fullwords; odd number of half words were rounded up
*-(the remaining halfword is wasted)
cc         IF( MOD(size-1,StnData_BOS_size).EQ.0 ) THEN
cc            size= size-1        !BOS was forced to round up
cc         ENDIF      
cc         OK= size.GE.StnData_BOS_size .and. MOD(size,StnData_BOS_size).EQ.0
cc         IF(.NOT.OK) RETURN
         NDATA =IW(idx1-4)
         do i = 1, NDATA
            j_adc=(i-1)*StnData_BOS_size
            j_pd      = iw16(2*idx1+j_adc+1)
            adc(j_pd) = iw16(2*idx1+j_adc+2)
         enddo
         NDATA =MIN(IW(idx0-4),st_max_pair)
         do i = 1, NDATA
            j_tdc=(i-1)*StnData_BOS_size
            pd(i)=iw16(2*idx0+j_tdc+1)
            sec(i) = int((pd(i)-1)/4)+1
            tdc(pd(i)) = iw16(2*idx0+j_tdc+2)
            if(tdc(pd(i)).lt.StnCal_Values(TDC_max,pd(i)).and.
     &         tdc(pd(i)).gt.StnCal_Values(TDC_min,pd(i))) then
               dat(pd(i)) = 1
            endif
         enddo
         do i = 1, NDATA
            if(dat(pd(i)).eq.1) then
               StNHits = StNHits + 1
               StFastbus_data(pd(i),St_TDC1) = tdc(pd(i))
               StFastbus_data(pd(i),St_ADC1) = adc(pd(i))
               StSecHit(StNHits)=sec(i)
               StFoundHit(StNHits)=pd(i)
            endif	
         enddo
      endif
      return
      end		

