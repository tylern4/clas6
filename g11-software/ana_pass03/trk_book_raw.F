      Subroutine trk_book_raw()
*_begin_doc
*  RCS ID string
*  $Id: trk_book_raw.F,v 1.12 1998/01/12 16:52:47 marki Exp $
*
*  Documentation for subroutine TRK_BOOK_RAW
*
*  Purpose:  This routine define the raw tracking histograms.
*  -------- 
*
*  Input Parameters: NONE
*  ----------------
*
*  Output Parameters: NONE
*  -----------------
*
*  Other routines:
*  ---------------
*
*  Notes:
*  ------
*
*  Author:         Francois H. Roudot  
*  -------        
*
*  Creation Date:  29 May, 1997
*  --------------
*
*  Major revisions:
*  ----------------
*	1.(11/9/97)Junho Yun 
*	   a) change his_title 
*
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

#include "bcs.inc"
c#include "sdakeys.inc"
#include "sdadigi.inc"
#include "trktcl_var.inc"



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
      PARAMETER ( CFILE = '$RCSfile: trk_book_raw.F,v $')
      PARAMETER ( CREVIS = '$Revision: 1.12 $')
      PARAMETER ( CSTATE = '$State: Exp $')
      PARAMETER ( CDATE = '$Date: 1998/01/12 16:52:47 $')
      PARAMETER ( CAUTHO = '$Author: marki $')
      DATA CRCSID/
     1'$Id: trk_book_raw.F,v 1.12 1998/01/12 16:52:47 marki Exp $
     3'/
*  Module information:
      CHARACTER*132  CRNAME, CRAUTH
      CHARACTER*132  CRMESS
      PARAMETER (CRNAME='TRK_BOOK_RAW')
      PARAMETER (CRAUTH='Francois H. Roudot')
*
*  Local User defined variables:
*  ----------------------------
      
      character*4 r_label(2)
      data r_label/'wire','tdc '/
      character*4 r2_label(4)
      data r2_label/'wire','tdc ','sect','layr'/
*
      integer Reg, Sec, HistID
cyun
      character*80 his_title
      character*1 Fsec
      character*1 Freg	
cyun

c ------------------------------------------------------------------------
c hbook stuff to define the TRK subdirctory both in memory and disk

      if (ipar_trk_histo.ge.3.or.ipar_trk_ntuple.ge.3) then
        call hcdir('//LUN1/TRK',' ')
        call hmdir('RAW','S')
        call hcdir('//PAWC/TRK',' ')
        call hmdir('RAW','S')
c                         --------------------
c                             book ntuples
c                         --------------------
c       RAT 97.06.05
c       the following ntuples have been superceded by a single one: 333
c       RAT
        if (ipar_trk_ntuple.ge.3) then                     
crat      call hbookn(101,'Raw SL1',2,' ',500,r_label)
crat      call hbookn(102,'Raw SL2',2,' ',500,r_label)
crat      call hbookn(103,'Raw SL3',2,' ',500,r_label)
crat      call hbookn(104,'Raw SL4',2,' ',500,r_label)
crat      call hbookn(105,'Raw SL5',2,' ',500,r_label)
crat      call hbookn(106,'Raw SL6',2,' ',500,r_label)
          call hbookn(333,'RAW',4,'//LUN1/TRK/RAW',500,r2_label)
        endif

c              -----------------------
c              book the RAW histograms
c              -----------------------

        if(ipar_trk_histo.ge.3) then           
	      call hbook2(10,'SL1',200,1,200,130,1,130,0.)
	      call hbook2(20,'SL2',200,1,200,130,1,130,0.)
	      call hbook2(30,'SL3',200,1,200,192,1,192,0.)
	      call hbook2(40,'SL4',200,1,200,192,1,192,0.)
	      call hbook2(50,'SL5',200,1,200,192,1,192,0.)
	      call hbook2(60,'SL6',200,1,200,192,1,192,0.)

c              book the multiplicity histograms
cyun
          do Reg=1,3
           do Sec=1,6
              HistID = 1 + 100*Sec + 1000*Reg
		write(Freg,'(i1)') Reg
		write(Fsec,'(i1)') Sec
		his_title='RAW Number of Hits R'//Freg//'S'//Fsec
              call hbook1(HistID, his_title, 199, 1.0, 200.0, 0.0)
            enddo
          enddo

c              book tdc and layer vs wire
          do Sec = 1,6
            Reg=1
            HistID = 3 + Sec*100 + Reg*1000
	    write(Fsec,'(i1)')Sec	
            his_title='RAW TDC R1S'//Fsec
            call hbook1(HistID, his_title, 200, 500.0, 3500.0, 0.0)  

            Reg=2
            HistID = 3 + Sec*100 + Reg*1000
            write(Fsec,'(i1)')Sec
            his_title='RAW TDC R2S'//Fsec
            call hbook1(HistID, his_title, 200, 1500.0, 6500.0, 0.0)  

            Reg=3
            HistID = 3 + Sec*100 + Reg*1000
            write(Fsec,'(i1)')Sec
            his_title='RAW TDC R3S'//Fsec
            call hbook1(HistID, his_title, 200, 1500.0, 8500.0, 0.0)  

            HistID = 2 + Sec*100 + 7000
            write(Fsec,'(i1)')Sec
            his_title='RAW layer vs wire Sec'//Fsec
            call hbook2(HistID,his_title,195, 0.0, 195.0, 37, 0.0, 37.0, 0.0)
          enddo
        endif
        call hcdir('//PAWC/TRK',' ')
      endif

      Return
      End









































