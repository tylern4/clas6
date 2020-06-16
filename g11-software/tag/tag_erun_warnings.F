        subroutine tag_erun_warnings
c
c_begin_doc
c  RCS ID string
c  $Id: tag_erun_warnings.F,v 1.2 1998/04/22 01:17:36 anciant Exp $
c
c  Documentation for subroutine tag_erun_warnings
c
c  Purpose: 
c  --------
c
c       Display the statistic of all the tagger warning messages that occured during the run
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
c  Notes:	Created: march 1998 
c  ------
c
c  Author:      E. Anciant
c  -------
c
c  Major revisions:
c  ----------------    
c
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
#include "tag_general.PAR"
#include "tagwarnings.CMN"
*
c_end_inc
c
c_begin_var
c  input/output variables:
c  -----------------------
*
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
     1'$Id: tag_erun_warnings.F,v 1.2 1998/04/22 01:17:36 anciant Exp $'   
     2/   
c  Module information:
      CHARACTER*(*)  CRNAME, CRAUTH
      CHARACTER*100  CRMESS
      PARAMETER (CRNAME='tag_erun_warnings')
      PARAMETER (CRAUTH='E Anciant ')
c
c  Local User defined variables:
c  -----------------------------
c        
c_end_var
c
c
c  executable code for routine tag_read_PS_BOS:
c----6----------------------------------------------------------------72
*
      if (wtrfb.ne.0) then
           write(crmess,1) wtrfb
1          format ('Not being able to read tagger data from BOS bank occured ',I2,' times')
      endif


      if (wta.ne.0) then
             write(crmess,2) wta
2            format ('Problems during tagger analysis occured ',I5,' times')
      endif


      if (wtpt.ne.0) then
             write(crmess,3) wtpt
3            format ('Problems during TAGR hits generation occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif


      if (wtiftb.ne.0) then
           write(crmess,4) wtiftb
4          format ('Not being abble to write intermediate results TAGI Tagger bank occured ',I5,' times')
           CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtftb.ne.0) then
           write(crmess,5) wtftb
5          format ('Not being abble to write Tagger output bank occured ',I5,' times')
           CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtr2b.ne.0) then
             write(crmess,1) wtr2b
6            format ('Not being able to convert CODA banks to physics BOS bank for TAGGER occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtfo.ne.0) then
             write(crmess,7) wtfo
7            format ('Problems during Tagger filtering occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtetgm.ne.0) then
             write(crmess,8) wtetgm
8            format ('Problems during E-T geometric matching occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtettm.ne.0) then
             write(crmess,9) wtettm
9            format ('Problems during E-T time coincidence occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtetb.ne.0) then
             write(crmess,10) wtetb
10            format ('Problems during E & T binning occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wttaa.ne.0) then
             write(crmess,11) wttaa
11            format ('Problems during T alone analysis occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtrtc.ne.0) then
             write(crmess,12) wtrtc
12            format ('Problems during RF - T time correction occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (witiftb.ne.0) then
              write(crmess,13) witiftb

13            format ('Mismatch between the number of events written to 
     +   the TAGI BOS Bank and the actual number of reconstructed events occured ',I5)
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtrpcb.ne.0) then
             write(crmess,14) wtrpcb
14           format ('problems while reading PC data from BOS bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtpcp.ne.0) then
             write(crmess,15) wtpcp
15           format ('Errors while processing Pair Counter occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtrpsb.ne.0) then
             write(crmess,16) wtrpsb
16           format ('problems while reading PS data from BOS bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtpsp.ne.0) then
             write(crmess,17) wtpsp
17           format ('Errors while processing Pair Spectrometer occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtrtcb.ne.0) then
             write(crmess,18) wtrtcb
18           format ('problems while reading TAC data from BOS bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wttcp.ne.0) then
             write(crmess,19) wttcp
19           format ('Errors while processing TAC occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (witrpcb.ne.0) then
             write(crmess,20) DSPCData_BOS_name,witrpcb
20           format (a4,' :malformed bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (witrpsb.ne.0) then
             write(crmess,21) DSPSData_BOS_name,witrpsb
21           format (a4,' :malformed bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (witrtcb.ne.0) then
             write(crmess,22) DSTCData_BOS_name,witrtcb
22           format (a4,' :malformed bank occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtagta.ne.0) then
             write(crmess,23) TagTData_BOS_name,wtagta
23           format (a4,'bos Bank corrupted or absent, occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtagte.ne.0) then
             write(crmess,24) TagTData_BOS_name,wtagte
24           format ('empty bank : ',a4,' occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtagtm.ne.0) then
             write(crmess,25) TagTData_BOS_name,wtagtm
25           format ('malformed bank : ',a4,' occured ',I5,' times')
             CALL RECMES(CRNAME,'I',CRMESS)
      endif

      if (wtid.ne.0) then
                    write(crmess,26) wtid
26                  format ('T counter ID out of range occured ',I5,' times')
                    CALL RECMES(CRNAME,'W',CRMESS)
      endif

      if (wtagea.ne.0) then
         write(crmess,27) TagEData_BOS_name,wtagea
27       format (a4,'bos bank corrupted or absent occured ',I5,' times')
         CALL RECMES(CRNAME,'W',CRMESS)
      endif

      if (wtagee.ne.0) then
           write(crmess,28) TagEData_BOS_name,wtagee
28         format ('empty bank : ',a4,' occured ',I5,' times')
           CALL RECMES(CRNAME,'W',CRMESS)
      endif

      if (wtagem.ne.0) then
           write(crmess,29) TagEData_BOS_name,wtagem
29         format ('malformed bank : ',a4,' occured ',I5,' times')
           CALL RECMES(CRNAME,'W',CRMESS)
      endif


      if (wtageo.ne.0) then
           write(crmess,30) wtageo
30         format (' hits in TAGE bank overflowing the size of the array occured ',I5,' times')
           CALL RECMES(CRNAME,'W',CRMESS)
      endif

      if (wecid.ne.0) then
               write(crmess,31) wecid
31             format ('E counter ID out of range occured ',I5,' times')
               CALL RECMES(CRNAME,'W',CRMESS)
      endif

      if (wits.ne.0) then
           write(crmess,32) wits
32         format ('TGS data corrupted or absent occured ',I5,' times')
           CALL RECMES(CRNAME,'W',CRMESS)
      endif

      if (witpt.ne.0) then
              write(crmess,33) witpt
33            format (' hits reconstructed in the Tagger bank TAGR overflowing the hits array, occured ',I5,' times')
              CALL RECMES(CRNAME,'W',CRMESS)
      endif


      return
      end