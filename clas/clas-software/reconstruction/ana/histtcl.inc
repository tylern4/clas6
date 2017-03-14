c      include file HISTTCL
c
c_begin_doc
c  RCS ID string
c  $Id: histtcl.inc,v 1.18 2009/03/31 19:03:01 myurov Exp $
c
c  Documentation for include file HISTTCL
c
c  Purpose:  holds the histo/ana RECSIS control variables for communication 
c  --------  with TCL script
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:   Francois Roudot      Created:  Thu May 29 10:00:00 EST 1997
c  -------
c
c  Major revisions:
c  ----------------
c
c_end_doc
c
c
c  Remember NO NESTED INCLUDES
c
c  COMMON BLOCK NAME should be the same as include file name!
c
c_begin_var
c  variables for include file HISTTCL:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CIHISTTCL
      DATA CIHISTTCL/
     1'$rcsId$'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in HISTTCL common block
c  --------------------------------------------
c
c_end_var
c

c  The following logicals are used to control calling various histo packages
c  LTRK_H_DO    TRUE -> perform tracking histo
c  LEGN_H_DO    TRUE -> perform calorimeter reconstruction histo
c  LCC_H_DO     TRUE -> perform cherenkov reconstruction histo
c  LIC_H_DO     TRUE -> perform DVCS calorimeter reconstruction histo
c  LTOF_H_DO    TRUE -> perform TOF reconstruction histo
c  LST_H_DO     TRUE -> perform start counter reconstruction histo
c  LUSR0_H_DO   FALSE -> perform USER code BEFORE event processing histo
c  LUSR1_H_DO   TRUE -> perform USER code after EVENT processing histo
c  LUSR2_H_DO   FALSE -> extra flag
c  LTIME_H_DO   TRUE -> perform event time analysis histo
c  LTAGGER_H_DO TRUE -> perform tagger stuff histo
c  LSEB_H_DO    TRUE -> perform Simple Event Builder histo
c  LEC1_H_DO    TRUE -> perform large angle calorimeter histo
c
c  LIC_HIST    TRUE -> perform histograming for inner calorimeter
c  LFEC_HIST    TRUE -> perform forward angle calorimeter histo
c  LSEB_HIST    TRUE -> perform Simple Event Builder histo
c  LMON_HIST    TRUE -> Open second file for monhistos
c
c  LECHB_NT_DO  TRUE -> perform N tuple output for ECHB hits
c  LICHB_NT_DO  TRUE -> perform N tuple output for ICHB hits
c  LSHHB_NT_DO  TRUE -> perform N tuple output for SHHB hits
c  LSEB_NT_DO   TRUE -> perform N tuple output for SEB banks.
c  LSEB_NTN_DO   TRUE -> Reduced N-tuple from SEB banks.
c  LTRK_NT_DO   TRUE -> perform N tuple output for DC tracks.
c  LTBTK_NT_DO  TRUE -> perform N tuple output for DC layers in the track.
c  LSCR_NT_DO   TRUE -> perform N tuple output for TOF hits.
c  LCCR_NT_DO   TRUE -> perform N tuple output for CC hitss.
c  LICR_NT_DO   TRUE -> perform N tuple output for IC single hitss.
c  LLAC_NT_DO   TRUE -> perform N tuple output for LAC hitss.
c  LTAGR_NT_DO  TRUE -> perform N tuple output for tagger hitss.
c  LMVRT_NT_DO  TRUE -> perform N tuple output for MVRT info.
c  LALL_NT_DO   TRUE -> Books and fills Simple Event Builder N-tuples. 
c  LMCTK_NT_DO  TRUE -> Books and fills n-tuples for MCTK. 
c  LPART_NT_DO  TRUE -> Books and fills n-tuples for PART. 
c
c  LEC_NT_DO    TRUE -> Books and fills n-tuples for EC raw data
c  LSC_NT_DO    TRUE -> Books and fills n-tuples for SC raw data
c  LCC_NT_DO    TRUE -> Books and fills n-tuples for CC raw data
c  LDC_NT_DO    TRUE -> Books and fills n-tuples for DC raw data 
c  nt_id_cut    #id for one particle in ntuple output
c               e.g.  id=11: at least 1 electron identified
 
       LOGICAL LTRK_H_DO,  LEGN_H_DO, LCC_H_DO, LIC_H_DO,   LTOF_H_DO,  LST_H_DO, 
     1         LUSR0_H_DO, LUSR1_H_DO, LUSR2_H_DO, LTIME_H_DO, LICHB_NT_DO, LICPB_NT_DO, LSHHB_NT_DO, LSHPB_NT_DO,
     2         LTAGGER_H_DO, LEC1_H_DO, LSEB_H_DO, LTRK_NT_DO,LMON_HIST,LIC_HIST,
     3         LFEC_HIST,  LSEB_HIST, LECHB_NT_DO, LTAGR_NT_DO, LSEB_NT_DO, LALL_NT_DO,
     4         LSCR_NT_DO,LCCR_NT_DO,LLAC_NT_DO,LTBTK_NT_DO,LMCTK_NT_DO, LSEB_NTN_DO,
     5	       LEC_NT_DO,LECPO_NT_DO,LSC_NT_DO,LCC_NT_DO,LDC_NT_DO,LPART_NT_DO,LICR_NT_DO,
     6         LMVRT_NT_DO,LGCPB_NT_DO 

       INTEGER nt_id_cut

       COMMON/HISTTCL/ LTRK_H_DO, LEGN_H_DO, LCC_H_DO, LIC_H_DO, LTOF_H_DO, LST_H_DO, 
     1         LUSR0_H_DO,  LUSR1_H_DO, LUSR2_H_DO, LTIME_H_DO, LTAGGER_H_DO, LICPB_NT_DO,
     2         LEC1_H_DO, LSEB_H_DO, LTRK_NT_DO,LMON_HIST, LSEB_NTN_DO,LIC_HIST,
     3         LFEC_HIST, LSEB_HIST, LECHB_NT_DO, LSEB_NT_DO, LALL_NT_DO, LTAGR_NT_DO,
     4         LSCR_NT_DO,LCCR_NT_DO,LLAC_NT_DO,LTBTK_NT_DO,LMCTK_NT_DO,LICHB_NT_DO, 
     5	       LEC_NT_DO,LECPO_NT_DO,LSC_NT_DO,LCC_NT_DO,LDC_NT_DO,LPART_NT_DO,LICR_NT_DO,
     6         nt_id_cut,LMVRT_NT_DO,LGCPB_NT_DO, LSHHB_NT_DO, LSHPB_NT_DO





