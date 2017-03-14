c      include file TAG_INF
c
c_begin_doc
c
c  Documentation for include file TAG_INF
c
c  Purpose:
c  --------
c
c  Used in routines:
c  ----------------
c
c  Notes:
c  ------
c
c  Author:  B. Asavapibhop        Created:  Wed Jan 14  EST 1997
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
c  variables for include file TAG_INF:
c  -----------------------------------
c
c----6----------------------------------------------------------------72
c  
c  Variables contained in TAG_INF common block
c  --------------------------------------------
c
        REAL TAGTIME            ! Event start time from tagger (ns)
        REAL TAGENERGY          ! Photon Energy(GeV)
        REAL DT_STTAG		! starttime_ST-starttime_TAG (ns)
        INTEGER GOODTAG		! number of tagger hits match with start counter
        INTEGER TAGTID          ! E counter id
        INTEGER TAGEID          ! T counter id
        INTEGER TAGSTATUS       ! status same as TAGR
	INTEGER TAGPTR		! pointer to the track in TAGR
c_end_var
c
      COMMON/TAG_INF/TAGTIME(MaxParticles),TAGENERGY(MaxParticles),
     &TAGTID(MaxParticles),TAGEID(MaxParticles),TAGSTATUS(MaxParticles),
     &TAGPTR(MaxParticles),DT_STTAG(MaxParticles),GOODTAG
    
c----6----------------------------------------------------------------72




