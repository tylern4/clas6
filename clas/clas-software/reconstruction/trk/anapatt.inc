
c_begin_doc
c  RCS ID string
c  $Id: anapatt.inc,v 1.11 2000/01/04 22:10:54 mueller Exp $
c
c  Purpose:
c  --------
C      COMMON block for Track Reconstruction
c
c  Author:
c  -------
c  Bogdan Niczyporuk,  June 1992
c
c  Major revisions:
c  ----------------
C         Modified by B.N. January 1996
C         Modified by C.A.Meyer 3.1997
C                     F.Klein  Nov.1997: 6-fold geometry, Covariance matrix...
C
C                         *** LEVEL = 1 (PR segm,link) ***
C
C segm(12,isg,isl,isec)   - Track Segment No "isg" in the SuperLayer "isl" and
C                           Sector "isec". Contains raw {id,tdc} for six layers
C nsegm(isl,isec)         - Number of Track Segments in SL "isl" & Sector "isec"
C---C.A.Meyer March 1997: Added segm_rp to connect to the DC1 bank.
C segm_rp(6,isg,isl,isec) - Pointer to the row in the DC1 bank
C
C    ===> e.g. [segm(i,isg,isl,isec), isg = 1,nsegm(isl,isec)] <===
C
C clust(isg,icl,isl,isec) - List of Track Segments (pointers to "segm" array)
C                           for Cluster No "icl" in the SuperLayer "isl" and
C                           Sector "isec"
C nsegmc(icl,isl,isec)    - Number of Track Segments in the Cluster "icl",
C                           SuperLayer "isl" and Sector "isec"
C nclust(isl,isec)        - Number of Clusters in the SL "isl" and Sector "isec"
C
C    ===> e.g. {[clust(isg,icl,isl,isec), isg = 1,nsegmc], icl = 1,nclust} <===
C
C pt_clust(isl,isec) - row# in DHCL bank for this cluster
C
C lnk_clust(isl,itr) - Linked Cluster No in the SuperLayer "isl" 
C                               for the Track Candidate "itr"
C lnk_segm(isl,itr)  - Linked Segment No in the SuperLayer "isl" 
C                               for the Track Candidate "itr"
C                           taken from the Dictionary Table (MC training)
C
C itr_level(itr)          - Level of Analysis for the Track Candidate "itr"
C itr_sect(itr)           - Sector Number of the Track Candidate "itr"
C itr_scid (1,itr)        - SC Slab Number
C itr_scid (2,itr)        - SC Plane Number
C itr_scid (3,itr)        - SC hit Number in the Data {sc_digi(5,ih,1,iec)}
C
C ntr_link                - Number of the Track Candidates (linked clusters)
C
C    ===> e.g. [lnk_clust(isl,itr), itr = 1,ntr_link] <===
C    ===> e.g. [lnk_segm (isl,itr), itr = 1,ntr_link] <=== 
C
C    ===> e.g. [itr_level(itr), itr = 1,ntr_link] <=== 
C    ===> e.g. [itr_sect (itr), itr = 1,ntr_link] <=== 
C
C                         *** LEVEL = 3 (PR dcam) ***
C
C dca_segm(isl,itr)      - Track Segment No with the resolved LR-ambiguity
C                           in the SuperLayer "isl" for "itr"
C
C    ===> e.g. [dca_segm (isl,itr), itr = 1,ntr_link] <===
C
C                         *** LEVELs = 2,4 (fitting, dcam) ***
C
C trkp(12,ip,itr)         - Track point info (size 12) for plane "ip" (1st plane
C                           is vertex plane and last is EC plane) & track "itr"
C
C    ===> e.g. {{[trk(i,il,itr), i=1,12], il=1,npl_dc}, itr=1,ntr_link} <===
C
C---C.A.Meyer March 1997: trk(12,...) --> trk(12,...) + trk_pt(...)
C
C trk_pt(il,itr)          - DC track info (size 12) for layer "il" & track "itr"
C trk( 1,il,itr)          - Wire number                                  {L=2,3}
C trk( 2,il,itr)          - Raw TDC [ns]                                 {L=3}
C trk( 3,il,itr)          - Drift time in a cell [ns]                    {L=3}
C trk( 4,il,itr)          - Measured distance of closest approach [cm]   {L=3}
C trk( 5,il,itr)          - Sigma of distance of closest approach [cm]   {L=2,3}
C trk( 6,il,itr)          - Velocity of a particle [in units of c]       {L=2,3}
C trk( 7,il,itr)          - Fitted distance of closest approach [cm]     {L=2,4}
C trk( 8,il,itr)          - Track length [cm] from origin to the layer   {L=2,4}
C trk( 9,il,itr)          - Wire length from the hit to PreAmp [cm]      {L=2,4}
C trk(10,il,itr)          - Track angle relative to the R of SL [rad]    {L=2,4}
C trk(11,il,itr)          - 10*isector + Level                           {L=2,4}
C trk(12,il,itr)          - Status of the hit in the layer "il"          {L=2,3}
C
*--c.a.meyer march 1997
*
C trk_pt(il,itr)          - Row number in the DC0 bank
*--
C
C    ===> e.g. {{[trkp(i,ip,itr), i=1,12], ip=1,npln}, itr=1,ntr_link} <===
C
C trkp( 1,ip,itr)         - x |
C trkp( 2,ip,itr)         - y |--> Track point coordinates [cm] at plane "ip"
C trkp( 3,ip,itr)         - z |
C trkp( 4,ip,itr)         - Cx |
C trkp( 5,ip,itr)         - Cy |--> Track direction (unit vector) at the {x,y,z}
C trkp( 6,ip,itr)         - Cz |
C trkp( 7,ip,itr)         - Particle momentum [GeV]                    {L=1,2,4}
C trkp( 8,ip,itr)         - Particle velocity [unit=c]
C trkp( 9,ip,itr)         - Charge [units: -1, 0, +1]
C trkp(10,ip,itr)         - Track length [cm] from origin to the plane "ip"
C trkp(11,ip,itr)         - 10*isector + Level
C trkp(12,ip,itr)         - Reduced Chi-Square value
C trkp(13,ip,itr)         - Bx |
C trkp(14,ip,itr)         - By |-> field at intersection point
C trkp(15,ip,itr)         - Bz |
C
C trk_err(id,itr)     Fit parameter and Covariance matrix
C trk_err(1,itr)      q/p
C trk_err(2,itr)      lambda  (dip angle = ASIN(cz))
C trk_err(3,itr)      phi 
C trk_err(4,itr)      D0      (min.distance to origin)
C trk_err(5,itr)      Z0      (min.distance to beamline)
C trk_err(6-20,itr)   Elements of Covariance Matrix:
C            id=  6   7   8   9   10  11  12  13  14  15  16  17  18  19  20
C               {c11,c12,c13,c14,c15,c22,c23,c24,c25,c33,c34,c35,c44,c45,c55}
C
Ccc not used any more (Nov.97 fklein):
Ccc  ntr_out     - Number of reconstracted tracks/event (Detector Coord. System)
Ccc  ev_out(1,itr) |
Ccc  ev_out(2,itr) |-> Starting point (x,y,z) of the track (e.g. target pos.)
Ccc  ev_out(3,itr) |
C
Ccc  ev_out(4,itr) |
Ccc  ev_out(5,itr) |-> Starting direction (unit vector) of the track
Ccc  ev_out(6,itr) |
C
Ccc  ev_out(7,itr)  -  Momentum of a particle [GeV]
Ccc  ev_out(8,itr)  -  beta (velocity)      [unit=c]
Ccc  ev_out(9,itr)  -  Charge of a particle [-1, 0, +1]
c_end_doc
c_begin_var
c  variables for include file:
c  ---------------------------
c  RCS information:
      CHARACTER*(132)  CIANAPATT
      DATA CIANAPATT/
     1'$rcsId$'
     2/
c_end_var

      INTEGER nsgmx, nsgcmx, nclmx, nplane
      PARAMETER (nsgmx=100, nsgcmx=20, nclmx=10, nplane=46)

      INTEGER MAX_TRACKS
      PARAMETER (MAX_TRACKS=120)
      INTEGER  TRK_LEVEL_Linking, TRK_LEVEL_hbt
      INTEGER  TRK_LEVEL_lramb, TRK_LEVEL_tbt
      INTEGER  TRK_LEVEL_vx, TRK_LEVEL_hbtb, TRK_LEVEL_tbon, TRK_LEVEL_scl
      PARAMETER ( TRK_LEVEL_Linking = 1 )
      PARAMETER ( TRK_LEVEL_hbt     = 2 )
      PARAMETER ( TRK_LEVEL_lramb   = 3 )
      PARAMETER ( TRK_LEVEL_tbt     = 4 )
      PARAMETER ( TRK_LEVEL_vx      = 5 )
      PARAMETER ( TRK_LEVEL_hbtb    = 6 )
      PARAMETER ( TRK_LEVEL_tbon    = 8 )
      PARAMETER ( TRK_LEVEL_scl    = 11 )

      INTEGER segm, nsegm
      INTEGER ntr_link   		!skipped (11.97): ntr_out
      INTEGER clust, nsegmc, nclust
      INTEGER lnk_clust,lnk_segm,dca_segm, itr_level,itr_sect
      integer clwmin,clwmax
      REAL trk, trkp, trk_err		!skipped (11.97): ev_out
*
*--c.a.meyer march 1997
*
      INTEGER  segm_rp,trk_pt, pt_clust
C
      COMMON/anapatt/ segm(12,nsgmx,6,6),nsegm(6,6)
     1 ,clust(nsgcmx,nclmx,6,6), nsegmc(nclmx,6,6), nclust(6,6)               
     2 ,lnk_clust(6,MAX_TRACKS), lnk_segm(6,MAX_TRACKS)
     2 ,dca_segm(6,MAX_TRACKS), pt_clust(nclmx,6,6)      
     3 ,itr_level(MAX_TRACKS), itr_sect(MAX_TRACKS), ntr_link
     4 ,trk(12,36,MAX_TRACKS), trkp(15,nplane,MAX_TRACKS) 
     4 ,trk_err(20,MAX_TRACKS)
     5 ,segm_rp(6,nsgmx,6,6), trk_pt(36,MAX_TRACKS)
     6 ,clwmin(6,nclmx,6,6,2),clwmax(6,nclmx,6,6,2)

***not used (11.97):      ,ev_out(9,MAX_TRACKS), ntr_out
*--
C
      save /anapatt/
C


