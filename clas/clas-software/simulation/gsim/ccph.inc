c  ccph.inc

c  $Id: ccph.inc,v 1.3 1998/05/09 19:30:30 vlassov Exp $

      character*132 ciccph
      data ciccph/
     1'$Id: ccph.inc,v 1.3 1998/05/09 19:30:30 vlassov Exp $'
     1/


c----  Alexander Vlassov  ------
C
C --- COMMON /CCPH/ - particle and photons tracks, TDC, ADC
C --- Just one event in the /ph/
C ---
C ---  Tracking part :
C ---
C --- mjtr    -  MAX number of tracks in one event (20)
C --- mstp    -  MAX number of steps
C --- stpmin  -  min distance between points on the track
C --- 
C --- jtr     -  number of tracks in event
C --- nstp()  -  number of steps for given track
C --- nvsc()  -  CC Sector  #
C --- nvch()  -  CC segment #
C --- nvtype() - Particle type (GEANT)
C --- vteta()  - Initial angle TETA from the TARGET in deg.
C --- vphi ()  - Initial angle PHI  from the TARGET in deg.
C --- rtr(k,i)-  coordinates of track
C --- vpmom() -  Particle momentum
C --- vlen () -  The particle trajectory length from the target
C ---            to the first point of the track
C ---
C ---  Ray tracing : 
C ---
C --- mig     -  MAX number of photons in the event (100)
C --- mref    -  max number of reflections for each photon (50)
C ---
C --- ig      -  number of photons in the event.
C --- nref(i) -  number of reflections (points) for given photon
C --- njtr(i) -  the track # for given photon
C --- tdc(i)  -  total length from target to PMT (cm)
C --- 
C --- ihsc(i) -  SECTOR #
C --- ihnc(i) -  0 if there are no signals in PMT
C ---            else ihnc(i) = +/- number of CC (+ ==> x>0)
C --- rl(,j,) -  RAY TRACING coordinates
C --- ncl(j,) -  RAY TRACING CC # .
C ---            j = 1 - initial point; j = nref(i) - final point.
C -------------------------------------------------------------
c
c --- in_part0   - Origin particle type
c --- v0teta     - Polar angle teta of the origin particle
c --- v0phi      - Azimuthal angle phi of the origin particle
c --- in_part    - Initial particle type
c --- nsc_in     - Initial particle Sector #
c --- ncc_in     - Initial particle Segment #
c --- xtr_in(3)  - Initial particle entry point in MARS
c --- amom_in(3) - Initial particle momentum at this point
c
c -------------------------------------------------------------
c
      INTEGER jtr,mjtr,ig,mig,nref,ncl,mref,ihnc,ihsc
      INTEGER igo,jtro,nstp,mstp,nvsc,nvch,nvtype,njtr
      INTEGER in_part0,in_part,nsc_in,ncc_in
c
      REAL rl, tdc, rtr, stpmin, vpmom, vteta, vphi, vlen
      REAL v0teta, v0phi, xtr_in, amom_in
C ---
      parameter (mjtr = 50)
      parameter (mig  = 200)
      parameter (mref = 100)
      parameter (mstp = 100)
      parameter (stpmin = 5.0)
C ---
      COMMON /CCPH/ jtr, ig, jtro, igo,
     &  ihnc(mig), ihsc(mig), nref(mig), njtr(mig), tdc(mig),
     &  nstp(mjtr), nvsc(mjtr), nvch(mjtr), nvtype(mjtr),  
     &  vphi(mjtr), vteta(mjtr), vlen(mjtr),
     &  ncl(mref,mig), rl(3,mref,mig), rtr(3,mstp,mjtr),
     &  vpmom(mstp,mjtr) 
      COMMON /CCINPR/ in_part0, v0teta, v0phi,
     &                in_part,nsc_in,ncc_in,xtr_in(3),amom_in(3)
C
