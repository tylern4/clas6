C
C Dictionary (templates) needed to form the track segments in any SuperLayer
C and link the track segments to form the candidate trajectories in a Sector. 
C                                               Bogdan Niczyporuk, June 1992
C
C
C  *** Dictionary to build the track segments ***
C  ----------------------------------------------
C
C  --->   ((ipat(la,isg),la = 1,6),iseg = 1,npat)
C
C This array contains relative (with sespect to the layer 3) wire positions 
C in all remaining layers of the SuperLayer (SL).
C Thus, by definition "ipat(3,isg) = 0". In order to decide that a track
C segment (DATA) make sense one has to know only the wire number in layer 3.
C
C In case when SL consists of 3 or 4 layers then: 
C   
C  --->   ipat_flg3(isg) = 1  or  ipat_flg4(isg) = 1
C
C Thus those track segments must not be used for SL's with 3 or 4 layers.
C
C dtyp = 1 (TORU), = 2 (MINT), = 3 (TORU+MINT), = 4 (UNIF), = 5 (NONE)
C utime  - UnixTime when dictionary has been generated
C
C
C  *** Dictionary to build the track candidates (linked segments) ***
C  ------------------------------------------------------------------
C
C The concept adopted here is to use the absolute wire No of layer 3 in each SL
C with the road width of 2 cells for Reg.1 and 3 cells for Reg.2 and 3.
C Thus, the success of linking will depend on Geometry and B-field (DATA vs MC).
C
C  ---> nlink1(1,ilnk,iroadSL2)  - road for Axial SL of Reg.1
C  ---> nlink1(2,ilnk,iroadSL2)  - road for Axial SL of Reg.2
C  ---> nlink1(3,ilnk,iroadSL2)  - road for Axial SL of Reg.3
C  ---> nlink1(4,ilnk,iroadSL2)  - not used
C
C  ---> nlink2(1,ilnk,iroadSL2)  - road for Stereo SL of Reg.1
C  ---> nlink2(2,ilnk,iroadSL2)  - road for Stereo SL of Reg.2
C  ---> nlink2(3,ilnk,iroadSL2)  - road for Stereo SL of Reg.3
C  ---> nlink2(4,ilnk,iroadSL2)  - not used
C
c  ---> nlink3(1,ilnk,iroadSL2)  - No. of simulated track for ilnk & iroadSL2
c  ---> nlink3(2,ilnk,iroadSL2)  - average momentum  (*10MeV): <p0> for 3400A
c                                         for given torus field: <p>=<p0> / xx%
c  ---> nlink4(1,ilnk,iroadSL2)  - charge (1=neg, 2=pos); average (d0+8cm)*16(offset)
c  ---> nlink4(2,ilnk,iroadSL2)  - average (z0+128cm)/2
c  ---> nlink4(3,ilnk,iroadSL2)  - average theta
c  ---> nlink4(4,ilnk,iroadSL2)  - average phi+30deg
C
C Total number of linked trajectories is
C
C  ---> SUMi=1,ns2 { lnk(i) } 
C
C    Modified July-24-1997  Richard A Thompson
C                           Added Width(6,2) array.
C-                          This is the segment road mask used in ana_segm.
C                           The segment road mask is essentially an .or. of all
C-                          segments roads (templates).
C-                          Width(la,1) is the downstream edge of the road mask
C-                          and Width(la,2) is the upstream edge of the road 
C-                          mask for layer la.  
C-            AUG-1997  F.Klein
C-                      prelim.: for linking wrt. SL3 & SL4 the dictionary
C-                               is reorganized (see comment in sda_dinit.F)
c-            JUL-2004  F.Klein
c-                      add another large array: link4 (start parameters)
c-
      INTEGER max_npat, max_lnk, ns2
      PARAMETER (max_npat = 200, max_lnk = 8000, ns2 = 72)
C
      INTEGER npat, ipat(6,max_npat), dtyp, utime, minsl(max_npat)
     1       ,ipat_flg3(max_npat), ipat_flg4(max_npat), Width(6,2)
C       
      INTEGER lnk(ns2,2) ,link1(max_lnk,ns2,2), link2(max_lnk,ns2,2)
     1                 ,link3(max_lnk,ns2,2),link4(max_lnk,ns2,2)
C
      COMMON/analink/ npat, ipat, dtyp, utime, lnk, minsl
     1               ,link1, link2, link3, link4,
     2                ipat_flg3,ipat_flg4, Width
C
C not used any more:
C      BYTE      nlink1(4,max_lnk,ns2), nlink2(4,max_lnk,ns2)
C      INTEGER*2 nlink3(2,max_lnk,ns2)
C      EQUIVALENCE ( link1(1,1),nlink1(1,1,1) ) 
C      EQUIVALENCE ( link2(1,1),nlink2(1,1,1) ) 
C      EQUIVALENCE ( link3(1,1),nlink3(1,1,1) ) 
            
      save /analink/
C
