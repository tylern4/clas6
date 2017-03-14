c  lcsc.inc

c  $Id: lcsc.inc,v 1.2 1997/04/24 15:38:48 holtrop Exp $

      character*132 cilcsc
      data cilcsc/
     1'$Id: lcsc.inc,v 1.2 1997/04/24 15:38:48 holtrop Exp $'
     1/


c  from patch  in file glas.car on Tue Nov 15 17:33:58 1994
c  this is seq: LCSC
*-----------------------------------------------------------------------
*   LCSC - Keep block containing parameters and variables for the
*   scintillator. Variables are initialized in GESC or SCSET.
*   PARAMETERS: NVSC        : Number of volume descriptors
*               NHSC        : Number of elements per hit
*               NDSC        : Number of elements per digitisation
*               NSCIN       : Number of trapezoids to describe one super layer
*-----------------------------------------------------------------------

      integer nvsc,nhsc,ndsc	
      parameter          (nvsc=2, nhsc=6, ndsc=4)
c      parameter          (nscin = 57)

c	real ysc
c	dimension ysc(nscin)

c	data ysc /32.29,48.14,64.00,79.85,95.71,106.56,122.42,138.27
c     +  ,154.13,169.98,185.83,201.69,217.54,233.40,249.25,265.11,
c     +  280.96,296.82,312.67,328.53,344.38,360.24,376.1,371.3,
c     +  378.16,385.01,391.87,398.73,405.61,412.47,419.33,426.19,
c     +  433.05,439.9,445.06,439.32,433.55,427.81,422.05,416.31,
c     +  410.54,404.77,399.03,393.27,387.53,383.82,369.04,354.28,
c     +  339.5,324.74,309.98,295.2,280.44,265.66,250.9,236.12,221.36/

