c  lcsets.inc

c  $Id: lcsets.inc,v 1.2 1997/04/24 15:38:48 holtrop Exp $

      character*132 cilcsets
      data cilcsets/
     1'$Id: lcsets.inc,v 1.2 1997/04/24 15:38:48 holtrop Exp $'
     1/


c  from patch  in file /usr/user3/burtin/gclas/glas.car on Tue Nov 15 11:02:18 EST 1994
c  this is seq: LCSETS
*-----------------------------------------------------------------------
*   LCSETS - Common block containing variables and flags for EC detector
*   VARIABLES : ISETxx     : Set number (posit. of set 'xx  ' in bank JSET)
*               IDETxx     : Detector number (posit. of det. in bank)
*               LxxDIG     : Flag for active detector 'xx  '.
*                            Preset through data card DIGI in xxSET.
*               CSETxx     : User set name
*               CDETxx     : Detector name (name of an existing volume)
*               MSLCH      : Number of super layers
*-----------------------------------------------------------------------


      integer mslch
      parameter     (mslch=6)

      integer isetch,isetsc,isetec,isetst,isetstc
      integer idetch,idetsc,idetec,idetst,idetstc
      character*4   csetch, csetsc, csetec, csetst,
     1              cdetch, cdetsc, cdetec, cdetst,
     2              csetstc, cdetstc

      common/lcsets/isetch(mslch), isetsc, isetec(3),
     1 		    isetst(2),isetstc(2),
     2              idetch(mslch), idetsc, idetec(3),
     3              idetst(2), idetstc(2),
     1              csetch(mslch), csetsc, csetec(3),
     2              cdetch(mslch), cdetsc, cdetec(3),
     3              csetst(2), cdetst(2), csetstc(2), cdetstc(2)


