c      include file CC_RC
c
c_begin_doc
c  RCS ID string
c  $Id: cc_rc.inc,v 1.4 2005/11/23 21:01:33 vlassov Exp $
c
c  Documentation for include file CC_RC
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
c  Author:   Alexander Vlassov    Created:  Thu Jun 27 11:23:44 EDT 1996
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
c  variables for include file CC_RC:
c  -----------------------------------
c  RCS information: 
      CHARACTER*(132)  CICC_RC
      DATA CICC_RC/
     1'$Id: cc_rc.inc,v 1.4 2005/11/23 21:01:33 vlassov Exp $'
     2/
c----6----------------------------------------------------------------72
c  
c  Variables contained in CC_RC common block
c  --------------------------------------------
c
      integer maxcrec
      parameter( maxcrec = 20)
      integer nrclust,     ! Number of separated hits in CC
     * !  For every hit   !
     * !  =============
     *        nrsect,     ! Sector #
     *        nrsegm,     ! 10 * Mean segment #
     *      nrsegm_p,     ! 10 * Max segment # in the cluster
     *      nrsegm_m,     ! 10 * Min segment # in the cluster
     *         nrphe,     ! Sum of photoelectrons obtained
     *        nrtime,     ! TOF from the target in channels 
*                         !     (50psec/channel)
     *        nrthet,     ! Estimated angle Theta in degrees
     *       nrdthet,     ! Estimated error of angle Theta in degrees
     *         nrphy,     ! -1 if Phy < 0 ; 1 if Phy > 0 ; 0 if Phy ~ 0 
     *         nriec,     ! Estimated i-coordinate in EC 
     *        nrdiec,     ! Estimated error of i-coordinate in EC 
     *         nrxec,     ! Estimated x-coordinate in EC 
     *        nrdxec,     ! Estimated error of x-coordinate in EC 
     *        nrstat      ! Status word (yet unclear) 
c
      integer   isgmdiff
      parameter(isgmdiff=30)    !   Parameter of the cluster-size
      integer NCOL_CCRC
      parameter(NCOL_CCRC = 12) ! Number of Columns in the CCRC bank
c_end_var
c
       COMMON /CC_RC/ nrclust,   nrsect(maxcrec),  nrsegm(maxcrec),
     *      nrsegm_p(maxcrec), nrsegm_m(maxcrec),   nrphe(maxcrec),
     *        nrtime(maxcrec),   nrthet(maxcrec), nrdthet(maxcrec),
     *         nrphy(maxcrec),    nriec(maxcrec),  nrdiec(maxcrec),
     *         nrxec(maxcrec),  nrdxec(maxcrec), nrstat(maxcrec)     
c----6----------------------------------------------------------------72




