*CMZ :  1.00/00 16/12/94  17.00.00  by  Pavel Binko
*-- Author :    Volker Blobel   16/12/94
      FUNCTION INDXA(IAR,IDENT,ND)
*
      INTEGER IAR(*)
*
************************************************************************
*
*     INDEX ABC - a simple dynamic memory managament system in
*                 three functions, working on array in argument of calls
*
*     Initialization (without call):
*        IAR(1) = NDIM = dimension of array (greater than 200)
*        IAR(2) = 0
*     The user may increase the value of IAR(1) at any time (within
*     the space available for the array).
*
*     Call for various operations:
*
*        INDEX = INDXA(IAR,IDENT,ND)
*
*        where: IAR   = array, organized by INDXA
*               IDENT = identifier of bank
*
*               ND    > 0    create bank with identifier IDENT *)
*                            returned index IND of new bank or zero
*
*               ND    = 0    find index of bank with identifier
*                            returned index IND of bank or zero
*
*               ND    = -1   drop bank with identifier
*                            returned zero
*
*               ND    < -1   make garbage collection
*                            returned zero
*
*
*     *) The data part of the bank is NOT reset to zero.
*     If a bank with the given identifier is already existing, it is
*     either changed to the required length, or dropped and recreated.
*     If space is insufficient, the bank is not created and zero
*     is returned.
*
*        IND = INDXB(IAR,IDENT)   find index of bank with IDENT
*                                  returned index IND or zero
*
*        IND = INDXC(IAR,IND)     return index of bank with next higher
*                                  IDENT, compared to bank at index IND.
*                                  return index of bank with smallest
*                                  iDENT, if IND = 0 or negative.
*
*     Layout of array:
*        IAR(1) = total dimension of array (NDIM)
*        IAR(2) = index of last used words in array
*        IAR(3) = number of dropped words
*        IAR(4) = last index in system bank
*        IAR(5) =   )
*        IAR(6) =   ) header of system bank for
*        IAR(7) =   )    pairs index/IDENT
*        IAR(8) =   )
*
*
*     Layout of active bank:
*        IAR(IND-3)  = NT = maximum number of data words in bank
*        IAR(IND-2)  = identifier IDENT
*        IAR(IND-1)  = user flag
*        IAR(IND  )  = ND = number of (used) data words in bank
*        IAR(IND+1)  =  )
*                       )     user data
*        IAR(IND+ND) =  )
*
*     Layout of dropped bank:
*        IAR(IND  )  = -NT - 4
*
*     After creation of a bank the user may reduce the number of words
*     in word IND to a smaller number (minimum zero), and increase it
*     again to a maximum value of IAR(IND-3). Word IND-1 is a user flag
*     and may be modified at any time by the user.
*
************************************************************************
*
*     reset first eight words after initialization
      IF(IAR(2).EQ.0) THEN
         IF(IAR(1).LE.200) STOP 'INDXA array short'
         IAR(2)=96
         IAR(3)=0
         IAR(4)=0
*        create system bank with 2*44 words (in total 96 words used)
         IAR(5)=88
         IAR(6)=0
         IAR(7)=0
*        ... with zero data words
         IAR(8)=0
*        system bank will contain pairs: IDENT/INDEX
      END IF
*     check for garbage collection
      IF(ND+2.LE.0) GOTO 40
*
*     try find index IND of bank with IDENT by loop through array
*
      IND=INDXB(IAR,IDENT)
*
   30 IF(ND.EQ.0) GOTO 100
*
      IF(ND.GT.0) THEN
*        bank to be created ...  does not exist
         IF(IND.EQ.0) GOTO 80
         IF(ND.LE.IAR(IND-3)) THEN
*        .. is existing with sufficient space
            IAR(IND)=ND
*           reduce, if last bank
            IF(IND+IAR(IND-3).EQ.IAR(2)) THEN
               IAR(2)=IAR(2)-IAR(IND-3)+IAR(IND)
               IAR(IND-3)=IAR(IND)
            END IF
            GOTO 100
         ELSE IF(IAR(2).EQ.IND+IAR(IND-3)) THEN
*           last bank, try to increase
            IF(IAR(2)+ND-IAR(IND-3).LE.IAR(1)) THEN
*              change length of last bank directly
               IAR(2)=IAR(2)+ND-IAR(IND-3)
               IAR(IND-3)=ND
               IAR(IND)=ND
               GOTO 100
            END IF
         END IF
*        ... has to be dropped and recreated
      END IF
*
*     drop bank
*
      IF(IND.NE.0) THEN
*        set to minus the total number of words of the bank
         IAR(IND)=-IAR(IND-3)-4
*        count number of free words
         IAR(3)=IAR(3)-IAR(IND)
*        remove entry from system bank
         DO 35 I=IAR(4)+1,8+IAR(8)
   35    IAR(I-2)=IAR(I)
         IAR(4)=0
         IAR(8)=IAR(8)-2
         IND=0
      END IF
      IF(ND+1.EQ.0) GOTO 90
*
*     garbage collection ...
*
   40 IF(IAR(3).EQ.0) GOTO 90
      NSH=0
      IND=8
*     ... by loop through whole array
   50 IF(IND.GT.IAR(2)) GOTO 70
      IF(IAR(IND).LT.0) THEN
*        bank is dropped
         NSH=NSH-IAR(IND)
         IND=IND-IAR(IND)
      ELSE
*        bank is active ...
         MD=IAR(IND-3)
         IF(NSH.NE.0) THEN
*           ... shift backward by NSH words
            DO 60 I=-3,MD
   60       IAR(IND+I-NSH)=IAR(IND+I)
*           update pointer in system bank
            BLA=INDXB(IAR,IAR(IND-NSH-2))
            IF(IAR(4).LE.0) STOP 'INDXA: negative index'
            IAR(IAR(4)-1)=IND-NSH
         END IF
         IND=IND+MD+4
      END IF
      GOTO 50
*     update control words
   70 IAR(2)=IAR(2)-NSH
      IAR(3)=0
      IND=0
*     end-of-garbage collection
      IF(ND.LE.0) GOTO 100
*     create new bank
   80 IF(IAR(8)+2.GT.IAR(5)) THEN
*        increase length of system bank by 2*100 words
         IF(IAR(2)+200.GT.IAR(1)) GOTO 90
*        shift all banks
         DO 82 I=IAR(2),8+IAR(5)+1,-1
   82    IAR(I+200)=IAR(I)
*        shift all pointers by 200
         DO  84 I=9,8+IAR(5),2
   84    IAR(I )=IAR(I)+200
         IAR(2)=IAR(2)+200
         IAR(5)=IAR(5)+200
      END IF
      IF(IAR(2)+4+ND.GT.IAR(1)) THEN
*        garbage collection necessary before creation
         IF(IAR(2)+2+ND-IAR(3).LE.IAR(1)) GOTO 40
*        insufficient space
         GOTO 90
      END IF
      IND=IAR(2)+4
*     define bank header ...
      IAR(IND-3)=ND
      IAR(IND-2)=IDENT
      IAR(IND-1)=0
      IAR(IND  )=ND
*     ... and update counter
      IAR(2)=IAR(2)+4+ND
*     ... and add entry to system bank
      BLA=INDXB(IAR,IDENT)
      IAR(4)=-IAR(4)
      DO 85 I=8+IAR(8),IAR(4)-1,-1
   85 IAR(I+2)=IAR(I)
      IAR(IAR(4)-1)=IND
      IAR(IAR(4)  )=IDENT
      IAR(8)=IAR(8)+2
      GOTO 100
*     reset index ...
   90 IND=0
*     ... and return
  100 INDXA=IND
*
      RETURN
      END
