!-----------------------------------------------------------------------
!       BANKname BANKtype      !Comments
 TABLE   HEAD      ! create write display delete ! Bank HEAD
!
!COL ATT-name FMT Min    Max   !Comments
   1 VERSION  I    0      2    ! Version Number
   2 NRUN     I    1      1000 ! Run Number (monotonically increasing)
   3 NEVENT   I    1      1000 ! Event Number (starting with 1 at run begin,
                               !   counting separately for physics and scalers events)
   4 TIME     I  100 100000000 ! Event Time (UNIX time = seconds as of January 1,1970)
   5 TYPE     I    0      1000 ! Event Type (Defined by on-line system or MC run:
                               !            = 1 - 9    - physics events:
                               !            =   2          - physics sync
                               !            =   4          - level2 late fail
                               !            = 10       - scaler events
                               !           >= 100      - database record events
                               !            = 0        - control events
                               !            < 0        - Monte Carlo Events:
                               !            =   -1          - SDA
                               !            =   -2          - GEANT
                               !            =   -3          - ClasSim
   6 ROC      I    0  20000000 !            = 0        - sync status is OK
                               !            > 0        - bit pattern of offending ROC's
   7 EVTCLASS I    1      20   ! Event Classification from DAQ:
                               !        0 Special Events (scaler, parameter, database, etc.)
                               !     1-15 Physics Events
                               !       16 Sync Event
                               !       17 Prestart Event
                               !       18 Go Event
                               !       19 Pause Event
                               !       20 End Event
   8 TRIGBITS I    0   9999999 ! Level 1 Trigger Latch Word (16 bits)
                               !    (see LATCH1 in tgbi.ddl)
 END TABLE
!




