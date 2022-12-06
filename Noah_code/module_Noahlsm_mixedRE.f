

  module module_Noahlsm_mixedRE

  implicit none

  INTEGER,PARAMETER  :: MAXITER      = 10
  REAL   ,PARAMETER  :: DTITER_MIN   = 1.  !min time step for RE interation [s]
  REAL   ,PARAMETER  :: HTOPMX       = 50. !max. ponding depth, any water beyond runs off [mm]

  contains

! ==================================================================================================
  SUBROUTINE MIXEDRE(OPT_RUN,OPT_WATRET,VARSD   ,NSOIL   ,DZSO   ,DT     ,          & !in
                     PDDUM  ,ETRANI    ,QSEVA   ,DMICE   ,DSH2O  ,FCR    ,SICEOLD , & !in
                     TOPOSV ,ILOC      ,JLOC    ,                                   & !in
                     BEXP   ,PSISAT    ,DKSAT   ,SMCMAX  ,SMCR   ,VGN    ,VGPSAT  , & !in
                     PSI    ,SH2O      ,WCND    ,ATMACT  ,ATM_BC ,DTFINE ,HTOP    , & !inout
                     ZWT    ,RSINEX    ,QDIS    ,QDRYC   ) !out

! ----------------------------------------------------------------------
        IMPLICIT NONE
! ----------------------------------------------------------------------
!inputs

      INTEGER,                  INTENT(IN) :: ILOC,JLOC
      INTEGER,                  INTENT(IN) :: OPT_RUN
      INTEGER,                  INTENT(IN) :: OPT_WATRET
      INTEGER,                  INTENT(IN) :: VARSD   !if variable soil depth is activated, see noah_driver
      INTEGER,                  INTENT(IN) :: NSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: DZSO
      REAL                    , INTENT(IN) :: DT 

      REAL                    , INTENT(IN) :: PDDUM   !upper BC (m/s)
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: ETRANI  !transpiration (m/s)
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: DMICE   !rate of phase change (m/s)
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: DSH2O   !rate of liquid water from SNOWWATER (m/s)
      REAL                    , INTENT(IN) :: QSEVA   !evaporation (m/s)
      REAL                    , INTENT(IN) :: TOPOSV  ! [m]

      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: BEXP    !B parameter
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: PSISAT  !air-entry potential [m]
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMCMAX  !porosity (m3/m3)
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: DKSAT   !saturated soil hydraulic conductivity (m/s)
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMCR    !residual volumetric soil moisure (m3/m3)
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: VGPSAT  !VG parameter psat (m)
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: VGN     !VG parameter n

      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: FCR
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICEOLD !soli. soil moisture content

! inouts

      INTEGER                 , INTENT(INOUT) :: ATM_BC !ATM_BC: 0->Neuman ; 1->Dirichlet
      REAL                    , INTENT(INOUT) :: ATMACT !atmospheric BC,actual(m/s,positive downward)
      REAL                    , INTENT(INOUT) :: DTFINE !finer time step
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: WCND   !hydrulic conductivity at node  (m/s)
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O   !liq. soil moisture content [m3/m3]
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: PSI    !soil water potential [m]
      REAL                    , INTENT(INOUT) :: HTOP   !surface ponding height [mm]; allow "-" for mass bal.

! outputs

      REAL,                     INTENT(OUT)   :: RSINEX !infiltration-excess surface runoff [mm/s]
      REAL,                     INTENT(OUT)   :: ZWT    !water table depth [m]
      REAL,                     INTENT(OUT)   :: QDIS   !m/s
      REAL,                     INTENT(OUT)   :: QDRYC  !dry limit (SMCR+0.001) correction to EDIR [mm/s]

!local

      INTEGER                               :: IZ
      INTEGER                               :: ITER
      INTEGER                               :: ATM_BC_OLD
      LOGICAL                               :: ICONV      !iteration convergency
      REAL, DIMENSION(1:NSOIL)              :: DV         !liq. moisture change between two interations
      REAL, DIMENSION(1:NSOIL)              :: DZI        !soil layer depth between two nodes [m]
      REAL, DIMENSION(1:NSOIL)              :: ZNODE      !layer middle (node) depth [m]
      REAL                                  :: DTFINE_ACC !accumulated time until Noah-MP DT 
      REAL                                  :: DTITER     !iteration timestep [s]
      REAL, DIMENSION(1:NSOIL)              :: SMX        !soil moisture at current timestep and iteration
      REAL, DIMENSION(1:NSOIL)              :: SS         !source +/sink - (m/s)
      REAL, DIMENSION(1:NSOIL)              :: SMX_OLD    !at previous sub-step
      REAL, DIMENSION(1:NSOIL)              :: PSI_OLD    !at previous sub-step
      REAL, DIMENSION(1:NSOIL)              :: SE     !liq. soil water saturation
      REAL, DIMENSION(1:NSOIL)              :: QDIS_RM    !SH2O removal from subsurface runoff [m/s]
      REAL, DIMENSION(1:NSOIL)              :: DVDH       !specific moisture capacity [m-1]
      REAL, DIMENSION(1:NSOIL)              :: DPSI
      REAL, DIMENSION(1:NSOIL)              :: SICE_TMP
      REAL, DIMENSION(1:NSOIL)              :: WFLUX      !water flux between layers during DTITER [m/s] ["+" downward]
      REAL, DIMENSION(1:NSOIL)              :: WFLUXDT    !water flux between layers during DT [m/s] ["+" downward]
      REAL                                  :: LBC

      REAL                                  :: RUNSRFDT   !infiltration surface runoff during Noah-MP DT [mm/s]
      REAL                                  :: ATMPOT     !atmospheric BC,potential(m/s,positive downward)

      INTEGER                               :: NBACK      !number of back step 
      INTEGER,DIMENSION(1:NSOIL)            :: FLAG_DRY
      LOGICAL                               :: ISWITCH
      REAL                                  :: PONDTH     !threshold for surface ponding (m)
      real               :: DRYCOR     !dry limit (SMCR+0.001) correction during DTITER to correct EDIR [m/s]
      real,DIMENSION(1:NSOIL)               :: PSI_SAT    !air-entry pressure [m] for VG and CH
      real,DIMENSION(1:NSOIL)               :: PSI_MIN    !minimum pressure at SMCR + 0.001 for VG [m]
     !REAL   :: HTOPMX      != 50. !max. ponding depth, any water beyond runs off [mm]

      REAL                                  :: BEG_WB
      REAL                                  :: END_WB
      REAL                                  :: WATERR
      REAL                                  :: SSSUM
      real                                  :: total_err
      INTEGER,PARAMETER                     :: OUT1 = 998
      INTEGER,PARAMETER                     :: OUT2 = 999


!------------------------------------------------------------------------------------------------------------------------
     DTFINE_ACC     = 0. 
     RUNSRFDT       = 0.
     QDRYC          = 0.
     WFLUXDT        = 0.

     IF (OPT_WATRET == 1) THEN
        PSI_SAT = -VGPSAT
        PONDTH  = PSI_SAT(1) - 0.01    !0.01 for stability
     ELSE IF (OPT_WATRET == 2) THEN
        PSI_SAT = -PSISAT              !0.1  for stability
        PONDTH  = PSI_SAT(1) - 0.1
     END IF

    !HTOPMX       = TOPOSV / 2.  !max. ponding depth, any water beyond runs off [mm]

     !layer thickness between two nodes

     DO IZ = 1,NSOIL-1
        DZI(IZ)    = (DZSO(IZ) + DZSO(IZ+1))/2.
     END DO
     DZI(NSOIL)    = DZSO(NSOIL)/2. 

     !node (middle of a layer) depth [m]

     ZNODE(1) = DZSO(1) / 2.
     DO IZ = 2, NSOIL
       ZNODE(IZ)  = ZNODE(IZ-1) + (DZSO(IZ)+DZSO(IZ-1))/2.
     ENDDO

     !mixed-form RE time step [s]
!    DTFINE         = MAX(DTFINE,10.)

     SMX     (:) = SH2O(:)
     SICE_TMP(:) = SICEOLD(:)
     ATM_BC_OLD  = ATM_BC

     !atmospheric BC, potential infiltration (m/s)
     ATMPOT = PDDUM - QSEVA

     !source and sink term (m/s)
     CALL DISCHARGE(NSOIL,DZSO,SMCMAX,SH2O,WCND,QDIS,QDIS_RM,ILOC,JLOC)

    !LBC      = QIN     * 1.e-3  !rechage mm/s -> m/s ("+" to the aquifers)
     LBC      = 0.    ! m/s

     SS(:) = -ETRANI(:)-QDIS_RM(:)-DMICE(:)+DSH2O(:)

     NBACK   = 0.;  total_err = 0.
     WFLUX   = 0.

     IF(ILOC == 137 .and. JLOC == 7      ) THEN
     !  write(*,*)'PDDUM=',PDDUM*DT*1000.
        write(*,*)'ATMPOT=',ATMPOT*DT*1000.
     !  write(*,*) 'PSI     =',PSI
     !  write(*,*) 'SH2O     =',SH2O
     !  write(*,*) "(DZSO(IZ),IZ=1,NSOIL)",(DZSO(IZ),IZ=1,NSOIL)
     !  write(*,*) 'SUM(SH2O)     =',SUM(SH2O*1000.*DZSO)
     !  write(*,*) 'SMX-SMCR     =',SMX-SMCR
     !  write(*,*) 'SICEOLD      =',SICEOLD
     !  write(*,*) 'SMCR,DKSAT    =',SMCR(1),DKSAT(1)
     !  write(*,*) 'DMICE    =',DMICE*DT*1000.
     END IF

     !mixed-form RE time step [s]
     DTFINE         = MAX(DTFINE,10.)

     IF(VGN(1) >= 3.10) THEN  !sand
        IF(ATMPOT*DT*1000. >=  1.0) DTFINE = DT/(ATMPOT*DT*1000.)
     END IF

!===================================================================================================
     DO WHILE (DTFINE_ACC < DT)

 100 DTITER = MIN(DTFINE,(DT - DTFINE_ACC))
     DTFINE_ACC = DTFINE_ACC + DTITER

    IF ((NBACK == 1) .AND. (DTFINE == DTITER_MIN)) THEN
       print *, 'SMC SOLVER ERROR'
       print *, 'ILOC,JLOC,DTFINE,DTFINE_ACC,NBACK'
       print *, ILOC,JLOC,DTFINE,DTFINE_ACC,NBACK
       STOP
     ENDIF

     SMX_OLD (:) = SMX (:); PSI_OLD (:) = PSI (:) 

     BEG_WB = SUM(SMX*DZSO*1000.)

     SICE_TMP(:) = SICE_TMP(:) + DMICE(:) * DTITER / DZSO(:)

     ITER  = 0;    ICONV = .FALSE.

     CALL ATM_SWITCH(ILOC,JLOC,ATM_BC,ATMPOT,ATMACT,PSI(1),PONDTH,DTITER,HTOP)

     DO WHILE ((ITER < MAXITER) .AND. (.NOT. ICONV))

       ITER = ITER + 1

        IF(ILOC == 137 .and. JLOC == 7      ) THEN
         write(*,*)'iter,DTITER,DTFINE_ACC=',iter,DTITER,DTFINE_ACC
         write(*,'(a12,15f16.5)') 'PSI       =',PSI
         write(*,'(a12,15f16.5)') 'SMX       =',SMX
        !write(*,*) 'SMX-SMCR=',SMX-SMCR
        END IF

       DV     = SMX - SMX_OLD

       CALL  SRT_MIXED (NSOIL  ,OPT_WATRET,DZSO    ,DZI      ,ZNODE   ,DTITER ,ILOC  ,JLOC,    & !in
                        ATMPOT ,ATMACT    ,LBC     ,SS       ,                                 & !in
                        FCR    ,SMX       ,SICE_TMP,DV       ,PONDTH  ,  & !in
                        PSI_SAT,BEXP      ,DKSAT   ,SMCMAX   ,PSISAT  ,SMCR   ,VGN ,VGPSAT  ,  & !in
                        PSI    ,WCND      ,DPSI    ,DVDH     ,ATM_BC  ,  &
                        PSI_MIN,DRYCOR    ,FLAG_DRY,WFLUX   )      !out

       DO IZ = 1,NSOIL
           CALL GET_SMC(OPT_WATRET ,PSI(IZ)   ,SICE_TMP(IZ) ,&
                        SMCMAX(IZ) ,BEXP(IZ)  ,PSISAT(IZ) ,SMCR(IZ) ,VGN(IZ) ,VGPSAT(IZ) , &
                        SE(IZ)     ,SMX(IZ))
       END DO

       IF(ILOC == 137 .and. JLOC == 7      ) THEN
      !  write(*,'(a12,15f12.5)') 'SMX-SMCR  =',SMX-SMCR
         write(*,'(a12,15E12.3)') 'DPSI      =',DPSI
         write(*,'(a12,15E12.3)') 'DVDH      =',DVDH
      !  write(*,'(a12,15E12.3)') 'DVDH*DPSI =',DVDH*DPSI
       END IF

       CALL ITERCONV(ITER,OPT_WATRET,NSOIL,PONDTH,PSI,PSI_SAT,DPSI,DVDH,ATM_BC,ATMPOT,ATMACT,ISWITCH,ICONV)

       IF (ISWITCH) CALL ATM_SWITCH(ILOC,JLOC,ATM_BC,ATMPOT,ATMACT,PSI(1),PONDTH,DTITER,HTOP)

  !    IF(ILOC == 137 .and. JLOC == 7      ) THEN
  !      write(*,*) "FLAG_DRY",FLAG_DRY
  !      write(*,*) "ATM_BC",ATM_BC
  !      write(*,*) "ATMPOT",ATMPOT*DTITER*1000.
  !      write(*,*) "ATMACT",ATMACT*DTITER*1000.
  !      write(*,*) "ATMPOT-ATMACT",(ATMPOT-ATMACT)*DTITER*1000.
      !  write(*,*) 'SICE_TMP   =',SICE_TMP
!        write(*,*) 'WCND_V    =',WCND_V
!        write(*,*) 'PSISAT,SMCMAX,BEXP',psisat(1),SMCMAX(1),BEXP(1)
  !      write(*,*) 'SMCMAX,VGN,VGPSAT,SMCR,WCDN(1)',SMCMAX(1),VGN(1),VGPSAT(1),SMCR(1),WCND(1)
  !      write(*,*) '-------------------------'
  !    END IF
        
       if (dtiter .le. DTITER_MIN*2.) then
         write(OUT2,*) '------------------------------------------------'
         write(OUT2,*) 'ILOC,JLOC,dtiter,DTFINE_ACC,NBACK=',ILOC,JLOC,psisat(1),smcmax(1),dtiter,DTFINE_ACC,NBACK
         write(OUT2,*) 'PDDUM=',PDDUM*DT*1000.
         write(OUT2,*) "ATMPOT",ATMPOT*DTITER*1000.
         write(OUT2,*) "ATMACT",ATMACT*DTITER*1000.
         write(OUT2,*) "PSI_MIN",PSI_MIN
         write(OUT2,*) "FLAG_DRY",FLAG_DRY
         write(OUT2,*) "ZWT",zwt
         write(OUT2,*) 'SH2O:',SMX
         write(OUT2,*) 'SMX-SMCR    =',SMX-SMCR
         write(OUT2,*) 'SICE_TMP:',SICE_TMP
         write(OUT2,*) 'PSI:',PSI
         write(OUT2,*) 'DPSI:',DPSI
         write(OUT2,*) 'DVDH:',DVDH
         write(OUT2,*) 'DVDH*DPSI:',DVDH*DPSI
         write(OUT2,*) 'SMCMAX,VGN,VGPSAT,SMCR',SMCMAX(1),VGN(1),VGPSAT(1),SMCR(1)
         write(OUT2,*) 'ICONV:',ICONV
         if (iconv) write(OUT2,*) 'CONVERGED'
       endif

     END DO ! iteration loop
!===================================================================================================

     !mass balance check
     END_WB = SUM(SMX*DZSO*1000.)
     SSSUM  = SUM(SS*DTITER*1000.)

     WATERR = (END_WB-BEG_WB)-SSSUM-(ATMACT-LBC+DRYCOR)*DTITER*1000.

     IF (.NOT. ICONV) THEN
        ATM_BC         = ATM_BC_OLD
        DTFINE_ACC     = DTFINE_ACC - DTITER

        SICE_TMP(:)    = SICE_TMP(:) - DMICE(:) * DTITER / DZSO(:)

        DTFINE         = MAX(DTITER*0.5,DTITER_MIN)
        PSI            = PSI_OLD
        SMX            = SMX_OLD
        IF (DTFINE == DTITER_MIN) NBACK = NBACK + 1
        GOTO 100 
     ELSE
        NBACK     = 0
        total_err= total_err + waterr
        QDRYC      = QDRYC     + DRYCOR*DTITER*1000.               !mm
        WFLUXDT    = WFLUXDT   + WFLUX *DTITER*1000.               !mm

        IF (ATM_BC ==1) THEN
           HTOP     = HTOP + (ATMPOT-ATMACT)*DTITER*1000.             !mm
           IF(HTOP >= HTOPMX) THEN
              RUNSRFDT = RUNSRFDT + (HTOP-HTOPMX)                     !mm
              HTOP     = HTOPMX                                       !mm
           END IF
        END IF 

        PSI_OLD    = PSI
        SMX_OLD    = SMX 
        ATM_BC_OLD = ATM_BC

        IF (ITER .LE. 3) DTFINE = MIN(DT,DTFINE*1.5)

     END IF

         IF(ILOC == 137 .and. JLOC == 7      ) THEN
          write(*,*) 'WATERR ==',WATERR
        ! write(*,*) "SMX-SMCR =",SMX-SMCR
        ! write(*,*) "PSI      =",PSI
        ! write(*,*) "ATMPOT",ATMPOT*DTITER*1000.
        ! write(*,*) "ATMACT",ATMACT*DTITER*1000.
        ! write(*,*) "ATMPOT-ATMACT",(ATMPOT-ATMACT)*DTITER*1000.
        ! write(*,*) 'OVSAT*DTITER*1000.  =',OVSAT*DTITER*1000.
        ! write(*,*) 'QDIS*DTITER*1000.  =',QDIS*DTITER*1000.
        ! write(*,*) '(END_WB-BEG_WB), ATMACT*DTITER*1000., SSSUM, LBC*DTITER*1000.'
        ! write(*,*)  (END_WB-BEG_WB), ATMACT*DTITER*1000., SSSUM, LBC*DTITER*1000.
        !write(*,*) 'QDIS_RM*1000.*DTITER =',QDIS_RM*1000.*DTITER
        !write(*,*) '  SS*1000.*DTITER =',SS*1000.*DTITER
        !write(*,*) '        ETRANI*1000.*DTITER   =',ETRANI*1000.*DTITER
        !write(*,*) '        QDIS_RM*1000.*DTITER =',QDIS_RM*1000.*DTITER
        !write(*,*) '        DMICE*1000.*DTITER    =',DMICE*1000.*DTITER
        !write(*,*) '        DSH2O*1000.*DTITER    =',DSH2O*1000.*DTITER
        ! write(*,*) "RUNSRFDT=",RUNSRFDT
        ! write(*,*) "ATMPOT*1000.*DTITER=",ATMPOT*1000.*DTITER
        ! write(*,*) "ATMACT*1000.*DTITER=",ATMACT*1000.*DTITER
        !write(*,*) "PSI-PSI_SAT      =",PSI-PSI_SAT
        !write(*,*) "SMX      =",SMX
        ! write(*,*) "SICE_TMP     =",SICE_TMP
!         write(*,*) "SICE+SMX =",SICE_TMP+SMX
!        write(*,*) ILOC,JLOC,'PSISAT,SMCMAX,BEXP',psisat(1),SMCMAX(1),BEXP(1)
!         write(*,*) ILOC,JLOC,'SMCMAX,VGN,VGPSAT,SMCR',SMCMAX(1),VGN(1),1./VGPSAT(1),SMCR(1)
!         print*,'----------------------------------------------------'
         END IF

    if (abs(WATERR) .gt. 0.1) then
    write(OUT1,*) 'ILOC,JLOC==',ILOC,JLOC
    write(OUT1,*) '##################################################################################################'
    write(OUT1,*) 'DTITER,DTFINE_ACC=',DTITER,DTFINE_ACC
    write(OUT1,*) 'WATERR,SMCMAX,SMCMAX-SICE(1)',WATERR,SMCMAX(1),SMCMAX(1)-SICE_TMP(1)
    write(OUT1,*) 'DRYCOR*DTITER*1000.=',DRYCOR*DTITER*1000.
    write(OUT1,*) '(END_WB-BEG_WB) , ATMACT*DTITER*1000. , SSSUM , LBC*DTITER*1000.'
    write(OUT1,*) (END_WB-BEG_WB) , ATMACT*DTITER*1000. , SSSUM , LBC*DTITER*1000.
    write(OUT1,*) 'ATM_BC = ',ATM_BC
    write(OUT1,*) "FLAG_DRY",FLAG_DRY
    write(OUT1,*) 'RUNSRF_ADJ',(ATMPOT-ATMACT)*DTITER*1000.
    write(OUT1,*) 'QDIS*DTITER',QDIS*DTITER
    write(OUT1,*) "ATMPOT*1000.*DTITER=",ATMPOT*1000.*DTITER
    write(OUT1,*) "ATMACT*1000.*DTITER=",ATMACT*1000.*DTITER
    write(OUT1,*) '      -DV   =',-DZSO(NSOIL)*DV(NSOIL)*1000.
    write(OUT1,*) '      SS    =',SS(NSOIL)*1000.*DTITER
    write(OUT1,*) '     -LBC   =',-LBC*1000.*DTITER
    write(OUT1,*) 'SS*DTITER*1000.=',SS*DTITER*1000.
    write(OUT1,*) '  ETRANI*1000.*DTITER   =',ETRANI*1000.*DTITER
    write(OUT1,*) '  QDIS_RM*1000.*DTITER =',QDIS_RM*1000.*DTITER
    write(OUT1,*) '  DMICE*1000.*DTITER    =',DMICE*1000.*DTITER
    write(OUT1,*) '  DSH2O*1000.*DTITER    =',DSH2O*1000.*DTITER
    write(OUT1,*) 'ZWT',zwt
    write(OUT1,*) 'WCND',wcnd
    write(OUT1,*) 'DVDH',dvdh
    write(OUT1,*) 'SH2O-SMCR',smx-SMCR
    write(OUT1,*) 'SICE_TMP',sice_TMP
    write(OUT1,*) 'SH2O+SICE',smx+sice_TMP
    write(OUT1,*) 'PSI',psi
    write(OUT1,*) 'DPSI:',DPSI
    write(OUT1,*) 'DVDH*DPSI   =',DVDH*DPSI
    write(OUT1,*) 'PSISAT,SMCMAX,BEXP',psisat(1),SMCMAX(1),BEXP(1)
    write(OUT1,*) 'SMCMAX,VGN,VGPSAT,SMCR',SMCMAX(1),VGN(1),VGPSAT(1),SMCR(1)
    endif

   END DO !Noah DT

   IF(ILOC == 137 .and. JLOC == 7      ) THEN
          write(*,*) 'HTOP     =',HTOP
          write(*,*) 'total_ERR =',total_err
   END IF

   SH2O(:) = SMX(:)

!  CALL GET_ZWT (OPT_RUN,VARSD,NSOIL,PSI_SAT,ZNODE,DZSO,PSI,WA,ZWT,ILOC,JLOC)

   QDRYC  = QDRYC  /DT       !mm/s
   WFLUXDT= WFLUXDT/DT       !mm/s
   RSINEX = RUNSRFDT/DT      !mm/s

  !IF(ILOC == 137 .and. JLOC == 7      ) THEN
  !  write(*,*) 'WFLUXDT*DT (mm/hour)=',WFLUXDT*DT
  !ENDIF

  END SUBROUTINE MIXEDRE

! ==================================================================================================

 SUBROUTINE SRT_MIXED (NSOIL   ,OPT_WATRET,DZSO     ,DZI      ,ZNODE   ,DT     ,ILOC,JLOC      , & !in
                       ATMPOT  ,ATMACT    ,LBC      ,SS       ,                                 & !in 
                       FCR     ,SH2O      ,SICE     ,DV       ,PONDTH  , & !in
                       PSI_SAT ,BEXP      ,DKSAT    ,SMCMAX   ,PSISAT  ,SMCR   ,VGN ,VGPSAT , & !in 
                       PSI     ,WCND      ,DPSI     ,DVDH     ,ATM_BC  ,  &
                       PSI_MIN ,DRYCOR    ,FLAG_DRY ,WFLUX    )       !out
! ----------------------------------------------------------------------
! calculate the right hand side of the time tendency term and the matrix
! coefficients for the tri-diagonal matrix of the implicit time scheme.
! ----------------------------------------------------------------------
        IMPLICIT NONE
! ----------------------------------------------------------------------
!input
      INTEGER,                     INTENT(IN)   :: OPT_WATRET
      INTEGER,                     INTENT(IN)   :: NSOIL,ILOC,JLOC
      INTEGER                    , INTENT(IN)   :: ATM_BC
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: DZSO
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: DZI
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: ZNODE
      REAL   ,                     INTENT(IN)   :: DT
      REAL   ,                     INTENT(IN)   :: PONDTH

      REAL   ,                     INTENT(IN)   :: ATMPOT  !atmospheric forcing(P-E),potential(m/s)
      REAL   ,                     INTENT(IN)   :: LBC     !lower BC (m/s)
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: SS      !source&sink (m/s)

      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: PSI_SAT
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: PSISAT
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: BEXP    !B parameter
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: SMCMAX  !porosity, saturated value of soil moisture (volumetric)
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: DKSAT   !saturated soil hydraulic conductivity(m/s)
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: SMCR    !residual volumetric soil watercontent(volumetric)
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: VGPSAT  !VG parameter (m)
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: VGN     !VG parameter n

      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: FCR
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: SH2O    !liq. soil moisture content
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: SICE    !soli. soil moisture content
      REAL   , DIMENSION(1:NSOIL), INTENT(IN)   :: DV      !liq. moisture change 

!in & out

      REAL   , DIMENSION(1:NSOIL), INTENT(INOUT):: PSI
      REAL   ,                     INTENT(INOUT):: ATMACT  !actual infiltation [m/s]

!outputs

      INTEGER, DIMENSION(1:NSOIL), INTENT(OUT)  :: FLAG_DRY
      REAL   , DIMENSION(1:NSOIL), INTENT(OUT)  :: WCND
      REAL   , DIMENSION(1:NSOIL), INTENT(OUT)  :: DVDH    !specific moisture capacity [m-1]
      REAL   , DIMENSION(1:NSOIL), INTENT(OUT)  :: DPSI    !water pressure change between two iterations [m]
      REAL   , DIMENSION(1:NSOIL), INTENT(OUT)  :: PSI_MIN !
      REAL   , DIMENSION(1:NSOIL), INTENT(OUT)  :: WFLUX
      REAL   ,                     INTENT(OUT)  :: DRYCOR  !water deficit to SMCR+0.001 fro 2 -> NSOIL-1 [m/s]

!local
      INTEGER                                   :: K

      REAL   , DIMENSION(1:NSOIL)               :: RHSTT
      REAL   , DIMENSION(1:NSOIL)               :: AI
      REAL   , DIMENSION(1:NSOIL)               :: BI
      REAL   , DIMENSION(1:NSOIL)               :: CI
      REAL   , DIMENSION(1:NSOIL)               :: DSMDZ
      REAL   , DIMENSION(1:NSOIL)               :: QI,QO
      REAL   , DIMENSION(1:NSOIL)               :: WCND_V !hydrulic conductivity at interface  (m/s)
      REAL   , DIMENSION(1:NSOIL) :: DRYERR

      REAL   , DIMENSION(1:NSOIL)               :: PI
      REAL                                      :: QIC    !water flux from a lower layer [m/s]
      REAL                                      :: QOC    !water flux to an upper layer [m/s]
      REAL                                      :: C      !a temporary varaible

!-------------------------------------------------------------------------------------------------------------------

      QOC         = 0. 
      QIC         = 0.
      FLAG_DRY(:) = 0
      DRYERR(:)   = 0.
      DRYCOR      = 0.

!-------------------------------------------------------------------------------------------------------------------
! (0) compute the minimum PSI equivalent to 0.001 m3/m3 in volumetric soil moisture for VG
!-------------------------------------------------------------------------------------------------------------------

       PSI_MIN(:)  = 0.
       IF (OPT_WATRET == 1) THEN
          DO K = 1, NSOIL
             CALL GET_PSI(OPT_WATRET, SMCR(K)+1.0E-3,SICE(K), &
                          SMCMAX(K),SMCR(K),VGN(K),VGPSAT(K), BEXP(K),  PSISAT(K)  , &
                          PSI_MIN(K))
          END DO
       END IF

!-------------------------------------------------------------------------------------------------------------------
!(1) update soil conductivity(WCND) & specific moisture capacity (DVDH) at node K using PSI/SMC
!-------------------------------------------------------------------------------------------------------------------

       DO K = 1,NSOIL
           CALL GET_CND (OPT_WATRET,PSI(K) ,SH2O(K) ,SICE(K) ,FCR(K) ,&
                         BEXP(K),DKSAT(K),SMCMAX(K) ,PSISAT(K),SMCR(K) ,VGN(K) ,VGPSAT(K) ,&
                         DVDH(K),WCND(K))
       END DO

!-------------------------------------------------------------------------------------------------------------------
!(2) update soil conductivety(WCND) at interface K-1/2 K+1/2 using soil depth-weighted average
!-------------------------------------------------------------------------------------------------------------------

       DO K = 1,NSOIL-1
         WCND_V(K)     =0.5*(WCND(K)+WCND(K+1))
       ENDDO
       WCND_V(NSOIL)     = WCND(NSOIL)

!-------------------------------------------------------------------------------------------------------------------
!(3) prepare the coefficient of mixed-RE
!-------------------------------------------------------------------------------------------------------------------
       K = 1
       IF (ATM_BC == 0) THEN
          QO(K)    = -ATMPOT
          QI(K)    = -WCND_V(K  )

          C        = DVDH(K)*DZSO(K)/DT
          AI(K)    = 0.
          CI(K)    = -WCND_V(K  ) / DZI(K  )
          BI(K)    = C - (AI(K)+CI(K))
          RHSTT(K) = C*PSI(K)-DV(K)*DZSO(K)/DT+(QI(K)-QO(K)+SS(K))
       ELSE
          AI(K)    = 0.
          CI(K)    = 0.
          BI(K)    = 1.
          RHSTT(K) = PONDTH
       ENDIF

       IF (OPT_WATRET == 1) THEN
          IF (PSI(K) <= PSI_MIN(K) .AND. (ATMPOT+SS(1)) <= 0.0) THEN 
             AI(K)    = 0.
             CI(K)    = 0.
             BI(K)    = 1.
             RHSTT(K) = PSI_MIN(K)

             FLAG_DRY(K) = 1
          END IF
       END IF

       DO K = 2, NSOIL-1
          C        = DVDH(K)*DZSO(K)/DT
          QO(K)    = -WCND_V(K-1)
          QI(K)    = -WCND_V(K  )

          AI(K)    = -WCND_V(K-1) / DZI(K-1)
          CI(K)    = -WCND_V(K  ) / DZI(K  )
          BI(K)    = C - (AI(K)+CI(K))
          RHSTT(K) = C*PSI(K)-DZSO(K)*DV(K)/DT+(QI(K)-QO(K)+SS(K))
       END DO

       IF (OPT_WATRET == 1) THEN
         DO K = 2,NSOIL-1

           QOC        = -WCND_V(K-1) * ((PSI(K)-PSI(K-1))/ DZI(K-1)-1.)

           IF (PSI(K) <= PSI_MIN(K) .AND. QOC+SS(K) <= 1.E-8) THEN 
             AI(K)    = 0.
             CI(K)    = 0.
             BI(K)    = 1.
             RHSTT(K) = PSI_MIN(K)

             FLAG_DRY(K) = 1
           END IF

         END DO
       END IF

       K = NSOIL
       C        = DVDH(K)*DZSO(K)/DT
       QO(K)    = -WCND_V(K-1)
       QI(K)    = -LBC

       AI(K)    = -WCND_V(K-1)/ DZI(K-1)
       CI(K)    = 0.
       BI(K)    = C - (AI(K)+CI(K))
       RHSTT(K) = C*PSI(K)-DV(K)*DZSO(K)/DT+(QI(K)-QO(K)+SS(K))

       IF (OPT_WATRET == 1) THEN
           K = NSOIL

           QOC        = -WCND_V(K-1) * ((PSI(K)-PSI(K-1))/ DZI(K-1)-1.)
           IF (PSI(K) <= PSI_MIN(K) .AND. QOC+SS(K) <= 1.E-8) THEN 
             AI(K)    = 0.
             CI(K)    = 0.
             BI(K)    = 1.
             RHSTT(K) = PSI_MIN(K)
             FLAG_DRY(K) = 1
           END IF

       END IF
!-------------------------------------------------------------------------------------------------------------------
!(4) solve the triangular matrix to get delta PSI
!-------------------------------------------------------------------------------------------------------------------

       CALL TRIDAG(NSOIL, AI, BI, CI, RHSTT, PI)

!-------------------------------------------------------------------------------------------------------------------
!(5) update pressure head (PSI) and its change (DPSI)
!-------------------------------------------------------------------------------------------------------------------

       DO K = 1,NSOIL
          DPSI(K)  =  PI(K) - PSI(K)
          PSI(K)   =  PI(K)
       ENDDO

!-------------------------------------------------------------------------------------------------------------------
!(6) calculate infiltration rate ( m/s) to keep the head at PONDTH
!-------------------------------------------------------------------------------------------------------------------

       IF (ATM_BC ==1) THEN                !specified head upper BC
          QIC     = -WCND_V(1) * ((PSI(2)-PSI(1))/ DZI(1) - 1.)
          ATMACT  = DZSO(1)*DV(1)/DT + QIC - SS(1)
       ELSE                                !flux upper BC
          ATMACT = ATMPOT
       ENDIF

       WFLUX(1)     = ATMACT
       DO K = 2,NSOIL
          WFLUX(K)   = -WCND_V(K-1) * ((PSI(K)-PSI(K-1)) / DZI(K-1)-1.)
       END DO

!-------------------------------------------------------------------------------------------------------------------
!(8) calculate the rate ( m/s) to keep the head at PSI_MIN (1 -> NSOIL-1)
!-------------------------------------------------------------------------------------------------------------------

       IF (FLAG_DRY(1) == 1) THEN        ! to correct surface evaporation 
          QIC        = WFLUX(2)
          IF(FLAG_DRY(2) == 1) QIC = 0.
          DRYERR(1)  = DZSO(1)*DV(1)/DT + QIC - SS(1) - ATMPOT   !m/s
       END IF

       DO K = 2,NSOIL
          IF (FLAG_DRY(K) == 1) THEN
             QOC        = WFLUX(K)            
             IF(K == NSOIL) THEN
                QIC = -LBC
             ELSE
                QIC        = WFLUX(K+1)
             END IF

             IF (FLAG_DRY(MIN(NSOIL,K+1)) == 1) QIC = 0. ; IF (FLAG_DRY(K-1) == 1) QOC = 0.
             DRYERR(K)  = DZSO(K)*DV(K)/DT + QIC - QOC - SS(K)   !m/s

!     IF(ILOC == 137 .and. JLOC == 7) THEN
!       write(*,*) 'K ==',K
!       write(*,*) 'DRYERR(K)=',DRYERR(k)*1000.*DT
!       write(*,*) '   DZSO*DV*1000.=',DZSO(K)*DV(K)*1000.
!       write(*,*) '   QIC*1000.*DT  =',QIC*1000.*DT
!       write(*,*) '   QOC*1000.*DT    =',QOC*1000.*DT
!       write(*,*) '   SS*1000.*DT     =',SS(K)*1000.*DT
!     ENDIF
          END IF
       END DO

       DRYCOR = SUM(DRYERR(1:NSOIL))  ! to correct EDIR

!     IF(ILOC == 137 .and. JLOC == 7) THEN
!       write(*,*) 'FLAG_DRYR=',FLAG_DRY
!       write(*,*) 'PSI=',PSI
!       write(*,*) 'SH2O=',SH2O
!       write(*,*) 'DRYERR=',DRYERR*1000.*DT
!     END IF

  END SUBROUTINE SRT_MIXED

! ==================================================================================================================

  SUBROUTINE ITERCONV(ITER,OPT_WATRET,NSOIL,PONDTH,PSI,PSI_SAT,DPSI,DVDH,ATM_BC,ATMPOT,ATMACT,ISWITCH,ICONV)
! ----------------------------------------------------------------------------------------------------------
!     determin if an iteration succeeds
! ----------------------------------------------------------------------------------------------------------
        IMPLICIT NONE 
! ----------------------------------------------------------------------------------------------------------
       INTEGER     ,INTENT(IN)             :: ITER
       INTEGER     ,INTENT(IN)             :: NSOIL
       INTEGER,                  INTENT(IN) :: OPT_WATRET
       REAL        ,INTENT(IN)             :: PONDTH
       REAL        ,INTENT(IN)             :: PSI(NSOIL)
       REAL        ,INTENT(IN)             :: PSI_SAT(NSOIL)
       REAL        ,INTENT(IN)             :: DPSI(NSOIL)
       REAL        ,INTENT(IN)             :: DVDH(NSOIL)
       INTEGER     ,INTENT(IN)             :: ATM_BC
       REAL        ,INTENT(IN)             :: ATMPOT
       REAL        ,INTENT(IN)             :: ATMACT

       LOGICAL     ,INTENT(OUT)            :: ICONV
       LOGICAL     ,INTENT(OUT)            :: ISWITCH 

       INTEGER                             :: K 
! ----------------------------------------------------------------------------------------------------------

       IF(ITER == 1) THEN
            ICONV = .FALSE.  !ensure at least 2 iterations -> DV /= 0. -> conserve mass
       ELSE
            ICONV = .TRUE.
       END IF

       ISWITCH = .FALSE.

       DO K=1,NSOIL
          IF(OPT_WATRET == 1) THEN
             IF(PSI(K) < -10000000.) THEN
                IF (ABS(DPSI(K))  >  1000.0   ) ICONV = .FALSE.
             ELSE IF(PSI(K) < -1000000.) THEN
                IF (ABS(DPSI(K))  >  100.0   ) ICONV = .FALSE.
             ELSE IF(PSI(K) < -100000.) THEN
                IF (ABS(DPSI(K))  >  10.0   ) ICONV = .FALSE.
             ELSE IF(PSI(K) < -10000.) THEN
                IF (ABS(DPSI(K))  >  1.0    ) ICONV = .FALSE.
             ELSE IF(PSI(K) < -1000.) THEN
                IF (ABS(DPSI(K))  >  1.0E-1 ) ICONV = .FALSE.
             ELSE IF(PSI(K) < -100.) THEN
                IF (ABS(DPSI(K))  >  1.0E-2 ) ICONV = .FALSE.
             ELSE IF(PSI(K) < -10.) THEN
                IF (ABS(DPSI(K))  >  1.0E-3 ) ICONV = .FALSE.
             ELSE IF(PSI(K) <  -1. ) THEN
                IF (ABS(DPSI(K))  >  1.0E-3 ) ICONV = .FALSE.
            !ELSE IF(PSI(K) >=  0.1 ) THEN
             ELSE IF(PSI(K) >=  0.0 ) THEN
                IF (ABS(DPSI(K))  >  1.0 ) ICONV = .FALSE.
             ELSE
                IF (ABS(DPSI(K))  >  1.0E-3 ) ICONV = .FALSE.    !-1.0->0.1 m
             END IF
          END IF

          IF(OPT_WATRET == 2) THEN
             IF(PSI(K) < -1000000.) THEN
                IF (ABS(DPSI(K))  >  100.0  ) ICONV = .FALSE.
             ELSE IF(PSI(K) < -100000.) THEN
                IF (ABS(DPSI(K))  >  10.0   ) ICONV = .FALSE.
             ELSE IF(PSI(K) < -10000.) THEN
                IF (ABS(DPSI(K))  >  1.0    ) ICONV = .FALSE.
             ELSE IF(PSI(K) < -1000.) THEN
                IF (ABS(DPSI(K))  >  1.0E-1 ) ICONV = .FALSE.
             ELSE IF(PSI(K) < -100.) THEN
                IF (ABS(DPSI(K))  >  1.0E-2 ) ICONV = .FALSE.
             ELSE IF(PSI(K) <  -10. ) THEN
                IF (ABS(DPSI(K))  >  1.0E-3 ) ICONV = .FALSE.
             ELSE IF(PSI(K) > PSI_SAT(K) ) THEN
                IF (ABS(DPSI(K))  >  1. ) ICONV = .FALSE.
             ELSE
                IF (ABS(DPSI(K))  >  1.0E-3 ) ICONV = .FALSE.
             END IF
          END IF
       END DO

       IF ((ICONV) .AND. (ATM_BC == 0)) THEN
          IF(PSI(1) >= PONDTH) ISWITCH = .TRUE.
       END IF

       IF ((ICONV) .AND. (ATM_BC == 1)) THEN
          IF ((ATMPOT /= 0.) .AND. (ATMACT-ATMPOT>1.e-4)) ISWITCH = .TRUE.
       ENDIF

       IF (ISWITCH) ICONV = .FALSE.

  END SUBROUTINE ITERCONV

! ==================================================================================================================
  SUBROUTINE GET_PSI (OPT_WATRET,SH2O    ,SICE   , &
                      SMCMAX    ,SMCR    ,VGN    ,VGPSAT, BEXP,  PSISAT  ,&
                      PSI)
! -----------------------------------------------------------------------------------
! calculate soil potential head (m) using CH retention curve, PSI is negtive
! ----------------------------------------------------------------------------------
        IMPLICIT NONE
! input 
        INTEGER ,INTENT(IN) :: OPT_WATRET   !
        REAL    ,INTENT(IN) :: SH2O         !liq.
        REAL    ,INTENT(IN) :: SICE         !ice
        REAL    ,INTENT(IN) :: SMCMAX       !porosity, saturated value of soil moisture (volumetric)
        REAL    ,INTENT(IN) :: BEXP         !B parameter
        REAL    ,INTENT(IN) :: PSISAT
        REAL    ,INTENT(IN) :: SMCR         !residual volumetric soil water content(L3/L3)
        REAL    ,INTENT(IN) :: VGN          !VG parameter n ()
        REAL    ,INTENT(IN) :: VGPSAT      !VG parameter (m)

! output
        REAL    ,INTENT(OUT) :: PSI         !soil water potential (m)

! local
        REAL                :: EPORE       !effective prosity
        REAL                :: VGM
        REAL                :: BETA
        REAL                :: SE           !soil effective saturation

! ----------------------------------------------------------------------------------

     EPORE = MAX(1.e-3,SMCMAX - SICE)

     IF (OPT_WATRET == 1) THEN

        VGM   = 1. - 1./VGN
        SE    = (SH2O - SMCR) / (EPORE - SMCR)
        BETA  = (SE**(-1./VGM) - 1.)**(1./VGN)
        PSI   = -1. * BETA * VGPSAT

     ELSE IF (OPT_WATRET == 2) THEN

        IF (SH2O <= EPORE) THEN
          PSI = -PSISAT * MAX(MIN(1.0,SH2O/EPORE),1.e-3)**(-BEXP)
        ELSE
          PSI = -PSISAT
        ENDIF

     END IF

  END SUBROUTINE GET_PSI

! ==================================================================================================================
  SUBROUTINE GET_EQM_PSI (OPT_WATRET, ZWT  ,ZNODE  ,PSISAT  ,VGPSAT ,PSI )
! -----------------------------------------------------------------------------------
! calculate equilibrium soil potential head (m) for initialization of PSI
! ----------------------------------------------------------------------------------
        IMPLICIT NONE
! ----------------------------------------------------------------------------------
! input 
      INTEGER ,INTENT(IN) :: OPT_WATRET   !
      REAL    ,INTENT(IN) :: ZWT
      REAL    ,INTENT(IN) :: ZNODE
      REAL    ,INTENT(IN) :: PSISAT
      REAL    ,INTENT(IN) :: VGPSAT !(L-1)

! output
      REAL    ,INTENT(OUT):: PSI

! ----------------------------------------------------------------------------------

     IF (OPT_WATRET == 1) THEN
       !PSI = MIN(-VGPSAT    , -VGPSAT - (ZWT - ZNODE))
        PSI =  -VGPSAT - (ZWT - ZNODE)
     ELSE IF (OPT_WATRET == 2) THEN
        PSI = MIN(-PSISAT-0.1, -PSISAT - (ZWT - ZNODE))
     END IF

  END SUBROUTINE GET_EQM_PSI

! ==================================================================================================================
  SUBROUTINE GET_CND (OPT_WATRET, PSI  ,SH2O   ,SICE   ,FCR   ,&
                      BEXP ,DKSAT  ,SMCMAX ,  PSISAT   ,SMCR  ,VGN ,VGPSAT, &
                      DVDH ,WCND)
! --------------------------------------------------------------------------------
! calculate specific moisture capacity and soil hydraulic conductivity using CH
! --------------------------------------------------------------------------------
        IMPLICIT NONE
! --------------------------------------------------------------------------------
! input 
        INTEGER ,INTENT(IN) :: OPT_WATRET   !
        REAL    ,INTENT(IN) :: PSI
        REAL    ,INTENT(IN) :: SH2O
        REAL    ,INTENT(IN) :: SICE
        REAL    ,INTENT(IN) :: BEXP         !B parameter
        REAL    ,INTENT(IN) :: SMCMAX       !porosity, saturated value of soil moisture (volumetric)
        REAL    ,INTENT(IN) :: PSISAT
        REAL    ,INTENT(IN) :: DKSAT        !saturated soil hydraulic conductivity
        REAL    ,INTENT(IN) :: SMCR         !residual volumetric soil water content(L3/L3)
        REAL    ,INTENT(IN) :: VGN          !VG parameter n
        REAL    ,INTENT(IN) :: VGPSAT       !VG parameter (L)
        REAL    ,INTENT(IN) :: FCR

! output
        REAL,INTENT(OUT) :: WCND
        REAL,INTENT(OUT) :: DVDH

! local
        REAL                :: SE
        REAL                :: VGM
        REAL                :: OMEGA,BETA,V1
        REAL :: EXPON
        REAL :: FACTR
        REAL :: EPORE !effective prosity
! ----------------------------------------------------------------------

    EPORE = MAX(1.e-3,SMCMAX - SICE)

    IF (OPT_WATRET == 1) THEN

        VGM  = 1. - 1./VGN

        IF (PSI <= -1.E-6) THEN
           SE    = MAX(0.,((SH2O+SICE) - SMCR)/(SMCMAX - SMCR))
           OMEGA = ABS(SE)**(1./VGM)
           V1    = 1. - (ABS(1. - OMEGA)**VGM)
           WCND  = SQRT(SE)*V1*V1*DKSAT

           BETA  = (1. + ABS(PSI/VGPSAT)**VGN)**(VGM + 1.)
           DVDH  = VGN * VGM / VGPSAT * (EPORE - SMCR) / BETA * ABS(PSI/VGPSAT)**(VGN-1)
        ELSE
           WCND  = DKSAT
           DVDH =  0.0
        END IF

    ELSE IF (OPT_WATRET == 2) THEN

        IF (PSI <= -PSISAT) THEN
          FACTR = MAX(0.01, (SH2O+SICE)/SMCMAX)
          EXPON = 2.0*BEXP + 3.0
          WCND  = DKSAT * FACTR ** EXPON
          DVDH  = -SH2O/(BEXP*PSI)
        ELSE
          WCND  = DKSAT
          DVDH  = 0.0
         !DVDH  = -SMCMAX/(BEXP*PSISAT)
        ENDIF

    END IF

    WCND  = (1.-FCR) * WCND 

  END SUBROUTINE GET_CND

! ==================================================================================================================
  SUBROUTINE GET_SMC (OPT_WATRET, PSI  ,SICE ,               &
                      SMCMAX ,BEXP ,PSISAT ,SMCR ,VGN ,VGPSAT  ,&
                      SE   ,SH2O)
! --------------------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity using BC
! --------------------------------------------------------------------------------
        IMPLICIT NONE
! --------------------------------------------------------------------------------

! input 

        INTEGER ,INTENT(IN) :: OPT_WATRET
        REAL    ,INTENT(IN) :: PSI
        REAL    ,INTENT(IN) :: SICE
        REAL    ,INTENT(IN) :: SMCMAX       !porosity, saturated value of soil moisture (volumetric)
        REAL    ,INTENT(IN) :: BEXP         !B parameter
        REAL    ,INTENT(IN) :: PSISAT
        REAL    ,INTENT(IN) :: SMCR         !residual volumetric soil water content(L3/L3)
        REAL    ,INTENT(IN) :: VGN          !VG parameter n
        REAL    ,INTENT(IN) :: VGPSAT       !VG parameter (L)

! output
        REAL    ,INTENT(OUT) :: SE          !effective saturation (-)
        REAL    ,INTENT(OUT) :: SH2O

        REAL                 :: EPORE !effective prosity
        REAL                 :: BETA
        REAL                 :: VGM

! --------------------------------------------------------------------------------
      EPORE = MAX(1.e-3,SMCMAX - SICE)

      IF(OPT_WATRET == 1) THEN

         VGM  = 1. - 1./VGN
         IF (PSI .LT. -1.0E-4) THEN
            BETA = (ABS(PSI/VGPSAT))**VGN
            SE   = ABS(1./(BETA+1.))**VGM
         ELSE
            SE=1.
         END IF
         SH2O = SE*(EPORE-SMCR) + SMCR

      ELSE IF(OPT_WATRET == 2) THEN

         IF (PSI <= -PSISAT) THEN
           SH2O    = EPORE * (-PSI/PSISAT)**(-1./BEXP)
         ELSE
           SH2O    = EPORE
         ENDIF
    
      END IF

  END SUBROUTINE GET_SMC

! ==================================================================================================================
! SOLVING A TRIDIAGONAL SYSTEM OF EQUATION (PRESS ET AL. 2002)
!-------------------------------------------------
  SUBROUTINE TRIDAG(N, A, B, C, R, U)
    IMPLICIT NONE

    !INPUT
    INTEGER          ,INTENT(IN)      :: N
    REAL             ,INTENT(IN)      :: A(N)
    REAL             ,INTENT(IN)      :: B(N)
    REAL             ,INTENT(IN)      :: C(N)
    REAL             ,INTENT(IN)      :: R(N)

    !OUTOUT
    REAL             ,INTENT(OUT)     :: U(N)

    !LOCAL
    INTEGER                           :: J
    REAL                              :: BET
    REAL                              :: GAM(N)
!-------------------------------------------------
    BET  = B(1)
    U(1) = R(1)/BET

    DO J=2,N
        GAM(J) = C(J-1) / BET
        BET    = B(J) - A(J)*GAM(J)

        IF (ABS(BET) .EQ. 0.) RETURN
        U(J) = (R(J) - A(J)*U(J-1))/BET

    ENDDO

    !back-substitution
    DO J=N-1,1,-1
        U(J) = U(J) - (GAM(J+1)*U(J+1))
    ENDDO

  END SUBROUTINE TRIDAG

! ==================================================================================================================
  SUBROUTINE ATM_SWITCH(ILOC,JLOC,ATM_BC,ATMPOT,ATMACT,PSI,PONDTH,DTITER,HTOP)
! -----------------------------------------------------------------------------------
! atmospheric BC switch in case of large precipitation
! ----------------------------------------------------------------------------------
     IMPLICIT NONE
! ----------------------------------------------------------------------

     INTEGER    , INTENT(INOUT)  :: ATM_BC !ATM_BC: 0->Neuman ; 1->Dirichlet
     REAL       , INTENT(IN)     :: ATMPOT
     REAL       , INTENT(IN)     :: HTOP
     REAL       , INTENT(INOUT)  :: ATMACT
     REAL       , INTENT(INOUT)  :: PSI
     INTEGER    , INTENT(IN)     :: ILOC,JLOC
     REAL       , INTENT(IN)     :: DTITER
     REAL       , INTENT(IN)     :: PONDTH

     !specified head BC
     IF (ATM_BC == 1) THEN
         IF ((ATMPOT < 0. .AND. ATMACT>0.) .OR. (ATMACT > ATMPOT)) THEN  
           ATM_BC    = 0
           GOTO 800
         ENDIF
     ENDIF

     !flux BC
     IF (ATM_BC == 0) THEN
        IF ((PSI >= PONDTH) .OR. HTOP > 0.) THEN
          ATM_BC     = 1
          GOTO 800
        ENDIF
     ENDIF

800 CONTINUE

  END SUBROUTINE ATM_SWITCH
! ==================================================================================================================
  SUBROUTINE DISCHARGE(NSOIL,DZSO,SMCMAX,SH2O,WCND,QDIS,QDIS_RM,ILOC,JLOC)
! ----------------------------------------------------------------------------------
! subsurface discharge based on TOPMODEL but using water deficit
! ----------------------------------------------------------------------------------

     IMPLICIT NONE

      INTEGER, INTENT(IN) :: NSOIL,ILOC,JLOC
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: DZSO
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMCMAX
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: WCND

      REAL,                     INTENT(OUT) :: QDIS
      REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: QDIS_RM

      INTEGER :: IZ
      REAL    :: FFF
      REAL    :: QDISMX
      REAL    :: WTSUB
      REAL    :: DZ
      REAL    :: DEFICIT
! ----------------------------------------------------------------------------------
      QDIS_RM = 0.

      DEFICIT = SUM( (SH2O(1:NSOIL)-SMCMAX(1:NSOIL))*DZSO(1:NSOIL) )  !m

      WTSUB = SUM( WCND(1:NSOIL)*DZSO(1:NSOIL) )
      DZ    = SUM( DZSO(1:NSOIL) )

      IF(ILOC == 137 .and. JLOC == 7      ) THEN
        write(*,*) 'DEFICIT=',DEFICIT
        write(*,*) 'WTSUB/DZ * 1000.=',WTSUB/DZ * 1000.
      END IF

      FFF      = 12.0
      QDISMX   = 10.*WTSUB/DZ  !m/s
      QDIS     = QDISMX * EXP(FFF*DEFICIT)  !m/s

      IF(ILOC == 137 .and. JLOC == 7      ) THEN
        write(*,*) 'QDIS*DT=',QDIS*3600.*1000.
      END IF

      QDIS_RM(1:NSOIL)=QDIS*WCND(1:NSOIL)*DZSO(1:NSOIL)/WTSUB

      !IF(ILOC == 137 .and. JLOC == 7      ) THEN
      !  write(*,*) 'WCND   =',WCND
      !  write(*,*) 'QDIS_RM=',QDIS_RM
      !END IF

  END SUBROUTINE DISCHARGE
! ==================================================================================================================

end module module_Noahlsm_mixedRE