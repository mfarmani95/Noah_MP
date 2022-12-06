MODULE module_sf_Noahlsm_gridded_input

  contains

!----------------------------------------------------------
        SUBROUTINE READLAND(DIR,IX,JX,VARSD,NSOIL,DZS, &
                            VEGTYP,SOLTYP,SLOPE,LAT,LON,LANDSEA,tbot,ISC, &
                            topostdv,nsoil_var)

	IMPLICIT NONE

        CHARACTER(len=256) :: DIR 
        INTEGER, INTENT(IN) :: IX,JX,NSOIL,VARSD
        REAL   , INTENT(IN) :: DZS(1:NSOIL)
        INTEGER :: I,J,K

        INTEGER, INTENT(OUT), DIMENSION(IX,JX) :: VEGTYP
        INTEGER, INTENT(OUT), DIMENSION(IX,JX) :: SOLTYP
        INTEGER, INTENT(OUT), DIMENSION(IX,JX) :: SLOPE
        INTEGER, INTENT(OUT), DIMENSION(IX,JX) :: LANDSEA
        INTEGER, INTENT(OUT), DIMENSION(IX,JX) :: ISC     !soil color index
        REAL,    INTENT(OUT), DIMENSION(IX,JX) :: LAT
        REAL,    INTENT(OUT), DIMENSION(IX,JX) :: LON
        REAL,    INTENT(OUT), DIMENSION(IX,JX) :: TBOT
        REAL,    INTENT(OUT), DIMENSION(IX,JX) :: topostdv
        INTEGER, INTENT(OUT), DIMENSION(IX,JX) :: NSOIL_VAR

        REAL,                 DIMENSION(IX,JX) :: ZSOIL_VAR
        REAL                                   :: ZMID
        REAL                                   :: ZSOIL(1:NSOIL)

        INCLUDE 'netcdf.inc'

        INTEGER STATUS
        INTEGER NCID
        INTEGER START(2), COUNT(2)
        DATA START /1,  1/       
        INTEGER lonID,latID,landID,plotID,vegeID,soilID,tbotID,iscID,topoID,soildID

        COUNT(1) = IX
        COUNT(2) = JX

        STATUS=NF_OPEN(TRIM(DIR)//'/static/lon_lat.nc',NF_NOWRITE,NCID)
        STATUS=NF_INQ_VARID (NCID, 'lat2d', latID)
        STATUS=NF_INQ_VARID (NCID, 'lon2d', lonID)
        status=NF_ENDDEF(ncid)
        STATUS=NF_GET_VARA_real(NCID,latID,START,COUNT,LAT)
        STATUS=NF_GET_VARA_real(NCID,lonID,START,COUNT,LON)
        STATUS=NF_CLOSE(NCID)

        STATUS=NF_OPEN(TRIM(DIR)//'/static/topostdv.nc',NF_NOWRITE,NCID)
        STATUS=NF_INQ_VARID (NCID, 'NLDAS_STD', topoID)
        status=NF_ENDDEF(ncid)
        STATUS=NF_GET_VARA_real(NCID,topoID,START,COUNT,topostdv)
        STATUS=NF_CLOSE(NCID)
 
        STATUS=NF_OPEN(TRIM(DIR)//'/static/landmask.nc',NF_NOWRITE,NCID)
        STATUS=NF_INQ_VARID (NCID, 'landmask', landID)
        status=NF_ENDDEF(ncid)
        STATUS=NF_GET_VARA_INT(NCID,landID,START,COUNT,LANDSEA)
        STATUS=NF_CLOSE(NCID)
 
!        STATUS=NF_OPEN(TRIM(DIR)//'/static/soilcolor.nc',NF_NOWRITE,NCID)
!        STATUS=NF_INQ_VARID (NCID, 'SC', iscID)
!        status=NF_ENDDEF(ncid)
!        STATUS=NF_GET_VARA_INT(NCID,iscID,START,COUNT,isc)
!        STATUS=NF_CLOSE(NCID)
         ISC(:,:)     = -999     !
 
        STATUS=NF_OPEN(TRIM(DIR)//'/static/veg_soil.nc',NF_NOWRITE,NCID)
        STATUS=NF_INQ_VARID (NCID, 'VEG2D', vegeID)
!        STATUS=NF_INQ_VARID (NCID, 'TOPSOIL2D', soilID)
        STATUS=NF_INQ_VARID (NCID, 'BOTSOIL2D', soilID)
        status=NF_ENDDEF(ncid)
        STATUS=NF_GET_VARA_INT(NCID,vegeID,START,COUNT,VEGTYP)
        STATUS=NF_GET_VARA_INT(NCID,soilID,START,COUNT,SOLTYP)
        STATUS=NF_CLOSE(NCID)
 
        STATUS=NF_OPEN(TRIM(DIR)//'/static/tbot.nc',NF_NOWRITE,NCID)
        STATUS=NF_INQ_VARID (NCID, 'TBOT', tbotID)
        status=NF_ENDDEF(ncid)
        STATUS=NF_GET_VARA_real(NCID,tbotID,START,COUNT,tbot)
        STATUS=NF_CLOSE(NCID)

        OPEN(10,FILE='surface_datalog.dat')

        IF (VARSD == 2) THEN
          STATUS=NF_OPEN(TRIM(DIR)//'/static/soildepth.nc',NF_NOWRITE,NCID)
          STATUS=NF_INQ_VARID (NCID, 'SOILDEPTH', soildID)
          STATUS=NF_ENDDEF(NCID)
          STATUS=NF_GET_VARA_real(NCID,soildID,START,COUNT,zsoil_var)
          STATUS=NF_CLOSE(NCID)

          ZSOIL(1) = -DZS(1)
          DO K=1,NSOIL-1
            ZSOIL(K+1) = ZSOIL(K) - DZS(K+1)
          END DO

          DO K=1,NSOIL
            write(*,*) DZS(K),ZSOIL(K)
          END DO

          DO J=1,JX
          DO I=1,IX

          IF (ZSOIL_VAR(I,J)>0. .AND. ZSOIL_VAR(I,J) <= 1.00) THEN
            write(10,*) 'I,J,ZSOIL_VAR(I,J)',I,J,ZSOIL_VAR(I,J)
            ZSOIL_VAR(I,J) = MAX(1.00,ZSOIL_VAR(I,J))
          END IF

          ZSOIL_VAR(I,J) = MIN(-ZSOIL(NSOIL),ZSOIL_VAR(I,J))

          IF ((ZSOIL_VAR(I,J) <= 0.) .AND. (VEGTYP(I,J) > 0)) ZSOIL_VAR(I,J) = 2.
          IF (ZSOIL_VAR(I,J) < 0.) CYCLE

            DO K=1,NSOIL-1
              IF ((-ZSOIL_VAR(I,J) <= ZSOIL(K)) .AND. (-ZSOIL_VAR(I,J) >= ZSOIL(K+1))) THEN
                 ZMID = ZSOIL(K) + 0.5*(ZSOIL(K+1) - ZSOIL(1))
                 IF (-ZSOIL_VAR(I,J) > ZMID) THEN
                   NSOIL_VAR(I,J) = K
                 ELSE
                   NSOIL_VAR(I,J) = K + 1
                 ENDIF
              ENDIF
            ENDDO

          ENDDO
          ENDDO
        ELSE
          NSOIL_VAR = NSOIL
        ENDIF

        write(10,*)'IX, IY,     longitue   latitude   landmask  vegetype  soiltype    isc   tbot topostd  soil depth  nsoil_var' 
        DO J=1,JX
        DO I=1,IX
          if(soltyp(i,j) == 14) vegtyp(i,j) = 16
          SLOPE(I,J)   = 3     !only used in runoff option 3
          if(LANDSEA(I,J) == 1) ISC(I,J)     = 2     !1-lightest -> 8-darkest

          write(10,10)I,J,LON(I,J),LAT(I,J),LANDSEA(I,J),VEGTYP(I,J),SOLTYP(I,J),ISC(I,J),&
                      TBOT(I,J),topostdv(i,j),zsoil_var(i,j),NSOIL_VAR(I,J)
        ENDDO
        ENDDO

 10     FORMAT(2X,2I5,2F10.2,4I10,3F10.2,I6)
        CLOSE(10)
        write(6,*) '------------- successful reading surface data ------------------'

! ----------------------------------------------------------------
  END SUBROUTINE READLAND
! ----------------------------------------------------------------
  SUBROUTINE NOAHMP_INIT ( MMINLU, SNOW , SNOWH , CANWAT , &
       ISLTYP ,   IVGTYP, SLOPETYP, SOILCOLOR ,istxy,&
       TSLB , SMOIS , SH2O , SICE,  DZS , FNDSOILW , FNDSNOWH ,             &
       TSK, isnowxy , tvxy     ,tgxy     ,canicexy ,         TMN,     XICE,   &
       canliqxy ,eahxy    ,tahxy    ,cmxy     ,chxy     ,                     &
       fwetxy   ,sneqvoxy ,alboldxy ,qsnowxy  ,wslakexy ,zwtxy    ,waxy     , &
       wtxy     ,tsnoxy   ,zsnsoxy  ,snicexy  ,snliqxy  ,radiusxy ,lfmassxy ,rtmassxy , &
       stmassxy ,woodxy   ,stblcpxy ,fastcpxy ,xsaixy   ,lai      ,           &
       socxy    ,wdocxy   ,ddocxy   ,micxy    ,wenzxy   ,denzxy   , &
       rootmsxy ,frootxy  ,krxy     ,mqxy     ,zsoil    ,stcxy    ,  &
       PSIXY    ,ATM_BCXY ,ATMACTXY ,DTFINEMXY,iopt_watret, &  
       QINXY    ,WCNDXY   ,nsoil_var,HTOPXY, &
       grainxy  ,gddxy    ,                                                   &
       croptype ,cropcat  ,                      &
!jref:start
       t2mvxy   ,t2mbxy   ,chstarxy,            &
!jref:end       
       NSOIL,  fini,                 &
       allowed_to_read , iopt_run,                         &
       sf_urban_physics,                         &  ! urban scheme
       ids,ide, jds,jde, kds,kde,                &
       ims,ime, jms,jme, kms,kme,                &
       its,ite, jts,jte, kts,kte,                &
       smoiseq  ,smcwtdxy ,rechxy   ,deeprechxy, areaxy, dx, dy, msftx, msfty,&     ! Optional groundwater
       wtddt    ,stepwtd  ,dt       ,qrfsxy     ,qspringsxy  , qslatxy    ,  &      ! Optional groundwater
       fdepthxy ,ht     ,riverbedxy ,eqzwt     ,rivercondxy ,pexpxy       ,  &      ! Optional groundwater
       rechclim                                                             )    ! Optional groundwater

  USE NOAHMP_TABLES
  USE MODULE_SF_NOAHMPLSM
  USE module_Noahlsm_mixedRE, only: GET_SMC,GET_CND,GET_EQM_PSI

  IMPLICIT NONE

! Initializing Canopy air temperature to 287 K seems dangerous to me [KWM].

    type (noahmp_parameters) :: parameters

    character(len=256)  :: fini

    INTEGER, INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           its,ite, jts,jte, kts,kte

    INTEGER, INTENT(IN)       ::     NSOIL, iopt_run

!niu    LOGICAL, INTENT(IN)       ::     restart,                    &
    LOGICAL       ::     restart,                    &
         &                           allowed_to_read
    INTEGER, INTENT(IN)       ::     sf_urban_physics                              ! urban, by yizhou

    REAL,    DIMENSION( NSOIL), INTENT(IN)    ::     DZS  ! Thickness of the soil layers [m]
    REAL,    INTENT(IN) , OPTIONAL ::     DX, DY
    REAL,    DIMENSION( ims:ime, jms:jme ) ,  INTENT(IN) , OPTIONAL :: MSFTX,MSFTY

   !REAL,    DIMENSION( ims:ime, NSOIL, jms:jme ) :: SICE
    REAL,    DIMENSION( ims:ime, NSOIL, jms:jme ) ,    &
         &   INTENT(INOUT)    ::     SMOIS,                      &
         &                           SH2O,                       &
         &                           SICE,                       &
         &                           TSLB

    REAL,    DIMENSION( ims:ime, jms:jme ) ,                     &
         &   INTENT(INOUT)    ::     SNOW,                       &
         &                           SNOWH,                      &
         &                           CANWAT

    INTEGER, DIMENSION( ims:ime, jms:jme ),                      &
         &   INTENT(IN)       ::     ISLTYP,  &
         &                           SLOPETYP,  &
         &                           SOILCOLOR,  &
         &                           IVGTYP, &
         &                           istxy

    LOGICAL, INTENT(IN)       ::     FNDSOILW,                   &
         &                           FNDSNOWH

    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: TSK         !skin temperature (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: TMN         !deep soil temperature (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: XICE         !sea ice fraction
    INTEGER, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isnowxy     !actual no. of snow layers
    REAL, DIMENSION(ims:ime,-2:NSOIL,jms:jme), INTENT(INOUT) :: zsnsoxy  !snow layer depth [m]
    REAL, DIMENSION(ims:ime,-2:    0,jms:jme), INTENT(INOUT) :: tsnoxy   !snow temperature [K]
    REAL, DIMENSION(ims:ime,-2:    0,jms:jme), INTENT(INOUT) :: snicexy  !snow layer ice [mm]
    REAL, DIMENSION(ims:ime,-2:    0,jms:jme), INTENT(INOUT) :: snliqxy  !snow layer liquid water [mm]
    REAL, DIMENSION(ims:ime,-2:    0,jms:jme), INTENT(INOUT) :: radiusxy !snow grain radius [um]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tvxy        !vegetation canopy temperature
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tgxy        !ground surface temperature
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canicexy    !canopy-intercepted ice (mm)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canliqxy    !canopy-intercepted liquid water (mm)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: eahxy       !canopy air vapor pressure (pa)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tahxy       !canopy air temperature (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: cmxy        !momentum drag coefficient
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chxy        !sensible heat exchange coefficient
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fwetxy      !wetted or snowed fraction of the canopy (-)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: sneqvoxy    !snow mass at last time step(mm h2o)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: alboldxy    !snow albedo at last time step (-)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: qsnowxy     !snowfall on the ground [mm/s]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wslakexy    !lake water storage [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: zwtxy       !water table depth [m]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: waxy        !water in the "aquifer" [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wtxy        !groundwater storage [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: lfmassxy    !leaf mass [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: rtmassxy    !mass of fine roots [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stmassxy    !stem mass [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: woodxy      !mass of wood (incl. woody roots) [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: grainxy     !mass of grain [g/m2] !XING
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: gddxy       !growing degree days !XING
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stblcpxy    !stable carbon in deep soil [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fastcpxy    !short-lived carbon, shallow soil [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: xsaixy      !stem area index
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: lai         !leaf area index

!niu added beg
    INTEGER :: nroot
    real, dimension(ims:ime,jms:jme) :: soilcxy    !total soil carbon
    real, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: socxy       !soil organic carbon          [g C/m2]
    real, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: wdocxy      !wet dissolved organic carbon [g C/m2]
    real, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: ddocxy      !dry dissolved organic carbon [g C/m2]
    real, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: micxy       !soil microbial biomass       [g C/m2]
    real, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: wenzxy      !wet soilenzyme               [g C/m2]
    real, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: denzxy      !dry soil enzyme              [g C/m2]
    real, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: krxy        !total soil carbon            [g C/m2]
    real, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: mqxy        !liquid water in living plant tissues [kg/m2]

    
    real, DIMENSION(ims:ime, 1:NSOIL,jms:jme), INTENT(OUT) :: rootmsxy  !carbon mass of fine roots [g/m2]
    real, DIMENSION(ims:ime, 1:NSOIL,jms:jme), INTENT(OUT) :: frootxy   !fractional root surface area [-]
    REAL, DIMENSION(ims:ime,-2:NSOIL,jms:jme), INTENT(OUT) :: stcxy     !snow (3L)/soil temperature (4L) [K]
    REAL, DIMENSION(1:NSOIL), INTENT(OUT)  :: ZSOIL      ! Depth of the soil layer bottom (m) from 
    !                                                   the surface (negative)


    integer, INTENT(IN) :: iopt_watret
    integer, DIMENSION(ims:ime,jms:jme)        , INTENT(IN)  :: nsoil_var
    integer, DIMENSION(ims:ime,jms:jme)        , INTENT(OUT) :: ATM_BCXY     !upper BC (0-flux; 1-state)
    real   , DIMENSION(ims:ime,jms:jme)        , INTENT(OUT) :: ATMACTXY     !
    real   , DIMENSION(ims:ime,jms:jme)        , INTENT(OUT) :: HTOPXY     !
    real   , DIMENSION(ims:ime,jms:jme)        , INTENT(OUT) :: DTFINEMXY     !
    real   , DIMENSION(ims:ime,1:nsoil,jms:jme), INTENT(OUT) :: PSIXY     !
    real   , DIMENSION(ims:ime,jms:jme)        , INTENT(OUT) :: QINXY     !
    real   , DIMENSION(ims:ime,1:nsoil,jms:jme), INTENT(OUT) :: WCNDXY     !

    real   , DIMENSION(ims:ime,1:nsoil,jms:jme) :: SEXY     !

!niu added end

    INTEGER, DIMENSION(ims:ime,  jms:jme), INTENT(OUT) :: cropcat
    REAL   , DIMENSION(ims:ime,5,jms:jme), INTENT(IN ) :: croptype

! IOPT_RUN = 5 option

    REAL, DIMENSION(ims:ime,1:nsoil,jms:jme), INTENT(INOUT) , OPTIONAL :: smoiseq !equilibrium soil moisture content [m3m-3]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: smcwtdxy    !deep soil moisture content [m3m-3]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: deeprechxy  !deep recharge [m]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: rechxy      !accumulated recharge [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qrfsxy      !accumulated flux from groundwater to rivers [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qspringsxy  !accumulated seeping water [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: qslatxy     !accumulated lateral flow [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: areaxy      !grid cell area [m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: FDEPTHXY    !efolding depth for transmissivity (m)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: HT          !terrain height (m)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: RIVERBEDXY  !riverbed depth (m)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) , OPTIONAL :: EQZWT       !equilibrium water table depth (m)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT), OPTIONAL :: RIVERCONDXY !river conductance
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT), OPTIONAL :: PEXPXY      !factor for river conductance
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: rechclim

    INTEGER,  INTENT(OUT) , OPTIONAL :: STEPWTD
    REAL, INTENT(IN) , OPTIONAL :: DT, WTDDT

!jref:start
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mvxy        !2m temperature vegetation part (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mbxy        !2m temperature bare ground part (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chstarxy        !dummy
!jref:end

    real                                               :: dummy
    real             ,dimension(      1:nsoil)         :: znode

   !REAL                      :: BEXP, SMCMAX, PSISAT
   !REAL                      :: VGM,  SMCR  , VGPSAT
    REAL                      :: VGN

    REAL                      :: FK, masslai,masssai

    REAL, PARAMETER           :: BLIM  = 5.5
    REAL, PARAMETER           :: HLICE = 3.335E5
!   REAL, PARAMETER           :: GRAV = 9.81
    REAL, PARAMETER           :: T0 = 273.15

    INTEGER                   :: errflag, i,j,itf,jtf,ns
    INTEGER                   :: junk

    character(len=240) :: err_message
    character(len=4)  :: MMINSL
    character(len=*), intent(in) :: MMINLU
    MMINSL='STAS'

    write(*,*) 'fini=', TRIM(fini)
    if (fini == 'arbitrary initialization') then
         restart  = .false.
    else
         restart  = .true.
    end if

    call read_mp_veg_parameters(trim(MMINLU))
    call read_mp_soil_parameters()
    call read_mp_rad_parameters()
    call read_mp_global_parameters()
    call read_mp_crop_parameters()

    ! Given the soil layer thicknesses (in DZS), initialize the soil layer
    ! depths from the surface.

    ZSOIL(1)         = -DZS(1)          ! negative
    DO NS=2, NSOIL
       ZSOIL(NS)       = ZSOIL(NS-1) - DZS(NS)
    END DO

    itf=min0(ite,ide-1)
    jtf=min0(jte,jde-1)

    write(*,*) 'ZSOIL=',ZSOIL
    write(*,*) 'restart=',restart
    write(*,*) 'NROOT_TABLE(IVGTYP(I,J))=',NROOT_TABLE(IVGTYP(200,100))

    IF (restart) then
        open(100,file =fini,form='unformatted', status = 'old')
       !open(100,file =fini, status = 'old')
        DO I = its,itf
        DO J = jts,jtf
         if(IVGTYP(I,J) > 0) then

            CALL TRANSFER_MP_PARAMETERS(IVGTYP(I,J),ISLTYP(I,J),nsoil_var(i,j),  &
                 SLOPETYP(I,J),SOILCOLOR(I,J),CROPTYPE(I,:,J),parameters,i,j)

            cmxy       (I,J) = 0.0
            chxy       (I,J) = 0.0
            fwetxy     (I,J) = 0.0
            sneqvoxy   (I,J) = 0.0
            alboldxy   (I,J) = 0.65
            qsnowxy    (I,J) = 0.0
            xsaixy     (I,J) = 0.1     ! stem area index
            lai        (I,J) = 1.0     ! leaf area index
            eahxy      (I,J) = 2000.   ! water vapor pressure within the canopy air
            tahxy      (I,J) = 287.    ! temperature within the canopy air

            DTFINEMXY(I,J)         = 10.

            read(100) junk, junk, isnowxy(I,J)
            read(100)(SMOIS(I,NS,J),NS=1,nsoil_var(i,j)),&
                     (stcxy(I,NS,J),NS=isnowxy(I,J)+1,nsoil_var(i,j)),&
                     (SH2O (I,NS,J),NS=1,nsoil_var(i,j))
            read(100) wslakexy(I,J)  ! lake water storage
            if(isnowxy(I,J) .lt. 0) then
               read(100)(snicexy(I,NS,J) ,NS = isnowxy(I,J)+1,0),&
                        (snliqxy(I,NS,J) ,NS = isnowxy(I,J)+1,0),&
                        (radiusxy(I,NS,J),NS = isnowxy(I,J)+1,0)
            end if
            read(100)(zsnsoxy(I,NS,J),NS = isnowxy(I,J)+1,nsoil_var(i,j))
            read(100)tvxy(I,J),tgxy(I,J),canicexy(I,J),canliqxy(I,J),&
                          SNOWH(I,J),SNOW(I,J),waxy(I,J),wtxy(I,J),&
                          zwtxy(I,J)
            read(100)lfmassxy(I,J),rtmassxy(I,J),&
                     (rootmsxy(I,NS,J),NS=1,parameters%nroot),&
                          stmassxy(I,J), &
                          woodxy  (I,J), &
                          STBLCPxy(I,J), &
                          FASTCPxy(I,J), &
                          socxy   (I,J), &
                          wdocxy  (I,J), &
                          ddocxy  (I,J), &
                          micxy   (I,J), &
                          wenzxy  (I,J), &
                          denzxy  (I,J)
            read(100) mqxy(I,J),krxy(I,J),(frootxy(I,NS,J),NS=1,parameters%nroot)
            read(100) (psixy(I,NS,J),NS=1,nsoil_var(i,j))
            read(100) (wcndxy(I,NS,J),NS=1,nsoil_var(i,j))
            read(100) HTOPXY(I,J)
            read(100) atm_bcxy(I,J)

         end if
        end do
        end do

        close(100)

    END IF

    IF( .NOT. restart ) THEN

       !
       ! initialize physical snow height SNOWH
       !
       IF(.NOT.FNDSNOWH)THEN
          ! If no SNOWH do the following
          CALL wrf_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT' )
          DO J = jts,jtf
             DO I = its,itf
                SNOWH(I,J)=SNOW(I,J)*0.005               ! SNOW in mm and SNOWH in m
             ENDDO
          ENDDO
       ENDIF


       ! Check if snow/snowh are consistent and cap SWE at 2000mm;
       !  the Noah-MP code does it internally but if we don't do it here, problems ensue
       DO J = jts,jtf
          DO I = its,itf
             IF ( SNOW(i,j) > 0. .AND. SNOWH(i,j) == 0. .OR. SNOWH(i,j) > 0. .AND. SNOW(i,j) == 0.) THEN
               WRITE(err_message,*)"problem with initial snow fields: snow/snowh>0 while snowh/snow=0 at i,j" &
                                     ,i,j,snow(i,j),snowh(i,j)
               CALL wrf_message(err_message)
             ENDIF
             IF ( SNOW( i,j ) > 2000. ) THEN
               SNOWH(I,J) = SNOWH(I,J) * 2000. / SNOW(I,J)      ! SNOW in mm and SNOWH in m
               SNOW (I,J) = 2000.                               ! cap SNOW at 2000, maintain density
             ENDIF
          ENDDO
       ENDDO

       errflag = 0
       DO j = jts,jtf
          DO i = its,itf
             !niuIF ( ISLTYP( i,j ) .LT. 1 ) THEN
             IF ( IVGTYP(I,J) > 0 .and. ISLTYP( i,j ) .LT. 1 ) THEN
                errflag = 1
                WRITE(err_message,*)"module_sf_noahlsm.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
                CALL wrf_message(err_message)
             ENDIF
          ENDDO
       ENDDO
       IF ( errflag .EQ. 1 ) THEN
          CALL wrf_error_fatal( "module_sf_noahlsm.F: lsminit: out of range value "// &
               "of ISLTYP. Is this field in the input?" )
       ENDIF

! initialize soil liquid water content SH2O

       write(*,*) 'ISICE_TABLE=',ISICE_TABLE

       DO J = jts , jtf
       DO I = its , itf
          if(IVGTYP(I,J) > 0) THEN  ! niu
            IF(IVGTYP(I,J)==ISICE_TABLE .AND. XICE(I,J) <= 0.0) THEN
              DO NS=1, NSOIL
                  SMOIS(I,NS,J) = 0.3                     ! glacier starts all frozen
                  SH2O(I,NS,J) = 0.3
                  TSLB(I,NS,J) = MIN(TSLB(I,NS,J),263.15) ! set glacier temp to at most -10C
                  stcxy(I,NS,J)= TSLB(I,NS,J)
              END DO
                !TMN(I,J) = MIN(TMN(I,J),263.15)         ! set deep temp to at most -10C
                SNOW(I,J) = MAX(SNOW(I,J), 10.0)        ! set SWE to at least 10mm
                SNOWH(I,J)=SNOW(I,J)*0.01               ! SNOW in mm and SNOWH in m
            ELSE

             IF(I == 1 .and. J == 210) THEN
                  write(*,*) 'nsoil_var(I,J)=',nsoil_var(I,J)
             END IF

             CALL TRANSFER_MP_PARAMETERS(IVGTYP(I,J),ISLTYP(I,J),nsoil_var(i,j),  &
                  SLOPETYP(I,J),SOILCOLOR(I,J),CROPTYPE(I,:,J),parameters,i,j)

             IF(I == 1 .and. J == 210) THEN
              write(*,*) 'IVGTYP(I,J)=',IVGTYP(I,J)
              write(*,*) 'ISLTYP(I,J)=',ISLTYP(I,J)
              write(*,*) 'parameters%SMCMAX(NS)=',parameters%SMCMAX
              write(*,*) 'parameters%SMCR(NS)=',parameters%SMCR
              write(*,*) 'parameters%vgpsat(NS)=',parameters%vgpsat
              write(*,*) 'parameters%psisat(NS)=',parameters%psisat
              write(*,*) 'parameters%VGN(NS)=',parameters%VGN
             END IF

              DO NS=1, NSOIL
                IF ( SMOIS(I,NS,J) > parameters%SMCMAX(NS) ) THEN
                   SMOIS(I,NS,J) = parameters%SMCMAX(NS)
                   SICE(I,NS,J)=SMOIS(I,NS,J) - SH2O(I,NS,J)
                END IF
              END DO

              DO NS=1, NSOIL
                   stcxy(I,NS,J)=TSLB(I,NS,J)
                   SH2O(I,NS,J)=SMOIS(I,NS,J)
                   SICE(I,NS,J)=SMOIS(I,NS,J) - SH2O(I,NS,J)
              END DO
            END IF

          END IF !(IVGTY(I,J)>0)
       ENDDO
       ENDDO
                write(*,*) 'TSLB=',TSLB(200,:,100)
                write(*,*) 'SH2O=',SH2O(200,:,100)
                write(*,*) 'TSK=',TSK(200,100)

!  ENDIF


       DO J = jts,jtf
          DO I = its,itf
           if(IVGTYP(I,J) > 0) THEN  ! niu
          !if(IVGTYP(I,J) > 0 .and. istxy(I,J) == 1) then

             znode(1) = -zsoil(1) / 2.
             do ns=2,nsoil
               znode(ns)  = -zsoil(ns-1) + 0.5 * (zsoil(ns-1) - zsoil(ns))
             enddo

             tvxy       (I,J) = TSK(I,J)
               if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tvxy(I,J) = 273.15
             tgxy       (I,J) = TSK(I,J)
               if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tgxy(I,J) = 273.15
             CANWAT     (I,J) = 0.0
             canliqxy   (I,J) = CANWAT(I,J)
             canicexy   (I,J) = 0.
             eahxy      (I,J) = 2000.
             tahxy      (I,J) = TSK(I,J)
               if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) tahxy(I,J) = 273.15
!             tahxy      (I,J) = 287.
!jref:start
             t2mvxy     (I,J) = TSK(I,J)
               if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) t2mvxy(I,J) = 273.15
             t2mbxy     (I,J) = TSK(I,J)
               if(snow(i,j) > 0.0 .and. tsk(i,j) > 273.15) t2mbxy(I,J) = 273.15
             chstarxy     (I,J) = 0.1
!jref:end

             cmxy       (I,J) = 0.0
             chxy       (I,J) = 0.0
             fwetxy     (I,J) = 0.0
             sneqvoxy   (I,J) = 0.0
             alboldxy   (I,J) = 0.65
             qsnowxy    (I,J) = 0.0
             wslakexy   (I,J) = 0.0

          ! CALL TRANSFER_MP_PARAMETERS(IVGTYP(I,J),ISLTYP(I,J),  &
          !                SLOPETYP(I,J),SOILCOLOR(I,J),CROPTYPE(I,:,J),parameters,i,j)

            CALL TRANSFER_MP_PARAMETERS(IVGTYP(I,J),ISLTYP(I,J),nsoil_var(i,j),  &
                 SLOPETYP(I,J),SOILCOLOR(I,J),CROPTYPE(I,:,J),parameters,i,j)

            atm_bcxy(I,J)        = 0
            atmactxy(I,J)        = 0.
            HTOPXY(I,J)          = 0.
            DTFINEMXY(I,J)  = 10.

            if(iopt_run.ne.5) then
                   waxy       (I,J) = 4700.
                   wtxy       (I,J) = waxy(i,j)
                   zwtxy      (I,J) = (25. + 2.0) - waxy(i,j)/1000/0.2
             else
                   waxy       (I,J) = 0.
                   wtxy       (I,J) = 0.
                   areaxy     (I,J) = (DX * DY) / ( MSFTX(I,J) * MSFTY(I,J) )
             endif

             if (opt_watret == 1) then
                zwtxy(i,j)           = 3.0
             elseif (opt_watret == 2) then
               !zwtxy(i,j)           = -zsoil(nsoil_var(i,j))
                zwtxy(i,j)           = 10.0
             end if

             IF(I == 1 .and. J == 210) THEN
              write(*,*) 'IVGTYP(I,J)=',IVGTYP(I,J)
              write(*,*) 'ISLTYP(I,J)=',ISLTYP(I,J)
              write(*,*) 'znode(:)=',znode(:)
              write(*,*) 'zwtxy(I,J)=',zwtxy(I,J)
              write(*,*) 'parameters%SMCMAX(NS)=',parameters%SMCMAX(1)
              write(*,*) 'parameters%SMCR(NS)=',parameters%SMCR(1)
              write(*,*) 'parameters%vgpsat(NS)=',parameters%vgpsat(1)
              write(*,*) 'parameters%psisat(NS)=',parameters%psisat(1)
              write(*,*) 'parameters%VGN(NS)=',parameters%VGN(1)
             END IF

            do NS = 1,nsoil_var(i,j)
               SICE(i,NS,j) = 0.

               call get_eqm_psi(opt_watret,zwtxy(i,j),znode(NS), &
                                parameters%PSISAT(NS),parameters%vgpsat(NS),psixy(i,NS,j))
               call get_smc(opt_watret,psixy(i,NS,j),SICE(i,NS,j), &
                            parameters%SMCMAX(NS),parameters%BEXP(NS),parameters%PSISAT(NS), &
                            parameters%smcr(NS)  ,parameters%VGN(NS) ,parameters%vgpsat(NS), &
                            sexy(i,NS,j),SH2O(i,NS,j))

               SMOIS(i,NS,j) = SH2O(i,NS,j) + SICE(i,NS,j)
             enddo !nz

             IF(I == 1 .and. J == 210) THEN
              write(*,*) '=====PSI=' ,PSIxy(I,:,J)
              write(*,*) '=====SH2O=',SH2O(I,:,J)
              write(*,*) '=====SMOIS=',SMOIS(I,:,J)
              write(*,*) '=====SICE=',SICE(I,:,J)
              write(*,*) '=====STC=',STCxy(I,:,J)
             END IF

            !do NS = 1,nsoil
             do NS = 1,nsoil_var(i,j)

               SICE(I,NS,J) = max(0.,(SMOIS(I,NS,J) - SH2O(I,NS,J)))

             ! sexy(I,NS,J)   = (SH2O(I,NS,J) - parameters%smcr(NS)) / &
             !                  (parameters%smcmax(NS)  - SICE(I,NS,J) - parameters%smcr(NS))

               if (opt_run == 6) then !initial hydrulic conductivity
                call  get_cnd (opt_watret, psixy(I,NS,J) ,SH2O(I,NS,J) ,SICE(I,NS,J), 0., &
                      parameters%bexp(NS),parameters%dksat(NS),parameters%smcmax(NS),parameters%psisat(NS), &
                      parameters%smcr(NS),parameters%vgn(NS),parameters%vgpsat(NS),dummy ,wcndxy(I,NS,J))
               endif
             end do !iz

             IF(I == 1 .and. J == 210) THEN
              write(*,*) '=====IVGTYP(I,J)=',IVGTYP(I,J)
              write(*,*) '=====wcndxy(I,NS,J)=' ,wcndxy(I,:,J)
             END IF

           IF(IVGTYP(I,J) == ISBARREN_TABLE .OR. IVGTYP(I,J) == ISICE_TABLE .OR. &
!niu             ( SF_URBAN_PHYSICS == 0 .AND. IVGTYP(I,J) == ISURBAN_TABLE )  .OR. &
              IVGTYP(I,J) == ISWATER_TABLE ) THEN
        
!niu         if(IVGTYP(I,J) == 16 .or. IVGTYP(I,J) == 19 .or. IVGTYP(I,J) == 24)
             lai        (I,J) = 0.0
             xsaixy     (I,J) = 0.0
             lfmassxy   (I,J) = 0.0
             stmassxy   (I,J) = 0.0
             rtmassxy   (I,J) = 0.0
             woodxy     (I,J) = 0.0
             stblcpxy   (I,J) = 0.0
             fastcpxy   (I,J) = 0.0
             grainxy    (I,J) = 1E-10
             gddxy      (I,J) = 0
             cropcat    (I,J) = 0

             CALL TRANSFER_MP_PARAMETERS(IVGTYP(I,J),ISLTYP(I,J),nsoil_var(i,j),  &
                  SLOPETYP(I,J),SOILCOLOR(I,J),CROPTYPE(I,:,J),parameters,i,j)

             nroot = parameters%nroot

             do NS = 1, nroot
               rootmsxy(I,NS,J)= 0.      !g/m3 * dz
               frootxy (I,NS,J)= 0.
             end do

             rtmassxy(I,J) = 0.0
             do NS = 1, nroot
               rtmassxy(I,J) = rtmassxy(I,J) + rootmsxy(I,NS,J)
             enddo

             woodxy(I,J)       = 0.
             soilcxy(I,J)      = 0.
             socxy(I,J)        = 0.
             wdocxy(I,J)       = 0.
             ddocxy(I,J)       = 0.
             micxy(I,J)        = 0.
             wenzxy(I,J)       = 0.
             denzxy(I,J)       = 0.

             mqxy(I,J)         = 0.0
             krxy(I,J)         = 0.8
             mqxy(I,J)         = 0.0    !kg/m2

           ELSE
        
             lai        (I,J) = max(lai(i,j),0.05)             ! at least start with 0.05 for arbitrary initialization (v3.7)
             xsaixy     (I,J) = max(0.1*lai(I,J),0.05)         ! MB: arbitrarily initialize SAI using input LAI (v3.7)
             masslai = 1000. / max(SLA_TABLE(IVGTYP(I,J)),1.0) ! conversion from lai to mass  (v3.7)
             lfmassxy   (I,J) = lai(i,j)*masslai               ! use LAI to initialize (v3.7)
             masssai = 1000. / 3.0                             ! conversion from lai to mass (v3.7)
             stmassxy   (I,J) = xsaixy(i,j)*masssai            ! use SAI to initialize (v3.7)
             woodxy     (I,J) = 500.0                          ! in the table or read from initialization
             stblcpxy   (I,J) = 1000.0                         !
             fastcpxy   (I,J) = 12000.0                         !

!niu added beg
             CALL TRANSFER_MP_PARAMETERS(IVGTYP(I,J),ISLTYP(I,J),nsoil_var(i,j),  &
                  SLOPETYP(I,J),SOILCOLOR(I,J),CROPTYPE(I,:,J),parameters,i,j)

             nroot = parameters%nroot

             do NS = 1, nroot
               rootmsxy(I,NS,J)= 100.0*DZS(NS)      !g/m3 * dz

               if(IVGTYP(I,J) .eq. 8  .or. IVGTYP(I,J) .eq.  9 .or. &
                  IVGTYP(I,J) .eq. 13 .or. IVGTYP(I,J) .eq. 18 .or. &
                  IVGTYP(I,J) .eq. 21 ) then
                  rootmsxy(I,NS,J)= 50.0*DZS(NS)      !g/m3 * dz
               end if

               if(IVGTYP(I,J) .eq. 2 .or. IVGTYP(I,J) .eq. 3 .or. &
                  IVGTYP(I,J) .eq. 4 .or. IVGTYP(I,J) .eq. 5) then
                  rootmsxy(I,NS,J)= 150.0*DZS(NS)      !g/m3 * dz 4-L roots
               end if

               if(IVGTYP(I,J) .eq. 6 .or. IVGTYP(I,J) .eq. 7) then
                  rootmsxy(I,NS,J)= 200.0*DZS(NS)      !g/m3 * dz 4-L roots
               end if

               if(IVGTYP(I,J) .eq. 22) then
                  rootmsxy(I,NS,J)= 50.0*DZS(NS)      !g/m3 * dz
               end if

              !frootxy(I,NS,J)   = DZS(NS)/(-ZSOIL(NSOIL))
               frootxy(I,NS,J)   = DZS(NS)/(-ZSOIL(nsoil_var(i,j)))
             end do

             rtmassxy(I,J) = 0.0
             do NS = 1, nroot
               rtmassxy(I,J) = rtmassxy(I,J) + rootmsxy(I,NS,J)
             enddo

             IF(I == 1 .and. J == 210) THEN
               write(*,*) 'nroot=',nroot
               write(*,*) 'rootmsxy=',rootmsxy(i,:,j)
               write(*,*) 'frootxy=',frootxy(i,:,j)
             END IF

             woodxy(I,J)       = 10000.0 * wdpool_table(IVGTYP(I,J))
             soilcxy(I,J)      = 12000.           !g/m2
             socxy(I,J)        = soilcxy(I,J)*0.960
             wdocxy(I,J)       = soilcxy(I,J)*0.000
             ddocxy(I,J)       = soilcxy(I,J)*0.0
             micxy(I,J)        = soilcxy(I,J)*0.020
             wenzxy(I,J)       = soilcxy(I,J)*0.000
             denzxy(I,J)       = soilcxy(I,J)*0.000

             mqxy(I,J)         = 5.0    !kg/m2
             krxy(I,J)         = 0.9    !btran
             if(IVGTYP(I,J) == 16 .or. IVGTYP(I,J) == 19 .or. IVGTYP(I,J) == 24) then
                mqxy(I,J)         = 0.0    !kg/m2
             end if

!niu added end

             grainxy    (I,J) = 1E-10
             gddxy      (I,J) = 0
             cropcat    (i,j) = default_crop_table

             if(croptype(i,5,j) >= 0.5) then
               rtmassxy(i,j) = 0.0
               woodxy  (i,j) = 0.0

               if(    croptype(i,1,j) > croptype(i,2,j) .and. &
                      croptype(i,1,j) > croptype(i,3,j) .and. &
                      croptype(i,1,j) > croptype(i,4,j) ) then   ! choose corn

                   cropcat (i,j) = 1
                   lfmassxy(i,j) =    lai(i,j)/0.035
                   stmassxy(i,j) = xsaixy(i,j)/0.003

               elseif(croptype(i,2,j) > croptype(i,1,j) .and. &
                      croptype(i,2,j) > croptype(i,3,j) .and. &
                      croptype(i,2,j) > croptype(i,4,j) ) then   ! choose soybean

                   cropcat (i,j) = 2
                   lfmassxy(i,j) =    lai(i,j)/0.015
                   stmassxy(i,j) = xsaixy(i,j)/0.003

               else

                   cropcat (i,j) = default_crop_table
                   lfmassxy(i,j) =    lai(i,j)/0.035
                   stmassxy(i,j) = xsaixy(i,j)/0.003

               end if

             end if

           END IF

           END IF  !niu

          enddo
       enddo

       ! Initialize snow/soil layer arrays ZSNSOXY, TSNOXY, SNICEXY, SNLIQXY, 
       ! and ISNOWXY, radiusXY
       CALL snow_init ( ims , ime , jms , jme , its , itf , jts , jtf , 3 , &
            &           NSOIL , zsoil , snow , tgxy , snowh ,     &
            &           zsnsoxy , tsnoxy , snicexy , snliqxy , isnowxy, radiusxy )

       DO J = jts,jtf
       DO I = its,itf
       if(IVGTYP(I,J) > 0) then
          do NS = isnowxy(I,J)+1,0
           stcxy(I,NS,J) =  tsnoxy(I,NS,J)
          end do
       end if
       end do
       end do

       !initialize arrays for groundwater dynamics iopt_run=5

       if(iopt_run.eq.5) then
          IF ( PRESENT(smoiseq)     .AND. &
            PRESENT(smcwtdxy)    .AND. &
            PRESENT(rechxy)      .AND. &
            PRESENT(deeprechxy)  .AND. &
            PRESENT(areaxy)      .AND. &
            PRESENT(dx)          .AND. &
            PRESENT(dy)          .AND. &
            PRESENT(msftx)       .AND. &
            PRESENT(msfty)       .AND. &
            PRESENT(wtddt)       .AND. &
            PRESENT(stepwtd)     .AND. &
            PRESENT(dt)          .AND. &
            PRESENT(qrfsxy)      .AND. &
            PRESENT(qspringsxy)  .AND. &
            PRESENT(qslatxy)     .AND. &
            PRESENT(fdepthxy)    .AND. &
            PRESENT(ht)          .AND. &
            PRESENT(riverbedxy)  .AND. &
            PRESENT(eqzwt)       .AND. &
            PRESENT(rivercondxy) .AND. &
            PRESENT(pexpxy)      .AND. &
            PRESENT(rechclim)    ) THEN

             STEPWTD = nint(WTDDT*60./DT)
             STEPWTD = max(STEPWTD,1)

              CALL groundwater_init ( &
      &       nsoil, zsoil , dzs  ,isltyp, ivgtyp,wtddt , &
      &       fdepthxy, ht, riverbedxy, eqzwt, rivercondxy, pexpxy , areaxy, zwtxy,   &
      &       smois,sh2o, smoiseq, smcwtdxy, deeprechxy, rechxy, qslatxy, qrfsxy, qspringsxy, &
      &       rechclim  ,                                   &
      &       ids,ide, jds,jde, kds,kde,                    &
      &       ims,ime, jms,jme, kms,kme,                    &
      &       its,ite, jts,jte, kts,kte                     )

          ELSE
             CALL wrf_error_fatal ('Not enough fields to use groundwater option in Noah-MP')
          END IF
       endif

    ENDIF
  END SUBROUTINE NOAHMP_INIT

!------------------------------------------------------------------------------------------
! ==================================================================================================
!------------------------------------------------------------------------------------------

  SUBROUTINE SNOW_INIT ( ims , ime , jms , jme , its , itf , jts , jtf ,                  &
       &                 NSNOW , NSOIL , ZSOIL , SWE , TGXY , SNODEP ,                    &
       &                 ZSNSOXY , TSNOXY , SNICEXY ,SNLIQXY , ISNOWXY, radiusXY )
!------------------------------------------------------------------------------------------
!   Initialize snow arrays for Noah-MP LSM, based in input SNOWDEP, NSNOW
!   ISNOWXY is an index array, indicating the index of the top snow layer.  Valid indices
!           for snow layers range from 0 (no snow) and -1 (shallow snow) to (-NSNOW)+1 (deep snow).
!   TSNOXY holds the temperature of the snow layer.  Snow layers are initialized with 
!          temperature = ground temperature [?].  Snow-free levels in the array have value 0.0
!   SNICEXY is the frozen content of a snow layer.  Initial estimate based on SNODEP and SWE
!   SNLIQXY is the liquid content of a snow layer.  Initialized to 0.0
!   ZNSNOXY is the layer depth from the surface.  
!------------------------------------------------------------------------------------------
    IMPLICIT NONE
!------------------------------------------------------------------------------------------
    INTEGER, INTENT(IN)                              :: ims, ime, jms, jme
    INTEGER, INTENT(IN)                              :: its, itf, jts, jtf
    INTEGER, INTENT(IN)                              :: NSNOW
    INTEGER, INTENT(IN)                              :: NSOIL
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SWE
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SNODEP
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: TGXY
    REAL,    INTENT(IN), DIMENSION(1:NSOIL)          :: ZSOIL

    INTEGER, INTENT(OUT), DIMENSION(ims:ime, jms:jme)                :: ISNOWXY ! Top snow layer index
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:NSOIL,jms:jme) :: ZSNSOXY ! Snow/soil layer depth from surface [m]
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: TSNOXY  ! Snow layer temperature [K]
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNICEXY ! Snow layer ice content [mm]
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNLIQXY ! snow layer liquid content [mm]
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: radiusXY! snow layer liquid content [mm]

! Local variables:
!   DZSNO   holds the thicknesses of the various snow layers.
!   DZSNOSO holds the thicknesses of the various soil/snow layers.
    INTEGER                           :: I,J,IZ
    REAL,   DIMENSION(-NSNOW+1:    0) :: DZSNO
    REAL,   DIMENSION(-NSNOW+1:NSOIL) :: DZSNSO

!------------------------------------------------------------------------------------------

    DO J = jts , jtf
       DO I = its , itf
          IF ( SNODEP(I,J) < 0.025 ) THEN
             ISNOWXY(I,J) = 0
             radiusXY(I,:,J) = 0.
             DZSNO(-NSNOW+1:0) = 0.
          ELSE
             IF ( ( SNODEP(I,J) >= 0.025 ) .AND. ( SNODEP(I,J) <= 0.05 ) ) THEN
                ISNOWXY(I,J)    = -1
                radiusXY(I,0,J) = 300.
                DZSNO(0)  = SNODEP(I,J)
             ELSE IF ( ( SNODEP(I,J) > 0.05 ) .AND. ( SNODEP(I,J) <= 0.10 ) ) THEN
                ISNOWXY(I,J)    = -2
                radiusXY(I,-1:0,J) = 300.
                DZSNO(-1) = SNODEP(I,J)/2.
                DZSNO( 0) = SNODEP(I,J)/2.
             ELSE IF ( (SNODEP(I,J) > 0.10 ) .AND. ( SNODEP(I,J) <= 0.25 ) ) THEN
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = 0.05
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1)
                radiusXY(I,-1:0,J) = 300.
             ELSE IF ( ( SNODEP(I,J) > 0.25 ) .AND. ( SNODEP(I,J) <= 0.45 ) ) THEN
                ISNOWXY(I,J)    = -3
                radiusXY(I,-2:0,J) = 300.
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.5*(SNODEP(I,J)-DZSNO(-2))
                DZSNO( 0) = 0.5*(SNODEP(I,J)-DZSNO(-2))
             ELSE IF ( SNODEP(I,J) > 0.45 ) THEN
                ISNOWXY(I,J)     = -3
                radiusXY(I,-2:0,J) = 300.
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.20
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1) - DZSNO(-2)
             ELSE
                CALL wrf_error_fatal("Problem with the logic assigning snow layers.")
             END IF
          END IF

          TSNOXY (I,-NSNOW+1:0,J) = 0.
          SNICEXY(I,-NSNOW+1:0,J) = 0.
          SNLIQXY(I,-NSNOW+1:0,J) = 0.
          DO IZ = ISNOWXY(I,J)+1 , 0
             TSNOXY(I,IZ,J)  = TGXY(I,J)  ! [k]
             SNLIQXY(I,IZ,J) = 0.00
             SNICEXY(I,IZ,J) = 1.00 * DZSNO(IZ) * (SWE(I,J)/SNODEP(I,J))  ! [kg/m3]
          END DO

          ! Assign local variable DZSNSO, the soil/snow layer thicknesses, for snow layers
          DO IZ = ISNOWXY(I,J)+1 , 0
             DZSNSO(IZ) = -DZSNO(IZ)
          END DO

          ! Assign local variable DZSNSO, the soil/snow layer thicknesses, for soil layers
          DZSNSO(1) = ZSOIL(1)
          DO IZ = 2 , NSOIL
             DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
          END DO

          ! Assign ZSNSOXY, the layer depths, for soil and snow layers
          ZSNSOXY(I,ISNOWXY(I,J)+1,J) = DZSNSO(ISNOWXY(I,J)+1)
          DO IZ = ISNOWXY(I,J)+2 , NSOIL
             ZSNSOXY(I,IZ,J) = ZSNSOXY(I,IZ-1,J) + DZSNSO(IZ)
          ENDDO

       END DO
    END DO

  END SUBROUTINE SNOW_INIT
! ==================================================================================================
  SUBROUTINE READFORC(IX,JX,DT,IYEAR,IMON,IDAY,IHOUR,DIR,TMP2M,QAIR,DLWRF,U,V,   &
          PRES,DSWRF,PRCP)

        IMPLICIT NONE

        CHARACTER(len=256),INTENT(IN) :: DIR

        INTEGER, INTENT(IN) :: IX,JX
        INTEGER, INTENT(IN) :: iyear,imon,iday,ihour
        REAL, INTENT(IN) :: DT
        REAL, INTENT(OUT), DIMENSION(IX,JX) :: DLWRF
        REAL, INTENT(OUT), DIMENSION(IX,JX) :: DSWRF
        REAL, INTENT(OUT), DIMENSION(IX,JX) :: PRCP
        REAL, INTENT(OUT), DIMENSION(IX,JX) :: TMP2M
        REAL, INTENT(OUT), DIMENSION(IX,JX) :: U
        REAL, INTENT(OUT), DIMENSION(IX,JX) :: V
        REAL, INTENT(OUT), DIMENSION(IX,JX) :: QAIR
        REAL, INTENT(OUT), DIMENSION(IX,JX) :: PRES
        REAL, allocatable, dimension(:,:) :: imergprec
        
        INTEGER :: I,J,ivar
        INTEGER, PARAMETER :: nvar = 8
        REAL, allocatable, dimension(:,:) :: var
        REAL, allocatable, dimension(:,:,:) :: varin
        real :: ea(IX,JX), eair(IX,JX)

        INCLUDE 'netcdf.inc'

        CHARACTER(len=256) :: ncfile
        CHARACTER(len=12) ::varname(nvar)
        CHARACTER(len=16) ::imergname
        INTEGER STATUS
        INTEGER NCIDin
        INTEGER varID
        INTEGER START(2), COUNT(2)
        DATA START / 1,  1/    !starting point

        data (varname(ivar), ivar=1,nvar) &
                      /'TAIR','QAIR','UAIR','VAIR', &
                       'PAIR','PRCP','SWDOWN','LWDOWN'  /

        data imergname /'precipitationCal' /

        allocate( var   (1:ix,1:jx) )
        allocate( varin (1:ix,1:jx,1:nvar) )
        allocate( imergprec   (1:ix,1:jx) )

        COUNT(1) = IX
        COUNT(2) = JX

        !$OMP PARALLEL DO
        do J=1,JX
        do I=1,IX
             var(i,j)      = -999.
             DSWRF(I,J)    = -999.
             DLWRF(I,J)    = -999.
             PRCP(I,J)     = -999.
             U(I,J)        = -999.
             V(I,J)        = -999.
             TMP2M(I,J)    = -999.
             QAIR(I,J)     = -999.
             PRES(I,J)     = -999.
        end do
        end do
        !$OMP END PARALLEL DO

       !write(*,*) '-------------------------------------------'
       !write(*,*) iyear,imon,iday,ihour
        write(ncfile,100) iyear,imon,iday,ihour
         if(imon < 10) then
           if(iday < 10) then
              if(ihour < 10) then
                  write(ncfile,100) iyear,iyear,imon,iday,ihour
  100             format('/',i4,'/',i4,'0',i1,'0',i1,'0',i1,'.nc')
              else
                  write(ncfile,200) iyear,iyear,imon,iday,ihour
  200             format('/',i4,'/',i4,'0',i1,'0',i1,i2,'.nc')
              end if
           else
              if(ihour < 10) then
                  write(ncfile,300) iyear,iyear,imon,iday,ihour
  300             format('/',i4,'/',i4,'0',i1,i2,'0',i1,'.nc')
              else
                  write(ncfile,400) iyear,iyear,imon,iday,ihour
  400             format('/',i4,'/',i4,'0',i1,i2,i2,'.nc')
              end if
           end if
         else
           if(iday < 10) then
              if(ihour < 10) then
                  write(ncfile,500) iyear,iyear,imon,iday,ihour
  500             format('/',i4,'/',i4,i2,'0',i1,'0',i1,'.nc')
              else
                  write(ncfile,600) iyear,iyear,imon,iday,ihour
  600             format('/',i4,'/',i4,i2,'0',i1,i2,'.nc')
              end if
           else
              if(ihour < 10) then
                  write(ncfile,700) iyear,iyear,imon,iday,ihour
  700             format('/',i4,'/',i4,i2,i2,'0',i1,'.nc')
              else
                  write(ncfile,800) iyear,iyear,imon,iday,ihour
  800             format('/',i4,'/',i4,i2,i2,i2,'.nc')
              end if
           end if
        end if

        !ncfile = TRIM(DIR)//ncfile
        ncfile = "/xdisk/niug/niug/NLDAS/NC"//ncfile
!        write(*,*) 'READFORC: opening ',trim(ncfile)
        STATUS=NF_OPEN(ncfile,NF_NOWRITE,NCIDin)
!        write(*,*) '-------------------------------------------'
        
        do ivar = 1,  nvar
           !write(*,*) varname(ivar)
           STATUS=NF_INQ_VARID (NCIDin, varname(ivar), varID)
           STATUS=NF_GET_VARA_real(NCIDin,varID,START,COUNT,var)
           !write(*,10) ((var(i,j),i=IX/2,IX/2+4),j=JX/2,JX/2+4)
           !write(*,10) ((var(i,j),i=317,321),j=4,4)
           !$OMP PARALLEL DO
           do I=1,IX
           do J=1,JX
             varin(i,j,ivar) = var(i,j)
           end do
           end do
           !$OMP END PARALLEL DO
        end do
        STATUS=NF_CLOSE(NCIDin)

        !!$OMP PARALLEL DO
        do I=1,IX
        do J=1,JX
              TMP2M(I,J)    = varin(i,j,1)
              QAIR(I,J)     = varin(i,j,2)
              U(I,J)        = sqrt(varin(i,j,3)**2 + varin(i,j,4)**2)
              V(I,J)        = 0.
              PRES(I,J)     = varin(i,j,5)
              PRCP(I,J)     = varin(i,j,6) / DT         ! NLDAS precipitation
              DSWRF(I,J)    = varin(i,j,7)
              DLWRF(I,J)    = varin(i,j,8)

              !IF(DLWRF(I,J) >= 500. ) THEN
              !   EAIR(i,j)   = QAIR(I,J)*PRES(I,J) / (0.622+0.378*QAIR(I,J))
              !   ea(i,j)     = 0.70 + 5.95e-05 *
              !   0.01*EAIR(i,j)*exp(1500.0/TMP2M(I,J))
              !   DLWRF(I,J)  = ea(i,j) * 5.67e-08 *TMP2M(I,J)**4
              !end if
        end do
        end do
        !!$OMP END PARALLEL DO
            
      do J=1,JX
        do I=1,IX
             imergprec(i,j)      = -999.
         end do
      end do
        
        write(*,*) '-------------------------------------------'
       !write(*,*) iyear,imon,iday,ihour
        write(ncfile,100) iyear,imon,iday,ihour
         if(imon < 10) then
           if(iday < 10) then
              if(ihour < 10) then
                  write(ncfile,1100) iyear,iyear,imon,iday,ihour
 1100           format('/imerg_',i4,'_regridded_3/',i4,'0',i1,'0',i1,'0',i1,'.nc')
              else
                  write(ncfile,1200) iyear,iyear,imon,iday,ihour
 1200             format('/imerg_',i4,'_regridded_3/',i4,'0',i1,'0',i1,i2,'.nc')
              end if
           else
              if(ihour < 10) then
                  write(ncfile,1300) iyear,iyear,imon,iday,ihour
 1300             format('/imerg_',i4,'_regridded_3/',i4,'0',i1,i2,'0',i1,'.nc')
              else
                  write(ncfile,1400) iyear,iyear,imon,iday,ihour
 1400             format('/imerg_',i4,'_regridded_3/',i4,'0',i1,i2,i2,'.nc')
              end if
           end if
         else
           if(iday < 10) then
              if(ihour < 10) then
                  write(ncfile,1500) iyear,iyear,imon,iday,ihour
 1500             format('/imerg_',i4,'_regridded_3/',i4,i2,'0',i1,'0',i1,'.nc')
              else
                  write(ncfile,1600) iyear,iyear,imon,iday,ihour
 1600             format('/imerg_',i4,'_regridded_3/',i4,i2,'0',i1,i2,'.nc')
              end if
           else
              if(ihour < 10) then
                  write(ncfile,1700) iyear,iyear,imon,iday,ihour
 1700             format('/imerg_',i4,'_regridded_3/',i4,i2,i2,'0',i1,'.nc')
              else
                  write(ncfile,1800) iyear,iyear,imon,iday,ihour
 1800             format('/imerg_',i4,'_regridded_3/',i4,i2,i2,i2,'.nc')
              end if
           end if
        end if

        print*, 'reading imerg data'
        ncfile = "/xdisk/niug/niug/jetal/imerg"//ncfile
        STATUS=NF_OPEN(ncfile,NF_NOWRITE,NCIDin)
            STATUS=NF_INQ_VARID (NCIDin, imergname, varID)
            STATUS=NF_GET_VARA_real(NCIDin,varID,START,COUNT,imergprec)
            STATUS=NF_CLOSE(NCIDin)
!
         do I=1,IX
         do J=1,JX
          PRCP(I,J)     = imergprec(i,j) / DT
         end do
        end do

  10 format(1x,5f15.5)

      return
! -----------------------------------------------------
  END SUBROUTINE READFORC
! -----------------------------------------------------
! ==================================================================================================
  SUBROUTINE READVEG(DIR,IX,JX,LANDSEA,SHDFAC,SHDFACMX)

        CHARACTER(len=256)  :: DIR
        INTEGER, INTENT(IN) :: IX,JX
        INTEGER, INTENT(IN), DIMENSION(IX,JX) :: LANDSEA
        REAL, INTENT(OUT), DIMENSION(IX,JX,12) :: SHDFAC
        REAL, INTENT(OUT), DIMENSION(IX,JX)    :: SHDFACMX
        INTEGER :: I,J,IM

        INCLUDE 'netcdf.inc'

        INTEGER STATUS,NCID
        INTEGER START(3), COUNT(3)
        DATA START /1,  1,  1/       
        INTEGER gvfID

        COUNT(1) = IX
        COUNT(2) = JX
        COUNT(3) = 12

        STATUS=NF_OPEN(TRIM(DIR)//'/static/gvf.nc',NF_NOWRITE,NCID)
        STATUS=NF_INQ_VARID (NCID, 'GVF', gvfID)
        status=NF_ENDDEF(ncid)
        STATUS=NF_GET_VARA_real(NCID,gvfID,START,COUNT,SHDFAC)
        STATUS=NF_CLOSE(NCID)

        open(30,file='shdfac_chk.dat',status='unknown')

        do i = 1,ix
        do j = 1,jx
          write(30,'(3I5,12F8.2)')i,j,landsea(i,j),(shdfac(i,j,im),im=1,12)
          
          if(landsea(i,j).eq.1) then
             shdfacmx(i,j) = shdfac(i,j,1)
             do im=1,12

               if(shdfac(i,j,im).lt.0.) then
                 write(*,*) 'veg. fraction on land points < 0)=',i,j,shdfac(i,j,im)
                 stop
               end if

               IF(shdfacmx(I,J) < shdfac(I,J,im)) THEN
                 shdfacmx(I,J) = shdfac(I,J,im)
               END IF

             end do
          end if
        end do
        end do
! -------------------------------------------------------
  END SUBROUTINE READVEG
! -------------------------------------------------------

END MODULE module_sf_Noahlsm_gridded_input
