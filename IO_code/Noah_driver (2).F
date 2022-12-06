      program Noah_driver

      use module_Noahlsm_utility
      use module_date_utilities
      use module_sf_Noahlsm_gridded_input
      use module_sf_Noah_NC_output
      use module_sf_noahmplsm
      USE NOAHMP_TABLES

      implicit none

      type(noahmp_parameters) :: parameters
      type(constants) :: rad_cons

      character(len=6)                     :: mdt         !model start month, day, and hour
      character(len=2)                     :: minute      !model start minute
      character(len=4)                     :: year        !model start year

      integer                              :: imonth      !only used to compute cosz
      integer                              :: iday        !only used to compute cosz
      integer                              :: itime       !only used to compute cosz

      integer                              :: nst         !number of time step during a day
      integer                              :: istep       !counting records in forcing data
      integer                              :: imstep      !for monthly output
      integer                              :: ND          !for monthly output
      integer                              :: ntimed      !for daily output
      integer                              :: idstep      !for 3-hourly output
      integer                              :: startstep   !starting time step to read forcings
      integer                              :: iy      !year loop index
      integer                              :: im      !month loop index
      integer                              :: id      !day loop index
      integer                              :: ih      !hour loop index
      integer                              :: startday    !starting day
      integer                              :: startmonth  !strating month
      integer                              :: ierr        !error message index
      integer, dimension(0:11)             :: jday        !julian day
      integer, dimension(0:11)             :: gday        !gorgian day
      integer, dimension(1:12)             :: nday        !days per month
      integer                              :: YEARLEN     !year length (day)
      real                                 :: JULIAN      !Julian day

      data (nday(imonth),imonth=1,12) /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      data (jday(imonth),imonth=0,11) / 0, 31, 59, 90,120,151,181,212,242,273,304,334/
      data (gday(imonth),imonth=0,11) / 0, 31, 60, 91,121,152,182,213,243,274,305,335/

      integer                              :: i          !do loop index in x-direction
      integer                              :: j          !do loop index in y-direction
      integer                              :: iz          !do loop index in z-direction
      integer                              :: it          !do loop index for time
      integer                              :: nsoil       !number of soil layers
      integer                              :: nx          !total x-direction points
      integer                              :: ny          !total y-direction points
      integer, parameter                   :: nsnow = 3   !maximum no. of snow layers

      integer                              :: ispin       !index for sinp-up
      integer                              :: nspin       !total spin-up times

!grided variables

      integer, allocatable, dimension(:,:) :: IVGTYP    !vegetation type
      integer, allocatable, dimension(:,:) :: ISLTYP    !soil type
      integer, allocatable, dimension(:,:) :: CROPTYPE  !(not used)
      integer, allocatable, dimension(:,:) :: SLOPETYP  !(only used for Noah runoff scheme)
      integer, allocatable, dimension(:,:) :: landsea   !land-sea mask (0-ocean; 1-land)
      integer, allocatable, dimension(:,:) :: ICE       !land-sea mask (0-ocean; 1-land)
      integer, allocatable, dimension(:,:) :: soilcolor !soil color index
      integer, allocatable, dimension(:,:) :: ist       !surface type: 1->soil; 2-> lake
      real, allocatable, dimension(:,:)    :: lat       !latitude
      real, allocatable, dimension(:,:)    :: cosz      !solar zenith angle
      real, allocatable, dimension(:,:)    :: lon       !longitude
      real, allocatable, dimension(:,:)    :: tbot      !soil temperature at zbot
      real, allocatable, dimension(:,:)    :: toposv    !standard dev. of DEM [m]
      real, allocatable, dimension(:,:,:)  :: FVEG      !greenness vegetation (shaded) fraction

      real, allocatable, dimension(:,:)    :: T_ML      !surface air temperature
      real, allocatable, dimension(:,:)    :: Q_ML      !surface air specific humidity
      real, allocatable, dimension(:,:)    :: QSFC1D    !surface air specific humidity
      real, allocatable, dimension(:,:)    :: QC        !cloud water mixing ratio (not used)
      real, allocatable, dimension(:,:)    :: lwdn      !downward longwave radiation
      real, allocatable, dimension(:,:)    :: U_ML      !wind speed at x-direction
      real, allocatable, dimension(:,:)    :: V_ML      !wind speed at y-direction
      real, allocatable, dimension(:,:)    :: P_ML      !surfae air pressure
      real, allocatable, dimension(:,:)    :: PSFC      !pressure at lowest model layer
      real, allocatable, dimension(:,:)    :: swdn      !downward shortwave radiation
      real, allocatable, dimension(:,:)    :: prcp      !precipitation
      real, allocatable, dimension(:,:)    :: PSNOW     !snow entering land model [mm/s]              ! MB/AN : v3.7
      real, allocatable, dimension(:,:)    :: PCONV     !convective precipitation entering  [mm/s]    ! MB/AN : v3.7
      real, allocatable, dimension(:,:)    :: PNONC     !non-convective precipitation entering [mm/s] ! MB/AN : v3.7
      real, allocatable, dimension(:,:)    :: PSHCV     !shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
      real, allocatable, dimension(:,:)    :: PGRPL     !graupel entering land model [mm/s]           ! MB/AN : v3.7
      real, allocatable, dimension(:,:)    :: PHAIL     !hail entering land model [mm/s]              ! MB/AN : v3.7
      real, allocatable, dimension(:,:)    :: O2PP     !
      real, allocatable, dimension(:,:)    :: CO2PP    !

      integer, allocatable, dimension(:,:) :: isnow     !actual no. of snow layers
      real, allocatable, dimension(:,:,:)  :: stc       !snow (3-L) /soil (4-L) tmperatures
      real, allocatable, dimension(:,:,:)  :: radius    !snow (3-L) grain radius  um
      real, allocatable, dimension(:,:,:)  :: smc       !vol. soil moisture (m3/m3)
      real, allocatable, dimension(:,:,:)  :: SMH2O     !vol. soil liquid water (m3/m3)
      real, allocatable, dimension(:,:,:)  :: sice      !vol. soil ice (m3/m3)
      real, allocatable, dimension(:,:,:)  :: SMCEQ     !equilibrium vol. soil moisture (m3/m3) (not used)
      real, allocatable, dimension(:,:)    :: tv        !vegetation canopy temperature
      real, allocatable, dimension(:,:)    :: tg        !ground surface temperature
      real, allocatable, dimension(:,:)    :: canice    !canopy-intercepted ice (mm)
      real, allocatable, dimension(:,:)    :: canliq    !canopy-intercepted liquid water (mm)
      real, allocatable, dimension(:,:)    :: SNDPTH    !snow depth (m)
      real, allocatable, dimension(:,:)    :: SWE       !snow water equivalent (mm)
      real, allocatable, dimension(:,:)    :: eah       !canopy air vapor pressure (pa)
      real, allocatable, dimension(:,:)    :: tah       !canopy air temperature (k)
      real, allocatable, dimension(:,:)    :: cm        !momentum drag coefficient
      real, allocatable, dimension(:,:)    :: ch        !sensible heat exchange coefficient
      real, allocatable, dimension(:,:)    :: CHV       !sensible heat exchange coefficient
      real, allocatable, dimension(:,:)    :: CHB       !sensible heat exchange coefficient
      real, allocatable, dimension(:,:)    :: CHLEAF    !leaf heat exchange coefficient
      real, allocatable, dimension(:,:)    :: CHUC      !under canopy exchange coefficient
      real, allocatable, dimension(:,:)    :: CHV2      !sensible heat exchange coefficient over vegetated fraction
      real, allocatable, dimension(:,:)    :: CHB2      !sensible heat exchange coefficient over bare-ground 
      real, allocatable, dimension(:,:)    :: FPICE     !snow fraction in precipitation
      real, allocatable, dimension(:,:)    :: PAH       !precipitation advected heat - total (W/m2)
      real, allocatable, dimension(:,:)    :: PAHV      !precipitation advected heat - vegetation net (W/m2)
      real, allocatable, dimension(:,:)    :: PAHG      !precipitation advected heat - under-canopy net (W/m2)
      real, allocatable, dimension(:,:)    :: PAHB      !precipitation advected heat - bare ground net (W/m2)
      real, allocatable, dimension(:,:)    :: EMISSI    !surface emmissivity
      real, allocatable, dimension(:,:)    :: SHC       !sensible heat (veg)
      real, allocatable, dimension(:,:)    :: SHB       !sensible heat (bare)
      real, allocatable, dimension(:,:)    :: SHG       !sensible heat (vegetated ground)
      real, allocatable, dimension(:,:)    :: TR        !water vapor flux (veg)
      real, allocatable, dimension(:,:)    :: EVC       !water vapor flux (veg)
      real, allocatable, dimension(:,:)    :: EVB       !water vapor flux (bare)
      real, allocatable, dimension(:,:)    :: EVG       !water vapor flux (vegetated ground)
      real, allocatable, dimension(:,:)    :: IRC       !longwave radiation flux (veg)
      real, allocatable, dimension(:,:)    :: IRB       !longwave radiation flux  (bare)
      real, allocatable, dimension(:,:)    :: IRG       !longwave radiation flux  (vegetated ground)
      real, allocatable, dimension(:,:)    :: GHB       !ground heat flux  (bare)
      real, allocatable, dimension(:,:)    :: GHV       !ground heat flux  (vegetated ground)
      real, allocatable, dimension(:,:)    :: TAUSS     !non-dimensional snow age
      real, allocatable, dimension(:,:)    :: RSSUN     !stomatal resistance (s/m)
      real, allocatable, dimension(:,:)    :: RSSHA     !stomatal resistance (s/m)
      real, allocatable, dimension(:,:)    :: BGAP      !between canopy gap fraction for beam (-)
      real, allocatable, dimension(:,:)    :: WGAP      !within canopy gap fraction for beam (-)

      real, allocatable, dimension(:,:,:)  :: qroot     !root water uptake of each layer (m/s)
      REAL, ALLOCATABLE, DIMENSION(:,:,:)  :: sadr      !root surface area density [m2/m3]

      real, allocatable, dimension(:,:)    :: fwet      !wetted or snowed fraction of the canopy (-)
      real, allocatable, dimension(:,:)    :: SWEO    !snow mass at last time step(mm h2o)
      real, allocatable, dimension(:,:)    :: albold    !snow albedo at last time step (-)
      real, allocatable, dimension(:,:)    :: qsnow     !snowfall on the ground [mm/s]
      real, allocatable, dimension(:,:)    :: wslake    !lake water storage [mm]
      real, allocatable, dimension(:,:,:)  :: FICEOLD   !snow layer ice fraction at previous step [mm]
  
      real, allocatable, dimension(:,:)    :: zwt       !water table depth [m]
      real, allocatable, dimension(:,:)    :: wa        !water in the "aquifer" [mm]
      real, allocatable, dimension(:,:)    :: wt        !groundwater storage [mm]
      real, allocatable, dimension(:,:,:)  :: tsno      !snow temperature [K]
      real, allocatable, dimension(:,:,:)  :: zsnso     !snow layer depth [m]
      real, allocatable, dimension(:,:,:)  :: snice     !snow layer ice [mm]
      real, allocatable, dimension(:,:,:)  :: snliq     !snow layer liquid water [mm]
      REAL, allocatable, dimension(:,:)    :: drymass   !total dry living carbon  mass [g/m2]
      REAL, allocatable, dimension(:,:)    :: lfmass    !leaf mass [g/m2]
      REAL, allocatable, dimension(:,:)    :: stmass    !stem mass [g/m2]
      REAL, allocatable, dimension(:,:)    :: rtmass    !root mass [g/m2]
      REAL, allocatable, dimension(:,:)    :: STBLCP    !stable carbon in deep soil [g/m2]
      REAL, allocatable, dimension(:,:)    :: FASTCP    !short-lived carbon, shallow soil [g/m2]
      REAL, allocatable, dimension(:,:)    :: wood      !mass of wood (incl. woody roots) [g/m2]
      REAL, allocatable, dimension(:,:)    :: soc       !soil organic carbon          [g C/m2]
      REAL, allocatable, dimension(:,:)    :: wdoc      !wet dissolved organic carbon [g C/m2]
      REAL, allocatable, dimension(:,:)    :: ddoc      !dry dissolved organic carbon [g C/m2]
      REAL, allocatable, dimension(:,:)    :: mic       !soil microbial biomass       [g C/m2]
      REAL, allocatable, dimension(:,:)    :: wenz      !wet soil enzyme              [g C/m2]
      REAL, allocatable, dimension(:,:)    :: denz      !dry soil enzyme              [g C/m2]
      REAL, allocatable, dimension(:,:)    :: soilc     !total soil carbon            [g C/m2]
      REAL, allocatable, dimension(:,:)    :: kr        !plant water stress factor [-]
      REAL, ALLOCATABLE, DIMENSION(:,:)    :: mq        !liquid water in living plant tissues [kg/m2]
      REAL, allocatable, dimension(:,:,:)  :: froot     !fractional root surface area [-]
      REAL, allocatable, dimension(:,:,:)  :: rootms    !carbon mass of fine roots [g/m2]

      integer, allocatable, dimension(:,:)    :: PGS    !plant growing stage [-]
      REAL, allocatable, dimension(:,:)    :: SMCWTD !soil water content between bottom of the soil and water table [m3/m3]
      REAL, allocatable, dimension(:,:)    :: DEEPRECH !recharge to or from the water table when deep [m]
      REAL, allocatable, dimension(:,:)    :: RECH !recharge to or from the water table when shallow [m] (diagnostic)

      REAL, allocatable, dimension(:,:)    :: PLAI      !leaf area index
      REAL, allocatable, dimension(:,:)    :: PSAI      !stem area index
      REAL, allocatable, dimension(:,:)    :: trad      !surface radiative temperature (k)
      REAL, allocatable, dimension(:,:)    :: TGB       !
      REAL, allocatable, dimension(:,:)    :: TGV       !
      REAL, allocatable, dimension(:,:)    :: T2MV      !2-m air temperature over vegetated part [k]
      REAL, allocatable, dimension(:,:)    :: T2MB      !2-m air temperature over bare ground part [k]
      REAL, allocatable, dimension(:,:)    :: Q2MV      !2-m air temperature over vegetated part [k]
      REAL, allocatable, dimension(:,:)    :: Q2MB      !2-m air temperature over bare ground part [k]
      REAL, allocatable, dimension(:,:)    :: t2m       !2 m air temperature (k)
      REAL, allocatable, dimension(:,:)    :: nee       !net ecosys exchange (g/m2/s C)
      REAL, allocatable, dimension(:,:)    :: gpp       !gross primary assimilation [g/m2/s C]
      REAL, allocatable, dimension(:,:)    :: npp       !net primary productivity [g/m2/s C]
      REAL, allocatable, dimension(:,:)    :: qco2      !soil surface co2 efflux (mg CO2/m2/s)
      REAL, allocatable, dimension(:,:)    :: FVEGMP    !greenness vegetation fraction [-] (not used)
      REAL, allocatable, dimension(:,:)    :: FVGMAX    !max greenness vegetation fraction [-] (not used)
      REAL, allocatable, dimension(:,:)    :: ndvi      !ndvi
      real, allocatable, dimension(:,:)    :: vmax      !max SOC decomposition rate per unit
                                                        !microbial biomass [g SOC/m2/[g MIC/m2]/s]
      real, allocatable, dimension(:,:)    :: vmaxup    !max DOC uptake rate [g DOC/m2/[g CMIC/m2]/s] 
      real, allocatable, dimension(:,:)    :: km        !Michaelis-Menten constant [g/m2] SOC decomp
      real, allocatable, dimension(:,:)    :: kmup      !Michaelis-Menten constant [g/m2] DOC uptake
      real, allocatable, dimension(:,:)    :: epslon    !carbon use efficiency

      real, allocatable, dimension(:,:)    :: QIN       !groundwater recharge [mm/s]
      real, allocatable, dimension(:,:)    :: runsf     !surface runoff [mm/s]
      real, allocatable, dimension(:,:)    :: runsb     !subsurface runoff [mm/s]
      real, allocatable, dimension(:,:)    :: ecan      !evaporation of intercepted water (mm/s)
      real, allocatable, dimension(:,:)    :: edir      !soil surface evaporation rate (mm/s]
      real, allocatable, dimension(:,:)    :: etran     !transpiration rate (mm/s)

      real, allocatable, dimension(:,:)    :: fsa       !total absorbed solar radiation (w/m2)
      real, allocatable, dimension(:,:)    :: fsr       !total reflected solar radiation (w/m2)
      real, allocatable, dimension(:,:)    :: fira      !total net longwave rad (w/m2) [+ to atm]
      real, allocatable, dimension(:,:)    :: fsh       !total sensible heat (w/m2) [+ to atm]
      real, allocatable, dimension(:,:)    :: fgev      !ground latent heat (w/m2) [+ to atm]
      real, allocatable, dimension(:,:)    :: fctr      !transpiration latent heat (w/m2) [+ to atm]
      real, allocatable, dimension(:,:)    :: fcev      !interception latent heat (w/m2) [+ to atm]
      real, allocatable, dimension(:,:)    :: flh       !total latent heat (w/m2) [+ to atm]
      real, allocatable, dimension(:,:)    :: SSOIL       !ground heat flux (w/m2) [+ to soil]
      real, allocatable, dimension(:,:)    :: apar      !photosyn active energy by canopy (w/m2)
      real, allocatable, dimension(:,:)    :: psn       !total photosynthesis (umol co2/m2/s) [+]
      real, allocatable, dimension(:,:)    :: sav       !solar rad absorbed by veg. (w/m2)
      real, allocatable, dimension(:,:)    :: sag       !solar rad absorbed by ground (w/m2)
      real, allocatable, dimension(:,:)    :: FSNO     !snow cover fraction on the ground (-)
      real, allocatable, dimension(:,:)    :: fsnoc     !snow cover fraction of the canopy (-)
      real, allocatable, dimension(:,:)    :: ALBSND    !snow albedo under the direct beam
      real, allocatable, dimension(:,:)    :: ALBSNI    !snow albedo under the diffuse radiation
      real, allocatable, dimension(:,:)    :: SALB      !surface albedo (-)
      real, allocatable, dimension(:,:)    :: QSNBOT    !snowmelt out bottom of pack [mm/s]
      real, allocatable, dimension(:,:)    :: POND      !surface ponding [mm]
      real, allocatable, dimension(:,:)    :: POND1     !surface ponding [mm]
      real, allocatable, dimension(:,:)    :: POND2     !surface ponding [mm]

      real, allocatable, dimension(:,:)    :: scf       !snow cover fraction of the grid cell (-)

      real, allocatable, dimension(:,:)    :: swdown    !downward solar radiation (- value removed)
      real, allocatable, dimension(:,:)    :: qsubcan   !sublimation (+ and -) from the canopy snow [mm/s]
      real, allocatable, dimension(:,:)    :: qsubgrd   !sublimation (+ and -) from the ground snow [mm/s]

!     integer, allocatable, dimension(:,:) :: atmbc !0->Neuman ;1->Dirichlet
!     real, allocatable, dimension(:,:) :: dtmixed !timestep (second) for the mixed-form RE (<dt)
!     real, allocatable, dimension(:,:) :: atmact  !infiltation [m/s]
!     real, allocatable, dimension(:,:,:)  :: liqdis    !
!     real, allocatable, dimension(:,:)    :: PONDSH    !surface ponding [mm]

! daily mean variables

      real, allocatable, dimension(:,:)    :: SNDPTHdm      !snow depth (m)
      real, allocatable, dimension(:,:)    :: SWEdm      !snow water equivelant (mm or kg/m2)
      real, allocatable, dimension(:,:)    :: scfdm        !snow cover fraction (-)
      real, allocatable, dimension(:,:)    :: FSNOdm      !snow cover fraction on the ground (-)
      real, allocatable, dimension(:,:)    :: runsfdm      !surface runoff (mm/s)
      real, allocatable, dimension(:,:)    :: runsbdm      !subsurface runoff (mm/s)
      real, allocatable, dimension(:,:)    :: qsubcandm    !sublimation (+ and -) from the canopy snow [mm/s]
      real, allocatable, dimension(:,:)    :: qsubgrddm    !sublimation (+ and -) from the ground snow [mm/s]
      real, allocatable, dimension(:,:,:)    :: smcdm    !
      real, allocatable, dimension(:,:,:)    :: SMH2Odm    !

! monthly mean variables

      real, allocatable, dimension(:,:)    :: SNDPTHm      !snow depth (m)
      real, allocatable, dimension(:,:)    :: SWEm      !snow water equivelant (mm or kg/m2)
      real, allocatable, dimension(:,:)    :: scfm        !snow cover fraction (-)
      real, allocatable, dimension(:,:)    :: FSNOm      !snow cover fraction on the ground (-)
      real, allocatable, dimension(:,:)    :: tgm         !ground surface temperature (K)
      real, allocatable, dimension(:,:)    :: prcpm       !precipitation (mm/s)
      real, allocatable, dimension(:,:)    :: QINM        !groundwater recharge (mm/s)
      real, allocatable, dimension(:,:)    :: runsfm      !surface runoff (mm/s)
      real, allocatable, dimension(:,:)    :: runsbm      !subsurface runoff (mm/s)
      real, allocatable, dimension(:,:)    :: ecanm       !evaporation of intercepted water (mm/s)
      real, allocatable, dimension(:,:)    :: edirm       !soil surface evaporation rate (mm/s]
      real, allocatable, dimension(:,:)    :: etranm      !transpiration rate (mm/s)
      real, allocatable, dimension(:,:)    :: zwtm        !the depth to water table (m)
      real, allocatable, dimension(:,:)    :: fsam        !total absorbed solar radiation (w/m2)
      real, allocatable, dimension(:,:)    :: firam       !total net longwave rad (w/m2) [+ to atm]
      real, allocatable, dimension(:,:)    :: fshm        !total sensible heat (w/m2) [+ to atm]
      real, allocatable, dimension(:,:)    :: flhm        !total latent heat (w/m2) [+ to atm]
      real, allocatable, dimension(:,:)    :: SSOILm      !ground heat flux (w/m2) [+ to soil]
      real, allocatable, dimension(:,:,:)  :: stcm        !soil temperature (K)
      real, allocatable, dimension(:,:,:)  :: smcm        !volumetric soil moisture (m3/m3)
      real, allocatable, dimension(:,:,:)  :: SMH2OM      !volumetric liquid water content in soil (m3/m3)
      real, allocatable, dimension(:,:)    :: aparm       !photosyn active energy by canopy (w/m2)
      real, allocatable, dimension(:,:)    :: psnm        !total photosynthesis (umol co2/m2/s) [+]
      real, allocatable, dimension(:,:)    :: savm        !solar rad absorbed by veg. (w/m2)
      real, allocatable, dimension(:,:)    :: sagm        !solar rad absorbed by ground (w/m2)
      real, allocatable, dimension(:,:)    :: PLAIm       !leaf area index (-)
      real, allocatable, dimension(:,:)    :: PSAIm       !stem area index (-)
      real, allocatable, dimension(:,:)    :: tradm       !surface radiative temperature (K)
      real, allocatable, dimension(:,:)    :: t2mm        !2 m air temperature (K)
      real, allocatable, dimension(:,:)    :: neem        !net ecosystem exchange (g/m2/s C)
      real, allocatable, dimension(:,:)    :: gppm        !gross primary productivity [g/m2/s C]
      real, allocatable, dimension(:,:)    :: nppm        !net primary productivity [g/m2/s C]
      real, allocatable, dimension(:,:)    :: fvegm       !greenness vegetation fraction [-]
      real, allocatable, dimension(:,:)    :: cmm         !surface exchange coeffcient for momentum [-]
      real, allocatable, dimension(:,:)    :: chm         !surface exchange coeffcient for heat [-]
      real, allocatable, dimension(:,:)    :: qco2m       !soil surface co2 efflux (mg CO2/m2/s)
      REAL, allocatable, dimension(:,:)    :: socm        !soil organic carbon          [g C/m3]
      REAL, allocatable, dimension(:,:)    :: wdocm       !wet dissolved organic carbon [g C/m3]
      REAL, allocatable, dimension(:,:)    :: ddocm       !dry dissolved organic carbon [g C/m3]
      REAL, allocatable, dimension(:,:)    :: micm        !soil microbial biomass       [g C/m3]
      REAL, allocatable, dimension(:,:)    :: wenzm       !wet soil enzyme              [g C/m3]
      REAL, allocatable, dimension(:,:)    :: denzm       !dry soil enzyme              [g C/m3]
      real, allocatable, dimension(:,:)    :: vmaxm   !max SOC decomposition rate per unit
                                                      !microbial biomass [g SOC/m2/[g MIC/m2]/s]
      real, allocatable, dimension(:,:)    :: vmaxupm !max DOC uptake rate [g DOC/m2/[g CMIC/m2]/s] 
      real, allocatable, dimension(:,:)    :: kmm     !Michaelis-Menten constant [g/m2] SOC decomp
      real, allocatable, dimension(:,:)    :: kmupm   !Michaelis-Menten constant [g/m2] DOC uptake
      real, allocatable, dimension(:,:)    :: epslonm !carbon use efficiency
      real, allocatable, dimension(:,:)    :: wam     !groundwater storage (mm)
      real, allocatable, dimension(:,:)    :: canwatm !water on the canopy (mm)
      real, allocatable, dimension(:,:)    :: ndvim   !NDVI (-)
      real, allocatable, dimension(:,:)    :: salbm   !monthly surface albedo (-)
      real, allocatable, dimension(:,:)    :: mqm !
      real, allocatable, dimension(:,:)    :: krm
      real, allocatable, dimension(:,:,:)  :: frootm
      real, allocatable, dimension(:,:)    :: rtmassm
      real, allocatable, dimension(:,:,:)  :: sadrm
      real, allocatable, dimension(:,:,:)  :: psim
      real, allocatable, dimension(:,:,:)  :: qrootm   !root water uptake [m/s]
      real, allocatable, dimension(:,:,:)  :: qrootnm  !negative part of qroot:
                                                       !hydraulic lift or descent [m/s]
      real, allocatable, dimension(:,:)    :: T_MLm    !2-m air temperature
      real, allocatable, dimension(:,:)    :: Q_MLm    !2-m specific humidity
      real, allocatable, dimension(:,:)    :: windm    !surface wind
      real, allocatable, dimension(:,:)    :: P_MLm    !surface air pressure
      real, allocatable, dimension(:,:)    :: HTOPM    !surface ponding depth [m]
      real, allocatable, dimension(:,:)    :: RSINEXM  !infiltration excess runoff [mm/s]

      integer, allocatable, dimension(:,:)    :: nndvi   !
      integer, allocatable, dimension(:,:)    :: nrs     !

      integer :: nn,nndvig
      real    :: fastcpgm,stblcpgm,ndvigm
      real    :: tvgm, tggm, etgm, runoffgm, smcgm(1:15),stcgm(1:15),radigm(-2:0)

!---------------------------------------------------------------------
! 1-d variables 
!---------------------------------------------------------------------
      real                              :: dt      !timestep (second) 
      real                              :: Z_ML    !height (m) above ground of atmos. forcing
      real                              :: Z0WRF   !roughness length (m)
      real                              :: foln    !nitrogen saturation

      real                              :: calday  !calendar day for computing solar zenith angle
!---------------------------------------------------------------------
!  declare/initialize constants
!---------------------------------------------------------------------
      character(len=14)  :: olddate
      character(len=14)  :: newdate
      character(len=256) :: dir      !directory where Run is
      character(len=256) :: fini     !directory where initial data files are
      character(len=8)   :: exp      !directory where an experiment is
      character(len=12)  :: MMINLU

      integer :: start_year,start_month,start_day,start_hour,start_min
      integer :: end_year
      integer :: IDVEG ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN ,IOPT_SFC  ,IOPT_FRZ , &
             IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF ,IOPT_TBOT ,IOPT_STC , &
             IOPT_RSF  ,IOPT_ROOT ,IOPT_WATRET,IOPT_SCM,IOPT_SMC

      LOGICAL :: allowed_to_read
      LOGICAL :: FNDSOILW, FNDSNOWH
      REAL    :: DX, DY
      real    :: DZ8W1D                                  !thickness of lowest layer (not used)
      INTEGER :: sf_urban_physics                        !urban, by yizhou

      integer, parameter                   :: MAXNSOIL=15 !max soil layers allowed
      real                                 :: DZS(1:MAXNSOIL)   !the thickness (m) of each soil layer 
      real                                 :: ZSOIL(1:MAXNSOIL) !depth of the bottom of soil layers(m)
      integer, allocatable, dimension(:,:) :: nsoil_var  !number of soil layers yhfang 0128 varsoil
      INTEGER, allocatable, dimension(:,:) :: cropcat
      real, allocatable, dimension(:,:,:)  :: crop       !5 crop areal fraction to initialize cropcat
      real, allocatable, dimension(:,:)    :: CANWAT     !canopy water
      real, allocatable, dimension(:,:)    :: chstar     !dummy
      REAL, allocatable, dimension(:,:)    :: GRAIN      !grain mass [g/m2]
      REAL, allocatable, dimension(:,:)    :: GDD        !growing degree days
      real, allocatable, dimension(:,:)    :: TSK        !skin temperature (k)
      real, allocatable, dimension(:,:,:)  :: TSLB       !4-L soil temperature (k)
      real, allocatable, dimension(:,:)    :: TMN        !deep soil temperature (k)
      real, allocatable, dimension(:,:)    :: SNOW
      real, allocatable, dimension(:,:)    :: SNOWH
      REAL, allocatable, DIMENSION(:,:)    :: XICE       !fraction of grid that is seaice

! for opt_run = 5 (lateral groundwater flow) not used

      REAL, allocatable, DIMENSION(:,:,:) :: smoiseq     !equilibrium soil moisture content [m3m-3]
      REAL, allocatable, DIMENSION(:,:)   :: smcwtdxy    !deep soil moisture content [m3m-3]
      REAL, allocatable, DIMENSION(:,:)   :: deeprechxy  !deep recharge [m]
      REAL, allocatable, DIMENSION(:,:)   :: rechxy      !accumulated recharge [mm]
      REAL, allocatable, DIMENSION(:,:)   :: qrfsxy      !accumulated flux from groundwater to rivers [mm]
      REAL, allocatable, DIMENSION(:,:)   :: qspringsxy  !accumulated seeping water [mm]
      REAL, allocatable, DIMENSION(:,:)   :: qslatxy     !accumulated lateral flow [mm]
      REAL, allocatable, DIMENSION(:,:)   :: areaxy      !grid cell area [m2]
      REAL, allocatable, DIMENSION(:,:)   :: FDEPTHXY    !efolding depth for transmissivity (m)
      REAL, allocatable, DIMENSION(:,:)   :: HT          !terrain height (m)
      REAL, allocatable, DIMENSION(:,:)   :: RIVERBEDXY  !riverbed depth (m)
      REAL, allocatable, DIMENSION(:,:)   :: EQZWT       !equilibrium water table depth (m)
      REAL, allocatable, DIMENSION(:,:)   :: RIVERCONDXY !river conductance
      REAL, allocatable, DIMENSION(:,:)   :: PEXPXY      !factor for river conductance
      REAL, allocatable, DIMENSION(:,:)   :: rechclim
      REAL, allocatable, DIMENSION(:,:)   :: MSFTX,MSFTY

      INTEGER :: STEPWTD
      REAL    :: WTDDT

! for opt_run = 6

      integer                              :: VARSD       !variable soil depth 
                                                          !0 -> 2m soil depth as implemented in NoahMP ; 
                                                          !1 -> variable soildepth to bedrock specified in namelist   
                                                          !2 -> variable soildepth to bed rock specified in static/soildepth.nc
      integer, allocatable, DIMENSION(:,:) :: ATM_BC      !0->Neuman ;1->Dirichlet
      REAL, allocatable, DIMENSION(:,:)    :: ATMACT      !actaul infiltration rate [m/s]
      REAL, allocatable, DIMENSION(:,:)    :: HTOP        !surface ponding hight [m]
      REAL, allocatable, DIMENSION(:,:)    :: RSINEX      !infiltration excess runoff [mm/s]
      REAL, allocatable, DIMENSION(:,:)    :: DTFINEM     !fine DT for mixed-form RE        
      REAL, allocatable, DIMENSION(:,:,:)  :: WCND        !hydraulic onductivity [m/s]
      REAL, allocatable, DIMENSION(:,:,:)  :: PSI         !pressure head [m]

      namelist /noahlsm_offline/DIR,fini     ,exp    ,varsd , nsoil   ,nx      ,ny     ,& 
                 DZS  ,start_year,start_month,start_day,start_hour,start_min ,& 
                 end_year  ,dt     ,Z_ML, nspin

      namelist /MP_OPTIONS/IDVEG ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN ,IOPT_SFC  ,IOPT_FRZ , &
                    IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF ,IOPT_TBOT ,IOPT_STC , &
                    IOPT_RSF  ,IOPT_ROOT ,IOPT_WATRET,IOPT_SCM,IOPT_SMC
!---------------------------------------------------------------------

      open(30,file="noah_offline.namelist",status='old',form="formatted")
      read(30,noahlsm_offline,iostat=ierr)
      if (ierr /= 0) then
         stop
      end if
      close(30)

      write(*,noahlsm_offline)

      write(mdt,'(i2.2,i2.2,i2.2)') start_month, start_day, start_hour
      write(minute,'(i2.2)') start_min
      print*, 'mdt, minute = ', mdt, minute
      write(year, '(i4.4)') start_year
      print*, 'year = ', year

      write(olddate,'(I4.4,I2.2,I2.2,I2.2,I2.2,I2.2)') & 
      start_year, start_month, start_day, start_hour, start_min, 0

      nst = int(86400./dt)
!----------------------------------------------------------------------
      open(31,file="options",status='old',form="formatted")
      read(31,MP_OPTIONS,iostat=ierr)
      close(31)

      CALL NOAHMP_OPTIONS(IDVEG ,IOPT_CRS  ,IOPT_BTR  ,IOPT_RUN ,IOPT_SFC  ,IOPT_FRZ , &
                      IOPT_INF  ,IOPT_RAD  ,IOPT_ALB  ,IOPT_SNF ,IOPT_TBOT ,IOPT_STC , &
                      IOPT_RSF  ,IOPT_ROOT ,IOPT_WATRET,IOPT_SCM)

!----------------------------------------------------------------------
! Allocate arrays for our gridded domain, now that we know the size
!----------------------------------------------------------------------
! land surface parameters

      allocate(lat   (1:nx,1:ny)); allocate(lon    (1:nx,1:ny)     ); allocate(cosz     (1:nx,1:ny))
      allocate(IVGTYP(1:nx,1:ny)); allocate(ISLTYP (1:nx,1:ny)     ); allocate(SLOPETYP (1:nx,1:ny))
      allocate(tbot  (1:nx,1:ny)); allocate(landsea(1:nx,1:ny)     ); allocate(ICE      (1:nx,1:ny))
      allocate( ist  (1:nx,1:ny)); allocate(FVEG   (1:nx,1:ny,1:12)); allocate(soilcolor(1:nx,1:ny))
      allocate(CROPTYPE(1:nx,1:ny)); allocate(toposv  (1:nx,1:ny))

      lat      (:,:)   = -999.0  ; lon      (:,:)   = -999.0        ; cosz     (:,:)   = -999.0
      IVGTYP   (:,:)   = -999    ; ISLTYP   (:,:)   = -999          ; SLOPETYP (:,:)   = -999
      landsea  (:,:)   = -999    ; ICE      (:,:)   = -999          ; soilcolor(:,:)   = -999
      ist      (:,:)   = -999    ; FVEG     (:,:,:) = -999.0        ; tbot     (:,:)   = -999.0
      CROPTYPE (:,:)   = -999

! forcing data

      allocate(T_ML  (1:nx,1:ny)); allocate(lwdn  (1:nx,1:ny))       ; allocate(Q_ML  (1:nx,1:ny))
      allocate(QSFC1D(1:nx,1:ny)); allocate(QC    (1:nx,1:ny))       ; allocate(U_ML  (1:nx,1:ny))
      allocate(V_ML  (1:nx,1:ny)); allocate(P_ML  (1:nx,1:ny))       ; allocate(PSFC  (1:nx,1:ny))
      allocate(swdn  (1:nx,1:ny)); allocate(prcp  (1:nx,1:ny))       ; allocate(PSNOW (1:nx,1:ny))
      allocate(PCONV (1:nx,1:ny)); allocate(PNONC (1:nx,1:ny))       ; allocate(PSHCV (1:nx,1:ny))
      allocate(PGRPL (1:nx,1:ny)); allocate(PHAIL (1:nx,1:ny))       ; allocate(CO2PP (1:nx,1:ny))
      allocate( O2PP (1:nx,1:ny))

      T_ML  (:,:) =-999.9       ;Q_ML  (:,:) =-999.9      ;QC    (:,:) =-999.9
      QSFC1D(:,:) =-999.9       ;lwdn  (:,:) =-999.       ;U_ML  (:,:) =-999.9
      V_ML  (:,:) =-999.9       ;P_ML  (:,:) =-999.9      ;PSFC  (:,:) =-999.9
      swdn  (:,:) =-999.9       ;prcp  (:,:) =-999.9      ;PSNOW (:,:) = 0.
      PCONV (:,:) = 0.          ;PNONC (:,:) = 0.         ;PSHCV (:,:) = 0.
      PGRPL (:,:) = 0.          ;PHAIL (:,:) = 0.         ;CO2PP (:,:) =-999.9
      O2PP  (:,:) =-999.9

! prognostic varaibles

! 2d and 3d prognostic variables
      allocate( smc    (1:nx,       1:nsoil,1:ny) ) ; allocate( SMCEQ  (1:nx,       1:nsoil,1:ny) )
      allocate( SMH2O  (1:nx,       1:nsoil,1:ny) ) ; allocate( sice   (1:nx,       1:nsoil,1:ny) )
      allocate( rootms (1:nx,       1:nsoil,1:ny) ) ; allocate( froot  (1:nx,       1:nsoil,1:ny) )
      allocate( qroot  (1:nx,       1:nsoil,1:ny) ) ; allocate( sadr   (1:nx,       1:nsoil,1:ny) )
      allocate( stc    (1:nx,-nsnow+1:nsoil,1:ny) ) ; allocate( zsnso  (1:nx,-nsnow+1:nsoil,1:ny) )
      allocate( tsno   (1:nx,-nsnow+1:    0,1:ny) ) ; allocate( snice  (1:nx,-nsnow+1:    0,1:ny) )
      allocate( snliq  (1:nx,-nsnow+1:    0,1:ny) ) ; allocate( FICEOLD(1:nx,-nsnow+1:    0,1:ny) )
      allocate( radius (1:nx,-nsnow+1:    0,1:ny) )

      allocate( tv     (1:nx,1:ny)) ; allocate( tg     (1:nx,1:ny)) ; allocate( canliq (1:nx,1:ny))             
      allocate( canice (1:nx,1:ny)) ; allocate( SNDPTH (1:nx,1:ny)) ; allocate( SWE    (1:nx,1:ny))
      allocate( zwt    (1:nx,1:ny)) ; allocate( wa     (1:nx,1:ny)) ; allocate( wt     (1:nx,1:ny))
      allocate( isnow  (1:nx,1:ny)) ; allocate( lfmass (1:nx,1:ny)) ; allocate( stmass (1:nx,1:ny))
      allocate( albsnd (1:nx,1:ny)) ; allocate( albsni (1:nx,1:ny)) ;
      allocate( rtmass (1:nx,1:ny)) ; allocate( STBLCP (1:nx,1:ny)) ; allocate( FASTCP (1:nx,1:ny))
      allocate( wood   (1:nx,1:ny)) ; allocate( drymass(1:nx,1:ny)) ; allocate( soc    (1:nx,1:ny))
      allocate( wdoc   (1:nx,1:ny)) ; allocate( ddoc   (1:nx,1:ny)) ; allocate( mic    (1:nx,1:ny))
      allocate( wenz   (1:nx,1:ny)) ; allocate( denz   (1:nx,1:ny)) ; allocate( soilc  (1:nx,1:ny))
      allocate( PLAI   (1:nx,1:ny)) ; allocate( PSAI   (1:nx,1:ny)) ; allocate( eah    (1:nx,1:ny))
      allocate( tah    (1:nx,1:ny)) ; allocate( fwet   (1:nx,1:ny)) ; allocate( SWEO   (1:nx,1:ny))
      allocate( albold (1:nx,1:ny)) ; allocate( qsnow  (1:nx,1:ny)) ; allocate( wslake (1:nx,1:ny))
      allocate( cm     (1:nx,1:ny)) ; allocate( ch     (1:nx,1:ny)) ; allocate( CHV    (1:nx,1:ny))
      allocate( CHB    (1:nx,1:ny)) ; allocate( CHLEAF (1:nx,1:ny)) ; allocate( CHUC   (1:nx,1:ny))
      allocate( CHV2   (1:nx,1:ny)) ; allocate( CHB2   (1:nx,1:ny)) ; allocate( FPICE  (1:nx,1:ny))
      allocate( PAH    (1:nx,1:ny)) ; allocate( PAHV   (1:nx,1:ny)) ; allocate( PAHG   (1:nx,1:ny))
      allocate( PAHB   (1:nx,1:ny)) ; allocate( EMISSI (1:nx,1:ny)) ; allocate( SHC    (1:nx,1:ny))
      allocate( SHG    (1:nx,1:ny)) ; allocate( SHB    (1:nx,1:ny)) ; allocate( TR     (1:nx,1:ny))
      allocate( EVC    (1:nx,1:ny)) ; allocate( EVG    (1:nx,1:ny)) ; allocate( EVB    (1:nx,1:ny))
      allocate( IRC    (1:nx,1:ny)) ; allocate( IRG    (1:nx,1:ny)) ; allocate( IRB    (1:nx,1:ny))
      allocate( GHV    (1:nx,1:ny)) ; allocate( GHB    (1:nx,1:ny)) ; allocate( RSSUN  (1:nx,1:ny))
      allocate( RSSHA  (1:nx,1:ny)) ; allocate( BGAP   (1:nx,1:ny)) ; allocate( WGAP   (1:nx,1:ny))
      allocate( TAUSS  (1:nx,1:ny)) ; allocate( kr     (1:nx,1:ny)) ; allocate( mq     (1:nx,1:ny))
      allocate( PGS    (1:nx,1:ny))

      smc  (:,:,:) = -999.9 ; SMCEQ  (:,:,:) = -999.9 ; SMH2O(:,:,:) = -999.9 ; sice  (:,:,:) = -999.9
      froot(:,:,:) = -999.9 ; qroot  (:,:,:) = -999.9 ; sadr (:,:,:) = -999.9 ; rootms(:,:,:) = -999.9
      stc  (:,:,:) = -999.9 ; zsnso  (:,:,:) = -999.9 ; tsno (:,:,:) = -999.9 ; snice (:,:,:) = -999.9
      snliq(:,:,:) = -999.9 ; FICEOLD(:,:,:) = -999.9 ; radius(:,:,:) = -999.9 ;

      tv     (:,:) = -999.9 ; tg     (:,:) = -999.9 ; canliq (:,:) = -999.9 ; canice (:,:) = -999.9 
      SNDPTH (:,:) = -999.9 ; SWE    (:,:) = -999.9 ; zwt    (:,:) = -999.9 ; wa     (:,:) = -999.9
      albsnd (:,:) = -999.9 ; albsni (:,:) = -999.9 ;
      wt     (:,:) = -999.9 ; isnow  (:,:) = -999   ; lfmass (:,:) = -999.9 ; stmass (:,:) = -999.9
      STBLCP (:,:) = -999.9 ; FASTCP (:,:) = -999.9 ; rtmass (:,:) = -999.9 ; wood   (:,:) = -999.9
      drymass(:,:) = -999.9 ; soc    (:,:) = -999.9 ; wdoc   (:,:) = -999.9 ; ddoc   (:,:) = -999.9
      mic    (:,:) = -999.9 ; wenz   (:,:) = -999.9 ; denz   (:,:) = -999.9 ; soilc  (:,:) = -999.9
      PLAI   (:,:) = -999.9 ; PSAI   (:,:) = -999.9 ; eah    (:,:) = -999.9 ; tah    (:,:) = -999.9
      fwet   (:,:) = -999.9 ; SWEO   (:,:) = -999.9 ; albold (:,:) = -999.9 ; qsnow  (:,:) = -999.9
      wslake (:,:) = -999.9 ; cm     (:,:) = -999.9 ; ch     (:,:) = -999.9 ; CHV    (:,:) = -999.9
      CHB    (:,:) = -999.9 ; CHLEAF (:,:) = -999.9 ; CHUC   (:,:) = -999.9 ; CHV2   (:,:) = -999.9
      CHB2   (:,:) = -999.9 ; FPICE  (:,:) = -999.9 ; PAH    (:,:) = -999.9 ; PAHV   (:,:) = -999.9
      PAHB   (:,:) = -999.9 ; PAHG   (:,:) = -999.9 ; TAUSS  (:,:) = -999.9 ; EMISSI (:,:) = -999.9
      SHG    (:,:) = -999.9 ; SHG    (:,:) = -999.9 ; SHC    (:,:) = -999.9 ; EVC    (:,:) = -999.9
      EVG    (:,:) = -999.9 ; EVB    (:,:) = -999.9 ; TR     (:,:) = -999.9 ; IRC    (:,:) = -999.9
      IRG    (:,:) = -999.9 ; IRB    (:,:) = -999.9 ; GHV    (:,:) = -999.9 ; GHB    (:,:) = -999.9
      RSSUN  (:,:) = -999.9 ; RSSHA  (:,:) = -999.9 ; BGAP   (:,:) = -999.9 ; WGAP   (:,:) = -999.9
      kr     (:,:) = -999.9 ; mq     (:,:) = -999.9 ; PGS    (:,:) = -999.9 ; SHB    (:,:) = -999.9

! for output
!     allocate( liqdis   (1:nx,1:nsoil,1:ny) )
      allocate(QIN     (1:nx,1:ny)) ; allocate(runsf  (1:nx,1:ny)) ; allocate(runsb  (1:nx,1:ny))
      allocate(ecan    (1:nx,1:ny)) ; allocate(edir   (1:nx,1:ny)) ; allocate(etran  (1:nx,1:ny)) 
      allocate(fsa     (1:nx,1:ny)) ; allocate(fsr    (1:nx,1:ny)) ; allocate(fira   (1:nx,1:ny))
      allocate(fsh     (1:nx,1:ny)) ; allocate(fgev   (1:nx,1:ny)) ; allocate(fctr   (1:nx,1:ny))
      allocate(fcev    (1:nx,1:ny)) ; allocate(flh    (1:nx,1:ny)) ; allocate(SSOIL  (1:nx,1:ny))
      allocate(apar    (1:nx,1:ny)) ; allocate(psn    (1:nx,1:ny)) ; allocate(sav    (1:nx,1:ny))
      allocate(sag     (1:nx,1:ny)) ; allocate(FSNO   (1:nx,1:ny)) ; allocate(fsnoc  (1:nx,1:ny))
      allocate(SALB    (1:nx,1:ny)) ; allocate(QSNBOT (1:nx,1:ny)) ; allocate(POND   (1:nx,1:ny))
      allocate(POND1   (1:nx,1:ny)) ; allocate(POND2  (1:nx,1:ny)) ; allocate(scf    (1:nx,1:ny))
      allocate(swdown  (1:nx,1:ny)) ; allocate(trad   (1:nx,1:ny)) ; allocate(TGB    (1:nx,1:ny))
      allocate(TGV     (1:nx,1:ny)) ; allocate(t2m    (1:nx,1:ny)) ; allocate(T2MV   (1:nx,1:ny))
      allocate(T2MB    (1:nx,1:ny)) ; allocate(Q2MV   (1:nx,1:ny)) ; allocate(Q2MB   (1:nx,1:ny))
      allocate(nee     (1:nx,1:ny)) ; allocate(gpp    (1:nx,1:ny)) ; allocate(npp    (1:nx,1:ny))
      allocate(qco2    (1:nx,1:ny)) ; allocate(vmax   (1:nx,1:ny)) ; allocate(km     (1:nx,1:ny))
      allocate(vmaxup  (1:nx,1:ny)) ; allocate(kmup   (1:nx,1:ny)) ; allocate(epslon (1:nx,1:ny))
      allocate(FVEGMP  (1:nx,1:ny)) ; allocate(FVGMAX (1:nx,1:ny)) ; allocate(ndvi   (1:nx,1:ny))
      allocate(qsubcan (1:nx,1:ny)) ; allocate(qsubgrd(1:nx,1:ny)) ; allocate(SMCWTD (1:nx,1:ny))       
      allocate(DEEPRECH(1:nx,1:ny)) ; allocate(RECH   (1:nx,1:ny))

!     liqdis   (:,:,:) = -999.9
!     PONDSH   (:,:) = -999.9
      QIN    (:,:) = -999.9 ; runsf  (:,:) = -999.9 ; runsb  (:,:) = -999.9 ; ecan   (:,:) = -999.9
      edir   (:,:) = -999.9 ; etran  (:,:) = -999.9 ; fsa    (:,:) = -999.9 ; fsr    (:,:) = -999.9
      fira   (:,:) = -999.9 ; fsh    (:,:) = -999.9 ; fgev   (:,:) = -999.9 ; fctr   (:,:) = -999.9
      fcev   (:,:) = -999.9 ; flh    (:,:) = -999.9 ; SSOIL  (:,:) = -999.9 ; apar   (:,:) = -999.9
      psn    (:,:) = -999.9 ; sav    (:,:) = -999.9 ; sag    (:,:) = -999.9 ; FSNO   (:,:) = -999.9
      fsnoc  (:,:) = -999.9 ; SALB   (:,:) = -999.9 ; QSNBOT (:,:) = -999.9 ; POND   (:,:) = -999.9
      POND1  (:,:) = -999.9 ; POND2  (:,:) = -999.9 ; scf    (:,:) = -999.9 ; swdown (:,:) = -999.9
      trad   (:,:) = -999.9 ; TGB    (:,:) = -999.9 ; TGV    (:,:) = -999.9 ; T2MV   (:,:) = -999.9
      T2MB   (:,:) = -999.9 ; Q2MV   (:,:) = -999.9 ; Q2MB   (:,:) = -999.9 ; t2m    (:,:) = -999.9
      nee    (:,:) = -999.9 ; gpp    (:,:) = -999.9 ; npp    (:,:) = -999.9 ; qco2   (:,:) = -999.9
      vmax   (:,:) = -999.9 ; km     (:,:) = -999.9 ; vmaxup (:,:) = -999.9 ; kmup   (:,:) = -999.9
      epslon (:,:) = -999.9 ; FVEGMP (:,:) = -999.9 ; FVGMAX (:,:) = -999.9 ; ndvi   (:,:) = -999.9
      qsubcan(:,:) = -999.9 ; qsubgrd(:,:) = -999.9 ; SMCWTD (:,:) = -999.9 ; DEEPRECH(:,:) = -999.9
      RECH   (:,:) = -999.9

! monthly mean variables for output
      allocate(frootm (1:nx,1:nsoil,1:ny)) ; allocate(sadrm  (1:nx,1:nsoil,1:ny))
      allocate(psim   (1:nx,1:nsoil,1:ny)) ; allocate(qrootm (1:nx,1:nsoil,1:ny))
      allocate(qrootnm(1:nx,1:nsoil,1:ny)) ; allocate(stcm   (1:nx,1:nsoil,1:ny))
      allocate(smcm   (1:nx,1:nsoil,1:ny)) ; allocate(SMH2OM (1:nx,1:nsoil,1:ny))

      allocate(SNDPTHdm (1:nx,1:ny)) ; allocate(SWEdm    (1:nx,1:ny)) ; allocate(scfdm    (1:nx,1:ny))
      allocate(FSNOdm   (1:nx,1:ny)) ; allocate(runsfdm  (1:nx,1:ny)) ; allocate(runsbdm  (1:nx,1:ny))
      allocate(qsubcandm(1:nx,1:ny)) ; allocate(qsubgrddm(1:nx,1:ny))
      allocate(smcdm    (1:nx,1:nsoil,1:ny)) ; allocate(SMH2Odm(1:nx,1:nsoil,1:ny))

      allocate(SNDPTHm(1:nx,1:ny)) ; allocate(SWEm   (1:nx,1:ny)) ; allocate(scfm   (1:nx,1:ny))
      allocate(FSNOm  (1:nx,1:ny)) ; allocate(tgm    (1:nx,1:ny)) ; allocate(prcpm  (1:nx,1:ny))
      allocate(QINM   (1:nx,1:ny)) ; allocate(runsfm (1:nx,1:ny)) ; allocate(runsbm (1:nx,1:ny))
      allocate(ecanm  (1:nx,1:ny)) ; allocate(edirm  (1:nx,1:ny)) ; allocate(etranm (1:nx,1:ny))
      allocate(zwtm   (1:nx,1:ny)) ; allocate(fsam   (1:nx,1:ny)) ; allocate(firam  (1:nx,1:ny))
      allocate(fshm   (1:nx,1:ny)) ; allocate(flhm   (1:nx,1:ny)) ; allocate(SSOILm (1:nx,1:ny))
      allocate(aparm  (1:nx,1:ny)) ; allocate(psnm   (1:nx,1:ny)) ; allocate(savm   (1:nx,1:ny))
      allocate(sagm   (1:nx,1:ny)) ; allocate(PLAIm  (1:nx,1:ny)) ; allocate(PSAIm  (1:nx,1:ny))
      allocate(tradm  (1:nx,1:ny)) ; allocate(t2mm   (1:nx,1:ny)) ; allocate(neem   (1:nx,1:ny))
      allocate(gppm   (1:nx,1:ny)) ; allocate(nppm   (1:nx,1:ny)) ; allocate(fvegm  (1:nx,1:ny))
      allocate(cmm    (1:nx,1:ny)) ; allocate(chm    (1:nx,1:ny)) ; allocate(rtmassm(1:nx,1:ny))
      allocate(qco2m  (1:nx,1:ny)) ; allocate(socm   (1:nx,1:ny))
      allocate(wdocm  (1:nx,1:ny)) ; allocate(ddocm  (1:nx,1:ny)) ; allocate(micm   (1:nx,1:ny))
      allocate(wenzm  (1:nx,1:ny)) ; allocate(denzm  (1:nx,1:ny)) ; allocate(vmaxm  (1:nx,1:ny))
      allocate(kmm    (1:nx,1:ny)) ; allocate(vmaxupm(1:nx,1:ny)) ; allocate(kmupm  (1:nx,1:ny))
      allocate(epslonm(1:nx,1:ny)) ; allocate(wam    (1:nx,1:ny)) ; allocate(canwatm(1:nx,1:ny))
      allocate(ndvim  (1:nx,1:ny)) ; allocate(T_MLm  (1:nx,1:ny)) ; allocate(nndvi  (1:nx,1:ny))
      allocate(nrs    (1:nx,1:ny)) ; allocate(Q_MLm  (1:nx,1:ny)) ; allocate(windm  (1:nx,1:ny))
      allocate(P_MLm  (1:nx,1:ny)) ; allocate(mqm    (1:nx,1:ny)) ; allocate(krm    (1:nx,1:ny))
      allocate(salbm  (1:nx,1:ny)) ; allocate(HTOPM  (1:nx,1:ny)) ; allocate(RSINEXM(1:nx,1:ny))

      frootm  (:,:,:) =-999.9 ; sadrm   (:,:,:) =-999.9 ; psim    (:,:,:) =-999.9
      qrootm  (:,:,:) =-999.9 ; qrootnm (:,:,:) =-999.9 ; stcm    (:,:,:) =-999.9
      smcm    (:,:,:) =-999.9 ; SMH2OM  (:,:,:) =-999.9

      SNDPTHdm(:,:) = -999.9 ; SWEdm   (:,:) = -999.9 ; scfdm    (:,:) = -999.9 ; FSNOdm   (:,:) = -999.9 
      runsfdm (:,:) = -999.9 ; runsbdm (:,:) = -999.9 ; qsubcandm(:,:) = -999.9 ; qsubgrddm(:,:) = -999.9 

      SNDPTHm (:,:) = -999.9 ; SWEm    (:,:) = -999.9 ; scfm    (:,:) = -999.9 ; FSNOm   (:,:) = -999.9
      tgm     (:,:) = -999.9 ; prcpm   (:,:) = -999.9 ; QINM    (:,:) = -999.9 ; runsfm  (:,:) = -999.9
      runsbm  (:,:) = -999.9 ; ecanm   (:,:) = -999.9 ; edirm   (:,:) = -999.9 ; etranm  (:,:) = -999.9
      zwtm    (:,:) = -999.9 ; fsam    (:,:) = -999.9 ; firam   (:,:) = -999.9 ; fshm    (:,:) = -999.9
      flhm    (:,:) = -999.9 ; SSOILm  (:,:) = -999.9 ; aparm   (:,:) = -999.9 ; psnm    (:,:) = -999.9
      savm    (:,:) = -999.9 ; sagm    (:,:) = -999.9 ; PLAIm   (:,:) = -999.9 ; PSAIm   (:,:) = -999.9
      tradm   (:,:) = -999.9 ; t2mm    (:,:) = -999.9 ; neem    (:,:) = -999.9 ; gppm    (:,:) = -999.9
      nppm    (:,:) = -999.9 ; fvegm   (:,:) = -999.9 ; cmm     (:,:) = -999.9 ; chm     (:,:) = -999.9
      qco2m   (:,:) = -999.9 ; socm    (:,:) = -999.9 ; krm     (:,:) = -999.9 ; rtmassm (:,:) = -999.9
      wdocm   (:,:) = -999.9 ; ddocm   (:,:) = -999.9 ; micm    (:,:) = -999.9 ; wenzm   (:,:) = -999.9
      denzm   (:,:) = -999.9 ; vmaxm   (:,:) = -999.9 ; kmm     (:,:) = -999.9 ; vmaxupm (:,:) = -999.9
      kmupm   (:,:) = -999.9 ; epslonm (:,:) = -999.9 ; wam     (:,:) = -999.9 ; canwatm (:,:) = -999.9
      ndvim   (:,:) = -999.9 ; nndvi   (:,:) = -999   ; nrs     (:,:) = -999   ; T_MLm   (:,:) = -999.9
      Q_MLm   (:,:) = -999.9 ; windm   (:,:) = -999.9 ; P_MLm   (:,:) = -999.9 ; mqm     (:,:) = -999.9
      salbm   (:,:) = -999.9 ; HTOPM   (:,:) = -999.9 ; RSINEXM (:,:) = -999.9

! added for coupling with WRF

      allocate(CANWAT    (1:nx,1:ny)) ; allocate(cropcat   (1:nx,1:ny)) ; allocate(chstar    (1:nx,1:ny))
      allocate(GDD       (1:nx,1:ny)) ; allocate(TSK       (1:nx,1:ny)) ; allocate(TMN       (1:nx,1:ny))
      allocate(GRAIN     (1:nx,1:ny)) ; allocate(XICE      (1:nx,1:ny)) ; allocate(crop      (1:nx,1:5,1:ny))
      allocate(TSLB      (1:nx,1:nsoil,1:ny))
      CANWAT(:,:)     = -999.9 ; cropcat(:,:)    = -999   ; chstar(:,:)     = -999.9 ; GDD(:,:)        = -999.9
      TSK(:,:)        = -999.9 ; TMN(:,:)        = -999.9 ; GRAIN(:,:)      = -999.9 ; XICE(:,:)       = -999.9
      crop(:,:,:)     = -999.9 ; TSLB(:,:,:)     = -999.9 ; 

! for lateral groundwater flow model (opt_run = 5; not used)

      allocate(smoiseq(1:nx,1:nsoil,1:ny))
      allocate(smcwtdxy(1:nx,1:ny))   ; allocate(deeprechxy(1:nx,1:ny)) ; allocate(rechxy (1:nx,1:ny))
      allocate(qrfsxy  (1:nx,1:ny))   ; allocate(qspringsxy(1:nx,1:ny)) ; allocate(qslatxy(1:nx,1:ny))
      allocate(areaxy  (1:nx,1:ny))   ; allocate(FDEPTHXY  (1:nx,1:ny)) ; allocate(HT     (1:nx,1:ny))
      allocate(RIVERBEDXY(1:nx,1:ny)) ; allocate(EQZWT     (1:nx,1:ny)) ; allocate(RIVERCONDXY(1:nx,1:ny))
      allocate(PEXPXY(1:nx,1:ny))     ; allocate(rechclim  (1:nx,1:ny)) ; allocate(MSFTX  (1:nx,1:ny))
      allocate(MSFTY(1:nx,1:ny))

      smoiseq(:,:,:)  = -999.9    
      smcwtdxy(:,:)   = -999.9 ; deeprechxy(:,:) = -999.9 ; rechxy(:,:)      = -999.9
      qrfsxy(:,:)     = -999.9 ; qspringsxy(:,:) = -999.9 ; qslatxy(:,:)     = -999.9
      areaxy(:,:)     = -999.9 ; FDEPTHXY(:,:)   = -999.9 ; HT(:,:)          = -999.9
      RIVERBEDXY(:,:) = -999.9 ; EQZWT(:,:)      = -999.9 ; RIVERCONDXY(:,:) = -999.9
      PEXPXY(:,:)     = -999.9 ; rechclim(:,:)   = -999.9 ; MSFTX(:,:)       = -999.9
      MSFTY(:,:)      = -999.9

! for mixed-from RE

      allocate( PSI     (1:nx,1:nsoil,1:ny) )
      allocate( WCND    (1:nx,1:nsoil,1:ny) )
      allocate( ATM_BC  (1:nx,1:ny) )
      allocate( ATMACT  (1:nx,1:ny) )
      allocate( HTOP    (1:nx,1:ny) )
      allocate( RSINEX  (1:nx,1:ny) )
      allocate( DTFINEM (1:nx,1:ny) )
      allocate( nsoil_var   (1:nx,1:ny) )

      PSI         (:,:,:)  = -999.9
      WCND        (:,:,:)  = -999.9
      DTFINEM     (:,:)    = -999.9
      ATM_BC      (:,:)    = -999
      HTOP        (:,:)    = -999.9
      RSINEX      (:,:)    = -999.9
      ATMACT      (:,:)    =  0.
      nsoil_var   (:,:)    = -999

!---------------------------------------------------------------------
! read landuse type and soil texture and other information
!----------------------------------------------------------------------

      call readland(dir,nx,ny,VARSD,nsoil,DZS, &
                     IVGTYP,ISLTYP,SLOPETYP,lat,lon,landsea,tbot,soilcolor, &
                     toposv,nsoil_var)

       FNDSNOWH       = .FALSE.
       foln           = 1.5        !saturated value
       CROPTYPE(:,:)  = 0          !0 -> use Noah-MP (croptype = cropcat; not used)
       crop(:,:,:)    = 0.0        !crop fraction = 0.0 not to use the crop model
       DX             = 12000.     !not used
       DY             = 12000.     !not used
       DZ8W1D         = 1.0        !not used
       SMCEQ(:,:,:)   = 0.4        !not used but in shallow water table of Fan
       TMN(:,:)       = 0.0        !not used
       PLAI(:,:)      = 0.1

       MMINLU         = "USGS"
       SWE(:,:)       = 0.0        !=SNOW (mm)
       SNDPTH(:,:)    = 0.0        !=SNOWH (m)
       XICE(:,:)      = 0.0
       SMC(:,:,:)     = 0.28       !=SMOIS
       TSLB(:,:,:)    = 287.0      !=TSLB (soil 1 - 4 layers)
       TSK(:,:)       = 287.0      !-> TV & TG
       QSFC1D(:,:)    = 0.015      !not used

       ist(:,:)       = 1
       do i = 1,nx; do j = 1,ny
        if(IVGTYP(I,J)  == 16) ist(I,J) = 2   ! lake points
        if(IVGTYP(I,J)  == 24) ist(I,J) = 2   ! glacier
       end do; end do

! green vegetation fraction

      call readveg(dir,nx,ny,landsea,FVEG,FVGMAX)

! snicar radiative constants for snow
 
      call read_rad(rad_cons)

!----------------------------------------------------------------------
! initialize model state
!----------------------------------------------------------------------

      CALL NOAHMP_INIT(MMINLU, SWE , SNDPTH , CANWAT , &
            ISLTYP ,   IVGTYP, SLOPETYP , SOILCOLOR, ist, &
            TSLB, SMC , SMH2O , SICE, DZS , FNDSOILW , FNDSNOWH ,                &
            TSK, isnow , tv     ,tg     ,canice ,         TMN,     XICE,   &
            canliq ,eah    ,tah    ,cm     ,ch     ,                     &
            fwet   ,SWEO   ,albold ,qsnow  ,wslake ,zwt    ,wa     , &
            wt     ,tsno   ,zsnso  ,snice  ,snliq  ,radius ,lfmass ,rtmass , &
            stmass ,wood   ,stblcp ,fastcp ,psai   ,plai   ,         &
            soc    ,wdoc   ,ddoc   ,mic    ,wenz   ,denz   , &
            rootms ,froot  ,kr     ,mq     ,ZSOIL  ,stc    , &
            PSI    ,ATM_BC ,ATMACT ,DTFINEM,iopt_watret, &  
            QIN    ,WCND   ,nsoil_var ,HTOP,  &
            GRAIN  ,GDD    ,                                                   &
            crop   ,cropcat,                                                   &
            t2mv   ,t2mb   ,chstar,            &
            nsoil, fini,                 &
            allowed_to_read , iopt_run,                         &
            sf_urban_physics,                         &  ! urban scheme
            1,nx+1, 1,ny+1, 1,30,                &
            1,nx  , 1,ny  , 1,30,                &
            1,nx  , 1,ny  , 1,30,                &
            smoiseq  ,smcwtdxy ,rechxy   ,deeprechxy, areaxy, dx, dy, msftx, msfty,&     ! Optional groundwater
            wtddt    ,stepwtd  ,dt       ,qrfsxy     ,qspringsxy  , qslatxy ,  &      ! Optional groundwater
            fdepthxy ,ht     ,riverbedxy ,eqzwt     ,rivercondxy ,pexpxy ,  &      ! Optional groundwater
            rechclim )    ! Optional groundwater

      TAUSS(:,:)  = 0.0
      qroot(:,:,:) = 0.

! ================================================================================
! BEGIN TIME LOOP
! ================================================================================
      do ispin = 1, nspin

      it = 0
      yrloop : do iy  = start_year, end_year
       startmonth = 1
       if(iy == start_year) startmonth = start_month

       if(mod(iy,4).eq.0) then
          nday(2) = 29
          istep   = gday(startmonth-1)*nst
          YEARLEN = 366
       else
          nday(2) = 28
          istep   = jday(startmonth-1)*nst
          YEARLEN = 365
       end if
       startstep  = istep+1

      mloop : do im  = startmonth,  12

       startday = 1
       if(iy == start_year .and. im == start_month) &
       startday = start_day

       imstep = 0
      dloop : do id  = startday, nday(im)
       idstep = 0
      hloop : do ih  = 1, nst
           
       it     = it     + 1
       istep  = istep  + 1
       imstep = imstep + 1
       idstep = idstep + 1

       read(mdt(1:2),'(i2)') imonth
       read(mdt(3:4),'(i2)') iday
       read(mdt(5:6),'(i2)') itime

       write(6,'(7i8)') it,iy,im,id,ih,itime,ispin

       !read forcing data.

       call readforc(nx    ,ny     ,dt      ,iy    ,im    ,id       ,&
                     itime ,dir    ,T_ML    ,Q_ML  ,lwdn  ,U_ML     ,&
                     V_ML  ,P_ML   ,swdn    ,prcp  )

       !calday to compute cosz
       calday = jday(im-1) + id + itime/24.
       JULIAN = jday(im-1) + id

       !compute cosz
       !$OMP PARALLEL DO
       do i=1,nx
       do j=1,ny
          call lsmzen (calday,lon(I,J),lat(I,J),cosz(I,J))

          PSFC(I,J)    = P_ML(I,J)
          QC(I,J)      = Q_ML(I,J)
          PCONV(I,J)   = prcp(I,J)     !all in convective prcp
          PNONC(I,J)   = 0.0
          PSHCV(I,J)   = 0.0
          PSNOW(I,J)   = 0.0
          PGRPL(I,J)   = 0.0
          PHAIL(I,J)   = 0.0
       end do
       end do
       !$OMP END PARALLEL DO

       !$OMP PARALLEL DO
       xloop : do  i=1,nx
       yloop : do  j=1,ny

       if(IVGTYP(I,J) > 0) then

        ICE(I,J)     = abs(landsea(I,J)-1)
        !CO2PP(I,J)  = (372.55+it*0.000203*3.)*1.e-06 * P_ML(I,J) !372.55 ppm (2000/01) -> 401 ppm (2015/12)
        !CO2PP(I,J)  = (337.+it*0.000203*3.)*1.e-06 * P_ML(I,J) !337 ppm (1980/01) -> 401 ppm (2015/12)
        !CO2PP(I,J)  = (280.+it*0.000203*3.)*1.e-06 * P_ML(I,J) !280 ppm (1948/01) -> 401 ppm (2015/12)
         CO2PP(I,J)  = 360.0 * 1.0e-06 * P_ML(I,J) !280 ppm (1948/01) -> 401 ppm (2015/12)

         O2PP(I,J)   = 0.209 * P_ML(I,J)    !partial pressure  o2 (pa)

         if(i == int(nx/2) .and. j == int(ny/2) ) &
         write(88,'(4i5,9f10.2)')iy,im,id,itime,cosz(I,J),swdn(I,J),lwdn(I,J),T_ML(I,J)&
                 ,U_ML(I,J),prcp(I,J)*dt,Q_ML(I,J)*1000,P_ML(I,J),CO2PP(I,J)/P_ML(I,J)*1.e6

         do iz=isnow(I,J)+1,0
           FICEOLD(I,iz,J) = snice(I,iz,J)/(snice(I,iz,J)+snliq(I,iz,J))
         end do

          !IF(I == 93 .and. J == 128) THEN
          ! write(*,*) 'IVGTYP(I,J)=',IVGTYP(I,J)
          ! write(*,*) 'swdn(I,J)=',swdn(I,J)
          ! write(*,*) 'lwdn(I,J)=',lwdn(I,J)
           !write(*,*) '=====SMH2O=',SMH2O(I,:,J)
           !write(*,*) '=====SMC=',SMC(I,:,J)
          !END IF

       ! write(*,*) "i,j,nsoil_var=",i,j,nsoil_VAR(i,j)

       !IMERG precipitation with no data
        IF(PCONV(I,J) < 0.0) THEN
          write(400,*) iy,im,id,itime,I,J,PCONV(I,J)*DT
          PCONV(I,J) = 0.0
        END IF

        CALL NOAHMP_SFLX (rad_cons,SLOPETYP(I,J)  ,SOILCOLOR(I,J), ISLTYP(I,J) , &
        I           ,J            ,LAT(I,J)    ,YEARLEN       ,JULIAN        ,COSZ(I,J)    , & ! IN : Time/Space
        DT          ,DX           ,DZ8W1D      ,nsoil_var(i,j),ZSOIL(1:nsoil_var(i,j))     ,NSNOW , & ! IN : config 
        FVEG(I,J,im),FVGMAX(I,J)  ,IVGTYP(I,J) ,ICE(I,J)      ,IST(I,J)      ,CROPTYPE(I,J), & ! IN : Veg/Soil
        SMCEQ(I,1:nsoil_var(i,j),J),TOPOSV(I,J)  ,                                           & ! IN : Veg/Soil
        T_ML(I,J)   ,P_ML(I,J)    ,PSFC(I,J)   ,U_ML(I,J)     ,V_ML(I,J)     ,Q_ML(I,J)    , & ! IN : Forcing
        QC(I,J)     ,SWDN(I,J)    ,LWDN(I,J)   ,                                            & ! IN : Forcing
        PCONV(I,J)  ,PNONC(I,J)   ,PSHCV(I,J)  ,PSNOW(I,J)    ,PGRPL(I,J)    ,PHAIL(I,J)   , & ! IN : Forcing
        TBOT(I,J)   ,CO2PP(I,J)   ,O2PP(I,J)   ,FOLN          ,FICEOLD(I,:,J),Z_ML         , & ! IN : Forcing
        ALBOLD(I,J) ,SWEO(I,J)    ,radius(I,:,J),                                            & ! IN/OUT : 
        STC(I,-nsnow+1:nsoil_var(i,j),J)  ,SMH2O(I,1:nsoil_var(i,j),J) ,SMC(I,1:nsoil_var(i,j),J)  , & !IN/OUT
        TAH(I,J)     ,EAH(I,J)      ,FWET(I,J)    , & ! IN/OUT : 
        CANLIQ(I,J) ,CANICE(I,J)  ,TV(I,J)     ,TG(I,J)       ,QSFC1D(I,J)   ,QSNOW(I,J)   , & ! IN/OUT : 
        ISNOW(I,J)  ,ZSNSO(I,-nsnow+1:nsoil_var(i,j),J)       ,SNDPTH(I,J) ,SWE(I,J)       , & ! IN/OUT :
        SNICE(I,-nsnow+1:0,J)  ,SNLIQ(I,-nsnow+1:0,J)         , & ! IN/OUT : 
        ZWT(I,J)    ,WA(I,J)      ,WT(I,J)     ,WSLAKE(I,J)   ,LFMASS(I,J)   ,RTMASS(I,J)  , & ! IN/OUT : 
        STMASS(I,J) ,WOOD(I,J)    ,STBLCP(I,J) ,FASTCP(I,J)   ,PLAI(I,J)     ,PSAI(I,J)    , & ! IN/OUT : 
        CM(I,J)     ,CH(I,J)      ,TAUSS(I,J)  ,                                             & ! IN/OUT : 
        GRAIN(I,J)  ,GDD(I,J)     ,PGS(I,J)    ,                                             & ! IN/OUT :
        SMCWTD(I,J) ,DEEPRECH(I,J),RECH(I,J)   ,                                             & ! IN/OUT :
        soc(I,J)    ,wdoc(I,J)    ,ddoc(I,J)   ,mic(I,J)      ,wenz(I,J)     ,denz(I,J)    , & ! IN/OUT (niu)
        mq(I,J)     ,kr(I,J)      ,froot(I,1:nsoil_var(i,j),J),ROOTMS(I,1:nsoil_var(i,j),J), & ! IN/OUT (niu)
        Z0WRF       ,                                                                        & ! OUT :
        FSA(I,J)    ,FSR(I,J)     ,FIRA(I,J)   ,FSH(I,J)      ,SSOIL(I,J)    ,FCEV(I,J)    , & ! OUT : 
        FGEV(I,J)   ,FCTR(I,J)    ,ECAN(I,J)   ,ETRAN(I,J)    ,edir(I,J)     ,TRAD(I,J)    , & ! OUT : 
        TGB(I,J)    ,TGV(I,J)     ,T2MV(I,J)   ,T2MB(I,J)     ,Q2MV(I,J)     ,Q2MB(I,J)    , & ! OUT : 
        RUNSF(I,J)  ,RUNSB(I,J)   ,APAR(I,J)   ,PSN(I,J)      ,SAV(I,J)      ,SAG(I,J)     , & ! OUT : 
        FSNO(I,J)   ,NEE(I,J)     ,GPP(I,J)    ,NPP(I,J)      ,FVEGMP(I,J)   ,SALB(I,J)    , & ! OUT : 
        QSNBOT(I,J) ,POND(I,J)    ,POND1(I,J)  ,POND2(I,J)    ,RSSUN(I,J)    ,RSSHA(I,J)   , & ! OUT : 
        ALBSND(I,J) ,ALBSNI(I,J)  , & !
        BGAP(I,J)   ,WGAP(I,J)    ,CHV(I,J)    ,CHB(I,J)      ,EMISSI(I,J)   ,               & ! OUT : 
        SHG(I,J)    ,SHC(I,J)     ,SHB(I,J)    ,EVG(I,J)      ,EVB(I,J)      ,GHV(I,J)     , & ! OUT :
        GHB(I,J)    ,IRG(I,J)     ,IRC(I,J)    ,IRB(I,J)      ,TR(I,J)       ,EVC(I,J)     , & ! OUT :
        CHLEAF(I,J) ,CHUC(I,J)    ,CHV2(I,J)   ,CHB2(I,J)     ,FPICE(I,J)    ,PAHV(I,J)    , & ! OUT :
        PAHG(I,J)   ,PAHB(I,J)    ,PAH(I,J)    ,                                             & ! OUT:
        qco2(I,J)   ,vmax(I,J)    ,km(I,J)     ,vmaxup(I,J)   ,kmup(I,J)     ,epslon(I,J)  , & ! out (niu)
        QIN(I,J)    ,ndvi(I,J)    ,swdown(I,J) ,qroot(I,1:nsoil_var(i,j),J)  ,sadr(I,1:nsoil_var(i,j),J)   , & !out (niu)
        qsubcan(I,J),qsubgrd(I,J) , &  !out
        VARSD       ,PSI(I,1:nsoil_var(i,j),J) ,ATM_BC(I,J)   ,ATMACT(I,J)   ,DTFINEM(I,J) , & ! mixed RE inout
        WCND(I,1:nsoil_var(i,j),J),HTOP(I,J)   ,RSINEX(I,J)   ) ! mixed RE

        endif         ! endif of land-points

      enddo yloop
      enddo xloop
      !$OMP END PARALLEL DO

        nn      = 0
        nndvig  = 0
        fastcpgm = 0.
        stblcpgm = 0.

        tvgm             = 0.
        tggm             = 0.
        etgm             = 0.
        runoffgm         = 0.
        smcgm(1:nsoil)   = 0.
        stcgm(1:nsoil)   = 0.
        radigm(-2:0)   = 0.

        !!$OMP PARALLEL DO
        do i=1,nx
        do j=1,ny
           if(IVGTYP(I,J) > 0) then

             tsno(I,isnow(I,J)+1:0,J) = stc(I,isnow(I,J)+1:0,J)
             flh(I,J)   = fcev(I,J) + fgev(I,J) + fctr(I,J)

             fsnoc(I,J)    = 0.
             if(tv(I,J).le.273.16) fsnoc(I,J)     = fwet(I,J)
             scf(I,J) = (1.-fvegMP(I,J)) * FSNO(I,J) +   &
                              fvegMP(I,J)  * fsnoc(I,J)

             soilc(I,J) = soc(I,J) + wdoc(I,J) + ddoc(I,J) &
                          + mic(I,J) + wenz(I,J) + denz(I,J)
             fastcpgm = fastcpgm + FASTCP(I,J)
             stblcpgm = stblcpgm + STBLCP(I,J)

             tvgm             = tvgm     + tv(I,J)
             tggm             = tggm     + tg(I,J)
             etgm             = etgm     + (etran(I,J)+edir(I,J)+ecan(I,J)) * 86400.
             runoffgm         = runoffgm + (runsb(I,J)+runsf(I,J))          * 86400.

            !write(500,*) I,J,runsb(i,j),runsf(i,j)
            !write(500,*) I,J,SMC(i,:,j),ISLTYP(I,J),IST(I,J)
            !write(500,*) I,J,STC(i,:,j),ISLTYP(I,J),IST(I,J)

             do iz = 1, nsoil_var(i,j)
               smcgm(iz)   = smcgm(iz) + smc(I,iz,J)
               stcgm(iz)   = stcgm(iz) + stc(I,iz,J)
             enddo
             do iz = -2,0
               radigm(iz)   = radigm(iz) + radius(I,iz,J)
             enddo

             if(ndvi(I,J) > 0.0) then
                nndvig  = nndvig + 1
                ndvigm  = ndvigm  + ndvi (I,J)
             end if

             nn      = nn + 1
           end if
        end do
        end do
        !!$OMP END PARALLEL DO

        fastcpgm = fastcpgm/nn
        stblcpgm = stblcpgm/nn
        if (nndvig > 0) then
           ndvigm  = ndvigm /nndvig
        end if

        tvgm             = tvgm    / nn
        tggm             = tggm    / nn
        etgm             = etgm    / nn
        runoffgm         = runoffgm    / nn

        do iz = 1, nsoil
          smcgm(iz)   = smcgm(iz) / nn
          stcgm(iz)   = stcgm(iz) / nn
        enddo

        do iz = -2,0
          radigm(iz)   = radigm(iz) / nn
        enddo

        write(401,'(2i5,3i3,2f10.1,10f8.2)')ispin,iy,im,id,ih,fastcpgm,&
               stblcpgm,ndvigm,radigm,smcgm(1),tvgm,tggm,etgm,runoffgm
!       write(401,'(2i5,3i3,6f6.2,8f8.2,2f6.2)')ispin,iy,im,id,ih,&
!             smcgm,stcgm,tvgm,tggm,etgm,runoffgm

!-------------------------------------------------------------------
! end of 1-d Noah processing
!-------------------------------------------------------------------

       !if(ispin == nspin) then
          !monthly output, one file per month (1 time layer)
          call nc_out(nsoil   ,nsnow   ,nx      ,ny      ,it      ,dt    , &
                      iy  ,im  ,id  ,DIR    ,EXP     ,lon   , &
                      lat     ,IVGTYP  ,imstep  ,nday    ,ND      , &
                      SNDPTH  ,SWE     ,tg      ,stc     ,smc     , &
                      SMH2O   ,prcp    ,runsf   ,runsb   ,ecan    , &
                      edir    ,etran   ,zwt     ,fsa     ,fira    , &
                      fsh     ,flh     ,SSOIL     ,apar    ,psn     , &
                      sav     ,sag     ,scf     ,PLAI    ,PSAI    , &
                      trad    ,nee     ,gpp     ,npp     ,t2m     , &
                      fvegMP  ,cm      ,ch      ,QIN     ,qco2    , &
                      soc     ,wdoc    ,ddoc    ,mic     ,wenz    , &
                      denz    ,vmax    ,km      ,vmaxup  ,kmup    , &
                      epslon  ,wa      ,canliq  ,canice  ,ndvi    , &
                      nndvi   ,mq      ,kr      ,rtmass  ,sadr    , &
                      qroot   ,psi     ,FSNO    ,salb    ,HTOP    , &
                      RSINEX  , &
                      T_ML    ,Q_ML    ,U_ML    ,V_ML    ,P_ML    , &
                      SNDPTHm ,SWEm    ,tgm     ,stcm    ,smcm    , &
                      SMH2OM  ,prcpm   ,runsfm  ,runsbm  ,ecanm   , &
                      edirm   ,etranm  ,zwtm    ,fsam    ,firam   , &
                      fshm    ,flhm    ,SSOILm  ,aparm   ,psnm    , &
                      savm    ,sagm    ,scfm    ,PLAIm   ,PSAIm   , &
                      tradm   ,neem    ,gppm    ,nppm    ,t2mm    , &
                      fvegm   ,cmm     ,chm     ,QINM    ,qco2m   , &
                      socm    ,wdocm   ,ddocm   ,micm    ,wenzm   , &
                      denzm   ,vmaxm   ,kmm     ,vmaxupm ,kmupm   , &
                      epslonm ,wam     ,canwatm ,ndvim   ,mqm     , &
                      krm     ,rtmassm ,sadrm   ,qrootm  ,qrootnm , &
                      T_MLm   ,Q_MLm   ,windm   ,P_MLm   ,psim    , &
                      FSNOm   ,salbm   ,HTOPM   ,RSINEXm )

         !daily output, one file per day

           call nc_out_daily(nx    ,ny     ,nsoil,it    ,dt    , &
                           iy ,im ,id,DIR   , &
                           EXP    ,lon    ,lat   ,IVGTYP, &
                           idstep ,ntimed ,  &
                           SNDPTH    ,SWE    ,scf       ,FSNO    , &
                           runsf    ,runsb    ,qsubcan   ,qsubgrd  , &
                           SMC      ,SMH2O    ,&
                           SNDPTHdm  ,SWEdm  ,scfdm     ,FSNOdm  , &
                           runsfdm  ,runsbdm  ,qsubcandm ,qsubgrddm, &
                           smcdm    ,SMH2OdM )

         !hrly output, one file per day (24 time layers)

     !   if(iy == 2100) then 
          call nc_out_hr(nsoil      ,nx    ,ny    ,it    ,dt  , &
                      iy,im ,id  ,DIR   ,EXP   ,lon , &
                      lat   ,IVGTYP ,idstep,nday  ,        &
                      SNDPTH ,SWE  ,tg    ,stc   ,smc   , &
                      SMH2O ,prcp   ,runsf ,runsb ,ecan  , &
                      edir  ,etran  ,zwt   ,fsa   ,fira  , &
                      fsh   ,flh    ,SSOIL   ,apar  ,psn   , &
                      sav   ,sag    ,FSNO ,PLAI  ,PSAI  , &
                      trad  ,nee    ,gpp   ,npp   ,t2m   , &
                      fveg  ,qco2   ,soc   , &
                      wdoc  ,ddoc   ,mic   ,wenz  ,denz  )
      !   end if
      ! endif

!update the time 

          call geth_newdate(newdate, olddate, nint(dt))
          year    = newdate(1:4)
          mdt     = newdate(5:10)
          minute  = newdate(11:12)
          olddate = newdate

       enddo hloop
       enddo dloop

       call write_ini(DIR,EXP    ,nx     ,ny      ,nsoil, nsoil_var   ,nsnow   , &
           iy   ,im  ,id-1   ,itime   ,lat   ,lon  ,IVGTYP, &
           ISLTYP ,SLOPETYP,CROPTYPE ,soilcolor, &
           smc    ,stc   ,SMH2O ,tsno    ,snice ,snliq , radius, &
           zsnso  ,isnow ,SNDPTH,SWE     ,canliq,canice, &
           tg     ,tv    ,wa    ,wt      ,zwt   ,lfmass, &
           rootms ,stmass,wood  ,soc     ,wdoc  ,ddoc  , &
           mic    ,wenz  ,denz  ,wslake  ,mq    ,kr    , &
           froot  ,rtmass,STBLCP,FASTCP  ,psi   ,wcnd  , &
           atm_bc ,HTOP  )

       enddo mloop
       enddo yrloop

       enddo ! spinup

      end program Noah_driver

! =====:=============================================================================================
! -----------------------------------------------------------------------------------------!
! Transfer esm_snicar parameters
! -----------------------------------------------------------------------------------------!

      SUBROUTINE read_rad(rad_cons)

      USE NOAHMP_TABLES
      USE MODULE_SF_NOAHMPLSM

      implicit none

      INTEGER, PARAMETER :: NRAD = 1451

      type (constants), intent(out) :: rad_cons

      call read_esm_snicar_parameters()

      rad_cons%ext_mie_bd(1:NRAD,1:5)    = ext_mie_bd_TABLE(1:NRAD,1:5)
      rad_cons%w_mie_bd  (1:NRAD,1:5)    = w_mie_bd_TABLE  (1:NRAD,1:5)
      rad_cons%g1_mie_bd (1:NRAD,1:5)    = g1_mie_bd_TABLE (1:NRAD,1:5)
      rad_cons%g2_mie_bd (1:NRAD,1:5)    = g2_mie_bd_TABLE (1:NRAD,1:5)
      rad_cons%g3_mie_bd (1:NRAD,1:5)    = g3_mie_bd_TABLE (1:NRAD,1:5)
      rad_cons%g4_mie_bd (1:NRAD,1:5)    = g4_mie_bd_TABLE (1:NRAD,1:5)
      rad_cons%ext_mie_lap_bd (1:7,1:5)  = ext_mie_lap_bd_TABLE(1:7,1:5)
      rad_cons%w_mie_lap_bd   (1:7,1:5)  = w_mie_lap_bd_TABLE  (1:7,1:5)
      rad_cons%g_mie_lap_bd   (1:7,1:5)  = g_mie_lap_bd_TABLE  (1:7,1:5)
      rad_cons%drdt0(1:8,1:31,1:11)      = drdt0_TABLE(1:8,1:31,1:11)
      rad_cons%tau  (1:8,1:31,1:11)      = tau_TABLE(1:8,1:31,1:11)
      rad_cons%kappa(1:8,1:31,1:11)      = kappa_TABLE(1:8,1:31,1:11)

      END SUBROUTINE read_rad
! ==================================================================================================

      SUBROUTINE LSMZEN (CALDAY, LOND, LATD, COSZ)

! cosine solar zenith angle from:
!    o day (1.x to 365.x), where x=0 (e.g. 213.0) denotes 00:00 at
!    greenwich
!    o latitude,  where SH = - and NH = +
!    o longitude, where WH = - and EH = +
! the solar declination must match that used in the atmospheric model.
! -----------------------------------------------------------------
! input
      REAL, INTENT(IN) :: CALDAY  !calendar day + fraction (1.xx -> 365.xx)
      REAL, INTENT(IN) :: LATD    !latitude  (degree): + = NH ( -90 -> 90)
      REAL, INTENT(IN) :: LOND    !longitude (degree): + = EH (-180 -> 180)

! output
      REAL,INTENT(OUT) :: COSZ    !cosine zenith angle

! local
      REAL :: DAYSPY              !days per year
      REAL :: PI                  !pi
      REAL :: THETA               !earth orbit seasonal angle in radians
      REAL :: DELTA               !solar declination angle  in radians
      REAL :: SIND                !sine   of declination
      REAL :: COSD                !cosine of declination
      REAL :: PHI                 !greenwich calendar day + longitude offset
      REAL :: LOCTIM              !local time (hour)
      REAL :: HRANG               !solar hour angle, 24 hour periodicity (radians)
      REAL :: LAT                 !latitude  (radians): + = NH
      REAL :: LON                 !longitude (radians): + = EH

      INTEGER MCSEC            !current seconds in day (0, ..., 86400)
! -----------------------------------------------------------------

      DAYSPY = 365.
      PI = 4.*ATAN(1.)

      LAT = LATD * PI/180.
      LON = LOND * PI/180

! solar declination: 

      THETA = (2.*PI*CALDAY)/DAYSPY
      DELTA = .006918 - .399912*COS(   THETA) + .070257*SIN(   THETA) &
                      - .006758*COS(2.*THETA) + .000907*SIN(2.*THETA) &
                      - .002697*COS(3.*THETA) + .001480*SIN(3.*THETA)
      SIND = SIN(DELTA)
      COSD = COS(DELTA)

! local time

         MCSEC = (CALDAY - INT(CALDAY)) * 86400.
         PHI = CALDAY + LON/(2.*PI)
         LOCTIM = (MCSEC + (PHI-CALDAY)*86400.) / 3600.

! hour angle

         HRANG = 360./24. * (LOCTIM-12.) * PI/180.

! cosine solar zenith angle. reset points with sun slightly below
! horizon
! to slightly above horizon, as discussed in notes.

         COSZ = SIN(LAT)*SIND + COS(LAT)*COSD*COS(HRANG)
         IF (COSZ .GE. -0.001 .AND. COSZ.LE. 0.) COSZ = 0.001

      END SUBROUTINE LSMZEN
! ==================================================================================================
      SUBROUTINE TRANSFER_MP_PARAMETERS(VEGTYPE,SOILTYPE,nsoil_var, &
                 SLOPETYPE,SOILCOLOR,CROPTYPE,parameters,ILOC,JLOC)

       USE NOAHMP_TABLES
       USE MODULE_SF_NOAHMPLSM

       implicit none

       INTEGER, INTENT(IN)    :: ILOC,JLOC
       INTEGER, INTENT(IN)    :: VEGTYPE
       INTEGER, INTENT(IN)    :: SOILTYPE
       INTEGER, INTENT(IN)    :: SLOPETYPE
       INTEGER, INTENT(IN)    :: SOILCOLOR
       INTEGER, INTENT(IN)    :: CROPTYPE
       INTEGER, INTENT(IN)    :: nsoil_var

      !type (noahmp_parameters), intent(inout) :: parameters
       type (noahmp_parameters), intent(out) :: parameters

       REAL    :: REFDK
       REAL    :: REFKDT
       REAL    :: FRZK
       REAL    :: FRZFACT
      !INTEGER, PARAMETER  :: NRAD = 1451

       parameters%ISWATER   =   ISWATER_TABLE
       parameters%ISBARREN  =  ISBARREN_TABLE
       parameters%ISICE     =     ISICE_TABLE
       parameters%ISCROP    =    ISCROP_TABLE
       parameters%EBLFOREST = EBLFOREST_TABLE

       parameters%URBAN_FLAG = .FALSE.
       IF( VEGTYPE == ISURBAN_TABLE                  .or. VEGTYPE == LOW_DENSITY_RESIDENTIAL_TABLE  .or. &
           VEGTYPE == HIGH_DENSITY_RESIDENTIAL_TABLE .or. VEGTYPE == HIGH_INTENSITY_INDUSTRIAL_TABLE ) THEN
          parameters%URBAN_FLAG = .TRUE.
       ENDIF

!------------------------------------------------------------------------------------------!
! Transfer veg parameters
!------------------------------------------------------------------------------------------!

       parameters%CH2OP  =  CH2OP_TABLE(VEGTYPE)       !maximum intercepted h2o per unit lai+sai (mm)
       parameters%DLEAF  =  DLEAF_TABLE(VEGTYPE)       !characteristic leaf dimension (m)
       parameters%Z0MVT  =  Z0MVT_TABLE(VEGTYPE)       !momentum roughness length (m)
       parameters%HVT    =    HVT_TABLE(VEGTYPE)       !top of canopy (m)
       parameters%HVB    =    HVB_TABLE(VEGTYPE)       !bottom of canopy (m)
       parameters%DEN    =    DEN_TABLE(VEGTYPE)       !tree density (no. of trunks per m2)
       parameters%RC     =     RC_TABLE(VEGTYPE)       !tree crown radius (m)
       parameters%MFSNO  =  MFSNO_TABLE(VEGTYPE)       !snowmelt m parameter ()
       parameters%SAIM   =   SAIM_TABLE(VEGTYPE,:)     !monthly stem area index, one-sided
       parameters%LAIM   =   LAIM_TABLE(VEGTYPE,:)     !monthly leaf area index, one-sided
       parameters%SLA    =    SLA_TABLE(VEGTYPE)       !single-side leaf area per Kg [m2/kg]
       parameters%DILEFC = DILEFC_TABLE(VEGTYPE)       !coeficient for leaf stress death [1/s]
       parameters%DILEFW = DILEFW_TABLE(VEGTYPE)       !coeficient for leaf stress death [1/s]
       parameters%FRAGR  =  FRAGR_TABLE(VEGTYPE)       !fraction of growth respiration  !original was 0.3 
       parameters%LTOVRC = LTOVRC_TABLE(VEGTYPE)       !leaf turnover [1/s]

       parameters%C3PSN  =  C3PSN_TABLE(VEGTYPE)       !photosynthetic pathway: 0. = c4, 1. = c3
       parameters%KC25   =   KC25_TABLE(VEGTYPE)       !co2 michaelis-menten constant at 25c (pa)
       parameters%AKC    =    AKC_TABLE(VEGTYPE)       !q10 for kc25
       parameters%KO25   =   KO25_TABLE(VEGTYPE)       !o2 michaelis-menten constant at 25c (pa)
       parameters%AKO    =    AKO_TABLE(VEGTYPE)       !q10 for ko25
       parameters%VCMX25 = VCMX25_TABLE(VEGTYPE)       !maximum rate of carboxylation at 25c (umol co2/m**2/s)
       parameters%AVCMX  =  AVCMX_TABLE(VEGTYPE)       !q10 for vcmx25
       parameters%BP     =     BP_TABLE(VEGTYPE)       !minimum leaf conductance (umol/m**2/s)
       parameters%MP     =     MP_TABLE(VEGTYPE)       !slope of conductance-to-photosynthesis relationship
       parameters%QE25   =   QE25_TABLE(VEGTYPE)       !quantum efficiency at 25c (umol co2 / umol photon)
       parameters%AQE    =    AQE_TABLE(VEGTYPE)       !q10 for qe25
       parameters%RMF25  =  RMF25_TABLE(VEGTYPE)       !leaf maintenance respiration at 25c (umol co2/m**2/s)
       parameters%RMS25  =  RMS25_TABLE(VEGTYPE)       !stem maintenance respiration at 25c (umol co2/kg bio/s)
       parameters%RMR25  =  RMR25_TABLE(VEGTYPE)       !root maintenance respiration at 25c (umol co2/kg bio/s)
       parameters%ARM    =    ARM_TABLE(VEGTYPE)       !q10 for maintenance respiration
       parameters%FOLNMX = FOLNMX_TABLE(VEGTYPE)       !foliage nitrogen concentration when f(n)=1 (%)
       parameters%TMIN   =   TMIN_TABLE(VEGTYPE)       !minimum temperature for photosynthesis (k)

       parameters%XL     =     XL_TABLE(VEGTYPE)       !leaf/stem orientation index
       parameters%RHOL   =   RHOL_TABLE(VEGTYPE,:)     !leaf reflectance: 1=vis, 2=nir
       parameters%RHOS   =   RHOS_TABLE(VEGTYPE,:)     !stem reflectance: 1=vis, 2=nir
       parameters%TAUL   =   TAUL_TABLE(VEGTYPE,:)     !leaf transmittance: 1=vis, 2=nir
       parameters%TAUS   =   TAUS_TABLE(VEGTYPE,:)     !stem transmittance: 1=vis, 2=nir

       parameters%MRP    =    MRP_TABLE(VEGTYPE)       !microbial respiration parameter (umol co2 /kg c/ s)
       parameters%CWPVT  =  CWPVT_TABLE(VEGTYPE)       !empirical canopy wind parameter

       parameters%WRRAT  =  WRRAT_TABLE(VEGTYPE)       !wood to non-wood ratio
       parameters%WDPOOL = WDPOOL_TABLE(VEGTYPE)       !wood pool (switch 1 or 0) depending on woody or not [-]
       parameters%TDLEF  =  TDLEF_TABLE(VEGTYPE)       !characteristic T for leaf freezing [K]

       parameters%NROOT  =  MIN(nsoil_var, NROOT_TABLE(VEGTYPE) )       !number of soil layers with root present

       parameters%RGL    =    RGL_TABLE(VEGTYPE)       !Parameter used in radiation stress function
       parameters%RSMIN  =     RS_TABLE(VEGTYPE)       !Minimum stomatal resistance [s m-1]
       parameters%HS     =     HS_TABLE(VEGTYPE)       !Parameter used in vapor pressure deficit function
       parameters%TOPT   =   TOPT_TABLE(VEGTYPE)       !Optimum transpiration air temperature [K]
       parameters%RSMAX  =  RSMAX_TABLE(VEGTYPE)       !Maximal stomatal resistance [s m-1]
       parameters%SRA    =    SRA_TABLE(VEGTYPE)       !specific root area [m2/kg]
       parameters%OMR    =    OMR_TABLE(VEGTYPE)       !root resistance
       parameters%MQX    =    MQX_TABLE(VEGTYPE)       !ratio of water to biomass
       parameters%RTOMAX = RTOMAX_TABLE(VEGTYPE)       !maximum root death rate
       parameters%RROOT  =  RROOT_TABLE(VEGTYPE)       !root radius (mm)
       parameters%SCEXP  =  SCEXP_TABLE(VEGTYPE)       !decay rate of the leaf death

!------------------------------------------------------------------------------------------!
! Transfer rad parameters
!------------------------------------------------------------------------------------------!

        parameters%ALBSAT    = ALBSAT_TABLE(SOILCOLOR,:)
        parameters%ALBDRY    = ALBDRY_TABLE(SOILCOLOR,:)
        parameters%ALBICE    = ALBICE_TABLE
        parameters%ALBLAK    = ALBLAK_TABLE
        parameters%OMEGAS    = OMEGAS_TABLE
        parameters%BETADS    = BETADS_TABLE
        parameters%BETAIS    = BETAIS_TABLE
        parameters%EG        = EG_TABLE

!------------------------------------------------------------------------------------------!
! Transfer crop parameters
!------------------------------------------------------------------------------------------!

       IF(CROPTYPE > 0) THEN
        parameters%PLTDAY    =    PLTDAY_TABLE(CROPTYPE)    ! Planting date
        parameters%HSDAY     =     HSDAY_TABLE(CROPTYPE)    ! Harvest date
        parameters%PLANTPOP  =  PLANTPOP_TABLE(CROPTYPE)    ! Plant density [per ha] - used?
        parameters%IRRI      =      IRRI_TABLE(CROPTYPE)    ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)
        parameters%GDDTBASE  =  GDDTBASE_TABLE(CROPTYPE)    ! Base temperature for GDD accumulation [C]
        parameters%GDDTCUT   =   GDDTCUT_TABLE(CROPTYPE)    ! Upper temperature for GDD accumulation [C]
        parameters%GDDS1     =     GDDS1_TABLE(CROPTYPE)    ! GDD from seeding to emergence
        parameters%GDDS2     =     GDDS2_TABLE(CROPTYPE)    ! GDD from seeding to initial vegetative 
        parameters%GDDS3     =     GDDS3_TABLE(CROPTYPE)    ! GDD from seeding to post vegetative 
        parameters%GDDS4     =     GDDS4_TABLE(CROPTYPE)    ! GDD from seeding to intial reproductive
        parameters%GDDS5     =     GDDS5_TABLE(CROPTYPE)    ! GDD from seeding to pysical maturity 
        parameters%C3C4      =      C3C4_TABLE(CROPTYPE)    ! photosynthetic pathway:  1. = c3 2. = c4
        parameters%AREF      =      AREF_TABLE(CROPTYPE)    ! reference maximum CO2 assimulation rate 
        parameters%PSNRF     =     PSNRF_TABLE(CROPTYPE)    ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
        parameters%I2PAR     =     I2PAR_TABLE(CROPTYPE)    ! Fraction of incoming solar radiation to photosynthetically active radiation
        parameters%TASSIM0   =   TASSIM0_TABLE(CROPTYPE)    ! Minimum temperature for CO2 assimulation [C]
        parameters%TASSIM1   =   TASSIM1_TABLE(CROPTYPE)    ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
        parameters%TASSIM2   =   TASSIM2_TABLE(CROPTYPE)    ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
        parameters%K         =         K_TABLE(CROPTYPE)    ! light extinction coefficient
        parameters%EPSI      =      EPSI_TABLE(CROPTYPE)    ! initial light use efficiency
        parameters%Q10MR     =     Q10MR_TABLE(CROPTYPE)    ! q10 for maintainance respiration
        parameters%FOLN_MX   =   FOLN_MX_TABLE(CROPTYPE)    ! foliage nitrogen concentration when f(n)=1 (%)
        parameters%LEFREEZ   =   LEFREEZ_TABLE(CROPTYPE)    ! characteristic T for leaf freezing [K]
        parameters%DILE_FC   =   DILE_FC_TABLE(CROPTYPE,:)  ! coeficient for temperature leaf stress death [1/s]
        parameters%DILE_FW   =   DILE_FW_TABLE(CROPTYPE,:)  ! coeficient for water leaf stress death [1/s]
        parameters%FRA_GR    =    FRA_GR_TABLE(CROPTYPE)    ! fraction of growth respiration
        parameters%LF_OVRC   =   LF_OVRC_TABLE(CROPTYPE,:)  ! fraction of leaf turnover  [1/s]
        parameters%ST_OVRC   =   ST_OVRC_TABLE(CROPTYPE,:)  ! fraction of stem turnover  [1/s]
        parameters%RT_OVRC   =   RT_OVRC_TABLE(CROPTYPE,:)  ! fraction of root tunrover  [1/s]
        parameters%LFMR25    =    LFMR25_TABLE(CROPTYPE)    ! leaf maintenance respiration at 25C [umol CO2/m**2  /s]
        parameters%STMR25    =    STMR25_TABLE(CROPTYPE)    ! stem maintenance respiration at 25C [umol CO2/kg bio/s]
        parameters%RTMR25    =    RTMR25_TABLE(CROPTYPE)    ! root maintenance respiration at 25C [umol CO2/kg bio/s]
        parameters%GRAINMR25 = GRAINMR25_TABLE(CROPTYPE)    ! grain maintenance respiration at 25C [umol CO2/kg bio/s]
        parameters%LFPT      =      LFPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to leaf
        parameters%STPT      =      STPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to stem
        parameters%RTPT      =      RTPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to root
        parameters%GRAINPT   =   GRAINPT_TABLE(CROPTYPE,:)  ! fraction of carbohydrate flux to grain
        parameters%BIO2LAI   =   BIO2LAI_TABLE(CROPTYPE)    ! leaf are per living leaf biomass [m^2/kg]
       END IF

!------------------------------------------------------------------------------------------!
! Transfer global parameters
!------------------------------------------------------------------------------------------!

        parameters%CO2        =         CO2_TABLE
        parameters%O2         =          O2_TABLE
        parameters%TIMEAN     =      TIMEAN_TABLE
        parameters%FSATMX     =      FSATMX_TABLE
        parameters%Z0SNO      =       Z0SNO_TABLE
        parameters%SSI        =         SSI_TABLE
        parameters%SWEMX      =       SWEMX_TABLE
        parameters%RSURF_SNOW =  RSURF_SNOW_TABLE

! ----------------------------------------------------------------------
!  Transfer soil parameters
! ----------------------------------------------------------------------

         parameters%BEXP   = BEXP_TABLE   (SOILTYPE)
         parameters%DKSAT  = DKSAT_TABLE  (SOILTYPE)
         parameters%DWSAT  = DWSAT_TABLE  (SOILTYPE)
         parameters%F1     = F1_TABLE     (SOILTYPE)
         parameters%PSISAT = PSISAT_TABLE (SOILTYPE)
         parameters%QUARTZ = QUARTZ_TABLE (SOILTYPE)
         parameters%SMCDRY = SMCDRY_TABLE (SOILTYPE)
         parameters%SMCMAX = SMCMAX_TABLE (SOILTYPE)
         parameters%SMCREF = SMCREF_TABLE (SOILTYPE)
         parameters%SMCWLT = SMCWLT_TABLE (SOILTYPE)
         parameters%REFDK  = REFDK_TABLE
         parameters%REFKDT = REFKDT_TABLE

         parameters%SMCR   = SMCR_TABLE   (SOILTYPE)
         parameters%VGN    = VGN_TABLE    (SOILTYPE)
         parameters%VGPSAT = VGPSAT_TABLE (SOILTYPE)

        !IF(ILOC == 1 .and. JLOC == 191) THEN
        !write(*,*) 'in TRANSFER======================'
        !write(*,*) "SOILTYP = ",SOILTYPE
        !write(*,*) "NSOIL_VAR = ",NSOIL_VAR
        !write(*,*) "SMCMAX_TABLE (SOILTYPE) = ",SMCMAX_TABLE (SOILTYPE)
        !write(*,*) 'SMCMAX,VGN,VGALPHA,SMCR',parameters%SMCMAX(1),parameters%VGN(1), &
        !             parameters%VGPSAT(1),parameters%SMCR(1)
        !END IF

! ----------------------------------------------------------------------
! Transfer GENPARM parameters
! ----------------------------------------------------------------------
         parameters%CSOIL  = CSOIL_TABLE
         parameters%ZBOT   = ZBOT_TABLE
         parameters%CZIL   = CZIL_TABLE

         FRZK   = FRZK_TABLE
         parameters%KDT    = parameters%REFKDT * parameters%DKSAT(1) / parameters%REFDK
         parameters%SLOPE  = SLOPE_TABLE(SLOPETYPE)

         IF(parameters%URBAN_FLAG)THEN  ! Hardcoding some urban parameters for soil
            parameters%SMCMAX = 0.45
            parameters%SMCREF = 0.42
            parameters%SMCWLT = 0.40
            parameters%SMCDRY = 0.40
            parameters%CSOIL  = 3.E6

            parameters%SMCR   = 0.06
            parameters%VGN    = 1.30
            parameters%VGPSAT = 0.50
         ENDIF
! adjust FRZK parameter to actual soil type: FRZK * FRZFACT

         IF(SOILTYPE /= 14) then
           FRZFACT = (parameters%SMCMAX(1) / parameters%SMCREF(1)) * (0.412 / 0.468)
           parameters%FRZX = FRZK * FRZFACT
         END IF

      END SUBROUTINE TRANSFER_MP_PARAMETERS

! ==================================================================================================
! ----------------------------------------------------------------------
    SUBROUTINE GROUNDWATER_INIT (   &
            &            NSOIL , ZSOIL , DZS, ISLTYP, IVGTYP, WTDDT , &
            &            FDEPTH, TOPO, RIVERBED, EQWTD, RIVERCOND, PEXP , AREA ,WTD ,  &
            &            SMOIS,SH2O, SMOISEQ, SMCWTDXY, DEEPRECHXY, RECHXY ,  &
            &            QSLATXY, QRFSXY, QSPRINGSXY,                  &
            &            rechclim  ,                                   &
            &            ids,ide, jds,jde, kds,kde,                    &
            &            ims,ime, jms,jme, kms,kme,                    &
            &            its,ite, jts,jte, kts,kte                     )


  USE NOAHMP_TABLES, ONLY : BEXP_TABLE,SMCMAX_TABLE,PSISAT_TABLE,SMCWLT_TABLE,DWSAT_TABLE,DKSAT_TABLE, &
                                ISURBAN_TABLE, ISICE_TABLE ,ISWATER_TABLE
  USE module_sf_noahmp_groundwater, ONLY : LATERALFLOW

! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------

    INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           its,ite, jts,jte, kts,kte
    INTEGER, INTENT(IN)                              :: NSOIL
    REAL,   INTENT(IN)                               ::     WTDDT
    REAL,    INTENT(IN), DIMENSION(1:NSOIL)          :: ZSOIL,DZS
    INTEGER, INTENT(IN), DIMENSION(ims:ime, jms:jme) :: ISLTYP, IVGTYP
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: FDEPTH, TOPO , AREA
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: rechclim
    REAL,    INTENT(OUT), DIMENSION(ims:ime, jms:jme) :: RIVERCOND
    REAL,    INTENT(INOUT), DIMENSION(ims:ime, jms:jme) :: WTD, RIVERBED, EQWTD, PEXP
    REAL,     DIMENSION( ims:ime , 1:nsoil, jms:jme ), &
         &    INTENT(INOUT)   ::                          SMOIS, &
         &                                                 SH2O, &
         &                                                 SMOISEQ
    REAL,    INTENT(INOUT), DIMENSION(ims:ime, jms:jme) ::  &
                                                           SMCWTDXY, &
                                                           DEEPRECHXY, &
                                                           RECHXY, &
                                                           QSLATXY, &
                                                           QRFSXY, &
                                                           QSPRINGSXY
! local
    INTEGER  :: I,J,K,ITER,itf,jtf, NITER, NCOUNT
    REAL :: BEXP,SMCMAX,PSISAT,SMCWLT,DWSAT,DKSAT
    REAL :: FRLIQ,SMCEQDEEP
    REAL :: DELTAT,RCOND,TOTWATER
    REAL :: AA,BBB,CC,DD,DX,FUNC,DFUNC,DDZ,EXPON,SMC,FLUX
    REAL, DIMENSION(1:NSOIL) :: SMCEQ
    REAL,      DIMENSION( ims:ime, jms:jme )    :: QLAT, QRF
    INTEGER,   DIMENSION( ims:ime, jms:jme )    :: LANDMASK !-1 for water (ice or no ice) and glacial areas, 1 for land where the LSM does its soil moisture calculations


       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)


    WHERE(IVGTYP.NE.ISWATER_TABLE.AND.IVGTYP.NE.ISICE_TABLE)
         LANDMASK=1
    ELSEWHERE
         LANDMASK=-1
    ENDWHERE

    PEXP = 1.0

    DELTAT=365.*24*3600. !1 year

!readjust the raw aggregated water table from hires, so that it is
!better compatible with topography

 DO NITER=1,500

    NCOUNT=0

!Calculate lateral flow

    QLAT = 0.
    CALL LATERALFLOW(ISLTYP,EQWTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA    &
                        ,ids,ide,jds,jde,kds,kde                      &
                        ,ims,ime,jms,jme,kms,kme                      &
                        ,its,ite,jts,jte,kts,kte                      )

    DO J=jts,jtf
       DO I=its,itf
          IF(LANDMASK(I,J).GT.0)THEN
            IF(QLAT(i,j).GT.1.e-2)THEN
                 NCOUNT=NCOUNT+1
                 EQWTD(i,j)=min(EQWTD(i,j)+0.25,0.)
            ENDIF
          ENDIF
        ENDDO
     ENDDO

   IF(NCOUNT.EQ.0)EXIT

 ENDDO

!after adjusting, where qlat > 1cm/year now wtd is at the surface.
!it may still happen that qlat + rech > 0 and eqwtd-rbed <0. There the wtd can
!rise to the surface (poor drainage) but the et will then increase.


!now, calculate rcond:

    DO J=jts,jtf
       DO I=its,itf

        DDZ = EQWTD(I,J)- ( RIVERBED(I,J)-TOPO(I,J) )
!dont allow riverbed above water table
        IF(DDZ.LT.0.)then
               RIVERBED(I,J)=TOPO(I,J)+EQWTD(I,J)
               DDZ=0.
        ENDIF


        TOTWATER = AREA(I,J)*(QLAT(I,J)+RECHCLIM(I,J)*0.001)/DELTAT

        IF (TOTWATER.GT.0) THEN
              RIVERCOND(I,J) = TOTWATER / MAX(DDZ,0.05)
        ELSE
              RIVERCOND(I,J)=0.01
!and make riverbed  equal to eqwtd, otherwise qrf might be too big...
              RIVERBED(I,J)=TOPO(I,J)+EQWTD(I,J)
        ENDIF


       ENDDO
    ENDDO

!make riverbed to be height down from the surface instead of above sea
!level

    RIVERBED = min( RIVERBED-TOPO, 0.)

!now inititalize wtd

    WTD = EQWTD


!now recompute lateral flow and flow to rivers to initialize deep soil
!moisture

    DELTAT = WTDDT * 60. !timestep in seconds for this calculation


!recalculate lateral flow

    QLAT = 0.
    CALL LATERALFLOW(ISLTYP,WTD,QLAT,FDEPTH,TOPO,LANDMASK,DELTAT,AREA &
                        ,ids,ide,jds,jde,kds,kde                      &
                        ,ims,ime,jms,jme,kms,kme                      &
                        ,its,ite,jts,jte,kts,kte                      )

!compute flux from grounwater to rivers in the cell

    DO J=jts,jtf
       DO I=its,itf
          IF(LANDMASK(I,J).GT.0)THEN
             IF(WTD(I,J) .GT. RIVERBED(I,J) .AND.  EQWTD(I,J) .GT. RIVERBED(I,J)) THEN
               RCOND = RIVERCOND(I,J) * EXP(PEXP(I,J)*(WTD(I,J)-EQWTD(I,J)))
             ELSE
               RCOND = RIVERCOND(I,J)
             ENDIF
             QRF(I,J) = RCOND * (WTD(I,J)-RIVERBED(I,J)) * DELTAT/AREA(I,J)
!for now, dont allow it to go from river to groundwater
             QRF(I,J) = MAX(QRF(I,J),0.)
          ELSE
             QRF(I,J) = 0.
          ENDIF
       ENDDO
    ENDDO

!now compute eq. soil moisture, change soil moisture to be compatible !with the water table and compute deep soil moisture

       DO J = jts,jtf
          DO I = its,itf
             BEXP   =   BEXP_TABLE(ISLTYP(I,J))
             SMCMAX = SMCMAX_TABLE(ISLTYP(I,J))
             SMCWLT = SMCWLT_TABLE(ISLTYP(I,J))
             IF(IVGTYP(I,J)==ISURBAN_TABLE)THEN
                 SMCMAX = 0.45
                 SMCWLT = 0.40
             ENDIF
             DWSAT  =   DWSAT_TABLE(ISLTYP(I,J))
             DKSAT  =   DKSAT_TABLE(ISLTYP(I,J))
             PSISAT = -PSISAT_TABLE(ISLTYP(I,J))
           IF ( ( BEXP > 0.0 ) .AND. ( smcmax > 0.0 ) .AND. ( -psisat > 0.0 ) ) THEN
             !initialize equilibrium soil moisture for water table
             !diagnostic
                    CALL EQSMOISTURE(NSOIL ,  ZSOIL , SMCMAX , SMCWLT , DWSAT, DKSAT  ,BEXP  , & !in
                                     SMCEQ                          ) !out

             SMOISEQ (I,1:NSOIL,J) = SMCEQ (1:NSOIL)


              !make sure that below the water table the layers are
              !saturated and initialize the deep soil moisture
             IF(WTD(I,J) < ZSOIL(NSOIL)-DZS(NSOIL)) THEN

!initialize deep soil moisture so that the flux compensates qlat+qrf
!use Newton-Raphson method to find soil moisture

                         EXPON = 2. * BEXP + 3.
                         DDZ = ZSOIL(NSOIL) - WTD(I,J)
                         CC = PSISAT/DDZ
                         FLUX = (QLAT(I,J)-QRF(I,J))/DELTAT

                         SMC = 0.5 * SMCMAX

                         DO ITER = 1, 100
                           DD = (SMC+SMCMAX)/(2.*SMCMAX)
                           AA = -DKSAT * DD  ** EXPON
                           BBB = CC * ( (SMCMAX/SMC)**BEXP - 1. ) + 1.
                           FUNC =  AA * BBB - FLUX
                           DFUNC = -DKSAT * (EXPON/(2.*SMCMAX)) * DD ** (EXPON - 1.) * BBB &
                                   + AA * CC * (-BEXP) * SMCMAX ** BEXP * SMC ** (-BEXP-1.)

                           DX = FUNC/DFUNC
                           SMC = SMC - DX
                           IF ( ABS (DX) < 1.E-6)EXIT
                         ENDDO

                  SMCWTDXY(I,J) = MAX(SMC,1.E-4)

             ELSEIF(WTD(I,J) < ZSOIL(NSOIL))THEN
                  SMCEQDEEP = SMCMAX * ( PSISAT / ( PSISAT - DZS(NSOIL) ) ) ** (1./BEXP)
!                  SMCEQDEEP = MAX(SMCEQDEEP,SMCWLT)
                  SMCEQDEEP = MAX(SMCEQDEEP,1.E-4)
                  SMCWTDXY(I,J) = SMCMAX * ( WTD(I,J) - (ZSOIL(NSOIL)-DZS(NSOIL))) + &
                                  SMCEQDEEP * (ZSOIL(NSOIL) - WTD(I,J))

             ELSE !water table within the resolved layers
                  SMCWTDXY(I,J) = SMCMAX
                  DO K=NSOIL,2,-1
                     IF(WTD(I,J) .GE. ZSOIL(K-1))THEN
                          FRLIQ = SH2O(I,K,J) / SMOIS(I,K,J)
                          SMOIS(I,K,J) = SMCMAX
                          SH2O(I,K,J) = SMCMAX * FRLIQ
                     ELSE
                          IF(SMOIS(I,K,J).LT.SMCEQ(K))THEN
                              WTD(I,J) = ZSOIL(K)
                          ELSE
                              WTD(I,J) = ( SMOIS(I,K,J)*DZS(K) - SMCEQ(K)*ZSOIL(K-1) + SMCMAX*ZSOIL(K) ) / &
                                         (SMCMAX - SMCEQ(K))
                          ENDIF
                          EXIT
                     ENDIF
                  ENDDO
             ENDIF
            ELSE
              SMOISEQ (I,1:NSOIL,J) = SMCMAX
              SMCWTDXY(I,J) = SMCMAX
              WTD(I,J) = 0.
            ENDIF

!zero out some arrays

             DEEPRECHXY(I,J) = 0.
             RECHXY(I,J) = 0.
             QSLATXY(I,J) = 0.
             QRFSXY(I,J) = 0.
             QSPRINGSXY(I,J) = 0.

          ENDDO
       ENDDO

    END  SUBROUTINE GROUNDWATER_INIT
! ==================================================================================================
! ----------------------------------------------------------------------
  SUBROUTINE EQSMOISTURE(NSOIL  ,  ZSOIL , SMCMAX , SMCWLT, DWSAT , DKSAT ,BEXP , & !in
                         SMCEQ                          )  !out
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  INTEGER,                         INTENT(IN) :: NSOIL !no. of soil layers
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL !depth of soil layer-bottom [m]
  REAL,                            INTENT(IN) :: SMCMAX , SMCWLT, BEXP , DWSAT, DKSAT
!output
  REAL,  DIMENSION(      1:NSOIL), INTENT(OUT) :: SMCEQ  !equilibrium soil water  content [m3/m3]
!local
  INTEGER                                     :: K , ITER
  REAL                                        :: DDZ , SMC, FUNC, DFUNC , AA, BB , EXPON, DX

!gmmcompute equilibrium soil moisture content for the layer when wtd=zsoil(k)


   DO K=1,NSOIL

            IF ( K == 1 )THEN
                DDZ = -ZSOIL(K+1) * 0.5
            ELSEIF ( K < NSOIL ) THEN
                DDZ = ( ZSOIL(K-1) - ZSOIL(K+1) ) * 0.5
            ELSE
                DDZ = ZSOIL(K-1) - ZSOIL(K)
            ENDIF

!use Newton-Raphson method to find eq soil moisture

            EXPON = BEXP +1.
            AA = DWSAT/DDZ
            BB = DKSAT / SMCMAX ** EXPON

            SMC = 0.5 * SMCMAX

         DO ITER = 1, 100
            FUNC = (SMC - SMCMAX) * AA +  BB * SMC ** EXPON
            DFUNC = AA + BB * EXPON * SMC ** BEXP

            DX = FUNC/DFUNC
            SMC = SMC - DX
            IF ( ABS (DX) < 1.E-6)EXIT
         ENDDO

!             SMCEQ(K) = MIN(MAX(SMC,SMCWLT),SMCMAX*0.99)
             SMCEQ(K) = MIN(MAX(SMC,1.E-4),SMCMAX*0.99)
   ENDDO

END  SUBROUTINE EQSMOISTURE
!
!------------------------------------------------------------------------------------------
      SUBROUTINE noahmp_urban(sf_urban_physics,   NSOIL,     IVGTYP,  ITIMESTEP,            & ! IN : Model configuration 
                                 DT,     COSZ_URB2D,     XLAT_URB2D,                        & ! IN : Time/Space-related
                                T3D,           QV3D,          U_PHY,      V_PHY,   SWDOWN,  & ! IN : Forcing
                                GLW,          P8W3D,         RAINBL,       DZ8W,      ZNT,  & ! IN : Forcing
                                TSK,            HFX,            QFX,         LH,   GRDFLX,  & ! IN/OUT : LSM 
                             ALBEDO,          EMISS,           QSFC,                        & ! IN/OUT : LSM 
                            ids,ide,        jds,jde,        kds,kde,                        &
                            ims,ime,        jms,jme,        kms,kme,                        &
                            its,ite,        jts,jte,        kts,kte,                        &
                         cmr_sfcdif,     chr_sfcdif,     cmc_sfcdif,                        &
                         chc_sfcdif,    cmgr_sfcdif,    chgr_sfcdif,                        &
                           tr_urb2d,       tb_urb2d,       tg_urb2d,                        & !H urban
                           tc_urb2d,       qc_urb2d,       uc_urb2d,                        & !H urban
                         xxxr_urb2d,     xxxb_urb2d,     xxxg_urb2d, xxxc_urb2d,            & !H urban
                          trl_urb3d,      tbl_urb3d,      tgl_urb3d,                        & !H urban
                           sh_urb2d,       lh_urb2d,        g_urb2d,   rn_urb2d,  ts_urb2d, & !H urban
                         psim_urb2d,     psih_urb2d,      u10_urb2d,  v10_urb2d,            & !O urban
                       GZ1OZ0_urb2d,     AKMS_URB2D,                                        & !O urban
                          th2_urb2d,       q2_urb2d,      ust_urb2d,                        & !O urban
                         declin_urb,      omg_urb2d,                                        & !I urban
                    num_roof_layers,num_wall_layers,num_road_layers,                        & !I urban
                                dzr,            dzb,            dzg,                        & !I urban
                         cmcr_urb2d,      tgr_urb2d,     tgrl_urb3d,  smr_urb3d,            & !H urban
                        drelr_urb2d,    drelb_urb2d,    drelg_urb2d,                        & !H urban
                      flxhumr_urb2d,  flxhumb_urb2d,  flxhumg_urb2d,                        & !H urban
                             julian,          julyr,                                        & !H urban
                          frc_urb2d,    utype_urb2d,                                        & !I urban
                                chs,           chs2,           cqs2,                        & !H
                   num_urban_layers,                                                        & !I multi-layer urban
                       num_urban_hi,                                                        & !I multi-layer urban
                          trb_urb4d,      tw1_urb4d,      tw2_urb4d,  tgb_urb4d,            & !H multi-layer urban
                         tlev_urb3d,     qlev_urb3d,                                        & !H multi-layer urban
                       tw1lev_urb3d,   tw2lev_urb3d,                                        & !H multi-layer urban
                        tglev_urb3d,    tflev_urb3d,                                        & !H multi-layer urban
                        sf_ac_urb3d,    lf_ac_urb3d,    cm_ac_urb3d,                        & !H multi-layer urban
                       sfvent_urb3d,   lfvent_urb3d,                                        & !H multi-layer urban
                       sfwin1_urb3d,   sfwin2_urb3d,                                        & !H multi-layer urban
                         sfw1_urb3d,     sfw2_urb3d,      sfr_urb3d,  sfg_urb3d,            & !H multi-layer urban
                           lp_urb2d,       hi_urb2d,       lb_urb2d,  hgt_urb2d,            & !H multi-layer urban
                           mh_urb2d,     stdh_urb2d,       lf_urb2d,                        & !SLUCM
                             th_phy,            rho,          p_phy,        ust,            & !I multi-layer urban
                                gmt,         julday,          xlong,       xlat,            & !I multi-layer urban
                            a_u_bep,        a_v_bep,        a_t_bep,    a_q_bep,            & !O multi-layer urban
                            a_e_bep,        b_u_bep,        b_v_bep,                        & !O multi-layer urban
                            b_t_bep,        b_q_bep,        b_e_bep,    dlg_bep,            & !O multi-layer urban
                           dl_u_bep,         sf_bep,         vl_bep                         & !O multi-layer urban
                 )

      USE module_sf_urban,    only: urban
      USE module_sf_bep,      only: bep
      USE module_sf_bep_bem,  only: bep_bem
!niu      USE module_ra_gfdleta,  only: cal_mon_day
      USE NOAHMP_TABLES, ONLY: ISURBAN_TABLE
!niu      USE module_model_constants, only: KARMAN, CP, xlv
!----------------------------------------------------------------
      IMPLICIT NONE
!----------------------------------------------------------------

      INTEGER,                                         INTENT(IN   ) :: sf_urban_physics   ! urban physics option
      INTEGER,                                         INTENT(IN   ) :: NSOIL     ! number of soil layers
      INTEGER, DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: IVGTYP    ! vegetation type
      INTEGER,                                         INTENT(IN   ) :: ITIMESTEP ! timestep number
      REAL,                                            INTENT(IN   ) :: DT        ! timestep [s]
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: COSZ_URB2D
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: XLAT_URB2D
      REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: T3D       ! 3D atmospheric temperature valid at mid-levels [K]
      REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: QV3D      ! 3D water vapor mixing ratio [kg/kg_dry]
      REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: U_PHY     ! 3D U wind component [m/s]
      REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: V_PHY     ! 3D V wind component [m/s]
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: SWDOWN    ! solar down at surface [W m-2]
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: GLW       ! longwave down at surface [W m-2]
      REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: P8W3D     ! 3D pressure, valid at interface [Pa]
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: RAINBL    ! total input precipitation [mm]
      REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: DZ8W      ! thickness of atmo layers [m]
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) :: ZNT       ! combined z0 sent to coupled model
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) :: TSK       ! surface radiative temperature [K]
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) :: HFX       ! sensible heat flux [W m-2]
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) :: QFX       ! latent heat flux [kg s-1 m-2]
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) :: LH        ! latent heat flux [W m-2]
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) :: GRDFLX    ! ground/snow heat flux [W m-2]
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) :: ALBEDO    ! total grid albedo []
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) :: EMISS     ! surface bulk emissivity
      REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(INOUT) :: QSFC      ! bulk surface mixing ratio

      INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &  ! d -> domain
           &                           ims,ime, jms,jme, kms,kme,  &  ! m -> memory
           &                           its,ite, jts,jte, kts,kte      ! t -> tile
! input variables surface_driver --> lsm

     INTEGER,                                                INTENT(IN) :: num_roof_layers
     INTEGER,                                                INTENT(IN) :: num_wall_layers
     INTEGER,                                                INTENT(IN) :: num_road_layers

     INTEGER,        DIMENSION( ims:ime, jms:jme ),          INTENT(IN) :: UTYPE_URB2D
     REAL,           DIMENSION( ims:ime, jms:jme ),          INTENT(IN) :: FRC_URB2D

     REAL, OPTIONAL, DIMENSION(1:num_roof_layers),           INTENT(IN) :: DZR
     REAL, OPTIONAL, DIMENSION(1:num_wall_layers),           INTENT(IN) :: DZB
     REAL, OPTIONAL, DIMENSION(1:num_road_layers),           INTENT(IN) :: DZG
     REAL, OPTIONAL,                                         INTENT(IN) :: DECLIN_URB
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),          INTENT(IN) :: OMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: TH_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: P_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: RHO

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),          INTENT(INOUT) :: UST
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),          INTENT(INOUT) :: CHS, CHS2, CQS2

     INTEGER,  INTENT(IN   )   ::  julian, julyr                  !urban

     REAL    , PARAMETER :: cp           = 1004.6   ! niu
     REAL    , PARAMETER :: KARMAN       = 0.4      ! niu 
     REAL    , PARAMETER :: xlv          = 2.5E6    ! niu

! local variables lsm --> urban
     INTEGER :: UTYPE_URB ! urban type [urban=1, suburban=2, rural=3]
     REAL    :: TA_URB       ! potential temp at 1st atmospheric level [K]
     REAL    :: QA_URB       ! mixing ratio at 1st atmospheric level  [kg/kg]
     REAL    :: UA_URB       ! wind speed at 1st atmospheric level    [m/s]
     REAL    :: U1_URB       ! u at 1st atmospheric level             [m/s]
     REAL    :: V1_URB       ! v at 1st atmospheric level             [m/s]
     REAL    :: SSG_URB      ! downward total short wave radiation    [W/m/m]
     REAL    :: LLG_URB      ! downward long wave radiation           [W/m/m]
     REAL    :: RAIN_URB     ! precipitation                          [mm/h]
     REAL    :: RHOO_URB     ! air density                            [kg/m^3]
     REAL    :: ZA_URB       ! first atmospheric level                [m]
     REAL    :: DELT_URB     ! time step                              [s]
     REAL    :: SSGD_URB     ! downward direct short wave radiation   [W/m/m]
     REAL    :: SSGQ_URB     ! downward diffuse short wave radiation  [W/m/m]
     REAL    :: XLAT_URB     ! latitude                               [deg]
     REAL    :: COSZ_URB     ! cosz
     REAL    :: OMG_URB      ! hour angle
     REAL    :: ZNT_URB      ! roughness length                       [m]
     REAL    :: TR_URB
     REAL    :: TB_URB
     REAL    :: TG_URB
     REAL    :: TC_URB
     REAL    :: QC_URB
     REAL    :: UC_URB
     REAL    :: XXXR_URB
     REAL    :: XXXB_URB
     REAL    :: XXXG_URB
     REAL    :: XXXC_URB
     REAL, DIMENSION(1:num_roof_layers) :: TRL_URB  ! roof layer temp [K]
     REAL, DIMENSION(1:num_wall_layers) :: TBL_URB  ! wall layer temp [K]
     REAL, DIMENSION(1:num_road_layers) :: TGL_URB  ! road layer temp [K]
     LOGICAL  :: LSOLAR_URB

!===hydrological variable for single layer UCM===

     INTEGER :: jmonth, jday
     REAL    :: DRELR_URB
     REAL    :: DRELB_URB
     REAL    :: DRELG_URB
     REAL    :: FLXHUMR_URB
     REAL    :: FLXHUMB_URB
     REAL    :: FLXHUMG_URB
     REAL    :: CMCR_URB
     REAL    :: TGR_URB

     REAL, DIMENSION(1:num_roof_layers) :: SMR_URB  ! green roof layer moisture
     REAL, DIMENSION(1:num_roof_layers) :: TGRL_URB ! green roof layer temp [K]

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: DRELR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: DRELB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: DRELG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: FLXHUMR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: FLXHUMB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: FLXHUMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: CMCR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                    INTENT(INOUT) :: TGR_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TGRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: SMR_URB3D


! state variable surface_driver <--> lsm <--> urban

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_wall_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_road_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D

! output variable lsm --> surface_driver

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIM_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: GZ1OZ0_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: U10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: V10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: TH2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: Q2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: AKMS_URB2D
     REAL,           DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: UST_URB2D


! output variables urban --> lsm

     REAL :: TS_URB           ! surface radiative temperature    [K]
     REAL :: QS_URB           ! surface humidity                 [-]
     REAL :: SH_URB           ! sensible heat flux               [W/m/m]
     REAL :: LH_URB           ! latent heat flux                 [W/m/m]
     REAL :: LH_KINEMATIC_URB ! latent heat flux, kinetic  [kg/m/m/s]
     REAL :: SW_URB           ! upward short wave radiation flux [W/m/m]
     REAL :: ALB_URB          ! time-varying albedo [fraction]
     REAL :: LW_URB           ! upward long wave radiation flux  [W/m/m]
     REAL :: G_URB            ! heat flux into the ground        [W/m/m]
     REAL :: RN_URB           ! net radiation                    [W/m/m]
     REAL :: PSIM_URB         ! shear f for momentum             [-]
     REAL :: PSIH_URB         ! shear f for heat                 [-]
     REAL :: GZ1OZ0_URB       ! shear f for heat                 [-]
     REAL :: U10_URB          ! wind u component at 10 m         [m/s]
     REAL :: V10_URB          ! wind v component at 10 m         [m/s]
     REAL :: TH2_URB          ! potential temperature at 2 m     [K]
     REAL :: Q2_URB           ! humidity at 2 m                  [-]
     REAL :: CHS_URB
     REAL :: CHS2_URB
     REAL :: UST_URB

! NUDAPT Parameters urban --> lam

     REAL :: mh_urb
     REAL :: stdh_urb
     REAL :: lp_urb
     REAL :: hgt_urb
     REAL, DIMENSION(4) :: lf_urb

! Local variables

     INTEGER :: I,J,K
     REAL :: Q1

! Noah UA changes

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMGR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHGR_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMC_SFCDIF
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHC_SFCDIF

! Variables for multi-layer UCM

     REAL, OPTIONAL,                                                    INTENT(IN   ) :: GMT
     INTEGER, OPTIONAL,                                                 INTENT(IN   ) :: JULDAY
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: XLAT, XLONG
     INTEGER,                                                           INTENT(IN   ) :: NUM_URBAN_LAYERS
     INTEGER,                                                           INTENT(IN   ) :: NUM_URBAN_HI
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_hi, jms:jme ),     INTENT(IN   ) :: hi_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: lp_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: lb_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: hgt_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: mh_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(IN   ) :: stdh_urb2d
     REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ),                  INTENT(IN   ) :: lf_urb2d

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: trb_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tgb_urb4d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tlev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: qlev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1lev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2lev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tglev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tflev_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: lf_ac_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: sf_ac_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: cm_ac_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: sfvent_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),                     INTENT(INOUT) :: lfvent_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin1_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin2_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw1_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw2_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfr_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfg_urb3d
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_u_bep   !Implicit momemtum component X-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_v_bep   !Implicit momemtum component Y-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_t_bep   !Implicit component pot. temperature
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_q_bep   !Implicit momemtum component X-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: a_e_bep   !Implicit component TKE
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_u_bep   !Explicit momentum component X-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_v_bep   !Explicit momentum component Y-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_t_bep   !Explicit component pot. temperature
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_q_bep   !Implicit momemtum component Y-direction
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: b_e_bep   !Explicit component TKE
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: vl_bep    !Fraction air volume in grid cell
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: dlg_bep   !Height above ground
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: sf_bep    !Fraction air at the face of grid cell
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            INTENT(INOUT) :: dl_u_bep  !Length scale

! Local variables for multi-layer UCM

     REAL,    DIMENSION( its:ite, jts:jte) :: HFX_RURAL,GRDFLX_RURAL
! ,LH_RURAL,RN_RURAL
     REAL,    DIMENSION( its:ite, jts:jte) :: QFX_RURAL
! ,QSFC_RURAL,UMOM_RURAL,VMOM_RURAL
     REAL,    DIMENSION( its:ite, jts:jte) :: ALB_RURAL,EMISS_RURAL,TSK_RURAL ! ,UST_RURAL
     REAL,    DIMENSION( its:ite, jts:jte) :: HFX_URB,UMOM_URB,VMOM_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: QFX_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: EMISS_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: RL_UP_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: RS_ABS_URB
     REAL,    DIMENSION( its:ite, jts:jte) :: GRDFLX_URB

     REAL :: SIGMA_SB,RL_UP_RURAL,RL_UP_TOT,RS_ABS_TOT,UMOM,VMOM
     REAL :: r1,r2,r3
     REAL :: CMR_URB, CHR_URB, CMC_URB, CHC_URB, CMGR_URB, CHGR_URB
     REAL :: frc_urb,lb_urb
     REAL :: check

      character(len=80) :: message
 
      DO J=JTS,JTE
      DO I=ITS,ITE
        HFX_RURAL(I,J)                = HFX(I,J)
        QFX_RURAL(I,J)                = QFX(I,J)
        GRDFLX_RURAL(I,J)             = GRDFLX(I,J)
        EMISS_RURAL(I,J)              = EMISS(I,J)
        TSK_RURAL(I,J)                = TSK(I,J)
        ALB_RURAL(I,J)                = ALBEDO(I,J)
      END DO
      END DO

      IF (SF_URBAN_PHYSICS == 1 ) THEN         ! Beginning of UCM CALL if block

!--------------------------------------
! URBAN CANOPY MODEL START
!--------------------------------------

      JLOOP : DO J = jts, jte

      ILOOP : DO I = its, ite


      IF( IVGTYP(I,J) == ISURBAN_TABLE .or. IVGTYP(I,J) == 31 .or. &
            IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33 ) THEN

      UTYPE_URB = UTYPE_URB2D(I,J) !urban type (low, high or industrial)

      TA_URB    = T3D(I,1,J)                                ! [K]            
      QA_URB    = QV3D(I,1,J)/(1.0+QV3D(I,1,J))             ! [kg/kg]       
      UA_URB    = SQRT(U_PHY(I,1,J)**2.+V_PHY(I,1,J)**2.)
      U1_URB    = U_PHY(I,1,J)
      V1_URB    = V_PHY(I,1,J)
      IF(UA_URB < 1.) UA_URB=1.                             ! [m/s]
      SSG_URB   = SWDOWN(I,J)                               ! [W/m/m]      
      SSGD_URB  = 0.8*SWDOWN(I,J)                           ! [W/m/m]     
      SSGQ_URB  = SSG_URB-SSGD_URB                          ! [W/m/m]
      LLG_URB   = GLW(I,J)                                  ! [W/m/m]
      RAIN_URB  = RAINBL(I,J)                               ! [mm]       
      RHOO_URB  = (P8W3D(I,KTS+1,J)+P8W3D(I,KTS,J))*0.5 / (287.04 * TA_URB * (1.0+ 0.61 * QA_URB)) ![kg/m/m/m] 
      ZA_URB    = 0.5*DZ8W(I,1,J)                           ! [m]         
      DELT_URB  = DT                                        ! [sec]
      XLAT_URB  = XLAT_URB2D(I,J)                           ! [deg]
      COSZ_URB  = COSZ_URB2D(I,J)
      OMG_URB   = OMG_URB2D(I,J)
      ZNT_URB   = ZNT(I,J)

      LSOLAR_URB = .FALSE.
      TR_URB = TR_URB2D(I,J)
      TB_URB = TB_URB2D(I,J)
      TG_URB = TG_URB2D(I,J)
      TC_URB = TC_URB2D(I,J)
      QC_URB = QC_URB2D(I,J)
      UC_URB = UC_URB2D(I,J)

      TGR_URB     = TGR_URB2D(I,J)
      CMCR_URB    = CMCR_URB2D(I,J)
      FLXHUMR_URB = FLXHUMR_URB2D(I,J)
      FLXHUMB_URB = FLXHUMB_URB2D(I,J)
      FLXHUMG_URB = FLXHUMG_URB2D(I,J)
      DRELR_URB   = DRELR_URB2D(I,J)
      DRELB_URB   = DRELB_URB2D(I,J)
      DRELG_URB   = DRELG_URB2D(I,J)

      DO K = 1,num_roof_layers
        TRL_URB(K) = TRL_URB3D(I,K,J)
        SMR_URB(K) = SMR_URB3D(I,K,J)
        TGRL_URB(K)= TGRL_URB3D(I,K,J)
      END DO

      DO K = 1,num_wall_layers
        TBL_URB(K) = TBL_URB3D(I,K,J)
      END DO
  
      DO K = 1,num_road_layers
        TGL_URB(K) = TGL_URB3D(I,K,J)
      END DO
  
      XXXR_URB = XXXR_URB2D(I,J)
      XXXB_URB = XXXB_URB2D(I,J)
      XXXG_URB = XXXG_URB2D(I,J)
      XXXC_URB = XXXC_URB2D(I,J)

! Limits to avoid dividing by small number
      IF (CHS(I,J) < 1.0E-02) THEN
        CHS(I,J)  = 1.0E-02
      ENDIF
      IF (CHS2(I,J) < 1.0E-02) THEN
        CHS2(I,J)  = 1.0E-02
      ENDIF
      IF (CQS2(I,J) < 1.0E-02) THEN
        CQS2(I,J)  = 1.0E-02
      ENDIF

      CHS_URB  = CHS(I,J)
      CHS2(I,J)= CQS2(I,J)
      CHS2_URB = CHS2(I,J)
      IF (PRESENT(CMR_SFCDIF)) THEN
        CMR_URB = CMR_SFCDIF(I,J)
        CHR_URB = CHR_SFCDIF(I,J)
        CMGR_URB = CMGR_SFCDIF(I,J)
        CHGR_URB = CHGR_SFCDIF(I,J)
        CMC_URB = CMC_SFCDIF(I,J)
        CHC_URB = CHC_SFCDIF(I,J)
      ENDIF

! NUDAPT for SLUCM

      MH_URB   = MH_URB2D(I,J)
      STDH_URB = STDH_URB2D(I,J)
      LP_URB   = LP_URB2D(I,J)
      HGT_URB  = HGT_URB2D(I,J)
      LF_URB   = 0.0
      DO K = 1,4
        LF_URB(K) = LF_URB2D(I,K,J)
      ENDDO
      FRC_URB  = FRC_URB2D(I,J)
      LB_URB   = LB_URB2D(I,J)
      CHECK    = 0
      IF (I.EQ.73.AND.J.EQ.125)THEN
        CHECK = 1
      END IF

! Call urban

!niu      CALL cal_mon_day(julian,julyr,jmonth,jday)
      CALL urban(LSOLAR_URB,                                                             & ! I
          num_roof_layers, num_wall_layers, num_road_layers,                           & ! C
                DZR,        DZB,        DZG,                                           & ! C
          UTYPE_URB,     TA_URB,     QA_URB,     UA_URB,   U1_URB, V1_URB, SSG_URB,    & ! I
           SSGD_URB,   SSGQ_URB,    LLG_URB,   RAIN_URB, RHOO_URB,                     & ! I
             ZA_URB, DECLIN_URB,   COSZ_URB,    OMG_URB,                               & ! I
           XLAT_URB,   DELT_URB,    ZNT_URB,                                           & ! I
            CHS_URB,   CHS2_URB,                                                       & ! I
             TR_URB,     TB_URB,     TG_URB,     TC_URB,   QC_URB, UC_URB,             & ! H
            TRL_URB,    TBL_URB,    TGL_URB,                                           & ! H
           XXXR_URB,   XXXB_URB,   XXXG_URB,   XXXC_URB,                               & ! H
             TS_URB,     QS_URB,     SH_URB,     LH_URB, LH_KINEMATIC_URB,             & ! O
             SW_URB,    ALB_URB,     LW_URB,      G_URB,   RN_URB, PSIM_URB, PSIH_URB, & ! O
         GZ1OZ0_URB,                                                                   & !O
            CMR_URB,    CHR_URB,    CMC_URB,    CHC_URB,                               &
            U10_URB,    V10_URB,    TH2_URB,     Q2_URB,                               & ! O
            UST_URB,     mh_urb,   stdh_urb,     lf_urb,   lp_urb,                     & ! 0
            hgt_urb,    frc_urb,     lb_urb,      check, CMCR_URB,TGR_URB,             & ! H
           TGRL_URB,    SMR_URB,   CMGR_URB,   CHGR_URB,   jmonth,                     & ! H
          DRELR_URB,  DRELB_URB,                                                       & ! H
          DRELG_URB,FLXHUMR_URB,FLXHUMB_URB,FLXHUMG_URB )

      TS_URB2D(I,J) = TS_URB

      ALBEDO(I,J)   = FRC_URB2D(I,J) * ALB_URB + (1-FRC_URB2D(I,J)) * ALBEDO(I,J)        ![-]      
      HFX(I,J)      = FRC_URB2D(I,J) * SH_URB  + (1-FRC_URB2D(I,J)) * HFX(I,J)           ![W/m/m] 
      QFX(I,J)      = FRC_URB2D(I,J) * LH_KINEMATIC_URB &
                       + (1-FRC_URB2D(I,J))* QFX(I,J)                                    ![kg/m/m/s] 
      LH(I,J)       = FRC_URB2D(I,J) * LH_URB  + (1-FRC_URB2D(I,J)) *LH(I,J)             ![W/m/m]   
      GRDFLX(I,J)   = FRC_URB2D(I,J) * (G_URB) + (1-FRC_URB2D(I,J)) *GRDFLX(I,J)         ![W/m/m]  
      TSK(I,J)      = FRC_URB2D(I,J) * TS_URB  + (1-FRC_URB2D(I,J)) * TSK(I,J)           ![K]    
!    Q1            = QSFC(I,J)/(1.0+QSFC(I,J))                                         
!    Q1            = FRC_URB2D(I,J) * QS_URB  + (1-FRC_URB2D(I,J)) * Q1 !    ![-]

! Convert QSFC back to mixing ratio

!    QSFC(I,J)     = Q1/(1.0-Q1)
      QSFC(I,J)    = FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*QSFC(I,J)               !!
      UST(I,J)     = FRC_URB2D(I,J) * UST_URB + (1-FRC_URB2D(I,J)) * UST(I,J)     ![m/s]

! Renew Urban State Variables

      TR_URB2D(I,J) = TR_URB
      TB_URB2D(I,J) = TB_URB
      TG_URB2D(I,J) = TG_URB
      TC_URB2D(I,J) = TC_URB
      QC_URB2D(I,J) = QC_URB
      UC_URB2D(I,J) = UC_URB

      TGR_URB2D(I,J)     = TGR_URB
      CMCR_URB2D(I,J)    = CMCR_URB
      FLXHUMR_URB2D(I,J) = FLXHUMR_URB
      FLXHUMB_URB2D(I,J) = FLXHUMB_URB
      FLXHUMG_URB2D(I,J) = FLXHUMG_URB
      DRELR_URB2D(I,J)   = DRELR_URB
      DRELB_URB2D(I,J)   = DRELB_URB
      DRELG_URB2D(I,J)   = DRELG_URB

      DO K = 1,num_roof_layers
        TRL_URB3D(I,K,J) = TRL_URB(K)
        SMR_URB3D(I,K,J) = SMR_URB(K)
        TGRL_URB3D(I,K,J)= TGRL_URB(K)
      END DO
      DO K = 1,num_wall_layers
        TBL_URB3D(I,K,J) = TBL_URB(K)
      END DO
      DO K = 1,num_road_layers
        TGL_URB3D(I,K,J) = TGL_URB(K)
      END DO

      XXXR_URB2D(I,J)    = XXXR_URB
      XXXB_URB2D(I,J)    = XXXB_URB
      XXXG_URB2D(I,J)    = XXXG_URB
      XXXC_URB2D(I,J)    = XXXC_URB

      SH_URB2D(I,J)      = SH_URB
      LH_URB2D(I,J)      = LH_URB
      G_URB2D(I,J)       = G_URB
      RN_URB2D(I,J)      = RN_URB
      PSIM_URB2D(I,J)    = PSIM_URB
      PSIH_URB2D(I,J)    = PSIH_URB
      GZ1OZ0_URB2D(I,J)  = GZ1OZ0_URB
      U10_URB2D(I,J)     = U10_URB
      V10_URB2D(I,J)     = V10_URB
      TH2_URB2D(I,J)     = TH2_URB
      Q2_URB2D(I,J)      = Q2_URB
      UST_URB2D(I,J)     = UST_URB
      AKMS_URB2D(I,J)    = KARMAN * UST_URB2D(I,J)/(GZ1OZ0_URB2D(I,J)-PSIM_URB2D(I,J))
      IF (PRESENT(CMR_SFCDIF)) THEN
        CMR_SFCDIF(I,J)  = CMR_URB
        CHR_SFCDIF(I,J)  = CHR_URB
        CMGR_SFCDIF(I,J) = CMGR_URB
        CHGR_SFCDIF(I,J) = CHGR_URB
        CMC_SFCDIF(I,J)  = CMC_URB
        CHC_SFCDIF(I,J)  = CHC_URB
      ENDIF

      ENDIF                                 ! urban land used type block

  ENDDO ILOOP                             ! of I loop
  ENDDO JLOOP                             ! of J loop

  ENDIF                                   ! sf_urban_physics = 1 block

!--------------------------------------
! URBAN CANOPY MODEL END
!--------------------------------------
!--------------------------------------
! URBAN BEP and BEM MODEL BEGIN
!--------------------------------------

      IF (SF_URBAN_PHYSICS == 2) THEN

      DO J=JTS,JTE
      DO I=      ITS,ITE
      
        EMISS_URB(I,J)       = 0.
        RL_UP_URB(I,J)       = 0.
        RS_ABS_URB(I,J)      = 0.
        GRDFLX_URB(I,J)      = 0.
        B_Q_BEP(I,KTS:KTE,J) = 0.

      END DO
      END DO

      CALL BEP(frc_urb2d,  utype_urb2d, itimestep,       dz8w,         &
                      dt,        u_phy,     v_phy,                     &
                  th_phy,          rho,     p_phy,     swdown,    glw, &
                     gmt,       julday,     xlong,       xlat,         &
              declin_urb,   cosz_urb2d, omg_urb2d,                     &
        num_urban_layers, num_urban_hi,                                &
               trb_urb4d,    tw1_urb4d, tw2_urb4d,  tgb_urb4d,         &
              sfw1_urb3d,   sfw2_urb3d, sfr_urb3d,  sfg_urb3d,         &
                lp_urb2d,     hi_urb2d,  lb_urb2d,  hgt_urb2d,         &
                 a_u_bep,      a_v_bep,   a_t_bep,                     &
                 a_e_bep,      b_u_bep,   b_v_bep,                     &
                 b_t_bep,      b_e_bep,   b_q_bep,    dlg_bep,         &
                dl_u_bep,       sf_bep,    vl_bep,                     &
               rl_up_urb,   rs_abs_urb, emiss_urb, grdflx_urb,         &
             ids,ide, jds,jde, kds,kde,                                &
             ims,ime, jms,jme, kms,kme,                                &
             its,ite, jts,jte, kts,kte )

      ENDIF ! SF_URBAN_PHYSICS == 2

      IF (SF_URBAN_PHYSICS == 3) THEN

      DO J=JTS,JTE
      DO I=ITS,ITE

        EMISS_URB(I,J)       = 0.
        RL_UP_URB(I,J)       = 0.
        RS_ABS_URB(I,J)      = 0.
        GRDFLX_URB(I,J)      = 0.
        B_Q_BEP(I,KTS:KTE,J) = 0.

      END DO
      END DO

     CALL BEP_BEM( frc_urb2d,  utype_urb2d,    itimestep,     dz8w,        &
                       dt,        u_phy,        v_phy,                     &
                   th_phy,          rho,        p_phy,       swdown,  glw, &
                      gmt,       julday,        xlong,         xlat,       &
               declin_urb,   cosz_urb2d,    omg_urb2d,                     &
         num_urban_layers, num_urban_hi,                                   &
                trb_urb4d,    tw1_urb4d,    tw2_urb4d,    tgb_urb4d,       &
               tlev_urb3d,   qlev_urb3d, tw1lev_urb3d, tw2lev_urb3d,       &
              tglev_urb3d,  tflev_urb3d,  sf_ac_urb3d,  lf_ac_urb3d,       &
              cm_ac_urb3d, sfvent_urb3d, lfvent_urb3d,                     &
             sfwin1_urb3d, sfwin2_urb3d,                                   &
               sfw1_urb3d,   sfw2_urb3d,    sfr_urb3d,    sfg_urb3d,       &
                 lp_urb2d,     hi_urb2d,     lb_urb2d,    hgt_urb2d,       &
                  a_u_bep,      a_v_bep,      a_t_bep,                     &
                  a_e_bep,      b_u_bep,      b_v_bep,                     &
                  b_t_bep,      b_e_bep,      b_q_bep,      dlg_bep,       &
                 dl_u_bep,       sf_bep,       vl_bep,                     &
                rl_up_urb,   rs_abs_urb,    emiss_urb,   grdflx_urb, qv3d, &
             ids,ide, jds,jde, kds,kde,                                    &
             ims,ime, jms,jme, kms,kme,&
             its,ite, jts,jte, kts,kte )

     ENDIF ! SF_URBAN_PHYSICS == 3

     IF((SF_URBAN_PHYSICS == 2).OR.(SF_URBAN_PHYSICS == 3))THEN

       sigma_sb=5.67e-08
       do j = jts, jte
       do i = its, ite
         UMOM_URB(I,J)     = 0.
         VMOM_URB(I,J)     = 0.
         HFX_URB(I,J)      = 0.
         QFX_URB(I,J)      = 0.

         do k=kts,kte
           a_u_bep(i,k,j) = a_u_bep(i,k,j)*frc_urb2d(i,j)
           a_v_bep(i,k,j) = a_v_bep(i,k,j)*frc_urb2d(i,j)
           a_t_bep(i,k,j) = a_t_bep(i,k,j)*frc_urb2d(i,j)
           a_q_bep(i,k,j) = 0.
           a_e_bep(i,k,j) = 0.
           b_u_bep(i,k,j) = b_u_bep(i,k,j)*frc_urb2d(i,j)
           b_v_bep(i,k,j) = b_v_bep(i,k,j)*frc_urb2d(i,j)
           b_t_bep(i,k,j) = b_t_bep(i,k,j)*frc_urb2d(i,j)
           b_q_bep(i,k,j) = b_q_bep(i,k,j)*frc_urb2d(i,j)
           b_e_bep(i,k,j) = b_e_bep(i,k,j)*frc_urb2d(i,j)
           HFX_URB(I,J)   = HFX_URB(I,J) + B_T_BEP(I,K,J)*RHO(I,K,J)*CP*DZ8W(I,K,J)*VL_BEP(I,K,J)
           QFX_URB(I,J)   = QFX_URB(I,J) + B_Q_BEP(I,K,J)*DZ8W(I,K,J)*VL_BEP(I,K,J)
           UMOM_URB(I,J)  = UMOM_URB(I,J)+ (A_U_BEP(I,K,J)*U_PHY(I,K,J)+B_U_BEP(I,K,J))*DZ8W(I,K,J)*VL_BEP(I,K,J)
           VMOM_URB(I,J)  = VMOM_URB(I,J)+ (A_V_BEP(I,K,J)*V_PHY(I,K,J)+B_V_BEP(I,K,J))*DZ8W(I,K,J)*VL_BEP(I,K,J)
           vl_bep(i,k,j)  = (1.-frc_urb2d(i,j)) + vl_bep(i,k,j)*frc_urb2d(i,j)
           sf_bep(i,k,j)  = (1.-frc_urb2d(i,j)) + sf_bep(i,k,j)*frc_urb2d(i,j)
         end do

         a_u_bep(i,1,j)   = (1.-frc_urb2d(i,j))*(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/   &
                               ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+a_u_bep(i,1,j)

         a_v_bep(i,1,j)   = (1.-frc_urb2d(i,j))*(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/   &
                               ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+a_v_bep(i,1,j)

         b_t_bep(i,1,j)   = (1.-frc_urb2d(i,j))*hfx_rural(i,j)/dz8w(i,1,j)/rho(i,1,j)/CP+ &
                                b_t_bep(i,1,j)

         b_q_bep(i,1,j)   = (1.-frc_urb2d(i,j))*qfx_rural(i,j)/dz8w(i,1,j)/rho(i,1,j)+b_q_bep(i,1,j)
     
         umom             = (1.-frc_urb2d(i,j))*ust(i,j)*ust(i,j)*u_phy(i,1,j)/               &
                              ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+umom_urb(i,j)

         vmom             = (1.-frc_urb2d(i,j))*ust(i,j)*ust(i,j)*v_phy(i,1,j)/               &
                              ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+vmom_urb(i,j)
         sf_bep(i,1,j)    = 1.

! using the emissivity and the total longwave upward radiation estimate ! the averaged skin temperature  

       IF (FRC_URB2D(I,J).GT.0.) THEN
         rl_up_rural   = -emiss_rural(i,j)*sigma_sb*(tsk_rural(i,j)**4.)-(1.-emiss_rural(i,j))*glw(i,j)
         rl_up_tot     = (1.-frc_urb2d(i,j))*rl_up_rural     + frc_urb2d(i,j)*rl_up_urb(i,j)
         emiss(i,j)    = (1.-frc_urb2d(i,j))*emiss_rural(i,j)+ frc_urb2d(i,j)*emiss_urb(i,j)
         ts_urb2d(i,j) = (max(0.,(-rl_up_urb(i,j)-(1.-emiss_urb(i,j))*glw(i,j))/emiss_urb(i,j)/sigma_sb))**0.25
         tsk(i,j)      = (max(0., (-1.*rl_up_tot-(1.-emiss(i,j))*glw(i,j) )/emiss(i,j)/sigma_sb))**.25
         rs_abs_tot    =(1.-frc_urb2d(i,j))*swdown(i,j)*(1.-albedo(i,j))+frc_urb2d(i,j)*rs_abs_urb(i,j)

         if(swdown(i,j) > 0.)then
           albedo(i,j) = 1.-rs_abs_tot/swdown(i,j)
         else
           albedo(i,j) = alb_rural(i,j)
         endif

! rename *_urb to sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d

         grdflx(i,j)   = (1.-frc_urb2d(i,j))*grdflx_rural(i,j)+ frc_urb2d(i,j)*grdflx_urb(i,j)
         qfx(i,j)      = (1.-frc_urb2d(i,j))*qfx_rural(i,j)   + qfx_urb(i,j)
         lh(i,j)       = qfx(i,j)*xlv
         hfx(i,j)      = hfx_urb(i,j)                         +(1-frc_urb2d(i,j))*hfx_rural(i,j)      ![W/m/m]
         sh_urb2d(i,j) = hfx_urb(i,j)/frc_urb2d(i,j)
         lh_urb2d(i,j) = qfx_urb(i,j)*xlv
         g_urb2d(i,j)  = grdflx_urb(i,j)
         rn_urb2d(i,j) = rs_abs_urb(i,j)+emiss_urb(i,j)*glw(i,j)-rl_up_urb(i,j)
         ust(i,j)      = (umom**2.+vmom**2.)**.25

       ELSE

         sh_urb2d(i,j)    = 0.
         lh_urb2d(i,j)    = 0.
         g_urb2d(i,j)     = 0.
         rn_urb2d(i,j)    = 0.

       ENDIF

       enddo ! jloop
       enddo ! iloop

      ENDIF ! SF_URBAN_PHYSICS == 2 or 3

!--------------------------------------
! URBAN BEP and BEM MODEL END
!--------------------------------------


      END SUBROUTINE noahmp_urban

!------------------------------------------------------------------------------------------

