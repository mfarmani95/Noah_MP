

MODULE MODULE_SF_NOAHMPLSM

  USE module_Noahlsm_mixedRE,only: mixedre,get_psi

  IMPLICIT NONE

  public  :: noahmp_options
  public  :: NOAHMP_SFLX

  private :: ATM
  private :: PHENOLOGY
  private :: PRECIP_HEAT
  private :: ENERGY
  private ::       THERMOPROP
  private ::               CSNOW
  private ::               TDFCND
  private ::       RADIATION
  private ::               ALBEDO
  private ::                         SNOW_AGE
  private ::                         SNOWALB_BATS  
  private ::                         SNOWALB_CLASS
  private ::                         SOIL_ALBEDO
  private ::                         ESM_SNICAR_direct
  private ::                         ESM_SNICAR_difus
  private ::                         GROUNDALB
  private ::                         TWOSTREAM
  private ::               SURRAD
  private ::       VEGE_FLUX
  private ::               SFCDIF1                  
  private ::               SFCDIF2                
  private ::               STOMATA                  
  private ::               CANRES                  
  private ::               ESAT
  private ::               RAGRB
  private ::       BARE_FLUX
  private ::       TSNOSOI
  private ::               HRT
  private ::               HSTEP   
  private ::                         ROSR12
  private ::       PHASECHANGE
  private ::               FRH2O           

  private :: WATER
  private ::       CANWATER
  private ::       SNOWWATER
  private ::               SNOWAGING
  private ::               SNOWFALL
  private ::               COMBINE
  private ::               DIVIDE
  private ::                         COMBO
  private ::               COMPACT
  private ::               SNOWH2O
  private ::       SOILWATER
  private ::               ZWTEQ
  private ::               INFIL
  private ::               SRT
  private ::                         WDFCND1        
  private ::                         WDFCND2        
  private ::                         WDFCND3
  private ::                         WDFCND4        
  private ::               SSTEP
  private ::       GROUNDWATER
  private ::       SHALLOWWATERTABLE

  private :: CARBON
  private ::       CO2FLUX
!  private ::       BVOCFLUX
!  private ::       CH4FLUX

  private :: ERROR

! =====================================options for different schemes================================
! **recommended

  INTEGER :: DVEG     ! options for dynamic vegetation: 
                      !   1 -> off (use table LAI; use FVEG = SHDFAC from input)
                      !   2 -> on  (together with OPT_CRS = 1)
                      !   3 -> off (use table LAI; calculate FVEG)
                      ! **4 -> off (use table LAI; use maximum vegetation fraction)
                      ! **5 -> on  (use maximum vegetation fraction)
                      !   6 -> on  (use FVEG = SHDFAC from input)
                      !   7 -> off (use input LAI; use FVEG = SHDFAC from input)
                      !   8 -> off (use input LAI; calculate FVEG)
                      !   9 -> off (use input LAI; use maximum vegetation fraction)
                      !  10 -> crop model on (use maximum vegetation fraction)

  INTEGER :: OPT_CRS  ! options for canopy stomatal resistance
                      ! **1 -> Ball-Berry
		      !   2 -> Jarvis

  INTEGER :: OPT_BTR  ! options for soil moisture factor for stomatal resistance
                      ! **1 -> Noah (soil moisture) 
                      !   2 -> CLM  (matric potential)
                      !   3 -> SSiB (matric potential)

  INTEGER :: OPT_RUN  ! options for runoff and groundwater
                      ! **1 -> TOPMODEL with groundwater (Niu et al. 2007 JGR) ;
                      !   2 -> TOPMODEL with an equilibrium water table (Niu et al. 2005 JGR) ;
                      !   3 -> original surface and subsurface runoff (free drainage)
                      !   4 -> BATS surface and subsurface runoff (free drainage)
                      !   5 -> Miguez-Macho&Fan groundwater scheme (Miguez-Macho et al. 2007 JGR; Fan et al. 2007 JGR)
		      !          (needs further testing for public use)

  INTEGER :: OPT_SFC  ! options for surface layer drag coeff (CH & CM)
                      ! **1 -> M-O
		      ! **2 -> original Noah (Chen97)
		      ! **3 -> MYJ consistent; 4->YSU consistent. MB: removed in v3.7 for further testing

  INTEGER :: OPT_FRZ  ! options for supercooled liquid water (or ice fraction)
                      ! **1 -> no iteration (Niu and Yang, 2006 JHM)
		      !   2 -> Koren's iteration 

  INTEGER :: OPT_INF  ! options for frozen soil permeability
                      ! **1 -> linear effects, more permeable (Niu and Yang, 2006, JHM)
                      !   2 -> nonlinear effects, less permeable (old)

  INTEGER :: OPT_RAD  ! options for radiation transfer
                      !   1 -> modified two-stream (gap = F(solar angle, 3D structure ...)<1-FVEG)
                      !   2 -> two-stream applied to grid-cell (gap = 0)
                      ! **3 -> two-stream applied to vegetated fraction (gap=1-FVEG)

  INTEGER :: OPT_ALB  ! options for ground snow surface albedo
                      !   1 -> BATS
		      ! **2 -> CLASS
                      !   3 -> ESM_SNICAR

  INTEGER :: OPT_SNF  ! options for partitioning  precipitation into rainfall & snowfall
                      ! **1 -> Jordan (1991)
		      !   2 -> BATS: when SFCTMP<TFRZ+2.2 
		      !   3 -> SFCTMP < TFRZ
		      !   4 -> Use WRF microphysics output

  INTEGER :: OPT_TBOT ! options for lower boundary condition of soil temperature
                      !   1 -> zero heat flux from bottom (ZBOT and TBOT not used)
                      ! **2 -> TBOT at ZBOT (8m) read from a file (original Noah)

  INTEGER :: OPT_STC  ! options for snow/soil temperature time scheme (only layer 1)
                      ! **1 -> semi-implicit; flux top boundary condition
		      !   2 -> full implicit (original Noah); temperature top boundary condition
                      !   3 -> same as 1, but FSNO for TS calculation (generally improves snow; v3.7)

  INTEGER :: OPT_RSF  ! options for surface resistent to evaporation/sublimation
                      ! **1 -> Sakaguchi and Zeng, 2009
		      !   2 -> Sellers (1992)
                      !   3 -> adjusted Sellers to decrease RSURF for wet soil
		      !   4 -> option 1 for non-snow; rsurf = rsurf_snow for snow (set in MPTABLE); AD v3.8


  INTEGER :: OPT_SCM  ! options for soil carbon model
                      !   1 -> first-order decay
                      !   2 -> 4 carbon pool model
                      !   3 -> 6 carbon pool model
                    
  INTEGER :: OPT_WATRET ! options for soil water retention model
                        ! 1 -> van Genutchen
                        ! 2 -> Clapp & Hornberger

  INTEGER :: OPT_ROOT ! options for root profile
                      !   1 -> dynamic root
                      !   2 -> static, even root profile

!------------------------------------------------------------------------------------------!
! Physical Constants:                                                                      !
!------------------------------------------------------------------------------------------!

  REAL, PARAMETER :: GRAV   = 9.80616   !acceleration due to gravity (m/s2)
  REAL, PARAMETER :: SB     = 5.67E-08  !Stefan-Boltzmann constant (w/m2/k4)
  REAL, PARAMETER :: VKC    = 0.40      !von Karman constant
  REAL, PARAMETER :: TFRZ   = 273.16    !freezing/melting point (k)
  REAL, PARAMETER :: HSUB   = 2.8440E06 !latent heat of sublimation (j/kg)
  REAL, PARAMETER :: HVAP   = 2.5104E06 !latent heat of vaporization (j/kg)
  REAL, PARAMETER :: HFUS   = 0.3336E06 !latent heat of fusion (j/kg)
  REAL, PARAMETER :: CWAT   = 4.188E06  !specific heat capacity of water (j/m3/k)
  REAL, PARAMETER :: CICE   = 2.094E06  !specific heat capacity of ice (j/m3/k)
  REAL, PARAMETER :: CPAIR  = 1004.64   !heat capacity dry air at const pres (j/kg/k)
  REAL, PARAMETER :: TKWAT  = 0.6       !thermal conductivity of water (w/m/k)
  REAL, PARAMETER :: TKICE  = 2.2       !thermal conductivity of ice (w/m/k)
  REAL, PARAMETER :: TKAIR  = 0.023     !thermal conductivity of air (w/m/k) (not used MB: 20140718)
  REAL, PARAMETER :: RAIR   = 287.04    !gas constant for dry air (j/kg/k)
  REAL, PARAMETER :: RW     = 461.269   !gas constant for  water vapor (j/kg/k)
  REAL, PARAMETER :: DENH2O = 1000.     !density of water (kg/m3)
  REAL, PARAMETER :: DENICE = 917.      !density of ice (kg/m3)

  INTEGER, PRIVATE, PARAMETER :: MBAND  = 2
  INTEGER, PRIVATE, PARAMETER :: NSOIL  = 15
 !INTEGER, PRIVATE, PARAMETER :: NSOIL  = 6
  INTEGER, PRIVATE, PARAMETER :: NSTAGE = 8
  INTEGER, PRIVATE, PARAMETER :: NRAD   = 1451 !(50um ->1500um)

  REAL   , PARAMETER :: FSDE         = 1.    !soil conductivity decrease due to frozon soil  

  type constants  ! define esm_snicar radiative parameters

!------------------------------------------------------------------------------------------!
! From the ESM_SNICAR.TBL file
!------------------------------------------------------------------------------------------!
! From the mie section of the ESM_SNICAR.TBL file
! five broadbands: 0.3-0.7 μm, 0.7-0.9 μm, 0.9-1.2 μm, 1.2-1.5 μm and 1.5-5 μm.
       REAL :: ext_mie_bd(1:NRAD, 1:5)
       REAL :: w_mie_bd  (1:NRAD, 1:5)
       REAL :: g1_mie_bd (1:NRAD, 1:5)
       REAL :: g2_mie_bd (1:NRAD, 1:5)
       REAL :: g3_mie_bd (1:NRAD, 1:5)
       REAL :: g4_mie_bd (1:NRAD, 1:5)
       REAL :: ext_mie_lap_bd (1:7, 1:5)  !LAP: light-absorbing particles
       REAL :: w_mie_lap_bd   (1:7, 1:5)
       REAL :: g_mie_lap_bd   (1:7, 1:5)
!------------------------------------------------------------------------------------------!
! From the snowaging section of the ESM_SNICAR.TBL file
       REAL :: drdt0(8,31,11)   ! snow aging parameters from Flanner
       REAL :: tau  (8,31,11)   ! snow aging parameters from Flanner
       REAL :: kappa(8,31,11)   ! snow aging parameters from Flanner

       REAL :: Tsnow(11)        ! [Kelvin]
       REAL :: dT2dZ(31)        ! [K/m]
       REAL :: sno_dns(8)       ! [kg/m3]

  END type constants

  TYPE noahmp_parameters ! define a NoahMP parameters type

!------------------------------------------------------------------------------------------!
! From the veg section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!

    LOGICAL :: URBAN_FLAG
    INTEGER :: ISWATER
    INTEGER :: ISBARREN
    INTEGER :: ISICE
    INTEGER :: ISCROP
    INTEGER :: EBLFOREST

    REAL :: CH2OP              !maximum intercepted h2o per unit lai+sai (mm)
    REAL :: DLEAF              !characteristic leaf dimension (m)
    REAL :: Z0MVT              !momentum roughness length (m)
    REAL :: HVT                !top of canopy (m)
    REAL :: HVB                !bottom of canopy (m)
    REAL :: DEN                !tree density (no. of trunks per m2)
    REAL :: RC                 !tree crown radius (m)
    REAL :: MFSNO              !snowmelt m parameter ()
    REAL :: SAIM(12)           !monthly stem area index, one-sided
    REAL :: LAIM(12)           !monthly leaf area index, one-sided
    REAL :: SLA                !single-side leaf area per Kg [m2/kg]
    REAL :: DILEFC             !coeficient for leaf stress death [1/s]
    REAL :: DILEFW             !coeficient for leaf stress death [1/s]
    REAL :: FRAGR              !fraction of growth respiration  !original was 0.3 
    REAL :: LTOVRC             !leaf turnover [1/s]

    REAL :: C3PSN              !photosynthetic pathway: 0. = c4, 1. = c3
    REAL :: KC25               !co2 michaelis-menten constant at 25c (pa)
    REAL :: AKC                !q10 for kc25
    REAL :: KO25               !o2 michaelis-menten constant at 25c (pa)
    REAL :: AKO                !q10 for ko25
    REAL :: VCMX25             !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    REAL :: AVCMX              !q10 for vcmx25
    REAL :: BP                 !minimum leaf conductance (umol/m**2/s)
    REAL :: MP                 !slope of conductance-to-photosynthesis relationship
    REAL :: QE25               !quantum efficiency at 25c (umol co2 / umol photon)
    REAL :: AQE                !q10 for qe25
    REAL :: RMF25              !leaf maintenance respiration at 25c (umol co2/m**2/s)
    REAL :: RMS25              !stem maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: RMR25              !root maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: ARM                !q10 for maintenance respiration
    REAL :: FOLNMX             !foliage nitrogen concentration when f(n)=1 (%)
    REAL :: TMIN               !minimum temperature for photosynthesis (k)
       
    REAL :: XL                 !leaf/stem orientation index
    REAL :: RHOL(MBAND)        !leaf reflectance: 1=vis, 2=nir
    REAL :: RHOS(MBAND)        !stem reflectance: 1=vis, 2=nir
    REAL :: TAUL(MBAND)        !leaf transmittance: 1=vis, 2=nir
    REAL :: TAUS(MBAND)        !stem transmittance: 1=vis, 2=nir

    REAL :: MRP                !microbial respiration parameter (umol co2 /kg c/ s)
    REAL :: CWPVT              !empirical canopy wind parameter

    REAL :: WRRAT              !wood to non-wood ratio
    REAL :: WDPOOL             !wood pool (switch 1 or 0) depending on woody or not [-]
    REAL :: TDLEF              !characteristic T for leaf freezing [K]

  INTEGER :: NROOT              !number of soil layers with root present
     REAL :: RGL                !Parameter used in radiation stress function
     REAL :: RSMIN              !Minimum stomatal resistance [s m-1]
     REAL :: HS                 !Parameter used in vapor pressure deficit function
     REAL :: TOPT               !Optimum transpiration air temperature [K]
     REAL :: RSMAX              !Maximal stomatal resistance [s m-1]

     REAL :: SRA                !min specific root area [m2/kg]
     REAL :: OMR                !root resistivity to water uptake [s]
     REAL :: MQX                !ratio of water storage to dry biomass [-]
     REAL :: RTOMAX             !max root turnover rate [g/m2/year]
     REAL :: RROOT              !mean radius of fine roots [mm]
     REAL :: SCEXP              !decay rate of cold stress for leaf death

     REAL :: SLAREA
     REAL :: EPS(5)

!------------------------------------------------------------------------------------------!
! From the rad section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!

     REAL :: ALBSAT(MBAND)       !saturated soil albedos: 1=vis, 2=nir
     REAL :: ALBDRY(MBAND)       !dry soil albedos: 1=vis, 2=nir
     REAL :: ALBICE(MBAND)       !albedo land ice: 1=vis, 2=nir
     REAL :: ALBLAK(MBAND)       !albedo frozen lakes: 1=vis, 2=nir
     REAL :: OMEGAS(MBAND)       !two-stream parameter omega for snow
     REAL :: BETADS              !two-stream parameter betad for snow
     REAL :: BETAIS              !two-stream parameter betad for snow
     REAL :: EG(2)               !emissivity

!------------------------------------------------------------------------------------------!
! From the globals section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!
 
     REAL :: CO2          !co2 partial pressure
     REAL :: O2           !o2 partial pressure
     REAL :: TIMEAN       !gridcell mean topgraphic index (global mean)
     REAL :: FSATMX       !maximum surface saturated fraction (global mean)
     REAL :: Z0SNO        !snow surface roughness length (m) (0.002)
     REAL :: SSI          !liquid water holding capacity for snowpack (m3/m3)
     REAL :: SWEMX        !new snow mass to fully cover old snow (mm)
     REAL :: RSURF_SNOW   !surface resistance for snow(s/m)

!------------------------------------------------------------------------------------------!
! From the crop section of MPTABLE.TBL
!------------------------------------------------------------------------------------------!
 
  INTEGER :: PLTDAY           ! Planting date
  INTEGER :: HSDAY            ! Harvest date
     REAL :: PLANTPOP         ! Plant density [per ha] - used?
     REAL :: IRRI             ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)
     REAL :: GDDTBASE         ! Base temperature for GDD accumulation [C]
     REAL :: GDDTCUT          ! Upper temperature for GDD accumulation [C]
     REAL :: GDDS1            ! GDD from seeding to emergence
     REAL :: GDDS2            ! GDD from seeding to initial vegetative 
     REAL :: GDDS3            ! GDD from seeding to post vegetative 
     REAL :: GDDS4            ! GDD from seeding to intial reproductive
     REAL :: GDDS5            ! GDD from seeding to pysical maturity 
  INTEGER :: C3C4             ! photosynthetic pathway:  1 = c3 2 = c4
     REAL :: AREF             ! reference maximum CO2 assimulation rate 
     REAL :: PSNRF            ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
     REAL :: I2PAR            ! Fraction of incoming solar radiation to photosynthetically active radiation
     REAL :: TASSIM0          ! Minimum temperature for CO2 assimulation [C]
     REAL :: TASSIM1          ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
     REAL :: TASSIM2          ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
     REAL :: K                ! light extinction coefficient
     REAL :: EPSI             ! initial light use efficiency
     REAL :: Q10MR            ! q10 for maintainance respiration
     REAL :: FOLN_MX          ! foliage nitrogen concentration when f(n)=1 (%)
     REAL :: LEFREEZ          ! characteristic T for leaf freezing [K]
     REAL :: DILE_FC(NSTAGE)  ! coeficient for temperature leaf stress death [1/s]
     REAL :: DILE_FW(NSTAGE)  ! coeficient for water leaf stress death [1/s]
     REAL :: FRA_GR           ! fraction of growth respiration 
     REAL :: LF_OVRC(NSTAGE)  ! fraction of leaf turnover  [1/s]
     REAL :: ST_OVRC(NSTAGE)  ! fraction of stem turnover  [1/s]
     REAL :: RT_OVRC(NSTAGE)  ! fraction of root tunrover  [1/s]
     REAL :: LFMR25           ! leaf maintenance respiration at 25C [umol CO2/m**2  /s]
     REAL :: STMR25           ! stem maintenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: RTMR25           ! root maintenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: GRAINMR25        ! grain maintenance respiration at 25C [umol CO2/kg bio/s]
     REAL :: LFPT(NSTAGE)     ! fraction of carbohydrate flux to leaf
     REAL :: STPT(NSTAGE)     ! fraction of carbohydrate flux to stem
     REAL :: RTPT(NSTAGE)     ! fraction of carbohydrate flux to root
     REAL :: GRAINPT(NSTAGE)  ! fraction of carbohydrate flux to grain
     REAL :: BIO2LAI          ! leaf are per living leaf biomass [m^2/kg]

!------------------------------------------------------------------------------------------!
! From the SOILPARM.TBL tables, as functions of soil category.
!------------------------------------------------------------------------------------------!
     REAL :: BEXP(NSOIL)   !B parameter
     REAL :: SMCDRY(NSOIL) !dry soil moisture threshold where direct evap from top
                           !layer ends (volumetric) (not used MB: 20140718)
     REAL :: SMCWLT(NSOIL) !wilting point soil moisture (volumetric)
     REAL :: SMCREF(NSOIL) !reference soil moisture (field capacity) (volumetric)
     REAL :: SMCMAX(NSOIL) !porosity, saturated value of soil moisture (volumetric)
     REAL :: PSISAT(NSOIL) !saturated soil matric potential
     REAL :: DKSAT(NSOIL)  !saturated soil hydraulic conductivity
     REAL :: DWSAT(NSOIL)  !saturated soil hydraulic diffusivity
     REAL :: QUARTZ(NSOIL) !soil quartz content
     REAL :: F1            !soil thermal diffusivity/conductivity coef (not used MB: 20140718)
     REAL :: SMCR(NSOIL)   !residual soil moisture (volumatric)
     REAL :: VGN(NSOIL)    !van Genuchten n parameter (=1.-1./n)
     REAL :: VGPSAT(NSOIL) !air-entry water potential (m)
!------------------------------------------------------------------------------------------!
! From the GENPARM.TBL file
!------------------------------------------------------------------------------------------!
     REAL :: SLOPE       !slope index (0 - 1)
     REAL :: CSOIL       !vol. soil heat capacity [j/m3/K]
     REAL :: ZBOT        !Depth (m) of lower boundary soil temperature
     REAL :: CZIL        !Calculate roughness length of heat
     REAL :: REFDK
     REAL :: REFKDT

     REAL :: KDT         !used in compute maximum infiltration rate (in INFIL)
     REAL :: FRZX        !used in compute maximum infiltration rate (in INFIL)

  END TYPE noahmp_parameters

contains
!
!== begin noahmp_sflx ==============================================================================

  SUBROUTINE NOAHMP_SFLX (rad_cons,SLOPETYP   ,SOILCOLOR, SOILTYP , &
                   ILOC    , JLOC    , LAT     , YEARLEN , JULIAN  , COSZ    , & ! IN : Time/Space-related
                   DT      , DX      , DZ8W    , NSOIL   , ZSOIL   , NSNOW   , & ! IN : Model configuration 
                   SHDFAC  , SHDMAX  , VEGTYP  , ICE     , IST     , CROPTYPE, & ! IN : Vegetation/Soil characteristics
                   SMCEQ   , TOPOSV  ,                                         & ! IN : Vegetation/Soil characteristics
                   SFCTMP  , SFCPRS  , PSFC    , UU      , VV      , Q2      , & ! IN : Forcing
                   QC      , SOLDN   , LWDN    ,                               & ! IN : Forcing
                   PRCPCONV, PRCPNONC, PRCPSHCV, PRCPSNOW, PRCPGRPL, PRCPHAIL, & ! IN : Forcing
                   TBOT    , CO2AIR  , O2AIR   , FOLN    , FICEOLD , ZLVL    , & ! IN : Forcing
                   ALBOLD  , SNEQVO  , radius  ,                               & ! IN/OUT : 
                   STC     , SH2O    , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
                   CANLIQ  , CANICE  , TV      , TG      , QSFC    , QSNOW   , & ! IN/OUT : 
                   ISNOW   , ZSNSO   , SNOWH   , SNEQV   , SNICE   , SNLIQ   , & ! IN/OUT : 
                   ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
                   STMASS  , WOOD    , STBLCP  , FASTCP  , LAI     , SAI     , & ! IN/OUT : 
                   CM      , CH      , TAUSS   ,                               & ! IN/OUT : 
                   GRAIN   , GDD     , PGS     ,                               & ! IN/OUT 
                   SMCWTD  , DEEPRECH, RECH    ,                               & ! IN/OUT :
                   soc     , wdoc    , ddoc    , mic     , wenz     ,denz    , & ! IN/OUT :
                   mq      , kr      , froot   , ROOTMS  ,                     & ! IN/OUT
                   Z0WRF   , &
                   FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & ! OUT : 
                   FGEV    , FCTR    , ECAN    , ETRAN   , EDIR    , TRAD    , & ! OUT :
                   TGB     , TGV     , T2MV    , T2MB    , Q2V     , Q2B     , & ! OUT :
                   RUNSRF  , RUNSUB  , APAR    , PSN     , SAV     , SAG     , & ! OUT :
                   FSNO    , NEE     , GPP     , NPP     , FVEG    , ALBEDO  , & ! OUT :
                   QSNBOT  , PONDING , PONDING1, PONDING2, RSSUN   , RSSHA   , & ! OUT :
                   ALBSND  , ALBSNI  ,                                         & ! OUT :
                   BGAP    , WGAP    , CHV     , CHB     , EMISSI  ,           & ! OUT :
                   SHG     , SHC     , SHB     , EVG     , EVB     , GHV     , & ! OUT :
                   GHB     , IRG     , IRC     , IRB     , TR      , EVC     , & ! OUT :
                   CHLEAF  , CHUC    , CHV2    , CHB2    , FPICE   , PAHV    , &
                   PAHG    , PAHB    , PAH     ,                               &
                   qco2    , vmax    , km      , vmaxup  , kmup    , epslon  , & ! out
                   QIN     , ndvi    , SWDOWN  , qroot   , sadr    ,           & ! out
                   qsubcan , qsubgrd ,                                         & ! out
                   VARSD   ,                                                   & ! mixed-RE in
                   PSI     , ATM_BC  , ATMACT  , DTFINEM , WCND    ,HTOP     , & ! mixed RE inout
                   RSINEX  )

! --------------------------------------------------------------------------------------------------
! Initial code: Guo-Yue Niu, Oct. 2007
! --------------------------------------------------------------------------------------------------
  implicit none
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters) :: parameters
  type (constants) :: rad_cons

  INTEGER                        , INTENT(IN)    :: ICE    !ice (ice = 1)
  INTEGER                        , INTENT(IN)    :: IST    !surface type 1->soil; 2->lake
  INTEGER                        , INTENT(IN)    :: VEGTYP !vegetation type 
  INTEGER                        , INTENT(IN)    :: CROPTYPE !crop type 
  INTEGER                        , INTENT(IN)    :: SOILTYP!soil type
  INTEGER                        , INTENT(IN)    :: SOILCOLOR !soil color
  INTEGER                        , INTENT(IN)    :: SLOPETYP  !slope type
  INTEGER                        , INTENT(IN)    :: NSNOW  !maximum no. of snow layers        
  INTEGER                        , INTENT(IN)    :: NSOIL  !no. of soil layers        
  INTEGER                        , INTENT(IN)    :: ILOC   !grid index
  INTEGER                        , INTENT(IN)    :: JLOC   !grid index
  REAL                           , INTENT(IN)    :: DT     !time step [sec]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !layer-bottom depth from soil surf (m)
  REAL                           , INTENT(IN)    :: Q2     !mixing ratio (kg/kg) lowest model layer
  REAL                           , INTENT(IN)    :: SFCTMP !surface air temperature [K]
  REAL                           , INTENT(IN)    :: UU     !wind speed in eastward dir (m/s)
  REAL                           , INTENT(IN)    :: VV     !wind speed in northward dir (m/s)
  REAL                           , INTENT(IN)    :: SOLDN  !downward shortwave radiation (w/m2)
  REAL                           , INTENT(IN)    :: LWDN   !downward longwave radiation (w/m2)
  REAL                           , INTENT(IN)    :: SFCPRS !pressure (pa)
  REAL                           , INTENT(INOUT) :: ZLVL   !reference height (m)
  REAL                           , INTENT(IN)    :: COSZ   !cosine solar zenith angle [0-1]
  REAL                           , INTENT(INOUT)    :: TBOT   !bottom condition for soil temp. [K]
  REAL                           , INTENT(IN)    :: TOPOSV !standard dev of DEM [m]
  REAL                           , INTENT(IN)    :: FOLN   !foliage nitrogen (%) [1-saturated]
  REAL                           , INTENT(IN)    :: SHDFAC !green vegetation fraction [0.0-1.0]
  INTEGER                        , INTENT(IN)    :: YEARLEN!Number of days in the particular year.
  REAL                           , INTENT(IN)    :: JULIAN !Julian day of year (floating point)
  REAL                           , INTENT(IN)    :: LAT    !latitude (radians)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD!ice fraction at last timestep
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: SMCEQ  !equilibrium soil water  content [m3/m3]
  REAL                           , INTENT(IN)    :: PRCPCONV ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPNONC ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPSHCV ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPSNOW ! snow entering land model [mm/s]              ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPGRPL ! graupel entering land model [mm/s]           ! MB/AN : v3.7
  REAL                           , INTENT(IN)    :: PRCPHAIL ! hail entering land model [mm/s]              ! MB/AN : v3.7

!jref:start; in 
  REAL                           , INTENT(IN)    :: QC     !cloud water mixing ratio
  REAL                           , INTENT(INOUT)    :: QSFC   !mixing ratio at lowest model layer
  REAL                           , INTENT(IN)    :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN)    :: DZ8W   !thickness of lowest layer
  REAL                           , INTENT(IN)    :: DX
  REAL                           , INTENT(IN)    :: SHDMAX  !yearly max vegetation fraction
!jref:end

! input/output : need arbitary intial values
  REAL                           , INTENT(INOUT) :: QSNOW  !snowfall [mm/s]
  REAL                           , INTENT(INOUT) :: FWET   !wetted or snowed fraction of canopy (-)
  REAL                           , INTENT(INOUT) :: SNEQVO !snow mass at last time step (mm)
  REAL                           , INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL                           , INTENT(INOUT) :: TAH    !canopy air tmeperature (k)
  REAL                           , INTENT(INOUT) :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL                           , INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL                           , INTENT(INOUT) :: CH     !sensible heat exchange coefficient
  REAL                           , INTENT(INOUT) :: TAUSS  !non-dimensional snow age
  REAL                           , INTENT(INOUT) :: KR     !=BTRAN
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: FROOT  !root fraction [-]

! prognostic variables
  INTEGER                        , INTENT(INOUT) :: ISNOW  !actual no. of snow layers [-]
  REAL                           , INTENT(INOUT) :: CANLIQ !intercepted liquid water (mm)
  REAL                           , INTENT(INOUT) :: CANICE !intercepted ice mass (mm)
  REAL                           , INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  !layer-bottom depth from snow surf [m]
  REAL                           , INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: radius !snow grain radius (um)
  REAL                           , INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL                           , INTENT(INOUT) :: TG     !ground temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow/soil temperature [k]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !liquid soil moisture [m3/m3]
  REAL                           , INTENT(INOUT) :: ZWT    !depth to water table [m]
  REAL                           , INTENT(INOUT) :: WA     !water storage in aquifer [mm]
  REAL                           , INTENT(INOUT) :: WT     !water in aquifer&saturated soil [mm]
  REAL                           , INTENT(INOUT) :: WSLAKE !lake water storage (can be neg.) (mm)
  REAL,                            INTENT(INOUT) :: SMCWTD !soil water content between bottom of the soil and water table [m3/m3]
  REAL,                            INTENT(INOUT) :: DEEPRECH !recharge to or from the water table when deep [m]
  REAL,                            INTENT(INOUT) :: RECH !recharge to or from the water table when shallow [m] (diagnostic)

  REAL                           , INTENT(INOUT) :: SOC    !soil organic carbon [g C/m2]
  REAL                           , INTENT(INOUT) :: WDOC   !wet dissolved organic carbon [g C/m2]
  REAL                           , INTENT(INOUT) :: DDOC   !dry dissolved organic  [g C/m2]
  REAL                           , INTENT(INOUT) :: MIC    !soil microbial biomass [g C/m2]
  REAL                           , INTENT(INOUT) :: WENZ   !wet soil enzyme [g C/m2]
  REAL                           , INTENT(INOUT) :: DENZ   !dry soil enzyme [g C/m2]
  REAL                           , INTENT(INOUT) :: MQ     !water stored in living tissues [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: ROOTMS !mass of live fine roots [g C/m2]

! output
  REAL                           , INTENT(OUT)   :: Z0WRF  !combined z0 sent to coupled model
  REAL                           , INTENT(OUT)   :: FSA    !total absorbed solar radiation (w/m2)
  REAL                           , INTENT(OUT)   :: FSR    !total reflected solar radiation (w/m2)
  REAL                           , INTENT(OUT)   :: FIRA   !total net LW rad (w/m2)  [+ to atm]
  REAL                           , INTENT(OUT)   :: FSH    !total sensible heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: FCEV   !canopy evap heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: FGEV   !ground evap heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: FCTR   !transpiration heat (w/m2) [+ to atm]
  REAL                           , INTENT(OUT)   :: SSOIL  !ground heat flux (w/m2)   [+ to soil]
  REAL                           , INTENT(OUT)   :: TRAD   !surface radiative temperature (k)
  REAL                                           :: TS     !surface temperature (k)
  REAL                           , INTENT(OUT)   :: ECAN   !evaporation of intercepted water (mm/s)
  REAL                           , INTENT(OUT)   :: ETRAN  !transpiration rate (mm/s)
  REAL                           , INTENT(OUT)   :: EDIR   !soil surface evaporation rate (mm/s]
  REAL                           , INTENT(OUT)   :: RUNSRF !surface runoff [mm/s] 
  REAL                           , INTENT(OUT)   :: RUNSUB !baseflow (saturation excess) [mm/s]
  REAL                           , INTENT(OUT)   :: PSN    !total photosynthesis (umol co2/m2/s) [+]
  REAL                           , INTENT(OUT)   :: APAR   !photosyn active energy by canopy (w/m2)
  REAL                           , INTENT(OUT)   :: SAV    !solar rad absorbed by veg. (w/m2)
  REAL                           , INTENT(OUT)   :: SAG    !solar rad absorbed by ground (w/m2)
  REAL                           , INTENT(OUT)   :: FSNO   !snow cover fraction on the ground (-)
  REAL                           , INTENT(OUT)   :: FVEG   !green vegetation fraction [0.0-1.0]
  REAL                           , INTENT(OUT)   :: ALBEDO !surface albedo [-]
  REAL                                           :: ERRWAT !water error [kg m{-2}]
  REAL                           , INTENT(OUT)   :: QSNBOT !snowmelt out bottom of pack [mm/s]
  REAL                           , INTENT(OUT)   :: PONDING!surface ponding [mm]
  REAL                           , INTENT(OUT)   :: PONDING1!surface ponding [mm]
  REAL                           , INTENT(OUT)   :: PONDING2!surface ponding [mm]

  REAL, DIMENSION(       1:NSOIL), INTENT(OUT)   :: SADR   !root surface area density [m2/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(OUT)   :: QROOT  !water uptake [m/s]
  REAL                           , INTENT(OUT)   :: QCO2   !co2 efflux ( g C m-2 s-1)
  real                           , INTENT(OUT)   :: VMAX   !maximum SOC decomposition rate per
                                                           !unit microbial biomass [g C m-2 [g CMIC m-2]-1 s-1]
  real                           , INTENT(OUT)   :: VMAXUP !maximum DOC uptake rate [g CDOC m-2 [g CMIC m-2]-1 s-1]
  real                           , INTENT(OUT)   :: KM     !Michaelis-Menten constant [g C m-2] for SOC  decomposition
  real                           , INTENT(OUT)   :: KMUP   !Michaelis-Menten constant [g C m-2] for DOC uptake
  real                           , INTENT(OUT)   :: EPSLON !carbon use efficiency

  REAL                           , INTENT(OUT)   :: NDVI   !NDVI
  REAL                           , INTENT(OUT)   :: QSUBCAN!sublimation/deposition from the canopy snow (mm/s)
  REAL                           , INTENT(OUT)   :: QSUBGRD!sublimation/deposition from the ground snow (mm/s)

!jref:start; output
  REAL                           , INTENT(OUT)     :: T2MV   !2-m air temperature over vegetated part [k]
  REAL                           , INTENT(OUT)     :: T2MB   !2-m air temperature over bare ground part [k]
  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: BGAP
  REAL, INTENT(OUT) :: WGAP
  REAL, DIMENSION(1:2)           , INTENT(OUT)   :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2)           , INTENT(OUT)   :: ALBSNI   !snow albedo (diffuse)
  REAL, INTENT(OUT) :: TGV
  REAL, INTENT(OUT) :: TGB
  REAL              :: Q1
  REAL, INTENT(OUT) :: EMISSI
!jref:end

! local
  INTEGER                                        :: IZ     !do-loop index
  INTEGER, DIMENSION(-NSNOW+1:NSOIL)             :: IMELT  !phase change index [1-melt; 2-freeze]
  REAL,    DIMENSION(-NSNOW+1:NSOIL)             :: XM     !melting or freezing water [kg/m2]
  REAL                                           :: CMC    !intercepted water (CANICE+CANLIQ) (mm)
  REAL                                           :: TAUX   !wind stress: e-w (n/m2)
  REAL                                           :: TAUY   !wind stress: n-s (n/m2)
  REAL                                           :: RHOAIR !density air (kg/m3)
  REAL, DIMENSION(-NSNOW+1:NSOIL)                :: DZSNSO !snow/soil layer thickness [m]
  REAL                                           :: THAIR  !potential temperature (k)
  REAL                                           :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                                           :: EAIR   !vapor pressure air (pa)
  REAL, DIMENSION(       1:    2)                :: SOLAD  !incoming direct solar rad (w/m2)
  REAL, DIMENSION(       1:    2)                :: SOLAI  !incoming diffuse solar rad (w/m2)
  REAL                                           :: QPRECC !convective precipitation (mm/s)
  REAL                                           :: QPRECL !large-scale precipitation (mm/s)
  REAL                                           :: IGS    !growing season index (0=off, 1=on)
  REAL                                           :: ELAI   !leaf area index, after burying by snow
  REAL                                           :: ESAI   !stem area index, after burying by snow
  REAL                                           :: BEVAP  !soil water evaporation factor (0 - 1)
  REAL, DIMENSION(       1:NSOIL)                :: BTRANI !Soil water transpiration factor (0 - 1)
  REAL                                           :: BTRAN  !soil water transpiration factor (0 - 1)
!  REAL                                           :: QIN    !groundwater recharge [mm/s]
  REAL                                           :: QDIS   !groundwater discharge [mm/s]
  REAL, DIMENSION(       1:NSOIL)                :: SICE   !soil ice content (m3/m3)
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNICEV !partial volume ice of snow [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0)                :: SNLIQV !partial volume liq of snow [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0)                :: EPORE  !effective porosity [m3/m3]
  REAL                                           :: TOTSC  !total soil carbon (g/m2)
  REAL                                           :: TOTLB  !total living carbon (g/m2)
  REAL                                           :: T2M    !2-meter air temperature (k)
  REAL                                           :: QDEW   !ground surface dew rate [mm/s]
  REAL                                           :: QVAP   !ground surface evap. rate [mm/s]
  REAL                                           :: LATHEA !latent heat [j/kg]
  REAL                                           :: SWDOWN !downward solar [w/m2]
  REAL                                           :: QMELT  !snowmelt [mm/s]
  REAL                                           :: BEG_WB !water storage at begin of a step [mm]
  REAL,INTENT(OUT)                                              :: IRC    !canopy net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: IRG    !ground net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHC    !canopy sen. heat [w/m2]   [+ to atm]
  REAL,INTENT(OUT)                                              :: SHG    !ground sen. heat [w/m2]   [+ to atm]
  REAL,INTENT(OUT)                                              :: EVG    !ground evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: GHV    !ground heat flux [w/m2]  [+ to soil]
  REAL,INTENT(OUT)                                              :: IRB    !net longwave rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHB    !sensible heat [w/m2]     [+ to atm]
  REAL,INTENT(OUT)                                              :: EVB    !evaporation heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: GHB    !ground heat flux [w/m2] [+ to soil]
  REAL,INTENT(OUT)                                              :: EVC    !canopy evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: TR     !transpiration heat [w/m2] [+ to atm]
  REAL, INTENT(OUT)   :: FPICE   !snow fraction in precipitation
  REAL, INTENT(OUT)   :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL, INTENT(OUT)   :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL, INTENT(OUT)   :: PAHB    !precipitation advected heat - bare ground net (W/m2)
  REAL, INTENT(OUT)                                           :: PAH     !precipitation advected heat - total (W/m2)

!jref:start 
  REAL                                           :: FSRV
  REAL                                           :: FSRG
  REAL,INTENT(OUT)                               :: Q2V
  REAL,INTENT(OUT)                               :: Q2B
  REAL :: Q2E
  REAL :: QFX
  REAL,INTENT(OUT)                               :: CHV    !sensible heat exchange coefficient over vegetated fraction
  REAL,INTENT(OUT)                               :: CHB    !sensible heat exchange coefficient over bare-ground
  REAL,INTENT(OUT)                               :: CHLEAF !leaf exchange coefficient
  REAL,INTENT(OUT)                               :: CHUC   !under canopy exchange coefficient
  REAL,INTENT(OUT)                               :: CHV2    !sensible heat exchange coefficient over vegetated fraction
  REAL,INTENT(OUT)                               :: CHB2    !sensible heat exchange coefficient over bare-ground
!jref:end  

! carbon
! inputs
  REAL                           , INTENT(IN)    :: CO2AIR !atmospheric co2 concentration (pa)
  REAL                           , INTENT(IN)    :: O2AIR  !atmospheric o2 concentration (pa)

! inputs and outputs : prognostic variables
  REAL                        , INTENT(INOUT)    :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT)    :: RTMASS !mass of fine roots [g/m2]
  REAL                        , INTENT(INOUT)    :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT)    :: WOOD   !mass of wood (incl. woody roots) [g/m2]
  REAL                        , INTENT(INOUT)    :: STBLCP !stable carbon in deep soil [g/m2]
  REAL                        , INTENT(INOUT)    :: FASTCP !short-lived carbon, shallow soil [g/m2]
  REAL                        , INTENT(INOUT)    :: LAI    !leaf area index [-]
  REAL                        , INTENT(INOUT)    :: SAI    !stem area index [-]
  REAL                        , INTENT(INOUT)    :: GRAIN  !grain mass [g/m2]
  REAL                        , INTENT(INOUT)    :: GDD    !growing degree days
  INTEGER                     , INTENT(INOUT)    :: PGS    !plant growing stage [-]

! outputs
  REAL                          , INTENT(OUT)    :: NEE    !net ecosys exchange (g/m2/s CO2)
  REAL                          , INTENT(OUT)    :: GPP    !net instantaneous assimilation [g/m2/s C]
  REAL                          , INTENT(OUT)    :: NPP    !net primary productivity [g/m2/s C]
  REAL                                           :: AUTORS !net ecosystem respiration (g/m2/s C)
  REAL                                           :: HETERS !organic respiration (g/m2/s C)
  REAL                                           :: TROOT  !root-zone averaged temperature (k)
  REAL                                           :: BDFALL   !bulk density of new snow (kg/m3)    ! MB/AN: v3.7
  REAL                                           :: RAIN     !rain rate                   (mm/s)  ! MB/AN: v3.7
  REAL                                           :: SNOW     !liquid equivalent snow rate (mm/s)  ! MB/AN: v3.7
  REAL                                           :: FP                                            ! MB/AN: v3.7
  REAL                                           :: PRCP                                          ! MB/AN: v3.7
!more local variables for precip heat MB
  REAL                                           :: QINTR   !interception rate for rain (mm/s)
  REAL                                           :: QDRIPR  !drip rate for rain (mm/s)
  REAL                                           :: QTHROR  !throughfall for rain (mm/s)
  REAL                                           :: QINTS   !interception (loading) rate for snowfall (mm/s)
  REAL                                           :: QDRIPS  !drip (unloading) rate for intercepted snow (mm/s)
  REAL                                           :: QTHROS  !throughfall of snowfall (mm/s)
  REAL                                           :: QRAIN   !rain at ground srf (mm/s) [+]
  REAL                                           :: SNOWHIN !snow depth increasing rate (m/s)
  REAL                                 :: LATHEAV !latent heat vap./sublimation (j/kg)
  REAL                                 :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                             :: FROZEN_GROUND ! used to define latent heat pathway
  LOGICAL                             :: FROZEN_CANOPY ! used to define latent heat pathway
 
  REAL                                           :: QSEVA  !soil surface evap rate [m/s]
  REAL, DIMENSION(       1:NSOIL)                :: ETRANI !transpiration rate [m/s] [+]
  REAL                                           :: PDDUM  !surface infiltration rate [m/s]
  REAL                                           :: CANHS  !canopy heat storage change (w/m2)
  REAL                                           :: CW_BEG
  REAL                                           :: SW_BEG,SW_END,ICEDRIP

! Mixed Richards' equation:

  INTEGER ,INTENT(IN)                     :: VARSD  !if variable soil depth is activated see noah_driver
  INTEGER                 , INTENT(INOUT) :: ATM_BC !ATM_BC: 0->Neuman (flux) ;1->Dirichlet (state)
  REAL                    , INTENT(INOUT) :: ATMACT
  REAL,                     INTENT(INOUT) :: DTFINEM
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: WCND   !hydraulic conductivity (m/s)

  REAL                    , INTENT(INOUT) :: HTOP               !surface ponding height [mm]

  REAL                    , INTENT(OUT)   :: RSINEX             !infiltration excess runoff [mm/s]
  REAL                    , INTENT(INOUT) :: QIN                !groundwater recharge [mm/s]
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: PSI                !pressure head (m)

  REAL, DIMENSION(1:NSOIL) :: SICEO  !soil ice content [m3/m3]
  REAl, DIMENSION(1:NSOIL) :: SH2OO  !liq soil water at previous time
  REAL                     :: QDRYC  !dry limit correction to EDIR [mm/s]
  REAL, DIMENSION(1:NSOIL) :: DMICE  !change rate of solid ice [m/s]
  REAL, DIMENSION(1:NSOIL) :: DSH2O  !change rate of liquid soil moisture [m/s]

  ! INTENT (OUT) variables need to be assigned a value.  These normally get assigned values
  ! only if DVEG == 2.
  nee   = 0.0
  npp   = 0.0
  gpp   = 0.0
  PAHV  = 0.
  PAHG  = 0.
  PAHB  = 0.
  PAH   = 0.

  QDRYC = 0.

! --------------------------------------------------------------------------------------------------

   CALL TRANSFER_MP_PARAMETERS(VEGTYP,SOILTYP,NSOIL,  &
              SLOPETYP,SOILCOLOR,CROPTYPE,parameters,ILOC,JLOC)

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    !write(*,*) '----------after TRANSFER-------------------------'
    !write(*,*) 'SWDOWN           =',SWDOWN
    !write(*,*) 'SOLDN   , LWDN=',SOLDN   , LWDN
    !write(*,*) "SOILTYP = ",SOILTYP
    !END IF

! re-process atmospheric forcing

   CALL ATM (parameters,SFCPRS  ,SFCTMP   ,Q2      ,                            &
             PRCPCONV, PRCPNONC,PRCPSHCV,PRCPSNOW,PRCPGRPL,PRCPHAIL, &
             SOLDN   ,COSZ     ,THAIR   ,QAIR    ,                   & 
             EAIR    ,RHOAIR   ,QPRECC  ,QPRECL  ,SOLAD   ,SOLAI   , &
             SWDOWN  ,BDFALL   ,RAIN    ,SNOW    ,FP      ,FPICE   , PRCP )     

! snow/soil layer thickness (m)

     DO IZ = ISNOW+1, NSOIL
         IF(IZ == ISNOW+1) THEN
           DZSNSO(IZ) = - ZSNSO(IZ)
         ELSE
           DZSNSO(IZ) = ZSNSO(IZ-1) - ZSNSO(IZ)
         END IF
     END DO

! root-zone temperature

     TROOT  = 0.
     DO IZ=1,parameters%NROOT
        TROOT = TROOT + STC(IZ)*DZSNSO(IZ)/(-ZSOIL(parameters%NROOT))
     ENDDO

! total water storage for water balance check
    
     IF(IST == 1) THEN

    !BEG_WB = 0.
    !DO IZ = 1,NSOIL
    !   BEG_WB = BEG_WB + SMC(IZ) * DZSNSO(IZ) * 1000.
    !END DO

     BEG_WB = SUM(SMC(1:NSOIL) * DZSNSO(1:NSOIL) * 1000.)

        IF (OPT_ROOT == 1) BEG_WB = BEG_WB + CANLIQ + CANICE + SNEQV + WA + MQ + HTOP
       !IF (OPT_ROOT == 2) BEG_WB = BEG_WB + CANLIQ + CANICE + SNEQV + WA
        IF (OPT_ROOT == 2) BEG_WB = BEG_WB + CANLIQ + CANICE + SNEQV + WA + HTOP

     END IF

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    !  write(*,*) '--1--,BEG_WB,CANLIQ,CANICE,SNEQV,WA,MQ,HTOP=',BEG_WB,CANLIQ,CANICE,SNEQV,WA,MQ,HTOP
    !END IF

! vegetation phenology

     CALL PHENOLOGY (parameters,VEGTYP ,croptype, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & !in
                     LAI    , SAI    , TROOT  , ELAI    , ESAI   ,IGS, PGS, ILOC,JLOC)

!input GVF should be consistent with LAI
     IF(DVEG == 1 .or. DVEG == 6 .or. DVEG == 7) THEN
        FVEG = SHDFAC
        IF(FVEG <= 0.05) FVEG = 0.05
     ELSE IF (DVEG == 2 .or. DVEG == 3 .or. DVEG == 8) THEN
        FVEG = 1.-EXP(-0.52*(LAI+SAI))
        IF(FVEG <= 0.05) FVEG = 0.05
     ELSE IF (DVEG == 4 .or. DVEG == 5 .or. DVEG == 9 .or. DVEG == 10) THEN
        FVEG = SHDMAX
        IF(FVEG <= 0.05) FVEG = 0.05
     ELSE
        WRITE(*,*) "-------- FATAL CALLED IN SFLX -----------"
        CALL wrf_error_fatal("Namelist parameter DVEG unknown") 
     ENDIF
!niu     IF(parameters%urban_flag .OR. VEGTYP == parameters%ISBARREN) FVEG = 0.0
     IF(VEGTYP == parameters%ISBARREN) FVEG = 0.0
     IF(ELAI+ESAI == 0.0) FVEG = 0.0

!niu water balance: CW_ERR = CW_BEG+(QINTR+QINTS-ICEDRIP)*DT-(CANICE+CANLIQ); CW_BEG = CANICE + CANLIQ
    CALL PRECIP_HEAT(parameters,ILOC   ,JLOC   ,VEGTYP ,DT     ,UU     ,VV     , & !in
                     ELAI   ,ESAI   ,FVEG   ,IST    ,                 & !in
                     BDFALL ,RAIN   ,SNOW   ,FP     ,                 & !in
                     CANLIQ ,CANICE ,TV     ,SFCTMP ,TG     ,         & !in
                     QINTR  ,QDRIPR ,QTHROR ,QINTS  ,QDRIPS ,QTHROS , & !out
                     PAHV   ,PAHG   ,PAHB   ,QRAIN  ,QSNOW  ,SNOWHIN, & !out
	             FWET   ,CMC    ,ICEDRIP                        )   !out

! compute energy budget (momentum & energy fluxes and phase changes) 

    SICE(:)  = MAX(0.0, SMC(:) - SH2O(:))  !mixed-form 
    SICEO(:) = SICE(:)                     !mixed-form

   !IF(ILOC == 137 .and. JLOC == 7) THEN
   !     write(*,'(a6,13F12.5)') 'SICE =',SICE*DZSNSO(1:NSOIL)*1000.
   !     write(*,'(a6,13F12.5)') 'SMC  =',SMC*DZSNSO(1:NSOIL)*1000.
   !     write(*,'(a6,13F12.5)') 'SH2O =',SH2O*DZSNSO(1:NSOIL)*1000.
   !END IF

    TBOT = MAX(287.,TBOT)      !niu added for not frozen from the bottom

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    !  write(*,*) 'SICE(1:NSOIL)*DZSNSO(1:NSOIL)*1000.=',SICE(1:NSOIL)*DZSNSO(1:NSOIL)*1000.
    !write(*,*) '----------before ENERGY-------------------------'
    !write(*,*) "PSI=",PSI
    !write(*,*) "(SH2O(IZ),IZ=1,NSOIL)",(SH2O(IZ),IZ=1,NSOIL)
    !write(*,*) "(SMC(IZ),IZ=1,NSOIL)",(SMC(IZ),IZ=1,NSOIL)
    !write(*,*) "(SICE(IZ),IZ=1,NSOIL)",(SICE(IZ),IZ=1,NSOIL)
    !write(*,*) "SOILTYP = ",SOILTYP
    !write(*,*) 'SMCMAX,VGN,VGALPHA,SMCR',parameters%SMCMAX(1),parameters%VGN(1), &
    !                 parameters%VGPSAT(1),parameters%SMCR(1)
    !write(*,*) "MQ == ",MQ 
    !write(*,*) "HTOP == ",HTOP
    !write(*,*) "WA=",WA
    !END IF

    CALL ENERGY (parameters,rad_cons,ICE    ,VEGTYP ,IST    ,NSNOW  ,NSOIL  , & !in
                 ISNOW  ,DT     ,RHOAIR ,SFCPRS ,QAIR   , & !in
                 SFCTMP ,THAIR  ,LWDN   ,UU     ,VV     ,ZLVL   , & !in
                 CO2AIR ,O2AIR  ,SOLAD  ,SOLAI  ,COSZ   ,IGS    , & !in
                 EAIR   ,TBOT   ,ZSNSO  ,ZSOIL  ,TOPOSV , & !in
                 ELAI   ,ESAI   ,FWET   ,FOLN   ,         & !in
                 FVEG   ,PAHV   ,PAHG   ,PAHB   ,                 & !in
                 QSNOW  ,DZSNSO ,LAT    ,CANLIQ ,CANICE ,iloc, jloc , & !in
		 Z0WRF  ,FROOT  ,KR     ,MQ     ,                     & !in
                 IMELT  ,SNICEV ,SNLIQV ,EPORE  ,T2M    ,FSNO   , & !out
                 SAV    ,SAG    ,QMELT  ,FSA    ,FSR    ,TAUX   , & !out
                 TAUY   ,FIRA   ,FSH    ,FCEV   ,FGEV   ,FCTR   , & !out
                 TRAD   ,PSN    ,APAR   ,SSOIL  ,BTRANI ,BTRAN  , & !out
                 PONDING,TS     ,LATHEAV,LATHEAG,frozen_canopy  ,FROZEN_GROUND,                         & !out
                 NDVI   ,PSI    ,CANHS  ,SICE   ,DMICE  , & !out
                 TV     ,TG     ,STC    ,SNOWH  ,EAH    ,TAH    , & !inout
                 SNEQVO ,SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & !inout
                 ALBOLD ,CM     ,CH     ,DX     ,DZ8W   ,Q2     , & !inout
                 TAUSS  ,                                         & !inout
!jref:start
                 QC     ,QSFC   ,PSFC   , & !in 
                 T2MV   ,T2MB  ,FSRV   , &
                 FSRG   ,RSSUN   ,RSSHA ,ALBSND  ,ALBSNI, BGAP   ,WGAP, TGV,TGB,&
                 Q1     ,Q2V    ,Q2B    ,Q2E    ,CHV   ,CHB     , & !out
                 EMISSI ,PAH    ,                                 &
                 SHG,SHC,SHB,EVG,EVB,GHV,GHB,IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2, & !out
                 radius ,XM)
!jref:end

    SNEQVO  = SNEQV

    QVAP = MAX( FGEV/LATHEAG, 0.)       ! positive part of fgev; Barlage change to ground v3.6
    QDEW = ABS( MIN(FGEV/LATHEAG, 0.))  ! negative part of fgev
    EDIR = QVAP - QDEW

! compute water budgets (water storages, ET components, and runoff)

     SH2OO(1:NSOIL) = SH2O(1:NSOIL)

    IF(ILOC == 137 .and. JLOC == 7) THEN
   ! write(*,*) '----------before WATER-------------------------'
     write(*,*) "EDIR =",EDIR*DT
   ! write(*,*) "QDRYC=",QDRYC*DT
   ! write(*,*) "QDEW =",QDEW*DT
   ! write(*,*) "PSI=",PSI
   ! write(*,*) "(SH2O(IZ),IZ=1,NSOIL)",(SH2O(IZ),IZ=1,NSOIL)
   ! write(*,*) "(SMC(IZ),IZ=1,NSOIL)",(SMC(IZ),IZ=1,NSOIL)
   ! write(*,*) "(SICE(IZ),IZ=1,NSOIL)",(SICE(IZ),IZ=1,NSOIL)
    END IF

     CALL WATER (parameters,rad_cons,VEGTYP ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & !in
                 VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !in
                 ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & !in
                 FICEOLD,PONDING,TG     ,IST    ,FVEG   ,TOPOSV , & !in
                 iloc   ,jloc   ,SMCEQ  ,TS     ,XM     , & !in
                 BDFALL ,FP     ,RAIN   ,SNOW   ,                 & !in  MB/AN: v3.7
		 QSNOW  ,QRAIN  ,SNOWHIN,LATHEAV,LATHEAG,frozen_canopy,FROZEN_GROUND,  & !in  MB
                 ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & !inout
                 SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & !inout
                 SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & !inout
                 SMCWTD ,DEEPRECH,RECH  ,radius                 , & !inout
                 CMC    ,ECAN   ,ETRAN  ,FWET   ,RUNSRF ,RUNSUB , & !out
                 QIN    ,QDIS   ,PONDING1       ,PONDING2,&
                 QSNBOT ,QSUBCAN,QSUBGRD ,                        &
                        VARSD  ,DMICE  , & !Mixed-RE in
                        ATM_BC ,PSI    ,DSH2O  ,ATMACT ,  & !Mixed-RE inout
                        DTFINEM,SICEO  ,HTOP   ,          & !Mixed-RE inout
                        QDRYC  ,WCND   ,RSINEX ,          & !Mixed-RE out
                        ROOTMS ,LFMASS ,RTMASS ,STMASS ,WOOD   ,  &
                        MQ     ,KR     ,QROOT  ,FROOT  ,SADR   ) !out

     EDIR = EDIR - QDRYC

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) '----------after water-------------------------'
    ! write(*,*) "EDIR =",EDIR*DT
    ! write(*,*) "QDRYC=",QDRYC*DT
    ! write(*,*) "PSI=",PSI
    !    write(*,'(a6,13F12.5)') 'SICE =',SICE*DZSNSO(1:NSOIL)*1000.
    !    write(*,'(a6,13F12.5)') 'SMC  =',SMC*DZSNSO(1:NSOIL)*1000.
    !    write(*,'(a6,13F12.5)') 'SH2O =',SH2O*DZSNSO(1:NSOIL)*1000.
    !END IF

! compute carbon budgets (carbon storages and co2 & bvoc fluxes)

   IF (DVEG == 2 .OR. DVEG == 5 .OR. DVEG == 6 .OR. (DVEG == 10 .and. CROPTYPE == 0)) THEN

   !niu water balance: SW_ERR=SW_BEG+(QROOTT-ETRAN)*DT-SW_END : SW_BEG = sum of soil water
    CALL CARBON (parameters, NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  , & !in
                 DZSNSO ,STC    ,SMC    ,TV     ,TG     ,PSN    , & !in
                 FOLN   ,BTRAN  ,APAR   ,FVEG   ,IGS    , & !in
                 TROOT  ,IST    ,LAT    ,SH2OO  ,SICEO  , & !in 
                 SH2O   ,SICE   ,KR     , & !in
                 ILOC   ,JLOC   ,PSI    ,ETRAN  ,O2AIR  , & !in
                 LFMASS ,ROOTMS ,STMASS ,WOOD   ,SOC    ,WDOC   , & !inout
                 DDOC   ,MIC    ,WENZ   ,DENZ   ,MQ     , & !inout
                 SADR   ,RTMASS ,FASTCP ,STBLCP , & !inout
                 GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  , & !out
                 TOTLB  ,LAI    ,SAI    ,QCO2   ,VMAX   ,KM     , & !out
                 VMAXUP ,KMUP   ,EPSLON ,FROOT  )   !out
   END IF

   IF (DVEG == 10 .and. CROPTYPE > 0) THEN
    CALL CARBON_CROP (parameters,NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  ,JULIAN , & !in 
                         DZSNSO ,STC    ,SMC    ,TV     ,PSN    ,FOLN   ,BTRAN  , & !in
			 SOLDN  ,T2M    ,                                         & !in
                         LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP ,GRAIN  , & !inout
			 LAI    ,SAI    ,GDD    ,                                 & !inout
                         GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  ,TOTLB, PGS    ) !out
   END IF

   DO IZ = 1,NSOIL
         SMC(IZ) = SH2O(IZ) + SICE(IZ)
   END DO
   
! water and energy balance check

     CALL ERROR (parameters,SWDOWN ,FSA    ,FSR    ,FIRA   ,FSH    ,FCEV   , & !in
                 FGEV   ,FCTR   ,SSOIL  ,BEG_WB ,CANLIQ ,CANICE , & !in
                 SNEQV  ,WA     ,SMC    ,DZSNSO ,PRCP   ,ECAN   , & !in
                 ETRAN  ,EDIR   ,RUNSRF ,RUNSUB ,DT     ,NSOIL  , & !in
                 NSNOW  ,IST    ,ERRWAT ,ILOC   , JLOC  ,FVEG   , &
                 SAV    ,SAG    ,FSRV   ,FSRG   ,ZWT    ,PAH    , &
                 PAHV   ,PAHG   ,PAHB   ,MQ     ,CANHS  ,SH2O   , &
                 ISNOW  , &   !in ( Except ERRWAT, which is out )
                 ETRANI ,QIN    ,PSI    ,QROOT  , &
                 SICE   ,VEGTYP ,SOILTYP,SH2OO  ,DMICE  ,DSH2O  ,HTOP )

! urban - jref
    QFX = ETRAN + ECAN + EDIR
    IF ( parameters%urban_flag ) THEN
       QSFC = QFX/(RHOAIR*CH) + QAIR
       Q2B = QSFC
    END IF

    IF(SNOWH <= 1.E-6 .OR. SNEQV <= 1.E-3) THEN
     SNOWH = 0.0
     SNEQV = 0.0
    END IF

    IF(SWDOWN.NE.0.) THEN
      ALBEDO = FSR / SWDOWN
    ELSE
      ALBEDO = -1.0E20
    END IF
    
  END SUBROUTINE NOAHMP_SFLX

!== begin atm ======================================================================================

  SUBROUTINE ATM (parameters,SFCPRS  ,SFCTMP   ,Q2      ,                             &
                  PRCPCONV,PRCPNONC ,PRCPSHCV,PRCPSNOW,PRCPGRPL,PRCPHAIL , &
                  SOLDN   ,COSZ     ,THAIR   ,QAIR    ,                    & 
                  EAIR    ,RHOAIR   ,QPRECC  ,QPRECL  ,SOLAD   , SOLAI   , &
		  SWDOWN  ,BDFALL   ,RAIN    ,SNOW    ,FP      , FPICE   ,PRCP )     
! --------------------------------------------------------------------------------------------------
! re-process atmospheric forcing
! ----------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
  REAL                          , INTENT(IN)  :: SFCPRS !pressure (pa)
  REAL                          , INTENT(IN)  :: SFCTMP !surface air temperature [k]
  REAL                          , INTENT(IN)  :: Q2     !mixing ratio (kg/kg)
  REAL                          , INTENT(IN)  :: PRCPCONV ! convective precipitation entering  [mm/s]    ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPNONC ! non-convective precipitation entering [mm/s] ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPSHCV ! shallow convective precip entering  [mm/s]   ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPSNOW ! snow entering land model [mm/s]              ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPGRPL ! graupel entering land model [mm/s]           ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: PRCPHAIL ! hail entering land model [mm/s]              ! MB/AN : v3.7
  REAL                          , INTENT(IN)  :: SOLDN  !downward shortwave radiation (w/m2)
  REAL                          , INTENT(IN)  :: COSZ   !cosine solar zenith angle [0-1]

! outputs

  REAL                          , INTENT(OUT) :: THAIR  !potential temperature (k)
  REAL                          , INTENT(OUT) :: QAIR   !specific humidity (kg/kg) (q2/(1+q2))
  REAL                          , INTENT(OUT) :: EAIR   !vapor pressure air (pa)
  REAL                          , INTENT(OUT) :: RHOAIR !density air (kg/m3)
  REAL                          , INTENT(OUT) :: QPRECC !convective precipitation (mm/s)
  REAL                          , INTENT(OUT) :: QPRECL !large-scale precipitation (mm/s)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAD  !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(       1:   2), INTENT(OUT) :: SOLAI  !incoming diffuse solar radiation (w/m2)
  REAL                          , INTENT(OUT) :: SWDOWN !downward solar filtered by sun angle [w/m2]
  REAL                          , INTENT(OUT) :: BDFALL  !!bulk density of snowfall (kg/m3) AJN
  REAL                          , INTENT(OUT) :: RAIN    !rainfall (mm/s) AJN
  REAL                          , INTENT(OUT) :: SNOW    !liquid equivalent snowfall (mm/s) AJN
  REAL                          , INTENT(OUT) :: FP      !fraction of area receiving precipitation  AJN
  REAL                          , INTENT(OUT) :: FPICE   !fraction of ice                AJN
  REAL                          , INTENT(OUT) :: PRCP    !total precipitation [mm/s]     ! MB/AN : v3.7

!locals

  REAL                                        :: PAIR   !atm bottom level pressure (pa)
  REAL                                        :: PRCP_FROZEN   !total frozen precipitation [mm/s] ! MB/AN : v3.7
  REAL, PARAMETER                             :: RHO_GRPL = 500.0  ! graupel bulk density [kg/m3] ! MB/AN : v3.7
  REAL, PARAMETER                             :: RHO_HAIL = 917.0  ! hail bulk density [kg/m3]    ! MB/AN : v3.7

  INTEGER :: ITER,NITER
  DATA NITER /100/
  REAL                                        :: TWET   !wet-bulb T (C) 
  REAL :: LATHEA,  DE, ESATAIR, GAMMA
  REAL :: T, TDC       !Kelvin to degree Celsius with limit -50 to +50
  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )

! --------------------------------------------------------------------------------------------------

!jref: seems like PAIR should be P1000mb??
       PAIR   = SFCPRS                   ! atm bottom level pressure (pa)
       THAIR  = SFCTMP * (SFCPRS/PAIR)**(RAIR/CPAIR) 

       QAIR   = Q2                       ! In WRF, driver converts to specific humidity

       EAIR   = QAIR*SFCPRS / (0.622+0.378*QAIR)
       RHOAIR = (SFCPRS-0.378*EAIR) / (RAIR*SFCTMP)

      !IF(SOLDN <= 0.) THEN 
       IF(COSZ <= 0.) THEN 
          SWDOWN = 0.
       ELSE
          SWDOWN = SOLDN
       END IF 

       IF(COSZ <= 0.) THEN 
          SOLAD(1) = 0.0     ! direct  vis
          SOLAD(2) = 0.0     ! direct  nir
          SOLAI(1) = SWDOWN*0.5     ! diffuse vis
          SOLAI(2) = SWDOWN*0.5     ! diffuse nir
       ELSE
          SOLAD(1) = SWDOWN*0.7*0.5     ! direct  vis
          SOLAD(2) = SWDOWN*0.7*0.5     ! direct  nir
          SOLAI(1) = SWDOWN*0.3*0.5     ! diffuse vis
          SOLAI(2) = SWDOWN*0.3*0.5     ! diffuse nir
       END IF 

       PRCP = PRCPCONV + PRCPNONC + PRCPSHCV

       IF(OPT_SNF == 4) THEN
         QPRECC = PRCPCONV + PRCPSHCV
	 QPRECL = PRCPNONC
       ELSE
         QPRECC = 0.10 * PRCP          ! should be from the atmospheric model
         QPRECL = 0.90 * PRCP          ! should be from the atmospheric model
       END IF

! fractional area that receives precipitation (see, Niu et al. 2005)
   
    FP = 0.0
    IF(QPRECC + QPRECL > 0.) & 
       FP = (QPRECC + QPRECL) / (10.*QPRECC + QPRECL)

! partition precipitation into rain and snow. Moved from CANWAT MB/AN: v3.7

! Jordan (1991)

     IF(OPT_SNF == 1) THEN
       IF(SFCTMP > TFRZ+2.5)THEN
           FPICE = 0.
       ELSE
         IF(SFCTMP <= TFRZ+0.5)THEN
           FPICE = 1.0
         ELSE IF(SFCTMP <= TFRZ+2.)THEN
           FPICE = 1.-(-54.632 + 0.2*SFCTMP)
         ELSE
           FPICE = 0.6
         ENDIF
       ENDIF
     ENDIF

     IF(OPT_SNF == 2) THEN
       IF(SFCTMP >= TFRZ+2.2) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF

     IF(OPT_SNF == 3) THEN
       IF(SFCTMP >= TFRZ) THEN
           FPICE = 0.
       ELSE
           FPICE = 1.0
       ENDIF
     ENDIF

! Behrangi et al. (2018) Q J R Meteorol Soc. 2018;144 (Suppl. 1):89–102

     IF(OPT_SNF == 5) THEN
        IF (SFCTMP .GT. TFRZ) THEN
           LATHEA = HVAP
        ELSE
           LATHEA = HSUB
        END IF
        GAMMA = CPAIR*SFCPRS/(0.622*LATHEA)

        ! wet-bulb temperature

        ! first guess using the 1/3 rule (see DOI:10.1175/BAMS-D-16-0246.1: BAMS (2017)a)

        TWET    = TDC(SFCTMP) - 5.0  !first guess in C

        IF(PRCP > 0.0) THEN
          DO ITER = 1, NITER
               ESATAIR = 610.8 * EXP((17.27*TWET)/(237.3+TWET))
               DE      = ESATAIR-EAIR

                IF(DE >= 0) THEN
                   TWET    = TWET - DE/GAMMA
                ELSE
                   TWET    = TWET - DE/GAMMA / (FLOAT(ITER)) * 3.
                END IF

               TWET = MAX(-50., TWET)

               IF(ABS(DE).LE.2.0) EXIT

          END DO
        END IF

        FPICE = 1.0/(1.0+5.E-5*exp(2.0*(TWET+4.))) !Figure 5c of Behrangi et al. (2018)
     ENDIF

! Hedstrom NR and JW Pomeroy (1998), Hydrol. Processes, 12, 1611-1625
! fresh snow density

!niu     BDFALL = MIN(120.,67.92+51.25*EXP((SFCTMP-TFRZ)/2.59))       !MB/AN: change to MIN  
     BDFALL = 67.92+51.25*EXP(MIN(2.5,(SFCTMP-TFRZ))/2.59)
     IF(OPT_SNF == 4) THEN
        PRCP_FROZEN = PRCPSNOW + PRCPGRPL + PRCPHAIL
        IF(PRCPNONC > 0. .and. PRCP_FROZEN > 0.) THEN
	  FPICE = MIN(1.0,PRCP_FROZEN/PRCPNONC)
	  FPICE = MAX(0.0,FPICE)
	  BDFALL = BDFALL*(PRCPSNOW/PRCP_FROZEN) + RHO_GRPL*(PRCPGRPL/PRCP_FROZEN) + &
	             RHO_HAIL*(PRCPHAIL/PRCP_FROZEN)
	ELSE
	  FPICE = 0.0
        ENDIF
	
     ENDIF

     RAIN   = PRCP * (1.-FPICE)
     SNOW   = PRCP * FPICE


  END SUBROUTINE ATM

!== begin phenology ================================================================================

  SUBROUTINE PHENOLOGY (parameters,VEGTYP ,croptype, SNOWH  , TV     , LAT   , YEARLEN , JULIAN , & !in
                        LAI    , SAI    , TROOT  , ELAI    , ESAI   , IGS, PGS,ILOC,JLOC)

! --------------------------------------------------------------------------------------------------
! vegetation phenology considering vegeation canopy being buries by snow and evolution in time
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN)  :: ILOC    !grid index
  INTEGER,INTENT(IN)  :: JLOC    !grid index
  INTEGER                , INTENT(IN   ) :: VEGTYP !vegetation type 
  INTEGER                , INTENT(IN   ) :: CROPTYPE !vegetation type 
  REAL                   , INTENT(IN   ) :: SNOWH  !snow height [m]
  REAL                   , INTENT(IN   ) :: TV     !vegetation temperature (k)
  REAL                   , INTENT(IN   ) :: LAT    !latitude (radians)
  INTEGER                , INTENT(IN   ) :: YEARLEN!Number of days in the particular year
  REAL                   , INTENT(IN   ) :: JULIAN !Julian day of year (fractional) ( 0 <= JULIAN < YEARLEN )
  real                   , INTENT(IN   ) :: TROOT  !root-zone averaged temperature (k)
  REAL                   , INTENT(INOUT) :: LAI    !LAI, unadjusted for burying by snow
  REAL                   , INTENT(INOUT) :: SAI    !SAI, unadjusted for burying by snow

! outputs
  REAL                   , INTENT(OUT  ) :: ELAI   !leaf area index, after burying by snow
  REAL                   , INTENT(OUT  ) :: ESAI   !stem area index, after burying by snow
  REAL                   , INTENT(OUT  ) :: IGS    !growing season index (0=off, 1=on)
  INTEGER                , INTENT(IN   ) :: PGS    !plant growing stage

! locals

  REAL                                   :: DB     !thickness of canopy buried by snow (m)
  REAL                                   :: FB     !fraction of canopy buried by snow
  REAL                                   :: SNOWHC !critical snow depth at which short vege
                                                   !is fully covered by snow

  INTEGER                                :: K       !index
  INTEGER                                :: IT1,IT2 !interpolation months
  REAL                                   :: DAY     !current day of year ( 0 <= DAY < YEARLEN )
  REAL                                   :: WT1,WT2 !interpolation weights
  REAL                                   :: T       !current month (1.00, ..., 12.00)
! --------------------------------------------------------------------------------------------------
  IF ( DVEG == 1 .or. DVEG == 3 .or. DVEG == 4 ) THEN

     IF (LAT >= 0.) THEN
        ! Northern Hemisphere
        DAY = JULIAN
     ELSE
        ! Southern Hemisphere.  DAY is shifted by 1/2 year.
        DAY = MOD ( JULIAN + ( 0.5 * YEARLEN ) , REAL(YEARLEN) )
     ENDIF

     T = 12. * DAY / REAL(YEARLEN)
     IT1 = T + 0.5
     IT2 = IT1 + 1
     WT1 = (IT1+0.5) - T
     WT2 = 1.-WT1
     IF (IT1 .LT.  1) IT1 = 12
     IF (IT2 .GT. 12) IT2 = 1

     LAI = WT1*parameters%LAIM(IT1) + WT2*parameters%LAIM(IT2)
     SAI = WT1*parameters%SAIM(IT1) + WT2*parameters%SAIM(IT2)
  ENDIF

  IF(DVEG == 7 .or. DVEG == 8 .or. DVEG == 9) THEN
    SAI = MAX(0.05,0.1 * LAI)  ! when reading LAI, set SAI to 10% LAI, but not below 0.05 MB: v3.8
    IF (LAI < 0.05) SAI = 0.0  ! if LAI below minimum, make sure SAI = 0
  ENDIF
!    IF(ILOC == 137 .and. JLOC == 7) THEN
!       write(*,*) 'in PHENOLOGY----3----:LAI,SAI=',LAI,SAI
!    END IF

  IF (SAI < 0.001 .and. CROPTYPE == 0) SAI = 0.001  ! MB: SAI CHECK, change to 0.05 v3.6 !niu
  IF ((LAI < 0.01) .and. CROPTYPE == 0) LAI = 0.01  ! MB: LAI CHECK !niu

!buried by snow

     DB = MIN( MAX(SNOWH - parameters%HVB,0.), parameters%HVT-parameters%HVB )
     FB = DB / MAX(1.E-06,parameters%HVT-parameters%HVB)

     IF(parameters%HVT> 0. .AND. parameters%HVT <= 1.0) THEN          !MB: change to 1.0 and 0.2 to reflect
       SNOWHC = parameters%HVT*EXP(-SNOWH/0.2)             !      changes to HVT in MPTABLE
       FB     = MIN(SNOWH,SNOWHC)/SNOWHC
     ENDIF

     ELAI =  LAI*(1.-FB)
     ESAI =  SAI*(1.-FB)

     IF (ESAI < 0.001 .and. CROPTYPE == 0) ESAI = 0.0    ! MB: ESAI CHECK, change to 0.05 v3.6 !niu
     IF ((ELAI < 0.001) .and. CROPTYPE == 0) ELAI = 0.0  ! MB: LAI CHECK !niu

  IF ( ( VEGTYP == parameters%iswater ) .OR. ( VEGTYP == parameters%ISBARREN ) .OR. &   !niu
       ( VEGTYP == parameters%ISICE   ) ) THEN                                          !niu
     LAI  = 0. ; ELAI = 0.
     SAI  = 0. ; ESAI = 0.
  ENDIF

     IF ((TV .GT. parameters%TMIN .and. CROPTYPE == 0).or.(PGS > 2 .and. PGS < 7 .and. CROPTYPE > 0)) THEN
         IGS = 1.
     ELSE
         IGS = 0.
     ENDIF

  END SUBROUTINE PHENOLOGY

!== begin precip_heat ==============================================================================

  SUBROUTINE PRECIP_HEAT (parameters,ILOC   ,JLOC   ,VEGTYP ,DT     ,UU     ,VV     , & !in
                          ELAI   ,ESAI   ,FVEG   ,IST    ,                 & !in
                          BDFALL ,RAIN   ,SNOW   ,FP     ,                 & !in
                          CANLIQ ,CANICE ,TV     ,SFCTMP ,TG     ,         & !in
                          QINTR  ,QDRIPR ,QTHROR ,QINTS  ,QDRIPS ,QTHROS , & !out
			  PAHV   ,PAHG   ,PAHB   ,QRAIN  ,QSNOW  ,SNOWHIN, & !out
			  FWET   ,CMC    ,ICEDRIP                        )   !out

! ------------------------ code history ------------------------------
! Michael Barlage: Oct 2013 - split CANWATER to calculate precip movement for 
!                             tracking of advected heat
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ input/output variables --------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN)  :: ILOC    !grid index
  INTEGER,INTENT(IN)  :: JLOC    !grid index
  INTEGER,INTENT(IN)  :: VEGTYP  !vegetation type
  INTEGER,INTENT(IN)  :: IST     !surface type 1-soil; 2-lake
  REAL,   INTENT(IN)  :: DT      !main time step (s)
  REAL,   INTENT(IN)  :: UU      !u-direction wind speed [m/s]
  REAL,   INTENT(IN)  :: VV      !v-direction wind speed [m/s]
  REAL,   INTENT(IN)  :: ELAI    !leaf area index, after burying by snow
  REAL,   INTENT(IN)  :: ESAI    !stem area index, after burying by snow
  REAL,   INTENT(IN)  :: FVEG    !greeness vegetation fraction (-)
  REAL,   INTENT(IN)  :: BDFALL  !bulk density of snowfall (kg/m3)
  REAL,   INTENT(IN)  :: RAIN    !rainfall (mm/s)
  REAL,   INTENT(IN)  :: SNOW    !snowfall (mm/s)
  REAL,   INTENT(IN)  :: FP      !fraction of the gridcell that receives precipitation
  REAL,   INTENT(IN)  :: TV      !vegetation temperature (k)
  REAL,   INTENT(IN)  :: SFCTMP  !model-level temperature (k)
  REAL,   INTENT(IN)  :: TG      !ground temperature (k)

! input & output
  REAL, INTENT(INOUT) :: CANLIQ  !intercepted liquid water (mm)
  REAL, INTENT(INOUT) :: CANICE  !intercepted ice mass (mm)

! output
  REAL, INTENT(OUT)   :: QINTR   !interception rate for rain (mm/s)
  REAL, INTENT(OUT)   :: QDRIPR  !drip rate for rain (mm/s)
  REAL, INTENT(OUT)   :: QTHROR  !throughfall for rain (mm/s)
  REAL, INTENT(OUT)   :: QINTS   !interception (loading) rate for snowfall (mm/s)
  REAL, INTENT(OUT)   :: QDRIPS  !drip (unloading) rate for intercepted snow (mm/s)
  REAL, INTENT(OUT)   :: QTHROS  !throughfall of snowfall (mm/s)
  REAL, INTENT(OUT)   :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL, INTENT(OUT)   :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL, INTENT(OUT)   :: PAHB    !precipitation advected heat - bare ground net (W/m2)
  REAL, INTENT(OUT)   :: QRAIN   !rain at ground srf (mm/s) [+]
  REAL, INTENT(OUT)   :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL, INTENT(OUT)   :: SNOWHIN !snow depth increasing rate (m/s)
  REAL, INTENT(OUT)   :: FWET    !wetted or snowed fraction of the canopy (-)
  REAL, INTENT(OUT)   :: CMC     !intercepted water (mm)
  REAL, INTENT(INOUT)   :: ICEDRIP !canice unloading
! --------------------------------------------------------------------

! ------------------------ local variables ---------------------------
  REAL                :: MAXSNO  !canopy capacity for snow interception (mm)
  REAL                :: MAXLIQ  !canopy capacity for rain interception (mm)
  REAL                :: FT      !temperature factor for unloading rate
  REAL                :: FV      !wind factor for unloading rate
  REAL                :: PAH_AC  !precipitation advected heat - air to canopy (W/m2)
  REAL                :: PAH_CG  !precipitation advected heat - canopy to ground (W/m2)
  REAL                :: PAH_AG  !precipitation advected heat - air to ground (W/m2)
! --------------------------------------------------------------------
! initialization

      QINTR   = 0.
      QDRIPR  = 0.
      QTHROR  = 0.
      QINTR   = 0.
      QINTS   = 0.
      QDRIPS  = 0.
      QTHROS  = 0.
      PAH_AC  = 0.
      PAH_CG  = 0.
      PAH_AG  = 0.
      PAHV    = 0.
      PAHG    = 0.
      PAHB    = 0.
      QRAIN   = 0.0
      QSNOW   = 0.0
      SNOWHIN = 0.0
      ICEDRIP = 0.0
!      print*, "precip_heat begin canopy balance:",canliq+canice+(rain+snow)*dt
!      print*,  "precip_heat snow*3600.0:",snow*3600.0
!      print*,  "precip_heat rain*3600.0:",rain*3600.0
!      print*,  "precip_heat canice:",canice
!      print*,  "precip_heat canliq:",canliq

! --------------------------- liquid water ------------------------------
! maximum canopy water

      MAXLIQ =  parameters%CH2OP * (ELAI+ ESAI)

! average interception and throughfall

      IF((ELAI+ ESAI).GT.0.) THEN
         QINTR  = FVEG * RAIN * FP  ! interception capability
         QINTR  = MIN(QINTR, (MAXLIQ - CANLIQ)/DT * (1.-EXP(-RAIN*DT/MAXLIQ)) )
         QINTR  = MAX(QINTR, 0.)
         QDRIPR = FVEG * RAIN - QINTR
         QTHROR = (1.-FVEG) * RAIN
         CANLIQ=MAX(0.,CANLIQ+QINTR*DT)
      ELSE
         QINTR  = 0.
         QDRIPR = 0.
         QTHROR = RAIN
	 IF(CANLIQ > 0.) THEN             ! FOR CASE OF CANOPY GETTING BURIED
	   QDRIPR = QDRIPR + CANLIQ/DT
	   CANLIQ = 0.0
	 END IF
      END IF
      
! heat transported by liquid water

      PAH_AC = FVEG * RAIN * (CWAT/1000.0) * (SFCTMP - TV)
      PAH_CG = QDRIPR * (CWAT/1000.0) * (TV - TG)
      PAH_AG = QTHROR * (CWAT/1000.0) * (SFCTMP - TG)
!      print*, "precip_heat PAH_AC:",PAH_AC
!      print*, "precip_heat PAH_CG:",PAH_CG
!      print*, "precip_heat PAH_AG:",PAH_AG

! --------------------------- canopy ice ------------------------------
! for canopy ice

      MAXSNO = 6.6*(0.27+46./BDFALL) * (ELAI+ ESAI)
     !MAXSNO = 4.0*(0.27+46./BDFALL) * (ELAI+ ESAI)

      IF((ELAI+ ESAI).GT.0.) THEN
         QINTS = FVEG * SNOW * FP
         QINTS = MIN(QINTS, (MAXSNO - CANICE)/DT * (1.-EXP(-SNOW*DT/MAXSNO)) )
         QINTS = MAX(QINTS, 0.)
         FT = MAX(0.0,(TV - 270.15) / 1.87E5)
         FV = SQRT(UU*UU + VV*VV) / 1.56E5
	 ! MB: changed below to reflect the rain assumption that all precip gets intercepted 
	 ICEDRIP = MAX(0.,CANICE) * (FV+FT)    !MB: removed /DT
         ICEDRIP = MIN(CANICE/DT,ICEDRIP)      !niu revised to keep water balance
         QDRIPS = (FVEG * SNOW - QINTS) + ICEDRIP
         QTHROS = (1.0-FVEG) * SNOW
         CANICE= MAX(0.,CANICE + (QINTS - ICEDRIP)*DT)
      ELSE
         QINTS  = 0.
         QDRIPS = 0.
         QTHROS = SNOW
	 IF(CANICE > 0.) THEN             ! FOR CASE OF CANOPY GETTING BURIED
	   QDRIPS = QDRIPS + CANICE/DT
	   CANICE = 0.0
	 END IF
      ENDIF
!      print*, "precip_heat canopy through:",3600.0*(FVEG * SNOW - QINTS)
!      print*, "precip_heat canopy drip:",3600.0*MAX(0.,CANICE) * (FV+FT)

! wetted fraction of canopy

      IF(CANICE.GT.0.) THEN
           FWET = MAX(0.,CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           FWET = MAX(0.,CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      FWET = MIN(FWET, 1.) ** 0.667

! total canopy water

      CMC = CANLIQ + CANICE

! heat transported by snow/ice

      PAH_AC = PAH_AC +  FVEG * SNOW * (CICE/1000.0) * (SFCTMP - TV)
      PAH_CG = PAH_CG + QDRIPS * (CICE/1000.0) * (TV - TG)
      PAH_AG = PAH_AG + QTHROS * (CICE/1000.0) * (SFCTMP - TG)
      
      PAHV = PAH_AC - PAH_CG
      PAHG = PAH_CG
      PAHB = PAH_AG
      
      IF (FVEG > 0.0 .AND. FVEG < 1.0) THEN
        PAHG = PAHG / FVEG         ! these will be multiplied by fraction later
	PAHB = PAHB / (1.0-FVEG)
      ELSEIF (FVEG <= 0.0) THEN
        PAHB = PAHG + PAHB         ! for case of canopy getting buried
        PAHG = 0.0
	PAHV = 0.0
      ELSEIF (FVEG >= 1.0) THEN
	PAHB = 0.0
      END IF
      
      PAHV = MAX(PAHV,-20.0)       ! Put some artificial limits here for stability
      PAHV = MIN(PAHV,20.0)
      PAHG = MAX(PAHG,-20.0)
      PAHG = MIN(PAHG,20.0)
      PAHB = MAX(PAHB,-20.0)
      PAHB = MIN(PAHB,20.0)
      
!      print*, 'precip_heat sfctmp,tv,tg:',sfctmp,tv,tg
!      print*, 'precip_heat 3600.0*qints+qdrips+qthros:',3600.0*(qints+qdrips+qthros)
!      print*, "precip_heat maxsno:",maxsno
!      print*, "precip_heat PAH_AC:",PAH_AC
!      print*, "precip_heat PAH_CG:",PAH_CG
!      print*, "precip_heat PAH_AG:",PAH_AG
      
!      print*, "precip_heat PAHV:",PAHV
!      print*, "precip_heat PAHG:",PAHG
!      print*, "precip_heat PAHB:",PAHB
!      print*, "precip_heat fveg:",fveg
!      print*,  "precip_heat qints*3600.0:",qints*3600.0
!      print*,  "precip_heat qdrips*3600.0:",qdrips*3600.0
!      print*,  "precip_heat qthros*3600.0:",qthros*3600.0
      
! rain or snow on the ground

      QRAIN   = QDRIPR + QTHROR
      QSNOW   = QDRIPS + QTHROS
      SNOWHIN = QSNOW/BDFALL

      IF (IST == 2 .AND. TG > TFRZ) THEN
         QSNOW   = 0.
         SNOWHIN = 0.
      END IF
!      print*,  "precip_heat qsnow*3600.0:",qsnow*3600.0
!      print*,  "precip_heat qrain*3600.0:",qrain*3600.0
!      print*,  "precip_heat SNOWHIN:",SNOWHIN
!      print*,  "precip_heat canice:",canice
!      print*,  "precip_heat canliq:",canliq
!      print*, "precip_heat end canopy balance:",canliq+canice+(qrain+qsnow)*dt
      

  END SUBROUTINE PRECIP_HEAT

!== begin error ====================================================================================

  SUBROUTINE ERROR (parameters,SWDOWN ,FSA    ,FSR    ,FIRA   ,FSH    ,FCEV   , &
                    FGEV   ,FCTR   ,SSOIL  ,BEG_WB ,CANLIQ ,CANICE , &
                    SNEQV  ,WA     ,SMC    ,DZSNSO ,PRCP   ,ECAN   , &
                    ETRAN  ,EDIR   ,RUNSRF ,RUNSUB ,DT     ,NSOIL  , &
                    NSNOW  ,IST    ,ERRWAT, ILOC   ,JLOC   ,FVEG   , &
                    SAV    ,SAG    ,FSRV   ,FSRG   ,ZWT    ,PAH    , &
                    PAHV   ,PAHG   ,PAHB   ,MQ     ,CANHS  ,SH2O   , &
                    ISNOW  ,&
                    ETRANI ,QIN    ,PSI    ,QROOT  , &
                    SICE   ,VEGTYP ,SOILTYP,SH2OO  ,DMICE  ,DSH2O  ,HTOP )
! --------------------------------------------------------------------------------------------------
! check surface energy balance and water balance
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER                        , INTENT(IN) :: ISNOW  !no. of snow layers
  INTEGER                        , INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER                        , INTENT(IN) :: IST    !surface type 1->soil; 2->lake
  INTEGER                        , INTENT(IN) :: ILOC   !grid index
  INTEGER                        , INTENT(IN) :: JLOC   !grid index
  REAL                           , INTENT(IN) :: SWDOWN !downward solar filtered by sun angle [w/m2]
  REAL                           , INTENT(IN) :: FSA    !total absorbed solar radiation (w/m2)
  REAL                           , INTENT(IN) :: FSR    !total reflected solar radiation (w/m2)
  REAL                           , INTENT(IN) :: FIRA   !total net longwave rad (w/m2)  [+ to atm]
  REAL                           , INTENT(IN) :: FSH    !total sensible heat (w/m2)     [+ to atm]
  REAL                           , INTENT(IN) :: FCEV   !canopy evaporation heat (w/m2) [+ to atm]
  REAL                           , INTENT(IN) :: FGEV   !ground evaporation heat (w/m2) [+ to atm]
  REAL                           , INTENT(IN) :: FCTR   !transpiration heat flux (w/m2) [+ to atm]
  REAL                           , INTENT(IN) :: SSOIL  !ground heat flux (w/m2)        [+ to soil]
  REAL                           , INTENT(IN) :: FVEG
  REAL                           , INTENT(IN) :: SAV
  REAL                           , INTENT(IN) :: SAG
  REAL                           , INTENT(IN) :: FSRV
  REAL                           , INTENT(IN) :: FSRG
  REAL                           , INTENT(IN) :: ZWT

  REAL                           , INTENT(IN) :: PRCP   !precipitation rate (kg m-2 s-1)
  REAL                           , INTENT(IN) :: ECAN   !evaporation of intercepted water (mm/s)
  REAL                           , INTENT(IN) :: ETRAN  !transpiration rate (mm/s)
  REAL                           , INTENT(IN) :: EDIR   !soil surface evaporation rate[mm/s]
  REAL                           , INTENT(IN) :: RUNSRF !surface runoff [mm/s] 
  REAL                           , INTENT(IN) :: RUNSUB !baseflow (saturation excess) [mm/s]
  REAL                           , INTENT(IN) :: CANLIQ !intercepted liquid water (mm)
  REAL                           , INTENT(IN) :: CANICE !intercepted ice mass (mm)
  REAL                           , INTENT(IN) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SH2O   !soil moisture (liq.) [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]
  REAL                           , INTENT(IN) :: WA     !water storage in aquifer [mm]
  REAL                           , INTENT(IN) :: DT     !time step [sec]
  REAL                           , INTENT(IN) :: BEG_WB !water storage at begin of a timesetp [mm]
  REAL                           , INTENT(IN) :: CANHS  !canopy heat storage change a time step (w/m2)
  REAL                           , INTENT(OUT) :: ERRWAT !error in water balance [mm/timestep]

  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: PSI  !
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ETRANI  !
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE  !
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: QROOT  !
  REAL                           , INTENT(IN) :: MQ     !water stored in living tissues [mm]
  REAL, DIMENSION(       1:NSOIL),INTENT(IN)  :: DSH2O  !change rate of liquid soil moisture[m/s]
  REAL, DIMENSION(       1:NSOIL),INTENT(IN)  :: DMICE  !change rate of ice [m/s]
  REAL                           , INTENT(IN) :: QIN
  REAL                           , INTENT(IN) :: HTOP   !surface ponding depth [mm]
  INTEGER                        , INTENT(IN) :: VEGTYP
  INTEGER                        , INTENT(IN) :: SOILTYP
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SH2OO  !soil moisture (liq.) [m3/m3]

  REAL, INTENT(IN)   :: PAH     !precipitation advected heat - total (W/m2)
  REAL, INTENT(IN)   :: PAHV    !precipitation advected heat - total (W/m2)
  REAL, INTENT(IN)   :: PAHG    !precipitation advected heat - total (W/m2)
  REAL, INTENT(IN)   :: PAHB    !precipitation advected heat - total (W/m2)

  INTEGER                                     :: IZ     !do-loop index
  REAL                                        :: END_WB !water storage at end of a timestep [mm]
  !KWM REAL                                        :: ERRWAT !error in water balance [mm/timestep]
  REAL                                        :: ERRENG !error in surface energy balance [w/m2]
  REAL                                        :: ERRSW  !error in shortwave radiation balance [w/m2]
  REAL                                        :: FSRVG
  CHARACTER(len=256)                          :: message
! --------------------------------------------------------------------------------------------------
!jref:start
   ERRSW   = SWDOWN - (FSA + FSR)
!   ERRSW   = SWDOWN - (SAV+SAG + FSRV+FSRG)
!   WRITE(*,*) "ERRSW =",ERRSW
   IF (ABS(ERRSW) > 0.01) THEN            ! w/m2
   WRITE(*,*) "VEGETATION!"
   WRITE(*,*) "SWDOWN*FVEG =",SWDOWN*FVEG
   WRITE(*,*) "FVEG*(SAV+SAG) =",FVEG*SAV + SAG
   WRITE(*,*) "FVEG*(FSRV +FSRG)=",FVEG*FSRV + FSRG
   WRITE(*,*) "GROUND!"
   WRITE(*,*) "(1-.FVEG)*SWDOWN =",(1.-FVEG)*SWDOWN
   WRITE(*,*) "(1.-FVEG)*SAG =",(1.-FVEG)*SAG
   WRITE(*,*) "(1.-FVEG)*FSRG=",(1.-FVEG)*FSRG
   WRITE(*,*) "FSRV   =",FSRV
   WRITE(*,*) "FSRG   =",FSRG
   WRITE(*,*) "FSR    =",FSR
   WRITE(*,*) "SAV    =",SAV
   WRITE(*,*) "SAG    =",SAG
   WRITE(*,*) "FSA    =",FSA
!jref:end   
      WRITE(message,*) 'ERRSW =',ERRSW
      call wrf_message(trim(message))
      call wrf_error_fatal("Stop in Noah-MP")
   END IF

   ERRENG = SAV+SAG-(FIRA+FSH+FCEV+FGEV+FCTR+SSOIL+CANHS) +PAH
   IF(ABS(ERRENG) > 0.01) THEN
      write(message,*) 'ERRENG =',ERRENG,' at i,j: ',ILOC,JLOC
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Net solar:       ",FSA
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Net longwave:    ",FIRA
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Total sensible:  ",FSH
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Canopy evap:     ",FCEV
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Ground evap:     ",FGEV
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Transpiration:   ",FCTR
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Total ground:    ",SSOIL
      call wrf_message(trim(message))
      WRITE(message,'(a17,4F10.4)') "Precip advected: ",PAH,PAHV,PAHG,PAHB
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Precip: ",PRCP
      call wrf_message(trim(message))
      WRITE(message,'(a17,F10.4)') "Veg fraction: ",FVEG
      call wrf_message(trim(message))
      call wrf_error_fatal("Energy budget problem in NOAHMP LSM")
   END IF

   IF (IST == 1) THEN                                       !soil
       !END_WB = 0.
       !DO IZ = 1,NSOIL
       !  END_WB = END_WB + SMC(IZ) * DZSNSO(IZ) * 1000.
       !END DO

        END_WB = SUM(SMC(1:NSOIL) * DZSNSO(1:NSOIL) * 1000.)

        IF(OPT_ROOT == 1) END_WB = END_WB + CANLIQ + CANICE + SNEQV + WA + MQ + HTOP
       !IF(OPT_ROOT == 2) END_WB = END_WB + CANLIQ + CANICE + SNEQV + WA
        IF(OPT_ROOT == 2) END_WB = END_WB + CANLIQ + CANICE + SNEQV + WA + HTOP

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    !  write(*,*) '--2--,END_WB,CANLIQ,CANICE,SNEQV,WA,MQ,HTOP=',END_WB,CANLIQ,CANICE,SNEQV,WA,MQ,HTOP
    !  WRITE(*,*) PRCP*DT,ECAN*DT,EDIR*DT,ETRAN*DT,RUNSRF*DT,RUNSUB*DT
    !END IF

        ERRWAT = END_WB-BEG_WB-(PRCP-ECAN-ETRAN-EDIR-RUNSRF-RUNSUB)*DT


        IF(ABS(ERRWAT) > 0.1) THEN
           if (ERRWAT > 0) then
              call wrf_message ('The model is gaining water (ERRWAT is positive)')
           else
              call wrf_message('The model is losing water (ERRWAT is negative)')
           endif

           print *,'ERRWAT=',ERRWAT
           print *,'VEGTYP,SOILTYP=',VEGTYP,SOILTYP
           write(*,*)'CANLIQ,CANICE,SNEQV,WA,MQ=',CANLIQ,CANICE,SNEQV,WA,MQ
           write(*,*) 'DSH2O*1000.*DT=',DSH2O*1000.*DT
           print *,ILOC,JLOC,vegtyp,parameters%smcmax(1)
           print *,'sh2oo:',sh2oo
           print *,'SMC:',SMC
           print *,'sh2o:',sh2o
           print *,'sice:',sice
           print *,'PSI',psi
           print *,'HTOP',HTOP
           print *,'zwt:',zwt
           print *,'prcp:',prcp*dt
           print *,'RUNSRF*dt,RUNSUB*dt',RUNSRF*dt,RUNSUB*dt
           print *,'qroot (mm)',qroot*dt*1.e3
           print *,'edir',edir*dt
           print *,'etran',etran*dt
           print *,'MQ:',mq
           print *,'WA:',wa

           write(message, *) 'ERRWAT =',ERRWAT, "kg m{-2} timestep{-1}"
           call wrf_message(trim(message))
           WRITE(message, &
           '("    I      J     END_WB     BEG_WB       PRCP       ECAN       EDIR      ETRAN      RUNSRF     RUNSUB   ZWT")')
           call wrf_message(trim(message))
           WRITE(message,'(i6,1x,i6,1x,2f15.3,9f11.5)')ILOC,JLOC,END_WB,BEG_WB,PRCP*DT,ECAN*DT,&
                EDIR*DT,ETRAN*DT,RUNSRF*DT,RUNSUB*DT,ZWT
           call wrf_message(trim(message))
          !call wrf_error_fatal("Water budget problem in NOAHMP LSM")
        END IF

   ELSE                 !KWM
      ERRWAT = 0.0      !KWM
   ENDIF

 END SUBROUTINE ERROR

!== begin energy ===================================================================================

  SUBROUTINE ENERGY (parameters,rad_cons,ICE    ,VEGTYP ,IST    ,NSNOW  ,NSOIL  , & !in
                     ISNOW  ,DT     ,RHOAIR ,SFCPRS ,QAIR   , & !in
                     SFCTMP ,THAIR  ,LWDN   ,UU     ,VV     ,ZREF   , & !in
                     CO2AIR ,O2AIR  ,SOLAD  ,SOLAI  ,COSZ   ,IGS    , & !in
                     EAIR   ,TBOT   ,ZSNSO  ,ZSOIL  ,TOPOSV , & !in
                     ELAI   ,ESAI   ,FWET   ,FOLN   ,         & !in
                     FVEG   ,PAHV   ,PAHG   ,PAHB   ,                 & !in
                     QSNOW  ,DZSNSO ,LAT    ,CANLIQ ,CANICE ,ILOC   , JLOC, & !in
		     Z0WRF  ,FROOT  ,KR     ,MQ     ,                 &
                     IMELT  ,SNICEV ,SNLIQV ,EPORE  ,T2M    ,FSNO   , & !out
                     SAV    ,SAG    ,QMELT  ,FSA    ,FSR    ,TAUX   , & !out
                     TAUY   ,FIRA   ,FSH    ,FCEV   ,FGEV   ,FCTR   , & !out
                     TRAD   ,PSN    ,APAR   ,SSOIL  ,BTRANI ,BTRAN  , & !out
                     PONDING,TS     ,LATHEAV,LATHEAG,frozen_canopy  ,FROZEN_GROUND,                       & !out
                     NDVI   ,PSI    ,CANHS  ,SICE   ,DMICE  , & !out
                     TV     ,TG     ,STC    ,SNOWH  ,EAH    ,TAH    , & !inout
                     SNEQVO ,SNEQV  ,SH2O   ,SMC    ,SNICE  ,SNLIQ  , & !inout
                     ALBOLD ,CM     ,CH     ,DX     ,DZ8W   ,Q2     , &   !inout
                     TAUSS  ,                                         & !inout
!jref:start
                     QC     ,QSFC   ,PSFC   , & !in 
                     T2MV   ,T2MB   ,FSRV   , &
                     FSRG   ,RSSUN  ,RSSHA  ,ALBSND  ,ALBSNI,BGAP   ,WGAP,TGV,TGB,&
                     Q1     ,Q2V    ,Q2B    ,Q2E    ,CHV  ,CHB, EMISSI,PAH  ,&
                     SHG,SHC,SHB,EVG,EVB,GHV,GHB,IRG,IRC,IRB,TR,EVC,CHLEAF,CHUC,CHV2,CHB2, & !out
                     radius ,XM)
!jref:end                            

! --------------------------------------------------------------------------------------------------
! we use different approaches to deal with subgrid features of radiation transfer and turbulent
! transfer. We use 'tile' approach to compute turbulent fluxes, while we use modified two-
! stream to compute radiation transfer. Tile approach, assemblying vegetation canopies together,
! may expose too much ground surfaces (either covered by snow or grass) to solar radiation. The
! modified two-stream assumes vegetation covers fully the gridcell but with gaps between tree
! crowns.
! --------------------------------------------------------------------------------------------------
! turbulence transfer : 'tile' approach to compute energy fluxes in vegetated fraction and
!                         bare fraction separately and then sum them up weighted by fraction
!                     --------------------------------------
!                    / O  O  O  O  O  O  O  O  /          / 
!                   /  |  |  |  |  |  |  |  | /          /
!                  / O  O  O  O  O  O  O  O  /          /
!                 /  |  |  |tile1|  |  |  | /  tile2   /
!                / O  O  O  O  O  O  O  O  /  bare    /
!               /  |  |  | vegetated |  | /          /
!              / O  O  O  O  O  O  O  O  /          /
!             /  |  |  |  |  |  |  |  | /          /
!            --------------------------------------
! --------------------------------------------------------------------------------------------------
! radiation transfer : modified two-stream (Yang and Friedl, 2003, JGR; Niu ang Yang, 2004, JGR)
!                     --------------------------------------  two-stream treats leaves as
!                    /   O   O   O   O   O   O   O   O    /  cloud over the entire grid-cell,
!                   /    |   |   |   |   |   |   |   |   / while the modified two-stream 
!                  /   O   O   O   O   O   O   O   O    / aggregates cloudy leaves into  
!                 /    |   |   |   |   |   |   |   |   / tree crowns with gaps (as shown in
!                /   O   O   O   O   O   O   O   O    / the left figure). We assume these
!               /    |   |   |   |   |   |   |   |   / tree crowns are evenly distributed
!              /   O   O   O   O   O   O   O   O    / within the gridcell with 100% veg
!             /    |   |   |   |   |   |   |   |   / fraction, but with gaps. The 'tile'
!            -------------------------------------- approach overlaps too much shadows.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  type (constants), intent(in) :: rad_cons
  integer                           , INTENT(IN)    :: ILOC
  integer                           , INTENT(IN)    :: JLOC
  INTEGER                           , INTENT(IN)    :: ICE    !ice (ice = 1)
  INTEGER                           , INTENT(IN)    :: VEGTYP !vegetation physiology type
  INTEGER                           , INTENT(IN)    :: IST    !surface type: 1->soil; 2->lake
  INTEGER                           , INTENT(IN)    :: NSNOW  !maximum no. of snow layers        
  INTEGER                           , INTENT(IN)    :: NSOIL  !number of soil layers
  INTEGER                           , INTENT(IN)    :: ISNOW  !actual no. of snow layers
  REAL                              , INTENT(IN)    :: DT     !time step [sec]
  REAL                              , INTENT(IN)    :: QSNOW  !snowfall on the ground (mm/s)
  REAL                              , INTENT(IN)    :: RHOAIR !density air (kg/m3)
  REAL                              , INTENT(IN)    :: EAIR   !vapor pressure air (pa)
  REAL                              , INTENT(IN)    :: SFCPRS !pressure (pa)
  REAL                              , INTENT(IN)    :: QAIR   !specific humidity (kg/kg)
  REAL                              , INTENT(IN)    :: SFCTMP !air temperature (k)
  REAL                              , INTENT(IN)    :: THAIR  !potential temperature (k)
  REAL                              , INTENT(IN)    :: LWDN   !downward longwave radiation (w/m2)
  REAL                              , INTENT(IN)    :: UU     !wind speed in e-w dir (m/s)
  REAL                              , INTENT(IN)    :: VV     !wind speed in n-s dir (m/s)
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAD  !incoming direct solar rad. (w/m2)
  REAL   , DIMENSION(       1:    2), INTENT(IN)    :: SOLAI  !incoming diffuse solar rad. (w/m2)
  REAL                              , INTENT(IN)    :: COSZ   !cosine solar zenith angle (0-1)
  REAL                              , INTENT(IN)    :: ELAI   !LAI adjusted for burying by snow
  REAL                              , INTENT(IN)    :: ESAI   !LAI adjusted for burying by snow
  REAL                              , INTENT(IN)    :: FWET   !fraction of canopy that is wet [-]
  REAL                              , INTENT(IN)    :: FVEG   !greeness vegetation fraction (-)
  REAL                              , INTENT(IN)    :: LAT    !latitude (radians)
  REAL                              , INTENT(IN)    :: TOPOSV !standard dev of DEM [m]
  REAL                              , INTENT(IN)    :: CANLIQ !canopy-intercepted liquid water (mm)
  REAL                              , INTENT(IN)    :: CANICE !canopy-intercepted ice mass (mm)
  REAL                              , INTENT(IN)    :: FOLN   !foliage nitrogen (%)
  REAL                              , INTENT(IN)    :: CO2AIR !atmospheric co2 concentration (pa)
  REAL                              , INTENT(IN)    :: O2AIR  !atmospheric o2 concentration (pa)
  REAL                              , INTENT(IN)    :: IGS    !growing season index (0=off, 1=on)

  REAL                              , INTENT(IN)    :: ZREF   !reference height (m)
  REAL                              , INTENT(IN)    :: TBOT   !bottom condition for soil temp. (k) 
  REAL   , DIMENSION(       1:NSOIL), INTENT(IN)    :: PSI    !surface layer soil matrix potential (m)  !Mixed-form
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: ZSNSO  !layer-bottom depth from snow surf [m]
  REAL   , DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !layer-bottom depth from soil surf [m]
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: DZSNSO !depth of snow & soil layer-bottom [m]
  REAL   , DIMENSION(       1:NSOIL), INTENT(IN)    :: FROOT  !root fraction [-]
  REAL                              , INTENT(IN)    :: KR     !=BTRAN (for dynamic rooti option)
  REAL                              , INTENT(IN)    :: MQ     !water in plant tissues [kg]
  REAL, INTENT(IN)   :: PAHV    !precipitation advected heat - vegetation net (W/m2)
  REAL, INTENT(IN)   :: PAHG    !precipitation advected heat - under canopy net (W/m2)
  REAL, INTENT(IN)   :: PAHB    !precipitation advected heat - bare ground net (W/m2)

!jref:start; in 
  REAL                              , INTENT(IN)    :: QC     !cloud water mixing ratio
  REAL                              , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
  REAL                              , INTENT(IN)    :: PSFC   !pressure at lowest model layer
  REAL                              , INTENT(IN)    :: DX     !horisontal resolution
  REAL                              , INTENT(IN)    :: DZ8W   !thickness of lowest layer
  REAL                              , INTENT(IN)    :: Q2     !mixing ratio (kg/kg)
  REAL,    DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: radius ! snow grain radius
!jref:end

! outputs
  REAL                              , INTENT(OUT)   :: Z0WRF  !combined z0 sent to coupled model
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT)   :: IMELT  !phase change index [1-melt; 2-freeze]
  REAL,    DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT)   :: XM     !melting or freezing water [kg/m2]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNICEV !partial volume ice [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: SNLIQV !partial volume liq. water [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(OUT)   :: EPORE  !effective porosity [m3/m3]
  REAL                              , INTENT(OUT)   :: FSNO   !snow cover fraction (-)
  REAL                              , INTENT(OUT)   :: QMELT  !snowmelt [mm/s]
  REAL                              , INTENT(OUT)   :: PONDING!pounding at ground [mm]
  REAL                              , INTENT(OUT)   :: SAV    !solar rad. absorbed by veg. (w/m2)
  REAL                              , INTENT(OUT)   :: SAG    !solar rad. absorbed by ground (w/m2)
  REAL                              , INTENT(OUT)   :: FSA    !tot. absorbed solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: FSR    !tot. reflected solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: TAUX   !wind stress: e-w (n/m2)
  REAL                              , INTENT(OUT)   :: TAUY   !wind stress: n-s (n/m2)
  REAL                              , INTENT(OUT)   :: FIRA   !total net LW. rad (w/m2)   [+ to atm]
  REAL                              , INTENT(OUT)   :: FSH    !total sensible heat (w/m2) [+ to atm]
  REAL                              , INTENT(OUT)   :: FCEV   !canopy evaporation (w/m2)  [+ to atm]
  REAL                              , INTENT(OUT)   :: FGEV   !ground evaporation (w/m2)  [+ to atm]
  REAL                              , INTENT(OUT)   :: FCTR   !transpiration (w/m2)       [+ to atm]
  REAL                              , INTENT(OUT)   :: TRAD   !radiative temperature (k)
  REAL                              , INTENT(OUT)   :: T2M    !2 m height air temperature (k)
  REAL                              , INTENT(OUT)   :: PSN    !total photosyn. (umolco2/m2/s) [+]
  REAL                              , INTENT(OUT)   :: APAR   !total photosyn. active energy (w/m2)
  REAL                              , INTENT(OUT)   :: SSOIL  !ground heat flux (w/m2)   [+ to soil]
  REAL   , DIMENSION(       1:NSOIL), INTENT(OUT)   :: BTRANI !soil water transpiration factor (0-1)
  REAL                              , INTENT(OUT)   :: BTRAN  !soil water transpiration factor (0-1)
!  REAL                              , INTENT(OUT)   :: LATHEA !latent heat vap./sublimation (j/kg)
  REAL                              , INTENT(OUT)   :: LATHEAV !latent heat vap./sublimation (j/kg)
  REAL                              , INTENT(OUT)   :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                           , INTENT(OUT)   :: FROZEN_GROUND ! used to define latent heat pathway
  LOGICAL                           , INTENT(OUT)   :: FROZEN_CANOPY ! used to define latent heat pathway
  REAL                              , INTENT(OUT)   :: NDVI   !NDVI

!jref:start  
  REAL                              , INTENT(OUT)   :: FSRV    !veg. reflected solar radiation (w/m2)
  REAL                              , INTENT(OUT)   :: FSRG    !ground reflected solar radiation (w/m2)
  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)
!jref:end - out for debug  

!jref:start; output
  REAL                              , INTENT(OUT)   :: T2MV   !2-m air temperature over vegetated part [k]
  REAL                              , INTENT(OUT)   :: T2MB   !2-m air temperature over bare ground part [k]
  REAL                              , INTENT(OUT)   :: BGAP
  REAL                              , INTENT(OUT)   :: WGAP
  REAL, DIMENSION(1:2)              , INTENT(OUT)   :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2)              , INTENT(OUT)   :: ALBSNI   !snow albedo (diffuse)
!jref:end

! input & output
  REAL                              , INTENT(INOUT) :: TS     !surface temperature (k)
  REAL                              , INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL                              , INTENT(INOUT) :: TG     !ground temperature (k)
  REAL   , DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow/soil temperature [k]
  REAL                              , INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL                              , INTENT(INOUT) :: SNEQV  !snow mass (mm)
  REAL                              , INTENT(INOUT) :: SNEQVO !snow mass at last time step (mm)
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !liquid soil moisture [m3/m3]
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow ice mass (kg/m2)
  REAL   , DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow liq mass (kg/m2)
  REAL                              , INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL                              , INTENT(INOUT) :: TAH    !canopy air temperature (k)
  REAL                              , INTENT(INOUT) :: ALBOLD !snow albedo at last time step(CLASS type)
  REAL                              , INTENT(INOUT) :: TAUSS  !non-dimensional snow age
  REAL                              , INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL                              , INTENT(INOUT) :: CH     !sensible heat exchange coefficient
  REAL                              , INTENT(INOUT) :: Q1
!  REAL                                              :: Q2E
  REAL,                               INTENT(OUT)   :: EMISSI
  REAL,                               INTENT(OUT)   :: PAH    !precipitation advected heat - total (W/m2)
  REAL,                                 INTENT(OUT) :: CANHS  !canopy heat storage change (w/m2)

! local
  INTEGER                                           :: IZ     !do-loop index
  LOGICAL                                           :: VEG    !true if vegetated surface
  REAL                                              :: UR     !wind speed at height ZLVL (m/s)
  REAL                                              :: ZLVL   !reference height (m)
  REAL                                              :: FSUN   !sunlit fraction of canopy [-]
  REAL                                              :: RB     !leaf boundary layer resistance (s/m)
  REAL                                              :: RSURF  !ground surface resistance (s/m)
  REAL                                              :: L_RSURF!Dry-layer thickness for computing RSURF (Sakaguchi and Zeng, 2009)
  REAL                                              :: D_RSURF!Reduced vapor diffusivity in soil for computing RSURF (SZ09)
  REAL                                              :: BEVAP  !soil water evaporation factor (0- 1)
  REAL                                              :: MOL    !Monin-Obukhov length (m)
  REAL                                              :: VAI    !sum of LAI  + stem area index [m2/m2]
  REAL                                              :: CWP    !canopy wind extinction parameter
  REAL                                              :: ZPD    !zero plane displacement (m)
  REAL                                              :: Z0M    !z0 momentum (m)
  REAL                                              :: ZPDG   !zero plane displacement (m)
  REAL                                              :: Z0MG   !z0 momentum, ground (m)
  REAL                                              :: EMV    !vegetation emissivity
  REAL                                              :: EMG    !ground emissivity
  REAL                                              :: FIRE   !emitted IR (w/m2)
  REAL                                              :: LAISUN !sunlit leaf area index (m2/m2)
  REAL                                              :: LAISHA !shaded leaf area index (m2/m2)
  REAL                                              :: PSNSUN !sunlit photosynthesis (umolco2/m2/s)
  REAL                                              :: PSNSHA !shaded photosynthesis (umolco2/m2/s)
!jref:start - for debug  
!  REAL                                              :: RSSUN  !sunlit stomatal resistance (s/m)
!  REAL                                              :: RSSHA  !shaded stomatal resistance (s/m)
!jref:end - for debug
  REAL                                              :: PARSUN !par absorbed per sunlit LAI (w/m2)
  REAL                                              :: PARSHA !par absorbed per shaded LAI (w/m2)

  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: FACT   !temporary used in phase change
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: DF     !thermal conductivity [w/m/k]
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: HCPCT  !heat capacity [j/m3/k]
  REAL, DIMENSION(-NSNOW+1:NSOIL)                   :: PHI    !light through water or snow (w/m2)
  REAL                                              :: BDSNO  !bulk density of snow (kg/m3)
  REAL                                              :: FMELT  !melting factor for snow cover frac
  REAL                                              :: GX     !temporary variable
!  REAL                                              :: GAMMA  !psychrometric constant (pa/k)
  REAL                                              :: GAMMAV  !psychrometric constant (pa/k)
  REAL                                              :: GAMMAG  !psychrometric constant (pa/k)
  REAL                                              :: RHSUR  !raltive humidity in surface soil/snow air space (-)

! temperature and fluxes over vegetated fraction

  REAL                                              :: TAUXV  !wind stress: e-w dir [n/m2]
  REAL                                              :: TAUYV  !wind stress: n-s dir [n/m2]
  REAL,INTENT(OUT)                                              :: IRC    !canopy net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: IRG    !ground net LW rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHC    !canopy sen. heat [w/m2]   [+ to atm]
  REAL,INTENT(OUT)                                              :: SHG    !ground sen. heat [w/m2]   [+ to atm]
!jref:start  
  REAL,INTENT(OUT)                                  :: Q2V
  REAL,INTENT(OUT)                                  :: Q2B
  REAL,INTENT(OUT)                                  :: Q2E
!jref:end  
  REAL,INTENT(OUT)                                              :: EVC    !canopy evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: EVG    !ground evap. heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: TR     !transpiration heat [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: GHV    !ground heat flux [w/m2]  [+ to soil]
  REAL,INTENT(OUT)                                  :: TGV    !ground surface temp. [k]
  REAL                                              :: CMV    !momentum drag coefficient
  REAL,INTENT(OUT)                                  :: CHV    !sensible heat exchange coefficient

! temperature and fluxes over bare soil fraction

  REAL                                              :: TAUXB  !wind stress: e-w dir [n/m2]
  REAL                                              :: TAUYB  !wind stress: n-s dir [n/m2]
  REAL,INTENT(OUT)                                              :: IRB    !net longwave rad. [w/m2] [+ to atm]
  REAL,INTENT(OUT)                                              :: SHB    !sensible heat [w/m2]     [+ to atm]
  REAL,INTENT(OUT)                                              :: EVB    !evaporation heat [w/m2]  [+ to atm]
  REAL,INTENT(OUT)                                              :: GHB    !ground heat flux [w/m2] [+ to soil]
  REAL,INTENT(OUT)                                  :: TGB    !ground surface temp. [k]
  REAL                                              :: CMB    !momentum drag coefficient
  REAL,INTENT(OUT)                                  :: CHB    !sensible heat exchange coefficient
  REAL,INTENT(OUT)                                  :: CHLEAF !leaf exchange coefficient
  REAL,INTENT(OUT)                                  :: CHUC   !under canopy exchange coefficient
!jref:start  
  REAL,INTENT(OUT)                                  :: CHV2    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL,INTENT(OUT)                                  :: CHB2    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL                                  :: noahmpres
  REAL                                  :: PSI1,SE
!jref:end  

  REAL, PARAMETER                   :: MPE    = 1.E-6
  REAL, PARAMETER                   :: PSIWLT = -300.  !metric potential for wilting point (m)
  REAL, PARAMETER                   :: Z0     = 0.002  ! Bare-soil roughness length (m) (i.e., under the canopy)

!mixed-form RE

  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: DMICE  !change rate of solid soil moisture (m/s)
  REAL   , DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE
  REAL   , DIMENSION(       1:NSOIL) :: VGM
! ---------------------------------------------------------------------------------------------------
! initialize fluxes from veg. fraction

    TAUXV     = 0.    
    TAUYV     = 0.
    IRC       = 0.
    SHC       = 0.
    IRG       = 0.
    SHG       = 0.
    EVG       = 0.       
    EVC       = 0.
    TR        = 0.
    GHV       = 0.       
    PSNSUN    = 0.
    PSNSHA    = 0.
    T2MV      = 0.
    Q2V       = 0.
    CHV       = 0.
    CHLEAF    = 0.
    CHUC      = 0.
    CHV2      = 0.

! wind speed at reference height: ur >= 1

    UR = MAX( SQRT(UU**2.+VV**2.), 1.0 )

! vegetated or non-vegetated

    VAI = ELAI + ESAI
    VEG = .FALSE.
    IF(VAI > 0.) VEG = .TRUE.

!   IF(ILOC == 137 .and. JLOC == 7) THEN
!    write(*,*) 'VAI , ELAI , ESAI', VAI , ELAI , ESAI
!    write(*,*) 'VEG,FVEG=', VEG, FVEG
!   END IF

! ground snow cover fraction [Niu and Yang, 2007, JGR]

     FSNO = 0.
     IF(SNOWH.GT.0.)  THEN
         BDSNO    = SNEQV / SNOWH
         FMELT    = (BDSNO/100.)**parameters%MFSNO
!        FSNO     = TANH( SNOWH /(2.5* Z0 * FMELT))
         FSNO     = TANH( SNOWH /(2.5* Z0 * FMELT))  &
                  * MIN(1.0,1.0/(5.0e-3*MAX(0.01,TOPOSV)))
     ENDIF

! ground roughness length

     IF(IST == 2) THEN
       IF(TG .LE. TFRZ) THEN
         Z0MG = 0.01 * (1.0-FSNO) + FSNO * parameters%Z0SNO
       ELSE
         Z0MG = 0.01  
       END IF
     ELSE
       Z0MG = Z0 * (1.0-FSNO) + FSNO * parameters%Z0SNO
     END IF

! roughness length and displacement height

     ZPDG  = SNOWH
     IF(VEG) THEN
        Z0M  = parameters%Z0MVT
        ZPD  = 0.65 * parameters%HVT
        IF(SNOWH.GT.ZPD) ZPD  = SNOWH
     ELSE
        Z0M  = Z0MG
        ZPD  = ZPDG
     END IF

     ZLVL = MAX(ZPD,parameters%HVT) + ZREF
     IF(ZPDG >= ZLVL) ZLVL = ZPDG + ZREF
!     UR   = UR*LOG(ZLVL/Z0M)/LOG(10./Z0M)       !input UR is at 10m

! canopy wind absorption coeffcient

     CWP = parameters%CWPVT

! Thermal properties of soil, snow, lake, and frozen soil

  CALL THERMOPROP (parameters,NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & !in
                   DT      ,SNOWH   ,SNICE   ,SNLIQ   , & !in
                   SMC     ,SH2O    ,TG      ,STC     ,UR      , & !in
                   LAT     ,Z0M     ,ZLVL    ,VEGTYP  , & !in
                   DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & !out
                   FACT    )                              !out

! Solar radiation: absorbed & reflected by the ground and canopy

  CALL  RADIATION (parameters,rad_cons,VEGTYP  ,IST     ,ICE     ,NSOIL   , & !in 
                   SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & !in
                   TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & !in
                   ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & !in
                   NSNOW   ,ISNOW   ,SNICE   ,SNLIQ   ,DZSNSO  , & !in
                   radius  ,FVEG    ,ILOC    ,JLOC    ,          & !in
                   ALBOLD  ,TAUSS   ,                            & !inout
                   FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & !out
                   SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , & 
                   FSRG    ,ALBSND  ,ALBSNI  ,BGAP    ,WGAP    ,NDVI  , & !out
                   PHI )  !out

   ! IF(ILOC == 137 .and. JLOC == 7) THEN
   !  write(*,*) '----------after RADIATION-------------------------'
   !  write(*,*) "SAG=",SAG
   !  write(*,*) "SAV=",SAV
   ! END IF

! vegetation and ground emissivity

     EMV = 1. - EXP(-(ELAI+ESAI)/1.0)
     IF (ICE == 1) THEN
       EMG = 0.98*(1.-FSNO) + 1.0*FSNO
     ELSE
       EMG = parameters%EG(IST)*(1.-FSNO) + 1.0*FSNO
     END IF

! soil moisture factor controlling stomatal resistance
   
     BTRAN = 0.

     IF(IST ==1 ) THEN

       IF(OPT_ROOT == 1) THEN
           BTRAN =  KR
       END IF

       IF(OPT_ROOT == 2) THEN
         DO IZ = 1, parameters%NROOT
            IF(OPT_BTR == 1) then                  ! Noah
              GX    = (SH2O(IZ)-parameters%SMCWLT(IZ)) / (parameters%SMCREF(IZ)-parameters%SMCWLT(IZ))
            END IF
            IF(OPT_BTR == 2) then                  ! CLM
              GX    = (1.-PSI(IZ)/PSIWLT)/(1.+parameters%PSISAT(IZ)/PSIWLT)
            END IF
            IF(OPT_BTR == 3) then                  ! SSiB
              GX    = 1.-EXP(-5.8*(LOG(PSIWLT/PSI(IZ))))
            END IF

            GX         = MIN(1.,MAX(MPE,GX))

            BTRANI(IZ) = GX * FROOT(IZ)
            BTRAN      = BTRAN + BTRANI(IZ)
         END DO
         BTRANI(1:parameters%NROOT) = BTRANI(1:parameters%NROOT)/BTRAN
       END IF
     END IF

! soil surface resistance for ground evap.

     IF(IST == 2) THEN
       RSURF = 1.          ! avoid being divided by 0
       RHSUR = 1.0
     ELSE

     IF(OPT_WATRET == 1) THEN
          BEVAP = MIN(0.999,MAX(0.0,(SMC(1)-parameters%SMCR(1))/(parameters%SMCMAX(1)-parameters%SMCR(1))))

         !for constant depth:
         !D_RSURF = 2.2E-5 * parameters%SMCMAX(1)**2.0*(1.0-BEVAP)**(2.0+3.0/parameters%BEXP(1))
         !L_RSURF = 0.02 * (exp((1.0-BEVAP)**4.00) - 1.0) / 1.71828

         !for variable soil depth:
          D_RSURF = 2.2E-5 * parameters%SMCMAX(1)**2.0*(1.0-parameters%SMCWLT(1)/parameters%SMCMAX(1))** (2.0+3.0/parameters%BEXP(1))
          L_RSURF = 0.01 * (exp((1.0-BEVAP)**3.00) - 1.0) / 1.71828

          RSURF = FSNO * 1. + (1.-FSNO) * MAX(10.,L_RSURF/D_RSURF)
         !RHSUR = FSNO + (1.-FSNO) * EXP(PSI(1)*GRAV/(RW*TG))
          RHSUR = FSNO + (1.-FSNO) * EXP(-(-MIN(-0.01,PSI(1)))**0.5*GRAV/(RW*TG))

          IF((SMC(1)-parameters%SMCR(1)) < 0.01 .and. SNOWH == 0.) RSURF = 1.E6

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) "RSURF=",RSURF
    ! write(*,*) "RHSUR=",RHSUR
    ! write(*,*) "BEVAP=",BEVAP
    ! write(*,*) "SMC(1)=",SMC(1)
    ! write(*,*) "PSI(1)=",PSI(1)
    !END iF

     END IF

     IF(OPT_WATRET == 2) THEN
       IF(OPT_RSF == 1 .OR. OPT_RSF == 4) THEN
         ! RSURF based on Sakaguchi and Zeng, 2009
         ! taking the "residual water content" to be the wilting point, 
         ! and correcting the exponent on the D term (typo in SZ09 ?)
         !L_RSURF = (-ZSOIL(1)) * ( exp ( (1.0 - MIN(1.0,SH2O(1)/parameters%SMCMAX(1))) ** 5.0 ) - 1.0 ) / ( 2.71828 - 1.0 ) 
         BEVAP = MIN(0.999,MAX(0.001,SMC(1)/parameters%SMCMAX(1)))
         L_RSURF = 0.10 * ( exp ( (1.0 - MIN(1.0,BEVAP)) ** 5.00) - 1.0 ) / ( 2.71828 - 1.0 ) 
         D_RSURF = 2.2E-5 * parameters%SMCMAX(1)**2.0 * ( 1.0 - parameters%SMCWLT(1) / parameters%SMCMAX(1) ) ** (2.0+3.0/parameters%BEXP(1))
         RSURF = L_RSURF / D_RSURF
       ELSEIF(OPT_RSF == 2) THEN
         RSURF = FSNO * 1. + (1.-FSNO)* EXP(8.25-4.225*BEVAP) !Sellers (1992) ! Older RSURF computations
       ELSEIF(OPT_RSF == 3) THEN
         RSURF = FSNO * 1. + (1.-FSNO)* EXP(8.25-6.0  *BEVAP) !adjusted to decrease RSURF for wet soil
       ENDIF

       IF(OPT_RSF == 4) THEN  ! AD: FSNO weighted; snow RSURF set in MPTABLE v3.8
         RSURF = 1. / (FSNO * (1./parameters%RSURF_SNOW) + (1.-FSNO) * (1./max(RSURF, 0.001)))
       ENDIF

       IF(SH2O(1) < 0.01 .and. SNOWH == 0.) RSURF = 1.E6
       RHSUR = FSNO + (1.-FSNO) * EXP(PSI(1)*GRAV/(RW*TG)) 
     END IF
     END IF

! urban - jref 
     IF (parameters%urban_flag .and. SNOWH == 0. ) THEN
        RSURF = 1.E6
     ENDIF

! set psychrometric constant

     IF (TV .GT. TFRZ) THEN           ! Barlage: add distinction between ground and 
        LATHEAV = HVAP                ! vegetation in v3.6
	frozen_canopy = .false.
     ELSE
        LATHEAV = HSUB
	frozen_canopy = .true.
     END IF
     GAMMAV = CPAIR*SFCPRS/(0.622*LATHEAV)

     IF (TG .GT. TFRZ) THEN
        LATHEAG = HVAP
	FROZEN_GROUND = .false.
     ELSE
        LATHEAG = HSUB
	FROZEN_GROUND = .true.
     END IF
     GAMMAG = CPAIR*SFCPRS/(0.622*LATHEAG)

!     IF (SFCTMP .GT. TFRZ) THEN
!        LATHEA = HVAP
!     ELSE
!        LATHEA = HSUB
!     END IF
!     GAMMA = CPAIR*SFCPRS/(0.622*LATHEA)

! Surface temperatures of the ground and canopy and energy fluxes

    CANHS = 0.   !for bare soil

    IF (VEG .AND. FVEG > 0) THEN 
    TGV = TG
    CMV = CM
    CHV = CH

!    IF(ILOC == 137 .and. JLOC == 7) THEN
!     write(*,*) '----------before VEGE_FLUX-------------------------'
!     write(*,*) "TV=",TV
!     write(*,*) "TGV=",TGV
!     write(*,*) "SAV=",SAV
!    END IF

    CALL VEGE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !in
                    DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !in
                    UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !in
                    EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG   , & !in
                    FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !in
                    ZLVL    ,ZPD     ,Z0M     ,FVEG    ,MQ      , & !in
                    Z0MG    ,EMV     ,EMG     ,CANLIQ  ,FSNO    , & !in
                    CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !in
                    RSURF   ,LATHEAV ,LATHEAG ,PARSUN  ,PARSHA  ,IGS     , & !in
                    FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !in
                    RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHV  ,PAHG  , & !in
                    EAH     ,TAH     ,TV      ,TGV     ,CMV     , & !inout
                    CHV     ,DX      ,DZ8W    ,                   & !inout
                    TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !out
                    SHC     ,EVG     ,EVC     ,TR      ,GHV     , & !out
                    T2MV    ,PSNSUN  ,PSNSHA  ,CANHS   ,        & !out
!jref:start
                    QC      ,QSFC    ,PSFC    , & !in
                    Q2V     ,CHV2, CHLEAF, CHUC)               !inout 
!jref:end                            
    END IF

    if (isnan(PARSUN)) then
    write(*,*) 'after VEGE_FLUX: iloc,jloc=',iloc,jloc
    write(*,*) 'PARSUN, PARSHA', PARSUN,PARSHA
    END IF

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) '----------after VEGE_FLUX-------------------------'
    ! write(*,*) "TGV=",TGV
    ! write(*,*) "TV=",TV
    !END IF

    TGB = TG
    CMB = CM
    CHB = CH
    CALL BARE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !in
                    LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !in
                    THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !in
                    DZSNSO  ,ZLVL    ,ZPDG    ,Z0MG    ,FSNO,          & !in
                    EMG     ,STC     ,DF      ,RSURF   ,LATHEAG  , & !in
                    GAMMAG   ,RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHB  , & !in
                    TGB     ,CMB     ,CHB     ,                   & !inout
                    TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !out
                    GHB     ,T2MB    ,DX      ,DZ8W    ,VEGTYP  , & !out
!jref:start
                    QC      ,QSFC    ,PSFC    , & !in
                    SFCPRS  ,Q2B,   CHB2)                          !in 

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) '----------after BARE_FLUX-------------------------'
    ! write(*,*) "TGB=",TGB
    !END IF

!jref:end                            

!energy balance at vege canopy: SAV          =(IRC+SHC+EVC+TR)     *FVEG  at   FVEG 
!energy balance at vege ground: SAG*    FVEG =(IRG+SHG+EVG+GHV)    *FVEG  at   FVEG
!energy balance at bare ground: SAG*(1.-FVEG)=(IRB+SHB+EVB+GHB)*(1.-FVEG) at 1-FVEG

    IF (VEG .AND. FVEG > 0) THEN 
        TAUX  = FVEG * TAUXV     + (1.0 - FVEG) * TAUXB
        TAUY  = FVEG * TAUYV     + (1.0 - FVEG) * TAUYB
        FIRA  = FVEG * IRG       + (1.0 - FVEG) * IRB       + IRC
        FSH   = FVEG * SHG       + (1.0 - FVEG) * SHB       + SHC
        FGEV  = FVEG * EVG       + (1.0 - FVEG) * EVB
        SSOIL = FVEG * GHV       + (1.0 - FVEG) * GHB
        FCEV  = EVC
        FCTR  = TR
	PAH   = FVEG * PAHG      + (1.0 - FVEG) * PAHB   + PAHV
        TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
        T2M   = FVEG * T2MV      + (1.0 - FVEG) * T2MB
        TS    = FVEG * TV        + (1.0 - FVEG) * TGB
        CM    = FVEG * CMV       + (1.0 - FVEG) * CMB      ! better way to average?
        CH    = FVEG * CHV       + (1.0 - FVEG) * CHB
        Q1    = FVEG * (EAH*0.622/(SFCPRS - 0.378*EAH)) + (1.0 - FVEG)*QSFC
        Q2E   = FVEG * Q2V       + (1.0 - FVEG) * Q2B
	Z0WRF = Z0M
    ELSE
        TAUX  = TAUXB
        TAUY  = TAUYB
        FIRA  = IRB
        FSH   = SHB
        FGEV  = EVB
        SSOIL = GHB
        TG    = TGB
        T2M   = T2MB
        FCEV  = 0.
        FCTR  = 0.
	PAH   = PAHB
        TS    = TG
        CM    = CMB
        CH    = CHB
        Q1    = QSFC
        Q2E   = Q2B
        RSSUN = 0.0
        RSSHA = 0.0
        TGV   = TGB
        CHV   = CHB
	Z0WRF = Z0MG
    END IF

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) '----------after average-------------------------'
     !write(*,*) "FVEG=",FVEG
     !write(*,*) "FCTR=",FCTR
    ! write(*,*) "TV=",TV
    ! write(*,*) "TG=",TG
    !END IF

    FIRE = LWDN + FIRA

    IF(FIRE <=0.) THEN
       WRITE(*,*) 'emitted longwave <0; skin T may be wrong due to inconsistent'
       WRITE(*,*) 'input of SHDFAC with LAI'
       WRITE(*,*) 'VEGTYP=',VEGTYP
       WRITE(*,*) ILOC, JLOC, 'SHDFAC=',FVEG,'VAI=',VAI,'TV=',TV,'TG=',TG
       WRITE(*,*) 'LWDN=',LWDN,'FIRA=',FIRA,'SNOWH=',SNOWH
       call wrf_error_fatal("STOP in Noah-MP")
    END IF

    ! Compute a net emissivity
    EMISSI = FVEG * ( EMG*(1-EMV) + EMV + EMV*(1-EMV)*(1-EMG) ) + &
         (1-FVEG) * EMG

    ! When we're computing a TRAD, subtract from the emitted IR the
    ! reflected portion of the incoming LWDN, so we're just
    ! considering the IR originating in the canopy/ground system.
    
    TRAD = ( ( FIRE - (1-EMISSI)*LWDN ) / (EMISSI*SB) ) ** 0.25

    ! Old TRAD calculation not taking into account Emissivity:
    ! TRAD = (FIRE/SB)**0.25

    APAR = PARSUN*LAISUN + PARSHA*LAISHA
    PSN  = PSNSUN*LAISUN + PSNSHA*LAISHA

    if (isnan(PSN)) then
    write(*,*) 'PSN: iloc,jloc=',iloc,jloc
    write(*,*) 'PSNSUN,LAISUN , PSNSHA,LAISHA', PSNSUN,LAISUN , PSNSHA,LAISHA
    end if

! 3L snow & 4L soil temperatures

    CALL TSNOSOI (parameters,ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !in
                  TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !in
                  SAG     ,DT      ,SNOWH   ,DZSNSO  ,PHI     , & !in
                  TG      ,ILOC    ,JLOC    ,                   & !in
                  STC     )                                       !inout

! adjusting snow surface temperature
     IF(OPT_STC == 2) THEN
      IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
        TGV = TFRZ
        TGB = TFRZ
          IF (VEG .AND. FVEG > 0) THEN
             TG    = FVEG * TGV       + (1.0 - FVEG) * TGB
             TS    = FVEG * TV        + (1.0 - FVEG) * TGB
          ELSE
             TG    = TGB
             TS    = TGB
          END IF
      END IF
     END IF

! Energy released or consumed by snow & frozen soil

 CALL PHASECHANGE (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !in
                   DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & !in
                   STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !inout
                   SMC     ,SH2O    ,SICE    ,DMICE   ,          & !inout
                   QMELT   ,IMELT   ,PONDING ,XM )                 !out

!     IF(ILOC == 137 .and. JLOC == 7      ) THEN
!      write(*,*) '----------after SOILPHASE-------------------------'
!      write(*,*) "(STC       ",STC
!     write(*,*) "(SICE      ",SICE
!     write(*,*) "(SICE/SH2O ",SICE/SH2O
!     write(*,*) "(DMICE    (IZ),IZ=1,NSOIL)",(DMICE   (IZ),IZ=1,NSOIL)
!     END IF

  END SUBROUTINE ENERGY

!== begin thermoprop ===============================================================================

  SUBROUTINE THERMOPROP (parameters,NSOIL   ,NSNOW   ,ISNOW   ,IST     ,DZSNSO  , & !in
                         DT      ,SNOWH   ,SNICE   ,SNLIQ   , & !in
                         SMC     ,SH2O    ,TG      ,STC     ,UR      , & !in
                         LAT     ,Z0M     ,ZLVL    ,VEGTYP  , & !in
                         DF      ,HCPCT   ,SNICEV  ,SNLIQV  ,EPORE   , & !out
                         FACT    )                                       !out
! ------------------------------------------------------------------------------------------------- 
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER                        , INTENT(IN)  :: NSOIL   !number of soil layers
  INTEGER                        , INTENT(IN)  :: NSNOW   !maximum no. of snow layers        
  INTEGER                        , INTENT(IN)  :: ISNOW   !actual no. of snow layers
  INTEGER                        , INTENT(IN)  :: IST     !surface type
  REAL                           , INTENT(IN)  :: DT      !time step [s]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNICE   !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)  :: SNLIQ   !snow liq mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO  !thickness of snow/soil layers [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)  :: SMC     !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)  :: SH2O    !liquid soil moisture [m3/m3]
  REAL                           , INTENT(IN)  :: SNOWH   !snow height [m]
  REAL,                            INTENT(IN)  :: TG      !surface temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC     !snow/soil/lake temp. (k)
  REAL,                            INTENT(IN)  :: UR      !wind speed at ZLVL (m/s)
  REAL,                            INTENT(IN)  :: LAT     !latitude (radians)
  REAL,                            INTENT(IN)  :: Z0M     !roughness length (m)
  REAL,                            INTENT(IN)  :: ZLVL    !reference height (m)
  INTEGER                        , INTENT(IN)  :: VEGTYP  !vegtyp type

! outputs
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: DF      !thermal conductivity [w/m/k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: HCPCT   !heat capacity [j/m3/k]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV  !partial volume of ice [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV  !partial volume of liquid water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE   !effective porosity [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: FACT    !computing energy for phase change
! --------------------------------------------------------------------------------------------------
! locals

  INTEGER :: IZ
  REAL, DIMENSION(-NSNOW+1:    0)              :: CVSNO   !volumetric specific heat (j/m3/k)
  REAL, DIMENSION(-NSNOW+1:    0)              :: TKSNO   !snow thermal conductivity (j/m3/k)
  REAL, DIMENSION(       1:NSOIL)              :: SICE    !soil ice content
! --------------------------------------------------------------------------------------------------

! compute snow thermal conductivity and heat capacity

    CALL CSNOW (parameters,ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & !in
                TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   !out

    DO IZ = ISNOW+1, 0
      DF   (IZ) = TKSNO(IZ)
      HCPCT(IZ) = CVSNO(IZ)
    END DO

! compute soil thermal properties

    DO  IZ = 1, NSOIL
       SICE(IZ)  = SMC(IZ) - SH2O(IZ)
       HCPCT(IZ) = SH2O(IZ)*CWAT + (1.0-parameters%SMCMAX(IZ))*parameters%CSOIL &
                + (parameters%SMCMAX(IZ)-SMC(IZ))*CPAIR + SICE(IZ)*CICE
       CALL TDFCND (parameters,IZ,DF(IZ), SMC(IZ), SH2O(IZ))
    END DO
       
    IF ( parameters%urban_flag ) THEN
       DO IZ = 1,NSOIL
         DF(IZ) = 3.24
       END DO
    ENDIF

! heat flux reduction effect from the overlying green canopy, adapted from 
! section 2.1.2 of Peters-Lidard et al. (1997, JGR, VOL 102(D4)).
! not in use because of the separation of the canopy layer from the ground.
! but this may represent the effects of leaf litter (Niu comments)
!       DF1 = DF1 * EXP (SBETA * SHDFAC)

! compute lake thermal properties 
! (no consideration of turbulent mixing for this version)

    IF(IST == 2) THEN
       DO IZ = 1, NSOIL 
         IF(STC(IZ) > TFRZ) THEN
            HCPCT(IZ) = CWAT
            DF(IZ)    = TKWAT  !+ KEDDY * CWAT 
         ELSE
            HCPCT(IZ) = CICE
            DF(IZ)    = TKICE 
         END IF
       END DO
    END IF

! combine a temporary variable used for melting/freezing of snow and frozen soil

    DO IZ = ISNOW+1,NSOIL
     FACT(IZ) = DT/(HCPCT(IZ)*DZSNSO(IZ))
    END DO

! snow/soil interface

    IF(ISNOW == 0) THEN
       DF(1) = (DF(1)*DZSNSO(1)+0.35*SNOWH)      / (SNOWH    +DZSNSO(1)) 
    ELSE
       DF(1) = (DF(1)*DZSNSO(1)+DF(0)*DZSNSO(0)) / (DZSNSO(0)+DZSNSO(1))
    END IF


  END SUBROUTINE THERMOPROP

!== begin csnow ====================================================================================

  SUBROUTINE CSNOW (parameters,ISNOW   ,NSNOW   ,NSOIL   ,SNICE   ,SNLIQ   ,DZSNSO  , & !in
                    TKSNO   ,CVSNO   ,SNICEV  ,SNLIQV  ,EPORE   )   !out
! --------------------------------------------------------------------------------------------------
! Snow bulk density,volumetric capacity, and thermal conductivity
!---------------------------------------------------------------------------------------------------
  IMPLICIT NONE
!---------------------------------------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                          INTENT(IN) :: ISNOW  !number of snow layers (-)            
  INTEGER                        ,  INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER                        ,  INTENT(IN) :: NSOIL  !number of soil layers
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN) :: SNICE  !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN) :: SNLIQ  !snow liq mass (kg/m2) 
  REAL, DIMENSION(-NSNOW+1:NSOIL),  INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]

! outputs

  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: CVSNO  !volumetric specific heat (j/m3/k)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: TKSNO  !thermal conductivity (w/m/k)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNICEV !partial volume of ice [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: SNLIQV !partial volume of liquid water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(OUT) :: EPORE  !effective porosity [m3/m3]

! locals

  INTEGER :: IZ
  REAL, DIMENSION(-NSNOW+1:    0) :: BDSNOI  !bulk density of snow(kg/m3)

!---------------------------------------------------------------------------------------------------
! thermal capacity of snow

  DO IZ = ISNOW+1, 0
      SNICEV(IZ)   = MIN(1., SNICE(IZ)/(DZSNSO(IZ)*DENICE) )
      EPORE(IZ)    = 1. - SNICEV(IZ)
      SNLIQV(IZ)   = MIN(EPORE(IZ),SNLIQ(IZ)/(DZSNSO(IZ)*DENH2O))
  ENDDO

  DO IZ = ISNOW+1, 0
      BDSNOI(IZ) = (SNICE(IZ)+SNLIQ(IZ))/DZSNSO(IZ)
      CVSNO(IZ) = CICE*SNICEV(IZ)+CWAT*SNLIQV(IZ)
!      CVSNO(IZ) = 0.525E06                          ! constant
  enddo

! thermal conductivity of snow

  DO IZ = ISNOW+1, 0
     TKSNO(IZ) = 3.2217E-6*BDSNOI(IZ)**2.           ! Stieglitz(yen,1965)
!    TKSNO(IZ) = 2E-2+2.5E-6*BDSNOI(IZ)*BDSNOI(IZ)   ! Anderson, 1976
!    TKSNO(IZ) = 0.35                                ! constant
!    TKSNO(IZ) = 2.576E-6*BDSNOI(IZ)**2. + 0.074    ! Verseghy (1991)
!    TKSNO(IZ) = 2.22*(BDSNOI(IZ)/1000.)**1.88      ! Douvill(Yen, 1981)
  ENDDO

  END SUBROUTINE CSNOW

!== begin tdfcnd ===================================================================================

  SUBROUTINE TDFCND (parameters, ISOIL, DF, SMC, SH2O)
! --------------------------------------------------------------------------------------------------
! Calculate thermal diffusivity and conductivity of the soil.
! Peters-Lidard approach (Peters-Lidard et al., 1998)
! --------------------------------------------------------------------------------------------------
! Code history:
! June 2001 changes: frozen soil condition.
! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER, INTENT(IN)    :: ISOIL  ! soil layer
    REAL, INTENT(IN)       :: SMC    ! total soil water
    REAL, INTENT(IN)       :: SH2O   ! liq. soil water
    REAL, INTENT(OUT)      :: DF     ! thermal diffusivity

! local variables
    REAL  :: AKE
    REAL  :: GAMMD
    REAL  :: THKDRY
    REAL  :: THKO     ! thermal conductivity for other soil components         
    REAL  :: THKQTZ   ! thermal conductivity for quartz
    REAL  :: THKSAT   ! 
    REAL  :: THKS     ! thermal conductivity for the solids
    REAL  :: THKW     ! water thermal conductivity
    REAL  :: SATRATIO
    REAL  :: XU
    REAL  :: XUNFROZ
! --------------------------------------------------------------------------------------------------
! We now get quartz as an input argument (set in routine redprm):
!      DATA QUARTZ /0.82, 0.10, 0.25, 0.60, 0.52,
!     &             0.35, 0.60, 0.40, 0.82/
! --------------------------------------------------------------------------------------------------
! If the soil has any moisture content compute a partial sum/product
! otherwise use a constant value which works well with most soils
! --------------------------------------------------------------------------------------------------
!  QUARTZ ....QUARTZ CONTENT (SOIL TYPE DEPENDENT)
! --------------------------------------------------------------------------------------------------
! USE AS IN PETERS-LIDARD, 1998 (MODIF. FROM JOHANSEN, 1975).

!                                  PABLO GRUNMANN, 08/17/98
! Refs.:
!      Farouki, O.T.,1986: Thermal properties of soils. Series on Rock
!              and Soil Mechanics, Vol. 11, Trans Tech, 136 pp.
!      Johansen, O., 1975: Thermal conductivity of soils. PH.D. Thesis,
!              University of Trondheim,
!      Peters-Lidard, C. D., et al., 1998: The effect of soil thermal
!              conductivity parameterization on surface energy fluxes
!              and temperatures. Journal of The Atmospheric Sciences,
!              Vol. 55, pp. 1209-1224.
! --------------------------------------------------------------------------------------------------
! NEEDS PARAMETERS
! POROSITY(SOIL TYPE):
!      POROS = SMCMAX
! SATURATION RATIO:
! PARAMETERS  W/(M.K)
    SATRATIO = SMC / parameters%SMCMAX(ISOIL)
    THKW = 0.57
!      IF (QUARTZ .LE. 0.2) THKO = 3.0
    THKO = 2.0
! SOLIDS' CONDUCTIVITY
! QUARTZ' CONDUCTIVITY
    THKQTZ = 7.7

! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
    THKS = (THKQTZ ** parameters%QUARTZ(ISOIL))* (THKO ** (1. - parameters%QUARTZ(ISOIL)))

! UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
    XUNFROZ = 1.0                       ! Prevent divide by zero (suggested by D. Mocko)
    IF(SMC > 0.) XUNFROZ = SH2O / SMC
! SATURATED THERMAL CONDUCTIVITY
    XU = XUNFROZ * parameters%SMCMAX(ISOIL)

! DRY DENSITY IN KG/M3
    THKSAT = THKS ** (1. - parameters%SMCMAX(ISOIL))* TKICE ** (parameters%SMCMAX(ISOIL) - XU)* THKW **   &
         (XU)

! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
    GAMMD = (1. - parameters%SMCMAX(ISOIL))*2700.

    THKDRY = (0.135* GAMMD+ 64.7)/ (2700. - 0.947* GAMMD)
! FROZEN
    IF ( (SH2O + 0.0005) <  SMC ) THEN
       AKE = SATRATIO
! UNFROZEN
! RANGE OF VALIDITY FOR THE KERSTEN NUMBER (AKE)
    ELSE

! KERSTEN NUMBER (USING "FINE" FORMULA, VALID FOR SOILS CONTAINING AT
! LEAST 5% OF PARTICLES WITH DIAMETER LESS THAN 2.E-6 METERS.)
! (FOR "COARSE" FORMULA, SEE PETERS-LIDARD ET AL., 1998).

       IF ( SATRATIO >  0.1 ) THEN

          AKE = LOG10 (SATRATIO) + 1.0

! USE K = KDRY
       ELSE

          AKE = 0.0
       END IF
!  THERMAL CONDUCTIVITY

    END IF

    DF = AKE * (THKSAT - THKDRY) + THKDRY


  end subroutine TDFCND

!== begin radiation ================================================================================

  SUBROUTINE RADIATION (parameters,rad_cons,VEGTYP  ,IST     ,ICE     ,NSOIL   , & !in
                        SNEQVO  ,SNEQV   ,DT      ,COSZ    ,SNOWH   , & !in
                        TG      ,TV      ,FSNO    ,QSNOW   ,FWET    , & !in
                        ELAI    ,ESAI    ,SMC     ,SOLAD   ,SOLAI   , & !in
                        NSNOW   ,ISNOW   ,SNICE   ,SNLIQ   ,DZSNSO  , & !in
                        radius  ,FVEG    ,ILOC    ,JLOC    ,          & !in
                        ALBOLD  ,TAUSS   ,                            & !inout
                        FSUN    ,LAISUN  ,LAISHA  ,PARSUN  ,PARSHA  , & !out
                        SAV     ,SAG     ,FSR     ,FSA     ,FSRV    , &
                        FSRG    ,ALBSND  ,ALBSNI  ,BGAP    ,WGAP    ,NDVI  ,& !out
                        PHI     )  !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  type (constants), intent(in) :: rad_cons
  INTEGER, INTENT(IN)                  :: ILOC
  INTEGER, INTENT(IN)                  :: JLOC
  INTEGER, INTENT(IN)                  :: VEGTYP !vegetation type
  INTEGER, INTENT(IN)                  :: IST    !surface type
  INTEGER, INTENT(IN)                  :: ICE    !ice (ice = 1)
  INTEGER, INTENT(IN)                  :: NSOIL  !number of soil layers

  REAL, INTENT(IN)                     :: DT     !time step [s]
  REAL, INTENT(IN)                     :: QSNOW  !snowfall (mm/s)
  REAL, INTENT(IN)                     :: SNEQVO !snow mass at last time step(mm)
  REAL, INTENT(IN)                     :: SNEQV  !snow mass (mm)
  REAL, INTENT(IN)                     :: SNOWH  !snow height (mm)
  REAL, INTENT(IN)                     :: COSZ   !cosine solar zenith angle (0-1)
  REAL, INTENT(IN)                     :: TG     !ground temperature (k)
  REAL, INTENT(IN)                     :: TV     !vegetation temperature (k)
  REAL, INTENT(IN)                     :: ELAI   !LAI, one-sided, adjusted for burying by snow
  REAL, INTENT(IN)                     :: ESAI   !SAI, one-sided, adjusted for burying by snow
  REAL, INTENT(IN)                     :: FWET   !fraction of canopy that is wet
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SMC    !volumetric soil water [m3/m3]
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAD  !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(1:2)    , INTENT(IN) :: SOLAI  !incoming diffuse solar radiation (w/m2)
  REAL, INTENT(IN)                     :: FSNO   !snow cover fraction (-)
  REAL, INTENT(IN)                     :: FVEG   !green vegetation fraction [0.0-1.0]

  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(in)  :: radius
  INTEGER,                          INTENT(IN)  :: NSNOW   !maximum no. of snow layers
  INTEGER,                          INTENT(IN)  :: ISNOW   !actual no. of snow layers
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN)  :: SNICE   !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN)  :: SNLIQ   !snow liq mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:NSOIL),  INTENT(IN)  :: DZSNSO  !snow/soil layer thickness [m]

! inout
  REAL,                  INTENT(INOUT) :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL,                  INTENT(INOUT) :: TAUSS  !non-dimensional snow age.

! output
  REAL, INTENT(OUT)                    :: FSUN   !sunlit fraction of canopy (-)
  REAL, INTENT(OUT)                    :: LAISUN !sunlit leaf area (-)
  REAL, INTENT(OUT)                    :: LAISHA !shaded leaf area (-)
  REAL, INTENT(OUT)                    :: PARSUN !average absorbed par for sunlit leaves (w/m2)
  REAL, INTENT(OUT)                    :: PARSHA !average absorbed par for shaded leaves (w/m2)
  REAL, INTENT(OUT)                    :: SAV    !solar radiation absorbed by vegetation (w/m2)
  REAL, INTENT(OUT)                    :: SAG    !solar radiation absorbed by ground (w/m2)
  REAL, INTENT(OUT)                    :: FSA    !total absorbed solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: FSR    !total reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: NDVI   !NDVI
  REAL, DIMENSION(-NSNOW+1:NSOIL),    INTENT(out)  :: PHI

!jref:start  
  REAL, INTENT(OUT)                    :: FSRV    !veg. reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: FSRG    !ground reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                    :: BGAP
  REAL, INTENT(OUT)                    :: WGAP
  REAL, DIMENSION(1:2), INTENT(OUT)    :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:2), INTENT(OUT)    :: ALBSNI   !snow albedo (diffuse)
!jref:end  

! local
  REAL                                 :: FAGE   !snow age function (0 - new snow)
  REAL, DIMENSION(1:2)                 :: ALBGRD !ground albedo (direct)
  REAL, DIMENSION(1:2)                 :: ALBGRI !ground albedo (diffuse)
  REAL, DIMENSION(1:2)                 :: ALBD   !surface albedo (direct)
  REAL, DIMENSION(1:2)                 :: ALBI   !surface albedo (diffuse)
  REAL, DIMENSION(1:2)                 :: FABD   !flux abs by veg (per unit direct flux)
  REAL, DIMENSION(1:2)                 :: FABI   !flux abs by veg (per unit diffuse flux)
  REAL, DIMENSION(1:2)                 :: FTDD   !down direct flux below veg (per unit dir flux)
  REAL, DIMENSION(1:2)                 :: FTID   !down diffuse flux below veg (per unit dir flux)
  REAL, DIMENSION(1:2)                 :: FTII   !down diffuse flux below veg (per unit dif flux)
  REAL, DIMENSION(-NSNOW+1:0,1:2)      :: F_abs_D
  REAL, DIMENSION(-NSNOW+1:0,1:2)      :: F_abs_I
  REAL, DIMENSION(1:2           )      :: F_btm_D
  REAL, DIMENSION(1:2           )      :: F_btm_I
!jref:start  
  REAL, DIMENSION(1:2)                 :: FREVI
  REAL, DIMENSION(1:2)                 :: FREVD
  REAL, DIMENSION(1:2)                 :: FREGI
  REAL, DIMENSION(1:2)                 :: FREGD
!jref:end

  REAL                                 :: FSHA   !shaded fraction of canopy
  REAL                                 :: VAI    !total LAI + stem area index, one sided

  REAL,PARAMETER :: MPE = 1.E-6
  LOGICAL VEG  !true: vegetated for surface temperature calculation

! --------------------------------------------------------------------------------------------------

! surface abeldo

   CALL ALBEDO (parameters,rad_cons,VEGTYP ,IST    ,ICE    ,NSOIL  , & !in
                DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & !in
                TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & !in
                SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & !in
                NSNOW  ,ISNOW  ,SNICE  ,SNLIQ  ,DZSNSO , radius, & !in
                ILOC   ,JLOC   ,                         & !in
                ALBOLD ,TAUSS                          , & !inout
                ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & !out
                FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & !)   !out
                FREVI  ,FREVD   ,FREGD ,FREGI  ,BGAP   , & !inout
                WGAP   ,ALBSND ,ALBSNI , &
                F_abs_D,F_abs_I,F_btm_D,F_btm_I)  ! out 

! surface radiation

     FSHA = 1.-FSUN
     LAISUN = ELAI*FSUN
     LAISHA = ELAI*FSHA
     VAI = ELAI+ ESAI
     IF (VAI .GT. 0.) THEN
        VEG = .TRUE.
     ELSE
        VEG = .FALSE.
     END IF

   CALL SURRAD (parameters,MPE    ,FSUN   ,FSHA   ,ELAI   ,VAI    , & !in
                LAISUN ,LAISHA ,SOLAD  ,SOLAI  ,FABD   , & !in
                FABI   ,FTDD   ,FTID   ,FTII   ,ALBGRD , & !in
                ALBGRI ,ALBD   ,ALBI   ,ILOC   ,JLOC   , & !in
                NSNOW  ,NSOIL  ,ISNOW  ,FSNO    , & ! in
                F_abs_D,F_abs_I,F_btm_D,F_btm_I , & ! in 
                ALBSND ,ALBSNI , & ! in
                PARSUN ,PARSHA ,SAV    ,SAG    ,FSA    , & !out
                FSR    ,NDVI   ,PHI    ,                 & !out
                FREVI  ,FREVD  ,FREGD  ,FREGI  ,FSRV   , & !inout
                FSRG)

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) '----------after SURRAD-------------------------'
    ! write(*,*) "SAG=",SAG
    ! write(*,*) "SAG=",SAV
    !END IF

  END SUBROUTINE RADIATION

!== begin albedo ===================================================================================

  SUBROUTINE ALBEDO (parameters,rad_cons,VEGTYP ,IST    ,ICE    ,NSOIL  , & !in
                     DT     ,COSZ   ,FAGE   ,ELAI   ,ESAI   , & !in
                     TG     ,TV     ,SNOWH  ,FSNO   ,FWET   , & !in
                     SMC    ,SNEQVO ,SNEQV  ,QSNOW  ,FVEG   , & !in
                     NSNOW  ,ISNOW  ,SNICE  ,SNLIQ  ,DZSNSO , radius, & !in
                     ILOC   ,JLOC   ,                         & !in
                     ALBOLD ,TAUSS                          , & !inout
                     ALBGRD ,ALBGRI ,ALBD   ,ALBI   ,FABD   , & !out
                     FABI   ,FTDD   ,FTID   ,FTII   ,FSUN   , & !out
                     FREVI  ,FREVD  ,FREGD  ,FREGI  ,BGAP   , & !out
                     WGAP   ,ALBSND ,ALBSNI , &
                     F_abs_D,F_abs_I,F_btm_D, F_btm_I )  ! out

! --------------------------------------------------------------------------------------------------
! surface albedos. also fluxes (per unit incoming direct and diffuse
! radiation) reflected, transmitted, and absorbed by vegetation.
! also sunlit fraction of the canopy.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (constants), intent(in) :: rad_cons
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN)  :: ILOC
  INTEGER,                  INTENT(IN)  :: JLOC
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: VEGTYP !vegetation type
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  INTEGER,                  INTENT(IN)  :: ICE    !ice (ice = 1)

  REAL,                     INTENT(IN)  :: DT     !time step [sec]
  REAL,                     INTENT(IN)  :: QSNOW  !snowfall
  REAL,                     INTENT(IN)  :: COSZ   !cosine solar zenith angle for next time step
  REAL,                     INTENT(IN)  :: SNOWH  !snow height (mm)
  REAL,                     INTENT(IN)  :: TG     !ground temperature (k)
  REAL,                     INTENT(IN)  :: TV     !vegetation temperature (k)
  REAL,                     INTENT(IN)  :: ELAI   !LAI, one-sided, adjusted for burying by snow
  REAL,                     INTENT(IN)  :: ESAI   !SAI, one-sided, adjusted for burying by snow
  REAL,                     INTENT(IN)  :: FSNO   !fraction of grid covered by snow
  REAL,                     INTENT(IN)  :: FWET   !fraction of canopy that is wet
  REAL,                     INTENT(IN)  :: SNEQVO !snow mass at last time step(mm)
  REAL,                     INTENT(IN)  :: SNEQV  !snow mass (mm)
  REAL,                     INTENT(IN)  :: FVEG   !green vegetation fraction [0.0-1.0]
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    !volumetric soil water (m3/m3)

  INTEGER                        ,  INTENT(IN)   :: NSNOW   !maximum no. of snow layers
  INTEGER                        ,  INTENT(IN)   :: ISNOW   !actual no. of snow layers
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN)   :: SNICE   !snow ice mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN)   :: SNLIQ   !snow liq mass (kg/m2)
  REAL, DIMENSION(-NSNOW+1:NSOIL),  INTENT(IN)   :: DZSNSO  !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:    0),  INTENT(IN)   :: radius  !snow grain radius

! inout
  REAL,                  INTENT(INOUT)  :: ALBOLD !snow albedo at last time step (CLASS type)
  REAL,                  INTENT(INOUT)  :: TAUSS  !non-dimensional snow age

! output
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRD !ground albedo (direct)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRI !ground albedo (diffuse)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBD   !surface albedo (direct)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBI   !surface albedo (diffuse)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FABD   !flux abs by veg (per unit direct flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FABI   !flux abs by veg (per unit diffuse flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTDD   !down direct flux below veg (per unit dir flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTID   !down diffuse flux below veg (per unit dir flux)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FTII   !down diffuse flux below veg (per unit dif flux)
  REAL,                     INTENT(OUT) :: FSUN   !sunlit fraction of canopy (-)

  REAL, DIMENSION(-NSNOW+1:0,1:2),    INTENT(out)  :: F_abs_D       !absorbed radiation by snow layers [w/m2]
  REAL, DIMENSION(-NSNOW+1:0,1:2),    INTENT(out)  :: F_abs_I       !absorbed radiation by snow layers [w/m2]
  REAL, DIMENSION(1:2),               INTENT(out)  :: F_btm_D       !absorbed radiation by soil surface [w/m2]
  REAL, DIMENSION(1:2),               INTENT(out)  :: F_btm_I       !absorbed radiation by soil surface [w/m2]

!jref:start
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREVD
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREVI
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREGD
  REAL, DIMENSION(1:    2), INTENT(OUT) :: FREGI
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBSND   !snow albedo (direct)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBSNI   !snow albedo (diffuse)
  REAL, INTENT(OUT) :: BGAP
  REAL, INTENT(OUT) :: WGAP
!jref:end

! ------------------------------------------------------------------------
! ------------------------ local variables -------------------------------
! local
  REAL                 :: FAGE     !snow age function
  REAL                 :: ALB
  INTEGER              :: IB       !indices
  INTEGER              :: IC       !direct beam: ic=0; diffuse: ic=1

  REAL                 :: WL       !fraction of LAI+SAI that is LAI
  REAL                 :: WS       !fraction of LAI+SAI that is SAI

  REAL, DIMENSION(1:2) :: RHO      !leaf/stem reflectance weighted by fraction LAI and SAI
  REAL, DIMENSION(1:2) :: TAU      !leaf/stem transmittance weighted by fraction LAI and SAI
  REAL, DIMENSION(1:2) :: FTDI     !down direct flux below veg per unit dif flux = 0

  REAL                 :: VAI      !ELAI+ESAI
  REAL                 :: GDIR     !average projected leaf/stem area in solar direction
  REAL                 :: EXT      !optical depth direct beam per unit leaf + stem area

  INTEGER                                          :: nbr_lyr
  INTEGER                                          :: nbr_aer
  INTEGER                                          :: SNO_SHP

  REAL, DIMENSION(1:NSNOW)                  :: dz       !snow layer thickness [m]
  REAL, DIMENSION(1:NSNOW)                  :: rho_snw  !snow layer density
  REAL, DIMENSION(1:NSNOW)                  :: rds_snw  !snow layer grain radius
  REAL, DIMENSION(-NSNOW+1:0,1:2)                  :: F_down_D !downward radiation [w/m2]
  REAL, DIMENSION(-NSNOW+1:0,1:2)                  :: F_down_I !downward radiation [w/m2]

  INTEGER              :: i, ii , id
  REAL, DIMENSION(1:2) :: ALBSOD !soil albedo (direct)
  REAL, DIMENSION(1:2) :: ALBSOI !soil albedo (diffuse)
  REAL                 :: time_begin1, time_begin2, time_begin3, time_end1, time_end2, time_end3
  REAL                 :: alb_dir, alb_dif, COSZEN
  REAL                 :: alb_t_dir_vis, alb_t_dir_nir, alb_t_dif_vis, alb_t_dif_nir
  REAL, DIMENSION(1:5) :: alb_0_dir, alb_t_dir, alb_0_dif, alb_t_dif
  REAL, DIMENSION(1:5) :: flx_clr=(/0.5028,0.2454,0.0900,0.0601,0.1017/)
  REAL, DIMENSION(1:5) :: flx_cld=(/0.5767,0.2480,0.0853,0.0462,0.0438/)

  INTEGER, PARAMETER :: APRX_TYP = 3       !approximation
  INTEGER, PARAMETER :: DELTA    = 1       !delta transformation
  INTEGER, PARAMETER :: NBAND    = 2       !number of solar radiation wave bands
  REAL   , PARAMETER :: MPE      = 1.E-06  !prevents overflow for division by zero
  REAL :: time2, time1
! --------------------------------------------------------------------------------------------------

  BGAP = 0.
  WGAP = 0.
  nbr_lyr  = 0
  nbr_aer  = 0

! initialize output because solar radiation only done if COSZ > 0

  DO IB = 1, NBAND
    ALBD(IB) = 0.
    ALBI(IB) = 0.
    ALBGRD(IB) = 0.2  !to aviod being divided by zero in TWOSTREAM
    ALBGRI(IB) = 0.2
    ALBSND(IB) = 0.0
    ALBSNI(IB) = 0.0
    FABD(IB) = 0.
    FABI(IB) = 0.
    FTDD(IB) = 0.
    FTID(IB) = 0.
    FTII(IB) = 0.
    IF (IB.EQ.1) FSUN = 0.

    F_btm_D (IB)  = 0.
    F_btm_I (IB)  = 0.
    F_abs_D (-NSNOW+1:0,IB)  = 0.
    F_abs_I (-NSNOW+1:0,IB)  = 0.
    F_down_D(-NSNOW+1:0,IB)  = 0.
    F_down_I(-NSNOW+1:0,IB)  = 0.
  END DO

    ALBSND(1)= 0.85          ! vis direct
    ALBSND(2)= 0.65          ! nir direct
    ALBSNI(1)= 0.85          ! vis diffuse
    ALBSNI(2)= 0.65          ! nir diffuse

  IF(COSZ <= 0.) GOTO 100

! weight reflectance/transmittance by LAI and SAI

  DO IB = 1, NBAND
    VAI = ELAI + ESAI
    WL  = ELAI / MAX(VAI,MPE)
    WS  = ESAI / MAX(VAI,MPE)
    RHO(IB) = MAX(parameters%RHOL(IB)*WL+parameters%RHOS(IB)*WS, MPE)
    TAU(IB) = MAX(parameters%TAUL(IB)*WL+parameters%TAUS(IB)*WS, MPE)
  END DO

! snow age

   CALL SNOW_AGE (parameters,DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)

! snow albedos: only if COSZ > 0 and FSNO > 0

  IF(OPT_ALB == 1) &
     CALL SNOWALB_BATS (parameters,NBAND, FSNO,COSZ,FAGE,ALBSND,ALBSNI)
  IF(OPT_ALB == 2) THEN
    !call CPU_TIME(time1)
     CALL SNOWALB_CLASS (parameters,NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
     ALBOLD = ALB
    !call CPU_TIME(time2)
  END IF

  IF(OPT_ALB == 3) THEN
    !call CPU_TIME(time1)

     COSZEN= MAX(0.001,COSZ) !0.65 !NIU ???

     call SOIL_ALBEDO (parameters, NSOIL   ,NBAND   ,ICE     ,IST     , & !in
                       SMC     ,COSZ    ,                               & !in
                       TG      ,ILOC    ,JLOC    ,                      & !in
                       ALBSOD  ,ALBSOI  )

     alb_t_dir(1)  =ALBSOD(1) ; alb_t_dir(2:5)=ALBSOD(2)
     alb_t_dif(1)  =ALBSOI(1) ; alb_t_dif(2:5)=ALBSOI(2)

     if(abs(ISNOW) > 0) then
          nbr_lyr       = abs(ISNOW)
          nbr_aer       = 7

          do i=1,nbr_lyr
             dz(i)      = DZSNSO(ISNOW+i)
             rho_snw(i) = (SNICE(ISNOW+i)+SNLIQ(ISNOW+i))/dz(i)  !kg/m3
             rds_snw(i) = radius(ISNOW+i)       !um
          end do
     end if

     if (nbr_lyr > 0) then

        SNO_SHP = 1

        !! direct
        do i=nbr_lyr,1,-1

           !**************************
           dz(i) = min(0.3,dz(i))
           !**************************

           alb_0_dir = alb_t_dir

!niu       ii=int(rds_snw(i))-50+1
           ii=MIN(1451,int(rds_snw(i))-50+1)

           if (i.eq.1) then

             call  ESM_SNICAR_direct ( APRX_TYP, DELTA, rds_snw(i), rho_snw(i), dz(i), alb_0_dir, alb_t_dir, COSZEN, &
                                 ii,rad_cons,nbr_aer, &
                                 F_abs_D(ISNOW+i,1), F_abs_D(ISNOW+i,2), F_down_D(ISNOW+i,1), F_down_D(ISNOW+i,2) )

           else
             call  ESM_SNICAR_difus  ( APRX_TYP, DELTA, rds_snw(i), rho_snw(i), dz(i), alb_0_dir, alb_t_dir, COSZ, &
                                 ii,rad_cons,nbr_aer, &
                                 F_abs_D(ISNOW+i,1), F_abs_D(ISNOW+i,2), F_down_D(ISNOW+i,1), F_down_D(ISNOW+i,2) )
           end if

        end do

        !! diffuse

        do i=nbr_lyr,1,-1

           !*****************************
           dz(i) = min(0.3,dz(i))
           !*****************************

           alb_0_dif = alb_t_dif

!niu       ii=int(rds_snw(i))-50+1
           ii=MIN(1451,int(rds_snw(i))-50+1)

           call  ESM_SNICAR_difus ( APRX_TYP, DELTA, rds_snw(i), rho_snw(i), dz(i), alb_0_dif, alb_t_dif, COSZ, &
                                   ii,rad_cons,nbr_aer, &
                                   F_abs_I(ISNOW+i,1), F_abs_I(ISNOW+i,2), F_down_I(ISNOW+i,1), F_down_I(ISNOW+i,2) )

        end do

        alb_t_dir_vis = alb_t_dir(1)
        alb_t_dir_nir = sum(flx_clr(2:5)*alb_t_dir(2:5))/sum(flx_clr(2:5))
        alb_dir       = sum(flx_clr*alb_t_dir)/sum(flx_clr)

        alb_t_dif_vis = alb_t_dif(1)
        alb_t_dif_nir = sum(flx_cld(2:5)*alb_t_dif(2:5))/sum(flx_cld(2:5))
        alb_dif       = sum(flx_cld*alb_t_dif)/sum(flx_cld)

        do i=ISNOW+1, 0
            if (i.gt.ISNOW+1) then

               do id=i-1,ISNOW+1,-1

                   F_abs_D(i,1) =F_down_D(id,1)*F_abs_D(i,1)
                   F_abs_I(i,1) =F_down_I(id,1)*F_abs_I(i,1)

                   F_abs_D(i,2) =F_down_D(id,2)*F_abs_D(i,2)
                   F_abs_I(i,2) =F_down_I(id,2)*F_abs_I(i,2)

               end do

            end if
        end do

        ALBSND(1)= alb_t_dir_vis        ! vis direct
        ALBSND(2)= alb_t_dir_nir        ! nir direct
        ALBSNI(1)= alb_t_dif_vis        ! vis diffuse
        ALBSNI(2)= alb_t_dif_nir        ! nir diffuse

        F_btm_D(1)   = 1.0-ALBSND(1)-sum(F_abs_D(:, 1) )
        F_btm_D(2)   = 1.0-ALBSND(2)-sum(F_abs_D(:, 2) )
        F_btm_I(1)   = 1.0-ALBSNI(1)-sum(F_abs_I(:, 1) )
        F_btm_I(2)   = 1.0-ALBSNI(2)-sum(F_abs_I(:, 2) )

     IF(ALBSNI(1)==0. .or. ALBSNI(2)==0.)  THEN
      write(*,*) "ILOC,JLOC,FSNO,SNOWH", ILOC,JLOC,FSNO,SNOWH
      write(*,*) 'ALBSNI(1),ALBSNO(2)=',ALBSNI(1),ALBSNI(2)
      write(*,*) "alb_t_dif_vis,alb_t_dif_nir =",alb_t_dif_vis,alb_t_dif_nir
     END IF

     end if

   alb=(sum(ALBSND)/2.+sum(ALBSNI)/2.)/2.

  END IF  !(OPT_ALB = 3)

! ground surface albedo

    !IF(ALBSNI(1)==0. .or. ALBSNI(2)==0.)  THEN
    ! write(*,*) "before GROUNDALB: ALBSND,FSNO =",ALBSND,FSNO
    ! write(*,*) "before GROUNDALB: ALBSNI,FSNO =",ALBSNI,FSNO
    !END IF

  CALL GROUNDALB (parameters,NSOIL   ,NBAND   ,ICE     ,IST     , & !in
                  FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & !in
                  TG      ,ILOC    ,JLOC    ,                   & !in
                  ALBGRD  ,ALBGRI  )                              !out

! loop over NBAND wavebands to calculate surface albedos and solar
! fluxes for unit incoming direct (IC=0) and diffuse flux (IC=1)

  DO IB = 1, NBAND
      IC = 0      ! direct
      CALL TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & !in
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !in
                      FABD   ,ALBD    ,FTDD    ,FTID    ,GDIR   , &!)   !out
                      FREVD  ,FREGD   ,BGAP    ,WGAP)

      IC = 1      ! diffuse
      CALL TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
                      FWET   ,TV      ,ALBGRD  ,ALBGRI  ,RHO    , & !in
                      TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !in
                      FABI   ,ALBI    ,FTDI    ,FTII    ,GDIR   , & !)   !out
                      FREVI  ,FREGI   ,BGAP    ,WGAP)

  END DO

! sunlit fraction of canopy. set FSUN = 0 if FSUN < 0.01.

  EXT = GDIR/MAX(0.0001,COSZ) * SQRT(1.-RHO(1)-TAU(1))
  FSUN = (1.-EXP(-EXT*VAI)) / MAX(EXT*VAI,MPE)
! FSUN = (1.-EXP(-EXT*MAX(VAI,0.01))) / EXT*MAX(VAI,0.01)
  EXT = FSUN

  IF (EXT .LT. 0.01) THEN
     WL = 0.
  ELSE
     WL = EXT 
  END IF
  FSUN = WL

100 CONTINUE

  END SUBROUTINE ALBEDO

!== begin surrad ===================================================================================

  SUBROUTINE SURRAD (parameters,MPE     ,FSUN    ,FSHA    ,ELAI    ,VAI     , & !in
                     LAISUN  ,LAISHA  ,SOLAD   ,SOLAI   ,FABD    , & !in
                     FABI    ,FTDD    ,FTID    ,FTII    ,ALBGRD  , & !in
                     ALBGRI  ,ALBD    ,ALBI    ,ILOC    ,JLOC    , & !in
                     NSNOW   ,NSOIL   ,ISNOW   ,FSNO    , & ! in
                     F_abs_D ,F_abs_I ,F_btm_D ,F_btm_I , & ! in
                     ALBSND  ,ALBSNI  , & ! in
                     PARSUN  ,PARSHA  ,SAV     ,SAG     ,FSA     , & !out
                     FSR     ,NDVI    ,PHI     , & !out
                     FREVI   ,FREVD   ,FREGD   ,FREGI   ,FSRV    , &
                     FSRG    )

! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER, INTENT(IN)              :: ILOC
  INTEGER, INTENT(IN)              :: JLOC
  REAL, INTENT(IN)                 :: MPE     !prevents underflow errors if division by zero

  REAL, INTENT(IN)                 :: FSUN    !sunlit fraction of canopy
  REAL, INTENT(IN)                 :: FSHA    !shaded fraction of canopy
  REAL, INTENT(IN)                 :: ELAI    !leaf area, one-sided
  REAL, INTENT(IN)                 :: VAI     !leaf + stem area, one-sided
  REAL, INTENT(IN)                 :: LAISUN  !sunlit leaf area index, one-sided
  REAL, INTENT(IN)                 :: LAISHA  !shaded leaf area index, one-sided

  REAL, DIMENSION(1:2), INTENT(IN) :: SOLAD   !incoming direct solar radiation (w/m2)
  REAL, DIMENSION(1:2), INTENT(IN) :: SOLAI   !incoming diffuse solar radiation (w/m2)
  REAL, DIMENSION(1:2), INTENT(IN) :: FABD    !flux abs by veg (per unit incoming direct flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FABI    !flux abs by veg (per unit incoming diffuse flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTDD    !down dir flux below veg (per incoming dir flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTID    !down dif flux below veg (per incoming dir flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: FTII    !down dif flux below veg (per incoming dif flux)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBGRD  !ground albedo (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBGRI  !ground albedo (diffuse)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBD    !overall surface albedo (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: ALBI    !overall surface albedo (diffuse)

  REAL, DIMENSION(1:2), INTENT(IN) :: FREVD    !overall surface albedo veg (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREVI    !overall surface albedo veg (diffuse)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREGD    !overall surface albedo grd (direct)
  REAL, DIMENSION(1:2), INTENT(IN) :: FREGI    !overall surface albedo grd (diffuse)

  INTEGER                        ,  INTENT(IN)   :: NSNOW   !maximum no. of snow layers
  INTEGER                        ,  INTENT(IN)   :: NSOIL   !maximum no. of soil layers
  INTEGER                        ,  INTENT(IN)   :: ISNOW   !actual no. of snow layers
  REAL, INTENT(IN)                 :: FSNO

  REAL, DIMENSION(1:2)           ,  INTENT(IN)   :: ALBSND  !ground albedo (direct)
  REAL, DIMENSION(1:2)           ,  INTENT(IN)   :: ALBSNI  !ground albedo (diffuse)
  REAL, DIMENSION(-NSNOW+1:0,1:2),  INTENT(IN)   :: F_abs_D
  REAL, DIMENSION(-NSNOW+1:0,1:2),  INTENT(IN)   :: F_abs_I
  REAL, DIMENSION(1:2)           ,  INTENT(IN)   :: F_btm_D
  REAL, DIMENSION(1:2)           ,  INTENT(IN)   :: F_btm_I

! output

  REAL, INTENT(OUT)                :: PARSUN  !average absorbed par for sunlit leaves (w/m2)
  REAL, INTENT(OUT)                :: PARSHA  !average absorbed par for shaded leaves (w/m2)
  REAL, INTENT(OUT)                :: SAV     !solar radiation absorbed by vegetation (w/m2)
  REAL, INTENT(OUT)                :: SAG     !solar radiation absorbed by ground (w/m2)
  REAL, INTENT(OUT)                :: FSA     !total absorbed solar radiation (w/m2)
  REAL, INTENT(OUT)                :: FSR     !total reflected solar radiation (w/m2)
  REAL, INTENT(OUT)                :: FSRV    !reflected solar radiation by vegetation
  REAL, INTENT(OUT)                :: FSRG    !reflected solar radiation by ground
  REAL, INTENT(OUT)                :: NDVI    !NDVI
  REAL, DIMENSION(-NSNOW+1:NSOIL),  INTENT(OUT)  :: PHI

! ------------------------ local variables ----------------------------------------------------
  INTEGER                          :: IB      !waveband number (1=vis, 2=nir)
  INTEGER                          :: NBAND   !number of solar radiation waveband classes

  REAL                             :: ABSO    !absorbed solar radiation (w/m2)
  REAL                             :: RNIR    !reflected solar radiation [nir] (w/m2)
  REAL                             :: RVIS    !reflected solar radiation [vis] (w/m2)
  REAL                             :: LAIFRA  !leaf area fraction of canopy
  REAL                             :: TRD     !transmitted solar radiation: direct (w/m2)
  REAL                             :: TRI     !transmitted solar radiation: diffuse (w/m2)
  REAL, DIMENSION(1:2)             :: CAD     !direct beam absorbed by canopy (w/m2)
  REAL, DIMENSION(1:2)             :: CAI     !diffuse radiation absorbed by canopy (w/m2)
  REAL                             :: ANIR    !albedo nir
  REAL                             :: AVIS    !albedo vis
  REAL, DIMENSION(1:2)             :: TRD2    !transmitted solar radiation: direct (w/m2)
  REAL, DIMENSION(1:2)             :: TRI2    !transmitted solar radiation: diffuse (w/m2)

! ---------------------------------------------------------------------------------------------
   NBAND = 2

! zero summed solar fluxes

    SAG = 0.
    SAV = 0.
    FSA = 0.
    PHI(-NSNOW+1:NSOIL) = 0.

   !IF(COSZ <= 0.) GOTO 100

! loop over nband wavebands

  DO IB = 1, NBAND

! absorbed by canopy

    CAD(IB) = SOLAD(IB)*FABD(IB)    
    CAI(IB) = SOLAI(IB)*FABI(IB)
    SAV     = SAV + CAD(IB) + CAI(IB)
    FSA     = FSA + CAD(IB) + CAI(IB)
 
! transmitted solar fluxes incident on ground

    TRD      = SOLAD(IB)*FTDD(IB)
    TRI      = SOLAD(IB)*FTID(IB) + SOLAI(IB)*FTII(IB)
    TRD2(IB) = SOLAD(IB)*FTDD(IB)
    TRI2(IB) = SOLAD(IB)*FTID(IB) + SOLAI(IB)*FTII(IB)

! solar radiation absorbed by ground surface

    ABSO= TRD*(1.-ALBGRD(IB)) + TRI*(1.-ALBGRI(IB))
    SAG = SAG + ABSO
    FSA = FSA + ABSO

  END DO

  if (OPT_ALB == 3) then

     if ( ISNOW .lt. 0 ) then
        PHI(-NSNOW+1:0) = F_abs_D(-NSNOW+1:0,1)*TRD2(1) + F_abs_D(-NSNOW+1:0,2)*TRD2(2) &
                        + F_abs_I(-NSNOW+1:0,1)*TRI2(1) + F_abs_I(-NSNOW+1:0,2)*TRI2(2)

        PHI(1)          = F_btm_D(1)*TRD2(1) + F_btm_D(2)*TRD2(2) &
                        + F_btm_I(1)*TRI2(1) + F_btm_I(2)*TRI2(2)
     end if

     PHI = PHI*FSNO

     SAG = SAG -SUM(PHI)

  end if

! partition visible canopy absorption to sunlit and shaded fractions
! to get average absorbed par for sunlit and shaded leaves

     LAIFRA = ELAI / MAX(VAI,MPE)
    !LAIFRA = ELAI / MAX(VAI,0.01)
     IF (FSUN .GT. 0.) THEN
        PARSUN = (CAD(1)+FSUN*CAI(1)) * LAIFRA / MAX(LAISUN,MPE)
        PARSHA = (FSHA*CAI(1))*LAIFRA / MAX(LAISHA,MPE)
     ELSE
        PARSUN = 0.
        PARSHA = (CAD(1)+CAI(1))*LAIFRA /MAX(LAISHA,MPE)
     ENDIF

    if (isnan(PARSUN)) then
    write(*,*) 'in SURRAD: iloc,jloc=',iloc,jloc
    write(*,*) 'PARSUN, PARSHA', PARSUN,PARSHA
    END IF

! reflected solar radiation

     RVIS = ALBD(1)*SOLAD(1) + ALBI(1)*SOLAI(1)
     RNIR = ALBD(2)*SOLAD(2) + ALBI(2)*SOLAI(2)
     FSR  = RVIS + RNIR

! reflected solar radiation of veg. and ground (combined ground)
     FSRV = FREVD(1)*SOLAD(1)+FREVI(1)*SOLAI(1)+FREVD(2)*SOLAD(2)+FREVI(2)*SOLAI(2)
     FSRG = FREGD(1)*SOLAD(1)+FREGI(1)*SOLAI(1)+FREGD(2)*SOLAD(2)+FREGI(2)*SOLAI(2)

     IF(FSR > 0.0) THEN
        ANIR = RNIR / (SOLAD(2)+SOLAI(2))
        AVIS = RVIS / (SOLAD(1)+SOLAI(1))
     ELSE
        ANIR = -0.99
        AVIS = -0.99
     END IF

     IF(FSR > 0.0) THEN
       NDVI = (ANIR-AVIS) / (ANIR+AVIS)
     ElSE
       NDVI = -0.99
     END IF

  END SUBROUTINE SURRAD

!== begin snow_age =================================================================================

  SUBROUTINE SNOW_AGE (parameters,DT,TG,SNEQVO,SNEQV,TAUSS,FAGE)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ code history ------------------------------------------------------------
! from BATS
! ------------------------ input/output variables --------------------------------------------------
!input
  type (noahmp_parameters), intent(in) :: parameters
   REAL, INTENT(IN) :: DT        !main time step (s)
   REAL, INTENT(IN) :: TG        !ground temperature (k)
   REAL, INTENT(IN) :: SNEQVO    !snow mass at last time step(mm)
   REAL, INTENT(IN) :: SNEQV     !snow water per unit ground area (mm)

!output
   REAL, INTENT(OUT) :: FAGE     !snow age

!input/output
   REAL, INTENT(INOUT) :: TAUSS      !non-dimensional snow age
!local
   REAL            :: TAGE       !total aging effects
   REAL            :: AGE1       !effects of grain growth due to vapor diffusion
   REAL            :: AGE2       !effects of grain growth at freezing of melt water
   REAL            :: AGE3       !effects of soot
   REAL            :: DELA       !temporary variable
   REAL            :: SGE        !temporary variable
   REAL            :: DELS       !temporary variable
   REAL            :: DELA0      !temporary variable
   REAL            :: ARG        !temporary variable
! See Yang et al. (1997) J.of Climate for detail.
!---------------------------------------------------------------------------------------------------

   IF(SNEQV.LE.0.0) THEN
          TAUSS = 0.
   ELSE IF (SNEQV.GT.800.) THEN
          TAUSS = 0.
   ELSE
          DELA0 = 1.E-6*DT
          ARG   = 5.E3*(1./TFRZ-1./TG)
          AGE1  = EXP(ARG)
          AGE2  = EXP(AMIN1(0.,10.*ARG))
          AGE3  = 0.3
          TAGE  = AGE1+AGE2+AGE3
          DELA  = DELA0*TAGE
          DELS  = AMAX1(0.0,SNEQV-SNEQVO) / parameters%SWEMX
          SGE   = (TAUSS+DELA)*(1.0-DELS)
          TAUSS = AMAX1(0.,SGE)
   ENDIF

   FAGE= TAUSS/(TAUSS+1.)

  END SUBROUTINE SNOW_AGE

!== begin snowalb_bats =============================================================================

  SUBROUTINE SNOWALB_BATS (parameters,NBAND,FSNO,COSZ,FAGE,ALBSND,ALBSNI)
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN) :: NBAND  !number of waveband classes

  REAL,INTENT(IN) :: COSZ    !cosine solar zenith angle
  REAL,INTENT(IN) :: FSNO    !snow cover fraction (-)
  REAL,INTENT(IN) :: FAGE    !snow age correction

! output

  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND !snow albedo for direct(1=vis, 2=nir)
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI !snow albedo for diffuse
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
  INTEGER :: IB          !waveband class

  REAL :: FZEN                 !zenith angle correction
  REAL :: CF1                  !temperary variable
  REAL :: SL2                  !2.*SL
  REAL :: SL1                  !1/SL
  REAL :: SL                   !adjustable parameter
  REAL, PARAMETER :: C1 = 0.2  !default in BATS 
  REAL, PARAMETER :: C2 = 0.5  !default in BATS
!  REAL, PARAMETER :: C1 = 0.2 * 2. ! double the default to match Sleepers River's
!  REAL, PARAMETER :: C2 = 0.5 * 2. ! snow surface albedo (double aging effects)
! ---------------------------------------------------------------------------------------------
! zero albedos for all points

        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.

! when cosz > 0

        SL=2.0
        SL1=1./SL
        SL2=2.*SL
        CF1=((1.+SL1)/(1.+SL2*MAX(0.0001,COSZ))-SL1)
        FZEN=AMAX1(CF1,0.)

        ALBSNI(1)=0.95*(1.-C1*FAGE)         
        ALBSNI(2)=0.65*(1.-C2*FAGE)        

        ALBSND(1)=ALBSNI(1)+0.4*FZEN*(1.-ALBSNI(1))    !  vis direct
        ALBSND(2)=ALBSNI(2)+0.4*FZEN*(1.-ALBSNI(2))    !  nir direct

  END SUBROUTINE SNOWALB_BATS

!== begin snowalb_class ============================================================================

  SUBROUTINE SNOWALB_CLASS (parameters,NBAND,QSNOW,DT,ALB,ALBOLD,ALBSND,ALBSNI,ILOC,JLOC)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN) :: ILOC !grid index
  INTEGER,INTENT(IN) :: JLOC !grid index
  INTEGER,INTENT(IN) :: NBAND  !number of waveband classes

  REAL,INTENT(IN) :: QSNOW     !snowfall (mm/s)
  REAL,INTENT(IN) :: DT        !time step (sec)
  REAL,INTENT(IN) :: ALBOLD    !snow albedo at last time step

! in & out

  REAL,                INTENT(INOUT) :: ALB        ! 
! output

  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSND !snow albedo for direct(1=vis, 2=nir)
  REAL, DIMENSION(1:2),INTENT(OUT) :: ALBSNI !snow albedo for diffuse
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
  INTEGER :: IB          !waveband class

! ---------------------------------------------------------------------------------------------
! zero albedos for all points

        ALBSND(1: NBAND) = 0.
        ALBSNI(1: NBAND) = 0.

! when cosz > 0

         ALB = 0.55 + (ALBOLD-0.55) * EXP(-0.01*DT/3600.)

! 1 mm fresh snow(SWE) -- 10mm snow depth, assumed the fresh snow density 100kg/m3
! here assume 1cm snow depth will fully cover the old snow

         IF (QSNOW > 0.) then
           ALB = ALB + MIN(QSNOW*DT,parameters%SWEMX) * (0.84-ALB)/(parameters%SWEMX)
         ENDIF

         ALBSNI(1)= ALB         ! vis diffuse
         ALBSNI(2)= ALB         ! nir diffuse
         ALBSND(1)= ALB         ! vis direct
         ALBSND(2)= ALB         ! nir direct

  END SUBROUTINE SNOWALB_CLASS

!== begin ESM-SNICAR ===============================================================================
  SUBROUTINE ESM_SNICAR_direct ( APRX_TYP, DELTA, rds_snw, rho_snw, Z, alb_0, alb_t, mu_not, &
                                 ii,rad_cons,nbr_aer, &
                                 F_abs_vis_D, F_abs_nir_D, F_btm_vis_D, F_btm_nir_D )
!-------------------------------------------------------------------------------------------
! ! ARGUMENTS:
  IMPLICIT NONE

  type (constants), intent(in) :: rad_cons

  INTEGER,INTENT(in)                    :: APRX_TYP, DELTA
  INTEGER,INTENT(in)                    :: ii,nbr_aer
  REAL,   INTENT(in)                    :: mu_not
  REAL,   INTENT(in)                    :: rds_snw         ! snow grain radius [um]
  REAL,   INTENT(in)                    :: rho_snw         ! snow density [kg/m3]
  REAL,   INTENT(in)                    :: z               ! snow layer thickness [m]
  REAL, dimension(1:5),   INTENT(in)    :: alb_0           ! basic albedo before the start of a snowfall or sleet event [none]
  REAL, dimension(1:5),   INTENT(out)   :: alb_t           ! basic surface albedo at present ( or last step )
  REAL,   INTENT(out)                   :: F_abs_vis_D, F_abs_nir_D
  REAL,   INTENT(out)                   :: F_btm_vis_D, F_btm_nir_D        ! volumetric heating W/m2

  ! LOCAL:
  REAL           :: mss_cnc_sot1, mss_cnc_sot2, mss_cnc_sot3, mss_cnc_dst1, mss_cnc_dst2, mss_cnc_dst3, mss_cnc_dst4, mss_cnc_ash1
  REAL,dimension(1:5) :: tau_sum, omega_sum, g_sum, tau, omega, g, tau_star, omega_star, g_star
  REAL,dimension(1:5) :: tau_snw, omega0, g0, to
  REAL,dimension(1:7,1:5) :: tau_aer
  REAL,dimension(1:5) :: gamma1, gamma2, gamma3, gamma4, lamda, Gamma
  REAL                :: mu_one
  REAL,dimension(1:5) :: C0_plus, C0_minus, Ct_plus, Ct_minus, Ssfc
  REAL,dimension(1:5) :: tmp1, tmp2, k1, k2, F_0_plus, F_0_minus, F_to_plus, F_to_minus
  REAL,dimension(1:5) :: F_net_sfc, F_net_btm, F_abs, direct
  REAL,dimension(1:5) :: flx_slr=(/0.5028,0.2454,0.0900,0.0601,0.1017/)
  REAL,dimension(1:5) :: Fs, Fd, abs_t, btm_t
  REAL,dimension(1:7) :: mss_cnc_aer
  INTEGER :: j
  REAL, parameter :: pi=3.1416

! ! EXECUTABLE CODE
       mss_cnc_sot1 = 0.
       mss_cnc_sot2 = 0.
       mss_cnc_sot3 = 0.
       mss_cnc_dst1 = 0.
       mss_cnc_dst2 = 0.
       mss_cnc_dst3 = 0.
       mss_cnc_dst4 = 0.
       mss_cnc_ash1 = 0.

     Fs=flx_slr/(mu_not*pi)
     Fd=0

     !tau_snw=e_snw*z*rho_snw
     tau_snw=rad_cons%ext_mie_bd(ii,:)*z*rho_snw

     if (mss_cnc_sot1.GT.0 .OR. mss_cnc_sot2.GT.0 .OR. mss_cnc_sot3.GT.0 .OR. mss_cnc_dst1.GT.0 .OR. &
         mss_cnc_dst2.GT.0 .OR. mss_cnc_dst3.GT.0 .OR. mss_cnc_dst4.GT.0 .OR. mss_cnc_ash1.GT.0) then

       tau_sum(1:5)   = 0.0
       omega_sum(1:5) = 0.0
       g_sum(1:5)     = 0.0

       mss_cnc_aer(1) = mss_cnc_sot1
       mss_cnc_aer(2) = mss_cnc_sot2
       mss_cnc_aer(3) = mss_cnc_dst1
       mss_cnc_aer(4) = mss_cnc_dst2
       mss_cnc_aer(5) = mss_cnc_dst3
       mss_cnc_aer(6) = mss_cnc_dst4
       mss_cnc_aer(7) = mss_cnc_ash1

       mss_cnc_aer    = mss_cnc_aer*10.**(-9.)

       do j=1,nbr_aer
              tau_aer(j,1:5)=rad_cons%ext_mie_lap_bd(j,:)*z*rho_snw*mss_cnc_aer(j)
       end do

       do j=1,nbr_aer
              tau_sum   = tau_sum + tau_aer(j,:)
              omega_sum = omega_sum + (tau_aer(j,:)*rad_cons%w_mie_lap_bd(j,:))
              g_sum     = g_sum + (tau_aer(j,:)*rad_cons%w_mie_lap_bd(j,:)*rad_cons%g_mie_lap_bd(j,:))
       end do

           tau   = tau_sum + tau_snw
           omega = (1/tau)*(omega_sum+ (rad_cons%w_mie_bd(ii,:)*tau_snw))
           g     = (1/(tau*omega))* (g_sum+ (rad_cons%g1_mie_bd(ii,:)*rad_cons%w_mie_bd(ii,:)*tau_snw))
     else
         tau   = tau_snw
         omega = rad_cons%w_mie_bd(ii,:)
         g     = rad_cons%g1_mie_bd(ii,:)
     end if

!Delta transformation
     if (DELTA == 1) then
           g_star     = g/(1+g)
           omega_star = ((1-(g**2))*omega) / (1-(omega*(g**2)))
           tau_star   = (1-(omega*(g**2)))*tau
     else
           g_star     = g
           omega_star = omega
           tau_star   = tau
     end if

! Start
     omega0=omega_star
     g0=g_star
     to=tau_star

     if (APRX_TYP == 1) then
           ! Eddington:
           gamma1 = (7-(omega0*(4+(3*g0))))/4
           gamma2 = -(1-(omega0*(4-(3*g0))))/4
           gamma3 = (2-(3*g0*mu_not))/4
           gamma4 = 1-gamma3
           mu_one = 0.5
     end if

     if (APRX_TYP == 2) then
           ! Quadrature:
           gamma1 = sqrt(3.0)*(2-(omega0*(1+g0)))/2
           gamma2 = omega0*sqrt(3.0)*(1-g0)/2
           gamma3 = (1-(sqrt(3.0)*g0*mu_not))/2
           gamma4 = 1-gamma3
           mu_one = 1/sqrt(3.0)
     end if

     if (APRX_TYP == 3) then
           ! Hemispheric mean:
           gamma1 = 2 - (omega0*(1+g0))
           gamma2 = omega0*(1-g0)
           gamma3 = (1-(sqrt(3.0)*g0*mu_not))/2
           gamma4 = 1-gamma3
           mu_one = 0.5
     end if

! Solution
        lamda=sqrt(gamma1**2-gamma2**2)
        Gamma=(gamma1-lamda)/gamma2

        C0_plus =omega0*pi*Fs*((gamma1-1/mu_not)*gamma3+gamma4*gamma2)/(lamda**2-1/mu_not**2)
        C0_minus=omega0*pi*Fs*((gamma1+1/mu_not)*gamma4+gamma2*gamma3)/(lamda**2-1/mu_not**2)
        Ct_plus =omega0*pi*Fs*exp(-to/mu_not)*((gamma1-1/mu_not)*gamma3+gamma4*gamma2)/(lamda**2-1/mu_not**2)
        Ct_minus=omega0*pi*Fs*exp(-to/mu_not)*((gamma1+1/mu_not)*gamma4+gamma2*gamma3)/(lamda**2-1/mu_not**2)
        Ssfc    =alb_0*mu_not*exp(-to/mu_not)*pi*Fs

        tmp1 =Gamma*C0_minus*exp(-lamda*to)-Ct_plus-alb_0*C0_minus*exp(-lamda*to)+alb_0*Ct_minus+Ssfc
        tmp2 =exp(lamda*to)-Gamma**2*exp(-lamda*to)-alb_0*Gamma*exp(lamda*to)+alb_0*Gamma*exp(-lamda*to)
        k1   =tmp1/tmp2
        k2   =-Gamma*k1-C0_minus
        F_0_plus=k1+Gamma*k2+C0_plus
        alb_t= F_0_plus/((mu_not*pi*Fs)+Fd)

        if (isnan(alb_t(5))) alb_t(5)=alb_0(5)
        if (isnan(alb_t(4))) alb_t(4)=alb_0(4)

       ! write(*,*) "alb_t=", alb_t

! flux absorption
    direct    =mu_not*pi*Fs*exp(-to/mu_not)
        F_to_plus =k1*exp(lamda*to)+Gamma*k2*exp(-lamda*to)+Ct_plus
        F_to_minus=Gamma*k1*exp(lamda*to)+k2*exp(-lamda*to)+Ct_minus+direct
        F_0_minus =0

  END SUBROUTINE ESM_SNICAR_direct

!-------------------------------------------------------------------------------------------
  SUBROUTINE ESM_SNICAR_difus ( APRX_TYP, DELTA, rds_snw, rho_snw, Z, alb_0, alb_t, mu_not, &
                                ii,rad_cons,nbr_aer, &
                                F_abs_vis_I, F_abs_nir_I, F_btm_vis_I, F_btm_nir_I)
!-------------------------------------------------------------------------------------------
! ! ARGUMENTS:
  IMPLICIT NONE

  type (constants), intent(in) :: rad_cons

  INTEGER,INTENT(in)                    :: APRX_TYP, DELTA
  INTEGER,INTENT(in)                    :: ii,nbr_aer
  REAL,   INTENT(in)                    :: mu_not
  REAL,   INTENT(in)                    :: rds_snw         ! snow grain radius [um]
  REAL,   INTENT(in)                    :: rho_snw         ! snow density [kg/m3]
  REAL,   INTENT(in)                    :: z               ! snow layer thickness [m]
  REAL, dimension(1:5),   INTENT(in)    :: alb_0           ! basic albedo before the start of a snowfall or sleet event [none]
  REAL, dimension(1:5),   INTENT(out)   :: alb_t           ! basic surface albedo at present ( or last step )
  REAL,   INTENT(out)                   :: F_abs_vis_I, F_abs_nir_I  , F_btm_vis_I, F_btm_nir_I       ! volumetric heating W/m3

  ! LOCAL:
  REAL           :: mss_cnc_sot1, mss_cnc_sot2, mss_cnc_sot3, mss_cnc_dst1, mss_cnc_dst2, mss_cnc_dst3, mss_cnc_dst4, mss_cnc_ash1
  REAL,dimension(1:5)     :: tau_sum, omega_sum, g_sum, tau, omega, g, tau_star, omega_star, g_star
  REAL,dimension(1:5)     :: tau_snw, omega0, g0, to
  REAL,dimension(1:7,1:5) :: tau_aer
  REAL,dimension(1:5)     :: gamma1, gamma2, gamma3, gamma4, lamda, Gamma
  REAL,dimension(1:5)     :: tmp1, tmp2, k1, k2, F_0_plus, F_0_minus, F_to_plus, F_to_minus
  REAL,dimension(1:5)     :: F_net_sfc, F_net_btm, F_abs
  REAL,dimension(1:5)     :: flx_slr=(/0.5767,0.2480,0.0853,0.0462,0.0438/)
  REAL,dimension(1:5)     :: Fs, Fd, abs_t, btm_t
  REAL,dimension(1:7)     :: mss_cnc_aer
  INTEGER                 :: j
  REAL, parameter         :: pi=3.1416
  REAL                    :: mu_one

! ! EXECUTABLE CODE

       mss_cnc_sot1 = 0.
       mss_cnc_sot2 = 0.
       mss_cnc_sot3 = 0.
       mss_cnc_dst1 = 0.
       mss_cnc_dst2 = 0.
       mss_cnc_dst3 = 0.
       mss_cnc_dst4 = 0.
       mss_cnc_ash1 = 0.

     Fs=0
     Fd=flx_slr

    !tau_snw=e_snw*z*rho_snw
     tau_snw=rad_cons%ext_mie_bd(ii,:)*z*rho_snw

     if (mss_cnc_sot1.GT.0 .OR. mss_cnc_sot2.GT.0 .OR. mss_cnc_sot3.GT.0 .OR. mss_cnc_dst1.GT.0 .OR. &
         mss_cnc_dst2.GT.0 .OR. mss_cnc_dst3.GT.0 .OR. mss_cnc_dst4.GT.0 .OR. mss_cnc_ash1.GT.0) then
           tau_sum(1:5)   = 0.0
           omega_sum(1:5) = 0.0
           g_sum(1:5)     = 0.0

           mss_cnc_aer(1) = mss_cnc_sot1
           mss_cnc_aer(2) = mss_cnc_sot2
           mss_cnc_aer(3) = mss_cnc_dst1
           mss_cnc_aer(4) = mss_cnc_dst2
           mss_cnc_aer(5) = mss_cnc_dst3
           mss_cnc_aer(6) = mss_cnc_dst4
           mss_cnc_aer(7) = mss_cnc_ash1

           mss_cnc_aer    = mss_cnc_aer*10.**(-9.0)

           do j=1,nbr_aer
              tau_aer(j,1:5)=rad_cons%ext_mie_lap_bd(j,:)*z*rho_snw*mss_cnc_aer(j)
           end do
           do j=1,nbr_aer
              tau_sum   = tau_sum + tau_aer(j,:)
              omega_sum = omega_sum + (tau_aer(j,:)*rad_cons%w_mie_lap_bd(j,:))
              g_sum     = g_sum + (tau_aer(j,:)*rad_cons%w_mie_lap_bd(j,:)*rad_cons%g_mie_lap_bd(j,:))
           end do

           tau   = tau_sum + tau_snw
           omega = (1/tau)*(omega_sum+ (rad_cons%w_mie_bd(ii,:)*tau_snw))
           g     = (1/(tau*omega))* (g_sum+ (rad_cons%g1_mie_bd(ii,:)*rad_cons%w_mie_bd(ii,:)*tau_snw))
     else
           tau   = tau_snw
           omega = rad_cons%w_mie_bd(ii,:)
           g     = rad_cons%g1_mie_bd(ii,:)
     end if

!Delta transformation
     if (DELTA == 1) then
           g_star     = g/(1+g)
           omega_star = ((1-(g**2))*omega) / (1-(omega*(g**2)))
           tau_star   = (1-(omega*(g**2)))*tau
     else
           g_star     = g
           omega_star = omega
           tau_star   = tau
     end if

! Start
     omega0=omega_star
     g0=g_star
     to=tau_star

     if (APRX_TYP == 1) then
           ! Eddington:
           gamma1 = (7-(omega0*(4+(3*g0))))/4
           gamma2 = -(1-(omega0*(4-(3*g0))))/4
           gamma3 = (2-(3*g0*mu_not))/4
           gamma4 = 1-gamma3
           mu_one = 0.5
     end if

     if (APRX_TYP == 2) then
           ! Quadrature:
           gamma1 = sqrt(3.0)*(2-(omega0*(1+g0)))/2
           gamma2 = omega0*sqrt(3.0)*(1-g0)/2
           gamma3 = (1-(sqrt(3.0)*g0*mu_not))/2
           gamma4 = 1-gamma3
           mu_one = 1/sqrt(3.0)
     end if

     if (APRX_TYP == 3) then
           ! Hemispheric mean:
           gamma1 = 2 - (omega0*(1+g0))
           gamma2 = omega0*(1-g0)
           gamma3 = (1-(sqrt(3.0)*g0*mu_not))/2
           gamma4 = 1-gamma3
           mu_one = 0.5
     end if

! Solution
        lamda=sqrt(gamma1**2-gamma2**2)
        Gamma=(gamma1-lamda)/gamma2

        tmp1=alb_0*exp(-lamda*to)-Gamma*exp(-lamda*to)
        tmp2=exp(lamda*to)-Gamma**2*exp(-lamda*to)-alb_0*Gamma*exp(lamda*to)+alb_0*Gamma*exp(-lamda*to)
        k1=tmp1/tmp2
        k2=1-Gamma*k1
        alb_t=k1+Gamma*k2

          if (isnan(alb_t(5))) alb_t(5)=alb_0(5)
          if (isnan(alb_t(4))) alb_t(4)=alb_0(4)

! flux absorption
        F_to_plus = k1*exp(lamda*to)+Gamma*k2*exp(-lamda*to)
        F_to_minus= Gamma*k1*exp(lamda*to)+k2*exp(-lamda*to)
        F_0_minus = 1
        F_0_plus  = alb_t

        F_to_plus = F_to_plus*Fd
        F_to_minus= F_to_minus*Fd
        F_0_minus = F_0_minus*Fd
        F_0_plus  = F_0_plus*Fd

        F_net_sfc = F_0_minus - F_0_plus
        F_net_btm = F_to_minus- F_to_plus

        F_abs     = F_net_sfc-F_net_btm

        if (isnan(F_abs(5)))     F_abs(5)=0
        if (isnan(F_net_btm(5))) F_net_btm(5)=0
        if (isnan(F_abs(4)))     F_abs(4)=0
        if (isnan(F_net_btm(4))) F_net_btm(4)=0

        abs_t       = F_abs/(Fs*mu_not*pi+Fd)
        F_abs_vis_I = abs_t(1)
        F_abs_nir_I = sum(Fd(2:5)*abs_t(2:5))/sum(Fd(2:5))

        btm_t       = F_net_btm/(Fs*mu_not*pi+Fd)
        F_btm_vis_I = btm_t(1)
        F_btm_nir_I = sum(Fd(2:5)*btm_t(2:5))/sum(Fd(2:5))

  END SUBROUTINE ESM_SNICAR_difus

!== begin soil_albedo ================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE SOIL_ALBEDO (parameters, NSOIL   ,NBAND   ,ICE     ,IST     , & !in
                        SMC     ,COSZ    ,                                 & !in
                        TG      ,ILOC    ,JLOC    ,                        & !in
                        ALBSOD  ,ALBSOI  )                                   !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN)  :: ILOC   !grid index
  INTEGER,                  INTENT(IN)  :: JLOC   !grid index
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: NBAND  !number of solar radiation waveband classes
  INTEGER,                  INTENT(IN)  :: ICE    !value of ist for land ice
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  REAL,                     INTENT(IN)  :: TG     !ground temperature (k)
  REAL,                     INTENT(IN)  :: COSZ   !cosine solar zenith angle (0-1)
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    !volumetric soil water content (m3/m3)



!output
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBSOD !soil albedo (direct)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBSOI !soil albedo (diffuse)
!local

  INTEGER                               :: IB     !waveband number (1=vis, 2=nir)
  REAL                                  :: INC    !soil water correction factor for soil albedo
! --------------------------------------------------------------------------------------------------

  DO IB = 1, NBAND
        INC = MAX(0.11-0.40*SMC(1), 0.)
        IF (IST .EQ. 1)  THEN                     !soil
           ALBSOD = MIN(parameters%ALBSAT(IB)+INC,parameters%ALBDRY(IB))
           ALBSOI = ALBSOD
        ELSE IF (TG .GT. TFRZ) THEN               !unfrozen lake, wetland
           ALBSOD = 0.06/(MAX(0.01,COSZ)**1.7 + 0.15)
           ALBSOI = 0.06
        ELSE                                      !frozen lake, wetland
           ALBSOD = parameters%ALBLAK(IB)
           ALBSOI = ALBSOD
        END IF

! increase desert and semi-desert albedos

!        IF (IST .EQ. 1 .AND. ISC .EQ. 9) THEN
!           ALBSOD = ALBSOD + 0.10
!           ALBSOI = ALBSOI + 0.10
!        END IF

  END DO

  END SUBROUTINE SOIL_ALBEDO

! ==================================================================================================
! --------------------------------------------------------------------------------------------------
  SUBROUTINE GROUNDALB (parameters,NSOIL   ,NBAND   ,ICE     ,IST     , & !in
                        FSNO    ,SMC     ,ALBSND  ,ALBSNI  ,COSZ    , & !in
                        TG      ,ILOC    ,JLOC    ,                   & !in
                        ALBGRD  ,ALBGRI  )                              !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN)  :: ILOC   !grid index
  INTEGER,                  INTENT(IN)  :: JLOC   !grid index
  INTEGER,                  INTENT(IN)  :: NSOIL  !number of soil layers
  INTEGER,                  INTENT(IN)  :: NBAND  !number of solar radiation waveband classes
  INTEGER,                  INTENT(IN)  :: ICE    !value of ist for land ice
  INTEGER,                  INTENT(IN)  :: IST    !surface type
  REAL,                     INTENT(IN)  :: FSNO   !fraction of surface covered with snow (-)
  REAL,                     INTENT(IN)  :: TG     !ground temperature (k)
  REAL,                     INTENT(IN)  :: COSZ   !cosine solar zenith angle (0-1)
  REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC    !volumetric soil water content (m3/m3)
  REAL, DIMENSION(1:    2), INTENT(IN)  :: ALBSND !direct beam snow albedo (vis, nir)
  REAL, DIMENSION(1:    2), INTENT(IN)  :: ALBSNI !diffuse snow albedo (vis, nir)

!output

  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRD !ground albedo (direct beam: vis, nir)
  REAL, DIMENSION(1:    2), INTENT(OUT) :: ALBGRI !ground albedo (diffuse: vis, nir)

!local 

  INTEGER                               :: IB     !waveband number (1=vis, 2=nir)
  REAL                                  :: INC    !soil water correction factor for soil albedo
  REAL                                  :: ALBSOD !soil albedo (direct)
  REAL                                  :: ALBSOI !soil albedo (diffuse)
! --------------------------------------------------------------------------------------------------

  DO IB = 1, NBAND
        INC = MAX(0.11-0.40*SMC(1), 0.)
        IF (IST .EQ. 1)  THEN                     !soil
           ALBSOD = MIN(parameters%ALBSAT(IB)+INC,parameters%ALBDRY(IB))
           ALBSOI = ALBSOD
        ELSE IF (TG .GT. TFRZ) THEN               !unfrozen lake, wetland
           ALBSOD = 0.06/(MAX(0.0001,COSZ)**1.7 + 0.15)
           ALBSOI = 0.06
        ELSE                                      !frozen lake, wetland
           ALBSOD = parameters%ALBLAK(IB)
           ALBSOI = ALBSOD
        END IF

! increase desert and semi-desert albedos

      ! IF (IST .EQ. 1 .AND. ISC .EQ. 9) THEN
      !    ALBSOD = ALBSOD + 0.10
      !    ALBSOI = ALBSOI + 0.10
      ! end if

        ALBGRD(IB) = ALBSOD*(1.-FSNO) + ALBSND(IB)*FSNO
        ALBGRI(IB) = ALBSOI*(1.-FSNO) + ALBSNI(IB)*FSNO

     IF(ALBGRI(IB) == 0.) THEN
      write(*,*) "IB,ALBGRI(IB)=",IB,ALBGRI(IB)
      write(*,*) "IB,ALBSOI,ALBSNI(IB),FSNO =",IB,ALBSOI,ALBSNI(IB),FSNO
     END IF
  END DO

  END SUBROUTINE GROUNDALB

!== begin twostream ================================================================================

  SUBROUTINE TWOSTREAM (parameters,IB     ,IC      ,VEGTYP  ,COSZ    ,VAI    , & !in
                        FWET   ,T       ,ALBGRD  ,ALBGRI  ,RHO    , & !in
                        TAU    ,FVEG    ,IST     ,ILOC    ,JLOC   , & !in
                        FAB    ,FRE     ,FTD     ,FTI     ,GDIR   , & !)   !out
                        FREV   ,FREG    ,BGAP    ,WGAP)

! --------------------------------------------------------------------------------------------------
! use two-stream approximation of Dickinson (1983) Adv Geophysics
! 25:305-353 and Sellers (1985) Int J Remote Sensing 6:1335-1372
! to calculate fluxes absorbed by vegetation, reflected by vegetation,
! and transmitted through vegetation for unit incoming direct or diffuse
! flux given an underlying surface with known albedo.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
   INTEGER,              INTENT(IN)  :: ILOC    !grid index
   INTEGER,              INTENT(IN)  :: JLOC    !grid index
   INTEGER,              INTENT(IN)  :: IST     !surface type
   INTEGER,              INTENT(IN)  :: IB      !waveband number
   INTEGER,              INTENT(IN)  :: IC      !0=unit incoming direct; 1=unit incoming diffuse
   INTEGER,              INTENT(IN)  :: VEGTYP  !vegetation type

   REAL,                 INTENT(IN)  :: COSZ    !cosine of direct zenith angle (0-1)
   REAL,                 INTENT(IN)  :: VAI     !one-sided leaf+stem area index (m2/m2)
   REAL,                 INTENT(IN)  :: FWET    !fraction of lai, sai that is wetted (-)
   REAL,                 INTENT(IN)  :: T       !surface temperature (k)

   REAL, DIMENSION(1:2), INTENT(IN)  :: ALBGRD  !direct  albedo of underlying surface (-)
   REAL, DIMENSION(1:2), INTENT(IN)  :: ALBGRI  !diffuse albedo of underlying surface (-)
   REAL, DIMENSION(1:2), INTENT(IN)  :: RHO     !leaf+stem reflectance
   REAL, DIMENSION(1:2), INTENT(IN)  :: TAU     !leaf+stem transmittance
   REAL,                 INTENT(IN)  :: FVEG    !green vegetation fraction [0.0-1.0]

! output

   REAL, DIMENSION(1:2), INTENT(OUT) :: FAB     !flux abs by veg layer (per unit incoming flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FRE     !flux refl above veg layer (per unit incoming flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FTD     !down dir flux below veg layer (per unit in flux)
   REAL, DIMENSION(1:2), INTENT(OUT) :: FTI     !down dif flux below veg layer (per unit in flux)
   REAL,                 INTENT(OUT) :: GDIR    !projected leaf+stem area in solar direction
   REAL, DIMENSION(1:2), INTENT(OUT) :: FREV    !flux reflected by veg layer   (per unit incoming flux) 
   REAL, DIMENSION(1:2), INTENT(OUT) :: FREG    !flux reflected by ground (per unit incoming flux)

! local
   REAL                              :: OMEGA   !fraction of intercepted radiation that is scattered
   REAL                              :: OMEGAL  !omega for leaves
   REAL                              :: BETAI   !upscatter parameter for diffuse radiation
   REAL                              :: BETAIL  !betai for leaves
   REAL                              :: BETAD   !upscatter parameter for direct beam radiation
   REAL                              :: BETADL  !betad for leaves
   REAL                              :: EXT     !optical depth of direct beam per unit leaf area
   REAL                              :: AVMU    !average diffuse optical depth

   REAL                              :: COSZI   !0.001 <= cosz <= 1.000
   REAL                              :: ASU     !single scattering albedo
   REAL                              :: CHIL    ! -0.4 <= xl <= 0.6

   REAL                              :: TMP0,TMP1,TMP2,TMP3,TMP4,TMP5,TMP6,TMP7,TMP8,TMP9
   REAL                              :: P1,P2,P3,P4,S1,S2,U1,U2,U3
   REAL                              :: B,C,D,D1,D2,F,H,H1,H2,H3,H4,H5,H6,H7,H8,H9,H10
   REAL                              :: PHI1,PHI2,SIGMA
   REAL                              :: FTDS,FTIS,FRES
   REAL                              :: DENFVEG
   REAL                              :: VAI_SPREAD
!jref:start
   REAL                              :: FREVEG,FREBAR,FTDVEG,FTIVEG,FTDBAR,FTIBAR
   REAL                              :: THETAZ
!jref:end   

!  variables for the modified two-stream scheme
!  Niu and Yang (2004), JGR

   REAL, PARAMETER :: PAI = 3.14159265 
   REAL :: HD       !crown depth (m)
   REAL :: BB       !vertical crown radius (m)
   REAL :: THETAP   !angle conversion from SZA 
   REAL :: FA       !foliage volume density (m-1)
   REAL :: NEWVAI   !effective LSAI (-)

   REAL,INTENT(INOUT) :: BGAP     !between canopy gap fraction for beam (-)
   REAL,INTENT(INOUT) :: WGAP     !within canopy gap fraction for beam (-)

   REAL :: KOPEN    !gap fraction for diffue light (-)
   REAL :: GAP      !total gap fraction for beam ( <=1-shafac )

! -----------------------------------------------------------------
! compute within and between gaps
     VAI_SPREAD = VAI
     if(VAI == 0.0) THEN
         GAP     = 1.0
         KOPEN   = 1.0
     ELSE
         IF(OPT_RAD == 1) THEN
	   DENFVEG = -LOG(MAX(1.0-FVEG,0.01))/(PAI*parameters%RC**2)
           HD      = parameters%HVT - parameters%HVB
           BB      = 0.5 * HD           
           THETAP  = ATAN(BB/parameters%RC * TAN(ACOS(MAX(0.001,COSZ))) )
           ! BGAP    = EXP(-parameters%DEN * PAI * parameters%RC**2/COS(THETAP) )
           BGAP    = EXP(-DENFVEG * PAI * parameters%RC**2/COS(THETAP) )
           FA      = VAI/(1.33 * PAI * parameters%RC**3.0 *(BB/parameters%RC)*DENFVEG)
           NEWVAI  = HD*FA
           WGAP    = (1.0-BGAP) * EXP(-0.5*NEWVAI/MAX(0.0001,COSZ))
           GAP     = MIN(1.0-FVEG, BGAP+WGAP)

           KOPEN   = 0.05
         END IF

         IF(OPT_RAD == 2) THEN
           GAP     = 0.0
           KOPEN   = 0.0
         END IF

         IF(OPT_RAD == 3) THEN
           GAP     = 1.0-FVEG
           KOPEN   = 1.0-FVEG
         END IF
     end if

! calculate two-stream parameters OMEGA, BETAD, BETAI, AVMU, GDIR, EXT.
! OMEGA, BETAD, BETAI are adjusted for snow. values for OMEGA*BETAD
! and OMEGA*BETAI are calculated and then divided by the new OMEGA
! because the product OMEGA*BETAI, OMEGA*BETAD is used in solution.
! also, the transmittances and reflectances (TAU, RHO) are linear
! weights of leaf and stem values.

     COSZI  = MAX(0.0001, COSZ)
     CHIL   = MIN( MAX(parameters%XL, -0.4), 0.6)
     IF (ABS(CHIL) .LE. 0.01) CHIL = 0.01
     PHI1   = 0.5 - 0.633*CHIL - 0.330*CHIL*CHIL
     PHI2   = 0.877 * (1.-2.*PHI1)
     GDIR   = PHI1 + PHI2*COSZI
     EXT    = GDIR/COSZI
     AVMU   = ( 1. - PHI1/PHI2 * LOG((PHI1+PHI2)/PHI1) ) / PHI2
     OMEGAL = RHO(IB) + TAU(IB)
     TMP0   = GDIR + PHI2*COSZI
     TMP1   = PHI1*COSZI
     ASU    = 0.5*OMEGAL*GDIR/TMP0 * ( 1.-TMP1/TMP0*LOG((TMP1+TMP0)/TMP1) )
     BETADL = (1.+AVMU*EXT)/(OMEGAL*AVMU*EXT)*ASU
     BETAIL = 0.5 * ( RHO(IB)+TAU(IB) + (RHO(IB)-TAU(IB))   &
            * ((1.+CHIL)/2.)**2 ) / OMEGAL

! adjust omega, betad, and betai for intercepted snow

     IF (T .GT. TFRZ) THEN                                !no snow
        TMP0 = OMEGAL
        TMP1 = BETADL
        TMP2 = BETAIL
     ELSE
        TMP0 =   (1.-FWET)*OMEGAL        + FWET*parameters%OMEGAS(IB)
        TMP1 = ( (1.-FWET)*OMEGAL*BETADL + FWET*parameters%OMEGAS(IB)*parameters%BETADS ) / TMP0
        TMP2 = ( (1.-FWET)*OMEGAL*BETAIL + FWET*parameters%OMEGAS(IB)*parameters%BETAIS ) / TMP0
     END IF

     OMEGA = TMP0
     BETAD = TMP1
     BETAI = TMP2

! absorbed, reflected, transmitted fluxes per unit incoming radiation

     B = 1. - OMEGA + OMEGA*BETAI
     C = OMEGA*BETAI
     TMP0 = AVMU*EXT
     D = TMP0 * OMEGA*BETAD
     F = TMP0 * OMEGA*(1.-BETAD)
     TMP1 = B*B - C*C
     H = SQRT(TMP1) / AVMU
     SIGMA = TMP0*TMP0 - TMP1
     if ( ABS (SIGMA) < 1.e-6 ) SIGMA = SIGN(1.e-6,SIGMA)
     P1 = B + AVMU*H
     P2 = B - AVMU*H
     P3 = B + TMP0
     P4 = B - TMP0
     S1 = EXP(-H*VAI)
     S2 = EXP(-EXT*VAI)
     IF (IC .EQ. 0) THEN
        U1 = B - C/ALBGRD(IB)
        U2 = B - C*ALBGRD(IB)
        U3 = F + C*ALBGRD(IB)
     ELSE
        U1 = B - C/ALBGRI(IB)
        U2 = B - C*ALBGRI(IB)
        U3 = F + C*ALBGRI(IB)
     END IF
     TMP2 = U1 - AVMU*H
     TMP3 = U1 + AVMU*H
     D1 = P1*TMP2/S1 - P2*TMP3*S1

     IF(isnan(D1)) THEN
       write(*,*) "ILOC,JLOC=",ILOC,JLOC
       write(*,*) "IC,IB,ALBGRD(IB)=",IC,IB,ALBGRD(IB)
       write(*,*) "IC,IB,ALBGRI(IB)=",IC,IB,ALBGRI(IB)
     END IF

     TMP4 = U2 + AVMU*H
     TMP5 = U2 - AVMU*H
     D2 = TMP4/S1 - TMP5*S1
     H1 = -D*P4 - C*F
     TMP6 = D - H1*P3/SIGMA
     TMP7 = ( D - C - H1/SIGMA*(U1+TMP0) ) * S2
     H2 = ( TMP6*TMP2/S1 - P2*TMP7 ) / D1
     H3 = - ( TMP6*TMP3*S1 - P1*TMP7 ) / D1
     H4 = -F*P3 - C*D
     TMP8 = H4/SIGMA
     TMP9 = ( U3 - TMP8*(U2-TMP0) ) * S2
     H5 = - ( TMP8*TMP4/S1 + TMP9 ) / D2
     H6 = ( TMP8*TMP5*S1 + TMP9 ) / D2
     H7 = (C*TMP2) / (D1*S1)
     H8 = (-C*TMP3*S1) / D1
     H9 = TMP4 / (D2*S1)
     H10 = (-TMP5*S1) / D2

     IF(isnan(H7)) THEN
      write(*,*) " IC,H7,H8    =", IC,H7,H8
      write(*,*) " D1,S1,TMP2,TMP3    =",D1,S1,TMP2,TMP3
      stop
     END IF

! downward direct and diffuse fluxes below vegetation
! Niu and Yang (2004), JGR.

     IF (IC .EQ. 0) THEN
        FTDS = S2                           *(1.0-GAP) + GAP
        FTIS = (H4*S2/SIGMA + H5*S1 + H6/S1)*(1.0-GAP)
     ELSE
        FTDS = 0.
        FTIS = (H9*S1 + H10/S1)*(1.0-KOPEN) + KOPEN
     END IF
     FTD(IB) = FTDS
     FTI(IB) = FTIS

! flux reflected by the surface (veg. and ground)

     IF (IC .EQ. 0) THEN
        FRES   = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) + ALBGRD(IB)*GAP        
        FREVEG = (H1/SIGMA + H2 + H3)*(1.0-GAP  ) 
        FREBAR = ALBGRD(IB)*GAP                   !jref - separate veg. and ground reflection
     ELSE
        FRES   = (H7 + H8) *(1.0-KOPEN) + ALBGRI(IB)*KOPEN        
        FREVEG = (H7 + H8) *(1.0-KOPEN) + ALBGRI(IB)*KOPEN
        FREBAR = 0                                !jref - separate veg. and ground reflection
     END IF
     FRE(IB) = FRES

     FREV(IB) = FREVEG 
     FREG(IB) = FREBAR 

! flux absorbed by vegetation

     FAB(IB) = 1. - FRE(IB) - (1.-ALBGRD(IB))*FTD(IB) &
                            - (1.-ALBGRI(IB))*FTI(IB)

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    !IF(isnan(FAB(IB))) THEN
    ! write(*,*) " IB,IC,FAB(IB)    =", IB,IC,FAB(IB)
    ! write(*,*) "      FRES(IB)    =", FRES
    ! write(*,*) "        H7,H8,KOPEN    =", H7,H8,KOPEN
    ! write(*,*) "        H1,SIGMA,H2,H3,GAP,ALBGRD(IB)"
    ! write(*,*)          H1,SIGMA,H2,H3,GAP,ALBGRD(IB) 
    ! write(*,*) " IB,ALBGRD(IB) =", IB,IC,ALBGRD(IB)
    ! write(*,*) " IB,ALBGRI(IB) =", IB,IC,ALBGRI(IB)
    ! write(*,*) " IB,FTD(IB)    =", IB,IC,FTD(IB)
    ! write(*,*) " IB,FTI(IB)    =", IB,IC,FTI(IB)
    !END IF


  END SUBROUTINE TWOSTREAM

!== begin vege_flux ================================================================================

  SUBROUTINE VEGE_FLUX(parameters,NSNOW   ,NSOIL   ,ISNOW   ,VEGTYP  ,VEG     , & !in
                       DT      ,SAV     ,SAG     ,LWDN    ,UR      , & !in
                       UU      ,VV      ,SFCTMP  ,THAIR   ,QAIR    , & !in
                       EAIR    ,RHOAIR  ,SNOWH   ,VAI     ,GAMMAV   ,GAMMAG,  & !in
                       FWET    ,LAISUN  ,LAISHA  ,CWP     ,DZSNSO  , & !in
                       ZLVL    ,ZPD     ,Z0M     ,FVEG    ,MQ      , & !in
                       Z0MG    ,EMV     ,EMG     ,CANLIQ  ,FSNO    , & !in
                       CANICE  ,STC     ,DF      ,RSSUN   ,RSSHA   , & !in
                       RSURF   ,LATHEAV ,LATHEAG  ,PARSUN  ,PARSHA  ,IGS     , & !in
                       FOLN    ,CO2AIR  ,O2AIR   ,BTRAN   ,SFCPRS  , & !in
                       RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHV    ,PAHG     , & !in
                       EAH     ,TAH     ,TV      ,TG      ,CM      , & !inout
                       CH      ,DX      ,DZ8W    ,                   & !
                       TAUXV   ,TAUYV   ,IRG     ,IRC     ,SHG     , & !out
                       SHC     ,EVG     ,EVC     ,TR      ,GH      , & !out
                       T2MV    ,PSNSUN  ,PSNSHA  ,CANHS   ,          & !out
                       QC      ,QSFC    ,PSFC    ,                   & !in
                       Q2V     ,CAH2    ,CHLEAF  ,CHUC    )            !inout 

! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve for vegetation (tv) and
! ground (tg) temperatures that balance the surface energy budgets

! vegetated:
! -SAV + IRC[TV] + SHC[TV] + EVC[TV] + TR[TV] = 0
! -SAG + IRG[TG] + SHG[TG] + EVG[TG] + GH[TG] = 0
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN) :: ILOC   !grid index
  INTEGER,                         INTENT(IN) :: JLOC   !grid index
  LOGICAL,                         INTENT(IN) :: VEG    !true if vegetated surface
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers        
  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
  INTEGER,                         INTENT(IN) :: VEGTYP !vegetation physiology type
  REAL,                            INTENT(IN) :: FVEG   !greeness vegetation fraction (-)
  REAL,                            INTENT(IN) :: SAV    !solar rad absorbed by veg (w/m2)
  REAL,                            INTENT(IN) :: SAG    !solar rad absorbed by ground (w/m2)
  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
  REAL,                            INTENT(IN) :: UR     !wind speed at height zlvl (m/s)
  REAL,                            INTENT(IN) :: UU     !wind speed in eastward dir (m/s)
  REAL,                            INTENT(IN) :: VV     !wind speed in northward dir (m/s)
  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
  REAL,                            INTENT(IN) :: THAIR  !potential temp at reference height (k)
  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at zlvl (pa)
  REAL,                            INTENT(IN) :: QAIR   !specific humidity at zlvl (kg/kg)
  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m**3)
  REAL,                            INTENT(IN) :: DT     !time step (s)
  REAL,                            INTENT(IN) :: FSNO     !snow fraction

  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
  REAL,                            INTENT(IN) :: FWET   !wetted fraction of canopy
  REAL,                            INTENT(IN) :: CWP    !canopy wind parameter

  REAL,                            INTENT(IN) :: VAI    !total leaf area index + stem area index
  REAL,                            INTENT(IN) :: LAISUN !sunlit leaf area index, one-sided (m2/m2)
  REAL,                            INTENT(IN) :: LAISHA !shaded leaf area index, one-sided (m2/m2)
  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum (m)
  REAL,                            INTENT(IN) :: Z0MG   !roughness length, momentum, ground (m)
  REAL,                            INTENT(IN) :: EMV    !vegetation emissivity
  REAL,                            INTENT(IN) :: EMG    !ground emissivity

  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thinkness of snow/soil layers (m)
  REAL,                            INTENT(IN) :: CANLIQ !intercepted liquid water (mm)
  REAL,                            INTENT(IN) :: CANICE !intercepted ice mass (mm)
  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
!  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/K)
!  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMAV  !psychrometric constant (pa/K)
  REAL,                            INTENT(IN) :: LATHEAV !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMAG  !psychrometric constant (pa/K)
  REAL,                            INTENT(IN) :: LATHEAG !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: PARSUN !par absorbed per unit sunlit lai (w/m2)
  REAL,                            INTENT(IN) :: PARSHA !par absorbed per unit shaded lai (w/m2)
  REAL,                            INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL,                            INTENT(IN) :: CO2AIR !atmospheric co2 concentration (pa)
  REAL,                            INTENT(IN) :: O2AIR  !atmospheric o2 concentration (pa)
  REAL,                            INTENT(IN) :: IGS    !growing season index (0=off, 1=on)
  REAL,                            INTENT(IN) :: SFCPRS !pressure (pa)
  REAL,                            INTENT(IN) :: BTRAN  !soil water transpiration factor (0 to 1)
  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity in surface soil/snow air space (-)

  REAL                           , INTENT(IN) :: QC     !cloud water mixing ratio
  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN) :: DX     !grid spacing
  REAL                           , INTENT(IN) :: Q2     !mixing ratio (kg/kg)
  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
  REAL, INTENT(IN)   :: PAHV  !precipitation advected heat - canopy net IN (W/m2)
  REAL, INTENT(IN)   :: PAHG  !precipitation advected heat - ground net IN (W/m2)
  REAL                           , INTENT(IN) :: MQ     !water in plant tissues [kg]

! input/output
  REAL,                         INTENT(INOUT) :: EAH    !canopy air vapor pressure (pa)
  REAL,                         INTENT(INOUT) :: TAH    !canopy air temperature (k)
  REAL,                         INTENT(INOUT) :: TV     !vegetation temperature (k)
  REAL,                         INTENT(INOUT) :: TG     !ground temperature (k)
  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient

! output
! -FSA + FIRA + FSH + (FCEV + FCTR + FGEV) + FCST + SSOIL = 0
  REAL,                           INTENT(OUT) :: TAUXV  !wind stress: e-w (n/m2)
  REAL,                           INTENT(OUT) :: TAUYV  !wind stress: n-s (n/m2)
  REAL,                           INTENT(OUT) :: IRC    !net longwave radiation (w/m2) [+= to atm]
  REAL,                           INTENT(OUT) :: SHC    !sensible heat flux (w/m2)     [+= to atm]
  REAL,                           INTENT(OUT) :: EVC    !evaporation heat flux (w/m2)  [+= to atm]
  REAL,                           INTENT(OUT) :: IRG    !net longwave radiation (w/m2) [+= to atm]
  REAL,                           INTENT(OUT) :: SHG    !sensible heat flux (w/m2)     [+= to atm]
  REAL,                           INTENT(OUT) :: EVG    !evaporation heat flux (w/m2)  [+= to atm]
  REAL,                           INTENT(OUT) :: TR     !transpiration heat flux (w/m2)[+= to atm]
  REAL,                           INTENT(OUT) :: GH     !ground heat (w/m2) [+ = to soil]
  REAL,                           INTENT(OUT) :: T2MV   !2 m height air temperature (k)
  REAL,                           INTENT(OUT) :: PSNSUN !sunlit leaf photosynthesis (umolco2/m2/s)
  REAL,                           INTENT(OUT) :: PSNSHA !shaded leaf photosynthesis (umolco2/m2/s)
  REAL,                           INTENT(OUT) :: CHLEAF !leaf exchange coefficient
  REAL,                           INTENT(OUT) :: CHUC   !under canopy exchange coefficient
  REAL,                           INTENT(OUT) :: CANHS  !canopy heat storage change (w/m2)

  REAL,                           INTENT(OUT) :: Q2V
  REAL :: CAH    !sensible heat conductance, canopy air to ZLVL air (m/s)
  REAL :: U10V    !10 m wind speed in eastward dir (m/s) 
  REAL :: V10V    !10 m wind speed in eastward dir (m/s) 
  REAL :: WSPD

! ------------------------ local variables ----------------------------------------------------
  REAL :: CW           !water vapor exchange coefficient
  REAL :: FV           !friction velocity (m/s)
  REAL :: WSTAR        !friction velocity n vertical direction (m/s) (only for SFCDIF2)
  REAL :: Z0H          !roughness length, sensible heat (m)
  REAL :: Z0HG         !roughness length, sensible heat (m)
  REAL :: RB           !bulk leaf boundary layer resistance (s/m)
  REAL :: RAMC         !aerodynamic resistance for momentum (s/m)
  REAL :: RAHC         !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWC         !aerodynamic resistance for water vapor (s/m)
  REAL :: RAMG         !aerodynamic resistance for momentum (s/m)
  REAL :: RAHG         !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWG         !aerodynamic resistance for water vapor (s/m)

  REAL, INTENT(OUT) :: RSSUN        !sunlit leaf stomatal resistance (s/m)
  REAL, INTENT(OUT) :: RSSHA        !shaded leaf stomatal resistance (s/m)

  REAL :: MOL          !Monin-Obukhov length (m)
  REAL :: DTV          !change in tv, last iteration (k)
  REAL :: DTG          !change in tg, last iteration (k)

  REAL :: AIR,CIR      !coefficients for ir as function of ts**4
  REAL :: CSH          !coefficients for sh as function of ts
  REAL :: CEV          !coefficients for ev as function of esat[ts]
  REAL :: CGH          !coefficients for st as function of ts
  REAL :: ATR,CTR      !coefficients for tr as function of esat[ts]
  REAL :: ATA,BTA      !coefficients for tah as function of ts
  REAL :: AEA,BEA      !coefficients for eah as function of esat[ts]

  REAL :: ESTV         !saturation vapor pressure at tv (pa)
  REAL :: ESTG         !saturation vapor pressure at tg (pa)
  REAL :: DESTV        !d(es)/dt at ts (pa/k)
  REAL :: DESTG        !d(es)/dt at tg (pa/k)
  REAL :: ESATW        !es for water
  REAL :: ESATI        !es for ice
  REAL :: DSATW        !d(es)/dt at tg (pa/k) for water
  REAL :: DSATI        !d(es)/dt at tg (pa/k) for ice

  REAL :: FM           !momentum stability correction, weighted by prior iters
  REAL :: FH           !sen heat stability correction, weighted by prior iters
  REAL :: FHG          !sen heat stability correction, ground
  REAL :: HCAN         !canopy height (m) [note: hcan >= z0mg]

  REAL :: A            !temporary calculation
  REAL :: B            !temporary calculation
  REAL :: HCV          !canopy heat capacity (J/kg)
  REAL :: CVH          !sensible heat conductance, leaf surface to canopy air (m/s)
  REAL :: CAW          !latent heat conductance, canopy air ZLVL air (m/s)
  REAL :: CTW          !transpiration conductance, leaf to canopy air (m/s)
  REAL :: CEW          !evaporation conductance, leaf to canopy air (m/s)
  REAL :: CGW          !latent heat conductance, ground to canopy air (m/s)
  REAL :: COND         !sum of conductances (s/m)
  REAL :: UC           !wind speed at top of canopy (m/s)
  REAL :: KH           !turbulent transfer coefficient, sensible heat, (m2/s)
  REAL :: H            !temporary sensible heat flux (w/m2)
  REAL :: HG           !temporary sensible heat flux (w/m2)
  REAL :: MOZ          !Monin-Obukhov stability parameter
  REAL :: MOZG         !Monin-Obukhov stability parameter
  REAL :: MOZOLD       !Monin-Obukhov stability parameter from prior iteration
  REAL :: FM2          !Monin-Obukhov momentum adjustment at 2m
  REAL :: FH2          !Monin-Obukhov heat adjustment at 2m
  REAL :: CH2          !Surface exchange at 2m
  REAL :: THSTAR          !Surface exchange at 2m

  REAL :: THVAIR
  REAL :: THAH 
  REAL :: RAHC2        !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWC2        !aerodynamic resistance for water vapor (s/m)
  REAL, INTENT(OUT):: CAH2         !sensible heat conductance for diagnostics
  REAL :: CH2V         !exchange coefficient for 2m over vegetation. 
  REAL :: CQ2V         !exchange coefficient for 2m over vegetation. 
  REAL :: EAH2         !2m vapor pressure over canopy
  REAL :: QFX        !moisture flux
  REAL :: E1           


  REAL :: VAIE         !total leaf area index + stem area index,effective
  REAL :: LAISUNE      !sunlit leaf area index, one-sided (m2/m2),effective
  REAL :: LAISHAE      !shaded leaf area index, one-sided (m2/m2),effective

  INTEGER :: K         !index
  INTEGER :: ITER      !iteration index

!jref - NITERC test from 5 to 20  
  INTEGER, PARAMETER :: NITERC = 10   !number of iterations for surface temperature !niu
!jref - NITERG test from 3-5
  INTEGER, PARAMETER :: NITERG = 6   !number of iterations for ground temperature !niu
  INTEGER :: MOZSGN    !number of times MOZ changes sign
  REAL    :: MPE       !prevents overflow error if division by zero

  INTEGER :: LITER     !Last iteration


  REAL :: T, TDC       !Kelvin to degree Celsius with limit -50 to +50

  character(len=80) ::  message

  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )
! ---------------------------------------------------------------------------------------------

        MPE = 1E-6
        LITER = 0
        FV = 0.1

! ---------------------------------------------------------------------------------------------
! initialization variables that do not depend on stability iteration
! ---------------------------------------------------------------------------------------------
        DTV = 0.
        DTG = 0.
        MOZ    = 0.
        MOZSGN = 0
        MOZOLD = 0.
        FH2    = 0.
        HG     = 0.
        H      = 0.
        QFX    = 0.

! convert grid-cell LAI to the fractional vegetated area (FVEG)

        VAIE    = VAI    / FVEG
        LAISUNE = LAISUN / FVEG
        LAISHAE = LAISHA / FVEG

! canopy heat capacity

      ! HCV = (MQ+CANLIQ)*CWAT/DENH2O + CANICE*CICE/DENICE    !j/m2/k
        HCV = 0.02*VAIE*CWAT + CANLIQ*CWAT/DENH2O + CANICE*CICE/DENICE    !j/m2/k

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) "TV=",TV
    ! write(*,*) "TG=",TG
    ! write(*,*) "HCV=",HCV
    !END IF

! saturation vapor pressure at ground temperature

        T = TDC(TG)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
           ESTG = ESATW
        ELSE
           ESTG = ESATI
        END IF

!jref - consistent surface specific humidity for sfcdif3 and sfcdif4

        QSFC = 0.622*EAIR/(PSFC-0.378*EAIR)  

! canopy height

        HCAN = parameters%HVT
!       IF(ILOC == 137 .and. JLOC == 7) &
!       write(*,*) HCAN,ZLVL,ZPD
!        UC = UR*LOG(HCAN/Z0M)/LOG(ZLVL/Z0M)

        IF((HCAN-ZPD) <= 0.) THEN
          WRITE(message,*) "CRITICAL PROBLEM: HCAN <= ZPD"
          call wrf_message ( message )
          WRITE(message,*) 'i,j point=',ILOC, JLOC
          call wrf_message ( message )
          WRITE(message,*) 'HCAN  =',HCAN
          call wrf_message ( message )
          WRITE(message,*) 'ZPD   =',ZPD
          call wrf_message ( message )
          write (message, *) 'SNOWH =',SNOWH
          call wrf_message ( message )
          call wrf_error_fatal ( "CRITICAL PROBLEM IN MODULE_SF_NOAHMPLSM:VEGEFLUX" )
        END IF
        UC = UR*LOG((HCAN-ZPD+Z0M)/Z0M)/LOG(ZLVL/Z0M)   ! MB: add ZPD v3.7

! prepare for longwave rad.

        AIR = -EMV*(1.+(1.-EMV)*(1.-EMG))*LWDN - EMV*EMG*SB*TG**4  
        CIR = (2.-EMV*(1.-EMG))*EMV*SB
! ---------------------------------------------------------------------------------------------
      loop1: DO ITER = 1, NITERC    !  begin stability iteration

       IF(ITER == 1) THEN
            Z0H  = Z0M  
            Z0HG = Z0MG
       ELSE
            Z0H  = Z0M    !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
            Z0HG = Z0MG   !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0MG))
       END IF

! aerodyn resistances between heights zlvl and d+z0v

       IF(OPT_SFC == 1) THEN
          CALL SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
                       MPE    ,ILOC   ,JLOC   ,                 & !in
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
                       CM     ,CH     ,FV     ,CH2     )          !out
       ENDIF
     
       IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(parameters,ITER   ,Z0M    ,TAH    ,THAIR  ,UR     , & !in
                       ZLVL   ,ILOC   ,JLOC   ,         & !in
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & !in
                       FV     )                                   !out
          ! Undo the multiplication by windspeed that SFCDIF2 
          ! applies to exchange coefficients CH and CM:
          CH = CH / UR
          CM = CM / UR
       ENDIF

       RAMC = MAX(1.,1./(CM*UR))
       RAHC = MAX(1.,1./(CH*UR))
       RAWC = RAHC

! aerodyn resistance between heights z0g and d+z0v, RAG, and leaf
! boundary layer resistance, RB
       
       CALL RAGRB(parameters,ITER   ,VAIE   ,RHOAIR ,HG     ,TAH    , & !in
                  ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & !in
                  Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & !in
                  TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & !inout
                  RAMG   ,RAHG   ,RAWG   ,RB     )           !out

! es and d(es)/dt evaluated at tv

       T = TDC(TV)
       CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
       IF (T .GT. 0.) THEN
          ESTV  = ESATW
          DESTV = DSATW
       ELSE
          ESTV  = ESATI
          DESTV = DSATI
       END IF

! stomatal resistance

     IF(ITER == 1) THEN
        IF (OPT_CRS == 1) then  ! Ball-Berry
         CALL STOMATA (parameters,VEGTYP,MPE   ,PARSUN ,FOLN  ,ILOC  , JLOC , & !in       
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !in
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !in
                       RSSUN ,PSNSUN)                         !out

         CALL STOMATA (parameters,VEGTYP,MPE   ,PARSHA ,FOLN  ,ILOC  , JLOC , & !in
                       TV    ,ESTV  ,EAH    ,SFCTMP,SFCPRS, & !in
                       O2AIR ,CO2AIR,IGS    ,BTRAN ,RB    , & !in
                       RSSHA ,PSNSHA)                         !out
        END IF

        IF (OPT_CRS == 2) then  ! Jarvis
         CALL  CANRES (parameters,PARSUN,TV    ,BTRAN ,EAH    ,SFCPRS, & !in
                       RSSUN ,PSNSUN,ILOC  ,JLOC   )          !out

         CALL  CANRES (parameters,PARSHA,TV    ,BTRAN ,EAH    ,SFCPRS, & !in
                       RSSHA ,PSNSHA,ILOC  ,JLOC   )          !out
        END IF
     END IF

! prepare for sensible heat flux above veg.

        CAH  = 1./RAHC
        CVH  = 2.*VAIE/RB
        CGH  = 1./RAHG
        COND = CAH + CVH + CGH
        ATA  = (SFCTMP*CAH + TG*CGH) / COND
        BTA  = CVH/COND
        CSH  = (1.-BTA)*RHOAIR*CPAIR*CVH

! prepare for latent heat flux above veg.

        CAW  = 1./RAWC
        CEW  = FWET*VAIE/RB
        CTW  = (1.-FWET)*(LAISUNE/(RB+RSSUN) + LAISHAE/(RB+RSSHA))
        CGW  = 1./(RAWG+RSURF)
        COND = CAW + CEW + CTW + CGW
        AEA  = (EAIR*CAW + ESTG*CGW) / COND
        BEA  = (CEW+CTW)/COND
        CEV  = (1.-BEA)*CEW*RHOAIR*CPAIR/GAMMAV   ! Barlage: change to vegetation v3.6
        CTR  = (1.-BEA)*CTW*RHOAIR*CPAIR/GAMMAV

! evaluate surface fluxes with current temperature and solve for dts

        TAH = ATA + BTA*TV               ! canopy air T.
        EAH = AEA + BEA*ESTV             ! canopy air e

        IRC = FVEG*(AIR + CIR*TV**4)
        SHC = FVEG*RHOAIR*CPAIR*CVH * (  TV-TAH)
        EVC = FVEG*RHOAIR*CPAIR*CEW * (ESTV-EAH) / GAMMAV ! Barlage: change to v in v3.6
        TR  = FVEG*RHOAIR*CPAIR*CTW * (ESTV-EAH) / GAMMAV

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) "FVEG,AIR,CIR,TV=",FVEG,AIR,CIR,TV
    ! write(*,*) "AIR (EMV,EMG,LWDN,TG)=",EMV,EMG,LWDN,TG
    ! write(*,*) "SHC (RHOAIR,CPAIR,CVH,TAH)",RHOAIR,CPAIR,CVH,TAH
    !END IF

        IF (TV > TFRZ) THEN
         EVC = MIN(CANLIQ*LATHEAV/DT,EVC)    ! Barlage: add if block for canice in v3.6
        ELSE
         EVC = MIN(CANICE*LATHEAV/DT,EVC)
        END IF

        B   = SAV-IRC-SHC-EVC-TR+PAHV                          !additional w/m2
!       A   = FVEG*(4.*CIR*TV**3 + CSH + (CEV+CTR)*DESTV)      !niu
        A   = FVEG*(4.*CIR*TV**3 + CSH + (CEV+CTR)*DESTV) + HCV/DT  !heat capacity added by niu; more stable
        DTV = B/A

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) "B,A=",B,A
    ! write(*,*) "B(SAV,IRC,SHC,EVC,TR,PAHV)=",SAV,IRC,SHC,EVC,TR,PAHV
    !END IF

        IRC = IRC + FVEG*4.*CIR*TV**3*DTV
        SHC = SHC + FVEG*CSH*DTV
        EVC = EVC + FVEG*CEV*DESTV*DTV
        TR  = TR  + FVEG*CTR*DESTV*DTV                               

! update vegetation surface temperature
        TV  = TV + DTV

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) "TV,DTV=",TV,DTV
    ! write(*,*) "TR,FVEG,CTR,DESTV,DTV=",TR,FVEG,CTR,DESTV,DTV
    !END IF

! for computing M-O length in the next iteration
        H  = RHOAIR*CPAIR*(TAH - SFCTMP) /RAHC        
        HG = RHOAIR*CPAIR*(TG  - TAH)   /RAHG

! consistent specific humidity from canopy air vapor pressure
        QSFC = (0.622*EAH)/(SFCPRS-0.378*EAH)

        IF (LITER == 1) THEN
           exit loop1 
        ENDIF
        IF (ITER >= 5 .AND. ABS(DTV) <= 0.01 .AND. LITER == 0) THEN
           LITER = 1
        ENDIF

     END DO loop1 ! end stability iteration

! canopy heat storage

     CANHS = DTV*HCV/DT       !w/m2

! under-canopy fluxes and tg

        AIR = - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
        CIR = EMG*SB
        CSH = RHOAIR*CPAIR/RAHG
        CEV = RHOAIR*CPAIR / (GAMMAG*(RAWG+RSURF))  ! Barlage: change to ground v3.6
        CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)

     loop2: DO ITER = 1, NITERG

        T = TDC(TG)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF

        IRG = CIR*TG**4 + AIR
        SHG = CSH * (TG         - TAH         )
        EVG = CEV * (ESTG*RHSUR - EAH         )
        GH  = CGH * (TG         - STC(ISNOW+1))

        B = SAG-IRG-SHG-EVG-GH+PAHG
        A = 4.*CIR*TG**3+CSH+CEV*DESTG+CGH
        DTG = B/A

        IRG = IRG + 4.*CIR*TG**3*DTG
        SHG = SHG + CSH*DTG
        EVG = EVG + CEV*DESTG*DTG
        GH  = GH  + CGH*DTG
        TG  = TG  + DTG

     END DO loop2
     
!     TAH = (CAH*SFCTMP + CVH*TV + CGH*TG)/(CAH + CVH + CGH)

! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.

     IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
!NIU     IF (SNOWH > 0.05 .AND. TG > TFRZ) THEN
     IF (SNOWH > 0.01 .AND. TG > TFRZ) THEN
        IF(OPT_STC == 1) TG  = TFRZ
        IF(OPT_STC == 3) TG  = (1.-FSNO)*TG + FSNO*TFRZ   ! MB: allow TG>0C during melt v3.7
        IRG = CIR*TG**4 - EMG*(1.-EMV)*LWDN - EMG*EMV*SB*TV**4
        SHG = CSH * (TG         - TAH)
        EVG = CEV * (ESTG*RHSUR - EAH)
        GH  = SAG+PAHG - (IRG+SHG+EVG)
     END IF
     END IF

! wind stresses

     TAUXV = -RHOAIR*CM*UR*UU
     TAUYV = -RHOAIR*CM*UR*VV

! consistent vegetation air temperature and vapor pressure since TG is not consistent with the TAH/EAH
! calculation.
!     TAH = SFCTMP + (SHG+SHC)/(RHOAIR*CPAIR*CAH) 
!     TAH = SFCTMP + (SHG*FVEG+SHC)/(RHOAIR*CPAIR*CAH) ! ground flux need fveg
!     EAH = EAIR + (EVC+FVEG*(TR+EVG))/(RHOAIR*CAW*CPAIR/GAMMAG )
!     QFX = (QSFC-QAIR)*RHOAIR*CAW !*CPAIR/GAMMAG

! 2m temperature over vegetation ( corrected for low CQ2V values )
   IF (OPT_SFC == 1 .OR. OPT_SFC == 2) THEN
!      CAH2 = FV*1./VKC*LOG((2.+Z0H)/Z0H)
      CAH2 = FV*VKC/LOG((2.+Z0H)/Z0H)
      CAH2 = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
      CQ2V = CAH2
      IF (CAH2 .LT. 1.E-5 ) THEN
         T2MV = TAH
!         Q2V  = (EAH*0.622/(SFCPRS - 0.378*EAH))
         Q2V  = QSFC
      ELSE
         T2MV = TAH - (SHG+SHC/FVEG)/(RHOAIR*CPAIR) * 1./CAH2
!         Q2V = (EAH*0.622/(SFCPRS - 0.378*EAH))- QFX/(RHOAIR*FV)* 1./VKC * LOG((2.+Z0H)/Z0H)
         Q2V = QSFC - ((EVC+TR)/FVEG+EVG)/(LATHEAV*RHOAIR) * 1./CQ2V
      ENDIF
   ENDIF

! update CH for output
     CH = CAH
     CHLEAF = CVH
     CHUC = 1./RAHG

  END SUBROUTINE VEGE_FLUX

!== begin bare_flux ================================================================================

  SUBROUTINE BARE_FLUX (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,SAG     , & !in
                        LWDN    ,UR      ,UU      ,VV      ,SFCTMP  , & !in
                        THAIR   ,QAIR    ,EAIR    ,RHOAIR  ,SNOWH   , & !in
                        DZSNSO  ,ZLVL    ,ZPD     ,Z0M     ,FSNO    , & !in
                        EMG     ,STC     ,DF      ,RSURF   ,LATHEA  , & !in
                        GAMMA   ,RHSUR   ,ILOC    ,JLOC    ,Q2      ,PAHB  , & !in
                        TGB     ,CM      ,CH      ,          & !inout
                        TAUXB   ,TAUYB   ,IRB     ,SHB     ,EVB     , & !out
                        GHB     ,T2MB    ,DX      ,DZ8W    ,IVGTYP  , & !out
                        QC      ,QSFC    ,PSFC    ,                   & !in
                        SFCPRS  ,Q2B     ,EHB2    )                     !in 

! --------------------------------------------------------------------------------------------------
! use newton-raphson iteration to solve ground (tg) temperature
! that balances the surface energy budgets for bare soil fraction.

! bare soil:
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  integer                        , INTENT(IN) :: ILOC   !grid index
  integer                        , INTENT(IN) :: JLOC   !grid index
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER,                         INTENT(IN) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(IN) :: DT     !time step (s)
  REAL,                            INTENT(IN) :: SAG    !solar radiation absorbed by ground (w/m2)
  REAL,                            INTENT(IN) :: LWDN   !atmospheric longwave radiation (w/m2)
  REAL,                            INTENT(IN) :: UR     !wind speed at height zlvl (m/s)
  REAL,                            INTENT(IN) :: UU     !wind speed in eastward dir (m/s)
  REAL,                            INTENT(IN) :: VV     !wind speed in northward dir (m/s)
  REAL,                            INTENT(IN) :: SFCTMP !air temperature at reference height (k)
  REAL,                            INTENT(IN) :: THAIR  !potential temperature at height zlvl (k)
  REAL,                            INTENT(IN) :: QAIR   !specific humidity at height zlvl (kg/kg)
  REAL,                            INTENT(IN) :: EAIR   !vapor pressure air at height (pa)
  REAL,                            INTENT(IN) :: RHOAIR !density air (kg/m3)
  REAL,                            INTENT(IN) :: SNOWH  !actual snow depth [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !thickness of snow/soil layers (m)
  REAL,                            INTENT(IN) :: ZLVL   !reference height (m)
  REAL,                            INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                            INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
  REAL,                            INTENT(IN) :: EMG    !ground emissivity
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !soil/snow temperature (k)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DF     !thermal conductivity of snow/soil (w/m/k)
  REAL,                            INTENT(IN) :: RSURF  !ground surface resistance (s/m)
  REAL,                            INTENT(IN) :: LATHEA !latent heat of vaporization/subli (j/kg)
  REAL,                            INTENT(IN) :: GAMMA  !psychrometric constant (pa/k)
  REAL,                            INTENT(IN) :: RHSUR  !raltive humidity in surface soil/snow air space (-)
  REAL,                            INTENT(IN) :: FSNO     !snow fraction

!jref:start; in 
  INTEGER                        , INTENT(IN) :: IVGTYP
  REAL                           , INTENT(IN) :: QC     !cloud water mixing ratio
  REAL                           , INTENT(INOUT) :: QSFC   !mixing ratio at lowest model layer
  REAL                           , INTENT(IN) :: PSFC   !pressure at lowest model layer
  REAL                           , INTENT(IN) :: SFCPRS !pressure at lowest model layer
  REAL                           , INTENT(IN) :: DX     !horisontal grid spacing
  REAL                           , INTENT(IN) :: Q2     !mixing ratio (kg/kg)
  REAL                           , INTENT(IN) :: DZ8W   !thickness of lowest layer
!jref:end
  REAL, INTENT(IN)   :: PAHB  !precipitation advected heat - ground net IN (W/m2)

! input/output
  REAL,                         INTENT(INOUT) :: TGB    !ground temperature (k)
  REAL,                         INTENT(INOUT) :: CM     !momentum drag coefficient
  REAL,                         INTENT(INOUT) :: CH     !sensible heat exchange coefficient

! output
! -SAB + IRB[TG] + SHB[TG] + EVB[TG] + GHB[TG] = 0

  REAL,                           INTENT(OUT) :: TAUXB  !wind stress: e-w (n/m2)
  REAL,                           INTENT(OUT) :: TAUYB  !wind stress: n-s (n/m2)
  REAL,                           INTENT(OUT) :: IRB    !net longwave rad (w/m2)   [+ to atm]
  REAL,                           INTENT(OUT) :: SHB    !sensible heat flux (w/m2) [+ to atm]
  REAL,                           INTENT(OUT) :: EVB    !latent heat flux (w/m2)   [+ to atm]
  REAL,                           INTENT(OUT) :: GHB    !ground heat flux (w/m2)  [+ to soil]
  REAL,                           INTENT(OUT) :: T2MB   !2 m height air temperature (k)
!jref:start
  REAL,                           INTENT(OUT) :: Q2B    !bare ground heat conductance
  REAL :: EHB    !bare ground heat conductance
  REAL :: U10B    !10 m wind speed in eastward dir (m/s)
  REAL :: V10B    !10 m wind speed in eastward dir (m/s)
  REAL :: WSPD
!jref:end

! local variables 

  REAL :: TAUX       !wind stress: e-w (n/m2)
  REAL :: TAUY       !wind stress: n-s (n/m2)
  REAL :: FIRA       !total net longwave rad (w/m2)      [+ to atm]
  REAL :: FSH        !total sensible heat flux (w/m2)    [+ to atm]
  REAL :: FGEV       !ground evaporation heat flux (w/m2)[+ to atm]
  REAL :: SSOIL      !soil heat flux (w/m2)             [+ to soil]
  REAL :: FIRE       !emitted ir (w/m2)
  REAL :: TRAD       !radiative temperature (k)
  REAL :: TAH        !"surface" temperature at height z0h+zpd (k)

  REAL :: CW         !water vapor exchange coefficient
  REAL :: FV         !friction velocity (m/s)
  REAL :: WSTAR      !friction velocity n vertical direction (m/s) (only for SFCDIF2)
  REAL :: Z0H        !roughness length, sensible heat, ground (m)
  REAL :: RB         !bulk leaf boundary layer resistance (s/m)
  REAL :: RAMB       !aerodynamic resistance for momentum (s/m)
  REAL :: RAHB       !aerodynamic resistance for sensible heat (s/m)
  REAL :: RAWB       !aerodynamic resistance for water vapor (s/m)
  REAL :: MOL        !Monin-Obukhov length (m)
  REAL :: DTG        !change in tg, last iteration (k)

  REAL :: CIR        !coefficients for ir as function of ts**4
  REAL :: CSH        !coefficients for sh as function of ts
  REAL :: CEV        !coefficients for ev as function of esat[ts]
  REAL :: CGH        !coefficients for st as function of ts

!jref:start
  REAL :: RAHB2      !aerodynamic resistance for sensible heat 2m (s/m)
  REAL :: RAWB2      !aerodynamic resistance for water vapor 2m (s/m)
  REAL,INTENT(OUT) :: EHB2       !sensible heat conductance for diagnostics
  REAL :: CH2B       !exchange coefficient for 2m temp.
  REAL :: CQ2B       !exchange coefficient for 2m temp.
  REAL :: THVAIR     !virtual potential air temp
  REAL :: THGH       !potential ground temp
  REAL :: EMB        !momentum conductance
  REAL :: QFX        !moisture flux
  REAL :: ESTG2      !saturation vapor pressure at 2m (pa)
  INTEGER :: VEGTYP     !vegetation type set to isbarren
  REAL :: E1
!jref:end

  REAL :: ESTG       !saturation vapor pressure at tg (pa)
  REAL :: DESTG      !d(es)/dt at tg (pa/K)
  REAL :: ESATW      !es for water
  REAL :: ESATI      !es for ice
  REAL :: DSATW      !d(es)/dt at tg (pa/K) for water
  REAL :: DSATI      !d(es)/dt at tg (pa/K) for ice

  REAL :: A          !temporary calculation
  REAL :: B          !temporary calculation
  REAL :: H          !temporary sensible heat flux (w/m2)
  REAL :: MOZ        !Monin-Obukhov stability parameter
  REAL :: MOZOLD     !Monin-Obukhov stability parameter from prior iteration
  REAL :: FM         !momentum stability correction, weighted by prior iters
  REAL :: FH         !sen heat stability correction, weighted by prior iters
  INTEGER :: MOZSGN  !number of times MOZ changes sign
  REAL :: FM2          !Monin-Obukhov momentum adjustment at 2m
  REAL :: FH2          !Monin-Obukhov heat adjustment at 2m
  REAL :: CH2          !Surface exchange at 2m

  INTEGER :: ITER    !iteration index
  INTEGER :: NITERB  !number of iterations for surface temperature
  REAL    :: MPE     !prevents overflow error if division by zero
!jref:start
!  DATA NITERB /3/
  DATA NITERB /5/
  SAVE NITERB
  REAL :: T, TDC     !Kelvin to degree Celsius with limit -50 to +50
  TDC(T)   = MIN( 50., MAX(-50.,(T-TFRZ)) )

! -----------------------------------------------------------------
! initialization variables that do not depend on stability iteration
! -----------------------------------------------------------------
        MPE = 1E-6
        DTG = 0.
        MOZ    = 0.
        MOZSGN = 0
        MOZOLD = 0.
        FH2    = 0.
        H      = 0.
        QFX    = 0.
        FV     = 0.1

        CIR = EMG*SB
        CGH = 2.*DF(ISNOW+1)/DZSNSO(ISNOW+1)

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) '----------begin BARE_FLUX-------------------------'
    ! write(*,*) "TGB=",TGB
    !END IF
! -----------------------------------------------------------------
      loop3: DO ITER = 1, NITERB  ! begin stability iteration

        IF(ITER == 1) THEN
            Z0H = Z0M 
        ELSE
            Z0H = Z0M !* EXP(-CZIL*0.4*258.2*SQRT(FV*Z0M))
        END IF

        IF(OPT_SFC == 1) THEN
          CALL SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
                       ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
                       MPE    ,ILOC   ,JLOC   ,                 & !in
                       MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
                       CM     ,CH     ,FV     ,CH2     )          !out
        ENDIF

        IF(OPT_SFC == 2) THEN
          CALL SFCDIF2(parameters,ITER   ,Z0M    ,TGB    ,THAIR  ,UR     , & !in
                       ZLVL   ,ILOC   ,JLOC   ,         & !in
                       CM     ,CH     ,MOZ    ,WSTAR  ,         & !in
                       FV     )                                   !out
          ! Undo the multiplication by windspeed that SFCDIF2 
          ! applies to exchange coefficients CH and CM:
          CH = CH / UR
          CM = CM / UR
          IF(SNOWH > 0.) THEN
             CM = MIN(0.01,CM)   ! CM & CH are too large, causing
             CH = MIN(0.01,CH)   ! computational instability
          END IF

        ENDIF

        RAMB = MAX(1.,1./(CM*UR))
        RAHB = MAX(1.,1./(CH*UR))
        RAWB = RAHB

!jref - variables for diagnostics         
        EMB = 1./RAMB
        EHB = 1./RAHB

! es and d(es)/dt evaluated at tg

        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
            DESTG = DSATW
        ELSE
            ESTG  = ESATI
            DESTG = DSATI
        END IF

        CSH = RHOAIR*CPAIR/RAHB
        CEV = RHOAIR*CPAIR/GAMMA/(RSURF+RAWB)

! surface fluxes and dtg

        IRB   = CIR * TGB**4 - EMG*LWDN
        SHB   = CSH * (TGB        - SFCTMP      )
        EVB   = CEV * (ESTG*RHSUR - EAIR        )
        GHB   = CGH * (TGB        - STC(ISNOW+1))

        B     = SAG-IRB-SHB-EVB-GHB+PAHB
        A     = 4.*CIR*TGB**3 + CSH + CEV*DESTG + CGH
        DTG   = B/A

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) '----------in BARE_FLUX-------------------------'
    ! write(*,*) "B,A=",B,A
     !write(*,*) "B(SAG,IRB,SHB,EVB,GHB,PAHB) =",SAG,IRB,SHB,EVB,GHB,PAHB
     !write(*,*) "A(CIR,TGB,CSH,CEV,DESTG,CGH)=",CIR,TGB,CSH,CEV,DESTG,CGH
    ! write(*,*) "ISNOW+1=",ISNOW+1
    ! write(*,*) "TGB         =",TGB
    ! write(*,*) "STC(ISNOW+1)=",STC(ISNOW+1)
    !END IF

        IRB = IRB + 4.*CIR*TGB**3*DTG
        SHB = SHB + CSH*DTG
        EVB = EVB + CEV*DESTG*DTG
        GHB = GHB + CGH*DTG

! update ground surface temperature
        TGB = TGB + DTG

! for M-O length
        H = CSH * (TGB - SFCTMP)

        T = TDC(TGB)
        CALL ESAT(T, ESATW, ESATI, DSATW, DSATI)
        IF (T .GT. 0.) THEN
            ESTG  = ESATW
        ELSE
            ESTG  = ESATI
        END IF
        QSFC = 0.622*(ESTG*RHSUR)/(PSFC-0.378*(ESTG*RHSUR))

        QFX = (QSFC-QAIR)*CEV*GAMMA/CPAIR
    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) '----------in BARE_FLUX-------------------------'
    ! write(*,*) "TGB=",TGB
    !END IF

     END DO loop3 ! end stability iteration
! -----------------------------------------------------------------

! if snow on ground and TG > TFRZ: reset TG = TFRZ. reevaluate ground fluxes.

     IF(OPT_STC == 1 .OR. OPT_STC == 3) THEN
!NIU     IF (SNOWH > 0.05 .AND. TGB > TFRZ) THEN
     IF (SNOWH > 0.01 .AND. TGB > TFRZ) THEN
          IF(OPT_STC == 1) TGB = TFRZ
          IF(OPT_STC == 3) TGB  = (1.-FSNO)*TGB + FSNO*TFRZ  ! MB: allow TG>0C during melt v3.7
          IRB = CIR * TGB**4 - EMG*LWDN
          SHB = CSH * (TGB        - SFCTMP)
          EVB = CEV * (ESTG*RHSUR - EAIR )          !ESTG reevaluate ?
          GHB = SAG+PAHB - (IRB+SHB+EVB)
     END IF
     END IF

! wind stresses
         
     TAUXB = -RHOAIR*CM*UR*UU
     TAUYB = -RHOAIR*CM*UR*VV

!jref:start; errors in original equation corrected.
! 2m air temperature
     IF(OPT_SFC == 1 .OR. OPT_SFC ==2) THEN
       EHB2  = FV*VKC/LOG((2.+Z0H)/Z0H)
       EHB2  = FV*VKC/(LOG((2.+Z0H)/Z0H)-FH2)
       CQ2B  = EHB2
       IF (EHB2.lt.1.E-5 ) THEN
         T2MB  = TGB
         Q2B   = QSFC
       ELSE
         T2MB  = TGB - SHB/(RHOAIR*CPAIR) * 1./EHB2
         Q2B   = QSFC - EVB/(LATHEA*RHOAIR)*(1./CQ2B + RSURF)
       ENDIF
       IF (parameters%urban_flag) Q2B = QSFC
     END IF

! update CH 
     CH = EHB

  END SUBROUTINE BARE_FLUX

!== begin ragrb ====================================================================================

  SUBROUTINE RAGRB(parameters,ITER   ,VAI    ,RHOAIR ,HG     ,TAH    , & !in
                   ZPD    ,Z0MG   ,Z0HG   ,HCAN   ,UC     , & !in
                   Z0H    ,FV     ,CWP    ,VEGTYP ,MPE    , & !in
                   TV     ,MOZG   ,FHG    ,ILOC   ,JLOC   , & !inout
                   RAMG   ,RAHG   ,RAWG   ,RB     )           !out
! --------------------------------------------------------------------------------------------------
! compute under-canopy aerodynamic resistance RAG and leaf boundary layer
! resistance RB
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,              INTENT(IN) :: ILOC   !grid index
  INTEGER,              INTENT(IN) :: JLOC   !grid index
  INTEGER,              INTENT(IN) :: ITER   !iteration index
  INTEGER,              INTENT(IN) :: VEGTYP !vegetation physiology type
  REAL,                 INTENT(IN) :: VAI    !total LAI + stem area index, one sided
  REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m3)
  REAL,                 INTENT(IN) :: HG     !ground sensible heat flux (w/m2)
  REAL,                 INTENT(IN) :: TV     !vegetation temperature (k)
  REAL,                 INTENT(IN) :: TAH    !air temperature at height z0h+zpd (k)
  REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
  REAL,                 INTENT(IN) :: Z0MG   !roughness length, momentum, ground (m)
  REAL,                 INTENT(IN) :: HCAN   !canopy height (m) [note: hcan >= z0mg]
  REAL,                 INTENT(IN) :: UC     !wind speed at top of canopy (m/s)
  REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat (m)
  REAL,                 INTENT(IN) :: Z0HG   !roughness length, sensible heat, ground (m)
  REAL,                 INTENT(IN) :: FV     !friction velocity (m/s)
  REAL,                 INTENT(IN) :: CWP    !canopy wind parameter
  REAL,                 INTENT(IN) :: MPE    !prevents overflow error if division by zero

! in & out

  REAL,              INTENT(INOUT) :: MOZG   !Monin-Obukhov stability parameter
  REAL,              INTENT(INOUT) :: FHG    !stability correction

! outputs
  REAL                             :: RAMG   !aerodynamic resistance for momentum (s/m)
  REAL                             :: RAHG   !aerodynamic resistance for sensible heat (s/m)
  REAL                             :: RAWG   !aerodynamic resistance for water vapor (s/m)
  REAL                             :: RB     !bulk leaf boundary layer resistance (s/m)


  REAL :: KH           !turbulent transfer coefficient, sensible heat, (m2/s)
  REAL :: TMP1         !temporary calculation
  REAL :: TMP2         !temporary calculation
  REAL :: TMPRAH2      !temporary calculation for aerodynamic resistances
  REAL :: TMPRB        !temporary calculation for rb
  real :: MOLG,FHGNEW,CWPC
! --------------------------------------------------------------------------------------------------
! stability correction to below canopy resistance

       MOZG = 0.
       MOLG = 0.

       IF(ITER > 1) THEN
        TMP1 = VKC * (GRAV/TAH) * HG/(RHOAIR*CPAIR)
        IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
        MOLG = -1. * FV**3 / TMP1
        MOZG = MIN( (ZPD-Z0MG)/MOLG, 1.)
       END IF

       IF (MOZG < 0.) THEN
          FHGNEW  = (1. - 15.*MOZG)**(-0.25)
       ELSE
          FHGNEW  = 1.+ 4.7*MOZG
       ENDIF

       IF (ITER == 1) THEN
          FHG = FHGNEW
       ELSE
          FHG = 0.5 * (FHG+FHGNEW)
       ENDIF

       CWPC = (CWP * VAI * HCAN * FHG)**0.5
!      CWPC = (CWP*FHG)**0.5

       TMP1 = EXP( -CWPC*Z0HG/HCAN )
       TMP2 = EXP( -CWPC*(Z0H+ZPD)/HCAN )
       TMPRAH2 = HCAN*EXP(CWPC) / CWPC * (TMP1-TMP2)

! aerodynamic resistances raw and rah between heights zpd+z0h and z0hg.

       KH  = MAX ( VKC*FV*(HCAN-ZPD), MPE )
       RAMG = 0.
       RAHG = TMPRAH2 / KH
       RAWG = RAHG

! leaf boundary layer resistance

       TMPRB  = CWPC*50. / (1. - EXP(-CWPC/2.))
       RB     = TMPRB * SQRT(parameters%DLEAF/UC)

  END SUBROUTINE RAGRB

!== begin sfcdif1 ==================================================================================

  SUBROUTINE SFCDIF1(parameters,ITER   ,SFCTMP ,RHOAIR ,H      ,QAIR   , & !in
       &             ZLVL   ,ZPD    ,Z0M    ,Z0H    ,UR     , & !in
       &             MPE    ,ILOC   ,JLOC   ,                 & !in
       &             MOZ    ,MOZSGN ,FM     ,FH     ,FM2,FH2, & !inout
       &             CM     ,CH     ,FV     ,CH2     )          !out
! -------------------------------------------------------------------------------------------------
! computing surface drag coefficient CM for momentum and CH for heat
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
! -------------------------------------------------------------------------------------------------
! inputs
    
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,              INTENT(IN) :: ILOC   !grid index
    INTEGER,              INTENT(IN) :: JLOC   !grid index
    INTEGER,              INTENT(IN) :: ITER   !iteration index
    REAL,                 INTENT(IN) :: SFCTMP !temperature at reference height (k)
    REAL,                 INTENT(IN) :: RHOAIR !density air (kg/m**3)
    REAL,                 INTENT(IN) :: H      !sensible heat flux (w/m2) [+ to atm]
    REAL,                 INTENT(IN) :: QAIR   !specific humidity at reference height (kg/kg)
    REAL,                 INTENT(IN) :: ZLVL   !reference height  (m)
    REAL,                 INTENT(IN) :: ZPD    !zero plane displacement (m)
    REAL,                 INTENT(IN) :: Z0H    !roughness length, sensible heat, ground (m)
    REAL,                 INTENT(IN) :: Z0M    !roughness length, momentum, ground (m)
    REAL,                 INTENT(IN) :: UR     !wind speed (m/s)
    REAL,                 INTENT(IN) :: MPE    !prevents overflow error if division by zero
! in & out

    INTEGER,           INTENT(INOUT) :: MOZSGN !number of times moz changes sign
    REAL,              INTENT(INOUT) :: MOZ    !Monin-Obukhov stability (z/L)
    REAL,              INTENT(INOUT) :: FM     !momentum stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH     !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FM2    !sen heat stability correction, weighted by prior iters
    REAL,              INTENT(INOUT) :: FH2    !sen heat stability correction, weighted by prior iters

! outputs

    REAL,                INTENT(OUT) :: CM     !drag coefficient for momentum
    REAL,                INTENT(OUT) :: CH     !drag coefficient for heat
!   REAL,                INTENT(OUT) :: FV     !friction velocity (m/s)
    REAL,                INTENT(INOUT) :: FV     !friction velocity (m/s)
    REAL,                INTENT(OUT) :: CH2    !drag coefficient for heat

! locals
    REAL    :: MOL                      !Monin-Obukhov length (m)
    REAL    :: TMPCM                    !temporary calculation for CM
    REAL    :: TMPCH                    !temporary calculation for CH
    REAL    :: FMNEW                    !stability correction factor, momentum, for current moz
    REAL    :: FHNEW                    !stability correction factor, sen heat, for current moz
    REAL    :: MOZOLD                   !Monin-Obukhov stability parameter from prior iteration
    REAL    :: TMP1,TMP2,TMP3,TMP4,TMP5 !temporary calculation
    REAL    :: TVIR                     !temporary virtual temperature (k)
    REAL    :: MOZ2                     !2/L
    REAL    :: TMPCM2                   !temporary calculation for CM2
    REAL    :: TMPCH2                   !temporary calculation for CH2
    REAL    :: FM2NEW                   !stability correction factor, momentum, for current moz
    REAL    :: FH2NEW                   !stability correction factor, sen heat, for current moz
    REAL    :: TMP12,TMP22,TMP32        !temporary calculation

    REAL    :: CMFM, CHFH, CM2FM2, CH2FH2
! -------------------------------------------------------------------------------------------------
! Monin-Obukhov stability parameter moz for next iteration

    MOZOLD = MOZ
  
    IF(ZLVL <= ZPD) THEN
       write(*,*) 'critical problem: ZLVL <= ZPD; model stops'
       call wrf_error_fatal("STOP in Noah-MP")
    ENDIF

    TMPCM = LOG((ZLVL-ZPD) / Z0M)
    TMPCH = LOG((ZLVL-ZPD) / Z0H)
    TMPCM2 = LOG((2.0 + Z0M) / Z0M)
    TMPCH2 = LOG((2.0 + Z0H) / Z0H)

    IF(ITER == 1) THEN
       FV   = 0.0
       MOZ  = 0.0
       MOL  = 0.0
       MOZ2 = 0.0
    ELSE
       TVIR = (1. + 0.61*QAIR) * SFCTMP
       TMP1 = VKC * (GRAV/TVIR) * H/(RHOAIR*CPAIR)
       IF (ABS(TMP1) .LE. MPE) TMP1 = MPE
       MOL  = -1. * FV**3 / TMP1
       MOZ  = MIN( (ZLVL-ZPD)/MOL, 1.)
       MOZ2  = MIN( (2.0 + Z0H)/MOL, 1.)
    ENDIF

! accumulate number of times moz changes sign.

    IF (MOZOLD*MOZ .LT. 0.) MOZSGN = MOZSGN+1
    IF (MOZSGN .GE. 2) THEN
       MOZ = 0.
       FM = 0.
       FH = 0.
       MOZ2 = 0.
       FM2 = 0.
       FH2 = 0.
    ENDIF

! evaluate stability-dependent variables using moz from prior iteration
    IF (MOZ .LT. 0.) THEN
       TMP1 = (1. - 16.*MOZ)**0.25
       TMP2 = LOG((1.+TMP1*TMP1)/2.)
       TMP3 = LOG((1.+TMP1)/2.)
       FMNEW = 2.*TMP3 + TMP2 - 2.*ATAN(TMP1) + 1.5707963
       FHNEW = 2*TMP2

! 2-meter
       TMP12 = (1. - 16.*MOZ2)**0.25
       TMP22 = LOG((1.+TMP12*TMP12)/2.)
       TMP32 = LOG((1.+TMP12)/2.)
       FM2NEW = 2.*TMP32 + TMP22 - 2.*ATAN(TMP12) + 1.5707963
       FH2NEW = 2*TMP22
    ELSE
       FMNEW = -5.*MOZ
       FHNEW = FMNEW
       FM2NEW = -5.*MOZ2
       FH2NEW = FM2NEW
    ENDIF

! except for first iteration, weight stability factors for previous
! iteration to help avoid flip-flops from one iteration to the next

    IF (ITER == 1) THEN
       FM = FMNEW
       FH = FHNEW
       FM2 = FM2NEW
       FH2 = FH2NEW
    ELSE
       FM = 0.5 * (FM+FMNEW)
       FH = 0.5 * (FH+FHNEW)
       FM2 = 0.5 * (FM2+FM2NEW)
       FH2 = 0.5 * (FH2+FH2NEW)
    ENDIF

! exchange coefficients

    FH = MIN(FH,0.9*TMPCH)
    FM = MIN(FM,0.9*TMPCM)
    FH2 = MIN(FH2,0.9*TMPCH2)
    FM2 = MIN(FM2,0.9*TMPCM2)

    CMFM = TMPCM-FM
    CHFH = TMPCH-FH
    CM2FM2 = TMPCM2-FM2
    CH2FH2 = TMPCH2-FH2
    IF(ABS(CMFM) <= MPE) CMFM = MPE
    IF(ABS(CHFH) <= MPE) CHFH = MPE
    IF(ABS(CM2FM2) <= MPE) CM2FM2 = MPE
    IF(ABS(CH2FH2) <= MPE) CH2FH2 = MPE
    CM  = VKC*VKC/(CMFM*CMFM)
    CH  = VKC*VKC/(CMFM*CHFH)
    CH2  = VKC*VKC/(CM2FM2*CH2FH2)
        
! friction velocity

    FV = UR * SQRT(CM)
    CH2  = VKC*FV/CH2FH2

  END SUBROUTINE SFCDIF1

!== begin sfcdif2 ==================================================================================

  SUBROUTINE SFCDIF2(parameters,ITER   ,Z0     ,THZ0   ,THLM   ,SFCSPD , & !in
                     ZLM    ,ILOC   ,JLOC   ,         & !in
                     AKMS   ,AKHS   ,RLMO   ,WSTAR2 ,         & !in
                     USTAR  )                                   !out

! -------------------------------------------------------------------------------------------------
! SUBROUTINE SFCDIF (renamed SFCDIF_off to avoid clash with Eta PBL)
! -------------------------------------------------------------------------------------------------
! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
! SEE CHEN ET AL (1997, BLM)
! -------------------------------------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER, INTENT(IN) :: ILOC
    INTEGER, INTENT(IN) :: JLOC
    INTEGER, INTENT(IN) :: ITER
    REAL,    INTENT(IN) :: ZLM, Z0, THZ0, THLM, SFCSPD
    REAL, intent(INOUT) :: AKMS
    REAL, intent(INOUT) :: AKHS
    REAL, intent(INOUT) :: RLMO
    REAL, intent(INOUT) :: WSTAR2
!   REAL,   intent(OUT) :: USTAR !bug fix niu
    REAL,   intent(INOUT) :: USTAR

    REAL     ZZ, PSLMU, PSLMS, PSLHU, PSLHS
    REAL     XX, PSPMU, YY, PSPMS, PSPHU, PSPHS
    REAL     ZILFC, ZU, ZT, RDZ, CXCH
    REAL     DTHV, DU2, BTGH, ZSLU, ZSLT, RLOGU, RLOGT
    REAL     ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4

    REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN,  &
         &         RLMA

    INTEGER  ILECH, ITR

    INTEGER, PARAMETER :: ITRMX  = 5
    REAL,    PARAMETER :: WWST   = 1.2
    REAL,    PARAMETER :: WWST2  = WWST * WWST
    REAL,    PARAMETER :: VKRM   = 0.40
    REAL,    PARAMETER :: EXCM   = 0.001
    REAL,    PARAMETER :: BETA   = 1.0 / 270.0
    REAL,    PARAMETER :: BTG    = BETA * GRAV
    REAL,    PARAMETER :: ELFC   = VKRM * BTG
    REAL,    PARAMETER :: WOLD   = 0.15
    REAL,    PARAMETER :: WNEW   = 1.0 - WOLD
    REAL,    PARAMETER :: PIHF   = 3.14159265 / 2.
    REAL,    PARAMETER :: EPSU2  = 1.E-4
    REAL,    PARAMETER :: EPSUST = 0.07
    REAL,    PARAMETER :: EPSIT  = 1.E-4
    REAL,    PARAMETER :: EPSA   = 1.E-8
    REAL,    PARAMETER :: ZTMIN  = -5.0
    REAL,    PARAMETER :: ZTMAX  = 1.0
    REAL,    PARAMETER :: HPBL   = 1000.0
    REAL,    PARAMETER :: SQVISC = 258.2
    REAL,    PARAMETER :: RIC    = 0.183
    REAL,    PARAMETER :: RRIC   = 1.0 / RIC
    REAL,    PARAMETER :: FHNEU  = 0.8
    REAL,    PARAMETER :: RFC    = 0.191
    REAL,    PARAMETER :: RFAC   = RIC / ( FHNEU * RFC * RFC )

! ----------------------------------------------------------------------
! NOTE: THE TWO CODE BLOCKS BELOW DEFINE FUNCTIONS
! ----------------------------------------------------------------------
! LECH'S SURFACE FUNCTIONS
    PSLMU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLMS (ZZ)= ZZ * RRIC -2.076* (1. -1./ (ZZ +1.))
    PSLHU (ZZ)= -0.96* log (1.0-4.5* ZZ)
    PSLHS (ZZ)= ZZ * RFAC -2.076* (1. -1./ (ZZ +1.))
! PAULSON'S SURFACE FUNCTIONS
    PSPMU (XX)= -2.* log ( (XX +1.)*0.5) - log ( (XX * XX +1.)*0.5)   &
         &        +2.* ATAN (XX)                                            &
         &- PIHF
    PSPMS (YY)= 5.* YY
    PSPHU (XX)= -2.* log ( (XX * XX +1.)*0.5)
    PSPHS (YY)= 5.* YY

! THIS ROUTINE SFCDIF CAN HANDLE BOTH OVER OPEN WATER (SEA, OCEAN) AND
! OVER SOLID SURFACE (LAND, SEA-ICE).
! ----------------------------------------------------------------------
!     ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
!     C......ZTFC=0.1
!     CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
! ----------------------------------------------------------------------
    ILECH = 0

! ----------------------------------------------------------------------
    ZILFC = - parameters%CZIL * VKRM * SQVISC
    ZU = Z0
    RDZ = 1./ ZLM
    CXCH = EXCM * RDZ
    DTHV = THLM - THZ0

! BELJARS CORRECTION OF USTAR
    DU2 = MAX (SFCSPD * SFCSPD,EPSU2)
    BTGH = BTG * HPBL

    IF(ITER == 1) THEN
        IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
           WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
        ELSE
           WSTAR2 = 0.0
        END IF
        USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)
        RLMO = ELFC * AKHS * DTHV / USTAR **3
    END IF
 
! ZILITINKEVITCH APPROACH FOR ZT
    ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
    ZSLU = ZLM + ZU
    ZSLT = ZLM + ZT
    RLOGU = log (ZSLU / ZU)
    RLOGT = log (ZSLT / ZT)

! ----------------------------------------------------------------------
! 1./MONIN-OBUKKHOV LENGTH-SCALE
! ----------------------------------------------------------------------
    ZETALT = MAX (ZSLT * RLMO,ZTMIN)
    RLMO = ZETALT / ZSLT
    ZETALU = ZSLU * RLMO
    ZETAU = ZU * RLMO
    ZETAT = ZT * RLMO

    IF (ILECH .eq. 0) THEN
       IF (RLMO .lt. 0.)THEN
          XLU4 = 1. -16.* ZETALU
          XLT4 = 1. -16.* ZETALT
          XU4  = 1. -16.* ZETAU
          XT4  = 1. -16.* ZETAT
          XLU  = SQRT (SQRT (XLU4))
          XLT  = SQRT (SQRT (XLT4))
          XU   = SQRT (SQRT (XU4))

          XT = SQRT (SQRT (XT4))
          PSMZ = PSPMU (XU)
          SIMM = PSPMU (XLU) - PSMZ + RLOGU
          PSHZ = PSPHU (XT)
          SIMH = PSPHU (XLT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          ZETAU  = MIN (ZETAU,ZTMAX/(ZSLU/ZU))   ! Barlage: add limit on ZETAU/ZETAT
          ZETAT  = MIN (ZETAT,ZTMAX/(ZSLT/ZT))   ! Barlage: prevent SIMM/SIMH < 0
          PSMZ = PSPMS (ZETAU)
          SIMM = PSPMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSPHS (ZETAT)
          SIMH = PSPHS (ZETALT) - PSHZ + RLOGT
       END IF
! ----------------------------------------------------------------------
! LECH'S FUNCTIONS
! ----------------------------------------------------------------------
    ELSE
       IF (RLMO .lt. 0.)THEN
          PSMZ = PSLMU (ZETAU)
          SIMM = PSLMU (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHU (ZETAT)
          SIMH = PSLHU (ZETALT) - PSHZ + RLOGT
       ELSE
          ZETALU = MIN (ZETALU,ZTMAX)
          ZETALT = MIN (ZETALT,ZTMAX)
          PSMZ = PSLMS (ZETAU)
          SIMM = PSLMS (ZETALU) - PSMZ + RLOGU
          PSHZ = PSLHS (ZETAT)
          SIMH = PSLHS (ZETALT) - PSHZ + RLOGT
       END IF
! ----------------------------------------------------------------------
       END IF

! ----------------------------------------------------------------------
! BELJAARS CORRECTION FOR USTAR
! ----------------------------------------------------------------------
       USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)

! ZILITINKEVITCH FIX FOR ZT
       ZT = MAX(1.E-6,EXP (ZILFC * SQRT (USTAR * Z0))* Z0)
       ZSLT = ZLM + ZT
!-----------------------------------------------------------------------
       RLOGT = log (ZSLT / ZT)
       USTARK = USTAR * VKRM
       IF(SIMM < 1.e-6) SIMM = 1.e-6        ! Limit stability function
       AKMS = MAX (USTARK / SIMM,CXCH)
!-----------------------------------------------------------------------
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
!-----------------------------------------------------------------------
       IF(SIMH < 1.e-6) SIMH = 1.e-6        ! Limit stability function
       AKHS = MAX (USTARK / SIMH,CXCH)

       IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
          WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
       ELSE
          WSTAR2 = 0.0
       END IF
!-----------------------------------------------------------------------
       RLMN = ELFC * AKHS * DTHV / USTAR **3
!-----------------------------------------------------------------------
!     IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110
!-----------------------------------------------------------------------
       RLMA = RLMO * WOLD+ RLMN * WNEW
!-----------------------------------------------------------------------
       RLMO = RLMA

!       write(*,'(a20,10f15.6)')'SFCDIF: RLMO=',RLMO,RLMN,ELFC , AKHS , DTHV , USTAR
!    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE SFCDIF2

!== begin esat =====================================================================================

  SUBROUTINE ESAT(T, ESW, ESI, DESW, DESI)
!---------------------------------------------------------------------------------------------------
! use polynomials to calculate saturation vapor pressure and derivative with
! respect to temperature: over water when t > 0 c and over ice when t <= 0 c
  IMPLICIT NONE
!---------------------------------------------------------------------------------------------------
! in

  REAL, intent(in)  :: T              !temperature

!out

  REAL, intent(out) :: ESW            !saturation vapor pressure over water (pa)
  REAL, intent(out) :: ESI            !saturation vapor pressure over ice (pa)
  REAL, intent(out) :: DESW           !d(esat)/dt over water (pa/K)
  REAL, intent(out) :: DESI           !d(esat)/dt over ice (pa/K)

! local

  REAL :: A0,A1,A2,A3,A4,A5,A6  !coefficients for esat over water
  REAL :: B0,B1,B2,B3,B4,B5,B6  !coefficients for esat over ice
  REAL :: C0,C1,C2,C3,C4,C5,C6  !coefficients for dsat over water
  REAL :: D0,D1,D2,D3,D4,D5,D6  !coefficients for dsat over ice

  PARAMETER (A0=6.107799961    , A1=4.436518521E-01,  &
             A2=1.428945805E-02, A3=2.650648471E-04,  &
             A4=3.031240396E-06, A5=2.034080948E-08,  &
             A6=6.136820929E-11)

  PARAMETER (B0=6.109177956    , B1=5.034698970E-01,  &
             B2=1.886013408E-02, B3=4.176223716E-04,  &
             B4=5.824720280E-06, B5=4.838803174E-08,  &
             B6=1.838826904E-10)

  PARAMETER (C0= 4.438099984E-01, C1=2.857002636E-02,  &
             C2= 7.938054040E-04, C3=1.215215065E-05,  &
             C4= 1.036561403E-07, C5=3.532421810e-10,  &
             C6=-7.090244804E-13)

  PARAMETER (D0=5.030305237E-01, D1=3.773255020E-02,  &
             D2=1.267995369E-03, D3=2.477563108E-05,  &
             D4=3.005693132E-07, D5=2.158542548E-09,  &
             D6=7.131097725E-12)

  ESW  = 100.*(A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+T*A6))))))
  ESI  = 100.*(B0+T*(B1+T*(B2+T*(B3+T*(B4+T*(B5+T*B6))))))
  DESW = 100.*(C0+T*(C1+T*(C2+T*(C3+T*(C4+T*(C5+T*C6))))))
  DESI = 100.*(D0+T*(D1+T*(D2+T*(D3+T*(D4+T*(D5+T*D6))))))

  END SUBROUTINE ESAT

!== begin stomata ==================================================================================

  SUBROUTINE STOMATA (parameters,VEGTYP  ,MPE     ,APAR    ,FOLN    ,ILOC    , JLOC, & !in
                      TV      ,EI      ,EA      ,SFCTMP  ,SFCPRS  , & !in
                      O2      ,CO2     ,IGS     ,BTRAN   ,RB      , & !in
                      RS      ,PSN     )                              !out
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
      INTEGER,INTENT(IN)  :: ILOC   !grid index
      INTEGER,INTENT(IN)  :: JLOC   !grid index
      INTEGER,INTENT(IN)  :: VEGTYP !vegetation physiology type

      REAL, INTENT(IN)    :: IGS    !growing season index (0=off, 1=on)
      REAL, INTENT(IN)    :: MPE    !prevents division by zero errors

      REAL, INTENT(IN)    :: TV     !foliage temperature (k)
      REAL, INTENT(IN)    :: EI     !vapor pressure inside leaf (sat vapor press at tv) (pa)
      REAL, INTENT(IN)    :: EA     !vapor pressure of canopy air (pa)
      REAL, INTENT(IN)    :: APAR   !par absorbed per unit lai (w/m2)
      REAL, INTENT(IN)    :: O2     !atmospheric o2 concentration (pa)
      REAL, INTENT(IN)    :: CO2    !atmospheric co2 concentration (pa)
      REAL, INTENT(IN)    :: SFCPRS !air pressure at reference height (pa)
      REAL, INTENT(IN)    :: SFCTMP !air temperature at reference height (k)
      REAL, INTENT(IN)    :: BTRAN  !soil water transpiration factor (0 to 1)
      REAL, INTENT(IN)    :: FOLN   !foliage nitrogen concentration (%)
      REAL, INTENT(IN)    :: RB     !boundary layer resistance (s/m)

! output
      REAL, INTENT(OUT)   :: RS     !leaf stomatal resistance (s/m)
      REAL, INTENT(OUT)   :: PSN    !foliage photosynthesis (umol co2 /m2/ s) [always +]

! in&out
      REAL                :: RLB    !boundary layer resistance (s m2 / umol)
! ---------------------------------------------------------------------------------------------

! ------------------------ local variables ----------------------------------------------------
      INTEGER :: ITER     !iteration index
      INTEGER :: NITER    !number of iterations

      DATA NITER /3/
      SAVE NITER

      REAL :: AB          !used in statement functions
      REAL :: BC          !used in statement functions
      REAL :: F1          !generic temperature response (statement function)
      REAL :: F2          !generic temperature inhibition (statement function)
      REAL :: TC          !foliage temperature (degree Celsius)
      REAL :: CS          !co2 concentration at leaf surface (pa)
      REAL :: KC          !co2 Michaelis-Menten constant (pa)
      REAL :: KO          !o2 Michaelis-Menten constant (pa)
      REAL :: A,B,C,Q     !intermediate calculations for RS
      REAL :: R1,R2       !roots for RS
      REAL :: FNF         !foliage nitrogen adjustment factor (0 to 1)
      REAL :: PPF         !absorb photosynthetic photon flux (umol photons/m2/s)
      REAL :: WC          !Rubisco limited photosynthesis (umol co2/m2/s)
      REAL :: WJ          !light limited photosynthesis (umol co2/m2/s)
      REAL :: WE          !export limited photosynthesis (umol co2/m2/s)
      REAL :: CP          !co2 compensation point (pa)
      REAL :: CI          !internal co2 (pa)
      REAL :: AWC         !intermediate calculation for wc
      REAL :: VCMX        !maximum rate of carbonylation (umol co2/m2/s)
      REAL :: J           !electron transport (umol co2/m2/s)
      REAL :: CEA         !constrain ea or else model blows up
      REAL :: CF          !s m2/umol -> s/m
      REAL :: C3PSNT      !as function of temperature

      F1(AB,BC) = AB**((BC-25.)/10.)
      F2(AB) = 1. + EXP((-2.2E05+710.*(AB+273.16))/(8.314*(AB+273.16)))
      REAL :: T
! ---------------------------------------------------------------------------------------------

! initialize RS=RSMAX and PSN=0 because will only do calculations
! for APAR > 0, in which case RS <= RSMAX and PSN >= 0

         CF = SFCPRS/(8.314*SFCTMP)*1.e06
         RS = 1./parameters%BP * CF
         PSN = 0.

         IF (APAR .LE. 0.) RETURN

         FNF = MIN( FOLN/MAX(MPE,parameters%FOLNMX), 1.0 )
         TC  = TV-TFRZ
         PPF = 4.6*APAR
         J   = PPF*parameters%QE25
         KC  = parameters%KC25 * F1(parameters%AKC,TC)
         KO  = parameters%KO25 * F1(parameters%AKO,TC)
         AWC = KC * (1.+O2/KO)
         CP  = 0.5*KC/KO*O2*0.21

         VCMX = parameters%VCMX25 / F2(TC) * FNF * BTRAN * F1(parameters%AVCMX,TC)

         C3PSNT = 1.0
         IF(TC > 36.) THEN
           C3PSNT = 1.0 - MAX(0.,(TC-36.)/25.)
         END IF

! first guess ci

         CI = 0.7*CO2*C3PSNT + 0.4*CO2*(1.-C3PSNT)

! rb: s/m -> s m**2 / umol

         RLB = RB/CF

! constrain ea

         CEA = MAX(0.25*EI*C3PSNT+0.40*EI*(1.-C3PSNT), MIN(EA,EI) )

! ci iteration
!jref: C3PSN is equal to 1 for all veg types.
       DO ITER = 1, NITER
            WJ = MAX(CI-CP,0.)*J/(CI+2.*CP)*C3PSNT + J*(1.-C3PSNT)
            WC = MAX(CI-CP,0.)*VCMX/(CI+AWC)*C3PSNT + VCMX*(1.-C3PSNT)
            WE = 0.5*VCMX*C3PSNT + 4000.*VCMX*CI/SFCPRS*(1.-C3PSNT)
            PSN = MIN(WJ,WC,WE) * IGS

            CS = MAX( CO2-1.37*RLB*SFCPRS*PSN, MPE )
            A = parameters%MP*PSN*SFCPRS*CEA / (CS*EI) + parameters%BP
            B = ( parameters%MP*PSN*SFCPRS/CS + parameters%BP ) * RLB - 1.
            C = -RLB
            IF (B .GE. 0.) THEN
               Q = -0.5*( B + SQRT(B*B-4.*A*C) )
            ELSE
               Q = -0.5*( B - SQRT(B*B-4.*A*C) )
            END IF
            R1 = Q/A
            R2 = C/Q
            RS = MAX(R1,R2)
            CI = MAX( CS-PSN*SFCPRS*1.65*RS, 0. )
       END DO 

       IF(isnan(PSN)) THEN
         write(*,*) 'in STOMATA: iloc,jloc',iloc,jloc
         write(*,*) 'WJ,WC,WE,IGS',WJ,WC,WE,IGS
         write(*,*) 'TC=',TC
         write(*,*) 'CI=',CI
         write(*,*) 'CP=',CP
         write(*,*) ' J=', J
         write(*,*) 'APAR =', APAR
       END IF

! rs, rb:  s m**2 / umol -> s/m

         RS = RS*CF

  END SUBROUTINE STOMATA

!== begin canres ===================================================================================

  SUBROUTINE CANRES (parameters,PAR   ,SFCTMP,RCSOIL ,EAH   ,SFCPRS , & !in
                     RC    ,PSN   ,ILOC   ,JLOC  )           !out

! --------------------------------------------------------------------------------------------------
! calculate canopy resistance which depends on incoming solar radiation,
! air temperature, atmospheric water vapor pressure deficit at the
! lowest model level, and soil moisture (preferably unfrozen soil
! moisture rather than total)
! --------------------------------------------------------------------------------------------------
! source:  Jarvis (1976), Noilhan and Planton (1989, MWR), Jacquemin and
! Noilhan (1990, BLM). Chen et al (1996, JGR, Vol 101(D3), 7251-7268), 
! eqns 12-14 and table 2 of sec. 3.1.2
! --------------------------------------------------------------------------------------------------
!niu    USE module_Noahlsm_utility
! --------------------------------------------------------------------------------------------------
    IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                  INTENT(IN)  :: ILOC   !grid index
    INTEGER,                  INTENT(IN)  :: JLOC   !grid index
    REAL,                     INTENT(IN)  :: PAR    !par absorbed per unit sunlit lai (w/m2)
    REAL,                     INTENT(IN)  :: SFCTMP !canopy air temperature
    REAL,                     INTENT(IN)  :: SFCPRS !surface pressure (pa)
    REAL,                     INTENT(IN)  :: EAH    !water vapor pressure (pa)
    REAL,                     INTENT(IN)  :: RCSOIL !soil moisture stress factor

!outputs

    REAL,                     INTENT(OUT) :: RC     !canopy resistance per unit LAI
    REAL,                     INTENT(OUT) :: PSN    !foliage photosynthesis (umolco2/m2/s)

!local

    REAL                                  :: RCQ
    REAL                                  :: RCS
    REAL                                  :: RCT
    REAL                                  :: FF
    REAL                                  :: Q2     !water vapor mixing ratio (kg/kg)
    REAL                                  :: Q2SAT  !saturation Q2
    REAL                                  :: DQSDT2 !d(Q2SAT)/d(T)

! RSMIN, RSMAX, TOPT, RGL, HS are canopy stress parameters set in REDPRM
! ----------------------------------------------------------------------
! initialize canopy resistance multiplier terms.
! ----------------------------------------------------------------------
    RC     = 0.0
    RCS    = 0.0
    RCT    = 0.0
    RCQ    = 0.0

!  compute Q2 and Q2SAT

    Q2 = 0.622 *  EAH  / (SFCPRS - 0.378 * EAH) !specific humidity [kg/kg]
    Q2 = Q2 / (1.0 + Q2)                        !mixing ratio [kg/kg]

    CALL CALHUM(parameters,SFCTMP, SFCPRS, Q2SAT, DQSDT2)

! contribution due to incoming solar radiation

    FF  = 2.0 * PAR / parameters%RGL                
    RCS = (FF + parameters%RSMIN / parameters%RSMAX) / (1.0+ FF)
    RCS = MAX (RCS,0.0001)

! contribution due to air temperature

    RCT = 1.0- 0.0016* ( (parameters%TOPT - SFCTMP)**2.0)
    RCT = MAX (RCT,0.0001)

! contribution due to vapor pressure deficit

    RCQ = 1.0/ (1.0+ parameters%HS * MAX(0.,Q2SAT-Q2))
    RCQ = MAX (RCQ,0.01)

! determine canopy resistance due to all factors

    RC  = parameters%RSMIN / (RCS * RCT * RCQ * RCSOIL)
    PSN = -999.99       ! PSN not applied for dynamic carbon

  END SUBROUTINE CANRES

!== begin calhum ===================================================================================

        SUBROUTINE CALHUM(parameters,SFCTMP, SFCPRS, Q2SAT, DQSDT2)

        IMPLICIT NONE

  type (noahmp_parameters), intent(in) :: parameters
        REAL, INTENT(IN)       :: SFCTMP, SFCPRS
        REAL, INTENT(OUT)      :: Q2SAT, DQSDT2
        REAL, PARAMETER        :: A2=17.67,A3=273.15,A4=29.65, ELWV=2.501E6,         &
                                  A23M4=A2*(A3-A4), E0=0.611, RV=461.0,             &
                                  EPSILON=0.622
        REAL                   :: ES, SFCPRSX

! Q2SAT: saturated mixing ratio
        ES = E0 * EXP ( ELWV/RV*(1./A3 - 1./SFCTMP) )
! convert SFCPRS from Pa to KPa
        SFCPRSX = SFCPRS*1.E-3
        Q2SAT = EPSILON * ES / (SFCPRSX-ES)
! convert from  g/g to g/kg
        Q2SAT = Q2SAT * 1.E3
! Q2SAT is currently a 'mixing ratio'

! DQSDT2 is calculated assuming Q2SAT is a specific humidity
        DQSDT2=(Q2SAT/(1+Q2SAT))*A23M4/(SFCTMP-A4)**2

! DG Q2SAT needs to be in g/g when returned for SFLX
        Q2SAT = Q2SAT / 1.E3

        END SUBROUTINE CALHUM

!== begin tsnosoi ==================================================================================

  SUBROUTINE TSNOSOI (parameters,ICE     ,NSOIL   ,NSNOW   ,ISNOW   ,IST     , & !in
                      TBOT    ,ZSNSO   ,SSOIL   ,DF      ,HCPCT   , & !in
                      SAG     ,DT      ,SNOWH   ,DZSNSO  ,PHI     , & !in
                      TG      ,ILOC    ,JLOC    ,                   & !in
                      STC     )                                       !inout
! --------------------------------------------------------------------------------------------------
! Compute snow (up to 3L) and soil (4L) temperature. Note that snow temperatures
! during melting season may exceed melting point (TFRZ) but later in PHASECHANGE
! subroutine the snow temperatures are reset to TFRZ for melting snow.
! --------------------------------------------------------------------------------------------------
  IMPLICIT NONE
! --------------------------------------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN)  :: ILOC
    INTEGER,                         INTENT(IN)  :: JLOC
    INTEGER,                         INTENT(IN)  :: ICE    !
    INTEGER,                         INTENT(IN)  :: NSOIL  !no of soil layers (4)
    INTEGER,                         INTENT(IN)  :: NSNOW  !maximum no of snow layers (3)
    INTEGER,                         INTENT(IN)  :: ISNOW  !actual no of snow layers
    INTEGER,                         INTENT(IN)  :: IST    !surface type

    REAL,                            INTENT(IN)  :: DT     !time step (s)
    REAL,                            INTENT(IN)  :: TBOT   !
    REAL,                            INTENT(IN)  :: SSOIL  !ground heat flux (w/m2)
    REAL,                            INTENT(IN)  :: SAG    !solar rad. absorbed by ground (w/m2)
    REAL,                            INTENT(IN)  :: SNOWH  !snow depth (m)
    REAL,                            INTENT(IN)  :: TG     !ground temperature (k)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  !layer-bot. depth from snow surf.(m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DZSNSO !snow/soil layer thickness (m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     !thermal conductivity
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  !heat capacity (J/m3/k)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: PHI   !light through water (w/m2)

!input and output

    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC

!local

    INTEGER                                      :: IZ
    REAL                                         :: ZBOTSNO   !ZBOT from snow surface
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: AI, BI, CI, RHSTS
    REAL                                         :: EFLXB !energy influx from soil bottom (w/m2)

    REAL, DIMENSION(-NSNOW+1:NSOIL) :: TBEG
    REAL                            :: ERR_EST !heat storage error  (w/m2)
    REAL                            :: SSOIL2  !ground heat flux (w/m2) (for energy check)
    REAL                            :: EFLXB2  !heat flux from the bottom (w/m2) (for energy check)
    character(len=256)              :: message
! ----------------------------------------------------------------------

! adjust ZBOT from soil surface to ZBOTSNO from snow surface

    ZBOTSNO = parameters%ZBOT - SNOWH    !from snow surface

! snow/soil heat storage for energy balance check

    DO IZ = ISNOW+1, NSOIL
       TBEG(IZ) = STC(IZ)
    ENDDO

! compute soil temperatures

      CALL HRT   (parameters,NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOTSNO   ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  EFLXB     )

      CALL HSTEP (parameters,NSNOW     ,NSOIL     ,ISNOW     ,DT        , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  STC       ) 

! update ground heat flux just for energy check, but not for final output
! otherwise, it would break the surface energy balance

    IF(OPT_TBOT == 1) THEN
       EFLXB2  = 0.
    ELSE IF(OPT_TBOT == 2) THEN
       EFLXB2  = DF(NSOIL)*(TBOT-STC(NSOIL)) / &
            (0.5*(ZSNSO(NSOIL-1)+ZSNSO(NSOIL)) - ZBOTSNO)
    END IF

    ! Skip the energy balance check for now, until we can make it work
    ! right for small time steps.
    return

! energy balance check

    ERR_EST = 0.0
    DO IZ = ISNOW+1, NSOIL
       ERR_EST = ERR_EST + (STC(IZ)-TBEG(IZ)) * DZSNSO(IZ) * HCPCT(IZ) / DT
    ENDDO

    if (OPT_STC == 1 .OR. OPT_STC == 3) THEN   ! semi-implicit
       ERR_EST = ERR_EST - (SSOIL +EFLXB)
    ELSE                     ! full-implicit
       SSOIL2 = DF(ISNOW+1)*(TG-STC(ISNOW+1))/(0.5*DZSNSO(ISNOW+1))   !M. Barlage
       ERR_EST = ERR_EST - (SSOIL2+EFLXB2)
    ENDIF

    IF (ABS(ERR_EST) > 1.) THEN    ! W/m2
       WRITE(message,*) 'TSNOSOI is losing(-)/gaining(+) false energy',ERR_EST,' W/m2'
       call wrf_message(trim(message))
       WRITE(message,'(i6,1x,i6,1x,i3,F18.13,5F20.12)') &
            ILOC, JLOC, IST,ERR_EST,SSOIL,SNOWH,TG,STC(ISNOW+1),EFLXB
       call wrf_message(trim(message))
       call wrf_error_fatal("STOP in Noah-MP")
       !niu      STOP
    END IF

  END SUBROUTINE TSNOSOI

!== begin hrt ======================================================================================

  SUBROUTINE HRT (parameters,NSNOW     ,NSOIL     ,ISNOW     ,ZSNSO     , &
                  STC       ,TBOT      ,ZBOT      ,DT        , &
                  DF        ,HCPCT     ,SSOIL     ,PHI       , &
                  AI        ,BI        ,CI        ,RHSTS     , &
                  BOTFLX    )
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! calculate the right hand side of the time tendency term of the soil
! thermal diffusion equation.  also to compute ( prepare ) the matrix
! coefficients for the tri-diagonal matrix of the implicit time scheme.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN)  :: NSOIL  !no of soil layers (4)
    INTEGER,                         INTENT(IN)  :: NSNOW  !maximum no of snow layers (3)
    INTEGER,                         INTENT(IN)  :: ISNOW  !actual no of snow layers
    REAL,                            INTENT(IN)  :: TBOT   !bottom soil temp. at ZBOT (k)
    REAL,                            INTENT(IN)  :: ZBOT   !depth of lower boundary condition (m)
                                                           !from soil surface not snow surface
    REAL,                            INTENT(IN)  :: DT     !time step (s)
    REAL,                            INTENT(IN)  :: SSOIL  !ground heat flux (w/m2)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: ZSNSO  !depth of layer-bottom of snow/soil (m)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: STC    !snow/soil temperature (k)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: DF     !thermal conductivity [w/m/k]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: HCPCT  !heat capacity [j/m3/k]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)  :: PHI    !light through water (w/m2)

! output

    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: RHSTS  !right-hand side of the matrix
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: AI     !left-hand side coefficient
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: BI     !left-hand side coefficient
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: CI     !left-hand side coefficient
    REAL,                            INTENT(OUT) :: BOTFLX !energy influx from soil bottom (w/m2)

! local

    INTEGER                                      :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DENOM
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: DTSDZ
    REAL, DIMENSION(-NSNOW+1:NSOIL)              :: EFLUX
    REAL                                         :: TEMP1
! ----------------------------------------------------------------------

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           DENOM(K)  = - ZSNSO(K) * HCPCT(K)
           TEMP1     = - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = DF(K) * DTSDZ(K) - SSOIL - PHI(K)
        ELSE IF (K < NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     = ZSNSO(K-1) - ZSNSO(K+1)
           DDZ(K)    = 2.0 / TEMP1
           DTSDZ(K)  = 2.0 * (STC(K) - STC(K+1)) / TEMP1
           EFLUX(K)  = (DF(K)*DTSDZ(K) - DF(K-1)*DTSDZ(K-1)) - PHI(K)
        ELSE IF (K == NSOIL) THEN
           DENOM(K)  = (ZSNSO(K-1) - ZSNSO(K)) * HCPCT(K)
           TEMP1     =  ZSNSO(K-1) - ZSNSO(K)
           IF(OPT_TBOT == 1) THEN
               BOTFLX     = 0. 
           END IF
           IF(OPT_TBOT == 2) THEN
               DTSDZ(K)  = (STC(K) - TBOT) / ( 0.5*(ZSNSO(K-1)+ZSNSO(K)) - ZBOT)
               BOTFLX    = -DF(K) * DTSDZ(K)
           END IF
           EFLUX(K)  = (-BOTFLX - DF(K-1)*DTSDZ(K-1) ) - PHI(K)
        END IF
    END DO

    DO K = ISNOW+1, NSOIL
        IF (K == ISNOW+1) THEN
           AI(K)    =   0.0
           CI(K)    = - DF(K)   * DDZ(K) / DENOM(K)
           IF (OPT_STC == 1 .OR. OPT_STC == 3 ) THEN
              BI(K) = - CI(K)
           END IF                                        
           IF (OPT_STC == 2) THEN
              BI(K) = - CI(K) + DF(K)/(0.5*ZSNSO(K)*ZSNSO(K)*HCPCT(K))
           END IF
        ELSE IF (K < NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = - DF(K  ) * DDZ(K  ) / DENOM(K) 
           BI(K)    = - (AI(K) + CI (K))
        ELSE IF (K == NSOIL) THEN
           AI(K)    = - DF(K-1) * DDZ(K-1) / DENOM(K) 
           CI(K)    = 0.0
           BI(K)    = - (AI(K) + CI(K))
        END IF
           RHSTS(K)  = EFLUX(K)/ (-DENOM(K))
    END DO

  END SUBROUTINE HRT

!== begin hstep ====================================================================================

  SUBROUTINE HSTEP (parameters,NSNOW     ,NSOIL     ,ISNOW     ,DT        ,  &
                    AI        ,BI        ,CI        ,RHSTS     ,  &
                    STC       )  
! ----------------------------------------------------------------------
! CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
! ----------------------------------------------------------------------
    implicit none
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN)    :: NSOIL
    INTEGER,                         INTENT(IN)    :: NSNOW
    INTEGER,                         INTENT(IN)    :: ISNOW
    REAL,                            INTENT(IN)    :: DT

! output & input
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: RHSTS
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC

! local
    INTEGER                                        :: K
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: RHSTSIN
    REAL, DIMENSION(-NSNOW+1:NSOIL)                :: CIIN
! ----------------------------------------------------------------------

    DO K = ISNOW+1,NSOIL
       RHSTS(K) =   RHSTS(K) * DT
       AI(K)    =      AI(K) * DT
       BI(K)    = 1. + BI(K) * DT
       CI(K)    =      CI(K) * DT
    END DO

! copy values for input variables before call to rosr12

    DO K = ISNOW+1,NSOIL
       RHSTSIN(K) = RHSTS(K)
       CIIN(K)    = CI(K)
    END DO

! solve the tri-diagonal matrix equation

    CALL ROSR12 (CI,AI,BI,CIIN,RHSTSIN,RHSTS,ISNOW+1,NSOIL,NSNOW)

! update snow & soil temperature

    DO K = ISNOW+1,NSOIL
       STC (K) = STC (K) + CI (K)
    END DO

  END SUBROUTINE HSTEP

!== begin rosr12 ===================================================================================

  SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NTOP,NSOIL,NSNOW)
! ----------------------------------------------------------------------
! SUBROUTINE ROSR12
! ----------------------------------------------------------------------
! INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
! ###                                            ### ###  ###   ###  ###
! #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
! # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
! # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
! # .                                          .   # #  .   # = #   .  #
! # .                                          .   # #  .   #   #   .  #
! # .                                          .   # #  .   #   #   .  #
! # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
! # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
! # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
! ###                                            ### ###  ###   ###  ###
! ----------------------------------------------------------------------
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: NTOP           
    INTEGER, INTENT(IN)   :: NSOIL,NSNOW
    INTEGER               :: K, KK

    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(IN):: A, B, D
    REAL, DIMENSION(-NSNOW+1:NSOIL),INTENT(INOUT):: C,P,DELTA

! ----------------------------------------------------------------------
! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
! ----------------------------------------------------------------------
    C (NSOIL) = 0.0
    P (NTOP) = - C (NTOP) / B (NTOP)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR THE 1ST SOIL LAYER
! ----------------------------------------------------------------------
    DELTA (NTOP) = D (NTOP) / B (NTOP)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
    DO K = NTOP+1,NSOIL
       P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
       DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&
            * P (K -1)))
    END DO
! ----------------------------------------------------------------------
! SET P TO DELTA FOR LOWEST SOIL LAYER
! ----------------------------------------------------------------------
    P (NSOIL) = DELTA (NSOIL)
! ----------------------------------------------------------------------
! ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
    DO K = NTOP+1,NSOIL
       KK = NSOIL - K + (NTOP-1) + 1
       P (KK) = P (KK) * P (KK +1) + DELTA (KK)
    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE ROSR12

!== begin phasechange ==============================================================================

  SUBROUTINE PHASECHANGE (parameters,NSNOW   ,NSOIL   ,ISNOW   ,DT      ,FACT    , & !in
                          DZSNSO  ,HCPCT   ,IST     ,ILOC    ,JLOC    , & !in
                          STC     ,SNICE   ,SNLIQ   ,SNEQV   ,SNOWH   , & !inout
                          SMC     ,SH2O    ,SICE    ,DMICE   ,          & !inout
                          QMELT   ,IMELT   ,PONDING ,XM      )            !out
! ----------------------------------------------------------------------
! melting/freezing of snow water and soil water
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! inputs

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER, INTENT(IN)                             :: ILOC   !grid index
  INTEGER, INTENT(IN)                             :: JLOC   !grid index
  INTEGER, INTENT(IN)                             :: NSNOW  !maximum no. of snow layers [=3]
  INTEGER, INTENT(IN)                             :: NSOIL  !No. of soil layers [=4]
  INTEGER, INTENT(IN)                             :: ISNOW  !actual no. of snow layers [<=3]
  INTEGER, INTENT(IN)                             :: IST    !surface type: 1->soil; 2->lake
  REAL, INTENT(IN)                                :: DT     !land model time step (sec)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: FACT   !temporary
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)     :: HCPCT  !heat capacity (J/m3/k)

! outputs
  INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: IMELT  !phase change index
  REAL,    DIMENSION(-NSNOW+1:NSOIL), INTENT(OUT) :: XM     !melting or freezing water [kg/m2]
  REAL,                               INTENT(OUT) :: QMELT  !snowmelt rate [mm/s]
  REAL,                               INTENT(OUT) :: PONDING!snowmelt when snow has no layer [mm]

! inputs and outputs

  REAL, INTENT(INOUT) :: SNEQV
  REAL, INTENT(INOUT) :: SNOWH
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT)  :: STC    !snow/soil layer temperature [k]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SH2O   !soil liquid water [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SMC    !total soil water [m3/m3]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT)  :: SNLIQ  !snow layer liquid water [mm]

! local

  INTEGER                         :: J         !do loop index
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: HM        !energy residual [w/m2]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WMASS0
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WICE0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: WLIQ0 
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MICE      !soil/snow ice mass [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: MLIQ      !soil/snow liquid water mass [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL) :: SUPERCOOL !supercooled water in soil (kg/m2)
  REAL                            :: HEATR     !energy residual or loss after melting/freezing
  REAL                            :: TEMP1     !temporary variables [kg/m2]
  REAL                            :: PROPOR
  REAL                            :: SMP       !frozen water potential (mm)
  REAL                            :: XMF       !total latent heat of phase change
  REAL                            :: BETA

! mixed-RE

  REAL, DIMENSION(       1:NSOIL)  :: VGM
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: DMICE  !soil solid water change(m/s)
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT)  :: SICE   !soil ice [m3/m3]
  REAL, DIMENSION(       1:NSOIL)                 :: SICEO  !soil ice [m3/m3]
  REAL, DIMENSION(-NSNOW+1:NSOIL)                 :: MICE_OLD      !soil/snow ice mass [mm]

! ----------------------------------------------------------------------
! Initialization

    QMELT   = 0.
    PONDING = 0.
    XMF     = 0.
    DMICE(1:NSOIL)= 0.

    DO J = -NSNOW+1, NSOIL
         SUPERCOOL(J) = 0.0
    END DO

    DO J = ISNOW+1,0       ! all layers
         MICE(J) = SNICE(J)
         MLIQ(J) = SNLIQ(J)
    END DO

    DO J = 1, NSOIL               ! soil
         MLIQ(J) =  SH2O(J)            * DZSNSO(J) * 1000.
         MICE(J) =  SICE(J)            * DZSNSO(J) * 1000.
        !MICE(J) = (SMC(J) - SH2O(J))  * DZSNSO(J) * 1000.  !mixed-RE
    END DO

    DO J = ISNOW+1,NSOIL       ! all layers
         IMELT(J)    = 0
         HM(J)       = 0.
         XM(J)       = 0.
         WICE0(J)    = MICE(J)
         WLIQ0(J)    = MLIQ(J)
         WMASS0(J)   = MICE(J) + MLIQ(J)

         MICE_OLD(J) = MICE(J)  !mixed-RE
    ENDDO

    if(ist == 1) then
      DO J = 1,NSOIL
         IF (OPT_FRZ == 1) THEN
            IF(STC(J) < TFRZ) THEN
               SMP = HFUS*(TFRZ-STC(J))/(GRAV*STC(J))             !(m)

               IF(OPT_WATRET == 1) THEN
                  VGM(J)       = 1.-1./parameters%VGN(J)
                  BETA         = (SMP/parameters%VGPSAT(J))**parameters%VGN(J)
                 !SUPERCOOL(J) = (parameters%SMCR(J)+0.001) + (parameters%SMCMAX(J)-parameters%SMCR(J))*(1+BETA)**(-VGM(J))
                  SUPERCOOL(J) = (parameters%SMCR(J)+0.001) + &
                                  parameters%SMCMAX(J)*(SMP/parameters%PSISAT(J))**(-1./parameters%BEXP(J))
                      !IF(ILOC == 137 .and. JLOC == 7) THEN
                      !write(*,*) 'SUPERCOOL(J)=',SUPERCOOL(J)
                      !write(*,*) 'parameters%SMCR(J)=',parameters%SMCR(J)
                      !write(*,*) 'EPORE=',EPORE(J)
                      !write(*,*) 'BETA=',BETA
                      !ENDIF

               END IF

               IF(OPT_WATRET == 2) THEN
                  SUPERCOOL(J) = MAX(0.001,parameters%SMCMAX(J)*(SMP/parameters%PSISAT(J))**(-1./parameters%BEXP(J)))
               END IF

               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.        !(mm)
            END IF
         END IF
         IF (OPT_FRZ == 2) THEN
               CALL FRH2O (parameters,J,SUPERCOOL(J),STC(J),SMC(J),SH2O(J))
               SUPERCOOL(J) = SUPERCOOL(J)*DZSNSO(J)*1000.        !(mm)
         END IF
      ENDDO
    end if

    DO J = ISNOW+1,NSOIL
         IF (MICE(J) > 0. .AND. STC(J) >= TFRZ) THEN  !melting 
             IMELT(J) = 1
         ENDIF
         IF (MLIQ(J) > SUPERCOOL(J) .AND. STC(J) < TFRZ) THEN
             IMELT(J) = 2
         ENDIF

         ! If snow exists, but its thickness is not enough to create a layer
         IF (ISNOW == 0 .AND. SNEQV > 0. .AND. J == 1) THEN
             IF (STC(J) >= TFRZ) THEN
                IMELT(J) = 1
             ENDIF
         ENDIF
    ENDDO

! Calculate the energy surplus and loss for melting and freezing

    DO J = ISNOW+1,NSOIL
         IF (IMELT(J) > 0) THEN
             HM(J) = (STC(J)-TFRZ)/FACT(J)
             STC(J) = TFRZ
         ENDIF

         IF (IMELT(J) == 1 .AND. HM(J) < 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         IF (IMELT(J) == 2 .AND. HM(J) > 0.) THEN
            HM(J) = 0.
            IMELT(J) = 0
         ENDIF
         XM(J) = HM(J)*DT/HFUS                           
    ENDDO

! The rate of melting and freezing for snow without a layer, needs more work.

    IF (ISNOW == 0 .AND. SNEQV > 0. .AND. XM(1) > 0.) THEN  
        TEMP1  = SNEQV
        SNEQV  = MAX(0.,TEMP1-XM(1))  
        PROPOR = SNEQV/TEMP1
        SNOWH  = MAX(0.,PROPOR * SNOWH)
        HEATR  = HM(1) - HFUS*(TEMP1-SNEQV)/DT  
        IF (HEATR > 0.) THEN
              XM(1) = HEATR*DT/HFUS             
              HM(1) = HEATR                    
        ELSE
              XM(1) = 0.
              HM(1) = 0.
        ENDIF
        QMELT   = MAX(0.,(TEMP1-SNEQV))/DT
        XMF     = HFUS*QMELT
        PONDING = TEMP1-SNEQV
    ENDIF

! The rate of melting and freezing for snow and soil

    DO J = ISNOW+1,NSOIL
      IF (IMELT(J) > 0 .AND. ABS(HM(J)) > 0.) THEN

         HEATR = 0.
         IF (XM(J) > 0.) THEN                            
            MICE(J) = MAX(0., WICE0(J)-XM(J))
            HEATR = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ELSE IF (XM(J) < 0.) THEN                      
            IF (J <= 0) THEN                             ! snow
               MICE(J) = MIN(WMASS0(J), WICE0(J)-XM(J))  
            ELSE                                         ! soil
              !IF (WMASS0(J) < SUPERCOOL(J)) THEN
              !   MICE(J) = 0.
              !ELSE
              !   MICE(J) = MIN(WMASS0(J) - SUPERCOOL(J),WICE0(J)-XM(J))
              !   MICE(J) = MAX(MICE(J),0.0)
              !ENDIF

               IF (WLIQ0(J) < SUPERCOOL(J)) THEN
                  MICE(J) = 0.
               ELSE
                  XM(J)   = MAX(XM(J),-MAX(0.,WLIQ0(J)-SUPERCOOL(J)))
                  MICE(J) = WICE0(J)-XM(J)
               ENDIF
            ENDIF
            HEATR = HM(J) - HFUS*(WICE0(J)-MICE(J))/DT
         ENDIF

         MLIQ(J) = MAX(0.,WMASS0(J)-MICE(J))

         IF (ABS(HEATR) > 0.) THEN
            STC(J) = STC(J) + FACT(J)*HEATR
            IF (J <= 0) THEN                             ! snow
               IF (MLIQ(J)*MICE(J)>0.) STC(J) = TFRZ
            END IF
         ENDIF

         XMF = XMF + HFUS * (WICE0(J)-MICE(J))/DT

         IF (J < 1) THEN
            QMELT = QMELT + MAX(0.,(WICE0(J)-MICE(J)))/DT
         ENDIF
      ENDIF
    ENDDO

    DO J = ISNOW+1,0             ! snow
       SNLIQ(J) = MLIQ(J)
       SNICE(J) = MICE(J)
    END DO

!   DO J = 1, NSOIL              ! soil
!      SH2O(J) =  MLIQ(J)            / (1000. * DZSNSO(J))
!      SMC(J)  = (MLIQ(J) + MICE(J)) / (1000. * DZSNSO(J))
!   END DO

!source/sink term in mixed-form

    IF (OPT_RUN == 6) THEN
       DO J = 1, NSOIL              ! soil
         MICE(J)    = MAX(0.,MICE(J))
         SICEO(J)   = SICE(J)
         SICE(J)    = MICE(J)               / (1000. * DZSNSO(J))
         DMICE(J)   = (MICE(J)-MICE_OLD(J)) /  1000. / DT  !m/s
       END DO
    ELSE
       DO J = 1, NSOIL              ! soil
         MICE(J) = MAX(0.,MICE(J))
         SH2O(J) = MLIQ(J)             / (1000. * DZSNSO(J))
         SICE(J) = MICE(J)             / (1000. * DZSNSO(J))
       END DO
    ENDIF

   ! IF(ILOC == 137 .and. JLOC == 7) THEN
   !  write(*,*) '----------in PHASECHAGE-------------------------'
   !  write(*,*) "(MICE_OLD(IZ),IZ=1,NSOIL)",(MICE_OLD(J),J=1,NSOIL)
   !  write(*,*) "(MICE    (IZ),IZ=1,NSOIL)",(MICE    (J),J=1,NSOIL)
   !  write(*,*) "(DMICE   (IZ),IZ=1,NSOIL)",(DMICE   (J),J=1,NSOIL)
   ! END IF
   
  END SUBROUTINE PHASECHANGE

!== begin frh2o ====================================================================================

  SUBROUTINE FRH2O (parameters,ISOIL,FREE,TKELV,SMC,SH2O)

! ----------------------------------------------------------------------
! SUBROUTINE FRH2O
! ----------------------------------------------------------------------
! CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF
! TEMPERATURE IS BELOW 273.15K (TFRZ).  REQUIRES NEWTON-TYPE ITERATION
! TO SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL
! (1999, JGR, VOL 104(D16), 19569-19585).
! ----------------------------------------------------------------------
! NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON
! ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN
! 4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT
! 1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH
! REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,
! KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE
! LIMIT OF FREEZING POINT TEMPERATURE TFRZ.
! ----------------------------------------------------------------------
! INPUT:

!   TKELV.........TEMPERATURE (Kelvin)
!   SMC...........TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC)
!   SH2O..........LIQUID SOIL MOISTURE CONTENT (VOLUMETRIC)
!   B.............SOIL TYPE "B" PARAMETER (FROM REDPRM)
!   PSISAT........SATURATED SOIL MATRIC POTENTIAL (FROM REDPRM)

! OUTPUT:
!   FREE..........SUPERCOOLED LIQUID WATER CONTENT [m3/m3]
! ----------------------------------------------------------------------
    IMPLICIT NONE
  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,INTENT(IN)   :: ISOIL
    REAL, INTENT(IN)     :: SH2O,SMC,TKELV
    REAL, INTENT(OUT)    :: FREE
    REAL                 :: BX,DENOM,DF,DSWL,FK,SWL,SWLK
    INTEGER              :: NLOG,KCOUNT
!      PARAMETER(CK = 0.0)
    REAL, PARAMETER      :: CK = 8.0, BLIM = 5.5, ERROR = 0.005,       &
         DICE = 920.0
    CHARACTER(LEN=80)    :: message

! ----------------------------------------------------------------------
! LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)
! SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS
! NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.
! ----------------------------------------------------------------------
    BX = parameters%BEXP(ISOIL)
! ----------------------------------------------------------------------
! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
! ----------------------------------------------------------------------

    IF (parameters%BEXP(ISOIL) >  BLIM) BX = BLIM
    NLOG = 0

! ----------------------------------------------------------------------
!  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (TFRZ), SH2O = SMC
! ----------------------------------------------------------------------
    KCOUNT = 0
    IF (TKELV > (TFRZ- 1.E-3)) THEN
       FREE = SMC
    ELSE

! ----------------------------------------------------------------------
! OPTION 1: ITERATED SOLUTION IN KOREN ET AL, JGR, 1999, EQN 17
! ----------------------------------------------------------------------
! INITIAL GUESS FOR SWL (frozen content)
! ----------------------------------------------------------------------
       IF (CK /= 0.0) THEN
          SWL = SMC - SH2O
! ----------------------------------------------------------------------
! KEEP WITHIN BOUNDS.
! ----------------------------------------------------------------------
          IF (SWL > (SMC -0.02)) SWL = SMC -0.02
! ----------------------------------------------------------------------
!  START OF ITERATIONS
! ----------------------------------------------------------------------
          IF (SWL < 0.) SWL = 0.
1001      Continue
          IF (.NOT.( (NLOG < 10) .AND. (KCOUNT == 0)))   goto 1002
          NLOG = NLOG +1
          DF = ALOG ( ( parameters%PSISAT(ISOIL) * GRAV / HFUS ) * ( ( 1. + CK * SWL )**2.) * &
               ( parameters%SMCMAX(ISOIL) / (SMC - SWL) )** BX) - ALOG ( - (               &
               TKELV - TFRZ)/ TKELV)
          DENOM = 2. * CK / ( 1. + CK * SWL ) + BX / ( SMC - SWL )
          SWLK = SWL - DF / DENOM
! ----------------------------------------------------------------------
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
! ----------------------------------------------------------------------
          IF (SWLK > (SMC -0.02)) SWLK = SMC - 0.02
          IF (SWLK < 0.) SWLK = 0.

! ----------------------------------------------------------------------
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
! ----------------------------------------------------------------------
          DSWL = ABS (SWLK - SWL)
! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! ----------------------------------------------------------------------
          SWL = SWLK
          IF ( DSWL <= ERROR ) THEN
             KCOUNT = KCOUNT +1
          END IF
! ----------------------------------------------------------------------
!  END OF ITERATIONS
! ----------------------------------------------------------------------
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
! ----------------------------------------------------------------------
          goto 1001
1002      continue
          FREE = SMC - SWL
       END IF
! ----------------------------------------------------------------------
! END OPTION 1
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
! IN KOREN ET AL., JGR, 1999, EQN 17
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
! ----------------------------------------------------------------------
       IF (KCOUNT == 0) THEN
          write(message, '("Flerchinger used in NEW version. Iterations=", I6)') NLOG
          call wrf_message(trim(message))
          FK = ( ( (HFUS / (GRAV * ( - parameters%PSISAT(ISOIL))))*                    &
               ( (TKELV - TFRZ)/ TKELV))** ( -1/ BX))* parameters%SMCMAX(ISOIL)
          IF (FK < 0.02) FK = 0.02
          FREE = MIN (FK, SMC)
! ----------------------------------------------------------------------
! END OPTION 2
! ----------------------------------------------------------------------
       END IF
    END IF
! ----------------------------------------------------------------------
  END SUBROUTINE FRH2O
! ----------------------------------------------------------------------
! ==================================================================================================
! **********************End of energy subroutines***********************
! ==================================================================================================

!== begin water ====================================================================================

  SUBROUTINE WATER (parameters,rad_cons,VEGTYP ,NSNOW  ,NSOIL  ,IMELT  ,DT     ,UU     , & !in
                    VV     ,FCEV   ,FCTR   ,QPRECC ,QPRECL ,ELAI   , & !in
                    ESAI   ,SFCTMP ,QVAP   ,QDEW   ,ZSOIL  ,BTRANI , & !in
                    FICEOLD,PONDING,TG     ,IST    ,FVEG   ,TOPOSV , & !in
                    ILOC   ,JLOC   ,SMCEQ  ,TS     ,XM     , & !in
                    BDFALL ,FP     ,RAIN   ,SNOW,                    & !in  MB/AN: v3.7
		    QSNOW  ,QRAIN  ,SNOWHIN,LATHEAV,LATHEAG,frozen_canopy,FROZEN_GROUND,    & !in  MB
                    ISNOW  ,CANLIQ ,CANICE ,TV     ,SNOWH  ,SNEQV  , & !inout
                    SNICE  ,SNLIQ  ,STC    ,ZSNSO  ,SH2O   ,SMC    , & !inout
                    SICE   ,ZWT    ,WA     ,WT     ,DZSNSO ,WSLAKE , & !inout
                    SMCWTD ,DEEPRECH,RECH  ,radius                 , & !inout
                    CMC    ,ECAN   ,ETRAN  ,FWET   ,RUNSRF ,RUNSUB , & !out
                    QIN    ,QDIS   ,PONDING1       ,PONDING2,        &
                    QSNBOT ,QSUBCAN,QSUBGRD,                         &
                        VARSD  ,DMICE  , & !Mixed-RE in
                        ATM_BC ,PSI    ,DSH2O  ,ATMACT , & !Mixed-RE inout
                        DTFINEM,SICEO  ,HTOP   ,         & !Mixed-RE inout
                        QDRYC  ,WCND   ,RSINEX , & !Mixed-RE out
                        ROOTMS ,LFMASS ,RTMASS ,STMASS ,WOOD   , &
                        MQ     ,KR     ,QROOT  ,FROOT  ,SADR   )  !out
! ----------------------------------------------------------------------  
! Code history:
! Initial code: Guo-Yue Niu, Oct. 2007
! ----------------------------------------------------------------------
  implicit none
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  type (constants), intent(in) :: rad_cons
  INTEGER,                         INTENT(IN)    :: ILOC    !grid index
  INTEGER,                         INTENT(IN)    :: JLOC    !grid index
  INTEGER,                         INTENT(IN)    :: VEGTYP  !vegetation type
  INTEGER,                         INTENT(IN)    :: NSNOW   !maximum no. of snow layers
  INTEGER                        , INTENT(IN)    :: IST     !surface type 1-soil; 2-lake
  INTEGER,                         INTENT(IN)    :: NSOIL   !no. of soil layers
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT   !melting state index [1-melt; 2-freeze]
  REAL,                            INTENT(IN)    :: DT      !main time step (s)
  REAL,                            INTENT(IN)    :: UU      !u-direction wind speed [m/s]
  REAL,                            INTENT(IN)    :: VV      !v-direction wind speed [m/s]
  REAL,                            INTENT(IN)    :: FCEV    !canopy evaporation (w/m2) [+ to atm ]
  REAL,                            INTENT(IN)    :: FCTR    !transpiration (w/m2) [+ to atm]
  REAL,                            INTENT(IN)    :: QPRECC  !convective precipitation (mm/s)
  REAL,                            INTENT(IN)    :: QPRECL  !large-scale precipitation (mm/s)
  REAL,                            INTENT(IN)    :: ELAI    !leaf area index, after burying by snow
  REAL,                            INTENT(IN)    :: ESAI    !stem area index, after burying by snow
  REAL,                            INTENT(IN)    :: SFCTMP  !surface air temperature [k]
  REAL,                            INTENT(IN)    :: QVAP    !soil surface evaporation rate[mm/s]
  REAL,                            INTENT(IN)    :: QDEW    !soil surface dew rate[mm/s]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL   !depth of layer-bottom from soil surface
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: BTRANI  !soil water stress factor (0 to 1)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD !ice fraction at last timestep
!  REAL                           , INTENT(IN)    :: PONDING ![mm]
  REAL                           , INTENT(IN)    :: TG      !ground temperature (k)
  REAL                           , INTENT(IN)    :: FVEG    !greeness vegetation fraction (-)
  REAL                           , INTENT(IN)    :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7
  REAL                           , INTENT(IN)    :: FP       !fraction of the gridcell that receives precipitation ! MB/AN: v3.7
  REAL                           , INTENT(IN)    :: RAIN     !rainfall (mm/s) ! MB/AN: v3.7
  REAL                           , INTENT(IN)    :: SNOW     !snowfall (mm/s) ! MB/AN: v3.7
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: SMCEQ   !equilibrium soil water content [m3/m3] (used in m-m&f groundwater dynamics)
  REAL                           , INTENT(IN)    :: QSNOW   !snow at ground srf (mm/s) [+]
  REAL                           , INTENT(IN)    :: QRAIN   !rain at ground srf (mm) [+]
  REAL                           , INTENT(IN)    :: SNOWHIN !snow depth increasing rate (m/s)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: XM      !melting or freezing water [kg/m2]
  REAL,                            INTENT(IN)    :: TS      !surface temperature [K] 
  REAL                           , INTENT(IN)    :: TOPOSV !standard dev of DEM [m]

! input/output
  INTEGER,                         INTENT(INOUT) :: ISNOW   !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: CANLIQ  !intercepted liquid water (mm)
  REAL,                            INTENT(INOUT) :: CANICE  !intercepted ice mass (mm)
  REAL,                            INTENT(INOUT) :: TV      !vegetation temperature (k)
  REAL,                            INTENT(INOUT) :: SNOWH   !snow height [m]
  REAL,                            INTENT(INOUT) :: SNEQV   !snow water eqv. [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE   !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ   !snow layer liquid water [mm]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC     !snow/soil layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO   !depth of snow/soil layer-bottom
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO  !snow/soil layer thickness [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O    !soil liquid water content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE    !soil ice content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SMC     !total soil water content [m3/m3]
  REAL,                            INTENT(INOUT) :: ZWT     !the depth to water table [m]
  REAL,                            INTENT(INOUT) :: WA      !water storage in aquifer [mm]
  REAL,                            INTENT(INOUT) :: WT      !water storage in aquifer 
                                                            !+ stuarated soil [mm]
  REAL,                            INTENT(INOUT) :: WSLAKE  !water storage in lake (can be -) (mm)
  REAL                           , INTENT(INOUT) :: PONDING ![mm]
  REAL,                            INTENT(INOUT) :: SMCWTD !soil water content between bottom of the soil and water table [m3/m3]
  REAL,                            INTENT(INOUT) :: DEEPRECH !recharge to or from the water table when deep [m]
  REAL,                            INTENT(INOUT) :: RECH !recharge to or from the water table when shallow [m] (diagnostic)
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: radius !snow grain size [um]

! output
  REAL,                            INTENT(OUT)   :: CMC     !intercepted water per ground area (mm)
  REAL,                            INTENT(OUT)   :: ECAN    !evap of intercepted water (mm/s) [+]
  REAL,                            INTENT(OUT)   :: ETRAN   !transpiration rate (mm/s) [+]
  REAL,                            INTENT(OUT)   :: FWET    !wetted/snowed fraction of canopy (-)
  REAL,                            INTENT(OUT)   :: RUNSRF  !surface runoff [mm/s] 
  REAL,                            INTENT(OUT)   :: RUNSUB  !baseflow (sturation excess) [mm/s]
  REAL,                            INTENT(OUT)   :: QDIS    !groundwater discharge [m/s]
  REAL,                            INTENT(OUT)   :: PONDING1
  REAL,                            INTENT(OUT)   :: PONDING2
  REAL,                            INTENT(OUT)   :: QSNBOT  !melting water out of snow bottom [mm/s]
  REAL,                            INTENT(OUT)   :: QSUBCAN !sublimation/deposition (+/-) from the canopy snow (mm/s)
  REAL,                            INTENT(OUT)   :: QSUBGRD !sublimation/deposition (+/-) from the ground snow (mm/s)
  REAL                              , INTENT(IN)   :: LATHEAV !latent heat vap./sublimation (j/kg)
  REAL                              , INTENT(IN)   :: LATHEAG !latent heat vap./sublimation (j/kg)
  LOGICAL                           , INTENT(IN)   :: FROZEN_GROUND ! used to define latent heat pathway
  LOGICAL                           , INTENT(IN)   :: FROZEN_CANOPY ! used to define latent heat pathway


! local
  INTEGER                                        :: IZ
  REAL                                           :: QSEVA   !soil surface evap rate [m/s]
  REAL                                           :: QINSUR  !water input on soil surface [m/s]
  REAL                                           :: QSDEW   !soil surface dew rate [mm/s]
  REAL                                           :: QSNFRO  !snow surface frost rate[mm/s]
  REAL                                           :: QSNSUB  !snow surface sublimation rate [mm/s]
  REAL, DIMENSION(       1:NSOIL)                :: ETRANI  !transpiration rate (mm/s) [+]
  REAL                                           :: QDRAIN  !soil-bottom free drainage [mm/s] 
  REAL                                           :: SNOFLOW !glacier flow [mm/s]
  REAL                                           :: FCRMAX !maximum of FCR (-)
  REAL                                           :: SWE_BEG,GW_BEG,CW_BEG !niu
  REAL                                           :: SW_BEG,SW_END         !niu

  REAL, PARAMETER ::  WSLMAX = 5000.      !maximum lake water storage (mm)

! Mixed Richards' equation:

  INTEGER ,INTENT(IN)                     :: VARSD  !if variable soil depth is activated see noah_driver
  REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: DMICE   !change rate of solid ice [m/s]

  REAL                    , INTENT(INOUT) :: QIN     !lower BC of RE due to recharge to groundwater(mm/s)
  INTEGER                 , INTENT(INOUT) :: ATM_BC !ATM_BC: 0->Neuman (flux) ;1->Dirichlet (state)
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: PSI    !prognostic pressure head (m)
  REAL                    , INTENT(INOUT) :: ATMACT
  REAL                    , INTENT(INOUT) :: HTOP   !surface ponding hight [mm]
  REAL,                     INTENT(INOUT) :: DTFINEM
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SICEO  !soil ice content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: WCND   !hydraulic conductivity (m/s)
  REAL,                     INTENT(OUT)   :: QDRYC   !dry limit correctoin to EDIR [mm/s]
  REAL                    , INTENT(OUT)   :: RSINEX   !infiltration excess runoff [mm/s]

  REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: ROOTMS            !root fraction
  REAL                    , INTENT(IN)    :: LFMASS            !leaf mass [g/m2]
  REAL                    , INTENT(IN)    :: RTMASS            !mass of fine roots [g/m2]
  REAL                    , INTENT(IN)    :: STMASS            !stem mass [g/m2]
  REAL                    , INTENT(IN)    :: WOOD              !mass of wood (incl. woody roots) [g/m2]
  REAL                    , INTENT(INOUT) :: MQ                !water in plant tissues [kg]
  REAL                    , INTENT(INOUT) :: KR                !water stress factor [-]
  REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: QROOT             !water uptake [m/s]
  REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: FROOT             !root fraction
  REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: SADR              !root surface area density [m2/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: DSH2O             !change rate of liquid soil moisture[m/s]
  REAL :: DRYC

  REAL :: SW_ERR,TRANS,DSH2OS
  REAL :: SWE_ERR

! ----------------------------------------------------------------------
! initialize

   ETRANI(1:NSOIL) = 0.
   SNOFLOW         = 0.
   RUNSUB          = 0.
   QINSUR          = 0.
   QROOT           = 0.
   DSH2O(:)        = 0.

! canopy-intercepted snowfall/rainfall, drips, and throughfall

   !niu water balance: CW_ERR=CW_BEG+(-ECAN)*DT-(CANICE+CANLIQ); CW_BEG = CANICE+ CANLIQ
   CALL CANWATER (parameters,VEGTYP ,DT     , & !in
                  FCEV   ,FCTR   ,ELAI   , & !in
                  ESAI   ,TG     ,FVEG   ,ILOC   , JLOC, & !in
                  BDFALL ,FROZEN_CANOPY  , & !in     
                  CANLIQ ,CANICE ,TV     ,                 & !inout
                  CMC    ,ECAN   ,ETRAN  , & !out
                  FWET   ,QSUBCAN)                           !out

! sublimation, frost, evaporation, and dew

     QSNSUB = 0.
     IF (SNEQV > 0.) THEN
       QSNSUB = MIN(QVAP, SNEQV/DT)
     ENDIF
     QSEVA = QVAP-QSNSUB

!      IF(ILOC == 137 .and. JLOC == 7) THEN
!        write(*,*) 'SNEQV =',SNEQV
!        write(*,*) 'QSNSUB*DT=',QSNSUB*DT
!        write(*,*) 'QVAP*DT=',QVAP*DT
!        write(*,*) 'QDEW*DT=',QDEW*DT
!        write(*,*) 'QSEVA*DT=',QSEVA*DT
!      END IF


     QSNFRO = 0.
     IF (SNEQV > 0.) THEN
        QSNFRO = QDEW
     ENDIF
     QSDEW = QDEW - QSNFRO

     QSUBGRD = QSNSUB + QSNFRO

     SWE_BEG = SNEQV

     !niu water balance: SWE_ERR=SWE_BEG+QSNOW*DT-(QSUBGRD+QSNBOT)*DT - SNEQV; SWE_BEG = SNEQV
     CALL SNOWWATER (parameters,rad_cons,NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & !in
          &          SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & !in
          &          QRAIN  ,FICEOLD,TS     ,XM     ,ILOC   ,JLOC   ,         & !in
          &          ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !inout
          &          SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & !inout
          &          radius ,DSH2O  , & !inout
          &          QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !out

     SWE_ERR=SWE_BEG+QSNOW*DT-(QSUBGRD+QSNBOT)*DT-DSH2O(1)*DT*1000. - SNEQV

    !IF(SWE_ERR>1.0) THEN
    !   write(*,*) 'ILOC,JLOC,SWE_ERR =',ILOC,JLOC,SWE_ERR
    !   write(*,*) 'SWE_BEG =',SWE_BEG
    !   write(*,*) 'SNEQV =',SNEQV
    !   write(*,*) 'QSNOW*DT =',QSNOW*DT
    !   write(*,*) 'QSNSUB*DT=',QSNSUB*DT
    !   write(*,*) 'QSNFRO*DT=',QSNFRO*DT
    !   write(*,*) 'QSNBOT*DT=',QSNBOT*DT
    !   write(*,*) 'DSH2O*DT*1000.=',DSH2O*DT*1000.
    !END IF

   IF(FROZEN_GROUND) THEN
      SICE(1) =  SICE(1) + (QSDEW-QSEVA)*DT/(DZSNSO(1)*1000.)
      QSDEW = 0.0
      QSEVA = 0.0
      IF(SICE(1) < 0.) THEN
         IF (OPT_RUN == 6) THEN !mixed-RE
             DSH2O(1) = DSH2O(1) + SICE(1)*DZSNSO(1)/DT         !m/s
             SICE(1)  = 0.
         ELSE
             SH2O(1) = SH2O(1) + SICE(1)
             SICE(1) = 0.
         END IF
      END IF
   END IF

     !IF(ILOC == 137 .and. JLOC == 7) THEN
     !  write(*,*) 'FROZEN_GROUND=',FROZEN_GROUND
     !  write(*,*) 'SICE(1) =',SICE(1)
     !  write(*,*) 'QVAP*DT=',QVAP*DT
     !  write(*,*) 'QDEW*DT=',QDEW*DT
     !  write(*,*) 'QSEVA*DT=',QSEVA*DT
     !END IF


      !IF(ILOC == 137 .and. JLOC == 7) THEN
      ! write(*,*) "------------after SNOWWATER----------"
      ! write(*,*) "(SICE(IZ),IZ=1,NSOIL)",(SICE(IZ),IZ=1,NSOIL)
      !END IF

! convert units (mm/s -> m/s)

    !PONDING: melting water from snow when there is no layer
    QINSUR = (PONDING+PONDING1+PONDING2)/DT * 0.001

    IF(ISNOW == 0) THEN
       QINSUR = QINSUR+(QSNBOT + QSDEW + QRAIN) * 0.001
    ELSE
       QINSUR = QINSUR+(QSNBOT + QSDEW) * 0.001
    ENDIF

    QSEVA  = QSEVA * 0.001 

    IF (OPT_ROOT == 1) THEN

       DRYC = (LFMASS + RTMASS + STMASS + WOOD*0.02)*1.E-3  !g/m2 -> kg/m2; 2% sapwood can store water

       CALL ROOTWATER(parameters, ILOC   ,JLOC     ,NSOIL  ,NSNOW  , &
                      DT     ,ZSOIL  ,ETRAN  ,PSI  ,STC    ,WCND   , &
                      DRYC   ,VEGTYP ,ROOTMS , &
                      MQ     ,KR     ,SH2O   , &
                      FROOT  ,SADR   ,QROOT  )
    END IF


    DO IZ = 1, parameters%NROOT
       IF(OPT_ROOT == 1) ETRANI(IZ) = QROOT(IZ)
       IF(OPT_ROOT == 2) ETRANI(IZ) = ETRAN * BTRANI(IZ) * 0.001
    ENDDO

     !IF(ILOC == 137 .and. JLOC == 7) THEN
     !  write(*,*) 'LFMASS , RTMASS , STMASS , WOOD=',LFMASS , RTMASS , STMASS , WOOD
     !  write(*,*) "QROOT after ROOTWATER=",QROOT*DT*1000.
     !  write(*,*) 'QINSUR*DT*1000.=',QINSUR*DT*1000.
     !  write(*,*) 'QSNBOT*DT=',QSNBOT*DT
     !  write(*,*) 'QSDEW*DT=',QSDEW*DT
     !  write(*,*) 'QRAIN*DT=',QRAIN*DT
     !  write(*,*) 'QSEVA *DT*1000.=',QSEVA *DT*1000.
     !END IF

! lake/soil water balances

    IF (IST == 2) THEN                                        ! lake
       RUNSRF = 0.
       IF(WSLAKE >= WSLMAX) RUNSRF = QINSUR*1000.             !mm/s
       WSLAKE = WSLAKE + (QINSUR-QSEVA)*1000.*DT -RUNSRF*DT   !mm

       SMC    = 1.0
       IF(VEGTYP == 16) THEN
          SH2O   = 1.0; SICE = 0.0
       END IF

       IF(VEGTYP == 24) THEN
          SICE   = 1.0; SH2O = 0.0
       END IF
    ELSE                                                      ! soil

      !IF(ILOC == 137 .and. JLOC == 7) THEN
      ! write(*,*) "------------1----------"
      ! write(*,*) "(SICE(IZ),IZ=1,NSOIL)",(SICE(IZ),IZ=1,NSOIL)
      ! write(*,*) "(SH2O(IZ),IZ=1,NSOIL)",(SH2O(IZ),IZ=1,NSOIL)
      ! write(*,*) "(DZSNSO(IZ),IZ=1,NSOIL)",(DZSNSO(IZ),IZ=1,NSOIL)
      ! write(*,*) 'SUM(SH2O)     =',SUM(SH2O(1:NSOIL)*1000.*DZSNSO(1:NSOIL))
      ! write(*,*) 'SUM(SMC)      =',SUM(SMC(1:NSOIL)*1000.*DZSNSO(1:NSOIL))
      ! write(*,*) 'HTOP=',HTOP
      !END IF

      !TRANS  = 0.
      !DSH2OS = 0.
      !DO IZ = 1,NSOIL
      !  TRANS  = TRANS  + ETRANI(IZ)
      !  DSH2OS = DSH2OS + DSH2O(IZ)
      !END DO

       SW_BEG = HTOP + SUM(SMC(1:NSOIL)*1000.*DZSNSO(1:NSOIL))

      !water balance: SW_ERR=SW_BEG+(QINSUR-QSEVA)*DT*1000.-RUNSRF*DT-SW_END;
      !SW_BEG = sum of soil water
       CALL  SOILWATER (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                        QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC , & !in
                        SH2O   ,SMC    ,ZWT    ,VEGTYP ,TOPOSV , & !inout
                        SMCWTD, DEEPRECH                       , & !inout
                        RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX , &  !out
                        VARSD  ,DMICE  , & !Mixed-RE in
                        ATM_BC ,PSI    ,DSH2O  ,ATMACT , & !Mixed-RE inout
                        DTFINEM,SICEO  ,HTOP   ,         & !Mixed-RE inout
                        QDIS   ,QDRYC  ,RSINEX )                          !Mixed-RE out

       IF(OPT_RUN == 6) THEN
          RUNSUB       = QDIS*1000. + QDRAIN          !mm/s
       END IF

      !IF(ILOC == 137 .and. JLOC == 7) THEN
      ! write(*,*) "------------2----------"
      ! write(*,*) "(SICE(IZ),IZ=1,NSOIL)",(SICE(IZ),IZ=1,NSOIL)
      ! write(*,*) "(SH2O(IZ),IZ=1,NSOIL)",(SH2O(IZ),IZ=1,NSOIL)
      ! write(*,*) "(DZSNSO(IZ),IZ=1,NSOIL)",(DZSNSO(IZ),IZ=1,NSOIL)
      ! write(*,*) 'SUM(SH2O)     =',SUM(SH2O(1:NSOIL)*1000.*DZSNSO(1:NSOIL))
      ! write(*,*) 'SUM(SMC)     =',SUM(SMC(1:NSOIL)*1000.*DZSNSO(1:NSOIL))
      ! write(*,*) 'HTOP=',HTOP
      !END IF

      !SW_END = HTOP + SUM(SMC(1:NSOIL)*1000.*DZSNSO(1:NSOIL))

      !SW_ERR=SW_BEG+(QINSUR-QSEVA-TRANS+DSH2OS)*DT*1000.-(RUNSRF+RUNSUB-QDRYC)*DT-SW_END
      !IF(abs(SW_ERR) >= 1.0) then
      !   write(*,*) 'in WATER:iloc,jloc,SW_ERR=',iloc,jloc,SW_ERR
      !   write(*,*) 'SW_BEG =',SW_BEG
      !   write(*,*) 'SW_END =',SW_END
      !   write(*,*) 'SW_BEG-SW_END =',SW_BEG-SW_END
      !   write(*,*) 'SH2O   =',SH2O
      !   write(*,*) 'QDRYC*DT    =',QDRYC*DT
      !   write(*,*) 'HTOP    =',HTOP
      !   write(*,*) 'ZWT    =',ZWT
      !   write(*,*) 'QINSUR*DT*1000 =',QINSUR*DT*1000.
      !   write(*,*) 'QSEVA*DT*1000  =',QSEVA*DT*1000.
      !   write(*,*) 'TRANS*DT*1000  =',TRANS*DT*1000.
      !   write(*,*) 'DSH2OS*DT*1000 =',DSH2OS*DT*1000.
      !   write(*,*) 'RUNSRF*DT      =',RUNSRF*DT
      !   write(*,*) 'RUNSUB*DT      =',RUNSUB*DT
      !   write(*,*) '    QDIS*DT    =',QDIS*DT
      !   write(*,*) '    QDRAIN*DT  =',QDRAIN*DT
      !end if

       IF(OPT_RUN == 1) THEN 
       !niu water balance:  GW_ERR=GW_BEG+(QIN-QDIS)*DT-WA; GW_BEG = WA
          CALL GROUNDWATER (parameters,NSNOW  ,NSOIL  ,DT     ,SICE   ,ZSOIL  , & !in
                            STC    ,WCND   ,FCRMAX ,ILOC   ,JLOC   , & !in
                            SH2O   ,ZWT    ,WA     ,WT     ,         & !inout
                            QIN    ,QDIS   )                           !out
          RUNSUB       = QDIS          !mm/s
       END IF

       IF(OPT_RUN == 3 .or. OPT_RUN == 4) THEN 
          RUNSUB       = RUNSUB + QDRAIN        !mm/s
       END IF

       DO IZ = 1,NSOIL
           SMC(IZ) = SH2O(IZ) + SICE(IZ)
       ENDDO
 
       IF(OPT_RUN == 5) THEN
          CALL SHALLOWWATERTABLE (parameters,NSNOW  ,NSOIL, ZSOIL, DT       , & !in
                         DZSNSO ,SMCEQ   ,ILOC , JLOC        , & !in
                         SMC    ,ZWT    ,SMCWTD ,RECH, QDRAIN  ) !inout

          SH2O(NSOIL) = SMC(NSOIL) - SICE(NSOIL)
          RUNSUB = RUNSUB + QDRAIN !it really comes from subroutine watertable, which is not called with the same frequency as the soil routines here
          WA = 0.
       ENDIF

    ENDIF

    RUNSUB       = RUNSUB + SNOFLOW         !mm/s

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) '----------after water-------------------------'
    ! write(*,*) "WA=",WA
    ! write(*,*) "(SH2O(IZ),IZ=1,NSOIL)",(SH2O(IZ),IZ=1,NSOIL)
    ! write(*,*) "(SMC(IZ),IZ=1,NSOIL)",(SMC(IZ),IZ=1,NSOIL)
    ! write(*,*) "(SICE(IZ),IZ=1,NSOIL)",(SICE(IZ),IZ=1,NSOIL)
    !    write(*,*) "----------------------RUNSUB=",RUNSUB
    !END IF

  END SUBROUTINE WATER

!== begin canwater =================================================================================

  SUBROUTINE CANWATER (parameters,VEGTYP ,DT     , & !in
                       FCEV   ,FCTR   ,ELAI   , & !in
                       ESAI   ,TG     ,FVEG   ,ILOC   , JLOC , & !in
                       BDFALL ,FROZEN_CANOPY  ,  & !in      
                       CANLIQ ,CANICE ,TV     ,                 & !inout
                       CMC    ,ECAN   ,ETRAN  , & !out
                       FWET   ,QSUBCAN)                           !out

! ------------------------ code history ------------------------------
! canopy hydrology
! --------------------------------------------------------------------
  IMPLICIT NONE
! ------------------------ input/output variables --------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,INTENT(IN)  :: ILOC    !grid index
  INTEGER,INTENT(IN)  :: JLOC    !grid index
  INTEGER,INTENT(IN)  :: VEGTYP  !vegetation type
  REAL,   INTENT(IN)  :: DT      !main time step (s)
  REAL,   INTENT(IN)  :: FCEV    !canopy evaporation (w/m2) [+ = to atm]
  REAL,   INTENT(IN)  :: FCTR    !transpiration (w/m2) [+ = to atm]
  REAL,   INTENT(IN)  :: ELAI    !leaf area index, after burying by snow
  REAL,   INTENT(IN)  :: ESAI    !stem area index, after burying by snow
  REAL,   INTENT(IN)  :: TG      !ground temperature (k)
  REAL,   INTENT(IN)  :: FVEG    !greeness vegetation fraction (-)
  LOGICAL                           , INTENT(IN)   :: FROZEN_CANOPY ! used to define latent heat pathway
  REAL                           , INTENT(IN)    :: BDFALL   !bulk density of snowfall (kg/m3) ! MB/AN: v3.7

! input & output
  REAL, INTENT(INOUT) :: CANLIQ  !intercepted liquid water (mm)
  REAL, INTENT(INOUT) :: CANICE  !intercepted ice mass (mm)
  REAL, INTENT(INOUT) :: TV      !vegetation temperature (k)

! output
  REAL, INTENT(OUT)   :: CMC     !intercepted water (mm)
  REAL, INTENT(OUT)   :: ECAN    !evaporation of intercepted water (mm/s) [+]
  REAL, INTENT(OUT)   :: ETRAN   !transpiration rate (mm/s) [+]
  REAL, INTENT(OUT)   :: FWET    !wetted or snowed fraction of the canopy (-)
  REAL, INTENT(OUT)   :: QSUBCAN !sublimation/deposition (+/-) from the canopy (mm/s)
! --------------------------------------------------------------------

! ------------------------ local variables ---------------------------
  REAL                :: MAXSNO  !canopy capacity for snow interception (mm)
  REAL                :: MAXLIQ  !canopy capacity for rain interception (mm)
  REAL                :: QEVAC   !evaporation rate (mm/s)
  REAL                :: QDEWC   !dew rate (mm/s)
  REAL                :: QFROC   !frost rate (mm/s)
  REAL                :: QSUBC   !sublimation rate (mm/s)
  REAL                :: QMELTC  !melting rate of canopy snow (mm/s)
  REAL                :: QFRZC   !refreezing rate of canopy liquid water (mm/s)
  REAL                :: CANMAS  !total canopy mass (kg/m2)
! --------------------------------------------------------------------
! initialization

      ECAN    = 0.0

! --------------------------- liquid water ------------------------------
! maximum canopy water

      MAXLIQ =  parameters%CH2OP * (ELAI+ ESAI)

! evaporation, transpiration, and dew

      IF (.NOT.FROZEN_CANOPY) THEN             ! Barlage: change to frozen_canopy
        ETRAN = MAX( FCTR/HVAP, 0. )
        QEVAC = MAX( FCEV/HVAP, 0. )
        QDEWC = ABS( MIN( FCEV/HVAP, 0. ) )
        QSUBC = 0.
        QFROC = 0.
        QSUBCAN = 0.
      ELSE
        ETRAN = MAX( FCTR/HSUB, 0. )
        QEVAC = 0.
        QDEWC = 0.
        QSUBC = MAX( FCEV/HSUB, 0. )
        QFROC = ABS( MIN( FCEV/HSUB, 0. ) )
        QSUBCAN = FCEV/HSUB   !sublimation from the canopy (mm/s)
      ENDIF

! canopy water balance. for convenience allow dew to bring CANLIQ above
! maxh2o or else would have to re-adjust drip

       QEVAC = MIN(CANLIQ/DT,QEVAC)
       CANLIQ=MAX(0.,CANLIQ+(QDEWC-QEVAC)*DT)
       IF(CANLIQ <= 1.E-06) CANLIQ = 0.0

! --------------------------- canopy ice ------------------------------
! for canopy ice

      MAXSNO = 6.6*(0.27+46./BDFALL) * (ELAI+ ESAI)

      QSUBC = MIN(CANICE/DT,QSUBC) 
      CANICE= MAX(0.,CANICE + (QFROC-QSUBC)*DT)
      IF(CANICE.LE.1.E-6) CANICE = 0.

     
! wetted fraction of canopy

      IF(CANICE.GT.0.) THEN
           FWET = MAX(0.,CANICE) / MAX(MAXSNO,1.E-06)
      ELSE
           FWET = MAX(0.,CANLIQ) / MAX(MAXLIQ,1.E-06)
      ENDIF
      FWET = MIN(FWET, 1.) ** 0.667

! phase change

      QMELTC = 0.
      QFRZC = 0.

      IF(CANICE.GT.1.E-6.AND.TV.GT.TFRZ) THEN
         QMELTC = MIN(CANICE/DT,(TV-TFRZ)*CICE*CANICE/DENICE/(DT*HFUS))
         CANICE = MAX(0.,CANICE - QMELTC*DT)
         CANLIQ = MAX(0.,CANLIQ + QMELTC*DT)
         TV     = FWET*TFRZ + (1.-FWET)*TV
      ENDIF

      IF(CANLIQ.GT.1.E-6.AND.TV.LT.TFRZ) THEN
         QFRZC  = MIN(CANLIQ/DT,(TFRZ-TV)*CWAT*CANLIQ/DENH2O/(DT*HFUS))
         CANLIQ = MAX(0.,CANLIQ - QFRZC*DT)
         CANICE = MAX(0.,CANICE + QFRZC*DT)
         TV     = FWET*TFRZ + (1.-FWET)*TV
      ENDIF

! total canopy water

      CMC = CANLIQ + CANICE

! total canopy evaporation

      ECAN = QEVAC + QSUBC - QDEWC - QFROC

  END SUBROUTINE CANWATER

!== begin snowwater ================================================================================

  SUBROUTINE SNOWWATER (parameters,rad_cons,NSNOW  ,NSOIL  ,IMELT  ,DT     ,ZSOIL  , & !in
                        SFCTMP ,SNOWHIN,QSNOW  ,QSNFRO ,QSNSUB , & !in
                        QRAIN  ,FICEOLD,TS     ,XM     ,ILOC   ,JLOC   ,         & !in
                        ISNOW  ,SNOWH  ,SNEQV  ,SNICE  ,SNLIQ  , & !inout
                        SH2O   ,SICE   ,STC    ,ZSNSO  ,DZSNSO , & !inout
                        radius ,DSH2O  , & ! inout
                        QSNBOT ,SNOFLOW,PONDING1       ,PONDING2)  !out
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  type (constants), intent(in) :: rad_cons
  INTEGER,                         INTENT(IN)    :: ILOC   !grid index
  INTEGER,                         INTENT(IN)    :: JLOC   !grid index
  INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers
  INTEGER,                         INTENT(IN)    :: NSOIL  !no. of soil layers
  INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  !melting state index [0-no melt;1-melt]
  REAL,                            INTENT(IN)    :: DT     !time step (s)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !depth of layer-bottom from soil surface
  REAL,                            INTENT(IN)    :: SFCTMP !surface air temperature [k]
  REAL,                            INTENT(IN)    :: SNOWHIN!snow depth increasing rate (m/s)
  REAL,                            INTENT(IN)    :: QSNOW  !snow at ground srf (mm/s) [+]
  REAL,                            INTENT(IN)    :: QSNFRO !snow surface frost rate[mm/s]
  REAL,                            INTENT(INOUT)    :: QSNSUB !snow surface sublimation rate[mm/s]
  REAL,                            INTENT(IN)    :: QRAIN  !snow surface rain rate[mm/s]
  REAL, DIMENSION(-NSNOW+1:0)    , INTENT(IN)    :: FICEOLD!ice fraction at last timestep
  REAL,                            INTENT(IN)    :: TS     !surface temperature [K]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: XM     !melting or freezing water [kg/m2]

! input & output
  INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: SNOWH  !snow height [m]
  REAL,                            INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid moisture (m3/m3)
  REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   !soil ice moisture (m3/m3)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  !depth of snow/soil layer-bottom
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: radius !snow grain size [um]

! output
  REAL,                              INTENT(OUT) :: QSNBOT !melting water out of snow bottom [mm/s]
  REAL,                              INTENT(OUT) :: SNOFLOW!glacier flow [mm]
  REAL,                              INTENT(OUT) :: PONDING1
  REAL,                              INTENT(OUT) :: PONDING2

  REAL   , DIMENSION(       1:NSOIL),INTENT(INOUT) :: DSH2O  !change rate of liquid soil moisture[m/s] mixed-RE

! local
  INTEGER :: IZ,i
  REAL    :: BDSNOW  !bulk density of snow (kg/m3)
! ----------------------------------------------------------------------
   SNOFLOW = 0.0
   PONDING1 = 0.0
   PONDING2 = 0.0

   CALL SNOWFALL (parameters,NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN, & !in
                  SFCTMP ,ILOC   ,JLOC   ,                 & !in
                  ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE  , & !inout
                  SNLIQ  ,SNEQV  ,radius )                   !inout

! MB: do each if block separately

   IF(ISNOW < 0) &        ! when multi-layer

   CALL SNOWAGING (parameters, rad_cons,NSOIL, NSNOW  , ISNOW,  STC,    DT, & !in
                   SNICE     , SNLIQ, DZSNSO , TS   ,   XM, IMELT, & !in
                   radius )

   CALL  COMPACT (parameters,NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & !in
                  SNLIQ  ,ZSOIL  ,IMELT  ,FICEOLD,ILOC   , JLOC ,& !in
                  ISNOW  ,DZSNSO ,ZSNSO  )                   !inout

   IF(ISNOW < 0) &        !when multi-layer
   CALL  COMBINE (parameters,NSNOW  ,NSOIL  ,DT   ,ILOC   ,JLOC   ,         & !in
                  ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
                  DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,radius , DSH2O, & !inout
                  PONDING1       ,PONDING2)                  !out

   IF(ISNOW < 0) &        !when multi-layer
   CALL   DIVIDE (parameters,NSNOW  ,NSOIL  ,                         & !in
                  ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO ,radius )   !inout

   CALL  SNOWH2O (parameters,NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !in 
                  QRAIN  ,ILOC   ,JLOC   ,                 & !in
                  ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & !inout
                  SNLIQ  ,SH2O   ,SICE   ,STC    ,radius , DSH2O,  & !inout
                  QSNBOT ,PONDING1       ,PONDING2)           !out

!set empty snow layers to zero

   do iz = -nsnow+1, isnow
        snice(iz) = 0.
        snliq(iz) = 0.
        stc(iz)   = 0.
        dzsnso(iz)= 0.
        zsnso(iz) = 0.
        radius(iz)= 0.
   enddo

!to obtain equilibrium state of snow in glacier region

   IF(SNEQV > 2000.) THEN   ! 2000 mm -> maximum water depth (not balanced)
      BDSNOW      = SNICE(0) / DZSNSO(0)
      SNOFLOW     = (SNEQV - 2000.)
      SNICE(0)    = SNICE(0)  - SNOFLOW 
      DZSNSO(0)   = DZSNSO(0) - SNOFLOW/BDSNOW
      SNOFLOW     = SNOFLOW / DT
   END IF

! sum up snow mass for layered snow

   IF(ISNOW < 0) THEN  ! MB: only do for multi-layer
       SNEQV = 0.
       DO IZ = ISNOW+1,0
             SNEQV = SNEQV + SNICE(IZ) + SNLIQ(IZ)
       ENDDO
   END IF

! Reset ZSNSO and layer thinkness DZSNSO

   DO IZ = ISNOW+1, 0
        DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

   DZSNSO(1) = ZSOIL(1)
   DO IZ = 2,NSOIL
        DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
   END DO

   ZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)
   DO IZ = ISNOW+2 ,NSOIL
       ZSNSO(IZ) = ZSNSO(IZ-1) + DZSNSO(IZ)
   ENDDO

   DO IZ = ISNOW+1 ,NSOIL
       DZSNSO(IZ) = -DZSNSO(IZ)
   END DO

  END SUBROUTINE SNOWWATER

!==begin snowaging ================================================================================================
   SUBROUTINE SNOWAGING (parameters, rad_cons,NSOIL, NSNOW  , ISNOW,  STC,    DT, & !in
                         SNICE     , SNLIQ, DZSNSO , TS   ,   XM, IMELT, & !in
                         radius )

 ! Methodology from Flanner
 ! original code: Wenli Wang 2020/6/9

  IMPLICIT NONE
 ! ----------------------------------------------------------------------
 ! input
    type (noahmp_parameters), intent(in)           :: parameters
    type (constants), intent(in)           :: rad_cons
    INTEGER,                         INTENT(IN)    :: NSOIL   !  no. of soil layers
    INTEGER,                         INTENT(IN)    :: NSNOW   ! maximum no. of snow layers
    REAL,                            INTENT(IN)    :: DT      ! time step (sec)

    INTEGER,                         INTENT(IN)    :: ISNOW   ! actual no. of snow layers
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: STC     ! snow layer temperature [k]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNICE   ! snow layer ice [mm]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNLIQ   ! snow layer liquid water [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: DZSNSO  ! snow layer depth [m]

    REAL,                            INTENT(IN)    :: TS      ! surface temperature [K]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: XM      ! melting or freezing water kg/m2
    INTEGER, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: IMELT   ! melting or freezing state

! input and output
    REAL, DIMENSION(-NSNOW+1:0)    , INTENT(INOUT) :: radius  ! snow grain radius [um]

! local variables
    REAL, DIMENSION(       1:NSNOW)                :: DZ      ! snow layer thickness [m]
    REAL, DIMENSION(       1:NSNOW)                :: SWICE   ! snow layer ice [mm]
    REAL, DIMENSION(       1:NSNOW)                :: SWLIQ   ! snow layer liquid water [mm]
    REAL, DIMENSION(       1:NSNOW)                :: TSNO    ! node temperature [k]
    REAL, DIMENSION(       1:NSNOW)                :: R       ! snow grain radius [um]
    REAL, DIMENSION(       1:NSNOW)                :: XM2       ! melting or freezing water kg/m2
    REAL, DIMENSION(       1:NSNOW)                :: IMELT2      ! melting or freezing state

    REAL, DIMENSION(       1:NSNOW)                :: DR_dry          ! dry snow grain radius change[um]
    REAL, DIMENSION(       1:NSNOW)                :: DR_wet          ! wet snow grain radius change[um]
    REAL, DIMENSION(       1:NSNOW)                :: DR_rfz          ! refreeze snow grain radius change[um]


    REAL                                           :: fliq

    REAL, DIMENSION(11)                            :: Tsnow_x   ! [Kelvin]
    REAL, DIMENSION(31)                            :: dT2dZ_x   ! [K/m]
    REAL, DIMENSION(8)                             :: sno_dns_x ! [kg/m3]


    REAL                                           :: dTdZ        ! [K/m]
    REAL                                           :: rho_snow    ! [kg/m3]

    REAL                                           :: drdt0_j     ! approximated snow aging parameters
    REAL                                           :: tau_j       ! approximated snow aging parameters
    REAL                                           :: kappa_j     ! approximated snow aging parameters

    REAL                                           :: Tup, Tbm

    INTEGER                                        :: MSNO, J
    INTEGER                                        :: in_t, in_dtdz, in_sdns
    REAL, PARAMETER                                :: r0  =  54.4999 ! [um]
    REAL, PARAMETER                                :: r_rfz  =  1000 ! [um]
    REAL, PARAMETER                                :: pi  =  3.1415926 ! [um]
    REAL                                           :: frfz ! fraction of refreeze water
! executable code

    DO J = 1,NSNOW
      IF (J <= ABS(ISNOW)) THEN
         DZ(J)    = abs(DZSNSO(J+ISNOW))  !J==1 is top layer wenli
         SWICE(J) = SNICE(J+ISNOW)
         SWLIQ(J) = SNLIQ(J+ISNOW)
         TSNO(J)  = STC(J+ISNOW)
         R(J)     = radius(J+ISNOW)    ! Wenli 2020/6/6
         XM2(J)   = abs(XM(J+ISNOW))
         IMELT2(J)= IMELT(J+ISNOW)
      END IF
    END DO

    MSNO = ABS(ISNOW)

    DO J = 1,MSNO
       ! Flanner: The temperature gradient calculation uses the modeled mid-layer temperatures to estimate layer interface temperatures,
       ! and then uses the layer interface temperatures to estimate the mid-layer temperature gradient.
       if (J==1) then
          Tup  = TSNO(J)
          Tbm  = (TSNO(J+1)*DZ(J)+TSNO(J)*DZ(J+1))/(DZ(J)+DZ(J+1))
       end if

!niu      if (J==MSNO) then
       if (MSNO>=2 .AND. J==MSNO) then
          Tup  = (TSNO(J-1)*DZ(J)+TSNO(J)*DZ(J-1))/(DZ(J)+DZ(J-1))
          Tbm  = (STC(1)*DZ(J)+TSNO(J)*DZSNSO(1))/(DZ(J)+DZSNSO(1))
       end if

       if (J>1 .and. J<MSNO) then
          Tup  = (TSNO(J-1)*DZ(J)+TSNO(J)*DZ(J-1))/(DZ(J)+DZ(J-1))
          Tbm  = (TSNO(J+1)*DZ(J)+TSNO(J)*DZ(J+1))/(DZ(J)+DZ(J+1))
       end if

       dTdZ = abs(Tup+Tbm)/DZ(J)

       rho_snow= (SWICE(J)+SWLIQ(J))/DZ(J)

       ! search for the nearest index for snow aging dimension
       Tsnow_x  =abs(rad_cons%Tsnow-TSNO(J))
       dT2dZ_x  =abs(rad_cons%dT2dZ-dTdZ)
       sno_dns_x=abs(rad_cons%sno_dns-rho_snow)

       in_t    = minloc(Tsnow_x,1)
       in_dtdz = minloc(dT2dZ_x,1)
       in_sdns = minloc(sno_dns_x,1)

       drdt0_j = rad_cons%drdt0(in_sdns, in_dtdz, in_t)
       tau_j   = rad_cons%tau  (in_sdns, in_dtdz, in_t)
       kappa_j = rad_cons%kappa(in_sdns, in_dtdz, in_t)

       DR_dry(J)= (drdt0_j*(tau_j/((R(J)-r0)+tau_j))**(1/kappa_j))*DT/3600

       fliq     = min(  0.1, SWLIQ(J)/(SWLIQ(J)+SWICE(J)) )
       DR_wet(J)= 1.e18*( DT*(4.22e-13*(fliq**3))/(4*pi*R(J)**2) )

       R(J)    = R(J) + DR_dry(J) + DR_wet(J)

       ! refreeze water
       if (IMELT2(J)==2) then
          frfz    = XM2(J)/(SWICE(J)+SWLIQ(J))
       else
          frfz    = 0.
       end if

       DR_rfz(J)  = frfz * r_rfz

       R(J)    = R(J)*(1-frfz) + DR_rfz(J)

    ENDDO

    DO J = ISNOW+1,0
       radius(J) = R(J-ISNOW)    ! Wenli
    END DO

  END SUBROUTINE snowaging

!== begin snowfall =================================================================================

  SUBROUTINE SNOWFALL (parameters,NSOIL  ,NSNOW  ,DT     ,QSNOW  ,SNOWHIN , & !in
                       SFCTMP ,ILOC   ,JLOC   ,                  & !in
                       ISNOW  ,SNOWH  ,DZSNSO ,STC    ,SNICE   , & !inout
                       SNLIQ  ,SNEQV  ,radius )                    !inout
! ----------------------------------------------------------------------
! snow depth and density to account for the new snowfall.
! new values of snow depth & density returned.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                            INTENT(IN) :: ILOC   !grid index
  INTEGER,                            INTENT(IN) :: JLOC   !grid index
  INTEGER,                            INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                            INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL,                               INTENT(IN) :: DT     !main time step (s)
  REAL,                               INTENT(IN) :: QSNOW  !snow at ground srf (mm/s) [+]
  REAL,                               INTENT(IN) :: SNOWHIN!snow depth increasing rate (m/s)
  REAL,                               INTENT(IN) :: SFCTMP !surface air temperature [k]

! input and output

  INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
  REAL,                            INTENT(INOUT) :: SNOWH  !snow depth [m]
  REAL,                            INTENT(INOUT) :: SNEQV  !swow water equivalent [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO !thickness of snow/soil layers (m)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE  !snow layer ice [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
  REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: radius

! local

  INTEGER :: NEWNODE            ! 0-no new layers, 1-creating new layers
! ----------------------------------------------------------------------
    NEWNODE  = 0

! shallow snow / no layer
    IF(SNOWH == 0.)  THEN
      radius(-2:0) = 0.
    ELSE
      IF (ISNOW==0) THEN
          radius(0) = 54.5
      END IF
    END IF

! shallow snow / no layer

    IF(ISNOW == 0 .and. QSNOW > 0.)  THEN
      SNOWH = SNOWH + SNOWHIN * DT
      SNEQV = SNEQV + QSNOW * DT
    END IF

! creating a new layer
 
!NIU    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.05) THEN
    IF(ISNOW == 0  .AND. QSNOW>0. .AND. SNOWH >= 0.02) THEN !NIU
      ISNOW    = -1
      NEWNODE  =  1
      DZSNSO(0)= SNOWH
      SNOWH    = 0.
      STC(0)   = MIN(273.16, SFCTMP)   ! temporary setup
      SNICE(0) = SNEQV
      SNLIQ(0) = 0.

      radius(0)= 54.5
    END IF

! snow with layers

    IF(ISNOW <  0 .AND. NEWNODE == 0 .AND. QSNOW > 0.) then
         radius(ISNOW+1) = 54.5 * QSNOW * DT/(SNICE(ISNOW+1)+ QSNOW * DT ) + &
                           radius(ISNOW+1)*SNICE(ISNOW+1)/(SNICE(ISNOW+1)+ QSNOW * DT )
         SNICE(ISNOW+1)  = SNICE(ISNOW+1)   + QSNOW   * DT
         DZSNSO(ISNOW+1) = DZSNSO(ISNOW+1)  + SNOWHIN * DT
    ENDIF

! ----------------------------------------------------------------------
  END SUBROUTINE SNOWFALL

!== begin combine ==================================================================================

  SUBROUTINE COMBINE (parameters,NSNOW  ,NSOIL  ,DT  ,ILOC   ,JLOC   ,         & !in
                      ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
                      DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,radius , DSH2O , & !inout
                      PONDING1       ,PONDING2)                  !out
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER, INTENT(IN)     :: ILOC
    INTEGER, INTENT(IN)     :: JLOC
    INTEGER, INTENT(IN)     :: NSNOW                        !maximum no. of snow layers
    INTEGER, INTENT(IN)     :: NSOIL                        !no. of soil layers

! input and output

    INTEGER,                         INTENT(INOUT) :: ISNOW !actual no. of snow layers
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O  !soil liquid moisture (m3/m3)
    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE  !soil ice moisture (m3/m3)
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   !snow layer temperature [k]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE !snow layer ice [mm]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ !snow layer liquid water [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO!snow layer depth [m]
    REAL,                            INTENT(INOUT) :: sneqv !snow water equivalent [m]
    REAL,                            INTENT(INOUT) :: snowh !snow depth [m]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: radius
    REAL,                            INTENT(OUT) :: PONDING1
    REAL,                            INTENT(OUT) :: PONDING2

    REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: DSH2O  !change rate of liquid soil moisture[m/s] for mixed-RE
    REAL   , INTENT(IN)     :: DT                           !time step [s]

! local variables:

    INTEGER :: I,J,K,L               ! node indices
    INTEGER :: ISNOW_OLD             ! number of top snow layer
    INTEGER :: MSSI                  ! node index
    INTEGER :: NEIBOR                ! adjacent node selected for combination
    REAL    :: ZWICE                 ! total ice mass in snow
    REAL    :: ZWLIQ                 ! total liquid water in snow

    REAL    :: DZMIN(3)              ! minimum of top snow layer
!   DATA DZMIN /0.045, 0.05, 0.2/    ! | niu STC & TG stable
!   DATA DZMIN /0.025, 0.025, 0.1/   ! MB: change limit
    DATA DZMIN /0.02, 0.05, 0.10/    ! niu
!-----------------------------------------------------------------------

       ISNOW_OLD = ISNOW

       DO J = ISNOW_OLD+1,0
          IF (SNICE(J) <= .1) THEN
             IF(J /= 0) THEN
                radius(J+1) = radius(J+1)* SNICE(J+1)/(SNICE(J+1)+SNICE(J)) + &
                              radius(J)*SNICE(J)/(SNICE(J+1)+SNICE(J))
                SNLIQ(J+1)  = SNLIQ(J+1) + SNLIQ(J)
                SNICE(J+1)  = SNICE(J+1) + SNICE(J)
             ELSE
               !SH2O(1) = SH2O(1)+SNLIQ(J)/(DZSNSO(1)*1000.)
                SICE(1) = SICE(1)+SNICE(J)/(DZSNSO(1)*1000.)

                IF (OPT_RUN == 6) THEN !mixed-RE
                  DSH2O(1) = DSH2O(1) + SNLIQ(J)/1000./DT         !m/s
                ELSE
                  SH2O(1)  = SH2O(1)+SNLIQ(J)/(DZSNSO(1)*1000.)
                  DSH2O(1) = 0.
                ENDIF

             ENDIF

             ! shift all elements above this down by one.
             IF (J > ISNOW+1 .AND. ISNOW < -1) THEN
                DO I = J, ISNOW+2, -1
                   radius(I)= radius(I-1)
                   STC(I)   = STC(I-1)
                   SNLIQ(I) = SNLIQ(I-1)
                   SNICE(I) = SNICE(I-1)
                   DZSNSO(I)= DZSNSO(I-1)
                END DO
             END IF
             ISNOW = ISNOW + 1
          END IF
       END DO

! to conserve water in case of too large surface sublimation

       IF(SICE(1) < 0.) THEN
          IF (OPT_RUN == 6) THEN  !mixed-RE
            DSH2O(1) = DSH2O(1) + SICE(1)*DZSNSO(1)/DT !m/s
          ELSE
            SH2O(1)  = SH2O(1) + SICE(1)
            DSH2O(1) = 0.
          ENDIF

          SICE(1) = 0.
       END IF

       SNEQV  = 0.
       SNOWH  = 0.
       ZWICE  = 0.
       ZWLIQ  = 0.

       DO J = ISNOW+1,0
             SNEQV = SNEQV + SNICE(J) + SNLIQ(J)
             SNOWH = SNOWH + DZSNSO(J)
             ZWICE = ZWICE + SNICE(J)
             ZWLIQ = ZWLIQ + SNLIQ(J)
       END DO

! check the snow depth - all snow gone
! the liquid water assumes ponding on soil surface.

!      IF (SNOWH < 0.05 ) THEN
       IF (SNOWH < 0.01 ) THEN !NIU
          ISNOW  = 0
          radius(-2:0) = 0.
          SNEQV = ZWICE
         !SH2O(1) = SH2O(1) + ZWLIQ / (DZSNSO(1) * 1000.)

          IF (OPT_RUN == 6) THEN !mixed-RE
            DSH2O(1) = DSH2O(1) + ZWLIQ/1000./DT       !m/s
          ELSE
            SH2O(1)  = SH2O(1) + ZWLIQ / (DZSNSO(1) * 1000.)
            DSH2O(1) = 0.
          ENDIF

          IF(SNEQV <= 0.) SNOWH = 0.
       END IF

! check the snow depth - snow layers combined

       IF (ISNOW < -1) THEN

          ISNOW_OLD = ISNOW
          MSSI     = 1

          DO I = ISNOW_OLD+1,0
             IF (DZSNSO(I) < DZMIN(MSSI)) THEN

                IF (I == ISNOW+1) THEN
                   NEIBOR = I + 1
                ELSE IF (I == 0) THEN
                   NEIBOR = I - 1
                ELSE
                   NEIBOR = I + 1
                   IF ((DZSNSO(I-1)+DZSNSO(I)) < (DZSNSO(I+1)+DZSNSO(I))) NEIBOR = I-1
                END IF

                ! Node l and j are combined and stored as node j.
                IF (NEIBOR > I) THEN
                   J = NEIBOR
                   L = I
                ELSE
                   J = I
                   L = NEIBOR
                END IF

                CALL COMBO (parameters,DZSNSO(J), SNLIQ(J), SNICE(J), STC(J), radius(J), &
                    DZSNSO(L), SNLIQ(L), SNICE(L), STC(L), radius(L) )

                ! Now shift all elements above this down one.
                IF (J-1 > ISNOW+1) THEN
                   DO K = J-1, ISNOW+2, -1
                      radius(K) = radius(K-1)
                      STC(K)   = STC(K-1)
                      SNICE(K) = SNICE(K-1)
                      SNLIQ(K) = SNLIQ(K-1)
                      DZSNSO(K) = DZSNSO(K-1)
                   END DO
                END IF

                ! Decrease the number of snow layers
                ISNOW = ISNOW + 1
                IF (ISNOW >= -1) EXIT
             ELSE

                ! The layer thickness is greater than the prescribed minimum value
                MSSI = MSSI + 1

             END IF

          END DO

       END IF

  END SUBROUTINE COMBINE

!== begin divide ===================================================================================

  SUBROUTINE DIVIDE (parameters,NSNOW  ,NSOIL  ,                         & !in
                     ISNOW  ,STC    ,SNICE  ,SNLIQ  ,DZSNSO ,radius  )  !inout
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER, INTENT(IN)                            :: NSNOW !maximum no. of snow layers [ =3]
    INTEGER, INTENT(IN)                            :: NSOIL !no. of soil layers [ =4]

! input and output

    INTEGER                        , INTENT(INOUT) :: ISNOW !actual no. of snow layers 
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC   !snow layer temperature [k]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNICE !snow layer ice [mm]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: SNLIQ !snow layer liquid water [mm]
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO!snow layer depth [m]
    REAL, DIMENSION(-NSNOW+1:    0), INTENT(INOUT) :: radius

! local variables:

    INTEGER                                        :: J     !indices
    INTEGER                                        :: MSNO  !number of layer (top) to MSNO (bot)
    REAL                                           :: DRR   !thickness of the combined [m]
    REAL, DIMENSION(       1:NSNOW)                :: DZ    !snow layer thickness [m]
    REAL, DIMENSION(       1:NSNOW)                :: SWICE !partial volume of ice [m3/m3]
    REAL, DIMENSION(       1:NSNOW)                :: SWLIQ !partial volume of liquid water [m3/m3]
    REAL, DIMENSION(       1:NSNOW)                :: TSNO  !node temperature [k]
    REAL, DIMENSION(       1:NSNOW)                :: R     !node radius [um]
    REAL                                           :: ZWICE !temporary
    REAL                                           :: ZWLIQ !temporary
    REAL                                           :: PROPOR!temporary
    REAL                                           :: DTDZ  !temporary

!   REAL, parameter    :: DZ1st = 0.05        !0.05
!   REAL, parameter    :: DZ2nd = 0.20        !0.2
    REAL, parameter    :: DZ1st = 0.02        !0.05  !NIU
    REAL, parameter    :: DZ2nd = 0.05        !0.2   !NIU
! ----------------------------------------------------------------------

    DO J = 1,NSNOW
          IF (J <= ABS(ISNOW)) THEN
             DZ(J)    = DZSNSO(J+ISNOW)
             SWICE(J) = SNICE(J+ISNOW)
             SWLIQ(J) = SNLIQ(J+ISNOW)
             TSNO(J)  = STC(J+ISNOW)
             R(J)     = radius(J+ISNOW) !Wenli 2020/6/6
          END IF
    END DO

       MSNO = ABS(ISNOW)

       IF (MSNO == 1) THEN
          ! Specify a new snow layer
          IF (DZ(1) > DZ1st) THEN
             MSNO = 2
             DZ(1)    = DZ(1)/2.
             SWICE(1) = SWICE(1)/2.
             SWLIQ(1) = SWLIQ(1)/2.
             R(1)     = R(1)

             DZ(2)    = DZ(1)
             SWICE(2) = SWICE(1)
             SWLIQ(2) = SWLIQ(1)
             TSNO(2)  = TSNO(1)
             R(2)     = R(1)
          END IF
       END IF

       IF (MSNO > 1) THEN
          IF (DZ(1) > DZ1st) THEN
             DRR      = DZ(1) - DZ1st
             PROPOR   = DRR/DZ(1)
             ZWICE    = PROPOR*SWICE(1)
             ZWLIQ    = PROPOR*SWLIQ(1)
             PROPOR   = DZ1st/DZ(1)
             SWICE(1) = PROPOR*SWICE(1)
             SWLIQ(1) = PROPOR*SWLIQ(1)
             DZ(1)    = DZ1st
             R(1)     = R(1)

             CALL COMBO (parameters,DZ(2), SWLIQ(2), SWICE(2), TSNO(2), R(2), &
                 DRR, ZWLIQ, ZWICE, TSNO(1), R(1))

             ! subdivide a new layer
             IF (MSNO <= 2 .AND. DZ(2) > DZ2nd) THEN  ! MB: change limit
!             IF (MSNO <= 2 .AND. DZ(2) > 0.10) THEN
                MSNO = 3
                DTDZ = (TSNO(1) - TSNO(2))/((DZ(1)+DZ(2))/2.)
                DZ(2)    = DZ(2)/2.
                SWICE(2) = SWICE(2)/2.
                SWLIQ(2) = SWLIQ(2)/2.
                R(2)     = R(2)

                DZ(3)    = DZ(2)
                SWICE(3) = SWICE(2)
                SWLIQ(3) = SWLIQ(2)
                TSNO(3) = TSNO(2) - DTDZ*DZ(2)/2.
                IF (TSNO(3) >= TFRZ) THEN
                   TSNO(3)  = TSNO(2)
                ELSE
                   TSNO(2) = TSNO(2) + DTDZ*DZ(2)/2.
                ENDIF

                R(3)    = R(2)
             END IF
          END IF
       END IF

       IF (MSNO > 2) THEN
          IF (DZ(2) > DZ2nd) THEN
             DRR      = DZ(2) - DZ2nd
             PROPOR   = DRR/DZ(2)
             ZWICE    = PROPOR*SWICE(2)
             ZWLIQ    = PROPOR*SWLIQ(2)
             PROPOR   = DZ2nd/DZ(2)
             SWICE(2) = PROPOR*SWICE(2)
             SWLIQ(2) = PROPOR*SWLIQ(2)
             DZ(2)    = DZ2nd
             CALL COMBO (parameters,DZ(3), SWLIQ(3), SWICE(3), TSNO(3), R(3), &
                 DRR,  ZWLIQ, ZWICE, TSNO(2), R(2))
          END IF
       END IF

       ISNOW = -MSNO

    DO J = ISNOW+1,0
             DZSNSO(J) = DZ(J-ISNOW)
             SNICE(J) = SWICE(J-ISNOW)
             SNLIQ(J) = SWLIQ(J-ISNOW)
             STC(J)   = TSNO(J-ISNOW)
             radius(J)= R(J-ISNOW) ! Wenli
    END DO

!    DO J = ISNOW+1,NSOIL
!    WRITE(*,'(I5,7F10.3)') J, DZSNSO(J), SNICE(J), SNLIQ(J),STC(J)
!    END DO

  END SUBROUTINE DIVIDE

!== begin combo ====================================================================================

  SUBROUTINE COMBO(parameters,DZ,  WLIQ,  WICE, T, R, DZ2, WLIQ2, WICE2, T2, R2)
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------s
! input

  type (noahmp_parameters), intent(in) :: parameters
    REAL, INTENT(IN)    :: DZ2   !nodal thickness of 2 elements being combined [m]
    REAL, INTENT(IN)    :: WLIQ2 !liquid water of element 2 [kg/m2]
    REAL, INTENT(IN)    :: WICE2 !ice of element 2 [kg/m2]
    REAL, INTENT(IN)    :: T2    !nodal temperature of element 2 [k]
    REAL, INTENT(IN)    :: R2    !nodal snow grain size of element 2 [um]

    REAL, INTENT(INOUT) :: DZ    !nodal thickness of 1 elements being combined [m]
    REAL, INTENT(INOUT) :: WLIQ  !liquid water of element 1
    REAL, INTENT(INOUT) :: WICE  !ice of element 1 [kg/m2]
    REAL, INTENT(INOUT) :: T     !node temperature of element 1 [k]
    REAL, INTENT(INOUT) :: R    !nodal snow grain size of element 1 [um]

! local 

    REAL                :: DZC   !total thickness of nodes 1 and 2 (DZC=DZ+DZ2).
    REAL                :: WLIQC !combined liquid water [kg/m2]
    REAL                :: WICEC !combined ice [kg/m2]
    REAL                :: TC    !combined node temperature [k]
    REAL                :: H     !enthalpy of element 1 [J/m2]
    REAL                :: H2    !enthalpy of element 2 [J/m2]
    REAL                :: HC    !temporary

!-----------------------------------------------------------------------

    DZC = DZ+DZ2
    WICEC = (WICE+WICE2)
    WLIQC = (WLIQ+WLIQ2)
    H = (CICE*WICE+CWAT*WLIQ) * (T-TFRZ)+HFUS*WLIQ
    H2= (CICE*WICE2+CWAT*WLIQ2) * (T2-TFRZ)+HFUS*WLIQ2

    HC = H + H2
    IF(HC < 0.)THEN
       TC = TFRZ + HC/(CICE*WICEC + CWAT*WLIQC)
    ELSE IF (HC.LE.HFUS*WLIQC) THEN
       TC = TFRZ
    ELSE
       TC = TFRZ + (HC - HFUS*WLIQC) / (CICE*WICEC + CWAT*WLIQC)
    END IF

    ! snow grain radius
    R=R*WICE/(WICE+WICE2)+R2*WICE2/(WICE+WICE2)

    DZ = DZC
    WICE = WICEC
    WLIQ = WLIQC
    T = TC

  END SUBROUTINE COMBO

!== begin compact ==================================================================================

  SUBROUTINE COMPACT (parameters,NSNOW  ,NSOIL  ,DT     ,STC    ,SNICE  , & !in
                      SNLIQ  ,ZSOIL  ,IMELT  ,FICEOLD,ILOC   , JLOC , & !in
                      ISNOW  ,DZSNSO ,ZSNSO )                    !inout
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
   INTEGER,                         INTENT(IN)    :: ILOC   !grid index
   INTEGER,                         INTENT(IN)    :: JLOC   !grid index
   INTEGER,                         INTENT(IN)    :: NSOIL  !no. of soil layers [ =4]
   INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers [ =3]
   INTEGER, DIMENSION(-NSNOW+1:0) , INTENT(IN)    :: IMELT  !melting state index [0-no melt;1-melt]
   REAL,                            INTENT(IN)    :: DT     !time step (sec)
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: STC    !snow layer temperature [k]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNICE  !snow layer ice [mm]
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: SNLIQ  !snow layer liquid water [mm]
   REAL, DIMENSION(       1:NSOIL), INTENT(IN)    :: ZSOIL  !depth of layer-bottom from soil srf
   REAL, DIMENSION(-NSNOW+1:    0), INTENT(IN)    :: FICEOLD!ice fraction at last timestep

! input and output
   INTEGER,                         INTENT(INOUT) :: ISNOW  ! actual no. of snow layers
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO ! snow layer thickness [m]
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: ZSNSO  ! depth of snow/soil layer-bottom

! local
   REAL, PARAMETER     :: C2 = 21.e-3   ![m3/kg] ! default 21.e-3
   REAL, PARAMETER     :: C3 = 2.5e-6   ![1/s]  
   REAL, PARAMETER     :: C4 = 0.04     ![1/k]
   REAL, PARAMETER     :: C5 = 2.0      !
   REAL, PARAMETER     :: DM = 100.0    !upper Limit on destructive metamorphism compaction [kg/m3]
   REAL, PARAMETER     :: ETA0 = 0.8e+6 !viscosity coefficient [kg-s/m2] 
                                        !according to Anderson, it is between 0.52e6~1.38e6
   REAL :: BURDEN !pressure of overlying snow [kg/m2]
   REAL :: DDZ1   !rate of settling of snow pack due to destructive metamorphism.
   REAL :: DDZ2   !rate of compaction of snow pack due to overburden.
   REAL :: DDZ3   !rate of compaction of snow pack due to melt [1/s]
   REAL :: DEXPF  !EXPF=exp(-c4*(273.15-STC)).
   REAL :: TD     !STC - TFRZ [K]
   REAL :: PDZDTC !nodal rate of change in fractional-thickness due to compaction [fraction/s]
   REAL :: VOID   !void (1 - SNICE - SNLIQ)
   REAL :: WX     !water mass (ice + liquid) [kg/m2]
   REAL :: BI     !partial density of ice [kg/m3]
   REAL, DIMENSION(-NSNOW+1:0) :: FICE   !fraction of ice at current time step

   INTEGER  :: J

! ----------------------------------------------------------------------
    BURDEN = 0.0

    DO J = ISNOW+1, 0

        WX      = SNICE(J) + SNLIQ(J)
        FICE(J) = SNICE(J) / WX
        VOID    = 1. - (SNICE(J)/DENICE + SNLIQ(J)/DENH2O) / DZSNSO(J)

        ! Allow compaction only for non-saturated node and higher ice lens node.
        IF (VOID > 0.001 .AND. SNICE(J) > 0.1) THEN
           BI = SNICE(J) / DZSNSO(J)
           TD = MAX(0.,TFRZ-STC(J))
           DEXPF = EXP(-C4*TD)

           ! Settling as a result of destructive metamorphism

           DDZ1 = -C3*DEXPF

           IF (BI > DM) DDZ1 = DDZ1*EXP(-46.0E-3*(BI-DM))

           ! Liquid water term

           IF (SNLIQ(J) > 0.01*DZSNSO(J)) DDZ1=DDZ1*C5

           ! Compaction due to overburden

           DDZ2 = -(BURDEN+0.5*WX)*EXP(-0.08*TD-C2*BI)/ETA0 ! 0.5*WX -> self-burden

           ! Compaction occurring during melt

           IF (IMELT(J) == 1) THEN
              DDZ3 = MAX(0.,(FICEOLD(J) - FICE(J))/MAX(1.E-6,FICEOLD(J)))
              DDZ3 = - DDZ3/DT           ! sometimes too large
           ELSE
              DDZ3 = 0.
           END IF

           ! Time rate of fractional change in DZ (units of s-1)

           PDZDTC = (DDZ1 + DDZ2 + DDZ3)*DT
           PDZDTC = MAX(-0.5,PDZDTC)

           ! The change in DZ due to compaction

           DZSNSO(J) = DZSNSO(J)*(1.+PDZDTC)
        END IF

        ! Pressure of overlying snow

        BURDEN = BURDEN + WX

    END DO

  END SUBROUTINE COMPACT

!== begin snowh2o ==================================================================================

  SUBROUTINE SNOWH2O (parameters,NSNOW  ,NSOIL  ,DT     ,QSNFRO ,QSNSUB , & !in 
                      QRAIN  ,ILOC   ,JLOC   ,                 & !in
                      ISNOW  ,DZSNSO ,SNOWH  ,SNEQV  ,SNICE  , & !inout
                      SNLIQ  ,SH2O   ,SICE   ,STC    ,radius , DSH2O , & !inout
                      QSNBOT ,PONDING1       ,PONDING2)          !out
! ----------------------------------------------------------------------
! Renew the mass of ice lens (SNICE) and liquid (SNLIQ) of the
! surface snow layer resulting from sublimation (frost) / evaporation (dew)
! ----------------------------------------------------------------------
   IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
   INTEGER,                         INTENT(IN)    :: ILOC   !grid index
   INTEGER,                         INTENT(IN)    :: JLOC   !grid index
   INTEGER,                         INTENT(IN)    :: NSNOW  !maximum no. of snow layers[=3]
   INTEGER,                         INTENT(IN)    :: NSOIL  !No. of soil layers[=4]
   REAL,                            INTENT(IN)    :: DT     !time step
   REAL,                            INTENT(IN)    :: QSNFRO !snow surface frost rate[mm/s]
   REAL,                            INTENT(IN)    :: QSNSUB !snow surface sublimation rate[mm/s]
   REAL,                            INTENT(IN)    :: QRAIN  !snow surface rain rate[mm/s]

! output

   REAL,                            INTENT(OUT)   :: QSNBOT !melting water out of snow bottom [mm/s]

! input and output

   INTEGER,                         INTENT(INOUT) :: ISNOW  !actual no. of snow layers
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: DZSNSO ! snow layer depth [m]
   REAL,                            INTENT(INOUT) :: SNOWH  !snow height [m]
   REAL,                            INTENT(INOUT) :: SNEQV  !snow water eqv. [mm]
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNICE  !snow layer ice [mm]
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: SNLIQ  !snow layer liquid water [mm]
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid moisture (m3/m3)
   REAL, DIMENSION(       1:NSOIL), INTENT(INOUT) :: SICE   !soil ice moisture (m3/m3)
   REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(INOUT) :: STC    !snow layer temperature [k]
   REAL, DIMENSION(-NSNOW+1:0),     INTENT(INOUT) :: radius !snow grain radius

! local variables:

   INTEGER                     :: J         !do loop/array indices
   REAL                        :: QIN       !water flow into the element (mm/s)
   REAL                        :: QOUT      !water flow out of the element (mm/s)
   REAL                        :: WGDIF     !ice mass after minus sublimation
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_LIQ   !partial volume of liquid water in layer
   REAL, DIMENSION(-NSNOW+1:0) :: VOL_ICE   !partial volume of ice lens in layer
   REAL, DIMENSION(-NSNOW+1:0) :: EPORE     !effective porosity = porosity - VOL_ICE
   REAL :: PROPOR, TEMP
   REAL :: PONDING1, PONDING2

   REAL, DIMENSION(     1:NSOIL) , INTENT(INOUT) :: DSH2O  !change rate of liquid soil moisture[m/s] for mixed-RE
! ----------------------------------------------------------------------

!for the case when SNEQV becomes '0' after 'COMBINE'

   IF(SNEQV == 0.) THEN
      SICE(1) =  SICE(1) + (QSNFRO-QSNSUB)*DT/(DZSNSO(1)*1000.)  ! Barlage: SH2O->SICE v3.6
      IF(SICE(1) < 0.) THEN
        !SH2O(1) = SH2O(1) + SICE(1)

         IF (OPT_RUN == 6) THEN !mixed-RE
           DSH2O(1) =  DSH2O(1) + SICE(1)*DZSNSO(1)/DT  !m/s 
         ELSE
           SH2O(1)  =  SH2O(1) + SICE(1)
           DSH2O(1) = 0.
         ENDIF

         SICE(1) = 0.

      END IF
   END IF

! for shallow snow without a layer
! snow surface sublimation may be larger than existing snow mass. To conserve water,
! excessive sublimation is used to reduce soil water. Smaller time steps would tend 
! to aviod this problem.

   IF(ISNOW == 0 .and. SNEQV > 0.) THEN
      TEMP   = SNEQV
      SNEQV  = SNEQV - QSNSUB*DT + QSNFRO*DT
      PROPOR = SNEQV/TEMP
      SNOWH  = MAX(0.,PROPOR * SNOWH)

      IF(SNEQV < 0.) THEN
         SICE(1) = SICE(1) + SNEQV/(DZSNSO(1)*1000.)
         SNEQV   = 0.
         SNOWH   = 0.
      END IF
      IF(SICE(1) < 0.) THEN
         IF (OPT_RUN == 6) THEN  !mixed RE
             DSH2O(1) = DSH2O(1) + SICE(1)*DZSNSO(1)/DT !m/s
         ELSE
             SH2O(1) = SH2O(1) + SICE(1)
             DSH2O(1)= 0.
         ENDIF

         SICE(1) = 0.
      END IF
   END IF

   IF(SNOWH <= 1.E-8 .OR. SNEQV <= 1.E-6) THEN
     SNOWH = 0.0
     SNEQV = 0.0
   END IF

! for deep snow

   IF ( ISNOW < 0 ) THEN !KWM added this IF statement to prevent out-of-bounds array references
      WGDIF = SNICE(ISNOW+1) - QSNSUB*DT + QSNFRO*DT
      SNICE(ISNOW+1) = WGDIF
      IF (WGDIF < 1.e-6 .and. ISNOW <0) THEN
         CALL  COMBINE (parameters,NSNOW  ,NSOIL  ,DT ,ILOC, JLOC   , & !in
              ISNOW  ,SH2O   ,STC    ,SNICE  ,SNLIQ  , & !inout
              DZSNSO ,SICE   ,SNOWH  ,SNEQV  ,radius , DSH2O ,& !inout
              PONDING1, PONDING2)                       !out
      ENDIF

      !KWM:  Subroutine COMBINE can change ISNOW to make it 0 again?
      IF ( ISNOW < 0 ) THEN !KWM added this IF statement to prevent out-of-bounds array references
         SNLIQ(ISNOW+1) = SNLIQ(ISNOW+1) + QRAIN * DT
         SNLIQ(ISNOW+1) = MAX(0., SNLIQ(ISNOW+1))
      ENDIF
      
   ENDIF !KWM  -- Can the ENDIF be moved toward the end of the subroutine (Just set QSNBOT=0)?

! Porosity and partial volume

   !KWM Looks to me like loop index / IF test can be simplified.

!niu  DO J = -NSNOW+1, 0
!niu     IF (J >= ISNOW+1) THEN
!niu        VOL_ICE(J)      = MIN(1., SNICE(J)/(DZSNSO(J)*DENICE))
!niu        EPORE(J)        = 1. - VOL_ICE(J)
!niu        VOL_LIQ(J)      = MIN(EPORE(J),SNLIQ(J)/(DZSNSO(J)*DENH2O))
!niu     END IF
!niu  END DO

   DO J = ISNOW+1, 0
             VOL_ICE(J)      = MIN(1., SNICE(J)/(DZSNSO(J)*DENICE))
             EPORE(J)        = 1. - VOL_ICE(J)
   END DO

   QIN = 0.
   QOUT = 0.

   !KWM Looks to me like loop index / IF test can be simplified.

   DO J = ISNOW+1, 0
            SNLIQ(J)   = SNLIQ(J) + QIN
            VOL_LIQ(J) = SNLIQ(J)/(DZSNSO(J)*DENH2O)   !niu: allowed to be > EPORE

            QOUT = MAX(0.,(VOL_LIQ(J)-parameters%SSI*EPORE(J))*DZSNSO(J))

            QOUT     = QOUT*DENH2O
            SNLIQ(J) = SNLIQ(J) - QOUT
            QIN      = QOUT
   END DO

!niu  DO J = -NSNOW+1, 0
!niu     IF (J >= ISNOW+1) THEN
!niu        SNLIQ(J) = SNLIQ(J) + QIN
!niu        IF (J <= -1) THEN
!niu           IF (EPORE(J) < 0.05 .OR. EPORE(J+1) < 0.05) THEN
!niu              QOUT = 0.
!niu           ELSE
!niu              QOUT = MAX(0.,(VOL_LIQ(J)-parameters%SSI*EPORE(J))*DZSNSO(J))
!niu              QOUT = MIN(QOUT,(1.-VOL_ICE(J+1)-VOL_LIQ(J+1))*DZSNSO(J+1))
!niu           END IF
!niu        ELSE
!niu           QOUT = MAX(0.,(VOL_LIQ(J) - parameters%SSI*EPORE(J))*DZSNSO(J))
!niu        END IF
!niu        QOUT = QOUT*1000.
!niu        SNLIQ(J) = SNLIQ(J) - QOUT
!niu        QIN = QOUT
!niu     END IF
!niu  END DO

! Liquid water from snow bottom to soil

   QSNBOT = QOUT / DT           ! mm/s

  END SUBROUTINE SNOWH2O

!== begin soilwater ================================================================================

  SUBROUTINE SOILWATER (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                        QINSUR ,QSEVA  ,ETRANI ,SICE   ,ILOC   , JLOC, & !in
                        SH2O   ,SMC    ,ZWT    ,VEGTYP ,TOPOSV ,& !inout
                        SMCWTD, DEEPRECH                       ,& !inout
                        RUNSRF ,QDRAIN ,RUNSUB ,WCND   ,FCRMAX ,& !out
                        VARSD  ,DMICE  , & !Mixed-RE in
                        ATM_BC ,PSI    ,DSH2O  ,ATMACT ,& !Mixed-RE inout
                        DTFINEM,SICEO  ,HTOP   ,        & !Mixed-RE inout
                        QDIS   ,QDRYC  ,RSINEX )                                  !Mixed-RE out
! ----------------------------------------------------------------------
! calculate surface runoff and soil moisture.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                     INTENT(IN) :: ILOC   !grid index
  INTEGER,                     INTENT(IN) :: JLOC   !grid index
  INTEGER,                     INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                     INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL,                        INTENT(IN) :: DT     !time step (sec)
  REAL, INTENT(IN)                        :: QINSUR !water input on soil surface [mm/s]
  REAL, INTENT(INOUT)                     :: QSEVA  !evap from soil surface [mm/s]
  REAL, DIMENSION(1:NSOIL),    INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(1:NSOIL),    INTENT(IN) :: ETRANI !evapotranspiration from soil layers [mm/s]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer depth [m]
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT)   :: SICE   !soil ice content [m3/m3]

  INTEGER,                     INTENT(IN) :: VEGTYP
  REAL                           , INTENT(IN)    :: TOPOSV !standard dev of DEM [m]

! input & output
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O   !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC    !total soil water content [m3/m3]
  REAL, INTENT(INOUT)                     :: ZWT    !water table depth [m]
  REAL,                     INTENT(INOUT) :: SMCWTD !soil moisture between bottom of the soil and the water table [m3/m3]
  REAL                    , INTENT(INOUT) :: DEEPRECH

! output
  REAL, INTENT(OUT)                       :: QDRAIN !soil-bottom free drainage [mm/s] 
  REAL, INTENT(OUT)                       :: RUNSRF !surface runoff [mm/s] 
  REAL, INTENT(OUT)                       :: RUNSUB !subsurface runoff [mm/s] 
  REAL, INTENT(OUT)                       :: FCRMAX !maximum of FCR (-)
  REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: WCND   !hydraulic conductivity (m/s)

! local
  INTEGER                                 :: K,IZ   !do-loop index
  INTEGER                                 :: ITER   !iteration index
  REAl                                    :: DTFINE !fine time step (s)
  REAL, DIMENSION(1:NSOIL)                :: RHSTT  !right-hand side term of the matrix
  REAL, DIMENSION(1:NSOIL)                :: AI     !left-hand side term
  REAL, DIMENSION(1:NSOIL)                :: BI     !left-hand side term
  REAL, DIMENSION(1:NSOIL)                :: CI     !left-hand side term

  REAL                                    :: FFF    !runoff decay factor (m-1)
  REAL                                    :: RSBMX  !baseflow coefficient [mm/s]
  REAL                                    :: PDDUM  !infiltration rate at surface (m/s)
  REAL                                    :: FICE   !ice fraction in frozen soil
  REAL                                    :: WPLUS  !saturation excess of the total soil [m]
  REAL                                    :: RSAT   !accumulation of WPLUS (saturation excess) [m]
  REAL                                    :: SICEMAX!maximum soil ice content (m3/m3)
  REAL                                    :: SH2OMIN!minimum soil liquid water content (m3/m3)
  REAL                                    :: WTSUB  !sum of WCND(K)*DZSNSO(K)
  REAL                                    :: MH2O   !water mass removal (mm)
  REAL                                    :: FSAT   !fractional saturated area (-)
  REAL, DIMENSION(1:NSOIL)                :: MLIQ   !
  REAL                                    :: XS     !
  REAL                                    :: WATMIN !
  REAL                                    :: QDRAIN_SAVE !
  REAL                                    :: RUNSRF_SAVE !
  REAL                                    :: EPORE  !effective porosity [m3/m3]
  REAL, DIMENSION(1:NSOIL)                :: FCR    !impermeable fraction due to frozen soil
  INTEGER                                 :: NITER  !iteration times soil moisture (-)
  REAL                                    :: SMCTOT !2-m averaged soil moisture (m3/m3)
  REAL                                    :: DZTOT  !2-m soil depth (m)
  REAL                                    :: VOID
  REAL, PARAMETER :: A = 4.0

! Mixed Richads' equation:

  INTEGER ,INTENT(IN)                     :: VARSD  !if variable soil depth is activated see noah_driver
  REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: DMICE   !change rate of solid ice [m/s]
  INTEGER                 , INTENT(INOUT) :: ATM_BC !ATM_BC: 0->Neuman (flux) ;1->Dirichlet (state)
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: PSI    !prognostic pressure head (m)
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: DSH2O  !change rate of liquid water[m/s],save acculated adjust sh2o
!  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SE     !effective saturation (-)
  REAL                    , INTENT(INOUT) :: ATMACT
  REAL                    , INTENT(INOUT) :: HTOP   !surface ponding depth (mm)
  REAL,                     INTENT(INOUT) :: DTFINEM
  REAL, DIMENSION(1:NSOIL)                :: VGN
  REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SICEO   !soil ice content [m3/m3]
  REAL,                     INTENT(OUT)   :: QDIS     !m/s
  REAL                    , INTENT(OUT)   :: QDRYC    !dry limit correction to EDIR [mm/s]
  REAL                    , INTENT(OUT)   :: RSINEX   !infiltration excess runoff [mm/s]
 
  REAL :: DEFICIT

  REAL :: SW_END,SW_BEG,SW_ERR,TRANS,DSH2OS
! ----------------------------------------------------------------------
    RUNSRF = 0.0
    PDDUM  = 0.0
    RSAT   = 0.0

! for the case when snowmelt water is too large

    DO K = 1,NSOIL
       EPORE   = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )

       IF (OPT_RUN == 6) THEN   !mixed-form
         RSAT     = 0.
         DSH2O(K) = DSH2O(K) + MAX(0.,(SH2O(K)-EPORE))*DZSNSO(K)/DT  ! m/s
       ELSE
         RSAT     = RSAT + MAX(0.,SH2O(K)-EPORE)*DZSNSO(K)
         DSH2O(K) = 0.
       ENDIF

       SH2O(K)  = MIN(EPORE,SH2O(K))

      !RSAT    = RSAT + MAX(0.,SH2O(K)-EPORE)*DZSNSO(K)  
      !SH2O(K) = MIN(EPORE,SH2O(K))             
    END DO

!impermeable fraction due to frozen soil

    DO K = 1,NSOIL
       FICE    = MIN(1.0,SICE(K)/parameters%SMCMAX(K))
       FCR(K)  = MAX(0.0,EXP(-A*(1.-FICE))- EXP(-A)) /  &
                        (1.0              - EXP(-A))
    END DO

! maximum soil ice content and minimum liquid water of all layers

    SICEMAX = 0.0
    FCRMAX  = 0.0
    SH2OMIN = parameters%SMCMAX(1)
    DO K = 1,NSOIL
       IF (SICE(K) > SICEMAX) SICEMAX = SICE(K)
       IF (FCR(K)  > FCRMAX)  FCRMAX  = FCR(K)
       IF (SH2O(K) < SH2OMIN) SH2OMIN = SH2O(K)
    END DO

!subsurface runoff for runoff scheme option 2

    IF(OPT_RUN == 2) THEN 
        FFF   = 2.0
        RSBMX = 4.0
        CALL ZWTEQ (parameters,NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
        RUNSUB = (1.0-FCRMAX) * RSBMX * EXP(-parameters%TIMEAN) * EXP(-FFF*ZWT)   ! mm/s
    END IF

!surface runoff and infiltration rate using different schemes

!jref impermable surface at urban
!niu    IF ( parameters%urban_flag ) FCR(1)= 0.95
    IF ( parameters%urban_flag ) FCR(1)= 0.3

    IF(OPT_RUN == 6) THEN

       DEFICIT = SUM((SH2O(1:NSOIL)-parameters%SMCMAX(1:NSOIL))*DZSNSO(1:NSOIL) )  !m

       FFF    = 12.0
       FSAT   = parameters%FSATMX*EXP(0.5*FFF*DEFICIT)
       
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF

    END IF

    IF(OPT_RUN == 1) THEN
       FFF = 3.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*ZWT)
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 5) THEN
       FFF = 6.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*MAX(-2.0-ZWT,0.))
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s
       END IF
    END IF

    IF(OPT_RUN == 2) THEN
       FFF   = 2.0
       FSAT   = parameters%FSATMX*EXP(-0.5*FFF*ZWT)
       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ( (1.0-FCR(1))*FSAT + FCR(1) )
         PDDUM  = QINSUR - RUNSRF                          ! m/s 
       END IF
    END IF

    IF(OPT_RUN == 3) THEN
       CALL INFIL (parameters,NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & !in
                   SICEMAX,QINSUR ,                         & !in
                   PDDUM  ,RUNSRF )                           !out
    END IF

    IF(OPT_RUN == 4) THEN
       SMCTOT = 0.
       DZTOT  = 0.
       DO K = 1,NSOIL
          DZTOT   = DZTOT  + DZSNSO(K)  
          SMCTOT  = SMCTOT + SMC(K)/parameters%SMCMAX(K)*DZSNSO(K)
          IF(DZTOT >= 2.0) EXIT
       END DO
       SMCTOT = SMCTOT/DZTOT
       FSAT   = MAX(0.01,SMCTOT) ** 4.        !BATS

       IF(QINSUR > 0.) THEN
         RUNSRF = QINSUR * ((1.0-FCR(1))*FSAT+FCR(1))  
         PDDUM  = QINSUR - RUNSRF                       ! m/s
       END IF
    END IF

    IF (PDDUM*DT>=1.00*DZSNSO(1)*parameters%SMCMAX(1) ) THEN   !mixed-form
       RUNSRF = RUNSRF + (PDDUM-1.0*DZSNSO(1)*parameters%SMCMAX(1)/DT)
       PDDUM  = 1.0*DZSNSO(1)*parameters%SMCMAX(1)/DT
    END IF

! solving SH2O & PSI

    IF (OPT_RUN ==6) THEN   !mixed-form Richards equation solver

    ! for water balance check

   ! SW_BEG = HTOP  !mm
   ! TRANS  = 0.
   ! DSH2OS = 0.
 
     !DO IZ = 1,NSOIL
     !     SW_BEG = SW_BEG + SMC(IZ)*DZSNSO(IZ)*1000.
     !     TRANS  = TRANS  + ETRANI(IZ)
     !     DSH2OS = DSH2OS + DSH2O(IZ)
     !END DO

     ! IF(ILOC == 137 .and. JLOC == 7) THEN
     !  write(*,*) "------------3SW----------"
     !  write(*,*) "(SH2O(IZ),IZ=1,NSOIL  )",(SH2O(IZ),IZ=1,NSOIL)
     !  write(*,*) "(DZSNSO(IZ),IZ=1,NSOIL)",(DZSNSO(IZ),IZ=1,NSOIL)
     !  write(*,*) 'SUM(SH2O)     =',SUM(SH2O(1:NSOIL)*1000.*DZSNSO(1:NSOIL))
     ! END IF

      !water balance: SW_ERR=SW_BEG+(QINSUR-QSEVA)*DT*1000.-RUNSRF*DT-SW_END;
      !SW_BEG = sum of soil water

     IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*)'PDDUM=',PDDUM*DT*1000.
    ! write(*,*)'QSEVA=',QSEVA*DT*1000.
      write(*,*)'FSAT  =',FSAT
      write(*,*)'RUNSRF=',RUNSRF
     END IF

      !IF(ILOC == 137 .and. JLOC == 7) THEN
      !  write(*,'(a6,13F12.5)') 'SICEO=',SICEO*DZSNSO(1:NSOIL)*1000.
      !  write(*,'(a6,13F12.5)') 'DMICE=',DMICE*1000.*DT
      !  write(*,'(a6,13F12.5)') 'DSH2O=',DSH2O*1000.*DT
      !END IF

      CALL  MIXEDRE (OPT_RUN,OPT_WATRET,VARSD   ,NSOIL   ,DZSNSO(1:NSOIL) ,DT        , & !in 
                     PDDUM  ,ETRANI    ,QSEVA   ,DMICE   ,DSH2O  ,FCR     ,SICEO  , & !in
                     TOPOSV ,ILOC   ,JLOC    ,                                              & !in
                     parameters%BEXP   ,parameters%PSISAT,parameters%DKSAT ,parameters%SMCMAX, & !in
                     parameters%SMCR   ,parameters%VGN   ,parameters%VGPSAT                  , & !in
                     PSI    ,SH2O      ,WCND             ,ATMACT    , & !inout
                     ATM_BC ,DTFINEM   ,HTOP    ,                                     & !inout
                     ZWT    ,RSINEX    ,QDIS    ,QDRYC   )                              !out

      RUNSRF = RUNSRF*1000. + RSINEX     !mm/s
      QDRAIN = 0.

       IF(ILOC == 137 .and. JLOC == 7) THEN
         write(*,*) 'HTOP=',HTOP
       END IF

     ! IF(ILOC == 137 .and. JLOC == 7) THEN
     !  write(*,*) "------------4SW----------"
     !  write(*,*) "(SH2O(IZ),IZ=1,NSOIL)  ",(SH2O(IZ),IZ=1,NSOIL)
     !  write(*,*) "(DZSNSO(IZ),IZ=1,NSOIL)",(DZSNSO(IZ),IZ=1,NSOIL)
     !  write(*,*) 'SUM(SH2O)     =',SUM(SH2O(1:NSOIL)*1000.*DZSNSO(1:NSOIL))
     ! END IF

     !SW_END = HTOP
     !DO IZ = 1,NSOIL
     !   SW_END = SW_END + SMC(IZ)*DZSNSO(IZ)*1000.
     !END DO

     !SW_ERR=SW_BEG+(QINSUR-QSEVA-TRANS+DSH2OS-QDIS)*DT*1000.-(RUNSRF+QDRAIN-QDRYC)*DT-SW_END
     !IF(abs(SW_ERR) >= 0.1) then
     !    write(*,*) 'in SOILWATER:ix,iy,SW_ERR=',iloc,jloc,SW_ERR
     !    write(*,*) 'SW_BEG =',SW_BEG
     !    write(*,*) 'SW_END =',SW_END
     !    write(*,*) 'SW_BEG-SW_END =',SW_BEG-SW_END
     !    write(*,*) 'HTOP   =',HTOP
     !    write(*,*) 'PDDUM*DT*1000 =',PDDUM*DT*1000.
     !    write(*,*) 'QINSUR*DT*1000 =',QINSUR*DT*1000.
     !    write(*,*) 'QSEVA*DT*1000  =',QSEVA*DT*1000.
     !    write(*,*) 'TRANS*DT*1000  =',TRANS*DT*1000.
     !    write(*,*) 'DSH2OS*DT*1000 =',DSH2OS*DT*1000.
     !    write(*,*) 'DMICE*DT*1000  =',DMICE*DT*1000.
     !    write(*,*) 'RUNSRF*DT      =',RUNSRF*DT
     !    write(*,*) '    RSAT*DT*1000.  =',RSAT*DT*1000.
     !    write(*,*) '    QDIS*DT*1000.  =',QDIS*DT*1000.
     !end if

    ELSE !Noah orginal solver

! determine iteration times and finer time step

    NITER = 1

    VOID  = MAX(0.0,DZSNSO(1)*(parameters%SMCMAX(1)-SMC(1)))

    IF (PDDUM*DT>VOID) THEN
        NITER = NITER*6
    END IF

    DTFINE  = DT / NITER

! solve soil moisture

    QDRAIN_SAVE = 0.0
    RUNSRF_SAVE = 0.0
    DO ITER = 1, NITER
       IF(QINSUR > 0. .and. OPT_RUN == 3) THEN
          CALL INFIL (parameters,NSOIL  ,DTFINE     ,ZSOIL  ,SH2O   ,SICE   , & !in
                      SICEMAX,QINSUR ,                         & !in
                      PDDUM  ,RUNSRF )                           !out
       END IF

       CALL SRT   (parameters,NSOIL  ,ZSOIL  ,DTFINE ,PDDUM  ,ETRANI , & !in
                   QSEVA  ,SH2O   ,SMC    ,SICE   ,ZWT    ,FCR    , & !in
                   SICEMAX,FCRMAX ,ILOC   ,JLOC   ,SMCWTD ,         & !in
                   RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & !out
                   WCND   )                                   !out

       CALL SSTEP (parameters,NSOIL  ,NSNOW  ,DTFINE ,ZSOIL  ,DZSNSO , & !in
                   SICE   ,ILOC   ,JLOC   ,ZWT            ,                 & !in
                   SH2O   ,SMC    ,AI     ,BI     ,CI     , & !inout
                   RHSTT  ,SMCWTD ,QDRAIN ,DEEPRECH,                                 & !inout
                   WPLUS)                                     !out

       RSAT =  RSAT + WPLUS
       QDRAIN_SAVE = QDRAIN_SAVE + QDRAIN
       RUNSRF_SAVE = RUNSRF_SAVE + RUNSRF
    END DO

    QDRAIN = QDRAIN_SAVE/NITER
    RUNSRF = RUNSRF_SAVE/NITER

! update PSI (mixed-form)

    DO IZ = 1,NSOIL
         CALL GET_PSI (OPT_WATRET ,SH2O(IZ) ,SICE(IZ)              ,&
                       parameters%SMCMAX(IZ)  ,parameters%SMCR(IZ) ,parameters%VGN(IZ) ,parameters%VGPSAT(IZ) ,&
                       parameters%BEXP(IZ),parameters%PSISAT(IZ)   , &
                       PSI(IZ))
    ENDDO

!mixed-form    ENDIF  !soil moisture sovler

    RUNSRF = RUNSRF * 1000. + RSAT * 1000./DT  ! m/s -> mm/s
    QDRAIN = QDRAIN * 1000.

    ENDIF  !soil moisture sovler

!WRF_HYDRO_DJG...
!yw    INFXSRT = RUNSRF * DT   !mm/s -> mm

! removal of soil water due to groundwater flow (option 2)

    IF(OPT_RUN == 2) THEN
         WTSUB = 0.
         DO K = 1, NSOIL
           WTSUB = WTSUB + WCND(K)*DZSNSO(K)
         END DO

         DO K = 1, NSOIL
           MH2O    = RUNSUB*DT*(WCND(K)*DZSNSO(K))/WTSUB       ! mm
           SH2O(K) = SH2O(K) - MH2O/(DZSNSO(K)*1000.)
         END DO
    END IF

! Limit MLIQ to be greater than or equal to watmin.
! Get water needed to bring MLIQ equal WATMIN from lower layer.

   IF(OPT_RUN /= 1) THEN
      DO IZ = 1, NSOIL
         MLIQ(IZ) = SH2O(IZ)*DZSNSO(IZ)*1000.
      END DO

      WATMIN = 0.01           ! mm
      DO IZ = 1, NSOIL-1
          IF (MLIQ(IZ) .LT. 0.) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = NSOIL
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        RUNSUB   = RUNSUB - XS/DT
        IF(OPT_RUN == 5)DEEPRECH = DEEPRECH - XS*1.E-3

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / (DZSNSO(IZ)*1000.)
      END DO
   END IF

  END SUBROUTINE SOILWATER

!== begin zwteq ====================================================================================

  SUBROUTINE ZWTEQ (parameters,NSOIL  ,NSNOW  ,ZSOIL  ,DZSNSO ,SH2O   ,ZWT)
! ----------------------------------------------------------------------
! calculate equilibrium water table depth (Niu et al., 2005)
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN) :: NSOIL  !no. of soil layers
  INTEGER,                         INTENT(IN) :: NSNOW  !maximum no. of snow layers
  REAL, DIMENSION(1:NSOIL),        INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer depth [m]
  REAL, DIMENSION(1:NSOIL),        INTENT(IN) :: SH2O   !soil liquid water content [m3/m3]

! output

  REAL,                           INTENT(OUT) :: ZWT    !water table depth [m]

! locals

  INTEGER :: K                      !do-loop index
  INTEGER, PARAMETER :: NFINE = 100 !no. of fine soil layers of 6m soil
  REAL    :: WD1                    !water deficit from coarse (4-L) soil moisture profile
  REAL    :: WD2                    !water deficit from fine (100-L) soil moisture profile
  REAL    :: DZFINE                 !layer thickness of the 100-L soil layers to 6.0 m
  REAL    :: TEMP                   !temporary variable
  REAL, DIMENSION(1:NFINE) :: ZFINE !layer-bottom depth of the 100-L soil layers to 6.0 m
! ----------------------------------------------------------------------

   WD1 = 0.
   DO K = 1,NSOIL
     WD1 = WD1 + (parameters%SMCMAX(K)-SH2O(K)) * DZSNSO(K) ! [m]
   ENDDO

   DZFINE = 3.0 * (-ZSOIL(NSOIL)) / NFINE  
   do K =1,NFINE
      ZFINE(K) = FLOAT(K) * DZFINE
   ENDDO

   ZWT = -3.*ZSOIL(NSOIL) - 0.001   ! initial value [m]

   WD2 = 0.
   DO K = 1,NFINE
     TEMP  = 1. + (ZWT-ZFINE(K))/parameters%PSISAT(K)
     WD2   = WD2 + parameters%SMCMAX(K)*(1.-TEMP**(-1./parameters%BEXP(K)))*DZFINE
     IF(ABS(WD2-WD1).LE.0.01) THEN
        ZWT = ZFINE(K)
        EXIT
     ENDIF
   ENDDO

  END SUBROUTINE ZWTEQ

!== begin infil ====================================================================================

  SUBROUTINE INFIL (parameters,NSOIL  ,DT     ,ZSOIL  ,SH2O   ,SICE   , & !in
                    SICEMAX,QINSUR ,                         & !in
                    PDDUM  ,RUNSRF )                           !out
! --------------------------------------------------------------------------------
! compute inflitration rate at soil surface and surface runoff
! --------------------------------------------------------------------------------
    IMPLICIT NONE
! --------------------------------------------------------------------------------
! inputs
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                  INTENT(IN) :: NSOIL  !no. of soil layers
  REAL,                     INTENT(IN) :: DT     !time step (sec)
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: ZSOIL  !depth of soil layer-bottom [m]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SH2O   !soil liquid water content [m3/m3]
  REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SICE   !soil ice content [m3/m3]
  REAL,                     INTENT(IN) :: QINSUR !water input on soil surface [mm/s]
  REAL,                     INTENT(IN) :: SICEMAX!maximum soil ice content (m3/m3)

! outputs
  REAL,                    INTENT(OUT) :: RUNSRF !surface runoff [mm/s] 
  REAL,                    INTENT(OUT) :: PDDUM  !infiltration rate at surface

! locals
  INTEGER :: IALP1, J, JJ,  K
  REAL                     :: VAL
  REAL                     :: DDT
  REAL                     :: PX
  REAL                     :: DT1, DD, DICE
  REAL                     :: FCR
  REAL                     :: SUM
  REAL                     :: ACRT
  REAL                     :: WDF
  REAL                     :: WCND
  REAL                     :: SMCAV
  REAL                     :: INFMAX
  REAL, DIMENSION(1:NSOIL) :: DMAX
  INTEGER, PARAMETER       :: CVFRZ = 3
! --------------------------------------------------------------------------------

    IF (QINSUR >  0.0) THEN
       DT1 = DT /86400.
       SMCAV = parameters%SMCMAX(1) - parameters%SMCWLT(1)

! maximum infiltration rate

       DMAX(1)= -ZSOIL(1) * SMCAV
       DICE   = -ZSOIL(1) * SICE(1)
       DMAX(1)= DMAX(1)* (1.0-(SH2O(1) + SICE(1) - parameters%SMCWLT(1))/SMCAV)

       DD = DMAX(1)

       DO K = 2,NSOIL
          DICE    = DICE + (ZSOIL(K-1) - ZSOIL(K) ) * SICE(K)
          DMAX(K) = (ZSOIL(K-1) - ZSOIL(K)) * SMCAV
          DMAX(K) = DMAX(K) * (1.0-(SH2O(K) + SICE(K) - parameters%SMCWLT(K))/SMCAV)
          DD      = DD + DMAX(K)
       END DO

       VAL = (1. - EXP ( - parameters%KDT * DT1))
       DDT = DD * VAL
       PX  = MAX(0.,QINSUR * DT)
       INFMAX = (PX * (DDT / (PX + DDT)))/ DT

! impermeable fraction due to frozen soil

       FCR = 1.
       IF (DICE >  1.E-2) THEN
          ACRT = CVFRZ * parameters%FRZX / DICE
          SUM = 1.
          IALP1 = CVFRZ - 1
          DO J = 1,IALP1
             K = 1
             DO JJ = J +1,IALP1
                K = K * JJ
             END DO
             SUM = SUM + (ACRT ** (CVFRZ - J)) / FLOAT(K)
          END DO
          FCR = 1. - EXP (-ACRT) * SUM
       END IF

! correction of infiltration limitation

       INFMAX = INFMAX * FCR

! jref for urban areas
!       IF ( parameters%urban_flag ) INFMAX == INFMAX * 0.05

       CALL WDFCND2 (parameters,WDF,WCND,SH2O(1),SICEMAX,1)
       INFMAX = MAX (INFMAX,WCND)
       INFMAX = MIN (INFMAX,PX)

       RUNSRF= MAX(0., QINSUR - INFMAX)
       PDDUM = QINSUR - RUNSRF

    END IF

  END SUBROUTINE INFIL

!== begin srt ======================================================================================

  SUBROUTINE SRT (parameters,NSOIL  ,ZSOIL  ,DT     ,PDDUM  ,ETRANI , & !in
                  QSEVA  ,SH2O   ,SMC    ,SICE      ,ZWT    ,FCR    , & !in
                  SICEMAX,FCRMAX ,ILOC   ,JLOC   ,SMCWTD ,         & !in
                  RHSTT  ,AI     ,BI     ,CI     ,QDRAIN , & !out
                  WCND   )                                   !out
! ----------------------------------------------------------------------
! calculate the right hand side of the time tendency term of the soil
! water diffusion equation.  also to compute ( prepare ) the matrix
! coefficients for the tri-diagonal matrix of the implicit time scheme.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                  INTENT(IN)  :: ILOC   !grid index
    INTEGER,                  INTENT(IN)  :: JLOC   !grid index
    INTEGER,                  INTENT(IN)  :: NSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: ZSOIL
    REAL,                     INTENT(IN)  :: DT
    REAL,                     INTENT(IN)  :: PDDUM
    REAL,                     INTENT(IN)  :: QSEVA
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: ETRANI
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SICE
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: SMC
    REAL,                     INTENT(IN)  :: ZWT    ! water table depth [m]
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: FCR
    REAL, INTENT(IN)                      :: FCRMAX !maximum of FCR (-)
    REAL,                     INTENT(IN)  :: SICEMAX!maximum soil ice content (m3/m3)
    REAL,                     INTENT(IN)  :: SMCWTD !soil moisture between bottom of the soil and the water table

! output

    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: RHSTT
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: AI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: BI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: CI
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: WCND    !hydraulic conductivity (m/s)
    REAL,                     INTENT(OUT) :: QDRAIN  !bottom drainage (m/s)

! local
    INTEGER                               :: K
    REAL, DIMENSION(1:NSOIL)              :: DDZ
    REAL, DIMENSION(1:NSOIL)              :: DENOM
    REAL, DIMENSION(1:NSOIL)              :: DSMDZ
    REAL, DIMENSION(1:NSOIL)              :: WFLUX
    REAL, DIMENSION(1:NSOIL)              :: WDF
    REAL, DIMENSION(1:NSOIL)              :: SMX
    REAL                                  :: TEMP1
    REAL                                  :: SMXWTD !soil moisture between bottom of the soil and water table
    REAL                                  :: SMXBOT  !soil moisture below bottom to calculate flux

! Niu and Yang (2006), J. of Hydrometeorology
! ----------------------------------------------------------------------

    IF(OPT_INF == 1) THEN ! Niu and Yang (2006)
      DO K = 1, NSOIL
        CALL WDFCND1 (parameters,WDF(K),WCND(K),SMC(K),FCR(K),K)
        SMX(K) = SMC(K)
      END DO
        IF(OPT_RUN == 5)SMXWTD=SMCWTD
    END IF

    IF(OPT_INF == 2) THEN ! Koren et al. (1999)
      DO K = 1, NSOIL
        CALL WDFCND2 (parameters,WDF(K),WCND(K),SH2O(K),SICEMAX,K)
        SMX(K) = SH2O(K)
      END DO
          IF(OPT_RUN == 5)SMXWTD=SMCWTD*SH2O(NSOIL)/SMC(NSOIL)  !same liquid fraction as in the bottom layer
    END IF

    IF(OPT_INF == 3) THEN ! Flerchinger and Saxton 1989; Cox et al. 1999; Hansson et al. 2004
      DO K = 1, NSOIL
        CALL WDFCND3 (parameters,WDF(K),WCND(K),SH2O(K),SICEMAX,K)
        SMX(K) = SH2O(K)
      END DO
          IF(OPT_RUN == 5)SMXWTD=SMCWTD*SH2O(NSOIL)/SMC(NSOIL)  !same liquid fraction as in the bottom layer
    END IF

    IF(OPT_INF == 4) THEN ! Zhao and Gray, 1997; NCAR CLM4.5 & CLM5.0
      DO K = 1, NSOIL
        CALL WDFCND4 (parameters,WDF(K),WCND(K),SH2O(K),SICE(K),K)
        SMX(K) = SH2O(K)
      END DO
          IF(OPT_RUN == 5)SMXWTD=SMCWTD*SH2O(NSOIL)/SMC(NSOIL)  !same liquid fraction as in the bottom layer
    END IF


    DO K = 1, NSOIL
       IF(K == 1) THEN
          DENOM(K) = - ZSOIL (K)
          TEMP1    = - ZSOIL (K+1)
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K) * DSMDZ(K) + WCND(K) - PDDUM + ETRANI(K) + QSEVA
       ELSE IF (K < NSOIL) THEN
          DENOM(k) = (ZSOIL(K-1) - ZSOIL(K))
          TEMP1    = (ZSOIL(K-1) - ZSOIL(K+1))
          DDZ(K)   = 2.0 / TEMP1
          DSMDZ(K) = 2.0 * (SMX(K) - SMX(K+1)) / TEMP1
          WFLUX(K) = WDF(K  ) * DSMDZ(K  ) + WCND(K  )         &
                   - WDF(K-1) * DSMDZ(K-1) - WCND(K-1) + ETRANI(K)
       ELSE
          DENOM(K) = (ZSOIL(K-1) - ZSOIL(K))
          IF(OPT_RUN == 1 .or. OPT_RUN == 2) THEN
             QDRAIN   = 0.
          END IF
          IF(OPT_RUN == 3) THEN
             QDRAIN   = parameters%SLOPE*WCND(K)
          END IF
          IF(OPT_RUN == 4) THEN
             QDRAIN   = (1.0-FCRMAX)*WCND(K)
          END IF
          IF(OPT_RUN == 5) THEN   !gmm new m-m&f water table dynamics formulation
             TEMP1    = 2.0 * DENOM(K)
             IF(ZWT < ZSOIL(NSOIL)-DENOM(NSOIL))THEN
!gmm interpolate from below, midway to the water table, to the middle of the auxiliary layer below the soil bottom
                SMXBOT = SMX(K) - (SMX(K)-SMXWTD) *  DENOM(K) * 2./ (DENOM(K) + ZSOIL(K) - ZWT)
             ELSE
                SMXBOT = SMXWTD
             ENDIF
             DSMDZ(K) = 2.0 * (SMX(K) - SMXBOT) / TEMP1
             QDRAIN   = WDF(K  ) * DSMDZ(K  ) + WCND(K  )
          END IF   
          WFLUX(K) = -(WDF(K-1)*DSMDZ(K-1))-WCND(K-1)+ETRANI(K) + QDRAIN
       END IF
    END DO

    DO K = 1, NSOIL
       IF(K == 1) THEN
          AI(K)    =   0.0
          BI(K)    =   WDF(K  ) * DDZ(K  ) / DENOM(K)
          CI(K)    = - BI (K)
       ELSE IF (K < NSOIL) THEN
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = - WDF(K  ) * DDZ(K  ) / DENOM(K)
          BI(K)    = - ( AI (K) + CI (K) )
       ELSE
          AI(K)    = - WDF(K-1) * DDZ(K-1) / DENOM(K)
          CI(K)    = 0.0
          BI(K)    = - ( AI (K) + CI (K) )
       END IF
          RHSTT(K) = WFLUX(K) / (-DENOM(K))
    END DO

! ----------------------------------------------------------------------
  END SUBROUTINE SRT

!== begin sstep ====================================================================================

  SUBROUTINE SSTEP (parameters,NSOIL  ,NSNOW  ,DT     ,ZSOIL  ,DZSNSO , & !in
                    SICE   ,ILOC   ,JLOC   ,ZWT            ,                 & !in
                    SH2O   ,SMC    ,AI     ,BI     ,CI     , & !inout
                    RHSTT  ,SMCWTD ,QDRAIN ,DEEPRECH,                                 & !inout
                    WPLUS  )                                   !out

! ----------------------------------------------------------------------
! calculate/update soil moisture content values 
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
!input

  type (noahmp_parameters), intent(in) :: parameters
    INTEGER,                         INTENT(IN) :: ILOC   !grid index
    INTEGER,                         INTENT(IN) :: JLOC   !grid index
    INTEGER,                         INTENT(IN) :: NSOIL  !
    INTEGER,                         INTENT(IN) :: NSNOW  !
    REAL, INTENT(IN)                            :: DT
    REAL, INTENT(IN)                            :: ZWT
    REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL
    REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE
    REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO ! snow/soil layer thickness [m]

!input and output
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: AI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: BI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: CI
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: RHSTT
    REAL                    , INTENT(INOUT) :: SMCWTD
    REAL                    , INTENT(INOUT) :: QDRAIN
    REAL                    , INTENT(INOUT) :: DEEPRECH

!output
    REAL, INTENT(OUT)                       :: WPLUS     !saturation excess water (m)

!local
    INTEGER                                 :: K
    REAL, DIMENSION(1:NSOIL)                :: RHSTTIN
    REAL, DIMENSION(1:NSOIL)                :: CIIN
    REAL                                    :: STOT
    REAL                                    :: EPORE
    REAL                                    :: WMINUS
! ----------------------------------------------------------------------
    WPLUS = 0.0

    DO K = 1,NSOIL
       RHSTT (K) =   RHSTT(K) * DT
       AI (K)    =      AI(K) * DT
       BI (K)    = 1. + BI(K) * DT
       CI (K)    =      CI(K) * DT
    END DO

! copy values for input variables before calling rosr12

    DO K = 1,NSOIL
       RHSTTIN(k) = RHSTT(K)
       CIIN(k)    = CI(K)
    END DO

! call ROSR12 to solve the tri-diagonal matrix

    CALL ROSR12 (CI,AI,BI,CIIN,RHSTTIN,RHSTT,1,NSOIL,0)

    DO K = 1,NSOIL
        SH2O(K) = SH2O(K) + CI(K)
    ENDDO

!  excessive water above saturation in a layer is moved to
!  its unsaturated layer like in a bucket

!gmmwith opt_run=5 there is soil moisture below nsoil, to the water table
  IF(OPT_RUN == 5) THEN

!update smcwtd

     IF(ZWT < ZSOIL(NSOIL)-DZSNSO(NSOIL))THEN
!accumulate qdrain to update deep water table and soil moisture later
        DEEPRECH =  DEEPRECH + DT * QDRAIN
     ELSE
        SMCWTD = SMCWTD + DT * QDRAIN  / DZSNSO(NSOIL)
        WPLUS        = MAX((SMCWTD-parameters%SMCMAX(NSOIL)), 0.0) * DZSNSO(NSOIL)
        WMINUS       = MAX((1.E-4-SMCWTD), 0.0) * DZSNSO(NSOIL)

        SMCWTD = MAX( MIN(SMCWTD,parameters%SMCMAX(NSOIL)) , 1.E-4)
        SH2O(NSOIL)    = SH2O(NSOIL) + WPLUS/DZSNSO(NSOIL)

!reduce fluxes at the bottom boundaries accordingly
        QDRAIN = QDRAIN - WPLUS/DT
        DEEPRECH = DEEPRECH - WMINUS
     ENDIF

  ENDIF

    DO K = NSOIL,2,-1
      EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
      WPLUS        = MAX((SH2O(K)-EPORE), 0.0) * DZSNSO(K)
      SH2O(K)      = MIN(EPORE,SH2O(K))
      SH2O(K-1)    = SH2O(K-1) + WPLUS/DZSNSO(K-1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(1) - SICE(1) ) )
    WPLUS        = MAX((SH2O(1)-EPORE), 0.0) * DZSNSO(1) 
    SH2O(1)      = MIN(EPORE,SH2O(1))

   IF(WPLUS > 0.0) THEN
    SH2O(2)      = SH2O(2) + WPLUS/DZSNSO(2)
    DO K = 2,NSOIL-1
      EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(K) - SICE(K) ) )
      WPLUS        = MAX((SH2O(K)-EPORE), 0.0) * DZSNSO(K)
      SH2O(K)      = MIN(EPORE,SH2O(K))
      SH2O(K+1)    = SH2O(K+1) + WPLUS/DZSNSO(K+1)
    END DO

    EPORE        = MAX ( 1.E-4 , ( parameters%SMCMAX(NSOIL) - SICE(NSOIL) ) )
    WPLUS        = MAX((SH2O(NSOIL)-EPORE), 0.0) * DZSNSO(NSOIL) 
    SH2O(NSOIL)  = MIN(EPORE,SH2O(NSOIL))
   END IF
   
    SMC = SH2O + SICE

  END SUBROUTINE SSTEP

!== begin wdfcnd1 ==================================================================================

  SUBROUTINE WDFCND1 (parameters,WDF,WCND,SMC,FCR,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input 
  type (noahmp_parameters), intent(in) :: parameters
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: FCR
    INTEGER,INTENT(IN)  :: ISOIL

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR ** EXPON
    WDF   = WDF * (1.0 - FCR)

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR ** EXPON
    WCND  = WCND * (1.0 - FCR)

  END SUBROUTINE WDFCND1

!== begin wdfcnd2 ==================================================================================

  SUBROUTINE WDFCND2 (parameters,WDF,WCND,SMC,SICE,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: SICE
    INTEGER,INTENT(IN)  :: ISOIL

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR1,FACTR2
    REAL :: VKWGT
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR1 = 0.05/parameters%SMCMAX(ISOIL)
    FACTR2 = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    FACTR1 = MIN(FACTR1,FACTR2)
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR2 ** EXPON

    IF (SICE > 0.0) THEN
    VKWGT = 1./ (1. + (500.* SICE)**3.)
    WDF   = VKWGT * WDF + (1.-VKWGT)*parameters%DWSAT(ISOIL)*(FACTR1)**EXPON
    END IF

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR2 ** EXPON

  END SUBROUTINE WDFCND2

!== begin wdfcnd3 ==================================================================================

  SUBROUTINE WDFCND3 (parameters,WDF,WCND,SMC,SICE,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: SICE
    INTEGER,INTENT(IN)  :: ISOIL

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR1
    REAL :: IMPEDF
! ----------------------------------------------------------------------

! soil water diffusivity

    FACTR1 = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = parameters%DWSAT(ISOIL) * FACTR1 ** EXPON

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = parameters%DKSAT(ISOIL) * FACTR1 ** EXPON

  END SUBROUTINE WDFCND3

!== begin wdfcnd3 ==================================================================================

  SUBROUTINE WDFCND4 (parameters,WDF,WCND,SMC,SICE,ISOIL)
! ----------------------------------------------------------------------
! calculate soil water diffusivity and soil hydraulic conductivity.
! ----------------------------------------------------------------------
    IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
    REAL,INTENT(IN)  :: SMC
    REAL,INTENT(IN)  :: SICE
    INTEGER,INTENT(IN)  :: ISOIL

! output
    REAL,INTENT(OUT) :: WCND
    REAL,INTENT(OUT) :: WDF

! local
    REAL :: EXPON
    REAL :: FACTR1
    REAL :: IMPEDF
! ----------------------------------------------------------------------

    IMPEDF = 1.0
    IF (SICE > 0.0) IMPEDF = 10.**(-6.0*SICE)

! soil water diffusivity

   !FACTR1 = MAX(0.01, SMC/parameters%SMCMAX(ISOIL))
    FACTR1 = MIN(1.0,MAX(1.E-4,SMC/(parameters%SMCMAX(ISOIL)-SICE)))
    EXPON = parameters%BEXP(ISOIL) + 2.0
    WDF   = IMPEDF * parameters%DWSAT(ISOIL) * FACTR1 ** EXPON

! hydraulic conductivity

    EXPON = 2.0*parameters%BEXP(ISOIL) + 3.0
    WCND  = IMPEDF * parameters%DKSAT(ISOIL) * FACTR1 ** EXPON

  END SUBROUTINE WDFCND4

! ==================================================================================================
  SUBROUTINE ROOTWATER(parameters, ILOC   ,JLOC     ,NSOIL  ,NSNOW  , &
                       DT     ,ZSOIL  ,ETRAN  ,PSI  ,STC    ,WCND   , &
                       DRYC   ,VEGTYP ,ROOTMS , &
                       MQ     ,KR     ,SH2O   , &
                       FROOT  ,SADR   ,QROOT  )

   IMPLICIT NONE

! inputs

  type (noahmp_parameters), intent(in) :: parameters

  INTEGER, INTENT(IN)                             :: ILOC
  INTEGER, INTENT(IN)                             :: JLOC
  INTEGER, INTENT(IN)                             :: NSNOW  !number of snow layers
  INTEGER, INTENT(IN)                             :: NSOIL  !number of soil layers
  INTEGER, INTENT(IN)                             :: VEGTYP !vegetation physiology type
  REAL   , INTENT(IN)                             :: DT     !time step [s]
  REAL   , INTENT(IN)                             :: DRYC   !dry C mass in living plant tissues [kg/m2]
  REAL   , INTENT(IN)                             :: ETRAN  !transpiration rate at each step [m/s]
  REAL   , INTENT(IN), DIMENSION(       1:NSOIL)  :: ZSOIL  !layer-bottom depth from soil surface [m]
  REAL   , INTENT(IN), DIMENSION(       1:NSOIL)  :: PSI    !suction head [m]
  REAL   , INTENT(IN), DIMENSION(-NSNOW+1:NSOIL)  :: STC    !soil temperature [k]
  REAL   , INTENT(IN), DIMENSION(       1:NSOIL)  :: ROOTMS       !mass of live fine roots [g C/m2]
  REAL   , INTENT(IN), DIMENSION(       1:NSOIL)  :: WCND         !hydraulic conductivity [m/s]

  REAL  , INTENT(INOUT)                           :: KR     !controls when to grow [-]
  REAL  , INTENT(INOUT)                           :: MQ     !water in plant tissues [kg]
  REAL  , INTENT(INOUT), DIMENSION(1:NSOIL)       :: SH2O   !soil water content [m3/m3]

! outputs

  REAL   , INTENT(OUT)  , DIMENSION(1:NSOIL)      :: FROOT  !root fraction
  REAL   , INTENT(OUT)  , DIMENSION(1:NSOIL)      :: QROOT  !water uptake [m/s]
  REAL   , INTENT(OUT)  , DIMENSION(1:NSOIL)      :: SADR   !root surface area density [m2/m3]

! locals

  INTEGER                  :: IZ           !do-loop index in z-direction 
  INTEGER                  :: ID           !do-loop index in diurnal time steps
  REAL                     :: MDRY         !dry mass in lviing tissue [kg/m2]
  REAL                     :: PB           !tissue balance pressure [bar]
  REAL                     :: SUMQR        !total water uptake per unit ground area [m/s]
  REAL                     :: MQMIN        !daily minimum of water in plant tissues [kg/m2] 
  REAL, DIMENSION(1:NSOIL) :: HR           !Root suction head [m]
  REAL, DIMENSION(1:NSOIL) :: OMS          !resistivity to water flow to root surface [s]
  REAL, DIMENSION(1:NSOIL) :: JR           !water uptake per unit root surface area [m/s]
  REAL, DIMENSION(1:NSOIL) :: DZ           !layer thickness [m]
  REAL, DIMENSION(1:NSOIL) :: ZNODE        !depth of the middle of each layer (node depth) [m]
  REAL, DIMENSION(1:NSOIL) :: WEIGHT       !weight of carbon partitioned into soil layers [-]
  REAL, DIMENSION(1:NSOIL) :: GX           !soil moisture factor for carbon partition [-]
  REAL                     :: SUMSADR      !total root surface area density [m2/m3]

  REAL, PARAMETER          :: C1     = 750.      !empirical constant [bar]
  REAL, PARAMETER          :: C2     = 1.        !empirical constant [bar]

  REAL, PARAMETER          :: PI     = 4*atan(1.0)
  REAL, PARAMETER          :: CPBM   = 10.2      !elevation head per 1 atm pressure [10.2 m/bar]
  REAL, PARAMETER          :: DENH2O = 1000.     !density of water [kg/m3]
  REAL, PARAMETER          :: PSIWLT = -306.     !suction head at wilting point [m] [~ -30 bar]
  REAL, PARAMETER          :: RM2CM  = 2.5       !2.375 = 1.+(22.*1.+11.*16.)/(12.*12.)  !sugar: C12 H22 O11
                                                 !ratio of total mass to carbon mass

  REAL ::  MQMAX
  REAL ::  ZROOT
  REAL ::  RWILT                                 !wilting point (in relative water storage; (1-RWLT) -> maximum raltive water loss)
  REAL ::  PBD                                   !leaf pressure when leaves with no water

!--------------------------------------------------------------------------------------------------------------    
     QROOT = 0.

! layer thikcness [m] and node (middle) depth in [m]

     DZ(1) = -ZSOIL(1)
     DO IZ = 2, NSOIL
        DZ(IZ)  = (ZSOIL(IZ-1) - ZSOIL(IZ))
     ENDDO

     ZNODE(1) = -ZSOIL(1) * 0.5
     DO IZ = 2, NSOIL
        ZNODE(IZ)  = -ZSOIL(IZ-1) + 0.5 * DZ(IZ)
     ENDDO

! root surface area density or root area index (RAI m2/m2) per unit depth [m2/m3]

     ZROOT = 0.
     DO IZ = 1, parameters%NROOT
       ZROOT     = ZROOT + DZ(IZ)
       SADR(IZ)  = ROOTMS(IZ)/DZ(IZ)/1000.*parameters%SRA  !g/m2 C -> kg/m2 C -> m2/m3 active roots
     ENDDO

! maximum water storage in plant tissues

     MDRY  = DRYC * RM2CM
     MQMAX = parameters%MQX * MDRY      !kg/m2
                                                        !to reach steady state)
    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) "MQMAX,MDRY =",MQMAX,MDRY
    !END IF

! permanent wilting point (to be consistent with Noah prescribed SMCWLT )

     PBD      = MAX(0., MQMAX*(C1*MDRY/(MDRY+MQMAX)**2+C2/MQMAX))     ! when leaves totally dry

! plant water availability for transpiration (=BTRAN)

     RWILT = 1.0 - 30.*(1.+parameters%MQX)**2./(C1*parameters%MQX)  ![30 bar]
    !KR    = MAX(1.E-6,MIN(1.0,(MQ-RWILT*MQMAX)/((1.-RWILT)*MQMAX)))
     KR    = MIN(1.0,MAX(1.E-6,(MAX(MQ,RWILT*MQMAX)-RWILT*MQMAX)/((1.-RWILT)*MQMAX)))

! plant water potential [bar]

    !PB    = MIN(750.,MAX(0.,(MQMAX-MQ)*(C1*MDRY/(MDRY+MQMAX)**2)))
     PB    = MIN(750.,MAX(0.,(MQMAX-MAX(MQ,RWILT*MQMAX))*(C1*MDRY/(MDRY+MQMAX)**2)))

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) "PB=",PB
    ! write(*,*) "MQ=",MQ
    ! write(*,*) "RWILT=",RWILT
    ! write(*,*) "MQMAX=",MQMAX
    ! write(*,*) "RWILT*MQMAX=",RWILT*MQMAX
    ! write(*,*) "ETRAN=",ETRAN
    ! write(*,*) "MDRY=",MDRY
    !END IF

! changes in liquid water in plant tissues in each root layer

     SUMQR = 0.0
     DO IZ = 1,parameters%NROOT
         HR(IZ)    = -CPBM*PB - ZNODE(IZ) ![m]
         OMS(IZ)   = MIN(1.0E20,1.0/WCND(IZ)*(PI*parameters%RROOT*1.0E-3/(2.0*SADR(IZ)))**0.5)

        !JR(IZ)    = ((PSI(IZ)-ZNODE(IZ))-HR(IZ)) / (parameters%OMR+OMS(IZ))
         JR(IZ)    = MAX(0.,((PSI(IZ)-ZNODE(IZ))-HR(IZ)) / (parameters%OMR+OMS(IZ)))

         IF(PSI(IZ) <= PSIWLT) JR(IZ) = 0.     ! much < -300 m for some soils

         QROOT(IZ) = MIN((SH2O(IZ)-0.0001)*DZ(IZ)/DT,SADR(IZ)*DZ(IZ)*JR(IZ))

         QROOT(IZ) = MIN((MQMAX-MQ)/DT*DZ(IZ)/ZROOT/1000.,QROOT(IZ))  !ensure MQ<MQMAX

         IF(MQ <= 0.1*MQMAX .and. QROOT(IZ) < 0.0) QROOT(IZ) = 0.0 !liquid water pressure is too low

         SUMQR     = SUMQR + QROOT(IZ)

     ENDDO

     MQ  = MQ + DENH2O*(SUMQR-ETRAN*1.0E-3)*DT                  !no bound for water balance (can be '-')

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    ! write(*,*) "MQMAX =",MQMAX
    ! write(*,*) "QROOT*DT*1000. =",QROOT*DT*1000.
    ! write(*,*) "JR*DT*1000.=",JR*DT*1000.
    ! write(*,*) "PSI-ZNODE=",PSI-ZNODE
    ! write(*,*) "HR       =",HR
    ! write(*,*) "OMS      =",OMS
    ! write(*,*) "PB=",PB
    ! write(*,*) "MQ=",MQ
    !END IF

! soil moisture updates

     SUMSADR = 0.
     DO IZ = 1, parameters%NROOT
         SUMSADR = SUMSADR+SADR(IZ)*DZ(IZ) !root area index (m2/m2)
     ENDDO

     DO IZ = 1, parameters%NROOT
         FROOT(IZ) = (SADR(IZ)*DZ(IZ))/SUMSADR
         IF(OPT_RUN /= 6) SH2O(IZ)  = SH2O(IZ) - QROOT(IZ)*DT/DZ(IZ)      !m/s
     ENDDO

     END SUBROUTINE ROOTWATER
! ==================================================================================================

!== begin groundwater ==============================================================================

  SUBROUTINE GROUNDWATER(parameters,NSNOW  ,NSOIL  ,DT     ,SICE   ,ZSOIL  , & !in
                         STC    ,WCND   ,FCRMAX ,ILOC   ,JLOC   , & !in
                         SH2O   ,ZWT    ,WA     ,WT     ,         & !inout
                         QIN    ,QDIS   )                           !out
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN) :: ILOC  !grid index
  INTEGER,                         INTENT(IN) :: JLOC  !grid index
  INTEGER,                         INTENT(IN) :: NSNOW !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL !no. of soil layers
  REAL,                            INTENT(IN) :: DT    !timestep [sec]
  REAL,                            INTENT(IN) :: FCRMAX!maximum FCR (-)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE  !soil ice content [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL !depth of soil layer-bottom [m]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: WCND  !hydraulic conductivity (m/s)
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC   !snow/soil temperature (k)

! input and output
  REAL, DIMENSION(    1:NSOIL), INTENT(INOUT) :: SH2O  !liquid soil water [m3/m3]
  REAL,                         INTENT(INOUT) :: ZWT   !the depth to water table [m]
  REAL,                         INTENT(INOUT) :: WA    !water storage in aquifer [mm]
  REAL,                         INTENT(INOUT) :: WT    !water storage in aquifer 
                                                           !+ saturated soil [mm]
! output
  REAL,                           INTENT(OUT) :: QIN   !groundwater recharge [mm/s]
  REAL,                           INTENT(OUT) :: QDIS  !groundwater discharge [mm/s]

! local
  REAL                                        :: FFF   !runoff decay factor (m-1)
  REAL                                        :: RSBMX !baseflow coefficient [mm/s]
  INTEGER                                     :: IZ    !do-loop index
  INTEGER                                     :: IWT   !layer index above water table layer
  REAL,  DIMENSION(    1:NSOIL)               :: DZMM  !layer thickness [mm]
  REAL,  DIMENSION(    1:NSOIL)               :: ZNODE !node depth [m]
  REAL,  DIMENSION(    1:NSOIL)               :: MLIQ  !liquid water mass [kg/m2 or mm]
  REAL,  DIMENSION(    1:NSOIL)               :: EPORE !effective porosity [-]
  REAL,  DIMENSION(    1:NSOIL)               :: HK    !hydraulic conductivity [mm/s]
  REAL,  DIMENSION(    1:NSOIL)               :: SMC   !total soil water  content [m3/m3]
  REAL(KIND=8)                                :: S_NODE!degree of saturation of IWT layer
  REAL                                        :: DZSUM !cumulative depth above water table [m]
  REAL                                        :: SMPFZ !matric potential (frozen effects) [mm]
  REAL                                        :: KA    !aquifer hydraulic conductivity [mm/s]
  REAL                                        :: WH_ZWT!water head at water table [mm]
  REAL                                        :: WH    !water head at layer above ZWT [mm]
  REAL                                        :: WS    !water used to fill air pore [mm]
  REAL                                        :: WTSUB !sum of HK*DZMM
  REAL                                        :: WATMIN!minimum soil vol soil moisture [m3/m3]
  REAL                                        :: XS    !excessive water above saturation [mm]
  REAL, PARAMETER                             :: ROUS = 0.2    !specific yield [-]
! REAL, PARAMETER                             :: CMIC = 0.20   !microprore content (0.0-1.0)
  REAL                                        :: CMIC != 0.20  !microprore content (0.0-1.0)
                                                               !0.0-close to free drainage
  REAL :: DZ1, DZ2
! -------------------------------------------------------------
      QDIS      = 0.0
      QIN       = 0.0

! Derive layer-bottom depth in [mm]
!KWM:  Derive layer thickness in mm

      DZMM(1) = -ZSOIL(1)*1.E3
      DO IZ = 2, NSOIL
         DZMM(IZ)  = 1.E3 * (ZSOIL(IZ - 1) - ZSOIL(IZ))
      ENDDO

! Derive node (middle) depth in [m]
!KWM:  Positive number, depth below ground surface in m
      ZNODE(1) = -ZSOIL(1) / 2.
      DO IZ = 2, NSOIL
         ZNODE(IZ)  = -ZSOIL(IZ-1) + 0.5 * (ZSOIL(IZ-1) - ZSOIL(IZ))
      ENDDO

! Convert volumetric soil moisture "sh2o" to mass

      DO IZ = 1, NSOIL
         SMC(IZ)      = SH2O(IZ) + SICE(IZ)
         MLIQ(IZ)     = SH2O(IZ) * DZMM(IZ)
         EPORE(IZ)    = MAX(0.01,parameters%SMCMAX(IZ) - SICE(IZ))
         HK(IZ)       = 1.E3*WCND(IZ)
      ENDDO

! The layer index of the first unsaturated layer,
! i.e., the layer right above the water table

      IWT = NSOIL
      DO IZ = 2,NSOIL
         IF(ZWT   .LE. -ZSOIL(IZ) ) THEN
            IWT = IZ-1
            EXIT
         END IF
      ENDDO

! Groundwater discharge [mm/s]

      IF(OPT_WATRET == 1) THEN
         FFF    = 0.3/(1.-1./parameters%VGN(IWT))
         RSBMX  = HK(IWT)*1.E3*EXP(5.0)  ! mm/s
        !CMIC   = 1.0
         CMIC   = 0.1

        !Hamornic average
        !KA  = 2.*(HK(IWT)*parameters%DKSAT(IWT)*1.E3)/(HK(IWT)+parameters%DKSAT(IWT)*1.E3)

        !Reciprocal-distance-sqaured average 

         DZ1 = 0.5
         DZ2 = MAX(0.0001,-WH_ZWT/1000.)
         KA  = (HK(IWT)/DZ1**2.0 + parameters%DKSAT(IWT)*1.E3/DZ2**2.0)/(1./DZ1**2.0 + 1./DZ2**2.0)

      END IF

      IF(OPT_WATRET == 2) THEN
         FFF   = parameters%BEXP(IWT)/3.
         RSBMX  = HK(IWT)*1.E3*EXP(4.0)  ! mm/s
         CMIC   = 0.5

        !Hamornic average
         KA  = 2.*(HK(IWT)*parameters%DKSAT(IWT)*1.E3)/(HK(IWT)+parameters%DKSAT(IWT)*1.E3)

        !arithmic average
        !KA  = (HK(IWT)+parameters%DKSAT(IWT)*1.E3) * 0.5
      END IF

!      QDIS = (1.0-FCRMAX)*RSBMX*EXP(-parameters%TIMEAN)*EXP(-FFF*(ZWT-2.0))
      QDIS = (1.0-FCRMAX)*RSBMX*EXP(-parameters%TIMEAN)*EXP(-FFF*ZWT)

! Matric potential at the layer above the water table

      IF(OPT_WATRET == 1) THEN
      S_NODE = (MAX(parameters%SMCR(IWT)+0.001,SMC(IWT))-parameters%SMCR(IWT))/(parameters%SMCMAX(IWT)-parameters%SMCR(IWT))
      S_NODE = MAX(REAL(0.01,KIND=8),MIN(1.0,S_NODE))
     !SMPFZ  =  -parameters%VGPSAT(IWT)*1000.*(S_NODE**(-1.0/parameters%VGM(IWT))-1.0)**(1.-parameters%VGM(IWT))
      SMPFZ  =  -parameters%VGPSAT(IWT)*1000.*(S_NODE**(-1.0/(1.-1./parameters%VGN(IWT)))-1.0)**(1./parameters%VGN(IWT))
      WH_ZWT = - (ZWT-parameters%VGPSAT(IWT))     * 1.E3             !(mm)
      END IF

      IF(OPT_WATRET == 2) THEN
      S_NODE = MIN(1.0,SMC(IWT)/parameters%SMCMAX(IWT) )
      S_NODE = MAX(S_NODE,REAL(0.01,KIND=8))
      SMPFZ  = -parameters%PSISAT(IWT)*1000.*S_NODE**(-parameters%BEXP(IWT))   ! m --> mm
      WH_ZWT =  - (ZWT-parameters%PSISAT(IWT))     * 1.E3             !(mm)
      END IF
 
      SMPFZ  = MAX(-750000.0,CMIC*SMPFZ)   

! Recharge rate qin to groundwater

!     KA  = HK(IWT)


      WH      = SMPFZ  - ZNODE(IWT)*1.E3              !(mm)
      QIN     = - KA * (WH_ZWT-WH)  /((ZWT-ZNODE(IWT))*1.E3)
      QIN     = MAX(-10.0/DT,MIN(10./DT,QIN))

! Water storage in the aquifer + saturated soil

      WT  = WT + (QIN - QDIS) * DT     !(mm)

      IF(IWT.EQ.NSOIL) THEN
         WA          = WA + (QIN - QDIS) * DT     !(mm)
         WT          = WA
         ZWT         = (-ZSOIL(NSOIL) + 25.) - WA/1000./ROUS      !(m)
         MLIQ(NSOIL) = MLIQ(NSOIL) - QIN * DT        ! [mm]

         MLIQ(NSOIL) = MLIQ(NSOIL) + MAX(0.,(WA - 5000.))
         WA          = MIN(WA, 5000.)
      ELSE
         
         IF (IWT.EQ.NSOIL-1) THEN
            ZWT = -ZSOIL(NSOIL)                   &
                 - (WT-ROUS*1000*25.) / (EPORE(NSOIL))/1000.
         ELSE
            WS = 0.   ! water used to fill soil air pores
            DO IZ = IWT+2,NSOIL
               WS = WS + EPORE(IZ) * DZMM(IZ)
            ENDDO
            ZWT = -ZSOIL(IWT+1)                  &
                  - (WT-ROUS*1000.*25.-WS) /(EPORE(IWT+1))/1000.
         ENDIF

         WTSUB = 0.
         DO IZ = 1, NSOIL
           WTSUB = WTSUB + HK(IZ)*DZMM(IZ)
         END DO

         DO IZ = 1, NSOIL           ! Removing subsurface runoff
         MLIQ(IZ) = MLIQ(IZ) - QDIS*DT*HK(IZ)*DZMM(IZ)/WTSUB
         END DO
      END IF

      ZWT = MAX(1.5,ZWT)

! Limit MLIQ to be greater than or equal to watmin.
! Get water needed to bring MLIQ equal WATMIN from lower layer.
!
      WATMIN = 0.01
      DO IZ = 1, NSOIL-1
          IF (MLIQ(IZ) .LT. 0.) THEN
             XS = WATMIN-MLIQ(IZ)
          ELSE
             XS = 0.
          END IF
          MLIQ(IZ  ) = MLIQ(IZ  ) + XS
          MLIQ(IZ+1) = MLIQ(IZ+1) - XS
      END DO

        IZ = NSOIL
        IF (MLIQ(IZ) .LT. WATMIN) THEN
           XS = WATMIN-MLIQ(IZ)
        ELSE
           XS = 0.
        END IF
        MLIQ(IZ) = MLIQ(IZ) + XS
        WA       = WA - XS
        WT       = WT - XS

      DO IZ = 1, NSOIL
        SH2O(IZ)     = MLIQ(IZ) / DZMM(IZ)
      END DO

  END SUBROUTINE GROUNDWATER

!== begin shallowwatertable ========================================================================

  SUBROUTINE SHALLOWWATERTABLE (parameters,NSNOW  ,NSOIL  ,ZSOIL, DT    , & !in
                         DZSNSO ,SMCEQ ,ILOC   ,JLOC         , & !in
                         SMC    ,WTD   ,SMCWTD ,RECH, QDRAIN  )  !inout
! ----------------------------------------------------------------------
!Diagnoses water table depth and computes recharge when the water table is within the resolved soil layers,
!according to the Miguez-Macho&Fan scheme
! ----------------------------------------------------------------------
  IMPLICIT NONE
! ----------------------------------------------------------------------
! input
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER,                         INTENT(IN) :: NSNOW !maximum no. of snow layers
  INTEGER,                         INTENT(IN) :: NSOIL !no. of soil layers
  INTEGER,                         INTENT(IN) :: ILOC,JLOC
  REAL,                            INTENT(IN) :: DT
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL !depth of soil layer-bottom [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO ! snow/soil layer thickness [m]
  REAL,  DIMENSION(      1:NSOIL), INTENT(IN) :: SMCEQ  !equilibrium soil water  content [m3/m3]

! input and output
  REAL,  DIMENSION(      1:NSOIL), INTENT(INOUT) :: SMC   !total soil water  content [m3/m3]
  REAL,                         INTENT(INOUT) :: WTD   !the depth to water table [m]
  REAL,                         INTENT(INOUT) :: SMCWTD   !soil moisture between bottom of the soil and the water table [m3/m3]
  REAL,                         INTENT(OUT) :: RECH ! groundwater recharge (net vertical flux across the water table), positive up
  REAL,                         INTENT(INOUT) :: QDRAIN
    
! local
  INTEGER                                     :: IZ    !do-loop index
  INTEGER                                     :: IWTD   !layer index above water table layer
  INTEGER                                     :: KWTD   !layer index where the water table layer is
  REAL                                        :: WTDOLD
  REAL                                        :: DZUP
  REAL                                        :: SMCEQDEEP
  REAL,  DIMENSION(       0:NSOIL)            :: ZSOIL0
! -------------------------------------------------------------


ZSOIL0(1:NSOIL) = ZSOIL(1:NSOIL)
ZSOIL0(0) = 0.         
 
!find the layer where the water table is
     DO IZ=NSOIL,1,-1
        IF(WTD + 1.E-6 < ZSOIL0(IZ)) EXIT
     ENDDO
        IWTD=IZ

        
        KWTD=IWTD+1  !layer where the water table is
        IF(KWTD.LE.NSOIL)THEN    !wtd in the resolved layers
           WTDOLD=WTD
           IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
        
               IF(SMC(KWTD).EQ.parameters%SMCMAX(KWTD))THEN !wtd went to the layer above
                      WTD=ZSOIL0(IWTD)
                      RECH=-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
                      IWTD=IWTD-1
                      KWTD=KWTD-1
                   IF(KWTD.GE.1)THEN
                      IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
                      WTDOLD=WTD
                      WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                        - SMCEQ(KWTD)*ZSOIL0(IWTD) + parameters%SMCMAX(KWTD)*ZSOIL0(KWTD) ) / &
                        ( parameters%SMCMAX(KWTD)-SMCEQ(KWTD) ), ZSOIL0(IWTD))
                      RECH=RECH-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
                      ENDIF
                   ENDIF
               ELSE  !wtd stays in the layer
                      WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                        - SMCEQ(KWTD)*ZSOIL0(IWTD) + parameters%SMCMAX(KWTD)*ZSOIL0(KWTD) ) / &
                        ( parameters%SMCMAX(KWTD)-SMCEQ(KWTD) ), ZSOIL0(IWTD))
                      RECH=-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
               ENDIF
           
           ELSE    !wtd has gone down to the layer below
               WTD=ZSOIL0(KWTD)
               RECH=-(WTDOLD-WTD) * (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))
               KWTD=KWTD+1
               IWTD=IWTD+1
!wtd crossed to the layer below. Now adjust it there
               IF(KWTD.LE.NSOIL)THEN
                   WTDOLD=WTD
                   IF(SMC(KWTD).GT.SMCEQ(KWTD))THEN
                   WTD = MIN( ( SMC(KWTD)*DZSNSO(KWTD) &
                   - SMCEQ(KWTD)*ZSOIL0(IWTD) + parameters%SMCMAX(KWTD)*ZSOIL0(KWTD) ) / &
                       ( parameters%SMCMAX(KWTD)-SMCEQ(KWTD) ) , ZSOIL0(IWTD) )
                   ELSE
                   WTD=ZSOIL0(KWTD)
                   ENDIF
                   RECH = RECH - (WTDOLD-WTD) * &
                                 (parameters%SMCMAX(KWTD)-SMCEQ(KWTD))

                ELSE
                   WTDOLD=WTD
!restore smoi to equilibrium value with water from the ficticious layer below
!                   SMCWTD=SMCWTD-(SMCEQ(NSOIL)-SMC(NSOIL))
!                   QDRAIN = QDRAIN - 1000 * (SMCEQ(NSOIL)-SMC(NSOIL)) * DZSNSO(NSOIL) / DT
!                   SMC(NSOIL)=SMCEQ(NSOIL)
!adjust wtd in the ficticious layer below
                   SMCEQDEEP = parameters%SMCMAX(NSOIL) * ( -parameters%PSISAT(NSOIL) / ( -parameters%PSISAT(NSOIL) - DZSNSO(NSOIL) ) ) ** (1./parameters%BEXP(NSOIL))
                   WTD = MIN( ( SMCWTD*DZSNSO(NSOIL) &
                   - SMCEQDEEP*ZSOIL0(NSOIL) + parameters%SMCMAX(NSOIL)*(ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) / &
                       ( parameters%SMCMAX(NSOIL)-SMCEQDEEP ) , ZSOIL0(NSOIL) )
                   RECH = RECH - (WTDOLD-WTD) * &
                                 (parameters%SMCMAX(NSOIL)-SMCEQDEEP)
                ENDIF
            
            ENDIF
        ELSEIF(WTD.GE.ZSOIL0(NSOIL)-DZSNSO(NSOIL))THEN
!if wtd was already below the bottom of the resolved soil crust
           WTDOLD=WTD
           SMCEQDEEP = parameters%SMCMAX(NSOIL) * ( -parameters%PSISAT(NSOIL) / ( -parameters%PSISAT(NSOIL) - DZSNSO(NSOIL) ) ) ** (1./parameters%BEXP(NSOIL))
           IF(SMCWTD.GT.SMCEQDEEP)THEN
               WTD = MIN( ( SMCWTD*DZSNSO(NSOIL) &
                 - SMCEQDEEP*ZSOIL0(NSOIL) + parameters%SMCMAX(NSOIL)*(ZSOIL0(NSOIL)-DZSNSO(NSOIL)) ) / &
                     ( parameters%SMCMAX(NSOIL)-SMCEQDEEP ) , ZSOIL0(NSOIL) )
               RECH = -(WTDOLD-WTD) * (parameters%SMCMAX(NSOIL)-SMCEQDEEP)
           ELSE
               RECH = -(WTDOLD-(ZSOIL0(NSOIL)-DZSNSO(NSOIL))) * (parameters%SMCMAX(NSOIL)-SMCEQDEEP)
               WTDOLD=ZSOIL0(NSOIL)-DZSNSO(NSOIL)
!and now even further down
               DZUP=(SMCEQDEEP-SMCWTD)*DZSNSO(NSOIL)/(parameters%SMCMAX(NSOIL)-SMCEQDEEP)
               WTD=WTDOLD-DZUP
               RECH = RECH - (parameters%SMCMAX(NSOIL)-SMCEQDEEP)*DZUP
               SMCWTD=SMCEQDEEP
           ENDIF

         
         ENDIF

IF(IWTD.LT.NSOIL .AND. IWTD.GT.0) THEN
  SMCWTD=parameters%SMCMAX(IWTD)
ELSEIF(IWTD.LT.NSOIL .AND. IWTD.LE.0) THEN
  SMCWTD=parameters%SMCMAX(1)
END IF

END  SUBROUTINE SHALLOWWATERTABLE

! ==================================================================================================
! ********************* end of water subroutines ******************************************
! ==================================================================================================

!== begin carbon ===================================================================================
   SUBROUTINE CARBON (parameters, NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  , & !in
                 DZSNSO ,STC    ,SMC    ,TV     ,TG     ,PSN    , & !in
                 FOLN   ,BTRAN  ,APAR   ,FVEG   ,IGS    , & !in
                 TROOT  ,IST    ,LAT    ,SH2OO  ,SICEO  , & !in 
                 SH2O   ,SICE   ,KR     , & !in
                 ILOC   ,JLOC   ,PSI    ,ETRAN  ,O2AIR  , & !in
                 LFMASS ,ROOTMS ,STMASS ,WOOD   ,SOC    ,WDOC   , & !inout
                 DDOC   ,MIC    ,WENZ   ,DENZ   ,MQ     , & !inout
                 SADR   ,RTMASS ,FASTCP ,STBLCP , & !inout
                 GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  , & !out
                 TOTLB  ,XLAI   ,XSAI   ,QCO2   ,VMAX   ,KM     , & !out
                 VMAXUP ,KMUP   ,EPSLON ,FROOT  )   !out

!  SUBROUTINE CARBON (parameters,NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  , & !in
!                     DZSNSO ,STC    ,SMC    ,TV     ,TG     ,PSN    , & !in
!                     FOLN   ,BTRAN  ,APAR   ,FVEG   ,IGS    , & !in
!                     TROOT  ,IST    ,LAT    ,ILOC   ,JLOC   , & !in
!                     LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP , & !inout
!                     GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  , & !out
!                     TOTLB  ,XLAI   ,XSAI   )                   !out
! ------------------------------------------------------------------------------------------
      IMPLICIT NONE
! ------------------------------------------------------------------------------------------
! inputs (carbon)

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER                        , INTENT(IN) :: ILOC   !grid index
  INTEGER                        , INTENT(IN) :: JLOC   !grid index
  INTEGER                        , INTENT(IN) :: VEGTYP !vegetation type 
  INTEGER                        , INTENT(IN) :: NSNOW  !number of snow layers
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  REAL                           , INTENT(IN) :: LAT    !latitude (radians)
  REAL                           , INTENT(IN) :: DT     !time step (s)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL  !depth of layer-bottom from soil surface
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !snow/soil temperature [k]
  REAL                           , INTENT(IN) :: TV     !vegetation temperature (k)
  REAL                           , INTENT(IN) :: TG     !ground temperature (k)
  REAL                           , INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL                           , INTENT(IN) :: BTRAN  !soil water transpiration factor (0 to 1)
  REAL                           , INTENT(IN) :: PSN    !total leaf photosyn (umolco2/m2/s) [+]
  REAL                           , INTENT(IN) :: APAR   !PAR by canopy (w/m2)
  REAL                           , INTENT(IN) :: IGS    !growing season index (0=off, 1=on)
  REAL                           , INTENT(IN) :: FVEG   !vegetation greenness fraction
  REAL                           , INTENT(IN) :: TROOT  !root-zone averaged temperature (k)
  INTEGER                        , INTENT(IN) :: IST    !surface type 1->soil; 2->lake

  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE   !soil ice at present time [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SH2OO  !soil liq at previous time [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICEO  !soil ice at previous time [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: PSI    !suction head [m]
  REAL                           , INTENT(IN) :: ETRAN  !transpiration rate at each step [m/s]
  REAL                           , INTENT(IN) :: O2AIR  !o2 (umol/mol)

! input & output (carbon)

  REAL                        , INTENT(INOUT) :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT) :: RTMASS !mass of fine roots [g/m2]
  REAL                        , INTENT(INOUT) :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT) :: WOOD   !mass of wood (incl. woody roots) [g/m2]
  REAL                        , INTENT(INOUT) :: STBLCP !stable carbon in deep soil [g/m2]
  REAL                        , INTENT(INOUT) :: FASTCP !short-lived carbon in shallow soil [g/m2]

  REAL                        , INTENT(INOUT) :: SOC    !soil organic carbon [g C/m2]
  REAL                        , INTENT(INOUT) :: WDOC   !wet dissolved organic carbon [g C/m2]
  REAL                        , INTENT(INOUT) :: DDOC   !dry dissolved organic  [g C/m2]
  REAL                        , INTENT(INOUT) :: MIC    !soil microbial biomass [g C/m2]
  REAL                        , INTENT(INOUT) :: WENZ   !wet soil enzyme [g C/m2]
  REAL                        , INTENT(INOUT) :: DENZ   !dry soil enzyme [g C/m2]
  REAL, DIMENSION(    1:NSOIL), INTENT(INOUT) :: SH2O !soil liq at present time [m3/m3]
  REAL                        , INTENT(INOUT) :: KR   !
  REAL                        , INTENT(INOUT) :: MQ   !water stored in living tissues [mm]
  REAL, DIMENSION(    1:NSOIL), INTENT(INOUT) :: ROOTMS       !mass of live fine roots [g C/m2]

! outputs: (carbon)

  REAL                          , INTENT(OUT) :: GPP    !net instantaneous assimilation [g/m2/s C]
  REAL                          , INTENT(OUT) :: NPP    !net primary productivity [g/m2/s C]
  REAL                          , INTENT(OUT) :: NEE    !net ecosystem exchange [g/m2/s CO2]
  REAL                          , INTENT(OUT) :: AUTORS !net ecosystem respiration [g/m2/s C]
  REAL                          , INTENT(OUT) :: HETERS !organic respiration [g/m2/s C]
  REAL                          , INTENT(OUT) :: TOTSC  !total soil carbon [g/m2 C]
  REAL                          , INTENT(OUT) :: TOTLB  !total living carbon ([g/m2 C]
  REAL                          , INTENT(OUT) :: XLAI   !leaf area index [-]
  REAL                          , INTENT(OUT) :: XSAI   !stem area index [-]
!  REAL                          , INTENT(OUT) :: VOCFLX(5) ! voc fluxes [ug C m-2 h-1]

  REAL, DIMENSION(      1:NSOIL), INTENT(OUT) :: FROOT  !root fraction
  REAL, DIMENSION(      1:NSOIL), INTENT(OUT) :: SADR   !root surface area density [m2/m3]
  REAL                          , INTENT(OUT) :: QCO2   !co2 efflux ( g C m-2 s-1)
  real                          , INTENT(OUT) :: VMAX   !maximum SOC decomposition rate per
                                                        !unit microbial biomass [g C m-2 [g CMIC m-2]-1 s-1]
  real                          , INTENT(OUT) :: VMAXUP !maximum DOC uptake rate [g CDOC m-2 [g CMIC m-2]-1 s-1]
  real                          , INTENT(OUT) :: KM     !Michaelis-Menten constant [g C m-2] for SOC  decomposition
  real                          , INTENT(OUT) :: KMUP   !Michaelis-Menten constant [g C m-2] for DOC uptake
  real                          , INTENT(OUT) :: EPSLON !carbon use efficiency

! local variables

  INTEGER :: J         !do-loop index
  REAL    :: WROOT     !root zone soil water [-]
  REAL    :: WSTRES    !water stress coeficient [-]  (1. for wilting )
  REAL    :: LAPM      !leaf area per unit mass [m2/g]
! ------------------------------------------------------------------------------------------

   IF ( ( VEGTYP == parameters%iswater ) .OR. ( VEGTYP == parameters%ISBARREN ) .OR. &
        ( VEGTYP == parameters%ISICE )) THEN        !niu (urban removed)
      XLAI   = 0.
      XSAI   = 0.
      GPP    = 0.
      NPP    = 0.
      NEE    = 0.
      AUTORS = 0.
      HETERS = 0.
      TOTSC  = 0.
      TOTLB  = 0.
      LFMASS = 0.
      RTMASS = 0.
      STMASS = 0.
      WOOD   = 0.
      STBLCP = 0.
      FASTCP = 0.
      QCO2   = 0.

      RETURN
   END IF

      LAPM       = parameters%SLA / 1000.   ! m2/kg -> m2/g

! water stress

      WROOT  = 0.
      DO J=1,parameters%NROOT
       !WROOT = WROOT + SMC(J)/parameters%SMCMAX(J) *  DZSNSO(J) / (-ZSOIL(parameters%NROOT))
        WROOT = WROOT + SMC(J) *  DZSNSO(J) / (-ZSOIL(parameters%NROOT)) !
      ENDDO

      CALL CO2FLUX (parameters, NSNOW  ,NSOIL  ,VEGTYP ,IGS    ,DT     , & !in
                    DZSNSO ,STC    ,PSN    ,TROOT  ,TV     , & !in
                    WROOT  ,FOLN   ,LAPM   , & !in
                    LAT    ,FVEG   ,SMC    ,SH2OO  ,SICEO  , & !in
                    SH2O   ,SICE   ,BTRAN  , & !in
                    ILOC   ,JLOC   ,ETRAN  , & !in
                    ZSOIL  ,PSI    ,O2AIR  , & !in
                    XLAI   ,XSAI   ,LFMASS ,ROOTMS ,STMASS , & !inout
                    WOOD   ,SOC    ,WDOC   ,MQ     ,FROOT  , & !inout
                    DDOC   ,MIC    ,WENZ   ,DENZ   ,KR     , & !inout
                    SADR   ,RTMASS ,FASTCP ,STBLCP , & !inout
                    GPP    ,NPP    ,NEE    ,AUTORS ,HETERS , & !out
                    VMAX   ,KM     ,VMAXUP ,KMUP   ,EPSLON , & !out
                    TOTSC  ,TOTLB  ,QCO2   )           !out

!   CALL BVOC (parameters,VOCFLX,  VEGTYP,  VEGFAC,   APAR,   TV)
!   CALL CH4

  END SUBROUTINE CARBON

!== begin co2flux ==================================================================================

  SUBROUTINE CO2FLUX (parameters, NSNOW  ,NSOIL  ,VEGTYP ,IGS    ,DT     , & !in
                      DZSNSO ,STC    ,PSN    ,TROOT  ,TV     , & !in
                      WROOT  ,FOLN   ,LAPM   , & !in
                      LAT    ,FVEG   ,SMC    ,SH2OO  ,SICEO  , & !in
                      SH2O   ,SICE   ,BTRAN  , & !in
                      ILOC   ,JLOC   ,ETRAN  , & !in
                      ZSOIL  ,PSI    ,O2AIR  , & !in
                      XLAI   ,XSAI   ,LFMASS ,ROOTMS ,STMASS , & !inout
                      WOOD   ,SOC    ,WDOC   ,MQ     ,FROOT  , & !inout
                      DDOC   ,MIC    ,WENZ   ,DENZ   ,KR     , & !inout
                      SADR   ,RTMASS ,FASTCP ,STBLCP , & !inout
                      GPP    ,NPP    ,NEE    ,AUTORS ,HETERS , & !out
                      VMAX   ,KM     ,VMAXUP ,KMUP   ,EPSLON , & !out
                      TOTSC  ,TOTLB  ,QCO2   )   !out

! -----------------------------------------------------------------------------------------
! The original code is from RE Dickinson et al.(1998), modifed by Guo-Yue Niu, 2004
! -----------------------------------------------------------------------------------------
  IMPLICIT NONE
! -----------------------------------------------------------------------------------------

! input

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER                        , INTENT(IN) :: ILOC   !grid index
  INTEGER                        , INTENT(IN) :: JLOC   !grid index
  INTEGER                        , INTENT(IN) :: VEGTYP !vegetation physiology type
  INTEGER                        , INTENT(IN) :: NSNOW  !number of snow layers
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  REAL                           , INTENT(IN) :: DT     !time step (s)
  REAL                           , INTENT(IN) :: LAT    !latitude (radians)
  REAL                           , INTENT(IN) :: IGS    !growing season index (0=off, 1=on)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL  !depth of layer-bottom from soil surface
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !snow/soil temperature [k]
  REAL                           , INTENT(IN) :: PSN    !total leaf photosynthesis (umolco2/m2/s)
  REAL                           , INTENT(IN) :: TROOT  !root-zone averaged temperature (k)
  REAL                           , INTENT(IN) :: TV     !leaf temperature (k)
  REAL                           , INTENT(IN) :: WROOT  !root zone soil water
  REAL                           , INTENT(IN) :: BTRAN  !water availability
  REAL                           , INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL                           , INTENT(IN) :: LAPM   !leaf area per unit mass [m2/g]
  REAL                           , INTENT(IN) :: FVEG   !vegetation greenness fraction

  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    !soil moisture (ice + liq.) [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICE   !soil ice at present time [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SH2OO  !soil liq at previous time [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SICEO  !soil ice at previous time [m3/m3]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: PSI    !suction head [m]
  REAL                           , INTENT(IN) :: ETRAN  !transpiration rate at each step [m/s]
  REAL                           , INTENT(IN) :: O2AIR  !o2 (umol/mol)

! input and output

  REAL                        , INTENT(INOUT) :: XLAI   !leaf  area index from leaf carbon [-]
  REAL                        , INTENT(INOUT) :: XSAI   !stem area index from leaf carbon [-]
  REAL                        , INTENT(INOUT) :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT) :: RTMASS !mass of fine roots [g/m2]
  REAL                        , INTENT(INOUT) :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT) :: FASTCP !short lived carbon [g/m2]
  REAL                        , INTENT(INOUT) :: STBLCP !stable carbon pool [g/m2]
  REAL                        , INTENT(INOUT) :: WOOD   !mass of wood (incl. woody roots) [g/m2]
  REAL                        , INTENT(INOUT) :: KR     !controls when to grow [-]
  REAL                        , INTENT(INOUT) :: MQ     !water in plant tissues [kg]
  REAL, DIMENSION(1:NSOIL)    , INTENT(INOUT) :: SH2O   !soil water content [m3/m3]
  REAL, DIMENSION(1:NSOIL)    , INTENT(INOUT) :: ROOTMS !mass of live fine roots [g C/m2]

  REAL                        , INTENT(INOUT) :: SOC    !soil organic carbon [g C/m2]
  REAL                        , INTENT(INOUT) :: WDOC   !wet dissolved organic carbon [g C/m2]
  REAL                        , INTENT(INOUT) :: DDOC   !dry dissolved organic  [g C/m2]
  REAL                        , INTENT(INOUT) :: MIC    !soil microbial biomass [g C/m2]
  REAL                        , INTENT(INOUT) :: WENZ   !wet soil enzyme [g C/m2]
  REAL                        , INTENT(INOUT) :: DENZ   !dry soil enzyme [g C/m2]

! output

  REAL                          , INTENT(OUT) :: GPP    !net instantaneous assimilation [g/m2/s]
  REAL                          , INTENT(OUT) :: NPP    !net primary productivity [g/m2]
  REAL                          , INTENT(OUT) :: NEE    !net ecosystem exchange (autors+heters-gpp)
  REAL                          , INTENT(OUT) :: AUTORS !net ecosystem resp. (maintance and growth)
  REAL                          , INTENT(OUT) :: HETERS !organic respiration
  REAL                          , INTENT(OUT) :: TOTSC  !total soil carbon (g/m2)
  REAL                          , INTENT(OUT) :: TOTLB  !total living carbon (g/m2)

  REAL                          , INTENT(OUT) :: QCO2   !co2 efflux ( g C m-2 s-1)
  REAL, DIMENSION(      1:NSOIL), INTENT(OUT) :: FROOT  !root fraction
  REAL, DIMENSION(      1:NSOIL), INTENT(OUT) :: SADR   !root surface area density [m2/m3]
  real                          , INTENT(OUT) :: VMAX   !maximum SOC decomposition rate per
                                     !unit microbial biomass [g C m-2 [g CMIC m-2]-1 s-1]
  real                          , INTENT(OUT) :: VMAXUP !maximum DOC uptake rate [g CDOC m-2 [g CMIC m-2]-1 s-1]
  real                          , INTENT(OUT) :: KM     !Michaelis-Menten constant [g C m-2] for SOC  decomposition
  real                          , INTENT(OUT) :: KMUP   !Michaelis-Menten constant [g C m-2] for DOC uptake
  real                          , INTENT(OUT) :: EPSLON !carbon use efficiency


! local

  INTEGER                :: IZ
!  REAL                   :: CFLUX    !carbon flux to atmosphere [g/m2/s]
  REAL                   :: LFMSMN   !minimum leaf mass [g/m2]
  REAL                   :: RSWOOD   !wood respiration [g/m2]
  REAL                   :: RSLEAF   !leaf maintenance respiration per timestep [g/m2]
  REAL                   :: RSROOT   !fine root respiration per time step [g/m2]
  REAL                   :: NPPL     !leaf net primary productivity [g/m2/s]
  REAL                   :: NPPR     !root net primary productivity [g/m2/s]
  REAL                   :: NPPW     !wood net primary productivity [g/m2/s]
  REAL                   :: NPPS     !wood net primary productivity [g/m2/s]
  REAL                   :: DIELF    !death of leaf mass per time step [g/m2]

  REAL                   :: ADDNPPLF !leaf assimil after resp. losses removed [g/m2]
  REAL                   :: ADDNPPST !stem assimil after resp. losses removed [g/m2]
  REAL                   :: CARBFX   !carbon assimilated per model step [g/m2]
  REAL                   :: GRLEAF   !growth respiration rate for leaf [g/m2/s]
  REAL                   :: GRROOT   !growth respiration rate for root [g/m2/s]
  REAL                   :: GRWOOD   !growth respiration rate for wood [g/m2/s]
  REAL                   :: GRSTEM   !growth respiration rate for stem [g/m2/s]
  REAL                   :: LEAFPT   !fraction of carbon allocated to leaves [-]
  REAL                   :: LFDEL    !maximum  leaf mass  available to change [g/m2/s]
  REAL                   :: LFTOVR   !stem turnover per time step [g/m2]
  REAL                   :: STTOVR   !stem turnover per time step [g/m2]
  REAL                   :: WDTOVR   !wood turnover per time step [g/m2]
  REAL                   :: RSSOIL   !soil respiration per time step [g/m2]
  REAL                   :: RTTOVR   !root carbon loss per time step by turnover [g/m2]
  REAL                   :: STABLC   !decay rate of fast carbon to slow carbon [g/m2/s]
  REAL                   :: WOODF    !calculated wood to root ratio [-]
  REAL                   :: NONLEF   !fraction of carbon to root and wood [-]
  REAL                   :: ROOTPT   !fraction of carbon flux to roots [-]
  REAL                   :: WOODPT   !fraction of carbon flux to wood [-]
  REAL                   :: STEMPT   !fraction of carbon flux to stem [-]
  REAL                   :: RESP     !leaf respiration [umol/m2/s]
  REAL                   :: RSSTEM   !stem respiration [g/m2/s]

  REAL                   :: FSW      !soil water factor for microbial respiration
  REAL                   :: FST      !soil temperature factor for microbial respiration
  REAL                   :: FNF      !foliage nitrogen adjustemt to respiration (<= 1)
  REAL                   :: TF       !temperature factor
  REAL                   :: RF       !respiration reduction factor (<= 1)
  REAL                   :: STDEL
  REAL                   :: STMSMN
  REAL                   :: SAPM     !stem area per unit mass (m2/g)
  REAL                   :: DIEST
! -------------------------- constants -------------------------------
  REAL                   :: BF       !parameter for present wood allocation [-]
  REAL                   :: RSWOODC  !wood respiration coeficient [1/s]
  REAL                   :: STOVRC   !stem turnover coefficient [1/s]
!  REAL                   :: RSDRYC   !degree of drying that reduces soil respiration [-]
  REAL                   :: RTOVRC   !root turnover coefficient [1/s]
  REAL                   :: WSTRC    !water stress coeficient [-]
  REAL                   :: LAIMIN   !minimum leaf area index [m2/m2]
  REAL                   :: XSAMIN   !minimum leaf area index [m2/m2]
  REAL                   :: SC
  REAL                   :: SD
  REAL                   :: VEGFRAC

! ------------------------- newly added ------------------------------
  REAL                   :: CERR     !imbalance of carbon [gC/m2]
  REAL                   :: BEG_C    !imbalance of carbon [gC/m2]
  REAL                   :: DRYC     !dry carbon mass [gC/m2]
  REAL                   :: RTEXD    !total root carbon exudation rate (g/m2/s)
  REAL                   :: LITC     !carbon content in  litter (g/m2/s)
  REAL                   :: TS1M     !top 1m soil temperature (K)
  REAL                   :: SM1M     !top 1m soil moisture (m3/m3)
  REAL                   :: DZSUM

  REAL ::      LFOLD,STOLD,RTOLD,WDOLD,FCOLD,SCOLD,SCPOLD

! Respiration as a function of temperature

  real :: r,x
          r(x) = exp(0.08*(x-298.16))
! ---------------------------------------------------------------------------------

    RSLEAF  = 0.
! constants
    RTOVRC  = 2.0E-8        !original was 2.0e-8
!    RSDRYC  = 40.0          !original was 40.0
    RSWOODC = 3.0E-10       !
    BF      = 0.90          !original was 0.90   ! carbon to roots
    WSTRC   = 1.0
    LAIMIN  = 0.05   
    XSAMIN  = 0.01     ! MB: change to prevent vegetation from not growing back in spring

    SAPM    = 0.01     ! 10 m2/kg --> 0.01 m2/g
    LFMSMN  = laimin/lapm
    STMSMN  = xsamin/sapm
! ---------------------------------------------------------------------------------
    !IF(ILOC == 137 .and. JLOC == 7) THEN
    !  write(*,*) 'TV,BTRAN=',TV,BTRAN
    !END IF

     IF(OPT_ROOT == 1) THEN
        RTMASS  = 0.
        DO IZ = 1, parameters%NROOT
          RTMASS = RTMASS + ROOTMS(IZ)    !g/m2
        END DO
     END IF

     IF(OPT_SCM /= 1 ) THEN
        FASTCP =   SOC + WDOC + DDOC + MIC + WENZ + DENZ      !g/m2 C
     END IF 

     TOTSC  =  STBLCP + FASTCP                                     !g/m2 C

     BEG_C  = LFMASS + RTMASS + WOOD + STMASS + TOTSC                 !g/m2 C

     LFOLD = LFMASS
     STOLD = STMASS
     RTOLD = RTMASS
     WDOLD = WOOD
     FCOLD = FASTCP
     SCPOLD = STBLCP
     SCOLD = STBLCP + FASTCP

! respiration

     IF(IGS .EQ. 0.) THEN
       RF = 0.5
     ELSE
       RF = 1.0
     ENDIF
            
     FNF     = MIN( FOLN/MAX(1.E-06,parameters%FOLNMX), 1.0 )
     TF      = parameters%ARM**( (MIN(TV,330.)-298.16)/10. )
    !RESP    = parameters%RMF25 * TF * FNF * XLAI * RF * (1.-WSTRES) ! umol/m2/s
     RESP    = parameters%RMF25 * TF * FNF * XLAI * RF * BTRAN/(0.5+BTRAN) ! umol/m2/s
     RSLEAF  = MIN((LFMASS-LFMSMN)/DT,RESP*12.e-6)                         ! g/m2/s
     
     RSSTEM  = MIN((STMASS-STMSMN)/DT,parameters%RMS25*TF*FNF*XSAI*RF*BTRAN/(0.5+BTRAN)*12.e-6) ! g/m2/s
     RSWOOD  = RSWOODC * R(MIN(TV,330.)) * WOOD*parameters%WDPOOL

! carbon assimilation
! 1 mole -> 12 g carbon or 44 g CO2; 1 umol -> 12.e-6 g carbon;

     CARBFX  = PSN * 12.e-6              ! umol co2 /m2/ s -> g/m2/s carbon

! plants first meet root water uptake

     IF(OPT_ROOT == 1) THEN
        ROOTPT = MAX(0.01,0.30*(1.-KR))
     END IF

     IF(OPT_ROOT == 2) THEN
        ROOTPT = 0.15
     END IF

! fraction of carbon aalocation

     IF(WOOD .GT. 0 ) THEN
       LEAFPT = (1.0-ROOTPT)*10000. *exp(-2.0*XLAI)/(1.+10000.*exp(-2.0*XLAI))
       NONLEF = (1.0-ROOTPT) - LEAFPT
       STEMPT = NONLEF*0.1
       NONLEF = NONLEF-STEMPT
       WOODPT = NONLEF*parameters%WDPOOL

     ELSE
       LEAFPT = 1.0-ROOTPT
       STEMPT = 1.-1500.*exp(-2.2*XLAI)/(1.+1500.*exp(-2.2*XLAI))
       LEAFPT = LEAFPT - STEMPT
       WOODPT = 0.
     END IF

     IF(ABS(LEAFPT+STEMPT+ROOTPT+WOODPT-1.0) >= 0.001) THEN
      write(*,*) 'XLAI,(LEAFPT+STEMPT+ROOTPT+WOODPT)=',XLAI,LEAFPT,STEMPT,ROOTPT,WOODPT
      call wrf_error_fatal("STOP in Noah-MP carbon partition")
      STOP
     END IF

! leaf and root turnover per time step

     LFTOVR = parameters%LTOVRC*1.E-6*(LFMASS-LFMSMN)
     STTOVR = parameters%LTOVRC*1.E-6*(STMASS-STMSMN)*0.001
     RTTOVR = RTOVRC*RTMASS
     WDTOVR = 9.5E-10*WOOD

! seasonal leaf die rate dependent on temp and water stress
! water stress is set to 1 at permanent wilting point

     SC  = EXP(-parameters%SCEXP*MAX(0.,TV-parameters%TDLEF)) * (LFMASS/120.) 
     SD  = EXP(-BTRAN*WSTRC)
     DIELF = MAX(0.,(LFMASS-LFMSMN)*1.E-6*(parameters%DILEFW*SD+parameters%DILEFC*SC))
     DIEST = MAX(0.,(STMASS-STMSMN)*1.E-6*(parameters%DILEFW*SD+parameters%DILEFC*SC))

! calculate growth respiration for leaf, rtmass and wood

     GRLEAF = MAX(0.0,parameters%FRAGR*(LEAFPT*CARBFX - RSLEAF))
     GRSTEM = MAX(0.0,parameters%FRAGR*(STEMPT*CARBFX - RSSTEM))
     GRWOOD = MAX(0.0,parameters%FRAGR*(WOODPT*CARBFX - RSWOOD))

! net primary productivities

     NPPL   = LEAFPT*CARBFX - RSLEAF - GRLEAF
     NPPS   = STEMPT*CARBFX - RSSTEM - GRSTEM
     NPPW   = WOODPT*CARBFX - RSWOOD - GRWOOD

    !IF(ILOC == 137 .and. JLOC == 7) THEN
    !  write(*,*) 'STEMPT*CARBFX , RSSTEM , GRSTEM=',STEMPT*CARBFX,RSSTEM,GRSTEM
    !END IF

! masses of plant components

     LFMASS = LFMASS + (NPPL-(DIELF+LFTOVR))*DT
     STMASS = STMASS + (NPPS-(DIEST+STTOVR))*DT   ! g/m2        
     WOOD   = (WOOD   + (NPPW-WDTOVR)*DT)*parameters%WDPOOL

! root mass and fraction

     IF(OPT_ROOT == 2) THEN

        RTTOVR = RTOVRC*RTMASS
        RSROOT = parameters%RMR25*(RTMASS*1E-3)*TF *RF* 12.e-6
        GRROOT = MAX(0.0,parameters%FRAGR*(ROOTPT*CARBFX - RSROOT))

        NPPR   = ROOTPT*CARBFX - RSROOT - GRROOT
        RTEXD  = 0.3 * ROOTPT*CARBFX                            !g/m2/s

        RTMASS = RTMASS + (NPPR-RTTOVR-RTEXD)*DT

        IF(RTMASS.LT.0.0) THEN
           RTTOVR = NPPR
           RTMASS = 0.0
        ENDIF

     END IF

     IF(OPT_ROOT == 1) THEN

       CALL ROOTMASS(parameters, ILOC     ,JLOC     ,NSOIL  ,NSNOW  , &
                      DT     ,ZSOIL  ,SH2O   ,STC    , &
                      RF     ,CARBFX , &
                      ROOTPT ,VEGTYP , &
                      ROOTMS , &
                      RTMASS ,RTEXD  ,RTTOVR , &
                      RSROOT ,GRROOT ,NPPR   )
     END IF

    !if (isnan(LFMASS)) then
    ! WRITE(*,*) 'ILOC, JLOC=',ILOC,JLOC
    ! write(*,*) 'LEAFPT,CARBFX,RSLEAF,GRLEAF,PSN,KR=',LEAFPT,CARBFX,RSLEAF,GRLEAF,PSN,KR
    ! write(*,*) 'LFMASS ,NPPL,LFTOVR,DIELF   =',LFMASS ,NPPL,LFTOVR,DIELF
    !END IF

!    IF(ILOC == 137 .and. JLOC == 7) THEN
!      write(*,*) 'LFMASS ,NPPL,LFTOVR,DIELF=',LFMASS ,NPPL,LFTOVR,DIELF
!      write(*,*) 'STMASS , NPPS,STTOVR=',STMASS , NPPS,STTOVR
!    END IF

     LITC  = (RTTOVR+LFTOVR+STTOVR+WDTOVR+DIELF+DIEST)*DT   !g/m2

! soil carbon budgets: MRP in umol co2 /kg c /s

  IF(OPT_SCM == 1 ) THEN

     FASTCP = FASTCP + LITC + RTEXD*DT  ! MB: add DIEST v3.7

     DZSUM = 0.
     SM1M  = 0.
     TS1M  = 0.
     DO IZ = 1,2
        DZSUM = DZSUM +          DZSNSO(IZ)
        SM1M  = SM1M  + SH2O(IZ)*DZSNSO(IZ)
        TS1M  = TS1M  + STC (IZ)*DZSNSO(IZ)
     END DO
     SM1M  = SM1M  / DZSUM
     TS1M  = TS1M  / DZSUM

    !FST  = 2.0**( (STC(1)-283.16)/10. )
    !FSW  = WROOT / (0.20+WROOT) * 0.23 / (0.23+WROOT)
     FST  = 2.0**( (TS1M-283.16)/10. )
     FSW  = SM1M / (0.20+SM1M) * 0.23 / (0.23+SM1M)
     RSSOIL = FSW * FST * 0.001*parameters%MRP* MAX(0.,FASTCP)*12.E-6  ! umol co2/m2/s -> kg C/m2/s

     FASTCP = FASTCP - RSSOIL*DT

  ELSE
     CALL CO2PRODUCT(parameters,NSOIL,     DT,    STC,    SMC,   SH2O, &
                                 SICE,SH2OO  ,SICEO  , O2AIR , DZSNSO, &
                                  SOC,   WDOC,   DDOC, MIC   ,   WENZ, &
                                 DENZ,   LITC,  RTEXD, RSSOIL,  NSNOW, &
                                 VMAX,     KM, VMAXUP,   KMUP, EPSLON, &
                                 ILOC,   JLOC)

     FASTCP = SOC + WDOC + DDOC + MIC + WENZ + DENZ   !g/m2 C

  END IF

     STABLC = 0.02*RSSOIL
     STBLCP = STBLCP + STABLC*DT

!  total carbon eflux from the soil surface

     QCO2   = 0.98*RSSOIL+RSROOT+GRROOT

! for outputs

     GPP    = CARBFX                                             !g/m2/s C
     NPP    = NPPL + NPPW + NPPR +NPPS                           !g/m2/s C
     AUTORS = RSROOT + RSWOOD + RSLEAF + RSSTEM + &              !g/m2/s C  MB: add RSSTEM, GRSTEM v3.7
              GRLEAF + GRROOT + GRWOOD + GRSTEM                  !g/m2/s C  MB: add 0.9* v3.7
     HETERS = 0.98*RSSOIL                                        !g/m2/s C
     NEE    = (AUTORS + HETERS - GPP)                            !g/m2/s C

! carbon balance check

     TOTSC  = FASTCP + STBLCP                                    !g/m2   C
     TOTLB  = LFMASS + RTMASS + WOOD + STMASS                    !g/m2 C

     CERR   = ((TOTSC+TOTLB)-BEG_C) + NEE*DT

     IF(ABS(CERR) .ge. 0.1) THEN
       WRITE(*,*) 'Carbon imbalance, CERR ======================================== ', CERR
       write(*,*) 'iloc,jloc,VEGTYP,TOTSC,TOTLB,TOTSC+TOTLB,BEG_C,NEE*DT'
       write(*,*) iloc,jloc,VEGTYP,TOTSC,TOTLB,TOTSC+TOTLB,BEG_C,NEE*DT
       write(*,*) 'AUTORS , HETERS , GPP=',AUTORS*DT , HETERS*DT , GPP*DT
       write(*,*) 'RSROOT,RSWOOD,RSLEAF,RSSTEM,GRLEAF,GRROOT,GRWOOD,GRSTEM',&
        RSROOT*DT,RSWOOD*DT,RSLEAF*DT,RSSTEM*DT,GRLEAF*DT,GRROOT*DT,GRWOOD*DT,GRSTEM*DT
       write(*,*)'LFMASS - LFOLD  = ',LFMASS-LFOLD
       write(*,*)'STMASS - STOLD  = ',STMASS-STOLD
       write(*,*)'RTMASS - RTOLD  = ',RTMASS-RTOLD
       write(*,*)'WDMASS - WDOLD  = ',WOOD  -WDOLD
       write(*,*)'TOTSC  - SCOLD  = ',TOTSC -SCOLD
       write(*,*)'FASTCP - FCOLD  = ',FASTCP-FCOLD
       write(*,*)'STBLCP - SCPOLD = ',STBLCP-SCPOLD
       call wrf_error_fatal("STOP in Noah-MP carbon balance ")
      !STOP
     END IF

! leaf area index and stem area index

     XLAI    = MAX(LFMASS*LAPM,LAIMIN)
     XSAI    = MAX(STMASS*SAPM,XSAMIN)

  END SUBROUTINE CO2FLUX

! ==================================================================================================
! ==================================================================================================
  SUBROUTINE ROOTMASS(parameters, ILOC     ,JLOC     ,NSOIL  ,NSNOW  , &
                      DT     ,ZSOIL  ,SH2O   ,STC    ,RF     ,CARBFX , &
                      ROOTPT ,VEGTYP ,ROOTMS , &
                      RTMASS ,RTEXD  ,RTTOVR , &
                      RSROOT ,GRROOT ,NPPR   )

   IMPLICIT NONE

! inputs

  type (noahmp_parameters), intent(in) :: parameters

  INTEGER, INTENT(IN)                             :: ILOC
  INTEGER, INTENT(IN)                             :: JLOC
  INTEGER, INTENT(IN)                             :: NSNOW  !number of snow layers
  INTEGER, INTENT(IN)                             :: NSOIL  !number of soil layers
  INTEGER, INTENT(IN)                             :: VEGTYP !vegetation physiology type
  REAL   , INTENT(IN)                             :: DT     !time step [s]
  REAL   , INTENT(IN)                             :: ROOTPT
  REAL   , INTENT(IN)                             :: CARBFX
  REAL   , INTENT(IN), DIMENSION(       1:NSOIL)  :: ZSOIL  !layer-bottom depth from soil surface [m]
  REAL   , INTENT(IN), DIMENSION(-NSNOW+1:NSOIL)  :: STC    !soil temperature [k]
  REAL   , INTENT(IN)                             :: RF       !respiration reduction factor (<= 1)

  REAL  , INTENT(INOUT), DIMENSION(1:NSOIL)       :: SH2O   !soil water content [m3/m3]
  REAL  , INTENT(INOUT), DIMENSION(1:NSOIL)       :: ROOTMS       !mass of live fine roots [g C/m2]

! outputs

  REAL   , INTENT(OUT)                            :: RTMASS !mass of fine roots [g/m2]
  REAL   , INTENT(OUT)                            :: RTEXD  !total root carbon exudation rate (g/m2/s)
  REAL   , INTENT(OUT)                            :: RTTOVR !root carbon loss per time step by turnover [g/m2/s]
  REAL   , INTENT(OUT)                            :: RSROOT !fine root respiration per time step [g/m2/s]
  REAL   , INTENT(OUT)                            :: GRROOT !growth respiration rate for root [g/m2/s]
  REAL   , INTENT(OUT)                            :: NPPR   !root net primary productivity [g/m2/s]

! locals

  INTEGER                  :: IZ           !do-loop index in z-direction 
  REAL, DIMENSION(1:NSOIL) :: RTPT         !fine root respiration per time step [g/m2/s]
  REAL, DIMENSION(1:NSOIL) :: ROOTEX       !total root carbon exudation rate (g/m2/s)
  REAL, DIMENSION(1:NSOIL) :: ROOTTO       !root carbon loss per time step by turnover [g/m2/s]
  REAL, DIMENSION(1:NSOIL) :: ROOTRS       !fine root respiration per time step [g/m2/s]
  REAL, DIMENSION(1:NSOIL) :: ROOTGR       !growth respiration rate for root [g/m2/s]
  REAL, DIMENSION(1:NSOIL) :: ROOTNPP      !root net primary productivity [g/m2/s]
  REAL, DIMENSION(1:NSOIL) :: DZ           !layer thickness [m]
  REAL, DIMENSION(1:NSOIL) :: ZNODE        !depth of the middle of each layer (node depth) [m]
  REAL, DIMENSION(1:NSOIL) :: WEIGHT       !weight of carbon partitioned into soil layers [-]
  REAL, DIMENSION(1:NSOIL) :: GX
  REAL, DIMENSION(1:NSOIL) :: WTDZ

  REAL, PARAMETER          :: RTMIN   =  0.1      !min root mass [g/m3]

  REAL ::  TF           ! root respiration Q10 apporach (Q10 =2)
  REAL ::  ST(1:NSOIL)  ! temperature stress
  REAL ::  SD(1:NSOIL)  ! drought stress
  REAL ::  RTM_ERR,RTMOLD,CARBIN,RTPTT,BAR
  REAL ::  SUMRF

!--------------------------------------------------------------------------------------------------------------    
! for carbon mass balance check

     RTMOLD = RTMASS

! layer thikcness [m] and node (middle) depth in [m]

     DZ(1) = -ZSOIL(1)
     DO IZ = 2, NSOIL
        DZ(IZ)  = (ZSOIL(IZ-1) - ZSOIL(IZ))
     ENDDO

     ZNODE(1) = -ZSOIL(1) * 0.5
     DO IZ = 2, NSOIL
        ZNODE(IZ)  = -ZSOIL(IZ-1) + 0.5 * DZ(IZ)
     ENDDO

! C partitioning to layers

     SUMRF = 0.
     DO IZ = 1, parameters%NROOT
         WTDZ(IZ)   = EXP(-0.1*ZNODE(IZ))   !to form background root profile (for even soil water)
         GX(IZ)     = SQRT(MAX(1.E-6,MIN(1.0,((SH2O(IZ)-parameters%SMCWLT(IZ))/(parameters%SMCREF(IZ)-parameters%SMCWLT(IZ))))))
         WEIGHT(IZ) = GX(IZ)*DZ(IZ)*WTDZ(IZ)
         SUMRF      = SUMRF + WEIGHT(IZ)
     ENDDO

! root carbon dynamics

    DO IZ = 1, parameters%NROOT

      RTPT(IZ)    = (ROOTPT*CARBFX) * WEIGHT(IZ)/SUMRF             ! g/m2/s

      TF          = parameters%ARM**( (MIN(330.,STC(IZ))-298.16)/10.)
      ROOTRS(IZ)  = parameters%RMR25*((ROOTMS(IZ)-RTMIN)*1E-3)*TF*RF*12.E-6    ! g/m2/s

      ST(IZ)      = 0.005
      SD(IZ)      = 0.1*(1.0-GX(IZ))

      ROOTTO(IZ)  = MAX(0.,(ROOTMS(IZ)-RTMIN))*parameters%RTOMAX/(86400.*365.)*MAX(SD(IZ),ST(IZ))
      ROOTEX(IZ)  = 0.3 * RTPT(IZ)
      ROOTGR(IZ)  = MAX(0.0, 0.2 * (RTPT(IZ) - ROOTRS(IZ)))
      ROOTNPP(IZ) = RTPT(IZ) - ROOTRS(IZ) - ROOTGR(IZ)

      ROOTMS(IZ)  = ROOTMS (IZ) + (ROOTNPP(IZ)-ROOTTO(IZ)-ROOTEX(IZ))*DT

      IF(ROOTMS(IZ).LT.0.0) THEN
           ROOTTO(IZ) = ROOTNPP(IZ)
           ROOTMS(IZ) = RTMIN
      ENDIF

    END DO

    RTPTT  = 0.
    RTMASS = 0.
    RTEXD  = 0.
    RTTOVR = 0.
    RSROOT = 0.
    GRROOT = 0.
    NPPR   = 0.

    DO IZ = 1, parameters%NROOT
      RTPTT  = RTPTT  + RTPT   (IZ)
      RTMASS = RTMASS + ROOTMS (IZ)
      RTEXD  = RTEXD  + ROOTEX (IZ)
      RTTOVR = RTTOVR + ROOTTO (IZ)
      RSROOT = RSROOT + ROOTRS (IZ)
      GRROOT = GRROOT + ROOTGR (IZ)
      NPPR   = NPPR   + ROOTNPP(IZ)
    END DO

    ! carbon balance check

    CARBIN = ROOTPT*CARBFX*DT
    RTM_ERR= (RTMASS-RTMOLD)-(CARBIN-(RTTOVR+RTEXD+RSROOT+GRROOT)*DT)

    IF(ABS(RTM_ERR) >=0.01) THEN
     write(*,*) 'root mass error',RTM_ERR
     write(*,*) 'iloc,iloc,VEGTYP,RTMASS,RTMOLD,CARBFX*ROOTPT*DT,RTPTT*DT,RTTOVR*DT,RTEXD*DT,RSROOT*DT,GRROOT*DT'
     write(*,*)  iloc,jloc,VEGTYP,RTMASS,RTMOLD,CARBFX*ROOTPT*DT,RTPTT*DT,RTTOVR*DT,RTEXD*DT,RSROOT*DT,GRROOT*DT
     DO IZ = 1, parameters%NROOT
      write(*,*) RTPT(IZ),WEIGHT(IZ),ROOTPT,CARBFX
     END DO
    call wrf_error_fatal("STOP in Noah-MP subroutine ROOTMASS ")
    END IF

    END SUBROUTINE ROOTMASS
! ==================================================================================================
!-------------------------------------------------------------------------------------------------
  SUBROUTINE CO2PRODUCT(parameters, NSOIL,  DTIME,    STC,    SMC,   SH2O, &
                                     SICE,SH2OO  ,SICEO  ,  O2AIR, DZSNSO, &
                                      SOC,   WDOC,   DDOC,    MIC,   WENZ, &
                                     DENZ,   LITC,  RTEXD,   RESP,  NSNOW, &
                                     VMAX,     KM, VMAXUP,   KMUP, EPSLON, &
                                     ix,iy)
!-------------------------------------------------------------------------------------------------

  implicit none

  !------------------arguments-----------------------------------------------
  type (noahmp_parameters), intent(in) :: parameters
  INTEGER                        , INTENT(IN)    :: ix,iy
  INTEGER                        , INTENT(IN)    :: NSNOW  !number of snow layers
  INTEGER                        , INTENT(IN)    :: NSOIL  !number of soil layers
  REAL                           , INTENT(IN)    :: DTIME  !time step [s]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: DZSNSO !snow/soil layer thickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN)    :: STC    !soil temperature [k]
  REAL, DIMENSION(1:NSOIL)       , INTENT(IN)    :: SMC    !moisture (ice + liq.) [m2/m2]
  REAL, DIMENSION(1:NSOIL)       , INTENT(IN)    :: SH2O   !soil liq at present time [m2/m2]
  REAL, DIMENSION(1:NSOIL)       , INTENT(IN)    :: SICE   !soil ice at present time [m2/m2]
  REAL, DIMENSION(1:NSOIL)       , INTENT(IN)    :: SH2OO  !soil liq at previous time [m2/m2]
  REAL, DIMENSION(1:NSOIL)       , INTENT(IN)    :: SICEO  !soil ice at previous time [m2/m2]
  REAL                           , INTENT(IN)    :: LITC   !litter pool size [g C m-2]
  REAL                           , INTENT(IN)    :: RTEXD  !root exudatation into DOC [g c/m2/s]
  REAL                           , INTENT(IN)    :: O2AIR  !O2 in the air [g O2 m-3]
  REAL                           , INTENT(INOUT) :: SOC    !soil organic carbon [g C/m2]
  REAL                           , INTENT(INOUT) :: WDOC   !wet dissolved organic carbon [g C/m2]
  REAL                           , INTENT(INOUT) :: DDOC   !dry dissolved organic  [g C/m2]
  REAL                           , INTENT(INOUT) :: MIC    !soil microbial biomass [g C/m2]
  REAL                           , INTENT(INOUT) :: WENZ   !wet soil enzyme [g C/m2]
  REAL                           , INTENT(INOUT) :: DENZ   !dry soil enzyme [g C/m2]
  REAL                           , INTENT(OUT)   :: RESP   !heterotrophic respiration [g C/m2/s]
  real     , INTENT(OUT)   :: VMAX   !maximum SOC decomposition rate per
                                     !unit microbial biomass [g CSOC m-2 [g CMIC m-2]-1 s-1]
  real     , INTENT(OUT)   :: VMAXUP !maximum DOC uptake rate [g CDOC m-2 [g CMIC m-2]-1 s-1] 
  real     , INTENT(OUT)   :: KM     !Michaelis-Menten constant [g C m-2] for SOC  decomposition
  real     , INTENT(OUT)   :: KMUP   !Michaelis-Menten constant [g C m-2] for DOC uptake
  real     , INTENT(OUT)   :: EPSLON !carbon use efficiency
!---------------local variables----------------------------------------------------
  integer  :: i,j,iz             !loop indexes
  real     :: rate_doc           !SOC decomposition rate and add into doc pool [g C m-2 s-1]
  real     :: rate_ndoc          !SOC decomposition rate and add into ndoc pool [g C m-2 s-1]
  real     :: up_rate            !DOC uptake rate [g C m-2 s-1]
  real     :: mic_death          !microbial mass death date [g C m-2 s-1]
  real     :: enz_death          !soil enzyme turnover date  [g C m-2 s-1]
  real     :: nenz_death         !soil enzyme turnover date in dry area  [g C m-2 s-1]
  real     :: enz_product        !soil enzyme production date [g C m-2 s-1]
  real     :: so2                !soil o2 concentration [g/m2]
  real     :: tsoil

  real     :: soil_h2o           !soil liq saturation
  real     :: soil_ice           !soil ice saturation
  real     :: POROS
  real     :: FTRAN
  real     :: SMOLD
  real     :: SM
  real     :: TOTSC_BEG
  real     :: TOTSC_END
  real     :: TOTSC_ERR
  real     :: DZSUM

  real, parameter :: vmax0      = 2.8e04  !a pre-exponential coefficient of vmax as an Arrhenius function
                                          !of temperatuer [g CSOC m-2 [g CMIC m-2]-1 s-1]
  real, parameter :: energy     = 4.7e04  !activation energy for the enzymatic reaction with SOC [J mol-1]
  real, parameter :: kmslope    = 5e03    !slope of km as linear function of temperature [g C m-2 C-1]
  real, parameter :: km0        = 5e05    !intercept of  km as linear function of temperature [g C m-2]
  real, parameter :: vmaxup0    = 2.8e04  !a pre-exponential coefficient of vmaxup as an Arrhenius function
                                          !of temperature[g CSOC m-2 [g CMIC m-2]-1 s-1]
  real, parameter :: energyup   = 4.7e04  !activation energy for the enzymatic reaction with DOC uptake [J mol-1]
  real, parameter :: kmupslope  = 10      !slope of kmup as linear function of temperature [g C m-2 C-1]
  real, parameter :: kmup0      = 100     !intercept of  kmup as linear function of temperature [g C m-2]
  real, parameter :: kmupo2     = 0.121   !Michaelis-Menten constant for O2 uptake [g O2 m-2 air]
  real, parameter :: mic_tover  = 1.0*5.56e-8 !the microbial biomass death rate constant [s-1]
  real, parameter :: enz_tover  = 1.0*2.78e-7 !soil enzye turnover rate constant in wet zone [s-1]
  real, parameter :: nenz_tover = 0.05*2.78e-7!soil enzye turnover rate constant in dry zone [s-1]
  real, parameter :: enz_ke     = 20.*1.39e-9 !soil enzye production rate constant [s-1]
  real, parameter :: delta      = 0.5     !the fraction of dead microbial biomass into SOC [-]
  real, parameter :: lit        = 0.3     !the fraction of litter into DOC
  real, parameter :: epslon0    = 0.63    !intercept of epslon as linear function of temperature
  real, parameter :: ep_slope   = -1.6e-2 !intercept of epslon as linear function of temperature
  real, parameter :: beta       = 0.75    !enzyme effiecency in the dry zone

!--------------end varialbes list----------------------------------------------------

      TOTSC_BEG   = SOC + WDOC + DDOC + MIC + WENZ + DENZ   !g/m2 C
      !write(*,*) TOTSC_BEG,SOC , WDOC , DDOC , MIC , WENZ , DENZ   !g/m2 C

      DZSUM    = 0.
      SM       = 0.
      tsoil    = 0.
      soil_h2o = 0.
      soil_ice = 0.
      POROS    = 0.
      SMOLD    = 0.

      DO IZ = 1,3
         DZSUM    = DZSUM    +                        DZSNSO(IZ)
         SM       = SM       +            SH2O   (IZ)*DZSNSO(IZ)
         SMOLD    = SMOLD    +            SH2OO  (IZ)*DZSNSO(IZ)
         tsoil    = tsoil    +            STC    (IZ)*DZSNSO(IZ)
         soil_h2o = soil_h2o +            SH2O   (IZ)*DZSNSO(IZ)
         soil_ice = soil_ice +            SICE   (IZ)*DZSNSO(IZ)
         POROS    = POROS    + parameters%SMCMAX (IZ)*DZSNSO(IZ)
      END DO

      SM       = SM       / DZSUM
      SMOLD    = SMOLD    / DZSUM
      tsoil    = tsoil    / DZSUM
      POROS    = POROS    / DZSUM

      soil_h2o = soil_h2o / DZSUM / POROS
      soil_ice = soil_ice / DZSUM / POROS

      so2      = max(0.0, o2air * (1.0-soil_h2o-soil_ice))

      IF(OPT_SCM == 3) THEN
        if(SM > SMOLD) then
          FTRAN = MAX(0.01,MIN(1.0,(SM - SMOLD)/(POROS-SMOLD)))

          wdoc = wdoc + ddoc*FTRAN
          ddoc = ddoc - ddoc*FTRAN

          wenz = wenz + denz*FTRAN
          denz = denz - denz*FTRAN

        else
          FTRAN = (SMOLD - SM) / SMOLD

          ddoc = ddoc + wdoc*FTRAN
          wdoc = wdoc - wdoc*FTRAN

          denz = denz + wenz*FTRAN
          wenz = wenz - wenz*FTRAN
        endif
      END IF

!SOC decomposition
      vmax   = vmax0 * exp(-energy/(8.31*tsoil))
      !km     = max(0.0, km0 + kmslope*(tsoil-tfrz))
      km     = max(0.0, km0 + kmslope*max(0.0,tsoil-tfrz))

!*(soil_mc(i)+soil_mcold(i))/2/poros

      rate_doc  = vmax * wenz*soc/(km+soc)*   soil_h2o
      rate_ndoc = vmax * denz*soc/(km+soc)*(1-soil_h2o)*beta

!DOC uptake rate

      vmaxup  = vmaxup0 * exp(-energyup/(8.31*tsoil))
      !kmup    = max(0.0, kmup0 + kmupslope*(tsoil-tfrz))
      kmup    = max(0.0, kmup0 + kmupslope*max(0.0,tsoil-tfrz))

      up_rate = vmaxup*mic * wdoc/(kmup+wdoc) * so2/(kmupo2+so2) * soil_h2o

      !up_rate = min(wdoc/dtime,up_rate)
      up_rate = max(0.,min(wdoc/dtime,up_rate))
      !epslon =  max(0.,epslon0+ep_slope*(tsoil-tfrz))
      epslon =  max(0.,epslon0+ep_slope*max(0.,tsoil-tfrz))

!respiration rate

      resp = up_rate*(1.-epslon)

!microbial mass death rate

      mic_death  = min(mic/dtime,mic*mic_tover)
      mic  = mic - mic_death*dtime + up_rate*dtime*epslon

!enzyme production rate
      enz_product= enz_ke*mic
      mic  = max(0.0,mic - enz_product*dtime)
!enzyme turnover rate

      enz_death  = min(wenz/dtime, wenz*enz_tover )
      nenz_death = min(denz/dtime, denz*nenz_tover)

!mass balance

     soc  = soc - (rate_doc+rate_ndoc)*dtime + mic_death*delta*dtime&
                   + litc*(1.-lit)

     wdoc = wdoc + rate_doc*dtime + mic_death*(1-delta)*dtime&
             + enz_death*dtime + litc*lit + rtexd*dtime - up_rate*dtime

     IF(OPT_SCM == 3) ddoc = ddoc + rate_ndoc*dtime + nenz_death*dtime
     IF(OPT_SCM == 2) ddoc = 0.

     wenz = wenz + (enz_product - enz_death)*dtime

     IF(OPT_SCM == 3) denz = denz - nenz_death*dtime
     IF(OPT_SCM == 2) denz = 0.

!Mass balance check

      TOTSC_END   = SOC + WDOC + DDOC + MIC + WENZ + DENZ   !g/m2 C

      TOTSC_ERR  = (TOTSC_END - TOTSC_BEG) - LITC - RTEXD*DTIME + RESP*DTIME

      IF(ABS(TOTSC_ERR) > 0.1) THEN
        write(*,*) 'TOTSC_ERR=',TOTSC_ERR
        write(*,*) 'TOTSC_END,TOTSC_BEG,LITC,RTEXD*DTIME,RESP*DTIME,up_rate,epslon,wdoc'
        write(*,*) TOTSC_END,TOTSC_BEG,LITC,RTEXD*DTIME,RESP*DTIME,up_rate,epslon,wdoc
        write(*,*) vmaxup,mic, wdoc,kmup,so2,kmupo2,soil_h2o
        !stop
      END IF

      if(soc<0. .or. ddoc<0. .or. wdoc<0. .or. mic<0. .or. wenz<0. .or. denz<0.) then
        write(*,*)'ix,iy,SOC,WDOC,DDOC,MIC,WENZ,DENZ,TSOIL,SMC(1)'   !g/m2 C
        write(*,*)ix,iy,SOC,WDOC,DDOC,MIC,WENZ,DENZ,TSOIL,SMC(1)   !g/m2 C
      call wrf_error_fatal("STOP in Noah-MP soil carbon")
      !STOP
     end if

 END SUBROUTINE CO2PRODUCT
! ==================================================================================================

!== begin carbon_crop ==============================================================================

 SUBROUTINE CARBON_CROP (parameters,NSNOW  ,NSOIL  ,VEGTYP ,DT     ,ZSOIL  ,JULIAN , & !in
                            DZSNSO ,STC    ,SMC    ,TV     ,PSN    ,FOLN   ,BTRAN  , & !in
                            SOLDN  ,T2M    ,                                         & !in
                            LFMASS ,RTMASS ,STMASS ,WOOD   ,STBLCP ,FASTCP ,GRAIN  , & !inout
			    XLAI   ,XSAI   ,GDD    ,                                 & !inout
                            GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,TOTSC  ,TOTLB, PGS    ) !out
! ------------------------------------------------------------------------------------------
! Initial crop version created by Xing Liu
! Initial crop version added by Barlage v3.8

! ------------------------------------------------------------------------------------------
      IMPLICIT NONE
! ------------------------------------------------------------------------------------------
! inputs (carbon)

  type (noahmp_parameters), intent(in) :: parameters
  INTEGER                        , INTENT(IN) :: NSNOW  !number of snow layers
  INTEGER                        , INTENT(IN) :: NSOIL  !number of soil layers
  INTEGER                        , INTENT(IN) :: VEGTYP !vegetation type 
  REAL                           , INTENT(IN) :: DT     !time step (s)
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: ZSOIL  !depth of layer-bottomfrom soil surface
  REAL                           , INTENT(IN) :: JULIAN !Julian day of year(fractional) ( 0 <= JULIAN < YEARLEN )
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: DZSNSO !snow/soil layerthickness [m]
  REAL, DIMENSION(-NSNOW+1:NSOIL), INTENT(IN) :: STC    !snow/soil temperature[k]
  REAL, DIMENSION(       1:NSOIL), INTENT(IN) :: SMC    !soil moisture (ice +liq.) [m3/m3]
  REAL                           , INTENT(IN) :: TV     !vegetation temperature(k)
  REAL                           , INTENT(IN) :: PSN    !total leaf photosyn(umolco2/m2/s) [+]
  REAL                           , INTENT(IN) :: FOLN   !foliage nitrogen (%)
  REAL                           , INTENT(IN) :: BTRAN  !soil watertranspiration factor (0 to 1)
  REAL                           , INTENT(IN) :: SOLDN  !Downward solar radiation
  REAL                           , INTENT(IN) :: T2M    !air temperature

! input & output (carbon)

  REAL                        , INTENT(INOUT) :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT) :: RTMASS !mass of fine roots[g/m2]
  REAL                        , INTENT(INOUT) :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT) :: WOOD   !mass of wood (incl.woody roots) [g/m2]
  REAL                        , INTENT(INOUT) :: STBLCP !stable carbon in deepsoil [g/m2]
  REAL                        , INTENT(INOUT) :: FASTCP !short-lived carbon inshallow soil [g/m2]
  REAL                        , INTENT(INOUT) :: GRAIN  !mass of GRAIN [g/m2]
  REAL                        , INTENT(INOUT) :: XLAI   !leaf area index [-]
  REAL                        , INTENT(INOUT) :: XSAI   !stem area index [-]
  REAL                        , INTENT(INOUT) :: GDD    !growing degree days

! outout
  REAL                          , INTENT(OUT) :: GPP    !net instantaneous assimilation [g/m2/s C]
  REAL                          , INTENT(OUT) :: NPP    !net primary productivity [g/m2/s C]
  REAL                          , INTENT(OUT) :: NEE    !net ecosystem exchange[g/m2/s CO2]
  REAL                          , INTENT(OUT) :: AUTORS !net ecosystem respiration [g/m2/s C]
  REAL                          , INTENT(OUT) :: HETERS !organic respiration[g/m2/s C]
  REAL                          , INTENT(OUT) :: TOTSC  !total soil carbon [g/m2C]
  REAL                          , INTENT(OUT) :: TOTLB  !total living carbon ([g/m2 C]

! local variables

  INTEGER :: J         !do-loop index
  REAL    :: WROOT     !root zone soil water [-]
  REAL    :: WSTRES    !water stress coeficient [-]  (1. for wilting )
  INTEGER :: IPA       !Planting index
  INTEGER :: IHA       !Havestindex(0=on,1=off)
  INTEGER, INTENT(OUT) :: PGS       !Plant growth stage

  REAL    :: PSNCROP 

! ------------------------------------------------------------------------------------------
   IF ( ( VEGTYP == parameters%iswater ) .OR. ( VEGTYP == parameters%ISBARREN ) .OR. &
        ( VEGTYP == parameters%ISICE ) .or. (parameters%urban_flag) ) THEN
      XLAI   = 0.
      XSAI   = 0.
      GPP    = 0.
      NPP    = 0.
      NEE    = 0.
      AUTORS = 0.
      HETERS = 0.
      TOTSC  = 0.
      TOTLB  = 0.
      LFMASS = 0.
      RTMASS = 0.
      STMASS = 0.
      WOOD   = 0.
      STBLCP = 0.
      FASTCP = 0.
      GRAIN  = 0.
      RETURN
   END IF

! water stress


   WSTRES  = 1.- BTRAN

   WROOT  = 0.
   DO J=1,parameters%NROOT
     WROOT = WROOT + SMC(J)/parameters%SMCMAX(J) *  DZSNSO(J) / (-ZSOIL(parameters%NROOT))
   ENDDO

   CALL PSN_CROP     ( parameters,                           & !in
                       SOLDN,   XLAI,    T2M,                & !in 
                       PSNCROP                             )   !out

   CALL GROWING_GDD  (parameters,                           & !in
                      T2M ,   DT,  JULIAN,                  & !in
                      GDD ,                                 & !inout 
                      IPA ,  IHA,     PGS)                    !out                        

   CALL CO2FLUX_CROP (parameters,                              & !in
                      DT     ,STC(1) ,PSN    ,TV     ,WROOT  ,WSTRES ,FOLN   , & !in
                      IPA    ,IHA    ,PGS    ,                                 & !in XING
                      XLAI   ,XSAI   ,LFMASS ,RTMASS ,STMASS ,                 & !inout
                      FASTCP ,STBLCP ,WOOD   ,GRAIN  ,GDD    ,                 & !inout
                      GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,                 & !out
                      TOTSC  ,TOTLB  )                                           !out

  END SUBROUTINE CARBON_CROP

!== begin co2flux_crop =============================================================================

  SUBROUTINE CO2FLUX_CROP (parameters,                                              & !in
                           DT     ,STC    ,PSN    ,TV     ,WROOT  ,WSTRES ,FOLN   , & !in
                           IPA    ,IHA    ,PGS    ,                                 & !in XING
                           XLAI   ,XSAI   ,LFMASS ,RTMASS ,STMASS ,                 & !inout
                           FASTCP ,STBLCP ,WOOD   ,GRAIN  ,GDD,                     & !inout
                           GPP    ,NPP    ,NEE    ,AUTORS ,HETERS ,                 & !out
                           TOTSC  ,TOTLB  )                                           !out
! -----------------------------------------------------------------------------------------
! The original code from RE Dickinson et al.(1998) and Guo-Yue Niu(2004),
! modified by Xing Liu, 2014.
! 
! -----------------------------------------------------------------------------------------
  IMPLICIT NONE
! -----------------------------------------------------------------------------------------

! input

  type (noahmp_parameters), intent(in) :: parameters
  REAL                           , INTENT(IN) :: DT     !time step (s)
  REAL                           , INTENT(IN) :: STC    !soil temperature[k]
  REAL                           , INTENT(IN) :: PSN    !total leaf photosynthesis (umolco2/m2/s)
  REAL                           , INTENT(IN) :: TV     !leaf temperature (k)
  REAL                           , INTENT(IN) :: WROOT  !root zone soil water
  REAL                           , INTENT(IN) :: WSTRES !soil water stress
  REAL                           , INTENT(IN) :: FOLN   !foliage nitrogen (%)
  INTEGER                        , INTENT(IN) :: IPA
  INTEGER                        , INTENT(IN) :: IHA
  INTEGER                        , INTENT(IN) :: PGS

! input and output

  REAL                        , INTENT(INOUT) :: XLAI   !leaf  area index from leaf carbon [-]
  REAL                        , INTENT(INOUT) :: XSAI   !stem area index from leaf carbon [-]
  REAL                        , INTENT(INOUT) :: LFMASS !leaf mass [g/m2]
  REAL                        , INTENT(INOUT) :: RTMASS !mass of fine roots [g/m2]
  REAL                        , INTENT(INOUT) :: STMASS !stem mass [g/m2]
  REAL                        , INTENT(INOUT) :: FASTCP !short lived carbon [g/m2]
  REAL                        , INTENT(INOUT) :: STBLCP !stable carbon pool [g/m2]
  REAL                        , INTENT(INOUT) :: WOOD   !mass of wood (incl. woody roots) [g/m2]
  REAL                        , INTENT(INOUT) :: GRAIN  !mass of grain (XING) [g/m2]
  REAL                        , INTENT(INOUT) :: GDD    !growing degree days (XING)

! output

  REAL                          , INTENT(OUT) :: GPP    !net instantaneous assimilation [g/m2/s]
  REAL                          , INTENT(OUT) :: NPP    !net primary productivity [g/m2]
  REAL                          , INTENT(OUT) :: NEE    !net ecosystem exchange (autors+heters-gpp)
  REAL                          , INTENT(OUT) :: AUTORS !net ecosystem resp. (maintance and growth)
  REAL                          , INTENT(OUT) :: HETERS !organic respiration
  REAL                          , INTENT(OUT) :: TOTSC  !total soil carbon (g/m2)
  REAL                          , INTENT(OUT) :: TOTLB  !total living carbon (g/m2)

! local

  REAL                   :: CFLUX    !carbon flux to atmosphere [g/m2/s]
  REAL                   :: LFMSMN   !minimum leaf mass [g/m2]
  REAL                   :: RSWOOD   !wood respiration [g/m2]
  REAL                   :: RSLEAF   !leaf maintenance respiration per timestep[g/m2]
  REAL                   :: RSROOT   !fine root respiration per time step [g/m2]
  REAL                   :: RSGRAIN  !grain respiration [g/m2]  
  REAL                   :: NPPL     !leaf net primary productivity [g/m2/s]
  REAL                   :: NPPR     !root net primary productivity [g/m2/s]
  REAL                   :: NPPW     !wood net primary productivity [g/m2/s]
  REAL                   :: NPPS     !wood net primary productivity [g/m2/s]
  REAL                   :: NPPG     !grain net primary productivity [g/m2/s] 
  REAL                   :: DIELF    !death of leaf mass per time step [g/m2]

  REAL                   :: ADDNPPLF !leaf assimil after resp. losses removed[g/m2]
  REAL                   :: ADDNPPST !stem assimil after resp. losses removed[g/m2]
  REAL                   :: CARBFX   !carbon assimilated per model step [g/m2]
  REAL                   :: CBHYDRAFX!carbonhydrate assimilated per model step [g/m2]
  REAL                   :: GRLEAF   !growth respiration rate for leaf [g/m2/s]
  REAL                   :: GRROOT   !growth respiration rate for root [g/m2/s]
  REAL                   :: GRWOOD   !growth respiration rate for wood [g/m2/s]
  REAL                   :: GRSTEM   !growth respiration rate for stem [g/m2/s]
  REAL                   :: GRGRAIN   !growth respiration rate for stem [g/m2/s]
  REAL                   :: LEAFPT   !fraction of carbon allocated to leaves [-]
  REAL                   :: LFDEL    !maximum  leaf mass  available to change[g/m2/s]
  REAL                   :: LFTOVR   !stem turnover per time step [g/m2]
  REAL                   :: STTOVR   !stem turnover per time step [g/m2]
  REAL                   :: WDTOVR   !wood turnover per time step [g/m2]
  REAL                   :: GRTOVR   !grainturnover per time step [g/m2]
  REAL                   :: RSSOIL   !soil respiration per time step [g/m2]
  REAL                   :: RTTOVR   !root carbon loss per time step by turnover[g/m2]
  REAL                   :: STABLC   !decay rate of fast carbon to slow carbon[g/m2/s]
  REAL                   :: WOODF    !calculated wood to root ratio [-]
  REAL                   :: NONLEF   !fraction of carbon to root and wood [-]
  REAL                   :: RESP     !leaf respiration [umol/m2/s]
  REAL                   :: RSSTEM   !stem respiration [g/m2/s]

  REAL                   :: FSW      !soil water factor for microbial respiration
  REAL                   :: FST      !soil temperature factor for microbialrespiration
  REAL                   :: FNF      !foliage nitrogen adjustemt to respiration(<= 1)
  REAL                   :: TF       !temperature factor
  REAL                   :: STDEL
  REAL                   :: STMSMN
  REAL                   :: SAPM     !stem area per unit mass (m2/g)
  REAL                   :: DIEST
  REAL                   :: STCONVERT   !stem to grain conversion [g/m2/s]
  REAL                   :: RTCONVERT   !root to grain conversion [g/m2/s]
! -------------------------- constants -------------------------------
  REAL                   :: BF       !parameter for present wood allocation [-]
  REAL                   :: RSWOODC  !wood respiration coeficient [1/s]
  REAL                   :: STOVRC   !stem turnover coefficient [1/s]
  REAL                   :: RSDRYC   !degree of drying that reduces soilrespiration [-]
  REAL                   :: RTOVRC   !root turnover coefficient [1/s]
  REAL                   :: WSTRC    !water stress coeficient [-]
  REAL                   :: LAIMIN   !minimum leaf area index [m2/m2]
  REAL                   :: XSAMIN   !minimum leaf area index [m2/m2]
  REAL                   :: SC
  REAL                   :: SD
  REAL                   :: VEGFRAC
  REAL                   :: TEMP

! Respiration as a function of temperature

  real :: r,x
          r(x) = exp(0.08*(x-298.16))
! ---------------------------------------------------------------------------------

! constants
    RSDRYC  = 40.0          !original was 40.0
    RSWOODC = 3.0E-10       !
    BF      = 0.90          !original was 0.90   ! carbon to roots
    WSTRC   = 100.0
    LAIMIN  = 0.05
    XSAMIN  = 0.05

    SAPM    = 3.*0.001      ! m2/kg -->m2/g
    LFMSMN  = laimin/0.035
    STMSMN  = xsamin/sapm
! ---------------------------------------------------------------------------------

! carbon assimilation
! 1 mole -> 12 g carbon or 44 g CO2 or 30 g CH20

     CARBFX     = PSN*12.e-6!*IPA   !umol co2 /m2/ s -> g/m2/s C
     CBHYDRAFX  = PSN*30.e-6!*IPA

! mainteinance respiration
     FNF     = MIN( FOLN/MAX(1.E-06,parameters%FOLN_MX), 1.0 )
     TF      = parameters%Q10MR**( (TV-298.16)/10. )
     RESP    = parameters%LFMR25 * TF * FNF * XLAI  * (1.-WSTRES)  ! umol/m2/s
     RSLEAF  = MIN((LFMASS-LFMSMN)/DT,RESP*30.e-6)                       ! g/m2/s
     RSROOT  = parameters%RTMR25*(RTMASS*1E-3)*TF * 30.e-6         ! g/m2/s
     RSSTEM  = parameters%STMR25*(STMASS*1E-3)*TF * 30.e-6         ! g/m2/s
     RSGRAIN = parameters%GRAINMR25*(GRAIN*1E-3)*TF * 30.e-6       ! g/m2/s

! calculate growth respiration for leaf, rtmass and grain

     GRLEAF  = MAX(0.0,parameters%FRA_GR*(parameters%LFPT(PGS)*CBHYDRAFX  - RSLEAF))
     GRSTEM  = MAX(0.0,parameters%FRA_GR*(parameters%STPT(PGS)*CBHYDRAFX  - RSSTEM))
     GRROOT  = MAX(0.0,parameters%FRA_GR*(parameters%RTPT(PGS)*CBHYDRAFX  - RSROOT))
     GRGRAIN = MAX(0.0,parameters%FRA_GR*(parameters%GRAINPT(PGS)*CBHYDRAFX  - RSGRAIN))

! leaf turnover, stem turnover, root turnover and leaf death caused by soil
! water and soil temperature stress

     LFTOVR  = parameters%LF_OVRC(PGS)*1.E-6*LFMASS
     RTTOVR  = parameters%RT_OVRC(PGS)*1.E-6*RTMASS
     STTOVR  = parameters%ST_OVRC(PGS)*1.E-6*STMASS
     SC  = EXP(-0.3*MAX(0.,TV-parameters%LEFREEZ)) * (LFMASS/120.)
     SD  = EXP((WSTRES-1.)*WSTRC)
     DIELF = LFMASS*1.E-6*(parameters%DILE_FW(PGS) * SD + parameters%DILE_FC(PGS)*SC)

! Allocation of CBHYDRAFX to leaf, stem, root and grain at each growth stage


     ADDNPPLF    = MAX(0.,parameters%LFPT(PGS)*CBHYDRAFX - GRLEAF-RSLEAF)
     ADDNPPLF    = parameters%LFPT(PGS)*CBHYDRAFX - GRLEAF-RSLEAF
     ADDNPPST    = MAX(0.,parameters%STPT(PGS)*CBHYDRAFX - GRSTEM-RSSTEM)
     ADDNPPST    = parameters%STPT(PGS)*CBHYDRAFX - GRSTEM-RSSTEM
    

! avoid reducing leaf mass below its minimum value but conserve mass

     LFDEL = (LFMASS - LFMSMN)/DT
     STDEL = (STMASS - STMSMN)/DT
     LFTOVR  = MIN(LFTOVR,LFDEL+ADDNPPLF)
     STTOVR  = MIN(STTOVR,STDEL+ADDNPPST)
     DIELF = MIN(DIELF,LFDEL+ADDNPPLF-LFTOVR)

! net primary productivities

     NPPL   = MAX(ADDNPPLF,-LFDEL)
     NPPL   = ADDNPPLF
     NPPS   = MAX(ADDNPPST,-STDEL)
     NPPS   = ADDNPPST
     NPPR   = parameters%RTPT(PGS)*CBHYDRAFX - RSROOT - GRROOT
     NPPG  =  parameters%GRAINPT(PGS)*CBHYDRAFX - RSGRAIN - GRGRAIN

! masses of plant components
  
     LFMASS = LFMASS + (NPPL-LFTOVR-DIELF)*DT
     STMASS = STMASS + (NPPS-STTOVR)*DT   ! g/m2
     RTMASS = RTMASS + (NPPR-RTTOVR)*DT
     GRAIN =  GRAIN + NPPG*DT 

     GPP = CBHYDRAFX* 0.4 !!g/m2/s C  0.4=12/30, CH20 to C

     STCONVERT = 0.0
     RTCONVERT = 0.0
     IF(PGS==6) THEN
       STCONVERT = STMASS*(0.00005*DT/3600.0)
       STMASS = STMASS - STCONVERT
       RTCONVERT = RTMASS*(0.0005*DT/3600.0)
       RTMASS = RTMASS - RTCONVERT
       GRAIN  = GRAIN + STCONVERT + RTCONVERT
     END IF
    
     IF(RTMASS.LT.0.0) THEN
       RTTOVR = NPPR
       RTMASS = 0.0
     ENDIF

     IF(GRAIN.LT.0.0) THEN
       GRAIN = 0.0
     ENDIF

 ! soil carbon budgets

!     IF(PGS == 1 .OR. PGS == 2 .OR. PGS == 8) THEN
!       FASTCP=1000
!     ELSE
       FASTCP = FASTCP + (RTTOVR+LFTOVR+STTOVR+DIELF)*DT 
!     END IF
     FST = 2.0**( (STC-283.16)/10. )
     FSW = WROOT / (0.20+WROOT) * 0.23 / (0.23+WROOT)
     RSSOIL = FSW * FST * parameters%MRP* MAX(0.,FASTCP*1.E-3)*12.E-6

     STABLC = 0.1*RSSOIL
     FASTCP = FASTCP - (RSSOIL + STABLC)*DT
     STBLCP = STBLCP + STABLC*DT

!  total carbon flux

     CFLUX  = - CARBFX + RSLEAF + RSROOT  + RSSTEM &
              + RSSOIL + GRLEAF + GRROOT                  ! g/m2/s 0.4=12/30, CH20 to C

! for outputs
                                                                 !g/m2/s C

     NPP   = (NPPL + NPPS+ NPPR +NPPG)*0.4      !!g/m2/s C  0.4=12/30, CH20 to C
 
  
     AUTORS = RSROOT + RSGRAIN  + RSLEAF +  &                     !g/m2/s C
              GRLEAF + GRROOT + GRGRAIN                           !g/m2/s C

     HETERS = RSSOIL                                             !g/m2/s C
     NEE    = (AUTORS + HETERS - GPP)*44./30.                    !g/m2/s CO2
     TOTSC  = FASTCP + STBLCP                                    !g/m2   C

     TOTLB  = LFMASS + RTMASS + GRAIN         

! leaf area index and stem area index
  
     XLAI    = MAX(LFMASS*parameters%BIO2LAI,LAIMIN)
     XSAI    = MAX(STMASS*SAPM,XSAMIN)

   
!After harversting
!     IF(PGS == 8 ) THEN
!       LFMASS = 0.62
!       STMASS = 0
!       GRAIN  = 0
!     END IF

!    IF(PGS == 1 .OR. PGS == 2 .OR. PGS == 8) THEN
    IF(PGS == 8 .and. (GRAIN > 0. .or. LFMASS > 0 .or. STMASS > 0 .or. RTMASS > 0)) THEN
     XLAI   = 0.05
     XSAI   = 0.05
     LFMASS = LFMSMN
     STMASS = STMSMN
     RTMASS = 0
     GRAIN  = 0
    END IF 
    
END SUBROUTINE CO2FLUX_CROP

!== begin growing_gdd ==============================================================================

  SUBROUTINE GROWING_GDD (parameters,                         & !in
                          T2M ,   DT, JULIAN,                 & !in
                          GDD ,                               & !inout 
                          IPA,   IHA,     PGS)                  !out  
!===================================================================================================

! input

  type (noahmp_parameters), intent(in) :: parameters
   REAL                     , INTENT(IN)        :: T2M     !Air temperature
   REAL                     , INTENT(IN)        :: DT      !time step (s)
   REAL                     , INTENT(IN)        :: JULIAN  !Julian day of year (fractional) ( 0 <= JULIAN < YEARLEN )

! input and output

   REAL                     , INTENT(INOUT)     :: GDD     !growing degress days

! output

   INTEGER                  , INTENT(OUT)       :: IPA     !Planting index index(0=off, 1=on)
   INTEGER                  , INTENT(OUT)       :: IHA     !Havestindex(0=on,1=off) 
   INTEGER                  , INTENT(OUT)       :: PGS     !Plant growth stage(1=S1,2=S2,3=S3)

!local 

   REAL                                         :: GDDDAY    !gap bewtween GDD and GDD8
   REAL                                         :: DAYOFS2   !DAYS in stage2
   REAL                                         :: TDIFF     !temperature difference for growing degree days calculation
   REAL                                         :: TC

   TC = T2M - 273.15

!Havestindex(0=on,1=off) 

   IPA = 1
   IHA = 1

!turn on/off the planting 
 
   IF(JULIAN < parameters%PLTDAY)  IPA = 0

!turn on/off the harvesting
    IF(JULIAN >= parameters%HSDAY) IHA = 0
   
!Calculate the growing degree days
   
    IF(TC <  parameters%GDDTBASE) THEN
      TDIFF = 0.0
    ELSEIF(TC >= parameters%GDDTCUT) THEN
      TDIFF = parameters%GDDTCUT - parameters%GDDTBASE
    ELSE
      TDIFF = TC - parameters%GDDTBASE
    END IF

    GDD     = (GDD + TDIFF) * IPA * IHA

    GDDDAY  = GDD / (86400.0 / DT)

   ! Decide corn growth stage, based on Hybrid-Maize 
   !   PGS = 1 : Before planting
   !   PGS = 2 : from tassel initiation to silking
   !   PGS = 3 : from silking to effective grain filling
   !   PGS = 4 : from effective grain filling to pysiological maturity 
   !   PGS = 5 : GDDM=1389
   !   PGS = 6 :
   !   PGS = 7 :
   !   PGS = 8 :
   !  GDDM = 1389
   !  GDDM = 1555
   ! GDDSK = 0.41*GDDM +145.4+150 !from hybrid-maize 
   ! GDDS1 = ((GDDSK-96)/38.9-4)*21
   ! GDDS1 = 0.77*GDDSK
   ! GDDS3 = GDDSK+170
   ! GDDS3 = 170

   PGS = 1                         ! MB: set PGS = 1 (for initialization during growing season when no GDD)

   IF(GDDDAY > 0.0) PGS = 2

   IF(GDDDAY >= parameters%GDDS1)  PGS = 3

   IF(GDDDAY >= parameters%GDDS2)  PGS = 4 

   IF(GDDDAY >= parameters%GDDS3)  PGS = 5

   IF(GDDDAY >= parameters%GDDS4)  PGS = 6

   IF(GDDDAY >= parameters%GDDS5)  PGS = 7

   IF(JULIAN >= parameters%HSDAY)  PGS = 8
 
   IF(JULIAN <  parameters%PLTDAY) PGS = 1   

END SUBROUTINE GROWING_GDD

!== begin psn_crop =================================================================================

SUBROUTINE PSN_CROP ( parameters,       & !in
                      SOLDN, XLAI,T2M,  & !in
                      PSNCROP        )    !out
!===================================================================================================

! input

  type (noahmp_parameters), intent(in) :: parameters
  REAL     , INTENT(IN)    :: SOLDN    ! downward solar radiation
  REAL     , INTENT(IN)    :: XLAI     ! LAI
  REAL     , INTENT(IN)    :: T2M      ! air temp
  REAL     , INTENT(OUT)   :: PSNCROP  !

!local

  REAL                     :: PAR      ! photosynthetically active radiation (w/m2) 1 W m-2 = 0.0864 MJ m-2 day-1
  REAL                     :: Amax     ! Maximum CO2 assimulation rate g/co2/s  
  REAL                     :: L1       ! Three Gaussian method
  REAL                     :: L2       ! Three Gaussian method
  REAL                     :: L3       ! Three Gaussian method
  REAL                     :: I1       ! Three Gaussian method
  REAL                     :: I2       ! Three Gaussian method
  REAL                     :: I3       ! Three Gaussian method
  REAL                     :: A1       ! Three Gaussian method
  REAL                     :: A2       ! Three Gaussian method
  REAL                     :: A3       ! Three Gaussian method
  REAL                     :: A        ! CO2 Assimulation 
  REAL                     :: TC

  TC = T2M - 273.15

  PAR = parameters%I2PAR * SOLDN * 0.0036  !w to MJ m-2

  IF(TC < parameters%TASSIM0) THEN
    Amax = 1E-10
  ELSEIF(TC >= parameters%TASSIM0 .and. TC < parameters%TASSIM1) THEN
    Amax = (TC - parameters%TASSIM0) * parameters%Aref / (parameters%TASSIM1 - parameters%TASSIM0)
  ELSEIF(TC >= parameters%TASSIM1 .and. TC < parameters%TASSIM2) THEN
    Amax = parameters%Aref
  ELSE
    Amax= parameters%Aref - 0.2 * (T2M - parameters%TASSIM2)
  ENDIF 
  
  Amax = max(amax,0.01)

  IF(XLAI <= 0.05) THEN
    L1 = 0.1127 * 0.05   !use initial LAI(0.05), avoid error
    L2 = 0.5    * 0.05
    L3 = 0.8873 * 0.05
  ELSE
    L1 = 0.1127 * XLAI
    L2 = 0.5    * XLAI
    L3 = 0.8873 * XLAI
  END IF

  I1 = parameters%k * PAR * exp(-parameters%k * L1)
  I2 = parameters%k * PAR * exp(-parameters%k * L2)
  I3 = parameters%k * PAR * exp(-parameters%k * L3)

  I1 = max(I1,1E-10)
  I2 = max(I2,1E-10)
  I3 = max(I3,1E-10)

  A1 = Amax * (1 - exp(-parameters%epsi * I1 / Amax))
  A2 = Amax * (1 - exp(-parameters%epsi * I2 / Amax)) * 1.6
  A3 = Amax * (1 - exp(-parameters%epsi * I3 / Amax))

  IF (XLAI <= 0.05) THEN
    A  = (A1+A2+A3) / 3.6 * 0.05
  ELSEIF (XLAI > 0.05 .and. XLAI <= 4.0) THEN
    A  = (A1+A2+A3) / 3.6 * XLAI
  ELSE
    A = (A1+A2+A3) / 3.6 * 4
  END IF

  A = A * parameters%PSNRF ! Attainable 

  PSNCROP = 6.313 * A   ! (1/44) * 1000000)/3600 = 6.313

END SUBROUTINE PSN_CROP

!== begin bvocflux =================================================================================

!  SUBROUTINE BVOCFLUX(parameters,VOCFLX,  VEGTYP,  VEGFRAC,  APAR,   TV )
!
! ------------------------------------------------------------------------------------------
!      implicit none
! ------------------------------------------------------------------------------------------
!
! ------------------------ code history ---------------------------
! source file:       BVOC
! purpose:           BVOC emissions
! DESCRIPTION:
! Volatile organic compound emission 
! This code simulates volatile organic compound emissions
! following the algorithm presented in Guenther, A., 1999: Modeling
! Biogenic Volatile Organic Compound Emissions to the Atmosphere. In
! Reactive Hydrocarbons in the Atmosphere, Ch. 3
! This model relies on the assumption that 90% of isoprene and monoterpene
! emissions originate from canopy foliage:
!    E = epsilon * gamma * density * delta
! The factor delta (longterm activity factor) applies to isoprene emission
! from deciduous plants only. We neglect this factor at the present time.
! This factor is discussed in Guenther (1997).
! Subroutine written to operate at the patch level.
! IN FINAL IMPLEMENTATION, REMEMBER:
! 1. may wish to call this routine only as freq. as rad. calculations
! 2. may wish to place epsilon values directly in pft-physiology file
! ------------------------ input/output variables -----------------
! input
!  integer                     ,INTENT(IN) :: vegtyp  !vegetation type 
!  real                        ,INTENT(IN) :: vegfrac !green vegetation fraction [0.0-1.0]
!  real                        ,INTENT(IN) :: apar    !photosynthesis active energy by canopy (w/m2)
!  real                        ,INTENT(IN) :: tv      !vegetation canopy temperature (k)
!
! output
!  real                        ,INTENT(OUT) :: vocflx(5) ! voc fluxes [ug C m-2 h-1]
!
! Local Variables
!
!  real, parameter :: R      = 8.314    ! univ. gas constant [J K-1 mol-1]
!  real, parameter :: alpha  = 0.0027   ! empirical coefficient
!  real, parameter :: cl1    = 1.066    ! empirical coefficient
!  real, parameter :: ct1    = 95000.0  ! empirical coefficient [J mol-1]
!  real, parameter :: ct2    = 230000.0 ! empirical coefficient [J mol-1]
!  real, parameter :: ct3    = 0.961    ! empirical coefficient
!  real, parameter :: tm     = 314.0    ! empirical coefficient [K]
!  real, parameter :: tstd   = 303.0    ! std temperature [K]
!  real, parameter :: bet    = 0.09     ! beta empirical coefficient [K-1]
!
!  integer ivoc        ! do-loop index
!  integer ityp        ! do-loop index
!  real epsilon(5)
!  real gamma(5)
!  real density
!  real elai
!  real par,cl,reciprod,ct
!
! epsilon :
!
!    do ivoc = 1, 5
!    epsilon(ivoc) = parameters%eps(VEGTYP,ivoc)
!    end do
!
! gamma : Activity factor. Units [dimensionless]
!
!      reciprod = 1. / (R * tv * tstd)
!      ct = exp(ct1 * (tv - tstd) * reciprod) / &
!           (ct3 + exp(ct2 * (tv - tm) * reciprod))
!
!      par = apar * 4.6 ! (multiply w/m2 by 4.6 to get umol/m2/s)
!      cl  = alpha * cl1 * par * (1. + alpha * alpha * par * par)**(-0.5)
!
!   gamma(1) = cl * ct ! for isoprenes
!
!   do ivoc = 2, 5
!   gamma(ivoc) = exp(bet * (tv - tstd))
!   end do
!
! Foliage density
!
! transform vegfrac to lai      
!
!   elai    = max(0.0,-6.5/2.5*alog((1.-vegfrac)))
!   density = elai / (parameters%slarea(VEGTYP) * 0.5)
!
! calculate the voc flux
!
!   do ivoc = 1, 5
!   vocflx(ivoc) = epsilon(ivoc) * gamma(ivoc) * density
!   end do
!
!   end subroutine bvocflux
! ==================================================================================================
! ********************************* end of carbon subroutines *****************************
! ==================================================================================================

!== begin noahmp_options ===========================================================================

  subroutine noahmp_options(idveg     ,iopt_crs  ,iopt_btr  ,iopt_run  ,iopt_sfc  ,iopt_frz , & 
                             iopt_inf  ,iopt_rad  ,iopt_alb  ,iopt_snf  ,iopt_tbot, iopt_stc, &
			     iopt_rsf ,iopt_root  ,iopt_watret, iopt_scm)

  implicit none

  INTEGER,  INTENT(IN) :: idveg     !dynamic vegetation (1 -> off ; 2 -> on) with opt_crs = 1
  INTEGER,  INTENT(IN) :: iopt_crs  !canopy stomatal resistance (1-> Ball-Berry; 2->Jarvis)
  INTEGER,  INTENT(IN) :: iopt_btr  !soil moisture factor for stomatal resistance (1-> Noah; 2-> CLM; 3-> SSiB)
  INTEGER,  INTENT(IN) :: iopt_run  !runoff and groundwater (1->SIMGM; 2->SIMTOP; 3->Schaake96; 4->BATS)
  INTEGER,  INTENT(IN) :: iopt_sfc  !surface layer drag coeff (CH & CM) (1->M-O; 2->Chen97)
  INTEGER,  INTENT(IN) :: iopt_frz  !supercooled liquid water (1-> NY06; 2->Koren99)
  INTEGER,  INTENT(IN) :: iopt_inf  !frozen soil permeability (1-> NY06; 2->Koren99)
  INTEGER,  INTENT(IN) :: iopt_rad  !radiation transfer (1->gap=F(3D,cosz); 2->gap=0; 3->gap=1-Fveg)
  INTEGER,  INTENT(IN) :: iopt_alb  !snow surface albedo (1->BATS; 2->CLASS)
  INTEGER,  INTENT(IN) :: iopt_snf  !rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
  INTEGER,  INTENT(IN) :: iopt_tbot !lower boundary of soil temperature (1->zero-flux; 2->Noah)

  INTEGER,  INTENT(IN) :: iopt_stc  !snow/soil temperature time scheme (only layer 1)
                                    ! 1 -> semi-implicit; 2 -> full implicit (original Noah)
  INTEGER,  INTENT(IN) :: iopt_rsf  !surface resistance (1->Sakaguchi/Zeng; 2->Seller; 3->mod Sellers; 4->1+snow)
  INTEGER,  INTENT(IN) :: iopt_root !1 -> dynamic root; 2 -> static, even root profile
  INTEGER,  INTENT(IN) :: iopt_watret  ! soil water retention 1 -> van Genutchen; 2 -> Clapp & Hornberger
  INTEGER,  INTENT(IN) :: iopt_scm  !soil carbon model; 1 -> 1st-order decay; 2 -> 4C  pool; 3 -> 6C  pool

! -------------------------------------------------------------------------------------------------

  dveg = idveg
  
  opt_crs  = iopt_crs  
  opt_btr  = iopt_btr  
  opt_run  = iopt_run  
  opt_sfc  = iopt_sfc  
  opt_frz  = iopt_frz  
  opt_inf  = iopt_inf  
  opt_rad  = iopt_rad  
  opt_alb  = iopt_alb  
  opt_snf  = iopt_snf  
  opt_tbot = iopt_tbot 
  opt_stc  = iopt_stc
  opt_rsf  = iopt_rsf
  opt_root = iopt_root
  opt_watret  = iopt_watret
  opt_scm  = iopt_scm
  
  end subroutine noahmp_options
 
END MODULE MODULE_SF_NOAHMPLSM

MODULE NOAHMP_TABLES

    IMPLICIT NONE

    INTEGER, PRIVATE, PARAMETER :: MVT   = 27
    INTEGER, PRIVATE, PARAMETER :: MBAND = 2
    INTEGER, PRIVATE, PARAMETER :: MSC   = 8
    INTEGER, PRIVATE, PARAMETER :: MAX_SOILTYP = 30
    INTEGER, PRIVATE, PARAMETER :: NCROP = 5
    INTEGER, PRIVATE, PARAMETER :: NSTAGE = 8
    INTEGER, PRIVATE, PARAMETER :: NRAD   = 1451

! ESM_SNICAR.TBL
    REAL :: ext_mie_bd_TABLE(NRAD, 5)
    REAL :: w_mie_bd_TABLE  (NRAD, 5)
    REAL :: g1_mie_bd_TABLE (NRAD, 5)
    REAL :: g2_mie_bd_TABLE (NRAD, 5)
    REAL :: g3_mie_bd_TABLE (NRAD, 5)
    REAL :: g4_mie_bd_TABLE (NRAD, 5)

    REAL :: ext_mie_lap_bd_TABLE(7, 5)
    REAL :: w_mie_lap_bd_TABLE  (7, 5)
    REAL :: g_mie_lap_bd_TABLE  (7, 5)

    REAL :: T_TABLE(11)
    REAL :: sno_dns_TABLE(8)
    REAL :: dTdz_TABLE(31)

    REAL :: drdt0_TABLE(8,31,11)
    REAL :: kappa_TABLE(8,31,11)
    REAL :: tau_TABLE  (8,31,11)

! MPTABLE.TBL vegetation parameters

    INTEGER :: ISURBAN_TABLE
    INTEGER :: ISWATER_TABLE
    INTEGER :: ISBARREN_TABLE
    INTEGER :: ISICE_TABLE
    INTEGER :: ISCROP_TABLE
    INTEGER :: EBLFOREST_TABLE
    INTEGER :: NATURAL_TABLE
    INTEGER :: LOW_DENSITY_RESIDENTIAL_TABLE
    INTEGER :: HIGH_DENSITY_RESIDENTIAL_TABLE
    INTEGER :: HIGH_INTENSITY_INDUSTRIAL_TABLE

    REAL :: CH2OP_TABLE(MVT)       !maximum intercepted h2o per unit lai+sai (mm)
    REAL :: DLEAF_TABLE(MVT)       !characteristic leaf dimension (m)
    REAL :: Z0MVT_TABLE(MVT)       !momentum roughness length (m)
    REAL :: HVT_TABLE(MVT)         !top of canopy (m)
    REAL :: HVB_TABLE(MVT)         !bottom of canopy (m)
    REAL :: DEN_TABLE(MVT)         !tree density (no. of trunks per m2)
    REAL :: RC_TABLE(MVT)          !tree crown radius (m)
    REAL :: MFSNO_TABLE(MVT)       !snowmelt curve parameter ()
    REAL :: SAIM_TABLE(MVT,12)     !monthly stem area index, one-sided
    REAL :: LAIM_TABLE(MVT,12)     !monthly leaf area index, one-sided
    REAL :: SLA_TABLE(MVT)         !single-side leaf area per Kg [m2/kg]
    REAL :: DILEFC_TABLE(MVT)      !coeficient for leaf stress death [1/s]
    REAL :: DILEFW_TABLE(MVT)      !coeficient for leaf stress death [1/s]
    REAL :: FRAGR_TABLE(MVT)       !fraction of growth respiration  !original was 0.3 
    REAL :: LTOVRC_TABLE(MVT)      !leaf turnover [1/s]

    REAL :: C3PSN_TABLE(MVT)       !photosynthetic pathway: 0. = c4, 1. = c3
    REAL :: KC25_TABLE(MVT)        !co2 michaelis-menten constant at 25c (pa)
    REAL :: AKC_TABLE(MVT)         !q10 for kc25
    REAL :: KO25_TABLE(MVT)        !o2 michaelis-menten constant at 25c (pa)
    REAL :: AKO_TABLE(MVT)         !q10 for ko25
    REAL :: VCMX25_TABLE(MVT)      !maximum rate of carboxylation at 25c (umol co2/m**2/s)
    REAL :: AVCMX_TABLE(MVT)       !q10 for vcmx25
    REAL :: BP_TABLE(MVT)          !minimum leaf conductance (umol/m**2/s)
    REAL :: MP_TABLE(MVT)          !slope of conductance-to-photosynthesis relationship
    REAL :: QE25_TABLE(MVT)        !quantum efficiency at 25c (umol co2 / umol photon)
    REAL :: AQE_TABLE(MVT)         !q10 for qe25
    REAL :: RMF25_TABLE(MVT)       !leaf maintenance respiration at 25c (umol co2/m**2/s)
    REAL :: RMS25_TABLE(MVT)       !stem maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: RMR25_TABLE(MVT)       !root maintenance respiration at 25c (umol co2/kg bio/s)
    REAL :: ARM_TABLE(MVT)         !q10 for maintenance respiration
    REAL :: FOLNMX_TABLE(MVT)      !foliage nitrogen concentration when f(n)=1 (%)
    REAL :: TMIN_TABLE(MVT)        !minimum temperature for photosynthesis (k)

    REAL :: XL_TABLE(MVT)          !leaf/stem orientation index
    REAL :: RHOL_TABLE(MVT,MBAND)  !leaf reflectance: 1=vis, 2=nir
    REAL :: RHOS_TABLE(MVT,MBAND)  !stem reflectance: 1=vis, 2=nir
    REAL :: TAUL_TABLE(MVT,MBAND)  !leaf transmittance: 1=vis, 2=nir
    REAL :: TAUS_TABLE(MVT,MBAND)  !stem transmittance: 1=vis, 2=nir

    REAL :: MRP_TABLE(MVT)         !microbial respiration parameter (umol co2 /kg c/ s)
    REAL :: CWPVT_TABLE(MVT)       !empirical canopy wind parameter

    REAL :: WRRAT_TABLE(MVT)       !wood to non-wood ratio
    REAL :: WDPOOL_TABLE(MVT)      !wood pool (switch 1 or 0) depending on woody or not [-]
    REAL :: TDLEF_TABLE(MVT)       !characteristic T for leaf freezing [K]

    INTEGER :: NROOT_TABLE(MVT)       !number of soil layers with root present
    REAL :: RGL_TABLE(MVT)         !Parameter used in radiation stress function
    REAL :: RS_TABLE(MVT)          !Minimum stomatal resistance [s m-1]
    REAL :: HS_TABLE(MVT)          !Parameter used in vapor pressure deficit function
    REAL :: TOPT_TABLE(MVT)        !Optimum transpiration air temperature [K]
    REAL :: RSMAX_TABLE(MVT)       !Maximal stomatal resistance [s m-1]

    REAL :: SRA_TABLE(MVT)         !min specific root area [m2/kg]
    REAL :: OMR_TABLE(MVT)         !root resistivity to water uptake [s]
    REAL :: MQX_TABLE(MVT)         !ratio of water storage to dry biomass [-]
    REAL :: RTOMAX_TABLE(MVT)      !max root turnover rate [g/m2/year]
    REAL :: RROOT_TABLE(MVT)       !mean radius of fine roots [mm]
    REAL :: SCEXP_TABLE(MVT)       !decay rate of cold stress for leaf death

! SOILPARM.TBL parameters

    INTEGER            :: SLCATS

    REAL :: BEXP_TABLE(MAX_SOILTYP)          !maximum intercepted h2o per unit lai+sai (mm)
    REAL :: SMCDRY_TABLE(MAX_SOILTYP)      !characteristic leaf dimension (m)
    REAL :: F1_TABLE(MAX_SOILTYP)         !momentum roughness length (m)
    REAL :: SMCMAX_TABLE(MAX_SOILTYP)      !top of canopy (m)
    REAL :: SMCREF_TABLE(MAX_SOILTYP)      !bottom of canopy (m)
    REAL :: PSISAT_TABLE(MAX_SOILTYP)      !tree density (no. of trunks per m2)
    REAL :: DKSAT_TABLE(MAX_SOILTYP)       !tree crown radius (m)
    REAL :: DWSAT_TABLE(MAX_SOILTYP)       !monthly stem area index, one-sided
    REAL :: SMCWLT_TABLE(MAX_SOILTYP)      !monthly leaf area index, one-sided
    REAL :: QUARTZ_TABLE(MAX_SOILTYP)         !single-side leaf area per Kg [m2/kg]
    REAL :: SMCR_TABLE(MAX_SOILTYP)        !van Genuchten residual soil moisture (m3/m3)
    REAL :: VGN_TABLE(MAX_SOILTYP)         !van Genuchten "n"
    REAL :: VGPSAT_TABLE(MAX_SOILTYP)      !van Genuchten air entry water pressure (m)

! GENPARM.TBL parameters

    REAL :: SLOPE_TABLE(9)    !slope factor for soil drainage
    
    REAL :: CSOIL_TABLE       !Soil heat capacity [J m-3 K-1]
    REAL :: REFDK_TABLE       !Parameter in the surface runoff parameterization
    REAL :: REFKDT_TABLE      !Parameter in the surface runoff parameterization
    REAL :: FRZK_TABLE        !Frozen ground parameter
    REAL :: ZBOT_TABLE        !Depth [m] of lower boundary soil temperature
    REAL :: CZIL_TABLE        !Parameter used in the calculation of the roughness length for heat

! MPTABLE.TBL radiation parameters

    REAL :: ALBSAT_TABLE(MSC,MBAND)   !saturated soil albedos: 1=vis, 2=nir
    REAL :: ALBDRY_TABLE(MSC,MBAND)   !dry soil albedos: 1=vis, 2=nir
    REAL :: ALBICE_TABLE(MBAND)       !albedo land ice: 1=vis, 2=nir
    REAL :: ALBLAK_TABLE(MBAND)       !albedo frozen lakes: 1=vis, 2=nir
    REAL :: OMEGAS_TABLE(MBAND)       !two-stream parameter omega for snow
    REAL :: BETADS_TABLE              !two-stream parameter betad for snow
    REAL :: BETAIS_TABLE              !two-stream parameter betad for snow
    REAL :: EG_TABLE(2)               !emissivity

! MPTABLE.TBL global parameters

    REAL :: CO2_TABLE      !co2 partial pressure
    REAL :: O2_TABLE       !o2 partial pressure
    REAL :: TIMEAN_TABLE   !gridcell mean topgraphic index (global mean)
    REAL :: FSATMX_TABLE   !maximum surface saturated fraction (global mean)
    REAL :: Z0SNO_TABLE    !snow surface roughness length (m) (0.002)
    REAL :: SSI_TABLE      !liquid water holding capacity for snowpack (m3/m3) (0.03)
    REAL :: SWEMX_TABLE    !new snow mass to fully cover old snow (mm)
    REAL :: RSURF_SNOW_TABLE    !surface resistance for snow(s/m)

! MPTABLE.TBL crop parameters

 INTEGER :: DEFAULT_CROP_TABLE          ! Default crop index
 INTEGER :: PLTDAY_TABLE(NCROP)         ! Planting date
 INTEGER :: HSDAY_TABLE(NCROP)          ! Harvest date
    REAL :: PLANTPOP_TABLE(NCROP)       ! Plant density [per ha] - used?
    REAL :: IRRI_TABLE(NCROP)           ! Irrigation strategy 0= non-irrigation 1=irrigation (no water-stress)

    REAL :: GDDTBASE_TABLE(NCROP)       ! Base temperature for GDD accumulation [C]
    REAL :: GDDTCUT_TABLE(NCROP)        ! Upper temperature for GDD accumulation [C]
    REAL :: GDDS1_TABLE(NCROP)          ! GDD from seeding to emergence
    REAL :: GDDS2_TABLE(NCROP)          ! GDD from seeding to initial vegetative 
    REAL :: GDDS3_TABLE(NCROP)          ! GDD from seeding to post vegetative 
    REAL :: GDDS4_TABLE(NCROP)          ! GDD from seeding to intial reproductive
    REAL :: GDDS5_TABLE(NCROP)          ! GDD from seeding to pysical maturity 

 INTEGER :: C3C4_TABLE(NCROP)           ! photosynthetic pathway:  1. = c3 2. = c4
    REAL :: AREF_TABLE(NCROP)           ! reference maximum CO2 assimulation rate 
    REAL :: PSNRF_TABLE(NCROP)          ! CO2 assimulation reduction factor(0-1) (caused by non-modeling part,e.g.pest,weeds)
    REAL :: I2PAR_TABLE(NCROP)          ! Fraction of incoming solar radiation to photosynthetically active radiation
    REAL :: TASSIM0_TABLE(NCROP)        ! Minimum temperature for CO2 assimulation [C]
    REAL :: TASSIM1_TABLE(NCROP)        ! CO2 assimulation linearly increasing until temperature reaches T1 [C]
    REAL :: TASSIM2_TABLE(NCROP)        ! CO2 assmilation rate remain at Aref until temperature reaches T2 [C]
    REAL :: K_TABLE(NCROP)              ! light extinction coefficient
    REAL :: EPSI_TABLE(NCROP)           ! initial light use efficiency

    REAL :: Q10MR_TABLE(NCROP)          ! q10 for maintainance respiration
    REAL :: FOLN_MX_TABLE(NCROP)        ! foliage nitrogen concentration when f(n)=1 (%)
    REAL :: LEFREEZ_TABLE(NCROP)        ! characteristic T for leaf freezing [K]

    REAL :: DILE_FC_TABLE(NCROP,NSTAGE) ! coeficient for temperature leaf stress death [1/s]
    REAL :: DILE_FW_TABLE(NCROP,NSTAGE) ! coeficient for water leaf stress death [1/s]
    REAL :: FRA_GR_TABLE(NCROP)         ! fraction of growth respiration

    REAL :: LF_OVRC_TABLE(NCROP,NSTAGE) ! fraction of leaf turnover  [1/s]
    REAL :: ST_OVRC_TABLE(NCROP,NSTAGE) ! fraction of stem turnover  [1/s]
    REAL :: RT_OVRC_TABLE(NCROP,NSTAGE) ! fraction of root tunrover  [1/s]
    REAL :: LFMR25_TABLE(NCROP)         !  leaf maintenance respiration at 25C [umol CO2/m**2  /s]
    REAL :: STMR25_TABLE(NCROP)         !  stem maintenance respiration at 25C [umol CO2/kg bio/s]
    REAL :: RTMR25_TABLE(NCROP)         !  root maintenance respiration at 25C [umol CO2/kg bio/s]
    REAL :: GRAINMR25_TABLE(NCROP)      ! grain maintenance respiration at 25C [umol CO2/kg bio/s]

    REAL :: LFPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to leaf
    REAL :: STPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to stem
    REAL :: RTPT_TABLE(NCROP,NSTAGE)    ! fraction of carbohydrate flux to root
    REAL :: GRAINPT_TABLE(NCROP,NSTAGE) ! fraction of carbohydrate flux to grain
    REAL :: BIO2LAI_TABLE(NCROP)        ! leaf are per living leaf biomass [m^2/kg]

CONTAINS

  subroutine read_esm_snicar_parameters()
    implicit none
    integer :: ierr
    logical :: file_named 

    REAL :: ext_mie_bd1(NRAD),ext_mie_bd2(NRAD),ext_mie_bd3(NRAD),ext_mie_bd4(NRAD),ext_mie_bd5(NRAD)
    REAL :: w_mie_bd1  (NRAD),w_mie_bd2  (NRAD),w_mie_bd3  (NRAD),w_mie_bd4  (NRAD),w_mie_bd5  (NRAD)
    REAL :: g1_mie_bd1 (NRAD),g1_mie_bd2 (NRAD),g1_mie_bd3 (NRAD),g1_mie_bd4 (NRAD),g1_mie_bd5 (NRAD)
    REAL :: g2_mie_bd1 (NRAD),g2_mie_bd2 (NRAD),g2_mie_bd3 (NRAD),g2_mie_bd4 (NRAD),g2_mie_bd5 (NRAD)
    REAL :: g3_mie_bd1 (NRAD),g3_mie_bd2 (NRAD),g3_mie_bd3 (NRAD),g3_mie_bd4 (NRAD),g3_mie_bd5 (NRAD)
    REAL :: g4_mie_bd1 (NRAD),g4_mie_bd2 (NRAD),g4_mie_bd3 (NRAD),g4_mie_bd4 (NRAD),g4_mie_bd5 (NRAD)
    REAL :: ext_mie_lap_bd1(7),ext_mie_lap_bd2(7),ext_mie_lap_bd3(7),ext_mie_lap_bd4(7),ext_mie_lap_bd5(7)
    REAL :: w_mie_lap_bd1  (7),w_mie_lap_bd2  (7),w_mie_lap_bd3  (7),w_mie_lap_bd4  (7),w_mie_lap_bd5  (7)
    REAL :: g_mie_lap_bd1  (7),g_mie_lap_bd2  (7),g_mie_lap_bd3  (7),g_mie_lap_bd4  (7),g_mie_lap_bd5  (7)

    REAL :: T(11)
    REAL :: sno_dns(8)
    REAL :: dTdz(31)

    REAL, DIMENSION(11) :: drdt0_1_1,drdt0_1_2,drdt0_1_3,drdt0_1_4,drdt0_1_5,drdt0_1_6,drdt0_1_7,drdt0_1_8,drdt0_1_9,drdt0_1_10, &
                           drdt0_1_11,drdt0_1_12,drdt0_1_13,drdt0_1_14,drdt0_1_15,drdt0_1_16,drdt0_1_17,drdt0_1_18,drdt0_1_19,drdt0_1_20, & 
                           drdt0_1_21,drdt0_1_22,drdt0_1_23,drdt0_1_24,drdt0_1_25,drdt0_1_26,drdt0_1_27,drdt0_1_28,drdt0_1_29,drdt0_1_30, drdt0_1_31
    REAL, DIMENSION(11) :: drdt0_2_1,drdt0_2_2,drdt0_2_3,drdt0_2_4,drdt0_2_5,drdt0_2_6,drdt0_2_7,drdt0_2_8,drdt0_2_9,drdt0_2_10, &
                           drdt0_2_11,drdt0_2_12,drdt0_2_13,drdt0_2_14,drdt0_2_15,drdt0_2_16,drdt0_2_17,drdt0_2_18,drdt0_2_19,drdt0_2_20, & 
                           drdt0_2_21,drdt0_2_22,drdt0_2_23,drdt0_2_24,drdt0_2_25,drdt0_2_26,drdt0_2_27,drdt0_2_28,drdt0_2_29,drdt0_2_30, drdt0_2_31 
    REAL, DIMENSION(11) :: drdt0_3_1,drdt0_3_2,drdt0_3_3,drdt0_3_4,drdt0_3_5,drdt0_3_6,drdt0_3_7,drdt0_3_8,drdt0_3_9,drdt0_3_10, &
                           drdt0_3_11,drdt0_3_12,drdt0_3_13,drdt0_3_14,drdt0_3_15,drdt0_3_16,drdt0_3_17,drdt0_3_18,drdt0_3_19,drdt0_3_20, & 
                           drdt0_3_21,drdt0_3_22,drdt0_3_23,drdt0_3_24,drdt0_3_25,drdt0_3_26,drdt0_3_27,drdt0_3_28,drdt0_3_29,drdt0_3_30, drdt0_3_31 
    REAL, DIMENSION(11) :: drdt0_4_1,drdt0_4_2,drdt0_4_3,drdt0_4_4,drdt0_4_5,drdt0_4_6,drdt0_4_7,drdt0_4_8,drdt0_4_9,drdt0_4_10, &
                           drdt0_4_11,drdt0_4_12,drdt0_4_13,drdt0_4_14,drdt0_4_15,drdt0_4_16,drdt0_4_17,drdt0_4_18,drdt0_4_19,drdt0_4_20, & 
                           drdt0_4_21,drdt0_4_22,drdt0_4_23,drdt0_4_24,drdt0_4_25,drdt0_4_26,drdt0_4_27,drdt0_4_28,drdt0_4_29,drdt0_4_30, drdt0_4_31 
    REAL, DIMENSION(11) :: drdt0_5_1,drdt0_5_2,drdt0_5_3,drdt0_5_4,drdt0_5_5,drdt0_5_6,drdt0_5_7,drdt0_5_8,drdt0_5_9,drdt0_5_10, &
                           drdt0_5_11,drdt0_5_12,drdt0_5_13,drdt0_5_14,drdt0_5_15,drdt0_5_16,drdt0_5_17,drdt0_5_18,drdt0_5_19,drdt0_5_20, & 
                           drdt0_5_21,drdt0_5_22,drdt0_5_23,drdt0_5_24,drdt0_5_25,drdt0_5_26,drdt0_5_27,drdt0_5_28,drdt0_5_29,drdt0_5_30, drdt0_5_31 
    REAL, DIMENSION(11) :: drdt0_6_1,drdt0_6_2,drdt0_6_3,drdt0_6_4,drdt0_6_5,drdt0_6_6,drdt0_6_7,drdt0_6_8,drdt0_6_9,drdt0_6_10, &
                           drdt0_6_11,drdt0_6_12,drdt0_6_13,drdt0_6_14,drdt0_6_15,drdt0_6_16,drdt0_6_17,drdt0_6_18,drdt0_6_19,drdt0_6_20, & 
                           drdt0_6_21,drdt0_6_22,drdt0_6_23,drdt0_6_24,drdt0_6_25,drdt0_6_26,drdt0_6_27,drdt0_6_28,drdt0_6_29,drdt0_6_30, drdt0_6_31 
    REAL, DIMENSION(11) :: drdt0_7_1,drdt0_7_2,drdt0_7_3,drdt0_7_4,drdt0_7_5,drdt0_7_6,drdt0_7_7,drdt0_7_8,drdt0_7_9,drdt0_7_10, &
                           drdt0_7_11,drdt0_7_12,drdt0_7_13,drdt0_7_14,drdt0_7_15,drdt0_7_16,drdt0_7_17,drdt0_7_18,drdt0_7_19,drdt0_7_20, & 
                           drdt0_7_21,drdt0_7_22,drdt0_7_23,drdt0_7_24,drdt0_7_25,drdt0_7_26,drdt0_7_27,drdt0_7_28,drdt0_7_29,drdt0_7_30, drdt0_7_31 
    REAL, DIMENSION(11) :: drdt0_8_1,drdt0_8_2,drdt0_8_3,drdt0_8_4,drdt0_8_5,drdt0_8_6,drdt0_8_7,drdt0_8_8,drdt0_8_9,drdt0_8_10, &
                           drdt0_8_11,drdt0_8_12,drdt0_8_13,drdt0_8_14,drdt0_8_15,drdt0_8_16,drdt0_8_17,drdt0_8_18,drdt0_8_19,drdt0_8_20, & 
                           drdt0_8_21,drdt0_8_22,drdt0_8_23,drdt0_8_24,drdt0_8_25,drdt0_8_26,drdt0_8_27,drdt0_8_28,drdt0_8_29,drdt0_8_30, drdt0_8_31 

    REAL, DIMENSION(11) :: kappa_1_1,kappa_1_2,kappa_1_3,kappa_1_4,kappa_1_5,kappa_1_6,kappa_1_7,kappa_1_8,kappa_1_9,kappa_1_10, &
                           kappa_1_11,kappa_1_12,kappa_1_13,kappa_1_14,kappa_1_15,kappa_1_16,kappa_1_17,kappa_1_18,kappa_1_19,kappa_1_20, & 
                           kappa_1_21,kappa_1_22,kappa_1_23,kappa_1_24,kappa_1_25,kappa_1_26,kappa_1_27,kappa_1_28,kappa_1_29,kappa_1_30, kappa_1_31
    REAL, DIMENSION(11) :: kappa_2_1,kappa_2_2,kappa_2_3,kappa_2_4,kappa_2_5,kappa_2_6,kappa_2_7,kappa_2_8,kappa_2_9,kappa_2_10, &
                           kappa_2_11,kappa_2_12,kappa_2_13,kappa_2_14,kappa_2_15,kappa_2_16,kappa_2_17,kappa_2_18,kappa_2_19,kappa_2_20, & 
                           kappa_2_21,kappa_2_22,kappa_2_23,kappa_2_24,kappa_2_25,kappa_2_26,kappa_2_27,kappa_2_28,kappa_2_29,kappa_2_30, kappa_2_31 
    REAL, DIMENSION(11) :: kappa_3_1,kappa_3_2,kappa_3_3,kappa_3_4,kappa_3_5,kappa_3_6,kappa_3_7,kappa_3_8,kappa_3_9,kappa_3_10, &
                           kappa_3_11,kappa_3_12,kappa_3_13,kappa_3_14,kappa_3_15,kappa_3_16,kappa_3_17,kappa_3_18,kappa_3_19,kappa_3_20, & 
                           kappa_3_21,kappa_3_22,kappa_3_23,kappa_3_24,kappa_3_25,kappa_3_26,kappa_3_27,kappa_3_28,kappa_3_29,kappa_3_30, kappa_3_31 
    REAL, DIMENSION(11) :: kappa_4_1,kappa_4_2,kappa_4_3,kappa_4_4,kappa_4_5,kappa_4_6,kappa_4_7,kappa_4_8,kappa_4_9,kappa_4_10, &
                           kappa_4_11,kappa_4_12,kappa_4_13,kappa_4_14,kappa_4_15,kappa_4_16,kappa_4_17,kappa_4_18,kappa_4_19,kappa_4_20, & 
                           kappa_4_21,kappa_4_22,kappa_4_23,kappa_4_24,kappa_4_25,kappa_4_26,kappa_4_27,kappa_4_28,kappa_4_29,kappa_4_30, kappa_4_31 
    REAL, DIMENSION(11) :: kappa_5_1,kappa_5_2,kappa_5_3,kappa_5_4,kappa_5_5,kappa_5_6,kappa_5_7,kappa_5_8,kappa_5_9,kappa_5_10, &
                           kappa_5_11,kappa_5_12,kappa_5_13,kappa_5_14,kappa_5_15,kappa_5_16,kappa_5_17,kappa_5_18,kappa_5_19,kappa_5_20, & 
                           kappa_5_21,kappa_5_22,kappa_5_23,kappa_5_24,kappa_5_25,kappa_5_26,kappa_5_27,kappa_5_28,kappa_5_29,kappa_5_30, kappa_5_31 
    REAL, DIMENSION(11) :: kappa_6_1,kappa_6_2,kappa_6_3,kappa_6_4,kappa_6_5,kappa_6_6,kappa_6_7,kappa_6_8,kappa_6_9,kappa_6_10, &
                           kappa_6_11,kappa_6_12,kappa_6_13,kappa_6_14,kappa_6_15,kappa_6_16,kappa_6_17,kappa_6_18,kappa_6_19,kappa_6_20, & 
                           kappa_6_21,kappa_6_22,kappa_6_23,kappa_6_24,kappa_6_25,kappa_6_26,kappa_6_27,kappa_6_28,kappa_6_29,kappa_6_30, kappa_6_31 
    REAL, DIMENSION(11) :: kappa_7_1,kappa_7_2,kappa_7_3,kappa_7_4,kappa_7_5,kappa_7_6,kappa_7_7,kappa_7_8,kappa_7_9,kappa_7_10, &
                           kappa_7_11,kappa_7_12,kappa_7_13,kappa_7_14,kappa_7_15,kappa_7_16,kappa_7_17,kappa_7_18,kappa_7_19,kappa_7_20, & 
                           kappa_7_21,kappa_7_22,kappa_7_23,kappa_7_24,kappa_7_25,kappa_7_26,kappa_7_27,kappa_7_28,kappa_7_29,kappa_7_30, kappa_7_31 
    REAL, DIMENSION(11) :: kappa_8_1,kappa_8_2,kappa_8_3,kappa_8_4,kappa_8_5,kappa_8_6,kappa_8_7,kappa_8_8,kappa_8_9,kappa_8_10, &
                           kappa_8_11,kappa_8_12,kappa_8_13,kappa_8_14,kappa_8_15,kappa_8_16,kappa_8_17,kappa_8_18,kappa_8_19,kappa_8_20, & 
                           kappa_8_21,kappa_8_22,kappa_8_23,kappa_8_24,kappa_8_25,kappa_8_26,kappa_8_27,kappa_8_28,kappa_8_29,kappa_8_30, kappa_8_31 

    REAL, DIMENSION(11) :: tau_1_1,tau_1_2,tau_1_3,tau_1_4,tau_1_5,tau_1_6,tau_1_7,tau_1_8,tau_1_9,tau_1_10, &
                           tau_1_11,tau_1_12,tau_1_13,tau_1_14,tau_1_15,tau_1_16,tau_1_17,tau_1_18,tau_1_19,tau_1_20, & 
                           tau_1_21,tau_1_22,tau_1_23,tau_1_24,tau_1_25,tau_1_26,tau_1_27,tau_1_28,tau_1_29,tau_1_30, tau_1_31
    REAL, DIMENSION(11) :: tau_2_1,tau_2_2,tau_2_3,tau_2_4,tau_2_5,tau_2_6,tau_2_7,tau_2_8,tau_2_9,tau_2_10, &
                           tau_2_11,tau_2_12,tau_2_13,tau_2_14,tau_2_15,tau_2_16,tau_2_17,tau_2_18,tau_2_19,tau_2_20, & 
                           tau_2_21,tau_2_22,tau_2_23,tau_2_24,tau_2_25,tau_2_26,tau_2_27,tau_2_28,tau_2_29,tau_2_30, tau_2_31 
    REAL, DIMENSION(11) :: tau_3_1,tau_3_2,tau_3_3,tau_3_4,tau_3_5,tau_3_6,tau_3_7,tau_3_8,tau_3_9,tau_3_10, &
                           tau_3_11,tau_3_12,tau_3_13,tau_3_14,tau_3_15,tau_3_16,tau_3_17,tau_3_18,tau_3_19,tau_3_20, & 
                           tau_3_21,tau_3_22,tau_3_23,tau_3_24,tau_3_25,tau_3_26,tau_3_27,tau_3_28,tau_3_29,tau_3_30, tau_3_31 
    REAL, DIMENSION(11) :: tau_4_1,tau_4_2,tau_4_3,tau_4_4,tau_4_5,tau_4_6,tau_4_7,tau_4_8,tau_4_9,tau_4_10, &
                           tau_4_11,tau_4_12,tau_4_13,tau_4_14,tau_4_15,tau_4_16,tau_4_17,tau_4_18,tau_4_19,tau_4_20, & 
                           tau_4_21,tau_4_22,tau_4_23,tau_4_24,tau_4_25,tau_4_26,tau_4_27,tau_4_28,tau_4_29,tau_4_30, tau_4_31 
    REAL, DIMENSION(11) :: tau_5_1,tau_5_2,tau_5_3,tau_5_4,tau_5_5,tau_5_6,tau_5_7,tau_5_8,tau_5_9,tau_5_10, &
                           tau_5_11,tau_5_12,tau_5_13,tau_5_14,tau_5_15,tau_5_16,tau_5_17,tau_5_18,tau_5_19,tau_5_20, & 
                           tau_5_21,tau_5_22,tau_5_23,tau_5_24,tau_5_25,tau_5_26,tau_5_27,tau_5_28,tau_5_29,tau_5_30, tau_5_31 
    REAL, DIMENSION(11) :: tau_6_1,tau_6_2,tau_6_3,tau_6_4,tau_6_5,tau_6_6,tau_6_7,tau_6_8,tau_6_9,tau_6_10, &
                           tau_6_11,tau_6_12,tau_6_13,tau_6_14,tau_6_15,tau_6_16,tau_6_17,tau_6_18,tau_6_19,tau_6_20, & 
                           tau_6_21,tau_6_22,tau_6_23,tau_6_24,tau_6_25,tau_6_26,tau_6_27,tau_6_28,tau_6_29,tau_6_30, tau_6_31 
    REAL, DIMENSION(11) :: tau_7_1,tau_7_2,tau_7_3,tau_7_4,tau_7_5,tau_7_6,tau_7_7,tau_7_8,tau_7_9,tau_7_10, &
                           tau_7_11,tau_7_12,tau_7_13,tau_7_14,tau_7_15,tau_7_16,tau_7_17,tau_7_18,tau_7_19,tau_7_20, & 
                           tau_7_21,tau_7_22,tau_7_23,tau_7_24,tau_7_25,tau_7_26,tau_7_27,tau_7_28,tau_7_29,tau_7_30, tau_7_31 
    REAL, DIMENSION(11) :: tau_8_1,tau_8_2,tau_8_3,tau_8_4,tau_8_5,tau_8_6,tau_8_7,tau_8_8,tau_8_9,tau_8_10, &
                           tau_8_11,tau_8_12,tau_8_13,tau_8_14,tau_8_15,tau_8_16,tau_8_17,tau_8_18,tau_8_19,tau_8_20, & 
                           tau_8_21,tau_8_22,tau_8_23,tau_8_24,tau_8_25,tau_8_26,tau_8_27,tau_8_28,tau_8_29,tau_8_30, tau_8_31 

    NAMELIST / noahmp_esm_snicar_parameters / ext_mie_bd1, ext_mie_bd2, ext_mie_bd3, ext_mie_bd4, ext_mie_bd5, &
                           w_mie_bd1 , w_mie_bd2 , w_mie_bd3 , w_mie_bd4 , w_mie_bd5 , &
                           g1_mie_bd1, g1_mie_bd2, g1_mie_bd3, g1_mie_bd4, g1_mie_bd5, &
                           g2_mie_bd1, g2_mie_bd2, g2_mie_bd3, g2_mie_bd4, g2_mie_bd5, &
                           g3_mie_bd1, g3_mie_bd2, g3_mie_bd3, g3_mie_bd4, g3_mie_bd5, &
                           g4_mie_bd1, g4_mie_bd2, g4_mie_bd3, g4_mie_bd4, g4_mie_bd5, &
                           ext_mie_lap_bd1, ext_mie_lap_bd2, ext_mie_lap_bd3, ext_mie_lap_bd4, ext_mie_lap_bd5, &
                           w_mie_lap_bd1, w_mie_lap_bd2, w_mie_lap_bd3, w_mie_lap_bd4, w_mie_lap_bd5, &
                           g_mie_lap_bd1, g_mie_lap_bd2, g_mie_lap_bd3, g_mie_lap_bd4, g_mie_lap_bd5

    NAMELIST / noahmp_snow_aging_parameters / T, sno_dns, dTdz, &
                           drdt0_1_1,drdt0_1_2,drdt0_1_3,drdt0_1_4,drdt0_1_5,drdt0_1_6,drdt0_1_7,drdt0_1_8,drdt0_1_9,drdt0_1_10, &
                           drdt0_1_11,drdt0_1_12,drdt0_1_13,drdt0_1_14,drdt0_1_15,drdt0_1_16,drdt0_1_17,drdt0_1_18,drdt0_1_19,drdt0_1_20, & 
                           drdt0_1_21,drdt0_1_22,drdt0_1_23,drdt0_1_24,drdt0_1_25,drdt0_1_26,drdt0_1_27,drdt0_1_28,drdt0_1_29,drdt0_1_30, drdt0_1_31, &
                           drdt0_2_1,drdt0_2_2,drdt0_2_3,drdt0_2_4,drdt0_2_5,drdt0_2_6,drdt0_2_7,drdt0_2_8,drdt0_2_9,drdt0_2_10, &
                           drdt0_2_11,drdt0_2_12,drdt0_2_13,drdt0_2_14,drdt0_2_15,drdt0_2_16,drdt0_2_17,drdt0_2_18,drdt0_2_19,drdt0_2_20, & 
                           drdt0_2_21,drdt0_2_22,drdt0_2_23,drdt0_2_24,drdt0_2_25,drdt0_2_26,drdt0_2_27,drdt0_2_28,drdt0_2_29,drdt0_2_30, drdt0_2_31, &
                           drdt0_3_1,drdt0_3_2,drdt0_3_3,drdt0_3_4,drdt0_3_5,drdt0_3_6,drdt0_3_7,drdt0_3_8,drdt0_3_9,drdt0_3_10, &
                           drdt0_3_11,drdt0_3_12,drdt0_3_13,drdt0_3_14,drdt0_3_15,drdt0_3_16,drdt0_3_17,drdt0_3_18,drdt0_3_19,drdt0_3_20, & 
                           drdt0_3_21,drdt0_3_22,drdt0_3_23,drdt0_3_24,drdt0_3_25,drdt0_3_26,drdt0_3_27,drdt0_3_28,drdt0_3_29,drdt0_3_30, drdt0_3_31, &
                           drdt0_4_1,drdt0_4_2,drdt0_4_3,drdt0_4_4,drdt0_4_5,drdt0_4_6,drdt0_4_7,drdt0_4_8,drdt0_4_9,drdt0_4_10, &
                           drdt0_4_11,drdt0_4_12,drdt0_4_13,drdt0_4_14,drdt0_4_15,drdt0_4_16,drdt0_4_17,drdt0_4_18,drdt0_4_19,drdt0_4_20, & 
                           drdt0_4_21,drdt0_4_22,drdt0_4_23,drdt0_4_24,drdt0_4_25,drdt0_4_26,drdt0_4_27,drdt0_4_28,drdt0_4_29,drdt0_4_30, drdt0_4_31, & 
                           drdt0_5_1,drdt0_5_2,drdt0_5_3,drdt0_5_4,drdt0_5_5,drdt0_5_6,drdt0_5_7,drdt0_5_8,drdt0_5_9,drdt0_5_10, &
                           drdt0_5_11,drdt0_5_12,drdt0_5_13,drdt0_5_14,drdt0_5_15,drdt0_5_16,drdt0_5_17,drdt0_5_18,drdt0_5_19,drdt0_5_20, & 
                           drdt0_5_21,drdt0_5_22,drdt0_5_23,drdt0_5_24,drdt0_5_25,drdt0_5_26,drdt0_5_27,drdt0_5_28,drdt0_5_29,drdt0_5_30, drdt0_5_31, &
                           drdt0_6_1,drdt0_6_2,drdt0_6_3,drdt0_6_4,drdt0_6_5,drdt0_6_6,drdt0_6_7,drdt0_6_8,drdt0_6_9,drdt0_6_10, &
                           drdt0_6_11,drdt0_6_12,drdt0_6_13,drdt0_6_14,drdt0_6_15,drdt0_6_16,drdt0_6_17,drdt0_6_18,drdt0_6_19,drdt0_6_20, & 
                           drdt0_6_21,drdt0_6_22,drdt0_6_23,drdt0_6_24,drdt0_6_25,drdt0_6_26,drdt0_6_27,drdt0_6_28,drdt0_6_29,drdt0_6_30, drdt0_6_31, & 
                           drdt0_7_1,drdt0_7_2,drdt0_7_3,drdt0_7_4,drdt0_7_5,drdt0_7_6,drdt0_7_7,drdt0_7_8,drdt0_7_9,drdt0_7_10, &
                           drdt0_7_11,drdt0_7_12,drdt0_7_13,drdt0_7_14,drdt0_7_15,drdt0_7_16,drdt0_7_17,drdt0_7_18,drdt0_7_19,drdt0_7_20, & 
                           drdt0_7_21,drdt0_7_22,drdt0_7_23,drdt0_7_24,drdt0_7_25,drdt0_7_26,drdt0_7_27,drdt0_7_28,drdt0_7_29,drdt0_7_30, drdt0_7_31, & 
                           drdt0_8_1,drdt0_8_2,drdt0_8_3,drdt0_8_4,drdt0_8_5,drdt0_8_6,drdt0_8_7,drdt0_8_8,drdt0_8_9,drdt0_8_10, &
                           drdt0_8_11,drdt0_8_12,drdt0_8_13,drdt0_8_14,drdt0_8_15,drdt0_8_16,drdt0_8_17,drdt0_8_18,drdt0_8_19,drdt0_8_20, & 
                           drdt0_8_21,drdt0_8_22,drdt0_8_23,drdt0_8_24,drdt0_8_25,drdt0_8_26,drdt0_8_27,drdt0_8_28,drdt0_8_29,drdt0_8_30, drdt0_8_31, & 

                           kappa_1_1,kappa_1_2,kappa_1_3,kappa_1_4,kappa_1_5,kappa_1_6,kappa_1_7,kappa_1_8,kappa_1_9,kappa_1_10, &
                           kappa_1_11,kappa_1_12,kappa_1_13,kappa_1_14,kappa_1_15,kappa_1_16,kappa_1_17,kappa_1_18,kappa_1_19,kappa_1_20, & 
                           kappa_1_21,kappa_1_22,kappa_1_23,kappa_1_24,kappa_1_25,kappa_1_26,kappa_1_27,kappa_1_28,kappa_1_29,kappa_1_30, kappa_1_31, &
                           kappa_2_1,kappa_2_2,kappa_2_3,kappa_2_4,kappa_2_5,kappa_2_6,kappa_2_7,kappa_2_8,kappa_2_9,kappa_2_10, &
                           kappa_2_11,kappa_2_12,kappa_2_13,kappa_2_14,kappa_2_15,kappa_2_16,kappa_2_17,kappa_2_18,kappa_2_19,kappa_2_20, & 
                           kappa_2_21,kappa_2_22,kappa_2_23,kappa_2_24,kappa_2_25,kappa_2_26,kappa_2_27,kappa_2_28,kappa_2_29,kappa_2_30, kappa_2_31, &
                           kappa_3_1,kappa_3_2,kappa_3_3,kappa_3_4,kappa_3_5,kappa_3_6,kappa_3_7,kappa_3_8,kappa_3_9,kappa_3_10, &
                           kappa_3_11,kappa_3_12,kappa_3_13,kappa_3_14,kappa_3_15,kappa_3_16,kappa_3_17,kappa_3_18,kappa_3_19,kappa_3_20, & 
                           kappa_3_21,kappa_3_22,kappa_3_23,kappa_3_24,kappa_3_25,kappa_3_26,kappa_3_27,kappa_3_28,kappa_3_29,kappa_3_30, kappa_3_31, &
                           kappa_4_1,kappa_4_2,kappa_4_3,kappa_4_4,kappa_4_5,kappa_4_6,kappa_4_7,kappa_4_8,kappa_4_9,kappa_4_10, &
                           kappa_4_11,kappa_4_12,kappa_4_13,kappa_4_14,kappa_4_15,kappa_4_16,kappa_4_17,kappa_4_18,kappa_4_19,kappa_4_20, & 
                           kappa_4_21,kappa_4_22,kappa_4_23,kappa_4_24,kappa_4_25,kappa_4_26,kappa_4_27,kappa_4_28,kappa_4_29,kappa_4_30, kappa_4_31, &
                           kappa_5_1,kappa_5_2,kappa_5_3,kappa_5_4,kappa_5_5,kappa_5_6,kappa_5_7,kappa_5_8,kappa_5_9,kappa_5_10, &
                           kappa_5_11,kappa_5_12,kappa_5_13,kappa_5_14,kappa_5_15,kappa_5_16,kappa_5_17,kappa_5_18,kappa_5_19,kappa_5_20, & 
                           kappa_5_21,kappa_5_22,kappa_5_23,kappa_5_24,kappa_5_25,kappa_5_26,kappa_5_27,kappa_5_28,kappa_5_29,kappa_5_30, kappa_5_31, &
                           kappa_6_1,kappa_6_2,kappa_6_3,kappa_6_4,kappa_6_5,kappa_6_6,kappa_6_7,kappa_6_8,kappa_6_9,kappa_6_10, &
                           kappa_6_11,kappa_6_12,kappa_6_13,kappa_6_14,kappa_6_15,kappa_6_16,kappa_6_17,kappa_6_18,kappa_6_19,kappa_6_20, & 
                           kappa_6_21,kappa_6_22,kappa_6_23,kappa_6_24,kappa_6_25,kappa_6_26,kappa_6_27,kappa_6_28,kappa_6_29,kappa_6_30, kappa_6_31, &
                           kappa_7_1,kappa_7_2,kappa_7_3,kappa_7_4,kappa_7_5,kappa_7_6,kappa_7_7,kappa_7_8,kappa_7_9,kappa_7_10, &
                           kappa_7_11,kappa_7_12,kappa_7_13,kappa_7_14,kappa_7_15,kappa_7_16,kappa_7_17,kappa_7_18,kappa_7_19,kappa_7_20, & 
                           kappa_7_21,kappa_7_22,kappa_7_23,kappa_7_24,kappa_7_25,kappa_7_26,kappa_7_27,kappa_7_28,kappa_7_29,kappa_7_30, kappa_7_31, &
                           kappa_8_1,kappa_8_2,kappa_8_3,kappa_8_4,kappa_8_5,kappa_8_6,kappa_8_7,kappa_8_8,kappa_8_9,kappa_8_10, &
                           kappa_8_11,kappa_8_12,kappa_8_13,kappa_8_14,kappa_8_15,kappa_8_16,kappa_8_17,kappa_8_18,kappa_8_19,kappa_8_20, & 
                           kappa_8_21,kappa_8_22,kappa_8_23,kappa_8_24,kappa_8_25,kappa_8_26,kappa_8_27,kappa_8_28,kappa_8_29,kappa_8_30, kappa_8_31, & 

                           tau_1_1,tau_1_2,tau_1_3,tau_1_4,tau_1_5,tau_1_6,tau_1_7,tau_1_8,tau_1_9,tau_1_10, &
                           tau_1_11,tau_1_12,tau_1_13,tau_1_14,tau_1_15,tau_1_16,tau_1_17,tau_1_18,tau_1_19,tau_1_20, & 
                           tau_1_21,tau_1_22,tau_1_23,tau_1_24,tau_1_25,tau_1_26,tau_1_27,tau_1_28,tau_1_29,tau_1_30, tau_1_31, &
                           tau_2_1,tau_2_2,tau_2_3,tau_2_4,tau_2_5,tau_2_6,tau_2_7,tau_2_8,tau_2_9,tau_2_10, &
                           tau_2_11,tau_2_12,tau_2_13,tau_2_14,tau_2_15,tau_2_16,tau_2_17,tau_2_18,tau_2_19,tau_2_20, & 
                           tau_2_21,tau_2_22,tau_2_23,tau_2_24,tau_2_25,tau_2_26,tau_2_27,tau_2_28,tau_2_29,tau_2_30, tau_2_31, &
                           tau_3_1,tau_3_2,tau_3_3,tau_3_4,tau_3_5,tau_3_6,tau_3_7,tau_3_8,tau_3_9,tau_3_10, &
                           tau_3_11,tau_3_12,tau_3_13,tau_3_14,tau_3_15,tau_3_16,tau_3_17,tau_3_18,tau_3_19,tau_3_20, & 
                           tau_3_21,tau_3_22,tau_3_23,tau_3_24,tau_3_25,tau_3_26,tau_3_27,tau_3_28,tau_3_29,tau_3_30, tau_3_31, &
                           tau_4_1,tau_4_2,tau_4_3,tau_4_4,tau_4_5,tau_4_6,tau_4_7,tau_4_8,tau_4_9,tau_4_10, &
                           tau_4_11,tau_4_12,tau_4_13,tau_4_14,tau_4_15,tau_4_16,tau_4_17,tau_4_18,tau_4_19,tau_4_20, & 
                           tau_4_21,tau_4_22,tau_4_23,tau_4_24,tau_4_25,tau_4_26,tau_4_27,tau_4_28,tau_4_29,tau_4_30, tau_4_31, &
                           tau_5_1,tau_5_2,tau_5_3,tau_5_4,tau_5_5,tau_5_6,tau_5_7,tau_5_8,tau_5_9,tau_5_10, &
                           tau_5_11,tau_5_12,tau_5_13,tau_5_14,tau_5_15,tau_5_16,tau_5_17,tau_5_18,tau_5_19,tau_5_20, & 
                           tau_5_21,tau_5_22,tau_5_23,tau_5_24,tau_5_25,tau_5_26,tau_5_27,tau_5_28,tau_5_29,tau_5_30, tau_5_31, &
                           tau_6_1,tau_6_2,tau_6_3,tau_6_4,tau_6_5,tau_6_6,tau_6_7,tau_6_8,tau_6_9,tau_6_10, &
                           tau_6_11,tau_6_12,tau_6_13,tau_6_14,tau_6_15,tau_6_16,tau_6_17,tau_6_18,tau_6_19,tau_6_20, & 
                           tau_6_21,tau_6_22,tau_6_23,tau_6_24,tau_6_25,tau_6_26,tau_6_27,tau_6_28,tau_6_29,tau_6_30, tau_6_31, &
                           tau_7_1,tau_7_2,tau_7_3,tau_7_4,tau_7_5,tau_7_6,tau_7_7,tau_7_8,tau_7_9,tau_7_10, &
                           tau_7_11,tau_7_12,tau_7_13,tau_7_14,tau_7_15,tau_7_16,tau_7_17,tau_7_18,tau_7_19,tau_7_20, & 
                           tau_7_21,tau_7_22,tau_7_23,tau_7_24,tau_7_25,tau_7_26,tau_7_27,tau_7_28,tau_7_29,tau_7_30, tau_7_31, &
                           tau_8_1,tau_8_2,tau_8_3,tau_8_4,tau_8_5,tau_8_6,tau_8_7,tau_8_8,tau_8_9,tau_8_10, &
                           tau_8_11,tau_8_12,tau_8_13,tau_8_14,tau_8_15,tau_8_16,tau_8_17,tau_8_18,tau_8_19,tau_8_20, & 
                           tau_8_21,tau_8_22,tau_8_23,tau_8_24,tau_8_25,tau_8_26,tau_8_27,tau_8_28,tau_8_29,tau_8_30, tau_8_31 

    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
    ext_mie_bd_TABLE  = -1.E36
    w_mie_bd_TABLE    = -1.E36
    g1_mie_bd_TABLE   = -1.E36
    g2_mie_bd_TABLE   = -1.E36
    g3_mie_bd_TABLE   = -1.E36
    g4_mie_bd_TABLE   = -1.E36

    ext_mie_lap_bd_TABLE  = -1.E36
    w_mie_lap_bd_TABLE    = -1.E36
    g_mie_lap_bd_TABLE    = -1.E36

    T_TABLE           = -1.E36
    sno_dns_TABLE     = -1.E36
    dTdz_TABLE        = -1.E36

    drdt0_TABLE       = -1.E36
    kappa_TABLE       = -1.E36
    tau_TABLE         = -1.E36

    inquire( file='ESM_SNICAR.TBL', exist=file_named ) 
    if ( file_named ) then
      open(15, file="ESM_SNICAR.TBL", status='old', form='formatted', action='read', iostat=ierr)
    else
      open(15, status='old', form='formatted', action='read', iostat=ierr)
    end if

    if (ierr /= 0) then
       write(*,'("WARNING: Cannot find file ESM_SNICAR.TBL")')
       call wrf_error_fatal("STOP in Noah-MP read_esm_snicar_parameters")
    endif

    read(15,noahmp_esm_snicar_parameters)

    read(15,noahmp_snow_aging_parameters)

    close(15)

    ext_mie_bd_TABLE(:,1)=ext_mie_bd1
    ext_mie_bd_TABLE(:,2)=ext_mie_bd2
    ext_mie_bd_TABLE(:,3)=ext_mie_bd3
    ext_mie_bd_TABLE(:,4)=ext_mie_bd4
    ext_mie_bd_TABLE(:,5)=ext_mie_bd5
    w_mie_bd_TABLE  (:,1)=w_mie_bd1
    w_mie_bd_TABLE  (:,2)=w_mie_bd2
    w_mie_bd_TABLE  (:,3)=w_mie_bd3
    w_mie_bd_TABLE  (:,4)=w_mie_bd4
    w_mie_bd_TABLE  (:,5)=w_mie_bd5
    g1_mie_bd_TABLE (:,1)=g1_mie_bd1
    g1_mie_bd_TABLE (:,2)=g1_mie_bd2
    g1_mie_bd_TABLE (:,3)=g1_mie_bd3
    g1_mie_bd_TABLE (:,4)=g1_mie_bd4
    g1_mie_bd_TABLE (:,5)=g1_mie_bd5
    g2_mie_bd_TABLE (:,1)=g2_mie_bd1
    g2_mie_bd_TABLE (:,2)=g2_mie_bd2
    g2_mie_bd_TABLE (:,3)=g2_mie_bd3
    g2_mie_bd_TABLE (:,4)=g2_mie_bd4
    g2_mie_bd_TABLE (:,5)=g2_mie_bd5
    g3_mie_bd_TABLE (:,1)=g3_mie_bd1
    g3_mie_bd_TABLE (:,2)=g3_mie_bd2
    g3_mie_bd_TABLE (:,3)=g3_mie_bd3
    g3_mie_bd_TABLE (:,4)=g3_mie_bd4
    g3_mie_bd_TABLE (:,5)=g3_mie_bd5
    g4_mie_bd_TABLE (:,1)=g4_mie_bd1
    g4_mie_bd_TABLE (:,2)=g4_mie_bd2
    g4_mie_bd_TABLE (:,3)=g4_mie_bd3
    g4_mie_bd_TABLE (:,4)=g4_mie_bd4
    g4_mie_bd_TABLE (:,5)=g4_mie_bd5
    ext_mie_lap_bd_TABLE(:,1)=ext_mie_lap_bd1
    ext_mie_lap_bd_TABLE(:,2)=ext_mie_lap_bd2
    ext_mie_lap_bd_TABLE(:,3)=ext_mie_lap_bd3
    ext_mie_lap_bd_TABLE(:,4)=ext_mie_lap_bd4
    ext_mie_lap_bd_TABLE(:,5)=ext_mie_lap_bd5
    w_mie_lap_bd_TABLE(:,1)=w_mie_lap_bd1
    w_mie_lap_bd_TABLE(:,2)=w_mie_lap_bd2
    w_mie_lap_bd_TABLE(:,3)=w_mie_lap_bd3
    w_mie_lap_bd_TABLE(:,4)=w_mie_lap_bd4
    w_mie_lap_bd_TABLE(:,5)=w_mie_lap_bd5
    g_mie_lap_bd_TABLE(:,1)=g_mie_lap_bd1
    g_mie_lap_bd_TABLE(:,2)=g_mie_lap_bd2
    g_mie_lap_bd_TABLE(:,3)=g_mie_lap_bd3
    g_mie_lap_bd_TABLE(:,4)=g_mie_lap_bd4
    g_mie_lap_bd_TABLE(:,5)=g_mie_lap_bd5

    T_TABLE       = T
    sno_dns_TABLE = sno_dns
    dTdz_TABLE    = dTdz

    drdt0_TABLE(1, 1,:)=drdt0_1_1
    drdt0_TABLE(1, 2,:)=drdt0_1_2
    drdt0_TABLE(1, 3,:)=drdt0_1_3
    drdt0_TABLE(1, 4,:)=drdt0_1_4
    drdt0_TABLE(1, 5,:)=drdt0_1_5
    drdt0_TABLE(1, 6,:)=drdt0_1_6
    drdt0_TABLE(1, 7,:)=drdt0_1_7
    drdt0_TABLE(1, 8,:)=drdt0_1_8
    drdt0_TABLE(1, 9,:)=drdt0_1_9
    drdt0_TABLE(1,10,:)=drdt0_1_10
    drdt0_TABLE(1,11,:)=drdt0_1_11
    drdt0_TABLE(1,12,:)=drdt0_1_12
    drdt0_TABLE(1,13,:)=drdt0_1_13
    drdt0_TABLE(1,14,:)=drdt0_1_14
    drdt0_TABLE(1,15,:)=drdt0_1_15
    drdt0_TABLE(1,16,:)=drdt0_1_16
    drdt0_TABLE(1,17,:)=drdt0_1_17
    drdt0_TABLE(1,18,:)=drdt0_1_18
    drdt0_TABLE(1,19,:)=drdt0_1_19
    drdt0_TABLE(1,20,:)=drdt0_1_20
    drdt0_TABLE(1,21,:)=drdt0_1_21
    drdt0_TABLE(1,22,:)=drdt0_1_22
    drdt0_TABLE(1,23,:)=drdt0_1_23
    drdt0_TABLE(1,24,:)=drdt0_1_24
    drdt0_TABLE(1,25,:)=drdt0_1_25
    drdt0_TABLE(1,26,:)=drdt0_1_26
    drdt0_TABLE(1,27,:)=drdt0_1_27
    drdt0_TABLE(1,28,:)=drdt0_1_28
    drdt0_TABLE(1,29,:)=drdt0_1_29
    drdt0_TABLE(1,30,:)=drdt0_1_30
    drdt0_TABLE(1,31,:)=drdt0_1_31

    drdt0_TABLE(2, 1,:)=drdt0_2_1
    drdt0_TABLE(2, 2,:)=drdt0_2_2
    drdt0_TABLE(2, 3,:)=drdt0_2_3
    drdt0_TABLE(2, 4,:)=drdt0_2_4
    drdt0_TABLE(2, 5,:)=drdt0_2_5
    drdt0_TABLE(2, 6,:)=drdt0_2_6
    drdt0_TABLE(2, 7,:)=drdt0_2_7
    drdt0_TABLE(2, 8,:)=drdt0_2_8
    drdt0_TABLE(2, 9,:)=drdt0_2_9
    drdt0_TABLE(2,10,:)=drdt0_2_10
    drdt0_TABLE(2,11,:)=drdt0_2_11
    drdt0_TABLE(2,12,:)=drdt0_2_12
    drdt0_TABLE(2,13,:)=drdt0_2_13
    drdt0_TABLE(2,14,:)=drdt0_2_14
    drdt0_TABLE(2,15,:)=drdt0_2_15
    drdt0_TABLE(2,16,:)=drdt0_2_16
    drdt0_TABLE(2,17,:)=drdt0_2_17
    drdt0_TABLE(2,18,:)=drdt0_2_18
    drdt0_TABLE(2,19,:)=drdt0_2_19
    drdt0_TABLE(2,20,:)=drdt0_2_20
    drdt0_TABLE(2,21,:)=drdt0_2_21
    drdt0_TABLE(2,22,:)=drdt0_2_22
    drdt0_TABLE(2,23,:)=drdt0_2_23
    drdt0_TABLE(2,24,:)=drdt0_2_24
    drdt0_TABLE(2,25,:)=drdt0_2_25
    drdt0_TABLE(2,26,:)=drdt0_2_26
    drdt0_TABLE(2,27,:)=drdt0_2_27
    drdt0_TABLE(2,28,:)=drdt0_2_28
    drdt0_TABLE(2,29,:)=drdt0_2_29
    drdt0_TABLE(2,30,:)=drdt0_2_30
    drdt0_TABLE(2,31,:)=drdt0_2_31

    drdt0_TABLE(3, 1,:)=drdt0_3_1
    drdt0_TABLE(3, 2,:)=drdt0_3_2
    drdt0_TABLE(3, 3,:)=drdt0_3_3
    drdt0_TABLE(3, 4,:)=drdt0_3_4
    drdt0_TABLE(3, 5,:)=drdt0_3_5
    drdt0_TABLE(3, 6,:)=drdt0_3_6
    drdt0_TABLE(3, 7,:)=drdt0_3_7
    drdt0_TABLE(3, 8,:)=drdt0_3_8
    drdt0_TABLE(3, 9,:)=drdt0_3_9
    drdt0_TABLE(3,10,:)=drdt0_3_10
    drdt0_TABLE(3,11,:)=drdt0_3_11
    drdt0_TABLE(3,12,:)=drdt0_3_12
    drdt0_TABLE(3,13,:)=drdt0_3_13
    drdt0_TABLE(3,14,:)=drdt0_3_14
    drdt0_TABLE(3,15,:)=drdt0_3_15
    drdt0_TABLE(3,16,:)=drdt0_3_16
    drdt0_TABLE(3,17,:)=drdt0_3_17
    drdt0_TABLE(3,18,:)=drdt0_3_18
    drdt0_TABLE(3,19,:)=drdt0_3_19
    drdt0_TABLE(3,20,:)=drdt0_3_20
    drdt0_TABLE(3,21,:)=drdt0_3_21
    drdt0_TABLE(3,22,:)=drdt0_3_22
    drdt0_TABLE(3,23,:)=drdt0_3_23
    drdt0_TABLE(3,24,:)=drdt0_3_24
    drdt0_TABLE(3,25,:)=drdt0_3_25
    drdt0_TABLE(3,26,:)=drdt0_3_26
    drdt0_TABLE(3,27,:)=drdt0_3_27
    drdt0_TABLE(3,28,:)=drdt0_3_28
    drdt0_TABLE(3,29,:)=drdt0_3_29
    drdt0_TABLE(3,30,:)=drdt0_3_30
    drdt0_TABLE(3,31,:)=drdt0_3_31

    drdt0_TABLE(4, 1,:)=drdt0_4_1
    drdt0_TABLE(4, 2,:)=drdt0_4_2
    drdt0_TABLE(4, 3,:)=drdt0_4_3
    drdt0_TABLE(4, 4,:)=drdt0_4_4
    drdt0_TABLE(4, 5,:)=drdt0_4_5
    drdt0_TABLE(4, 6,:)=drdt0_4_6
    drdt0_TABLE(4, 7,:)=drdt0_4_7
    drdt0_TABLE(4, 8,:)=drdt0_4_8
    drdt0_TABLE(4, 9,:)=drdt0_4_9
    drdt0_TABLE(4,10,:)=drdt0_4_10
    drdt0_TABLE(4,11,:)=drdt0_4_11
    drdt0_TABLE(4,12,:)=drdt0_4_12
    drdt0_TABLE(4,13,:)=drdt0_4_13
    drdt0_TABLE(4,14,:)=drdt0_4_14
    drdt0_TABLE(4,15,:)=drdt0_4_15
    drdt0_TABLE(4,16,:)=drdt0_4_16
    drdt0_TABLE(4,17,:)=drdt0_4_17
    drdt0_TABLE(4,18,:)=drdt0_4_18
    drdt0_TABLE(4,19,:)=drdt0_4_19
    drdt0_TABLE(4,20,:)=drdt0_4_20
    drdt0_TABLE(4,21,:)=drdt0_4_21
    drdt0_TABLE(4,22,:)=drdt0_4_22
    drdt0_TABLE(4,23,:)=drdt0_4_23
    drdt0_TABLE(4,24,:)=drdt0_4_24
    drdt0_TABLE(4,25,:)=drdt0_4_25
    drdt0_TABLE(4,26,:)=drdt0_4_26
    drdt0_TABLE(4,27,:)=drdt0_4_27
    drdt0_TABLE(4,28,:)=drdt0_4_28
    drdt0_TABLE(4,29,:)=drdt0_4_29
    drdt0_TABLE(4,30,:)=drdt0_4_30
    drdt0_TABLE(4,31,:)=drdt0_4_31

    drdt0_TABLE(5, 1,:)=drdt0_5_1
    drdt0_TABLE(5, 2,:)=drdt0_5_2
    drdt0_TABLE(5, 3,:)=drdt0_5_3
    drdt0_TABLE(5, 4,:)=drdt0_5_4
    drdt0_TABLE(5, 5,:)=drdt0_5_5
    drdt0_TABLE(5, 6,:)=drdt0_5_6
    drdt0_TABLE(5, 7,:)=drdt0_5_7
    drdt0_TABLE(5, 8,:)=drdt0_5_8
    drdt0_TABLE(5, 9,:)=drdt0_5_9
    drdt0_TABLE(5,10,:)=drdt0_5_10
    drdt0_TABLE(5,11,:)=drdt0_5_11
    drdt0_TABLE(5,12,:)=drdt0_5_12
    drdt0_TABLE(5,13,:)=drdt0_5_13
    drdt0_TABLE(5,14,:)=drdt0_5_14
    drdt0_TABLE(5,15,:)=drdt0_5_15
    drdt0_TABLE(5,16,:)=drdt0_5_16
    drdt0_TABLE(5,17,:)=drdt0_5_17
    drdt0_TABLE(5,18,:)=drdt0_5_18
    drdt0_TABLE(5,19,:)=drdt0_5_19
    drdt0_TABLE(5,20,:)=drdt0_5_20
    drdt0_TABLE(5,21,:)=drdt0_5_21
    drdt0_TABLE(5,22,:)=drdt0_5_22
    drdt0_TABLE(5,23,:)=drdt0_5_23
    drdt0_TABLE(5,24,:)=drdt0_5_24
    drdt0_TABLE(5,25,:)=drdt0_5_25
    drdt0_TABLE(5,26,:)=drdt0_5_26
    drdt0_TABLE(5,27,:)=drdt0_5_27
    drdt0_TABLE(5,28,:)=drdt0_5_28
    drdt0_TABLE(5,29,:)=drdt0_5_29
    drdt0_TABLE(5,30,:)=drdt0_5_30
    drdt0_TABLE(5,31,:)=drdt0_5_31

    drdt0_TABLE(6, 1,:)=drdt0_6_1
    drdt0_TABLE(6, 2,:)=drdt0_6_2
    drdt0_TABLE(6, 3,:)=drdt0_6_3
    drdt0_TABLE(6, 4,:)=drdt0_6_4
    drdt0_TABLE(6, 5,:)=drdt0_6_5
    drdt0_TABLE(6, 6,:)=drdt0_6_6
    drdt0_TABLE(6, 7,:)=drdt0_6_7
    drdt0_TABLE(6, 8,:)=drdt0_6_8
    drdt0_TABLE(6, 9,:)=drdt0_6_9
    drdt0_TABLE(6,10,:)=drdt0_6_10
    drdt0_TABLE(6,11,:)=drdt0_6_11
    drdt0_TABLE(6,12,:)=drdt0_6_12
    drdt0_TABLE(6,13,:)=drdt0_6_13
    drdt0_TABLE(6,14,:)=drdt0_6_14
    drdt0_TABLE(6,15,:)=drdt0_6_15
    drdt0_TABLE(6,16,:)=drdt0_6_16
    drdt0_TABLE(6,17,:)=drdt0_6_17
    drdt0_TABLE(6,18,:)=drdt0_6_18
    drdt0_TABLE(6,19,:)=drdt0_6_19
    drdt0_TABLE(6,20,:)=drdt0_6_20
    drdt0_TABLE(6,21,:)=drdt0_6_21
    drdt0_TABLE(6,22,:)=drdt0_6_22
    drdt0_TABLE(6,23,:)=drdt0_6_23
    drdt0_TABLE(6,24,:)=drdt0_6_24
    drdt0_TABLE(6,25,:)=drdt0_6_25
    drdt0_TABLE(6,26,:)=drdt0_6_26
    drdt0_TABLE(6,27,:)=drdt0_6_27
    drdt0_TABLE(6,28,:)=drdt0_6_28
    drdt0_TABLE(6,29,:)=drdt0_6_29
    drdt0_TABLE(6,30,:)=drdt0_6_30
    drdt0_TABLE(6,31,:)=drdt0_6_31

    drdt0_TABLE(7, 1,:)=drdt0_7_1
    drdt0_TABLE(7, 2,:)=drdt0_7_2
    drdt0_TABLE(7, 3,:)=drdt0_7_3
    drdt0_TABLE(7, 4,:)=drdt0_7_4
    drdt0_TABLE(7, 5,:)=drdt0_7_5
    drdt0_TABLE(7, 6,:)=drdt0_7_6
    drdt0_TABLE(7, 7,:)=drdt0_7_7
    drdt0_TABLE(7, 8,:)=drdt0_7_8
    drdt0_TABLE(7, 9,:)=drdt0_7_9
    drdt0_TABLE(7,10,:)=drdt0_7_10
    drdt0_TABLE(7,11,:)=drdt0_7_11
    drdt0_TABLE(7,12,:)=drdt0_7_12
    drdt0_TABLE(7,13,:)=drdt0_7_13
    drdt0_TABLE(7,14,:)=drdt0_7_14
    drdt0_TABLE(7,15,:)=drdt0_7_15
    drdt0_TABLE(7,16,:)=drdt0_7_16
    drdt0_TABLE(7,17,:)=drdt0_7_17
    drdt0_TABLE(7,18,:)=drdt0_7_18
    drdt0_TABLE(7,19,:)=drdt0_7_19
    drdt0_TABLE(7,20,:)=drdt0_7_20
    drdt0_TABLE(7,21,:)=drdt0_7_21
    drdt0_TABLE(7,22,:)=drdt0_7_22
    drdt0_TABLE(7,23,:)=drdt0_7_23
    drdt0_TABLE(7,24,:)=drdt0_7_24
    drdt0_TABLE(7,25,:)=drdt0_7_25
    drdt0_TABLE(7,26,:)=drdt0_7_26
    drdt0_TABLE(7,27,:)=drdt0_7_27
    drdt0_TABLE(7,28,:)=drdt0_7_28
    drdt0_TABLE(7,29,:)=drdt0_7_29
    drdt0_TABLE(7,30,:)=drdt0_7_30
    drdt0_TABLE(7,31,:)=drdt0_7_31

    drdt0_TABLE(8, 1,:)=drdt0_8_1
    drdt0_TABLE(8, 2,:)=drdt0_8_2
    drdt0_TABLE(8, 3,:)=drdt0_8_3
    drdt0_TABLE(8, 4,:)=drdt0_8_4
    drdt0_TABLE(8, 5,:)=drdt0_8_5
    drdt0_TABLE(8, 6,:)=drdt0_8_6
    drdt0_TABLE(8, 7,:)=drdt0_8_7
    drdt0_TABLE(8, 8,:)=drdt0_8_8
    drdt0_TABLE(8, 9,:)=drdt0_8_9
    drdt0_TABLE(8,10,:)=drdt0_8_10
    drdt0_TABLE(8,11,:)=drdt0_8_11
    drdt0_TABLE(8,12,:)=drdt0_8_12
    drdt0_TABLE(8,13,:)=drdt0_8_13
    drdt0_TABLE(8,14,:)=drdt0_8_14
    drdt0_TABLE(8,15,:)=drdt0_8_15
    drdt0_TABLE(8,16,:)=drdt0_8_16
    drdt0_TABLE(8,17,:)=drdt0_8_17
    drdt0_TABLE(8,18,:)=drdt0_8_18
    drdt0_TABLE(8,19,:)=drdt0_8_19
    drdt0_TABLE(8,20,:)=drdt0_8_20
    drdt0_TABLE(8,21,:)=drdt0_8_21
    drdt0_TABLE(8,22,:)=drdt0_8_22
    drdt0_TABLE(8,23,:)=drdt0_8_23
    drdt0_TABLE(8,24,:)=drdt0_8_24
    drdt0_TABLE(8,25,:)=drdt0_8_25
    drdt0_TABLE(8,26,:)=drdt0_8_26
    drdt0_TABLE(8,27,:)=drdt0_8_27
    drdt0_TABLE(8,28,:)=drdt0_8_28
    drdt0_TABLE(8,29,:)=drdt0_8_29
    drdt0_TABLE(8,30,:)=drdt0_8_30
    drdt0_TABLE(8,31,:)=drdt0_8_31

    kappa_TABLE(1, 1,:)=kappa_1_1
    kappa_TABLE(1, 2,:)=kappa_1_2
    kappa_TABLE(1, 3,:)=kappa_1_3
    kappa_TABLE(1, 4,:)=kappa_1_4
    kappa_TABLE(1, 5,:)=kappa_1_5
    kappa_TABLE(1, 6,:)=kappa_1_6
    kappa_TABLE(1, 7,:)=kappa_1_7
    kappa_TABLE(1, 8,:)=kappa_1_8
    kappa_TABLE(1, 9,:)=kappa_1_9
    kappa_TABLE(1,10,:)=kappa_1_10
    kappa_TABLE(1,11,:)=kappa_1_11
    kappa_TABLE(1,12,:)=kappa_1_12
    kappa_TABLE(1,13,:)=kappa_1_13
    kappa_TABLE(1,14,:)=kappa_1_14
    kappa_TABLE(1,15,:)=kappa_1_15
    kappa_TABLE(1,16,:)=kappa_1_16
    kappa_TABLE(1,17,:)=kappa_1_17
    kappa_TABLE(1,18,:)=kappa_1_18
    kappa_TABLE(1,19,:)=kappa_1_19
    kappa_TABLE(1,20,:)=kappa_1_20
    kappa_TABLE(1,21,:)=kappa_1_21
    kappa_TABLE(1,22,:)=kappa_1_22
    kappa_TABLE(1,23,:)=kappa_1_23
    kappa_TABLE(1,24,:)=kappa_1_24
    kappa_TABLE(1,25,:)=kappa_1_25
    kappa_TABLE(1,26,:)=kappa_1_26
    kappa_TABLE(1,27,:)=kappa_1_27
    kappa_TABLE(1,28,:)=kappa_1_28
    kappa_TABLE(1,29,:)=kappa_1_29
    kappa_TABLE(1,30,:)=kappa_1_30
    kappa_TABLE(1,31,:)=kappa_1_31

    kappa_TABLE(2, 1,:)=kappa_2_1
    kappa_TABLE(2, 2,:)=kappa_2_2
    kappa_TABLE(2, 3,:)=kappa_2_3
    kappa_TABLE(2, 4,:)=kappa_2_4
    kappa_TABLE(2, 5,:)=kappa_2_5
    kappa_TABLE(2, 6,:)=kappa_2_6
    kappa_TABLE(2, 7,:)=kappa_2_7
    kappa_TABLE(2, 8,:)=kappa_2_8
    kappa_TABLE(2, 9,:)=kappa_2_9
    kappa_TABLE(2,10,:)=kappa_2_10
    kappa_TABLE(2,11,:)=kappa_2_11
    kappa_TABLE(2,12,:)=kappa_2_12
    kappa_TABLE(2,13,:)=kappa_2_13
    kappa_TABLE(2,14,:)=kappa_2_14
    kappa_TABLE(2,15,:)=kappa_2_15
    kappa_TABLE(2,16,:)=kappa_2_16
    kappa_TABLE(2,17,:)=kappa_2_17
    kappa_TABLE(2,18,:)=kappa_2_18
    kappa_TABLE(2,19,:)=kappa_2_19
    kappa_TABLE(2,20,:)=kappa_2_20
    kappa_TABLE(2,21,:)=kappa_2_21
    kappa_TABLE(2,22,:)=kappa_2_22
    kappa_TABLE(2,23,:)=kappa_2_23
    kappa_TABLE(2,24,:)=kappa_2_24
    kappa_TABLE(2,25,:)=kappa_2_25
    kappa_TABLE(2,26,:)=kappa_2_26
    kappa_TABLE(2,27,:)=kappa_2_27
    kappa_TABLE(2,28,:)=kappa_2_28
    kappa_TABLE(2,29,:)=kappa_2_29
    kappa_TABLE(2,30,:)=kappa_2_30
    kappa_TABLE(2,31,:)=kappa_2_31

    kappa_TABLE(3, 1,:)=kappa_3_1
    kappa_TABLE(3, 2,:)=kappa_3_2
    kappa_TABLE(3, 3,:)=kappa_3_3
    kappa_TABLE(3, 4,:)=kappa_3_4
    kappa_TABLE(3, 5,:)=kappa_3_5
    kappa_TABLE(3, 6,:)=kappa_3_6
    kappa_TABLE(3, 7,:)=kappa_3_7
    kappa_TABLE(3, 8,:)=kappa_3_8
    kappa_TABLE(3, 9,:)=kappa_3_9
    kappa_TABLE(3,10,:)=kappa_3_10
    kappa_TABLE(3,11,:)=kappa_3_11
    kappa_TABLE(3,12,:)=kappa_3_12
    kappa_TABLE(3,13,:)=kappa_3_13
    kappa_TABLE(3,14,:)=kappa_3_14
    kappa_TABLE(3,15,:)=kappa_3_15
    kappa_TABLE(3,16,:)=kappa_3_16
    kappa_TABLE(3,17,:)=kappa_3_17
    kappa_TABLE(3,18,:)=kappa_3_18
    kappa_TABLE(3,19,:)=kappa_3_19
    kappa_TABLE(3,20,:)=kappa_3_20
    kappa_TABLE(3,21,:)=kappa_3_21
    kappa_TABLE(3,22,:)=kappa_3_22
    kappa_TABLE(3,23,:)=kappa_3_23
    kappa_TABLE(3,24,:)=kappa_3_24
    kappa_TABLE(3,25,:)=kappa_3_25
    kappa_TABLE(3,26,:)=kappa_3_26
    kappa_TABLE(3,27,:)=kappa_3_27
    kappa_TABLE(3,28,:)=kappa_3_28
    kappa_TABLE(3,29,:)=kappa_3_29
    kappa_TABLE(3,30,:)=kappa_3_30
    kappa_TABLE(3,31,:)=kappa_3_31

    kappa_TABLE(4, 1,:)=kappa_4_1
    kappa_TABLE(4, 2,:)=kappa_4_2
    kappa_TABLE(4, 3,:)=kappa_4_3
    kappa_TABLE(4, 4,:)=kappa_4_4
    kappa_TABLE(4, 5,:)=kappa_4_5
    kappa_TABLE(4, 6,:)=kappa_4_6
    kappa_TABLE(4, 7,:)=kappa_4_7
    kappa_TABLE(4, 8,:)=kappa_4_8
    kappa_TABLE(4, 9,:)=kappa_4_9
    kappa_TABLE(4,10,:)=kappa_4_10
    kappa_TABLE(4,11,:)=kappa_4_11
    kappa_TABLE(4,12,:)=kappa_4_12
    kappa_TABLE(4,13,:)=kappa_4_13
    kappa_TABLE(4,14,:)=kappa_4_14
    kappa_TABLE(4,15,:)=kappa_4_15
    kappa_TABLE(4,16,:)=kappa_4_16
    kappa_TABLE(4,17,:)=kappa_4_17
    kappa_TABLE(4,18,:)=kappa_4_18
    kappa_TABLE(4,19,:)=kappa_4_19
    kappa_TABLE(4,20,:)=kappa_4_20
    kappa_TABLE(4,21,:)=kappa_4_21
    kappa_TABLE(4,22,:)=kappa_4_22
    kappa_TABLE(4,23,:)=kappa_4_23
    kappa_TABLE(4,24,:)=kappa_4_24
    kappa_TABLE(4,25,:)=kappa_4_25
    kappa_TABLE(4,26,:)=kappa_4_26
    kappa_TABLE(4,27,:)=kappa_4_27
    kappa_TABLE(4,28,:)=kappa_4_28
    kappa_TABLE(4,29,:)=kappa_4_29
    kappa_TABLE(4,30,:)=kappa_4_30
    kappa_TABLE(4,31,:)=kappa_4_31

    kappa_TABLE(5, 1,:)=kappa_5_1
    kappa_TABLE(5, 2,:)=kappa_5_2
    kappa_TABLE(5, 3,:)=kappa_5_3
    kappa_TABLE(5, 4,:)=kappa_5_4
    kappa_TABLE(5, 5,:)=kappa_5_5
    kappa_TABLE(5, 6,:)=kappa_5_6
    kappa_TABLE(5, 7,:)=kappa_5_7
    kappa_TABLE(5, 8,:)=kappa_5_8
    kappa_TABLE(5, 9,:)=kappa_5_9
    kappa_TABLE(5,10,:)=kappa_5_10
    kappa_TABLE(5,11,:)=kappa_5_11
    kappa_TABLE(5,12,:)=kappa_5_12
    kappa_TABLE(5,13,:)=kappa_5_13
    kappa_TABLE(5,14,:)=kappa_5_14
    kappa_TABLE(5,15,:)=kappa_5_15
    kappa_TABLE(5,16,:)=kappa_5_16
    kappa_TABLE(5,17,:)=kappa_5_17
    kappa_TABLE(5,18,:)=kappa_5_18
    kappa_TABLE(5,19,:)=kappa_5_19
    kappa_TABLE(5,20,:)=kappa_5_20
    kappa_TABLE(5,21,:)=kappa_5_21
    kappa_TABLE(5,22,:)=kappa_5_22
    kappa_TABLE(5,23,:)=kappa_5_23
    kappa_TABLE(5,24,:)=kappa_5_24
    kappa_TABLE(5,25,:)=kappa_5_25
    kappa_TABLE(5,26,:)=kappa_5_26
    kappa_TABLE(5,27,:)=kappa_5_27
    kappa_TABLE(5,28,:)=kappa_5_28
    kappa_TABLE(5,29,:)=kappa_5_29
    kappa_TABLE(5,30,:)=kappa_5_30
    kappa_TABLE(5,31,:)=kappa_5_31

    kappa_TABLE(6, 1,:)=kappa_6_1
    kappa_TABLE(6, 2,:)=kappa_6_2
    kappa_TABLE(6, 3,:)=kappa_6_3
    kappa_TABLE(6, 4,:)=kappa_6_4
    kappa_TABLE(6, 5,:)=kappa_6_5
    kappa_TABLE(6, 6,:)=kappa_6_6
    kappa_TABLE(6, 7,:)=kappa_6_7
    kappa_TABLE(6, 8,:)=kappa_6_8
    kappa_TABLE(6, 9,:)=kappa_6_9
    kappa_TABLE(6,10,:)=kappa_6_10
    kappa_TABLE(6,11,:)=kappa_6_11
    kappa_TABLE(6,12,:)=kappa_6_12
    kappa_TABLE(6,13,:)=kappa_6_13
    kappa_TABLE(6,14,:)=kappa_6_14
    kappa_TABLE(6,15,:)=kappa_6_15
    kappa_TABLE(6,16,:)=kappa_6_16
    kappa_TABLE(6,17,:)=kappa_6_17
    kappa_TABLE(6,18,:)=kappa_6_18
    kappa_TABLE(6,19,:)=kappa_6_19
    kappa_TABLE(6,20,:)=kappa_6_20
    kappa_TABLE(6,21,:)=kappa_6_21
    kappa_TABLE(6,22,:)=kappa_6_22
    kappa_TABLE(6,23,:)=kappa_6_23
    kappa_TABLE(6,24,:)=kappa_6_24
    kappa_TABLE(6,25,:)=kappa_6_25
    kappa_TABLE(6,26,:)=kappa_6_26
    kappa_TABLE(6,27,:)=kappa_6_27
    kappa_TABLE(6,28,:)=kappa_6_28
    kappa_TABLE(6,29,:)=kappa_6_29
    kappa_TABLE(6,30,:)=kappa_6_30
    kappa_TABLE(6,31,:)=kappa_6_31

    kappa_TABLE(7, 1,:)=kappa_7_1
    kappa_TABLE(7, 2,:)=kappa_7_2
    kappa_TABLE(7, 3,:)=kappa_7_3
    kappa_TABLE(7, 4,:)=kappa_7_4
    kappa_TABLE(7, 5,:)=kappa_7_5
    kappa_TABLE(7, 6,:)=kappa_7_6
    kappa_TABLE(7, 7,:)=kappa_7_7
    kappa_TABLE(7, 8,:)=kappa_7_8
    kappa_TABLE(7, 9,:)=kappa_7_9
    kappa_TABLE(7,10,:)=kappa_7_10
    kappa_TABLE(7,11,:)=kappa_7_11
    kappa_TABLE(7,12,:)=kappa_7_12
    kappa_TABLE(7,13,:)=kappa_7_13
    kappa_TABLE(7,14,:)=kappa_7_14
    kappa_TABLE(7,15,:)=kappa_7_15
    kappa_TABLE(7,16,:)=kappa_7_16
    kappa_TABLE(7,17,:)=kappa_7_17
    kappa_TABLE(7,18,:)=kappa_7_18
    kappa_TABLE(7,19,:)=kappa_7_19
    kappa_TABLE(7,20,:)=kappa_7_20
    kappa_TABLE(7,21,:)=kappa_7_21
    kappa_TABLE(7,22,:)=kappa_7_22
    kappa_TABLE(7,23,:)=kappa_7_23
    kappa_TABLE(7,24,:)=kappa_7_24
    kappa_TABLE(7,25,:)=kappa_7_25
    kappa_TABLE(7,26,:)=kappa_7_26
    kappa_TABLE(7,27,:)=kappa_7_27
    kappa_TABLE(7,28,:)=kappa_7_28
    kappa_TABLE(7,29,:)=kappa_7_29
    kappa_TABLE(7,30,:)=kappa_7_30
    kappa_TABLE(7,31,:)=kappa_7_31

    kappa_TABLE(8, 1,:)=kappa_8_1
    kappa_TABLE(8, 2,:)=kappa_8_2
    kappa_TABLE(8, 3,:)=kappa_8_3
    kappa_TABLE(8, 4,:)=kappa_8_4
    kappa_TABLE(8, 5,:)=kappa_8_5
    kappa_TABLE(8, 6,:)=kappa_8_6
    kappa_TABLE(8, 7,:)=kappa_8_7
    kappa_TABLE(8, 8,:)=kappa_8_8
    kappa_TABLE(8, 9,:)=kappa_8_9
    kappa_TABLE(8,10,:)=kappa_8_10
    kappa_TABLE(8,11,:)=kappa_8_11
    kappa_TABLE(8,12,:)=kappa_8_12
    kappa_TABLE(8,13,:)=kappa_8_13
    kappa_TABLE(8,14,:)=kappa_8_14
    kappa_TABLE(8,15,:)=kappa_8_15
    kappa_TABLE(8,16,:)=kappa_8_16
    kappa_TABLE(8,17,:)=kappa_8_17
    kappa_TABLE(8,18,:)=kappa_8_18
    kappa_TABLE(8,19,:)=kappa_8_19
    kappa_TABLE(8,20,:)=kappa_8_20
    kappa_TABLE(8,21,:)=kappa_8_21
    kappa_TABLE(8,22,:)=kappa_8_22
    kappa_TABLE(8,23,:)=kappa_8_23
    kappa_TABLE(8,24,:)=kappa_8_24
    kappa_TABLE(8,25,:)=kappa_8_25
    kappa_TABLE(8,26,:)=kappa_8_26
    kappa_TABLE(8,27,:)=kappa_8_27
    kappa_TABLE(8,28,:)=kappa_8_28
    kappa_TABLE(8,29,:)=kappa_8_29
    kappa_TABLE(8,30,:)=kappa_8_30
    kappa_TABLE(8,31,:)=kappa_8_31

    tau_TABLE(1, 1,:)=tau_1_1
    tau_TABLE(1, 2,:)=tau_1_2
    tau_TABLE(1, 3,:)=tau_1_3
    tau_TABLE(1, 4,:)=tau_1_4
    tau_TABLE(1, 5,:)=tau_1_5
    tau_TABLE(1, 6,:)=tau_1_6
    tau_TABLE(1, 7,:)=tau_1_7
    tau_TABLE(1, 8,:)=tau_1_8
    tau_TABLE(1, 9,:)=tau_1_9
    tau_TABLE(1,10,:)=tau_1_10
    tau_TABLE(1,11,:)=tau_1_11
    tau_TABLE(1,12,:)=tau_1_12
    tau_TABLE(1,13,:)=tau_1_13
    tau_TABLE(1,14,:)=tau_1_14
    tau_TABLE(1,15,:)=tau_1_15
    tau_TABLE(1,16,:)=tau_1_16
    tau_TABLE(1,17,:)=tau_1_17
    tau_TABLE(1,18,:)=tau_1_18
    tau_TABLE(1,19,:)=tau_1_19
    tau_TABLE(1,20,:)=tau_1_20
    tau_TABLE(1,21,:)=tau_1_21
    tau_TABLE(1,22,:)=tau_1_22
    tau_TABLE(1,23,:)=tau_1_23
    tau_TABLE(1,24,:)=tau_1_24
    tau_TABLE(1,25,:)=tau_1_25
    tau_TABLE(1,26,:)=tau_1_26
    tau_TABLE(1,27,:)=tau_1_27
    tau_TABLE(1,28,:)=tau_1_28
    tau_TABLE(1,29,:)=tau_1_29
    tau_TABLE(1,30,:)=tau_1_30
    tau_TABLE(1,31,:)=tau_1_31

    tau_TABLE(2, 1,:)=tau_2_1
    tau_TABLE(2, 2,:)=tau_2_2
    tau_TABLE(2, 3,:)=tau_2_3
    tau_TABLE(2, 4,:)=tau_2_4
    tau_TABLE(2, 5,:)=tau_2_5
    tau_TABLE(2, 6,:)=tau_2_6
    tau_TABLE(2, 7,:)=tau_2_7
    tau_TABLE(2, 8,:)=tau_2_8
    tau_TABLE(2, 9,:)=tau_2_9
    tau_TABLE(2,10,:)=tau_2_10
    tau_TABLE(2,11,:)=tau_2_11
    tau_TABLE(2,12,:)=tau_2_12
    tau_TABLE(2,13,:)=tau_2_13
    tau_TABLE(2,14,:)=tau_2_14
    tau_TABLE(2,15,:)=tau_2_15
    tau_TABLE(2,16,:)=tau_2_16
    tau_TABLE(2,17,:)=tau_2_17
    tau_TABLE(2,18,:)=tau_2_18
    tau_TABLE(2,19,:)=tau_2_19
    tau_TABLE(2,20,:)=tau_2_20
    tau_TABLE(2,21,:)=tau_2_21
    tau_TABLE(2,22,:)=tau_2_22
    tau_TABLE(2,23,:)=tau_2_23
    tau_TABLE(2,24,:)=tau_2_24
    tau_TABLE(2,25,:)=tau_2_25
    tau_TABLE(2,26,:)=tau_2_26
    tau_TABLE(2,27,:)=tau_2_27
    tau_TABLE(2,28,:)=tau_2_28
    tau_TABLE(2,29,:)=tau_2_29
    tau_TABLE(2,30,:)=tau_2_30
    tau_TABLE(2,31,:)=tau_2_31

    tau_TABLE(3, 1,:)=tau_3_1
    tau_TABLE(3, 2,:)=tau_3_2
    tau_TABLE(3, 3,:)=tau_3_3
    tau_TABLE(3, 4,:)=tau_3_4
    tau_TABLE(3, 5,:)=tau_3_5
    tau_TABLE(3, 6,:)=tau_3_6
    tau_TABLE(3, 7,:)=tau_3_7
    tau_TABLE(3, 8,:)=tau_3_8
    tau_TABLE(3, 9,:)=tau_3_9
    tau_TABLE(3,10,:)=tau_3_10
    tau_TABLE(3,11,:)=tau_3_11
    tau_TABLE(3,12,:)=tau_3_12
    tau_TABLE(3,13,:)=tau_3_13
    tau_TABLE(3,14,:)=tau_3_14
    tau_TABLE(3,15,:)=tau_3_15
    tau_TABLE(3,16,:)=tau_3_16
    tau_TABLE(3,17,:)=tau_3_17
    tau_TABLE(3,18,:)=tau_3_18
    tau_TABLE(3,19,:)=tau_3_19
    tau_TABLE(3,20,:)=tau_3_20
    tau_TABLE(3,21,:)=tau_3_21
    tau_TABLE(3,22,:)=tau_3_22
    tau_TABLE(3,23,:)=tau_3_23
    tau_TABLE(3,24,:)=tau_3_24
    tau_TABLE(3,25,:)=tau_3_25
    tau_TABLE(3,26,:)=tau_3_26
    tau_TABLE(3,27,:)=tau_3_27
    tau_TABLE(3,28,:)=tau_3_28
    tau_TABLE(3,29,:)=tau_3_29
    tau_TABLE(3,30,:)=tau_3_30
    tau_TABLE(3,31,:)=tau_3_31

    tau_TABLE(4, 1,:)=tau_4_1
    tau_TABLE(4, 2,:)=tau_4_2
    tau_TABLE(4, 3,:)=tau_4_3
    tau_TABLE(4, 4,:)=tau_4_4
    tau_TABLE(4, 5,:)=tau_4_5
    tau_TABLE(4, 6,:)=tau_4_6
    tau_TABLE(4, 7,:)=tau_4_7
    tau_TABLE(4, 8,:)=tau_4_8
    tau_TABLE(4, 9,:)=tau_4_9
    tau_TABLE(4,10,:)=tau_4_10
    tau_TABLE(4,11,:)=tau_4_11
    tau_TABLE(4,12,:)=tau_4_12
    tau_TABLE(4,13,:)=tau_4_13
    tau_TABLE(4,14,:)=tau_4_14
    tau_TABLE(4,15,:)=tau_4_15
    tau_TABLE(4,16,:)=tau_4_16
    tau_TABLE(4,17,:)=tau_4_17
    tau_TABLE(4,18,:)=tau_4_18
    tau_TABLE(4,19,:)=tau_4_19
    tau_TABLE(4,20,:)=tau_4_20
    tau_TABLE(4,21,:)=tau_4_21
    tau_TABLE(4,22,:)=tau_4_22
    tau_TABLE(4,23,:)=tau_4_23
    tau_TABLE(4,24,:)=tau_4_24
    tau_TABLE(4,25,:)=tau_4_25
    tau_TABLE(4,26,:)=tau_4_26
    tau_TABLE(4,27,:)=tau_4_27
    tau_TABLE(4,28,:)=tau_4_28
    tau_TABLE(4,29,:)=tau_4_29
    tau_TABLE(4,30,:)=tau_4_30
    tau_TABLE(4,31,:)=tau_4_31

    tau_TABLE(5, 1,:)=tau_5_1
    tau_TABLE(5, 2,:)=tau_5_2
    tau_TABLE(5, 3,:)=tau_5_3
    tau_TABLE(5, 4,:)=tau_5_4
    tau_TABLE(5, 5,:)=tau_5_5
    tau_TABLE(5, 6,:)=tau_5_6
    tau_TABLE(5, 7,:)=tau_5_7
    tau_TABLE(5, 8,:)=tau_5_8
    tau_TABLE(5, 9,:)=tau_5_9
    tau_TABLE(5,10,:)=tau_5_10
    tau_TABLE(5,11,:)=tau_5_11
    tau_TABLE(5,12,:)=tau_5_12
    tau_TABLE(5,13,:)=tau_5_13
    tau_TABLE(5,14,:)=tau_5_14
    tau_TABLE(5,15,:)=tau_5_15
    tau_TABLE(5,16,:)=tau_5_16
    tau_TABLE(5,17,:)=tau_5_17
    tau_TABLE(5,18,:)=tau_5_18
    tau_TABLE(5,19,:)=tau_5_19
    tau_TABLE(5,20,:)=tau_5_20
    tau_TABLE(5,21,:)=tau_5_21
    tau_TABLE(5,22,:)=tau_5_22
    tau_TABLE(5,23,:)=tau_5_23
    tau_TABLE(5,24,:)=tau_5_24
    tau_TABLE(5,25,:)=tau_5_25
    tau_TABLE(5,26,:)=tau_5_26
    tau_TABLE(5,27,:)=tau_5_27
    tau_TABLE(5,28,:)=tau_5_28
    tau_TABLE(5,29,:)=tau_5_29
    tau_TABLE(5,30,:)=tau_5_30
    tau_TABLE(5,31,:)=tau_5_31

    tau_TABLE(6, 1,:)=tau_6_1
    tau_TABLE(6, 2,:)=tau_6_2
    tau_TABLE(6, 3,:)=tau_6_3
    tau_TABLE(6, 4,:)=tau_6_4
    tau_TABLE(6, 5,:)=tau_6_5
    tau_TABLE(6, 6,:)=tau_6_6
    tau_TABLE(6, 7,:)=tau_6_7
    tau_TABLE(6, 8,:)=tau_6_8
    tau_TABLE(6, 9,:)=tau_6_9
    tau_TABLE(6,10,:)=tau_6_10
    tau_TABLE(6,11,:)=tau_6_11
    tau_TABLE(6,12,:)=tau_6_12
    tau_TABLE(6,13,:)=tau_6_13
    tau_TABLE(6,14,:)=tau_6_14
    tau_TABLE(6,15,:)=tau_6_15
    tau_TABLE(6,16,:)=tau_6_16
    tau_TABLE(6,17,:)=tau_6_17
    tau_TABLE(6,18,:)=tau_6_18
    tau_TABLE(6,19,:)=tau_6_19
    tau_TABLE(6,20,:)=tau_6_20
    tau_TABLE(6,21,:)=tau_6_21
    tau_TABLE(6,22,:)=tau_6_22
    tau_TABLE(6,23,:)=tau_6_23
    tau_TABLE(6,24,:)=tau_6_24
    tau_TABLE(6,25,:)=tau_6_25
    tau_TABLE(6,26,:)=tau_6_26
    tau_TABLE(6,27,:)=tau_6_27
    tau_TABLE(6,28,:)=tau_6_28
    tau_TABLE(6,29,:)=tau_6_29
    tau_TABLE(6,30,:)=tau_6_30
    tau_TABLE(6,31,:)=tau_6_31

    tau_TABLE(7, 1,:)=tau_7_1
    tau_TABLE(7, 2,:)=tau_7_2
    tau_TABLE(7, 3,:)=tau_7_3
    tau_TABLE(7, 4,:)=tau_7_4
    tau_TABLE(7, 5,:)=tau_7_5
    tau_TABLE(7, 6,:)=tau_7_6
    tau_TABLE(7, 7,:)=tau_7_7
    tau_TABLE(7, 8,:)=tau_7_8
    tau_TABLE(7, 9,:)=tau_7_9
    tau_TABLE(7,10,:)=tau_7_10
    tau_TABLE(7,11,:)=tau_7_11
    tau_TABLE(7,12,:)=tau_7_12
    tau_TABLE(7,13,:)=tau_7_13
    tau_TABLE(7,14,:)=tau_7_14
    tau_TABLE(7,15,:)=tau_7_15
    tau_TABLE(7,16,:)=tau_7_16
    tau_TABLE(7,17,:)=tau_7_17
    tau_TABLE(7,18,:)=tau_7_18
    tau_TABLE(7,19,:)=tau_7_19
    tau_TABLE(7,20,:)=tau_7_20
    tau_TABLE(7,21,:)=tau_7_21
    tau_TABLE(7,22,:)=tau_7_22
    tau_TABLE(7,23,:)=tau_7_23
    tau_TABLE(7,24,:)=tau_7_24
    tau_TABLE(7,25,:)=tau_7_25
    tau_TABLE(7,26,:)=tau_7_26
    tau_TABLE(7,27,:)=tau_7_27
    tau_TABLE(7,28,:)=tau_7_28
    tau_TABLE(7,29,:)=tau_7_29
    tau_TABLE(7,30,:)=tau_7_30
    tau_TABLE(7,31,:)=tau_7_31

    tau_TABLE(8, 1,:)=tau_8_1
    tau_TABLE(8, 2,:)=tau_8_2
    tau_TABLE(8, 3,:)=tau_8_3
    tau_TABLE(8, 4,:)=tau_8_4
    tau_TABLE(8, 5,:)=tau_8_5
    tau_TABLE(8, 6,:)=tau_8_6
    tau_TABLE(8, 7,:)=tau_8_7
    tau_TABLE(8, 8,:)=tau_8_8
    tau_TABLE(8, 9,:)=tau_8_9
    tau_TABLE(8,10,:)=tau_8_10
    tau_TABLE(8,11,:)=tau_8_11
    tau_TABLE(8,12,:)=tau_8_12
    tau_TABLE(8,13,:)=tau_8_13
    tau_TABLE(8,14,:)=tau_8_14
    tau_TABLE(8,15,:)=tau_8_15
    tau_TABLE(8,16,:)=tau_8_16
    tau_TABLE(8,17,:)=tau_8_17
    tau_TABLE(8,18,:)=tau_8_18
    tau_TABLE(8,19,:)=tau_8_19
    tau_TABLE(8,20,:)=tau_8_20
    tau_TABLE(8,21,:)=tau_8_21
    tau_TABLE(8,22,:)=tau_8_22
    tau_TABLE(8,23,:)=tau_8_23
    tau_TABLE(8,24,:)=tau_8_24
    tau_TABLE(8,25,:)=tau_8_25
    tau_TABLE(8,26,:)=tau_8_26
    tau_TABLE(8,27,:)=tau_8_27
    tau_TABLE(8,28,:)=tau_8_28
    tau_TABLE(8,29,:)=tau_8_29
    tau_TABLE(8,30,:)=tau_8_30
    tau_TABLE(8,31,:)=tau_8_31

  end subroutine read_esm_snicar_parameters


  subroutine read_mp_veg_parameters(DATASET_IDENTIFIER)
    implicit none
    character(len=*), intent(in) :: DATASET_IDENTIFIER
    integer :: ierr
    INTEGER :: IK,IM

    integer :: NVEG
    character(len=256) :: VEG_DATASET_DESCRIPTION

    INTEGER :: ISURBAN
    INTEGER :: ISWATER
    INTEGER :: ISBARREN
    INTEGER :: ISICE
    INTEGER :: ISCROP
    INTEGER :: EBLFOREST
    INTEGER :: NATURAL
    INTEGER :: LOW_DENSITY_RESIDENTIAL
    INTEGER :: HIGH_DENSITY_RESIDENTIAL
    INTEGER :: HIGH_INTENSITY_INDUSTRIAL

    REAL, DIMENSION(MVT) :: SAI_JAN,SAI_FEB,SAI_MAR,SAI_APR,SAI_MAY,SAI_JUN, &
                                     SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC
    REAL, DIMENSION(MVT) :: LAI_JAN,LAI_FEB,LAI_MAR,LAI_APR,LAI_MAY,LAI_JUN, &
                                     LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC
    REAL, DIMENSION(MVT) :: RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, &
                                     TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR
    REAL, DIMENSION(MVT) :: CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, &
                     AVCMX, AQE, LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  &
                     BP, MP, QE25, RMS25, RMR25, ARM, FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, &
                     SRA, OMR, MQX, RTOMAX, RROOT, SCEXP, &
                     SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    NAMELIST / noahmp_usgs_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noahmp_usgs_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
         LOW_DENSITY_RESIDENTIAL, HIGH_DENSITY_RESIDENTIAL, HIGH_INTENSITY_INDUSTRIAL, &
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, SRA, OMR, MQX, RTOMAX, RROOT, SCEXP, &
         SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
         LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
         RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    NAMELIST / noahmp_modis_veg_categories / VEG_DATASET_DESCRIPTION, NVEG
    NAMELIST / noahmp_modis_parameters / ISURBAN, ISWATER, ISBARREN, ISICE, ISCROP, EBLFOREST, NATURAL, &
         LOW_DENSITY_RESIDENTIAL, HIGH_DENSITY_RESIDENTIAL, HIGH_INTENSITY_INDUSTRIAL, &
         CH2OP, DLEAF, Z0MVT, HVT, HVB, DEN, RC, MFSNO, XL, CWPVT, C3PSN, KC25, AKC, KO25, AKO, AVCMX, AQE, &
         LTOVRC,  DILEFC,  DILEFW,  RMF25 ,  SLA   ,  FRAGR ,  TMIN  ,  VCMX25,  TDLEF ,  BP, MP, QE25, RMS25, RMR25, ARM, &
         FOLNMX, WDPOOL, WRRAT, MRP, NROOT, RGL, RS, HS, TOPT, RSMAX, SRA, OMR, MQX, RTOMAX, RROOT,SCEXP, &
         SAI_JAN, SAI_FEB, SAI_MAR, SAI_APR, SAI_MAY, SAI_JUN,SAI_JUL,SAI_AUG,SAI_SEP,SAI_OCT,SAI_NOV,SAI_DEC, &
         LAI_JAN, LAI_FEB, LAI_MAR, LAI_APR, LAI_MAY, LAI_JUN,LAI_JUL,LAI_AUG,LAI_SEP,LAI_OCT,LAI_NOV,LAI_DEC, &
         RHOL_VIS, RHOL_NIR, RHOS_VIS, RHOS_NIR, TAUL_VIS, TAUL_NIR, TAUS_VIS, TAUS_NIR, SLAREA, EPS1, EPS2, EPS3, EPS4, EPS5

    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
    CH2OP_TABLE  = -1.E36
    DLEAF_TABLE  = -1.E36
    Z0MVT_TABLE  = -1.E36
    HVT_TABLE    = -1.E36
    HVB_TABLE    = -1.E36
    DEN_TABLE    = -1.E36
    RC_TABLE     = -1.E36
    MFSNO_TABLE  = -1.E36
    RHOL_TABLE   = -1.E36
    RHOS_TABLE   = -1.E36
    TAUL_TABLE   = -1.E36
    TAUS_TABLE   = -1.E36
    XL_TABLE     = -1.E36
    CWPVT_TABLE  = -1.E36
    C3PSN_TABLE  = -1.E36
    KC25_TABLE   = -1.E36
    AKC_TABLE    = -1.E36
    KO25_TABLE   = -1.E36
    AKO_TABLE    = -1.E36
    AVCMX_TABLE  = -1.E36
    AQE_TABLE    = -1.E36
    LTOVRC_TABLE = -1.E36
    DILEFC_TABLE = -1.E36
    DILEFW_TABLE = -1.E36
    RMF25_TABLE  = -1.E36
    SLA_TABLE    = -1.E36
    FRAGR_TABLE  = -1.E36
    TMIN_TABLE   = -1.E36
    VCMX25_TABLE = -1.E36
    TDLEF_TABLE  = -1.E36
    BP_TABLE     = -1.E36
    MP_TABLE     = -1.E36
    QE25_TABLE   = -1.E36
    RMS25_TABLE  = -1.E36
    RMR25_TABLE  = -1.E36
    ARM_TABLE    = -1.E36
    FOLNMX_TABLE = -1.E36
    WDPOOL_TABLE = -1.E36
    WRRAT_TABLE  = -1.E36
    MRP_TABLE    = -1.E36
    SAIM_TABLE   = -1.E36
    LAIM_TABLE   = -1.E36
    NROOT_TABLE  = -99999
    RGL_TABLE    = -1.E36
    RS_TABLE     = -1.E36
    HS_TABLE     = -1.E36
    TOPT_TABLE   = -1.E36
    RSMAX_TABLE  = -1.E36
    SRA_TABLE    = -1.E36     !min specific root area [m2/kg]
    OMR_TABLE    = -1.E36     !root resistivity to water uptake [s]
    MQX_TABLE    = -1.E36     !ratio of water storage to dry biomass [-]
    RTOMAX_TABLE = -1.E36     !max root turnover rate [g/m2/year]
    RROOT_TABLE  = -1.E36     !mean radius of fine roots [mm]
    SCEXP_TABLE  = -1.E36     !
    ISURBAN_TABLE      = -99999
    ISWATER_TABLE      = -99999
    ISBARREN_TABLE     = -99999
    ISICE_TABLE        = -99999
    ISCROP_TABLE       = -99999
    EBLFOREST_TABLE    = -99999
    NATURAL_TABLE      = -99999
    LOW_DENSITY_RESIDENTIAL_TABLE   = -99999
    HIGH_DENSITY_RESIDENTIAL_TABLE  = -99999
    HIGH_INTENSITY_INDUSTRIAL_TABLE = -99999

    open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    if (ierr /= 0) then
       write(*,'("****** Error ******************************************************")')
       write(*,'("Cannot find file MPTABLE.TBL")')
       write(*,'("STOP")')
       write(*,'("*******************************************************************")')
       call wrf_error_fatal("STOP in Noah-MP read_mp_veg_parameters")
    endif

    if ( trim(DATASET_IDENTIFIER) == "USGS" ) then
       read(15,noahmp_usgs_veg_categories)
       read(15,noahmp_usgs_parameters)
    else if ( trim(DATASET_IDENTIFIER) == "MODIFIED_IGBP_MODIS_NOAH" ) then
       read(15,noahmp_modis_veg_categories)
       read(15,noahmp_modis_parameters)
    else
       write(*,'("Unrecognized DATASET_IDENTIFIER in subroutine READ_MP_VEG_PARAMETERS")')
       write(*,'("DATASET_IDENTIFIER = ''", A, "''")') trim(DATASET_IDENTIFIER)
       call wrf_error_fatal("STOP in Noah-MP read_mp_veg_parameters")
    endif
    close(15)

                      ISURBAN_TABLE   = ISURBAN
                      ISWATER_TABLE   = ISWATER
                     ISBARREN_TABLE   = ISBARREN
                        ISICE_TABLE   = ISICE
                       ISCROP_TABLE   = ISCROP
                    EBLFOREST_TABLE   = EBLFOREST
                      NATURAL_TABLE   = NATURAL
      LOW_DENSITY_RESIDENTIAL_TABLE   = LOW_DENSITY_RESIDENTIAL
     HIGH_DENSITY_RESIDENTIAL_TABLE   = HIGH_DENSITY_RESIDENTIAL
    HIGH_INTENSITY_INDUSTRIAL_TABLE   = HIGH_INTENSITY_INDUSTRIAL

     CH2OP_TABLE(1:NVEG)  = CH2OP(1:NVEG)
     DLEAF_TABLE(1:NVEG)  = DLEAF(1:NVEG)
     Z0MVT_TABLE(1:NVEG)  = Z0MVT(1:NVEG)
       HVT_TABLE(1:NVEG)  = HVT(1:NVEG)
       HVB_TABLE(1:NVEG)  = HVB(1:NVEG)
       DEN_TABLE(1:NVEG)  = DEN(1:NVEG)
        RC_TABLE(1:NVEG)  = RC(1:NVEG)
     MFSNO_TABLE(1:NVEG)  = MFSNO(1:NVEG)
        XL_TABLE(1:NVEG)  = XL(1:NVEG)
     CWPVT_TABLE(1:NVEG)  = CWPVT(1:NVEG)
     C3PSN_TABLE(1:NVEG)  = C3PSN(1:NVEG)
      KC25_TABLE(1:NVEG)  = KC25(1:NVEG)
       AKC_TABLE(1:NVEG)  = AKC(1:NVEG)
      KO25_TABLE(1:NVEG)  = KO25(1:NVEG)
       AKO_TABLE(1:NVEG)  = AKO(1:NVEG)
     AVCMX_TABLE(1:NVEG)  = AVCMX(1:NVEG)
       AQE_TABLE(1:NVEG)  = AQE(1:NVEG)
    LTOVRC_TABLE(1:NVEG)  = LTOVRC(1:NVEG)
    DILEFC_TABLE(1:NVEG)  = DILEFC(1:NVEG)
    DILEFW_TABLE(1:NVEG)  = DILEFW(1:NVEG)
     RMF25_TABLE(1:NVEG)  = RMF25(1:NVEG)
       SLA_TABLE(1:NVEG)  = SLA(1:NVEG)
     FRAGR_TABLE(1:NVEG)  = FRAGR(1:NVEG)
      TMIN_TABLE(1:NVEG)  = TMIN(1:NVEG)
    VCMX25_TABLE(1:NVEG)  = VCMX25(1:NVEG)
     TDLEF_TABLE(1:NVEG)  = TDLEF(1:NVEG)
        BP_TABLE(1:NVEG)  = BP(1:NVEG)
        MP_TABLE(1:NVEG)  = MP(1:NVEG)
      QE25_TABLE(1:NVEG)  = QE25(1:NVEG)
     RMS25_TABLE(1:NVEG)  = RMS25(1:NVEG)
     RMR25_TABLE(1:NVEG)  = RMR25(1:NVEG)
       ARM_TABLE(1:NVEG)  = ARM(1:NVEG)
    FOLNMX_TABLE(1:NVEG)  = FOLNMX(1:NVEG)
    WDPOOL_TABLE(1:NVEG)  = WDPOOL(1:NVEG)
     WRRAT_TABLE(1:NVEG)  = WRRAT(1:NVEG)
       MRP_TABLE(1:NVEG)  = MRP(1:NVEG)
     NROOT_TABLE(1:NVEG)  = NROOT(1:NVEG)
       RGL_TABLE(1:NVEG)  = RGL(1:NVEG)
        RS_TABLE(1:NVEG)  = RS(1:NVEG)
        HS_TABLE(1:NVEG)  = HS(1:NVEG)
      TOPT_TABLE(1:NVEG)  = TOPT(1:NVEG)
     RSMAX_TABLE(1:NVEG)  = RSMAX(1:NVEG)
       SRA_TABLE(1:NVEG)  = SRA(1:NVEG)    !min specific root area [m2/kg]
       OMR_TABLE(1:NVEG)  = OMR(1:NVEG)    !root resistivity to water uptake [s]
       MQX_TABLE(1:NVEG)  = MQX(1:NVEG)    !ratio of water storage to dry biomass [-]
    RTOMAX_TABLE(1:NVEG)  = RTOMAX(1:NVEG) !max root turnover rate [g/m2/year]
     RROOT_TABLE(1:NVEG)  = RROOT(1:NVEG)  !mean radius of fine roots [mm]
     SCEXP_TABLE(1:NVEG)  = SCEXP(1:NVEG)  !

!     write(*,*) SRA_TABLE(1:NVEG)
!     write(*,*) OMR_TABLE(1:NVEG)
!     write(*,*) MQX_TABLE(1:NVEG)
!     write(*,*) RTOMAX_TABLE(1:NVEG)
!     write(*,*) RROOT_TABLE(1:NVEG)
!     write(*,*) SCEXP_TABLE(1:NVEG)

    ! Put LAI and SAI into 2d array from monthly lines in table; same for canopy radiation properties

    SAIM_TABLE(1:NVEG, 1) = SAI_JAN(1:NVEG)
    SAIM_TABLE(1:NVEG, 2) = SAI_FEB(1:NVEG)
    SAIM_TABLE(1:NVEG, 3) = SAI_MAR(1:NVEG)
    SAIM_TABLE(1:NVEG, 4) = SAI_APR(1:NVEG)
    SAIM_TABLE(1:NVEG, 5) = SAI_MAY(1:NVEG)
    SAIM_TABLE(1:NVEG, 6) = SAI_JUN(1:NVEG)
    SAIM_TABLE(1:NVEG, 7) = SAI_JUL(1:NVEG)
    SAIM_TABLE(1:NVEG, 8) = SAI_AUG(1:NVEG)
    SAIM_TABLE(1:NVEG, 9) = SAI_SEP(1:NVEG)
    SAIM_TABLE(1:NVEG,10) = SAI_OCT(1:NVEG)
    SAIM_TABLE(1:NVEG,11) = SAI_NOV(1:NVEG)
    SAIM_TABLE(1:NVEG,12) = SAI_DEC(1:NVEG)

    LAIM_TABLE(1:NVEG, 1) = LAI_JAN(1:NVEG)
    LAIM_TABLE(1:NVEG, 2) = LAI_FEB(1:NVEG)
    LAIM_TABLE(1:NVEG, 3) = LAI_MAR(1:NVEG)
    LAIM_TABLE(1:NVEG, 4) = LAI_APR(1:NVEG)
    LAIM_TABLE(1:NVEG, 5) = LAI_MAY(1:NVEG)
    LAIM_TABLE(1:NVEG, 6) = LAI_JUN(1:NVEG)
    LAIM_TABLE(1:NVEG, 7) = LAI_JUL(1:NVEG)
    LAIM_TABLE(1:NVEG, 8) = LAI_AUG(1:NVEG)
    LAIM_TABLE(1:NVEG, 9) = LAI_SEP(1:NVEG)
    LAIM_TABLE(1:NVEG,10) = LAI_OCT(1:NVEG)
    LAIM_TABLE(1:NVEG,11) = LAI_NOV(1:NVEG)
    LAIM_TABLE(1:NVEG,12) = LAI_DEC(1:NVEG)

    RHOL_TABLE(1:NVEG,1)  = RHOL_VIS(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    RHOL_TABLE(1:NVEG,2)  = RHOL_NIR(1:NVEG) !leaf reflectance: 1=vis, 2=nir
    RHOS_TABLE(1:NVEG,1)  = RHOS_VIS(1:NVEG) !stem reflectance: 1=vis, 2=nir
    RHOS_TABLE(1:NVEG,2)  = RHOS_NIR(1:NVEG) !stem reflectance: 1=vis, 2=nir
    TAUL_TABLE(1:NVEG,1)  = TAUL_VIS(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    TAUL_TABLE(1:NVEG,2)  = TAUL_NIR(1:NVEG) !leaf transmittance: 1=vis, 2=nir
    TAUS_TABLE(1:NVEG,1)  = TAUS_VIS(1:NVEG) !stem transmittance: 1=vis, 2=nir
    TAUS_TABLE(1:NVEG,2)  = TAUS_NIR(1:NVEG) !stem transmittance: 1=vis, 2=nir

  end subroutine read_mp_veg_parameters

  subroutine read_mp_soil_parameters()
    IMPLICIT NONE
    INTEGER :: IERR
    CHARACTER*4         :: SLTYPE
    INTEGER             :: ITMP, NUM_SLOPE, LC
    CHARACTER(len=256)  :: message
    

    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
       BEXP_TABLE = -1.E36
     SMCDRY_TABLE = -1.E36
         F1_TABLE = -1.E36
     SMCMAX_TABLE = -1.E36
     SMCREF_TABLE = -1.E36
     PSISAT_TABLE = -1.E36
      DKSAT_TABLE = -1.E36
      DWSAT_TABLE = -1.E36
     SMCWLT_TABLE = -1.E36
     QUARTZ_TABLE = -1.E36
       SMCR_TABLE = -1.E36
        VGN_TABLE = -1.E36
     VGPSAT_TABLE = -1.E36
      SLOPE_TABLE = -1.E36
      CSOIL_TABLE = -1.E36
      REFDK_TABLE = -1.E36
     REFKDT_TABLE = -1.E36
       FRZK_TABLE = -1.E36
       ZBOT_TABLE = -1.E36
       CZIL_TABLE = -1.E36

!
!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!
    OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
    IF(ierr .NE. 0 ) THEN
      WRITE(message,FMT='(A)') 'module_sf_noahmpdrv.F: read_mp_soil_parameters: failure opening SOILPARM.TBL'
      CALL wrf_error_fatal ( message )
    END IF

    READ (19,*)
    READ (19,*) SLTYPE
    READ (19,*) SLCATS
    WRITE( message , * ) 'SOIL TEXTURE CLASSIFICATION = ', TRIM ( SLTYPE ) , ' FOUND', &
               SLCATS,' CATEGORIES'
    CALL wrf_message ( message )

    DO LC=1,SLCATS
      READ (19,*) ITMP,BEXP_TABLE(LC),SMCDRY_TABLE(LC),F1_TABLE(LC),SMCMAX_TABLE(LC),    &
                  SMCREF_TABLE(LC),PSISAT_TABLE(LC),DKSAT_TABLE(LC), DWSAT_TABLE(LC),   &
                  SMCWLT_TABLE(LC),QUARTZ_TABLE(LC),SMCR_TABLE(LC),VGN_TABLE(LC),       &
                  VGPSAT_TABLE(LC)
      write (*,*) ITMP,BEXP_TABLE(LC),SMCDRY_TABLE(LC),F1_TABLE(LC),SMCMAX_TABLE(LC),    &
                  SMCREF_TABLE(LC),PSISAT_TABLE(LC),DKSAT_TABLE(LC), DWSAT_TABLE(LC),   &
                  SMCWLT_TABLE(LC),QUARTZ_TABLE(LC),SMCR_TABLE(LC),VGN_TABLE(LC),       &
                  VGPSAT_TABLE(LC)
    ENDDO

    CLOSE (19)

!
!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
    OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
    IF(ierr .NE. 0 ) THEN
      WRITE(message,FMT='(A)') 'module_sf_noahlsm.F: read_mp_soil_parameters: failure opening GENPARM.TBL'
      CALL wrf_error_fatal ( message )
    END IF

    READ (19,*)
    READ (19,*)
    READ (19,*) NUM_SLOPE

    DO LC=1,NUM_SLOPE
        READ (19,*) SLOPE_TABLE(LC)
    ENDDO

    READ (19,*)
    READ (19,*)
    READ (19,*)
    READ (19,*)
    READ (19,*)
    READ (19,*) CSOIL_TABLE
    READ (19,*)
    READ (19,*)
    READ (19,*)
    READ (19,*) REFDK_TABLE
    READ (19,*)
    READ (19,*) REFKDT_TABLE
    READ (19,*)
    READ (19,*) FRZK_TABLE
    READ (19,*)
    READ (19,*) ZBOT_TABLE
    READ (19,*)
    READ (19,*) CZIL_TABLE
    READ (19,*)
    READ (19,*)
    READ (19,*)
    READ (19,*)

    CLOSE (19)

  end subroutine read_mp_soil_parameters

  subroutine read_mp_rad_parameters()
    implicit none
    integer :: ierr

    REAL :: ALBICE(MBAND),ALBLAK(MBAND),OMEGAS(MBAND),BETADS,BETAIS,EG(2)
    REAL :: ALBSAT_VIS(MSC)
    REAL :: ALBSAT_NIR(MSC)
    REAL :: ALBDRY_VIS(MSC)
    REAL :: ALBDRY_NIR(MSC)

    NAMELIST / noahmp_rad_parameters / ALBSAT_VIS,ALBSAT_NIR,ALBDRY_VIS,ALBDRY_NIR,ALBICE,ALBLAK,OMEGAS,BETADS,BETAIS,EG


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
    ALBSAT_TABLE     = -1.E36
    ALBDRY_TABLE     = -1.E36
    ALBICE_TABLE     = -1.E36
    ALBLAK_TABLE     = -1.E36
    OMEGAS_TABLE     = -1.E36
    BETADS_TABLE     = -1.E36
    BETAIS_TABLE     = -1.E36
    EG_TABLE         = -1.E36

    open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    if (ierr /= 0) then
       write(*,'("****** Error ******************************************************")')
       write(*,'("Cannot find file MPTABLE.TBL")')
       write(*,'("STOP")')
       write(*,'("*******************************************************************")')
       call wrf_error_fatal("STOP in Noah-MP read_mp_rad_parameters")
    endif

    read(15,noahmp_rad_parameters)
    close(15)

    ALBSAT_TABLE(:,1) = ALBSAT_VIS ! saturated soil albedos: 1=vis, 2=nir
    ALBSAT_TABLE(:,2) = ALBSAT_NIR ! saturated soil albedos: 1=vis, 2=nir
    ALBDRY_TABLE(:,1) = ALBDRY_VIS ! dry soil albedos: 1=vis, 2=nir
    ALBDRY_TABLE(:,2) = ALBDRY_NIR ! dry soil albedos: 1=vis, 2=nir
    ALBICE_TABLE      = ALBICE
    ALBLAK_TABLE      = ALBLAK
    OMEGAS_TABLE      = OMEGAS
    BETADS_TABLE      = BETADS
    BETAIS_TABLE      = BETAIS
    EG_TABLE          = EG

  end subroutine read_mp_rad_parameters

  subroutine read_mp_global_parameters()
    implicit none
    integer :: ierr

    REAL :: CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI,SWEMX,RSURF_SNOW

    NAMELIST / noahmp_global_parameters / CO2,O2,TIMEAN,FSATMX,Z0SNO,SSI,SWEMX,RSURF_SNOW


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
       CO2_TABLE     = -1.E36
        O2_TABLE     = -1.E36
    TIMEAN_TABLE     = -1.E36
    FSATMX_TABLE     = -1.E36
     Z0SNO_TABLE     = -1.E36
       SSI_TABLE     = -1.E36
     SWEMX_TABLE     = -1.E36
RSURF_SNOW_TABLE     = -1.E36

    open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    if (ierr /= 0) then
       write(*,'("****** Error ******************************************************")')
       write(*,'("Cannot find file MPTABLE.TBL")')
       write(*,'("STOP")')
       write(*,'("*******************************************************************")')
       call wrf_error_fatal("STOP in Noah-MP read_mp_global_parameters")
    endif

    read(15,noahmp_global_parameters)
    close(15)

       CO2_TABLE     = CO2
        O2_TABLE     = O2
    TIMEAN_TABLE     = TIMEAN
    FSATMX_TABLE     = FSATMX
     Z0SNO_TABLE     = Z0SNO
       SSI_TABLE     = SSI
     SWEMX_TABLE     = SWEMX
RSURF_SNOW_TABLE     = RSURF_SNOW

  end subroutine read_mp_global_parameters

  subroutine read_mp_crop_parameters()
    implicit none
    integer :: ierr

 INTEGER                   :: DEFAULT_CROP
 INTEGER, DIMENSION(NCROP) :: PLTDAY
 INTEGER, DIMENSION(NCROP) :: HSDAY
    REAL, DIMENSION(NCROP) :: PLANTPOP
    REAL, DIMENSION(NCROP) :: IRRI
    REAL, DIMENSION(NCROP) :: GDDTBASE
    REAL, DIMENSION(NCROP) :: GDDTCUT
    REAL, DIMENSION(NCROP) :: GDDS1
    REAL, DIMENSION(NCROP) :: GDDS2
    REAL, DIMENSION(NCROP) :: GDDS3
    REAL, DIMENSION(NCROP) :: GDDS4
    REAL, DIMENSION(NCROP) :: GDDS5
 INTEGER, DIMENSION(NCROP) :: C3C4
    REAL, DIMENSION(NCROP) :: AREF
    REAL, DIMENSION(NCROP) :: PSNRF
    REAL, DIMENSION(NCROP) :: I2PAR
    REAL, DIMENSION(NCROP) :: TASSIM0
    REAL, DIMENSION(NCROP) :: TASSIM1
    REAL, DIMENSION(NCROP) :: TASSIM2
    REAL, DIMENSION(NCROP) :: K
    REAL, DIMENSION(NCROP) :: EPSI
    REAL, DIMENSION(NCROP) :: Q10MR
    REAL, DIMENSION(NCROP) :: FOLN_MX
    REAL, DIMENSION(NCROP) :: LEFREEZ
    REAL, DIMENSION(NCROP) :: DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8
    REAL, DIMENSION(NCROP) :: DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8
    REAL, DIMENSION(NCROP) :: FRA_GR
    REAL, DIMENSION(NCROP) :: LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8
    REAL, DIMENSION(NCROP) :: ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8
    REAL, DIMENSION(NCROP) :: RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8
    REAL, DIMENSION(NCROP) :: LFMR25
    REAL, DIMENSION(NCROP) :: STMR25
    REAL, DIMENSION(NCROP) :: RTMR25
    REAL, DIMENSION(NCROP) :: GRAINMR25
    REAL, DIMENSION(NCROP) :: LFPT_S1,LFPT_S2,LFPT_S3,LFPT_S4,LFPT_S5,LFPT_S6,LFPT_S7,LFPT_S8
    REAL, DIMENSION(NCROP) :: STPT_S1,STPT_S2,STPT_S3,STPT_S4,STPT_S5,STPT_S6,STPT_S7,STPT_S8
    REAL, DIMENSION(NCROP) :: RTPT_S1,RTPT_S2,RTPT_S3,RTPT_S4,RTPT_S5,RTPT_S6,RTPT_S7,RTPT_S8
    REAL, DIMENSION(NCROP) :: GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8
    REAL, DIMENSION(NCROP) :: BIO2LAI


    NAMELIST / noahmp_crop_parameters /DEFAULT_CROP,   PLTDAY,     HSDAY,  PLANTPOP,      IRRI,  GDDTBASE,   GDDTCUT,     GDDS1,     GDDS2, &
                                             GDDS3,     GDDS4,     GDDS5,      C3C4,      AREF,     PSNRF,     I2PAR,   TASSIM0, &
                                           TASSIM1,   TASSIM2,         K,      EPSI,     Q10MR,   FOLN_MX,   LEFREEZ,            &
                                        DILE_FC_S1,DILE_FC_S2,DILE_FC_S3,DILE_FC_S4,DILE_FC_S5,DILE_FC_S6,DILE_FC_S7,DILE_FC_S8, &
                                        DILE_FW_S1,DILE_FW_S2,DILE_FW_S3,DILE_FW_S4,DILE_FW_S5,DILE_FW_S6,DILE_FW_S7,DILE_FW_S8, &
                                            FRA_GR,                                                                              &
                                        LF_OVRC_S1,LF_OVRC_S2,LF_OVRC_S3,LF_OVRC_S4,LF_OVRC_S5,LF_OVRC_S6,LF_OVRC_S7,LF_OVRC_S8, &
                                        ST_OVRC_S1,ST_OVRC_S2,ST_OVRC_S3,ST_OVRC_S4,ST_OVRC_S5,ST_OVRC_S6,ST_OVRC_S7,ST_OVRC_S8, &
                                        RT_OVRC_S1,RT_OVRC_S2,RT_OVRC_S3,RT_OVRC_S4,RT_OVRC_S5,RT_OVRC_S6,RT_OVRC_S7,RT_OVRC_S8, &
                                            LFMR25,    STMR25,    RTMR25, GRAINMR25,                                             &
                                           LFPT_S1,   LFPT_S2,   LFPT_S3,   LFPT_S4,   LFPT_S5,   LFPT_S6,   LFPT_S7,   LFPT_S8, &
                                           STPT_S1,   STPT_S2,   STPT_S3,   STPT_S4,   STPT_S5,   STPT_S6,   STPT_S7,   STPT_S8, &
                                           RTPT_S1,   RTPT_S2,   RTPT_S3,   RTPT_S4,   RTPT_S5,   RTPT_S6,   RTPT_S7,   RTPT_S8, &
                                        GRAINPT_S1,GRAINPT_S2,GRAINPT_S3,GRAINPT_S4,GRAINPT_S5,GRAINPT_S6,GRAINPT_S7,GRAINPT_S8, &
                                           BIO2LAI


    ! Initialize our variables to bad values, so that if the namelist read fails, we come to a screeching halt as soon as we try to use anything.
 DEFAULT_CROP_TABLE     = -99999
       PLTDAY_TABLE     = -99999
        HSDAY_TABLE     = -99999
     PLANTPOP_TABLE     = -1.E36
         IRRI_TABLE     = -1.E36
     GDDTBASE_TABLE     = -1.E36
      GDDTCUT_TABLE     = -1.E36
        GDDS1_TABLE     = -1.E36
        GDDS2_TABLE     = -1.E36
        GDDS3_TABLE     = -1.E36
        GDDS4_TABLE     = -1.E36
        GDDS5_TABLE     = -1.E36
         C3C4_TABLE     = -99999
         AREF_TABLE     = -1.E36
        PSNRF_TABLE     = -1.E36
        I2PAR_TABLE     = -1.E36
      TASSIM0_TABLE     = -1.E36
      TASSIM1_TABLE     = -1.E36
      TASSIM2_TABLE     = -1.E36
            K_TABLE     = -1.E36
         EPSI_TABLE     = -1.E36
        Q10MR_TABLE     = -1.E36
      FOLN_MX_TABLE     = -1.E36
      LEFREEZ_TABLE     = -1.E36
      DILE_FC_TABLE     = -1.E36
      DILE_FW_TABLE     = -1.E36
       FRA_GR_TABLE     = -1.E36
      LF_OVRC_TABLE     = -1.E36
      ST_OVRC_TABLE     = -1.E36
      RT_OVRC_TABLE     = -1.E36
       LFMR25_TABLE     = -1.E36
       STMR25_TABLE     = -1.E36
       RTMR25_TABLE     = -1.E36
    GRAINMR25_TABLE     = -1.E36
         LFPT_TABLE     = -1.E36
         STPT_TABLE     = -1.E36
         RTPT_TABLE     = -1.E36
      GRAINPT_TABLE     = -1.E36
      BIO2LAI_TABLE     = -1.E36


    open(15, file="MPTABLE.TBL", status='old', form='formatted', action='read', iostat=ierr)
    if (ierr /= 0) then
       write(*,'("****** Error ******************************************************")')
       write(*,'("Cannot find file MPTABLE.TBL")')
       write(*,'("STOP")')
       write(*,'("*******************************************************************")')
       call wrf_error_fatal("STOP in Noah-MP read_mp_crop_parameters")
    endif

    read(15,noahmp_crop_parameters)
    close(15)

 DEFAULT_CROP_TABLE      = DEFAULT_CROP
       PLTDAY_TABLE      = PLTDAY
        HSDAY_TABLE      = HSDAY
     PLANTPOP_TABLE      = PLANTPOP
         IRRI_TABLE      = IRRI
     GDDTBASE_TABLE      = GDDTBASE
      GDDTCUT_TABLE      = GDDTCUT
        GDDS1_TABLE      = GDDS1
        GDDS2_TABLE      = GDDS2
        GDDS3_TABLE      = GDDS3
        GDDS4_TABLE      = GDDS4
        GDDS5_TABLE      = GDDS5
         C3C4_TABLE      = C3C4
         AREF_TABLE      = AREF
        PSNRF_TABLE      = PSNRF
        I2PAR_TABLE      = I2PAR
      TASSIM0_TABLE      = TASSIM0
      TASSIM1_TABLE      = TASSIM1
      TASSIM2_TABLE      = TASSIM2
            K_TABLE      = K
         EPSI_TABLE      = EPSI
        Q10MR_TABLE      = Q10MR
      FOLN_MX_TABLE      = FOLN_MX
      LEFREEZ_TABLE      = LEFREEZ
      DILE_FC_TABLE(:,1) = DILE_FC_S1
      DILE_FC_TABLE(:,2) = DILE_FC_S2
      DILE_FC_TABLE(:,3) = DILE_FC_S3
      DILE_FC_TABLE(:,4) = DILE_FC_S4
      DILE_FC_TABLE(:,5) = DILE_FC_S5
      DILE_FC_TABLE(:,6) = DILE_FC_S6
      DILE_FC_TABLE(:,7) = DILE_FC_S7
      DILE_FC_TABLE(:,8) = DILE_FC_S8
      DILE_FW_TABLE(:,1) = DILE_FW_S1
      DILE_FW_TABLE(:,2) = DILE_FW_S2
      DILE_FW_TABLE(:,3) = DILE_FW_S3
      DILE_FW_TABLE(:,4) = DILE_FW_S4
      DILE_FW_TABLE(:,5) = DILE_FW_S5
      DILE_FW_TABLE(:,6) = DILE_FW_S6
      DILE_FW_TABLE(:,7) = DILE_FW_S7
      DILE_FW_TABLE(:,8) = DILE_FW_S8
       FRA_GR_TABLE      = FRA_GR
      LF_OVRC_TABLE(:,1) = LF_OVRC_S1
      LF_OVRC_TABLE(:,2) = LF_OVRC_S2
      LF_OVRC_TABLE(:,3) = LF_OVRC_S3
      LF_OVRC_TABLE(:,4) = LF_OVRC_S4
      LF_OVRC_TABLE(:,5) = LF_OVRC_S5
      LF_OVRC_TABLE(:,6) = LF_OVRC_S6
      LF_OVRC_TABLE(:,7) = LF_OVRC_S7
      LF_OVRC_TABLE(:,8) = LF_OVRC_S8
         STPT_TABLE(:,4) = STPT_S4
         STPT_TABLE(:,5) = STPT_S5
         STPT_TABLE(:,6) = STPT_S6
         STPT_TABLE(:,7) = STPT_S7
         STPT_TABLE(:,8) = STPT_S8
         RTPT_TABLE(:,1) = RTPT_S1
         RTPT_TABLE(:,2) = RTPT_S2
         RTPT_TABLE(:,3) = RTPT_S3
         RTPT_TABLE(:,4) = RTPT_S4
         RTPT_TABLE(:,5) = RTPT_S5
         RTPT_TABLE(:,6) = RTPT_S6
         RTPT_TABLE(:,7) = RTPT_S7
         RTPT_TABLE(:,8) = RTPT_S8
      GRAINPT_TABLE(:,1) = GRAINPT_S1
      GRAINPT_TABLE(:,2) = GRAINPT_S2
      GRAINPT_TABLE(:,3) = GRAINPT_S3
      GRAINPT_TABLE(:,4) = GRAINPT_S4
      GRAINPT_TABLE(:,5) = GRAINPT_S5
      GRAINPT_TABLE(:,6) = GRAINPT_S6
      GRAINPT_TABLE(:,7) = GRAINPT_S7
      GRAINPT_TABLE(:,8) = GRAINPT_S8
      BIO2LAI_TABLE      = BIO2LAI

  end subroutine read_mp_crop_parameters

END MODULE NOAHMP_TABLES
