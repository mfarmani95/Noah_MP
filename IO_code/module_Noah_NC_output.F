MODULE module_sf_Noah_NC_output


  contains

!--------------------------------------------------------------------
    SUBROUTINE NC_OUT(nsoil   ,nsnow   ,nx      ,ny      ,it      ,dt      ,  &
                      iyear   ,imonth  ,iday    ,DIR     ,EXP     ,lonxy   ,  &
                      latxy   ,vegtypxy,imstep  ,nday    ,ND      , &
                      snowhxy ,sneqvxy ,tgxy    ,stcxy   ,smcxy   , &
                      sh2oxy  ,prcpxy  ,runsfxy ,runsbxy ,ecanxy  , &
                      edirxy  ,etranxy ,zwtxy   ,fsaxy   ,firaxy  , &
                      fshxy   ,flhxy   ,fghxy   ,aparxy  ,psnxy   , &
                      savxy   ,sagxy   ,scfxy   ,xlaixy  ,xsaixy  , &
                      tradxy  ,neexy   ,gppxy   ,nppxy   ,t2mxy   , &
                      fvegxy  ,cmxy    ,chxy    ,qinxy   ,qco2xy  , &
                      socxy   ,wdocxy  ,ddocxy  ,micxy   ,wenzxy  , &
                      denzxy  ,vmaxxy  ,kmxy    ,vmaxupxy,kmupxy  , &
                      epslonxy,waxy    ,canliqxy,canicexy,ndvixy  , &
                      NSUN    ,mqxy    ,krxy    ,rtmassxy,sadrxy  , &
                      qrootxy ,psixy   ,fsnogxy ,salbxy  ,HTOPXY  , &
                      RSINEXXY, &
                      sfctmpxy,q2xy    ,uuxy    ,vvxy    ,sfcprsxy, &
                      snowhm  ,sneqvm  ,tgm     ,stcm    ,smcm    , &
                      sh2om   ,prcpm   ,runsfm  ,runsbm  ,ecanm   , &
                      edirm   ,etranm  ,zwtm    ,fsam    ,firam   , &
                      fshm    ,flhm    ,fghm    ,aparm   ,psnm    , &
                      savm    ,sagm    ,scfm    ,xlaim   ,xsaim   , &
                      tradm   ,neem    ,gppm    ,nppm    ,t2mm    , &
                      fvegm   ,cmm     ,chm     ,qinm    ,qco2m   , &
                      socm    ,wdocm   ,ddocm   ,micm    ,wenzm   , &
                      denzm   ,vmaxm   ,kmm     ,vmaxupm ,kmupm   , &
                      epslonm ,wam     ,canwatm ,ndvim   ,mqm     , &
                      krm     ,rtmassm ,sadrm   ,qrootm  ,qrootnm , &
                      sfctmpm ,q2m     ,windm   ,sfcprsm ,psim    , &
                      fsnogm  ,salbm   ,HTOPM   ,RSINEXM )

    use module_sf_noahmplsm
    USE NOAHMP_TABLES

    implicit none

    integer :: nroot

! inputs

    integer, intent(in) :: iyear,imonth,iday
    CHARACTER(len=256),INTENT(IN) :: DIR
    CHARACTER(len=8),INTENT(IN) :: EXP
    integer, intent(in)         :: it
    integer, intent(in)         :: imstep
    integer, intent(in)         :: nx
    integer, intent(in)         :: ny 
    integer, intent(in)         :: nsoil
    integer, intent(in)         :: nsnow
    integer, intent(in)         :: nday(12)
    real, intent(in)            :: dt
    integer, dimension(1:nx,1:ny), intent(in)      :: vegtypxy
    real, dimension(1:nx,1:ny), intent(in)         :: lonxy
    real, dimension(1:nx,1:ny), intent(in)         :: latxy
    real, dimension(1:nx,1:ny), intent(in)         :: snowhxy
    real, dimension(1:nx,1:ny), intent(in)         :: sneqvxy 
    real, dimension(1:nx,1:ny), intent(in)         :: tgxy
    real, dimension(1:nx,1:ny), intent(in)         :: prcpxy
    real, dimension(1:nx,1:ny), intent(in)         :: qinxy
    real, dimension(1:nx,1:ny), intent(in)         :: runsfxy
    real, dimension(1:nx,1:ny), intent(in)         :: runsbxy
    real, dimension(1:nx,1:ny), intent(in)         :: ecanxy
    real, dimension(1:nx,1:ny), intent(in)         :: edirxy
    real, dimension(1:nx,1:ny), intent(in)         :: etranxy
    real, dimension(1:nx,1:ny), intent(in)         :: zwtxy
    real, dimension(1:nx,1:ny), intent(in)         :: fsaxy
    real, dimension(1:nx,1:ny), intent(in)         :: firaxy
    real, dimension(1:nx,1:ny), intent(in)         :: fshxy
    real, dimension(1:nx,1:ny), intent(in)         :: flhxy
    real, dimension(1:nx,1:ny), intent(in)         :: fghxy
    real, dimension(1:nx,1:ny), intent(in)         :: aparxy
    real, dimension(1:nx,1:ny), intent(in)         :: psnxy
    real, dimension(1:nx,1:ny), intent(in)         :: savxy
    real, dimension(1:nx,1:ny), intent(in)         :: sagxy
    real, dimension(1:nx,1:ny), intent(in)         :: scfxy
    real, dimension(1:nx,1:ny), intent(in)         :: fsnogxy
!    real, dimension(1:nx,1:ny), intent(in)         :: rsxy
!    real, dimension(1:nx,1:ny), intent(in)         :: rsurxy
    real, dimension(1:nx,-nsnow+1:nsoil,1:ny), intent(in) :: stcxy
    real, dimension(1:nx,       1:nsoil,1:ny), intent(in) :: smcxy
    real, dimension(1:nx,       1:nsoil,1:ny), intent(in) :: sh2oxy
    real, dimension(1:nx,1:ny), intent(in)         :: xlaixy
    real, dimension(1:nx,1:ny), intent(in)         :: xsaixy
    real, dimension(1:nx,1:ny), intent(in)         :: tradxy
    real, dimension(1:nx,1:ny), intent(in)         :: neexy
    real, dimension(1:nx,1:ny), intent(in)         :: gppxy
    real, dimension(1:nx,1:ny), intent(in)         :: nppxy
    real, dimension(1:nx,1:ny), intent(in)         :: t2mxy
    real, dimension(1:nx,1:ny), intent(in)         :: fvegxy
    real, dimension(1:nx,1:ny), intent(in)         :: cmxy
    real, dimension(1:nx,1:ny), intent(in)         :: chxy
    real, dimension(1:nx,1:ny), intent(in)         :: qco2xy
    real, dimension(1:nx,1:ny), intent(in)         :: socxy
    real, dimension(1:nx,1:ny), intent(in)         :: wdocxy
    real, dimension(1:nx,1:ny), intent(in)         :: ddocxy
    real, dimension(1:nx,1:ny), intent(in)         :: micxy
    real, dimension(1:nx,1:ny), intent(in)         :: wenzxy
    real, dimension(1:nx,1:ny), intent(in)         :: denzxy
    real, dimension(1:nx,1:ny), intent(in)         :: vmaxxy
    real, dimension(1:nx,1:ny), intent(in)         :: kmxy
    real, dimension(1:nx,1:ny), intent(in)         :: vmaxupxy
    real, dimension(1:nx,1:ny), intent(in)         :: kmupxy
    real, dimension(1:nx,1:ny), intent(in)         :: epslonxy
    real, dimension(1:nx,1:ny), intent(in)         :: waxy
    real, dimension(1:nx,1:ny), intent(in)         :: canliqxy
    real, dimension(1:nx,1:ny), intent(in)         :: canicexy
    real, dimension(1:nx,1:ny), intent(in)         :: ndvixy
    real, dimension(1:nx,1:ny), intent(in)         :: salbxy
    real, dimension(1:nx,1:ny), intent(in)         :: mqxy
    real, dimension(1:nx,1:ny), intent(in)         :: krxy
    real, dimension(1:nx,1:ny), intent(in)         :: rtmassxy
!   real, dimension(1:nx,1:ny,1:nsoil), intent(in) :: rootmsxy
    real, dimension(1:nx,1:nsoil,1:ny), intent(in) :: sadrxy
    real, dimension(1:nx,1:nsoil,1:ny), intent(in) :: psixy
    real, dimension(1:nx,1:nsoil,1:ny), intent(in) :: qrootxy
    real, dimension(1:nx,1:ny), intent(in)         :: sfctmpxy
    real, dimension(1:nx,1:ny), intent(in)         :: q2xy
    real, dimension(1:nx,1:ny), intent(in)         :: uuxy
    real, dimension(1:nx,1:ny), intent(in)         :: vvxy
    real, dimension(1:nx,1:ny), intent(in)         :: sfcprsxy
    real, dimension(1:nx,1:ny), intent(in)         :: HTOPXY
    real, dimension(1:nx,1:ny), intent(in)         :: RSINEXXY

    integer :: ND
    INTEGER, PARAMETER :: nvar = 65
    integer :: ix,iy,iz
    integer, dimension(1:nsoil)  :: ilev
    real, dimension(1:nx)        :: lon
    real, dimension(1:ny)        :: lat
    real, dimension(nx,ny)       :: snowhm
    real, dimension(nx,ny)       :: sneqvm
    real, dimension(nx,ny)       :: tgm
    real, dimension(nx,ny)       :: prcpm
    real, dimension(nx,ny)       :: qinm
    real, dimension(nx,ny)       :: runsfm
    real, dimension(nx,ny)       :: runsbm
    real, dimension(nx,ny)       :: ecanm
    real, dimension(nx,ny)       :: edirm
    real, dimension(nx,ny)       :: etranm
    real, dimension(nx,ny)       :: zwtm
    real, dimension(nx,ny)       :: fsam
    real, dimension(nx,ny)       :: firam
    real, dimension(nx,ny)       :: fshm
    real, dimension(nx,ny)       :: flhm
    real, dimension(nx,ny)       :: fghm
    real, dimension(nx,ny)       :: aparm
    real, dimension(nx,ny)       :: psnm
    real, dimension(nx,ny)       :: savm
    real, dimension(nx,ny)       :: sagm
    real, dimension(nx,ny)       :: scfm
    real, dimension(nx,ny)       :: fsnogm
!    real, dimension(nx,ny)       :: rsm
!    real, dimension(nx,ny)       :: rsurm
    real, dimension(nx,ny)       :: xlaim
    real, dimension(nx,ny)       :: xsaim
    real, dimension(nx,ny)       :: tradm
    real, dimension(nx,ny)       :: neem
    real, dimension(nx,ny)       :: gppm
    real, dimension(nx,ny)       :: nppm
    real, dimension(nx,ny)       :: t2mm
    real, dimension(nx,ny)       :: fvegm
    real, dimension(nx,ny)       :: cmm
    real, dimension(nx,ny)       :: chm

    real, dimension(nx,ny)       :: qco2m
    real, dimension(nx,ny)       :: socm
    real, dimension(nx,ny)       :: wdocm
    real, dimension(nx,ny)       :: ddocm
    real, dimension(nx,ny)       :: micm
    real, dimension(nx,ny)       :: wenzm
    real, dimension(nx,ny)       :: denzm
    real, dimension(nx,ny)       :: vmaxm
    real, dimension(nx,ny)       :: kmm
    real, dimension(nx,ny)       :: vmaxupm
    real, dimension(nx,ny)       :: kmupm
    real, dimension(nx,ny)       :: epslonm
    real, dimension(nx,ny)       :: wam
    real, dimension(nx,ny)       :: canwatm
    real, dimension(nx,ny)       :: ndvim
    real, dimension(nx,ny)       :: salbm
    real, dimension(nx,ny)       :: mqm
    real, dimension(nx,ny)       :: krm
    real, dimension(nx,ny)       :: rtmassm
    real, dimension(nx,ny)       :: sfctmpm
    real, dimension(nx,ny)       :: q2m
    real, dimension(nx,ny)       :: windm
    real, dimension(nx,ny)       :: sfcprsm
    integer, dimension(nx,ny)    :: NSUN
!   integer, dimension(nx,ny)    :: NRS

    real, dimension(nx,nsoil,ny) :: stcm
    real, dimension(nx,nsoil,ny) :: smcm
    real, dimension(nx,nsoil,ny) :: sh2om
    real, dimension(nx,nsoil,ny) :: psim     !(m)
    real, dimension(nx,nsoil,ny) :: sadrm
    real, dimension(nx,nsoil,ny) :: qrootm   !root water uptake (in m/s)
    real, dimension(nx,nsoil,ny) :: qrootnm  !water redistribution by roots  
                                             !(hydraulic lift or descent: negative qroot in m/s)
    real, dimension(nx,ny)       :: HTOPM
    real, dimension(nx,ny)       :: RSINEXM

    real ::  miss
    character(len=256) :: ncfile
    character(len=120) :: vname
    character(len=120) :: vunit

    INCLUDE 'netcdf.inc'

    INTEGER STATUS
    INTEGER NCID
    INTEGER varID(nvar),lonID,latID,levID
    INTEGER VARDIMS(2),VARDIMS3d(3),XDIM,YDIM,ZDIM,ZDIMr
    INTEGER START3d(3), COUNT3d(3)
    INTEGER START(2), COUNT(2)
    DATA START   /  1,   1/
    DATA START3d /  1,   1,   1/
    DATA miss/-999.9/


     count(1)    = nx
     count(2)    = ny

     count3d(1)  = nx
     count3d(2)  = nsoil
     count3d(3)  = ny

! compute monthly mean

     if(imstep == 1) then

        ND    = 0
        !$OMP PARALLEL DO
        do iy = 1,ny
        do ix = 1,nx
        if(vegtypxy(ix,iy) > 0) then
           snowhm(ix,iy) = 0.
           sneqvm(ix,iy) = 0.
           tgm   (ix,iy) = 0.
           prcpm (ix,iy) = 0.
           qinm  (ix,iy) = 0.
           runsfm(ix,iy) = 0.
           runsbm(ix,iy) = 0.
           ecanm (ix,iy) = 0.
           edirm (ix,iy) = 0.
           etranm(ix,iy) = 0.
           zwtm  (ix,iy) = 0.
           fsam  (ix,iy) = 0.
           firam (ix,iy) = 0.
           fshm  (ix,iy) = 0.
           flhm  (ix,iy) = 0.
           fghm  (ix,iy) = 0.
           aparm (ix,iy) = 0.
           psnm  (ix,iy) = 0.
           savm  (ix,iy) = 0.
           sagm  (ix,iy) = 0.
           scfm  (ix,iy) = 0.
           fsnogm(ix,iy) = 0.
!           rsm   (ix,iy) = 0.
!           rsurm (ix,iy) = 0.
           xlaim (ix,iy) = 0.
           xsaim (ix,iy) = 0.
           tradm (ix,iy) = 0.
           t2mm  (ix,iy) = 0.
           neem  (ix,iy) = 0.
           gppm  (ix,iy) = 0.
           nppm  (ix,iy) = 0.
           fvegm (ix,iy) = 0.
           chm   (ix,iy) = 0.
           cmm   (ix,iy) = 0.

           qco2m (ix,iy) = 0.
           socm  (ix,iy) = 0.
           wdocm (ix,iy) = 0.
           ddocm (ix,iy) = 0.
           micm  (ix,iy) = 0.
           wenzm (ix,iy) = 0.
           denzm (ix,iy) = 0.
           vmaxm (ix,iy) = 0.
           kmm   (ix,iy) = 0.
           vmaxupm (ix,iy) = 0.
           kmupm   (ix,iy) = 0.
           epslonm (ix,iy) = 0.
           wam     (ix,iy) = 0.
           canwatm (ix,iy) = 0.
           ndvim   (ix,iy) = 0.
           salbm   (ix,iy) = 0.
           mqm     (ix,iy) = 0.
           krm     (ix,iy) = 0.
           rtmassm (ix,iy) = 0.
           sfctmpm (ix,iy) = 0.
           q2m     (ix,iy) = 0.
           windm   (ix,iy) = 0.
           sfcprsm (ix,iy) = 0.
           HTOPM   (ix,iy) = 0.
           RSINEXM (ix,iy) = 0.

           NSUN   (ix,iy) = 0
!          NRS    (ix,iy) = 0

           do iz = 1, nsoil
              stcm  (ix,iz,iy)  = 0.
              smcm  (ix,iz,iy)  = 0.
              sh2om (ix,iz,iy)  = 0.
              psim  (ix,iz,iy)  = 0.
           end do

           nroot = INT(NROOT_TABLE(VEGTYPXY(ix,iy)))
           do iz = 1, nroot
              sadrm  (ix,iz,iy) = 0.
              qrootm (ix,iz,iy) = 0.
              qrootnm(ix,iz,iy) = 0.
           end do
        end if
        end do
        end do
        !$OMP END PARALLEL DO
     end if

     ND = ND + 1
     !$OMP PARALLEL DO
     do iy = 1,ny
     do ix = 1,nx
     if(vegtypxy(ix,iy) > 0) then
        snowhm(ix,iy) = snowhm(ix,iy) + snowhxy(ix,iy)
        sneqvm(ix,iy) = sneqvm(ix,iy) + sneqvxy(ix,iy)
        tgm   (ix,iy) = tgm   (ix,iy) + tgxy   (ix,iy)
        prcpm (ix,iy) = prcpm (ix,iy) + prcpxy (ix,iy)
        qinm  (ix,iy) = qinm(ix,iy)   + qinxy  (ix,iy)
        runsfm(ix,iy) = runsfm(ix,iy) + runsfxy(ix,iy)
        runsbm(ix,iy) = runsbm(ix,iy) + runsbxy(ix,iy)
        ecanm (ix,iy) = ecanm (ix,iy) + ecanxy (ix,iy)
        edirm (ix,iy) = edirm (ix,iy) + edirxy (ix,iy)
        etranm(ix,iy) = etranm(ix,iy) + etranxy(ix,iy)
        zwtm  (ix,iy) = zwtm  (ix,iy) + zwtxy  (ix,iy)
        fsam  (ix,iy) = fsam  (ix,iy) + fsaxy  (ix,iy)
        firam (ix,iy) = firam (ix,iy) + firaxy (ix,iy)
        fshm  (ix,iy) = fshm  (ix,iy) + fshxy  (ix,iy)
        flhm  (ix,iy) = flhm  (ix,iy) + flhxy  (ix,iy)
        fghm  (ix,iy) = fghm  (ix,iy) + fghxy  (ix,iy)
        aparm (ix,iy) = aparm (ix,iy) + aparxy (ix,iy)
        psnm  (ix,iy) = psnm  (ix,iy) + psnxy  (ix,iy)
        savm  (ix,iy) = savm  (ix,iy) + savxy  (ix,iy)
        sagm  (ix,iy) = sagm  (ix,iy) + sagxy  (ix,iy)
        scfm  (ix,iy) = scfm  (ix,iy) + scfxy  (ix,iy)
        fsnogm(ix,iy) = fsnogm(ix,iy) + fsnogxy(ix,iy)
!        rsurm (ix,iy) = rsurm (ix,iy) + rsurxy (ix,iy)
        xlaim (ix,iy) = xlaim (ix,iy) + xlaixy (ix,iy)
        xsaim (ix,iy) = xsaim (ix,iy) + xsaixy (ix,iy)
        tradm (ix,iy) = tradm (ix,iy) + tradxy (ix,iy)
        t2mm  (ix,iy) = t2mm  (ix,iy) + t2mxy  (ix,iy)
        neem  (ix,iy) = neem  (ix,iy) + neexy  (ix,iy)
        gppm  (ix,iy) = gppm  (ix,iy) + gppxy  (ix,iy)
        nppm  (ix,iy) = nppm  (ix,iy) + nppxy  (ix,iy)
        fvegm (ix,iy) = fvegm (ix,iy) + fvegxy (ix,iy)
        cmm   (ix,iy) = cmm   (ix,iy) + cmxy   (ix,iy)
        chm   (ix,iy) = chm   (ix,iy) + chxy   (ix,iy)
        qco2m (ix,iy) = qco2m (ix,iy) + qco2xy (ix,iy)
        socm  (ix,iy) = socm  (ix,iy) + socxy  (ix,iy)
        wdocm (ix,iy) = wdocm (ix,iy) + wdocxy (ix,iy)
        ddocm (ix,iy) = ddocm (ix,iy) + ddocxy (ix,iy)
        micm  (ix,iy) = micm  (ix,iy) + micxy  (ix,iy)
        wenzm (ix,iy) = wenzm (ix,iy) + wenzxy (ix,iy)
        denzm (ix,iy) = denzm (ix,iy) + denzxy (ix,iy)
        vmaxm (ix,iy) = vmaxm (ix,iy) + vmaxxy (ix,iy)
        kmm   (ix,iy) = kmm (ix,iy)   + kmxy (ix,iy)
        vmaxupm (ix,iy) = vmaxupm (ix,iy) + vmaxupxy (ix,iy)
        kmupm   (ix,iy) = kmupm   (ix,iy) + kmupxy   (ix,iy)
        epslonm (ix,iy) = epslonm (ix,iy) + epslonxy (ix,iy)
        wam     (ix,iy) = wam     (ix,iy) + waxy     (ix,iy)
        canwatm (ix,iy) = canwatm (ix,iy) + canliqxy (ix,iy) &
                        + canicexy(ix,iy)
        mqm     (ix,iy) = mqm     (ix,iy) + mqxy     (ix,iy)
        krm     (ix,iy) = krm     (ix,iy) + krxy     (ix,iy)
        sfctmpm (ix,iy) = sfctmpm (ix,iy) + sfctmpxy (ix,iy)
        q2m     (ix,iy) = q2m     (ix,iy) + q2xy     (ix,iy)
        windm   (ix,iy) = windm   (ix,iy) + sqrt(uuxy(ix,iy)**2+vvxy(ix,iy)**2)
        sfcprsm (ix,iy) = sfcprsm (ix,iy) + sfcprsxy (ix,iy)
        rtmassm (ix,iy) = rtmassm (ix,iy) + rtmassxy (ix,iy)
        HTOPM   (ix,iy) = HTOPM   (ix,iy) + MAX(0.,HTOPXY   (ix,iy))
        RSINEXM (ix,iy) = RSINEXM (ix,iy) + RSINEXXY (ix,iy)

        do iz = 1, nsoil
        stcm    (ix,iz,iy) = stcm    (ix,iz,iy)  + stcxy    (ix,iz,iy)
        smcm    (ix,iz,iy) = smcm    (ix,iz,iy)  + smcxy    (ix,iz,iy)
        sh2om   (ix,iz,iy) = sh2om   (ix,iz,iy)  + sh2oxy   (ix,iz,iy)
        psim    (ix,iz,iy) = psim    (ix,iz,iy)  + psixy    (ix,iz,iy)
        end do

        nroot = INT(NROOT_TABLE(VEGTYPXY(ix,iy)))
        do iz = 1, nroot
        sadrm   (ix,iz,iy) = sadrm   (ix,iz,iy)  + sadrxy   (ix,iz,iy)
        qrootm  (ix,iz,iy) = qrootm  (ix,iz,iy)  + qrootxy  (ix,iz,iy)
        qrootnm (ix,iz,iy) = qrootnm (ix,iz,iy)  + min(0.0,qrootxy(ix,iz,iy))
        end do

     end if
     end do
     end do
     !$OMP END PARALLEL DO

     do iy = 1,ny
     do ix = 1,nx
     if(vegtypxy(ix,iy) > 0) then
        if(ndvixy(ix,iy) > 0.) then
            NSUN(ix,iy)     = NSUN(ix,iy) + 1
            ndvim   (ix,iy) = ndvim(ix,iy) + ndvixy (ix,iy)
            salbm   (ix,iy) = salbm(ix,iy) + salbxy (ix,iy)
        end if
!        if(rsxy(ix,iy) > 0.) then
!            NRS(ix,iy)     = NRS(ix,iy) + 1
!             rsm   (ix,iy) = rsm  (ix,iy) + rsxy   (ix,iy)
!        end if
     end if
     end do
     end do

     !write(*,*) 'ND=',ND,'NDVIxy=',NDVIxy(220,110),'NSUN=',NSUN(220,110)


     if(ND == nday(imonth)*(86400./dt)) then
        do ix = 1,nx
          lon(ix) = lonxy(ix,1)
        end do
 
        do iy = 1, ny
          lat(iy) = latxy(1,iy)
        end do

        do iz = 1, nsoil
          ilev(iz) = iz
        end do

        !$OMP PARALLEL DO
        do iy = 1,ny
        do ix = 1,nx
        if(vegtypxy(ix,iy) > 0) then
          snowhm(ix,iy) = snowhm(ix,iy) / ND
          sneqvm(ix,iy) = sneqvm(ix,iy) / ND
          tgm   (ix,iy) = tgm   (ix,iy) / ND
          prcpm (ix,iy) = prcpm (ix,iy) / ND
          qinm(ix,iy)   = qinm(ix,iy)   / ND
          runsfm(ix,iy) = runsfm(ix,iy) / ND
          runsbm(ix,iy) = runsbm(ix,iy) / ND
          ecanm (ix,iy) = ecanm (ix,iy) / ND
          edirm (ix,iy) = edirm (ix,iy) / ND
          etranm(ix,iy) = etranm(ix,iy) / ND
          zwtm  (ix,iy) = zwtm  (ix,iy) / ND
          fsam  (ix,iy) = fsam  (ix,iy) / ND
          firam (ix,iy) = firam (ix,iy) / ND
          fshm  (ix,iy) = fshm  (ix,iy) / ND
          flhm  (ix,iy) = flhm  (ix,iy) / ND
          fghm  (ix,iy) = fghm  (ix,iy) / ND
          aparm (ix,iy) = aparm (ix,iy) / ND
          psnm  (ix,iy) = psnm  (ix,iy) / ND
          savm  (ix,iy) = savm  (ix,iy) / ND
          sagm  (ix,iy) = sagm  (ix,iy) / ND
          scfm  (ix,iy) = scfm  (ix,iy) / ND
          fsnogm(ix,iy) = fsnogm(ix,iy) / ND
!          rsurm (ix,iy) = rsurm (ix,iy) / ND
          xlaim (ix,iy) = xlaim (ix,iy) / ND
          xsaim (ix,iy) = xsaim (ix,iy) / ND
          tradm (ix,iy) = tradm (ix,iy) / ND
          t2mm  (ix,iy) = t2mm  (ix,iy) / ND
          neem  (ix,iy) = neem  (ix,iy) / ND
          gppm  (ix,iy) = gppm  (ix,iy) / ND
          nppm  (ix,iy) = nppm  (ix,iy) / ND
          fvegm (ix,iy) = fvegm (ix,iy) / ND
          cmm   (ix,iy) = cmm   (ix,iy) / ND
          chm   (ix,iy) = chm   (ix,iy) / ND
          qco2m (ix,iy) = qco2m (ix,iy) / ND
          socm  (ix,iy) = socm  (ix,iy) / ND
          wdocm (ix,iy) = wdocm (ix,iy) / ND
          ddocm (ix,iy) = ddocm (ix,iy) / ND
          micm  (ix,iy) = micm  (ix,iy) / ND
          wenzm (ix,iy) = wenzm (ix,iy) / ND
          denzm (ix,iy) = denzm (ix,iy) / ND
          vmaxm (ix,iy) = vmaxm (ix,iy) / ND
          kmm   (ix,iy) = kmm (ix,iy)   / ND
          vmaxupm (ix,iy) = vmaxupm (ix,iy) / ND
          kmupm   (ix,iy) = kmupm   (ix,iy) / ND
          epslonm (ix,iy) = epslonm (ix,iy) / ND
          wam     (ix,iy) = wam     (ix,iy) / ND
          canwatm (ix,iy) = canwatm (ix,iy) / ND
          mqm     (ix,iy) = mqm     (ix,iy) / ND
          krm     (ix,iy) = krm     (ix,iy) / ND
          rtmassm (ix,iy) = rtmassm (ix,iy) / ND
          sfctmpm (ix,iy) = sfctmpm (ix,iy) / ND
          q2m     (ix,iy) = q2m     (ix,iy) / ND
          windm   (ix,iy) = windm   (ix,iy) / ND
          sfcprsm (ix,iy) = sfcprsm (ix,iy) / ND
          HTOPM   (ix,iy) = HTOPM   (ix,iy) / ND
          RSINEXM (ix,iy) = RSINEXM (ix,iy) / ND

          if(NSUN(ix,iy) > 0) then
              ndvim   (ix,iy) = ndvim (ix,iy) / NSUN(ix,iy)
              salbm   (ix,iy) = salbm (ix,iy) / NSUN(ix,iy)
          end if
!          if(NRS(ix,iy) > 0) then
!              rsm     (ix,iy) = rsm   (ix,iy) / NRS(ix,iy)
!          end if

          do iz = 1, nsoil
            stcm    (ix,iz,iy) = stcm   (ix,iz,iy) / ND
            smcm    (ix,iz,iy) = smcm   (ix,iz,iy) / ND
            sh2om   (ix,iz,iy) = sh2om  (ix,iz,iy) / ND
            psim    (ix,iz,iy) = psim   (ix,iz,iy) / ND  !m/s
          end do

          nroot = INT(NROOT_TABLE(VEGTYPXY(ix,iy)))
          do iz = 1,nroot
            sadrm   (ix,iz,iy) = sadrm  (ix,iz,iy) / ND
            qrootm  (ix,iz,iy) = qrootm (ix,iz,iy) / ND !m/s
            qrootnm (ix,iz,iy) = qrootnm(ix,iz,iy) / ND !m/s
          end do
        end if
        end do 
        end do
        !$OMP END PARALLEL DO

        ! creat nc output files and initialize output file ID

        if(imonth < 10) then
              write(*,*) TRIM(EXP),iyear,imonth
              write(ncfile,100) TRIM(EXP),iyear,imonth
        else
              write(*,*) TRIM(EXP),iyear,imonth
              write(ncfile,200) TRIM(EXP),iyear,imonth
        end if

  100   format('/results/',a,'/hist/Noah.monthlymean.',i4,'0',i1,'.nc')
  200   format('/results/',a,'/hist/Noah.monthlymean.',i4,i2,'.nc')

        write(*,*) '----------------------------------------------------------------------------'
        write(*,*) 'opening ncfile: ',TRIM(DIR)//TRIM(ncfile)
        write(*,*) '----------------------------------------------------------------------------'

        ncfile = TRIM(DIR)//ncfile

        STATUS=NF_CREATE(ncfile,NF_CLOBBER, NCID)
        STATUS=NF_DEF_DIM(NCID, 'nx'  ,nx      , XDIM)
        STATUS=NF_DEF_DIM(NCID, 'ny'  ,ny      , YDIM)
        STATUS=NF_DEF_DIM(NCID, 'nz'  ,nsoil   , ZDIM)

        VARDIMS(1) = XDIM
        VARDIMS(2) = YDIM

        VARDIMS3d(1) = XDIM
        VARDIMS3d(2) = ZDIM
        VARDIMS3d(3) = YDIM

        STATUS = nf_def_var(NCID,'lon',nf_float,1,XDIM,lonID)
        STATUS = nf_def_var(NCID,'lat',nf_float,1,YDIM,latID)
        STATUS = nf_def_var(NCID,'lev',nf_float,1,ZDIM,levID)

     ! initilize output variables ID

        STATUS = nf_def_var(NCID,'SNOWD',NF_FLOAT,2,VARDIMS,varID(1))
        vname = 'snow depth'
        vunit = 'm'
        STATUS = nf_put_att_text(NCID,varID(1),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(1),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(1),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SWE',NF_FLOAT,2,VARDIMS,varID(2))
        vname = 'snow water equivalent'
        vunit = 'kg/m2'
        STATUS = nf_put_att_text(NCID,varID(2),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(2),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(2),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'TG',NF_FLOAT,2,VARDIMS,varID(3))
        vname = 'ground surface temperature'
        vunit = 'K'
        STATUS = nf_put_att_text(NCID,varID(3),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(3),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(3),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'PRCP',NF_FLOAT,2,VARDIMS,varID(4))
        vname = 'precipitation'
        vunit = 'mm/s'
        STATUS = nf_put_att_text(NCID,varID(4),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(4),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(4),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'RUNSF',NF_FLOAT,2,VARDIMS,varID(5))
        vname = 'surface runoff'
        vunit = 'mm/s'
        STATUS = nf_put_att_text(NCID,varID(5),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(5),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(5),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'RUNSB',NF_FLOAT,2,VARDIMS,varID(6))
        vname = 'subsurface runoff'
        vunit = 'mm/s'
        STATUS = nf_put_att_text(NCID,varID(6),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(6),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(6),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'ECAN',NF_FLOAT,2,VARDIMS,varID(7))
        vname = 'canopy interception loss'
        vunit = 'mm/s'
        STATUS = nf_put_att_text(NCID,varID(7),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(7),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(7),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'ESOIL',NF_FLOAT,2,VARDIMS,varID(8))
        vname = 'soil surface evaporation'
        vunit = 'mm/s'
        STATUS = nf_put_att_text(NCID,varID(8),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(8),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(8),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'ETRAN',NF_FLOAT,2,VARDIMS,varID(9))
        vname = 'transpiration'
        vunit = 'mm/s'
        STATUS = nf_put_att_text(NCID,varID(9),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(9),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(9),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'ZWT',NF_FLOAT,2,VARDIMS,varID(10))
        vname = 'depth to water table'
        vunit = 'm'
        STATUS = nf_put_att_text(NCID,varID(10),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(10),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(10),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'STC',NF_FLOAT,3,VARDIMS3d,varID(11))
        vname = 'soil temperature (4-L)'
        vunit = 'K'
        STATUS = nf_put_att_text(NCID,varID(11),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(11),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(11),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SMC',NF_FLOAT,3,VARDIMS3d,varID(12))
        vname = 'soil moisture content (4-L)'
        vunit = 'm3/m3'
        STATUS = nf_put_att_text(NCID,varID(12),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(12),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(12),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SH2O',NF_FLOAT,3,VARDIMS3d,varID(13))
        vname = 'soil liquid water content (4-L)'
        vunit = 'm3/m3'
        STATUS = nf_put_att_text(NCID,varID(13),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(13),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(13),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'FSA',NF_FLOAT,2,VARDIMS,varID(14))
        vname = 'net solar radiation'
        vunit = 'W/m2'
        STATUS = nf_put_att_text(NCID,varID(14),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(14),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(14),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'FIRA',NF_FLOAT,2,VARDIMS,varID(15))
        vname = 'net longwave radiation'
        vunit = 'W/m2'
        STATUS = nf_put_att_text(NCID,varID(15),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(15),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(15),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'FSH',NF_FLOAT,2,VARDIMS,varID(16))
        vname = 'sensible heat flux'
        vunit = 'W/m2'
        STATUS = nf_put_att_text(NCID,varID(16),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(16),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(16),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'FLH',NF_FLOAT,2,VARDIMS,varID(17))
        vname = 'latent heat flux'
        vunit = 'W/m2'
        STATUS = nf_put_att_text(NCID,varID(17),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(17),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(17),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'FGH',NF_FLOAT,2,VARDIMS,varID(18))
        vname = 'ground heat flux'
        vunit = 'W/m2'
        STATUS = nf_put_att_text(NCID,varID(18),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(18),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(18),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'APAR',NF_FLOAT,2,VARDIMS,varID(19))
        vname = 'photosyn active radiation'
        vunit = 'W/m2'
        STATUS = nf_put_att_text(NCID,varID(19),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(19),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(19),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'PSN',NF_FLOAT,2,VARDIMS,varID(20))
        vname = 'total photosynthesis'
        vunit = 'umol co2/m2/s'
        STATUS = nf_put_att_text(NCID,varID(20),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(20),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(20),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SAV',NF_FLOAT,2,VARDIMS,varID(21))
        vname = 'solar rad absorbed by veg.`'
        vunit = 'W/m2'
        STATUS = nf_put_att_text(NCID,varID(21),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(21),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(21),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SAG',NF_FLOAT,2,VARDIMS,varID(22))
        vname = 'solar rad absorbed by ground'
        vunit = 'W/m2'
        STATUS = nf_put_att_text(NCID,varID(22),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(22),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(22),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SCF',NF_FLOAT,2,VARDIMS,varID(23))
        vname = 'snow cover fraction of a grid (SCFg*(1-GVF)+SCFc*GVF)'
        vunit = 'fraction'
        STATUS = nf_put_att_text(NCID,varID(23),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(23),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(23),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'LAI',NF_FLOAT,2,VARDIMS,varID(24))
        vname = 'leaf area index'
        vunit = 'm2/m2'
        STATUS = nf_put_att_text(NCID,varID(24),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(24),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(24),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SAI',NF_FLOAT,2,VARDIMS,varID(25))
        vname = 'stem area index'
        vunit = 'm2/m2'
        STATUS = nf_put_att_text(NCID,varID(25),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(25),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(25),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'TRAD',NF_FLOAT,2,VARDIMS,varID(26))
        vname = 'surface radiative temperature'
        vunit = 'K'
        STATUS = nf_put_att_text(NCID,varID(26),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(26),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(26),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'NEE',NF_FLOAT,2,VARDIMS,varID(27))
        vname = 'net CO2 flux'
        vunit = 'g/m2/s CO2'
        STATUS = nf_put_att_text(NCID,varID(27),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(27),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(27),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'GPP',NF_FLOAT,2,VARDIMS,varID(28))
        vname = 'GPP'
        vunit = 'g/m2/s carbon'
        STATUS = nf_put_att_text(NCID,varID(28),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(28),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(28),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'NPP',NF_FLOAT,2,VARDIMS,varID(29))
        vname = 'NPP'
        vunit = 'g/m2/s carbon'
        STATUS = nf_put_att_text(NCID,varID(29),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(29),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(29),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'T2M',NF_FLOAT,2,VARDIMS,varID(30))
        vname = '2-m air temperature'
        vunit = 'K'
        STATUS = nf_put_att_text(NCID,varID(30),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(30),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(30),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'FVEG',NF_FLOAT,2,VARDIMS,varID(31))
        vname = 'greenness vegetation fraction'
        vunit = 'fraction'
        STATUS = nf_put_att_text(NCID,varID(31),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(31),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(31),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'CM',NF_FLOAT,2,VARDIMS,varID(32))
        vname = 'Surface exchange coefficient for momentum'
        vunit = '-'
        STATUS = nf_put_att_text(NCID,varID(32),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(32),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(32),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'CH',NF_FLOAT,2,VARDIMS,varID(33))
        vname = 'Surface exchange coefficient for heat'
        vunit = '-'
        STATUS = nf_put_att_text(NCID,varID(33),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(33),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(33),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'QCO2',NF_FLOAT,2,VARDIMS,varID(34))
        vname = 'CO2 efflux at soil surface'
        vunit = 'g/m2/s'
        STATUS = nf_put_att_text(NCID,varID(34),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(34),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(34),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SOC',NF_FLOAT,2,VARDIMS,varID(35))
        vname = 'soil soc concentration'
        vunit = 'g/m2'
        STATUS = nf_put_att_text(NCID,varID(35),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(35),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(35),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'WDOC',NF_FLOAT,2,VARDIMS,varID(36))
        vname = 'soil doc concentration (wet)'
        vunit = 'g/m2'
        STATUS = nf_put_att_text(NCID,varID(36),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(36),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(36),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'MIC',NF_FLOAT,2,VARDIMS,varID(37))
        vname = 'soil mic concentration'
        vunit = 'g/m2'
        STATUS = nf_put_att_text(NCID,varID(37),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(37),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(37),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'WENZ',NF_FLOAT,2,VARDIMS,varID(38))
        vname = 'soil enzyme concentration (wet)'
        vunit = 'g/m2'
        STATUS = nf_put_att_text(NCID,varID(38),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(38),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(38),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'DDOC',NF_FLOAT,2,VARDIMS,varID(39))
        vname = 'soil doc concentration (dry)'
        vunit = 'g/m2'
        STATUS = nf_put_att_text(NCID,varID(39),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(39),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(39),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'DENZ',NF_FLOAT,2,VARDIMS,varID(40))
        vname = 'soil enzyme concentration (dry)'
        vunit = 'g/m2'
        STATUS = nf_put_att_text(NCID,varID(40),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(40),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(40),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'VMAX',NF_FLOAT,2,VARDIMS,varID(41))
!        vname = 'max SOC decomposition rate per unit microbial biomass'
!        vunit = 'g SOC/m2/s/[g MIC/m2]'
!        STATUS = nf_put_att_text(NCID,varID(41),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(41),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(41),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'KM',NF_FLOAT,2,VARDIMS,varID(42))
!        vname = 'Michaelis-Menten constant for SOC decomp'
!        vunit = 'g/m2'
!        STATUS = nf_put_att_text(NCID,varID(42),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(42),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(42),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'VMAXUP',NF_FLOAT,2,VARDIMS,varID(43))
!        vname = 'max DOC uptake rate per unit microbial mass'
!        vunit = 'g DOC/m2/s/[g MIC/m2]'
!        STATUS = nf_put_att_text(NCID,varID(43),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(43),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(43),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'KMUP',NF_FLOAT,2,VARDIMS,varID(44))
!        vname = 'Michaelis-Menten constant for DOC uptake'
!        vunit = 'g/m2'
!        STATUS = nf_put_att_text(NCID,varID(44),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(44),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(44),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'CUE',NF_FLOAT,2,VARDIMS,varID(45))
!        vname = 'carbon use efficiency'
!        vunit = '-'
!        STATUS = nf_put_att_text(NCID,varID(45),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(45),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(45),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'QIN',NF_FLOAT,2,VARDIMS,varID(46))
        vname = 'groundwater recharge'
        vunit = 'mm/s'
        STATUS = nf_put_att_text(NCID,varID(46),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(46),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(46),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'WAQ',NF_FLOAT,2,VARDIMS,varID(47))
        vname = 'groundwater storage'
        vunit = 'mm'
        STATUS = nf_put_att_text(NCID,varID(47),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(47),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(47),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'CANWAT',NF_FLOAT,2,VARDIMS,varID(48))
        vname = 'canopy water storage (ice+liq)'
        vunit = 'mm'
        STATUS = nf_put_att_text(NCID,varID(48),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(48),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(48),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'NDVI',NF_FLOAT,2,VARDIMS,varID(49))
        vname = 'NDVI (ANIR-AVIS)/(ANIR+AVIS)'
        vunit = '-'
        STATUS = nf_put_att_text(NCID,varID(49),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(49),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(49),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'MQ',NF_FLOAT,2,VARDIMS,varID(50))
        vname = 'plant water storage or water in living plant tissues'
        vunit = 'kg/m2 or mm'
        STATUS = nf_put_att_text(NCID,varID(50),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(50),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(50),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'KR',NF_FLOAT,2,VARDIMS,varID(51))
        vname = 'water storage deficit of plants or beta factor'
        vunit = '-'
        STATUS = nf_put_att_text(NCID,varID(51),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(51),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(51),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'ROOTM',NF_FLOAT,2,VARDIMS,varID(52))
        vname = 'total root biomass (active + non-active)'
        vunit = 'g/m2'
        STATUS = nf_put_att_text(NCID,varID(52),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(52),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(52),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SADR',NF_FLOAT,3,VARDIMS3d,varID(53))
        vname = 'root area density (4-L) or RAI (m2/m2) / depth (m)'
        vunit = 'm2/m3'
        STATUS = nf_put_att_text(NCID,varID(53),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(53),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(53),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'QROOT',NF_FLOAT,3,VARDIMS3d,varID(54))
        vname = 'water flux from layers of roots'
        vunit = 'm/s'
        STATUS = nf_put_att_text(NCID,varID(54),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(54),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(54),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'QROOTN',NF_FLOAT,3,VARDIMS3d,varID(55))
        vname = 'water redistribution by roots: hydraulic lift or descent (- part of QROOT)'
        vunit = 'm/s'
        STATUS = nf_put_att_text(NCID,varID(55),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(55),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(55),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'TAIR',NF_FLOAT,2,VARDIMS,varID(56))
        vname = '2-m air temperature'
        vunit = 'K'
        STATUS = nf_put_att_text(NCID,varID(56),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(56),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(56),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'QAIR',NF_FLOAT,2,VARDIMS,varID(57))
        vname = '2-m air specific humidity'
        vunit = 'kg/kg'
        STATUS = nf_put_att_text(NCID,varID(57),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(57),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(57),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'WIND',NF_FLOAT,2,VARDIMS,varID(58))
        vname = '10-m wind'
        vunit = 'm/s'
        STATUS = nf_put_att_text(NCID,varID(58),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(58),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(58),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'PAIR',NF_FLOAT,2,VARDIMS,varID(59))
        vname = 'Surface air pressure'
        vunit = 'Pa'
        STATUS = nf_put_att_text(NCID,varID(59),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(59),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(59),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'PSI',NF_FLOAT,3,VARDIMS3d,varID(60))
        vname = 'soil water potential'
        vunit = 'm'
        STATUS = nf_put_att_text(NCID,varID(60),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(60),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(60),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SCFG',NF_FLOAT,2,VARDIMS,varID(61))
        vname = 'snow cover fraction on the ground'
        vunit = 'fraction'
        STATUS = nf_put_att_text(NCID,varID(61),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(61),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(61),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SALB',NF_FLOAT,2,VARDIMS,varID(62))
        vname = 'Surface Albedo'
        vunit = '-'
        STATUS = nf_put_att_text(NCID,varID(62),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(62),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(62),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'HTOP',NF_FLOAT,2,VARDIMS,varID(63))
        vname = 'surface ponding head (depth)'
        vunit = 'm'
        STATUS = nf_put_att_text(NCID,varID(63),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(63),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(63),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'RSINEX',NF_FLOAT,2,VARDIMS,varID(64))
        vname = 'infiltration excess runoff '
        vunit = 'm/s'
        STATUS = nf_put_att_text(NCID,varID(64),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(64),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(64),'missing_value',NF_FLOAT,1,miss)

        STATUS = NF_ENDDEF(NCID)
        status= NF_CLOSE(NCID)

        ! NC format output

        STATUS = NF_OPEN (ncfile, NF_WRITE, NCID)
        status = nf_put_vara_real(NCID,lonID,1,nx,lon)
        status = nf_put_vara_real(NCID,latID,1,ny,lat)
        status = nf_put_vara_int (NCID,levID,1,nsoil,ilev)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 1),START,COUNT,snowhm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 2),START,COUNT,sneqvm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 3),START,COUNT,tgm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 4),START,COUNT,prcpm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 5),START,COUNT,runsfm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 6),START,COUNT,runsbm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 7),START,COUNT,ecanm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 8),START,COUNT,edirm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 9),START,COUNT,etranm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(10),START,COUNT,zwtm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(11),START3d,COUNT3d,stcm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(12),START3d,COUNT3d,smcm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(13),START3d,COUNT3d,sh2om)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(14),START,COUNT,fsam)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(15),START,COUNT,firam)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(16),START,COUNT,fshm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(17),START,COUNT,flhm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(18),START,COUNT,fghm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(19),START,COUNT,aparm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(20),START,COUNT,psnm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(21),START,COUNT,savm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(22),START,COUNT,sagm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(23),START,COUNT,scfm)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(62),START,COUNT,rsm)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(63),START,COUNT,rsurm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(24),START,COUNT,xlaim)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(25),START,COUNT,xsaim)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(26),START,COUNT,tradm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(27),START,COUNT,neem)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(28),START,COUNT,gppm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(29),START,COUNT,nppm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(30),START,COUNT,t2mm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(31),START,COUNT,fvegm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(32),START,COUNT,cmm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(33),START,COUNT,chm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(34),START,COUNT,qco2m)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(35),START,COUNT,socm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(36),START,COUNT,wdocm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(37),START,COUNT,micm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(38),START,COUNT,wenzm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(39),START,COUNT,ddocm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(40),START,COUNT,denzm)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(41),START,COUNT,vmaxm)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(42),START,COUNT,kmm)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(43),START,COUNT,vmaxupm)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(44),START,COUNT,kmupm)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(45),START,COUNT,epslonm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(46),START,COUNT,qinm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(47),START,COUNT,wam)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(48),START,COUNT,canwatm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(49),START,COUNT,ndvim)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(50),START,COUNT,mqm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(51),START,COUNT,krm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(52),START,COUNT,rtmassm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(53),START3d,COUNT3d,sadrm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(54),START3d,COUNT3d,qrootm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(55),START3d,COUNT3d,qrootnm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(56),START,COUNT,sfctmpm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(57),START,COUNT,q2m)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(58),START,COUNT,windm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(59),START,COUNT,sfcprsm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(60),START3d,COUNT3d,psim)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(61),START,COUNT,fsnogm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(62),START,COUNT,salbm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(63),START,COUNT,HTOPM)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(64),START,COUNT,RSINEXM)

        status= NF_CLOSE(NCID)

        ND = 0
        do ix = 1,nx
        do iy = 1,ny
        if(vegtypxy(ix,iy) > 0) then
           snowhm(ix,iy) = 0.
           sneqvm(ix,iy) = 0.
           tgm   (ix,iy) = 0.
           prcpm (ix,iy) = 0.
           qinm(ix,iy)   = 0.
           runsfm(ix,iy) = 0.
           runsbm(ix,iy) = 0.
           ecanm (ix,iy) = 0.
           edirm (ix,iy) = 0.
           etranm(ix,iy) = 0.
           zwtm  (ix,iy) = 0.
           fsam  (ix,iy) = 0.
           firam (ix,iy) = 0.
           fshm  (ix,iy) = 0.
           flhm  (ix,iy) = 0.
           fghm  (ix,iy) = 0.
           aparm (ix,iy) = 0.
           psnm  (ix,iy) = 0.
           savm  (ix,iy) = 0.
           sagm  (ix,iy) = 0.
           scfm  (ix,iy) = 0.
           fsnogm(ix,iy) = 0.
!          rsurm (ix,iy) = 0.
           xlaim (ix,iy) = 0.
           xsaim (ix,iy) = 0.
           tradm (ix,iy) = 0.
           t2mm  (ix,iy) = 0.
           neem  (ix,iy) = 0.
           gppm  (ix,iy) = 0.
           nppm  (ix,iy) = 0.
           fvegm (ix,iy) = 0.
           cmm   (ix,iy) = 0.
           chm   (ix,iy) = 0.
           qco2m (ix,iy) = 0.
           socm  (ix,iy) = 0.
           wdocm (ix,iy) = 0.
           ddocm (ix,iy) = 0.
           micm  (ix,iy) = 0.
           wenzm (ix,iy) = 0.
           denzm (ix,iy) = 0.
           vmaxm (ix,iy) = 0.
           kmm   (ix,iy) = 0.
           vmaxupm (ix,iy) = 0.
           kmupm   (ix,iy) = 0.
           epslonm (ix,iy) = 0.
           wam     (ix,iy) = 0.
           canwatm (ix,iy) = 0.
           mqm     (ix,iy) = 0.
           krm     (ix,iy) = 0.
           rtmassm (ix,iy) = 0.
           sfctmpm (ix,iy) = 0.
           q2m     (ix,iy) = 0.
           windm   (ix,iy) = 0.
           sfcprsm (ix,iy) = 0.
!          rsm     (ix,iy) = 0.
           ndvim   (ix,iy) = 0.
           salbm   (ix,iy) = 0.
           NSUN    (ix,iy) = 0
!          NRS     (ix,iy) = 0
           HTOPM   (ix,iy) = 0.
           RSINEXM (ix,iy) = 0.

           do iz = 1, nsoil
             stcm   (ix,iz,iy) = 0.
             smcm   (ix,iz,iy) = 0.
             sh2om  (ix,iz,iy) = 0.
             psim   (ix,iz,iy) = 0.
           end do
           do iz = 1,nroot
             sadrm  (ix,iz,iy) = 0.
             qrootm (ix,iz,iy) = 0.
             qrootnm(ix,iz,iy) = 0.
           end do
        end if
        end do
        end do
     end if

! -------------------------------------------------------------------
    END SUBROUTINE NC_OUT
! -------------------------------------------------------------------
    SUBROUTINE nc_out_daily(nx    ,ny     ,nsoil ,it    ,dt    , &
                            iyear ,imonth ,iday  ,DIR   , &
                            EXP    ,lonxy ,latxy ,vegtyp, &
                            idstep ,ND    ,  &
                            snowh    ,sneqv    ,scf       ,fsnog    , &
                            runsf    ,runsb    ,qsubcan   ,qsubgrd  , &
                            smc      ,sh2o     , &
                            snowhm   ,sneqvm   ,scfm      ,fsnogm   , &
                            runsfm   ,runsbm   ,qsubcanm  ,qsubgrdm , &
                            smcm     ,sh2om    )

    implicit none

! inputs
    integer, intent(in) :: iyear,imonth,iday
    CHARACTER(len=256),INTENT(IN) :: DIR
    CHARACTER(len=8),INTENT(IN) :: EXP
    integer, intent(in)         :: it
    integer, intent(in)         :: idstep
    integer, intent(in)         :: nx
    integer, intent(in)         :: ny
    integer, intent(in)         :: nsoil
    real, intent(in)            :: dt
    integer, dimension(1:nx,1:ny), intent(in)      :: vegtyp
    real, dimension(1:nx,1:ny), intent(in)         :: lonxy
    real, dimension(1:nx,1:ny), intent(in)         :: latxy
    real, dimension(1:nx,1:ny), intent(in)         :: snowh
    real, dimension(1:nx,1:ny), intent(in)         :: sneqv
    real, dimension(1:nx,1:ny), intent(in)         :: scf
    real, dimension(1:nx,1:ny), intent(in)         :: fsnog
    real, dimension(1:nx,1:ny), intent(in)         :: runsf
    real, dimension(1:nx,1:ny), intent(in)         :: runsb
    real, dimension(1:nx,1:ny), intent(in)         :: qsubcan
    real, dimension(1:nx,1:ny), intent(in)         :: qsubgrd
    real, dimension(1:nx,       1:nsoil,1:ny), intent(in) :: smc
    real, dimension(1:nx,       1:nsoil,1:ny), intent(in) :: sh2o

    integer ,intent(inout) :: ND
    INTEGER, PARAMETER :: nvar = 10
    integer :: ix,iy,iz

    real, dimension(1:nx)        :: lon
    real, dimension(1:ny)        :: lat
    real, dimension(nx,ny)       :: snowhm
    real, dimension(nx,ny)       :: sneqvm
    real, dimension(nx,ny)       :: scfm
    real, dimension(nx,ny)       :: fsnogm
    real, dimension(nx,ny)       :: runsfm
    real, dimension(nx,ny)       :: runsbm
    real, dimension(nx,ny)       :: qsubcanm
    real, dimension(nx,ny)       :: qsubgrdm
    real, dimension(1:nx,       1:nsoil,1:ny) :: smcm
    real, dimension(1:nx,       1:nsoil,1:ny) :: sh2om

    real ::  miss
    character(len=256) :: ncfile
    character(len=120) :: vname
    character(len=120) :: vunit

    INCLUDE 'netcdf.inc'

    INTEGER STATUS
    INTEGER NCID
    INTEGER varID(nvar),lonID,latID,levID
    INTEGER VARDIMS(2),XDIM,YDIM,ZDIM
    INTEGER START(2), COUNT(2)
    INTEGER START3D(3), COUNT3D(3)
    INTEGER VARDIMS3D(3)
    DATA START     /  1,   1/
    DATA START3D   /  1,    1,   1/
    DATA miss/-999.9/

! compute daily mean

     if(idstep == 1) then
        ND    = 0
        !$OMP PARALLEL DO
        do iy = 1,ny
        do ix = 1,nx
        if(vegtyp(ix,iy) > 0) then
           snowhm(ix,iy) = 0.
           sneqvm(ix,iy) = 0.
           scfm  (ix,iy) = 0.
           fsnogm(ix,iy) = 0.
           runsfm(ix,iy) = 0.
           runsbm(ix,iy) = 0.
           qsubcanm(ix,iy) = 0.
           qsubgrdm(ix,iy) = 0.
           do iz = 1, nsoil
             smcm    (ix,iz,iy) = 0.
             sh2om   (ix,iz,iy) = 0.
           end do
        end if
        end do
        end do
        !$OMP END PARALLEL DO
     end if

     ND = ND + 1
     !$OMP PARALLEL DO
     do iy = 1,ny
     do ix = 1,nx
       if(vegtyp(ix,iy) > 0) then
        snowhm(ix,iy) = snowhm(ix,iy) + snowh(ix,iy)
        sneqvm(ix,iy) = sneqvm(ix,iy) + sneqv(ix,iy)
        scfm  (ix,iy) = scfm  (ix,iy) + scf  (ix,iy)
        fsnogm(ix,iy) = fsnogm(ix,iy) + fsnog(ix,iy)
        runsfm(ix,iy) = runsfm(ix,iy) + runsf(ix,iy)
        runsbm(ix,iy) = runsbm(ix,iy) + runsb(ix,iy)
        qsubcanm(ix,iy) = qsubcanm(ix,iy) + qsubcan(ix,iy)
        qsubgrdm(ix,iy) = qsubgrdm(ix,iy) + qsubgrd(ix,iy)
        do iz = 1, nsoil
           smcm    (ix,iz,iy) = smcm    (ix,iz,iy)  + smc    (ix,iz,iy)
           sh2om   (ix,iz,iy) = sh2om   (ix,iz,iy)  + sh2o   (ix,iz,iy)
        end do
       end if
     end do
     end do
     !$OMP END PARALLEL DO

     write(*,*) 'ND=',ND,int(86400./dt)

     if(ND == int(86400./dt)) then

        count(1)   = nx
        count(2)   = ny

        count3d(1)  = nx
        count3d(2)  = nsoil
        count3d(3)  = ny

        !$OMP PARALLEL DO
        do ix = 1,nx
          lon(ix) = lonxy(ix,1)
        end do
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do iy = 1, ny
          lat(iy) = latxy(1,iy)
        end do
        !$OMP END PARALLEL DO

        !$OMP PARALLEL DO
        do iy = 1,ny
        do ix = 1,nx
        if(vegtyp(ix,iy) > 0) then
          snowhm(ix,iy) = snowhm(ix,iy) / ND
          sneqvm(ix,iy) = sneqvm(ix,iy) / ND
          scfm  (ix,iy) = scfm  (ix,iy) / ND
          fsnogm(ix,iy) = fsnogm(ix,iy) / ND
          runsfm(ix,iy) = runsfm(ix,iy) / ND
          runsbm(ix,iy) = runsbm(ix,iy) / ND
          qsubcanm(ix,iy) = qsubcanm(ix,iy) / ND
          qsubgrdm(ix,iy) = qsubgrdm(ix,iy) / ND
          do iz = 1, nsoil
            smcm    (ix,iz,iy) = smcm    (ix,iz,iy) / ND 
            sh2om   (ix,iz,iy) = sh2om   (ix,iz,iy) / ND 
          end do
        end if
        end do
        end do
        !$OMP END PARALLEL DO

        ! creat nc output files and initialize output file ID

        if(imonth < 10) then
           if(iday < 10) then
              write(*,*) TRIM(EXP),iyear,imonth,iday
              write(ncfile,100) TRIM(EXP),iyear,imonth,iday
           else
              write(*,*) TRIM(EXP),iyear,imonth,iday
              write(ncfile,200) TRIM(EXP),iyear,imonth,iday
           end if
        else
           if(iday < 10) then
              write(*,*) TRIM(EXP),iyear,imonth,iday
              write(ncfile,300) TRIM(EXP),iyear,imonth,iday
           else
              write(*,*) TRIM(EXP),iyear,imonth,iday
              write(ncfile,400) TRIM(EXP),iyear,imonth,iday
           end if
        end if
  100   format('/results/',a,'/daily/Noah.dailymean.',i4,'0',i1,'0',i1,'.nc')
  200   format('/results/',a,'/daily/Noah.dailymean.',i4,'0',i1,i2,'.nc')
  300   format('/results/',a,'/daily/Noah.dailymean.',i4,i2,'0',i1,'.nc')
  400   format('/results/',a,'/daily/Noah.dailymean.',i4,i2,i2,'.nc')

        write(*,*) '----------------------------------------------------------------------------'
        write(*,*) 'opening ncfile: ../Noah_data',trim(ncfile)
        write(*,*) '----------------------------------------------------------------------------'
        ncfile = TRIM(DIR)//ncfile

        STATUS=NF_CREATE(ncfile,NF_CLOBBER, NCID)
        STATUS=NF_DEF_DIM(NCID, 'nx'   ,nx      , XDIM)
        STATUS=NF_DEF_DIM(NCID, 'ny'   ,ny      , YDIM)
        STATUS=NF_DEF_DIM(NCID, 'nz'   ,nsoil   , ZDIM)
        VARDIMS(1) = XDIM
        VARDIMS(2) = YDIM
        VARDIMS3d(1) = XDIM
        VARDIMS3d(2) = ZDIM
        VARDIMS3d(3) = YDIM

     ! initilize output variables ID

        STATUS = nf_def_var(NCID,'lon',nf_float,1,XDIM,lonID)
        vname = 'longitude coordinate'
        vunit = 'degrees_east'
        STATUS = nf_put_att_text(NCID,lonID,'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,lonID,'units'    ,lencs(vunit),vunit)

        STATUS = nf_def_var(NCID,'lat',nf_float,1,YDIM,latID)
        vname = 'latitude coordinate'
        vunit = 'degrees_north'
        STATUS = nf_put_att_text(NCID,latID,'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,latID,'units'    ,lencs(vunit),vunit)

!       STATUS = nf_def_var(NCID,'SNOWD',NF_FLOAT,2,VARDIMS,varID(1))
!       vname = 'snow depth'
!       vunit = 'm'
!       STATUS = nf_put_att_text(NCID,varID(1),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(1),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(1),'missing_value',NF_FLOAT,1,miss)

!       STATUS = nf_def_var(NCID,'SWE',NF_FLOAT,2,VARDIMS,varID(2))
!       vname = 'snow water equivalent'
!       vunit = 'kg/m2'
!       STATUS = nf_put_att_text(NCID,varID(2),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(2),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(2),'missing_value',NF_FLOAT,1,miss)

!       STATUS = nf_def_var(NCID,'SCF',NF_FLOAT,2,VARDIMS,varID(3))
!       vname = 'snow cover fraction of a grid cell'
!       vunit = '-'
!       STATUS = nf_put_att_text(NCID,varID(3),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(3),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(3),'missing_value',NF_FLOAT,1,miss)

!       STATUS = nf_def_var(NCID,'SCFG',NF_FLOAT,2,VARDIMS,varID(4))
!       vname = 'snow cover fraction of at ground'
!       vunit = '-'
!       STATUS = nf_put_att_text(NCID,varID(4),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(4),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(4),'missing_value',NF_FLOAT,1,miss)

!       STATUS = nf_def_var(NCID,'RUNSF',NF_FLOAT,2,VARDIMS,varID(5))
!       vname = 'surface runoff'
!       vunit = 'mm/s'
!       STATUS = nf_put_att_text(NCID,varID(5),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(5),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(5),'missing_value',NF_FLOAT,1,miss)

!       STATUS = nf_def_var(NCID,'RUNSB',NF_FLOAT,2,VARDIMS,varID(6))
!       vname = 'subsurface runoff'
!       vunit = 'mm/s'
!       STATUS = nf_put_att_text(NCID,varID(6),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(6),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(6),'missing_value',NF_FLOAT,1,miss)

!       STATUS = nf_def_var(NCID,'QSUBCAN',NF_FLOAT,2,VARDIMS,varID(7))
!       vname = 'sublimation from canopy-intercepted snow'
!       vunit = 'mm/s'
!       STATUS = nf_put_att_text(NCID,varID(7),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(7),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(7),'missing_value',NF_FLOAT,1,miss)

!       STATUS = nf_def_var(NCID,'QSUBGRD',NF_FLOAT,2,VARDIMS,varID(8))
!       vname = 'sublimation from ground snow'
!       vunit = 'mm/s'
!       STATUS = nf_put_att_text(NCID,varID(8),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(8),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(8),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SMC',NF_FLOAT,3,VARDIMS3d,varID(9))
        vname = 'soil moisture content (4L: 0.0-0.1m; 0.1-0.4m; 0.4-1.0m; 1.0-2.0m)'
        vunit = 'm3/m3'
        STATUS = nf_put_att_text(NCID,varID( 9),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID( 9),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID( 9),'missing_value',NF_FLOAT,1,miss)

        STATUS = nf_def_var(NCID,'SH2O',NF_FLOAT,3,VARDIMS3d,varID(10))
        vname = 'soil liquid water content (4L: 0.0-0.1m; 0.1-0.4m; 0.4-1.0m; 1.0-2.0m)'
        vunit = 'm3/m3'
        STATUS = nf_put_att_text(NCID,varID(10),'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,varID(10),'units'    ,lencs(vunit),vunit)
        status = nf_put_att_REAL(NCID,varID(10),'missing_value',NF_FLOAT,1,miss)

        STATUS = NF_ENDDEF(NCID)
        status = NF_CLOSE(NCID)

     ! NC format output

        STATUS = NF_OPEN (ncfile, NF_WRITE, NCID)
        status = nf_put_vara_real(NCID, lonID,START(1),COUNT(1),  lon)
        status = nf_put_vara_real(NCID, latID,START(2),COUNT(2),  lat)
       !STATUS = NF_PUT_VARA_REAL(NCID,varID( 1),START,COUNT,snowhm)
       !STATUS = NF_PUT_VARA_REAL(NCID,varID( 2),START,COUNT,sneqvm)
       !STATUS = NF_PUT_VARA_REAL(NCID,varID( 3),START,COUNT,scfm)
       !STATUS = NF_PUT_VARA_REAL(NCID,varID( 4),START,COUNT,fsnogm)
       !STATUS = NF_PUT_VARA_REAL(NCID,varID( 5),START,COUNT,runsfm)
       !STATUS = NF_PUT_VARA_REAL(NCID,varID( 6),START,COUNT,runsbm)
       !STATUS = NF_PUT_VARA_REAL(NCID,varID( 7),START,COUNT,qsubcanm)
       !STATUS = NF_PUT_VARA_REAL(NCID,varID( 8),START,COUNT,qsubgrdm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 9),START3D,COUNT3D,smcm)
        STATUS = NF_PUT_VARA_REAL(NCID,varID(10),START3D,COUNT3D,sh2om)

        status= NF_CLOSE(NCID)

        ND    = 0
        !$OMP PARALLEL DO
        do iy = 1,ny
        do ix = 1,nx
        if(vegtyp(ix,iy) > 0) then
           snowhm(ix,iy) = 0.
           sneqvm(ix,iy) = 0.
           scfm(ix,iy)   = 0.
           fsnogm(ix,iy) = 0.
           runsfm(ix,iy) = 0.
           runsbm(ix,iy) = 0.
           qsubcanm(ix,iy) = 0.
           qsubgrdm(ix,iy) = 0.
           do iz = 1, nsoil
             smcm    (ix,iz,iy) = 0. 
             sh2om   (ix,iz,iy) = 0.
           end do
        end if
        end do
        end do
        !$OMP END PARALLEL DO

     end if

    END SUBROUTINE NC_OUT_daily

!--------------------------------------------------------------------
    SUBROUTINE NC_OUT_hr(nsoil   ,nx      ,ny      ,it      ,dt  , &
                      iyear   ,imonth  ,iday    ,DIR     ,EXP     ,lonxy    , &
                      latxy   ,vegtypxy,idstep  ,nday    ,&
                      snowhxy ,sneqvxy ,tgxy    ,stcxy   ,smcxy   , &
                      sh2oxy  ,prcpxy  ,runsfxy ,runsbxy ,ecanxy  , &
                      edirxy  ,etranxy ,zwtxy   ,fsaxy   ,firaxy  , &
                      fshxy   ,flhxy   ,fghxy   ,aparxy  ,psnxy   , &
                      savxy   ,sagxy   ,fsnoxy  ,xlaixy  ,xsaixy  , &
                      tradxy  ,neexy   ,gppxy   ,nppxy   ,tsxy    , &
                      fvegxy  ,qco2xy  ,socxy   , &
                      wdocxy  ,ddocxy  ,micxy   ,wenzxy  ,denzxy  )

    implicit none

! inputs
    integer, intent(in) :: iyear,imonth
    CHARACTER(len=256),INTENT(IN) :: DIR
    CHARACTER(len=8),INTENT(IN) :: EXP
    integer, intent(in)         :: it
    integer, intent(in)         :: idstep
    integer, intent(in)         :: nx
    integer, intent(in)         :: ny 
    integer, intent(in)         :: nsoil
    integer, intent(in)         :: iday
    integer, intent(in)         :: nday(12)
    real, intent(in)            :: dt
    integer, dimension(1:nx,1:ny), intent(in)      :: vegtypxy
    real, dimension(1:nx,1:ny), intent(in)         :: lonxy
    real, dimension(1:nx,1:ny), intent(in)         :: latxy
    real, dimension(1:nx,1:ny), intent(in)         :: snowhxy
    real, dimension(1:nx,1:ny), intent(in)         :: sneqvxy
    real, dimension(1:nx,1:ny), intent(in)         :: tgxy
    real, dimension(1:nx,1:ny), intent(in)         :: prcpxy
    real, dimension(1:nx,1:ny), intent(in)         :: runsfxy
    real, dimension(1:nx,1:ny), intent(in)         :: runsbxy
    real, dimension(1:nx,1:ny), intent(in)         :: ecanxy
    real, dimension(1:nx,1:ny), intent(in)         :: edirxy
    real, dimension(1:nx,1:ny), intent(in)         :: etranxy
    real, dimension(1:nx,1:ny), intent(in)         :: zwtxy
    real, dimension(1:nx,1:ny), intent(in)         :: fsaxy
    real, dimension(1:nx,1:ny), intent(in)         :: firaxy
    real, dimension(1:nx,1:ny), intent(in)         :: fshxy
    real, dimension(1:nx,1:ny), intent(in)         :: flhxy
    real, dimension(1:nx,1:ny), intent(in)         :: fghxy
    real, dimension(1:nx,1:ny), intent(in)         :: aparxy
    real, dimension(1:nx,1:ny), intent(in)         :: psnxy
    real, dimension(1:nx,1:ny), intent(in)         :: savxy
    real, dimension(1:nx,1:ny), intent(in)         :: sagxy
    real, dimension(1:nx,1:ny), intent(in)         :: fsnoxy
    real, dimension(1:nx,1:nsoil,1:ny), intent(in) :: stcxy
    real, dimension(1:nx,1:nsoil,1:ny), intent(in) :: smcxy
    real, dimension(1:nx,1:nsoil,1:ny), intent(in) :: sh2oxy
    real, dimension(1:nx,1:ny), intent(in)         :: xlaixy
    real, dimension(1:nx,1:ny), intent(in)         :: xsaixy
    real, dimension(1:nx,1:ny), intent(in)         :: tradxy
    real, dimension(1:nx,1:ny), intent(in)         :: neexy
    real, dimension(1:nx,1:ny), intent(in)         :: gppxy
    real, dimension(1:nx,1:ny), intent(in)         :: nppxy
    real, dimension(1:nx,1:ny), intent(in)         :: tsxy
    real, dimension(1:nx,1:ny), intent(in)         :: fvegxy

    real, dimension(1:nx,1:ny), intent(in)         :: qco2xy
    real, dimension(1:nx,1:ny), intent(in) :: socxy
    real, dimension(1:nx,1:ny), intent(in) :: wdocxy
    real, dimension(1:nx,1:ny), intent(in) :: ddocxy
    real, dimension(1:nx,1:ny), intent(in) :: micxy
    real, dimension(1:nx,1:ny), intent(in) :: wenzxy
    real, dimension(1:nx,1:ny), intent(in) :: denzxy

    INTEGER, PARAMETER :: nvar = 60
    integer :: N,ND,ix,iy,iz
    integer :: nt
    integer, dimension(1:nsoil)  :: ilev
    real, dimension(1:nx)        :: lon
    real, dimension(1:ny)        :: lat

    real ::  miss
    character(len=120) :: ncfile
    character(len=120) :: vname
    character(len=120) :: vunit

    INCLUDE 'netcdf.inc'

    INTEGER STATUS
    INTEGER NCID
    INTEGER varID(nvar),lonID,latID,levID,TIMID
    INTEGER XDIM,YDIM,ZDIM,TDIM
    INTEGER VARDIMS(3)
    INTEGER START(3), COUNT(3)
    INTEGER VARDIMS4d(4)
    INTEGER START4d(4), COUNT4d(4)
    DATA START   /  1,   1,   1/
    DATA START4d /  1,   1,   1,   1/
    DATA miss/-999.9/

    save ncfile, TIMID, varID

     count(1)   = nx
     count(2)   = ny
     count(3)   = 1

     count4d(1) = nx
     count4d(2) = nsoil
     count4d(3) = ny
     count4d(4) = 1

     start(3)   = idstep
     start4d(4) = idstep

     IF(idstep == 1) THEN 

        do ix = 1,nx
          lon(ix) = lonxy(ix,1)
        end do

        do iy = 1, ny
          lat(iy) = latxy(1,iy)
        end do

        do iz = 1, nsoil
          ilev(iz) = iz
        end do

        nt = 86400./dt

        ! creat nc output files and initialize output file ID

        if(imonth < 10) then
           if(iday < 10) then
              write(*,*) TRIM(EXP),iyear,imonth,iday
              write(ncfile,100) TRIM(EXP),iyear,imonth,iday
           else
              write(*,*) TRIM(EXP),iyear,imonth,iday
              write(ncfile,200) TRIM(EXP),iyear,imonth,iday
           end if
        else
           if(iday < 10) then
              write(*,*) TRIM(EXP),iyear,imonth,iday
              write(ncfile,300) TRIM(EXP),iyear,imonth,iday
           else
              write(*,*) TRIM(EXP),iyear,imonth,iday
              write(ncfile,400) TRIM(EXP),iyear,imonth,iday
           end if
        end if
  100   format('/results/',a,'/hrly/Noah.hrly.',i4,'0',i1,'0',i1,'.nc')
  200   format('/results/',a,'/hrly/Noah.hrly.',i4,'0',i1,i2,'.nc')
  300   format('/results/',a,'/hrly/Noah.hrly.',i4,i2,'0',i1,'.nc')
  400   format('/results/',a,'/hrly/Noah.hrly.',i4,i2,i2,'.nc')

        write(*,*) '----------------------------------------------------------------------------'
        write(*,*) 'opening ncfile: ../Noah_data',trim(ncfile)
        write(*,*) '----------------------------------------------------------------------------'

        ncfile = TRIM(DIR)//ncfile

        STATUS=NF_CREATE(ncfile,NF_CLOBBER, NCID)
        STATUS=NF_DEF_DIM(NCID, 'time' ,nt      , TDIM)
        STATUS=NF_DEF_DIM(NCID, 'nx'   ,nx      , XDIM)
        STATUS=NF_DEF_DIM(NCID, 'ny'   ,ny      , YDIM)
        STATUS=NF_DEF_DIM(NCID, 'nz'   ,nsoil   , ZDIM)
        VARDIMS(1) = XDIM
        VARDIMS(2) = YDIM
        VARDIMS(3) = TDIM

        VARDIMS4d(1) = XDIM
        VARDIMS4d(2) = ZDIM
        VARDIMS4d(3) = YDIM
        VARDIMS4d(4) = TDIM


     ! initilize output variables ID

        status = nf_def_var(NCID,'time',nf_float,1,TDIM,TIMID)
        vname = 'time step'
        vunit = 'hrly timestep'
        STATUS = nf_put_att_text(NCID,TIMID,'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,TIMID,'units'    ,lencs(vunit),vunit)

        STATUS = nf_def_var(NCID,'lon',nf_float,1,XDIM,lonID)
        vname = 'longitude coordinate'
        vunit = 'degrees_east'
        STATUS = nf_put_att_text(NCID,lonID,'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,lonID,'units'    ,lencs(vunit),vunit)

        STATUS = nf_def_var(NCID,'lat',nf_float,1,YDIM,latID)
        vname = 'latitude coordinate'
        vunit = 'degrees_north'
        STATUS = nf_put_att_text(NCID,latID,'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,latID,'units'    ,lencs(vunit),vunit)

        STATUS = nf_def_var(NCID,'lev',nf_int,1,ZDIM,levID)
        vname = 'soil layers'
        vunit = 'from surface to bottom'
        STATUS = nf_put_att_text(NCID,levID,'long_name',lencs(vname),vname)
        STATUS = nf_put_att_text(NCID,levID,'units'    ,lencs(vunit),vunit)

!       STATUS = nf_def_var(NCID,'SNOWD',NF_FLOAT,3,VARDIMS,varID(1))
!       vname = 'snow depth'
!       vunit = 'm'
!       STATUS = nf_put_att_text(NCID,varID(1),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(1),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(1),'missing_value',NF_FLOAT,1,miss)

!       STATUS = nf_def_var(NCID,'SWE',NF_FLOAT,3,VARDIMS,varID(2))
!       vname = 'snow water equivalent'
!       vunit = 'kg/m2'
!       STATUS = nf_put_att_text(NCID,varID(2),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(2),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(2),'missing_value',NF_FLOAT,1,miss)

!       STATUS = nf_def_var(NCID,'SCF',NF_FLOAT,3,VARDIMS,varID(3))
!       vname = 'snow cover fraction'
!       vunit = '-'
!       STATUS = nf_put_att_text(NCID,varID(3),'long_name',lencs(vname),vname)
!       STATUS = nf_put_att_text(NCID,varID(3),'units'    ,lencs(vunit),vunit)
!       status = nf_put_att_REAL(NCID,varID(3),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'TG',NF_FLOAT,3,VARDIMS,varID(3))
!        vname = 'ground surface temperature'
!        vunit = 'K'
!        STATUS = nf_put_att_text(NCID,varID(3),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(3),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(3),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'PRCP',NF_FLOAT,3,VARDIMS,varID(4))
!        vname = 'precipitation'
!        vunit = 'mm/s'
!        STATUS = nf_put_att_text(NCID,varID(4),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(4),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(4),'missing_value',NF_FLOAT,1,miss)

         STATUS = nf_def_var(NCID,'RUNSF',NF_FLOAT,3,VARDIMS,varID(5))
         vname = 'surface runoff'
         vunit = 'mm/s'
         STATUS = nf_put_att_text(NCID,varID(5),'long_name',lencs(vname),vname)
         STATUS = nf_put_att_text(NCID,varID(5),'units'    ,lencs(vunit),vunit)
         status = nf_put_att_REAL(NCID,varID(5),'missing_value',NF_FLOAT,1,miss)

         STATUS = nf_def_var(NCID,'RUNSB',NF_FLOAT,3,VARDIMS,varID(6))
         vname = 'subsurface runoff'
         vunit = 'mm/s'
         STATUS = nf_put_att_text(NCID,varID(6),'long_name',lencs(vname),vname)
         STATUS = nf_put_att_text(NCID,varID(6),'units'    ,lencs(vunit),vunit)
         status = nf_put_att_REAL(NCID,varID(6),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'ECAN',NF_FLOAT,3,VARDIMS,varID(7))
!        vname = 'canopy interception loss'
!        vunit = 'mm/s'
!        STATUS = nf_put_att_text(NCID,varID(7),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(7),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(7),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'ESOIL',NF_FLOAT,3,VARDIMS,varID(8))
!        vname = 'soil surface evaporation'
!        vunit = 'mm/s'
!        STATUS = nf_put_att_text(NCID,varID(8),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(8),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(8),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'ETRAN',NF_FLOAT,3,VARDIMS,varID(9))
!        vname = 'transpiration'
!        vunit = 'mm/s'
!        STATUS = nf_put_att_text(NCID,varID(9),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(9),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(9),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'ZWT',NF_FLOAT,3,VARDIMS,varID(10))
!        vname = 'depth to water table'
!        vunit = 'm'
!        STATUS = nf_put_att_text(NCID,varID(10),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(10),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(10),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'STC',NF_FLOAT,4,VARDIMS4d,varID(11))
!        vname = 'soil temperature (4-L)'
!        vunit = 'K'
!        STATUS = nf_put_att_text(NCID,varID(11),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(11),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(11),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'SMC',NF_FLOAT,4,VARDIMS4d,varID(12))
!        vname = 'soil moisture content (4-L)'
!        vunit = 'm3/m3'
!        STATUS = nf_put_att_text(NCID,varID(12),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(12),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(12),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'SH2O',NF_FLOAT,4,VARDIMS4d,varID(13))
!        vname = 'soil liquid water content (4-L)'
!        vunit = 'm3/m3'
!        STATUS = nf_put_att_text(NCID,varID(13),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(13),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(13),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'FSA',NF_FLOAT,3,VARDIMS,varID(14))
!        vname = 'net solar radiation'
!        vunit = 'W/m2'
!        STATUS = nf_put_att_text(NCID,varID(14),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(14),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(14),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'FIRA',NF_FLOAT,3,VARDIMS,varID(15))
!        vname = 'net longwave radiation'
!        vunit = 'W/m2'
!        STATUS = nf_put_att_text(NCID,varID(15),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(15),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(15),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'FSH',NF_FLOAT,3,VARDIMS,varID(16))
!        vname = 'sensible heat flux'
!        vunit = 'W/m2'
!        STATUS = nf_put_att_text(NCID,varID(16),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(16),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(16),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'FLH',NF_FLOAT,3,VARDIMS,varID(17))
!        vname = 'latent heat flux'
!        vunit = 'W/m2'
!        STATUS = nf_put_att_text(NCID,varID(17),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(17),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(17),'missing_value',NF_FLOAT,1,miss)
!        STATUS = nf_def_var(NCID,'FGH',NF_FLOAT,3,VARDIMS,varID(18))
!        vname = 'ground heat flux'
!        vunit = 'W/m2'
!        STATUS = nf_put_att_text(NCID,varID(18),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(18),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(18),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'APAR',NF_FLOAT,3,VARDIMS,varID(19))
!        vname = 'photosyn active radiation'
!        vunit = 'W/m2'
!        STATUS = nf_put_att_text(NCID,varID(19),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(19),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(19),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'PSN',NF_FLOAT,3,VARDIMS,varID(20))
!        vname = 'total photosynthesis'
!        vunit = 'umol co2/m2/s'
!        STATUS = nf_put_att_text(NCID,varID(20),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(20),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(20),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'SAV',NF_FLOAT,3,VARDIMS,varID(21))
!        vname = 'solar rad absorbed by veg.`'
!        vunit = 'W/m2'
!        STATUS = nf_put_att_text(NCID,varID(21),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(21),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(21),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'SAG',NF_FLOAT,3,VARDIMS,varID(22))
!        vname = 'solar rad absorbed by ground'
!        vunit = 'W/m2'
!        STATUS = nf_put_att_text(NCID,varID(22),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(22),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(22),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'FSNO',NF_FLOAT,3,VARDIMS,varID(23))
!        vname = 'snow cover fraction on the ground'
!        vunit = 'fraction'
!        STATUS = nf_put_att_text(NCID,varID(23),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(23),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(23),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'LAI',NF_FLOAT,3,VARDIMS,varID(24))
!        vname = 'leaf area index'
!        vunit = 'm2/m2'
!        STATUS = nf_put_att_text(NCID,varID(24),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(24),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(24),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'SAI',NF_FLOAT,3,VARDIMS,varID(25))
!        vname = 'stem area index'
!        vunit = 'm2/m2'
!        STATUS = nf_put_att_text(NCID,varID(25),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(25),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(25),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'TRAD',NF_FLOAT,3,VARDIMS,varID(26))
!        vname = 'surface radiative temperature'
!        vunit = 'K'
!        STATUS = nf_put_att_text(NCID,varID(26),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(26),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(26),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'NEE',NF_FLOAT,3,VARDIMS,varID(27))
!        vname = 'net CO2 flux'
!        vunit = 'g/m2/s CO2'
!        STATUS = nf_put_att_text(NCID,varID(27),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(27),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(27),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'GPP',NF_FLOAT,3,VARDIMS,varID(28))
!        vname = 'GPP'
!        vunit = 'g/m2/s carbon'
!        STATUS = nf_put_att_text(NCID,varID(28),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(28),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(28),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'NPP',NF_FLOAT,3,VARDIMS,varID(29))
!        vname = 'NPP'
!        vunit = 'g/m2/s carbon'
!        STATUS = nf_put_att_text(NCID,varID(29),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(29),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(29),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'T2M',NF_FLOAT,3,VARDIMS,varID(30))
!        vname = '2-m air temperature'
!        vunit = 'K'
!        STATUS = nf_put_att_text(NCID,varID(30),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(30),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(30),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'FVEG',NF_FLOAT,3,VARDIMS,varID(31))
!        vname = 'greenness vegetation fraction'
!        vunit = 'fraction'
!        STATUS = nf_put_att_text(NCID,varID(31),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(31),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(31),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'QCO2',NF_FLOAT,3,VARDIMS,varID(32))
!        vname = 'soil surface co2 flux'
!        vunit = 'g/m2/s'
!        STATUS = nf_put_att_text(NCID,varID(32),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(32),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(32),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'SOC',NF_FLOAT,3,VARDIMS,varID(33))
!        vname = 'soil soc concentration'
!        vunit = 'g/m3'
!        STATUS = nf_put_att_text(NCID,varID(33),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(33),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(33),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'WDOC',NF_FLOAT,3,VARDIMS,varID(34))
!        vname = 'soil doc concentration'
!        vunit = 'g/m3'
!        STATUS = nf_put_att_text(NCID,varID(34),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(34),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(34),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'MIC',NF_FLOAT,3,VARDIMS,varID(35))
!        vname = 'soil mic concentration'
!        vunit = 'g/m3'
!        STATUS = nf_put_att_text(NCID,varID(35),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(35),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(35),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'WENZ',NF_FLOAT,3,VARDIMS,varID(36))
!        vname = 'soil enzyme concentration'
!        vunit = 'g/m3'
!        STATUS = nf_put_att_text(NCID,varID(36),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(36),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(36),'missing_value',NF_FLOAT,1,miss)

!        STATUS = nf_def_var(NCID,'DDOC',NF_FLOAT,3,VARDIMS,varID(37))
!        vname = 'soil doc concentration'
!        vunit = 'g/m3'
!        STATUS = nf_put_att_text(NCID,varID(37),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(37),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(37),'missing_value',NF_FLOAT,1,miss)
!
!        STATUS = nf_def_var(NCID,'DENZ',NF_FLOAT,3,VARDIMS,varID(38))
!        vname = 'soil enzyme concentration'
!        vunit = 'g/m3'
!        STATUS = nf_put_att_text(NCID,varID(38),'long_name',lencs(vname),vname)
!        STATUS = nf_put_att_text(NCID,varID(38),'units'    ,lencs(vunit),vunit)
!        status = nf_put_att_REAL(NCID,varID(38),'missing_value',NF_FLOAT,1,miss)

        STATUS = NF_ENDDEF(NCID)
        status = NF_CLOSE(NCID)

     END IF

     ! NC format output
      
        STATUS = NF_OPEN (ncfile, NF_WRITE, NCID)

        STATUS = NF_PUT_VARA_REAL(NCID,TIMID,START(3),COUNT(3),idstep*1.)
        if(idstep == 1)then
        status = nf_put_vara_real(NCID, lonID,START(1),COUNT(1),  lon)
        status = nf_put_vara_real(NCID, latID,START(2),COUNT(2),  lat)
        status = nf_put_vara_int (NCID, levID,       1,   nsoil, ilev)
        end if
!        STATUS = NF_PUT_VARA_REAL(NCID,varID( 1),START,COUNT,snowhxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID( 2),START,COUNT,sneqvxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID( 3),START,COUNT,tgxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID( 4),START,COUNT,prcpxy)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 5),START,COUNT,runsfxy)
        STATUS = NF_PUT_VARA_REAL(NCID,varID( 6),START,COUNT,runsbxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID( 7),START,COUNT,ecanxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID( 8),START,COUNT,edirxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID( 9),START,COUNT,etranxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(10),START,COUNT,zwtxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(11),START4d,COUNT4d,stcxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(12),START4d,COUNT4d,smcxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(13),START4d,COUNT4d,sh2oxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(14),START,COUNT,fsaxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(15),START,COUNT,firaxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(16),START,COUNT,fshxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(17),START,COUNT,flhxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(18),START,COUNT,fghxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(19),START,COUNT,aparxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(20),START,COUNT,psnxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(21),START,COUNT,savxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(22),START,COUNT,sagxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(23),START,COUNT,fsnoxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(24),START,COUNT,xlaixy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(25),START,COUNT,xsaixy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(26),START,COUNT,tradxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(27),START,COUNT,neexy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(28),START,COUNT,gppxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(29),START,COUNT,nppxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(30),START,COUNT,tsxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(31),START,COUNT,fvegxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(32),START,COUNT,qco2xy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(33),START,COUNT,socxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(34),START,COUNT,wdocxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(35),START,COUNT,micxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(36),START,COUNT,wenzxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(37),START,COUNT,ddocxy)
!        STATUS = NF_PUT_VARA_REAL(NCID,varID(38),START,COUNT,denzxy)

        status= NF_CLOSE(NCID)

! -------------------------------------------------------------------
    END SUBROUTINE NC_OUT_hr
! -------------------------------------------------------------------
! -------------------------------------------------------------------
   SUBROUTINE write_ini(DIR,EXP  ,nx      ,ny      ,nsoil, nsoil_var   ,nsnow   ,iyear   , &
                        imonth   ,iday    ,itime   ,latxy   ,lonxy   ,vegtypxy, &
                        soiltypxy,SLOPETYPxy,CROPTYPxy,soilcolor ,&
                        smcxy    ,stcxy   ,sh2oxy  ,tsnoxy  ,snicexy ,snliqxy , radiusxy ,&
                        zsnsoxy  ,isnoxy  ,snowhxy ,sneqvxy ,canliqxy,canicexy, &
                        tgxy     ,tvxy    ,waxy    ,wtxy    ,zwtxy   ,lfmassxy, &
                        rootmsxy ,stmassxy,woodxy  ,socxy   ,wdocxy  ,ddocxy  , &
                        micxy    ,wenzxy  ,denzxy  ,wslakexy,mqxy    ,krxy    , &
                        frootxy  ,rtmassxy,STBLCP  ,FASTCP  ,psixy   ,wcndxy  , &
                        atm_bcxy ,htopxy  )
! -------------------------------------------------------------------
   use module_sf_noahmplsm
   USE NOAHMP_TABLES

   implicit none

     type (noahmp_parameters) :: parameters

     CHARACTER(len=256),INTENT(IN) :: DIR
     CHARACTER(len=8),INTENT(IN) :: EXP
     integer :: nroot
     integer, intent(in) :: nx
     integer, intent(in) :: ny
     integer, intent(in) :: nsoil
     integer, dimension(nx,ny), intent(in) :: nsoil_var
     integer, intent(in) :: nsnow
     integer, intent(in) :: iyear
     integer, intent(in) :: imonth
     integer, intent(in) :: iday
     integer, intent(in) :: itime

     integer, intent(in), dimension(nx,ny)     :: soiltypxy
     integer, intent(in), dimension(nx,ny)     :: vegtypxy
     integer, intent(in), dimension(nx,ny)     :: slopetypxy
     integer, intent(in), dimension(nx,ny)     :: croptypxy
     integer, intent(in), dimension(nx,ny)     :: soilcolor
     integer, intent(in), dimension(nx,ny)     :: atm_bcxy

     real, dimension(1:nx,1:ny), intent(in)         :: lonxy
     real, dimension(1:nx,1:ny), intent(in)         :: latxy
     real, dimension(1:nx,1:ny), intent(in)         :: htopxy
     real, intent(in), dimension(nx,       1:nsoil,ny) :: smcxy    ! 1
     real, intent(in), dimension(nx,-nsnow+1:nsoil,ny) :: stcxy    ! 2
     real, intent(in), dimension(nx,       1:nsoil,ny) :: sh2oxy   ! 3
     real, intent(in), dimension(nx,       1:nsoil,ny) :: psixy    ! 3
     real, intent(in), dimension(nx,       1:nsoil,ny) :: wcndxy    ! 3

     real, intent(in), dimension(nx,-nsnow+1:    0,ny) :: tsnoxy   ! 4
     real, intent(in), dimension(nx,-nsnow+1:    0,ny) :: snicexy  ! 5
     real, intent(in), dimension(nx,-nsnow+1:    0,ny) :: snliqxy  ! 6
     real, intent(in), dimension(nx,-nsnow+1:    0,ny) :: radiusxy !18

     real, intent(in), dimension(nx,-nsnow+1:nsoil,ny) :: zsnsoxy  ! 7
     real, intent(in), dimension(nx,ny)                :: tvxy     ! 8
     real, intent(in), dimension(nx,ny)                :: tgxy     ! 9
     real, intent(in), dimension(nx,ny)                :: canliqxy !10
     real, intent(in), dimension(nx,ny)                :: canicexy !11
     real, intent(in), dimension(nx,ny)                :: snowhxy  !12
     real, intent(in), dimension(nx,ny)                :: sneqvxy  !13
     real, intent(in), dimension(nx,ny)                :: waxy     !14
     real, intent(in), dimension(nx,ny)                :: wtxy     !15
     real, intent(in), dimension(nx,ny)                :: zwtxy    !16
     integer, intent(in), dimension(nx,ny)             :: isnoxy   !17
     REAL, intent(in), dimension(nx,ny)    :: lfmassxy!leaf mass [g/m2]
     REAL, intent(in), dimension(nx,nsoil,ny)  :: rootmsxy!carbon mass of fine roots [g/m2]
     REAL, intent(in), dimension(nx,nsoil,ny)  :: frootxy !fraction of fine roots [g/m2]
     REAL, intent(in), dimension(nx,ny)    :: stmassxy!stem mass [g/m2]
     REAL, intent(in), dimension(nx,ny)    :: rtmassxy! [g/m2]
     REAL, intent(in), dimension(nx,ny)    :: STBLCP  ! [g/m2]
     REAL, intent(in), dimension(nx,ny)    :: FASTCP  ! [g/m2]
     REAL, intent(in), dimension(nx,ny)    :: woodxy  !mass of wood (incl. woody roots) [g/m2]
     REAL, intent(in), dimension(nx,ny)    :: socxy  ! soil organic carbon density [g/m3]a
     REAL, intent(in), dimension(nx,ny)    :: wdocxy ! dissolved organic carbon density [g/m3]
     REAL, intent(in), dimension(nx,ny)    :: ddocxy ! dissolved organic carbon density [g/m3]a
     REAL, intent(in), dimension(nx,ny)    :: micxy  ! microbial biomass density [g/m3]
     REAL, intent(in), dimension(nx,ny)    :: wenzxy ! enzyme density [g/m3]
     REAL, intent(in), dimension(nx,ny)    :: denzxy ! enzyme density [g/m3]
     REAL, intent(in), dimension(nx,ny)    :: wslakexy !
     REAL, intent(in), dimension(nx,ny)    :: mqxy   !
     REAL, intent(in), dimension(nx,ny)    :: krxy   !


     integer :: ix,iy,iz
     integer, dimension(1:nsoil)     :: ilev
     integer, dimension(-nsnow+1:0)  :: ilevs
     real, dimension(1:nx)           :: lon
     real, dimension(1:ny)           :: lat
     real, dimension(1:nx,       1:nsoil,1:ny) :: tsoilxy
     integer, dimension(1:nx,1:ny)             :: isnowxy
     real, dimension(1:nx,-nsnow+1:    0,1:ny) :: zsnowxy
     real, dimension(1:nx,-nsnow+1:    0,1:ny) :: tsnowxy
     real, dimension(1:nx,-nsnow+1:    0,1:ny) :: snowicexy
     real, dimension(1:nx,-nsnow+1:    0,1:ny) :: snowliqxy
     real, dimension(1:nx,-nsnow+1:    0,1:ny) :: radiuswxy

     integer ,parameter :: nvar = 24
     real    ,parameter :: miss = -999.9
     integer ,parameter :: imiss= -999
     character(len=256) :: ncfile, acfile
     character(len=120) :: vname
     character(len=120) :: vunit

     INCLUDE 'netcdf.inc'


     LOGICAL NC
     INTEGER STATUS
     INTEGER NCID
     INTEGER varID(nvar),lonID,latID,levID,levsID
     INTEGER VARDIMS(2),VARDIMS3d(3),VARDIMS3ds(3)
     INTEGER XDIM,YDIM,ZDIM,ZSDIM
     INTEGER START3d(3), COUNT3d(3), COUNT3ds(3)
     INTEGER START(2), COUNT(2)
     DATA START   /  1,   1/
     DATA START3d /  1,   1,   1/

     NC = .FALSE.


     count(1)    = nx
     count(2)    = ny
     count3d(1)  = nx
     count3d(2)  = nsoil
     count3d(3)  = ny

     count3ds(1) = nx
     count3ds(2) = nsnow
     count3ds(3) = ny
 
     do ix = 1,nx
          lon(ix) = lonxy(ix,1)
     end do
 
     do iy = 1, ny
          lat(iy) = latxy(1,iy)
     end do

     do iz = 1, nsoil
          ilev(iz) = iz
     end do

     do iz = -nsnow+1,0
          ilevs(iz) = iz
     end do

     if(imonth < 10) then
          write(*,*) TRIM(EXP),iyear,imonth,iday
          write(ncfile,100) TRIM(EXP),iyear,imonth,iday
          write(acfile,101) TRIM(EXP),iyear,imonth,iday
     else
          write(*,*) TRIM(EXP),iyear,imonth,iday
          write(ncfile,200) TRIM(EXP),iyear,imonth,iday
          write(acfile,201) TRIM(EXP),iyear,imonth,iday
     end if

     !write(*,*) '================================================================================'
     !write(*,*) 'openning initial file: ../Noah_data',trim(ncfile)
     !write(*,*) '================================================================================'
     ncfile = TRIM(DIR)//ncfile
     acfile = TRIM(DIR)//acfile
     !write(*,*) trim(acfile)

     IF(NC) THEN

     !open nc file

     STATUS=NF_CREATE(ncfile,NF_CLOBBER, NCID)
     STATUS=NF_DEF_DIM(NCID, 'nx'  ,nx      , XDIM)
     STATUS=NF_DEF_DIM(NCID, 'ny'  ,ny      , YDIM)
     STATUS=NF_DEF_DIM(NCID, 'ns'  ,nsnow   , ZSDIM)
     STATUS=NF_DEF_DIM(NCID, 'nz'  ,nsoil   , ZDIM)
     VARDIMS(1)    = XDIM
     VARDIMS(2)    = YDIM
     VARDIMS3d(1)  = XDIM
     VARDIMS3d(2)  = YDIM
     VARDIMS3d(3)  = ZDIM
     VARDIMS3ds(1) = XDIM
     VARDIMS3ds(2) = YDIM
     VARDIMS3ds(3) = ZSDIM
     STATUS = nf_def_var(NCID,'lon' ,nf_float,1,XDIM , lonID)
     STATUS = nf_def_var(NCID,'lat' ,nf_float,1,YDIM , latID)
     STATUS = nf_def_var(NCID,'levs',nf_float,1,ZSDIM,levsID)
     STATUS = nf_def_var(NCID,'lev' ,nf_float,1,ZDIM , levID)

     ! initilize output variables ID

     STATUS = nf_def_var(NCID,'ISNOW',NF_INT,2,VARDIMS,varID(1))
     vname = 'number of actual snow layers'
     vunit = '-'
     STATUS = nf_put_att_text(NCID,varID(1),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(1),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_INT (NCID,varID(1),'missing_value',NF_INT,1,imiss)

     STATUS = nf_def_var(NCID,'TV',NF_FLOAT,2,VARDIMS,varID(2))
     vname = 'vegetation canopy temperature'
     vunit = 'K'
     STATUS = nf_put_att_text(NCID,varID(2),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(2),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(2),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'TG',NF_FLOAT,2,VARDIMS,varID(3))
     vname = 'ground surface temperature'
     vunit = 'K'
     STATUS = nf_put_att_text(NCID,varID(3),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(3),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(3),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'CANICE',NF_FLOAT,2,VARDIMS,varID(4))
     vname = 'canopy-intercepted ice mass'
     vunit = 'mm'
     STATUS = nf_put_att_text(NCID,varID(4),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(4),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(4),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'CANLIQ',NF_FLOAT,2,VARDIMS,varID(5))
     vname = 'canopy-intercepted liquid water'
     vunit = 'mm'
     STATUS = nf_put_att_text(NCID,varID(5),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(5),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(5),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'SNOWH',NF_FLOAT,2,VARDIMS,varID(6))
     vname = 'snow depth'
     vunit = 'm'
     STATUS = nf_put_att_text(NCID,varID(6),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(6),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(6),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'SWE',NF_FLOAT,2,VARDIMS,varID(7))
     vname = 'snow water equivalent'
     vunit = 'mm'
     STATUS = nf_put_att_text(NCID,varID(7),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(7),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(7),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'WA',NF_FLOAT,2,VARDIMS,varID(8))
     vname = 'water stored in aquifers'
     vunit = 'mm'
     STATUS = nf_put_att_text(NCID,varID(8),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(8),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(8),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'WT',NF_FLOAT,2,VARDIMS,varID(9))
     vname = 'total water stored in aquifers and soils'
     vunit = 'mm'
     STATUS = nf_put_att_text(NCID,varID(9),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(9),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(9),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'ZWT',NF_FLOAT,2,VARDIMS,varID(10))
     vname = 'water table depth'
     vunit = 'm'
     STATUS = nf_put_att_text(NCID,varID(10),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(10),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(10),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'LFMASS',NF_FLOAT,2,VARDIMS,varID(11))
     vname = 'leaf carbon mass'
     vunit = 'g/m2'
     STATUS = nf_put_att_text(NCID,varID(11),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(11),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(11),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'RTMASS',NF_FLOAT,3,VARDIMS3d,varID(12))
     vname = 'live active root carbon mass'
     vunit = 'g/m2'
     STATUS = nf_put_att_text(NCID,varID(12),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(12),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(12),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'STMASS',NF_FLOAT,2,VARDIMS,varID(13))
     vname = 'stem carbon mass'
     vunit = 'g/m2'
     STATUS = nf_put_att_text(NCID,varID(13),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(13),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(13),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'WOOD',NF_FLOAT,2,VARDIMS,varID(14))
     vname = 'wood carbon mass'
     vunit = 'g/m2'
     STATUS = nf_put_att_text(NCID,varID(14),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(14),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(14),'missing_value',NF_FLOAT,1,miss)


     STATUS = nf_def_var(NCID,'SMC',NF_FLOAT,3,VARDIMS3d,varID(17))
     vname = 'soil moisture content (ice+liq) (4-L)'
     vunit = 'm3/m3'
     STATUS = nf_put_att_text(NCID,varID(17),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(17),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(17),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'SH2O',NF_FLOAT,3,VARDIMS3d,varID(18))
     vname = 'soil liquiq water (4-L)'
     vunit = 'm3/m3'
     STATUS = nf_put_att_text(NCID,varID(18),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(18),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(18),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'STC',NF_FLOAT,3,VARDIMS3d,varID(19))
     vname = 'soil temperature (4-L)'
     vunit = 'K'
     STATUS = nf_put_att_text(NCID,varID(19),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(19),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(19),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'ZSNOW',NF_FLOAT,3,VARDIMS3ds,varID(20))
     vname = 'snow layer depth (from top to bottom)'
     vunit = 'm'
     STATUS = nf_put_att_text(NCID,varID(20),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(20),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(20),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'TSNOW',NF_FLOAT,3,VARDIMS3ds,varID(21))
     vname = 'snow layer temperature (from top to bottom)'
     vunit = 'k'
     STATUS = nf_put_att_text(NCID,varID(21),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(21),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(21),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'SNICE',NF_FLOAT,3,VARDIMS3ds,varID(22))
     vname = 'snow layer ice mass (from top to bottom)'
     vunit = 'mm'
     STATUS = nf_put_att_text(NCID,varID(22),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(22),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(22),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'SNLIQ',NF_FLOAT,3,VARDIMS3ds,varID(23))
     vname = 'snow layer liquid water mass (from top to bottom)'
     vunit = 'mm'
     STATUS = nf_put_att_text(NCID,varID(23),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(23),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(23),'missing_value',NF_FLOAT,1,miss)

     STATUS = nf_def_var(NCID,'SNGRAIN',NF_FLOAT,3,VARDIMS3ds,varID(24))
     vname = 'snow grain radius of each layer'
     vunit = 'um'
     STATUS = nf_put_att_text(NCID,varID(24),'long_name',lencs(vname),vname)
     STATUS = nf_put_att_text(NCID,varID(24),'units'    ,lencs(vunit),vunit)
     status = nf_put_att_REAL(NCID,varID(24),'missing_value',NF_FLOAT,1,miss)

     STATUS = NF_ENDDEF(NCID)
     status = NF_CLOSE(NCID)

     STATUS = NF_OPEN (ncfile, NF_WRITE, NCID)

     isnowxy  (:,:)   = -999
     zsnowxy  (:,:,:) = -999.9
     tsnowxy  (:,:,:) = -999.9
     snowicexy(:,:,:) = -999.9
     snowliqxy(:,:,:) = -999.9
     radiuswxy(:,:,:) = -999.9

     do ix = 1,nx
     do iy = 1,ny
      if(vegtypxy(ix,iy) > 0) then
        isnowxy(ix,iy) = -isnoxy(ix,iy)
        do iz = 1,nsoil
           tsoilxy(ix,iz,iy) = stcxy(ix,iz,iy)
        end do
        do iz = isnoxy(ix,iy)+1,0
           zsnowxy  (ix,iz,iy)  = -zsnsoxy(ix,iz,iy)
           tsnowxy  (ix,iz,iy)  =  tsnoxy (ix,iz,iy)
           snowicexy(ix,iz,iy)  =  snicexy(ix,iz,iy)
           snowliqxy(ix,iz,iy)  =  snliqxy(ix,iz,iy)
           radiuswxy(ix,iz,iy)  =  radiusxy(ix,iz,iy)
        end do
      end if
     end do
     end do

     status = nf_put_vara_real(NCID,lonID ,1,   nx,  lon)
     status = nf_put_vara_real(NCID,latID ,1,   ny,  lat)
     status = nf_put_vara_int (NCID,levsID,1,nsnow,ilevs)
     status = nf_put_vara_int (NCID,levID ,1,nsoil, ilev)
     STATUS = NF_PUT_VARA_INT (NCID,varID( 1),START,COUNT, isnowxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID( 2),START,COUNT,    tvxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID( 3),START,COUNT,    tgxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID( 4),START,COUNT,canicexy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID( 5),START,COUNT,canliqxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID( 6),START,COUNT, snowhxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID( 7),START,COUNT, sneqvxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID( 8),START,COUNT,    waxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID( 9),START,COUNT,    wtxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID(10),START,COUNT,   zwtxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID(11),START,COUNT,lfmassxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID(13),START,COUNT,stmassxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID(14),START,COUNT,  woodxy)

     STATUS = NF_PUT_VARA_REAL(NCID,varID(12),START3d, COUNT3d , rootmsxy)
     STATUS = NF_PUT_VARA_REAL(NCID,varID(17),START3d ,COUNT3d ,    smcxy) ! 1 -> nsoil
     STATUS = NF_PUT_VARA_REAL(NCID,varID(18),START3d ,COUNT3d ,   sh2oxy) ! 1 -> nsoil
     STATUS = NF_PUT_VARA_REAL(NCID,varID(19),START3d ,COUNT3d ,  tsoilxy) ! 1 -> nsoil
     STATUS = NF_PUT_VARA_REAL(NCID,varID(20),START3d ,COUNT3ds,  zsnowxy) ! isnow+1 -> 0
     STATUS = NF_PUT_VARA_REAL(NCID,varID(21),START3d ,COUNT3ds,  tsnowxy) ! isnow+1 -> 0
     STATUS = NF_PUT_VARA_REAL(NCID,varID(22),START3d ,COUNT3ds,snowicexy) ! isnow+1 -> 0
     STATUS = NF_PUT_VARA_REAL(NCID,varID(23),START3d ,COUNT3ds,snowliqxy) ! isnow+1 -> 0
     STATUS = NF_PUT_VARA_REAL(NCID,varID(24),START3d ,COUNT3ds,radiuswxy) ! isnow+1 -> 0

     status= NF_CLOSE(NCID)

     ENDIF

     open(200, file = acfile,form='unformatted',status = 'unknown')
    !open(200, file = acfile,status = 'unknown')

     do ix = 1,nx
     do iy = 1,ny

      if(vegtypxy(ix,iy) > 0) then

          CALL TRANSFER_MP_PARAMETERS(VEGTYPXY(ix,iy),SOILTYPXY(ix,iy), nsoil_var(ix,iy) ,&
               SLOPETYPXY(ix,iy),SOILCOLOR(ix,iy),CROPTYPXY(ix,iy),parameters,ix,iy)

         !if(ix == 1 .and. iy == 191) then
         !   write(*,*) 'nroot=',nroot
         !   write(*,*) "in write_ini, rootmsxy=",(rootmsxy(ix,iz,iy),iz=1,parameters%nroot)
         ! end if 

         write(200)ix,iy,isnoxy(ix,iy)
         write(200)(smcxy (ix,iz,iy),iz=1,nsoil_var(ix,iy)),&
                   (stcxy (ix,iz,iy),iz=isnoxy(ix,iy)+1,nsoil_var(ix,iy)),&
                   (sh2oxy(ix,iz,iy),iz=1,nsoil_var(ix,iy))
         write(200) wslakexy(ix,iy)  ! lake water storage
         if(isnoxy(ix,iy) .lt. 0) then
         write(200)  (snicexy(ix,iz,iy),iz = isnoxy(ix,iy)+1,0),&
                     (snliqxy(ix,iz,iy),iz = isnoxy(ix,iy)+1,0),&
                     (radiusxy(ix,iz,iy),iz = isnoxy(ix,iy)+1,0)
         end if
         write(200)(zsnsoxy(ix,iz,iy),iz = isnoxy(ix,iy)+1,nsoil_var(ix,iy))
         write(200)tvxy(ix,iy),tgxy(ix,iy),canicexy(ix,iy),canliqxy(ix,iy),&
                       snowhxy(ix,iy),sneqvxy(ix,iy),waxy(ix,iy),wtxy(ix,iy),&
                       zwtxy(ix,iy)
         write(200) lfmassxy(ix,iy),rtmassxy(ix,iy),&
                    (rootmsxy(ix,iz,iy),iz=1,parameters%nroot),&
                      stmassxy(ix,iy), &
                      woodxy(ix,iy), &
                      STBLCP(ix,iy), &
                      FASTCP(ix,iy), &
                      socxy (ix,iy), &
                      wdocxy(ix,iy), &
                      ddocxy(ix,iy), &
                      micxy (ix,iy), &
                      wenzxy(ix,iy), &
                      denzxy(ix,iy)
         write(200) mqxy(ix,iy),krxy(ix,iy),(frootxy(ix,iz,iy),iz=1,parameters%nroot)
         write(200) (psixy(ix,iz,iy),iz=1,nsoil_var(ix,iy))
         write(200) (wcndxy(ix,iz,iy),iz=1,nsoil_var(ix,iy))
         write(200) htopxy(ix,iy)
         write(200) atm_bcxy(ix,iy)
      end if
     end do
     end do

     close(200)

  100   format('/results/',a,'/ini/Noah.ini.',i4,'0',i1,i2,'2400.nc')
  200   format('/results/',a,'/ini/Noah.ini.',i4,i2,i2,'2400.nc')
  101   format('/results/',a,'/ini/Noah.ini.',i4,'0',i1,i2,'2400.dat')
  201   format('/results/',a,'/ini/Noah.ini.',i4,i2,i2,'2400.dat')
!  300   format(1x,3i5)

   END SUBROUTINE write_ini

! -------------------------------------------------------------------
    integer function lencs (chrstr)

      implicit none
      character(len=*), intent(in) :: chrstr       !input character string
      integer l

      lencs = 0
      do l = len(chrstr),1,-1
         if (chrstr(l:l).ne.' ' .and. chrstr(l:l).ne.char(0)) then
            lencs = l
            goto 10
         end if
      end do
   10 return

     end function lencs

END MODULE module_sf_Noah_NC_output
