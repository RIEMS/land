!###############################################################################
! Module contains all FUN subroutine and functions
MODULE module_sf_biogeochem_fun
  IMPLICIT NONE
  PRIVATE ! Make everything in this module private by default.
  INTEGER, PARAMETER  :: nVegTyp = 20    ! Currently MODIS Veg
  REAL, DIMENSION(nVegTyp) :: CNlf, CNfr, CNlw, CNdw, ra, rb
  DATA CNlf/35, 30, 25, 25, 30, 25, 25, 25, 25, 25, &
    25, 27, 25, 25, 25, 25, 25, 25, 25, 25/
  DATA CNfr/42, 42, 42, 42, 42, 42, 42, 42, 42, 42, &
    25, 45, 42, 42, 42, 25, 25, 42, 42, 42/
  DATA CNlw/50, 50, 50, 50, 50, 50, 50, 50, 50, 0, &
    25, 0, 0, 0, 0, 25, 25, 50, 25, 0/
  DATA CNdw/500, 500, 500, 500, 500, 500, 500, 500, 500, 0, &
    25, 0, 0, 0, 0, 25, 25, 500, 250, 0/
  DATA RA/7, 7, 7, 6, 7, 7, 7, 11, 11, 11, &
    6, 6, 6, 6, 6, 6, 6, 7, 9, 11/
  DATA RB/2, 1, 2, 2, 2, 1.5, 1.5, 2, 2, 2, &
    3, 3, 3, 3, 3, 3, 3, 1.5, 1.8, 2/

! Fixation cost parameters (cost_fixN) based off of Houlton et al. 2008
!   REAL, PARAMETER :: sFIX = -6.25 ! An empirical parameter, it is -6.25 in Houlton et al (2008, Nature).
  REAL, PARAMETER :: aFIX = -3.62
  REAL, PARAMETER :: bFIX = 0.27
  REAL, PARAMETER :: cFIX = 25.15
! Active uptake cost parameters (cost_active)
  REAL, PARAMETER ::   kN = 1.0 ! A parameter for active uptake (gC m-2), Fisher et al (2010, GBC) used 1.0 gC m-2.
  REAL, PARAMETER ::   kC = 1.0 ! A parameter for active uptake (gC m-2), Fisher et al (2010, GBC) used 1.0 gC m-2.
! Resorb cost parameters (cost_resorb)
  REAL, PARAMETER ::   kR = 0.01 ! A parameter for retranslocation (gC m-2), Fisher et al (2010, GBC) used 0.01 gC m-2
!   REAL, PUBLIC    :: SOILN  ! Amount of available soil N [gN m-2]

  PUBLIC  :: FUN
  PRIVATE ::     FUN_COST_ACTIVE
  PRIVATE ::     FUN_COST_FIX
  PRIVATE ::     FUN_COST_RETRAN

CONTAINS
!###############################################################################
!========           SUBROUTINE: FIXATION & UPTAKE OF NITROGEN           ========
!###############################################################################
  SUBROUTINE FUN(ILOC, JLOC, DT, NSOIL, VEGTYP, LFMASS, RTMASS, STMASS, & ! IN
                 WOOD, DIELF, LFTOVR, ZSOIL, SMC, ETRANI, STC, & ! IN
                 NPP, RESP, SOL_NO3, SOL_NH3, NROOT, & ! INOUT
                 NDEM0, NH4up, NO3up, Npassiv, Nactive, Nfix, Nretrans, & ! OUT
                 Nuptake, NPPactv, NPPfix, NPPretr, NPPup, CNlitlf, NLIMIT) ! OUT

! -----------------------------------------------------------------------------------------
! The original code is from Fisher et al. (2010, GBC), first added here with modification
! by X. Cai, 2014
! -----------------------------------------------------------------------------------------
    IMPLICIT NONE
!#--Variables with INTENT (IN, OUT, or INOUT)
    INTEGER, INTENT(IN)    :: ILOC   !grid index
    INTEGER, INTENT(IN)    :: JLOC   !grid index
    REAL, INTENT(IN)    :: DT     ! Time step [sec]
    INTEGER, INTENT(IN)  :: NSOIL  ! No. of soil layers
    INTEGER, INTENT(IN)  :: NROOT  ! Rooting depth [as the number of layers]
    INTEGER, INTENT(IN) :: VEGTYP ! Vegetation type
    REAL, INTENT(IN)    :: LFMASS ! Leaf mass [gC m-2]
    REAL, INTENT(IN)    :: RTMASS ! Mass of fine roots [gC m-2]
    REAL, INTENT(IN)    :: STMASS ! Stem mass [gC m-2], use CNlw
    REAL, INTENT(IN)    :: WOOD   ! Mass of wood (incl. woody roots) [gC m-2]
    REAL, INTENT(IN)    :: DIELF  ! Death of leaf mass per time step [gC m-2]
    REAL, INTENT(IN)    :: LFTOVR ! Leaf turnover per time step [gC m-2]
    REAL, INTENT(IN)    :: ZSOIL(NSOIL)  ! Layer-bottom depth from soil surf [m]
    REAL, INTENT(IN)    :: SMC(NSOIL)    ! Soil moisture content [-]
    REAL, INTENT(IN)    :: STC(NSOIL)    ! Soil temperature [Kelvin]
!   REAL, INTENT(IN)    :: FIXER  ! logical yes or no if ecosystem has fixation.
    ! 0 = non-fixer, 1=fixer
    REAL, INTENT(IN)    :: ETRANI(NSOIL) ! Tranpiration from soil layers [m/s]
    REAL, INTENT(INOUT) :: NPP    ! Net primary production [gC m-2 timestep-1]
    !  On entry, npp has been calculated assuming no N limitation.
    !  On exit, npp has been reduced to account for expenditure on N uptake.
    REAL*8, INTENT(INOUT) :: SOL_NH3(NSOIL + 1)! Amount of nitrogen stored in the ammonium
    ! pool in soil layer [gN m-2]
    REAL*8, INTENT(INOUT) :: SOL_NO3(NSOIL + 1)! Amount of nitrogen stored in the nitrate
    ! pool in soil layer [gN m-2]
    REAL                :: NH4conc(NSOIL) ! Concentration of available NH4 in
    ! soil water [gN/gH2O]
    REAL                :: NO3conc(NSOIL) ! Concentration of available NO3 in
    ! soil water [gN/gH2O]
    REAL                :: NH4avl(NSOIL) ! Amount of NH4 in the soil that is available
    ! to plants (above crit) [gN m-2 layer-1]
    REAL                :: NO3avl(NSOIL) ! Amount of NO3 in the soil that is
    ! to plants (above crit) [gN m-2 layer-1]
    REAL, INTENT(INOUT) :: RESP     ! Leaf respiration [umol/m2/s]
    REAL, INTENT(OUT)   :: NDEM0    ! N demand to grow plants [gN m-2 s-1]
    REAL, INTENT(OUT)   :: NH4up(NSOIL)  ! NH4 uptake [gN m-2]
    REAL, INTENT(OUT)   :: NO3up(NSOIL)  ! NO3 uptake [gN m-2]
    REAL, INTENT(OUT)   :: CNlitlf  ! C:N ratio of leaf litter [gN/gC]
    REAL, INTENT(OUT)   :: Npassiv  ! N acquired by passive uptake [gN m-2 s-1]
    REAL, INTENT(OUT)   :: Nactive  ! N acquired by active uptake [gN m-2 s-1]
    REAL, INTENT(OUT)   :: Nfix     ! N acquired by fixing [gN m-2 s-1]
    REAL, INTENT(OUT)   :: Nretrans ! N acquired by retranslocation [gN m-2 s-1]
    REAL, INTENT(OUT)   :: Nuptake  ! Total N acquired by plants through passive
    ! and active uptake, fixing and retranslocation [gN m-2 s-1]
    REAL, INTENT(OUT)   :: NPPactv  ! NPP reduction to pay for active uptake of N
    ! [gC m-2 s-1]
    REAL, INTENT(OUT)   :: NPPfix   ! NPP reduction to pay for fixing N [gC m-2 s-1]
    REAL, INTENT(OUT)   :: NPPretr  ! NPP reduction to pay for retranslocation of
    ! N [gC m-2 s-1]
    REAL, INTENT(OUT)   :: NPPup    ! Total NPP reduction to pay for N [gC m-2 s-1]
    REAL, INTENT(OUT)   :: NLIMIT   ! N limitation [gN m-2 s-1]
!#--Local variables.
    REAL       :: DZSOIL(NSOIL)     ! Thickness of each soil layer [m]
    INTEGER, SAVE       :: iTS = 1  ! The count of model time step in a FUN time step
    REAL                :: CNplant  ! Average C:N ratio of whole plant [gC gN-1]
    REAL*8     :: REALTEMP          ! A temporary variable with real type.
    REAL, PARAMETER :: smallValue = 1.0E-12 ! A small value.
!   REAL, PARAMETER :: smallValue = 1.0E-10 ! A small value.
    REAL, PARAMETER :: bigCOST = 1.0E10  ! An arbitrary large cost [gC gN-1]
    REAL, PARAMETER :: CNlitt = 3000. ! Maximum allowed value of C:N in leaf litter
    INTEGER, PARAMETER :: NCOST = 4     ! Number of processes through which N
    ! can be extracted. Passive & active uptake, fixing & retranslocation
    INTEGER, PARAMETER :: icostActNH4 = 1 ! Process number for active uptake of NH4
    INTEGER, PARAMETER :: icostActNO3 = 2 ! Process number for active uptake of NO3
    INTEGER, PARAMETER :: icostFix = 3 ! Process number for fixing
    INTEGER, PARAMETER :: icostRetran = 4 ! Process number for retranslocation
    INTEGER, PARAMETER :: FIXER = 1 ! 0=non-fixer; 1=fixer
    REAL :: SWD(NSOIL)    ! Soil water depth [m], measure of the amount of total
    ! soil volume that the plants can take up water from [m].
    REAL :: NPPleft! Remaining (unspent) NPP over the FUN timestep
    ! that has still to be matched with N [gC m-2]
!   REAL :: COSTcheap(NSOIL)  ! Cheapest N cost in each soil layer [gC gN-1]
    REAL :: COST(NSOIL, NCOST)  ! Cost of N via each process [gC gN-1]
    REAL :: COSTacq            ! Integrated resistance of costs [gC gN-1]
    REAL :: COST1              ! For shorter form of COST(IZ,IC) [gC gN-1]
!   REAL :: COSTs(NSOIL,NCOST) ! Sorted COST [gC gN-1]
    REAL :: ROOTfr(NROOT)  ! Fraction of roots in each soil layer with roots
    REAL :: ROOT(NROOT)  ! Root mass in each soil layer with roots [g/m2]
    REAL :: ZS0(NSOIL + 1)  ! Same as ZSOIL but add the land surface as a layer [m]
!   INTEGER :: iminCost(NSOIL) ! Index (in cost) of lowest cost
    INTEGER :: IC   ! Do-loop index with NCOST
    INTEGER :: IZ   ! Do-loop index with NSOIL
    INTEGER :: iCOST(NCOST * NSOIL, 2) ! Indexes of ranked COST
    LOGICAL :: RANKED(NSOIL, NCOST)  ! TRUE when a layer has been ranked
    REAL    :: TsDegC(NSOIL)        ! Soil temperature [degrees C]
    INTEGER :: IMINLOC(2) ! Location (indexes) of the minimum value in COST
    REAL*8  :: DN         ! N acquired from the C expenditure (gN m-2)
    REAL*8  :: DNPP       ! C (NPP) expended in N acquisition (gC m-2)
    REAL    :: NDIELF     ! N stored in leaves that are about to fall off (gN m-2)
    REAL    :: NLFTOVRmin ! Mimimum allowed value of leaf litter N (gN m-2)
    REAL    :: NDEM       ! Updated N demand to grow plants [gN m-2 s-1]
    REAL, PARAMETER :: CNTOVRmax = 3000. ! Maximum allowed value of C:N in leaf litter
    REAL, PARAMETER :: MPE = 1.E-6 ! Prevents overflow error if division by zero
    REAL    :: NPP0       ! Incoming NPP (gC m-2)
    REAL*8  :: XX         ! A temparary variable

    REAL :: sFIX = -62.5
!   REAL, DIMENSION(4) :: FR_N_Z
!   DATA FR_N_Z / 0.11, 0.26, 0.35, 0.28 / ! Estimated from Eqn. (3.1.1.1) or
    ! Fig. 3.:1-3 of SWAT Theoretical Documentation V2009 (Neitsch et al., 2011).
    real*8  :: ynh3(20), yno3(20)        !-XTC-WRITE

    ynh3(:) = 0.                        !-XTC-WRITE
    yno3(:) = 0.                        !-XTC-WRITE
    DO IZ = 1, NSOIL                        !-XTC-WRITE
      ynh3(1) = ynh3(1) + SOL_NH3(IZ)   !-XTC-WRITE
      yno3(1) = yno3(1) + SOL_NO3(IZ)   !-XTC-WRITE
    ENDDO                                !-XTC-WRITE

! Calculate the thichness of each soil layer
    DZSOIL(1) = -ZSOIL(1)
    DO IZ = 2, NSOIL
      DZSOIL(IZ) = ZSOIL(IZ - 1) - ZSOIL(IZ)
      NO3avl(IZ) = SOL_NO3(IZ + 1)
      NH4avl(IZ) = SOL_NH3(IZ + 1)
    ENDDO
    ZS0(1) = 0.0  ! First layer is the land surface.
    ZS0(2:NSOIL + 1) = -ZSOIL
    NO3avl(1) = SOL_NO3(1) + SOL_NO3(2)
    NH4avl(1) = SOL_NH3(1) + SOL_NH3(2)

! Convert soil moisture to water depth; soil N to concertration.
    DO IZ = 1, NSOIL ! Actually only NROOT layers with values.
      SWD(IZ) = SMC(IZ) * DZSOIL(IZ) + ETRANI(IZ) * DT
!      NH4avl(IZ)   = SOILN / 2. * FR_N_Z(IZ)
!      NO3avl(IZ)   = SOILN / 2. * FR_N_Z(IZ)
      NH4conc(IZ) = NH4avl(IZ) / SWD(IZ)
      NO3conc(IZ) = NO3avl(IZ) / SWD(IZ)
    ENDDO

!############### Start to Calculate Nitrogen Uptake & Fixation  ################
! Initialise output fluxes.
    Nactive = 0.0
    Nfix = 0.0
    Npassiv = 0.0
    Nretrans = 0.0
    Nuptake = 0.0
    NPPactv = 0.0
    NPPfix = 0.0
    NPPretr = 0.0
    NPPup = 0.0
    NH4up = 0.0
    NO3up = 0.0
    NLIMIT = 0.0
! Initialise leaf litter C:N to leaf value.
    CNlitlf = CNlf(VEGTYP)

    NPP0 = NPP !--FUNTEST

! Calculate C:N ratio of whole plant, and the associated N demand.
    CNplant = (CNlf(VEGTYP) * LFMASS + CNfr(VEGTYP) * RTMASS + CNfr(VEGTYP) * STMASS &
               + CNlw(VEGTYP) * WOOD) / (LFMASS + RTMASS + STMASS + WOOD)
    NDEM0 = NPP / CNplant ! This N demand is only for output.
    NDEM = NDEM0

! Calculate passive N update by transpiration stream from each soil layer.
    DO IZ = 1, NSOIL
      NH4up(IZ) = NH4conc(IZ) * ETRANI(IZ) * DT
      NO3up(IZ) = NO3conc(IZ) * ETRANI(IZ) * DT
      NH4avl(IZ) = NH4avl(IZ) - NH4up(IZ)
      NO3avl(IZ) = NO3avl(IZ) - NO3up(IZ)
      Npassiv = Npassiv + (NH4up(IZ) + NO3up(IZ)) / DT !--FUNTEST
    ENDDO
!#--If Npassiv > NDEM, return excess N to soil (via whatever mechanisms, e.g.
!   diffusion). Add back to NO3 and NH4 pools proportionally to extraction.
    IF (Npassiv > NDEM .AND. Npassiv > 0.) THEN
      DO IZ = 1, NSOIL
        NH4up(IZ) = NH4up(IZ) * NDEM / Npassiv
        NO3up(IZ) = NO3up(IZ) * NDEM / Npassiv
        ! Fraction of the returned ETRANI to the soil water in the layer.
        REALTEMP = (1.0 - NDEM / Npassiv) * ETRANI(IZ) * DT / SWD(IZ)
        ! Update N concentration in soil.
        NH4conc(IZ) = NH4conc(IZ) * (1.0 + REALTEMP)
        NO3conc(IZ) = NO3conc(IZ) * (1.0 + REALTEMP)
        NH4avl(IZ) = NH4conc(IZ) * (SWD(IZ) - ETRANI(IZ) * DT)
        NO3avl(IZ) = NO3conc(IZ) * (SWD(IZ) - ETRANI(IZ) * DT)
        ! N immobilization in dry soil.
        IF (SWD(IZ) <= smallValue) THEN
          NH4up(IZ) = 0.0
          NO3up(IZ) = 0.0
        ENDIF
      ENDDO
      Npassiv = NDEM

    ELSE ! Npassiv < NDEM
!################ ACTIVE Uptake, Fixation, & RETRANSLOCATION  #################
! If passive uptake is insufficient, consider fixation, active uptake & retranslocation.
      NLFTOVRmin = LFTOVR / CNTOVRmax
      NDIELF = (MAX(DIELF, 0.) + LFTOVR) / CNlf(VEGTYP) ! Use CNlitlf rather than CNlf?
      ! (DIELF+LFTOVR) vs. DIELF
!      NDIELF     = MAX(DIELF,0.) / CNlf(VEGTYP)
      NDEM = NDEM - Npassiv
      NLIMIT = NDEM

! Calculate root distribution in each soil layer (Zeng, 2001, J Hydromet.).
      REALTEMP = 0.0 ! Total fraction in the first trial estimation.
      DO IZ = 1, NROOT
        IF (IZ < NROOT) THEN
          ROOTfr(IZ) = 0.5 * (EXP(-RA(VEGTYP) * ZS0(IZ)) + EXP(-RB(VEGTYP) * ZS0(IZ)) &
                              - EXP(-RA(VEGTYP) * ZS0(IZ + 1)) - EXP(-RB(VEGTYP) * ZS0(IZ + 1)))
        ELSE
          ROOTfr(IZ) = 0.5 * (EXP(-RA(VEGTYP) * ZS0(IZ)) + EXP(-RB(VEGTYP) * ZS0(IZ)))
        ENDIF
        REALTEMP = REALTEMP + ROOTfr(IZ)
      ENDDO

      DO IZ = 1, NROOT
        ROOTfr(IZ) = ROOTfr(IZ) / REALTEMP
        ROOT(IZ) = ROOTfr(IZ) * RTMASS
        TsDegC(IZ) = STC(IZ) - 273.15
      ENDDO

      NPPleft = NPP - Npassiv * CNplant

      DO IZ = 1, NSOIL
!#--Cost of active uptake.
        IF (IZ > NROOT) THEN
          COST(IZ, icostActNo3) = bigCOST
        ELSE
          COST(IZ, icostActNo3) = FUN_COST_ACTIVE(NO3avl(IZ), bigCOST, &
                                                  DZSOIL(IZ), kC, kN, RTMASS, ROOTfr(IZ), smallValue)
        ENDIF
      ENDDO
      COST(:, icostActNH4) = bigCOST  !-XTC-20150101 Ammonium may not be taken up directly
!#--Cost of fixation.
      COST(1, icostFix) = FUN_COST_FIX(FIXER, aFIX, bFIX, cFIX, bigCOST, &
                                       ROOTfr(1), sFIX, TsDegC(1), VEGTYP)

!#--Cost of retranslocation.
!   This is set to the same value for all layers for ease of use, although in
!   fact it does not depend on layer. This is also the demand for future N, to
!   replace this leaf... 'nitrogen maintenance demand'.
      COST(:, icostRetran) = FUN_COST_RETRAN(DT, NSOIL, bigCOST, kR, NDIELF, &
                                             smallValue)

! Integrated resistance (Brzostek et al, 2014, JGR).
! Same as Ohm's law: DN = DNPP / COSTacq
! Resistors in parallel: 1/COSTacq = 1/COSTfix + 1/COSTactive + 1/COSTresorb
      XX = 0.
      DO IZ = 1, NSOIL
        XX = XX + 1./COST(IZ, icostActNo3)
      ENDDO
      XX = XX + 1./COST(1, icostFix) + 1./COST(1, icostRetran)
      COSTacq = 1./XX
! Carbon spent on N acquisition (DNPP, like the voltage in Ohm's law) is
! calculated by simultaneously solving eq 6b-6d from the Fisher et al. (2010).
      DNPP = NPPleft / (1.0 + CNplant / COSTacq)
      DN = DNPP / COSTacq
! N uptake from each pathway is calculated as following. N uptake is truncated
! if N demand is met or leafN pool or soilN pool is exhausted.  When using the
! iterative cost functions N uptake is truncated when the portion of the leafN
! or soilN pool in that iteration is exhausted.
! If use active NO3 uptake, decrement available store and update concentration.
      DO IZ = 1, NSOIL
        DN = DNPP / COST(IZ, icostActNo3)
        REALTEMP = NO3avl(IZ) * 0.5 ! Assume plants can only use up to 50% of
        ! soil N from each soil layer.
        IF (DN > REALTEMP) THEN
          DN = REALTEMP
          REALTEMP = DNPP * COSTacq / COST(IZ, icostActNo3)
        ENDIF
        NO3avl(IZ) = NO3avl(IZ) - DN * DT
        IF (SWD(IZ) > smallValue) THEN
          NO3conc(IZ) = NO3avl(IZ) / SWD(IZ)
        ELSE
          NO3conc(IZ) = 0.0
        ENDIF
        Nactive = Nactive + DN
        NPPactv = NPPactv + DNPP
      END DO
! Use fixation.
      DN = DNPP / COST(1, icostFix)
      Nfix = Nfix + DN
      NPPfix = DNPP * COSTacq / COST(1, icostFix)
! If use retranslocation, update N in falling leaves.
      DN = DNPP / COST(1, icostRetran)
      NPPretr = DNPP * COSTacq / COST(1, icostRetran)
      REALTEMP = MIN(NDIELF * 0.5, NDIELF - NLFTOVRmin)
      IF (DN > REALTEMP) THEN
        DN = REALTEMP
      ENDIF
      Nretrans = Nretrans + DN
      NDIELF = NDIELF - DN

! Update NPPleft and NDEM
      NPPleft = NPPleft - DNPP - DN * CNplant
      NDEM = NDEM - (Npassiv + Nactive + Nfix + Nretrans)

! Decrement NPP by amount spent on N. Add C spent on N uptake to plant respiration.
      NPP = NPP - DNPP
      RESP = RESP + DNPP

! N limitation of plants.
      NLIMIT = NDEM
!      NPP    = MAX(NPP-NPPleft, 0.)

    ENDIF ! Npassiv >= NDEM

    Nuptake = Npassiv + Nactive + Nfix + Nretrans
    NPPup = NPPactv + NPPfix + NPPretr
!      SOILN   = SOILN   + Nuptake

! Update the C:N ratio of the litter.
    IF (LFTOVR > 0.0) CNlitlf = LFTOVR / MAX(NDIELF, MPE)

! Update the soil nitrate and ammonium.
    DO IZ = 2, NSOIL
      SOL_NO3(IZ + 1) = NO3avl(IZ)
      SOL_NH3(IZ + 1) = NH4avl(IZ)
    ENDDO
    XX = SOL_NO3(1) + SOL_NO3(2) - NO3avl(1)
    SOL_NO3(1) = SOL_NO3(1) - XX * (0.01 / DZSOIL(1)) ! 0.01 m is depth of surface layer
    SOL_NO3(2) = SOL_NO3(2) - XX * ((DZSOIL(1) - 0.01) / DZSOIL(1))
    XX = SOL_NH3(1) + SOL_NH3(2) - NH4avl(1)
    SOL_NH3(1) = SOL_NH3(1) - XX * (0.01 / DZSOIL(1))
    SOL_NH3(2) = SOL_NH3(2) - XX * ((DZSOIL(1) - 0.01) / DZSOIL(1))

    DO IZ = 1, NSOIL                        !-XTC-WRITE
      ynh3(2) = ynh3(2) + SOL_NH3(IZ)   !-XTC-WRITE
      yno3(2) = yno3(2) + SOL_NO3(IZ)   !-XTC-WRITE
    ENDDO                                !-XTC-WRITE
    ynh3(3) = ynh3(1) - ynh3(2)          !-XTC-WRITE
    yno3(3) = yno3(1) - yno3(2)          !-XTC-WRITE
    XX = 60.*30.
9200 FORMAT(999(E22.14, ',', :))            !-XTC-WRITE

  END SUBROUTINE FUN

!###############################################################################
!========          FUNCTION: COST OF ACTIVE UPTAKE OF NITROGEN          ========
!###############################################################################
  REAL FUNCTION FUN_COST_ACTIVE(availN, bigCOST, dzSoilC, kc_active, kn_active, &
                                root, rootFrac, smallValue)
!-----------------------------------------------------------------------
! Function to calculate the cost of active uptake of NH4 or NO3 from a layer.
!-----------------------------------------------------------------------
    IMPLICIT NONE
!--------------------------------------------------------------------------
! Scalar arguments with intent(in).
!--------------------------------------------------------------------------
    REAL, INTENT(in) :: availN   !  amount of N (as NH4 or NO3 ) in the soil
!                                 that is available to plants (gN m-2)
    REAL, INTENT(in) :: bigCOST  !  an arbitrary large cost (gC/gN)
    REAL, INTENT(in) :: dzSoilC  !  thickness of soil layer (m)
    REAL, INTENT(in) :: kc_active!  Constant for cost of active uptake (gC m-2)
    REAL, INTENT(in) :: kn_active!  Constant for cost of active uptake (gC m-2)
    REAL, INTENT(in) :: root     !  root biomass (gC m-2)
    REAL, INTENT(in) :: rootFrac !  fraction of roots that are in this layer
    REAL, INTENT(in) :: smallValue !  a small number

!--------------------------------------------------------------------------
    IF (rootFrac > 1.0e-4) THEN

      IF (availN > smallValue) THEN
        FUN_COST_ACTIVE = kn_active * dzsoilC / (availN / 1000.) * &
                          kc_active / (rootFrac * (root / 1000.) / dzSoilC)
        ! Why dzSoilC is added to the equation?--XTC
      ELSE
        FUN_COST_ACTIVE = bigCOST   !  arbitrary large number
      ENDIF

    ELSE
!   There are very few roots in this layer. Set a high cost.
      FUN_COST_ACTIVE = bigCOST

    ENDIF

  END FUNCTION FUN_COST_ACTIVE
!###############################################################################
!========               FUNCTION: COST OF FIXING NITROGEN               ========
!###############################################################################
  REAL FUNCTION FUN_COST_FIX(fixer, a_fix, b_fix, c_fix, bigCOST, rootFrac, s_fix &
                             , tsoilDegC, VEGTYP)
!-----------------------------------------------------------------------
! Function to calculate the cost of fixing N by nodules.
!-----------------------------------------------------------------------
    IMPLICIT NONE
!--Scalar arguments with intent(in).
    INTEGER, INTENT(in) :: fixer  !  flag indicating if plant is a fixer
!                                  1=yes, otherwise no.
    REAL, INTENT(in) :: a_fix     !  As in Houlton et al. (Nature) 2008
    REAL, INTENT(in) :: b_fix     !  As in Houlton et al. (Nature) 2008
    REAL, INTENT(in) :: c_fix     !  As in Houlton et al. (Nature) 2008
    REAL, INTENT(in) :: bigCOST   !  an arbitrary large cost (gC/gN)
    REAL, INTENT(in) :: rootFrac  !  fraction of roots that are in this layer
    REAL :: s_fix     !  Inverts Houlton et al. 2008 and constrains
!                                  between 7.5 and 12.5
    REAL, INTENT(in) :: tsoilDegC !  soil temperature (degrees Celsius)
    INTEGER, INTENT(IN) :: VEGTYP ! Vegetation type

!--------------------------------------------------------------------------
    IF (VEGTYP == 10) THEN
      s_fix = -30
    ELSE IF (VEGTYP == 12 .or. VEGTYP == 14 .or. VEGTYP == 8 .or. VEGTYP == 9) THEN
      s_fix = -40
    ENDIF

    IF (fixer == 1) THEN

      IF (rootFrac > 1.0e-4) THEN
        FUN_COST_FIX = s_fix * EXP(a_fix + b_fix * tsoilDegC &
                                   * (1.0 - 0.5 * tsoilDegC / c_fix)) - 2.0 * s_fix
      ELSE
!     There are very few roots in this layer.
!     Set cost to an arbitrary high value.
        FUN_COST_FIX = bigCOST
      ENDIF

    ELSE
!   Plant is not a fixer. Set cost to an arbitrary high value.
      FUN_COST_FIX = bigCOST
!    RESULT = cost_of_n

    ENDIF

  END FUNCTION FUN_COST_FIX
!###############################################################################
!========         FUNCTION: COST OF RETRANSLOCATION OF NITROGEN         ========
!###############################################################################
  REAL FUNCTION FUN_COST_RETRAN(dt, nzSoilC, bigCOST, kr_resorb, n_falling_leaves &
                                , smallValue)
!-----------------------------------------------------------------------
! Function to calculate the cost of retranslocation of N from leaves.
!    Although the cost is not associated with layers, an array rather than
!    scalar is used to allow this cost to be compared in another part of
!    FUN with other costs that do vary between layers.
!-----------------------------------------------------------------------
    IMPLICIT NONE
!--Scalar arguments with intent(in).
    REAL, INTENT(IN)    :: DT     ! Time step [sec]
    INTEGER, INTENT(in) :: nzSoilC       !  number of layers
    REAL, INTENT(in) :: bigCOST          !  an arbitrary large cost (gC/gN)
    REAL, INTENT(in) :: kr_resorb        !  Cost of retranslocation of N
!                                           from falling leaves (gC/gN)
    REAL, INTENT(in) :: n_falling_leaves !  N stored in leaves that are
!                                           about to fall off (gN m-2)
    REAL, INTENT(in) :: smallValue       !  a small number
!--------------------------------------------------------------------------

    IF (n_falling_leaves > smallValue) THEN
!     FUN_COST_RETRAN = (kr_resorb*dt/3600./24./365.) / (n_falling_leaves/1000.)
      FUN_COST_RETRAN = (kr_resorb * dt / 3600) / (n_falling_leaves / 1000.)
!    FUN_COST_RETRAN = 20.
    ELSE
!   Little N available. Set cost to an arbitrary high value.
      FUN_COST_RETRAN = bigCOST
    ENDIF

  END FUNCTION FUN_COST_RETRAN
!###############################################################################
END MODULE module_sf_biogeochem_fun
!###############################################################################
!========                   MODULE: module_sf_biogeochem_ncycle                   ========
!###############################################################################
MODULE module_sf_biogeochem_ncycle

  IMPLICIT NONE
!   PRIVATE ! Make everything in this module private by default.

  PUBLIC  :: NCYCLE
  PRIVATE ::     ysed         !! sediment
  PRIVATE ::     nminrl       !! mineralization and immobilization
  PRIVATE ::     nitvol       !! daily nitrification (NH3 to NO3) and volatilization of NH3
  PRIVATE ::     enrsb        !! if (runsrf > 0. .and. peakr > 1.e-6 .and. precipday > 0.)
  PRIVATE ::     orgn         !! the amount of organic nitrogen removed in surface runoff
  PRIVATE ::     nrain        !! add nitrate in rainfall to soil profile
  PRIVATE ::     nlch         !! compute nitrate movement leaching

!! name               |units         |definition
!! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!--PARAMETERS
  REAL               :: &!Parameters
    cmn, &!mg N /l      !Rate factor for humus mineralization of active organic nitrogen.
    !MIN: 0.001   MAX: 0.003   DEFAULT: 0.003
    drydep_nh4, &!g/m2/yr      !atmospheric dry deposition of ammonia
    !MIN: 0      MAX: 0.25   DEFAULT: 0
    drydep_no3, &!g/m2/yr      !atmospheric dry deposition of nitrates
    !MIN: 0      MAX: 0.3    DEFAULT: 0
    rammo, &!mg N /l      !Atmospheric deposition of ammonium
    !MIN: 0      MAX: 2.5    DEFAULT: 0
    rcn, &!mg N /l      !Concentration of nitrogen in the rainfall
    !MIN: 0      MAX: 2.5    DEFAULT: 0
    nperco, &!none         !nitrate percolation coefficient (0-1)
    !MIN: 0      MAX: 1.0    DEFAULT: 0
    !0:concentration of nitrate in surface runoff
    !  is zero; 1:surface runoff has same concentration
    !  of nitrate as percolate
    nactfr              !none         !nitrogen active pool fraction. The fraction of
  !  organic nitrogen in the active pool.

  INTEGER, PARAMETER            :: INUM = 1000
  INTEGER, PARAMETER            :: JNUM = 1000
  INTEGER, PARAMETER :: NSOL = 5    !Number of soil layer
  INTEGER, PARAMETER :: MVT = 20   !Number of soil type
  INTEGER, PARAMETER :: R8 = SELECTED_REAL_KIND(14) !Kind number of 8 byte real

!!--SOIL CHARACTERISTICS INPUTS
  REAL               :: &
    FERTN(INUM, JNUM), &!g/m2         !N fertilizer
    SBD(INUM, NSOL, JNUM), &!Mg/m**3      !bulk density of the soil
    AWC(INUM, NSOL, JNUM), &!mm mm-1      !Available water capacity of the soil layer
    OCC(INUM, NSOL, JNUM), &!% soil mass  !Organic carbon content
    CLAY(INUM, NSOL, JNUM), &!% soil mass  !Clay content
    SILT(INUM, NSOL, JNUM), &!% soil mass  !Silt content
    SAND(INUM, NSOL, JNUM), &!% soil mass  !Sand content
    ROCK(INUM, NSOL, JNUM), &!% soil mass  !Rock content
    USLE_MU(INUM, JNUM), &!-            !Product of USLE K,P,LS,exp(rock)
    USLE_K(INUM, NSOL, JNUM), &!-            !USLE equation soil erodibility (K) factor
    USLE_L(INUM, JNUM), &!m            !USLE equation slope length (L) factor
    USLE_P(INUM, JNUM), &!-            !USLE equation support practice (P) factor
    USLE_S(INUM, JNUM), &!%(m/m)       !USLE equation slope steepness (S) factor
    USLE_C(MVT), &!-            !minimum value of the USLE C factor for water erosion
    DET_SAG(INUM, JNUM), &!-            !Fraction of detached sediment as sand.
    DET_SIL(INUM, JNUM), &!-            !Fraction of detached sediment as silt.
    DET_CLA(INUM, JNUM), &!-            !Fraction of detached sediment as clay.
    DET_SAN(INUM, JNUM), &!-            !Small aggregate fraction.
    DET_LAG(INUM, JNUM), &!-            !Large aggregate fraction.
    ANION_EXCL, &!none         !fraction of porosity from which anions are excluded
    RSDCOPL(MVT)        !none         !plant residue decomposition coefficient.
  !The fraction of residue which will decompose in
  !  a day assuming optimal moisture,
  !  temperature, C:N ratio, and C:P ratio
  COMMON SBD, AWC, OCC, CLAY, SILT, SAND, ROCK, USLE_MU, USLE_K, USLE_L, USLE_P, USLE_S, USLE_C, ANION_EXCL, &
    DET_SAG, DET_SIL, DET_CLA, DET_SAN, DET_LAG, RSDCOPL, FERTN

CONTAINS
!###############################################################################
!========      SUBROUTINE: TOP LEVEL OF THE SOIL NITROGEN DYNAMICS      ========
!###############################################################################
  SUBROUTINE NCYCLE(ILOC, JLOC, DT, YEARLEN, JULIAN, DAYLEN, DAYLTH, NSNOW, NSOIL, & ! IN
                    VEGTYP, PRCP, RUNSRF, RUNSBZ, SMCWLT, SMCMAX, SMCREF, & ! IN
                    SNEQV, LFTOVR, TOTLB, SMC, STC, ZSOIL, WFLUX, & ! IN
                    SOL_AON, SOL_FON, SOL_NH3, SOL_NO3, SOL_RSD, SOL_SON, & ! INOUT
                    LATNO3, NO3PCP, PERCN, SEDORGN, SEDYLD, SURQNO3, HMN, & ! OUT
                    RMN, RWN, WDN, SUMNO3, SUMORGN, RNIT, RVOL, & ! OUT
                    CNO3, LAT, LON) ! OUT

! -----------------------------------------------------------------------------------------
! The original code is from Neitsch et al. (2011, SWAT Theoretical Documentation), first
! added here with modification by X. Cai, 2014
! -----------------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER, INTENT(IN)    :: ILOC   !grid index
    INTEGER, INTENT(IN)    :: JLOC   !grid index
    INTEGER, INTENT(IN) :: &
      nsnow, &!-            !maximum no. of snow layers
      nsoil, &!-            !!number of soil layers
      vegtyp, &!none         !land cover type
      yearlen             !day          !Number of days in the particular year

    REAL, INTENT(IN)    :: &
      dt, &!s            !time step [sec]
      julian, &!day          !Julian day of year (fractional, 0 <= JULIAN < YEARLEN)
      daylen, &!hr           !day length
      lat, &!degree       !latitude
      lon, &!degree       !longtitude
      daylth, &!hr           !threshold daylength to initiate dormancy
      lftovr, &!gC m-2       !leaf turnover per time step
      prcp, &!g m-2 s-1    !precipitation rate
      runsrf, &!mm/s         !surface runoff
      smcmax(nsol - 1), &!-            !porosity, saturated value of soil moisture (volumetric)
      smcref(nsol - 1), &!-            !reference soil moisture (field capacity) (volumetric)
      smcwlt(nsol - 1), &!-            !wilting point soil moisture (volumetric)
      sneqv, &!mm H2O       !amount of water in snow in HRU on current day
      totlb, &!g/m2         !total living carbon
      runsbz(nsoil), &!mm/s         !lateral flow (sturation excess)
      smc(nsoil), &!-            !soil moisture content
      stc(-NSNOW + 1:NSOIL), &!Kelvin      !soil temperature
      wflux(nsoil), &!m/s          !water flux between soil layers
      zsoil(nsoil)        !m            !layer-bottom depth from soil surf

    REAL*8, INTENT(INOUT):: &
      sol_aon(nsol), &!g N/m2       !amount of nitrogen stored in the active
      !organic (humic) nitrogen pool
      sol_fon(nsol), &!g N/m2       !amount of nitrogen stored in the fresh
      !organic (residue) pool
      sol_nh3(nsol), &!g N/m2       !amount of nitrogen stored in the ammonium pool
      !in soil layer
      sol_rsd(nsol), &!g/m2         !amount of organic matter in the soil
      !classified as residue
      sol_no3(nsol), &!g N/m2       !amount of nitrogen stored in the nitrate pool
      !in soil layer
      sol_son(nsol)       !g N/m2       !amount of nitrogen stored in the stable
    !organic N pool

    REAL, INTENT(OUT)   :: &
      cno3(nsol), &!g N/mm H2O   !concentration of nitrate in leached solution
      latno3, &!g N/m2       !amount of nitrate transported with lateral flow
      no3pcp, &!g N/m2       !nitrate added to the soil in rainfall
      percn, &!g N/m2       !amount of nitrate percolating past bottom
      !of soil profile
      sedorgn, &!g N/m2       !amount of organic nitrogen in surface runoff
      sedyld, &!g/m2         !daily soil loss caused by water erosion
      surqno3, &!g N/m2       !amount of nitrate transported with surface runoff
      hmn, &!g N/m2       !amount of nitrogen moving from active
      !organic to nitrate pool in soil profile
      rmn, &!g N/m2       !amount of nitrogen moving from the fresh
      !organic (residue) to the nitrate(80%) and
      !active organic(20%) pools in soil profile
      rwn, &!g N/m2       !amount of nitrogen moving from active
      !organic to stable organic pool in soil profile
      wdn, &!g N/m2       !amount of nitrogen lost from nitrate pool
      !by denitrification in soil profile on
      rnit, &!g N/m2       !amount of nitrogen moving from the NH3 to the
      !NO3 pool due to nitrification
      rvol, &!g N/m2       !amount of nitrogen lost from the NH3 pool due
      !to volatilization
      sumno3, &!g N/m2       !amount of nitrogen stored in the nitrate pool
      !in the soil profile
      sumorgn             !g N/m2       !amount of nitrogen stored in the organic N pools
    !in the soil profile

    INTEGER             :: iz, iz0

    REAL                :: &!Local
      cklsp, &!             !product of USLE C,P,LS,K,exp(rock)
      erorgn, &!none         !organic N enrichment ratio, if left blank
      !the model will calculate for every event
      enratio, &!none         !enrichment ratio
      peakr, &!mm/s         !peak runoff rate
      rsdco_pl, &!none         !rsdcopl(VEGTYP), rate coefficient for mineralization
      !of the residue fresh organic nutrients
      sol_cov, &!g/m2         !amount of residue on soil surface
      surfq, &!mm H2O       !surface runoff generated on each time step
      uslec, &!-            !minimum value of the USLE C factor for water erosion
      latf(nsol), &!mm/s         !lateral flow (sturation excess)
      sol_bd(nsol), &!Mg/m**3      !bulk density of the soil
      sol_cbn(nsol), &!%            !percent organic carbon in soil layer
      sol_prk(nsol), &!mm H2O       !percolation from soil layer
      sol_fc(nsol), &!mm H2O       !amount of water available to plants in soil
      !layer at field capacity (fc - wp)
      sol_wp(nsol), &!mm H2O       !water content of  at -1.5 MPa (wilting point)
      sol_st(nsol), &!mm H2O       !amount of water stored in the soil layer (less wp water)
      sol_ul(nsol), &!mm H2O       !amount of water held in the soil layer at saturation
      sol_tmp(nsol), &!deg C        !daily average temperature of soil layer
      sol_dz(nsol), &!mm           !thickness of each soil layer
      sol_z(nsol)         !m            !depth to bottom of soil layer

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
    real*8  :: xj(4) !-XTC-WRITE  sedorgn*(sol_aon(1)/xx)
    real*8  :: xaon(20), xfon(20), xson(20), xnh3(20), xrsd(20), xno3(20) !-XTC-WRITE
    integer, save :: kk = 1 !-XTC-WRITE
!   IF ( MOD( kk, 96 ) == 1 ) THEN                                      !-XTC-WRITE
    xj(:) = 0.                                                         !-XTC-WRITE
    xaon(:) = 0.                                                      !-XTC-WRITE
    xfon(:) = 0.                                                      !-XTC-WRITE
    xson(:) = 0.                                                      !-XTC-WRITE
    xnh3(:) = 0.                                                      !-XTC-WRITE
    xrsd(:) = 0.                                                      !-XTC-WRITE
    xno3(:) = 0.                                                      !-XTC-WRITE
!   END IF                                                             !-XTC-WRITE
!print *,"bio, fert, bd_1",FERTN(ILOC,JLOC),ILOC,JLOC,SBD(ILOC,1,JLOC)
!IF(ILOC == 150 .and. JLOC == 100) THEN
!     !write(*,*)"sol_cbn,sol_bd,clay,usle_l",OCC(ILOC,1,JLOC),SBD(ILOC,1,JLOC),CLAY(ILOC,1,JLOC),USLE_L(ILOC,JLOC)
!         print *,"rsdcopl",RSDCOPL
!         print *,"usle_c",USLE_C
!         print *,"cmn,rammo,rcn",cmn,rammo,rcn
!ENDIF
    usle_c = (/0.001, 0.001, 0.001, 0.001, 0.001, 0.003, 0.003, 0.003, 0.003, 0.003, &
               0.002, 0.2, 0.01, 0.006, 0.00, 0.001, 0.00, 0.003, 0.003, 0.003/)
    rsdcopl = (/0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, &
                0.05, 0.05, 0.05, 0.05, 0.05, 0.00, 0.05, 0.05, 0.05, 0.05/)

    cmn = 0.003
    drydep_nh4 = 0.2
    drydep_no3 = 0.2
    rammo = 1.0
    rcn = 1.5
    nperco = 0.3
    nactfr = 0.4

!   print *,"after define"
!   print *,"rsdcopl",RSDCOPL
!   print *,"usle_c",USLE_C
!   print *,"cmn,rammo,rcn",cmn,rammo,rcn

!! Add 10mm layer at surface of soil
    DO iz = nsol, 1, -1
      iz0 = iz - 1
      IF (iz == 1) iz0 = 1
      latf(iz) = runsbz(iz0)
      sol_tmp(iz) = stc(iz0) - 273.15 !Kelvin -> deg C
      sol_z(iz) = -zsoil(iz0) ! zsoil is negative.
      sol_prk(iz) = wflux(iz0)
    END DO
    sol_z(1) = 0.010 ! 10 mm
    sol_z(2) = sol_z(2)
    sol_prk(1) = -sol_prk(1)
    sol_prk(2) = -sol_prk(2)
!! Calculate the thichness of each soil layer
    sol_dz(1) = sol_z(1)
    DO iz = 2, nsol
      sol_dz(iz) = sol_z(iz) - sol_z(iz - 1)
    ENDDO
    sol_dz = sol_dz * 1000. !unit from m to mm.
    latf(1) = runsbz(1) * sol_z(1) / (-zsoil(1))
    latf(2) = runsbz(1) - latf(1)

    sol_wp(1) = smcwlt(1) * sol_dz(1)
    sol_ul(1) = smcmax(1) * sol_dz(1)
    sol_fc(1) = smcref(1) * sol_dz(1) - sol_wp(1)
    sol_st(1) = smc(1) * sol_dz(1) - sol_wp(1)
    DO iz = 2, nsol
      sol_wp(iz) = smcwlt(iz - 1) * sol_dz(iz)
      sol_ul(iz) = smcmax(iz - 1) * sol_dz(iz)
      sol_fc(iz) = smcref(iz - 1) * sol_dz(iz) - sol_wp(iz)
      sol_st(iz) = smc(iz) * sol_dz(iz) - sol_wp(iz)
    ENDDO

    surfq = runsrf * dt
    latf = latf * dt
    sol_prk = sol_prk * dt * 1000.*(-1.) !m to mm, positive upward to positive downward

    uslec = usle_c(VEGTYP)
    peakr = runsrf * MAX(dt / 600., 1.5) !-XTC: Assume peakr is at least 1.5 times higher
    !      than runsrf, depending on model time step
    !! calculate residue on soil surface
    sol_cov = Max(.8 * totlb + sol_rsd(1), 0.)
    CALL ysed(ILOC, JLOC, peakr, sneqv, sol_cov, runsrf, uslec, usle_mu(ILOC, JLOC), &! IN
              cklsp, sedyld)! OUT

    CALL fert(dt, vegtyp, yearlen, julian, sol_dz, lat, lon, fertn(ILOC, JLOC), &! IN
              sol_aon, sol_fon, sol_rsd, sol_nh3, sol_no3, xnh3, xno3)! INOUT !-XTC-WRITE  xnh3, xno3

    CALL dorm(vegtyp, daylen, daylth, totlb, &! IN
              sol_fon, sol_rsd, xfon, xrsd)! INOUT !-XTC-WRITE  xfon

!! Compute nitrogen mineralization
    rsdco_pl = rsdcopl(VEGTYP)
    sol_cbn = occ(ILOC, :, JLOC)
    sol_rsd(1) = sol_rsd(1) + lftovr * dt * 0.65 !65% leaf turnover is returned to soil
    sol_fon(1) = sol_fon(1) + lftovr * dt * 0.65 * 0.053
!   xaon(4) = xj                                                            !-XTC-WRITE
!   xaon(5) = sol_aon(1)+sol_aon(2)+sol_aon(3)+sol_aon(4)+sol_aon(5)        !-XTC-WRITE
!   IF ( MOD( kk, 96 ) == 0 ) THEN                                       !-XTC-WRITE
    CALL nminrl(iloc, jloc, dt, vegtyp, rsdco_pl, sol_cbn, sol_fc, sol_st, sol_tmp, &! IN
                sol_aon, sol_fon, sol_no3, sol_son, sol_rsd, &! INOUT
                hmn, rmn, rwn, wdn, xaon, xfon, xson, xno3, xrsd)! OUT   !-XTC-WRITE  xaon, xfon, xson, xno3

!   xaon(4) = xj                                                            !-XTC-WRITE
!   xaon(5) = sol_aon(1)+sol_aon(2)+sol_aon(3)+sol_aon(4)+sol_aon(5)        !-XTC-WRITE

    CALL nitvol(dt, sol_fc, sol_st, sol_tmp, sol_wp, sol_z, &! IN
                sol_nh3, sol_no3, xnh3, xno3, &! INOUT !-XTC-WRITE  xnh3, xno3
                rnit, rvol)! OUT

!! Compute enrichment ratio for nutrient and organic nitrogen removed in surface runoff
    erorgn = 0.
    sol_bd = sbd(ILOC, :, JLOC)
    IF (runsrf > 0. .AND. peakr > 1.e-6) THEN
      IF (prcp > 0.) THEN
        CALL enrsb(sedyld, runsrf, &! IN
                   enratio)! OUT
!         CALL orgn(dt, enratio, erorgn, sedyld, sol_bd, sol_z,          &! IN
!                 sol_aon, sol_fon, sol_rsd, sol_son,                    &! INOUT
!                 sedorgn, xj                                            )! OUT     !-XTC-WRITE xj
      ENDIF
    ENDIF

!! Add nitrate in rainfall to soil profile
    CALL nrain(dt, prcp, &! IN
               sol_nh3, sol_no3, &! INOUT
               no3pcp, xnh3, xno3)! OUT !-XTC-WRITE  xnh3, xno3
!! Compute nitrate movement leaching
    CALL nlch(dt, latf, sol_prk, sol_ul, surfq, &! IN
              sol_no3, &! INOUT
              cno3, latno3, percn, surqno3, xno3)! OUT !-XTC-WRITE  xnh3, xno3
!! Calculate final nitrogen levels in soil
    sumno3 = 0.0
    sumorgn = 0.0
    DO iz = 1, nsol
      sumno3 = sumno3 + sol_no3(iz)
      sumorgn = sumorgn + sol_aon(iz) + sol_son(iz) + sol_fon(iz)
    END DO

    xaon(4) = xj(1)                                                         !-XTC-WRITE
    xfon(4) = xj(2)                                                         !-XTC-WRITE
    xson(4) = xj(3)                                                         !-XTC-WRITE
    xrsd(4) = xj(4)                                                         !-XTC-WRITE
    xaon(5) = sol_aon(1) + sol_aon(2) + sol_aon(3) + sol_aon(4) + sol_aon(5)        !-XTC-WRITE
    xfon(5) = sol_fon(1) + sol_fon(2) + sol_fon(3) + sol_fon(4) + sol_fon(5)        !-XTC-WRITE
    xson(5) = sol_son(1) + sol_son(2) + sol_son(3) + sol_son(4) + sol_son(5)        !-XTC-WRITE
    xnh3(5) = sol_nh3(1) + sol_nh3(2) + sol_nh3(3) + sol_nh3(4) + sol_nh3(5)        !-XTC-WRITE
    xrsd(5) = sol_rsd(1) + sol_rsd(2) + sol_rsd(3) + sol_rsd(4) + sol_rsd(5)        !-XTC-WRITE
    xno3(10) = sol_no3(1) + sol_no3(2) + sol_no3(3) + sol_no3(4) + sol_no3(5)        !-XTC-WRITE

    kk = kk + 1
9200 FORMAT(999(E22.14, ',', :))                                          !-XTC-WRITE
  END SUBROUTINE NCYCLE
!###############################################################################
!========                  SUBROUTINE: SEDIMENT YIELD                   ========
!###############################################################################
  subroutine ysed(ILOC, JLOC, peakr, sneqv, sol_cov, runsrf, uslec, usle_mu, &! IN
                  cklsp, sedyld)! OUT

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily soil loss caused by water erosion
!!    using the modified universal soil loss equation

!!    name               |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    INTEGER, INTENT(IN)    :: ILOC   !grid index
    INTEGER, INTENT(IN)    :: JLOC   !grid index
    REAL, INTENT(IN)    :: &
      peakr, &!mm/s         !peak runoff rate
      sneqv, &!mm H2O       !amount of water in snow in HRU on current day
      sol_cov, &!g/m2         !amount of residue on soil surface
      runsrf, &!mm H2O       !surface runoff
      usle_mu             !none         !product of USLE K,LS,P,exp(rock)

    REAL, INTENT(INOUT) :: &
      uslec               !-            !minimum value of the USLE C factor for water erosion

    REAL, INTENT(OUT)   :: &
      cklsp, &!product of USLE C,P,LS,K,exp(rock)
      sedyld              !g/m2/s       !soil loss caused by water erosion

    REAL                :: &!Local
      sanyld, &!g/m2         !sand yield
      silyld, &!g/m2         !silt yield
      clayld, &!g/m2         !clay yield
      sagyld, &!g/m2         !small Aggregate yield
      lagyld, &!g/m2         !large Aggregate yield
      c, &!
      cvm, &!none         !natural log of uslec (the minimum value
      !of the USLE C factor for the land cover)
      idplt = 1.0, &!none         !land cover code from crop.dat
      usle_cf             !-            !USLE equation cover and management (C) factor

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

    !! USLE equation cover and management (C) factor
    if (uslec < 1.e-4) uslec = 0.001
    cvm = Log(uslec) !or 1.463 log(uslec) + 0.1034 ?
    c = 0.
    if (idplt > 0) then
      c = Exp((-.2231 - cvm) * Exp(-.00115 * sol_cov) + cvm)
    else
      if (sol_cov > 1.e-4) then
        c = Exp(-.2231 * Exp(-.00115 * sol_cov))
      else
        c = .8
      end if
    end if
    usle_cf = c

    cklsp = 0.
    cklsp = usle_cf * usle_mu

    !! compute sediment yield with musle
    sedyld = (runsrf * peakr)**.56 * cklsp
    if (sedyld < 0.) sedyld = 0.

    !!adjust sediment yield for protection of snow cover
    if (sneqv <= 0.) then
      if (sedyld < 1.e-6) sedyld = 0.0
    else if (sneqv > 100.) then
      sedyld = 0.
    else
      sedyld = sedyld / Exp(sneqv * 3./25.4)
    end if

    !!Particle size distribution of sediment yield
    sanyld = sedyld * det_san(ILOC, JLOC)    !! Sand yield
    silyld = sedyld * det_sil(ILOC, JLOC)    !! Silt yield
    clayld = sedyld * det_cla(ILOC, JLOC)    !! Clay yield
    sagyld = sedyld * det_sag(ILOC, JLOC)    !! Small Aggregate yield
    lagyld = sedyld * det_lag(ILOC, JLOC)    !! Large Aggregate yield
  end subroutine ysed
!###############################################################################
!========              SUBROUTINE: FERTILIZER APPLICATION               ========
!###############################################################################
  subroutine fert(dt, vegtyp, yearlen, julian, sol_dz, lat, lon, nfert, &! IN
                  sol_aon, sol_fon, sol_rsd, sol_nh3, sol_no3, xnh3, xno3)! INOUT !-XTC-WRITE  xnh3, xno3

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the enrichment ratio for nutrient and
!!    pesticide transport with runoff

!!    name               |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    INTEGER, INTENT(IN) :: &
      vegtyp, &!none         !land cover type
      yearlen             !day          !Number of days in the particular year

    REAL, INTENT(IN)    :: &
      nfert, &!g/m2         !N fertilizer
      dt, &!s            !time step [sec]
      julian, &!day          !Julian day of year (fractional, 0 <= JULIAN < YEARLEN)
      sol_dz(nsol), &!mm           !thickness of each soil layer
      lat, &!degree       !latitude
      lon                 !degree       !longtitude
    REAL*8, INTENT(INOUT) :: &
      sol_aon(nsol), &!g N/m2       !amount of nitrogen stored in the active
      !organic (humic) nitrogen pool
      sol_fon(nsol), &!g N/m2       !amount of nitrogen stored in the fresh
      !organic (residue) pool
      sol_rsd(nsol), &!g/m2         !amount of organic matter in the soil
      !classified as residue
      sol_nh3(nsol), &!g N/m2       !amount of nitrogen stored in the ammonium pool
      !in soil layer
      sol_no3(nsol)       !g N/m2       !amount of nitrogen stored in the nitrate pool
    !in soil layer

    INTEGER, PARAMETER  :: &
      istill = 1, &!-            !0: no-tillage; 1: tillage
      shift = 0          !-            !0:close the station; 1:open the station

    REAL, PARAMETER     :: &
      effmix = 0.3, &!none         !mixing efficiency of tillage operation
      frminn = 1.0, &!-            !fraction of fertilizer that is mineral N (NO3 + NH4)
      frnh3n = 0.4, &!-            !fraction of mineral N in fertilizer that is NH3-N
      frsurf = 0.2        !-            !fraction of fertilizer is applied to the top 10 mm of soil,
    !the remaining fraction is applied to first soil layer
    !  nfert  = 20.0        !g N/m2       !total amount of nitrogen fertilizer application in a year
    !differ with land use types and location

    INTEGER             :: &
      k                   !none         !counter (soil layer)

    REAL                :: &!Local
      nt20, &!s            !the number of time steps in 20 days
      xx(2)               !-            !fraction of fertilizer applied to layer

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
    real*8, intent(inout)    :: xnh3(20), xno3(20)    !-XTC-WRITE
    integer                  :: i, j
    real, save               :: iyr = 2000.0, xl = 0. !-XTC-REAL_FERT

    REAL, DIMENSION(22)      :: fert_o, fert_y, fert_d
    REAL, DIMENSION(22)      :: till_y, till_d
    DATA fert_y/2004, 2004, 2005, 2005, 2006, 2006, 2007, 2007, 2008, 2008, &
      2009, 2009, 2010, 2010, 2011, 2011, 2012, 2012, 2013, 2013, &
      2014, 2014/
    DATA fert_d/230, 160, 193, 137, 195, 112, 200, 134, 205, 156, &
      201, 117, 204, 125, 195, 121, 212, 153, 222, 120, &
      207, 117/
    DATA fert_o/8.832, 2.208, 8.832, 2.208, 8.832, 2.208, 8.832, 2.208, 8.832, 2.208, &
      8.832, 2.208, 8.832, 2.208, 4.416, 1.104, 8.832, 2.208, 8.832, 2.208, &
      8.832, 2.208/
    DATA till_y/2004, 2004, 2005, 2005, 2006, 2006, 2007, 2007, 2008, 2008, &
      2009, 2009, 2010, 2010, 2011, 2011, 2012, 2012, 2013, 2013, &
      2014, 2014/
    DATA till_d/220, 150, 183, 127, 185, 102, 190, 124, 195, 146, &
      191, 107, 194, 115, 185, 111, 202, 143, 212, 110, &
      197, 107/
!      DATA till_m/  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,  0.,&
!                    0.,  0.,  0.,  1.,  1.,  0.,  0.,  1.,  1.,  1.,  1.,  0. /
    xx(1) = frsurf
    xx(2) = 1.0 - frsurf
    nt20 = 86400.*20./dt

    if (julian == 364.0) iyr = iyr + 0.5

    if ((lat < 38. .and. lat > 36.) .and. (lon < 112. .and. lon > 107.) .and. shift == 1) then
      if (vegtyp == 12 .or. vegtyp == 14) then !only apply fertilizer to cropland .
        do j = 1, 22
          if (iyr == fert_y(j) .and. julian == fert_d(j)) then
            do k = 1, 2
              sol_no3(k) = sol_no3(k) + fert_o(j) * frminn * (1.-frnh3n) * xx(k)
              sol_nh3(k) = sol_nh3(k) + fert_o(j) * frminn * frnh3n * xx(k)
              xnh3(1) = xnh3(1) + fert_o(j) * frminn * frnh3n * xx(k)       !-XTC-WRITE
              xno3(1) = xno3(1) + fert_o(j) * frminn * (1.-frnh3n) * xx(k)   !-XTC-WRITE
            end do
          end if
        end do
      end if
    else
      if (vegtyp == 12 .or. vegtyp == 14) then !only apply fertilizer to cropland .
        if (julian >= 170 .and. julian < 190) then
          do k = 1, 2
            !N fertilizer is applied in 20 days
            sol_no3(k) = sol_no3(k) + nfert * frminn * (1.-frnh3n) * xx(k) / nt20
            sol_nh3(k) = sol_nh3(k) + nfert * frminn * frnh3n * xx(k) / nt20
            xnh3(1) = xnh3(1) + nfert * frminn * frnh3n * xx(k) / nt20 !-XTC-WRITE
            xno3(1) = xno3(1) + nfert * frminn * (1.-frnh3n) * xx(k) / nt20 !-XTC-WRITE
          end do
        end if
      end if
    end if

!redistribute residue and nutrients during tillage
    if ((lat < 38. .and. lat > 36.) .and. (lon < 112. .and. lon > 107.) .and. shift == 1) then
      if (vegtyp == 12 .or. vegtyp == 14 .and. istill == 1) then
        do j = 1, 22
          if (iyr == till_y(j) .and. julian == till_d(j)) then
            do k = 1, 2
              sol_no3(k) = sol_no3(k) * (1.-effmix) +                 &
&                         (sol_no3(1) + sol_no3(2)) * effmix *          &
&                         sol_dz(k) / (sol_dz(1) + sol_dz(2))
              sol_nh3(k) = sol_nh3(k) * (1.-effmix) +                 &
&                         (sol_nh3(1) + sol_nh3(2)) * effmix *          &
&                         sol_dz(k) / (sol_dz(1) + sol_dz(2))
              sol_rsd(k) = sol_rsd(k) * (1.-effmix) +                 &
&                         (sol_rsd(1) + sol_rsd(2)) * effmix *          &
&                         sol_dz(k) / (sol_dz(1) + sol_dz(2))
              sol_fon(k) = sol_fon(k) * (1.-effmix) +                 &
&                         (sol_fon(1) + sol_fon(2)) * effmix *          &
&                         sol_dz(k) / (sol_dz(1) + sol_dz(2))
            end do
            !! remove all residue from soil surface if mixing with moldboard
            !! plow (emix = 0.95 in default tillage database)
            !if (till_m(j) == 1.) then
            !  sol_rsd(2) = sol_rsd(2) + sol_rsd(1)
            !  sol_rsd(1) = 0.
            !  sol_fon(2) = sol_fon(2) + sol_fon(1)
            !  sol_fon(1) = 0.
            !end if
          end if
        end do
      end if
    else
      if (vegtyp == 12 .or. vegtyp == 14 .and. istill == 1) then !tillage
        if (julian == 160) then
          xl = xl + 1.
          if (MOD(xl, 4.) == 1.) then
            do k = 1, 2
              sol_no3(k) = sol_no3(k) * (1.-effmix) +                 &
&                         (sol_no3(1) + sol_no3(2)) * effmix *          &
&                         sol_dz(k) / (sol_dz(1) + sol_dz(2))
              sol_nh3(k) = sol_nh3(k) * (1.-effmix) +                 &
&                         (sol_nh3(1) + sol_nh3(2)) * effmix *          &
&                         sol_dz(k) / (sol_dz(1) + sol_dz(2))
              sol_rsd(k) = sol_rsd(k) * (1.-effmix) +                 &
&                         (sol_rsd(1) + sol_rsd(2)) * effmix *          &
&                         sol_dz(k) / (sol_dz(1) + sol_dz(2))
              sol_fon(k) = sol_fon(k) * (1.-effmix) +                 &
&                         (sol_fon(1) + sol_fon(2)) * effmix *          &
&                         sol_dz(k) / (sol_dz(1) + sol_dz(2))
            end do
          end if
        end if
      end if
    end if
  end subroutine fert
!###############################################################################
!========                     SUBROUTINE: DORMANCY                      ========
!###############################################################################
  subroutine dorm(vegtyp, daylen, daylth, totlb, &! IN
                  sol_fon, sol_rsd, xfon, xrsd)! INOUT !-XTC-WRITE  xfon, xrsd

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the enrichment ratio for nutrient and
!!    pesticide transport with runoff

!!    name               |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    INTEGER, INTENT(IN) :: &
      vegtyp              !none         !land cover type

    REAL, INTENT(IN)    :: &
      daylen, &!hr           !day length
      daylth, &!hr           !threshold daylength to initiate dormancy
      totlb               !g/m2         !total living carbon

    REAL*8, INTENT(INOUT) :: &
      sol_fon(nsol), &!g N/m2       !amount of nitrogen stored in the fresh
      !organic (residue) pool
      sol_rsd(nsol)       !g/m2         !amount of organic matter in the soil
    !classified as residue

    REAL, PARAMETER     :: &
      cnb = 0.053         !-            !fraction of plant biomass that is nitrogen
    !cnb=0.58/11, it could be a dynamic variable

    INTEGER, SAVE       :: &!Local
      idorm = 0           !none         !dormancy status code:
    !0: land cover growing; 1: land cover dormant

    REAL                :: &!Local
      bioleaf, &!             !fraction of biomass converted to resibue
      resnew              !-            !

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

    real*8, intent(inout)    :: xfon(20), xrsd(20) !-XTC-WRITE

!! check for beginning of dormant season
    if (idorm == 0 .and. daylen < daylth) then

      select case (vegtyp)
      case (1, 2, 3, 4, 5) ! forest
        bioleaf = 0.3
        ! totlb      = .2 * totlb * bioleaf
      case (6, 7, 8, 9, 10) ! perennial (pasture/alfalfa)
        bioleaf = 0.95
      case (12, 14) ! Crop and more
        bioleaf = 0.4
      end select

      idorm = 1
      resnew = totlb * bioleaf
9201  FORMAT(999(A, F7.2, 1x, :))                                          !-XTC-WRITE
!-XTC-WRITE  20140813
      sol_rsd(1) = sol_rsd(1) + resnew
      sol_rsd(1) = Max(sol_rsd(1), 0.)
      sol_fon(1) = sol_fon(1) + resnew * cnb
      xfon(1) = xfon(1) + resnew * cnb !-XTC-WRITE 20140813
      xrsd(1) = xrsd(1) + resnew       !-XTC-WRITE 20140813

    end if

!! check if end of dormant period
    if (idorm == 1 .and. daylen >= daylth) then
      idorm = 0
    end if
  end subroutine dorm
!###############################################################################
!========          SUBROUTINE: MINERALIZATION & IMMOBILIZATION          ========
!###############################################################################
  subroutine nminrl(iloc, jloc, dt, vegtyp, rsdco_pl, sol_cbn, sol_fc, sol_st, sol_tmp, &! IN
                    sol_aon, sol_fon, sol_no3, sol_son, sol_rsd, &! INOUT
                    hmn, rmn, rwn, wdn, xaon, xfon, xson, xno3, xrsd)! OUT !-XTC-WRITE

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily nitrogen mineralization and
!!    immobilization considering fresh organic material (plant residue)
!!    and active and stable humus material

!!    name               |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    INTEGER, INTENT(IN)    :: ILOC   !grid index
    INTEGER, INTENT(IN)    :: JLOC   !grid index
    INTEGER, INTENT(IN) :: &
      vegtyp             !none         !land cover type
    REAL, INTENT(IN)    :: &
      dt, &!s            !time step [sec]
      rsdco_pl, &!none         !rsdcopl(VEGTYP), rate coefficient for mineralization
      !of the residue fresh organic nutrients
      sol_cbn(nsol), &!%            !percent organic carbon in soil layer
      sol_fc(nsol), &!mm H2O       !amount of water available to plants in soil
      !layer at field capacity (fc - wp)
      sol_tmp(nsol)       !deg C        !soil temperature for each layer

    REAL, INTENT(INOUT) :: &
      sol_st(nsol)        !mm H2O       !amount of water stored in the soil layer

    REAL*8, INTENT(INOUT) :: &
      sol_aon(nsol), &!g N/m2       !amount of nitrogen stored in the active
      !organic (humic) nitrogen pool
      sol_fon(nsol), &!g N/m2       !amount of nitrogen stored in the fresh
      !organic (residue) pool
      sol_no3(nsol), &!g N/m2       !amount of nitrogen stored in the
      !nitrate pool in soil layer
      sol_son(nsol), &!g N/m2       !amount of nitrogen stored in the stable
      !organic N pool
      sol_rsd(nsol)       !g/m2         !amount of organic matter in the soil
    !classified as residue

    REAL, INTENT(OUT)   :: &
      hmn, &!g N/m2       !amount of nitrogen moving from active
      !organic to nitrate pool in soil profile
      rmn, &!g N/m2       !amount of nitrogen moving from the fresh
      !organic (residue) to the nitrate(80%) and
      !active organic(20%) pools in soil profile
      rwn, &!g N/m2       !amount of nitrogen moving from active organic
      !to stable organic pool in soil profile
      wdn                 !g N/m2       !amount of nitrogen lost from nitrate pool
    !by denitrification in soil profile

    REAL                :: &!Local
      ca, &!
      cdg, &!none         !soil temperature factor
      cdn = 1.4, &!
      cnr, &!             !C:N ratio of the residue
      cnrf, &!             !nutrient cycling residue composition factor
      csf, &!none         !combined temperature/soil water factor
      decr, &!             !decay ratio
      decr_min = 0.01, &!             !
      ft_scale, &!none         !temporal scaling factor
      idplt = 1.0, &!none         !land cover code from crop.dat
      r4, &!
      rdc, &!
      sdnco = 0.85, &!none         !denitrification threshold: fraction of field capacity
      sut, &!none         !soil water factor
      xx                  !varies       !variable to hold intermediate calculation result

    INTEGER             :: &
      k                   !none         !counter (soil layer)

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Exp, Sqrt, Min, Abs, Amax1

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

    real*8, intent(inout)    :: xaon(20), xfon(20), xson(20), xno3(20), xrsd(20) !-XTC-WRITE  20140812

    ft_scale = dt / 86400. !-XTC: This facotr converts some rates from SWAT's daily time
    !      step to subdaily

    IF (vegtyp == 10 .or. vegtyp <= 5) THEN
      sdnco = 0.9125
    ENDIF

    do k = 1, nsol

      !! mineralization can occur only if temp above 0 deg
      if (sol_tmp(k) > 0.) then
        !! compute soil water factor
        sut = 0.
        if (sol_st(k) < 0.) sol_st(k) = .0000001
        sut = .1 + .9 * Sqrt(sol_st(k) / sol_fc(k))
!          sut = Min(1., sut)
        sut = Max(.05, sut)

        !!compute soil temperature factor
        xx = 0.
        cdg = 0.
        xx = sol_tmp(k)
        cdg = .9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1
        cdg = Max(.1, cdg)

        !!compute combined factor
        xx = 0.
        csf = 0.
        xx = cdg * sut
        if (xx < 0.) xx = 0.
        if (xx > 1.e6) xx = 1.e6
        csf = Sqrt(xx)

        !! compute flow from active to stable pools
        rwn = 0

        rwn = .1e-4 * ft_scale * (sol_aon(k) * (1./nactfr - 1.) - sol_son(k)) !-XTC-20140613: Add ft_scale

        if (rwn > 0.) then
          rwn = Min(rwn, sol_aon(k))
        else
          rwn = -(Min(Abs(rwn), sol_son(k)))
        endif
        sol_son(k) = sol_son(k) + rwn

        sol_aon(k) = Max(1.e-6, sol_aon(k) - rwn)
        xaon(2) = xaon(2) + rwn                    !-XTC-WRITE
        xson(1) = xson(1) + rwn                    !-XTC-WRITE

        !! compute humus mineralization on active organic n
        hmn = 0.
        hmn = cmn * csf * sol_aon(k) * ft_scale !-XTC-20140613: Add ft_scale
        hmn = Min(hmn, sol_aon(k))
        !! move mineralized nutrients between pools
        sol_aon(k) = Max(1.e-6, sol_aon(k) - hmn)
        sol_no3(k) = sol_no3(k) + hmn
        xaon(3) = xaon(3) + hmn                    !-XTC-WRITE
        xno3(3) = xno3(3) + hmn                    !-XTC-WRITE

        !! compute residue decomp and mineralization of
        !! fresh organic n and p (upper two layers only)
        rmn = 0.
        if (k <= 2) then
          r4 = 0.
          r4 = .58 * sol_rsd(k)

          if (sol_fon(k) + sol_no3(k) > 1.e-4) then
            cnr = 0.
            cnr = r4 / (sol_fon(k) + sol_no3(k))
            if (cnr > 500.) cnr = 500.
            cnrf = 0.
            cnrf = Exp(-.693 * (cnr - 25.) / 25.)
          else
            cnrf = 1.
          end if

          ca = 0.
          decr = 0.
          rdc = 0.
          ca = Min(cnrf, 1.)
          if (idplt > 0) then
            decr = rsdco_pl * ca * csf
          else
            decr = 0.05
          end if
          decr = Max(decr_min, decr)
          decr = Min(decr, 1.)
          decr = decr * ft_scale !-XTC-20140712: Add ft_scale
          sol_rsd(k) = max(1.e-6, sol_rsd(k))
          rdc = decr * sol_rsd(k)
          sol_rsd(k) = sol_rsd(k) - rdc
          if (sol_rsd(k) < 0.) sol_rsd(k) = 0.
          rmn = decr * sol_fon(k)

          sol_fon(k) = max(1.e-6, sol_fon(k))
          sol_fon(k) = sol_fon(k) - rmn
          sol_no3(k) = sol_no3(k) + .8 * rmn
          sol_aon(k) = sol_aon(k) + .2 * rmn

          xaon(1) = xaon(1) + 0.2 * rmn                !-XTC-WRITE
          xfon(2) = xfon(2) + rmn                !-XTC-WRITE
          xno3(4) = xno3(4) + 0.8 * rmn                !-XTC-WRITE
          xrsd(2) = xrsd(2) + rdc                !-XTC-WRITE
        end if

        !! compute denitrification
        wdn = 0.
!          write(*,*) sdnco, sut, cdn
        if (sut >= sdnco .and. sol_no3(k) > 0.) then
          wdn = sol_no3(k) * (1.-Exp(-cdn * cdg * sol_cbn(k)))
          wdn = wdn * ft_scale !-XTC-20140613: Add ft_scale
          xx = k !-XTC-WRITE
        else
          wdn = 0.
        endif
        sol_no3(k) = sol_no3(k) - wdn
        xno3(6) = xno3(6) + wdn
      end if
    end do
9200 FORMAT(999(E22.14, ',', :))                                                       !-XTC-WRITE
    hmn = xno3(3)
    rmn = xfon(2) ! 0.8 * rmn + 0.2 * rmn
    rwn = xson(1)
    wdn = xno3(6)
  end subroutine nminrl
!###############################################################################
!========          SUBROUTINE: NITRIFICATION & VOLATILIZATION           ========
!###############################################################################
  subroutine nitvol(dt, sol_fc, sol_st, sol_tmp, sol_wp, sol_z, &! IN
                    sol_nh3, sol_no3, xnh3, xno3, &! INOUT  !-XTC-WRITE  xnh3, xno3
                    rnit, rvol)! OUT

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily nitrification (NH3 to NO3)
!!    and volatilization of NH3

!!    name               |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    REAL, INTENT(IN)    :: &
      dt, &!s            !time step [sec]
      !!    nsol,              &!none         !number of layers in soil profile
      sol_fc(nsol), &!mm H2O       !amount of water available to plants in soil
      !layer at field capacity (fc - wp)
      sol_st(nsol), &!mm H2O       !amount of water stored in the soil layer
      !on any given day (less wp water)
      sol_tmp(nsol), &!deg C        !daily average temperature of soil layer
      sol_wp(nsol), &!mm H20       !water content of soil at -1.5 MPa (wilting
      !point)
      sol_z(nsol)         !mm           !depth to bottom of soil layer

    REAL(R8), INTENT(INOUT) :: &
      sol_nh3(nsol), &!g N/m2       !amount of nitrogen stored in the ammonium
      !pool in soil layer
      sol_no3(nsol)       !g N/m2       !amount of nitrogen stored in the
    !nitrate pool in soil layer

    REAL, INTENT(OUT)   :: &
      rnit, &!g N/m2       !amount of nitrogen moving from the NH3 to the
      !NO3 pool (nitrification) in the layer
      rvol                !g N/m2       !amount of nitrogen lost from the NH3 pool due
    !to volatilization

    REAL                :: &!Local
      akn, &!
      akv, &!
      cecf, &!none         !volatilization CEC factor
      dmidl, &!
      dpf, &!
      ft_scale, &!none         !temporal scaling factor
      !      rnit,              &!g N/m2       !amount of nitrogen moving from the NH3 to the
      !NO3 pool (nitrification) in the layer
      rnv, &!
      !      rvol,              &!g N/m2       !amount of nitrogen lost from the NH3 pool due
      !to volatilization
      sw25, &!
      swf, &!
      swwp, &!
      tf, &!
      xx                  !

    INTEGER             :: &!Local
      k                   !none         !counter (soil layer)

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
    real*8, intent(inout)    :: xnh3(20), xno3(20) !-XTC-WRITE  20140812

    cecf = 0.15
    ft_scale = dt / 86400. !-XTC: temporal scaling factor

    do k = 1, nsol
      tf = 0.
      tf = .41 * (sol_tmp(k) - 5.) / 10.

      if (sol_nh3(k) > 0. .and. tf >= 0.001) then
        sw25 = 0.
        swwp = 0.
        sw25 = sol_wp(k) + 0.25 * sol_fc(k)
        swwp = sol_wp(k) + sol_st(k)
        if (swwp < sw25) then
          swf = 0.
          swf = (swwp - sol_wp(k)) / (sw25 - sol_wp(k))
        else
          swf = 1.
        endif

        if (k == 1) then
          xx = 0.
        else
          xx = 0.
          xx = sol_z(k - 1)
        endif

        dmidl = 0.
        dpf = 0.
        akn = 0.
        akv = 0.
        rnv = 0.
        rnit = 0.
        rvol = 0.
        dmidl = (sol_z(k) + xx) / 2.
        dpf = 1.-dmidl / (dmidl + Exp(4.706 - .0305 * dmidl))
        akn = tf * swf
        akv = tf * dpf * cecf
        rnv = sol_nh3(k) * (1.-Exp(-akn - akv)) * ft_scale !-XTC-20140613: Add ft_scale
        rnit = 1.-Exp(-akn)
        rvol = 1.-Exp(-akv)

        !! calculate nitrification (NH3 => NO3)
        if (rvol + rnit > 1.e-6) then
          rvol = rnv * rvol / (rvol + rnit)
          rnit = rnv - rvol
          if (rnit < 0.) rnit = 0.
          sol_nh3(k) = Max(1.e-6, sol_nh3(k) - rnit)
          xnh3(3) = xnh3(3) + rnit !-XTC-WRITE  20140812
        endif
        if (sol_nh3(k) < 0.) then
          rnit = rnit + sol_nh3(k)
          sol_nh3(k) = 0.
        endif
        sol_no3(k) = sol_no3(k) + rnit
        xno3(5) = xno3(5) + rnit !-XTC-WRITE  20140812

        !! calculate ammonia volatilization
        sol_nh3(k) = Max(1.e-6, sol_nh3(k) - rvol)
        xnh3(4) = xnh3(4) + rvol !-XTC-WRITE  20140812
        if (sol_nh3(k) < 0.) then
          rvol = rvol + sol_nh3(k)
          sol_nh3(k) = 0.
        endif
      end if

    end do
    rnit = xno3(5)
    rvol = xnh3(4)
  end subroutine nitvol
!###############################################################################
!========                 SUBROUTINE: ENRICHMENT RATIO                  ========
!###############################################################################
  subroutine enrsb(sedyld, runsrf, &! IN
                   enratio)! OUT

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the enrichment ratio for nutrient and
!!    pesticide transport with runoff

!!    name               |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    REAL, INTENT(IN)    :: &
      !      da_ha,             &!ha           !area of watershed in hectares
      runsrf              !mm H2O       !surface runoff generated on day in subbasin

    REAL, INTENT(INOUT) :: &
      sedyld              !g/m2         !daily soil loss caused by water erosion in HRU

    REAL, INTENT(OUT)   :: &
      enratio             !none         !enrichment ratio

    REAL                :: &!Local
      cy                  !

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

    if (sedyld < 1.e-4) sedyld = 0.0
!      if (sedyld < 1.e-4) then
!        sedyld = 0.0
!        sanyld = 0.0
!        silyld = 0.0
!        clayld = 0.0
!        sagyld = 0.0
!        lagyld = 0.0
!      end if

!! CREAMS method for calculating enrichment ratio
    cy = 0.
    !! subbasin sediment calculations
    cy = .1 * sedyld / (runsrf + 1.e-6)

    if (cy > 1.e-6) then
      enratio = .78 * cy**(-.2468)
    else
      enratio = 0.
    endif

    if (enratio > 3.5) enratio = 3.5
  end subroutine enrsb
!###############################################################################
!========                 SUBROUTINE: ORGANIC NITROGEN                  ========
!###############################################################################
  subroutine orgn(dt, enratio, erorgn, sedyld, sol_bd, sol_z, &! IN
                  sol_aon, sol_fon, sol_rsd, sol_son, &! INOUT
                  sedorgn, xj)! OUT      !-XTC-WRITE  xj

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic nitrogen removed in
!!    surface runoff

!!    name               |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    REAL, INTENT(IN)    :: &
      dt, &!s            !time step [sec]
      enratio, &!none         !enrichment ratio calculated for day in HRU
      erorgn, &!none         !organic N enrichment ratio, if left blank
      !the model will calculate for every event
      sedyld, &!g/m2/s       !soil loss caused by water erosion
      sol_bd(nsol), &!Mg/m**3      !bulk density of the soil
      sol_z(nsol)         !mm           !depth to bottom of soil layer

    REAL*8, INTENT(INOUT) :: &
      sol_aon(nsol), &!g N/m2       !amount of nitrogen stored in the active
      !organic (humic) nitrogen pool
      sol_fon(nsol), &!g N/m2       !amount of nitrogen stored in the fresh
      !organic (residue) pool
      sol_rsd(nsol), &!g/m2         !amount of organic matter in the soil
      !classified as residue
      sol_son(nsol)       !g N/m2       !amount of nitrogen stored in the stable
    !organic N pool

    REAL, INTENT(OUT)   :: &
      sedorgn             !g N/m2       !amount of organic nitrogen in surface runoff
    !for the time step

    REAL                :: &!Local
      conc, &!             !conc,entration of organic N in soil
      er, &!none         !enrichment ratio
      wt1, &!none         !conversion factor (mg/kg => g/m2)
      xx                  !g N/m2       !amount of organic N in first soil layer

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

    integer :: iz !-XTC-WRITE
    real*8, intent(inout)    :: xj(4) !-XTC-WRITE

    xx = 0.
    wt1 = 0.    !! conversion factor
    er = 0.     !! enrichment ratio
    xx = sol_son(1) + sol_aon(1) + sol_fon(1)
    wt1 = sol_bd(1) * sol_z(1) / 100.

    if (erorgn > .001) then
      er = erorgn
    else
      er = enratio
    end if

    conc = 0.
    conc = xx * er / wt1
    sedorgn = .001 * conc * sedyld * dt !-XTC 20150205 multiply by dt

    !! update soil nitrogen pools
    if (xx > 1.e-6) then
      sol_aon(1) = sol_aon(1) - sedorgn * (sol_aon(1) / xx)
      sol_son(1) = sol_son(1) - sedorgn * (sol_son(1) / xx)
      sol_fon(1) = sol_fon(1) - sedorgn * (sol_fon(1) / xx)
      sol_rsd(1) = sol_rsd(1) - sedorgn * (sol_rsd(1) / xx) !-XTC 20140813 Update sol_rsd and sol_fon simultaneously, not included in SWAT
      xj(1) = xj(1) + sedorgn * (sol_aon(1) / xx)           !-XTC-WRITE
      xj(2) = xj(2) + sedorgn * (sol_fon(1) / xx)           !-XTC-WRITE
      xj(3) = xj(3) + sedorgn * (sol_son(1) / xx)           !-XTC-WRITE
      xj(4) = xj(4) + sedorgn * (sol_rsd(1) / xx)           !-XTC-WRITE

      if (sol_aon(1) < 0.) then
        sedorgn = sedorgn + sol_aon(1)
        sol_aon(1) = 0.
      end if

      if (sol_son(1) < 0.) then
        sedorgn = sedorgn + sol_son(1)
        sol_son(1) = 0.
      end if

      if (sol_fon(1) < 0.) then
        sedorgn = sedorgn + sol_fon(1)
        sol_fon(1) = 0.
      end if
    end if
  end subroutine orgn
!###############################################################################
!========                    SUBROUTINE: DEPOSITION                     ========
!###############################################################################
  subroutine nrain(dt, prcp, &! IN
                   sol_nh3, sol_no3, &! INOUT
                   no3pcp, xnh3, xno3)! OUT !-XTC-WRITE

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds nitrate from rainfall to the soil profile
!!    name               |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    REAL, INTENT(IN)    :: &
      dt, &!s            !time step [sec]
      !!    drydep_nh4,        &!g/m2/yr      !atmospheric dry deposition of ammonia
      !!    drydep_no3,        &!g/m2/yr      !atmospheric dry deposition of nitrates
      prcp                !mm H2O       !precipitation for the time step
!!    rcn                 !mg/L         !Concentration of nitrogen in the rainfall

    REAL(R8), INTENT(INOUT) :: &
      sol_nh3(nsol), &!g N/m2       !amount of nitrogen stored in the ammonium
      !pool in soil layer
      sol_no3(nsol)       !g N/m2       !amount of N stored in the ammonium pool
    !in soil layer

    REAL, INTENT(OUT)   :: &
      no3pcp              !g N/m2       !nitrate added to the soil in rainfall

    REAL                :: &!Local
      nsecyr = 31556909., &!-            !=365.242*86400., number of second in a year
      nh3pcp              !none         !

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
    real*8, intent(inout)    :: xnh3(20), xno3(20) !-XTC-WRITE

!! calculate nitrate in precipitation
    nh3pcp = .001 * rammo * prcp * dt
    no3pcp = .001 * rcn * prcp * dt
    sol_nh3(1) = sol_nh3(1) + nh3pcp + drydep_nh4 / nsecyr * dt
    sol_no3(1) = sol_no3(1) + no3pcp + drydep_no3 / nsecyr * dt
    xnh3(2) = xnh3(2) + nh3pcp + drydep_nh4 / nsecyr * dt
    xno3(2) = xno3(2) + no3pcp + drydep_no3 / nsecyr * dt
  end subroutine nrain
!###############################################################################
!========                 SUBROUTINE: NITROGEN LEACHING                 ========
!###############################################################################
  subroutine nlch(dt, latf, sol_prk, sol_ul, surfq, &! IN
                  sol_no3, &! INOUT
                  cno3, latno3, percn, surqno3, xno3)! OUT  !-XTC-WRITE  xno3

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates the loss of nitrate via surface runoff,
!!    lateral flow, tile flow, and percolation out of the profile

!!    name               |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    REAL, INTENT(IN)    :: &
      dt, &!s            !time step [sec]
      !!    nsol,              &!none         !number of layers in soil profile
      !!    anion_excl,        &!none         !fraction of porosity from which anions
      !are excluded
      latf(1:nsol), &!mm H2O       !lateral flow in soil layer on current day
      !!    nperco              !none         !nitrate percolation coefficient (0-1)
      sol_prk(1:nsol), &!mm H2O       !percolation from soil layer on current day
      sol_ul(1:nsol), &!mm H2O       !amount of water held in the soil layer at
      !saturation
      surfq               !mm H2O       !surface runoff generated on a given time step

    REAL(R8), INTENT(INOUT)   :: &
      sol_no3(1:nsol)     !g N/m2       !amount of nitrogen stored in the nitrate pool
    !in soil layer

    REAL, INTENT(OUT)   :: &
      cno3(1:nsol), &!g N/mm H2O   !concentration of nitrate in leached solution
      latno3, &!g N/m2       !amount of nitrate transported with lateral flow
      percn, &!g N/m2       !amount of nitrate percolating past bottom
      !of soil profile
      surqno3             !g N/m2       !amount of nitrate transported with surface
    !runoff

    REAL                :: &!Local
      co, &!g N/mm       !concentration of nitrate in solution
      cosurf, &!g N/mm       !concentration of nitrate in surface runoff
      ft_scale, &!none         !temporal scaling factor
      nloss, &!             !
      percnlyr, &!g N/m2       !nitrate leached to next lower layer with
      !percolation
      sro, &!mm H2O       !surface runoff
      ssfnlyr, &!g N/m2       !nitrate transported in lateral flow from layer
      vno3, &!
      vv, &!mm H2O       !water mixing with nutrient in layer
      ww                  !varies       !variable to hold intermediate calculation result

    INTEGER             :: &
      jj                  !none         !counter (soil layer)

    REAL, PARAMETER     :: dis_stream = 35. ! average distance to stream [m]

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
    real*8, intent(inout)    :: xno3(20) !-XTC-WRITE
    real                     :: a_excl, nlchcon, tmp  !-XTC-WRITE

!      ft_scale = dt / 86400. !-XTC: temporal scaling factor
    percnlyr = 0.
    latno3 = 0.
    do jj = 1, nsol

      !! add nitrate leached from layer above
      sol_no3(jj) = sol_no3(jj) + percnlyr
      xno3(9) = xno3(9) - percnlyr !-XTC-WRITE 20141207
      if (sol_no3(jj) < 1.e-6) sol_no3(jj) = 0.0

      !! determine concentration of nitrate in mobile water
      sro = 0.
      vv = 0.
      vno3 = 0.
      co = 0.
      if (jj == 1) then
        sro = surfq
      else
        sro = 0.
      end if
      vv = (sol_prk(jj) + sro + latf(jj) + 1.e-10) ! * dt !-XTC 20150205 multiply by dt
      a_excl = anion_excl
      if (jj == 3) a_excl = 0.5
      if (jj >= 4) a_excl = 0.75
!        ww = -vv / ((1. - anion_excl) * sol_ul(jj))
      ww = -vv / ((1.-a_excl) * sol_ul(jj))
      vno3 = sol_no3(jj) * (1.-Exp(ww))
      if (vv > 1.e-10) co = Max(vno3 / vv, 0.)  ! * ft_scale !-XTC 20150205 multiply by ft_scale
      cno3(jj) = co
      !! calculate nitrate in surface runoff
      cosurf = 0.
      cosurf = nperco * co
      if (jj == 1) then
        surqno3 = surfq * cosurf ! * dt !-XTC 20150205 multiply by dt
        surqno3 = Min(surqno3, sol_no3(jj))
        sol_no3(jj) = sol_no3(jj) - surqno3
        xno3(7) = xno3(7) + surqno3 !-XTC-WRITE
      endif

      !! calculate nitrate in tile flow
!!      if (ldrain == jj) then
!!        tileno3 = co * qtile     !Daniel 1/2012
!!        tileno3 = Min(tileno3, sol_no3(jj))
!!        sol_no3(jj) = sol_no3(jj) - tileno3!!
!!      end if

      !! calculate nitrate in lateral flow
      ssfnlyr = 0.
      if (jj == 1) then
        ssfnlyr = cosurf * latf(jj) ! * dt !-XTC 20150205 multiply by dt
      else
        ssfnlyr = co * latf(jj) ! * dt !-XTC 20150205 multiply by dt
      end if
      ssfnlyr = Min(ssfnlyr, sol_no3(jj))
      latno3 = latno3 + ssfnlyr

      sol_no3(jj) = sol_no3(jj) - ssfnlyr
      xno3(8) = xno3(8) + ssfnlyr !-XTC-WRITE

      !! calculate nitrate in percolate
      percnlyr = 0.
      percnlyr = co * sol_prk(jj) ! * dt !-XTC 20150205 multiply by dt
!        if(jj == nsol) percnlyr = Max(percnlyr, 0.)       !-XTC 20150206
      percnlyr = Min(percnlyr, sol_no3(jj))
      sol_no3(jj) = sol_no3(jj) - percnlyr
      xno3(9) = xno3(9) + percnlyr !-XTC-WRITE
    end do
    !! calculate nitrate leaching from soil profile
    percn = percnlyr
    tmp = max(sol_prk(nsol), 1.E-8)                                !-XTC-WRITE
!      nlchcon = percn / tmp                                          !-XTC-WRITE
9200 FORMAT(999(E22.14, ',', :))                                      !-XTC-WRITE

    nloss = (2.18 * dis_stream - 8.63) / 100.
    nloss = amax1(0., nloss)
    nloss = Amin1(1., nloss)
    latno3 = (1.-nloss) * latno3
!      latno3 = co                                          !-XTC-WRITE

  end subroutine nlch
END MODULE module_sf_biogeochem_ncycle

MODULE module_sf_biogeochem
  USE module_sf_biogeochem_fun
  USE module_sf_biogeochem_ncycle

contains
END MODULE module_sf_biogeochem
