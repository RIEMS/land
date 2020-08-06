module module_ldas_config
   ! the configurations of a simulation
   use module_noahmp_option
   implicit none
   integer :: nsoil ! number of soil layers

   integer :: opt_gla ! glacier option (1->phase change; 2->simple)
   integer :: opt_z0tlnd ! option of Chen adjustment of Czil (not used)
   logical :: update_lai
   logical :: update_veg
   integer :: spinup_loop
   logical :: reset_spinup_date
   real :: dtbl ! timestep [s]

   character(len=256) :: indir
   integer :: forcing_timestep
   integer :: noah_timestep
   integer :: start_year
   integer :: start_month
   integer :: start_day
   integer :: start_hour
   integer :: start_min
   character(len=256) :: outdir = "."
   character(len=256) :: resdir = "."
   character(len=256) :: restart_filename_requested = " "
   integer :: restart_frequency_hours
   integer :: output_timestep
   integer :: spinup_loops

   integer :: sf_urban_physics = 0
   integer :: num_urban_ndm = 1
   integer :: num_urban_ng = 1
   integer :: num_urban_nwr = 1
   integer :: num_urban_ngb = 1
   integer :: num_urban_nf = 1
   integer :: num_urban_nz = 1
   integer :: num_urban_nbui = 1
   integer :: num_urban_hi = 15
   real :: urban_atmosphere_thickness = 2.0

   integer, parameter :: MAX_SOIL_LEVELS = 10 ! maximum soil levels in namelist
   real, dimension(MAX_SOIL_LEVELS) :: soil_thick_input ! depth to soil interfaces from namelist [m]

   ! derived urban dimensions
   integer :: num_urban_atmosphere ! atmospheric levels including
   integer :: urban_map_zrd
   integer :: urban_map_zwd
   integer :: urban_map_gd
   integer :: urban_map_zd
   integer :: urban_map_zdf
   integer :: urban_map_bd
   integer :: urban_map_wd
   integer :: urban_map_gbd
   integer :: urban_map_fbd

   integer :: xstart = 1
   integer :: ystart = 1
   integer :: xend = 0
   integer :: yend = 0
   character(len=256) :: forcing_name_T = "T2D"
   character(len=256) :: forcing_name_Q = "Q2D"
   character(len=256) :: forcing_name_U = "U2D"
   character(len=256) :: forcing_name_V = "V2D"
   character(len=256) :: forcing_name_P = "PSFC"
   character(len=256) :: forcing_name_LW = "LWDOWN"
   character(len=256) :: forcing_name_SW = "SWDOWN"
   character(len=256) :: forcing_name_PR = "RAINRATE"
   character(len=256) :: forcing_name_SN = ""

   integer :: split_output_count = 1
   logical :: skip_first_output = .false.
   integer :: khour
   integer :: kday
   real :: zlvl
   character(len=256) :: hrldas_setup_file = " "
   character(len=256) :: spatial_filename = " "
   character(len=256) :: external_veg_filename_template = " "
   character(len=256) :: external_lai_filename_template = " "

contains
   subroutine ldas_config_load()
      implicit none
      integer :: ierr
      character(len=256) :: msg
      logical :: isvalid
      namelist / NOAHLSM_OFFLINE /    &
#ifdef WRF_HYDRO
         finemesh, finemesh_factor, forc_typ, snow_assim , GEO_STATIC_FLNM, HRLDAS_ini_typ, &
#endif
         indir, nsoil, soil_thick_input, forcing_timestep, noah_timestep, &
         start_year, start_month, start_day, start_hour, start_min, &
         outdir, skip_first_output, &
         resdir, restart_filename_requested, restart_frequency_hours, output_timestep, &
         spinup_loops, &
         forcing_name_T, forcing_name_Q, forcing_name_U, forcing_name_V, forcing_name_P, &
         forcing_name_LW, forcing_name_SW, forcing_name_PR, forcing_name_SN, &

         dveg, opt_crs, &
         opt_btr, opt_run, opt_sfc, opt_frz, &
         opt_inf, opt_rad, opt_alb, &
         opt_snf, opt_tbot, opt_stc, &
         opt_gla, opt_rsf, &

         opt_soil, opt_pedo, opt_crop, &

         sf_urban_physics, num_urban_hi, urban_atmosphere_thickness, &
         num_urban_ndm, num_urban_ng, num_urban_nwr, num_urban_ngb , &
         num_urban_nf, num_urban_nz, num_urban_nbui, &


         split_output_count, &
         khour, kday, zlvl, hrldas_setup_file, &
         spatial_filename, &
         external_veg_filename_template, external_lai_filename_template, &
         xstart, xend, ystart, yend

      ! Initialize namelist variables to dummy values, so we can tell
      ! if they have not been set properly.
      nsoil                   = -999
      soil_thick_input        = -999
      dtbl                    = -999
      start_year              = -999
      start_month             = -999
      start_day               = -999
      start_hour              = -999
      start_min               = -999
      khour                   = -999
      kday                    = -999
      zlvl                    = -999
      forcing_timestep        = -999
      noah_timestep           = -999
      output_timestep         = -999
      spinup_loops            = 0
      restart_frequency_hours = -999

      open(30, file="ldas.namelist", form="FORMATTED", status="OLD", action="READ")
      read(30, NOAHLSM_OFFLINE, iostat=ierr)
      if (ierr /= 0) then
         write(*,'(/," Namelist ERROR: Problem reading the file ldas.namelist",/)')
         rewind(30)
         read(30, NOAHLSM_OFFLINE)
         stop " Namelist ERROR: Problem reading namelist NOAHLSM_OFFLINE"
      endif
      close(30)

      dtbl = real(noah_timestep)

      update_lai = .true.   ! default: use LAI if present in forcing file
      if (dveg == 2 .or. dveg == 5 .or. &
         dveg == 6 .or. dveg == 10) &
         update_lai = .false.

      update_veg = .false.  ! default: don't use VEGFRA if present in forcing file
      if (dveg == 1 .or. dveg == 6 .or. dveg == 7) &
         update_veg = .true.

      if (nsoil < 0) then
         stop " Namelist ERROR: NSOIL must be set in the namelist."
      endif

      if ((khour < 0) .and. (kday < 0)) then
         write(*, '(" Namelist ERROR: Either KHOUR or KDAY must be defined.")')
         stop
      else if (( khour < 0 ) .and. (kday > 0)) then
         khour = kday * 24
      else if ((khour > 0) .and. (kday > 0)) then
         write(*, '("Namelist warning:  KHOUR and KDAY both defined.")')
      else
         ! all is well.  KHOUR defined
      endif

      if (forcing_timestep < 0) then
         write(*, '(" Namelist ERROR: FORCING_TIMESTEP needs to be set greater than zero.")')
         stop
      endif

      if (noah_timestep < 0) then
         write(*, '(" Namelist ERROR: NOAH_TIMESTEP needs to be set greater than zero.")')
         write(*, '("                 900 seconds is recommended.")')
         stop
      endif

      ! Check that OUTPUT_TIMESTEP fits into NOAH_TIMESTEP:
      if (output_timestep /= 0) then
         if (mod(output_timestep, noah_timestep) > 0) then
            write(*, '(" Namelist ERROR: OUTPUT_TIMESTEP should set to an integer multiple of NOAH_TIMESTEP.")')
            write(*, '("                 OUTPUT_TIMESTEP = ", I12, " seconds")') output_timestep
            write(*, '("                 NOAH_TIMESTEP   = ", I12, " seconds")') noah_timestep
            stop
         endif
      endif

      ! Check that RESTART_FREQUENCY_HOURS fits into NOAH_TIMESTEP:
      if (restart_frequency_hours /= 0) then
         if (mod(restart_frequency_hours*3600, noah_timestep) > 0) then
            write(*, '(" Namelist ERROR: RESTART_FREQUENCY_HOURS (converted to seconds) should set to an ")')
            write(*, '("                 integer multiple of NOAH_TIMESTEP.")')
            write(*, '("                 RESTART_FREQUENCY_HOURS = ", I12, " hours:  ", I12, " seconds")') &
               restart_frequency_hours, restart_frequency_hours*3600
            write(*, '("                 NOAH_TIMESTEP           = ", I12, " seconds")') noah_timestep
            stop
         endif
      endif

      call noahmp_option_check(isvalid, msg)
      if (.not. isvalid) then
         write(*, '("Namelist ERROR: ",A)') msg
      end if

      if (sf_urban_physics == 2 .or. sf_urban_physics == 3) then
         if ( urban_atmosphere_thickness <= 0.0) then
            write(*, '(" Namelist ERROR: When running BEP/BEM, URBAN_ATMOSPHERE_LEVELS must contain at least 3 levels")')
            stop
         endif
         num_urban_atmosphere = int(zlvl/urban_atmosphere_thickness)
         if (zlvl - num_urban_atmosphere*urban_atmosphere_thickness >= 0.5*urban_atmosphere_thickness)  &
            num_urban_atmosphere = num_urban_atmosphere + 1
         if ( num_urban_atmosphere <= 2) then
            write(*, '(" Namelist ERROR: When running BEP/BEM, num_urban_atmosphere must contain at least 3 levels, ")')
            write(*, '("                 decrease URBAN_ATMOSPHERE_THICKNESS")')
            stop
         endif
      endif
   end subroutine ldas_config_load
end module module_ldas_config
