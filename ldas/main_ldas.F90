program ldas
   use module_ldas_noahmp, only: land_driver_ini, land_driver_exe
   implicit none

   integer :: ITIME, NTIME

   call land_driver_ini(NTIME)

   do ITIME = 0, NTIME
       call land_driver_exe(ITIME)
   end do

#ifdef WRF_HYDRO
   call hydro_finish()
#endif
end program ldas
