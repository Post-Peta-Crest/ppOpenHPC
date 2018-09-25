program main
#ifdef ATM
use mod_atm
#endif
#ifdef OCN
use mod_ocn
#endif
#ifdef CHM
use mod_chm
#endif
#ifdef MI6
use mi6
#endif
logical :: loop_flag


loop_flag = .true.

#ifdef ATM
  call atm_init()

  do while(loop_flag)
    call atm_run(loop_flag)
  end do

  call atm_fin()
#endif

#ifdef OCN
  call ocn_init()

  do while(loop_flag)
    call ocn_run(loop_flag)
  end do

  call ocn_fin()
#endif

#ifdef CHM
  call chm_init()

  do while(loop_flag)
    call chm_run(loop_flag)
  end do

  call chm_fin()
#endif

#ifdef MI6
  call mi6_init()

  do while(loop_flag)
    call mi6_run(loop_flag)
  end do

  call mi6_fin()
#endif

  stop

end program

