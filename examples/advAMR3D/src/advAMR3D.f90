program advAMR3D
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer::it,index,iLv
  double precision::stime,etime,time
  ! -- MPI init --
  call ppohAMRFDM_init(st_comm_info)
  ! -- read input.dat --
  if(st_comm_info%rank==0) print *,'### read input.dat ###'
  call ppohAMRFDM_input_data(st_param,st_comm_info)
  ! -- preprocessing --
  if(st_comm_info%rank==0) print *,'### in pre-process ###'
  call ppohAMRFDM_pre(st_param,st_meshset,st_comm_info)
  ! -- setup initial condition --
  if(st_comm_info%rank==0) print *,'### set initial consitions ###'
  call setup_model(st_param,st_meshset,st_comm_info)
  ! -- initial AMR --
  if(st_comm_info%rank==0) print *,'### initial mesh refinement ###'
  call ppohAMRFDM_refine_init(st_param,st_meshset,st_comm_info)
  ! -- initial output --
  if(st_comm_info%rank==0) print *,'output initial conditions'
  call output_data(0,st_param,st_meshset,st_comm_info)
  time=0.0d0; stime=mpi_wtime()
  ! -- start main loop --
  if(st_comm_info%rank==0) print *,'### start main loop ###'
  do it=1,st_param%itmax
     time=time+st_param%dt; st_param%it=it
     if(st_comm_info%rank==0) then
        print *,'   loop iteration start:',it
     endif
     do index=1,st_param%itorder(st_param%LvMax+1)
        iLv=st_param%lvorder(index)
        ! -- AMR Part --
        call ppohAMRFDM_refine(iLv,st_param,st_meshset,st_comm_info)
        ! -- Kernel Part --
        call kernel_adv(iLv,st_param,st_meshset,st_comm_info)
     enddo
     ! -- DDD Part --
     call ppohAMRFDM_DDD(st_param,st_meshset,st_comm_info)
     !-- output --
     if(st_comm_info%rank==0) then
        print *,'     loop iteration end:',it
        print *,'elapsed simulation time:',time
        print *,'################################'
     endif
     call output_data(it,st_param,st_meshset,st_comm_info)
  enddo
  etime=mpi_wtime()
  if(st_comm_info%rank==0) print *,'Elapsed time[s]:',etime-stime
  ! -- MPI finalize --
  call ppohAMRFDM_finalize
  stop
end program advAMR3D
