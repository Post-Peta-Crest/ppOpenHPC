program post_vtk
  implicit none
  integer::ixmax,iymax,izmax,pxmax,pymax,pzmax,LvMax,npy
  double precision::dx(1:3)
  integer::istep,sstep,estep,nosum,elsum
  integer,allocatable::RF(:,:,:,:),NF(:,:,:,:),ID(:,:,:,:)
  double precision,allocatable::RD(:,:,:,:,:)
  ! -- load input data --
  call input_data
  ! -- allocate memory --
  call allocate_memory
  ! -- load iteration count --
  call load_count
  ! -- main loop for iteration step --
  do istep=sstep,estep
     print *,'step',istep
     ! -- read data --
     call read_data
     ! -- set flag --
     call set_flag
     ! -- output --
     call output_vtk
  enddo
  
contains
  
  subroutine input_data
    implicit none
    character(len=90)::filename,tmpc
    integer::tmpi,n_gp,n_pr
    double precision::tmpd
    filename="../run/input.dat"
    open(unit=7,file=filename,status='old')
    read(7,*) tmpc
    read(7,*) tmpc
    read(7,*) n_gp
    read(7,*) tmpc
    read(7,*) tmpc
    read(7,*) n_pr
    read(7,*) tmpc
    read(7,*) tmpi
    read(7,*) tmpc
    read(7,*) LvMax
    read(7,*) tmpc
    read(7,*) tmpi
    read(7,*) tmpc
    read(7,*) tmpc
    read(7,*) tmpi
    read(7,*) tmpc
    read(7,*) tmpd
    read(7,*) tmpc
    read(7,*) npy
    read(7,*) tmpc
    read(7,*) tmpi
    read(7,*) tmpc
    read(7,*) tmpd
    read(7,*) tmpc
    read(7,*) tmpd
    read(7,*) tmpc
    read(7,*) dx(1),dx(2),dx(3)
    read(7,*) tmpc
    read(7,*) tmpd
    read(7,*) tmpc
    read(7,*) tmpi
    close(7)
    ixmax=2**n_gp; iymax=2**n_gp; izmax=2**n_gp
    pxmax=2**n_pr; pymax=2**n_pr; pzmax=2**n_pr
    return
  end subroutine input_data

  subroutine allocate_memory
    implicit none
    allocate(RD(1:npy,0:LvMax,&
         1:pxmax*ixmax*(2**LvMax),&
         1:pymax*iymax*(2**LvMax),&
         1:pzmax*izmax*(2**LvMax)))
    allocate(RF(0:LvMax,&
         1:pxmax*ixmax*(2**LvMax),&
         1:pymax*iymax*(2**LvMax),&
         1:pzmax*izmax*(2**LvMax)))
    allocate(NF(0:LvMax,&
         0:pxmax*ixmax*(2**LvMax),&
         0:pymax*iymax*(2**LvMax),&
         0:pzmax*izmax*(2**LvMax)))
    allocate(ID(0:LvMax,&
         0:pxmax*ixmax*(2**LvMax),&
         0:pymax*iymax*(2**LvMax),&
         0:pzmax*izmax*(2**LvMax)))
    return
  end subroutine allocate_memory

  subroutine load_count
    implicit none
    character(len=100)::filename,tmp
    filename="./out_range.dat"
    open(unit=10,file=filename,status='old')
    read(10,*) tmp
    read(10,*) tmp
    read(10,*) sstep,estep
    close(10)
    return
  end subroutine load_count

  subroutine read_data
    implicit none
    integer::Nint,ix,iy,iz,irank,stat,iLv
    integer::iPOS(1:3)
    double precision::F(1:npy)
    character(len=80)::filename
    RD(:,:,:,:,:)=0.0d0; RF(:,:,:,:)=0
    do irank=0,pxmax*pymax*pzmax-1
       write(filename,'("../run/results/prop",i5.5,"rank",i4.4,".dat")') istep,irank
       open(unit=10,file=filename,status='old')
       do while(.true.)
          read(10,'(I9,2X,I9,2X,I9,2X,E14.7,2X,I9)',iostat=stat) iPOS(1),iPOS(2),iPOS(3),F(1),iLv
          if(stat<0) exit
          Nint=2**(LvMax-iLv)
          ix=(iPOS(1)+Nint)/(2*Nint); iy=(iPOS(2)+Nint)/(2*Nint); iz=(iPOS(3)+Nint)/(2*Nint)
          ! -- storage raster data system --
          RD(1,iLv,ix,iy,iz)=F(1); RF(iLv,ix,iy,iz)=1
       enddo
       close(10)
    enddo
    return
  end subroutine read_data
  
  subroutine set_flag
    implicit none
    integer::iLv,ix,iy,iz
    ! -- set flag for nodes --
    NF(:,:,:,:)=0; elsum=0
    do iLv=0,LvMax
       do iz=1,pzmax*izmax*(2**iLv)
          do iy=1,pymax*iymax*(2**iLv)
             do ix=1,pxmax*ixmax*(2**iLv)
                if(RF(iLv,ix,iy,iz)==1) then
                   NF(iLv,ix-1,iy-1,iz-1)=1
                   NF(iLv,ix-1,iy-1,iz  )=1
                   NF(iLv,ix-1,iy  ,iz-1)=1
                   NF(iLv,ix-1,iy  ,iz  )=1
                   NF(iLv,ix  ,iy-1,iz-1)=1
                   NF(iLv,ix  ,iy-1,iz  )=1
                   NF(iLv,ix  ,iy  ,iz-1)=1
                   NF(iLv,ix  ,iy  ,iz  )=1
                   elsum=elsum+1
                endif
             enddo
          enddo
       enddo
    enddo
    ! -- set node index --
    nosum=0; ID(:,:,:,:)=0
    do iLv=0,LvMax
       do iz=0,pzmax*izmax*(2**iLv)
          do iy=0,pymax*iymax*(2**iLv)
             do ix=0,pxmax*ixmax*(2**iLv)
                if(NF(iLv,ix,iy,iz)==1) then
                   nosum=nosum+1
                   ID(iLv,ix,iy,iz)=nosum
                endif
             enddo
          enddo
       enddo
    enddo
    return
  end subroutine set_flag

  subroutine output_vtk
    implicit none
    integer::iLv,ix,iy,iz
    character(len=80)::filename
    write(filename,'("./output/prop",i5.5,".vtk")') istep
    open(unit=10,file=filename,status='replace')
    write(10,'("# vtk DataFile Version 2.0")')
    write(10,'("prop",i5.5)') istep
    write(10,'("ASCII")')
    write(10,'("DATASET UNSTRUCTURED_GRID")')
    !
    write(10,'("POINTS ",I9.9," double")') nosum
    do iLv=0,LvMax
       do iz=0,pzmax*izmax*(2**iLv)
          do iy=0,pymax*iymax*(2**iLv)
             do ix=0,pxmax*ixmax*(2**iLv)
                if(NF(iLv,ix,iy,iz)==1) then
                   write(10,'(E14.7,1X,E14.7,1X,E14.7)') &
                        dble(ix)*dx(1)*(0.5d0**iLv), &
                        dble(iy)*dx(2)*(0.5d0**iLv), &
                        dble(iz)*dx(3)*(0.5d0**iLv)
                endif
             enddo
          enddo
       enddo
    enddo
    !
    write(10,'("CELLS ",i9.9," ",i9.9)') elsum,elsum*9
    do iLv=0,LvMax
       do iz=1,pzmax*izmax*(2**iLv)
          do iy=1,pymax*iymax*(2**iLv)
             do ix=1,pxmax*ixmax*(2**iLv)
                if(RF(iLv,ix,iy,iz)==1) then
                   write(10,'("8 ",I9,1X,I9,1X,I9,1X,I9,1X,I9,1X,I9,1X,I9,1X,I9)') &
                        ID(iLv,ix-1,iy-1,iz-1)-1,&
                        ID(iLv,ix  ,iy-1,iz-1)-1,&
                        ID(iLv,ix  ,iy  ,iz-1)-1,&
                        ID(iLv,ix-1,iy  ,iz-1)-1,&
                        ID(iLv,ix-1,iy-1,iz  )-1,&
                        ID(iLv,ix  ,iy-1,iz  )-1,&
                        ID(iLv,ix  ,iy  ,iz  )-1,&
                        ID(iLv,ix-1,iy,  iz  )-1
                endif
             enddo
          enddo
       enddo
    enddo
    write(10,'("CELL_TYPES ",i9.9)') elsum
    do iLv=0,LvMax
       do iz=1,pzmax*izmax*(2**iLv)
          do iy=1,pymax*iymax*(2**iLv)
             do ix=1,pxmax*ixmax*(2**iLv)
                if(RF(iLv,ix,iy,iz)==1) then
                   write(10,'(I9)') 12
                endif
             enddo
          enddo
       enddo
    enddo
    ! -- data --
    write(10,'("CELL_DATA ",I9)') elsum
    write(10,'("SCALARS F(1) double")')
    write(10,'("LOOKUP_TABLE default")')
    do iLv=0,LvMax
       do iz=1,pzmax*izmax*(2**iLv)
          do iy=1,pymax*iymax*(2**iLv)
             do ix=1,pxmax*ixmax*(2**iLv)
                if(RF(iLv,ix,iy,iz)==1) then
                   write(10,'(E14.7)') RD(1,iLv,ix,iy,iz)
                endif
             enddo
          enddo
       enddo
    enddo
    close(10)
    return
  end subroutine output_vtk

end program post_vtk
