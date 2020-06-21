subroutine kernel_adv(iLv,st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer,intent(in)::iLv
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer::nID
  double precision::dt,dx(1:3)
  ! -- get parameters --
  call ppohAMRFDM_load_parameters(iLv,dt,dx,nID,st_param)
  ! -- x direction --
  call advection_RCIP("x",iLv,dt,dx,nID,st_meshset)
  call ppohAMRFDM_passings(iLv,st_param,st_meshset,st_comm_info)
  ! -- y direction --
  call advection_RCIP("y",iLv,dt,dx,nID,st_meshset)
  call ppohAMRFDM_passings(iLv,st_param,st_meshset,st_comm_info)
  ! -- z direction --
  call advection_RCIP("z",iLv,dt,dx,nID,st_meshset)
  call ppohAMRFDM_passings(iLv,st_param,st_meshset,st_comm_info)
  return
end subroutine kernel_adv

subroutine advection_RCIP(ch,iLv,dt,dx,nID,st_meshset)
  use m_ppohAMRFDM_util
  implicit none
  character(len=1),intent(in)::ch
  integer,intent(in)::iLv,nID
  double precision,intent(in)::dt,dx(1:3)
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_octset)::p0
  integer::index
  double precision::us(1:3),PV,PVup,VIA,VIAup,nPIV,nVIA
  us(1)=1.0d0; us(2)=1.0d0; us(3)=0.0d0
  !$omp parallel default(none) &
  !$omp& private(index,p0,PV,PVup,VIA,VIAup,nPIV,nVIA) &
  !$omp& shared(nID,iLv,ch,us,dt,dx,st_meshset)
  !$omp do
  do index=1,nID
     call ppohAMRFDM_load_mesh(iLv,index,p0,st_meshset)
     if(ch=="x") then
        ! -- x, dfdx (1,2) --
        PV=p0%F(1); PVup=p0%octNb1%F(1)
        VIA=p0%F(2); VIAup=p0%octNb1%F(2)
        call RCIP1D(PV,PVup,VIA,VIAup,us(1),nPIV,nVIA,dt,dx(1))
        p0%C(1)=nPIV; p0%C(2)=nVIA
        ! -- dfdy, dfdxdy (3,5) --
        PV=p0%F(3); PVup=p0%octNb1%F(3)
        VIA=p0%F(5); VIAup=p0%octNb1%F(5)
        call RCIP1D(PV,PVup,VIA,VIAup,us(1),nPIV,nVIA,dt,dx(1))
        p0%C(3)=nPIV; p0%C(5)=nVIA
        ! -- dfdz, dfdxdz (4,7) --
        PV=p0%F(4); PVup=p0%octNb1%F(4)
        VIA=p0%F(7); VIAup=p0%octNb1%F(7)
        call RCIP1D(PV,PVup,VIA,VIAup,us(1),nPIV,nVIA,dt,dx(1))
        p0%C(4)=nPIV; p0%C(7)=nVIA
     else if(ch=="y") then
        ! -- f, dfdy (1,3) --
        PV=p0%F(1); PVup=p0%octNb3%F(1)
        VIA=p0%F(3); VIAup=p0%octNb3%F(3)
        call RCIP1D(PV,PVup,VIA,VIAup,us(2),nPIV,nVIA,dt,dx(2))
        p0%C(1)=nPIV; p0%C(3)=nVIA
        ! -- dfdx, dfdxdy (2,5) --
        PV=p0%F(2); PVup=p0%octNb3%F(2)
        VIA=p0%F(5); VIAup=p0%octNb3%F(5)
        call RCIP1D(PV,PVup,VIA,VIAup,us(2),nPIV,nVIA,dt,dx(2))
        p0%C(2)=nPIV; p0%C(5)=nVIA
        ! -- dfdz, dfdydz (4,6) --
        PV=p0%F(4); PVup=p0%octNb3%F(4)
        VIA=p0%F(6); VIAup=p0%octNb3%F(6)
        call RCIP1D(PV,PVup,VIA,VIAup,us(2),nPIV,nVIA,dt,dx(2))
        p0%C(4)=nPIV; p0%C(6)=nVIA
     else if(ch=="z") then
        ! -- f, dfdz (1,4) --
        PV=p0%F(1); PVup=p0%octNb5%F(1)
        VIA=p0%F(4); VIAup=p0%octNb5%F(4)
        call RCIP1D(PV,PVup,VIA,VIAup,us(3),nPIV,nVIA,dt,dx(3))
        p0%C(1)=nPIV; p0%C(4)=nVIA
        ! -- dfdx, dfdxdz (2,7) --
        PV=p0%F(2); PVup=p0%octNb5%F(2)
        VIA=p0%F(7); VIAup=p0%octNb5%F(7)
        call RCIP1D(PV,PVup,VIA,VIAup,us(3),nPIV,nVIA,dt,dx(3))
        p0%C(2)=nPIV; p0%C(7)=nVIA
        ! -- dfdy, dfdydz (3,6) --
        PV=p0%F(3); PVup=p0%octNb5%F(3)
        VIA=p0%F(6); VIAup=p0%octNb5%F(6)
        call RCIP1D(PV,PVup,VIA,VIAup,us(3),nPIV,nVIA,dt,dx(3))
        p0%C(3)=nPIV; p0%C(6)=nVIA
     endif
     call ppohAMRFDM_store_mesh(iLv,index,p0,st_meshset)
  enddo
  !$omp end do
  !$omp do
  do index=1,nID
     call ppohAMRFDM_load_mesh(iLv,index,p0,st_meshset)
     if(ch=="x") then
        p0%F(1)=p0%C(1); p0%F(2)=p0%C(2)
        p0%F(3)=p0%C(3); p0%F(5)=p0%C(5)
        p0%F(4)=p0%C(4); p0%F(7)=p0%C(7)
     else if(ch=="y") then
        p0%F(1)=p0%C(1); p0%F(3)=p0%C(3)
        p0%F(2)=p0%C(2); p0%F(5)=p0%C(5)
        p0%F(4)=p0%C(4); p0%F(6)=p0%C(6)
     else if(ch=="z") then
        p0%F(1)=p0%C(1); p0%F(4)=p0%C(4)
        p0%F(2)=p0%C(2); p0%F(7)=p0%C(7)
        p0%F(3)=p0%C(3); p0%F(6)=p0%C(6)
     endif
     call ppohAMRFDM_store_mesh(iLv,index,p0,st_meshset)
  enddo
  !$omp end do
  !$omp end parallel
  return
end subroutine advection_RCIP
