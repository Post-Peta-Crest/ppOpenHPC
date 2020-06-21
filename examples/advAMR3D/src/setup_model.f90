subroutine setup_model(st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  type(st_ppohAMRFDM_octset)::p0
  integer::index,ix,iy,iz,nID
  integer::six,siy,siz,eix,eiy,eiz
  double precision::dt,dx(1:3)
  call ppohAMRFDM_load_parameters(st_param%initLv,dt,dx,nID,st_param)
  six=(st_param%pxmax*st_param%ixmax*2**st_param%initLv)/2&
     -(st_param%pxmax*st_param%ixmax*2**st_param%initLv)/16+1
  eix=(st_param%pxmax*st_param%ixmax*2**st_param%initLv)/2&
     +(st_param%pxmax*st_param%ixmax*2**st_param%initLv)/16
  siy=(st_param%pymax*st_param%iymax*2**st_param%initLv)/2&
     -(st_param%pymax*st_param%iymax*2**st_param%initLv)/16+1
  eiy=(st_param%pymax*st_param%iymax*2**st_param%initLv)/2&
     +(st_param%pymax*st_param%iymax*2**st_param%initLv)/16
  siz=(st_param%pzmax*st_param%izmax*2**st_param%initLv)/2&
     -(st_param%pzmax*st_param%izmax*2**st_param%initLv)/16+1
  eiz=(st_param%pzmax*st_param%izmax*2**st_param%initLv)/2&
     +(st_param%pzmax*st_param%izmax*2**st_param%initLv)/16
  do index=1,nID
     call ppohAMRFDM_load_mesh(st_param%initLv,index,p0,st_meshset)
     p0%F(:)=1.0d0
     call ppohAMRFDM_load_index(st_param%initLv,index,st_param,st_meshset,ix,iy,iz)
     if(ix>=six.and.ix<=eix.and.iy>=siy.and.iy<=eiy.and.iz>=siz.and.iz<=eiz) then
        p0%F(1)=2.0d0
     endif
     call ppohAMRFDM_store_mesh(st_param%initLv,index,p0,st_meshset)
  enddo
  call ppohAMRFDM_passings(st_param%initLv,st_param,st_meshset,st_comm_info)
  ! -- set gradient variables for CIP scheme --
  do index=1,nID
     call ppohAMRFDM_load_mesh(st_param%initLv,index,p0,st_meshset)
     p0%F(2)=0.5d0*(p0%octNb2%F(1)-p0%octNb1%F(1))/dx(1)
     p0%F(3)=0.5d0*(p0%octNb4%F(1)-p0%octNb3%F(1))/dx(2)
     p0%F(4)=0.5d0*(p0%octNb6%F(1)-p0%octNb5%F(1))/dx(3)
     p0%F(5)=0.25d0*(p0%octNb2%octNb4%F(1)+p0%octNb1%octNb3%F(1) &
          -p0%octNb1%octNb4%F(1)-p0%octNb2%octNb3%F(1))/(dx(1)*dx(2))
     p0%F(6)=0.25d0*(p0%octNb4%octNb6%F(1)+p0%octNb3%octNb5%F(1) &
          -p0%octNb3%octNb6%F(1)-p0%octNb4%octNb5%F(1))/(dx(2)*dx(3))
     p0%F(7)=0.25d0*(p0%octNb2%octNb6%F(1)+p0%octNb1%octNb5%F(1) &
          -p0%octNb1%octNb6%F(1)-p0%octNb2%octNb5%F(1))/(dx(1)*dx(3))
     call ppohAMRFDM_store_mesh(st_param%initLv,index,p0,st_meshset)
  enddo
  call ppohAMRFDM_passings(st_param%initLv,st_param,st_meshset,st_comm_info)
  return
end subroutine setup_model
