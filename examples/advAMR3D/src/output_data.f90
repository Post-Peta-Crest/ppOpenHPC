subroutine output_data(step,st_param,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer,intent(in)::step
  type(st_ppohAMRFDM_param)::st_param
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  type(st_ppohAMRFDM_octset),pointer::p0
  integer::iLv,index,ID
  character(len=80)::filename
  write(filename,'("./results/prop",i5.5,"rank",i4.4,".dat")') step,st_comm_info%rank
  open(unit=10,file=filename,status='replace')
  do iLv=0,st_param%LvMax
     if(st_meshset%MinID(1,iLv)>=st_meshset%MaxID(1,iLv)) cycle
     do index=st_meshset%MinID(1,iLv),st_meshset%MaxID(1,iLv)
        p0 => st_meshset%Mesh(index)
        if((p0%iFLG(1)>=0).and.(p0%iFLG(1)<=st_param%nfg-1)) then
           do ID=1,st_param%npy
              if(dabs(p0%F(ID))<1.0d-15) p0%F(ID)=0.0d0
           enddo
           write(10,'(I9,2X,I9,2X,I9,2X,E14.7,2X,I9)') p0%iPOS(1),p0%iPOS(2),p0%iPOS(3),p0%F(1),iLv
        endif
     enddo
  enddo
  close(10)
  return
end subroutine output_data
