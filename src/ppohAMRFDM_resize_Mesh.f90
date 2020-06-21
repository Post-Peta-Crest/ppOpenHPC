subroutine ppohAMRFDM_resize_Mesh(RSize,GSize,st_meshset,st_comm_info)
  use m_ppohAMRFDM_util
  implicit none
  integer(kind=ppohAMRFDM_kint),intent(in)::RSize,GSize
  type(st_ppohAMRFDM_meshset)::st_meshset
  type(st_ppohAMRFDM_comm_info)::st_comm_info
  integer(kind=ppohAMRFDM_kint)::index,RSize0,GSize0,nRSize,nGSize
  type(st_ppohAMRFDM_octset),pointer::p0,p1,p2
  RSize0=size(st_meshset%Mesh); GSize0=size(st_meshset%GMesh)
  if(RSize==RSize0.and.GSize==GSize0) return
  allocate(st_meshset% Mesh2(1:RSize))
  allocate(st_meshset%GMesh2(1:GSize))
  if(RSize<RSize0) then
     nRSize=RSize
  else
     nRSize=RSize0
  endif
  if(GSize<GSize0) then
     nGSize=GSize
  else
     nGSize=GSize0
  endif
  ! -- Copy to tmp --
  !$omp parallel default(none) &
  !$omp& private(index,p0,p1,p2) &
  !$omp& shared(st_meshset,st_comm_info,nRSize,nGSize)
  !$omp do
  do index=1,nRSize
     p0 => st_meshset%Mesh (index)
     p1 => st_meshset%Mesh2(index)
     ! -- substitute --
     st_meshset%Mesh2(index)=st_meshset%Mesh(index)
     ! -- reconnect pointers --
     ! - Prt -
     if(p0%octLv>0) then
        p2 => p0%octPrt
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octPrt => st_meshset%Mesh2(p2%octN)
           else
              p1%octPrt => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octPrt)
        endif
     else
        nullify(p1%octPrt)
     endif
     ! - Nb -
     p2 => p0%octNb1
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb1 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb1 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb1)
     endif
     p2 => p0%octNb2
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb2 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb2 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb2)
     endif
     p2 => p0%octNb3
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb3 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb3 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb3)
     endif
     p2 => p0%octNb4
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb4 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb4 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb4)
     endif
     p2 => p0%octNb5
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb5 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb5 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb5)
     endif
     p2 => p0%octNb6
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb6 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb6 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb6)
     endif
     ! - Ch -
     if(p0%iFLG(1)>0) then
        p2 => p0%octCh1
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh1 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh1 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh1)
        endif
        p2 => p0%octCh2
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh2 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh2 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh2)
        endif
        p2 => p0%octCh3
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh3 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh3 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh3)
        endif
        p2 => p0%octCh4
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh4 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh4 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh4)
        endif
        p2 => p0%octCh5
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh5 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh5 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh5)
        endif
        p2 => p0%octCh6
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh6 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh6 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh6)
        endif
        p2 => p0%octCh7
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh7 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh7 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh7)
        endif
        p2 => p0%octCh8
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh8 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh8 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh8)
        endif
     else
        nullify(p1%octCh1); nullify(p1%octCh2)
        nullify(p1%octCh3); nullify(p1%octCh4)
        nullify(p1%octCh5); nullify(p1%octCh6)
        nullify(p1%octCh7); nullify(p1%octCh8)
     endif
  enddo
  !$omp end do
  !$omp do
  do index=1,nGSize
     p0 => st_meshset%GMesh (index)
     p1 => st_meshset%GMesh2(index)
     ! -- substitute --
     st_meshset%GMesh2(index)=st_meshset%GMesh(index)
     ! -- reconnect pointers --
     ! - Prt -
     if(p0%octLv>0) then
        p2 => p0%octPrt
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octPrt => st_meshset%Mesh2(p2%octN)
           else
              p1%octPrt => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octPrt)
        endif
     else
        nullify(p1%octPrt)
     endif
     ! - Nb -
     p2 => p0%octNb1
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb1 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb1 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb1)
     endif
     p2 => p0%octNb2
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb2 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb2 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb2)
     endif
     p2 => p0%octNb3
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb3 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb3 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb3)
     endif
     p2 => p0%octNb4
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb4 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb4 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb4)
     endif
     p2 => p0%octNb5
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb5 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb5 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb5)
     endif
     p2 => p0%octNb6
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb6 => st_meshset%Mesh2(p2%octN)
        else
           p1%octNb6 => st_meshset%GMesh2(p2%octN)
        endif
     else
        nullify(p1%octNb6)
     endif
     if(p0%iFLG(1)>0) then
        ! - Ch -
        p2 => p0%octCh1
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh1 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh1 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh1)
        endif
        p2 => p0%octCh2
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh2 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh2 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh2)
        endif
        p2 => p0%octCh3
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh3 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh3 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh3)
        endif
        p2 => p0%octCh4
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh4 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh4 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh4)
        endif
        p2 => p0%octCh5
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh5 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh5 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh5)
        endif
        p2 => p0%octCh6
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh6 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh6 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh6)
        endif
        p2 => p0%octCh7
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh7 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh7 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh7)
        endif
        p2 => p0%octCh8
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh8 => st_meshset%Mesh2(p2%octN)
           else
              p1%octCh8 => st_meshset%GMesh2(p2%octN)
           endif
        else
           nullify(p1%octCh8)
        endif
     else
        nullify(p1%octCh1); nullify(p1%octCh2)
        nullify(p1%octCh3); nullify(p1%octCh4)
        nullify(p1%octCh5); nullify(p1%octCh6)
        nullify(p1%octCh7); nullify(p1%octCh8)
     endif
  enddo
  !$omp end do
  !$omp end parallel
  ! - resize Mesh -
  deallocate(st_meshset% Mesh)
  deallocate(St_meshset%GMesh)
  allocate(st_meshset% Mesh(1:RSize))
  allocate(st_meshset%GMesh(1:GSize))
  ! - reconstruct Mesh -
  !$omp parallel default(none) &
  !$omp& private(index,p0,p1,p2) &
  !$omp& shared(st_meshset,st_comm_info,nRSize,nGSize)
  !$omp do
  do index=1,nRSize
     p0 => st_meshset%Mesh2(index)
     p1 => st_meshset% Mesh(index)
     ! -- substitute --
     st_meshset%Mesh(index)=st_meshset%Mesh2(index)
     ! -- reconnect pointers --
     ! - Prt -
     if(p0%octLv>0) then
        p2 => p0%octPrt
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octPrt => st_meshset%Mesh(p2%octN)
           else
              p1%octPrt => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octPrt)
        endif
     else
        nullify(p1%octPrt)
     endif
     ! - Nb -
     p2 => p0%octNb1
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb1 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb1 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb1)
     endif
     p2 => p0%octNb2
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb2 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb2 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb2)
     endif
     p2 => p0%octNb3
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb3 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb3 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb3)
     endif
     p2 => p0%octNb4
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb4 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb4 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb4)
     endif
     p2 => p0%octNb5
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb5 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb5 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb5)
     endif
     p2 => p0%octNb6
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb6 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb6 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb6)
     endif
     ! - Ch -
     if(p0%iFLG(1)>0) then
        p2 => p0%octCh1
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh1 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh1 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh1)
        endif
        p2 => p0%octCh2
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh2 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh2 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh2)
        endif
        p2 => p0%octCh3
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh3 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh3 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh3)
        endif
        p2 => p0%octCh4
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh4 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh4 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh4)
        endif
        p2 => p0%octCh5
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh5 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh5 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh5)
        endif
        p2 => p0%octCh6
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh6 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh6 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh6)
        endif
        p2 => p0%octCh7
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh7 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh7 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh7)
        endif
        p2 => p0%octCh8
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh8 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh8 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh8)
        endif
     else
        nullify(p1%octCh1); nullify(p1%octCh2)
        nullify(p1%octCh3); nullify(p1%octCh4)
        nullify(p1%octCh5); nullify(p1%octCh6)
        nullify(p1%octCh7); nullify(p1%octCh8)
     endif
  enddo
  !$omp end do
  !$omp do
  do index=1,nGSize
     p0 => st_meshset%GMesh2(index)
     p1 => st_meshset%GMesh (index)
     ! -- substitute --
     st_meshset%GMesh(index)=st_meshset%GMesh2(index)
     ! -- reconnect pointers --
     ! - Prt -
     if(p0%octLv>0) then
        p2 => p0%octPrt
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octPrt => st_meshset%Mesh(p2%octN)
           else
              p1%octPrt => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octPrt)
        endif
     else
        nullify(p1%octPrt)
     endif
     ! - Nb -
     p2 => p0%octNb1
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb1 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb1 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb1)
     endif
     p2 => p0%octNb2
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb2 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb2 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb2)
     endif
     p2 => p0%octNb3
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb3 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb3 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb3)
     endif
     p2 => p0%octNb4
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb4 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb4 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb4)
     endif
     p2 => p0%octNb5
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb5 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb5 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb5)
     endif
     p2 => p0%octNb6
     if(associated(p2)) then
        if(p2%MrtN>=st_comm_info%MinMn.and.&
             p2%MrtN<=st_comm_info%MaxMn) then
           p1%octNb6 => st_meshset%Mesh(p2%octN)
        else
           p1%octNb6 => st_meshset%GMesh(p2%octN)
        endif
     else
        nullify(p1%octNb6)
     endif
     ! - Ch -
     if(p0%iFLG(1)>0) then
        p2 => p0%octCh1
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh1 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh1 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh1)
        endif
        p2 => p0%octCh2
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh2 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh2 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh2)
        endif
        p2 => p0%octCh3
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh3 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh3 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh3)
        endif
        p2 => p0%octCh4
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh4 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh4 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh4)
        endif
        p2 => p0%octCh5
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh5 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh5 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh5)
        endif
        p2 => p0%octCh6
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh6 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh6 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh6)
        endif
        p2 => p0%octCh7
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh7 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh7 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh7)
        endif
        p2 => p0%octCh8
        if(associated(p2)) then
           if(p2%MrtN>=st_comm_info%MinMn.and.&
                p2%MrtN<=st_comm_info%MaxMn) then
              p1%octCh8 => st_meshset%Mesh(p2%octN)
           else
              p1%octCh8 => st_meshset%GMesh(p2%octN)
           endif
        else
           nullify(p1%octCh8)
        endif
     else
        nullify(p1%octCh1); nullify(p1%octCh2)
        nullify(p1%octCh3); nullify(p1%octCh4)
        nullify(p1%octCh5); nullify(p1%octCh6)
        nullify(p1%octCh7); nullify(p1%octCh8)
     endif
  enddo
  !$omp end do
  !$omp end parallel
  deallocate(st_meshset% Mesh2)
  deallocate(st_meshset%GMesh2)
  return
end subroutine ppohAMRFDM_resize_Mesh
