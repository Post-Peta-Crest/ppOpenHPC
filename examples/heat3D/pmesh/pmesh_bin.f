      include 'mpif.h'
      include 'precision.inc'

      integer(kind=kint ), dimension(:,:), allocatable :: sta1
      integer(kind=kint ), dimension(:,:), allocatable :: sta2
      integer(kind=kint ), dimension(:  ), allocatable :: req1
      integer(kind=kint ), dimension(:  ), allocatable :: req2  

      integer :: PEsmpTOT, FTflag
      integer :: PETOT, my_rank, SOLVER_COMM, errno

      character(len= 4 ) ::  penum, penum_left
      character(len= 9 ) ::  fname, rname

      integer          :: NEIBPETOT
      integer, pointer :: NOD_STACK_IMPORT(:), NOD_IMPORT(:)
      integer, pointer :: NOD_STACK_EXPORT(:), NOD_EXPORT(:)
      integer, pointer :: NOD_EXPORT_NEW(:), NEIBPE(:), RNEIBPE(:)
      integer, pointer :: xNEIBPE(:)

      integer, pointer :: VAL_STACK_IMPORT(:), VAL_NOD_IMPORT(:)
      integer, pointer :: VAL_STACK_EXPORT(:), VAL_NOD_EXPORT(:)
      integer, pointer :: ELM_STACK_IMPORT(:), ELM_NOD_IMPORT(:)
      integer, pointer :: ELM_STACK_EXPORT(:), ELM_NOD_EXPORT(:)
      integer, pointer :: WS(:), WR(:), IWK(:)

      integer:: ICELTOT
      integer(kind=kint), dimension(:,:),allocatable :: ICELNOD
      integer(kind=kint), dimension(:),  allocatable :: BC_STACK
      integer(kind=kint), dimension(:),  allocatable :: BC_NOD

      integer(kind=kint), dimension(:,:),  
     &                         allocatable :: pNODE_ID, pELEM_ID

      real   (kind=kreal), dimension(:,:),  allocatable :: XYZ
      character(len=80), dimension(4) :: BC_NAME

      integer(kind=kint ), parameter  ::  l_err = 6

      integer, parameter   ::  ndepth = 1
      integer(kind=kint ), parameter  ::  neibpetot_max = 26

      integer(kind=kint )        ::  nx_all , ny_all , nz_all
      integer(kind=kint ), save  ::  npp, ndx, ndy, ndz           
      integer(kind=kint ), save  ::  ITERmax

      integer(kind=kint )  ::  nxi, nyi, nzi  
      integer(kind=kint )  ::  nx , ny , nz 

      integer(kind=kint )  ::  ipe    , jpe    , kpe    , pe_id
      integer(kind=kint )  ::  inp    , jnp    , knp
      integer(kind=kint )  ::  inp_st , jnp_st , knp_st ,               &
     &                         inp_end, jnp_end, knp_end

      integer(kind=kint )  ::  ioff   , joff   , koff
      integer(kind=kint )  ::  i      , j      , k  
      integer(kind=kint )  ::  i_st   , j_st   , k_st   ,               &
     &                         i_end  , j_end  , k_end
      integer(kind=kint )  ::  is     , js     , ks     ,               &
     &                         ie     , je     , ke   

      integer(kind=kint )  ::  nodtot,   intnodtot, elmtot, enod
      integer(kind=kint )  ::  item_pos, item_tot

      character(len=12 )   ::  WORKFIL
      character(len=80 )   ::  meshfil, header

      integer(kind=kint ), dimension(:,:,:), allocatable  :: node_id_lc
      integer(kind=kint ), dimension(:,:,:), allocatable  :: node_id_gl
      integer(kind=kint ), dimension(:), allocatable  :: Pindex

      integer(kind=kint )  ::  node_id, element_id, element_id_gl

      integer(kind=kint), dimension(:), allocatable :: nWORK

      integer(kind=kint )  ::  elmgrptot, nodgrptot, sufgrptot

      real   (kind=kreal)  ::  forc
      integer(kind=kint )  ::  i1 , i2, i3, i4, i5, i6, i7, i8

      real   (kind=kreal)                :: eps         = 1d-7
      integer(kind=kint )                :: max_itr   
      real   (kind=kreal)                :: large_value = 1.0e30_kreal
      integer(kind=kint )                :: nparm       =   3
      real   (kind=kreal)                :: p_corner, p_line, p_mid

      integer(kind=kint ), parameter            ::  nindex = 6

      integer :: CONFIGopt
      integer, dimension(:), allocatable :: IWKG
      integer, dimension(:), allocatable :: INT_ELM_LIST

      call MPI_INIT      (ierr)
      call MPI_COMM_SIZE (MPI_COMM_WORLD, PETOT, ierr )
      call MPI_COMM_RANK (MPI_COMM_WORLD, my_rank, ierr )

      write(WORKFIL,'(a,i6.6)') 'wk.p_', my_rank

      BC_NAME(1)= 'Xmin'
      BC_NAME(2)= 'Ymin'
      BC_NAME(3)= 'Zmin'
      BC_NAME(4)= 'Zmax'

      if (my_rank.eq.0) then
        open (11, file='mesh.inp',status='unknown', form='formatted')
        read (11,*)  npx, npy, npz
        read (11,*)  ndx, ndy, ndz
        read (11,'(a80)')  header
        write (*,'(3i10)') npx, npy, npz
        write (*,'(3i10)') ndx, ndy, ndz
        write (*,'(a79)') header
        close (11)
      endif

      call mpi_bcast (npx, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call mpi_bcast (npy, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call mpi_bcast (npz, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call mpi_bcast (ndx, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call mpi_bcast (ndy, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call mpi_bcast (ndz, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
      call mpi_bcast (header,80, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)

      nxi= npx/ndx
      nyi= npy/ndy
      nzi= npz/ndz
 
      CONFIGopt=1

      nx_all= ndx * nxi
      ny_all= ndy * nyi
      nz_all= ndz * nzi
!
! ***** set internal node count
!

      nxi = nx_all / ndx
      nyi = ny_all / ndy
      nzi = nz_all / ndz
!
! ***** allocate nodal id table
!
      allocate ( node_id_lc(nxi+2*ndepth,nyi+2*ndepth,nzi+2*ndepth) )
      allocate ( node_id_gl(nxi+2*ndepth,nyi+2*ndepth,nzi+2*ndepth) )
!
! **********   domain loop for each pe   **********
!
      pe_id = 1

!      write (*,*) 'INITIAL ID ?'
!      read  (*,*) INIT_ID 
      INIT_ID= 0
      INIT_ID= INIT_ID + 1

      do kpe=1,ndz
        do jpe=1,ndy
          do ipe=1,ndx
!
! ***** open output file
!
          if (pe_id-1.eq.my_rank) then

!
! ***** set and write basic local model parameters
!                                       .. pe nod per 1 line
                             nx = nxi
            if (ipe /=   1)  nx = nx + ndepth
            if (ipe /= ndx)  nx = nx + ndepth

                             ny = nyi
            if (jpe /=   1)  ny = ny + ndepth
            if (jpe /= ndy)  ny = ny + ndepth

                             nz = nzi
            if (kpe /=   1)  nz = nz + ndepth
            if (kpe /= ndz)  nz = nz + ndepth

!                                       .. total node
            nodtot    = nx  * ny  * nz
!                                       .. internal nodes
            intnodtot = nxi * nyi * nzi
!                                       .. search neighbor pe
                            inp_st  = -1
            if (ipe ==   1) inp_st  =  0
                            inp_end =  1
            if (ipe == ndx) inp_end =  0

                            jnp_st  = -1
            if (jpe ==   1) jnp_st  =  0
                            jnp_end =  1
            if (jpe == ndy) jnp_end =  0

                            knp_st  = -1
            if (kpe ==   1) knp_st  =  0
                            knp_end =  1
            if (kpe == ndz) knp_end =  0
!                                       .. set neighbor pe
            neibpetot = 0
            do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end
                do inp=inp_st,inp_end

                  if ((inp==0).and.(jnp==0).and.(knp==0)) cycle
                  neibpetot = neibpetot  + 1
                enddo
              enddo
            enddo

            allocate (NOD_STACK_IMPORT(0:neibpetot))
            allocate (NOD_STACK_EXPORT(0:neibpetot))

            if (neibpetot.eq.0) then
              allocate (NEIBPE(1))              
             else
              allocate (NEIBPE(neibpetot))
           endif     

            NOD_STACK_IMPORT= 0
            NOD_STACK_EXPORT= 0
            NEIBPE= 0

            neibpetot = 0
            do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end
                do inp=inp_st,inp_end

                  if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

                  neibpetot = neibpetot  + 1
                  neibpe     (neibpetot) =  pe_id    +                  &
     &                                  inp + jnp*ndx + knp*ndx*ndy
                enddo
              enddo
            enddo

            do neib= 1, NEIBPETOT
              ip= NEIBPE(neib)
              NEIBPE(neib)= ip - 1
            enddo

            NP= nodtot
            N = intnodtot

            allocate (XYZ(3,NP))

!
! ***** set coordinate off set (starting corner for pe node)
!

!
! ***** set nodal position off set (i,j,k starting position -1)
!
                        ioff = (ipe-1)*nxi
            if (ipe/=1) ioff = ioff - ndepth
                        joff = (jpe-1)*nyi
            if (jpe/=1) joff = joff - ndepth
                        koff = (kpe-1)*nzi
            if (kpe/=1) koff = koff - ndepth
!
! ***** set and write coordinate for internal nodes
!
            node_id = 0
                            i_st  =     1         + ndepth
            if (ipe ==   1) i_st  =     1
                            i_end =  i_st + nxi-1

                            j_st  =     1         + ndepth
            if (jpe ==   1) j_st  =     1
                            j_end =  j_st + nyi-1

                            k_st  =     1         + ndepth
            if (kpe ==   1) k_st  =     1
                            k_end =  k_st + nzi-1

            do k=k_st,k_end
              do j=j_st,j_end
                do i=i_st,i_end

                  node_id = node_id + 1

                  node_id_lc(i,j,k) =  node_id
                  node_id_gl(i,j,k) = (ioff+i  ) +                      &
     &                                (joff+j-1)*nx_all +               &
     &                                (koff+k-1)*nx_all*ny_all 
                  XYZ(1,node_id)= dfloat(ioff+i-1)
                  XYZ(2,node_id)= dfloat(joff+j-1)
                  XYZ(3,node_id)= dfloat(koff+k-1)
                enddo
              enddo
            enddo
!
! ***** set and write coordinate for sleeve area nodes
!
!!          write(l_out,'(a)') ' '

            do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end
                do inp=inp_st,inp_end

                  if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

!                                       .. start side

                  if ( inp == -1 )  is=      1
                  if ( inp == -1 )  ie=      1
                  if ( jnp == -1 )  js=      1
                  if ( jnp == -1 )  je=      1
                  if ( knp == -1 )  ks=      1
                  if ( knp == -1 )  ke=      1

!                                       .. finish side

                  if ( inp ==  1 )  is=i_end+1
                  if ( inp ==  1 )  ie=i_end+1
                  if ( jnp ==  1 )  js=j_end+1
                  if ( jnp ==  1 )  je=j_end+1
                  if ( knp ==  1 )  ks=k_end+1
                  if ( knp ==  1 )  ke=k_end+1

!                                       .. line pattern

                  if ( inp ==  0 )  is=i_st
                  if ( inp ==  0 )  ie=i_end
                  if ( jnp ==  0 )  js=j_st
                  if ( jnp ==  0 )  je=j_end
                  if ( knp ==  0 )  ks=k_st
                  if ( knp ==  0 )  ke=k_end

                  do k=ks,ke
                   do j=js,je
                    do i=is,ie

                      node_id = node_id + 1

                      node_id_lc(i,j,k) =  node_id
                      node_id_gl(i,j,k) = (ioff+i  ) +                  &
     &                                    (joff+j-1)*nx_all +           &
     &                                    (koff+k-1)*nx_all*ny_all 
                      XYZ(1,node_id)= dfloat(ioff+i-1)
                      XYZ(2,node_id)= dfloat(joff+j-1)
                      XYZ(3,node_id)= dfloat(koff+k-1)
                    enddo
                   enddo
                  enddo

                enddo
              enddo
            enddo
!
! ..... write 2.2 element (connection)
!

            elmtot = (nx-1)*(ny-1)*(nz-1)
            enod   =  8
          
            ICELTOT= elmtot
            allocate (ICELNOD(ICELTOT,8))
            element_id = 0

            do k=1,nz-1
              do j=1,ny-1
                do i=1,nx-1

                  element_id    =  element_id + 1
                  element_id_gl =  (ioff+i  ) +                         &
     &                             (joff+j-1)*(nx_all-1) +              &
     &                             (koff+k-1)*(nx_all-1)*(ny_all-1)

                  i1 = node_id_lc( i  , j  , k   )
                  i2 = node_id_lc( i+1, j  , k   )
                  i3 = node_id_lc( i  , j+1, k   )
                  i4 = node_id_lc( i+1, j+1, k   )
                  i5 = node_id_lc( i  , j  , k+1 )
                  i6 = node_id_lc( i+1, j  , k+1 )
                  i7 = node_id_lc( i  , j+1, k+1 )
                  i8 = node_id_lc( i+1, j+1, k+1 )

                  ICELNOD(element_id,1)= i1
                  ICELNOD(element_id,2)= i2
                  ICELNOD(element_id,3)= i4
                  ICELNOD(element_id,4)= i3
                  ICELNOD(element_id,5)= i5
                  ICELNOD(element_id,6)= i6
                  ICELNOD(element_id,7)= i8
                  ICELNOD(element_id,8)= i7

                enddo
              enddo
            enddo
!
! ..... write 3.import / export information
!
!
! ***** set and write import nodes
!                                     .... count nodes 

            NOD_STACK_IMPORT= 0
            NOD_STACK_EXPORT= 0

            neibpetot = 0
            node_id   = 0

            do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end
                do inp=inp_st,inp_end

                  if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

!                                       .. start side

                  if ( inp == -1 )  is=      1
                  if ( inp == -1 )  ie=      1
                  if ( jnp == -1 )  js=      1
                  if ( jnp == -1 )  je=      1
                  if ( knp == -1 )  ks=      1
                  if ( knp == -1 )  ke=      1

!                                       .. finish side

                  if ( inp ==  1 )  is=i_end+1
                  if ( inp ==  1 )  ie=i_end+1
                  if ( jnp ==  1 )  js=j_end+1
                  if ( jnp ==  1 )  je=j_end+1
                  if ( knp ==  1 )  ks=k_end+1
                  if ( knp ==  1 )  ke=k_end+1

!                                       .. line pattern

                  if ( inp ==  0 )  is=i_st
                  if ( inp ==  0 )  ie=i_end
                  if ( jnp ==  0 )  js=j_st
                  if ( jnp ==  0 )  je=j_end
                  if ( knp ==  0 )  ks=k_st
                  if ( knp ==  0 )  ke=k_end

                  do k=ks,ke
                   do j=js,je
                    do i=is,ie
                      node_id = node_id + 1
                    enddo
                   enddo
                  enddo

                  neibpetot = neibpetot  + 1
                  NOD_STACK_IMPORT(neibpetot) = node_id

                enddo
              enddo
            enddo

!                                     .... write nodes 
            neibpetot = 0
            open   (21, file= WORKFIL, form= 'unformatted',             &
     &                  status='unknown')
            rewind (21)
            do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end
                do inp=inp_st,inp_end

                  if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

!                                       .. start side

                  if ( inp == -1 )  is=      1
                  if ( inp == -1 )  ie=      1
                  if ( jnp == -1 )  js=      1
                  if ( jnp == -1 )  je=      1
                  if ( knp == -1 )  ks=      1
                  if ( knp == -1 )  ke=      1

!                                       .. finish side

                  if ( inp ==  1 )  is=i_end+1
                  if ( inp ==  1 )  ie=i_end+1
                  if ( jnp ==  1 )  js=j_end+1
                  if ( jnp ==  1 )  je=j_end+1
                  if ( knp ==  1 )  ks=k_end+1
                  if ( knp ==  1 )  ke=k_end+1

!                                       .. line pattern

                  if ( inp ==  0 )  is=i_st
                  if ( inp ==  0 )  ie=i_end
                  if ( jnp ==  0 )  js=j_st
                  if ( jnp ==  0 )  je=j_end
                  if ( knp ==  0 )  ks=k_st
                  if ( knp ==  0 )  ke=k_end

                  neibpetot = neibpetot  + 1

                  do k=ks,ke
                   do j=js,je
                    do i=is,ie
                      write(21) node_id_lc(i,j,k)
                    enddo
                   enddo
                  enddo

                enddo
              enddo
            enddo

            close (21)

            if (neibpetot.ne.0) then
              nn= NOD_STACK_IMPORT(neibpetot)
              allocate (NOD_IMPORT(nn))
              NOD_IMPORT= 0
              open   (21, file= WORKFIL, form= 'unformatted',           &
     &                  status='old')
              rewind (21)
              allocate (nWORK(nn))
              do i= 1, nn
                read (21) nWORK(i)
              enddo

              do kk= 1, nn
                NOD_IMPORT(kk)= nWORK(kk)
              enddo

              deallocate (nWORK) 
            endif

!
! ***** set and write export nodes
!                                     .... count nodes 

            neibpetot = 0
            node_id   = 0

            do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end
                do inp=inp_st,inp_end

                  if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

!                                       .. start side

                  if ( inp == -1 )  is=i_st
                  if ( inp == -1 )  ie=i_st
                  if ( jnp == -1 )  js=j_st
                  if ( jnp == -1 )  je=j_st
                  if ( knp == -1 )  ks=k_st
                  if ( knp == -1 )  ke=k_st

!                                       .. finish side

                  if ( inp ==  1 )  is=i_end
                  if ( inp ==  1 )  ie=i_end
                  if ( jnp ==  1 )  js=j_end
                  if ( jnp ==  1 )  je=j_end
                  if ( knp ==  1 )  ks=k_end
                  if ( knp ==  1 )  ke=k_end

!                                       .. line pattern

                  if ( inp ==  0 )  is=i_st
                  if ( inp ==  0 )  ie=i_end
                  if ( jnp ==  0 )  js=j_st
                  if ( jnp ==  0 )  je=j_end
                  if ( knp ==  0 )  ks=k_st
                  if ( knp ==  0 )  ke=k_end

                  do k=ks,ke
                   do j=js,je
                    do i=is,ie
                      node_id = node_id + 1
                    enddo
                   enddo
                  enddo

                  neibpetot = neibpetot  + 1
                  NOD_STACK_EXPORT(neibpetot) = node_id

                enddo
              enddo
            enddo

!                                     .... write nodes 
            neibpetot = 0

            open   (21, file= WORKFIL, form= 'unformatted',             &
     &                  status='old')
            rewind (21)
            do knp=knp_st,knp_end
              do jnp=jnp_st,jnp_end
                do inp=inp_st,inp_end

                  if ((inp==0).and.(jnp==0).and.(knp==0)) cycle

!                                       .. start side

                  if ( inp == -1 )  is=i_st
                  if ( inp == -1 )  ie=i_st
                  if ( jnp == -1 )  js=j_st
                  if ( jnp == -1 )  je=j_st
                  if ( knp == -1 )  ks=k_st
                  if ( knp == -1 )  ke=k_st

!                                       .. finish side

                  if ( inp ==  1 )  is=i_end
                  if ( inp ==  1 )  ie=i_end
                  if ( jnp ==  1 )  js=j_end
                  if ( jnp ==  1 )  je=j_end
                  if ( knp ==  1 )  ks=k_end
                  if ( knp ==  1 )  ke=k_end

!                                       .. line pattern

                  if ( inp ==  0 )  is=i_st
                  if ( inp ==  0 )  ie=i_end
                  if ( jnp ==  0 )  js=j_st
                  if ( jnp ==  0 )  je=j_end
                  if ( knp ==  0 )  ks=k_st
                  if ( knp ==  0 )  ke=k_end

                  neibpetot = neibpetot  + 1

                  do k=ks,ke
                   do j=js,je
                    do i=is,ie

                      write(21)  node_id_lc(i,j,k)

                    enddo
                   enddo
                  enddo

                enddo
              enddo
            enddo

            close (21)

            nn= NOD_STACK_EXPORT(neibpetot)
            if (neibpetot.ne.0) then
              open   (21, file= WORKFIL, form= 'unformatted',           &
     &                  status='old')
              rewind (21)
              allocate (NOD_EXPORT(nn), NOD_EXPORT_NEW(nn))
              allocate (nWORK(nn))
              do i= 1, nn
                read (21) nWORK(i)
              enddo

              do kk= 1, nn
                NOD_EXPORT(kk)= nWORK(kk)
              enddo
              deallocate (nWORK) 
            endif
!
! ..... write 4.group information
!

! 
! ***** write boundary condition (x,y,z=0 plane sym., x-force)
!
            elmgrptot = 0
            nodgrptot = 0
            sufgrptot = 0
!                                       ... node    group

!                                        .. count node group and stack

            nodgrptot = 4
            allocate (BC_STACK(0:nodgrptot))
            BC_STACK= 0

!                                        .. count nodal item length
!                                                 .. XMIN
            item_tot = 0
            item_pos = 0

            item_pos= 1
            BC_STACK(item_pos)= BC_STACK(item_pos-1)
            if (ipe == 1) then 
              item_tot = item_tot +  ny*nz
              BC_STACK(item_pos) = item_tot
            endif
!                                                 .. YMIN
            item_pos= 2
            BC_STACK(item_pos)= BC_STACK(item_pos-1)
            if (jpe == 1) then 
              item_tot = item_tot +  nx*nz
              BC_STACK(item_pos) = item_tot
            endif
!                                                 .. ZMIN
            item_pos= 3
            BC_STACK(item_pos)= BC_STACK(item_pos-1)
            if (kpe == 1) then 
              item_tot = item_tot +  nx*ny
              BC_STACK(item_pos) = item_tot
            endif

!                                                 .. ZMAX
            item_pos= 4
            BC_STACK(item_pos)= BC_STACK(item_pos-1)
            if (kpe == ndz) then 
              item_tot = item_tot +  nx*ny
              BC_STACK(item_pos) = item_tot
            endif

            item_pos = 0

            nn= BC_STACK(4)
            allocate (BC_NOD(nn))

!                                                 .. XMIN
            if (ipe == 1) then 
              icc= 0
              do k= 1, nz
                do j= 1, ny
                  icc= icc + 1
                  in = BC_STACK(0) + icc
                  BC_NOD(in)= node_id_lc(1,j,k)
                enddo
              enddo
            endif
!                                                 .. YMIN
            if (jpe == 1) then 
              icc= 0
              do k= 1, nz
                do i= 1, nx
                  icc= icc + 1
                  in = BC_STACK(1) + icc
                  BC_NOD(in)= node_id_lc(i,1,k)
                enddo
              enddo
            endif
!                                                 .. ZMIN
            if (kpe == 1) then 
              icc= 0
              do j= 1, ny
                do i= 1, nx
                  icc= icc + 1
                  in = BC_STACK(2) + icc
                  BC_NOD(in)= node_id_lc(i,j,1)
                enddo
              enddo
            endif
!                                                 .. ZMAX
            if (kpe == ndz) then 
              icc= 0
              do j= 1, ny
                do i= 1, nx
                  icc= icc + 1
                  in = BC_STACK(3) + icc
                  BC_NOD(in)= node_id_lc(i,j,nz)
                enddo
              enddo
            endif

!C
!C +-------------+
!C | OUTPUT FILE |
!C +-------------+
!C===
      N0= 0
      N1= 1
      N4= 4
      N8= 361

      call DEFINE_FILE_NAME (header, meshfil, pe_id-1)
!C===

!C
!C +---------------+
!C | LOCAL NODE ID |
!C +---------------+
!C===
      allocate (pNODE_ID(NP,2), pELEM_ID(ICELTOT,2))
      allocate (WS(NP), WR(NP))

      do i= 1, N
        pNODE_ID(i,1)= i
        pNODE_ID(i,2)= my_rank
      enddo
   
      do ib= 1, NEIBPETOT
        neib= NEIBPE(ib)
        do k= NOD_STACK_IMPORT(ib-1)+1, NOD_STACK_IMPORT(ib)
          ii= NOD_IMPORT(k)
          pNODE_ID(ii,2)= neib
        enddo
      enddo

      allocate (sta1(MPI_STATUS_SIZE,NEIBPETOT))
      allocate (sta2(MPI_STATUS_SIZE,NEIBPETOT))
      allocate (req1(NEIBPETOT))
      allocate (req2(NEIBPETOT))

      do ib= 1, NEIBPETOT
        istart= NOD_STACK_EXPORT(ib-1)
        inum  = NOD_STACK_EXPORT(ib  ) - istart
        do k= NOD_STACK_EXPORT(ib-1)+1, NOD_STACK_EXPORT(ib)
          ii= NOD_EXPORT(k)
          WS(k)= pNODE_ID(ii,1)
        enddo
        call MPI_ISEND                                                  &
     &       (WS(istart+1), inum, MPI_INTEGER,                          &
     &        NEIBPE(ib), 0, MPI_COMM_WORLD, req1(ib), ierr)
      enddo

      do ib= 1, NEIBPETOT
        istart= NOD_STACK_IMPORT(ib-1)
        inum  = NOD_STACK_IMPORT(ib  ) - istart
        call MPI_IRECV                                                  &
     &       (WR(istart+1), inum, MPI_INTEGER,                          &
     &        NEIBPE(ib), 0, MPI_COMM_WORLD, req2(ib), ierr)
      enddo
      call MPI_WAITALL (NEIBPETOT, req2, sta2, ierr)
   
      do ib= 1, NEIBPETOT
        do k= NOD_STACK_IMPORT(ib-1)+1, NOD_STACK_IMPORT(ib)
          ii= NOD_IMPORT(k)
          pNODE_ID(ii,1)= WR(k)
        enddo
      enddo
      call MPI_WAITALL (NEIBPETOT, req1, sta1, ierr)
      deallocate (sta1, sta2, req1, req2, WS, WR)
!C===

!C
!C +-------------------+
!C | LOCAL ELEM ID - 1 |
!C +-------------------+
!C===

!C
!C-- LIST of INTERNAL ELEMENTs
      icou= 0
      allocate (INT_ELM_LIST(ICELTOT))
      do icel= 1, ICELTOT
        in1= ICELNOD(icel,1)
        in2= ICELNOD(icel,2)
        in3= ICELNOD(icel,3)
        in4= ICELNOD(icel,4)
        in5= ICELNOD(icel,5)
        in6= ICELNOD(icel,6)
        in7= ICELNOD(icel,7)
        in8= ICELNOD(icel,8)

        ip1= pNODE_ID(in1,2)
        ip2= pNODE_ID(in2,2)
        ip3= pNODE_ID(in3,2)
        ip4= pNODE_ID(in4,2)
        ip5= pNODE_ID(in5,2)
        ip6= pNODE_ID(in6,2)
        ip7= pNODE_ID(in7,2)
        ip8= pNODE_ID(in8,2)

        ip0= min(ip1,ip2,ip3,ip4,ip5,ip6,ip7,ip8)
        pELEM_ID(icel,2)= ip0

        if (ip0.eq.my_rank) then
          icou= icou + 1
          INT_ELM_LIST(icou)= icel
          pELEM_ID(icel,1)  = icou
        endif
      enddo
      ICELTOT_INT= icou

!C
!C-- LIST of EXPORTED ELEMENTs 
      allocate (ELM_STACK_EXPORT(0:NEIBPETOT), IWK(NEIBPETOT))
      allocate (rNEIBPE(0:PETOT), xNEIBPE(0:NEIBPETOT))
      rNEIBPE= -1
      IWK    =  0
      do ib= 1, NEIBPETOT
        neib= NEIBPE(ib)
        rNEIBPE(neib)= ib
      enddo
      rNEIBPE(my_rank)= 0

      ELM_STACK_EXPORT= 0
      do icel= 1, ICELTOT
        xNEIBPE= 0
        in1= ICELNOD(icel,1)
        in2= ICELNOD(icel,2)
        in3= ICELNOD(icel,3)
        in4= ICELNOD(icel,4)
        in5= ICELNOD(icel,5)
        in6= ICELNOD(icel,6)
        in7= ICELNOD(icel,7)
        in8= ICELNOD(icel,8)

        ip1= pNODE_ID(in1,2)
        ip2= pNODE_ID(in2,2)
        ip3= pNODE_ID(in3,2)
        ip4= pNODE_ID(in4,2)
        ip5= pNODE_ID(in5,2)
        ip6= pNODE_ID(in6,2)
        ip7= pNODE_ID(in7,2)
        ip8= pNODE_ID(in8,2)

        iv1= rNEIBPE(ip1)
        iv2= rNEIBPE(ip2)
        iv3= rNEIBPE(ip3)
        iv4= rNEIBPE(ip4)
        iv5= rNEIBPE(ip5)
        iv6= rNEIBPE(ip6)
        iv7= rNEIBPE(ip7)
        iv8= rNEIBPE(ip8)

        if (pELEM_ID(icel,2).eq.my_rank) then
        if (xNEIBPE(iv1).eq.0.and.ip1.ne.my_rank) then
          ELM_STACK_EXPORT(iv1)= ELM_STACK_EXPORT(iv1) + 1
                   xNEIBPE(iv1)= 1
        endif
        if (xNEIBPE(iv2).eq.0.and.ip2.ne.my_rank) then
          ELM_STACK_EXPORT(iv2)= ELM_STACK_EXPORT(iv2) + 1
                   xNEIBPE(iv2)= 1
        endif
        if (xNEIBPE(iv3).eq.0.and.ip3.ne.my_rank) then
          ELM_STACK_EXPORT(iv3)= ELM_STACK_EXPORT(iv3) + 1
                   xNEIBPE(iv3)= 1
        endif
        if (xNEIBPE(iv4).eq.0.and.ip4.ne.my_rank) then
          ELM_STACK_EXPORT(iv4)= ELM_STACK_EXPORT(iv4) + 1
                   xNEIBPE(iv4)= 1
        endif
        if (xNEIBPE(iv5).eq.0.and.ip5.ne.my_rank) then
          ELM_STACK_EXPORT(iv5)= ELM_STACK_EXPORT(iv5) + 1
                   xNEIBPE(iv5)= 1
        endif
        if (xNEIBPE(iv6).eq.0.and.ip6.ne.my_rank) then
          ELM_STACK_EXPORT(iv6)= ELM_STACK_EXPORT(iv6) + 1
                   xNEIBPE(iv6)= 1
        endif
        if (xNEIBPE(iv7).eq.0.and.ip7.ne.my_rank) then
          ELM_STACK_EXPORT(iv7)= ELM_STACK_EXPORT(iv7) + 1
                   xNEIBPE(iv7)= 1
        endif
        if (xNEIBPE(iv8).eq.0.and.ip8.ne.my_rank) then
          ELM_STACK_EXPORT(iv8)= ELM_STACK_EXPORT(iv8) + 1
                   xNEIBPE(iv8)= 1
        endif
        endif
      enddo

      do ib= 1, NEIBPETOT
        ELM_STACK_EXPORT(ib)= ELM_STACK_EXPORT(ib-1) + 
     &                        ELM_STACK_EXPORT(ib) 
      enddo

      nn= ELM_STACK_EXPORT(NEIBPETOT)
      allocate (ELM_NOD_EXPORT(nn))

      do icel= 1, ICELTOT
        xNEIBPE= 0
        in1= ICELNOD(icel,1)
        in2= ICELNOD(icel,2)
        in3= ICELNOD(icel,3)
        in4= ICELNOD(icel,4)
        in5= ICELNOD(icel,5)
        in6= ICELNOD(icel,6)
        in7= ICELNOD(icel,7)
        in8= ICELNOD(icel,8)

        ip1= pNODE_ID(in1,2)
        ip2= pNODE_ID(in2,2)
        ip3= pNODE_ID(in3,2)
        ip4= pNODE_ID(in4,2)
        ip5= pNODE_ID(in5,2)
        ip6= pNODE_ID(in6,2)
        ip7= pNODE_ID(in7,2)
        ip8= pNODE_ID(in8,2)

        iv1= rNEIBPE(ip1)
        iv2= rNEIBPE(ip2)
        iv3= rNEIBPE(ip3)
        iv4= rNEIBPE(ip4)
        iv5= rNEIBPE(ip5)
        iv6= rNEIBPE(ip6)
        iv7= rNEIBPE(ip7)
        iv8= rNEIBPE(ip8)

        if (pELEM_ID(icel,2).eq.my_rank) then
        if (xNEIBPE(iv1).eq.0.and.ip1.ne.my_rank) then
          IWK(iv1)= IWK(iv1) + 1
               kk = IWK(iv1) + ELM_STACK_EXPORT(iv1-1)
          ELM_NOD_EXPORT(kk)= pELEM_ID(icel,1)
                   xNEIBPE(iv1)= 1
        endif
        if (xNEIBPE(iv2).eq.0.and.ip2.ne.my_rank) then
          IWK(iv2)= IWK(iv2) + 1
               kk = IWK(iv2) + ELM_STACK_EXPORT(iv2-1)
          ELM_NOD_EXPORT(kk)= pELEM_ID(icel,1)
                   xNEIBPE(iv2)= 1
        endif
        if (xNEIBPE(iv3).eq.0.and.ip3.ne.my_rank) then
          IWK(iv3)= IWK(iv3) + 1
               kk = IWK(iv3) + ELM_STACK_EXPORT(iv3-1)
          ELM_NOD_EXPORT(kk)= pELEM_ID(icel,1)
                   xNEIBPE(iv3)= 1
        endif
        if (xNEIBPE(iv4).eq.0.and.ip4.ne.my_rank) then
          IWK(iv4)= IWK(iv4) + 1
               kk = IWK(iv4) + ELM_STACK_EXPORT(iv4-1)
          ELM_NOD_EXPORT(kk)= pELEM_ID(icel,1)
                   xNEIBPE(iv4)= 1
        endif
        if (xNEIBPE(iv5).eq.0.and.ip5.ne.my_rank) then
          IWK(iv5)= IWK(iv5) + 1
               kk = IWK(iv5) + ELM_STACK_EXPORT(iv5-1)
          ELM_NOD_EXPORT(kk)= pELEM_ID(icel,1)
                   xNEIBPE(iv5)= 1
        endif
        if (xNEIBPE(iv6).eq.0.and.ip6.ne.my_rank) then
          IWK(iv6)= IWK(iv6) + 1
               kk = IWK(iv6) + ELM_STACK_EXPORT(iv6-1)
          ELM_NOD_EXPORT(kk)= pELEM_ID(icel,1)
                   xNEIBPE(iv6)= 1
        endif
        if (xNEIBPE(iv7).eq.0.and.ip7.ne.my_rank) then
          IWK(iv7)= IWK(iv7) + 1
               kk = IWK(iv7) + ELM_STACK_EXPORT(iv7-1)
          ELM_NOD_EXPORT(kk)= pELEM_ID(icel,1)
                   xNEIBPE(iv7)= 1
        endif
        if (xNEIBPE(iv8).eq.0.and.ip8.ne.my_rank) then
          IWK(iv8)= IWK(iv8) + 1
               kk = IWK(iv8) + ELM_STACK_EXPORT(iv8-1)
          ELM_NOD_EXPORT(kk)= pELEM_ID(icel,1)
                   xNEIBPE(iv8)= 1
        endif
        endif
      enddo

      deallocate (IWK)

!C
!C-- CREATING COMMUNICATION TABLES for ELEMENT INFO. 
      allocate (WS(NEIBPETOT), WR(NEIBPETOT))
      allocate (ELM_STACK_IMPORT(0:NEIBPETOT))
      ELM_STACK_IMPORT= 0

      allocate (sta1(MPI_STATUS_SIZE,NEIBPETOT))
      allocate (sta2(MPI_STATUS_SIZE,NEIBPETOT))
      allocate (req1(NEIBPETOT))
      allocate (req2(NEIBPETOT))

      do ib= 1, NEIBPETOT
        WS(ib)= ELM_STACK_EXPORT(ib) - ELM_STACK_EXPORT(ib-1)
      enddo

      do ib= 1, NEIBPETOT
        call MPI_ISEND                                                  &
     &       (WS(ib), 1, MPI_INTEGER,                                   &
     &        NEIBPE(ib), 0, MPI_COMM_WORLD, req1(ib), ierr)
      enddo

      do ib= 1, NEIBPETOT
        call MPI_IRECV                                                  &
     &       (WR(ib), 1, MPI_INTEGER,                                   &
     &        NEIBPE(ib), 0, MPI_COMM_WORLD, req2(ib), ierr)
      enddo
      call MPI_WAITALL (NEIBPETOT, req2, sta2, ierr)
   
      do ib= 1, NEIBPETOT
        ELM_STACK_IMPORT(ib)= ELM_STACK_IMPORT(ib-1) + WR(ib)
      enddo
      call MPI_WAITALL (NEIBPETOT, req1, sta1, ierr)
      deallocate (sta1, sta2, req1, req2, WS, WR)
!C===

!C
!C +-------------------+
!C | LOCAL ELEM ID - 2 |
!C +-------------------+
!C===
      ni= max(NEIBPETOT,ELM_STACK_IMPORT(NEIBPETOT))
      ne= max(NEIBPETOT,ELM_STACK_EXPORT(NEIBPETOT))
      allocate (ELM_NOD_IMPORT(ni))
      allocate (WS(ne), WR(ni))

      allocate (sta1(MPI_STATUS_SIZE,NEIBPETOT))
      allocate (sta2(MPI_STATUS_SIZE,NEIBPETOT))
      allocate (req1(NEIBPETOT))
      allocate (req2(NEIBPETOT))

      do ib= 1, NEIBPETOT
        istart= ELM_STACK_EXPORT(ib-1)
        inum  = ELM_STACK_EXPORT(ib  ) - istart
        do k= ELM_STACK_EXPORT(ib-1)+1, ELM_STACK_EXPORT(ib)
          WS(k)= ELM_NOD_EXPORT(k)
        enddo
        call MPI_ISEND                                                  &
     &       (WS(istart+1), inum, MPI_INTEGER,                          &
     &        NEIBPE(ib), 0, MPI_COMM_WORLD, req1(ib), ierr)
      enddo

      do ib= 1, NEIBPETOT
        istart= ELM_STACK_IMPORT(ib-1)
        inum  = ELM_STACK_IMPORT(ib  ) - istart
        call MPI_IRECV                                                  &
     &       (WR(istart+1), inum, MPI_INTEGER,                          &
     &        NEIBPE(ib), 0, MPI_COMM_WORLD, req2(ib), ierr)
      enddo
      call MPI_WAITALL (NEIBPETOT, req2, sta2, ierr)
   
      do ib= 1, NEIBPETOT
        do k= ELM_STACK_IMPORT(ib-1)+1, ELM_STACK_IMPORT(ib)
          ELM_NOD_IMPORT(k)= WR(k)
        enddo
      enddo
      call MPI_WAITALL (NEIBPETOT, req1, sta1, ierr)
      deallocate (sta1, sta2, req1, req2, WS, WR)

      allocate (IWK(ni))
      icou= 0
      do ib= 1, NEIBPETOT
        neib= NEIBPE(ib)
      do icel= 1, ICELTOT
        if (pELEM_ID(icel,2).eq.neib) then
          icou= icou + 1
          IWK(icou)= icel
        endif
      enddo
      enddo

      do ib= 1, NEIBPETOT
        neib= NEIBPE(ib)
        do k= ELM_STACK_IMPORT(ib-1)+1, ELM_STACK_IMPORT(ib)
          icel= IWK(k)
          pELEM_ID(icel,1)= ELM_NOD_IMPORT(k)          
        enddo
      enddo
!C===

!C
!C +--------+
!C | OUTPUT |
!C +--------+
!C===
      open (35, file=meshfil, form='unformatted')
        write (35) N0
        write (35) N0
        write (35) my_rank
        write (35) NEIBPETOT
        write (35) (NEIBPE(ib), ib= 1, NEIBPETOT)
        write (35) N0

        write (35) NP, N
     
        do i= 1, NP
          write (35) 
     &           pNODE_ID(i,1), pNODE_ID(i,2), N0,
     &          (XYZ(k,i),k=1,3)
        enddo

        write (35) ICELTOT, ICELTOT_INT
        write (35) (N8, ii= 1, ICELTOT)
        do icel= 1, ICELTOT
          write (35) 
     &           pELEM_ID(icel,1), pELEM_ID(icel,2), N0, N1, 
     &          (ICELNOD(icel,k), k= 1,8)
        enddo
        do icel= 1, ICELTOT
          write (35) N0, N0, N0, N0, N0, N0, N0
        enddo

        write (35) (INT_ELM_LIST(k),k= 1, ICELTOT_INT)
        write (35) (NOD_STACK_IMPORT(neib), neib=1,NEIBPETOT)
        if (NEIBPETOT.ne.0) then
          write (35) (NOD_IMPORT(k),
     &                k=1,NOD_STACK_IMPORT(NEIBPETOT))
        endif
        write (35) (NOD_STACK_EXPORT(neib), neib=1,NEIBPETOT)
        if (NEIBPETOT.ne.0) then
          write (35) (NOD_EXPORT(k),
     &                k=1,NOD_STACK_EXPORT(NEIBPETOT))
        endif

        write (35) N4
        write (35) (BC_STACK(k), k=1,N4)
        do ig= 1, N4
          write (35) BC_NAME(ig)
          iS= BC_STACK(ig-1)+1
          iE= BC_STACK(ig)
          if (BC_STACK(ig)-BC_STACK(ig-1).ne.0) then
            write (35) (BC_NOD(kk), kk= iS, iE)
          endif
        enddo
        write (35) N0
        write (35) N0
        close (35)
!C===
          endif
          pe_id = pe_id + 1
          enddo
        enddo
      enddo

      call MPI_FINALIZE (errno)
      stop
      end

      subroutine DEFINE_FILE_NAME (HEADERo, filename, my_rank)

      character (len=80) ::  HEADERo, filename
      character (len=80) ::  HEADER
      character (len= 1) ::  SUBindex1
      character (len= 2) ::  SUBindex2
      character (len= 3) ::  SUBindex3
      character (len= 4) ::  SUBindex4
      character (len= 5) ::  SUBindex5
      character (len= 6) ::  SUBindex6
      integer:: LENGTH, ID
   
      HEADER= adjustL (HEADERo)
      LENGTH= len_trim(HEADER)

      if (my_rank.le.9) then
        ID= 1
        write(SUBindex1 ,'(i1.1)') my_rank
       else if (my_rank.le.99) then
        ID= 2
        write(SUBindex2 ,'(i2.2)') my_rank
       else if (my_rank.le.999) then
        ID= 3
        write(SUBindex3 ,'(i3.3)') my_rank
       else if (my_rank.le.9999) then
        ID= 4
        write(SUBindex5 ,'(i4.4)') my_rank
       else if (my_rank.le.99999) then
        ID= 5
        write(SUBindex6 ,'(i5.5)') my_rank
       else if (my_rank.le.999999) then
        ID= 6
        write(SUBindex4 ,'(i6.6)') my_rank
      endif

      if (ID.eq.1) filename= HEADER(1:LENGTH)//'.'//SUBindex1
      if (ID.eq.2) filename= HEADER(1:LENGTH)//'.'//SUBindex2
      if (ID.eq.3) filename= HEADER(1:LENGTH)//'.'//SUBindex3
      if (ID.eq.4) filename= HEADER(1:LENGTH)//'.'//SUBindex4
      if (ID.eq.5) filename= HEADER(1:LENGTH)//'.'//SUBindex5
      if (ID.eq.6) filename= HEADER(1:LENGTH)//'.'//SUBindex6


      end subroutine define_file_name

