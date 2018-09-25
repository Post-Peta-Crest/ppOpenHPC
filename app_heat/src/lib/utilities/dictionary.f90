!/*=====================================================================*
! *                                                                     *
! *   Software Name : ppohFEM                                           *
! *         Version : 1.0                                               *
! *                                                                     *
! *   License                                                           *
! *     This file is part of ppohFEM.                                   *
! *     ppohFEM is a free software, you can use it under the terms      *
! *     of The MIT License (MIT). See LICENSE file and User's guide     *
! *     for more details.                                               *
! *                                                                     *
! *   ppOpen-HPC project:                                               *
! *     Open Source Infrastructure for Development and Execution of     *
! *     Large-Scale Scientific Applications on Post-Peta-Scale          *
! *     Supercomputers with Automatic Tuning (AT).                      *
! *                                                                     *
! *   Organizations:                                                    *
! *     The University of Tokyo                                         *
! *       - Information Technology Center                               *
! *       - Atmosphere and Ocean Research Institute (AORI)              *
! *       - Interfaculty Initiative in Information Studies              *
! *         /Earthquake Research Institute (ERI)                        *
! *       - Graduate School of Frontier Science                         *
! *     Kyoto University                                                *
! *       - Academic Center for Computing and Media Studies             *
! *     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  *
! *                                                                     *
! *   Sponsorship:                                                      *
! *     Japan Science and Technology Agency (JST), Basic Research       *
! *     Programs: CREST, Development of System Software Technologies    *
! *     for post-Peta Scale High Performance Computing.                 *
! *                                                                     *
! *   Copyright (c) 2015 The University of Tokyo                        *
! *                       - Graduate School of Frontier Science         *
! *                                                                     *
! *=====================================================================*/


type LIST_DATA
    character(len=DICT_KEY_LENGTH) :: key
    type(DICT_DATA)                :: value
end type LIST_DATA

type HASH_LIST
    type(LINKED_LIST), pointer :: list
end type HASH_LIST

type DICT_STRUCT
    private
    type(HASH_LIST), pointer, dimension(:) :: table
end type DICT_STRUCT

!
! We do not want everything to be public
!
private :: LIST_DATA
private :: HASH_LIST
private :: LINKED_LIST
private :: list_create
private :: list_destroy
private :: list_count
private :: list_next
private :: list_insert
private :: list_insert_head
private :: list_delete_element
private :: list_get_data
private :: list_put_data
private :: dict_get_elem
private :: dict_hashkey

integer, parameter, private :: hash_size  = 499
integer, parameter, private :: multiplier = 1

include 'linkedlist.f90'

!
! Routines and functions specific to dictionaries
!

! dict_create --
!     Create and initialise a dictionary
! Arguments:
!     dict       Pointer to new dictionary
!     key        Key for the first element
!     value      Value for the first element
! Note:
!     This version assumes a shallow copy is enough
!     (that is, there are no pointers within the data
!     to be stored)
!     It also assumes the argument list does not already
!     refer to a list. Use dict_destroy first to
!     destroy up an old list.
!
subroutine dict_create( dict, key, value )
    type(DICT_STRUCT), pointer   :: dict
    character(len=*), intent(in) ::  key
    type(DICT_DATA), intent(in)  :: value

    type(LIST_DATA)              :: data
    integer                      :: i
    integer                      :: hash

    allocate( dict )
    allocate( dict%table(hash_size) )

    do i = 1,hash_size
        dict%table(i)%list => null()
    enddo

    data%key   = key
    data%value = value

    hash = dict_hashkey( trim(key ) )
    call list_create( dict%table(hash)%list, data )

end subroutine dict_create

! dict_destroy --
!     Destroy an entire dictionary
! Arguments:
!     dict       Pointer to the dictionary to be destroyed
! Note:
!     This version assumes that there are no
!     pointers within the data that need deallocation
!
subroutine dict_destroy( dict )
    type(DICT_STRUCT), pointer  :: dict

    integer                     :: i

    do i = 1,size(dict%table)
        if ( associated( dict%table(i)%list ) ) then
            call list_destroy( dict%table(i)%list )
        endif
    enddo
    deallocate( dict%table )
    deallocate( dict )

end subroutine dict_destroy

! dict_add_key
!     Add a new key
! Arguments:
!     dict       Pointer to the dictionary
!     key        Key for the new element
!     value      Value for the new element
! Note:
!     If the key already exists, the
!     key's value is simply replaced
!
subroutine dict_add_key( dict, key, value )
    type(DICT_STRUCT), pointer   :: dict
    character(len=*), intent(in) :: key
    type(DICT_DATA), intent(in)  :: value

    type(LIST_DATA)              :: data
    type(LINKED_LIST), pointer   :: elem
    integer                      :: hash

    elem => dict_get_elem( dict, key )

    if ( associated(elem) ) then
        elem%data%value = value
    else
        data%key   = key
        data%value = value
        hash       = dict_hashkey( trim(key) )
        if ( associated( dict%table(hash)%list ) ) then
            call list_insert( dict%table(hash)%list, data )
        else
            call list_create( dict%table(hash)%list, data )
        endif
    endif

end subroutine dict_add_key

! dict_delete_key
!     Delete a key-value pair from the dictionary
! Arguments:
!     dict       Dictionary in question
!     key        Key to be removed
!
subroutine dict_delete_key( dict, key )
    type(DICT_STRUCT), pointer   :: dict
    character(len=*), intent(in) :: key

    type(LINKED_LIST), pointer   :: elem
    integer                      :: hash

    elem => dict_get_elem( dict, key )

    if ( associated(elem) ) then
        hash = dict_hashkey( trim(key) )
        call list_delete_element( dict%table(hash)%list, elem )
    endif
end subroutine dict_delete_key

! dict_get_key
!     Get the value belonging to a key
! Arguments:
!     dict       Pointer to the dictionary
!     key        Key for which the values are sought
!
function dict_get_key( dict, key ) result(value)
    type(DICT_STRUCT), pointer   :: dict
    character(len=*), intent(in) :: key
    type(DICT_DATA), pointer     :: value

    type(LINKED_LIST), pointer   :: elem

    elem => dict_get_elem( dict, key )

    if ( associated(elem) ) then
        value => elem%data%value
    else
        nullify(value)
    endif
end function dict_get_key

! dict_has_key
!     Check if the dictionary has a particular key
! Arguments:
!     dict       Pointer to the dictionary
!     key        Key to be sought
!
function dict_has_key( dict, key ) result(has)
    type(DICT_STRUCT), pointer   :: dict
    character(len=*), intent(in) :: key
    logical                      :: has

    type(LINKED_LIST), pointer   :: elem

    elem => dict_get_elem( dict, key )

    has = associated(elem)
end function dict_has_key

! dict_get_elem
!     Find the element with a particular key
! Arguments:
!     dict       Pointer to the dictionary
!     key        Key to be sought
!
function dict_get_elem( dict, key ) result(elem)
    type(DICT_STRUCT), pointer   :: dict
    character(len=*), intent(in) :: key

    type(LINKED_LIST), pointer   :: elem
    integer                      :: hash

    hash = dict_hashkey( trim(key) )

    elem => dict%table(hash)%list
    do while ( associated(elem) )
        if ( elem%data%key .eq. key ) then
            exit
        else
            elem => list_next( elem )
        endif
    enddo
end function dict_get_elem

! dict_hashkey
!     Determine the hash value from the string
! Arguments:
!     key        String to be examined
!
integer function dict_hashkey( key )
    character(len=*), intent(in) :: key

!    integer                      :: hash
    integer                      :: i

    dict_hashkey = 0

    do i = 1,len(key)
        dict_hashkey = multiplier * dict_hashkey + ichar(key(i:i))
    enddo

    dict_hashkey = 1 + mod( dict_hashkey-1, hash_size )
end function dict_hashkey

