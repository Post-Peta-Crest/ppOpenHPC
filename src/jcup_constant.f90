!=======+=========+=========+=========+=========+=========+=========+=========+
!
!Copyright (c) 2011, arakawa@rist.jp
!All rights reserved.
!

module jcup_constant

!--------------------------------   public  ----------------------------------!

  integer,public :: NUM_OF_EXCHANGE_DATA = 1

  character(len=*), public, parameter :: NO_NAME = "NO_NAME_ASSIGNED"
  integer,public,parameter :: NO_DATA = -999999  
  integer,public,parameter :: NO_GRID = -999999
  integer,public,parameter :: NAME_LEN = 64 ! 2014/11/12 [MOD] 32 -> 64
  integer,public,parameter :: STRING_LEN = 256
  integer,public,parameter :: MAX_MODEL  = 8
  integer,public,parameter :: MAX_DOMAIN = 5
  integer,public,parameter :: MAX_GRID   = 8
  integer,public,parameter :: NO_MODEL = -999
  integer,public,parameter :: NUM_OF_EXCHANGE_GRID = 3
  integer,public,parameter :: FINE_NUM  = 99

  integer,public,parameter :: CONCURRENT_SEND_RECV = 0 !(my time lag = -1, target time lag = -1)
  integer,public,parameter :: ADVANCE_SEND_RECV = -1   !(my time lag = -1, target time lag =  1)
  integer,public,parameter :: BEHIND_SEND_RECV = 1     !(my time lag =  1, target time lag = -1)
  integer,public,parameter :: IMMEDIATE_SEND_RECV = 2  !(my time lag =  0, target time lag =  0) 
  integer,public,parameter :: NO_SEND_RECV = 99999999  !

  integer,public,parameter :: COMP_PARALLEL = 1
  integer,public,parameter :: COMP_SERIAL   = 2
  integer,public,parameter :: COMP_SUPERSET = 3
  integer,public,parameter :: COMP_SUBSET   = 4
  integer,public,parameter :: COMP_OVERLAP  = 5

  character(len=*),public,parameter :: CONF_FILE = 'coupling.conf'

  integer,parameter,public :: ANY_TAG = 99
  integer,parameter,public :: SOME_TAG = 98
  integer,parameter,public :: SEND_RECV_OK = 1
  integer,parameter,public :: SEND_RECV_NG = 0
  integer,parameter,public :: SEND_SKIP = -1
  integer,parameter,public :: RECV_SKIP = -1

  integer,parameter,public :: DATA_EXCHANGE_ERROR = SEND_RECV_NG
  integer,parameter,public :: DATA_EXCHANGE_OK    = SEND_RECV_OK

  integer,parameter,public :: DATE_LEN = 14
  integer,parameter,public :: NUM_OF_EXCHANGE_CODE = 10
  integer,parameter,public :: GRID_SET_END = -999
  integer,parameter,public :: INFO_FLAG = 3
  integer,parameter,public :: SEND_FLAG = 0
  integer,parameter,public :: RECV_FLAG = 1
  integer,parameter,public :: FINISH_FLAG = -1
  integer,parameter,public :: REGRID_FLAG  = -2
  integer,parameter,public :: ABORT_FLAG  = 9999

  integer,parameter,public :: COUPLER_MODEL_COMM = 1
  integer,parameter,public :: INTRA_COUPLER_COMM = 2

  integer,parameter,public :: TOP_BOUNDARY    = 1
  integer,parameter,public :: BOTTOM_BOUNDARY = 2
  integer,parameter,public :: EXCHANGE_2D       = 1
  integer,parameter,public :: EXCHANGE_3D       = 2
  integer,parameter,public :: EXCHANGE_PARTICLE = 3
  integer,parameter,public :: EXCHANGE_DIRECT   = 4
  integer,parameter,public :: INT_DATA    = 1
  integer,parameter,public :: REAL_DATA   = 2
  integer,parameter,public :: DOUBLE_DATA = 3

  integer,parameter,public :: N_DATA_GRID = 13

  integer,parameter,public :: DATA_1D = 1
  integer,parameter,public :: DATA_2D = 2
  integer,parameter,public :: DATA_25D = 25
  integer,parameter,public :: DATA_3D = 3
  integer,parameter,public :: DATA_P  = 4
  integer,parameter,public :: END_FLAG = -999


  integer,parameter,public :: DEBUG_FILE_ID = 800

end module jcup_constant
