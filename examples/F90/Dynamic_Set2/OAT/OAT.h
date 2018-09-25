 
c     === common variables

      character*10000  OAT_AllRoutines
      character*10000  OAT_InstallRoutines
      character*10000  OAT_StaticRoutines
      character*10000  OAT_DynamicRoutines
      common /OAT_DRL/OAT_AllRoutines,
     &      OAT_InstallRoutines, 
     &      OAT_StaticRoutines,  
     &      OAT_DynamicRoutines

      integer  OAT_NUMPROCS
      integer  OAT_STARTTUNESIZE
      integer  OAT_ENDTUNESIZE
      integer  OAT_SAMPDIST
      integer  OAT_TUNESTATIC
      integer  OAT_DEBUG
      common /OAT_SBP/OAT_NUMPROCS,
     &         OAT_STARTTUNESIZE,  
     &         OAT_ENDTUNESIZE, 
     &         OAT_SAMPDIST,
     &         OAT_TUNESTATIC,
     &         OAT_DEBUG

      integer  OAT_ALL
      integer  OAT_INSTALL
      integer  OAT_STATIC
      integer  OAT_DYNAMIC
      parameter (OAT_ALL = 0)
      parameter (OAT_INSTALL = 1)
      parameter (OAT_STATIC = 2)
      parameter (OAT_DYNAMIC = 3)

      integer  myid
      integer  nprocs
      common  /OAT_pval/myid,nprocs     

      logical OAT_STATICTUNE
      common /OAT_ST/OAT_STATICTUNE
      logical OAT_DYNAMICTUNE
      common /OAT_DT/OAT_DYNAMICTUNE


c       === for fitting
c         === data array defines
c         === upper bound for total sampling points
      integer  OATLSM_MAX_N
      parameter (OATLSM_MAX_N = 512)
      integer  OATLSM_MAX_NPARM
      parameter (OATLSM_MAX_NPARM = 512)

c        === upper bound for the order of linear polynonial equation
      integer  OATLSM_MAX_M
      parameter (OATLSM_MAX_M = 10)

c        === upper bound for the number of sampling points for matrix dimension
      integer  OATLSM_MAX_SAMP
      parameter (OATLSM_MAX_SAMP = 512)

c      include 'mpif.h'
!       === AT region variables
      integer iusw1_DU_MatVec

      common /OAT_ATswitches/iusw1_DU_MatVec

      integer iusw1_DU_MatVec_flag

      common /OAT_ATswitchFlags/iusw1_DU_MatVec_flag

