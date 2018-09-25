!=====================================================================!
!                                                                     !
!   Software Name : ppOpen-APPL/FDM                                   !
!         Version : 0.3                                               !
!                                                                     !
!   License                                                           !
!     This file is part of ppOpen-APPL/FDM.                           !
!     ppOpen-APPL/FDM is a free software, you can use it under the    !
!     terms of The MIT License (MIT). See LICENSE file and User's     !
!     guide for more details.                                         !
!                                                                     !
!   ppOpen-HPC project:                                               !
!     Open Source Infrastructure for Development and Execution of     !
!     Large-Scale Scientific Applications on Post-Peta-Scale          !
!     Supercomputers with Automatic Tuning (AT).                      !
!                                                                     !
!   Organizations:                                                    !
!     The University of Tokyo                                         !
!       - Information Technology Center                               !
!       - Atmosphere and Ocean Research Institute (AORI)              !
!       - Interfaculty Initiative in Information Studies              !
!         /Earthquake Research Institute (ERI)                        !
!       - Graduate School of Frontier Science                         !
!     Kyoto University                                                !
!       - Academic Center for Computing and Media Studies             !
!     Japan Agency for Marine-Earth Science and Technology (JAMSTEC)  !
!                                                                     !
!   Sponsorship:                                                      !
!     Japan Science and Technology Agency (JST), Basic Research       !
!     Programs: CREST, Development of System Software Technologies    !
!     for post-Peta Scale High Performance Computing.                 !
!                                                                     !
!                 Copyright (c) 2015 T.Furumura                       !
!                                                                     !
!=====================================================================!
module ppohFDM_m_source
  !
  ! This module sets up the source-time function used for 2D simulation
  !
  use ppohFDM_m_stdlib
  use ppohFDM_m_params
  !, only : NTMAX, DX, DZ, DT, NX, NZ, KFS, I0, K0
  use ppohFDM_m_comvar
  !, only : vx, vz, Sxx, Sxz, Szz, den
  implicit none
  private

  !=Public Procedures
  public :: ppohFDM_source__setup
  public :: ppohFDM_source__bforce

  !=Public Variables/Parameters
  !-- << Source >>
  real(PN), parameter, public :: UC      = 1E-12               ! Unit Conversion Coef
  real(PN), parameter, public :: RMO     = 1E12 * UC           ! N.m (times UC)
  
  real(PN), public :: Mxx, Mxz, Mzz   ! Moment Tensor

  !!-- << Source Time Function >>
  real(PN) :: STIME  (NTMAX)
  real(PN) :: dxm, dzm

contains


  subroutine ppohFDM_source__setup_param()
  !
  ! Sets up the source parameters defined in "source.dat".
  !
    real :: Xtmp, Ztmp
    character(len=80) filename
    character(len=80) tmp1
    character(len=80) tmp2
    
    filename="source.dat"
    open(7,file=filename,status='old')
    read(7,*) tmp1
    read(7,*) tmp2
    read(7,*) tmp1
    read(7,*) tmp2
    read(7, *) Xtmp, Ztmp
    I0 = int (Xtmp/Dx + 0.5)
    K0 = int (Ztmp/Dz + 0.5)+KFS
    STRIKE = 0.0
    DIP    = 45.0
    RAKE   = 90.0
    AT = 4.0_PN/4
    T0 = AT * 2
    read(7, *) STRIKE, DIP, RAKE
    read(7, *) AT, T0
    print *,'SOURCE XYZ'
    print *,I0, K0
    print *,'STRIKE, DIP, RAKE, AT, T0'
    print *,STRIKE, DIP, RAKE, AT, T0
    close(7)
  end subroutine ppohFDM_source__setup_param


  subroutine ppohFDM_source__setup()
    !
    ! Sets up the sample source (preparing for a benchmark test)
    !
    integer :: it
    real(PN) :: t

    call ppohFDM_source__setup_param()
    
    call ppohFDM_sld2moment2(STRIKE, DIP, RAKE, RMO, Mxx, Mzz, Mxz)
    Mxx = 1.0
    Mzz = 1.0
    Mxz = 0.0_PN
    
    ! Body Force Grid 
    DXM = DX*DX*DZ
    DZM = DX*DZ*DZ
    
    ! Use IHERRMANN for Body Force Source, HERRMANN for Stree Drop Source
    do it = 1, NTMAX
       T = (it-1)*DT
       STIME (it) =  IKUPPER (AT, T, T0)   
       !     STIME (it) =  KUPPER (AT, T, T0)   
    end do
    
  end subroutine ppohFDM_source__setup


  subroutine ppohFDM_source__bforce(it)
    !
    ! Adopts the effect of a seismic source by using body force
    !
    integer, intent(in) :: it
    
    real(PN) :: FX1, FZ1
    
    !! Mxx
    FX1              = RMO*Mxx*STIME(IT) / DXM
    VX(I0,   K0) = VX(I0,   K0) + FX1/DEN(I0,   K0)*DT
    VX(I0-1, K0) = VX(I0-1, K0) - FX1/DEN(I0-1, K0)*DT
    
    !! Mzz
    FZ1              = RMO*Mzz*STIME(IT) / DZM
    VZ(I0, K0)   = VZ(I0, K0)   + FZ1/DEN(I0, K0)*DT
    VZ(I0, K0-1) = VZ(I0, K0-1) - FZ1/DEN(I0, K0-1)*DT
    
    !! Mxz
    FX1              = RMO*Mxz*STIME(IT) / (DZM*4) 
    VX(I0-1, K0+1) = VX(I0-1, K0+1) + FX1/DEN(I0-1, K0+1)*DT
    VX(I0,   K0+1) = VX(I0,   K0+1) + FX1/DEN(I0,   K0+1)*DT
    VX(I0-1, K0-1) = VX(I0-1, K0-1) - FX1/DEN(I0-1, K0-1)*DT
    VX(I0,   K0-1) = VX(I0,   K0-1) - FX1/DEN(I0,   K0-1)*DT
    
    FZ1              = RMO*Mxz*STIME(IT) / (DXM*4) 
    !!    FZ1 =- Fz1  ! torque
    VZ(I0+1, K0-1) = VZ(I0+1, K0-1) + FZ1/DEN(I0+1, K0-1)*DT
    VZ(I0+1, K0)   = VZ(I0+1, K0)   + FZ1/DEN(I0+1, K0)*DT
    VZ(I0-1, K0-1) = VZ(I0-1, K0-1) - FZ1/DEN(I0-1, K0-1)*DT
    VZ(I0-1, K0)   = VZ(I0-1, K0)   - FZ1/DEN(I0-1, K0)*DT
    
  end subroutine ppohFDM_source__bforce


  subroutine ppohFDM_source__stressdrop(it)
    integer, intent(in) :: it

    real(PN) :: RMO1
    real(PN) :: FTMAX
    
    FTMAX = T0 + AT*2
    
    RMO1 = RMO*STIME(IT)*DT/(DX*DZ)
    SXX(I0,K0) = SXX(I0,K0) - Mxx*RMO1
    SZZ(I0,K0) = SZZ(I0,K0) - Mzz*RMO1
    SXZ(I0,K0) = SXZ(I0,K0) - Mxz*RMO1
    
    
  end subroutine ppohFDM_source__stressdrop


  function KUPPER( A, X, X0 )     
    !
    ! Single-lobed Kupper function for moment rate function
    ! Normalized by int_0^\infty Kupper(t) dt = 1
    !
    real(PN) :: A                    ! Characteristic Time
    real(PN) :: X                    ! Time
    real(PN) :: X0                   ! Origin  Time
    real(PN) :: KUPPER               ! Herrman function for moment rate

    real(PN) ::  A2, T

    
    A2  = 4 * A
    T   = X - X0 + 2*A
    
    if ( 0 <= T .and. T <= A2 ) then
       KUPPER = 9*PI/(16*A2) *( sin(PI*t/a2) - sin(3*PI*t/a2)/3 )
    else
       KUPPER = 0.0_PN
    end if
    
    return
    
  end function KUPPER


  function IKUPPER ( A, X, X0 )     
    !
    ! Integrated single-lobed kupper function for moment function
    !
    !=Input Arguments
    real(PN) :: A
    real(PN) :: X
    real(PN) :: X0
    real(PN) :: IKUPPER

    real(PN) :: A2, T

    
    A2  = 4 * A                                                     
    T   = X - X0 + 2*A
    
    if( T <= 0 ) then
       IKUPPER = 0.0_PN
       return
    else if( T <= A2 ) then                    
       IKUPPER = ( 8 - 9*cos( PI* T/A2 ) + cos( 3*PI*T/A2 ) ) / 16.0_PN
       return                                                        
    else                                                              
       IKUPPER  = 1.0_PN
       return                                                        
    end if
  end function IKUPPER


  function  DHERRMANN ( A, X, X0 )                              
    real(PN), intent(in) :: A
    real(PN), intent(in) :: X
    real(PN), intent(in) :: X0
    real(PN)             :: DHERRMANN

    real(PN) :: A2, T, TD

    
    A2  = 2.0_PN * A                                                     
    T   = X - X0                                                      
    TD  = ( T + A2 ) / A                                              
    if( T <= -A2 ) then                                             
       DHERRMANN  = 0.0_PN
       return                                                        
    else if( (T  > -A2) .and. (T <= -A) ) then                    
       DHERRMANN  = (       TD    ) / A2  * 2.0_PN
       return                                                        
    else if( (T >  -A ) .and. (T <=  A) ) then                    
       DHERRMANN  = (       -TD    + 2.0_PN         ) / A2  * 2.0_PN    
       return                                                        
    else if( (T >  A ) .and. (T <= A2) ) then                    
       DHERRMANN  = (        TD    - 4.0_PN         ) / A2  * 2.0_PN  
       return                                                        
    else                                                              
       DHERRMANN  = 0.0_PN
       return                                                        
    end if
    
  end function DHERRMANN


  function  HERRMANN ( A, X, X0 )     
    real(PN), intent(in) :: A
    real(PN), intent(in) :: X
    real(PN), intent(in) :: X0
    real(PN)             :: HERRMANN
    real(PN) :: A2, T, TD
    A2  = 2.0_PN * A                                                     
    T   = X - X0                                                      
    TD  = ( T + A2 ) / A                                              
    if( T <= -A2 ) then                                             
       HERRMANN  = 0.0_PN                                                
       return                                                        
    else if( (T > -A2) .and. (T <= -A) ) then                    
       HERRMANN  = ( 0.5_PN * TD**2 ) / A2                               
       return                                                        
    else if( (T > -A ) .and. (T <=  A) ) then                    
       HERRMANN  = ( -0.5_PN * TD**2 + 2.0_PN * TD - 1.0_PN ) / A2    
       return                                                        
    else if( (T >  A ) .and. (T <= A2) ) then                    
       HERRMANN  = (  0.5_PN * TD**2 - 4.0_PN * TD + 8.0_PN ) / A2    
       return                                                        
    else                                                              
       HERRMANN  = 0.0_PN                                                
       return                                                        
    end if
  end function HERRMANN


  function IHERRMANN ( A, X, X0 )     
    !
    ! Integral of Herrman function with respect to time
    !
    real(PN), intent(in) :: A
    real(PN), intent(in) :: X
    real(PN), intent(in) :: X0
    real(PN)             :: IHERRMANN
    real(PN) :: A2, T, TD

    A2  = 2.0_PN * A                                                     
    T   = X - X0                  
    TD  = ( T + A2 ) / A                                              
    if( T <= -A2 ) then                                             
       IHERRMANN  = 0.0_PN                                                
       return                                                        
    else if( (T > -A2) .and. (T <= -A) ) then                    
       IHERRMANN  = TD**3/12.0_PN                              
       return                                                        
    else if( (T > -A ) .and. (T <=  A) ) then                    
       IHERRMANN  = ( 2.0_PN - 6.0_PN*TD+6.0_PN*TD**2-TD**3)/12.0_PN    
       return                                                        
    else if( (T >  A ) .and. (T <= A2) ) then                    
       IHERRMANN  = ( -52.0_PN + 48.0_PN*TD - 12.0_PN*TD**2 + TD**3 )/12.0_PN  
       return                                                        
    else                                                              
       IHERRMANN  = 1.0_PN                                                
       return                                                        
    end if
  end function IHERRMANN


  subroutine ppohFDM_sld2moment2(strike, dip, rake, M0, Mxx, Mzz, Mxz )
    !
    ! Calcluate six moment tensor components from strike, dip, and rake [deg]
    ! under the double-couple assumptions for 2dcode
    !
    real(PN), intent(in)  :: strike, dip, rake ! fault mechanism parameters
    real(PN), intent(in)  :: M0                ! seismic moment
    real(PN), intent(out) :: Mxx, Mzz, Mxz

    !=Local Variables
    real(PN) :: sind, cosd, sin2d, cos2d, cosl, sinl
    real(PN) :: sinf, cosf, sin2f, cos2f
    
    sind  = sin(     dip    * DEG2RAD )
    cosd  = cos(     dip    * DEG2RAD )
    sin2d = sin( 2 * dip    * DEG2RAD )
    cos2d = cos( 2 * dip    * DEG2RAD )
    sinl  = sin(     rake   * DEG2RAD )
    cosl  = cos(     rake   * DEG2RAD )
    sinf  = sin(     strike * DEG2RAD )
    cosf  = cos(     strike * DEG2RAD )
    sin2f = sin( 2 * strike * DEG2RAD )
    cos2f = cos( 2 * strike * DEG2RAD )
    
    Mxx = - M0 * ( sind*cosl*sin2f + sin2d*sinl*sinf*sinf )
    Mxz = - M0 * ( cosd*cosl*cosf  + cos2d*sinl*sinf      )
    Mzz =   M0 * (                   sin2d*sinl           )
    
  end subroutine ppohFDM_sld2moment2
  
end module ppohFDM_m_source
