module mod_kml
  implicit none 

  public :: kml_init_polygon
  public :: kml_init_point
  public :: kml_init
  public :: kml_final
  public :: dump_poly_to_kml
  public :: dump_point_to_kml
  public :: set_kml_name
  
  integer :: lun = 88

  integer, parameter :: NAME_LEN = 32
  integer, parameter :: COLOR_LEN = 8

  type kml_line_type
    character(len=COLOR_LEN) :: color
    integer :: width
  end type

  type kml_polygon_type
    character(len=NAME_LEN) :: Style_id
    type(kml_line_type) :: line
    character(len=COLOR_LEN) :: color
    integer :: is_fill ! 0 or 1
    type(kml_polygon_type), pointer :: next_ptr
  end type

  type(kml_polygon_type), pointer :: polygon_ptr => null()
  
  type kml_point_type
    character(len=NAME_LEN) :: style_id
    real(kind=4) :: scale
    character(len=COLOR_LEN) :: color
    type(kml_point_type), pointer :: next_ptr    
  end type

  type(kml_point_type), pointer :: point_ptr => null()

contains


!!========================================================================
!!
subroutine kml_init_polygon(style_id, line_opacity, line_r, line_g, line_b, line_w, poly_opacity, poly_r, poly_g, poly_b)
    implicit none
    character(len=*), intent(IN) :: style_id
    real(kind=8), intent(IN) :: line_opacity
    integer, intent(IN) :: line_r, line_g, line_b
    integer, intent(IN) :: line_w
    real(kind=8), intent(IN) :: poly_opacity
    integer, intent(IN) :: poly_r, poly_g, poly_b
    type(kml_polygon_type), pointer :: current_ptr
    type(kml_polygon_type), pointer :: before_ptr

    if (.not.associated(polygon_ptr)) then
      allocate(polygon_ptr)
      current_ptr => polygon_ptr
      current_ptr%Style_id = style_id
      current_ptr%line%color = get_color(line_opacity, line_r, line_g, line_b)
      current_ptr%line%width = line_w
      current_ptr%color = get_color(poly_opacity, poly_r, poly_g, poly_b)
      if (poly_opacity <= 0.0000001d0) then 
        current_ptr%is_fill = 0
      else
        current_ptr%is_fill = 1
      end if
      current_ptr%next_ptr => null()
      return
    end if

    current_ptr => polygon_ptr

    do while(associated(current_ptr))
      before_ptr => current_ptr
      current_ptr => current_ptr%next_ptr
    end do
    
    allocate(current_ptr)
    before_ptr%next_ptr => current_ptr
    current_ptr%Style_id = style_id
    current_ptr%line%color = get_color(line_opacity, line_r, line_g, line_b)
    current_ptr%line%width = line_w
    current_ptr%color = get_color(poly_opacity, poly_r, poly_g, poly_b)
    current_ptr%next_ptr => null()
    
end subroutine kml_init_polygon

!!========================================================================
!!
subroutine kml_init_point(style_id, scale, point_r, point_g, point_b)
    implicit none
    character(len=*), intent(IN) :: style_id
    real(kind=8), intent(IN) :: scale
    integer, intent(IN) :: point_r, point_g, point_b
    type(kml_point_type), pointer :: current_ptr
    type(kml_point_type), pointer :: before_ptr

    if (.not.associated(point_ptr)) then
      allocate(point_ptr)
      current_ptr => point_ptr
      current_ptr%Style_id = style_id
      current_ptr%color = get_color(100.d0, point_r, point_g, point_b)
      current_ptr%scale = scale
      current_ptr%next_ptr => null()
      return
    end if

    current_ptr => point_ptr

    do while(associated(current_ptr))
      before_ptr => current_ptr
      current_ptr => current_ptr%next_ptr
    end do
    
    allocate(current_ptr)
    before_ptr%next_ptr => current_ptr
    current_ptr%Style_id = style_id
    current_ptr%color = get_color(100.d0, point_r, point_g, point_b)
    current_ptr%scale = scale
    current_ptr%next_ptr => null()
    
end subroutine kml_init_point

!!========================================================================
!!
subroutine kml_init(file)
    character(len=*),intent(in) :: file


    if ( trim(file) == '-' ) then !! to stdout
      lun = 6
    else
      open(unit=lun,file=trim(file),form='formatted')
      write(0,*)'Write to '//trim(file)
      rewind(lun)
    end if

    write(lun,'(A)')'<?xml version="1.0" encoding="UTF-8"?>'
    write(lun,'(A)')'<kml xmlns="http://earth.google.com/kml/2.1">'
    write(lun,'(A)')'  <Document>'
!!$d    write(lun,'(A)')'    <name>NICOCO Grid Mapping</name>'
    call write_polygon_style()
    call write_point_style()
    return

end subroutine kml_init


!!========================================================================
!!
subroutine set_kml_name( compo, idx )
  character(len=*), intent(in) :: compo
  integer,intent(in), optional :: idx

  if (present(idx)) then
    write(lun,'(A,I0,A)')'<name>compo='//compo//', index=',idx,'</name>'
  else
    write(lun,'(A,A,A)')'<name>compo=', trim(compo),'</name>'
  end if

end subroutine set_kml_name

!!========================================================================
!!
subroutine kml_final()
  implicit none

  write(lun,'(A)')'  </Document>'
  write(lun,'(A)')'</kml>'
  close(lun)

end subroutine kml_final

!!========================================================================
!!
character(len=COLOR_LEN) function get_color(opacity, r, g, b)
    implicit none
    real(kind=8), intent(IN) :: opacity
    integer, intent(IN) :: r, g, b

    if (opacity < 0.d0) get_color(1:2) = "00"
    if (opacity > 100.d0) get_color(1:2) = "ff"

    if ((opacity >= 0.d0).and.(opacity <= 100.d0)) get_color(1:2) = dec2hex(int(opacity*255))
    get_color(3:4) = dec2hex(r)
    get_color(5:6) = dec2hex(g)
    get_color(7:8) = dec2hex(b)
 
end function get_color

!!========================================================================
!!
character(len=2) function dec2hex(dec)
    implicit none
    integer, intent(IN) :: dec ! 0<= int <= 255  
    character(len=1) :: h(0:15) = (/ '0','1','2','3','4','5','6','7', & 
                                  '8','9','a','b','c','d','e','f' /)
    integer :: dec_num
    integer :: i
    
    if (dec < 0) then
      dec2hex = "00"
      return
    end if
 
    if (dec > 255) then
      dec2hex = "ff"
      return
    end if
 
    dec_num = dec
    dec2hex = "00"
    hxd : do i = 0, 1
      dec2hex(2-i:2-i) = h(mod(dec_num, 16))
      dec_num = dec_num/16 
      if (dec_num == 0) exit hxd
    end do hxd

end function dec2hex

!!========================================================================
!!
subroutine write_polygon_style()
    implicit none
    type(kml_polygon_type), pointer :: current_ptr

    current_ptr => polygon_ptr

    do while(associated(current_ptr))
      write(lun,'("    <Style id=",A,">")') '"'//trim(current_ptr%style_id)//'"'
      write(lun,'("      <LineStyle>")')
      write(lun,'("        <color>",A,"</color>")') current_ptr%line%color
      write(lun,'("        <width>",I0,"</width>")') current_ptr%line%width
      write(lun,'("      </LineStyle>")') 
      write(lun,'("      <PolyStyle>")')
      write(lun,'("        <color>",A,"</color>")') current_ptr%color
      write(lun,'("        <fill>",I0,"</fill>")') current_ptr%is_fill 
      write(lun,'("        <outline>1</outline>")') 
      write(lun,'("      </PolyStyle>")')
      write(lun,'("    </Style>")')
      current_ptr => current_ptr%next_ptr
    end do

end subroutine write_polygon_style

!!========================================================================
!!
subroutine write_point_style()
    implicit none
    type(kml_point_type), pointer :: current_ptr

    current_ptr => point_ptr

    do while(associated(current_ptr))
      write(lun,'("    <Style id=",A,">")') '"'//trim(current_ptr%style_id)//'"'
      write(lun,'("      <IconStyle>")')
      write(lun,'("        <color>",A,"</color>")') current_ptr%color
      write(lun,'("        <scale>",F0.2,"</scale>")') current_ptr%scale
      write(lun,'("      </IconStyle>")') 
      write(lun,'("    </Style>")')
      current_ptr => current_ptr%next_ptr
    end do

end subroutine write_point_style

!!========================================================================
!!
subroutine dump_poly_to_kml(compo, idx1, idx2, poly, coeff)
    character(len=*),intent(in) :: compo 
    integer, intent(in) :: idx1
    integer, intent(IN), optional :: idx2 
    real(kind=8), intent(in) :: poly(:)
    real(kind=8), intent(in), optional :: coeff

    integer :: np !< number of vertex of the given polygon

    integer :: n

    character(len=*),parameter :: form_vert='(F0.10,",",F0.10)'


    np = size(poly)/2


    write(lun,'(A)')'<Placemark>'

    if (.not.present(idx2)) then
      write(lun,'(A,A,A,I0,A)')'<name>', trim(compo), ' idx=',idx1,'</name>'
    else
      write(lun,'(A,A,A,I0,A,I0,A)')'<name>', trim(compo), ' i=',idx1,', j=',idx2,'</name>'
    end if
    write(lun,'(A,A,A)')     '<styleUrl>#', trim(compo),'</styleUrl>'

    if ( present( coeff ) )  then
      write(lun,'(A,f0.10,A)')'<description>coefficient=',coeff,'</description>'
    else
      if (.not.present(idx2)) then
        write(lun,'(A,A,A,I0,A)')'<description>',trim(compo),' idx=',idx1,'</description>>'
      else
        write(lun,'(A,A,A,I0,A,I0,A)')'<description>',trim(compo),' i=',idx1,', j=',idx2,'</description>>'
      end if
    endif

    write(lun,'(A)')'<Polygon>'
    write(lun,'(A)')'<outerBoundaryIs>'
    write(lun,'(A)')'<LinearRing>'
    write(lun,'(A)')'<coordinates>'
    do n=1,np
      write(lun,form_vert)poly(2*n-1),poly(2*n)
    end do
    write(lun,form_vert)poly(1),poly(2) !< close polygon.
    write(lun,'(A)')'</coordinates>'
    write(lun,'(A)')'</LinearRing>'
    write(lun,'(A)')'</outerBoundaryIs>'
    write(lun,'(A)')'</Polygon>'
    write(lun,'(A)')'</Placemark>'

end subroutine dump_poly_to_kml

!!========================================================================
!!
subroutine dump_point_to_kml(compo, idx, lon, lat, coeff)
    character(len=*),intent(in) :: compo 
    integer, intent(in) :: idx
    real(kind=8), intent(in) :: lon, lat
    real(kind=8), intent(in), optional :: coeff

    character(len=*),parameter :: form_vert='(F0.10,",",F0.10)'

    write(lun,'(A)')'<Placemark>'

    write(lun,'(A,A,A,I0,A)')'<name>', trim(compo), ' idx=',idx,'</name>'

    write(lun,'(A,A,A)')     '<styleUrl>#', trim(compo),'</styleUrl>'

    if ( present( coeff ) )  then
      write(lun,'(A,f0.10,A)')'<description>coefficient=',coeff,'</description>'
    else
      write(lun,'(A,A,A,I0,A)')'<description>',trim(compo),' idx=',idx,'</description>>'
    endif

    write(lun,'(A)')'<Point>'
     write(lun,'(A)')'<coordinates>'
       write(lun,form_vert) lon, lat
    write(lun,'(A)')'</coordinates>'
     write(lun,'(A)')'</Point>'
    write(lun,'(A)')'</Placemark>'

end subroutine dump_point_to_kml

end module mod_kml
