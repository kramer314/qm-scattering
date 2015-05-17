! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.

module files
  implicit none

  private
  public ensure_dir

  contains
    subroutine ensure_dir(dir)
      character(*), intent(in) :: dir
      call system("mkdir -p "//trim(dir))
    end subroutine ensure_dir

end module files
