! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.

module numerics
  use progvars

  implicit none

  contains

    subroutine linspace(x_min, x_max, x_arr, dx)
      ! Create a linear space betwen x_min, x_max, store in x_arr

      implicit none

      real(dp), intent(in) :: x_min, x_max
      real(dp), intent(out) :: x_arr(:)
      real(dp), intent(out) :: dx

      integer :: i_x, n_x

      n_x = size(x_arr)
      dx = (x_max - x_min) / n_x

      do i_x = 1, n_x
         x_arr(i_x) = x_min + i_x * dx
      end do

    end subroutine linspace

end module numerics
