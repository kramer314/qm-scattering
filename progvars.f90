! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.

module progvars
  ! Global program variables
  implicit none

  ! Double precision type / formatting
  integer, parameter :: dp = kind(0.d0)
  character(*), parameter :: dp_format = "(es23.16)"

  ! Constants
  real(dp), parameter :: pi = 4_dp * atan(1.0_dp)
  real(dp), parameter :: e = exp(1.0_dp)
  complex(dp), parameter :: j = (0.0_dp, 1.0_dp)

  ! Output parameters
  character(120) :: output_folder
  character(120) :: ak_out, ek_out, psit_out, psi0_out, psik_out
  character(120) :: x_out, k_out, t_out
  
  ! Grid parameters
  integer(8) :: n_x, n_k, n_t
  real(dp) :: x_min, x_max, dx, k_min, k_max, dk, t_min, t_max, dt
  
  ! Work arrays
  real(dp), allocatable :: x_range(:), k_range(:), t_range(:)
  real(dp), allocatable :: ek_arr(:)
  complex(dp), allocatable :: ak_arr(:), psik_arr(:,:), psi0_arr(:), psixt_arr(:,:)

end module progvars
