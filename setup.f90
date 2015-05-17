! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.

module setup
  ! Setup params, etc.

  use progvars
  use params, only: assign_params, psi0, psik, ek
  use numerics, only: linspace
  use files, only: ensure_dir

  implicit none

  private
  public init

  contains

    subroutine init
      ! Set parameters, allocate and initialize various arrays
      implicit none

      call assign_params
      call ensure_dir(output_dir)
      call allocate_arrays
      call init_grids
      call init_psi
      call init_ek
    end subroutine init

    subroutine allocate_arrays
      ! Allocate arrays
      implicit none

      allocate(x_range(n_x), psi0_arr(n_x))
      allocate(k_range(n_k), ak_arr(n_k), ek_arr(n_k))
      allocate(t_range(n_t))
      allocate(psik_arr(n_x, n_k))
      allocate(psixt_arr(n_x, n_t))

    end subroutine allocate_arrays

    subroutine init_grids
      ! Setup numerical grids
      implicit none

      call linspace(x_min, x_max, x_range, dx)
      call linspace(k_min, k_max, k_range, dk)
      call linspace(t_min, t_max, t_range, dt)
    end subroutine init_grids

    subroutine init_psi
      ! Setup psi0 and psik arrays
      implicit none

      real(dp) :: x, k
      integer :: i_x, i_k

      do i_x = 1, n_x
         x = x_range(i_x)
         psi0_arr(i_x) = psi0(x)

         do i_k = 1, n_k
            k = k_range(i_k)
            psik_arr(i_x, i_k) = psik(x,k)
         end do

      end do
      
    end subroutine init_psi

    subroutine init_ek
      ! Setup ek array of energies
      implicit none

      real(dp) :: k
      integer :: i_k

      do i_k = 1, n_k
         k = k_range(i_k)
         ek_arr(i_k) = ek(k)
      end do

    end subroutine init_ek

end module setup
