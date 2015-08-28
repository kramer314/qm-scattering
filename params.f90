! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.

module params
  ! Set parameters that define the scattering problem
  ! This file specifies a Gaussian wavepacket incident on the following
  ! potential:
  !   V(x) = oo, x = 0
  !   V(x) = \delta(x - a), x > 0
  use progvars

  implicit none

  private
  public psi0, psik, ek, assign_params

  ! Problem-specific variables
  real(dp) :: gamma, a, m
  real(dp) :: x_0, k_0, d_x, d_k
  integer(8) :: x_stdev, k_stdev
  
  contains

    subroutine assign_params
      implicit none

      ! Assign output parameters
      output_dir = "./output/"
      ak_out = "ak.dat"
      ek_out = "ek.dat"
      psit_out = "psit.dat"
      psi0_out = "psi0.dat"
      psik_out = "psik.dat"
      x_out = "x_range.dat"
      k_out = "k_range.dat"
      t_out = "t_range.dat"

      output_grid = .False.
      output_psi0 = .True.
      output_psik = .False.
      output_ak = .True.
      output_ek = .False.
      output_psixt = .True.

      ! Assign potential parameters
      gamma = 20.0_dp
      a = 1.0_dp
      m = 1.0_dp

      ! Assign wavepacket parameters
      d_k = 0.4_dp * pi / a
      d_x = 1.0_dp / d_k
      k_0 = -1.4_dp * pi / a

      x_stdev = 4.0_dp
      k_stdev = 4.0_dp

      x_0 = a + x_stdev * d_x

      ! Assign grid and propagation parameters
      x_min = 0.0_dp
      x_max = x_0 + x_stdev * d_x
      n_x = 5e2

      k_min = k_0 - k_stdev * d_k
      k_max = k_0 + k_stdev * d_k
      n_k = 1e2

      t_min = 0.0_dp
      t_max = 5.0_dp
      n_t = 5e2

    end subroutine assign_params

    complex(dp) function psi0(x) result(val)
      ! Initial wavefunction
      implicit none

      real(dp), intent(in) :: x

      real(dp) :: norm

      norm = (pi * d_x**2) ** (-1.0_dp/4)
      val = norm * exp(-0.5_dp * ((x - x_0) / d_x)**2) * exp(j * k_0 * x)

    end function psi0

    complex(dp) function psik(x, k) result(val)
      ! Momentum-normalized continuum eigenstates
      implicit none

      real(dp), intent(in) :: x, k
      real(dp) :: c_left, c_right, ka, delta

      c_right = sqrt(2.0_dp / pi)
      ka = k * a
      delta = atan(ka * tan(ka) / (ka + gamma * tan(ka)))
      c_left = sin(delta) / sin(ka) * c_right

      val = 0
      if (0 .le. x .and. x .le. a) then
         val = c_left * sin(k * x)
      else if (x .ge. a) then
         val = c_right * sin(k * (x - a) + delta)
      end if

    end function psik

    real(dp) function ek(k) result(val)
      ! Free particle energy
      implicit none

      real(dp), intent(in) :: k

      val = k**2 / (2.0_dp * m)
    end function ek
    
end module params
