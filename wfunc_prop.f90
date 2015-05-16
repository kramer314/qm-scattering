! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.

module wfunc_prop
  use progvars

  implicit none
  private
  public propagate

  contains
    subroutine propagate
      call calculate_ak
      call calculate_psixt
    end subroutine propagate

    subroutine calculate_ak
      real(dp) :: trapz_const
      complex(dp) :: ak, fx_l, fx_r
      integer :: i_x, i_k

      write(*,*) "Calculating expansion coefficients a(k)"
      
      trapz_const = dx / 2.0_dp
      
      do i_k = 1, n_k
         ak = 0.0_dp

         do i_x = 1, n_x - 1

            fx_l = psi0_arr(i_x) * conjg(psik_arr(i_x, i_k))
            fx_r = psi0_arr(i_x + 1) * conjg(psik_arr(i_x + 1, i_k))
            ak = ak + fx_l + fx_r

         end do

         ak_arr(i_k) = ak * trapz_const

      end do

    end subroutine calculate_ak

    subroutine calculate_psixt
      integer :: i_x, i_t

      write(*,*) "Calculating psi(x,t)"

      do i_t = 1, n_t

         write(*,*) i_t, n_t

         do i_x = 1, n_x

            psixt_arr(i_x, i_t) = psixt(i_x, i_t)

         end do

      end do

    end subroutine calculate_psixt

    complex(dp) function psixt(i_x, i_t) result(val)
      integer, intent(in) :: i_x, i_t
      integer :: i_k
      real(dp) :: trapz_cnst, ek_l, ek_r, t
      complex(dp) :: ak_l, ak_r, pk_l, pk_r, fk_l, fk_r

      trapz_cnst = dk / 2
      t = t_range(i_t)

      val = 0.0_dp
      do i_k = 1, n_k - 1

         ak_l = ak_arr(i_k)
         ek_l = ek_arr(i_k)
         pk_l = psik_arr(i_x, i_k)

         ak_r = ak_arr(i_k + 1)
         ek_l = ek_arr(i_k + 1)
         pk_l = psik_arr(i_x, i_k + 1)

         fk_l = ak_l * exp(-j * ek_l * t) * pk_l
         fk_r = ak_r * exp(-j * ek_r * t) * pk_r

         val = val + fk_l + fk_r

      end do

      val = val * trapz_cnst

    end function psixt
      

end module wfunc_prop
