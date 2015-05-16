! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.

module output
  use progvars

  implicit none

  private
  public write_output

  integer :: i_x, i_k, i_t

  contains
    subroutine write_output
      implicit none

      call init
      call write_grid
      call write_psi0
      call write_psik
      call write_ak
      call write_ek
      call write_psixt
      call cleanup
    end subroutine write_output

    subroutine init
      implicit none

      open(unit=1, file=x_out)
      open(unit=2, file=k_out)
      open(unit=3, file=t_out)
      open(unit=4, file=psi0_out)
      open(unit=5, file=psik_out)
      open(unit=6, file=ak_out)
      open(unit=7, file=ek_out)
      open(unit=8, file=psit_out)
    end subroutine init

    subroutine write_grid
      implicit none

      do i_x = 1, n_x
         write(1, dp_format) x_range(i_x)
      end do
      do i_k = 2, n_k
         write(1, dp_format) k_range(i_k)
      end do
      do i_t = 3, n_t
         write(1, dp_format) t_range(i_t)
      end do
    end subroutine write_grid

    subroutine write_psi0
      implicit none

      do i_x = 1, n_x
         write(4, dp_format) abs(psi0_arr(i_x))**2
      end do

    end subroutine write_psi0

    subroutine write_psik
      implicit none

      do i_x = 1, n_x
         do i_k = 1, n_k
            write(5, dp_format, advance="no") abs(psik_arr(i_x, i_k))**2
         end do
      end do

    end subroutine write_psik

    subroutine write_ak
      implicit none

      do i_k = 1, n_k
         write(6, dp_format) abs(ak_arr(i_k))**2
      end do
    end subroutine write_ak

    subroutine write_ek
      implicit none

      do i_k = 1, n_k
         write(7, dp_format) ek_arr(i_k)**2
      end do
    end subroutine write_ek

    subroutine write_psixt
      do i_x = 1, n_x
         do i_t = 1, n_t
            write(8, dp_format, advance="no") abs(psixt_arr(i_x, i_t))**2
         end do
         write(8, *)
      end do
         
    end subroutine write_psixt

    subroutine cleanup
      implicit none

      close(unit=1)
      close(unit=2)
      close(unit=3)
      close(unit=4)
      close(unit=5)
      close(unit=6)
      close(unit=7)
      close(unit=8)
    end subroutine cleanup
end module output
