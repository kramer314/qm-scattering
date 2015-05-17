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

      if (output_grid) then
         call write_grid
      end if

      if (output_psi0) then
         call write_psi0
      end if

      if (output_psik) then
         call write_psik
      end if

      if (output_ak) then
         call write_ak
      end if

      if (output_ek) then
         call write_ak
      end if

      if (output_psixt) then
         call write_psixt
      end if

    end subroutine write_output

    subroutine write_grid
      implicit none

      open(unit=1, file=trim(output_dir)//trim(x_out))
      open(unit=2, file=trim(output_dir)//trim(k_out))
      open(unit=3, file=trim(output_dir)//trim(t_out))
         
      do i_x = 1, n_x
         write(1, dp_format) x_range(i_x)
      end do
      do i_k = 2, n_k
         write(2, dp_format) k_range(i_k)
      end do
      do i_t = 3, n_t
         write(3, dp_format) t_range(i_t)
      end do

      close(unit=1)
      close(unit=2)
      close(unit=3)
    end subroutine write_grid

    subroutine write_psi0
      implicit none

      open(unit=4, file=trim(output_dir)//trim(psi0_out))

      do i_x = 1, n_x
         write(4, dp_format) abs(psi0_arr(i_x))**2
      end do

      close(unit=4)

    end subroutine write_psi0

    subroutine write_psik
      implicit none

      open(unit=5, file=trim(output_dir)//trim(psik_out))

      do i_x = 1, n_x
         do i_k = 1, n_k
            write(5, dp_format, advance="no") abs(psik_arr(i_x, i_k))**2
         end do
      end do

      close(unit=5)

    end subroutine write_psik

    subroutine write_ak
      implicit none

      open(unit=6, file=trim(output_dir)//trim(ak_out))

      do i_k = 1, n_k
         write(6, dp_format) abs(ak_arr(i_k))**2
      end do

      close(unit=6)

    end subroutine write_ak

    subroutine write_ek
      implicit none

      open(unit=7, file=trim(output_dir)//trim(ek_out))
      
      do i_k = 1, n_k
         write(7, dp_format) ek_arr(i_k)**2
      end do

      close(unit=7)

    end subroutine write_ek

    subroutine write_psixt
      implicit none

      open(unit=8, file=trim(output_dir)//trim(psit_out))

      do i_x = 1, n_x
         do i_t = 1, n_t
            write(8, dp_format, advance="no") abs(psixt_arr(i_x, i_t))**2
         end do
         write(8, *)
      end do

      close(unit=8)
         
    end subroutine write_psixt

end module output
