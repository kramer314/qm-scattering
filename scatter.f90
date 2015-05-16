! Copyright (c) 2015 Alex Kramer <kramer.alex.kramer@gmail.com>
! See the LICENSE.txt file at the top-level directory of this distribution.

program scatter
  use progvars
  use setup, only: init
  use wfunc_prop, only: propagate
  use output, only: write_output

  implicit none

  call init
  call propagate
  call write_output

end program scatter
