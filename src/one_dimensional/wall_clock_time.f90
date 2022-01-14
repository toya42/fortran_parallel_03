module mod_wall_clock_time
  use,intrinsic :: iso_fortran_env
  implicit none
  private
  public wall_clock_time
  contains
  !---
    subroutine wall_clock_time(time)
      use omp_lib
      real(real64),intent(out) :: time

      time = omp_get_wtime()
      !call cpu_time(time)

    end subroutine wall_clock_time
  !---
end module mod_wall_clock_time