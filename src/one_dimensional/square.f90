module mod_square
  use,intrinsic :: iso_fortran_env
  use mod_wall_clock_time, only : wall_clock_time
  implicit none
  private
  public square
  interface square
    module procedure square_i4,square_r4,square_r8,square_c8
  end interface square

  contains
    !---
    subroutine square_i4(time_do,time_array,n,a,b)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      integer(int32),dimension(n),intent(inout) :: a,b
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)**2
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)**2
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine square_i4
    !---
    subroutine square_r4(time_do,time_array,n,a,b)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      real(real32),dimension(n),intent(inout) :: a,b
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)**2
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)**2
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine square_r4
    !---
    subroutine square_r8(time_do,time_array,n,a,b)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      real(real64),dimension(n),intent(inout) :: a,b
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)**2
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)**2
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine square_r8
    !---
    subroutine square_c8(time_do,time_array,n,a,b)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      complex(real64),dimension(n),intent(inout) :: a,b
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)**2
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)**2
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine square_c8
    !---
end module mod_square