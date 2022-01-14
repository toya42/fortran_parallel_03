module mod_summation
  use,intrinsic :: iso_fortran_env
  use mod_wall_clock_time, only : wall_clock_time
  implicit none
  private
  !public summation
  !interface summation
  !  module procedure summation_i4,summation_r4,summation_r8,summation_c8
  !end interface summation
  public summation_i4,summation_r4,summation_r8,summation_c8

  contains
    !---
    subroutine summation_i4(time_do,time_array,n,a,b,c)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      integer(int32),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)+c(i)
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)+c(1:n)
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine summation_i4
    !---
    subroutine summation_r4(time_do,time_array,n,a,b,c)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      real(real32),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)+c(i)
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)+c(1:n)
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine summation_r4
    !---
    subroutine summation_r8(time_do,time_array,n,a,b,c)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      real(real64),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)+c(i)
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)+c(1:n)
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine summation_r8
    !---
    subroutine summation_c8(time_do,time_array,n,a,b,c)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      complex(real64),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)+c(i)
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)+c(1:n)
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine summation_c8
    !---
end module mod_summation