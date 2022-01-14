module mod_multiplication
  use,intrinsic :: iso_fortran_env
  use mod_wall_clock_time, only : wall_clock_time
  implicit none
  private
  public multiplication
  interface multiplication
    module procedure multiplication_i4,multiplication_r4,multiplication_r8,multiplication_c8
  end interface multiplication

  contains
    !---
    subroutine multiplication_i4(time_do,time_array,n,a,b,c)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      integer(int32),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)*c(i)
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)*c(1:n)
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine multiplication_i4
    !---
    subroutine multiplication_r4(time_do,time_array,n,a,b,c)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      real(real32),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)*c(i)
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)*c(1:n)
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine multiplication_r4
    !---
    subroutine multiplication_r8(time_do,time_array,n,a,b,c)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      real(real64),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)*c(i)
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)*c(1:n)
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine multiplication_r8
    !---
    subroutine multiplication_c8(time_do,time_array,n,a,b,c)
      real(real64),intent(out) :: time_do,time_array
      integer(int32),intent(in) :: n
      complex(real64),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i
      real(real64) :: t1,t2

      call wall_clock_time(t1)
      do i=1,n
        a(i) = b(i)*c(i)
      end do
      call wall_clock_time(t2)
      time_do = t2-t1

      call wall_clock_time(t1)
      a(1:n) = b(1:n)*c(1:n)
      call wall_clock_time(t2)
      time_array = t2-t1

    end subroutine multiplication_c8
    !---
end module mod_multiplication