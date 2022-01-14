module mod_tests_int4
  use,intrinsic :: iso_fortran_env
  use mod_wall_clock_time, only : wall_clock_time
  implicit none
  private
  public addition_int4,multiplication_int4,division_int4
  contains
    !---
    subroutine addition_int4(nl,ni,wctime,n,a,b,c)
      integer(int32),intent(in) :: nl,ni,n
      real(real64),dimension(ni),intent(out) :: wctime
      integer(int32),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i,l
      real(real64) :: t1,t2

      ! implimentation 1
      call wall_clock_time(t1)
      do l=1,nl
        do i=1,n
          a(i) = b(i)+c(i)
        end do
      end do
      call wall_clock_time(t2)
      wctime(1) = wctime(1)+t2-t1

      ! implimentation 2
      call wall_clock_time(t1)
      do l=1,nl
        a(1:n) = b(1:n)+c(1:n)
      end do
      call wall_clock_time(t2)
      wctime(2) = wctime(2)+t2-t1

      ! implimentation 3
      call wall_clock_time(t1)
      do l=1,nl
        !$omp parallel do shared(n,a,b,c) private(i)
        do i=1,n
          a(i) = b(i)+c(i)
        end do
        !$omp end parallel do
      end do
      call wall_clock_time(t2)
      wctime(3) = wctime(3)+t2-t1

      ! implimentation 4
      call wall_clock_time(t1)
      do l=1,nl
        !$omp workshare
        a(1:n) = b(1:n)+c(1:n)
        !$omp end workshare
      end do
      call wall_clock_time(t2)
      wctime(4) = wctime(4)+t2-t1

      ! implimentation 5
      call wall_clock_time(t1)
      do l=1,nl
        do concurrent (i=1:n)
          a(i) = b(i)+c(i)
        end do
      end do
      call wall_clock_time(t2)
      wctime(5) = wctime(5)+t2-t1

    end subroutine addition_int4
    !---
    subroutine multiplication_int4(nl,ni,wctime,n,a,b,c)
      integer(int32),intent(in) :: nl,ni,n
      real(real64),dimension(ni),intent(out) :: wctime
      integer(int32),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i,l
      real(real64) :: t1,t2

      ! implimentation 1
      call wall_clock_time(t1)
      do l=1,nl
        do i=1,n
          a(i) = b(i)*c(i)
        end do
      end do
      call wall_clock_time(t2)
      wctime(1) = wctime(1)+t2-t1

      ! implimentation 2
      call wall_clock_time(t1)
      do l=1,nl
        a(1:n) = b(1:n)*c(1:n)
      end do
      call wall_clock_time(t2)
      wctime(2) = wctime(2)+t2-t1

      ! implimentation 3
      call wall_clock_time(t1)
      do l=1,nl
        !$omp parallel do shared(n,a,b,c) private(i)
        do i=1,n
          a(i) = b(i)*c(i)
        end do
        !$omp end parallel do
      end do
      call wall_clock_time(t2)
      wctime(3) = wctime(3)+t2-t1

      ! implimentation 4
      call wall_clock_time(t1)
      do l=1,nl
        !$omp workshare
        a(1:n) = b(1:n)*c(1:n)
        !$omp end workshare
      end do
      call wall_clock_time(t2)
      wctime(4) = wctime(4)+t2-t1

      ! implimentation 5
      call wall_clock_time(t1)
      do l=1,nl
        do concurrent (i=1:n)
          a(i) = b(i)*c(i)
        end do
      end do
      call wall_clock_time(t2)
      wctime(5) = wctime(5)+t2-t1


    end subroutine multiplication_int4
    !---
    subroutine division_int4(nl,ni,wctime,n,a,b,c)
      integer(int32),intent(in) :: nl,ni,n
      real(real64),dimension(ni),intent(out) :: wctime
      integer(int32),dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i,l
      real(real64) :: t1,t2

      ! implimentation 1
      call wall_clock_time(t1)
      do l=1,nl
        do i=1,n
          a(i) = b(i)/c(i)
        end do
      end do
      call wall_clock_time(t2)
      wctime(1) = wctime(1)+t2-t1

      ! implimentation 2
      call wall_clock_time(t1)
      do l=1,nl
        a(1:n) = b(1:n)/c(1:n)
      end do
      call wall_clock_time(t2)
      wctime(2) = wctime(2)+t2-t1

      ! implimentation 3
      call wall_clock_time(t1)
      do l=1,nl
        !$omp parallel do shared(n,a,b,c) private(i)
        do i=1,n
          a(i) = b(i)/c(i)
        end do
        !$omp end parallel do
      end do
      call wall_clock_time(t2)
      wctime(3) = wctime(3)+t2-t1

      ! implimentation 4
      call wall_clock_time(t1)
      do l=1,nl
        !$omp workshare
        a(1:n) = b(1:n)/c(1:n)
        !$omp end workshare
      end do
      call wall_clock_time(t2)
      wctime(4) = wctime(4)+t2-t1

      ! implimentation 5
      call wall_clock_time(t1)
      do l=1,nl
        do concurrent (i=1:n)
          a(i) = b(i)/c(i)
        end do
      end do
      call wall_clock_time(t2)
      wctime(5) = wctime(5)+t2-t1

    end subroutine division_int4
    !---
end module mod_tests_int4