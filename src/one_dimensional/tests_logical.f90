module mod_tests_logical
  use,intrinsic :: iso_fortran_env
  use mod_wall_clock_time, only : wall_clock_time
  implicit none
  private
  public logical_disjunction,logical_conjunction
  contains
    !---
    subroutine logical_disjunction(nl,ni,wctime,n,a,b,c)
      integer(int32),intent(in) :: nl,ni,n
      real(real64),dimension(ni),intent(out) :: wctime
      logical,dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i,l
      real(real64) :: t1,t2

      ! implimentation 1
      call wall_clock_time(t1)
      do l=1,nl
        do i=1,n
          a(i) = b(i) .or. c(i)
        end do
      end do
      call wall_clock_time(t2)
      wctime(1) = wctime(1)+t2-t1

      ! implimentation 2
      call wall_clock_time(t1)
      do l=1,nl
        a(1:n) = b(1:n) .or. c(1:n)
      end do
      call wall_clock_time(t2)
      wctime(2) = wctime(2)+t2-t1

      ! implimentation 3
      call wall_clock_time(t1)
      do l=1,nl
        !$omp parallel do shared(n,a,b,c) private(i)
        do i=1,n
          a(i) = b(i) .or. c(i)
        end do
        !$omp end parallel do
      end do
      call wall_clock_time(t2)
      wctime(3) = wctime(3)+t2-t1

      ! implimentation 4
      call wall_clock_time(t1)
      do l=1,nl
        !$omp workshare
        a(1:n) = b(1:n) .or. c(1:n)
        !$omp end workshare
      end do
      call wall_clock_time(t2)
      wctime(4) = wctime(4)+t2-t1

      ! implimentation 5
      call wall_clock_time(t1)
      do l=1,nl
        do concurrent (i=1:n)
          a(i) = b(i) .or. c(i)
        end do
      end do
      call wall_clock_time(t2)
      wctime(5) = wctime(5)+t2-t1

    end subroutine logical_disjunction
    !---
    subroutine logical_conjunction(nl,ni,wctime,n,a,b,c)
      integer(int32),intent(in) :: nl,ni,n
      real(real64),dimension(ni),intent(out) :: wctime
      logical,dimension(n),intent(inout) :: a,b,c
      integer(int32) :: i,l
      real(real64) :: t1,t2

      ! implimentation 1
      call wall_clock_time(t1)
      do l=1,nl
        do i=1,n
          a(i) = b(i) .and. c(i)
        end do
      end do
      call wall_clock_time(t2)
      wctime(1) = wctime(1)+t2-t1

      ! implimentation 2
      call wall_clock_time(t1)
      do l=1,nl
        a(1:n) = b(1:n) .and. c(1:n)
      end do
      call wall_clock_time(t2)
      wctime(2) = wctime(2)+t2-t1

      ! implimentation 3
      call wall_clock_time(t1)
      do l=1,nl
        !$omp parallel do shared(n,a,b,c) private(i)
        do i=1,n
          a(i) = b(i) .and. c(i)
        end do
        !$omp end parallel do
      end do
      call wall_clock_time(t2)
      wctime(3) = wctime(3)+t2-t1

      ! implimentation 4
      call wall_clock_time(t1)
      do l=1,nl
        !$omp workshare
        a(1:n) = b(1:n) .and. c(1:n)
        !$omp end workshare
      end do
      call wall_clock_time(t2)
      wctime(4) = wctime(4)+t2-t1

      ! implimentation 5
      call wall_clock_time(t1)
      do l=1,nl
        do concurrent (i=1:n)
          a(i) = b(i) .and. c(i)
        end do
      end do
      call wall_clock_time(t2)
      wctime(5) = wctime(5)+t2-t1

    end subroutine logical_conjunction
    !---
end module mod_tests_logical