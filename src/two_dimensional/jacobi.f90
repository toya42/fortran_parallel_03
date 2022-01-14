module mod_tests_jacobi_iteration_4_point_stencil
  use,intrinsic :: iso_fortran_env
  use mod_wall_clock_time, only : wall_clock_time
  implicit none
  private 
  public tests_jacobi
  contains
    !---
    subroutine tests_jacobi(nsize,nloop,nimp,wct,a,b)
      integer(int32),intent(in) :: nsize,nloop,nimp
      real(real64),dimension(nimp),intent(out) :: wct
      real(real64),dimension(nsize,nsize),intent(inout) :: a,b
      integer(int32) :: i,j,k,l
      real(real64) :: t1,t2,error

      ! implementation 1 "sequential do"
      i = 1
      call wall_clock_time(t1)
      do l=1,nloop
        do k=2,nsize-1
          do j=2,nsize-1
            b(j,k) = 0.25d0*(a(j,k-1)+a(j-1,k)+a(j+1,k)+a(j,k+1))
          end do
        end do
        !error = sum(abs(a-b))
        do k=2,nsize-1
          do j=2,nsize-1
            a(j,k) = b(j,k)
          end do
        end do
      end do
      call wall_clock_time(t2)
      wct(i) = t2-t1

      ! implementation 2 "OpenMP do"
      i = 2
      call wall_clock_time(t1)
      do l=1,nloop
        !$omp parallel do private(j,k) shared(nsize,a,b)
        do k=2,nsize-1
          do j=2,nsize-1
            b(j,k) = 0.25d0*(a(j,k-1)+a(j-1,k)+a(j+1,k)+a(j,k+1))
          end do
        end do
        !$omp end parallel do
        !error = sum(abs(a-b))
        !$omp parallel do private(j,k) shared(nsize,a,b)
        do k=2,nsize-1
          do j=2,nsize-1
            a(j,k) = b(j,k)
          end do
        end do
        !$omp end parallel do
      end do
      call wall_clock_time(t2)
      wct(i) = t2-t1

      ! implementation 3 "do concurrent (single loop)"
      i = 3
      call wall_clock_time(t1)
      do l=1,nloop
        do concurrent(k=2:nsize-1,j=2:nsize-1)
          b(j,k) = 0.25d0*(a(j,k-1)+a(j-1,k)+a(j+1,k)+a(j,k+1))
        end do
        !error = sum(abs(a-b))
        do concurrent(k=2:nsize-1,j=2:nsize-1)
        !do concurrent(j=2:nsize-1,k=2:nsize-1)
          a(j,k) = b(j,k)
        end do
      end do
      call wall_clock_time(t2)
      wct(i) = t2-t1

       ! implementation 4 "do concurrent (double loop)"
      i = 4
      call wall_clock_time(t1)
      do l=1,nloop
        do concurrent(k=2:nsize-1)
          do concurrent(j=2:nsize-1)
            b(j,k) = 0.25d0*(a(j,k-1)+a(j-1,k)+a(j+1,k)+a(j,k+1))
          end do
        end do
        !error = sum(abs(a-b))
        do concurrent(k=2:nsize-1)
          do concurrent(j=2:nsize-1)
            a(j,k) = b(j,k)
          end do
        end do
      end do
      call wall_clock_time(t2)
      wct(i) = t2-t1

    end subroutine tests_jacobi
    !---
end module mod_tests_jacobi_iteration_4_point_stencil