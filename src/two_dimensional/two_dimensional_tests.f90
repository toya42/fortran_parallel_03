module mod_2d
  use,intrinsic :: iso_fortran_env
  use mod_wall_clock_time, only : wall_clock_time
  use mod_tests_jacobi_iteration_4_point_stencil
  use mod_tests_riemann_flux_roe
  !use mod_tests_metropolis_method_monte_carlo
  implicit none
  private
  public two_dimensional_tests

  contains
    !---
    subroutine two_dimensional_tests(nsize,nsize2,nloop,nloop2,ntrial,napara,nomp)
      integer(int32),intent(in) :: nsize,nsize2,nloop,nloop2,ntrial,napara,nomp

      integer(int32),parameter :: num_tests = 3
      integer(int32),parameter :: max_num_implementation = 5

      character(len=50),dimension(num_tests) :: testname
      ! test 1 : jacobi iteration (4 point stencil)
      real(real64),dimension(:,:),allocatable :: amatrix,bmatrix
      ! test 2 : riemann flux (roe's upwind flux)
      real(real64),dimension(:,:,:),allocatable :: primitives,flux_2d

      integer(int32) :: num_implementation
      character(len=50),dimension(:),allocatable :: implementation_name
      ! wall clock time
      real(real64),dimension(ntrial,max_num_implementation) :: wct_raw  ! wall clock time of each trial
      real(real64),dimension(max_num_implementation) :: wct_ave,wct_sgm ! average and sigma

      integer(int32) ::n,index


      !---parameters---
      print *,'# nsize( jacobi)',nsize
      print *,'# nloop( jacobi)',nloop
      print *,'# nsize(riemann)',nsize2
      print *,'# nloop(riemann)',nloop2
      print *,'# ntrial',ntrial

      ! test 1 : jacobi iteration (4 point stencil)
      print *,'### test 1 ###'
      index = 1
      testname(index) = 'jacobi iteration'
      print *,testname(index)
      num_implementation = 4
      allocate(implementation_name(num_implementation))
      implementation_name(1) ='sequential do'
      implementation_name(2) ='OpenMP do'
      implementation_name(3) ='do concurrent(single loop)'
      implementation_name(4) ='do concurrent(double loop)'

      allocate(amatrix(nsize,nsize),bmatrix(nsize,nsize))
      call random_number(amatrix)
      call random_number(bmatrix)
      wct_raw = 0.0d0;wct_ave=0.0d0;wct_sgm=0.0d0

      do n=1,ntrial
        call tests_jacobi(nsize,nloop,num_implementation,wct_raw(n,:),amatrix,bmatrix)
      end do
      do n=1,num_implementation
        call ave_and_sgm(ntrial,wct_raw(:,n),wct_ave(n),wct_sgm(n))
      end do

      call display(index,napara,nomp,num_implementation,implementation_name,wct_ave,wct_sgm,testname(index))
      deallocate(amatrix,bmatrix,implementation_name)

      ! test 2 : riemann problem
      print *,'### test 2 ###'
      index = 2
      testname(index) = 'riemann problem'
      print *,testname(index)
      num_implementation = 4
      allocate(implementation_name(num_implementation))
      implementation_name(1) ='sequential do'
      implementation_name(2) ='OpenMP do'
      implementation_name(3) ='do concurrent(single loop)'
      implementation_name(4) ='do concurrent(double loop)'

      allocate(primitives(4,nsize2,nsize2),flux_2d(4,nsize2,nsize2))
      call random_number(primitives)
      primitives = primitives+1.0d0
      wct_raw = 0.0d0;wct_ave=0.0d0;wct_sgm=0.0d0

      do n=1,ntrial
        call tests_riemann_flux_roe(nsize2,nloop2,num_implementation,wct_raw(n,:),primitives,flux_2d)
      end do
      do n=1,num_implementation
        call ave_and_sgm(ntrial,wct_raw(:,n),wct_ave(n),wct_sgm(n))
      end do

      call display(index,napara,nomp,num_implementation,implementation_name,wct_ave,wct_sgm,testname(index))
      deallocate(primitives,flux_2d,implementation_name)



    end subroutine two_dimensional_tests
    !---
    subroutine display(index,napara,nomp,nimp,impname,wct_ave,wct_sgm,testname)
      integer(int32),intent(in) :: index,napara,nomp,nimp
      real(real64),dimension(nimp),intent(inout) :: wct_ave(:),wct_sgm(:)
      character(len=50),intent(in) :: testname
      character(len=50),intent(in) :: impname(nimp)
      integer(int32) :: k
      real(real64) :: tmp
      character(len=50) :: filename

      write(filename,'(i3.3,".csv")') index
      open(11,file=filename)
      write(11,*) 'test_name',",",'implementation_name',",",'num_of_threads(auto-parallelize)',",",&
      &                             'num_of_threads(OpenMP)',",",'average[s]',",",'sigma[s]'
      ! data file
      do k=1,nimp
        write(11,*) &
        &     trim(testname),",",trim(impname(k)),",",napara,",",nomp,",",wct_ave(k),",",wct_sgm(k)
      end do
      close(11)

      ! display
      tmp = wct_ave(1)
      wct_ave(:) = wct_ave(:)/tmp
      wct_sgm(:) = wct_sgm(:)/tmp
      print *,'# ',testname
      print *,'# reference time[s]:',tmp
      do k=1,nimp
        write(output_unit,'(a20,f6.3,a3,f7.4)') trim(impname(k)),wct_ave(k),'+-',wct_sgm(k)
      end do
      write(output_unit,*)

    end subroutine display
    !---
    subroutine ave_and_sgm(n,raw,ave,sgm)
      integer(int32),intent(in) :: n
      real(real64),dimension(n),intent(in) :: raw
      real(real64),intent(out) :: ave,sgm
      integer(int32) :: i

      ave = sum(raw(1:n))/real(n,real64)
      sgm = 0.0d0
      do i=1,n
        sgm = sgm + (ave-raw(i))**2
      end do
      sgm = sqrt(sgm/real(n,real64))

      !print *,ave,sgm

    end subroutine ave_and_sgm
    !---
end module mod_2d