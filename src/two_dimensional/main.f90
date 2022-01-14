module parameters
  use,intrinsic :: iso_fortran_env
  implicit none
  integer(int32),parameter :: nsize_jacobi = 10**3
  integer(int32),parameter :: nsize_riemann = 2*10**2
  integer(int32) :: nloop_jacobi,nloop_riemann,ntrial,napara,nomp
end module parameters

program one_d
  use,intrinsic :: iso_fortran_env
  use parameters
!  use type_definition
  use mod_2d,only : two_dimensional_tests
  implicit none

  print *,'number of loops( jacobi):'
  read *,nloop_jacobi
  print *,'number of loops(riemann):'
  read *,nloop_riemann
  print *,'number of trials'
  read *,ntrial
  print *,'number of threads(auto-parallelize)'
  read *,napara
  print *,'number of threads(OpenMP)'
  read *,nomp

  call two_dimensional_tests(nsize_jacobi,nsize_riemann,&
  &                           nloop_jacobi,nloop_riemann,ntrial,napara,nomp)

end program one_d