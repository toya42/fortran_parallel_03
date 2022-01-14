module parameters
  use,intrinsic :: iso_fortran_env
  implicit none
  integer(int32),parameter :: nsize = 10**5
  integer(int32) :: nloop,ntrial,napara,nomp
end module parameters

program one_d
  use,intrinsic :: iso_fortran_env
  use parameters
!  use type_definition
  use mod_1d,only : one_dimensional_tests
  implicit none
!  type(arrays_static) :: arrays_defined_at_main
  integer(int32), dimension(nsize) :: ai_static,bi_static,ci_static
  real(real32),   dimension(nsize) :: ar_static,br_static,cr_static
  real(real64),   dimension(nsize) :: ad_static,bd_static,cd_static
  complex(real64),dimension(nsize) :: ac_static,bc_static,cc_static
  logical,        dimension(nsize) :: al_static,bl_static,cl_static

  print *,'number of loops:'
  read *,nloop
  print *,'number of trials'
  read *,ntrial
  print *,'number of threads(auto-parallelize)'
  read *,napara
  print *,'number of threads(OpenMP)'
  read *,nomp

  call one_dimensional_tests(nsize,nloop,ntrial,napara,nomp,ai_static,bi_static,ci_static,&
  &                                             ar_static,br_static,cr_static,&
  &                                             ad_static,bd_static,cd_static,&
  &                                             ac_static,bc_static,cc_static,&
  &                                             al_static,bl_static,cl_static)

end program one_d