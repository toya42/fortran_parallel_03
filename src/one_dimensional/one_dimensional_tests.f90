module mod_1d
  use,intrinsic :: iso_fortran_env
  use mod_wall_clock_time, only : wall_clock_time
  use mod_tests_int4
  use mod_tests_real4
  use mod_tests_real8
  use mod_tests_complex8
  use mod_tests_logical
  implicit none
  private
  public one_dimensional_tests

  interface
     subroutine sub_int4 (nl,ni,time,nm,a,b,c)
        use,intrinsic :: iso_fortran_env
        implicit none
        integer(int32),intent(in) :: nl,ni,nm
        real(real64),dimension(ni),intent(out) :: time
        integer(int32),dimension(nm),intent(inout) :: a,b,c
     end subroutine sub_int4
  end interface
  interface
     subroutine sub_real4 (nl,ni,time,nm,a,b,c)
        use,intrinsic :: iso_fortran_env
        implicit none
        integer(int32),intent(in) :: nl,ni,nm
        real(real64),dimension(ni),intent(out) :: time
        real(real32),dimension(nm),intent(inout) :: a,b,c
     end subroutine sub_real4
  end interface
  interface
     subroutine sub_real8 (nl,ni,time,nm,a,b,c)
        use,intrinsic :: iso_fortran_env
        implicit none
        integer(int32),intent(in) :: nl,ni,nm
        real(real64),dimension(ni),intent(out) :: time
        real(real64),dimension(nm),intent(inout) :: a,b,c
     end subroutine sub_real8
  end interface
  interface
     subroutine sub_complex8 (nl,ni,time,nm,a,b,c)
        use,intrinsic :: iso_fortran_env
        implicit none
        integer(int32),intent(in) :: nl,ni,nm
        real(real64),dimension(ni),intent(out) :: time
        complex(real64),dimension(nm),intent(inout) :: a,b,c
     end subroutine sub_complex8
  end interface
  interface
     subroutine sub_logical (nl,ni,time,nm,a,b,c)
        use,intrinsic :: iso_fortran_env
        implicit none
        integer(int32),intent(in) :: nl,ni,nm
        real(real64),dimension(ni),intent(out) :: time
        logical,dimension(nm),intent(inout) :: a,b,c
     end subroutine sub_logical
  end interface

  type :: type_int4
    procedure(sub_int4),pointer,nopass :: pp_int4=>null()
  end type type_int4
  type :: type_real4
    procedure(sub_real4),pointer,nopass :: pp_real4=>null()
  end type type_real4
  type :: type_real8
    procedure(sub_real8),pointer,nopass :: pp_real8=>null()
  end type type_real8
  type :: type_complex8
    procedure(sub_complex8),pointer,nopass :: pp_complex8=>null()
  end type type_complex8
  type :: type_logical
    procedure(sub_logical),pointer,nopass :: pp_logical=>null()
  end type type_logical

  contains
    !---
    subroutine one_dimensional_tests(nsize,nloop,ntrial,napara,nomp,ai_static,bi_static,ci_static,&
    &                                                   ar_static,br_static,cr_static,&
    &                                                   ad_static,bd_static,cd_static,&
    &                                                   ac_static,bc_static,cc_static,&
    &                                                   al_static,bl_static,cl_static)
      integer(int32),intent(in) :: nsize,nloop,ntrial,napara,nomp
      integer(int32), dimension(nsize),intent(inout) :: ai_static,bi_static,ci_static
      real(real32),   dimension(nsize),intent(inout) :: ar_static,br_static,cr_static
      real(real64),   dimension(nsize),intent(inout) :: ad_static,bd_static,cd_static
      complex(real64),dimension(nsize),intent(inout) :: ac_static,bc_static,cc_static
      logical,        dimension(nsize),intent(inout) :: al_static,bl_static,cl_static
      integer(int32), dimension(nsize) :: ai_stack,bi_stack,ci_stack
      real(real32),   dimension(nsize) :: ar_stack,br_stack,cr_stack
      real(real64),   dimension(nsize) :: ad_stack,bd_stack,cd_stack
      complex(real64),dimension(nsize) :: ac_stack,bc_stack,cc_stack
      logical,        dimension(nsize) :: al_stack,bl_stack,cl_stack
      integer(int32), dimension(:),allocatable :: ai_heap,bi_heap,ci_heap
      real(real32),   dimension(:),allocatable :: ar_heap,br_heap,cr_heap
      real(real64),   dimension(:),allocatable :: ad_heap,bd_heap,cd_heap
      complex(real64),dimension(:),allocatable :: ac_heap,bc_heap,cc_heap
      logical,        dimension(:),allocatable :: al_heap,bl_heap,cl_heap

      integer(int32),parameter :: num_tests_int4 = 3
      integer(int32),parameter :: num_implimentation_int4 = 5
      type(type_int4) :: tests_int4(num_tests_int4)

      integer(int32),parameter :: num_tests_real4 = 3
      integer(int32),parameter :: num_implimentation_real4 = 5
      type(type_real4) :: tests_real4(num_tests_real4)

      integer(int32),parameter :: num_tests_real8 = 3
      integer(int32),parameter :: num_implimentation_real8 = 5
      type(type_real8) :: tests_real8(num_tests_real8)

      integer(int32),parameter :: num_tests_complex8 = 3
      integer(int32),parameter :: num_implimentation_complex8 = 5
      type(type_complex8) :: tests_complex8(num_tests_complex8)

      integer(int32),parameter :: num_tests_logical = 2
      integer(int32),parameter :: num_implimentation_logical = 5
      type(type_logical) :: tests_logical(num_tests_logical)

      integer(int32),parameter :: max_num_tests = 10
      integer(int32),parameter :: max_num_implimentation = 10
      !integer(int32),parameter :: ntrial = 10
      real(real64),dimension(ntrial,max_num_implimentation,max_num_tests,3) :: wct_raw
      real(real64),dimension(max_num_implimentation,max_num_tests,3) :: wct_ave,wct_sgm
      character(len=50),dimension(max_num_tests) :: testname
      character(len=50),dimension(max_num_implimentation) :: impname
      integer(int32) :: num_implimentation,num_tests

      integer(int32) ::i,j,n,variable_type_index

      impname(1) ='sequential-do'
      impname(2) ='array'
      impname(3) ='OpenMP-do'
      impname(4) ='workshare'
      impname(5) ='do-concurrent'

      !---initialization---
      call initialize(nsize,ai_static,bi_static,ci_static,&
      &                     ar_static,br_static,cr_static,&
      &                     ad_static,bd_static,cd_static,&
      &                     ac_static,bc_static,cc_static,&
      &                     al_static,bl_static,cl_static)

      !---test---
      print *,'# nsize',nsize
      print *,'# nloop',nloop
      print *,'# ntrial',ntrial

      ! integer
      print *,'### integer ###'
      variable_type_index=1
      allocate(ai_heap(nsize),bi_heap(nsize),ci_heap(nsize))
      ai_stack = ai_static; bi_stack = bi_static; ci_stack = ci_static
      ai_heap  = ai_static; bi_heap  = bi_static; ci_heap  = ci_static
      wct_raw = 0.0d0;wct_ave=0.0d0;wct_sgm=0.0d0

      num_implimentation = num_implimentation_int4
      num_tests = num_tests_int4
      testname(1) = 'addition_int4'
      tests_int4(1)%pp_int4 => addition_int4
      testname(2) = 'multiplication_int4'
      tests_int4(2)%pp_int4 => multiplication_int4
      testname(3) = 'division_int4'
      tests_int4(3)%pp_int4 => division_int4

      do i = 1,num_tests
        do n=1,ntrial
          call tests_int4(i)%pp_int4(nloop,num_implimentation,wct_raw(n,:,i,1),nsize,ai_static,bi_static,ci_static)
          call tests_int4(i)%pp_int4(nloop,num_implimentation,wct_raw(n,:,i,2),nsize,ai_stack, bi_stack, ci_stack)
          call tests_int4(i)%pp_int4(nloop,num_implimentation,wct_raw(n,:,i,3),nsize,ai_heap,  bi_heap,  ci_heap)
        end do
        do j=1,num_implimentation
          call ave_and_sgm(ntrial,wct_raw(:,j,i,1),wct_ave(j,i,1),wct_sgm(j,i,1))
          call ave_and_sgm(ntrial,wct_raw(:,j,i,2),wct_ave(j,i,2),wct_sgm(j,i,2))
          call ave_and_sgm(ntrial,wct_raw(:,j,i,3),wct_ave(j,i,3),wct_sgm(j,i,3))
        end do
      end do

      deallocate(ai_heap,bi_heap,ci_heap)
      call display(variable_type_index,napara,nomp,num_implimentation,impname,3,num_tests,wct_ave,wct_sgm,testname)


      ! real(real32)
      print *,'### real ###'
      variable_type_index=2
      allocate(ar_heap(nsize),br_heap(nsize),cr_heap(nsize))
      ar_stack = ar_static; br_stack = br_static; cr_stack = cr_static
      ar_heap  = ar_static; br_heap  = br_static; cr_heap  = cr_static
      wct_raw = 0.0d0;wct_ave=0.0d0;wct_sgm=0.0d0

      num_implimentation = num_implimentation_real4
      num_tests = num_tests_real4
      testname(1) = 'addition_real4'
      tests_real4(1)%pp_real4 => addition_real4
      testname(2) = 'multiplication_real4'
      tests_real4(2)%pp_real4 => multiplication_real4
      testname(3) = 'division_real4'
      tests_real4(3)%pp_real4 => division_real4

      do i = 1,num_tests
        do n=1,ntrial
          call tests_real4(i)%pp_real4(nloop,num_implimentation,wct_raw(n,:,i,1),nsize,ar_static,br_static,cr_static)
          call tests_real4(i)%pp_real4(nloop,num_implimentation,wct_raw(n,:,i,2),nsize,ar_stack, br_stack, cr_stack)
          call tests_real4(i)%pp_real4(nloop,num_implimentation,wct_raw(n,:,i,3),nsize,ar_heap,  br_heap,  cr_heap)
        end do
        do j=1,num_implimentation
          call ave_and_sgm(ntrial,wct_raw(:,j,i,1),wct_ave(j,i,1),wct_sgm(j,i,1))
          call ave_and_sgm(ntrial,wct_raw(:,j,i,2),wct_ave(j,i,2),wct_sgm(j,i,2))
          call ave_and_sgm(ntrial,wct_raw(:,j,i,3),wct_ave(j,i,3),wct_sgm(j,i,3))
        end do
      end do

      deallocate(ar_heap,br_heap,cr_heap)
      call display(variable_type_index,napara,nomp,num_implimentation,impname,3,num_tests,wct_ave,wct_sgm,testname)

      ! real(real64)
      print *,'### double precision ###'
      variable_type_index=3
      allocate(ad_heap(nsize),bd_heap(nsize),cd_heap(nsize))
      ad_stack = ad_static; bd_stack = bd_static; cd_stack = cd_static
      ad_heap  = ad_static; bd_heap  = bd_static; cd_heap  = cd_static
      wct_raw = 0.0d0;wct_ave=0.0d0;wct_sgm=0.0d0

      num_implimentation = num_implimentation_real8
      num_tests = num_tests_real8
      testname(1) = 'addition_real8'
      tests_real8(1)%pp_real8 => addition_real8
      testname(2) = 'multiplication_real8'
      tests_real8(2)%pp_real8 => multiplication_real8
      testname(3) = 'division_real8'
      tests_real8(3)%pp_real8 => division_real8

      do i = 1,num_tests
        do n=1,ntrial
          call tests_real8(i)%pp_real8(nloop,num_implimentation,wct_raw(n,:,i,1),nsize,ad_static,bd_static,cd_static)
          call tests_real8(i)%pp_real8(nloop,num_implimentation,wct_raw(n,:,i,2),nsize,ad_stack, bd_stack, cd_stack)
          call tests_real8(i)%pp_real8(nloop,num_implimentation,wct_raw(n,:,i,3),nsize,ad_heap,  bd_heap,  cd_heap)
        end do
        do j=1,num_implimentation
          call ave_and_sgm(ntrial,wct_raw(:,j,i,1),wct_ave(j,i,1),wct_sgm(j,i,1))
          call ave_and_sgm(ntrial,wct_raw(:,j,i,2),wct_ave(j,i,2),wct_sgm(j,i,2))
          call ave_and_sgm(ntrial,wct_raw(:,j,i,3),wct_ave(j,i,3),wct_sgm(j,i,3))
        end do
      end do

      deallocate(ad_heap,bd_heap,cd_heap)
      call display(variable_type_index,napara,nomp,num_implimentation,impname,3,num_tests,wct_ave,wct_sgm,testname)

      ! complex(real64)
      variable_type_index=4
      print *,'### complex(real64) ###'
      allocate(ac_heap(nsize),bc_heap(nsize),cc_heap(nsize))
      ac_stack = ac_static; bc_stack = bc_static; cc_stack = cc_static
      ac_heap  = ac_static; bc_heap  = bc_static; cc_heap  = cc_static
      wct_raw = 0.0d0;wct_ave=0.0d0;wct_sgm=0.0d0

      num_implimentation = num_implimentation_complex8
      num_tests = num_tests_complex8
      testname(1) = 'addition_complex8'
      tests_complex8(1)%pp_complex8 => addition_complex8
      testname(2) = 'multiplication_complex8'
      tests_complex8(2)%pp_complex8 => multiplication_complex8
      testname(3) = 'division_complex8'
      tests_complex8(3)%pp_complex8 => division_complex8

      do i = 1,num_tests
        do n=1,ntrial
          call tests_complex8(i)%pp_complex8(nloop,num_implimentation,wct_raw(n,:,i,1),nsize,ac_static,bc_static,cc_static)
          call tests_complex8(i)%pp_complex8(nloop,num_implimentation,wct_raw(n,:,i,2),nsize,ac_stack, bc_stack, cc_stack)
          call tests_complex8(i)%pp_complex8(nloop,num_implimentation,wct_raw(n,:,i,3),nsize,ac_heap,  bc_heap,  cc_heap)
        end do
        do j=1,num_implimentation
          call ave_and_sgm(ntrial,wct_raw(:,j,i,1),wct_ave(j,i,1),wct_sgm(j,i,1))
          call ave_and_sgm(ntrial,wct_raw(:,j,i,2),wct_ave(j,i,2),wct_sgm(j,i,2))
          call ave_and_sgm(ntrial,wct_raw(:,j,i,3),wct_ave(j,i,3),wct_sgm(j,i,3))
        end do
      end do

      deallocate(ac_heap,bc_heap,cc_heap)
      call display(variable_type_index,napara,nomp,num_implimentation,impname,3,num_tests,wct_ave,wct_sgm,testname)

      ! logical
      print *,'### logical ###'
      variable_type_index=5
      allocate(al_heap(nsize),bl_heap(nsize),cl_heap(nsize))
      al_stack = al_static; bl_stack = bl_static; cl_stack = cl_static
      al_heap  = al_static; bl_heap  = bl_static; cl_heap  = cl_static
      wct_raw = 0.0d0;wct_ave=0.0d0;wct_sgm=0.0d0

      num_implimentation = num_implimentation_logical
      num_tests = num_tests_logical
      testname(1) = 'disjunction_logical'
      tests_logical(1)%pp_logical => logical_disjunction
      testname(2) = 'conjunction_logical'
      tests_logical(2)%pp_logical => logical_conjunction

      do i = 1,num_tests_logical
        do n=1,ntrial
          call tests_logical(i)%pp_logical(nloop,num_implimentation,wct_raw(n,:,i,1),nsize,al_static,bl_static,cl_static)
          call tests_logical(i)%pp_logical(nloop,num_implimentation,wct_raw(n,:,i,2),nsize,al_stack, bl_stack, cl_stack)
          call tests_logical(i)%pp_logical(nloop,num_implimentation,wct_raw(n,:,i,3),nsize,al_heap,  bl_heap,  cl_heap)
        end do
        do j=1,num_implimentation
          call ave_and_sgm(ntrial,wct_raw(:,j,i,1),wct_ave(j,i,1),wct_sgm(j,i,1))
          call ave_and_sgm(ntrial,wct_raw(:,j,i,2),wct_ave(j,i,2),wct_sgm(j,i,2))
          call ave_and_sgm(ntrial,wct_raw(:,j,i,3),wct_ave(j,i,3),wct_sgm(j,i,3))
        end do
 
      end do

      deallocate(al_heap,bl_heap,cl_heap)
      call display(variable_type_index,napara,nomp,num_implimentation,impname,3,num_tests,wct_ave,wct_sgm,testname)


    end subroutine one_dimensional_tests
    !---
    subroutine display(index,napara,nomp,nimp,impname,ntype,ntest,wct_ave,wct_sgm,testname)
      integer(int32),intent(in) :: index,napara,nomp,nimp,ntype,ntest
      real(real64),intent(inout) :: wct_ave(:,:,:),wct_sgm(:,:,:)
      character(len=50),intent(in) :: testname(ntest)
      character(len=50),intent(in) :: impname(nimp)
      integer(int32) :: i,j,k
      real(real64) :: tmp
      character(len=50) :: filename

      write(filename,'(i3.3,".csv")') index
      open(11,file=filename)
      !write(11,'(6(a40,","),a40)') 'test_name','array_type','implimentation_name','num_of_threads(auto-parallelize)',&
      write(11,*) 'test_name',",",'array_type',",",'implimentation_name',",",'num_of_threads(auto-parallelize)',",",&
      &                             'num_of_threads(OpenMP)',",",'average[s]',",",'sigma[s]'
      do i=1,ntest
        ! data file
       do j=1,3
          do k=1,nimp
            !write(11,'(a40,",",i4,",",a40,",",i4,",",i4,",",E18.8e3,",",E18.8e3)') &
            write(11,*) &
            &     trim(testname(i)),",",j,",",trim(impname(k)),",",napara,",",nomp,",",wct_ave(k,i,j),",",wct_sgm(k,i,j)
          end do
        end do
        ! display
        tmp = wct_ave(1,i,1)
        wct_ave(:,i,:) = wct_ave(:,i,:)/tmp
        wct_sgm(:,i,:) = wct_sgm(:,i,:)/tmp
        print *,'# ',testname(i)
        print *,'# reference time[s]:',tmp
        write(output_unit,'(a8,5a16)') '#  ',trim(impname(1)),trim(impname(3)),&
        &                                    trim(impname(2)),trim(impname(4)),trim(impname(5))
        do j=1,ntype
          !write(output_unit,'(a6,i4,5e13.5)') 'type',j,wct(1,i,j),wct(3,i,j),wct(2,i,j),wct(4,i,j),wct(5,i,j)
          !write(output_unit,'(a6,i4,5f13.5)') 'type',j,wct(1,i,j),wct(3,i,j),wct(2,i,j),wct(4,i,j),wct(5,i,j)
          write(output_unit,'(a6,i2,5(f6.3,a3,f7.4))') 'type',j,wct_ave(1,i,j),'+-',wct_sgm(1,i,j),  &
          &                                                     wct_ave(3,i,j),'+-',wct_sgm(3,i,j),  &
          &                                                     wct_ave(2,i,j),'+-',wct_sgm(2,i,j),  &
          &                                                     wct_ave(4,i,j),'+-',wct_sgm(4,i,j),  &
          &                                                     wct_ave(5,i,j),'+-',wct_sgm(5,i,j)
        end do
        write(output_unit,*)
      end do
      close(11)
 
    end subroutine display
    !---
    subroutine initialize(nsize,ai_static,bi_static,ci_static,&
      &                         ar_static,br_static,cr_static,&
      &                         ad_static,bd_static,cd_static,&
      &                         ac_static,bc_static,cc_static,&
      &                         al_static,bl_static,cl_static)
      integer(int32),intent(in) :: nsize
      integer(int32), dimension(nsize),intent(out) :: ai_static,bi_static,ci_static
      real(real32),   dimension(nsize),intent(out) :: ar_static,br_static,cr_static
      real(real64),   dimension(nsize),intent(out) :: ad_static,bd_static,cd_static
      complex(real64),dimension(nsize),intent(out) :: ac_static,bc_static,cc_static
      logical,        dimension(nsize),intent(out) :: al_static,bl_static,cl_static
      integer(int32) :: i
 
      ! (1) type 1 (defined at main. static?)
      !call random_number(ad_static) ! 0 ~ 1.0
      !call random_number(bd_static)
      !call random_number(cd_static)
      !ad_static = 2.0*(ad_static-0.5) ! -1.0 ~ 1.0
      !bd_static = 2.0*(bd_static-0.5)
      !cd_static = 2.0*(cd_static-0.5)
      !! integer (4byte)
      !ai_static = int(ad_static*1.0d4,int32)  ! -1e4 ~ 1e4
      !bi_static = int(bd_static*1.0d4,int32)
      !ci_static = int(cd_static*1.0d4,int32)
      !! double precision
      !ad_static = ad_static*1.0d10  ! -1e10 ~ 1e10
      !bd_static = bd_static*1.0d10
      !cd_static = cd_static*1.0d10
      ad_static = 12.0d0;bd_static=6.0d0;cd_static=2.0d0
      ai_static = int(ad_static,int32)  ! -1e4 ~ 1e4
      bi_static = int(bd_static,int32)
      ci_static = int(cd_static,int32)
 
      ! real
      ar_static = real(ad_static,real32)
      br_static = real(bd_static,real32)
      cr_static = real(cd_static,real32)
      ! complex
      ac_static = cmplx(ad_static,bd_static,real64)
      bc_static = cmplx(bd_static,cd_static,real64)
      cc_static = cmplx(cd_static,ad_static,real64)
      ! logical
      al_static = .false.
      bl_static = .false.
      cl_static = .false.
      do i=1,nsize
        if(ad_static(i)>0.0d0) al_static(i) = .true.
        if(bd_static(i)>0.0d0) bl_static(i) = .true.
        if(cd_static(i)>0.0d0) cl_static(i) = .true.
      end do
    end subroutine initialize
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
end module mod_1d