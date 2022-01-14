module mod_tests_riemann_flux_roe
  use,intrinsic :: iso_fortran_env
  use mod_wall_clock_time, only : wall_clock_time
  implicit none
  real(real64),parameter :: gmma = 1.4d0
  real(real64),parameter :: gmm1 = gmma-1.0d0
  real(real64),parameter :: gm1i = 1.0d0/gmm1
  private
  public tests_riemann_flux_roe
  contains
    !---
    subroutine tests_riemann_flux_roe(nsize,nloop,nimp,wct,primitives_2d,roeflux_2d)
      integer(int32),intent(in) :: nsize,nloop,nimp
      real(real64),dimension(nimp),intent(out) :: wct
      real(real64),dimension(4,nsize,nsize),intent(in) :: primitives_2d
      real(real64),dimension(4,nsize,nsize),intent(out) :: roeflux_2d
      integer(int32) :: i,j,k,l
      real(real64) :: t1,t2
      real(real64),dimension(5) :: pl,pr,roeflux_3d

      ! implementation 1 "sequential do"
      i = 1
      call wall_clock_time(t1)
      do l=1,nloop
        ! flux (x direction)
        do k=2,nsize-1
          do j=1,nsize-1
            ! left
            pl(1:3) = primitives_2d(1:3,j,k)
            pl(4) = 0.0d0
            pl(5) = primitives_2d(4,j,k)
            ! right
            pr(1:3) = primitives_2d(1:3,j+1,k)
            pr(4) = 0.0d0
            pr(5) = primitives_2d(4,j+1,k)

            call flux_roe(pl,pr,roeflux_3d)

            roeflux_2d(1:3,j,k) = roeflux_3d(1:3)
            roeflux_2d(  4,j,k) = roeflux_3d(5)
          end do
        end do
        ! flux (y direction)
        do j=2,nsize-1
          do k=1,nsize-1
            ! left
            pl(1) = primitives_2d(1,j,k)
            pl(2) = primitives_2d(3,j,k)
            pl(3) = primitives_2d(2,j,k)
            pl(4) = 0.0d0
            pl(5) = primitives_2d(4,j,k)
            ! right
            pr(1) = primitives_2d(1,j,k+1)
            pr(2) = primitives_2d(3,j,k+1)
            pr(3) = primitives_2d(2,j,k+1)
            pr(4) = 0.0d0
            pr(5) = primitives_2d(4,j,k+1)

            call flux_roe(pl,pr,roeflux_3d)

            roeflux_2d(1,j,k) = roeflux_3d(1)
            roeflux_2d(2,j,k) = roeflux_3d(3)
            roeflux_2d(3,j,k) = roeflux_3d(2)
            roeflux_2d(4,j,k) = roeflux_3d(5)
          end do
        end do
      end do
      call wall_clock_time(t2)
      wct(i) = t2-t1

      ! implementation 2 "OpenMP do"
      i = 2
      call wall_clock_time(t1)
      !!$omp parallel
      do l=1,nloop
        !$omp parallel do private(j,k,pl,pr,roeflux_3d) &
        !$omp             shared(nsize,primitives_2d,roeflux_2d)
        do k=2,nsize-1
          do j=1,nsize-1
            ! left
            pl(1:3) = primitives_2d(1:3,j,k)
            pl(4) = 0.0d0
            pl(5) = primitives_2d(4,j,k)
            ! right
            pr(1:3) = primitives_2d(1:3,j+1,k)
            pr(4) = 0.0d0
            pr(5) = primitives_2d(4,j+1,k)

            call flux_roe(pl,pr,roeflux_3d)

            roeflux_2d(1:3,j,k) = roeflux_3d(1:3)
            roeflux_2d(  4,j,k) = roeflux_3d(5)
          end do
        end do
        !$omp end parallel do
        !$omp parallel do private(j,k,pl,pr,roeflux_3d) &
        !$omp             shared(nsize,primitives_2d,roeflux_2d)
        do j=2,nsize-1
          do k=1,nsize-1
            ! left
            pl(1) = primitives_2d(1,j,k)
            pl(2) = primitives_2d(3,j,k)
            pl(3) = primitives_2d(2,j,k)
            pl(4) = 0.0d0
            pl(5) = primitives_2d(4,j,k)
            ! right
            pr(1) = primitives_2d(1,j,k+1)
            pr(2) = primitives_2d(3,j,k+1)
            pr(3) = primitives_2d(2,j,k+1)
            pr(4) = 0.0d0
            pr(5) = primitives_2d(4,j,k+1)

            call flux_roe(pl,pr,roeflux_3d)

            roeflux_2d(1,j,k) = roeflux_3d(1)
            roeflux_2d(2,j,k) = roeflux_3d(3)
            roeflux_2d(3,j,k) = roeflux_3d(2)
            roeflux_2d(4,j,k) = roeflux_3d(5)
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
        do concurrent(k=2:nsize-1,j=1:nsize-1)
          ! left
          pl(1:3) = primitives_2d(1:3,j,k)
          pl(4) = 0.0d0
          pl(5) = primitives_2d(4,j,k)
          ! right
          pr(1:3) = primitives_2d(1:3,j+1,k)
          pr(4) = 0.0d0
          pr(5) = primitives_2d(4,j+1,k)

          call flux_roe(pl,pr,roeflux_3d)

          roeflux_2d(1:3,j,k) = roeflux_3d(1:3)
          roeflux_2d(  4,j,k) = roeflux_3d(5)
        end do
        do concurrent(j=2:nsize-1,k=1:nsize-1)
          ! left
          pl(1) = primitives_2d(1,j,k)
          pl(2) = primitives_2d(3,j,k)
          pl(3) = primitives_2d(2,j,k)
          pl(4) = 0.0d0
          pl(5) = primitives_2d(4,j,k)
          ! right
          pr(1) = primitives_2d(1,j,k+1)
          pr(2) = primitives_2d(3,j,k+1)
          pr(3) = primitives_2d(2,j,k+1)
          pr(4) = 0.0d0
          pr(5) = primitives_2d(4,j,k+1)

          call flux_roe(pl,pr,roeflux_3d)

          roeflux_2d(1,j,k) = roeflux_3d(1)
          roeflux_2d(2,j,k) = roeflux_3d(3)
          roeflux_2d(3,j,k) = roeflux_3d(2)
          roeflux_2d(4,j,k) = roeflux_3d(5)
        end do
      end do
      call wall_clock_time(t2)
      wct(i) = t2-t1

       ! implementation 4 "do concurrent (double loop)"
      i = 4
      call wall_clock_time(t1)
      do l=1,nloop
        do concurrent(k=2:nsize-1)
          do concurrent(j=1:nsize-1)
            ! left
            pl(1:3) = primitives_2d(1:3,j,k)
            pl(4) = 0.0d0
            pl(5) = primitives_2d(4,j,k)
            ! right
            pr(1:3) = primitives_2d(1:3,j+1,k)
            pr(4) = 0.0d0
            pr(5) = primitives_2d(4,j+1,k)

            call flux_roe(pl,pr,roeflux_3d)

            roeflux_2d(1:3,j,k) = roeflux_3d(1:3)
            roeflux_2d(  4,j,k) = roeflux_3d(5)
          end do
        end do
        do concurrent(j=2:nsize-1)
          do concurrent(k=1:nsize-1)
            ! left
            pl(1) = primitives_2d(1,j,k)
            pl(2) = primitives_2d(3,j,k)
            pl(3) = primitives_2d(2,j,k)
            pl(4) = 0.0d0
            pl(5) = primitives_2d(4,j,k)
            ! right
            pr(1) = primitives_2d(1,j,k+1)
            pr(2) = primitives_2d(3,j,k+1)
            pr(3) = primitives_2d(2,j,k+1)
            pr(4) = 0.0d0
            pr(5) = primitives_2d(4,j,k+1)

            call flux_roe(pl,pr,roeflux_3d)

            roeflux_2d(1,j,k) = roeflux_3d(1)
            roeflux_2d(2,j,k) = roeflux_3d(3)
            roeflux_2d(3,j,k) = roeflux_3d(2)
            roeflux_2d(4,j,k) = roeflux_3d(5)
          end do
        end do
      end do
      call wall_clock_time(t2)
      wct(i) = t2-t1

    end subroutine tests_riemann_flux_roe
    !---
    pure subroutine roe_average(pl,pr,rave)
      real(real64),dimension(5),intent(in) :: pl,pr
      real(real64),dimension(5),intent(out) :: rave
      real(real64) :: sqrtrl,sqrtrr,deno,e,hl,hr,v

      ! left state
      sqrtrl = sqrt(pl(1))
      e = 0.5d0*pl(1)*(pl(2)**2+pl(3)**2+pl(4)**2)+gm1i*pl(5)
      hl = (e+pl(5))/pl(1)
      ! right state
      sqrtrr = sqrt(pr(1))
      e = 0.5d0*pr(1)*(pr(2)**2+pr(3)**2+pr(4)**2)+gm1i*pr(5)
      hr = (e+pr(5))/pr(1)
      ! roe average
      deno = 1.0d0/(sqrtrl+sqrtrr)
      rave(1:3) = (sqrtrl*pl(2:4)+sqrtrr*pr(2:4))*deno  ! velocity
      rave(4)   = (sqrtrl*hl     +sqrtrr*hr     )*deno  ! entropy
      v = 0.5d0*(rave(1)**2+rave(2)**2+rave(3)**2)
      rave(5) = sqrt(gmm1*(rave(4)-v))            ! speed of sound

    end subroutine roe_average
    !---
    pure subroutine flux_roe(pl,pr,f)
      real(real64),dimension(5),intent(in) :: pl,pr
      real(real64),dimension(5),intent(out) :: f
      real(real64),dimension(5) :: fl,fr,ql,qr,dq
      real(real64),dimension(5) :: rave,lmd,k1,k2,k3,k4,k5,alpha

      call conservatives(pl,ql)
      call conservatives(pr,qr)
      dq(1:5) = ql(1:5)-ql(1:5)
      call roe_average(pl,pr,rave)
      call eigenvalues(rave(1),rave(4),lmd)
      call right_eigenvectors(rave,k1,k2,k3,k4,k5)
      call wave_strength(rave,dq,alpha)
      call flux(pl,fl)
      call flux(pr,fr)
      f(1:5) = 0.5d0*(fl(1:5)+fr(1:5))-0.5d0*( alpha(1)*abs(lmd(1))*k1(1:5)  &
      &                                       +alpha(2)*abs(lmd(2))*k2(1:5)  &
      &                                       +alpha(3)*abs(lmd(3))*k3(1:5)  &
      &                                       +alpha(4)*abs(lmd(4))*k4(1:5)  &
      &                                       +alpha(5)*abs(lmd(5))*k5(1:5) )

    end subroutine flux_roe
    !---
    pure subroutine eigenvalues(u,a,lmd)
      real(real64),intent(in) :: u,a
      real(real64),dimension(5),intent(out) :: lmd

      lmd(1) = u-a
      lmd(2) = u
      lmd(3) = u
      lmd(4) = u
      lmd(5) = u+a

    end subroutine eigenvalues
    !---
    pure subroutine right_eigenvectors(rave,k1,k2,k3,k4,k5)
      real(real64),dimension(5),intent(in) :: rave
      real(real64),dimension(5),intent(out) :: k1,k2,k3,k4,k5

      k1(1) = 1.0d0
      k1(2) = rave(1)-rave(5)
      k1(3) = rave(2)
      k1(4) = rave(3)
      k1(5) = rave(4)-rave(1)*rave(5)

      k2(1) = 1.0d0
      k2(2) = rave(1)
      k2(3) = rave(2)
      k2(4) = rave(3)
      k2(5) = 0.5d0*(rave(1)**2+rave(2)**2+rave(3)**2)

      k3(1) = 0.0d0
      k3(2) = 0.0d0
      k3(3) = 1.0d0
      k3(4) = 0.0d0
      k3(5) = rave(2)

      k4(1) = 0.0d0
      k4(2) = 0.0d0
      k4(3) = 0.0d0
      k4(4) = 1.0d0
      k4(5) = rave(3)

      k5(1) = 1.0d0
      k5(2) = rave(1)+rave(5)
      k5(3) = rave(2)
      k5(4) = rave(3)
      k5(5) = rave(4)+rave(1)*rave(5)

    end subroutine right_eigenvectors
    !---
    pure subroutine flux(p,f)
      real(real64),dimension(5),intent(in) :: p
      real(real64),dimension(5),intent(out) :: f
      real(real64) :: e

      e = 0.5d0*p(1)*(p(2)**2+p(3)**2+p(4)**2)+gm1i*p(5)
      f(1) = p(1)*p(2)
      f(2) = f(1)*p(2)+p(5)
      f(3) = f(1)*p(3)
      f(4) = f(1)*p(4)
      f(5) = (e+p(5))*p(2)

    end subroutine flux
    !---
    pure subroutine conservatives(p,q)
      real(real64),dimension(5),intent(in) :: p
      real(real64),dimension(5),intent(out) :: q

      q(1) = p(1)
      q(2:4) = p(1)*p(2:4)
      q(5) = 0.5d0*p(1)*(p(2)**2+p(3)**2+p(4)**2)+gm1i*p(5)

    end subroutine conservatives
    !---
    pure subroutine wave_strength(rave,dq,alpha)
      real(real64),dimension(5),intent(in) :: rave,dq
      real(real64),dimension(5),intent(out) :: alpha
      real(real64) :: dq5bar,ainv

      alpha(3) = dq(3)-rave(2)*dq(1)
      alpha(4) = dq(4)-rave(3)*dq(1)

      dq5bar = dq(5)-(dq(3)-rave(2)*dq(1))*rave(2)&
      &             -(dq(4)-rave(3)*dq(1))*rave(3)
      ainv = 1.0d0/rave(5)
      alpha(2) = gmm1*ainv**2*(dq(1)*(rave(4)-rave(1)**2)+rave(1)*dq(2)-dq5bar)
      alpha(1) = 0.5d0*ainv*(dq(1)*(rave(1)+rave(5))-dq(2)-rave(5)*alpha(2))
      alpha(5) = dq(1)-alpha(1)-alpha(2)

    end subroutine wave_strength
    !---
end module mod_tests_riemann_flux_roe