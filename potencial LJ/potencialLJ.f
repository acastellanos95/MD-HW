      program potencial
      implicit none
c     implicit double precision(a-h,o-z)
      integer i,np
      real*8 r0,r,dr
      real*8 rinv,r6,r12,ulj

      r0 = 0.01d0

      write(6,*)'dar el incremento dr'
      read(5,*) dr
c      dr = 0.05d0

      r = r0

      write(6,*)'dar el numero de puntos'
      read(5,*) np

c      np = 100

      do i=1,np
        rinv = 1.0d0/r
        r6 = rinv**6
        r12 = r6**2
        ulj=4.0d0*(r12 - r6)
        write(20,*)r,ulj
        r = r + dr
      enddo

      stop
      end
