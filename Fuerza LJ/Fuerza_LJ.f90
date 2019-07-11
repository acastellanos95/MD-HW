program main
implicit none
real(8)::r0,r,sig
real(8)::eps,rinv,r7
real(8)::r13,Flj,dr
integer(8)::i,np

! declaraciones previas y asignaciones
r0 = 0.01d0
r = r0

!variables of sigma and epsilon
sig = 1
eps = 1 

! argon
! sig = 3.405e-10_8
! eps = 1.6537e-21_8

! pedir dr
write(*,*) 'dar el incremento dr'
read(*,*) dr
write(*,*)'dar el numero de puntos'
read(*,*) np

! calculo de la fuerza y escritura de datos
do i=1,np
rinv = 1.0d0/r
r7 = (6*sig**6)*(rinv**7)
r13 = -(12*sig**12)*(rinv**13)
Flj=-4.0d0*eps*(r13 + r7)
write(20,*)r,Flj
r = r + dr
enddo

stop
end