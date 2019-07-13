program main
    implicit none
    integer(8)::N,i,j,AllocateStatus
    real(8)::Lx,Ly,Lz
    ! posicion y velocidad en 3D
    real(8), dimension(:), allocatable :: Rx, Ry, Rz
    real(8), dimension(:), allocatable :: Vx, Vy, Vz

    
    ! Leer datos y almacenar en variables
    open(1, file = "posvel0.dat")
        ! Leer N
        read(1,*) N
        ALLOCATE ( Rx(N), Ry(N), Rz(N), Vx(N), Vy(N), Vz(N), STAT = AllocateStatus)
        IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
        ! imprimir N
        print *, 'N', N
        ! Leer  dimension de la caja de simulacion
        read(1,*) Lx, Ly, Lz
        ! imprimir la dimension
        print *, 'X Y Z', Lx, Ly, Lz
        ! Iterar en posiciones y velocidades para asignaciones
        do i=1,N
            read(1,*) j, Rx(i), Ry(i), Rz(i), Vx(i), Vy(i), Vz(i)
            print *, j, 'pos', Rx(i), Ry(i), Rz(i), 'vel', Vx(i), Vy(i), Vz(i)
        end do
    close(1)

    !Calculo de la energia cinetica 
    
end program main