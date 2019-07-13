program main
    implicit none
    integer(8)::N, i, j, l, AllocateStatus
    real(8)::Lx, Ly, Lz
    ! posicion y velocidad en 3D
    real(8), dimension(:), allocatable :: Rx, Ry, Rz
    real(8), dimension(:), allocatable :: Vx, Vy, Vz
    ! masa
    real(8), dimension(:), allocatable :: m
    ! Energia, cinetica y potencial, distancia entre la posicion i,j, potencial LJ
    real(8):: K, U, H, r_ij, u_lj

    ! Leer datos y almacenar en variables
    open(1, file = "posvel0.dat")
        ! Leer N
        read(1,*) N
        allocate ( Rx(N), Ry(N), Rz(N), Vx(N), Vy(N), Vz(N), STAT = AllocateStatus)
        if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
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

    ! Asignar masas
    allocate(m(N), STAT = AllocateStatus)
    if (AllocateStatus /= 0) STOP "*** Not enough memory ***"
    do 1 i=1, N
        m(i) = 1.0d0
    1 end do

    ! Calcular Energia Cinetica
    K = 0.0d0
    do 2 i=1, N
        K = K + (1.0d0/2)*m(i)*(Vx(i)**2+Vy(i)**2+Vz(i)**2)
    2 end do
    print *, 'Energia Cinetica', K

    ! Calcular Energia Potencial
    U = 0.0d0
    do 3 i=1,N-1
        do 4 l=i+1,N
            r_ij = sqrt((Rx(i)-Rx(j))**2 + (Ry(i)-Ry(j))**2 + (Rz(i)-Rz(j))**2)
            u_lj = 4.0d0*((1.0d0/r_ij)**12-(1.0d0/r_ij)**6)
            U = U + u_lj
        4 end do
    3 end do
    print *, 'Energia Potencial', U

    ! Escribir Energia total
    H = K + U
    print *, 'Energia total', H
end program main