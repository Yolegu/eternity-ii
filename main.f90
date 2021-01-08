module m_eternity

    implicit none

    type :: t_quart
        integer :: id
        character(10) :: couleur
        character(10) :: forme
    end type

    type :: t_piece
        integer :: id
        type(t_quart) :: up, down, left, right
        logical :: corner
        logical :: border
        logical :: nocor_nobor
        logical :: posed
        integer :: clk_cnt = 0
    contains
        procedure :: rotate_clk
    end type

    type :: t_square
        integer :: i, j
        logical :: covered
        type(t_piece) :: piece
        logical :: border
        logical :: corner
        logical :: nocor_nobor
    contains
        procedure :: pose
        procedure :: check_borders
    end type

    type(t_piece) :: piece(256)
    type(t_piece) :: piece_out
    type(t_square) :: square(18, 18)

contains

    subroutine read_pieces()

        integer :: unit
        integer :: i
        integer :: border

        open(newunit = unit, file = "pieces.txt", status = "old")

        read(unit, *)

        do i = 1, 256
            read(unit,*) piece(i)%id, piece(i)%up%id, piece(i)%right%id, &
                & piece(i)%down%id, piece(i)%left%id
            piece%posed = .false.
        end do

        piece(1:4)%corner = .true.
        piece(5:28)%border = .true.

        do i = 1, 256

            border = 0

            if (piece(i)%up%id == 23) then
                border = border + 1
            end if

            if (piece(i)%right%id == 23) then
                border = border + 1
            end if

            if (piece(i)%down%id == 23) then
                border = border + 1
            end if

            if (piece(i)%left%id == 23) then
                border = border + 1
            end if

            if (border == 1) then
                piece(i)%border = .true.
                piece(i)%corner = .false.
            elseif (border == 2) then
                piece(i)%border = .false.
                piece(i)%corner = .true.
            end if

            if (piece(i)%border .or. piece(i)%corner) then
                piece(i)%nocor_nobor = .false.
            else
                piece(i)%nocor_nobor = .true.
            end if
        end do

        close(unit)

        piece_out%up%id = 23
        piece_out%right%id = 23
        piece_out%down%id = 23
        piece_out%left%id = 23
        piece_out%id = 257

    end subroutine

    !------

    subroutine rotate_clk(self)

        class(t_piece) :: self
        type(t_piece) :: self_copy

        self_copy = self
        self%up = self_copy%left
        self%right = self_copy%up
        self%down = self_copy%right
        self%left = self_copy%down

        self%clk_cnt = self%clk_cnt + 1
        self%clk_cnt = mod(self%clk_cnt, 4)

    end subroutine

    !------

    subroutine pose(self, piece, n_border_min_opt)

        class(t_square) :: self
        type(t_piece) :: piece
        logical :: up_match, right_match, down_match, left_match
        integer :: rot
        integer, optional :: n_border_min_opt
        integer :: n_border_min
        integer :: n_border

        rot = 0
        self%piece = piece

        if (present(n_border_min_opt)) then
            n_border_min = n_border_min_opt
        else
            n_border_min = 4
        end if

        do while (.not.(self%covered) .and. rot < 4)

            call self%check_borders(up_match, right_match, down_match, left_match)

            n_border = 0
            if (up_match) n_border = n_border + 1
            if (right_match) n_border = n_border + 1
            if (down_match) n_border = n_border + 1
            if (left_match) n_border = n_border + 1

            !            write(*,*)n_border, n_border_min
            !            read(*,*)

            !            if (up_match .and. right_match .and. down_match .and. left_match) then
            if (n_border >= n_border_min) then
                self%covered = .true.
                piece%posed = .true.
                exit
            else
                self%covered = .false.
            end if

            call self%piece%rotate_clk()
            rot = rot + 1

        end do

        if (.not.(self%covered)) then
            self%piece%id = 0
        end if

    end subroutine

    !------

    subroutine fill_grid()

        integer :: i, j, k, l
        integer :: imin, imax, idir, jmin, jmax, jdir
        integer :: itemp, jtemp
        integer :: n_borders

        ! initialisation des cases
        do i = 1, 18
            do j = 1, 18
                square(i,j)%i = i
                square(i,j)%j = j
                square(i,j)%covered = .false.
            end do
        end do

        do i = 1, 256
            if (piece(i)%id == 139) call square(10, 9)%pose(piece(i))
        end do

        ! murs invisibles autour de la grille
        do i = 1, 18
            square(1,i)%piece = piece_out
            square(1,i)%covered = .true.
            square(18,i)%piece = piece_out
            square(18,i)%covered = .true.
            square(i,1)%piece = piece_out
            square(i,1)%covered = .true.
            square(i,18)%piece = piece_out
            square(i,18)%covered = .true.
        end do

        square(2,3:16)%border = .true.
        square(17,3:16)%border = .true.
        square(3:16,2)%border = .true.
        square(3:16,17)%border = .true.

        square(2,2)%corner = .true.
        square(2,17)%corner = .true.
        square(17,2)%corner = .true.
        square(17,17)%corner = .true.

        do i = 2, 17
            do j = 2, 17
                if (square(i,j)%border .or. square(i,j)%corner) then
                    square(i,j)%nocor_nobor = .false.
                else
                    square(i,j)%nocor_nobor = .true.
                end if
            end do
        end do

        !        do i = 2, 17
        !            lp_j: do j = 2, 17
        !
        !                do k = 1, 256
        !
        !                    if (square(i,j)%covered) then
        !                        cycle lp_j
        !                    end if
        !
        !                    if (piece(k)%posed) cycle
        !                    if (square(i,j)%corner .and. .not.(piece(k)%corner)) cycle
        !                    if (square(i,j)%border .and. .not.(piece(k)%border)) cycle
        !                    if (square(i,j)%nocor_nobor .and. .not.(piece(k)%nocor_nobor)) cycle
        !
        !                    call square(i,j)%pose(piece(k))
        !
        !                end do
        !            end do lp_j
        !        end do

        do l = 1, 30
            selectcase(l)
                case(1)
                    imin = 9
                    imax = 10
                    idir = -1
                    jmin = 10
                    jmax = 10
                    jdir = 1
                case(2)
                    imin = 9
                    imax = 9
                    idir = 1
                    jmin = 8
                    jmax = 9
                    jdir = -1
                case(3)
                    imin = 10
                    imax = 11
                    idir = 1
                    jmin = 8
                    jmax = 8
                    jdir = 1
                case(4)
                    imin = 11
                    imax = 11
                    idir = 1
                    jmin = 9
                    jmax = 11
                    jdir = 1
                case(5)
                    imin = 8
                    imax = 10
                    idir = -1
                    jmin = 11
                    jmax = 11
                    jdir = 1
                case(6)
                    imin = 8
                    imax = 8
                    idir = 1
                    jmin = 7
                    jmax = 10
                    jdir = -1
                case(7)
                    imin = 9
                    imax = 12
                    idir = 1
                    jmin = 7
                    jmax = 7
                    jdir = 1
                case(8)
                    imin = 12
                    imax = 12
                    idir = 1
                    jmin = 8
                    jmax = 12
                    jdir = 1
                case(9)
                    imin = 7
                    imax = 11
                    idir = -1
                    jmin = 12
                    jmax = 12
                    jdir = 1
                case(10)
                    imin = 7
                    imax = 7
                    idir = 1
                    jmin = 6
                    jmax = 11
                    jdir = -1
                case(11)
                    imin = 8
                    imax = 13
                    idir = 1
                    jmin = 6
                    jmax = 6
                    jdir = 1
                case(12)
                    imin = 13
                    imax = 13
                    idir = 1
                    jmin = 7
                    jmax = 13
                    jdir = 1
                case(13)
                    imin = 6
                    imax = 12
                    idir = -1
                    jmin = 13
                    jmax = 13
                    jdir = 1
                case(14)
                    imin = 6
                    imax = 6
                    idir = 1
                    jmin = 5
                    jmax = 12
                    jdir = -1
                case(15)
                    imin = 7
                    imax = 14
                    idir = 1
                    jmin = 5
                    jmax = 5
                    jdir = 1
                case(16)
                    imin = 14
                    imax = 14
                    idir = 1
                    jmin = 6
                    jmax = 14
                    jdir = 1
                case(17)
                    imin = 5
                    imax = 13
                    idir = -1
                    jmin = 14
                    jmax = 14
                    jdir = 1
                case(18)
                    imin = 5
                    imax = 5
                    idir = 1
                    jmin = 4
                    jmax = 13
                    jdir = -1
                case(19)
                    imin = 6
                    imax = 15
                    idir = 1
                    jmin = 4
                    jmax = 4
                    jdir = 1
                case(20)
                    imin = 15
                    imax = 15
                    idir = 1
                    jmin = 5
                    jmax = 15
                    jdir = 1
                case(21)
                    imin = 4
                    imax = 14
                    idir = -1
                    jmin = 15
                    jmax = 15
                    jdir = 1
                case(22)
                    imin = 4
                    imax = 4
                    idir = 1
                    jmin = 3
                    jmax = 14
                    jdir = -1
                case(23)
                    imin = 5
                    imax = 16
                    idir = 1
                    jmin = 3
                    jmax = 3
                    jdir = 1
                case(24)
                    imin = 16
                    imax = 16
                    idir = 1
                    jmin = 4
                    jmax = 16
                    jdir = 1
                case(25)
                    imin = 3
                    imax = 15
                    idir = -1
                    jmin = 16
                    jmax = 16
                    jdir = 1
                case(26)
                    imin = 3
                    imax = 3
                    idir = 1
                    jmin = 2
                    jmax = 15
                    jdir = -1
                case(27)
                    imin = 4
                    imax = 17
                    idir = 1
                    jmin = 2
                    jmax = 2
                    jdir = 1
                case(28)
                    imin = 17
                    imax = 17
                    idir = 1
                    jmin = 3
                    jmax = 17
                    jdir = 1
                case(29)
                    imin = 2
                    imax = 16
                    idir = -1
                    jmin = 17
                    jmax = 17
                    jdir = 1
                case(30)
                    imin = 2
                    imax = 2
                    idir = 1
                    jmin = 2
                    jmax = 16
                    jdir = -1
            end select

            if (idir == -1) then
                itemp = imin
                imin = imax
                imax = itemp
            elseif (jdir == -1) then
                jtemp = jmin
                jmin = jmax
                jmax = jtemp
            end if

            do i = imin, imax, idir
                lp_j: do j = jmin, jmax, jdir

                    do k = 1, 256

                        if (square(i,j)%covered) then
                            cycle lp_j
                        end if

                        if (piece(k)%posed) cycle
                        if (square(i,j)%corner .and. .not.(piece(k)%corner)) cycle
                        if (square(i,j)%border .and. .not.(piece(k)%border)) cycle
                        if (square(i,j)%nocor_nobor .and. .not.(piece(k)%nocor_nobor)) cycle

                        call square(i,j)%pose(piece(k))

                    end do

                    if (.not.(square(i,j)%covered)) return

                end do lp_j
            end do

        end do

        !        do n_borders = 3, 1, -1
        !
        !            do i = 3, 16
        !                lp_j2: do j = 3, 16
        !                    do k = 1, 256
        !
        !                        if (square(i,j)%covered) then
        !                            cycle lp_j2
        !                        end if
        !
        !                        if (piece(k)%posed) cycle
        !                        if (square(i,j)%corner .and. .not.(piece(k)%corner)) cycle
        !                        if (square(i,j)%border .and. .not.(piece(k)%border)) cycle
        !                        if (square(i,j)%nocor_nobor .and. .not.(piece(k)%nocor_nobor)) cycle
        !
        !                        call square(i,j)%pose(piece(k), n_borders)
        !
        !                    end do
        !                end do lp_j2
        !            end do
        !
        !        end do

    end subroutine

    !------

    function score() result(sc)

        integer :: i, j
        real(8) :: sc

        sc = 0
        do i = 2, 17
            do j = 2, 17

                if (.not.(square(i,j)%covered)) cycle

                if (square(i-1,j)%covered .and. &
                        & square(i,j)%piece%up%id == square(i-1,j)%piece%down%id .and. &
                        & square(i,j)%piece%up%id /= 23) then
                    sc = sc + 0.5d0
                end if

                if (square(i,j+1)%covered .and. &
                        & square(i,j)%piece%right%id == square(i,j+1)%piece%left%id .and. &
                        & square(i,j)%piece%right%id /= 23) then
                    sc = sc + 0.5d0
                end if

                if (square(i+1,j)%covered .and. &
                        & square(i,j)%piece%down%id == square(i+1,j)%piece%up%id .and. &
                        & square(i,j)%piece%down%id /= 23) then
                    sc = sc + 0.5d0
                end if

                if (square(i,j-1)%covered .and. &
                        & square(i,j)%piece%left%id == square(i,j-1)%piece%right%id .and. &
                        & square(i,j)%piece%left%id /= 23) then
                    sc = sc + 0.5d0
                end if

            end do
        end do

    end function

    !------

    subroutine write_grid()

        integer :: unit
        integer :: i, j, k = 0, x, ioerr, line
        character(100) :: k_str
        logical :: opened_once = .false.
        real(8) :: sc
        real(8) :: sc_max = 0.d0
        integer :: tid

        if (.not.(opened_once)) then

            open(20, file = "score.csv")

            line = 0
            read(20,*,iostat = ioerr)
            do
                read(20,*, iostat = ioerr)k, x, sc_max
                if (ioerr /= 0) then
                    if (line == 0) then
                        backspace(20)
                        write(20,*)"Grid number ; Pieces posed (over 256) ; Sides connected (over 480)"
                    end if
                    exit
                else
                    line = line + 1
                end if
            end do

            close(20)

            opened_once = .true.

        end if

        sc = score()

        sc_max = max(sc, sc_max)
        k = k + 1

        open(20, file = "score.csv", position = "append")

        write(20,"(I0, A3, I3, A3, F5.1)")k, " ; ", count(piece%posed), " ; ", sc
        close(20)

        write(k_str,"(I0)")k

        open(newunit = unit, file = "grids\"//trim(k_str)//".txt")
        do i = 2, 17
            write(unit,"(*(A1, I3, A1, I1, A1))")("(", square(i, j)%piece%id, " ", &
                & square(i,j)%piece%clk_cnt, ")", j = 2, 17)
        end do
        close(unit)

    end subroutine

    !------

    subroutine check_borders(self, up_match, right_match, down_match, left_match)

        class(t_square) :: self
        logical :: up_match, right_match, down_match, left_match
        integer :: i, j

        i = self%i
        j = self%j

        up_match = .true.
        right_match = .true.
        down_match = .true.
        left_match = .true.

        if (square(i + 1, j)%covered) then
            if (self%piece%down%id /= square(i + 1, j)%piece%up%id) then
                down_match = .false.
            end if
        end if

        if (square(i - 1, j)%covered) then
            if (self%piece%up%id /= square(i - 1, j)%piece%down%id) then
                up_match = .false.
            end if
        end if

        if (square(i, j + 1)%covered) then
            if (self%piece%right%id /= square(i, j + 1)%piece%left%id) then
                right_match = .false.
            end if
        end if

        if (square(i, j - 1)%covered) then
            if (self%piece%left%id /= square(i, j - 1)%piece%right%id) then
                left_match = .false.
            end if
        end if

    end subroutine

    !------

    subroutine init_random_seed()
        implicit none
        integer, allocatable :: seed(:)
        integer :: i, n, un, istat, dt(8), pid, t(2), s
        integer(8) :: count, tms

        call random_seed(size = n)
        allocate(seed(n))
        ! First try if the OS provides a random number generator
        open(newunit=un, file="/dev/urandom", access="stream", &
            form="unformatted", action="read", status="old", iostat=istat)
        if (istat == 0) then
            read(un) seed
            close(un)
        else
            ! Fallback to XOR:ing the current time and pid. The PID is
            ! useful in case one launches multiple instances of the same
            ! program in parallel.
            call system_clock(count)
            if (count /= 0) then
                t = transfer(count, t)
            else
                call date_and_time(values=dt)
                tms = (dt(1) - 1970) * 365_8 * 24 * 60 * 60 * 1000 &
                    + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
                    + dt(3) * 24 * 60 * 60 * 60 * 1000 &
                    + dt(5) * 60 * 60 * 1000 &
                    + dt(6) * 60 * 1000 + dt(7) * 1000 &
                    + dt(8)
                t = transfer(tms, t)
            end if
            s = ieor(t(1), t(2))
            pid = getpid() + 1099279 ! Add a prime
            s = ieor(s, pid)
            if (n >= 3) then
                seed(1) = t(1) + 36269
                seed(2) = t(2) + 72551
                seed(3) = pid
                if (n > 3) then
                    seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
                end if
            else
                seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
            end if
        end if
        call random_seed(put=seed)
    end subroutine init_random_seed

    !------

    subroutine shuffle_pieces()

        real(8) :: vec(256)
        type(t_piece) :: piece_temp
        real(8) :: temp
        integer :: bubble, lsup, j

        call init_random_seed()
        call random_number(vec)

        lsup = 256 !lsup is the size of the array to be used

        do while (lsup > 1)
            bubble = 0 !bubble in the greatest element out of order
            do j = 1, (lsup-1)
                if (vec(j) > vec(j+1)) then

                    temp = vec(j)
                    vec(j) = vec(j+1)
                    vec(j+1) = temp

                    piece_temp = piece(j)
                    piece(j) = piece(j+1)
                    piece(j+1) = piece_temp

                    bubble = j

                endif
            enddo
            lsup = bubble
        enddo

    end subroutine

    !------

    subroutine clear_grid()

        piece%posed = .false.
        square%covered = .false.

    end subroutine

end module

program main

    use m_eternity
    implicit none

    real(8) :: sc
    real(8) :: sc_max = 0.d0
    integer :: i

    call read_pieces()

    do

        call shuffle_pieces()
        call fill_grid()
        sc = score()
        if (sc > sc_max) then
            sc_max = sc
            call write_grid()
        end if

        call clear_grid()

    end do

end
