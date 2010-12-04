module Model1 
    use Frame
  use X_Misc
  implicit none
  ! モデル設定パラメータ:
  ! 実装上の注意: 内容はtenativeかつadhocである。型を立てるのは、記述上の便宜のためであって、
  ! 設計上の理由ではない。

  ! ToDo: fun city で作った町を、ファイルに書き出すことにしたらどう?
  ! そのほうが、まち構成るーるをシェアできてよい。
  !
  !
contains
  subroutine fromBase0(idx)
    integer :: idx
    idx = idx + 1
  end subroutine

  integer function roleFromString(str) result(it)
    character(256) :: str
    select case (str)
    case ("Employed"); it = Employed
    case ("Hausfrau"); it = Hausfrau
    case ("Student "); it = Student 
    end select
  end function

  integer function place_kFromString(str) result(it)
    character(256) :: str
    select case (str)
    case ("Cram"); it = PL_CRAM
    case ("Sch"); it = PL_SCH
    case ("Corp"); it = PL_CORP 
    case ("Home"); it = PL_HOME
    case ("Super"); it = PL_SUPER
    case ("Park"); it = PL_PARK 
    case ("Train"); it = PL_TRAIN
    case default; 
      write(*,*) 'parse error: unknown place_k symbol:', trim(str)
      stop
    end select
  end function

  integer function healthFromString(str) result(it)
    character(256) :: str
    select case (str)
    case ("SUS"); it = HL_SUS
    case ("EXP"); it = HL_EXP 
    case ("INF"); it = HL_INF
    case ("VAC"); it = HL_VAC
    case ("REC"); it = HL_REC 
    case default; 
      write(*,*) 'parse error: unknown place_k symbol:', trim(str)
      stop
    end select
  end function

  subroutine simpleReadCheck(city_)
    type(city) :: city_
    integer ::i, j, k
    write(*,*) "#areas  = ", size(city_%area)
    do i = 1, size(city_%area)
    write(*,*) "  #persons = ", size(city_%area(i)%person)
    do j = 1, size(city_%area(i)%place)
    write(*,*) "  #place(",j,")", size(city_%area(i)%place(j)%o)
    end do
    end do
    write(*,*) "#trains = ", size(city_%train)
  end subroutine

  subroutine readCity(city_,filename)
    type(city) :: city_
    character(*)   :: filename
    character(256) :: temp
    integer, parameter :: ist = 8932
    open(ist, file=filename)
    call readArea(city_%area)
    call readTrain(city_%train)
    call readTime(city_%time)
  contains
    subroutine check(get,rule)
      character(*) :: get, rule
      if (trim(get).ne.trim(rule)) then
        write(*,*) 'readCity: parse error: ', trim(get), ' should be ', trim(rule)
        stop
      end if
    end subroutine 
    subroutine readPlace_t(id)
      type(place_t) :: id
      read(ist,*) id%area_t, temp, id%id
      id%place_k = place_kFromString(temp)
    end subroutine 
    subroutine readHealth(h)
      type(health) :: h
      read(ist,*,err=999) temp, h%time
      h%kind = healthFromString(temp)
      return
  999 read(ist,*) temp
      write(*,*) 'parse error in readHealth: ', trim(temp)
      stop
    end subroutine 
    subroutine readPerson(p)
      integer :: i, j, n, m
      type(person), allocatable, dimension(:) :: p
      read(ist,*) temp; call check(temp,'person:')
      read(ist,*) n; allocate(p(n))
      do i = 1, n
        read(ist,*) temp; p(i)%role = roleFromString(temp)
        select case (p(i)%role)
        case (Employed); p(i)%mkSched => schedEmp
        case (Hausfrau); p(i)%mkSched => schedHaus
        case (Student) ; p(i)%mkSched => schedStu
        end select
        read(ist,*) temp; call check(temp,'belong:')
        read(ist,*) m; allocate(p(i)%belong(m))
        do j = 1, m
          call readPlace_t(p(i)%belong(j))
        end do
        read(ist,*) temp; call check(temp,'health:')
        call readHealth(p(i)%health(1))
      end do
    end subroutine
    subroutine readPlace(pl)
      type(place) :: pl
      call readPlace_t(pl%id)
      read(ist,*) temp; call check(temp,'betaN:')
      read(ist,*) pl%betaN
    end subroutine
    subroutine readPlaceGroup(pl)
      type(place_vector), allocatable, dimension(:) :: pl
      integer :: i, n, idx, j, m
      read(ist,*) temp; call check(temp,'place-group:')
      read(ist,*) n; allocate(pl(n))
      do i = 1, n
        read(ist,*) temp; idx = place_kFromString(temp)
        read(ist,*) m; allocate(pl(i)%o(m))
        do j = 1, m
          call readPlace(pl(i)%o(j))
        end do
      end do
    end subroutine
    subroutine readArea(area_)
      type(area), allocatable, dimension(:) :: area_
      integer :: i, n
      read(ist,*) temp; call check(temp,'area:')
      read(ist,*) n; allocate(area_(n))
      do i = 1, n
        read(ist,*,err=999) area_(i)%id; call fromBase0(area_(i)%id)
        call readPerson(area_(i)%person)
        call readPlaceGroup(area_(i)%place)
      end do
      return
  999 read(ist,*) temp
      write(*,*) temp
      stop
    end subroutine
    subroutine readTime(t)
      integer :: t
      read(ist,*) temp; call check(temp,'time:')
      read(ist,*) t
    end subroutine
    subroutine readMobSched(sch)
      type(mob_sched), allocatable, dimension(:) :: sch
      integer :: i, n
      read(ist,*) n; allocate(sch(n))
      read(ist,*) temp; call check(temp,'sked:')
      do i = 1, n
        read(ist,*) sch(i)%time, sch(i)%area_t
      end do
    end subroutine
    subroutine readTrain(tr)
      type(mobil), allocatable, dimension(:) :: tr 
      integer :: i, n
      read(ist,*) temp; call check(temp,'train:')
      read(ist,*) n; allocate(tr(n))
      do i = 1, n
        call readPlace(tr(i)%pl)
        tr(i)%iSked = 0
        call readMobSched(tr(i)%sked)
      end do
    end subroutine
  end subroutine

  character(16) function showPlace_k(k) result (it)
    integer :: k
    select case (k)
    case (PL_CRAM ); it = "PL_CRAM"
    case (PL_SCH  ); it = "PL_SCH"
    case (PL_CORP ); it = "PL_CORP"
    case (PL_HOME ); it = "PL_HOME"
    case (PL_SUPER); it = "PL_SUPER"
    case (PL_PARK ); it = "PL_PARK"
    case (PL_TRAIN); it = "PL_TRAIN"
    end select
  end function
  subroutine writeSched(ost, sch)
    integer :: ost, i
    type(sched)   :: sch
    do i = 1, sch%n
      write(ost,*) sch%time(i), &
        sch%place_t(i)%area_t, &
        trim(showPlace_k(sch%place_t(i)%place_k)),&
        sch%place_t(i)%id
    end do
  end subroutine

  type(place_t) function findBelong(xs,kind,doesMatch) result (it)
    type(place_t) :: xs(:)
    integer :: i, kind
    logical, optional :: doesMatch
    do i = 1, size(xs) 
      if (xs(i)%place_k .eq. kind) then
        it = xs(i)
        if (present(doesMatch)) doesMatch = .true.
        return
      end if
    end do
    if (present(doesMatch)) doesMatch = .false.
  end function

  integer(2) function int_comp(i,j)
    integer :: i, j
    int_comp = i - j
  end function



  subroutine schedEmp(p, t, sch)
    class(person) :: p ! implicitly passed pointer to object
    integer       :: t, isize, today, i
    type(sched)   :: sch
    type(tmcomp) :: tm

    tm    = timecomp(t)
    today = tm%day * idays

    if (tm%step .eq. 0) then
      if (1 .le. tm%weekday .and. tm%weekday .le. 5) then
        sch%n = 2
        sch%time   (1) = today + irndIn ( 6*ihours, 10*ihours)
        sch%place_t(1) = findBelong(p%belong, PL_CORP)
        sch%time   (2) = today + irndIn (18*ihours, 22*ihours)
        sch%place_t(2) = findBelong(p%belong, PL_HOME)
      else
        sch%n = irndIn (1, size(p%belong))
        do i = 1, sch%n
          sch%time(i) = today + irndIn (10*ihours, 18*ihours)
        end do
        isize = LOC(sch%time(2)) - LOC(sch%time(1))
        call qsort(sch%time(i), sch%n, isize, int_comp)
        do i = 1, sch%n
          sch%place_t(i) = p%belong(irndIn (1, sch%n))
        end do
        sch%n = sch%n + 1
        sch%time   (sch%n) = today + 18*ihours
        sch%place_t(sch%n) = findBelong(p%belong, PL_HOME)
      end if
    end if
  end subroutine 

  subroutine schedStu(p, t, sch)
    class(person) :: p ! implicitly passed pointer to object
    integer       :: t, isize, today, i
    type(sched)   :: sch
    logical       :: exists
    type(tmcomp)  :: tm
    type(place_t) :: ignore

    tm    = timecomp(t)
    today = tm%day * idays

    if (tm%step .eq. 0) then
      if (1 .le. tm%weekday .and. tm%weekday .le. 5) then
        sch%n = 2
        sch%time   (1) = today + irndIn ( 6*ihours, 8*ihours)
        sch%place_t(1) = findBelong(p%belong, PL_CORP)
        sch%time   (2) = today + irndIn (16*ihours,19*ihours)
        sch%place_t(2) = findBelong(p%belong, PL_HOME)
        ignore = findBelong(p%belong, PL_CRAM, exists)
        if (exists) then
          sch%n = sch%n + 2
          sch%time   (3) = today + irndIn (19*ihours, 20*ihours)
          sch%place_t(3) = findBelong(p%belong, PL_CORP)
          sch%time   (4) = today + irndIn (21*ihours, 22*ihours)
          sch%place_t(4) = findBelong(p%belong, PL_HOME)
        end if
      else
        sch%n = irndIn (1, size(p%belong))
        do i = 1, sch%n
          sch%time(i) = today + irndIn (10*ihours, 18*ihours)
        end do
        isize = LOC(sch%time(2)) - LOC(sch%time(1))
        call qsort(sch%time(i), sch%n, isize, int_comp)
        do i = 1, sch%n
          sch%place_t(i) = p%belong(irndIn (1, sch%n))
        end do
        sch%n = sch%n + 1
        sch%time   (sch%n) = today + 18*ihours
        sch%place_t(sch%n) = findBelong(p%belong, PL_HOME)
      end if
    end if
  end subroutine 

  integer(2) function sched2_comp(x, y) result (cmp)
    type sched2
      integer :: tIn, tOut
      type(place_t) :: place_t
    end type
    type (sched2) :: x, y
    if (x%tIn .lt. y%tIn) then
      cmp = -1; return
    else if (x%tIn .gt. y%tIn) then
      cmp =  1; return
    else
      cmp = 0
    end if
  end function

  subroutine schedHaus(p, t, sch)
    class(person) :: p ! implicitly passed pointer to object
    integer       :: t, isize, today, i, j, idx(size(p%belong)), kind, nOthers, nO
    type(sched)   :: sch
    logical       :: exists
    type(tmcomp)  :: tm
    type(place_t) :: ignore, home
    integer :: tInSuper, tOutSuper
    
    type sched2
      integer :: tIn, tOut
      type(place_t) :: place_t
    end type
    type (sched2), allocatable :: schedO(:)
    type (sched2) :: schedS

    tm    = timecomp(t)
    today = tm%day * idays

    if (tm%step .eq. 0) then
      ! (0) スーパー、家、以外の場所を数え上げる。
      j = 0
      do i = 1, size(p%belong) 
        kind = p%belong(i)%place_k
        if (kind .ne. PL_SUPER .and. kind .ne. PL_HOME) then
          j = j + 1
          idx(j) = i
        end if
      end do
      nOthers = j

      ! 注意: nO in [0, nOthers + 1) = [0,nOthers]
      nO = irndIn (0, nOthers + 1) 
      allocate(schedO(nO + 1))      ! スーパー + その他

      !(* 1. 一日のどこかの時点でたかだか1時間の買い物をする *)
      schedO(1)%tIn     = today         + irndIn (10*ihours  , 20*ihours)
      schedO(1)%tOut    = schedO(1)%tIn + irndIn (10*iminutes,  1*ihours)
      schedO(1)%place_t = findBelong(p%belong, PL_SUPER)
      ! 注: 本当はランダムにスーパーを選ぶ

      !(* 2. 別の場所への移動計画 *)
      do i = 2, nO + 1
        schedO(i)%tIn     = today         + irndIn (10*ihours, 20*ihours)
        schedO(i)%tOut    = schedO(i)%tIn + irndIn ( 0*ihours,  2*ihours)
        schedO(i)%place_t = p%belong(idx(irndIn (1,nOthers+1)))
      end do

      write(*,*) 'sizeof = ', sizeof_sched2()
      write(*,*) 'nItems = ', size(schedO)
      call qsort(schedO, size(schedO), sizeof_sched2(), sched2_comp)

      !(* 3. 場所毎の滞在時刻の間隔から旅程を連結する *)
      home = findBelong(p%belong, PL_HOME)
      sch%n = 0
      call connect (schedO, sch%n) !build outcome of this proc.
    end if
  contains
    integer function sizeof_sched2() result (it)
      type(sched2) :: x(2)
      it = LOC(x(2)) - LOC(x(1))
    end function

    recursive subroutine connect(schedO, n)  
      type(sched2) :: schedO(:)
      integer      :: n
      if (size(schedO) .ge. 2) then
        if (schedO(1)%tIn + 2*ihours .lt. schedO(2)%tIn) then !   tb < tb' 
          n = n + 1
          sch%time(n)    = schedO(1)%tIn
          sch%place_t(n) = schedO(1)%place_t
          n = n + 1
          sch%time(n)    = schedO(1)%tOut
          sch%place_t(n) = home
          call connect(schedO(2:),n)
        else 
          n = n + 1
          sch%time(n)    = schedO(1)%tIn
          sch%place_t(n) = schedO(1)%place_t
          call connect(schedO(2:),n)
        end if
      else if (size(schedO) .eq. 1) then
          n = n + 1
          sch%time(n)    = schedO(1)%tIn
          sch%place_t(n) = schedO(1)%place_t
          n = n + 1
          sch%time(n)    = schedO(1)%tOut
          sch%place_t(n) = home
      else
        continue ! unreachable
      end if
    end subroutine
  end subroutine

  subroutine run1 (recstep, tStop, dir, tag, city_)
    use Record
    integer :: recstep, tStop, nstep, i, j
    character(*) :: dir, tag
    type(city) :: city_
    integer, parameter :: os = 2333
    type(nVis) :: pop(size(city_%area))
    !とりあえず、無視
    !val () = writeConf conf (dir^"/conf_"^ tag^".csv")
    open(os, file=dir//"/pop_"//tag//".csv")
    call showPopTag(os, 5)
    nstep   = tStop / recstep
    do i = 1, nstep
      do j = 1, recstep
        call advanceTime(city_)
      end do
      pop = reducePop(city_)
      call showPop(os, city_%time, pop)
    end do
  end subroutine
end module
