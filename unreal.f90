module X_Misc
end module

module Frame
!  use X_Misc
  implicit none
  integer, parameter :: alpha = 1d0/3.5d0
  integer, parameter :: gamma = 1d0/3.0d0

  real(8), parameter :: dt = 1.0/1440.0 
  integer, parameter :: steps_per_day = nint(1.0/dt)

  ! 時間の単位 (in steps)
  real(8), parameter :: days    = 1.0/dt
  real(8), parameter :: hours   = 1.0/24.0/dt
  real(8), parameter :: minutes = 1.0/24.0/60.0/dt
  
  integer, parameter :: idays   = nint(days)
  integer, parameter :: ihours  = nint(hours)
  integer, parameter :: iminutes= nint(minutes)

  type health
    integer :: kind
    integer :: time
  end type
  ! health%kind型定数
  integer, parameter :: HL_NA  = 0 !未使用配列要素
  integer, parameter :: HL_SUS = 1
  integer, parameter :: HL_EXP = 2
  integer, parameter :: HL_INF = 3
  integer, parameter :: HL_VAC = 4
  integer, parameter :: HL_REC = 5 
  integer, parameter :: HL_MAX = HL_REC

  ! place_k型定数
  integer, parameter :: PL_MIN   = 1
  integer, parameter :: PL_CRAM  = 1
  integer, parameter :: PL_SCH   = 2
  integer, parameter :: PL_CORP  = 3
  integer, parameter :: PL_HOME  = 4
  integer, parameter :: PL_SUPER = 5
  integer, parameter :: PL_PARK  = 6
  integer, parameter :: PL_TRAIN = 7
  integer, parameter :: PL_MAX   = PL_TRAIN

  type place_t
    integer :: place_k, area_t, id
    integer :: tVis !使わないかもしれない
  end type

  integer, parameter :: NONE = 1
  integer, parameter :: SOME = 1
  type place_t_option
    integer       :: kind
    type(place_t) :: o
  end type

  type nVis 
    integer :: s, e, i, r, v
  end type
 
  type pTrns 
    real :: s2e, e2i, i2r
  end type

  type place
    type(place_t) :: id
    type (nVis)   :: nVis
    integer       :: size
    real(8)       :: betaN
    type (pTrns)  :: pTrns
  end type

  type mob_sched
    integer :: time, area_t
  end type

  integer, parameter :: SCHED_MAX = 10
  type sched
    integer :: n
    integer, dimension(SCHED_MAX) :: time
    type(place_t), dimension(SCHED_MAX) :: place_t
  end type

  type mobil 
    type(place)     :: pl
    integer         :: iSked
    type(mob_sched), allocatable, dimension(:) :: sked
  end type

  type person
    integer :: age, gender, role
    type(place_t), allocatable, dimension(:) :: place_t
    type(place_t) :: visit
    type(place_t_option) :: dest
    ! 注. MLでは place_t option型
    !   SOME a => visitのどれかの要素へのポインタ
    !   NONE   => NULLポインタ (nullify(dest)でセットする)
    type(health), dimension(HL_MAX) :: health
    integer :: nHealth
    type(sched) :: sched
    procedure(scheduler_t), pointer :: mkSched => NULL()
  end type

  type place_vector
    type(place), allocatable, dimension(:) :: o
  end type

  type area
    integer :: id
    type(person), allocatable, dimension(:) :: person
    type(place_vector), allocatable, dimension(:) :: place
  end type

  type city
    type(area) , allocatable, dimension(:) :: area
    type(mobil), allocatable, dimension(:) :: train
    integer                                :: time
  end type

  type tmcomp
    integer :: day
    integer :: weekday
    real(8) :: hour
    integer :: step  
  end type
contains
  type(tmcomp) function timecomp (t)
    integer :: t
    timecomp%step    = modulo(t, steps_per_day)
    timecomp%day     = t / steps_per_day
    timecomp%weekday = modulo(timecomp%day, 7)  ! Sun:0 & Sat:6
    timecomp%hour    = 24.0*dt*real(timecomp%step, kind=8)
  end function

  ! スケジューラ手続きの型:
  ! 【実装上の注意】通常の手続きの形をしているが、実質は、フィールド 
  !  person%mkSched の型を記述するためのものである。
  ! 入力: 
  !   person_: 対象の人の人間
  !   time   : 現在時刻
  ! 出力: 
  !   person%sched: スケジュール
  subroutine scheduler_t(person_, time)
    class(person) :: person_ ! type(person)ではNG
    integer       :: time
  end subroutine

  integer function catchTrain(trains, now, src, dst) result (i)
    type(mobil) :: trains(:)
    integer     :: now, src, dst
    integer     :: step
    step = modulo(now, steps_per_day)
    do i = 1, size(trains)
      if (exists(matchSrc, trains(i)%sked) .and. &
          exists(matchDst, trains(i)%sked)) return
    end do
    i = 0
    return
  contains
    logical function matchSrc(sked) result (f)
      type(mob_sched) :: sked
      f = sked%time .eq. step .and. sked%area_t .eq. src
    end function
    logical function matchDst(sked) result (f)
      type(mob_sched) :: sked
      f = sked%time .ge. step .and. sked%area_t .eq. dst
    end function
    logical recursive function exists(f,sked) result (ans)
      type(mob_sched) :: sked(:)
      logical, external :: f
      ans = f(sked(1)) 
      if (size(sked).eq.1) return
      ans = ans .or. exists(f,sked(2:))
    end function
  end function

  subroutine updateHealth(city_,p)
    type(city), target, intent(inout) :: city_
    type(person), intent(inout) :: p
    type(pTrns), pointer :: pTrns
    real(8) :: rnd
    if (p%visit%place_k .eq. PL_TRAIN) then
      pTrns => city_%train(p%visit%id)%pl%pTrns
    else
      pTrns => city_%area(p%visit%area_t)%place(p%visit%place_k)%o(p%visit%id)%pTrns
    end if

    select case (p%health(p%nHealth)%kind)
    case (HL_SUS)
      if (rnd .lt. pTrns%s2e) then
        p%nHealth = p%nHealth + 1
        p%health(p%nHealth) = health(HL_EXP, city_%time)
      end if
    case (HL_EXP)
      if (rnd .lt. pTrns%e2i) then
        p%nHealth = p%nHealth + 1
        p%health(p%nHealth) = health(HL_INF, city_%time)
      end if
    case (HL_INF)
      if (rnd .lt. pTrns%i2r) then
        p%nHealth = p%nHealth + 1
        p%health(p%nHealth) = health(HL_REC, city_%time)
      end if
    end select
  end subroutine

  subroutine evalPerson(city_, p)
    type(city), intent(inout) :: city_
    type(person), intent(inout) :: p
    type(place_t) :: dest
    integer :: idxTr
    call p%mkSched(city_%time)
    if (p%dest%kind .eq. NONE) then
      dest = consumeSched()
      if (p%dest%o%area_t .eq. p%visit%area_t) then
        p%visit = dest
      else
        p%dest%kind = SOME
        p%dest%o    = dest
      end if
    else !if (p%dest%kind .eq. SOME) then
      if (p%visit%place_k .eq. PL_TRAIN) then 
        if (city_%train(p%visit%id)%pl%id%area_t .eq. p%dest%o%area_t) then
          p%visit     = dest
          p%dest%kind = NONE
        endif
      else
        idxTr = catchTrain(city_%train, city_%time, p%visit%area_t, p%dest%o%area_t)
        if (idxTr .ge. 1) then
          p%visit = city_%train(idxTr)%pl%id
        end if
      endif
    endif
    call updateHealth(city_,p)
  contains
    function consumeSched() result (dest)
      type(place_t) :: dest
      integer :: n
      if (p%sched%n .ge. 1 .and. city_%time .ge. p%sched%time(1)) then
          dest = p%sched%place_t(1)
          p%sched%place_t(1:p%sched%n - 1) = p%sched%place_t(2:p%sched%n)
          p%sched%n = p%sched%n - 1
      else
        dest = p%visit
      end if
    end function 
  end subroutine

  subroutine evalTrain (city_, tr)
    type(city)   :: city_
    type(mobil), target :: tr
    type(mob_sched), pointer :: sch
    integer :: time_today
    sch => tr%sked(tr%iSked)
    time_today = modulo(city_%time, steps_per_day)
    if (time_today .eq. sch%time) then
      tr%pl%id%area_t = sch%area_t 
      tr%iSked        = modulo(tr%iSked + 1, size(tr%sked))
    end if
  end subroutine 

  ! 次のステップで使う(健康状態の)遷移確率を計算する。
  subroutine estTransit(city_)
    type(city), target :: city_
    integer :: j, k, l
    real(8) :: s, e, i, r, v, n, beta
    type(place), pointer :: p
    !$omp do private (k)
    do j = 1, size(city_%area)
      do k = 1, PL_MAX
        do l = 1, size(city_%area(j)%place(k)%o)
          p => city_%area(j)%place(k)%o(l)
          s = p%nVis%s
          e = p%nVis%e
          i = p%nVis%i
          r = p%nVis%r
          v = p%nVis%v
          n = s + e + i + r + v
          beta = 0d0; if (n > 0d0) beta = p%betaN / n
          p%pTrns%s2e = beta * i * dt
          p%pTrns%e2i = alpha * dt
          p%pTrns%i2r = gamma * dt
        end do
      end do
    end do
    !$omp end do
   
    !$omp do
    do k = 1, size(city_%train)
      p => city_%train(k)%pl
      s = p%nVis%s
      e = p%nVis%e
      i = p%nVis%i
      r = p%nVis%r
      v = p%nVis%v
      n = s + e + i + r + v
      beta = 0d0; if (n > 0d0) beta = p%betaN / n
      p%pTrns%s2e = beta * i * dt
      p%pTrns%e2i = alpha * dt
      p%pTrns%i2r = gamma * dt
    end do
    !$omp end do
  end subroutine 

  ! 個人の移動の結果を、場所毎の人口に反映させる
  !  - 各場所の訪問者数を数え上げる
  !  - それに応じて、次のステップで使う(健康状態の)遷移確率を計算する。
  subroutine evalPlace(city_) 
    type(city), target :: city_
    integer :: i
    !$omp do
    do i = 1, size(city_%area)
      call cntPersons(city_%area(i))
    end do
    !$omp end do
    call estTransit(city_)
  contains
    subroutine addVis(p)
      type(person) :: p
      type(nVis), pointer :: nVis

      if (p%visit%place_k .eq. PL_TRAIN) then
        nVis => city_%train(p%visit%id)%pl%nVis
      else
        nVis => city_%area(p%visit%area_t)%place(p%visit%place_k)%o(p%visit%id)%nVis
      end if

      select case (p%health(p%nHealth)%kind)
      case (HL_SUS)
        !$omp atomic
        nVis%s = nVis%s + 1
      case (HL_EXP)
        !$omp atomic
        nVis%e = nVis%e + 1
      case (HL_INF)
        !$omp atomic
        nVis%i = nVis%i + 1
      case (HL_VAC)
        !$omp atomic
        nVis%v = nVis%v + 1
      case (HL_REC)
        !$omp atomic
        nVis%r = nVis%r + 1
      case default
        write(*,*) 'evalPlace: Undeined health state: ', p%health(p%nHealth)
      end select
    end subroutine
       
    subroutine cntPersons (area_)
      type(area) :: area_
      integer    :: i, j, k

      do i = 1, PL_MAX
        do k = 1, size(area_%place(i)%o)
          area_%place(i)%o(k)%nVis = nVis(0, 0, 0, 0, 0)
        end do
      end do
      !$omp barrier

      do k = 1, size(area_%person)
        call addVis(area_%person(k))
      end do
    end subroutine
  end subroutine

  subroutine advanceTime(city_)
    type(city) :: city_
    integer :: i, j
    !$omp do private(j)
    do i=1, size(city_%area) 
      do j=1, size(city_%area(i)%person)
        call evalPerson(city_, city_%area(i)%person(j))
      end do
    end do
    !$omp end do

    !$omp barrier
    call evalPlace(city_) 
    !$omp barrier
  end subroutine
end module
