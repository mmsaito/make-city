(*'
A. 本ソフトウェアは Everyone's Ware です。このソフトを手にした一人一人が、
   ご自分の作ったものを扱うのと同じように、自由に利用することが出来ます。

  A-1. フリーウェアです。作者からは使用料等を要求しません。
  A-2. 有料無料や媒体の如何を問わず、自由に転載・再配布できます。
  A-3. いかなる種類の 改変・他プログラムでの利用 を行っても構いません。
  A-4. 変更したものや部分的に使用したものは、あなたのものになります。
       公開する場合は、あなたの名前の下で行って下さい。

B. このソフトを利用することによって生じた損害等について、作者は
   責任を負わないものとします。各自の責任においてご利用下さい。

C. 著作者人格権は ○○○○ に帰属します。著作権は放棄します。

D. 以上の３項は、ソース・実行バイナリの双方に適用されます。
*)
module X_Misc
  implicit none
contains
! 乱数
  real(8) function rndIn (x,y)
    real(8) :: x, y, r
    call random_number(r) ! r in [0,1)
    rndIn = x + r *  (y - x)  
  end function
  integer function irndIn (i, j)
    integer :: i, j
    irndIn = int(rndIn(dble(i),dble(j)))
  end function
end module

module Frame
!  use X_Misc
!$ use omp_lib
  implicit none
  real(8), parameter :: alpha = 1d0/3.5d0
  real(8), parameter :: gamma = 1d0/3.0d0

  real(8), parameter :: dt = 1d0/1440d0 
  integer, parameter :: steps_per_day = nint(1.0/dt)

  ! 時間の単位 (in steps)
  real(8), parameter :: days    = 1d0/dt
  real(8), parameter :: hours   = 1d0/24d0/dt
  real(8), parameter :: minutes = 1d0/24d0/60d0/dt
  
  integer, parameter :: idays   = nint(days)
  integer, parameter :: ihours  = nint(hours)
  integer, parameter :: iminutes= nint(minutes)

  type health
    sequence
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
  integer, parameter :: PL_CRAM  = 1
  integer, parameter :: PL_SCH   = 2
  integer, parameter :: PL_CORP  = 3
  integer, parameter :: PL_HOME  = 4
  integer, parameter :: PL_SUPER = 5
  integer, parameter :: PL_PARK  = 6
  integer, parameter :: PL_TRAIN = 7
  integer, parameter :: PL_MAX   = PL_TRAIN

  type place_t
    sequence
    integer :: area_t, place_k, id
    integer :: tVis !使わないかもしれない
  end type

  integer, parameter :: NONE = 0
  integer, parameter :: SOME = 1
  type place_t_option
    sequence
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

  integer, parameter :: SCHED_MAX = 20
  type sched
    sequence
    integer :: n
    integer, dimension(SCHED_MAX) :: time
    type(place_t), dimension(SCHED_MAX) :: place_t
  end type

  type mobil 
    type(place)     :: pl
    integer         :: iSked
    type(mob_sched), allocatable, dimension(:) :: sked
    logical         :: onService
  end type

  !person%role 定数
  integer, parameter :: Employed = 1
  integer, parameter :: Hausfrau = 2
  integer, parameter :: Student  = 3

  type person
    sequence
    integer :: age, gender, role
    type(place_t), allocatable, dimension(:) :: belong
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
  !   sched_: スケジュール
  subroutine scheduler_t(person_, time, sched_)
    type(person) :: person_ ! type(person)ではNG
    integer       :: time
    type(sched)   :: sched_
  end subroutine

  ! person%belongからカテゴリに一致するものを探す
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
    if (present(doesMatch)) then
      doesMatch = .false.
    else
      write(*,*) 'runtime error: cant find ', kind
      stop
    end if
  end function


  logical function catchTrain__matchSrc(src, step, sked) result (f)
    type(mob_sched) :: sked
    integer :: src, step
    f = sked%time .ge. step .and. sked%area_t .eq. src
  end function

  logical function catchTrain__matchDst(dst, step, sked) result (f)
    type(mob_sched) :: sked
    integer :: dst, step
    f = sked%time .ge. step .and. sked%area_t .eq. dst
  end function

  ! ToDo: 非常に非効率的。かしこいアルゴにせよ。ex.bsearch
  integer function catchTrain(trains, now, src, dst) result (i)
    type(mobil) :: trains(:)
    integer     :: now, src, dst
    integer     :: step
    integer     :: ref_test
    ref_test = src
    step = modulo(now, steps_per_day)
    do i = 1, size(trains)
      ref_test = src
      if (.not. trains(i)%onService) cycle
! 内部関数渡しにすると、ときどき、クラッシュするようだ。
!      if (.not. exists(matchSrc, trains(i)%sked)) cycle
!      if (.not. exists(matchDst, trains(i)%sked)) cycle
      if (.not. exists2(catchTrain__matchSrc, src, step, trains(i)%sked)) cycle
      if (.not. exists2(catchTrain__matchDst, dst, step, trains(i)%sked)) cycle
      return
    end do
    i = 0
    return
  contains
!    logical function matchSrc(sked) result (f)
!      type(mob_sched) :: sked
!      integer :: sked_time, sked_area, x_step, x_src, x_f
!      f = sked%time .eq. step .and. sked%area_t .eq. src
!    end function
!    logical function matchDst(sked) result (f)
!      type(mob_sched) :: sked
!      f = sked%time .ge. step .and. sked%area_t .eq. dst
!    end function
!    logical recursive function exists(f,sked) result (ans)
!      type(mob_sched) :: sked(:)
!      logical, external :: f
!      ans = f(sked(1)) 
!      if (size(sked).eq.1) return
!      ans = ans .or. exists(f,sked(2:))
!    end function

    logical function exists2(f,place,time,sked) result (ans)
      integer :: place, time
      type(mob_sched) :: sked(:)
      logical, external :: f
      integer :: i
      ans = .false.
      do i = 1, size(sked)
        ans = f(place,time,sked(i))
        if (ans) return  
      end do
    end function

    logical function exists(f,sked) result (ans)
      type(mob_sched) :: sked(:)
      interface
        logical function f(sked)
          import
          type(mob_sched) :: sked
        end function
      end interface
      integer :: i
      ans = .false.
      do i = 1, size(sked)
        ans = f(sked(i))
        if (ans) return  
      end do
    end function
  end function

  subroutine updateHealth(city_,p)
    type(city), target, intent(inout) :: city_
    type(person), intent(inout) :: p
    type(pTrns), pointer :: pTrns_
    real(8) :: rnd
    if (p%visit%place_k .eq. PL_TRAIN) then
      pTrns_ => city_%train(p%visit%id)%pl%pTrns
    else
      pTrns_ => city_%area(p%visit%area_t)%place(p%visit%place_k)%o(p%visit%id)%pTrns
    end if

    !if (pTrns%s2e .gt. 0) write(*,*) pTrns%s2e 
    !if (pTrns%e2i .gt. 0) write(*,*) pTrns%e2i

    call random_number(rnd)
    select case (p%health(p%nHealth)%kind)
    case (HL_SUS)
      if (rnd .lt. pTrns_%s2e) then
        p%nHealth = p%nHealth + 1
        p%health(p%nHealth) = health(HL_EXP, city_%time)
      end if
    case (HL_EXP)
      if (rnd .lt. pTrns_%e2i) then
        p%nHealth = p%nHealth + 1
        p%health(p%nHealth) = health(HL_INF, city_%time)
      end if
    case (HL_INF)
      if (rnd .lt. pTrns_%i2r) then
        p%nHealth = p%nHealth + 1
        p%health(p%nHealth) = health(HL_REC, city_%time)
      end if
    end select
  end subroutine

  subroutine evalPerson(city_, p)
    type(city), intent(inout) :: city_
    type(person), intent(inout) :: p
    type(place_t) :: dest, tmp
    integer :: idxTr

    call p%mkSched(city_%time, p%sched)
    if (p%dest%kind .eq. NONE) then
      dest = consumeSched()
      if (dest%area_t .eq. p%visit%area_t) then
        p%visit     = dest
        p%dest%kind = NONE
      else
        p%dest%kind = SOME
        p%dest%o    = dest
      end if
    else if (p%dest%kind .eq. SOME) then
      if (p%visit%place_k .eq. PL_TRAIN) then 
        if (city_%train(p%visit%id)%pl%id%area_t .eq. p%dest%o%area_t) then
          p%visit     = p%dest%o
          p%dest%kind = NONE
        endif
      else
        idxTr = catchTrain(city_%train, city_%time, p%visit%area_t, p%dest%o%area_t)
        if (idxTr .ge. 1) then
          p%visit = city_%train(idxTr)%pl%id
        end if
      endif
    else
      write(*,*) 'runtime error: bad option kind = ', p%dest%kind, SOME, NONE
      stop
    endif
    call updateHealth(city_,p)
  contains
    function consumeSched() result (dest)
      type(place_t) :: dest
      if (p%sched%n .ge. 1 .and. city_%time .ge. p%sched%time(1)) then
          dest = p%sched%place_t(1)
          !Here I make a bug to forget shift ..%time. SoA is bad!! use AoS!!
          p%sched%time(1:p%sched%n - 1)    = p%sched%time(2:p%sched%n)
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
    ! 【意味上の注意】発車時刻になったら、次の停車駅にワープする。

    ! 電車が走っていない適当な時間(3:00)に車庫出しする。
    if (time_today .eq. 180)&
      tr%onService    = .true.

    if (time_today .eq. sch%time) then
      tr%pl%id%area_t = sch%area_t 
      tr%iSked        = tr%iSked + 1
      if (tr%iSked .gt.  size(tr%sked)) then
        tr%onService = .false.
        tr%iSked = 1
      end if
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
      do k = 1, size(city_%area(j)%place)
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
    integer :: i, j, k
    !$omp do private(j,k)
    do i = 1, size(city_%area)
      do j = 1, size(city_%area(i)%place)
        do k = 1, size(city_%area(i)%place(j)%o)
          city_%area(i)%place(j)%o(k)%nVis = nVis(0, 0, 0, 0, 0)
        end do
      end do
    end do
    !$omp end do
    !$omp barrier

    !$omp do
    do i = 1, size(city_%area)
      do k = 1, size(city_%area(i)%person)
        call addVis(city_%area(i)%person(k))
      end do
    end do
    !$omp end do
    call estTransit(city_)
  contains
    subroutine addVis(p)
      type(person) :: p
      type(nVis), pointer :: nVis_

      if (p%visit%place_k .eq. PL_TRAIN) then
        nVis_ => city_%train(p%visit%id)%pl%nVis
      else
        nVis_ => city_%area(p%visit%area_t)%place(p%visit%place_k)%o(p%visit%id)%nVis
      end if

      select case (p%health(p%nHealth)%kind)
      case (HL_SUS)
        !$omp atomic
        nVis_%s = nVis_%s + 1
      case (HL_EXP)
        !$omp atomic
        nVis_%e = nVis_%e + 1
      case (HL_INF)
        !$omp atomic
        nVis_%i = nVis_%i + 1
      case (HL_VAC)
        !$omp atomic
        nVis_%v = nVis_%v + 1
      case (HL_REC)
        !$omp atomic
        nVis_%r = nVis_%r + 1
      case default
        write(*,*) 'evalPlace: Undeined health state: ', p%health(p%nHealth)
      end select
    end subroutine
       
    subroutine cntPersons (area_)
      type(area) :: area_
      integer    :: i, j, k

    end subroutine
  end subroutine

  subroutine advanceTime(city_)
    type(city) :: city_
    integer :: i, j, k

    !$omp barrier
    !$omp do private(j)
    do i = 1, size(city_%area) 
      do j = 1, size(city_%area(i)%person)
        call evalPerson(city_, city_%area(i)%person(j))
      end do
    end do
    !$omp end do
    !$omp barrier

    !$omp do 
      do i = 1, size(city_%train)
        call evalTrain (city_, city_%train(i))
      end do
    !$omp end do

    !$omp master
    city_%time = city_%time + 1
    !$omp end master
    !$omp barrier
    !$omp flush

    call evalPlace(city_)
    !$omp barrier
  end subroutine
end module

module Record
  use Frame
contains
  type(nVis) function reducePop1 (area_) result (it)
    type(area) :: area_
    integer :: i
    it%s = 0
    it%e = 0
    it%i = 0
    it%r = 0
    do i = 1, size(area_%person)
      select case (area_%person(i)%health(area_%person(i)%nHealth)%kind)
      case (HL_SUS); it%s = it%s + 1
      case (HL_EXP); it%e = it%e + 1
      case (HL_INF); it%i = it%i + 1
      case (HL_REC); it%r = it%r + 1
      end select
    end do
  end function
  function reducePop(city_) result (it)
    type(city) :: city_
    type(nVis) :: it(size(city_%area))
    integer    :: i
    do i = 1, size(it)
      it(i) = reducePop1(city_%area(i))
    end do
  end function
  subroutine  showPopTag(os, n)
    integer :: os, n
    write(os,'("t,",$)')
    do i = 1, n
      write(os,'("s,e,i,r,",$)')
    end do
    write(os,*)
  end subroutine

  subroutine  showPop(os, t, pop)
    integer    :: os, t
    type(nVis) :: pop(:)
    write(os,'(I8,",",$)') t
    do i = 1, size(pop)
      write(os,'(4(I6,","),$)') pop(i)%s, pop(i)%e, pop(i)%i, pop(i)%r
    end do
    write(os,*)
  end subroutine
end module
