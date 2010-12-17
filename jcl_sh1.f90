!実験スクリプト(共有メモリ版)その1
program JCL1
  use Frame
  use Model1
  type(city),target :: city_
  integer :: i, seed(256), t0, t1
  integer, external :: time

  t0 = time()

  !call random_seed(get=seed)
  !seed(1) = seed(1) + 1
  !call random_seed(put=seed)

  !call readCity(city_, "test200000.city")  
  call readCity(city_, "test3000-sig1.0.city")
  call simpleReadCheck(city_)
  call run1(1, 1440*180, "./", "test", city_)

  t1 = time()
  write(*,*) t1 - t0
end program
