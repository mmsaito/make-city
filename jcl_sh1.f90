!実験スクリプト(共有メモリ版)その1
program JCL1
  use Frame
  use Model1
  type(city),target :: city_
  integer :: i
  
  !call readCity(city_, "test200000.city")  
  call readCity(city_, "test3000.city")
  !call simpleReadCheck(city_)
  call run1(1, 1440, "./", "test", city_)
end program
