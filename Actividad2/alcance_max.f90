program alcance_max
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, t, u, x_max, y
  real :: theta, v, vx, vy

  ! Leer valores para el ángulo a, el tiempo t, y la velocidad inicial u desde la terminal
  write(*,*) 'Dame el ángulo y la rapidez inicial'
  read(*,*) a, u

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! ecuación para calcular tiempo final
  t = ((2*u) * (sin(a))) / g

  ! las ecuaciones de la posición en x_max y y
  x_max = ((u*u) * (sin(2*a))) / g
  y = u * sin(a) * t - 0.5 * g * t * t

  ! La velocidad al tiempo t
  vx = u * cos(a)
  vy = u * sin(a) - g * t
  v = sqrt(vx * vx + vy * vy)
  theta = atan(vy / vx) * 180.0 / pi
 
 ! escribiendo el resultado en la pantalla
  write(*,*) 'x_max: ',x_max,'  y: ',y 
  write(*,*) 'v: ',v,'  theta: ',theta
  write(*,*)   't: ',t
end program alcance_max
