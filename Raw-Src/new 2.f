			module funcs
			  implicit none
			contains
			  real function funcX(x)
				real, intent(in) :: x
				funcX = x*x
			  end function funcX
			end module funcs

			module mult
			  use funcs
			  contains
			  SUBROUTINE trapzd(func, a, b, s, n)
			  		  REAL func
					  EXTERNAL func
					  INTEGER n
					  REAL a, b, s
					  
					  INTEGER it, j
					  REAL del, sum, tnm, x
					  
					  if (n.eq.1) then
									s = 0.5*(b-a)*(func(a) + func(b))
					  else
									it = 2**(n-2)
									tnm=it
									del=(b-a)/tnm
									x=a+0.5*del
									sum = 0.
									do j = 1, it
														sum = sum + func(x)
														x = x + del
									end do
									s = 0.5*(s+(b-a)*sum/tnm)
					  endif
					  return
			  END SUBROUTINE trapzd

			  SUBROUTINE qsimp(a, b, s)
					  INTEGER JMAX
					  REAL a, b, s, EPS
					  
					  PARAMETER (EPS=1.e-6, JMAX=20)

					  INTEGER j
					  REAL os, ost, st
					  ost=-1.e30
					  os=-1.e30
					  do j = 1, JMAX
								call trapzd(funcX, a, b, st, j)
								s=(4.*st-ost)/3.
								jj: if (j.gt.5) then
									kk: if (abs(s-os).lt.EPS*abs(os)) then
											 return
									end if kk
									mm: if (s.eq.0 .and. olds.eq.0) then
											return
									end if mm
								end if jj
								os = s
								ost = st
					 end do
			 END SUBROUTINE qsimp
			end module mult

			program funcargs
			  !use funcs
			  use mult
			  implicit none
			  real :: x
			  real :: v, s
			  x = 2
			  v = 3
			  call qsimp(x, v, s)
			  write (*,*) s
			end program funcargs 