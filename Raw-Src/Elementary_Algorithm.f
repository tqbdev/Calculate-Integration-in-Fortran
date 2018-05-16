      PROGRAM CalIntegrator
	  REAL a, b, s, pi
	  INTEGER n
	  pi = 3.141592654
	  a = pi/2
	  b = pi
	  CALL qtrap(a, b, s)
	  WRITE (*,*) s
	  END PROGRAM CalIntegrator
	  
	  FUNCTION xFunc(x)
	  REAL :: xFunc
	  REAL :: x
	  xFunc = x
	  END FUNCTION xFunc
	  
	  SUBROUTINE trapzd(a, b, s, n)
	  INTEGER n
	  REAL a, b, s
	  INTEGER it, j
	  REAL del, sum, tnm, x
	  if (n.eq.1) then
C
	  			    s = 0.5*(b-a)*(sin(a)/a +sin(b)/b)
	  else
	  				it = 2**(n-2)
					tnm=it
					del=(b-a)/tnm
					x=a+0.5*del
					sum = 0.
					do j = 1, it
C
									    sum = sum + sin(x)/x
										x = x + del
					end do
					s = 0.5*(s+(b-a)*sum/tnm)
	  endif
	  return
	  END SUBROUTINE trapzd
	  
	  SUBROUTINE qtrap(a, b, s)
	  INTEGER JMAX
	  REAL a, b, s, EPS
	  PARAMETER (EPS=1.e-6, JMAX=20)
	  
	  INTEGER k
	  REAL olds
	  olds=-1.e30
	  do j = 1, JMAX
	  			call trapzd(a, b, s, j)
				if (j.gt.5) then
					return
				end if
				olds = s
	 end do
	 END SUBROUTINE qtrap