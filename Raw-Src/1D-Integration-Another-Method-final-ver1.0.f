      PROGRAM CalIntegrator
	  REAL a, b, s, pi
	  INTEGER n
	  pi = 3.141592654
	  a = 0
	  b = 2
	  CALL qsimp(a, b, s)
	  WRITE (*,*) s
	  END PROGRAM CalIntegrator
	  
	  FUNCTION xFunc(x)
	  		REAL :: xFunc
			REAL :: x
			xFunc = (x**4)*log(x + sqrt(x**2+1))
			RETURN
	  END FUNCTION xFunc
	  
	  SUBROUTINE trapzd(a, b, s, n)
	  INTEGER n
	  REAL a, b, s
	  INTEGER it, j
	  REAL del, sum, tnm, x
	  if (n.eq.1) then
C
	  			    s = 0.5*(b-a)*(xFunc(a) +xFunc(b))
	  else
	  				it = 2**(n-2)
					tnm=it
					del=(b-a)/tnm
					x=a+0.5*del
					sum = 0.
					do j = 1, it
C
									    sum = sum + xFunc(x)
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
	  			call trapzd(a, b, st, j)
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