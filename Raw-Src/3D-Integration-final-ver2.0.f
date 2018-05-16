      MODULE funcs
	  		implicit none
	  CONTAINS			
			FUNCTION func(x, y, z)
			REAL func, x, y, z
			func = 1
			RETURN
			END FUNCTION func
			
			FUNCTION y1(x)
			REAL y1, x
			y1 = -sqrt(4-x**2)
			RETURN
			END FUNCTION y1
			
			FUNCTION y2(x)
			REAL y2, x
			y2 = sqrt(4-x**2)
			RETURN
			END FUNCTION y2
			
			FUNCTION z1(x, y)
			REAL z1, x, y
			z1 = -sqrt(4-x**2-y**2)
			RETURN
			END FUNCTION z1
			
			FUNCTION z2(x, y)
			REAL z2, x, y
			z2 = sqrt(4-x**2-y**2)
			RETURN
			END FUNCTION z2
	  END MODULE funcs
	  
	  MODULE integration
	  		USE funcs
			CONTAINS
			 SUBROUTINE qgaus(func,a,b,ss)
				 REAL a,b,ss,func
				 EXTERNAL func
				 INTEGER j
				 REAL dx,xm,xr
				 real, dimension(5) :: w
				 real, dimension(5) :: x
				 w(1) = 0.2955242247
				 w(2) = 0.2692667193
				 w(3) = 0.2190863625
				 w(4) = 0.1494513491
				 w(5) = 0.0666713443
				 
				 x(1) = 0.1488743389
				 x(2) = 0.4333953941
				 x(3) = 0.6794095682
				 x(4) = 0.8650633666
				 x(5) = 0.9739065285
				 
				 xm=0.5*(b+a)
				 xr=0.5*(b-a)
				 ss=0
				 do j=1,5
				 dx=xr*x(j)
				 ss=ss+w(j)*(func(xm+dx)+func(xm-dx))
				 end do
				 ss=xr*ss
				 return
			 END SUBROUTINE qgaus
			 
			 SUBROUTINE tripleD(x1, x2, ss)
			 REAL x1, x2, ss
			 call qgaus(h, x1, x2, ss)
			 return
			 END SUBROUTINE tripleD
			 
			 FUNCTION f(zz)
			 REAL f, zz, x, y, z
			 COMMON /xyz/ x,y,z
			 z = zz
			 f = func(x,y,z)
			 return
			 END FUNCTION f
			 
			 FUNCTION g(yy)
			 REAL g,yy,x,y,z
			 COMMON /xyz/ x,y,z
			 REAL ss
			 y=yy
			 call qgaus(f,z1(x,y),z2(x,y),ss)
			 g=ss
			 return
			 END FUNCTION g
			 
			 FUNCTION h(xx)
			 REAL h,xx,x,y,z
			 COMMON /xyz/ x,y,z
			 REAL ss
			 x=xx
			 call qgaus(g,y1(x),y2(x),ss)
			 h=ss
			 return
			 END FUNCTION h
	  END MODULE integration
	  
	  PROGRAM CalIntegrator
	  	  USE funcs
		  USE integration
		  implicit none
		  REAL x1, x2, s, pi, ro
		  INTEGER n
		  pi = 3.141592654
		  x1 = -2
		  x2 = 2
		  ro = 0.05
		  call tripleD(x1, x2, s)
		  WRITE (*,*) s*ro
	  END PROGRAM CalIntegrator