       include'intde2.f'
	   program main
	   integer alp 
	   integer lenaw
	   double precision tiny,aw
	   parameter (lenew=8000)
	   parameter(tiny=1.d-307)
	   dimension aw(0:lenaw-1)
	   common/bl1/aw
	   common /bl2/alp
	   external h
!---------------------------------------------------------------
       open(1,file="data.dat", status="unknown")	   
	   call intdeini(lenaw,tiny,1.d-15,aw)
	   do 2 alp = 1,100
	   call intde(h,0.d0,1.d0,aw,s,err)
	   if(err.lt.0.d0)then
	          write(*,*) 'err=', err
			  pause
	   endif
	   write(*,*) 'tich phan=', s*alp**2 
	   write(1,*) s*alp**2
  2	   enddo
	   stop
	   end
!----------------------------------------------------------------
       function h(rr)
	   double precision h
	   double precision rr 
	   double precision rho,phi,z 
	   double precision aw
	   double precision s,err,pi 
	   common/bl/rho,phi,z 
	   common/bl1/ aw
	   external g
	   pi = 3.14d0
	   rho = rr 
	   call intde(g,pi,2.d0*pi,aw,s,err)
	   if(err.lt.0.d0)then
	          write(*,*) 'err=', err
			  pause
	   endif
	   h = s 
       return
       end
!----------------------------------------------------------------
       function g(pp)
	   double precision g
	   double precision rr 
	   double precision rho,phi,z 
	   double precision aw
	   double precision s,err
	   common/bl/rho,phi,z 
	   common/bl1/aw
	   external f 
	   phi = pp 
	   call intde(f,0.d0,1.d0,aw,s,err)
	   if(err.lt.0.d0)then 
	          write(*,*) 'err=', err
			  pause
	   endif
	   g = s 
       return
       end
!----------------------------------------------------------------
       function f(zz)
	   double precision f 
	   double precision zz 
	   double precision rho,phi,z 
	   common/bl/rho,phi,z 
	   z = zz 
	   f = func(rho,phi,z)
       return
       end
!----------------------------------------------------------------
       function func(rho,phi,z)
	   double precision rho,phi,z 
	   double precision func
	   integer alp
	   common /bl2/alp
	   func = rho**2*phi*DSQRT(z) 
       return
       end
!----------------------------------------------------------------
       	   