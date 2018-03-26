program integral1
   implicit none
   integer impr, i
   double precision s, f0, h, x, r, f1, pi

   impr = 6
   s = 0.
   f0 = 1.
   h = 1./65536.
   x=h
   do 2 i=1,65535
   r = 2. * (1./(1.+X**2))
   s = s+r
2  x = x+h
   f1 = 0.5
   PI = 4.*((f0+s+f1)*h/2.)
   print *, PI
!   write(impr,3)PI
!3  format (1X,F8.5)
   stop 
end
