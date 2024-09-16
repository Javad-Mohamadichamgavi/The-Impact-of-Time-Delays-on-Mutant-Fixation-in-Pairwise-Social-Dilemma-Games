subroutine dynamic(a,b,c,d,w,ncc,n,fix,fixtim,ffi,ggi,ppas,pnex,pstay,size1,TC,TD,mu)
	implicit none
	integer, intent(in) :: mu,n,TC,TD,ncc,size1
	real(8), intent(in) :: w
	real(8), intent(inout) , Dimension(mu)  :: fix
	real(8), intent(inout) , Dimension(mu)  :: fixtim
	real(8), intent(inout) , Dimension(TC)  :: ffi
	real(8), intent(inout) , Dimension(size1)  :: ppas,pnex,pstay
	real(8), Dimension(TC)  :: fi
	real(8), intent(inout) , Dimension(TD)  :: ggi
	real(8), Dimension(TD)  :: gi
	integer:: nc,npas,vvv
	real(8), intent(in) :: a,b,c,d
	integer :: i,j,l,step
	real(8) :: r(2),pc,zz1,zz2
	real(8) :: f,g
	Do i=1,mu
		nc=ncc
		j=1
		fi=ffi
		gi=ggi
		vvv=2
		step=0.
		Do while (j<2)
			step=step+1.
			f=fi(1)
			g=gi(1)
			npas=nc
			call random_number(r)
			pc=(f*nc)/(f*nc+(n-nc)*g)
			zz1=float(nc-1)/float(n-1)
			zz2=float(n-nc-1)/float(n-1)
			if (r(1)<=pc) then
				if (r(2)>zz1) nc=nc+1
			else
				if (r(2)>zz2) nc=nc-1
			end if
			f=1-w+w*((a*(nc-1)+(n-nc)*b)/(n-1))
			g=1-w+w*((nc*c+(n-nc-1)*d)/(n-1))
			do l=1,TC-1
				fi(l)=fi(l+1)
			end DO
			fi(TC)=f
			do l=1,TD-1
				gi(l)=gi(l+1)
			end DO
			gi(TD)=g
			if (nc-npas==1) then
				pnex(vvv)=pnex(vvv)+1
			elseif (npas-nc==1) then
				ppas(vvv)=ppas(vvv)+1
			else
				pstay(vvv)=pstay(vvv)+1
			end if
			vvv=(npas-1)*3+(nc-npas+2)	
			if (nc==n) then
				goto 11
			end if 
			if (nc==0) then
				goto 12
			end if
		end DO
11		fix(i)=1
		fixtim(i)=step
12		continue				
	end DO
end subroutine dynamic
