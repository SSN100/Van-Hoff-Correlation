program vH
implicit none

integer :: i, j, k, l, num, skip
real :: max_dr, dt, bin_width, com(3, 24000), dr2(3)
integer, parameter :: n_bins=100, s=24000
real, parameter :: min_dr=0.0
real, allocatable, dimension(:) :: dr
integer, allocatable, dimension(:) :: hist
dt=0.5
skip=int(dt*1000*6)*4
num=s-skip

allocate(dr(num))
allocate(hist(n_bins))
open(unit=10, file="srimayee_msd.txt", status="old")
open(unit=11, file="output.xvg", status="new")
read(10, *) com

do i=1, num
    dr2=(com(:, i)-com(:, i+skip))**2
    dr(i)=sqrt(sum(dr2))
end do

max_dr=maxval(dr)
bin_width=(max_dr-min_dr)/n_bins

hist=0
do k=1, n_bins
    do j=1, num
        if(dr(j)>((k-1)*bin_width) .and. dr(j)<=(k*bin_width)) then
	    hist(k)=hist(k)+1
        end if
    end do
end do

do l=1, n_bins
    write(11, fmt=50) l*bin_width, hist(l)
    50 format(f8.5,i8)
end do
end program vH
