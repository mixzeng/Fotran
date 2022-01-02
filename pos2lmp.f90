program vasp2lmps
implicit real*8 (a-h,o-z)
real(Kind=8), dimension(1:3,1:3) :: latt,stpos
real(Kind=8), dimension(1:3) :: tpos
real(Kind=8), dimension(:,:),allocatable::atm_pos
character*5, dimension(:),allocatable::atm_name
integer,dimension(:),allocatable::natm
character(len=20) :: arg,poscar,dc
character(len=512) :: line
logical :: separator
!n=iargc()
call getarg(1,arg)
read(arg,*)poscar

nelement=0
separator=.true.
!allocate (atm_name(1:nelement),natm(1:nelement))

open(11,file=poscar,status='old')
open(12,file='in.data')

read(11,*)
read(11,*)cons_latt
read(11,*)latt(1,1:3)
read(11,*)latt(2,1:3)
read(11,*)latt(3,1:3)
read(11,'(a512)')line
do i=1,len_trim(line)
  select case(line(i:i))
  case(" ",",",char(9))
    separator = .true.
  case default
    if(separator)then
      nelement=nelement+1
    end if
    separator = .false.
  end select
end do
write(*,*)nelement
backspace(11)
allocate (atm_name(1:nelement),natm(1:nelement))
read(11,*)atm_name(1:nelement)
read(11,*)natm(1:nelement)
read(11,*)dc
dc=trim(adjustl(dc))

ntot=sum(natm)
allocate(atm_pos(1:ntot,1:3))
latt=latt*cons_latt

write(*,*)latt(1,1:3)
write(*,*)latt(2,1:3)
write(*,*)latt(3,1:3)
write(*,*)ntot

write(12,*)'vasp2lmps'
write(12,*)
write(12,"(i8,a12)")ntot,' atoms'
write(12,"(i8,a12)")nelement,'atom types'
write(12,*)
write(12,"(2f10.4,a10)")0.d0,latt(1,1),'  xlo xhi'
write(12,"(2f10.4,a10)")0.d0,latt(2,2),'  ylo yhi'
write(12,"(2f10.4,a10)")0.d0,latt(3,3),'  zlo zhi'
write(12,*)
!write(12,"(a8)")'Masses'
!write(12,*)
!write(12,"(i3,f8.3)")1,55.847
!write(12,*)
write(12,"(a8)")'Atoms'
write(12,*)

do i=1,ntot
  read(11,*)tpos
  if(dc(1:1)=="d" .or. dc(1:1)=="D")then
    stpos=spread(tpos,1,3)
    stpos=matmul(stpos,latt)
    tpos=stpos(1,:)
  end if
  atm_pos(i,:)=tpos
enddo

nid=0
do i=1,nelement
  do j=1,natm(i)
    nid=nid+1
    write(12,"(2i6,3f8.3)")nid,i,atm_pos(nid,:)
  enddo
enddo
end
