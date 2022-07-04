program sudoku
implicit none

integer :: h,i,j,k,c,check=0, switch=0
integer, parameter :: n=9
integer :: main(n,n), pox(n,n,n)

call zero_matrix(n,n,main)
main ( 1,5 ) =4
main ( 1,9 ) =9
main ( 2,3 ) =2
main ( 2,5 ) =1
main ( 3,1 ) =5
main ( 3,8 ) =7
main ( 3,9 ) =3
main ( 4,2 ) =9
main ( 5,3 ) =4
main ( 5,7 ) =1
main ( 6,4 ) =5
main ( 6,6 ) =7
main ( 7,3 ) =1
main ( 7,5 ) =2
main ( 8,6 ) =3
main ( 8,8 ) =8
main ( 8,9 ) =5
call print_matrix(n,n,main(:,:))

do i=1,n
do j=1,n
do k=1,n
    pox(i,j,k)=k !initialization of stratified starting pox
end do
end do
end do

!call update_pox(n,n,main(:,:),pox(:,:,:),pox(:,:,:)) !is equivalent!!

        do !c=1,5 !MAIN CYCLE

call update_pox(n,n,main,pox,pox)

if (switch==0) then
    do i=1,n
    call print_matrix(n,n,pox(:,:,i))
    end do
switch=1
end if

!f.e. line and column, if the considered cell of main 
!is 0 and "her under pox vector 1..9" is all 0 
!except 1 value, then that 1 value goes in main
call rule1(n,main,pox, main,pox) !call update_pox inside to reduce iterations

!call rule2(n,main,pox, main,pox)

        call compute_check(n,main,check)
            if (check==1) then
            print *, 'finisheed'
            call print_matrix(n,n,main)
            stop
            end if
        end do !MAIN CYCLE
call print_matrix(n,n,main)
end program
!________________________________________________
!BEGIN UPDATES_POX
!________________________________________________
SUBROUTINE update_pox (nr,nc,main,poxin,poxout)
implicit none
integer,intent(in) :: nr,nc
integer, intent(in) :: main(nr,nc), poxin(nr,nc, nc)
integer, intent(out) :: poxout(nr,nc, nc)
integer :: i,j,k

!do k=1,nc
poxout(:,:,:)=poxin(:,:,:) !poxout(:,:,k)=poxin(:,:,k)
!end do

do i=1,nr
do j=1,nc
    if (main(i,j)/=0) then
        do k=1,nc
            poxout(i,j,k)=0 !remove possibilities where solved
        end do
        call update_pox_line(nr,nc,main(:,:),poxout(:,:,:),poxout(:,:,:),i,j)
        call update_pox_column(nr,nc,main(:,:),poxout(:,:,:),poxout(:,:,:),i,j)
        call update_pox_square(nr,nc,main(:,:),poxout(:,:,:),poxout(:,:,:),i,j)
    end if
end do
end do

END SUBROUTINE update_pox
!_______________________________________________
SUBROUTINE update_pox_line(nr,nc,main,poxin,poxout,x,y)
implicit none
integer,intent(in) :: nr,nc,x,y
integer, intent(in) :: main(nr,nc), poxin(nr,nc, nc)
integer, intent(out) :: poxout(nr,nc, nc)
integer :: i

poxout(:,:,:)=poxin(:,:,:)
do i=1,nr
!va a zero nello strato main(x,y)=eg4 la riga x e colonna y
poxout(i,y,main(x,y))=0
end do
END SUBROUTINE update_pox_line
!________________________________________________
SUBROUTINE update_pox_column(nr,nc,main,poxin,poxout,x,y)
implicit none
integer,intent(in) :: nr,nc,x,y
integer, intent(in) :: main(nr,nc), poxin(nr,nc, nc)
integer, intent(out) :: poxout(nr,nc, nc)
integer :: j
poxout(:,:,:)=poxin(:,:,:)
do j=1,nc
poxout(x,j,main(x,y))=0
end do
END SUBROUTINE update_pox_column
!_______________________________________________
SUBROUTINE update_pox_square(nr,nc,main,poxin,poxout,x,y)
implicit none
integer,intent(in) :: nr,nc,x,y
integer, intent(in) :: main(nr,nc), poxin(nr,nc, nc)
integer, intent(out) :: poxout(nr,nc, nc)
integer :: i,j, h,k
h=(x-1)/3 !coordinates of the square: 0,1,2
k=(y-1)/3
poxout(:,:,:)=poxin(:,:,:)
do i=3*h+1,(h+1)*3 !3*h+3
do j=3*k+1,(k+1)*3
poxout(i,j,main(x,y))=0
end do
end do
END SUBROUTINE update_pox_square
!_______________________________________________
!END UPDATES_POX
!________________________________________________

SUBROUTINE compute_check (n,main,check)
implicit none
integer,intent(in) :: n
integer, intent(in) :: main(n,n)
integer, intent(out) :: check
integer :: i,j
check=1

do i=1,n
do j=1,n
if (main(i,j)==0) then
check=0
exit
end if
end do
if (check==0) exit
end do
END SUBROUTINE compute_check
!________________________________________________
!BEGIN(RULES)
!________________________________________________
SUBROUTINE rule1(n,mainin,poxin, main,pox )
implicit none
integer,intent(in) :: n
integer, intent(in) :: mainin(n,n),poxin(n,n,n)
integer, intent(out) :: main(n,n),pox(n,n,n)
integer :: h,i,j,k,num

main(:,:)=mainin(:,:)
pox(:,:,:)=poxin(:,:,:)

do i=1,n
do j=1,n
    k=0 
    num=0
    if (main(i,j)==0) then
        do h=1,n
            if (pox(i,j,h)/=0 ) then
            num=num+1
            k=h
            end if
        end do
        if (num==1) then
            main(i,j)=k
            call update_pox(n,n,main,pox,pox)
        end if
    end if
end do
end do
END SUBROUTINE rule1
!nessun altro numero può stare in quella casella
!________________________________________________
!nessun altra casella può tenere quel numero
SUBROUTINE rule2(n,mainin,poxin, main,pox )
implicit none
integer,intent(in) :: n
integer, intent(in) :: mainin(n,n),poxin(n,n,n)
integer, intent(out) :: main(n,n),pox(n,n,n)
integer :: h,i,j,k

main(:,:)=mainin(:,:)
pox(:,:,:)=poxin(:,:,:)

do i=1,n
do j=1,n
        do h=1,n
    call avail_line(n,main,pox, feuL)
    call avail_col(n,main,pox, feuC)
    call avail_squa(n,main,pox, feuS)
if (feuL==1 .or. feuC==1 .or. feuS==1) then
main(i,j)=h
call update_pox(n,n,main,pox,pox)
end if
        end do
end do
end do
END SUBROUTINE rule2
!________________________________________________
SUBROUTINE avail_line(n,main,pox, feuL)
implicit none
integer,intent(in) :: n
integer, intent(in) :: mainin(n,n),poxin(n,n,n)
integer, intent(out) :: main(n,n),pox(n,n,n)
integer :: h,i,j,k

END SUBROUTINE avail_line
!________________________________________________
SUBROUTINE avail_col(n,main,pox, feuC)
implicit none
integer,intent(in) :: n
integer, intent(in) :: mainin(n,n),poxin(n,n,n)
integer, intent(out) :: main(n,n),pox(n,n,n)
integer :: h,i,j,k


END SUBROUTINE avail_col
!________________________________________________
SUBROUTINE avail_squa(n,main,pox, feuS)
implicit none
integer,intent(in) :: n
integer, intent(in) :: mainin(n,n),poxin(n,n,n)
integer, intent(out) :: main(n,n),pox(n,n,n)
integer :: h,i,j,k


END SUBROUTINE avail_squa
!________________________________________________
!END(RULES)
!________________________________________________
SUBROUTINE print_matrix (nr,nc,mat)
implicit none
integer,intent(in) :: nr,nc
integer, intent(in) :: mat(nr,nc)
integer :: i,j
do i=1,nr
	!print *, (mat(i,j), j=1,nc )
	write(*,80) (mat(i,j),j=1,nc )
	80 FORMAT ('',9I4)
    if (mod(i,3)==0) then
        print *, '- - - - - - - - - - - - - - - - - -'    
    end if
end do
print *, '_________________________________'
END SUBROUTINE print_matrix
!_______________________________________________

!________________________________________________
SUBROUTINE zero_matrix (nr,nc,mat)
implicit none
integer,intent(in) :: nr,nc
integer, intent(out) :: mat(nr,nc)
integer :: i,j
do i=1,nr
    do j=1,nc
    mat(i,j)=0
    end do
end do
END SUBROUTINE zero_matrix
!_______________________________________________