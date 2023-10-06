module matrix_iter

    real(8),parameter :: eps=1E-8					! Машинный нуль

contains

function jacobi(A,B) result (X)		! Итерационный метод Якоби решения СЛАУ
    implicit none

    real(8),dimension(1:,1:),intent(in) :: A
    real(8),dimension(1:),intent(in) :: B
    real(8),dimension(1:size(B)) :: X

    real(8),dimension(1:size(B),1:size(B)) :: DZ,Drev
    real(8),dimension(1:size(B)) :: G,X0
    integer :: i

    if(nodiagdom(A)) stop "Нет диагонального преобладания!"

    DZ=0.0						! Матрица для хранения D и Z
    Drev=0.0					! Матрица для хранения D^{-1}
    X0=B

    forall(i=1:size(B))
        DZ(i,i)=A(i,i)
        Drev(i,i)=1.0/A(i,i)
    end forall

    DZ=matmul(Drev,DZ-A)
    G=matmul(Drev,B)

    do
        X=matmul(DZ,X0)+G
        if (sum(abs(X-X0))<eps) exit
        X0=X
    end do
end function jacobi


function seidel(A,B) result (X)		! Итерационный метод Зейделя решения СЛАУ
    implicit none

    real(8),dimension(1:,1:),intent(in) :: A
    real(8),dimension(1:),intent(in) :: B
    real(8),dimension(1:size(B)) :: X

    real(8),dimension(1:size(B),1:size(B)) :: P
    real(8),dimension(1:size(B)) :: Q,X0
    integer :: i,j,n

    if(nodiagdom(A)) stop "Нет диагонального преобладания!"

    n=size(B)
    forall(i=1:n,j=1:n)	P(i,j)=-A(i,j)/A(i,i)
    forall(i=1:n)	Q(i)=B(i)/A(i,i)
    X0=B

    do
        do i=1,n
            X(i)=dot_product(P(i,1:i-1),X(1:i-1))+dot_product(P(i,i+1:n),X0(i+1:n))+Q(i)
        end do
        if (sum(abs(X-X0))<eps) exit
        X0=X
    end do

end function seidel

function relax(A,B) result (X)		! Итерационный метод релаксации решения СЛАУ
    implicit none

    real(8),dimension(1:,1:),intent(in) :: A
    real(8),dimension(1:),intent(in) :: B
    real(8),dimension(1:size(B)) :: X

    real(8),dimension(1:size(B),1:size(B)) :: P
    real(8),dimension(1:size(B)) :: Q
    integer :: i,j

    if(nodiagdom(A)) stop "Нет диагонального преобладания!"

    forall(i=1:size(B))
        P(i,:)=-A(i,:)/A(i,i)
        Q(i)=B(i)/A(i,i)
    end forall

    X=0.0

    do
        j=maxloc(abs(Q),dim=1)
        X(j)=X(j)+Q(j)
        if (abs(Q(j))<eps) exit
        Q=Q+Q(j)*P(:,j)
    end do

end function relax

function nodiagdom(A) result(L)
    implicit none
    real(8),dimension(1:,1:) :: A
    logical :: L
    integer :: i

  L=any((/(2*abs(A(i,i))<sum(abs(A(i,:))),i=1,size(A,1))/))

end function nodiagdom

end module matrix_iter
