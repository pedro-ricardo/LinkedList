! -----------
! Test suite
! -----------

program test_link
    use Linked_List
    implicit none
    
    type vector
        double precision, dimension(:), allocatable:: vec
    end type vector
    type(vector)::Vel
    
    type(list):: L
    type(node), pointer:: curr
    integer::i
    
    allocate(Vel%vec(5))
    
    do i=1,size(Vel%vec)
        Vel%vec(i) = i
    end do
    
    L = start_list()
    call L%append(2*3)
    call L%append(23)
    call L%append(21.d0)
    call L%append('nada')
    call L%append(.false.)
    call L%append(42.3d-2)
    call L%append(Vel)
    
    curr => L%head
    do while ( associated(curr) )
        select type (item =>curr%item)
        type is (integer)
            write(*,*)'List > ',item
        type is (double precision)
            write(*,*)'List > ',item
        type is (character(*))
            write(*,*)'List > ',item
        type is (logical)
            write(*,*)'List > ',item
        type is (vector)
            write(*,*)'List > ',item%vec
        class default
            write(*,*)'other'
        end select
        curr => curr%next
    end do
    
    write(*,*)'Done'
    
end program test_link


