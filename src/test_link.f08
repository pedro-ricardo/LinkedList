! -----------
! Test suite
! -----------

program test_link
    use Linked_List
    implicit none
    type(list):: L
    type(node), pointer:: curr
    
    L = start_list()
    call L%append(2*3)
    call L%append(23)
    call L%append(21.d0)
    call L%append('nada')
    call L%append(.false.)
    call L%append(42.3d-2)
    
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
        class default
            write(*,*)'other'
        end select
        curr => curr%next
    end do
    
    write(*,*)'Done'
    
end program test_link


