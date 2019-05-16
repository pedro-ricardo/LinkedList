! -----------
! Test suite
! -----------

program test_link
    use Linked_List
    implicit none
    
    type struct
        integer:: a=1,b=2,c=3
        double precision::d=5
    end type struct
    type(struct):: Vel2

    type vector
        double precision, dimension(3):: vec
    end type vector
    type(vector)::Vel
    
    type(list):: L
    type(node), pointer:: curr
    integer::i

    do i=1,size(Vel%vec)
        Vel%vec(i) = i
    end do
    
    !-------------
    !Start list
    !-------------
    call L%start()

    !-------------
    !Append items
    !-------------
    call L%append(2*3)
    call L%append(23)
    call L%append(21.d0)
    call L%append('nada')
    call L%append(.false.)
    call L%append(42.3d-2)
    call L%append(Vel)
    call L%append(Vel2)
    
    !-------------
    !Foward sweep
    !-------------
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
        type is (struct)
            write(*,*)'List > ',item%a,item%b,item%c,item%d
        class default
            write(*,*)'other'
        end select
        curr => curr%next
    end do

    !-------------
    ! Remove item 4 from list
    !-------------
    call L%pop_node(4)
    
    write(*,*)'----'
    
    !-------------
    !Backward sweep
    !-------------
    curr => L%tail
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
        type is (struct)
            write(*,*)'List > ',item%a,item%b,item%c,item%d
        class default
            write(*,*)'other'
        end select
        curr => curr%prev
    end do
    write(*,*)'Done'

    !-------------
    !Destroy the list and frees the memmory
    !-------------
    call L%destroy()

end program test_link


