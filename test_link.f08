module link
    implicit none
    
    !-------------------------------
    ! List Types
    type :: node    
        type(node), pointer :: next => null()
        class(*), allocatable :: item
    contains
        final :: node_finalizer
    end type node

    type :: list
        integer :: num_nodes = 0
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure:: append => list_append_item
        final :: list_finalizer
    end type list
    !-------------------------------
    
    contains
    ! ##############################################################################
    ! Description
    pure function start_list( ) result( val )
        implicit none
        type(list) :: val
    end function start_list
    ! ##############################################################################
    
    ! ##############################################################################
    ! Description
    pure function start_node( item ) result( val )
        class(*), intent(in), optional :: item
        type(node) :: val

        if (present(item)) allocate(val%item, source=item)
    end function start_node
    ! ##############################################################################

    ! ##############################################################################
    ! Description
    elemental subroutine node_finalizer( this )
        implicit none
        type(node), intent(inout) :: this
        
        if (allocated(this%item)) deallocate(this%item)
        if (associated(this%next)) nullify(this%next)
        
    end subroutine node_finalizer
    ! ##############################################################################

    ! ##############################################################################
    ! Description
    elemental subroutine list_finalizer( this )
        implicit none
        type(list), intent(inout) :: this
        
        this%num_nodes = 0
        if (associated(this%head)) nullify(this%head)
        
    end subroutine list_finalizer
    ! ##############################################################################

    ! ##############################################################################
    ! Description
    pure subroutine list_append_item( this, item )
        implicit none
        class(list), intent(inout) :: this
        class(*), intent(in) :: item

        if (associated(this%tail)) then
            allocate(this%tail%next, source=start_node(item))
            this%tail => this%tail%next
        else
            allocate(this%head, source=start_node(item))
            this%tail => this%head
        end if
        this%num_nodes = this%num_nodes + 1
        
    end subroutine list_append_item
    ! ##############################################################################

end module link

! -----------
! Test suite
! -----------

program test_link
    use link
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


