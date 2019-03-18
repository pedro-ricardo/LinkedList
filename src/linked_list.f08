module Linked_List
implicit none

!-------------------------------
type :: node
    type(node), pointer :: next => null()
    class(*), allocatable :: item
    contains
    final :: node_finalizer
end type node
!-------------------------------

!-------------------------------
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
! Returns a initialized list. Seems useless at this point due to standard values
! present on declarations.
pure function start_list( ) result( val )
    implicit none
    type(list) :: val

end function start_list
! ##############################################################################

! ##############################################################################
! Retuns a initialized node. Can have an object in it or be empty.
pure function start_node( item ) result( val )
    class(*), intent(in), optional :: item
    type(node) :: val

    !Insert a item to the node if present
    if (present(item)) allocate(val%item, source=item)
    
end function start_node
! ##############################################################################

! ##############################################################################
! Delete a node from the list and frees the memory in the item.
elemental subroutine node_finalizer( this )
    implicit none
    type(node), intent(inout) :: this
    
    !Deallocate it's item
    if (allocated(this%item)) deallocate(this%item)
    !Nullify it's pointer
    if (associated(this%next)) nullify(this%next)

end subroutine node_finalizer
! ##############################################################################

! ##############################################################################
! Delete the entire list. Nullifing the head and triggering one node_finalizer
! after the other
elemental subroutine list_finalizer( this )
    implicit none
    type(list), intent(inout) :: this

    this%num_nodes = 0
    if (associated(this%head)) nullify(this%head)

end subroutine list_finalizer
! ##############################################################################

! ##############################################################################
! Insert an item by creating a new node on the list.
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


end module Linked_List