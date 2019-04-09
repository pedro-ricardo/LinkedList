module Linked_List
implicit none

!#####################################
!            Module Usage:
!#####################################
private
  
!The following subroutines/variables are the only
!ones accessible outside this module
public:: node, list
!-------------------------------------

!-------------------------------------
type :: node
    type(node), pointer :: next => null()
    class(*), allocatable :: item
    contains
    procedure, private :: destroy => node_finalizer
end type node
!-------------------------------------

!-------------------------------------
type :: list
    integer :: num_nodes = 0
    type(node), pointer :: head => null()
    type(node), pointer :: tail => null()
    contains
    procedure:: start => start_list
    procedure:: append => list_append_item
    procedure :: destroy => list_finalizer
end type list
!-------------------------------------

contains

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
! Returns a initialized list. Seems useless at this point due to standard values
! present on declarations.
pure subroutine start_list( this_list )
    implicit none
    class(list), intent(inout) :: this_list

    this_list%num_nodes = 0

end subroutine start_list
! ##############################################################################

! ##############################################################################
! Delete a node from the list and frees the memory in the item.
elemental subroutine node_finalizer( this_node )
    implicit none
    class(node), intent(inout) :: this_node
    
    !Deallocate it's item
    if (allocated(this_node%item)) deallocate(this_node%item)
    !Nullify it's pointer
    if (associated(this_node%next)) then
        call this_node%next%destroy()
        deallocate(this_node%next)
        nullify(this_node%next)
    end if
    
end subroutine node_finalizer
! ##############################################################################

! ##############################################################################
! Delete the entire list. Nullifing the head and triggering one node_finalizer
! after the other
elemental subroutine list_finalizer( this_list )
    implicit none
    class(list), intent(inout) :: this_list

    this_list%num_nodes = 0
    if (associated(this_list%head)) then
        call this_list%head%destroy()
        deallocate(this_list%head)
        nullify(this_list%head)
    end if
    
end subroutine list_finalizer
! ##############################################################################

! ##############################################################################
! Insert an item by creating a new node on the list.
pure subroutine list_append_item( this_list, item )
    implicit none
    class(list), intent(inout) :: this_list
    class(*), intent(in) :: item

    if (associated(this_list%tail)) then
        allocate(this_list%tail%next, source=start_node(item))
        this_list%tail => this_list%tail%next
    else
        allocate(this_list%head, source=start_node(item))
        this_list%tail => this_list%head
    end if

    this_list%num_nodes = this_list%num_nodes + 1

end subroutine list_append_item
! ##############################################################################


end module Linked_List