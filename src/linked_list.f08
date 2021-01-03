module Linked_List
!------------------------------------------------------------------------------
! Modern Fortran Heterogeneous Linked List
!------------------------------------------------------------------------------
!
! Free from memory leaks,
! Tested with gfortran 9.3.1 and valgrind 3.15.0
!
! Author:
! Pedro Ricardo C. Souza
! Distributed by GitHub under GPL 3.0 licence 
! 
!------------------------------------------------------------------------------
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
! To transform the generic list in 
! homogeneous list, just define your 
! fixed type for the list and then
! search for 'class(*)' and replace 
! with 'type(your_type)'
!-------------------------------------

!-------------------------------------
type node
    type(node), pointer :: next => null()
    type(node), pointer :: prev => null()
    class(*), allocatable :: item
    contains
    procedure, private :: destroy => node_finalizer
    procedure, private :: destroy_all => node_finalizer_snowball
end type node
!-------------------------------------

!-------------------------------------
type list
    integer, private :: num_nodes = 0
    type(node), pointer :: head => null()
    type(node), pointer :: tail => null()
    contains
    procedure:: start => start_list
    procedure:: append => list_append_item
    procedure:: destroy => list_finalizer
    procedure:: foreach => list_foreach
    procedure:: pop_node => list_pop_node_n
    procedure:: pop_this => list_pop_node
    procedure:: len => list_get_len
    final:: list_destroy
end type list
!-------------------------------------

!-------------------------------------
! Definition of a subroutine that operates on node item
interface
subroutine process(item)
    implicit none
    !Entrada:
    class(*), intent(inout) :: item
    
end subroutine process
end interface
!-------------------------------------

contains

! ##############################################################################
! Retuns a initialized node. Can have an object in it or be empty.
pure function start_node( item ) result( val )
    implicit none
    type(node) :: val
    !Entrada:
    class(*), intent(in), optional :: item
    !Local:
    
    !Insert a item to the node if present
    if (present(item)) allocate(val%item, source=item)
    
end function start_node
! ##############################################################################

! ##############################################################################
! Returns a initialized list. Seems useless at this point due to standard values
! present on declarations.
pure subroutine start_list( this_list )
    implicit none
    !Entrada:
    class(list), intent(inout) :: this_list
    !Local:

    this_list%num_nodes = 0

end subroutine start_list
! ##############################################################################

! ##############################################################################
! Delete all nodes in sequence from the list and frees the memory in the items.
pure recursive subroutine node_finalizer_snowball( this_node )
    implicit none
    !Entrada:
    class(node), intent(inout) :: this_node
    !Local:
    
    !Deallocate it's item
    if (allocated(this_node%item)) deallocate(this_node%item)
    !Nullify it's pointers
    if (associated(this_node%next)) then
        call this_node%next%destroy_all()
        deallocate(this_node%next)
        nullify(this_node%next)
    end if
    nullify(this_node%prev)
    
end subroutine node_finalizer_snowball
! ##############################################################################

! ##############################################################################
! Pop out a node from the list, by a given number.
pure subroutine list_pop_node_n( this_list, node_num )
    implicit none
    !Entrada:
    class(list), intent(inout) :: this_list
    integer, intent(in):: node_num
    !Local:
    type(node), pointer:: curr
    integer:: cont

    !Foward sweep
    curr => this_list%head
    cont = 1
    do while ( associated(curr) )
        if (cont==node_num) then
            
            call this_list%pop_this(curr)
            
            !Exit when found
            return
        end if
        curr => curr%next
        cont = cont+1
    end do
    
    
end subroutine list_pop_node_n
! ##############################################################################

! ##############################################################################
! Pop out a node from the list, by the given node.
pure subroutine list_pop_node(this_list, this_node)
    implicit none
    !Entrada:
    class(list), intent(inout) :: this_list
    type(node), pointer :: this_node
    !Local:
    
    if (associated(this_node%prev).and.associated(this_node%next)) then
        !In List middle
        this_node%next%prev => this_node%prev
        this_node%prev%next => this_node%next

    else if (associated(this_node%prev)) then
        !In List tail
        nullify(this_node%prev%next)
        this_list%tail => this_node%prev

    else if (associated(this_node%next)) then
        !In List head
        nullify(this_node%next%prev)
        this_list%head => this_node%next
    end if

    !Destroy node content
    call this_node%destroy()            
    !Free it's memmory
    deallocate(this_node)
    
    !Remove node from count
    this_list%num_nodes = this_list%num_nodes - 1
    
    
end subroutine list_pop_node
! ##############################################################################

! ##############################################################################
! Delete a node from the list and frees the memory in the item.
pure subroutine node_finalizer( this_node )
    implicit none
    !Entrada:
    class(node), intent(inout) :: this_node
    !Local:
    
    !Deallocate it's item
    if (allocated(this_node%item)) deallocate(this_node%item)
    !Nullify it's pointers
    nullify(this_node%next)
    nullify(this_node%prev)
        
end subroutine node_finalizer
! ##############################################################################

! ##############################################################################
! Delete the entire list. Nullifing the head and triggering one node_finalizer
! after the other
pure subroutine list_finalizer( this_list )
    implicit none
    !Entrada:
    class(list), intent(inout) :: this_list
    !Local:

    this_list%num_nodes = 0
    if (associated(this_list%head)) then
        call this_list%head%destroy_all()
        deallocate(this_list%head)
        nullify(this_list%head)
        nullify(this_list%tail)
    end if
    
end subroutine list_finalizer
! ##############################################################################

! ##############################################################################
! Insert an item by creating a new node on the list.
pure subroutine list_append_item( this_list, item )
    implicit none
    !Entrada:
    class(list), intent(inout) :: this_list
    class(*), intent(in) :: item
    !Local:

    if (associated(this_list%tail)) then
        allocate(this_list%tail%next, source=start_node(item))
        this_list%tail%next%prev => this_list%tail
        this_list%tail => this_list%tail%next
    else
        allocate(this_list%head, source=start_node(item))
        this_list%tail => this_list%head
    end if

    this_list%num_nodes = this_list%num_nodes + 1

end subroutine list_append_item
! ##############################################################################

! ##############################################################################
! Loop through nodes executing the subroutine on items
subroutine list_foreach(this_list, subr)
    implicit none
    !Entrada:
    class(list), intent(inout) :: this_list
    procedure(process):: subr
    !Local:
    type(node), pointer:: curr
    
    !Foward sweep
    curr => this_list%head
    do while ( associated(curr) )
        if (allocated(curr%item)) call subr(curr%item)
        curr => curr%next
    end do
    
end subroutine list_foreach
! ##############################################################################

! ##############################################################################
! Return the number of element in the list
function list_get_len(this_list) result(len)
    implicit none
    !Saida:
    integer:: len
    !Entrada:
    class(list), intent(inout):: this_list
    !Local:
    
    len = this_list%num_nodes
    
end function list_get_len
! ##############################################################################

! ##############################################################################
! Same as 'list_finalizer' but with different
! declaration needed by the 'final' procedure.
subroutine list_destroy(this_list)
    implicit none
    !Entrada:
    type(list), intent(inout):: this_list
    !Local:
    
    call this_list%destroy()
    
end subroutine list_destroy
! ##############################################################################

end module Linked_List