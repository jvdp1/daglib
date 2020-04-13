!*******************************************************************************
!>
!  DAG module test program.

    program dag_pedigree

    use dag_module

    implicit none

    type(dag) :: d
    integer,dimension(:),allocatable :: order
    integer :: istat
    integer :: un,io,a,sire,dam

    integer :: n_nodes
    character(len=*),parameter :: filetype = 'pdf'  !! filetype for output plot ('pdf', png', etc.)


    open(newunit = un, file = './src/tests/pedigree.dat', action = 'read', status = 'old')
    n_nodes = 0
    do
        read(un,*,iostat=io)a
        if(io.ne.0)exit
        n_nodes = n_nodes +1
    end do

    call d%set_vertices(n_nodes)

    rewind(un)

    do
        read(un,*,iostat=io)a,sire,dam
        if(io.ne.0)exit
        if(sire.ne.0)then
            call d%set_edges(sire,[a])     !sire depends on a
            call d%set_vertex_info(sire,attributes='shape=square,fillcolor="SlateGray1",style=filled')
        end if
        if(dam.ne.0)then
            call d%set_edges(dam,[a])     !dam depends on a
            call d%set_vertex_info(dam,attributes='shape=circle,fillcolor="cornsilk",style=filled')
        end if
    end do

    close(un)

    ! TODO combine set_edges and set_vertex_info into one routine maybe.

    call d%toposort(order,istat)

    write(*,*) ''
    write(*,*) 'istat=', istat
    write(*,*) 'order=', order
    write(*,*) ''

    call d%save_digraph_s('test.dot','TB',300)
    call execute_command_line('dot -T'//filetype//' -o test.'//filetype//' test.dot')

    ! cleanup:
    call d%destroy()

    end program dag_pedigree
!*******************************************************************************
