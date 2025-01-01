program imprimirclass
  implicit none

  class(*), allocatable :: mivariable

  ! asignar diferentes valores para probar
  if (allocated(mivariable)) deallocate(mivariable)  ! desasignar memoria antes de asignar
  allocate(mivariable, source=42)            ! entero
  call imprimir(mivariable)

  if (allocated(mivariable)) deallocate(mivariable)  ! desasignar memoria antes de asignar
  allocate(mivariable, source=3.14)         ! número real
  call imprimir(mivariable)

  if (allocated(mivariable)) deallocate(mivariable)  ! desasignar memoria antes de asignar
  allocate(mivariable, source="hola mundo") ! cadena de texto
  call imprimir(mivariable)

contains

  subroutine imprimir(variable)
    class(*), intent(in) :: variable
    select type (variable)
      type is (integer)
        print *, "integer: ", variable
      type is (real)
        print *, "real: ", variable
      type is (character(*))
        print *, "character: ", variable
      class default
        print *, "tipo desconocido o no manejado."
    end select
  end subroutine imprimir

end program imprimirclass
