package xyz.hyperreal.mips1


object ExceptionInstructions {

  val BREAK =
    new Instruction {
      def execute( cpu: CPU, inst: Int ) = {
        cpu.exception( BREAK_Exception )
      }
    }
  val SYSCALL =
    new Instruction {
      def execute( cpu: CPU, inst: Int ) = {
        cpu.exception( SYSCALL_Exception )
      }
    }

}