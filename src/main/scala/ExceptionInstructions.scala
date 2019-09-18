package xyz.hyperreal.mips1


object ExceptionInstructions {

  val BREAK =
    new Instruction {
      def execute( cpu: CPU, inst: Int ) = {
        cpu.break = true
        false
      }
    }

}