package xyz.hyperreal.mips1


object JumpInstructions {

  val JR =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = {
        val dest = cpu.regs(rs)

        if ((dest & 0x3) != 0)
          cpu.exception( "address" )
        else
          cpu.pc = dest
      }
    }


}