package xyz.hyperreal.mips1


object JumpInstructions {

  val BEQ =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay2( rs, rt, (cpu, rsv, rtv) => if (rsv == rtv) cpu.pc += imm<<2 )
    }
  val BGEZ =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => if (rsv >= 0) cpu.pc += imm<<2 )
    }
  val JR =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) =
        cpu.delay1( rs, (cpu, dest) => if ((dest & 0x3) != 0) cpu.exception("address") else cpu.pc = dest )
    }

}