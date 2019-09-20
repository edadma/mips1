package xyz.hyperreal.mips1


object SetInstructions {

  val SLT =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, if (cpu.regs(rs) < cpu.regs(rt)) 1 else 0 )
    }
  val SLTI =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = cpu.put( rt, if (cpu.regs(rs) < imm) 1 else 0 )
    }
  val SLTIU =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = cpu.put( rt, if ((cpu.regs(rs).toLong&0xFFFFFFFF) < (imm.toLong&0xFFFFFFFF)) 1 else 0 )
    }
  val SLTU =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, if ((cpu.regs(rs).toLong&0xFFFFFFFF) < (cpu.regs(rt).toLong&0xFFFFFFFF)) 1 else 0 )
    }

}