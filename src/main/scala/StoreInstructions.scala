package xyz.hyperreal.mips1


object StoreInstructions {

  val SB =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.mem.writeByte( cpu.regs(rs) + imm, cpu.regs(rt) )
    }
  val SH =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.mem.writeShort( cpu.regs(rs) + imm, cpu.regs(rt) )
    }
  val SW =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.mem.writeInt( cpu.regs(rs) + imm, cpu.regs(rt) )
    }

}