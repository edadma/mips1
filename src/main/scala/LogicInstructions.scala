package xyz.hyperreal.mips1


object LogicInstructions {

  val AND =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.regs(rs) & cpu.regs(rt) )
    }
  val ANDI =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = cpu.put( rt, cpu.regs(rs) & imm )
    }
  val NOR =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, ~(cpu.regs(rs) | cpu.regs(rt)) )
    }
  val OR =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.regs(rs) | cpu.regs(rt) )
    }
  val ORI =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = cpu.put( rt, cpu.regs(rs) | (imm&0xFFFF) )
    }

}