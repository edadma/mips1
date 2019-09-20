package xyz.hyperreal.mips1


object ShitInstructions {

  val SLL =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.regs(rt) << shamt )
    }
  val SLLV =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.regs(rt) << (cpu.regs(rt)&0x1F) )
    }
  val SRA =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.regs(rt) >> shamt )
    }
  val SRAV =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.regs(rt) >> (cpu.regs(rt)&0x1F) )
    }
  val SRL =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.regs(rt) >>> shamt )
    }
  val SRLV =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.regs(rt) >>> (cpu.regs(rt)&0x1F) )
    }

}