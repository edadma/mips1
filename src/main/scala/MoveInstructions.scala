package xyz.hyperreal.mips1


object MoveInstructions {

  val MFHI =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.hi )
    }
  val MFLO =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, cpu.lo )
    }
  val MTHI =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.hi = cpu.regs(rd)
    }
  val MTLO =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.lo = cpu.regs(rd)
    }

}