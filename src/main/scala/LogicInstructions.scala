package xyz.hyperreal.mips1


object LogicInstructions {

  val AND =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, rs & rt )
    }
  val ANDI =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = cpu.put( rt, rs & imm )
    }


}