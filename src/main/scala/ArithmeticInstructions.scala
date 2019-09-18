package xyz.hyperreal.mips1


object ArithmeticInstructions {

  def add( a: Int, b: Int, c: Int, cpu: CPU ) = {
    val sum64 = a + b.toLong
    val sum32 = sum64.toInt

    if ((sum64 >> 63) * (sum32 >> 31) < -1)
      cpu.exception("overflow")
    else
      cpu.put( c, sum32 )
  }

  val ADD =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = add( rs, rt, rd, cpu )
    }
  val ADDI =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = add( rs, imm, rt, cpu )
    }
  val ADDIU =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = cpu.put( rt, rs + imm )
    }
  val ADDU =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = cpu.put( rd, rs + rt )
    }

}