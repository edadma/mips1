package xyz.hyperreal.mips1


object ArithmeticInstructions {

  val ADDI =
    new ITypeInstruction {
      override def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) = {
        val sum64 = rs + imm.toLong
        val sum32 = sum64.toInt

        if ((sum64 >> 63) * (sum32 >> 31) < -1)
          cpu.exception("overflow")
        else
          cpu.put(rt, rs + imm)
      }
    }

}