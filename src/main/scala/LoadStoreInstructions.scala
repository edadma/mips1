package xyz.hyperreal.mips1


object LoadStoreInstructions {

  val LB =
    new ITypeInstruction {
      override def perform(cpu: CPU, inst: Int ) =
        cpu.put( rt(inst), cpu.mem.readByte(cpu.regs(rs(inst)) + imm(inst)) )
    }

}