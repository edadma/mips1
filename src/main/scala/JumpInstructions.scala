package xyz.hyperreal.mips1


object JumpInstructions {

  val J =
    new JTypeInstruction {
      def perform( cpu: CPU, targ: Int ) =
        cpu.delay0( cpu => cpu.pc = (cpu.pc&0xF0000000)|targ )
    }
  val JAL =
    new JTypeInstruction {
      def perform( cpu: CPU, targ: Int ) = {
        cpu.regs(31) = cpu.pc + 8
        cpu.delay0( cpu => cpu.pc = (cpu.pc&0xF0000000)|targ )
      }
    }
  val JALR =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) = {
        cpu.put( rd, cpu.pc + 8 )
        cpu.delay1( rs, (cpu, rsv) => if ((rsv & 0x3) != 0) cpu.exception("address") else cpu.pc = rsv )
      }
    }
  val JR =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) =
        cpu.delay1( rs, (cpu, dest) => if ((dest & 0x3) != 0) cpu.exception("address") else cpu.pc = dest )
    }

}