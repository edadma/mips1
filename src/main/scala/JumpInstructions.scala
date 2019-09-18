package xyz.hyperreal.mips1


object JumpInstructions {

  val BEQ =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay2( rs, rt, (cpu, rsv, rtv) => if (rsv == rtv) cpu.pc += imm<<2 )
    }
  val BGEZ =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => if (rsv >= 0) cpu.pc += imm<<2 )
    }
  val BGEZAL =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => {cpu.put( 31, cpu.pc + 4 ); if (rsv >= 0) cpu.pc += imm<<2} )
    }
  val BGTZ =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => if (rsv > 0) cpu.pc += imm<<2 )
    }
  val BLEZ =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => if (rsv <= 0) cpu.pc += imm<<2 )
    }
  val BLTZ =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => if (rsv < 0) cpu.pc += imm<<2 )
    }
  val BLTZAL =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay1( rs, (cpu, rsv) => {cpu.put( 31, cpu.pc + 4 ); if (rsv < 0) cpu.pc += imm<<2} )
    }
  val BNE =
    new ITypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, imm: Int ) =
        cpu.delay2( rs, rt, (cpu, rsv, rtv) => if (rsv != rtv) cpu.pc += imm<<2 )
    }
  val J =
    new JTypeInstruction {
      def perform( cpu: CPU, targ: Int ) =
        cpu.delay0( cpu => cpu.pc = (cpu.pc&0xF0000000)|targ )
    }
  val JAL =
    new JTypeInstruction {
      def perform( cpu: CPU, targ: Int ) =
        cpu.delay0( cpu => {cpu.put( 31, cpu.pc + 4 ); cpu.pc = (cpu.pc&0xF0000000)|targ} )
    }
  val JR =
    new RTypeInstruction {
      def perform( cpu: CPU, rs: Int, rt: Int, rd: Int, shamt: Int, func: Int ) =
        cpu.delay1( rs, (cpu, dest) => if ((dest & 0x3) != 0) cpu.exception("address") else cpu.pc = dest )
    }

}