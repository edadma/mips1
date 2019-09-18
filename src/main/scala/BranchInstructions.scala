package xyz.hyperreal.mips1


object BranchInstructions {

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

}