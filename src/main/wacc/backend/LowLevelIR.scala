package wacc.backend

object Arm {

   /* Addressing modes */
   sealed trait Address
   case class AddrRegOffset(baseAddr: PhysicalReg, offset: PhysicalReg) extends  Address
   case class AddrImmOffset(baseAddr: PhysicalReg, offset: Immediate) extends Address
   case class AddrRegShift(base: PhysicalReg,
                              offset: PhysicalReg,
                              shift: ShiftAmounts.ShiftAmount) extends Address
   case class AddrPreIndex(baseAddr: PhysicalReg,
                              offset: Immediate) extends Address /* E.g. [sp, #-16]! */
   case class AddrPostIndex(baseAddr: PhysicalReg,
                              offset: Immediate) extends Address /* E.g. [sp], #16 */
   case class AddrLabel(name: String) extends Address

   /* An immediate value */
   case class Immediate(value: Int)

   /* ---------------------------------------------------------------------------------------------- */

   sealed trait Instruction

   /* Arithmetic and Logic */

   /* PhysicalReg arithmetic instructions */
   case class AddReg(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: PhysicalReg) extends Instruction
   case class SubReg(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: PhysicalReg) extends Instruction
   case class MulReg(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: PhysicalReg) extends Instruction
   case class DivReg(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: PhysicalReg) extends Instruction
   case class Neg(size: Size,
                     reg: PhysicalReg,
                     src: PhysicalReg) extends Instruction
   case class LslReg(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: PhysicalReg) extends Instruction
   case class LsrReg(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: PhysicalReg) extends Instruction
   case class AsrReg(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: PhysicalReg) extends Instruction

   /* Immediate arithmetic instructions */
   case class AddImm(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: Immediate) extends Instruction
   case class SubImm(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: Immediate) extends Instruction
   case class LslImm(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: Immediate) extends Instruction
   case class LsrImm(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg, src2: Immediate) extends Instruction
   case class AsrImm(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: Immediate) extends Instruction

   /* Overflow checking arithmetic */
   case class AddsReg(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: PhysicalReg) extends Instruction

   case class SubsReg(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: PhysicalReg) extends Instruction

   case class SubsImm(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: Immediate) extends Instruction

   case class AddsImm(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: Immediate) extends Instruction

   case class Negs(size: Size,
                     reg: PhysicalReg,
                     src: PhysicalReg) extends Instruction

   /* Register logical instructions */
   case class OrReg(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: PhysicalReg) extends Instruction
   case class AndReg(size: Size,
                        reg: PhysicalReg,
                        src1: PhysicalReg,
                        src2: PhysicalReg) extends Instruction

   /* Immediate logical instructions */
   case class OrImm(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: Immediate) extends Instruction
   case class AndImm(size: Size,
                     reg: PhysicalReg,
                     src1: PhysicalReg,
                     src2: Immediate) extends Instruction

   /* Data Movement*/
   case class MovReg(size: Size, reg: PhysicalReg, src: PhysicalReg) extends Instruction
   case class Movz(size: Size, dest: PhysicalReg, imm: Immediate) extends Instruction
   case class Movk(size: Size,
                     dest: PhysicalReg,
                     imm: Immediate,
                     shift: ShiftAmounts.ShiftAmount) extends Instruction

   case class Load(size: Size, reg: PhysicalReg, src: Address) extends Instruction
   case class Store(size: Size, reg: PhysicalReg, src: Address) extends Instruction
   case class Ldur(size: Size, reg: PhysicalReg, src: Address) extends Instruction
   case class Stur(size: Size, reg: PhysicalReg, src: Address) extends Instruction
   case class LoadByte(size: Size, dest: PhysicalReg, addr: Address) extends Instruction
   case class StoreByte(size: Size, src: PhysicalReg, addr: Address) extends Instruction
   case class Lea(destReg: PhysicalReg, label: AddrLabel) extends Instruction

   /* Conditionals */
   case class CmpReg(size: Size, src1: PhysicalReg, src2: PhysicalReg) extends Instruction
   case class CmpImm(size: Size, src1: PhysicalReg, src2: Immediate) extends Instruction
   case class BranchUncond(label: AddrLabel) extends Instruction
   case class BranchCond(cond: Condition, label: AddrLabel) extends Instruction
   case class LabelDef(label: AddrLabel) extends Instruction

   /* Functions */
   case class Bl(label: AddrLabel) extends Instruction
   case object Ret extends Instruction

   /* Stack operations */
   case class StorePair(size: Size, 
                           src1: PhysicalReg, 
                           src2: PhysicalReg, 
                           addr: Address) extends Instruction

   case class LoadPair(size: Size, 
                        dest1: PhysicalReg, 
                        dest2: PhysicalReg, 
                        addr: Address) extends Instruction

   /* Alias instructions (real aarch64 but they're convenient wrappers
      over multiple other aarch64 instructions */

   /* w1, w2, w3:  dest = w3 - (w1 * w2)
      `MSub` makes the mod operator much easier */
   case class MSub(size: Size, dest: PhysicalReg, mult1: PhysicalReg,
                     mult2: PhysicalReg,
                     sub: PhysicalReg) extends Instruction

   /* Sets the value of the condition flag `cond` to the destination PhysicalReg */
   case class CSet(size: Size, dest: PhysicalReg, cond: Condition) extends Instruction

   /* Easily check for overflow on multiplication as it is "multiply long" meaning
      we can check the top 32-bits of the 64-bit result and if it's not zero, then there
      was overflow as WACC uses 32-bit ints.
      
      The S stands for sign-extend so it's going to a bigger result size instead of truncating.

      It's always 32 bit regs in and 64 bit reg out by design so `Size` is not needed
   */
   case class SMull(dest: PhysicalReg, src1: PhysicalReg, src2: PhysicalReg) extends Instruction

   /* Sign extend a word to promote a number from a 32-bit regster
      to a 64-bit register

      Always 32 bit to 64 bit so no `Size` needed
   */
   case class Sxtw(dest: PhysicalReg, src: PhysicalReg) extends Instruction

   /* Branch if the test register is equal to 0 (prevents cmp + branch) */
   case class Cbz(size: Size, reg: PhysicalReg, label: AddrLabel) extends Instruction  

   /* Branch if the test register is not equal to 0 (prevents cmp + branch) */
   case class Cbnz(size: Size, reg: PhysicalReg, label: AddrLabel) extends Instruction 

   /* Conditional Select */
   case class Csel(size: Size,
                   dest: PhysicalReg,
                   src1: PhysicalReg,
                   src2: PhysicalReg,
                   cond: Condition) extends Instruction

   /* ------------------------------------------------------------------------------------------- */

   case class DataDirective(label: AddrLabel, storedValue: String)
   case class Program(data: List[DataDirective], body: List[Instruction])
}
