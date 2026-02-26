package wacc.backend

import wacc.frontend.KnownType
import wacc.frontend.SemanticType

object MemoryLayout {
    val NULL_PTR = 0 /* Null pointer (pair) representation */
    val PAIR_SLOT_SIZE = 8 /* Store pair elemens in 8 byte sized slots */
    val PAIR_HEAP_SIZE_B = 16 /* Pairs are always stored in 16 bytes in heap (each slot 8 bytes) */
    val ARRAY_LEN_HEADER_SIZE_B = 4 /* Size of the integer length header preceding array elements. */
    val RETURN_REG_ID = -1 /* Identifier for the return register */
}

object Constants {
    val TRUE = 1
    val FALSE = 0
}

object AARch64Limits {
    val ARM_NUM_REGS_FOR_FUNC_ARGS = 8 /* Arm allocates parameters into registers for the first 8 arguments */
    val ARM_STACK_ALIGNMENT_BYTES = 16 /* ARM requires that the stack MUST be aligned to 16 bytes */

    /* ARM immediate signed offset bounds allowed by the STR/LDR/STUR/LDUR instructions */
    val ARM_IMMEDIATE_SIGNED_OFFSET_LOW = -256
    val ARM_IMMEDIATE_SIGNED_OFFSET_HIGH = 255
    val ARM_IMMEDIATE_UNSIGNED_HIGH = 65535
}



/**
  * Size in bytes for registers/memory operations
  */
enum Size {
    case B, W, X
}

/**
  * Companion object of `Size` enum
  */
object Size {

    /* Get the numerical value of the `Size` class */
    def bytes(s: Size): Int = s match {
        case `B` => 1
        case `W` => 4
        case `X` => 8
    }

    /* Get the size of a type */
    def fromType(typ: KnownType): Size = typ match {
        case KnownType.Char | KnownType.Bool => Size.B
        case KnownType.Int => Size.W
        case KnownType.String | _: KnownType.Array | _: KnownType.Pair => Size.X
    }

    /* Get the size of a semantic type */
    def fromSemType(typ: SemanticType): Size = typ match {
        case t: KnownType => fromType(t)
        case wacc.frontend.PairAny | wacc.frontend.Null | wacc.frontend.EmptyArray => Size.X
        case _ => Size.X /* we will not have `?` in the typed metadata but in case... */
    }
}

/**
  * The size of a shift in an instruction like MOVK, and in
  * shifted register addressing .
  */
object ShiftAmounts {
    opaque type ShiftAmount = Int
    val _0: ShiftAmount = 0
    val _16: ShiftAmount = 16
    val _32: ShiftAmount = 32 
    val _48: ShiftAmount = 48

    def value(shift: ShiftAmount): Int = shift
}

/**
  * The conditions for branching instructions
  */
enum Condition {
    case EQ, NE, LT, LE, GT, GE, HI, VS
}

/**
  * The positions in a pair
  */
enum PairPos {
    case Fst
    case Snd
}

/**
  * The type of heap object being freed
  */
enum FreeType {
    case Pair
    case Array
}
