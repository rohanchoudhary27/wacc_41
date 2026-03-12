package wacc.backend.unit

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import wacc.backend.*
import wacc.backend.TAC

/**
* Unit tests for the high level internal representation
*/
class HighLevelIRTest extends AnyFlatSpec with Matchers {

    "The High level IR should" should "validate register size on binary operations" in {
        val reg32 = TAC.VirtualReg(1, Size.W)
        val reg64 = TAC.VirtualReg(2, Size.X)
        val imm = TAC.Immediate(5)

        /* dest, src1 mismatch */
        an [IllegalArgumentException] should be thrownBy {
            TAC.Sub(reg64, reg32, imm) 
        }

        /* src1, src2 mismatch */
        an [IllegalArgumentException] should be thrownBy {
            TAC.Sub(reg64, reg64, reg32) 
        }

        noException should be thrownBy {
            TAC.Sub(reg64, reg64, imm)
        }

    }

    it should "validate register size on unary operations" in {
        val reg32 = TAC.VirtualReg(1, Size.W)
        val reg64 = TAC.VirtualReg(2, Size.X)

        /* Mismatch size */
        an [IllegalArgumentException] should be thrownBy {
            TAC.Not(reg64, reg32) 
        }

        /* Mismatch size */
        an [IllegalArgumentException] should be thrownBy {
            TAC.Not(reg32, reg64)
        }

        noException should be thrownBy {
            TAC.Not(reg32, reg32)
        }
    }

    it should "validate register size on pointer operations" in {
        val reg32 = TAC.VirtualReg(1, Size.W)
        val reg64 = TAC.VirtualReg(2, Size.X)

        /* Pointer can't be 32 bit */
        an [IllegalArgumentException] should be thrownBy {
            TAC.PairLoad(reg32, PairPos.Fst, reg32)
        }

        noException should be thrownBy {
            TAC.PairStore(reg64, PairPos.Snd, reg64)
        }
    }

    it should "validate array index size on array operations" in {
        val reg32 = TAC.VirtualReg(1, Size.W)
        val reg64 = TAC.VirtualReg(2, Size.X)

        /* Index must be 32 bit */
        an [IllegalArgumentException] should be thrownBy {
            TAC.ArrayLoad(reg64, reg64, reg32)
        }

        noException should be thrownBy {
            TAC.ArrayLoad(reg64, reg32, reg32)
        }
    }

    it should "validate register size on comparison operations" in {
        val reg32 = TAC.VirtualReg(1, Size.W)
        val reg64 = TAC.VirtualReg(2, Size.X)

        /* two registers must be same size */
        an [IllegalArgumentException] should be thrownBy {
            TAC.Cmp(reg32, reg64)
        }

        noException should be thrownBy {
            TAC.Cmp(reg32, reg32)
        }
    }

    it should "validate address base register size" in {
        val reg32 = TAC.VirtualReg(1, Size.W)
        val reg64 = TAC.VirtualReg(2, Size.X)
        val imm = TAC.Immediate(4)

        /* Base register must be 64 bit*/
        an [IllegalArgumentException] should be thrownBy {
            TAC.AddrImmOffset(reg32, imm)
        }

        noException should be thrownBy {
            TAC.AddrRegOffset(reg64, reg32)
        }
    }

    it should "validate record pointer size on RecordLoad" in {
        val reg32 = TAC.VirtualReg(1, Size.W)
        val reg64 = TAC.VirtualReg(2, Size.X)

        /* Record pointer must be 64 bit */
        an [IllegalArgumentException] should be thrownBy {
            TAC.RecordLoad(reg32, 0, reg64)
        }

        noException should be thrownBy {
            TAC.RecordLoad(reg64, 0, reg64)
        }
    }

    it should "validate record pointer size on RecordStore" in {
        val reg32 = TAC.VirtualReg(1, Size.W)
        val reg64 = TAC.VirtualReg(2, Size.X)

        /* Record pointer must be 64 bit */
        an [IllegalArgumentException] should be thrownBy {
            TAC.RecordStore(reg32, 0, reg64)
        }

        noException should be thrownBy {
            TAC.RecordStore(reg64, 0, reg64)
        }
    }

    it should "allow RecordLoad with different field indices" in {
        val reg64 = TAC.VirtualReg(1, Size.X)
        val dest = TAC.VirtualReg(2, Size.X)

        noException should be thrownBy {
            TAC.RecordLoad(reg64, 0, dest)
        }
        noException should be thrownBy {
            TAC.RecordLoad(reg64, 1, dest)
        }
        noException should be thrownBy {
            TAC.RecordLoad(reg64, 5, dest)
        }
    }

    it should "allow RecordStore with immediate source" in {
        val reg64 = TAC.VirtualReg(1, Size.X)

        noException should be thrownBy {
            TAC.RecordStore(reg64, 0, TAC.Immediate(42))
        }
    }
}
