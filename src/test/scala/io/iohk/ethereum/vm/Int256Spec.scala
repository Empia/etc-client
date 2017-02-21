package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.vm.Generators._
import io.iohk.ethereum.vm.Int256Like._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Int256Spec extends FunSuite with PropertyChecks {

  val Modulus: BigInt = BigInt(2).pow(Size * 8)

  val Zero: Int256 = Int256(BigInt(0))

  val MaxSignedValue: BigInt = BigInt(2).pow(Size * 8 - 1) - 1

  val specialNumbers = Array(BigInt(-1), BigInt(0), BigInt(1), Modulus - 1, 1 - Modulus, 2 - Modulus)

  val pairs: Array[(BigInt, BigInt)] = specialNumbers
    .combinations(2)
    .map {case Array(n1, n2) => n1 -> n2}
    .toArray

  val specialCases = Table(("n1", "n2"), pairs: _*)

  def toSignedBigInt(n: BigInt): BigInt = {
    val m = (n % Modulus + Modulus) % Modulus
    if (m > MaxSignedValue) m - Modulus else m
  }

  test("&") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) & Int256(n2)) == Int256(n1 & n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) & Int256(n2)) == Int256(n1 & n2))
    }
  }

  test("|") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) | Int256(n2)) == Int256(n1 | n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) | Int256(n2)) == Int256(n1 | n2))
    }
  }

  test("^") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) ^ Int256(n2)) == Int256(n1 ^ n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) ^ Int256(n2)) == Int256(n1 ^ n2))
    }
  }

  test("~") {
    forAll(bigIntGen) { n: BigInt =>
      assert(~Int256(n) == Int256(~n))
    }
    forAll(Table("n", specialNumbers: _*)) { n: BigInt =>
      assert(~Int256(n) == Int256(~n))
    }
  }

  test("negation") {
    forAll(bigIntGen) {(n: BigInt) =>
      assert(-Int256(n) == Int256(-n))
    }
    forAll(Table(("n"), specialNumbers: _*)) {(n: BigInt) =>
      assert(-Int256(n) == Int256(-n))
    }
    assert(-Int256(BigInt(1)) == Int256(BigInt(-1)))
  }

  test("+") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) + Int256(n2) == Int256(n1 + n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) + Int256(n2) == Int256(n1 + n2))
    }
  }

  test("-") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) - Int256(n2) == Int256(n1 - n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) - Int256(n2) == Int256(n1 - n2))
    }
  }

  test("*") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) * Int256(n2) == Int256(n1 * n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) * Int256(n2) == Int256(n1 * n2))
    }
  }

  test("div") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((Int256(n1) div Int256(n2)) == Int256(toSignedBigInt(n1) / toSignedBigInt(n2)))
      }
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      whenever(n1 > 0 && n2 > 0) {
        assert((Int256(n1) div Int256(n2)) == Int256(toSignedBigInt(n1) / toSignedBigInt(n2)))
      }
    }
    assert((Int256(1) div Zero) == Zero)
  }

  test("%") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((Int256(n1) % Int256(n2)) == Int256(toSignedBigInt(n1) % toSignedBigInt(n2)))
      }
    }
    assert((Int256(-1) % Int256(Modulus - 1)) == Zero)
    assert((Int256(1) % Zero) == Zero)
  }

  test("addmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(Int256(n1).addmod(Int256(n2), Int256(n3)) == Int256((toSignedBigInt(n1) + toSignedBigInt(n2)) % toSignedBigInt(n3)))
      }
    }
    assert(Int256(42).addmod(Int256(42), Zero) == Zero)
  }

  test("mulmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(Int256(n1).mulmod(Int256(n2), Int256(n3)) == Int256((toSignedBigInt(n1) * toSignedBigInt(n2)) % toSignedBigInt(n3)))
      }
    }
    assert(Int256(42).mulmod(Int256(42), Zero) == Zero)
  }

  test("**") { // TODO
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert(Int256(n1) ** Int256(n2) == Int256(toSignedBigInt(n1).modPow(toSignedBigInt(n2), Modulus)))
      }
    }
  }

  test("signExtend") {
    val testData = Table[Int256, Int256, Int256](
      ("word", "extension", "result"),
      (Int256(42), Int256(3), Int256(42)),
      (Int256(42), Int256(1), Int256(42)),
      (Int256(42), Int256(-1), Int256(42)),
      (Int256(42), Int256(0), Int256(42)),
      (Int256(42), Int256(Size), Int256(42)),
      (Int256(42), Int256(Size + 1), Int256(42)),
      (Int256(-42), Int256(Size), Int256(-42)),
      (Int256(-42), Int256(Size + 1), Int256(-42)),
      (Int256(-42), Int256(-11), Int256(-42)),
      (Int256(-1), Int256(1), Int256(-0xff01)),
      (Int256(0x1a81ff), Int256(1), Int256(Array.fill[Byte](30)(-1) ++ Array[Byte](-127, -1))))

    forAll(testData) { (word, extension, result) =>
      assert((word).signExtend(extension) == result)
    }
  }

  test("getByte") {
    val dw1 = Int256(ByteString((100 to 131).map(_.toByte).toArray))

    val testData = Table[Int256, Int256, Int256](
      ("word", "idx", "result"),
      (Int256(42), Int256(-1), Zero),
      (Int256(42), Int256(Size), Zero),
      (Int256(42), Int256(Size + 1), Zero),
      (Int256(1), Int256(287), Zero),
      (dw1, Int256(0), Int256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ 100.toByte)),
      (dw1, Int256(1), Int256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ 101.toByte)),
      (dw1, Int256(30), Int256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ -126.toByte)),
      (dw1, Int256(31), Int256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ -125.toByte)),
      (Int256(ByteString(Array.fill[Byte](32)(-50))), Int256(8), Int256(ByteString(-50))))

    forAll(testData) { (a, b, result) =>
      assert(a.getByte(b) == result)
    }
  }

  test("intValue") {
    assert(specialNumbers.map(Int256(_).intValue).toSeq == Seq(Int.MaxValue, 0, 1, Int.MaxValue, 1, 2))
  }

  test("comparison") {
    type CFUINT = (Int256, Int256) => Boolean
    type CFBI = (BigInt, BigInt) => Boolean
    case class Cmp(uint: CFUINT, bi: CFBI)

    val cmpFuncInt256 = Seq[CFUINT](_ > _, _ >= _, _ < _, _ <= _)
    val cmpFuncBigInt = Seq[CFBI](_ > _, _ >= _, _ < _, _ <= _)
    val comparators = cmpFuncInt256.zip(cmpFuncBigInt).map(Cmp.tupled)

    val uin256Gen = bigIntGen.map(Int256(_))

    forAll(Table("comparators", comparators: _*)) { cmp =>
      forAll(bigIntGen, bigIntGen) { (a, b) =>
        val (x, y) = (Int256(a), Int256(b))
        assert(cmp.uint(x, y) == cmp.bi(x.n, y.n))
      }
      forAll(specialCases) { (a, b) =>
        val (x, y) = (Int256(a), Int256(b))
        assert(cmp.uint(x, y) == cmp.bi(x.n, y.n))
      }
    }
  }

  test("Passing too long ByteString should throw an exception") {
    assertThrows[IllegalArgumentException] {
      Int256(ByteString(Array.fill(Size + 1)(1.toByte)))
    }
  }

  test("Int256 converted to a byte array should always have length 32 bytes") {
    forAll(bigIntGen) { n =>
      assert(Int256(n).bytes.size == 32)
    }
    // regression
    assert(Int256(BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639935")).bytes.size == 32)
  }

  test("2-way bytes conversion") {
    forAll(getInt256Gen()) { x =>
      val y = Int256(x.bytes)
      assert(x.bytes === y.bytes)
    }

    forAll(getByteStringGen(0, 32)) { xs =>
      val ys = Int256(xs).bytes
      assert(xs.dropWhile(_ == 0) === ys.dropWhile(_ == 0))
    }
  }

  test("byteSize") {
    val table = Table[BigInt, Int](("x", "expected"),
                                   (0, 0),
                                   (1, 1),
                                   (255, 1),
                                   (256, 2),
                                   (65535, 2),
                                   (65536, 3),
                                   (BigInt(2).pow(256) - 1, 1),
                                   (BigInt(2).pow(256), 0))
    forAll(table) { (x, expected) =>
      assert(Int256(x).byteSize === expected)
    }

    forAll(getInt256Gen(min = 1)) { x =>
      import math._
      val byteSize = 1 + floor(log(x.n.doubleValue) / log(256)).toInt
      assert(x.byteSize === byteSize)
    }
  }
}
