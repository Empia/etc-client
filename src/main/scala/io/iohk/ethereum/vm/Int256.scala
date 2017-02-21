package io.iohk.ethereum.vm

import akka.util.ByteString

object Int256Like {

  val Size: Int = 32

  val Zeros: ByteString = ByteString(Array.fill[Byte](Size)(0))

  val Modulus: BigInt = BigInt(2).pow(Size * 8)

}

trait Int256Like[T <: Int256Like[T]] extends Ordered[T] {

  import Int256Like._

  def n: BigInt

  protected def fromBigInt(n: BigInt): T

  protected def fromByteString(n: ByteString): T

  /** Converts a BigInt to a ByteString.
   *  Output ByteString is padded with 0's from the left side up to MaxLength bytes.
   */
  lazy val bytes: ByteString = {
    val bs: ByteString = ByteString(n.toByteArray).takeRight(Size)
    val padLength: Int = Size - bs.length
    if (padLength > 0)
      Zeros.take(padLength) ++ bs
    else
      bs
  }

  def &(that: T): T = fromBigInt(this.n & that.n)

  def |(that: T): T = fromBigInt(this.n | that.n)

  def ^(that: T): T = fromBigInt(this.n ^ that.n)

  def unary_~ : T = fromBigInt(~n)

  def unary_- : T = fromBigInt(-n)

  def +(that: T): T = fromBigInt(this.n + that.n)

  def -(that: T): T = fromBigInt(this.n - that.n)

  def *(that: T): T = fromBigInt(this.n * that.n)

  def div(that: T): T = zeroCheck(that) { fromBigInt(this.n / that.n) }

  def %(that: T): T = zeroCheck(that) { fromBigInt(this.n % that.n) }

  def addmod(that: T, modulus: T): T = zeroCheck(modulus) {
    fromBigInt((this.n + that.n) % modulus.n)
  }

  def mulmod(that: T, modulus: T): T = zeroCheck(modulus) {
    fromBigInt((this.n * that.n) % modulus.n)
  }

  def **(that: T): T = fromBigInt(this.n.modPow(that.n, Modulus))

  def getByte(that: T): T = {
    if (that.n > 31 || that.n < 0)
      fromBigInt(BigInt(0))
    else
      fromByteString(Zeros.take(Size - 1) :+ this.bytes(that.n.toInt))
  }


  protected def zeroCheck(that: T)(result: =>T): T = if (that.n == BigInt(0)) fromBigInt(BigInt(0)) else result

  def compare(that: T): Int = this.n.compare(that.n)

  /**
   * @return an Int with MSB=0, thus a value in range [0, Int.MaxValue]
   */
  def intValue: Int = n.intValue & Int.MaxValue

  /**
   * @return a Long with MSB=0, thus a value in range [0, Long.MaxValue]
   */
  def longValue: Long = n.longValue & Long.MaxValue

  /**
   * Used for gas calculation for EXP opcode. See YP Appendix H.1 (220)
   * For n > 0: (n.bitLength - 1) / 8 + 1 == 1 + floor(log_256(n))
   *
   * @return Size in bytes excluding the leading 0 bytes
   */
  def byteSize: Int = if (this.n == BigInt(0)) 0 else (n.bitLength - 1) / 8 + 1

}

object UInt256 {

  import Int256Like._

  def apply(n: BigInt): UInt256 = new UInt256(fixBigInt(n))

  def apply(bytes: IndexedSeq[Byte]): UInt256 = {
    require(bytes.length <= Size,
            s"Input byte array cannot be longer than $Size: ${bytes.length}")
    UInt256(bytes.foldLeft(BigInt(0)){(n, b) => (n << 8) + (b & 0xff)})
  }

  def apply(int256: Int256): UInt256 = new UInt256(toUnsignedBigInt(int256.n))

  lazy val Zero: UInt256 = UInt256(BigInt(0))

  private def fixBigInt(n: BigInt): BigInt = (n % Modulus + Modulus) % Modulus

  private def toUnsignedBigInt(n: BigInt): BigInt = if (n < 0) n + Modulus else n

}

class UInt256 private (val n: BigInt) extends Int256Like[UInt256] {

  override protected def fromBigInt(n: BigInt): UInt256 = UInt256(n)

  override protected def fromByteString(bytes: ByteString): UInt256 = UInt256(bytes)

  override def equals(that: Any): Boolean = {
    that match {
      case that: UInt256 => this.n.equals(that.n)
      case other => false
    }
  }

  override def hashCode: Int = n.hashCode()

  override def toString: String = f"UInt256(0x$n%02x)"

}

object Int256 {

  import Int256Like._

  // TODO
  def apply(n: BigInt): Int256 = new Int256(toSignedBigInt(fixBigInt(n)))

  def apply(bytes: IndexedSeq[Byte]): Int256 = {
    require(bytes.length <= Size,
            s"Input byte array cannot be longer than $Size: ${bytes.length}")
    Int256(bytes.foldLeft(BigInt(0)){(n, b) => (n << 8) + (b & 0xff)})
  }

  def apply(uint256: UInt256): Int256 = new Int256(toSignedBigInt(uint256.n))

  // TODO remove lazy
  lazy val Zero: Int256 = Int256(BigInt(0))

  private val MaxSignedValue: BigInt = BigInt(2).pow(Size * 8 - 1) - 1

  private def fixBigInt(n: BigInt): BigInt = (n % Modulus + Modulus) % Modulus

  private def toSignedBigInt(n: BigInt): BigInt = if (n > MaxSignedValue) n - Modulus else n

}

class Int256 private (val n: BigInt) extends Int256Like[Int256] {

  import Int256Like._

  override protected def fromBigInt(n: BigInt): Int256 = Int256(n)

  override protected def fromByteString(bytes: ByteString): Int256 = Int256(bytes)

  def signExtend(that: Int256): Int256 = {
    if (that.n < 0 || that.n > 31) {
      this
    } else {
      val idx = that.n.toByte
      val mask: Int = if (this.n.testBit((idx * 8) + 7)) 0xFF else 0x00
      val leftFill: Array[Byte] = Array.fill(Size - idx - 1)(mask.toByte)
      val bs = ByteString(leftFill) ++ this.bytes.takeRight(idx + 1)
      Int256(bs)
    }
  }

  override def equals(that: Any): Boolean = {
    that match {
      case that: Int256 => this.n.equals(that.n)
      case other => false
    }
  }

  override def hashCode: Int = n.hashCode()

  override def toString: String = f"Int256(0x$n%02x)"

}
