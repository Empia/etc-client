package io.iohk.ethereum.vm

/**
  * Marker trait for errors that may occur during program execution
  */
sealed trait ProgramError
case class  InvalidOpCode(code: Byte) extends ProgramError {
  override def toString: String =
    f"${getClass.getSimpleName}(0x${code.toInt & 0xff}%02x)"
}
case object OutOfGas extends ProgramError
case class InvalidJump(dest: Int) extends ProgramError {
  override def toString: String =
    f"${getClass.getSimpleName}(0x$dest%04x)"
}

sealed trait StackError extends ProgramError
case object StackOverflow extends StackError
case object StackUnderflow extends StackError
