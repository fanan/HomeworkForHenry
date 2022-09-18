package utils

object WithClose {
  def apply[A <: AutoCloseable, B](closeable: A)(f: A => B): B = {
    val result = f(closeable)
    closeable.close()
    result
  }
}
