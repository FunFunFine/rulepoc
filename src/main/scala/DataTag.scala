import izumi.reflect.Tag
import scala.compiletime.*

trait DataTag[D]:
  type Data = D
  def tag: Tag[D]
  override def toString: String =  s"Tag for $tag"

object DataTag:
  given [A: Tag]: DataTag[A] with
    val tag = summon[Tag[A]]

