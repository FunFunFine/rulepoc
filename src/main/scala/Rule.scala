import Tuple.*
import scala.compiletime.*
import Tuple.{Map => TMap}
import scala.quoted.*
import izumi.reflect.Tag


trait Rule[P, S, D <: Tuple]:
  type Data = D
  def status: S
  def isFitPayment(payment: P): Boolean
  def isFitClient(payment: P): D => Boolean

enum PaymentStatus:
  case Deny
  case Ok
  case Probe
  case Wait

type PaymentRule[Data <: Tuple] = Rule[Payment, PaymentStatus, Data]

case class Payment(id: Long, amount: Double)

case object LimitRule extends PaymentRule[EmptyTuple]:
  def status: PaymentStatus = PaymentStatus.Deny
  def isFitPayment(payment: Payment): Boolean = payment.amount > 600000
  def isFitClient(payment: Payment): EmptyTuple => Boolean = _ => true


case object ColorRule extends PaymentRule[Int *: EmptyTuple]:
  def status: PaymentStatus = PaymentStatus.Deny
  def isFitPayment(payment: Payment): Boolean = true
  def isFitClient(payment: Payment): (Int *: EmptyTuple) => Boolean = 
      case i *: EmptyTuple  => (2 == i)


case object ComplexRule extends PaymentRule[(Int, String)]:
  def status: PaymentStatus = PaymentStatus.Deny
  def isFitPayment(payment: Payment): Boolean = true
  def isFitClient(payment: Payment): ((Int, String)) => Boolean = 
      (i, s) => (i.toString == s.take(1))

trait Loader[A]:
  def load(p: Payment): A


object StringLoader extends Loader[String]:
  def load(p: Payment): String = p.id.toString


object IntLoader extends Loader[Int]:
  def load(p: Payment): Int = p.id.toInt


case class LoadTag[D]():
  override def toString = s"LoadTag of "




object Rule:
  inline def tags[D <: Tuple]: D TMap DataTag = summonAll[D TMap DataTag] 


val rules = (LimitRule, ComplexRule, ColorRule)

type RTS[RS <: Tuple] <: Tuple = RS match
  case PaymentRule[d] *: rs => (d TMap DataTag) AddAllIfNew RTS[rs]
  case _ *: rs =>  RTS[rs]
  case EmptyTuple => EmptyTuple

inline def rts[RS <: Tuple](rs:RS): RTS[RS] = 
  inline rs match
    case cons : (PaymentRule[d] *: rs) => addAllIfNew((Rule.tags[d]), rts(cons.tail))
    case cons : (_ *: rs) => rts(cons.tail)
    case _ : EmptyTuple => EmptyTuple



val rt = rts(rules)

val loaders = (StringLoader, IntLoader)


val payment = Payment(40403L, 40_000)
// val preliminaryRules = rules.toList


@main
def main = println(rt)
//  println((LimitRule.dataTags))



 