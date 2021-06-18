import Tuple.*
import scala.compiletime.*
import Tuple.{Map => TMap}

type AddIfNew[A, T <: Tuple] <: Tuple = T match
  case EmptyTuple => A *: EmptyTuple
  case A *: t => A *: t
  case h *: t =>  h *: AddIfNew[A, t]

inline def addIfNew[A, T <: Tuple](a: A, t: T): A AddIfNew T =
  inline t match
    case _ : EmptyTuple => a *: EmptyTuple
    case alreadyHere : (A *: t) => alreadyHere
    case notHead : (h *: t) => notHead.head *: addIfNew(a, notHead.tail)


type AddAllIfNew[AT <: Tuple, T <: Tuple] <: Tuple = 
  AT match
    case EmptyTuple => T
    case h *: t => h AddIfNew (AddAllIfNew[t, T])

inline def addAllIfNew[AT <: Tuple , T <: Tuple](at: AT, t: T): AT AddAllIfNew T =
  inline at match
    case _ : EmptyTuple => t
    case cons : (h *: t) => addIfNew(cons.head, addAllIfNew(cons.tail, t))



@main def addall = println(addAllIfNew((1, true),(4L, false)))

    