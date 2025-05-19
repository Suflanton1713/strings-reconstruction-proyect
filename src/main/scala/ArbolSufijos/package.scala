package object ArbolSufijos {
  // Definiendo otra estructura para manipular Seq[Seq[Char]]
  abstract class Trie
  case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie
  case class Hoja(car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _)    => c
    }
  }

  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, lt) => lt.map(t => raiz(t))
      case Hoja(c, _)      => Seq[Char](c)
    }
  }

  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    s match {
      case s1 +: ss => t match {
        case Node(_, _, hijos) =>
          hijos.exists(node => raiz(node) == s1 && pertenece(ss, node))
        case Head(_, _) => false
      }

      case Seq() => t match {
        case Node(_, marcada, _) => marcada
        case Head(_, marcada)    => marcada
      }
    }
  }


  def adicionar(s: Seq[Char], t: Trie): Trie = {
    if(pertenece(s)){Trie}else{
      s match{
        case s1 +: ss => t match{
          case Node(_, _, hijos)=> hijos.filter(h => raiz(h) == s1).head
          case Head(c,m) => adicionar(ss, Nodo(car = c, marcada = m, hijos = List()))
        }
        case s1 => Head(car = s1, marcada = true)

        case Seq() =>  t 
      }
    }
  }

  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // dada una secuencia no vacia de secuencias devuelve el arbol de sufijos asociado a esas secuencias

    ???
  }
}
