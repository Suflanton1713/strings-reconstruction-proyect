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
    // Devuelve true si la secuencia s es reconocida por el trie t, y false si no.
    (s, t) match {
      case (Nil, Nodo(_, marcada, _)) => marcada
      case (Nil, Hoja(_, marcada))    => marcada
      case (h +: tail, Nodo(_, _, hijos)) =>
        hijos.find(raiz(_) == h) match {
          case Some(sub) => pertenece(tail, sub)
          case None => false
        }
      case (h +: tail, Hoja(c, _)) =>
        h == c && tail.isEmpty
      case _ => false
    }
  }

  def adicionar(s: Seq[Char], t: Trie): Trie = {
    // Adiciona una secuencia de uno o mas caracteres a un trie
    def aux(seq: Seq[Char], trie: Trie): Trie = (seq, trie) match {
          case (Nil, Nodo(c, _, hijos)) =>
            Nodo(c, true, hijos)
          case (Nil, Hoja(c, _)) =>
            Hoja(c, true)

          case (h +: rest, Nodo(c, marcada, hijos)) =>
            val (found, others) = hijos.partition(tr => raiz(tr) == h)
            val newChild = if (found.isEmpty) {
              aux(rest, Nodo(h, rest.isEmpty, Nil))
            } else {
              aux(rest, found.head)
            }
            Nodo(c, marcada, newChild :: others.filterNot(_ == found.headOption.getOrElse(null)))

          case (h +: rest, Hoja(c, marcada)) =>
            val newChild = aux(rest, Nodo(h, rest.isEmpty, Nil))
            Nodo(c, marcada, List(newChild))
        }
        aux(s, t)
  }

  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // dada una secuencia no vacia de secuencias devuelve el arbol de sufijos asociado a esas secuencias
    val raizTrie: Trie = Nodo(' ', false, Nil)
    ss.flatMap(s => s.indices.map(i => s.drop(i))).foldLeft(raizTrie)((acc, suf) => adicionar(suf, acc))
  }
}
