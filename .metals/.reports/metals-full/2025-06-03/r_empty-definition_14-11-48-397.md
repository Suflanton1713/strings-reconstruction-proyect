error id: file:///C:/Users/Usuario/OneDrive/Escritorio/Universidad/4%20Semestre/FDP%20funcional%20y%20concurrente/Proyecto/strings-reconstruction-proyect/src/main/scala/ReconstCadenasPar/package.scala:filter.
file:///C:/Users/Usuario/OneDrive/Escritorio/Universidad/4%20Semestre/FDP%20funcional%20y%20concurrente/Proyecto/strings-reconstruction-proyect/src/main/scala/ReconstCadenasPar/package.scala
empty definition using pc, found symbol in pc: filter.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -common/candidatos/filter.
	 -common/candidatos/filter#
	 -common/candidatos/filter().
	 -scala/collection/parallel/CollectionConverters.candidatos.filter.
	 -scala/collection/parallel/CollectionConverters.candidatos.filter#
	 -scala/collection/parallel/CollectionConverters.candidatos.filter().
	 -Oraculo.candidatos.filter.
	 -Oraculo.candidatos.filter#
	 -Oraculo.candidatos.filter().
	 -ArbolSufijos.candidatos.filter.
	 -ArbolSufijos.candidatos.filter#
	 -ArbolSufijos.candidatos.filter().
	 -candidatos/filter.
	 -candidatos/filter#
	 -candidatos/filter().
	 -scala/Predef.candidatos.filter.
	 -scala/Predef.candidatos.filter#
	 -scala/Predef.candidatos.filter().
offset: 1182
uri: file:///C:/Users/Usuario/OneDrive/Escritorio/Universidad/4%20Semestre/FDP%20funcional%20y%20concurrente/Proyecto/strings-reconstruction-proyect/src/main/scala/ReconstCadenasPar/package.scala
text:
```scala
import common._
import scala.collection.parallel.CollectionConverters._
import Oraculo._
import ArbolSufijos._

package object ReconstCadenasPar {
  // Ahora versiones paralelas

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa paralelismo de tareas
    ???
  }

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa paralelismo de tareas y/o datos
    def extender(k: Int, SCk: Set[Seq[Char]]): Set[Seq[Char]] = {
    if (k > n) SCk
    else {
      val candidatos = for {
        w <- SCk
        a <- alfabeto
      } yield w :+ a

      val filtrados =
        if (candidatos.size >= umbral)
          candidatos.toVector.par.filter(o).seq.toSet // <--- esta parte cambia
        else
          candidatos.fil@@ter(o).toSet


      extender(k + 1, filtrados)
    }
    }

    extender(1, Set(Seq())).find(_.length == n).getOrElse(Seq())
 }

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa paralelismo de tareas y/o datos

    ???
  }

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa paralelismo de tareas y/o datos

    ???
  }

  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa arboles de sufijos para guardar Seq[Seq[Char]]
    // Usa paralelismo de tareas y/o datos

    ???
  }
}

```


#### Short summary: 

empty definition using pc, found symbol in pc: filter.