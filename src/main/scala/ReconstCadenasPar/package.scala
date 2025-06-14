import common.*

import scala.collection.parallel.CollectionConverters.*
import Oraculo.*
import ArbolSufijos.*

import scala.Console.println

package object ReconstCadenasPar {
  // Ahora versiones paralelas

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa paralelismo de tareas
    // Genera combinaciones de forma perezosa
    def generarCombinaciones(n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(Seq.empty)
      else {
        val sufijos = generarCombinaciones(n - 1)
        for {
          suf <- sufijos
          c <- alfabeto
        } yield c +: suf
      }
    }

    val total = math.pow(4, n).toLong
    val cuarto = (total / 4).toInt

    val combinaciones = generarCombinaciones(n)

    // Búsqueda paralela en 4 cuartos del espacio
    val (res1, res2, res3, res4) = parallel(
      combinaciones.slice(0, cuarto).find(o),
      combinaciones.slice(cuarto, cuarto * 2).find(o),
      combinaciones.slice(cuarto * 2, cuarto * 3).find(o),
      combinaciones.slice(cuarto * 3, cuarto * 4).find(o)
    )

    // Devuelve el primero que encuentre (si hay alguno)
    res1.orElse(res2).orElse(res3).orElse(res4).getOrElse(Seq.empty)
  }

  def reconstruirCadenaIngenuoParConIterator(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Genera combinaciones de forma perezosa
    def generarCombinaciones(n: Int): Iterator[Seq[Char]] = {
      if (n == 0) Iterator(Seq.empty)
      else {
        val sufijos = generarCombinaciones(n - 1)
        for {
          suf <- sufijos
          c <- alfabeto
        } yield c +: suf
      }
    }

    val total = math.pow(4, n)
    val cuarto = (total / 4).toInt

    // Búsqueda paralela en 4 cuartos del espacio
    val (res1, res2, res3, res4) = parallel(
      generarCombinaciones(n).slice(0, cuarto).find(o),
      generarCombinaciones(n).slice(cuarto, cuarto * 2).find(o),
      generarCombinaciones(n).slice(cuarto * 2, cuarto * 3).find(o),
      generarCombinaciones(n).slice(cuarto * 3, cuarto * 4).find(o)
    )

    println((res1, res2, res3, res4))

    // Devuelve el primero que encuentre (si hay alguno)
    res1.orElse(res2).orElse(res3).orElse(res4).getOrElse(Seq.empty)
  }


  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa paralelismo de tareas y/o datos

    val ans = (1 to n).foldLeft(Seq(Seq.empty[Char])) { (cadenasPrevias, _) =>
      val (izq, der) = cadenasPrevias.splitAt(cadenasPrevias.size / 2)
      val cadIzq = for {
        prev <- izq
        w <- alfabeto

      } yield prev :+ w

      val cadDer = for {
        prev <- der
        w <- alfabeto

      } yield prev :+ w
      (cadIzq ++ cadDer).filter(o)

    }

    ans.flatten
  }

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa paralelismo de tareas y/o datos
    def generarSigmaK(sc: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      (for {
        w <- sc.par
        v <- sc
      } yield w ++ v).seq
    }

    def aux(scInicial: Seq[Seq[Char]], k: Int): Seq[Char] = {
      val sigmaK = generarSigmaK(scInicial).par.filter(o).seq
      val cadenaW = sigmaK.head
      if (k == n) cadenaW else aux(sigmaK, k * 2)
    }
    val initialSc = alfabeto.par.map(c => Seq(c)).filter(o).seq
    aux(initialSc, 2)
  }

  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa paralelismo de tareas y/o datos
      def filtrar(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
        (for {
          w <- sc.par
          v <- sc
          s = w ++ v
          if (1 until k).forall(c => sc.contains(s.slice(c, c + k)))
        } yield s).seq
      }

      def aux(scInicial: Seq[Seq[Char]], k: Int): Seq[Char] = {
        val sigmaK = filtrar(scInicial, k).par.filter(o).seq
        val cadenaW = sigmaK.head
        if (k == n) cadenaW else aux(sigmaK, k * 2)
      }

      val initialSc = alfabeto.par.map(c => Seq(c)).filter(o).seq
      aux(initialSc, 2)
    }


  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa arboles de sufijos para guardar Seq[Seq[Char]]
    // Usa paralelismo de tareas y/o datos
    def filtrar(sc1: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      val scTrie = arbolDeSufijos(sc1)
      (for {
        w <- sc1.par
        v <- sc1
        s = w ++ v
        if (1 until k).forall(c => pertenece(s.slice(c, c + k), scTrie))
      } yield s).seq
    }

    def aux(scInicial: Seq[Seq[Char]], k: Int): Seq[Char] = {
      val sigmaK = filtrar(scInicial, k).par.filter(o).seq
      val cadenaW = sigmaK.head
      if (k == n) cadenaW else aux(sigmaK, k * 2)
    }

    val initialSc = alfabeto.par.map(c => Seq(c)).filter(o).seq
    aux(initialSc, 2)
  }
    
    
    
}
