import common.*

import scala.collection.parallel.CollectionConverters.*
import Oraculo.*
import ArbolSufijos.*
import ReconstCadenas.*

package object ReconstCadenasPar {

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def generarCombinaciones(n: Int): LazyList[Seq[Char]] = {
      if (n == 0) LazyList(Seq.empty)
      else for {
        suf <- generarCombinaciones(n - 1)
        c <- LazyList.from(alfabeto)
      } yield c +: suf
    }

    val combinaciones = generarCombinaciones(n)

    val total = math.pow(alfabeto.length, n).toInt
    val octavo = total / 8

    val ((res1, res2, res3, res4), (res5, res6, res7, res8)) = parallel(
      parallel(
        combinaciones.slice(0 * octavo, 1 * octavo).find(o),
        combinaciones.slice(1 * octavo, 2 * octavo).find(o),
        combinaciones.slice(2 * octavo, 3 * octavo).find(o),
        combinaciones.slice(3 * octavo, 4 * octavo).find(o)
      ),
      parallel(
        combinaciones.slice(4 * octavo, 5 * octavo).find(o),
        combinaciones.slice(5 * octavo, 6 * octavo).find(o),
        combinaciones.slice(6 * octavo, 7 * octavo).find(o),
        combinaciones.slice(7 * octavo, total).find(o)
      )
    )
    
    res1.orElse(res2).orElse(res3).orElse(res4)
      .orElse(res5).orElse(res6).orElse(res7).orElse(res8)
      .getOrElse(Seq.empty)
  }

  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def generarCombinaciones(n: Int): Seq[Seq[Char]] = {
      if (n == 0) LazyList(Seq.empty)
      else for {
        suf <- generarCombinaciones(n - 1).filter(o)
        c <- alfabeto.par
      } yield c +: suf
    }
    

    generarCombinaciones(n).headOption.getOrElse(Seq.empty)


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
      val sigmaK = filtrar(scInicial, k / 2).par.filter(o).seq
      val cadenaW = sigmaK.head
      if (k == n) cadenaW else aux(sigmaK, k * 2)
    }

    val initialSc = alfabeto.map(c => Seq(c)).filter(o)
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
      val sigmaK = filtrar(scInicial, k / 2).par.filter(o).seq
      val cadenaW = sigmaK.head
      if (k == n) cadenaW else aux(sigmaK, k * 2)
    }

    val initialSc = alfabeto.map(c => Seq(c)).filter(o)
    aux(initialSc, 2)
  }


}
