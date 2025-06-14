import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {


  def reconstruirCadenaIngenuoConIterator(n: Int, o: Oraculo): Seq[Char] = {
    def generarSigmaN(n: Int): Iterator[Seq[Char]] = {
      if (n == 0) Iterator(Seq.empty)
      else {
        val sufijos = generarSigmaN(n - 1) // evalúa solo una vez
        for {
          suf <- sufijos
          c <- alfabeto
        } yield c +: suf
      }
    }
    generarSigmaN(n).find(o).getOrElse(Seq.empty)
  }



  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    def generarSigmaN(n: Int): Seq[Seq[Char]] = {
      if (n == 0) Seq(Seq.empty)
      else {
        val sufijos = generarSigmaN(n - 1) // evalúa solo una vez
        for {
          suf <- sufijos
          c <- alfabeto
        } yield c +: suf
      }
    }

    generarSigmaN(n).find(o).getOrElse(Seq.empty)
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    def generarValidas(k: Int): Seq[Seq[Char]] = {
      if (k == 0) Seq(Seq.empty)
      else {
        for {
          sufijo <- generarValidas(k - 1)
          c <- alfabeto
          nuevaCadena = c +: sufijo
          if o(nuevaCadena)
        } yield nuevaCadena
      }
    }

    generarValidas(n).headOption.getOrElse(Seq.empty)
  }


  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    def generarSigmaK(sc: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      for{
        w <- sc
        v <- sc
      }yield w ++ v
    }
    def aux(scInicial: Seq[Seq[Char]], k: Int): Seq[Char] = {
      val sigmaK = generarSigmaK(scInicial).filter(o)
      val cadenaW = sigmaK.head
      if(k == n) cadenaW else aux(sigmaK, k * 2)
    }
    val initialSc = alfabeto.map(c=> Seq(c)).filter(o)
    aux(initialSc, 2)
  }

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido
    def filtrar(sc: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      for{
        w <- sc
        v <- sc
        s = w ++ v
        if (1 until k).forall(c => sc.contains(s.slice(c, c + k)))
      } yield s
    }

    def aux(scInicial: Seq[Seq[Char]], k: Int): Seq[Char] = {
      val sigmaK = filtrar(scInicial, k).filter(o)
      val cadenaW = sigmaK.head
      if(k == n) cadenaW else aux(sigmaK, k * 2)
    }

    val initialSc = alfabeto.map(c=> Seq(c)).filter(o)
    aux(initialSc, 2)
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido
    // Usa arboles de sufijos para guardar Seq[Seq[Char]]
    def filtrar(sc1: Seq[Seq[Char]], k:Int): Seq[Seq[Char]] = {
      val k = sc1.length;
      val scTrie = arbolDeSufijos(sc1);
      for {
        w <- sc1
        v <- sc1
        s = w ++ v
        if (1 until k).forall(c => pertenece(s.slice(c, c + k), scTrie))
      } yield s
    }

    def aux(scInicial: Seq[Seq[Char]], k:Int): Seq[Char] = {
      val sigmaK = filtrar(scInicial, k).filter(o)
      val cadenaW = sigmaK.head
      if (k == n) cadenaW else aux(sigmaK, k * 2)
    }

    val initialSc = alfabeto.map(c=> Seq(c)).filter(o)
    aux(initialSc, 2)
  }


}
