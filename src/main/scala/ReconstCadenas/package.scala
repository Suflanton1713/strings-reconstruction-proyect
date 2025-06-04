import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // Genera solo las cadenas de longitud n (Sigma^n)
    def generarSigmaN(n: Int): Seq[Seq[Char]] = {
      (1 to n).foldLeft(Seq(Seq.empty[Char])) { (cadenasPrevias, _) =>
        val (izq, der) = cadenasPrevias.splitAt(cadenasPrevias.size / 2)

        val cadIzq = for {
          prev <- izq
          w <- alfabeto
        } yield prev :+ w

        val cadDer = for {
          prev <- der
          w <- alfabeto
        } yield prev :+ w

        cadIzq ++ cadDer
      }
    }

    val sigma_n = generarSigmaN(n)

    val (initSigmaN,finalSigmaN) = sigma_n.splitAt(sigma_n.length/2)

    val initDec = initSigmaN.find(c => o(c)).getOrElse(Seq())
    val finalDec = finalSigmaN.find(c => o(c)).getOrElse(Seq())

    initDec++finalDec
  }



  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    val ans = (1 to n).foldLeft(Seq(Seq.empty[Char])) { (cadenasPrevias, _) =>
      val (izq, der) = cadenasPrevias.splitAt(cadenasPrevias.size / 2)
      val cadIzq = for {
        prev <- izq
        w <- alfabeto
        if o(prev :+ w)
      } yield prev :+ w

      val cadDer = for {
        prev <- der
        w <- alfabeto
        if o(prev :+ w)
      } yield prev :+ w
      (cadIzq ++ cadDer)

    }

    ans.flatten
  }

  
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    def generarSigmaK(sc1: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      for{
        w <- sc1
        v <- sc1
      }yield w ++ v
    }
    def aux(scInicial: Seq[Seq[Char]]): Seq[Char] = {
      val sigmaK = generarSigmaK(scInicial).filter(o)
      val cadenaW = sigmaK.head
      if(cadenaW.length == n) cadenaW else aux(sigmaK)
    }
    val initialSc = alfabeto.map(c=> Seq(c)).filter(o)
    aux(initialSc)
  }

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido
    def generarSigmaK(sc1: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      val k = sc1.length;
      for{
        w <- sc1
        v <- sc1
        s = w ++ v
        if (1 until k).forall(c => sc1.contains(s.slice(c, c + k)))
      } yield s
    }

    def aux(scInicial: Seq[Seq[Char]]): Seq[Char] = {
      val sigmaK = generarSigmaK(scInicial).filter(o)
      val cadenaW = sigmaK.head
      if(cadenaW.length == n) cadenaW else aux(sigmaK)
    }

    val initialSc = alfabeto.map(c=> Seq(c)).filter(o)
    aux(initialSc)
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido
    // Usa arboles de sufijos para guardar Seq[Seq[Char]]
    def generarSigmaK(sc1: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      val k = sc1.length;
      val scTrie = arbolDeSufijos(sc1);
      for {
        w <- sc1
        v <- sc1
        s = w ++ v
        if (1 until k).forall(c => pertenece(s.slice(c, c + k), scTrie))
      } yield s
    }

    def aux(scInicial: Seq[Seq[Char]]): Seq[Char] = {
      val sigmaK = generarSigmaK(scInicial).filter(o)
      val cadenaW = sigmaK.head
      if (cadenaW.length == n) cadenaW else aux(sigmaK)
    }

    val initialSc = alfabeto.map(c=> Seq(c)).filter(o)
    aux(initialSc)
  }


}
