import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    def f(actual: List[Seq[Char]], cont: Int): List[Seq[Char]] = {
      if(cont == n)
        actual
      else {
        val nuevo = for {
          possibility <- actual
          letter <- alfabeto
        } yield possibility :+ letter
        f(nuevo, cont + 1)
      }
    }
    val combinations = f(List(Seq.empty), 0)
    combinations.find(o).getOrElse(Seq.empty)
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s

    ???
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s

    if (n == 0) Seq.empty
    else {
      // reconstruir la primera mitad
      val s1 = reconstruirCadenaIngenuo(n / 2, intento => o(intento ++ Seq.fill(n / 2)('a')))

      // crear un oráculo parcial para la segunda mitad
      val s2 = reconstruirCadenaIngenuo(n / 2, intento => o(s1 ++ intento))

      s1 ++ s2
    }
  }

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido

    ???
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s
    // Usa el filtro para ir mas rapido
    // Usa arboles de sufijos para guardar Seq[Seq[Char]]

    ???
  }

}
