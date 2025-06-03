error id: file:///C:/Users/Usuario/OneDrive/Escritorio/Universidad/4%20Semestre/FDP%20funcional%20y%20concurrente/Proyecto/strings-reconstruction-proyect/src/main/scala/ReconstCadenas/package.scala:`<none>`.
file:///C:/Users/Usuario/OneDrive/Escritorio/Universidad/4%20Semestre/FDP%20funcional%20y%20concurrente/Proyecto/strings-reconstruction-proyect/src/main/scala/ReconstCadenas/package.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -ArbolSufijos.
	 -ArbolSufijos#
	 -ArbolSufijos().
	 -Oraculo.
	 -Oraculo#
	 -Oraculo().
	 -.
	 -#
	 -().
	 -scala/Predef.
	 -scala/Predef#
	 -scala/Predef().
offset: 948
uri: file:///C:/Users/Usuario/OneDrive/Escritorio/Universidad/4%20Semestre/FDP%20funcional%20y%20concurrente/Proyecto/strings-reconstruction-proyect/src/main/scala/ReconstCadenas/package.scala
text:
```scala
import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    ???
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s

      def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
        var SCk: Set[Seq[Char]] = Set(Seq()) // SC0 ← {""}

        for (k <- 1 to n) {
          val candidatos = for {
            w <- SCk
            a <- alfabeto
          } yield w :+ a

          SCk = candidatos.filter(w => o(w)).toSet

          SCk.find(_.lengt@@h == n) match {
            case Some(resultado) => return resultado
            case None            => ()
          }
        }

        Seq() // No debería alcanzarse si el oráculo es correcto
      }
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    // recibe la longitud de la secuencia que hay que reconstruir (n, potencia de 2), y un oraculo para esa secuencia
    // y devuelve la secuencia reconstruida
    // Usa la propiedad de que si s=s1++s2 entonces s1 y s2 tambien son subsecuencias de s

    ???
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

```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.