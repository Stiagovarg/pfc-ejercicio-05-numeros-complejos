package taller

/** Punto 4. Un número complejo r + i·i, con r la parte real e i la
  * imaginaria. Los objetos no cambian: cada operación devuelve uno nuevo.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Complejos(val r: Double, val i: Double) {

  def +(otro: Complejos): Complejos = {
    new Complejos(this.r+otro.r, this.i+otro.i)
  } // Completar

  def -(otro: Complejos): Complejos = {
    new Complejos(this.r-otro.r, this.i-otro.i)
  } // Completar

  def *(otro: Complejos): Complejos = {
    new Complejos(this.r*otro.r-this.i*otro.i, this.r*otro.i + this.i*otro.r)
  } // Completar

  def /(otro: Complejos): Complejos = {
    val denom = otro.r * otro.r + otro.i * otro.i
    new Complejos(
      Math.round((this.r * otro.r + this.i * otro.i) / denom * 1000) / 1000.0,
      Math.round((otro.r * this.i - this.r * otro.i) / denom * 1000) / 1000.0
    )
  }

  // "a + bi" con las dos partes redondeadas a tres decimales; si la parte
  // imaginaria es negativa, "a - bi".
  override def toString: String = if(i>=  0)r+" + "+i+"i" else r+" - "+i.abs+"i" // Completar
}
