package taller

/** Punto 3. Un número racional x/y guardado en su forma normalizada:
  * numerador y denominador sin factores comunes, el signo en el numerador y
  * el denominador siempre positivo. El cero se guarda como 0/1.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Racional(x: Int, y: Int) {

  require(y != 0, "El denominador no puede ser cero")

  // Precondición del constructor: el denominador no es cero.
  // Completar

  // El máximo común divisor de dos enteros no negativos.
  @scala.annotation.tailrec
  private def mcd(a: Int, b: Int): Int =
    if (b == 0) a
    else mcd(b, a % b)// Completar

  // Numerador y denominador ya normalizados.
  def numer: Int = if(y<0)-x / mcd(x.abs, y.abs) else x / mcd(x.abs, y.abs) // Completar

  def denom: Int = y.abs / mcd(x.abs, y.abs) // Completar

  def +(r: Racional): Racional = {
    new Racional(this.numer*r.denom + this.denom*r.numer, this.denom*r.denom)
  } // Completar

  def -(r: Racional): Racional = {
    new Racional(this.numer*r.denom - this.denom*r.numer, this.denom*r.denom)
  } // Completar

  def *(r: Racional): Racional = {
    new Racional(this.numer*r.numer, this.denom*r.denom)
  } // Completar

  def /(r: Racional): Racional = {
    new Racional(this.numer*r.denom, this.denom*r.numer)
  } // Completar

  // Si los dos racionales representan el mismo número.
  def ==(r: Racional): Boolean = {
    if(this.numer == r.numer && this.denom ==r.denom)true else false
  } // Completar

  def <(r: Racional): Boolean = {
    if(this.numer*r.denom<r.numer*this.denom) true else false
  } // Completar

  // El mayor de los dos.
  def max(r: Racional): Racional = {
    if(this.numer*r.denom>r.numer*this.denom) new Racional(this.numer,this.denom) else new Racional(r.numer,r.denom)
  } // Completar

  // "n/d", o solo "n" cuando el denominador es 1.
  override def toString: String = if(denom==1)numer+"" else if (numer==0) "0" else numer+"/"+denom// Completar
}
