package taller

/** Punto 1. Un rectángulo dado por su base y su altura, ambas enteras. Los
  * objetos no cambian: rotar y escalar devuelven un rectángulo nuevo.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Rectangulo(b: Int, h: Int) {

  // Selectoras: la base y la altura con que se construyó el rectángulo.
  def base: Int = b// Completar

  def altura: Int = h// Completar

  def area: Int = {
    val area = b*h
    area
  } // Completar

  def perimetro: Int = {
    val perimetro = (2*b)+(2*h)
    perimetro
  } // Completar

  def esCuadrado: Boolean = {
    if(b==h)true else false
  }    // Completar

  // El rectángulo con base y altura intercambiadas.
  def rotar: Rectangulo = {
    new Rectangulo(this.h,this.b)
  } // Completar

  // El rectángulo con los dos lados multiplicados por k.
  def escalar(k: Int): Rectangulo = {
    new Rectangulo(this.base*k, this.altura*k)
  } // Completar

  // Si este rectángulo entra dentro de otro, tal cual o rotado.
  def cabeEn(otro: Rectangulo): Boolean = {
    if(otro.base >= this.base && otro.altura >= this.altura ||
      otro.base >= this.altura && otro.altura >= this.base)true else false
  }// Completar

  // El de mayor área entre este y otro; con áreas iguales, este.
  def elMayor(otro: Rectangulo): Rectangulo = {
    if(otro.area>this.area) new Rectangulo(otro.base,otro.altura) else new Rectangulo(b, h)

  } // Completar

  // La forma "3x4": base, la letra x y altura.
  override def toString: String = b+"x"+h // Completar
}
