import benchmarks.syntax._
import benchmarks.dimension.syntax._
import benchmarks.matrix.syntax._

package object benchmarks {

  object implicits
    extends RangeSyntax
    with ListSyntax
    with DimensionSyntax
    with MatrixSyntax
    with cec.cec2005.Params
    with cec.cec2013.niching.Params

}
