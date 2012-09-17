package de.sciss.lucre.expr

import de.sciss.lucre.stm.{InMemory, Sys}

object ExtractorSuite extends App /* TODO: FunSuite */ {
   sealed trait Identified[ +S, +A ]
   case class IsVariable[ S <: Sys[ S ], A ]( v: Expr.Var[ S, A ]) extends Identified[ S, A ]
   case class IsConstant[ A ]( value: A ) extends Identified[ Nothing, A ]
   case object IsOther extends Identified[ Nothing, Nothing ]

   def identify[ S <: Sys[ S ], A ]( expr: Expr[ S, A ]) : Identified[ S, A ] = expr match {
      case Expr.Var( v )   => IsVariable( v )
      case Expr.Const( c ) => IsConstant( c )
      case _               => IsOther
   }

   type S = InMemory
   implicit val sys = InMemory()

   val strings = Strings[ S ]
   import strings._

   val _const  = Const( "hallo" )
   val _var    = sys.step { implicit tx => Var( "welt" )}
   val _other  = sys.step { implicit tx => _var.reverse }

   assert( identify( _const ) == IsConstant( "hallo" ))
   assert( identify( _var ).isInstanceOf[ IsVariable[ _, _ ]])
   assert( identify( _other ) == IsOther )
   println( "ExtractorSuite ok." )
}
