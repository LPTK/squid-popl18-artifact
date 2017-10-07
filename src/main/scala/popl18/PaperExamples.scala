package popl18

import squid.utils._

import scala.io.StdIn

//object Embedding extends squid.ir.SimpleAST
object Embedding extends squid.ir.SimpleANF
import Embedding.Predef._
import Embedding.Quasicodes._

object PaperExamples extends App {
  
  def id[T](x: T): T = x
  
  test("Introduction") {
    
    def optim[T,C](pgrm: IR[T,C]): IR[T,C] = pgrm rewrite {
      case ir"val p: (Int,Int) = ($a:Int,$b:Int); $body: $bt" =>
        val body2 = body.rewrite { case ir"${body.p}._1" => a case ir"${body.p}._2" => b }
        body2.p ~> ir"($a,$b)"
    }
    
    assertEqt(
      optim(ir{
        val n = StdIn.readInt()
        val my = (n, n+1)
        println(my)
        my._1 + my._2 * 2
      }),
      ir{
        val a = scala.io.StdIn.readInt();
        val b = a + 1
        scala.Predef.println(scala.Tuple2.apply[scala.Int, scala.Int](a, b));
        a + b * 2
      }
    )
    
    type T = Int
    
    assertEqt(
      ir"(x: T) => ${ id(ir"?x:T") }",
      ir"(y: T) => y"
    )
    
  }
  
  test("Motivation") {
    
    val x = ir"123"
    
    assertEqt(
      ir"Math.pow($x,2)", 
      {import Math.pow; ir"pow($x,2)"}
    )
    
    assertEqt(
      ir"val a = List(1,2,3); a.size", 
      ir"((b: List[Int]) => b.size)(List[Int](1,2,3))"
    )
    
  }
  
  test("Squid And Application Examples") {
    
    test("The Squid Way") {
        
      type T = Int
      
      assertEqt(
         { val inner = id(ir"?x: Int"); ir"(x: Int) => $inner" }, 
        ir"(x: T) => ${ id(ir"?x:T") }"
      )
      
      val fx: IR[Int,{val x:Int}] = ir"(?x: Int)+1"
      
      assertEqt((ir"$fx.toDouble" : IR[Double,{val x:Int}]), ir"((?x:Int)+1).toDouble")
      
      assertEqt((ir"val x = 0; $fx .toDouble" : IR[Double,{}]), ir"val x = 0; (x+1).toDouble")
      
    }
    
    test("Rewrite Rules and Polymorphism") {
        
      val f: IR[Int => Int, {val y:Int}] = ir"(x: Int) => x + (?y:Int)"
      val g: IR[Int, {val y:Int; val z:Int}] = f match { case ir"{z => $body}" => body }
      assert(g.toString == """ir"?z.+(?y)"""")
      
    }
    
    test("Rewrite Rules and Polymorphism") {
        
      val pgrm = ir"List(1,2,3).foldLeft(0)((acc,x) => acc+x) + 321"
      
      val pgrm2 = pgrm rewrite {
        case ir"($ls: List[$t]).foldLeft[$r]($init)($f)" => 
          ir""" var cur = $init
            $ls.foreach(x => cur = $f(cur, x))
            cur """ 
      }
      
      assertEqt(pgrm2, 
        ir"val ls = List(1,2,3); var cur = 0; ls.foreach(x => cur = ((acc:Int,x:Int) => acc+x)(cur,x)); cur + 321")
      
    }
    
    test("Fixed Point Rewritings") {
      
      def isTrivial(cde: IR[_,_]) = true  // we use an ANF IR (see: https://github.com/epfldata/squid/blob/master/doc/Intermediate_Representations.md#the-a-normal-form-anf)
      
      import Math.pow
      import squid.ir.{SimpleRuleBasedTransformer, BottomUpTransformer, FixPointTransformer}
      
      object Tr extends Embedding.SelfTransformer with SimpleRuleBasedTransformer with BottomUpTransformer with FixPointTransformer {
        rewrite {
          case ir"pow($x,$exp)" if !isTrivial(x) => ir"val base = $x; pow(base,$exp)" 
          case ir"pow($x,0)" => ir"1.0"
          case ir"pow($x,${Const(exp)})" if exp.isWhole && exp > 0 =>
            if (exp % 2 == 0) ir"val tmp = pow($x,${Const(exp/2)}); tmp * tmp"
            else ir"$x * pow($x,${Const(exp-1)})"
        }
      }
      assertEqt(ir"pow(.5,3)" transformWith Tr, 
        ir"0.5*{val tmp_0 = 0.5*${ir"1.0"}; tmp_0*tmp_0}")
      assertEqt(ir"pow(readDouble,3)" transformWith Tr, 
        ir"val x_0 = readDouble; val tmp_1 = x_0*1.0; x_0*(tmp_1*tmp_1)")
      
    }
    
    test("Free Variables Substitution") {
      
      val a: IR[Int, {val x: Int}] = ir"(?x: Int) + 1"
      assertEqt(a, ir"(?x: Int) + 1")
      
      val b: IR[Int, {}] = a.subs(('x, ir"27"))
      assertEqt(b, ir"27 + ${ir"1"}")
      
      val c: IR[Int, {}] = a subs 'x -> ir"27"
      assertEqt(c, b)
      
    }
    
    test("Motivating Example: Array of Tuples Optimization") {
      
      def optimize[T](pgrm: IR[T,{}]) : IR[T,{}] = pgrm rewrite { 
        case ir"val $arr = new Array[($ta,$tb)]($size); $body: $bt" =>
          val a = ir"?a : Array[$ta]" ; val b = ir"?b : Array[$tb]"
          val body2 = body rewrite {
            case ir"$$arr($i)._1" => ir"$a($i)"
            case ir"$$arr($i)._2" => ir"$b($i)"
            case ir"$$arr($i) = ($va: $$ta, $vb: $$tb)" => ir"$a($i) = $va; $b($i) = $vb"
            case ir"$$arr($i)" => ir"($a($i), $b($i))"
            case ir"$$arr.size" => ir"$a.size" }
            val body3 = body2 subs 'arr -> (Abort())
            ir"val a = new Array[$ta]($size); val b = new Array[$tb]($size); $body3" 
      }
      
      val pgrm = ir"""if(readInt>0){val a = new Array[(Int,String)](3); a(0) = (36,"ok"); a.size}"""
      
      assertEqt(optimize(pgrm), ir{
        val r = scala.Predef.readInt();
        if (r > 0) {
          val left = new Array[Int](3);
          val right = new Array[String](3);
          left(0) = 36
          right(0) = "ok"
          left.size
        }
      })
      
    }
    
  }
  
  test("Embedding in Scala") {
    
    test("Context Requirements") {
      
      val pgrm = ir"val x: Int = ?y ; println(x + (?y:Int))"
      val pgrm2: IR[Unit,{val y:Int; val z:Int}] = 
        pgrm rewrite { case ir"($n:Int) + ($m:Int)" => ir"(-$m - $n) * (?z : Int)" }
      
      assertEqt(pgrm2,
        ir"val x: Int = ?y ; println((-(?y:Int) - x) * (?z:Int))")
      
    }
    
    test("Use of Runtime Reflection and Metaprogramming") {
      
      test("Implementation of run and compile") {
        
        assert(ir"List(1,2,3).map(_+1)".run == List(2,3,4))
        assert(ir"List(1,2,3).map(_+1)".compile == List(2,3,4))
        
      }
      
      test("Subtype Checking in Code Pattern Matching") {
        
        code"List(1,2,3)" match {
          case code"$ls: Seq[Any]" => // note: runtime subtyping check happening here (List[Int] <: Seq[Any])
            assertEqt(ls, code"List(1,2,3)")
        }
        
        code"Math.pow(.5,3)" match {
          case code"Math.pow($x,$y)" => // note: no type annotation necessary thanks to non-overloaded Math.pow
            assertEqt(x, code".5")
            assertEqt(y, code"3D")
        }
        
      }
      
    }
    
  }
  
  test("Related Work") {
    
    assertEqt(
      code"(x: Int) => ${ code{code"?x:Int"}.run }",
      code"(x: Int) => x"
    )
    
  }
  
  println(s"Errors: $errCount")
  
}
