/**
* Projet d'implementation d'un arbre en scala 
* @author Riad TOUATI
*/

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]


object Tree {

  /**
    * @param arbre
    * @return retourne la taille de l'arbre (feuille plsu noeuds)  
  */
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(a) => 1
      case Branch(a, b) => size(a) + size(b) + 1
    }

  /**
    * @param arbre
    * @return retourne l'élement max contenue dans l'arbre   
  */
  def  maximum(tree:Tree[Int]):Int = 
    tree match {
      case  Leaf(l) => l
      case  Branch (a, b) => maximum (a) max maximum (b)
  } 

  /**
    * @param arbre
    * @return retourne la profondeur de l'arbre de la racine à la feuille 
  */
  def  depth(tree:Tree[Int]): Int = 
    tree match {
      case Leaf(_) => 0
      case Branch(a, b) => Math.max( depth(a),  depth(b)) + 1
  }
  
  /**
    * @param arbre
    * @param fonction qui va appliquer un traitement aux élements de l'arbre 
    * @return retourne l taille de l'arbre  
  */
  def  map [A,B]( tree:Tree[A])( f:A => B ):Tree[B] = 
  tree match {
    case  Leaf(l) => Leaf(f(l))
    case  Branch(a, b) => Branch (map(a)(f),map(b)(f))
  }
  /**
   *  fonction utiliser par la fonction map
    * @param un nombre entier 
    * @return retroune la factorielle du nombre passer en paramétre
  */
  def fac (n:Int):Int = {
      if (n == 0) return 1
      return n * fac(n - 1)
    }

  /**
    * fonction pour parcourir les élement de l'arbre
    * @param arbre
    * @param f1 function pour appliqué un premier traitement aux élement de l'arbre 
    * @param f2 function pour appliquer des opérations sur les élements de l'arbre (addiction, produit ...etc)
    * @return retourne l retrourne le resulat envoyé par la fonction f2 appliqué aux élement de l'arbre 
  */
  def fold[A,B](tree:Tree[A])(f1:A => B)(f2:(B, B) => B):B = 
  tree match {
    case Leaf(v) => f1(v)
    case Branch(l, r) => f2(fold(l)(f1)(f2), fold(r)(f1)(f2))
  }

  /**
    * fonction size en utilisant l'iterateur fold
    * @param arbre
    * @return retourne l taille de l'arbre  
  */
  def sizeWithFold[A](tree:Tree[A]): Int = 
    fold(tree)(_ => 1)(1 + _ + _ )
  
  /**
    * fonction maximum en utilisant l'iterateur fold
    * @param arbre
    * @return retourne l'élement maximum de l'arbre 
  */
  def maximumWithFold[A](tree:Tree[Int]):Int = 
    fold(tree)((valeur: Int) => valeur)(_ max _)
  
  /**
    * fonction depth en utilisant l'iterateur fold
    * @param arbre
    * @return retourne la profondeur de la l'arbre  
  */
  def depthWithFold[A](tree:Tree[A]): Int =
    fold(tree)(_ => 0)((l:Int, r:Int) => 1 + (l max r))

  /**
    * fonction map en utilisant l'iterateur fold
    * @param arbre
    * @return retourne un arbre à partir de l'arbre passé en paramétre en appliquant un traitement é ses élements
  */
  def mapWithFold[A, B](tree:Tree[A])(f:A => B):Tree[B] =
    fold(tree)( (a:A) => Leaf(f(a)): Tree[B] ) ( Branch(_ , _) )

  def main(args: Array[String]): Unit = {
    val tree1 = Branch(
                        Branch ( Leaf("a"), Leaf("b") ), Branch( Leaf("c"), Leaf("d") ) )
    val tree = Branch( Branch( Leaf(3), Branch( Leaf(2), Leaf(4) ) ), Leaf(12) )
                

    println("************** Affichage du Tree en question  ******************\n")
    println(tree)
    println("\n")

    println("************** Size qui retourne le nombre de noeuds et feuille du Tree ******************\n")
    println(size(tree))
    println("\n")

    println("************** maximum qui retroune l'élement maximum du Tree ******************\n")
    println(maximum(tree))
    println("\n")

    println("************** depth qui retourne la profondeur du tree de la racine à la feuille ******************\n")
    println(depth(tree))
    println("\n")

    println("************** Map avec la fonction factrielle appliquée aux élement du Tree ******************\n")
    println(map(tree)(fac))
    println("\n")

    println("************** Test de fold pour calculer la somme des élements de l'arbre  ******************\n")
    println( fold(tree)(_ + 0)( _ + _) )
    println("\n")

    println("************** Test de size en utilisant fold  ******************\n")
    println( sizeWithFold(tree) )
    println("\n")

    println("************** Teste de maximum en utilisant fold ******************\n")
    println( maximumWithFold(tree) )
    println("\n")

    println("************** Test de depth en utilisant fold  ******************\n")
    println( depthWithFold(tree) )
    println("\n")

    println("************** Teste de map en utilisant fold  ******************\n")
    println( mapWithFold(tree)(fac) )
    println("\n")

  }
}