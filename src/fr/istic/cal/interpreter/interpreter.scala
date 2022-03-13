package fr.istic.cal.interpreter

/*
 * VEUILLEZ INSCRIRE CI-DESSOUS VOTRE NOM ET VOTRE PRENOM :
 *
 * ETUDIANT 1 :
 *
 * ETUDIANT 2 :
 *
 */

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */
  type Memory = List[(Variable, Value)]

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return m(v), c'est-à-dire la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */
  def lookUp(v: Variable, mem: Memory): Value = {
    mem match {
      case Nil       => NlValue
      case e :: tail => if (v == e._1) e._2 else lookUp(v, tail)
    }
  }

  /**
   * @param v : une variable
   * @param d : une valeur
   * @param mem : une mémoire
   * @return la mémoire modifiée par l'affectation [v->d]
   */
  def assign(v: Variable, d: Value, mem: Memory): Memory = {
    mem match {
      case Nil       => (v, d) :: Nil
      case e :: tail => if (e._1 == v) ((v, d) :: tail) else (e :: assign(v, d, tail))
    }
  }

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */
  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    expression match {
      case Nl         => NlValue
      case Cst(x)     => CstValue(x)
      case VarExp(x)  => lookUp(Var(x), mem)
      case Cons(a, b) => ConsValue(interpreterExpr(a, mem), interpreterExpr(b, mem))
      case Eq(a, b)   => if (interpreterExpr(a, mem) != interpreterExpr(b, mem)) NlValue else ConsValue(NlValue, NlValue)
      case Hd(e) => interpreterExpr(e, mem) match {
        case ConsValue(a, b) => a
        case _               => NlValue
      }
      case Tl(e) => interpreterExpr(e, mem) match {
        case ConsValue(a, b) => b
        case _               => NlValue
      }
    }
  }

  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant l'expression de cette valeur
   */
  def valueToExpression(value: Value): Expression = {
    value match {
      case NlValue         => Nl
      case ConsValue(a, b) => Cons(valueToExpression(a), valueToExpression(b))
      case CstValue(x)     => Cst(x)
    }
  }

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */
  def interpreterCommand(command: Command, memory: Memory): Memory = {
    command match {
      case Nop       => memory
      case Set(v, e) => assign(v, interpreterExpr(e, memory), memory)
      
      case While(cond, body) => interpreterExpr(cond, memory) match {
        case NlValue => memory
        case _       => interpreterCommand(While(cond, body), interpreterCommands(body, memory))
      }
      
      case If(cond, e1, e2) => interpreterExpr(cond, memory) match {
        case NlValue => interpreterCommands(e2, memory)
        case _       => interpreterCommands(e1, memory)
      }
      
      case For(cond, body) => interpreterExpr(cond, memory) match {
        case NlValue => memory
        //TODO: doesn't work
        case _ => interpreterCommands(body ++
                  List(For(Tl(valueToExpression(interpreterExpr(cond, memory))), body)), memory)
      }
      
    }
  }

  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
    commands match {
      case Nil       => memory
      case head :: tail => interpreterCommands(tail, interpreterCommand(head, memory))
    }
  }

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste non vide de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */
  // TODO TP2
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = ???

  /**
   * @param vars : une liste non vide décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = ???

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
  def interpreter(program: Program, vals: List[Value]): List[Value] = ???

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

}