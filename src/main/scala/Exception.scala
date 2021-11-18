object UnificationExceptions {
  class TypeMismatchException(a: Type, b: Type) extends RuntimeException {
    override def getMessage: String = s"$a is not unifiable with $b"
  }

  class UnificationFailedException(eq: Eq) extends RuntimeException {
    override def getMessage: String = eq match
      case eq@Eq(a: Type, b: Type) => s"Unification failed for $eq"
      case _ => "Unification failed"
  }
}

object EquationExceptions {
  class LetInferenceException(x: Var, e1: Term) extends RuntimeException {
    override def getMessage: String = s"Could not infer type of $x = $e1"
  }

  class VarNotFoundInEnvException(x: Var) extends RuntimeException {
    override def getMessage: String = s"Var $x not found in environment"
  }
}

object SolveurExceptions {
  class ClashTypeException(x: Type, y: Type, t: Type) extends RuntimeException {
    override def getMessage: String = s"$x has conflicting types $y and $t"
  }

  class GoalNotFoundException() extends RuntimeException {
    override def getMessage: String = s"${TVar.t0} not found"
  }
}

