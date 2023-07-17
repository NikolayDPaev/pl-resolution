module Language exposing (..)

type alias Language = { 
    vars : List String,
    consts : List String,
    preds : List String,
    funcs : List String
    }

type Term
    = Constant String
    | Variable String
    | Function String (List Term)

printTerm : Term -> String
printTerm term =
    case term of
        Constant str -> str
        Variable str -> str
        Function str terms ->
            str ++ "(" ++ String.dropRight 2 (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" terms) ++ ")"

type Literal
    = PositivePredicate String (List Term)
    | NegativePredicate String (List Term)

printLiteral : Literal -> String
printLiteral literal = 
    case literal of
        PositivePredicate p terms -> 
            case (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" terms) of 
                "" -> p
                list -> p ++ "(" ++ String.dropRight 2 list ++ ")"
        NegativePredicate p terms -> 
            case (List.foldl (\ x acc -> acc ++ (printTerm x) ++ ", ") "" terms) of 
                "" -> "¬" ++ p
                list -> "¬" ++ p ++ "(" ++ String.dropRight 2 list ++ ")"

type Op
    = And
    | Or
    | Impl
    | Eqv

printOp : Op -> String
printOp op =
    case op of
        And -> "&"
        Or -> "∨"
        Impl -> "⇒"
        Eqv -> "⇔"

type Formula
    = Literal Literal
    | Negation Formula
    | Operation Formula Op Formula
    | Exists String Formula
    | ForAll String Formula

printFormula : Formula -> String
printFormula f =
    case f of
        Literal literal -> printLiteral literal
        Negation f1 -> "¬" ++ printFormula f1
        Operation f1 op f2 -> "(" ++ printFormula f1 ++ " " ++ printOp op ++ " " ++ printFormula f2 ++ ")"
        Exists x f1 -> "∃" ++ x ++ "(" ++ printFormula f1 ++ ")"
        ForAll x f1 -> "∀" ++ x ++ "(" ++ printFormula f1 ++ ")"

negate : Formula -> Formula
negate formula =
    case formula of
        Literal literal ->
            case literal of
                PositivePredicate p terms ->
                    Literal (NegativePredicate p terms)

                NegativePredicate p terms ->
                    Literal (PositivePredicate p terms)
        _ ->
            Negation formula

type alias Disjunct =
    List Literal

eliminateImplAndEqv : Formula -> Formula
eliminateImplAndEqv formula =
    case formula of
        Operation a op b ->
            let
                newA = eliminateImplAndEqv a
                newB = eliminateImplAndEqv b
            in
            case op of
                Impl -> Operation (negate newA) Or newB
                Eqv -> Operation (Operation (negate newA) Or newB) And (Operation newA Or (negate newB))
                _ -> Operation newA op newB
        Negation f -> Negation (eliminateImplAndEqv f)
        Exists x f -> Exists x (eliminateImplAndEqv f)
        ForAll x f -> ForAll x (eliminateImplAndEqv f)
        _ -> formula

moveNegations : Formula -> Formula
moveNegations outerFormula =
    case outerFormula of
        Negation formula ->
            case formula of
                Literal _ -> negate formula
                Negation f -> moveNegations f
                Operation a op b ->
                    case op of
                        And ->
                            Operation (moveNegations (Negation a)) Or (moveNegations (Negation b))
                        Or ->
                            Operation (moveNegations (Negation a)) And (moveNegations (Negation b))
                        Impl ->
                            Operation (moveNegations a) And (moveNegations (Negation b))
                        Eqv ->
                            Operation (moveNegations (Negation a)) Eqv (moveNegations (Negation b))
                
                Exists x f -> ForAll x (moveNegations (Negation f))
                ForAll x f -> Exists x (moveNegations (Negation f))

        Literal _ -> outerFormula
        Operation a op b -> Operation (moveNegations a) op (moveNegations b)
        Exists x f -> Exists x (moveNegations f)
        ForAll x f -> ForAll x (moveNegations f)



-- to CNF
-- to PNF
-- to List of Disjuncts 
