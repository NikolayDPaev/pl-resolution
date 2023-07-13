module Formula exposing (..)

type Term
    = Constant String
    | Variable String
    | Formula String (List Term)


type Literal
    = PositivePredicate String (List Term)
    | NegativePredicate String (List Term)


type Op
    = And
    | Or
    | Impl
    | Eqv


type Formula
    = Literal Literal
    | Negation Formula
    | Operation Formula Op Formula
    | Exists String Formula
    | ForAll String Formula


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
                newA =
                    eliminateImplAndEqv a

                newB =
                    eliminateImplAndEqv b
            in
            case op of
                Impl ->
                    Operation (negate newA) Or newB

                Eqv ->
                    Operation (Operation (negate newA) Or newB) And (Operation newA Or (negate newB))

                _ ->
                    formula

        _ ->
            formula


moveNegations : Formula -> Formula
moveNegations outerFormula =
    case outerFormula of
        Negation formula ->
            case formula of
                Literal _ ->
                    negate formula

                Negation f ->
                    moveNegations f

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

                Exists x f ->
                    ForAll x (moveNegations (Negation f))

                ForAll x f ->
                    Exists x (moveNegations (Negation f))

        Literal _ ->
            outerFormula

        Operation a op b ->
            Operation (moveNegations a) op (moveNegations b)

        Exists x f ->
            Exists x (moveNegations f)

        ForAll x f ->
            ForAll x (moveNegations f)


-- todo: parser
-- todo: printer

-- to CNF
-- to PNF
-- to List of Disjuncts 
