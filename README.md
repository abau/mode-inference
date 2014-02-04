Run

    $ ghci ModeInference
    *ModeInference> runOnFile "Test/1.type" "ListBool ? {Nil; Cons (Bool ? {False;True}) self}"

    $ ghci ModeInference
    *ModeInference> constraintsOnFile "Test/1.type" "ListBool ? {Nil; Cons (Bool ? {False;True}) self}"

