module Test where

import Test.HUnit
import Interprete
import Parser

testAll = TestList [TestLabel "test01" test01,
                    TestLabel "test02" test02,
                    TestLabel "test03" test03,
                    TestLabel "test04" test04,
                    TestLabel "test05" test05,
                    TestLabel "test06" test06,
                    TestLabel "test07" test07,
                    TestLabel "test08" test08,
                    TestLabel "test09" test09,
                    TestLabel "test10" test10,
                    TestLabel "test11" test11,
                    TestLabel "test12" test12,
                    TestLabel "test14" test14,
                    TestLabel "test15" test15,
                    TestLabel "test16" test16,
                    TestLabel "test17" test17,
                    TestLabel "test18" test18,
                    TestLabel "test19" test19,
                    TestLabel "test21" test21,
                    TestLabel "test22" test22,
                    TestLabel "test23" test23,
                    TestLabel "test24" test24,
                    TestLabel "test25" test25,
                    TestLabel "test26" test26,
                    TestLabel "test27" test27,
                    TestLabel "test28" test28,
                    TestLabel "test28" test29,
                    TestLabel "test30" test30,
                    TestLabel "test32" test32,
                    TestLabel "test33" test33,
                    TestLabel "test34" test34,
                    TestLabel "test36" test36,

                    TestLabel "test38" test38,
                    TestLabel "test39" test39,
                    TestLabel "test40" test40,
                    TestLabel "test41" test41,
                    TestLabel "test42" test42]

-- Q1
test01 = TestList [TestCase ((@=?) (runParser espacesP "") (Just ((),""))),
                   TestCase ((@=?) (runParser espacesP " ") (Just ((),""))),
                   TestCase ((@=?) (runParser espacesP "     ") (Just ((),""))),
                   TestCase ((@=?) (runParser espacesP "  a ") (Just ((),"a ")))]

--Q2
test02 = TestList [TestCase ((@=?) (runParser nomP "") (Nothing)),
                   TestCase ((@=?) (runParser nomP "abc") (Just ("abc",""))),
                   TestCase ((@=?) (runParser nomP "abc def") (Just ("abc","def"))),
                   TestCase ((@=?) (runParser nomP "abc    ") (Just ("abc",""))),
                   TestCase ((@=?) (runParser nomP "abc   def") (Just ("abc","def"))),
                   TestCase ((@=?) (runParser nomP " abc") (Nothing))]


--Q3
test03 = TestList [ TestCase ((@=?) (runParser varP "") (Nothing)),
                    TestCase ((@=?) (runParser varP "abc") (Just (Var "abc",""))),
                    TestCase ((@=?) (runParser varP "abc def") (Just (Var "abc","def"))),
                    TestCase ((@=?) (runParser varP "abc    ") (Just (Var "abc",""))),
                    TestCase ((@=?) (runParser varP " abc") (Nothing))]

-- Q4.
test04 = TestList [ --TestCase ((@=?) (applique []) (undefined)),
                    TestCase ((@=?) (applique [Var "a"]) (Var "a")),
                    TestCase ((@=?) (applique [Var "a", Var "b"]) (App (Var "a") (Var "b"))),
                    TestCase ((@=?) (applique [Var "a", Var "b", Var "c"]) (App (App (Var "a") (Var "b")) (Var "c")))]


-- Q5
test05 = TestList [ TestCase ((@=?) (runParser exprsP "") (Nothing)),
                    TestCase ((@=?) (runParser exprsP "a") (Just (Var "a",""))),
                    TestCase ((@=?) (runParser exprsP "a    ") (Just (Var "a",""))),
                    TestCase ((@=?) (runParser exprsP "a b") (Just (App (Var "a") (Var "b"),""))),
                    TestCase ((@=?) (runParser exprsP "a b c") (Just (App (App (Var "a") (Var "b")) (Var "c"),""))),
                    TestCase ((@=?) (runParser exprsP "a b c d") (Just (App (App (App (Var "a") (Var "b")) (Var "c")) (Var "d"),"")))]

-- Q6
test06 = TestList [ TestCase ((@=?) (runParser lambdaP "\\ x -> x") (Just (Lam "x" (Var "x"),""))),
                    TestCase ((@=?) (runParser lambdaP "\\ x -> x x") (Just (Lam "x" (App (Var "x") (Var "x")),""))),
                    TestCase ((@=?) (runParser lambdaP "\\ x -> x x  ") (Just (Lam "x" (App (Var "x") (Var "x")),"")))]

-- Q7
test07 = TestCase ((@=?) (runParser exprP "\\x -> x") (Just (Lam "x" (Var "x"),"")))


-- Q8
test08 = TestList [ TestCase ((@=?) (runParser exprParentheseeP "()") (Nothing)),
                    TestCase ((@=?) (runParser exprParentheseeP "(x)") (Just (Var "x",""))),
                    TestCase ((@=?) (runParser exprParentheseeP "(x y)") (Just (App (Var "x") (Var "y"),""))),
                    TestCase ((@=?) (runParser exprParentheseeP "(\\x -> x)") (Just (Lam "x" (Var "x"),""))),

                    TestCase ((@=?) (runParser exprP "(\\x -> x) y") (Just (Lam "x" (Var "x"),"y"))),
                    TestCase ((@=?) (runParser exprsP "(\\x -> x) y") (Just (App (Lam "x" (Var "x")) (Var "y"),""))),
                    TestCase ((@=?) (runParser exprP "\\x -> x y") (Just (Lam "x" (App (Var "x") (Var "y")),""))),
                    TestCase ((@=?) (runParser exprsP "\\x -> x y") (Just (Lam "x" (App (Var "x") (Var "y")),"")))]


-- Q9
test09 = TestList [ TestCase ((@=?) (runParser nombreP "123") (Just (Lit (Entier 123),""))),
                    TestCase ((@=?) (runParser nombreP "  123") (Nothing)),
                    TestCase ((@=?) (runParser nombreP "123  ") (Just (Lit (Entier 123),"")))]


-- Q10
test10 = TestList [ TestCase ((@=?) (runParser booleenP "True") (Just (Lit (Bool True),""))),
                    TestCase ((@=?) (runParser booleenP "False") (Just (Lit (Bool False),"")))]


-- Q11
test11 = TestList [ TestCase ((@=?) (runParser exprsP " x x") (Nothing)),
                    TestCase ((@=?) (runParser expressionP " x x") (Just (App (Var "x") (Var "x"),"")))]


-- Q12
test12 = TestList [ --TestCase ((@=?) (show (ras "")) ("*** Exception: Erreur d’analyse syntaxique")),
                    TestCase ((@=?) (ras "x y") (App (Var "x") (Var "y"))),
                    TestCase ((@=?) (ras "\\x -> x x") (Lam "x" (App (Var "x") (Var "x"))))]


-- Q14
test14 = TestList [ TestCase ((@=?) (show (VFonctionA undefined)) ("λ")),
                    TestCase ((@=?) (show (VLitteralA (Entier 12))) ("12")),
                    TestCase (assertBool (show (VLitteralA (Bool True))) (True))]


-- Q15
test15 = TestList [ TestCase ((@=?) (show (interpreteA [] (ras "12"))) ("12")),
                    TestCase (assertBool (show (interpreteA [] (ras "True"))) (True)),
                    TestCase ((@=?) (show (interpreteA [] (ras "λx -> x"))) ("λ")),
                    TestCase ((@=?) (show (interpreteA [] (ras "(λx -> x) 12"))) ("12")),
                    --TestCase ((@=?) (show (interpreteA [] (ras "x"))) ("*** Exception: Maybe.fromJust: Nothing")),
                    TestCase ((@=?) (show (interpreteA [("x", VLitteralA (Entier 45))] (ras "x"))) ("45")),
                    TestCase ((@=?) (show (interpreteA [("x", VLitteralA (Entier 45))] (ras "(λx -> x) 67"))) ("67"))]

-- Q16
test16 = TestList [ TestCase ((@=?) (show (interpreteA [("neg", negA)] (ras "neg 12"))) ("-12")),
                    TestCase ((@=?) (show (interpreteA [("neg", negA)] (ras "neg"))) ("λ"))]
                    --TestCase ((@=?) (show (interpreteA [("neg", negA)] (ras "neg True"))) ("*** Exception: Interprete.hs:92:20-71: Non-exhaustive patterns in lambda"))]

-- Q17
test17 = TestList [ TestCase ((@=?) (show (interpreteA [("add", addA)] (ras "add"))) ("λ")),
                    TestCase ((@=?) (show (interpreteA [("add", addA)] (ras "add 12"))) ("λ")),
                    TestCase ((@=?) (show (interpreteA [("add", addA)] (ras "add 12 23"))) ("35")),
                    TestCase ((@=?) (show (interpreteA [("sub", subA)] (ras "sub 20 15"))) ("5")),
                    TestCase ((@=?) (show (interpreteA [("mult", multA)] (ras "mult 8 9"))) ("72")),
                    TestCase ((@=?) (show (interpreteA [("div", divA)] (ras "div 35 5"))) ("7"))]

-- Q18
test18 = TestList [ TestCase ((@=?) (show (interpreteA envA (ras "soust 5 3"))) ("2")),
                    TestCase ((@=?) (show (interpreteA envA (ras "mult 12 4"))) ("48")),
                    TestCase ((@=?) (show (interpreteA envA (ras "quot (mult 12 4) 2"))) ("24")),
                    TestCase ((@=?) (show (interpreteA envA (ras "quot (mult 12 4) 5"))) ("9"))]

-- Q19
test19 = TestList [ TestCase ((@=?) (show (interpreteA [("if", ifthenelseA)] (ras "if True 1 2"))) ("1")),
                    TestCase ((@=?) (show (interpreteA [("if", ifthenelseA)] (ras "if False 1 2"))) ("2")),
                    TestCase ((@=?) (show (interpreteA [("if", ifthenelseA)] (ras "if True 1"))) ("λ"))]

-- Q21
test21 = TestList [ TestCase ((@=?) (show (VFonctionB undefined)) ("λ")),
                    TestCase ((@=?) (show (VLitteralB (Entier 12))) ("12")),
                    TestCase (assertBool (show (VLitteralB (Bool True))) (True))]


-- Q22
test22 = TestList [ TestCase ((@=?) (show (interpreteB [] (ras "1"))) ("Right 1")),
                    TestCase ((@=?) (show (interpreteB [] (ras "λx -> x"))) ("Right λ")),
                    TestCase ((@=?) (show (interpreteB [] (ras "(λx -> x) 1"))) ("Right 1")),
                    TestCase ((@=?) (show (interpreteB [] (ras "λx -> y"))) ("Right λ")),
                    TestCase ((@=?) (show (interpreteB [] (ras "(λx -> y) 1"))) ("Left \"la variable y n'est pas definie\"")),
                    TestCase ((@=?) (show (interpreteB [] (ras "x"))) ("Left \"la variable x n'est pas definie\"")),
                    TestCase ((@=?) (show (interpreteB [] (ras "1 2"))) ("Left \"1 n'est pas une fonction, application impossible\"")),
                    TestCase ((@=?) (show (interpreteB [] (ras "1 2 x"))) ("Left \"1 n'est pas une fonction, application impossible\"")),
                    TestCase ((@=?) (show (interpreteB [] (ras "x 1 2 x"))) ("Left \"la variable x n'est pas definie\"")),
                    TestCase ((@=?) (show (interpreteB [] (ras "x 1 2"))) ("Left \"la variable x n'est pas definie\"")),
                    TestCase ((@=?) (show (interpreteB [] (ras "(λx -> x) x"))) ("Left \"la variable x n'est pas definie\"")),
                    TestCase ((@=?) (show (interpreteB [] (ras "(λx -> x) y"))) ("Left \"la variable y n'est pas definie\""))]


-- Q23
test23 = TestList [ TestCase ((@=?) (show (interpreteB [("add",addB)] (ras "add 1 2"))) ("Right 3")),
                    TestCase ((@=?) (show (interpreteB [("add",addB)] (ras "add 1 True"))) ("Left \"True n'est pas un entier\"")),
                    TestCase ((@=?) (show (interpreteB [("add",addB)] (ras "add (λx -> x) y"))) ("Left \"\\955 n'est pas un entier\"")),
                    TestCase ((@=?) (show (interpreteB [("add",addB)] (ras "add 1 y"))) ("Left \"la variable y n'est pas definie\"")),
                    TestCase ((@=?) (show (interpreteB [("add",addB)] (ras "add 1 ((λx -> x) y)"))) ("Left \"la variable y n'est pas definie\"")),
                    TestCase ((@=?) (show (interpreteB [("add",addB)] (ras "add 1 (2 y)"))) ("Left \"2 n'est pas une fonction, application impossible\""))]


-- Q24
test24 = TestList [ TestCase ((@=?) (show (interpreteB [("quot",quotB)] (ras "quot 13 4"))) ("Right 3")),
                    TestCase ((@=?) (show (interpreteB [("quot",quotB)] (ras "quot 13 0"))) ("Left \"division par zero\""))]


-- Q25
test25 = TestList [ TestCase ((@=?) (show (VFonctionC undefined)) ("λ")),
                    TestCase ((@=?) (show (VLitteralC (Entier 12))) ("12")),
                    TestCase (assertBool (show (VLitteralC (Bool True))) (True))]


-- Q26
test26 = TestList [ TestCase ((@=?) (show (interpreteC [] (ras "1"))) "(\"\",1)"),
                    TestCase ((@=?) (show (interpreteC [] (ras "(λx -> x) 1"))) "(\".\",1)"),
                    TestCase ((@=?) (show (interpreteC [] (ras "(λx -> λy -> x) 1 2"))) "(\"..\",1)")]

-- Q27
test27 = TestList [ TestCase ((@=?) (show (interpreteC [("ping",pingC)] (ras "ping 12"))) ("(\".p\",12)")),
                    TestCase ((@=?) (show (interpreteC [("ping",pingC)] (ras "ping ping"))) ("(\".p\",λ)")),
                    TestCase ((@=?) (show (interpreteC [("ping",pingC)] (ras "ping ping 12"))) ("(\".p.p\",12)")),
                    TestCase ((@=?) (show (interpreteC [("ping",pingC)] (ras "(λx -> x x) ping 12"))) ("(\"..p.p\",12)"))]

-- Q28
test28 = TestList [ TestCase ((@=?) (show (VFonctionM undefined)) ("λ")),
                    TestCase ((@=?) (show (VLitteralM (Entier 12))) ("12")),
                    TestCase (assertBool (show (VLitteralM (Bool True))) (True))]
      

-- Q29
test29 = TestList [ TestCase ((@=?) (show (interpreteSimpleM [] (ras "1"))) ("S 1")),
                    TestCase ((@=?) (show (interpreteSimpleM [] (ras "λx -> x"))) ("S λ")),
                    TestCase ((@=?) (show (interpreteSimpleM [] (ras "(λx -> x) 1"))) ("S 1")),
                    TestCase ((@=?) (show (interpreteSimpleM [] (ras "(λx -> λy -> x) 1 2"))) ("S 1")),
                    TestCase ((@=?) (show (interpreteSimpleM [] (ras "λx -> y"))) ("S λ"))]


-- Q30
test30 = TestList [ TestCase ((@=?) (show (Interprete.fmap (\x->2*x) (S 1))) ("S 2")),
                    TestCase ((@=?) (show ((Interprete.<*>) (S (\x->x+1)) (S 5))) ("S 6"))]


-- Q32
test32 = TestList [ TestCase ((@=?) (show (interpreteSimpleM [] (ras "1"))) (show (interpreteS [] (ras "1")))),
                    TestCase ((@=?) (show (interpreteSimpleM [] (ras "λx -> x"))) (show (interpreteS [] (ras "λx -> x")))),
                    TestCase ((@=?) (show (interpreteSimpleM [] (ras "(λx -> x) 1"))) (show  (interpreteS [] (ras "(λx -> x) 1")))),
                    TestCase ((@=?) (show (interpreteSimpleM [] (ras "(λx -> λy -> x) 1 2"))) (show  (interpreteS [] (ras "(λx -> λy -> x) 1 2")))),
                    TestCase ((@=?) (show (interpreteSimpleM [] (ras "λx -> y"))) (show  (interpreteS [] (ras "λx -> y"))))]


-- Q33
test33 = TestList [ TestCase ((@=?) (show (interpreteMT [("ping", pingM)] (ras "ping 12"))) ("T (\"p\",12)")),
                    TestCase ((@=?) (show (interpreteMT [("ping", pingM)] (ras "ping 12"))) ("T (\"p\",12)")),
                    TestCase ((@=?) (show (interpreteMT [("ping", pingM)] (ras "ping ping"))) ("T (\"p\",λ)")),
                    TestCase ((@=?) (show (interpreteMT [("ping", pingM)] (ras "ping ping 12"))) ("T (\"pp\",12)")),
                    TestCase ((@=?) (show (interpreteMT [("ping", pingM)] (ras "(λx -> x ) ping 12"))) ("T (\"p\",12)")),
                    TestCase ((@=?) (show (interpreteMT [("ping", pingM)] (ras "(λx -> x x) ping 12"))) ("T (\"pp\",12)"))]


-- Q34
test34 = TestList [ TestCase ((@=?) ("T " ++ show (interpreteC [("ping",pingC)] (ras "ping 12"))) (show (interpreteMT' [("ping",pingM)] (ras "ping 12")))),
                    TestCase ((@=?) ("T " ++ show (interpreteC [("ping",pingC)] (ras "ping ping"))) (show (interpreteMT' [("ping",pingM)] (ras "ping ping")))),
                    TestCase ((@=?) ("T " ++ show (interpreteC [("ping",pingC)] (ras "ping ping 12"))) (show (interpreteMT' [("ping",pingM)] (ras "ping ping 12")))),
                    TestCase ((@=?) ("T " ++ show (interpreteC [("ping",pingC)] (ras "(λx -> x x) ping 12"))) (show (interpreteMT' [("ping",pingM)] (ras "(λx -> x x) ping 12"))))]


-- Q36
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

test36 = TestList [ TestCase ((@=?) (replace (show (interpreteB [] (ras "1"))) "Right" "Succes") (show (interpreteEE [] (ras "1")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "λx -> x"))) "Right" "Succes") (show (interpreteEE [] (ras "λx -> x")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "(λx -> x) 1"))) "Right" "Succes") (show (interpreteEE [] (ras "(λx -> x) 1")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "λx -> y"))) "Right" "Succes") (show (interpreteEE [] (ras "λx -> y")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "(λx -> y) 1"))) "Left" "Erreur") (show (interpreteEE [] (ras "(λx -> y) 1")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "x"))) "Left" "Erreur") (show (interpreteEE [] (ras "x")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "1 2"))) "Left" "Erreur") (show (interpreteEE [] (ras "1 2")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "1 2 x"))) "Left" "Erreur") (show (interpreteEE [] (ras "1 2 x")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "x 1 2 x"))) "Left" "Erreur") (show (interpreteEE [] (ras "x 1 2 x")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "x 1 2"))) "Left" "Erreur") (show (interpreteEE [] (ras "x 1 2")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "(λx -> x) x"))) "Left" "Erreur") (show (interpreteEE [] (ras "(λx -> x) x")))),
                    TestCase ((@=?) (replace (show (interpreteB [] (ras "(λx -> x) y"))) "Left" "Erreur") (show (interpreteEE [] (ras "(λx -> x) y"))))]


-- Q38
test38 = TestList []


-- Q39
test39 = TestList []


-- Q40
test40 = TestList []


-- Q41
test41 = TestList []


-- Q42
test42 = TestList []