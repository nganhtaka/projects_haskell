-- Q6.
-- Teste pour la fonction hauteur :
quickCheck prop_hauteurPeigne

-- Teste pour la fonction hauteur' :
quickCheck prop_hauteurPeigne'

-- Q7.
-- Teste pour la fonction taille :
quickCheck prop_taillePeigne

-- Teste pour la fonction taille' :
quickCheck prop_taillePeigne'


-- Q10
-- Teste pour la fonction estComplet :
quickCheck prop_completPeigne

-- Teste pour la fonction estComplet' :
quickCheck prop_completPeigne'


-- Q11
-- Teste pour la fonction complet :
quickCheckWith (stdArgs {maxSize=17}) prop_complet1

-- Teste pour la fonction complet de l'arbre de l'hauteur 18 :
quickCheckWith (stdArgs {maxSize=18}) prop_complet1
quickCheckWith (stdArgs {maxSize=19}) prop_complet1


-- Q13.
take 5 unicodeL
-- [((),'a'),((),'b'),((),'c'),((),'d'),((),'e')]

completeList 2
-- [((),'a'),((),'b'),((),'c')]

-- Q14.
-- Teste pour la fonction aplatit avec un arbre de hauteur 4 :
quickCheck prop_aplatit

-- Q15.

-- Teste pour la fonction element :
quickCheck prop_element1
quickCheck prop_element2

element 3 tree1
-- True
element 4 tree1
-- False
element 'a' tree2
-- True
element 'b' tree2
-- False

element '3' tree2
-- False
element 'd' tree5
-- True
element 'z' tree5
-- False
element 'f' tree5
-- True


abr = foldl (flip $ insert) Feuille ['a'..'z']
quickCheck (prop_insert1 abr)
quickCheck (prop_insert3 abr)