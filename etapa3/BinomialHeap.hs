module BinomialHeap where

import Data.Function (on)
import Data.List hiding (insert)
import Data.Foldable
import Data.Char (toUpper)

{-
    Reprezentarea unui arbore binomial, având priorități de tipul p și chei
    de tipul k. Conform proprietății definitorii a acestor arbori, fiecare
    copil i dintre cei r copii ai unui nod de rang r, cu 1 <= i <= r, trebuie
    să aibă exact r-i copii la rândul său, adică r-1, r-2, ..., 1, 0 copii,
    exact în această ordine descrescătoare. Dacă rădăcina are r copii, atunci,
    în conjuncție cu proprietate anterioară, întregul arbore are 2^r noduri.
-}
data BinomialTree p k
    = EmptyTree
    | Node { prio :: p, key :: k, children :: [BinomialTree p k] }
    deriving (Eq)

{-
    Reprezentarea unui heap binomial, ca listă de arbori binomiali ce respectă
    proprietatea de heap. Mai precis, considerăm că, în fiecare arbore binomial,
    rădăcina are cea mai mică prioritate; analog pt subarbori. Câmpul size
    desemnează numărul de elemente din heap (nu numărul de arbori). Mai precis,
    dimensiunea este egală cu suma dimensiunilor arborilor din listă. Cum
    dimensiunea unui arbore de rang r este 2^r, dimensiunea este o sumă
    de puteri ale lui 2, unde exponenții sunt dimensiunile câmpurilor children
    (rangurile) din rădăcinile arborilor nevizi.
-}
data BinomialHeap p k = BinomialHeap { size :: Int, trees :: [BinomialTree p k] }
    deriving (Eq)

{-
    *** TODO ***

    Construiește recursiv un arbore binomial de rang r din doi arbori binomiali
    de rang r-1, atașând unul dintre arbori drept prim copil al rădăcinii
    celuilalt. Maniera în care se realizează atașarea trebuie să țină cont
    de faptul că cei doi arbori respectă proprietatea de heap, și că arborele
    rezultant trebuie de asemenea să o respecte. Astfel, arborele cu cheia mai
    mică din rădăcină trebuie să încorporeze arborele cu cheia mai mare.

    Atenție! Cei doi arbori primiți ca parametru au întotdeauna același rang,
    conform principiului de construcție. Funcția nu necesită parcurgeri
    recursive, putând opera direct la nivelul rădăcinilor.

    Constrângeri: utilizați gărzi.

    Hint: pt pattern matching, pot fi utile alias-urile (@).

    Exemple:

    > attach (Node 0 'a' []) (Node 1 'b' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}

    > attach (Node 1 'b' []) (Node 0 'a' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}
-}
attach :: Ord p => BinomialTree p k -> BinomialTree p k -> BinomialTree p k
attach t1@(Node p1 k1 c1) t2@(Node p2 k2 c2)
    | p1 < p2  = Node p1 k1 (t2 : c1) -- daca prioritatea primului arbore e mai mica decat al doilea, il punem pe al doilea(t2) copil
    | otherwise = Node p2 k2 (t1 : c2) -- daca nu invers
attach EmptyTree t = t -- daca unul dintre arbori e EmptyTree intoarcem arborele care nu e EmptyTree(t)
attach t EmptyTree = t

{-
    *** TODO ***

    Introduce un arbore binomial nevid într-o listă cu alți arbori binomiali,
    toți arborii respectând proprietatea de heap. Cum nu pot exista simultan
    în listă doi arbori binomiali cu același rang, la întâlnirea unui arbore
    cu același rang cu cel care se dorește introdus, este necesară atșarea
    unuia la celălalt, cu crearea unui transport.

    Operația o oglindește pe cea de incrementare a unui număr binar din etapa 1.
    Astfel, elementele EmptyTree sunt analoagele biților 0, iar elementele Node,
    biților 1. O diferență este că, în această etapă, biții 1 „nu mai arată toți
    la fel”, ci elementele Node au rangul dat de poziția în listă. Spre exemplu:
    * un element Node de pe poziția 0 din listă trebuie să aibă rangul 0
      (dimensiunea 2^0 = 1)
    * un element Node de pe poziția 1 din listă trebuie să aibă rangul 1
      (dimensiunea 2^1 = 2)
    * un element Node de pe poziția 2 din listă trebuie să aibă rangul 2
      (dimensiunea 2^2 = 4)
    etc.

    Gestiunea transportului apărut în incrementare corespunde operației attach.
    Modul în care va fi utilizată mai departe funcția insertTree garantează
    respectarea presupunerilor funcției attach, cum că arborii primiți ca
    parametru au întotdeauna același rang.

    Constrângeri: utilizați
    * construcția case
    * funcția attach.

    Exemple:

    > insertTree (Node 1 'a' []) []
    [Node {prio = 1, key = 'a', children = []}]

    > insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > insertTree (Node 3 'c' []) $ insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}
rank :: BinomialTree p k -> Int -- dimeansiunea unui arbore e rankul
rank EmptyTree = 0
rank (Node p k c) = 1 + length c -- 1 plus lungimea listei copiilor

insertTree :: Ord p => BinomialTree p k -> [BinomialTree p k] -> [BinomialTree p k]
insertTree tree [] = [tree]  -- daca e goala returam arborele singur sa formam lista
insertTree tree (t:ts) = case compare (rank tree) (rank t) of   -- comparam rankul inputului cu rankul primului arbore din lista
  LT -> tree : t : ts               -- daca e mai mic adaugam la inceput de lista
  GT -> insertTree (attach tree t) ts   -- daca e mai mare atasam inputul ca copil la la primul arbore
  EQ -> EmptyTree : insertTree (attach tree t) ts -- daca avem rank egal il atasam la primul arbore si adaugam un EmptyTree 
  -- la inceput pentru a satisface propritatea de heap

{-
    *** TODO ***

    Heap-ul vid.
-}
emptyHeap :: BinomialHeap p k
emptyHeap = BinomialHeap { size = 0, trees = [] }

{-
    *** TODO ***

    Introduce o cheie cu prioritatea aferentă într-un heap binomial.

    Constrângeri: utilizați funcția insertTree.

    Exemple:

    > insert 1 'a' emptyHeap
    BinomialHeap 
        { size = 1
        , trees = [Node {prio = 1, key = 'a', children = []}]
        }

    > insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap
        { size = 2
        , trees = [ EmptyTree
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }

    > insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap 
        { size = 3
        , trees = [ Node {prio = 3, key = 'c', children = []}
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }
-}
insert :: Ord p => p -> k -> BinomialHeap p k -> BinomialHeap p k
insert prio key (BinomialHeap size trees) = -- functia are 3 argumente prio, key si arborele, la fiecare insert crestem dimensiunea 
    BinomialHeap (size+1) (insertTree (Node prio key []) trees) -- si intoarcem trees folosindu-ne de insertTree ca sa inseram

{-
    *** TODO ***

    Dacă heap-ul nu este vid, întoarce perechea formată din prioritatea minimă
    și cheia aferentă; în caz contrar, întoarce Nothing. Cum toți arborii din
    listă respectă proprietatea de heap, este suficient să parcurgeți doar
    rădăcinile pt a o determina pe cea cu prioritate minimă, fără a fi necesară
    explorarea nivelurilor inferioare ale arborilor.

    Constrângeri: pt selectarea arborilor nevizi din listă (ignorând elementele
    EmptyTree), utilizați list comprehension cu pattern matching.

    Hint: pt determinarea elementului minim dintr-o listă pe baza valorii
    calculate de o funcție numită criteriu, utilizați o expresie de forma:
    minimumBy (compare `on` criteriu) lista.

    Exemple:

    > findMin emptyHeap
    Nothing

    > findMin $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    Just (1,'a')
-}
findMin :: Ord p => BinomialHeap p k -> Maybe (p, k)  --returam un tulpe cu priority minim p si key k 
findMin (BinomialHeap _ trees) =
  case [node | Node prio key _ <- trees, let node = (prio, key)] of  -- folosim let sa extragem prioritatea si cheia nodului
    [] -> Nothing  -- daca avem emptyHeap returnam Nothing
    xs -> Just $ minimumBy (compare `on` fst) xs  -- folosim minimumBy ca sa gasim tulpe cu prioritate minima comparand dupa primul element

{-
    Funcția zipExtend este similară funcției predefinite zip. Scopul ei este
    de a compensa limitarea funcției zip, care se oprește când atinge sfârșitul
    listei mai scurte. Primii doi parametri reprezintă valori cu care se extind
    prima, respectiv a doua listă, în funcție de care listă este mai scurtă.
    O puteți folosi în cele ce urmează.

    Exemple:

    > zipExtend 0 'z' [1,2] "abcd"
    [(1,'a'),(2,'b'),(0,'c'),(0,'d')]

    > zipExtend 0 'z' [1,2,3,4] "ab"
    [(1,'a'),(2,'b'),(3,'z'),(4,'z')]
-}
zipExtend :: a -> b -> [a] -> [b] -> [(a, b)]
zipExtend a' _  [] bs = zip (repeat a') bs
zipExtend _  b' as [] = zip as (repeat b')
zipExtend a' b' (a : as) (b : bs) = (a, b) : zipExtend a' b' as bs

{-
    *** TODO ***

    Combină două liste de arbori binomiali care respectă proprietatea de heap.
    Observațiile din comentariile funcției insertTree, legate de necesitatea
    atașării arborilor cu același rang, rămân valabile.

    Operația o oglindește pe cea de adunare a două numere binare din etapa 1.

    Constrângeri:
    * evitați recursivitatea explicită
    * utilizați funcția zipExtend pt a facilita aplicarea unor funcționale.

    Exemple:

    > mergeTrees [Node 1 'a' []] []
    [Node {prio = 1, key = 'a', children = []}]

    > mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > mergeTrees [Node 3 'c' []] $ mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}
mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
mergeTrees trees1 trees2 = let result = mapAccumL combineTrees EmptyTree (zipExtend EmptyTree EmptyTree trees1 trees2)  -- combinam arborii folosind combineTrees 
    in case fst result of  -- cu zipExtend daca o lista e mai scurta punem EmptyTree sa aiba aceeasi lungime si apoi facem zip pe ele, 
        EmptyTree -> snd result  -- extragem al doilea element daca e Empty, iar daca e primul concatenam cu urmatorul arbore pe mai multe cazuri 
        t -> snd result ++ [t]
    where
        combineTrees EmptyTree (EmptyTree, EmptyTree) = (EmptyTree, EmptyTree)
        combineTrees EmptyTree (t@(Node _ _ p), EmptyTree) = (EmptyTree, t)
        combineTrees EmptyTree (EmptyTree, t@(Node _ _ p)) = (EmptyTree, t)
        combineTrees EmptyTree (t1@(Node _ _ p1), t2@(Node _ _ p2)) = (attach t1 t2, EmptyTree)  -- cand e la final dam attach intre cei doi si se termina 
        combineTrees carry@(Node _ _ p) (EmptyTree, EmptyTree) = (EmptyTree, carry)
        combineTrees carry@(Node _ _ p) (t@(Node _ _ p'), EmptyTree) = (attach carry t, EmptyTree)
        combineTrees carry@(Node _ _ p) (EmptyTree, t@(Node _ _ p')) = (attach carry t, EmptyTree)
        combineTrees carry@(Node _ _ p) (t1@(Node _ _ p1), t2@(Node _ _ p2)) = (attach t1 t2, carry)
        -- facem carry pentru a retine arborele de la attach t1 cu t2 pentru a continua mai departe    


{-
    *** TODO ***

    Combină două heap-uri binomiale.

    Constrângeri: utilizați funcția mergeTrees.

    Exemple: similare cu cele de la mergeTrees.
-}
merge :: Ord p => BinomialHeap p k -> BinomialHeap p k -> BinomialHeap p k
merge heap1 heap2 = BinomialHeap (size heap1 + size heap2) (mergeTrees (trees heap1) (trees heap2))  -- dimeniunea noului Heap e suma celor doua heap1 si heap2 
 -- facem mergeTrees pe ele, trees rezultatul la merge de heap1 cu heap2

----------------------------------- Etapa 3 ------------------------------------

{-
    *** TODO ***

    Funcție ajutătoare, care izolează, pe rând, câte un element al unei liste,
    înlocuindu-l în cadrul listei cu un altul primit ca parametru. Întoarce
    o listă de perechi, care conțin pe prima poziție un element al listei
    originale, iar pe a doua, lista din care provine elementul, dar după
    înlocuirea de mai sus. Se va evita introducerea elementului înlocuitor
    pe ultima poziție din listă.

    Constrângeri:
    * puteți utiliza și recursivitate explicită, dar utilizați funcționale
      pe cât posibil (măcar pt părți din rezolvare)
    * NU este permisă adăugarea de asumpții asupra tipului a, e.g. Eq a.

    Exemple:

    > isolate 0 [1,2,3]'
    
    [(1,[0,2,3]),(2,[1,0,3]),(3,[1,2])]  -- fără 0 în ultima listă
-}
isolate :: a -> [a] -> [(a, [a])]
isolate placeHolder xs = zip xs modifiedLists  -- facem zip intre lista initiala si toate listele modificate pentru a ne da rezultatul
  where
    modifiedLists = map (\(ys, zs) -> if null zs then ys else ys ++ placeHolder:zs) $ zip (inits xs) (tail $ tails xs) -- generam o lista de perechi
  -- in care facem perechi dintre primul element si coada, lista crescand si coada scazand, iar cu map iteram pentru a modifica listele cu placeHolderul

-- daca coada zs e empty inseamna ca suntem la finalul listei si nu mai trebuie sa izolam si lasam ys
-- daca nu e punem prima parte din lista(ys) dupa placeHolderul si dupa tailul ramas din lista(zs)

{-
    *** TODO ***

    Elimină din heap prima rădăcină de prioritate minimă. Aceasta presupune
    înlăturarea întregului arbore cu rădăcina de prioritate minimă din lista
    de arbori ai heap-ului (prin înlocuirea lui cu EmptyTree) și apoi
    combinarea (mergeTrees) noii liste de arbori cu lista de subarbori (orfani)
    ai rădăcinii tocmai înlăturate.

    Atenție! Având în vedere că lista de arbori ai heap-ului este ordonată
    crescător în raport cu rangul, iar lista de subarbori orfani este ordonată
    descrescător (conform structurii arborilor binomiali), este necesară
    inversarea ultimeia înainte de combinarea celor două liste!

    Constrângeri: utilizați isolate.

    Hint: vedeți indicațiile de la findMin pentru determinarea primei rădăcini
    de prioritate minimă.

    Exemple:

    > removeMin emptyHeap
    BinomialHeap {size = 0, trees = []}

    > removeMin $ insert 1 'a' emptyHeap
    BinomialHeap {size = 0, trees = []}

    > removeMin $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap {size = 1, trees = [Node {prio = 2, key = 'b', children = []}]}

    > removeMin $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap
        { size = 2
        , trees = [ EmptyTree
                  , Node {prio = 2, key = 'b', children = [Node {prio = 3, key = 'c', children = []}]}
                  ]
        }

    După ce implementați, răspundeți la întrebarea: Care este contribuția
    evaluării leneșe la utilizarea eficientă a funcției isolate?
-}  --folosim isolate ca sa mentinem proprietea de Heap si pentru eficienta (izolam prima data tree minim)
removeMin :: (Ord p, Eq k) => BinomialHeap p k -> BinomialHeap p k
removeMin heap@(BinomialHeap size trees) = case findMin heap of
    Nothing -> BinomialHeap size []  -- daca nu avem nimic returam un heap de acelasi size ca EmptyTree
    Just (minPrio, minKey) ->  
        let isolatedTrees = isolate EmptyTree trees  -- folosim isolate ca sa impartim in segmente de tipul arbore si fratii sai
            (minTree, restTrees) = head [ (t, rs) | (t, rs) <- isolatedTrees, rootKey t == Just minKey ]  -- t este egala cu cheia minima si rs sunt arborii de dupa t, iar cu head luam prima pereche care satisface
            orphanedSubtrees = reverse $ children minTree  -- primeste lista inversata a copiilor lui minTree, care au devenit orfani 
        in BinomialHeap (size - 1) $ mergeTrees orphanedSubtrees restTrees  -- dupa decrementam cu 1 adica minTree-ul nostru si combinam orphanedSubtrees cu restTrees

rootKey :: BinomialTree p k -> Maybe k  -- folosim ca sa returnam cheia radacinii arborelui
rootKey EmptyTree = Nothing
rootKey (Node _ k _) = Just k


{-
    *** TODO ***

    Instanțiați clasa Show pt tipul (BinomialTree p k) în următorul mod:
    * un arbore vid este reprezentat prin "*"
    * un arbore nevid este reprezentat prin:

        prioritate (cheie)
          <copil 1>
          ...
          <copil n>
      
      unde reprezentarea copiilor este indentată cu 2 spații față de cea
      a părintelui.
    
    Hint: replicate și intercalate.

    Exemple:

    > EmptyTree
    *

    > Node 1 'a' []
    1 ('a')

    > Node 1 'a' [Node 2 'b' []]
    1 ('a')
      2 ('b')

    > Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    1 ('a')
      3 ('c')
        4 ('d')
      2 ('b')
-}
instance (Show p, Show k) => Show (BinomialTree p k) where  -- instanta a clase Show
  show tree = intercalate "\n" $ showTree 0 tree  -- convertim arborele intr-o lista de caractere cu showTree apoi folosim intercalate pentru a pune linii noi
      where  -- tree arborele curent procesat
        showTree _ EmptyTree = ["*"]  -- daca e Empty punem *
        showTree depth (Node prio key children) =  -- depth adancimea nodului curent
            [concat [replicate (depth * 2) ' ', show prio, " (", show key, ")"]] ++  -- pentru fiecare adancime punem 2 spatii inainte, dupa punem prio si dupa cheia 
            concatMap (showTree (depth + 1)) children  -- si dupa concatenam in continuare cu copii

{-
    *** TODO ***

    Instanțiați clasa Show pt tipul (BinomialHeap p k) cu reprezentarea:

    <arbore 1>
    ...
    <arbore n>
    
    Exemple:

    > insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap                                        
    3 ('c')
    1 ('a')
      2 ('b')
    
    > insert 5 'e' $ insert 4 'd' $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    5 ('e')
    *
    1 ('a')
      3 ('c')
        4 ('d')
      2 ('b')
-}
instance (Show p, Show k) => Show (BinomialHeap p k) where
    show (BinomialHeap _ trees) = intercalate "\n" $ map show trees  -- folosim map pentru a aplica show fiecarui arbore din lista 

{-
    *** TODO ***

    Instanțiați clasa Functor cu constructorul (BinomialTree p). Observați
    că clasa Functor așteaptă constructori unari de tip, dar BinomialTree
    este binar; acest lucru înseamnă că BinomialTree trebuie aplicat parțial
    pe tipul priorităților, care conduce la fixarea acestuia, și varierea doar
    a tipului cheilor (vedeți tipul lui fmap mai jos).

    fmap aplică o funcție pe cheile din arbore, fără a altera structura acestuia.

    Exemple:

    > fmap toUpper $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    1 ('A')
      3 ('C')
        4 ('D')
      2 ('B')
-}
instance Functor (BinomialTree p) where  -- rezultatul sunt cheile la care s-a aplicat functia toUpper(f) 
    -- fmap :: (k1 -> k2) -> BinomialTree p k1 -> BinomialTree p k2 
    fmap f (Node p k ts) = Node p (f k) (fmap (fmap f) ts)  -- aplicam functia f pe cheia k pentru a obtine noua cheie, apoi aplicam fmap recursiv pe fiecare subarbore
    fmap _ EmptyTree = EmptyTree  -- din ts pentru a aplica functia f pe cheile acestora 
    -- daca e EmptyTree nu mai aplicam nicio functie si returnam EmptyTree

{-
    *** TODO ***

    Instanțiați clasa Functor cu constructorul (BinomialHeap p). Observațiile
    aferente instanței pt (BinomialTree p) de mai sus sunt valabile și aici.

    fmap aplică o funcție pe cheile din heap, fără a altera structura listei
    de arbori sau a arborilor înșiși.

    Exemple:

    > fmap toUpper $ insert 5 'e' $ insert 4 'd' $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    5 ('E')
    *
    1 ('A')
      3 ('C')
        4 ('D')
      2 ('B')
-}
instance Functor (BinomialHeap p) where
    -- fmap :: (k1 -> k2) -> BinomialHeap p k1 -> BinomialHeap p k2
    fmap f heap = heap { trees = fmap (fmap f) (trees heap) }  -- actualizam trees folosind fmap pentru a aplica recursiv functia 
  -- f pe cheile fiecarui arbore din lista trees
{-
    *** TODO BONUS ***

    Instanțiați clasa Foldable cu constructorul (BinomialTree p) astfel încât
    funcția foldr să împăturească toate cheile din arbore la o singură valoare
    (vedeți tipul lui foldr).

    Dacă încercați să dați o definiție directă, similară celei pe listele
    standard, veți observa că, la un nod din arbore, aveți la dispoziție o cheie
    de tipul k și o listă de acumulatori având tipul [b] (rezultată din
    împăturirea recursivă a copiilor), astfel încât nu este clar pe ce parametru
    de tipul b trebuie aplicată funcția f de la foldr. Cumva, ar trebui să
    combinăm toți acumulatorii din listă într-unul singur de tipul b, dar
    nu știm să facem acest lucru pt orice tip b.

    Prin urmare, vom căuta o abordare alternativă. Observăm că, prin aplicarea
    parțială a funcției f asupra unei chei de tipul k, obținem o funcție cu
    tipul (b -> b). Dacă ne mulțumim doar cu aceste aplicații parțiale, putem
    obține prin împăturirea recursivă a copiilor o listă cu tipul [b -> b], în
    loc de [b]. Pe aceste funcții le putem combina întotdeauna prin compunere,
    indiferent de tipul b. În final, la nivelul rădăcinii, obținem o singură
    funcție, pe care o putem aplica asupra acumulatorului inițial de la foldr.

    Constrângeri: Nu se acceptă liniarizarea prealabilă a arborelui într-o listă
    de chei și aplicarea foldr pe lista rezultantă. Dimpotrivă, în exemplele
    de mai jos, liniarizarea însăși este definită pe baza lui foldr implementat
    de voi.

    Exemple:

    -- Cheia maximă din arbore (caracterele sunt ordonate)
    > foldr max 'a' $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    'd'

    -- maximum este predefinit, într-o manieră similară celei cu foldr de mai sus.
    > maximum $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    'd'

    -- Liniarizarea arborelui, i.e. lista cheilor (String = [Char])
    > foldr (:) [] $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    "acdb"

    -- toList este predefinit prin foldr ca mai sus.
    > toList $ Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]
    "acdb"
-}
instance Foldable (BinomialTree p) where
    -- foldr :: (k -> b -> b) -> b -> BinomialTree p k -> b
    foldr _ acc EmptyTree = acc  -- daca arborele e gol rezultatul e acc neschimbat 
    foldr f acc (Node _ key children) =  -- impaturim recursiv efectiv cheile arborilor intr-o singura valoare cu acumulatorul acc, 
        let childAcc = foldr (\child acc' -> foldr f acc' child) acc children  -- cu foldr traversam aplicand functia f , obtinem suma cheilor
        in f key childAcc  -- aplicam f pe fiecare copil 
      -- si combinam cheia key cu childAcc folosind f obtinand valoarea finala pentru nodul curent