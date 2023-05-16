module BinaryNumber where

import Data.List
import Data.Tuple (swap)

{-
    Reprezentarea unui număr binar ca listă finită sau infinită de biți.
    Primul bit este cel mai puțin semnificativ.

    De exemplu, 6 poate fi reprezentat ca [0, 1, 1, 0, 0, ...].

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type BinaryNumber = [Int]

{-
    *** TODO ***

    Transformă un număr din reprezentarea binară finită în cea zecimală.
    
    Constrângeri: funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând o funcțională (fără recursivitate explicită).

    Exemple:
    
    > toDecimal [0,1,1]
    6
-}
toDecimal :: BinaryNumber -> Int
toDecimal = foldr (\x acc -> x + 2 * acc) 0 -- acc creste cu 2*  si la final avem acc cu valoarea 

{-
    *** TODO ***

    Transformă un număr din reprezentarea zecimală în cea binară infinită.

    Constrângeri: pt bonus 10p, funcția trebuie definită
    * în stil point-free (fără explicitarea parametrului formal)
    * utilizând funcționala unfoldr (fără recursivitate explicită).

    Spre deosebire de foldr, care împăturește o listă la o singură valoare,
    unfoldr desfășoară procesul invers, construind o listă pe baza unei valori
    inițiale.
    
    Hint: divMod.

    Exemple:
    
    > take 10 $ toBinary 6
    [0,1,1,0,0,0,0,0,0,0]
-}
toBinary :: Int -> BinaryNumber
toBinary = unfoldr (\n -> if n == 0 then Just(0,0) else Just (n `mod` 2, n `div` 2))   --generam perechi (next binary elem(restul),  next state)

{-
    *** TODO ***

    Incrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la transport!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > inc [0,1,1] 
    [1,1,1]

    > inc [1,1,1]
    [0,0,0,1]
-}
inc :: BinaryNumber -> BinaryNumber
inc bits = inc' bits 1  -- carr de incepu 1
  where
    inc' [] 1 = [1]  --daca avem lista goala si carry 1 returnam 1
    inc' [] 0 = []    --daca avem lista goala si carry 1 returm lista (am completat operatiile)
    inc' (b:bs) carry = case (b, carry) of
      (0, 0) -> 0 : inc' bs 0  -- daca avem bit 0 si carry 0 returnam bit 0 cu apel recursiv de carry 0
      (0, 1) -> 1 : inc' bs 0
      (1, 0) -> 1 : inc' bs 0
      (1, 1) -> 0 : inc' bs 1

{-
   *** TODO ***

    Decrementează cu 1 reprezentarea finită sau infinită a unui număr binar.
    Atenție la împrumut!

    Constrângeri: utilizați
    * recursivitate explicită
    * pattern matching.

    Exemple:

    > dec [1,1,1]
    [0,1,1]

    > dec [0,0,0,1]
    [1,1,1]
-}
dec :: BinaryNumber -> BinaryNumber
dec bits = dec' bits 1  -- carr de incepu 1
  where
    dec' [] 1 = [1]  --daca avem lista goala si carry 1 returnam 1
    dec' [] 0 = []   --daca avem lista goala si carry 1 returm lista (am completat operatiile)
    dec' (b:bs) borrow = case (b, borrow) of
      (0, 0) -> 0 : dec' bs 0  -- daca avem bit 0 si carry 0 returnam bit 0 cu apel recursiv de carry 0
      (0, 1) -> 1 : dec' bs 1
      (1, 0) -> 1 : dec' bs 0
      (1, 1) -> 0 : dec' bs 0

{-
    *** TODO ***

    Adună două numere binare, asumând reprezentări infinite, pentru ușurința
    aplicării funcționalelor.

    Constrângeri: utilizați
    * where sau let
    * pt bonus 10p, funcționala mapAccumL (fără recursivitate explicită).

    mapAccumL are tipul (a -> b -> (a, c)) -> a -> [b] -> (a, [c]).
    Așa cum sugerează numele, combină comportamentele funcționalelor:
    * map, transformând element cu element [b] în [c]
    * foldl, utilizând în același timp un acumulator de tipul a.

    Exemple:

    > take 10 $ add (toBinary 74) (toBinary 123)
    [1,0,1,0,0,0,1,1,0,0]
    
    > toDecimal $ take 10 $ add (toBinary 74) (toBinary 123)
    197
-}
add :: BinaryNumber -> BinaryNumber -> BinaryNumber
add bits1 bits2 = result
    where
        (carry, result) = mapAccumL addBit 0 (zip bits1 bits2) -- facem perchi cu zip(primul bit din primul numar cu primul din al doliea s.a.m.d)
        addBit :: Int -> (Int, Int) -> (Int, Int)  -- aplicam fiecarei perechi functia addBit
        addBit carry (bit1, bit2) =
            let sum = bit1 + bit2 + carry  -- facem suma perechei cu a carry si retinem carry pentru urmatoare pereche sa adunam
                (newCarry, newBit) = sum `divMod` 2  
            in (newCarry, newBit)

{-
    *** TODO ***

    În pregătirea operației de înmulțire a două numere binare, cu reprezentări
    infinite, stivuiește copii deplasate la dreapta ale lui bits1, înmulțite
    cu bit-ul curent din bits2. Deplasarea se face adăugând la stânga lui bits1
    un număr de 0-uri dat de numărul liniei curente. Întoarce o listă infinită
    de liste infinite.

    Vizual:

    0 1 1 0 ... *   <- bits1
    1 0 1 0 ...     <- bits2
    -----------
   |0 1 1 0 ...        înmulțire bits1 cu 1 și deplasare 0 poziții la dreapta
    0|0 0 0 0 ...      înmulțire bits1 cu 0 și deplasare 1 poziție la dreapta
    0 0|0 1 1 0 ...    înmulțire bits1 cu 1 și deplasare 2 poziții la dreapta

    Constrângeri:
    * Corpul funcției trebuie să fie un list comprehension.
    * Nu utilizați recursivitate explicită.

    Hint: iterate pt generarea secvențelor de deplasare spre dreapta cu biți 0.

    Exemple:

    (exemplul vizual)
    > take 3 $ map (take 6) $ stack (toBinary 6) (toBinary 5)
    [[0,1,1,0,0,0],[0,0,0,0,0,0],[0,0,0,1,1,0]]
-}
stack :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
stack bits1 bits2 = [shift n (map (*b) bits1) | (n,b) <- zip [0..] bits2]  --n index, b bits, cream o noua lista-- o folosim ca sa stim cate zerouri trebuie sa adaugam ca sa aiba ca bits2
    where  -- returnam o lista de liste unde fiecare element din bits1 e multiplicat cu b din bits2(fiecare bit din bits2 o lista)
        shift n = (replicate n 0 ++)  -- facem n zerouri si ++ adaugam dupa bits 1
{-
    *** TODO ***

    Întoarce o listă infinită de numere binare, care pe poziția i >= 0 conține
    rezultatul înmulțirii lui bits1 cu numărul format din primii i biți
    ai lui bits2, i.e. suma primelor i linii întoarse de stack.

    Constrângeri:
    * Utilizați funcționala scanl (fără recursivitate explicită).

    Spre deosebire de foldl, care întoarce acumulatorul final, scanl întoarce
    o listă cu toate acumulatoarele intermediare.

    Exemple:
    
    > take 5 $ map (toDecimal . take 10) $ multiply (toBinary 6) (toBinary 5) 
    [0,6,6,30,30]
-}
multiply :: BinaryNumber -> BinaryNumber -> [BinaryNumber]
multiply bits1 bits2 = scanl add (repeat 0) (stack bits1 bits2) --scanl makes a foldr while adding the previous numbers 

--scanl aplica add la fiecare repeat 0 cu stack bits1 bits2
--prima valoare a accumulatorului e repeat 0 care genereaza o ininitate de liste de 0 la care adaugam  si returnam o infinitate de liste binare care reprezinta produsele dintre bits1 si bits2
{-
    *** TODO ***

    Întrebare de reflecție, la care veți răspunde la prezentarea temei.

    Având în vedere că liniile întoarse de stack care conțin exclusiv biți 0
    nu contribuie la rezultatul unei înmulțiri, să presupunem că modificăm
    definiția funcției stack astfel încât să construiască doar liniile utile;
    de exemplu, folosind filter sau pattern matching în list comprehension
    pt a păstra doar biții 1 din bits2. Ce probleme ar putea crea această
    abordare?

    -- pierdem precizie in cadrul inmultirii, probleme de gestionare a lungimii numerelor
    -- daca am vrea sa multiplicam bits1 cu putere a 2 sa il aducem in decimal am putea pierde termeni necesari pentru rezultatul corect
-}
