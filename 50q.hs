import Data.Char 
import Data.Either
import Data.List

--1
enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 x xs = [x..xs] --constroi uma lista de um x até um xs

--2
enumFromThenTo1 ::  Int -> Int -> Int -> [Int]
enumFromThenTo1 x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y] 

--ou 

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | z>x = x:(enumFromThenTo' (x+(y-x)) (y+(y-x)) z) --Se o z maior que x então começamos a lista com x e (adicionamos ao valor de x a sua subtração com y) && (a adição a y de y-x), a segunda vai resultar no número a implementar a seguir
                      | z==x = [x]
                      | otherwise = []

--3
concatenacao ::  [a] -> [a] -> [a]
concatenacao [] [] = []
concatenacao [] l = l 
concatenacao l [] = l
concatenacao (x:xs) l = x : concatenacao xs l -- Começa com o x e vai juntando o resto da lista a uma lista só

--4
exclamacao :: [a] -> Int -> a
exclamacao (h:t) x 
 | x == 0 = h -- Se for  dado um 0 junto com a lista...retribui o primeiro elemento da lista 
 | otherwise = exclamacao t (x-1) -- Caso contrário vai descontando ao x um valor até chegar a zero, aí atrivui o valor da lista onde este se encontra

--5
reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:y) = (reverse1 y) ++ [x]  -- Começa com a observação da lista começando por dar o x no último lugar

--6
take1 ::  Int -> [a] -> [a]
take1 n  _ | n <= 0 = [] --Se o n for menor ou igual a zero retorna a lista vazia
take1 _ [] = []
take1 n (x:xs) = x : take1 (n-1) xs -- Dependendo do valor de n, vai contruindo a lista e retirando um valor a n a cada vez que o corre

--7
drop1 ::  Int -> [a] -> [a]
drop1 n xs | n <= 0 = xs --Se o n for menor ou igual a zero retorna a o xs inteiro
drop1 _ [] = []
drop1 n (x:xs) = drop1 (n-1) xs -- Dependendo do valor de n vai retirando elementos à lista até o n=0

--8
zip1 ::  [a] -> [b] -> [(a,b)]
zip1 [] [] = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys -- Junta os dois primeiros elementos de duas listas e faz o mesmo para os próximos elementos

--9
elem1 ::  Eq a => a -> [a] ->Bool
elem1 _ []          = False
elem1 a (h:t)
        | a == h    = True --Verifica se o elemento antes da lista é igual ao primeiro elemento da lista, se n for vai verificar no resto da lista
        | otherwise = elem1 a t 

--10
replicate1 :: Int -> a -> [a]
replicate1 0 _  = []
replicate1 x y  = y : replicate1 (x-1) y -- Pega no y e vai replicá-lo x vezes, ou seja, a cada vez que o replica vai tirar uma unidade a x

--11
intersperse1 :: a -> [a] ->[a]
intersperse1 _ []    = []
intersperse1 _ [a]   = [a]
intersperse1 x (h:t) = h : x : intersperse1 x t -- Pega no elemento x e vai intercalando-o no meio dos outros elementos da lista

--12 
group1 :: Eq a => [a] -> [[a]]
group1 [] = []
group1 [a] = [[a]]
group1 (x:xs)
       | x == head (c1) = (x:c1) : c2 --Se x for igual ao elemento a seguir ele junta-o a ele formando uma lista, verifica o resto da lista para ver se há mais elementos iguais consecutivos
       | otherwise = [x]: (c1:c2) --Se n for, não junta e faz uma lista apenas de x e  vai procurar elementos iguais consecutivos
         where 
           (c1:c2) = group1 xs

--13 
concat1 ::  [[a]] -> [a]
concat1 [] = [] 
concat1 (h:t) 
        | length h == 0 = concat1 t --Se o length de h for igual a 0, junta as listas mais pequenas numa só sem o h
        | otherwise = h ++ concat1 t --Se o h existir, ou seja se length de h > 0, continua a juntar as listas mas desta vez com o h


--14
inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 l = inits1 (init l) ++ [l] -- Acaba com a lista l na totalidade, nos elementos anteriores vai tirando o último valor à lista, até chegar a []

--15
tails1 :: [a] -> [[a]] 
tails1 [] = [[]]
tails1 l = [l] ++ (tails1 (tail l)) -- Começa com a lista l na totalidade e vai tirando o primeiro elemento até ficar com []

--16
isPrefixOf1 :: Eq a => [a]-> [a] -> Bool
isPrefixOf1 [] _ = True
isPrefixOf1 (a:b) (c:d)
          | a == c = isPrefixOf1 b d -- Verifica se a cabeça da lista é igual à da outra e depois verifica o resto da lista caso seja preciso
          | otherwise = False

--17
isSuffixOf1 :: Eq a => [a]-> [a] -> Bool
isSuffixOf1 []  _ = True
isSuffixOf1 l l2
          | last l == last l2 = isSuffixOf1 (init l) (init l2) -- Dá o último da lista l e depois procura todos menos o último (função init), para verificar se é verdadeiro ou não
          | otherwise = False

--18
isSubsequenceOf1 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf1 [] [] = True
isSubsequenceOf1 _ []  = False
isSubsequenceOf1 (a:b) (c:d) 
               | a == c = isSubsequenceOf1 b d 
               | otherwise = isSubsequenceOf1 (a:b) d

--19
elemIndices1 :: Eq a => a -> [a] -> [Int]
elemIndices1 n [] = []
elemIndices1 n l | n == last l = elemIndices1 n (init l) ++ [(length l-1)] -- Verifica se o último nr da lista é igual ao n e dps vai verififcar o resto da lista metendo-os numa lista
                 | otherwise = elemIndices1 n (init l) -- se o último elemento n for igual ele vai ver todos os outros da mesma forma que fez em cima


--20
nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (x:xs) = x: nub1 (filter (/=x) xs) --Põe o x no ínicio da lista e vai verificar no resto da lista se existem mais x, se existirem elimina-os, faz isso para todos os elementos que sejam iguais

--Input: filter (>5) [1,2,3,4,5,6,7,8]
--Output: [6,7,8]

--21
delete1 :: Eq a => a -> [a] -> [a]
delete1 _ [] = []
delete1 n (x:xs) | n==x = xs -- Se o n for igual ao x, retira-se o x e ficamos com o resto da lista
                 | otherwise = x:(delete1 n xs) -- Se o de cima n se verificar a função fica com o x e vai verificar no resto da lista onde estão os números iguais ao n e posteriormente eliminá-los

--22
barrabarra :: Eq a => [a] -> [a]-> [a] --[1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1]
barrabarra [] _ = []
barrabarra (x:xs) [] = (x:xs) 
barrabarra (x:xs) (y:ys) | x == y = barrabarra xs ys -- Verifica se o primeiro é igual nos dois e neste caso como é elimina-o, depois verfica as igualdades nas duas listas
                         | otherwise = x : barrabarra xs (y:ys) -- Se o primeiro não for igual pega na lista completa e continua a procurar no ys da outra

--23
union1 :: Eq a => [a] -> [a]-> [a]
union1 [] [] = []
union1 l [] = l 
union1 [] l = l 
union1 (x:xs) (y:ys) | elem y (x:xs) = union1 (x:xs) ys  -- Verifica se o y está em alguma parte da lista x:xs, neste caso é vdd, logo, vai verificar o resto para a lista ys e une a lista x:xs com alguns elementos de ys
                     | otherwise = (union1 (x:xs) ys) ++ [y] -- Como y n pertence a x:xs ele verifica os outros elementos de ys unindo-os depois com y


--24
intersect1 :: Eq a => [a] ->[a] -> [a] -- [1,1,2,3,4] [1,3,5] corresponde a [1,1,3] apenas mete no resultado os elementos da primeira lista que pertencem à segunda
intersect1 [] [] = []
intersect1 _ [] = []
intersect1 [] _ = []
intersect1 (x:xs) (y:ys) | x==y = x:(intersect1 xs (y:ys)) -- Verifica se o x pertence ao y:ys, confirmando-se vai procurar no xs mais elementos iguais aos da lista y:ys
                         | otherwise = intersect1 xs (y:ys)


--25
insert1 :: Ord a => a -> [a] -> [a] --insert 25 [1,20,30,40] corresponde a [1,20,25,30,40] vai nr a nr procurar um maior do que ele para se colocar antes desse
insert1 n [] = [n]
insert1 n (x:xs) | n >= x = x:(insert1 n xs) -- Se n >= x então fica o x no ínicio da lista e o n no meio 
                 | otherwise = n:(x:xs) -- Se n <= x então fica o n no ínicio da lista e o x no meio 

--ou

insert2 :: Ord a => a -> [a] -> [a] -- Neste insert2 acontece o mesmo que em cima mas por uma ordem contrária 
insert2 n [] = [n]
insert2 n (x:xs) | n <= x = n:(x:xs)
                 | otherwise = x:(insert2 n xs)

--26
unwords1 :: [String] -> String -- unwords ["Programacao", "Funcional"]corresponde a"Programacao Funcional"
unwords1 [] = "Empty list... Try again"
unwords1 (x:[]) = x -- SE só tiver uma string na lista
unwords1 (x:xs) = x++" "++unwords1 xs --concatena as duas strings, tirando-as da lista e pondo-as na mesma string

--27
unlines1 :: [String] -> String --unlines ["Prog", "Func"]corresponde a"Prog\nFunc\n"
unlines1 [] = ""
unlines1 (x:[]) = x 
unlines1 (x:xs) = x++"/n"++(unlines1 xs) -- Pega na primeira string da lista, junta-a com o /n e posteriormente junta-a com as strings seguintes da lista

--28
pMaior :: Ord a => [a] -> Int --Se o primeiro elemento da lista for o maior a função deve retornar o 0
pMaior [] = error "Lista vazia" 
--pMaior l = (-1) * snd (maximum (zip l [0,-1..]))
pMaior l = posicao maior l
  where maior = maximum l
        posicao :: Eq a => a -> [a] -> Int
        posicao x (h:t) | x==h = 0 --Se estiver na cabeça dá logo zero
                        | otherwise = 1 + posicao x t --Se não estiver procura no resto da lista e à medida que vai avançando adiciona 1 ao número, pois a posição aumenta

pMaior2 l = if (maximum l == head l) then 0 
            else 1+pMaior2 (tail l) 


--29
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x:[]) = False
temRepetidos (a:b) = if elem a b then True else temRepetidos b --Primeiro verifica se existe algum elemento igual ao primeiro dentro da lista, depois verifica dentro de b se existem elemntos iguais entre si

--30
algarismos :: [Char] -> [Char] --algarismos "123xp5" corresponde a "1235"
algarismos [] = []
algarismos (x:xs) | elem x ['0'..'9'] = x:(algarismos xs) -- Vê se o x é igual a algum algarismo de 1 a 9, neste caso, como o é mete o x no ínicio da e vai verificar o resto 
                  | otherwise = algarismos xs -- Como o x n é igual verifica apenas o resto

--31
posImpares :: [a] -> [a]
posImpares [] = []
posImpares (x:[]) = []
posImpares (x:y:xs) = y:(posImpares xs) --Temos de definir 3 variáveis para mostrar ao Haskell que tipo de números é que nós queremos ter na lista, ou seja, os ímpares

--32
posPares :: [a] -> [a]
posPares [] = []
posPares (x:[]) = [x]
posPares (x:y:z:xs) = x:z:(posPares xs) --Definimos 4 variáveis para ele reconhecer que tem de andar de dois em dois 

--33
isSorted :: Ord a => [a] -> Bool
isSorted [] = True 
isSorted (x:[]) = True
isSorted (x:y:xs) | x <= y = isSorted (y:xs) --Analisamos se o primeiro é menor ou igual ao segundo, se assim for, vai analisar o resto da lista
                  | otherwise = False

--34
iSort :: Ord a => [a] -> [a] -- Assuma, se precisar, que existe definida a função insert:: Ord a => a -> [a] -> [a] que dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista.
iSort [] = []
iSort (x:[]) = [x]
iSort (x:xs) = insert1 x (iSort xs) -- Ordena uma lista, ou seja vai nr a nr usando o insert (que ordena um nr numa determinada lista) e ordena a lista

--35 IMPORT DATA,CHAR ??
menor :: String -> String -> Bool --menor "sai" "saiu" corresponde a True enquanto que menor "programacao" "funcional" corresponde a False. 
menor "" "" = False
menor "" _ = True
menor _ "" = False
menor (x:xs) (y:ys) | (ord x) < (ord y) = True --Vê se o x vem antes de y no dicionário se vier é vdd
                    | (ord x) > (ord y) = False -- Se não vier é falso
                    | otherwise = menor xs ys --Se forem iguais vai ao resto da string/palavra analisar a  diferença entre as duas, qwuando envontrar determina se é maior ou menor, ie, False e True respetivamente

--36
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet a ((x,y):xs) | a == x = True -- Verifica se o primeiro elemento de fora é igual ao do da primeira dupla da lista
                      | otherwise = elemMSet a xs --Verifica no resto da lista se aquele elemento é igual a algum

--37
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((x,y):xs) = y + (lengthMSet xs) -- Calcula a soma dos inteiros que estão dentro das duplas contidas na lista, aqui calcula o primeiro e o interpretador calcula o resto

--38
converteMSet :: [(a,Int)] -> [a] --converteMSet [('b',2), ('a',4), ('c',1)] corresponde a "bbaaaac"
converteMSet [] = []
converteMSet ((x,y):xs) = sox (x,y) ++ (converteMSet xs) --Faz a função sox para tds os elementos da sua lista
                          where sox (x,0) = []
                                sox (x,y) = [x] ++ sox (x ,(y-1)) -- Pega no x e no y e calcula a quantidade de vezes que vai ter de repetir o x

--39
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] -- insereMSet 'c' [('b',2), ('a',4), ('c',1)] corresponde  a [(’b’,2),(’a’,4), (’c’,2)]
insereMSet x [] = [(x,1)]
insereMSet a ((x,y):xs) | a == x = ((x,y+1):xs) -- Se o a=x então o número no y vai aumentar 
                        | otherwise = ((x,y):xs) ++ insereMSet a xs --Se não se verificar ele procura no resto da lista um elemento igual a 'a'

--40
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] --removeMSet 'c' [('b',2), ('a',4), ('c',1)] corresponde  a [(’b’,2),(’a’,4)]
removeMSet x [] = []
removeMSet a ((x,y):xs) | a == x && y>1 = ((x,y-1):xs) -- O a=x e o y>1 apenas se tira um valor ao y 
                        | a == x && y==1 = (xs) -- Como neste caso ia ficar zero, tira-se a dupla inteira
                        | otherwise = (x,y) : removeMSet a xs -- O a =\ x por isso analisa o resto da lista

--41 
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (x:xs) = aux xs x 1
    where
        aux [] elem num = [(elem, num)]
        aux (y:ys) elem num
            | y == elem = aux ys elem (num+1) --Se o y = elem 
            | otherwise = (elem, num):aux ys y 1

constroiMset2:: Ord a => [a] -> [(a,Int)]
constroiMset2 [] = []
constroiMset2 (x:xs) = (x,1+length(filter (==x) (xs))) : constroiMset2 (filter (/=x) (xs)) -- Começa com x e adiciona 1 ao comprimento, se encontrar algum elemento igual a x em xs

--42 DUVIDA
partitionEithers1 :: [Either a b] -> ([a],[b])
partitionEithers1 [] = ([],[])
partitionEithers1 ((Left a):xs) = (a:nx, ny) -- Se for à esquerda acrescenta o a à esquerda de tudo
    where (nx,ny) = partitionEithers1 xs
partitionEithers1 ((Right a):xs) = (nx,a:ny) --Se for à direita acrescenta à direita do primeiro elemento
    where (nx,ny) = partitionEithers1 xs

--43 DUVIDA
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = 
    case x of Nothing -> catMaybes xs -- Se x não for igual a nada vamos analisar o xs
              Just x -> x:catMaybes xs -- Caso tenha alguma coisa começamos com o  x e analisamos o resto 

--44
data Movimento = Norte | Sul | Este | Oeste
       deriving Show 

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:xs) = posicao (x,(y+1)) xs --Muito geral dependendo de para onde ele se mover, aumenta ou diminui um valor no gráfico
posicao (x,y) (Sul:xs) = posicao (x,(y-1)) xs
posicao (x,y) (Este:xs) = posicao ((x+1),y) xs
posicao (x,y) (Oeste:xs) = posicao ((x-1),y) xs

--45
caminho ::  (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf)
    | xf > xi = [Este] ++ caminho (xi+1,yi) (xf, yf) --Basicamente ele recebe dois conjuntos de coordenadas, inicial e final, analisa os dois e descreve o tipo de movimento que foi feito de um para o outro
    | xf < xi = [Oeste] ++ caminho (xi-1,yi) (xf,yf)
    | yf > yi = [Norte] ++ caminho (xi,yi+1) (xf,yf)
    | yf < yi = [Sul] ++ caminho (xi,yi-1) (xf,yf)
    | otherwise = []

--46
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (Norte:xs) = vertical xs --Analisa se é norte ou Sul no ínicio, se for percorre o resto da lista, se não for, é logo falso
vertical (Sul:xs) = vertical xs
vertical (x:xs) = False

--47 DUVIDA
data Posicao = Pos Int Int
       deriving Show

maisCentral :: [Posicao] -> Posicao -- dada uma lista nao vazia de posicoes, determina a que esta mais perto da origem (note que as coordenadas de cada pontos ao numeros inteiros)
maisCentral [] = error "lista vazia"
maisCentral (Pos a b : []) = (Pos a b)
maisCentral ((Pos x1 y1):(Pos x2 y2):xs) | dist' (Pos x1 y1) <= dist' (Pos x2 y2) = maisCentral ((Pos x1 y1):xs) --Se a dist do primeiro for menor do que a do segundo vai comparar o primeiro ao resto da lista
                                         | dist' (Pos x1 y1) > dist' (Pos x2 y2) = maisCentral ((Pos x2 y2):xs)
                                                                                   where dist' (Pos x y) = sqrt (fromIntegral(y^2 + x^2)) --Função dist é basicamente a fórmula para calcular a distância do ponto ao centro usualmente usada em matemática

--48 Não está a funcionar ???!
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x y) ((Pos x1 y1):xs)  | x==x1 && y==(y1+1) = (Pos x1 y1):(vizinhos (Pos x y) xs) --Analisa a função e vê se há vizinhaça na unidade mais próxima, seja no x ou no y 
                                     | x==x1 && y==(y1-1) = (Pos x1 y1):(vizinhos (Pos x y) xs)
                                     | y==y1 && x==(x1+1) = (Pos x1 y1):(vizinhos (Pos x y) xs)
                                     | y==y1 && x==(x1-1) = (Pos x1 y1):(vizinhos (Pos x y) xs)
                                     | otherwise = vizinhos (Pos x y) xs -- Caso não haja nada entre aqueles dois, vai procurar no resto da lista

vizinhos2 :: Posicao -> [Posicao] -> [Posicao]
vizinhos2 _ [] = []
vizinhos2 (Pos x1 y1) ((Pos x2 y2):xs)
    | abs (x1-x2) <= 1 && abs (y1-y2) <= 1 = (Pos x2 y2) : vizinhos2 (Pos x1 y1) xs
    | otherwise = vizinhos2 (Pos x1 y1) xs

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada ((Pos x y):[]) = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2):xs) | y1 == y2 = mesmaOrdenada ((Pos x1 y1):xs) --Verifica se a ordenada de um é igual à do outro, se for vai ver o resto da lista para comparar 
                                           | otherwise = False

--50 --DUVIDA
data Semaforo = Verde | Amarelo | Vermelho
    deriving Show

interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK [] = True
interseccaoOK s = if (aux s 0) <= 1 then True else False --Testa o valor que vem da função auxiliar
    where
        aux [] ac = ac
        aux (x:xs) ac = 
            case x of Vermelho -> aux xs (ac+1) -- se for Vermelho vai acrescentar um valor
                      n -> aux xs (ac) --caso contrário n adiciona nada

