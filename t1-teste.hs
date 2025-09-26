import Test.HUnit

{--
 - T1 - Programacao Funcional
 - Aluno: Germano Bruscato Correa
 - Matricula: 24180395-6
 -
 -
 - Com excecao da representacao binaria de parte fracionaria,
 - todas representacoes em binario utilizam MSB a esquerda
 -
 - Exemplo de uso das funcoes solicitadas estao em um comentario
 - exatamente acima da declaracao do type da funcao. Demais funcoes
 - auxiliares que criei tambem tem exemplos, e explicacao do que fazem
--}


{-- 1) Definir uma função recursiva que recebe um número binário
 - (interpretado como número inteiro sem sinal) e retorna o valor 
 - equivalente em decimal.
 - 𝑏𝑖𝑛2𝑑𝑒𝑐 ∷ [𝐼𝑛𝑡] → 𝐼𝑛𝑡
--}

-- ex: bin2dec [1,1,0,1] = 13
bin2dec :: [Int] -> Int
bin2dec []     = 0
bin2dec (0:xs) = 0 + (bin2dec xs)
bin2dec (1:xs) = 2 ^ (length xs) + (bin2dec xs)

{-- 2) Definir uma função recursiva que recebe um número decimal
 - inteiro não-negativo, um número de bits desejado e retorna o
 - valor equivalente em binário (interpretado como número inteiro
 - sem sinal) com o número de bits informado. Por exemplo, 
 - 𝑑𝑒𝑐2𝑏𝑖𝑛 2 8 deve retornar [0,0,0,0,0,0,1,0].
 - 𝑑𝑒𝑐2𝑏𝑖𝑛 ∷ 𝐼𝑛𝑡 → 𝐼𝑛𝑡 → [𝐼𝑛𝑡]
--}

-- TODO: lidar com overflow
-- ex: dec2bin 7 4 = [0,1,1,1]
dec2bin :: Int -> Int -> [Int]
dec2bin _ 0 = []
dec2bin 0 n = dec2bin         0 (n - 1) ++ [0]
dec2bin x n = dec2bin quociente (n - 1) ++ [bit]
            where
               quociente = x `div` 2
               bit       = x `mod` 2 

{-- 3) Definir uma função recursiva que recebe um número decimal 
 - inteiro, um número de bits desejado e retorna o valor equivalente
 - em binário na representação de complemento de dois com o número
 - de bits informado. Por exemplo, 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 (−2) 8 deve
 - retornar [1,1,1,1,1,1,1,0]
 - 𝑑𝑒𝑐2𝑏𝑖𝑛𝑐𝑜𝑚𝑝𝑙 ∷ 𝐼𝑛𝑡 → 𝐼𝑛𝑡 → [𝐼𝑛𝑡]
 - --}

-- TODO: se usarmos apenas 4 bits em n, e o numero tiver 4 bits (por ex: 8 -> 1000) a representacao fica errada, deveria dar overflow
-- ex: dec2bincompl (-13) 8 = [1,1,1,1,0,0,1,1]
dec2bincompl :: Int -> Int -> [Int]
dec2bincompl x n | x < 0           = snd (somabin complemento_um soma_um) -- snd porque somabin retorna uma tupla, primeiro elemento é o overflow/carry
                 | otherwise       = dec2bin x n
                 where
                    complemento_um = flip_bits (dec2bin (abs x) n)
                    soma_um        = left_shift [0,1] n -- 01 para repetir o 0 à esquerda

-- negacao/flip de uma lista de bits
-- ex: flip_bits [1,0,1,1] = [0,1,0,0]
flip_bits :: [Int] -> [Int]
flip_bits xs = [abs (x - 1) | x <- xs]

-- left shift do MSB
-- ex: left_shift [1,0,1,0] 8 = [1,1,1,1,1,0,1,0]
left_shift :: [Int] -> Int -> [Int]
left_shift (x:xs) n = take shift (repeat x) ++ (x:xs)
                    where
                       shift = n - length xs - 1

-- soma duas listas de bits
-- somabin list_bits_x list_bits_y = (carry, [resultado])
-- ex: somabin [1,1,1] [0,0,1] = (1, [0,0,0])
somabin :: [Int] -> [Int] -> (Int, [Int])
somabin [] []         = (0, [])
somabin (x:xs) (y:ys) = (bit_carry, bit_result:tail_result)
                      where
                         (tail_carry, tail_result) = somabin xs ys
                         (bit_carry, bit_result) = somabit x y tail_carry

-- realiza a soma de dois bits
-- somabit x y carry = (carry, resultado)
-- ex: somabit 1 1 0 = (1, 0)
somabit :: Int -> Int -> Int -> (Int, Int)
somabit a b c | a == b = (a, c)
somabit a b 0          = (0, 1)
somabit a b 1          = (1, 0)
{--
- "tabela verdade" para somabit a b c | a == b = (a, c)
somabit 1 1 1 = (1, 1)
somabit 1 1 0 = (1, 0)
somabit 0 0 1 = (0, 1)
somabit 0 0 0 = (0, 0)
- "tabela verdade" para somabit a b 0 = (0, 1)
somabit 1 0 0 = (0, 1)
somabit 0 1 0 = (0, 1)
- "tabela verdade" para somabit a b 1 = (1, 0)
somabit 1 0 1 = (1, 0)
somabit 0 1 1 = (1, 0)
--}


{-- 4)
 - Definir uma função recursiva que recebe um número fracionário
 - decimal por parâmetro e devolve um número binário de ponto fixo
 - de 32 bits. O número binário de ponto fixo dever ser representado
 - por uma tupla com dois números binários tal que a parte inteira
 - deve estar na representação de complemento de dois com 16 bits
 - e a parte fracionária deve estar na representação de binário
 - fracionado com 16 bits. Você deve definir uma forma adequada
 - de representar o resultado caso o número decimal estoure a
 - representação. Por exemplo, 𝑓𝑟𝑎𝑐2𝑏𝑖𝑛 (−8.5) deve retornar
 - ([1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0], [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).
 - 𝑓𝑟𝑎𝑐2𝑏𝑖𝑛 ∷ 𝐷𝑜𝑢𝑏𝑙𝑒 → ([𝐼𝑛𝑡], [𝐼𝑛𝑡])
 - --}

-- ex: frac2bin (-7.65) = ([1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1],[1,0,1,0,0,1,1,0,0,1,1,0,0,1,1,0])
frac2bin :: Double -> ([Int], [Int])
frac2bin dec = (dec2bincompl parte_inteira 16, dec2binfrac parte_fracionaria 16)
             where
                parte_inteira     = truncate dec
                parte_fracionaria = abs(dec - fromIntegral parte_inteira)


-- calcula apenas a parte fracionaria
-- ex: dec2binfrac 0.5 4 = [1,0,0,0]
dec2binfrac :: Double -> Int -> [Int]
dec2binfrac _   0 = []
dec2binfrac 0.0 n = 0             : dec2binfrac 0.0 (n - 1)
dec2binfrac dec n = parte_inteira : dec2binfrac fracao_restante (n - 1)
                          where
                             mult            = dec * 2
                             parte_inteira   = truncate mult
                             fracao_restante = mult - fromIntegral parte_inteira

-- 1) bin2dec :: [Int] -> Int
test_bin2dec :: Test
test_bin2dec = TestList
    [
        "Exemplo dado" ~: bin2dec [1,1,0,1] ~?= 13,
        "Zero"        ~: bin2dec [] ~?= 0,
        "Um"          ~: bin2dec [1] ~?= 1,
        "15"          ~: bin2dec [1,1,1,1] ~?= 15,
        "8"           ~: bin2dec [1,0,0,0] ~?= 8,
        "255"         ~: bin2dec [1,1,1,1,1,1,1,1] ~?= 255
    ]

-- 2) dec2bin :: Int -> Int -> [Int] (Sem lidar com Overflow)
test_dec2bin :: Test
test_dec2bin = TestList
    [
        "Exemplo dado (7, 4)"        ~: dec2bin 7 4 ~?= [0,1,1,1],
        "Exemplo dado (2, 8)"        ~: dec2bin 2 8 ~?= [0,0,0,0,0,0,1,0],
        "Zero com 4 bits"            ~: dec2bin 0 4 ~?= [0,0,0,0],
        "Max 4 bits (15)"            ~: dec2bin 15 4 ~?= [1,1,1,1],
        "10 com 6 bits"              ~: dec2bin 10 6 ~?= [0,0,1,0,1,0]
        -- Ignorando casos de overflow (e.g., dec2bin 16 4) conforme instruído.
    ]

-- Auxiliar: flip_bits :: [Int] -> [Int]
test_flip_bits :: Test
test_flip_bits = TestList
    [
        "Exemplo dado"        ~: flip_bits [1,0,1,1] ~?= [0,1,0,0],
        "Todos uns"           ~: flip_bits [1,1,1,1] ~?= [0,0,0,0],
        "Todos zeros"         ~: flip_bits [0,0,0,0] ~?= [1,1,1,1],
        "Lista vazia"         ~: flip_bits [] ~?= []
    ]

-- Auxiliar: left_shift :: [Int] -> Int -> [Int]
test_left_shift :: Test
test_left_shift = TestList
    [
        "Exemplo dado"        ~: left_shift [1,0,1,0] 8 ~?= [1,1,1,1,1,0,1,0], -- 4 bits originais, 4 de preenchimento (sign extension)
        "Shift com 0 MSB"     ~: left_shift [0,1,0,1] 8 ~?= [0,0,0,0,0,1,0,1],
        "Sem shift"           ~: left_shift [1,0,1] 3 ~?= [1,0,1], -- n = length (x:xs)
        "Shift min (1 bit)"   ~: left_shift [0,1] 3 ~?= [0,0,1]
    ]

-- Auxiliar: somabit :: Int -> Int -> Int -> (Int, Int)
test_somabit :: Test
test_somabit = TestList
    [
        -- Casos a == b
        "1 + 1 + 1" ~: somabit 1 1 1 ~?= (1, 1),
        "1 + 1 + 0" ~: somabit 1 1 0 ~?= (1, 0),
        "0 + 0 + 1" ~: somabit 0 0 1 ~?= (0, 1),
        "0 + 0 + 0" ~: somabit 0 0 0 ~?= (0, 0),
        -- Casos a != b
        "1 + 0 + 0" ~: somabit 1 0 0 ~?= (0, 1),
        "0 + 1 + 0" ~: somabit 0 1 0 ~?= (0, 1),
        "1 + 0 + 1" ~: somabit 1 0 1 ~?= (1, 0),
        "0 + 1 + 1" ~: somabit 0 1 1 ~?= (1, 0)
    ]

-- Auxiliar: somabin :: [Int] -> [Int] -> (Int, [Int])
test_somabin :: Test
test_somabin = TestList
    [
        "Exemplo dado (7+1=8)" ~: somabin [1,1,1] [0,0,1] ~?= (1, [0,0,0]),
        "4 + 4 = 8 (sem overflow)" ~: somabin [1,0,0] [1,0,0] ~?= (1, [0,0,0]), -- [1,0,0] + [1,0,0] = [0,0,0] com carry 1
        "1 + 1 = 2" ~: somabin [0,1] [0,1] ~?= (0, [1,0]),
        "3 + 5 = 8" ~: somabin [0,1,1] [1,0,1] ~?= (1, [0,0,0]),
        "Lista vazia" ~: somabin [] [] ~?= (0, [])
    ]


-- 3) dec2bincompl :: Int -> Int -> [Int] (Ignorando casos de overflow)
test_dec2bincompl :: Test
test_dec2bincompl = TestList
    [
        -- Casos de números negativos
        "Exemplo dado (-13, 8)" ~: dec2bincompl (-13) 8 ~?= [1,1,1,1,0,0,1,1],
        "-1 com 4 bits"          ~: dec2bincompl (-1) 4 ~?= [1,1,1,1],
        "-8 com 4 bits"          ~: dec2bincompl (-8) 4 ~?= [1,0,0,0],
        "-127 com 8 bits"        ~: dec2bincompl (-127) 8 ~?= [1,0,0,0,0,0,0,1],
        -- Casos de números positivos (devem usar dec2bin)
        "Positivo (7, 4)"        ~: dec2bincompl 7 4 ~?= [0,1,1,1],
        "Zero (0, 4)"            ~: dec2bincompl 0 4 ~?= [0,0,0,0]
        -- Ignorando o TODO sobre o overflow em 4 bits (e.g., 8 -> [1,0,0,0])
    ]

-- Auxiliar: dec2binfrac :: Double -> Int -> [Int]
test_dec2binfrac :: Test
test_dec2binfrac = TestList
    [
        "Exemplo dado (0.5, 4)"      ~: dec2binfrac 0.5 4 ~?= [1,0,0,0],
        "0.25 com 4 bits"           ~: dec2binfrac 0.25 4 ~?= [0,1,0,0],
        "0.75 com 8 bits"           ~: dec2binfrac 0.75 8 ~?= [1,1,0,0,0,0,0,0],
        "0.65 com 16 bits (aproximado)" ~: assertBool "0.65 16 bits" (dec2binfrac 0.65 16 == [1,0,1,0,0,1,1,0,0,1,1,0,0,1,1,0]), -- Exemplo de frac2bin
        "Zero"                      ~: dec2binfrac 0.0 5 ~?= [0,0,0,0,0],
        "Lista vazia"               ~: dec2binfrac 0.1 0 ~?= []
    ]

-- 4) frac2bin :: Double -> ([Int], [Int]) (Ignorando representação de overflow)
test_frac2bin :: Test
test_frac2bin = TestList
    [
        "Exemplo dado (-8.5)"            ~: frac2bin (-8.5) ~?= ([1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0], [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),
        "Exemplo dado (-7.65)"            ~: frac2bin (-7.65) ~?= ([1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1], [1,0,1,0,0,1,1,0,0,1,1,0,0,1,1,0]),
        "Positivo (1.25)"                ~: frac2bin 1.25 ~?= ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1], [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),
        "Zero (0.0)"                     ~: frac2bin 0.0 ~?= ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        -- Ignorando a forma adequada de lidar com o overflow na representação (TODO implícito).
    ]

-- Conjunto de todos os testes
allTests :: Test
allTests = TestList
    [
        TestLabel "bin2dec Tests"                  test_bin2dec,
        TestLabel "dec2bin Tests"                  test_dec2bin,
        TestLabel "flip_bits Tests"                test_flip_bits,
        TestLabel "left_shift Tests"               test_left_shift,
        TestLabel "somabit Tests"                  test_somabit,
        TestLabel "somabin Tests"                  test_somabin,
        TestLabel "dec2bincompl Tests"             test_dec2bincompl,
        TestLabel "dec2binfrac Tests"              test_dec2binfrac,
        TestLabel "frac2bin Tests"                 test_frac2bin
    ]

-- Função principal para rodar os testes
runAllTests :: IO Counts
runAllTests = runTestTT allTests
