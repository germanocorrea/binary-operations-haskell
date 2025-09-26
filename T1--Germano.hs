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

-- ex: dec2bin 7 4 = [0,1,1,1]
dec2bin :: Int -> Int -> [Int]
dec2bin x 0 | x > 0 = error "overflow, use mais bits para n"
            | otherwise = []
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
dec2binfrac _   0 = [] -- caso haja um estouro na representação, essa regra garante que seja truncado o reulstado
dec2binfrac 0.0 n = 0             : dec2binfrac 0.0 (n - 1)
dec2binfrac dec n = parte_inteira : dec2binfrac fracao_restante (n - 1)
                          where
                             mult            = dec * 2
                             parte_inteira   = truncate mult
                             fracao_restante = mult - fromIntegral parte_inteira
