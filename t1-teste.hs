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
