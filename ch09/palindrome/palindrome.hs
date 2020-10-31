-- respondPalindromes contents =
--   unlines
--     ( map
--         ( \xs ->
--             if isPalindromes xs then "palindrome" else "not a palindrome"
--         )
--         (lines contents)
--     )
--   where
--     isPalindromes xs = xs == reverse xs

main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes =
  unlines
    . map
      ( \xs ->
          if isPalindrome xs then "palindrome" else "not a palindrome"
      )
    . lines
  where
    isPalindrome xs = xs == reverse xs

-- $ cat words.txt | runhaskell palindromes.hs
-- not a palindrome
-- palindrome
-- palindrome
-- palindrome