### Definition
 - A parser is a function that takes some textual input (it could be a String, or another datatype such as
   ByteString or Text) and returns some structure as an output. That structure might be a tree, for example,
   or an indexed map of locations in the parsed data. Parsers analyze structure in conformance with rules
   specified in a grammar, whether it’s a grammar of a human language, a programming language, or a format
   such as JSON.

 - A parser combinator is a higher-order function that takes parsers as input and returns a new parser as output.

 - `Parsers` are functions, so parser combinators are higher-order functions that can take parsers as arguments.
   Usually the argument passing is elided because the interface of parsers will often be like the `State` monad
   which permits `Reader`-style implicit argument passing. Among other things, combinators allow for recursion
   and for gluing together parsers in a modular fashion to parse data according to complex rules.

***Haskell's parsing ecosystem***
 - Haskell has several excellent parsing libraries available. `parsec` and `attoparsec` are perhaps the two most
   well known parser combinator libraries in Haskell, but there is also `megaparsec` and others. `aeson` and
   `cassava` are among the libraries designed for parsing specific types of data ( `JSON` data and `CSV` data,
   respectively).

 - If you intend to do a lot of parsing in production, you may need to get comfortable using `attoparsec`, as
   it is particularly known for very speedy parsing

 - Haskell parsing libraries
   - [parsers - Parsing combinators](https://hackage.haskell.org/package/parsers)
   - [trifecta - modern parser combinator library with convenient diagnostics](https://hackage.haskell.org/package/trifecta-1.5.2)
   - [aeson - Fast JSON parsing and encoding](https://hackage.haskell.org/package/aeson)
   - [cassava - A CSV parsing and encoding library](https://hackage.haskell.org/package/cassava)
   - [parsec - Monadic parser combinators](https://hackage.haskell.org/package/parsec)
   - [megaparsec - Monadic parser combinators](https://hackage.haskell.org/package/megaparsec)
   - [attoparsec - Fast combinator parsing for bytestrings and text](https://hackage.haskell.org/package/attoparsec)

### For further reading
 - [Parsec try a-or-b considered harmful - by Edward Z. Yang](http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/)
 - [Parsing CSS with Parsec - by Jakub Arnold](https://blog.jakuba.net/2014/08/10/Parsing-CSS-with-Parsec/)
 - [Real World Haskell - by Bryan O'Sullivan](http://book.realworldhaskell.org/read/)
 - [Real World Haskell - Parsing a Binary Data Format](http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html)
 - [Real World Haskell - Parsec parsing library](http://book.realworldhaskell.org/read/using-parsec.html)
 - [Why Parsing Tools Are Hard - by Josh Haberman](http://blog.reverberate.org/2013/09/ll-and-lr-in-context-why-parsing-tools.html)
 - [Parsing JSON with Aeson - School of Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json)
 - [24 Days of Hackage: aeson - by Oliver Charles](https://ocharles.org.uk/posts/2012-12-07-24-days-of-hackage-aeson.html)
 - [Introduction to parsing text in Haskell with Parsec](https://www.cnblogs.com/ncore/p/6892500.html)
