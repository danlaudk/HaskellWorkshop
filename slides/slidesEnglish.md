% Haskell: Functional Programming 
% Han Joosten & Dan Lau
% 2024-03-11


# Welcome!

~~~~ {.haskell}
main = putStrLn "hello!"
~~~~

* My name is Han Joosten
* I started using Haskell in 2006
* I am a co-developer of [Ampersand](https://github.com/AmpersandTarski)

# Slides

~~~~
https://hanjoosten.github.io/HaskellWorkshop
~~~~

* Suited for workshop AND self-learning
* forked from [Bryan O’Sullivan](https://github.com/bos/strange-loop-2011)


# Han's background in (functional) programming

* Introduced to FP at university
    * Twentel, Miranda, Lisp
* Several specification languages:
    * Lotos, ASF+SDF, Typol, LaTeX
* After graduation in 1990 I programmed mainly in imperative programming languages 
    * Pascal, C, Perl, Rexx, Cool:Gen (integrated Case tool)
* I've found Haskell to be a great general-purpose language
* The community of people is amazing


# Dan's background

* 4 year old Scala developer
* previously 4 years data science & econometrics (in R, also FP, without knowing it)
* In 4 Ordina assignments, teams are FP. Current teammates learning Rust. Deploying blockchain
* Completed LG1 last year and facilitated LG2 FP in Elm
* Reactive experience with Akka actors (JVM) 
* Kotlin developer Thom van Kalkeren (Codestar) joins tomorrow

# Discuss

* What is Functional Programming
* What are your experiences? Why FP?
- Do you share some common reactions
- Good use cases for FP 
- Where FP not useful
- Trends and architectures where FP plays a role

Form groups of 3 to a whiteboard/flipboard, and write down some answers. 
We will then go from board to board, and another group will try to riff on what you have written on your board

# Workshop: What to expect 1

* Haskell is a fairly big language
* Since so much is unfamiliar to newcomers, expect to wander far from
  your comfort zone
* We're going to teach you *interesting* things, but not *everything*


# What to expect 2

* This is a *hands-on* workshop: you'll be writing code!
* Work in pairs (pair programming)
* There will be a short break every 45 minutes
* Don't be afraid to ask questions!

# Your tools

* You've already created your Haskell environment, right?

    * [https://github.com/hanjoosten/haskellProjectTemplate/blob/main/README.md](https://github.com/hanjoosten/haskellProject/blob/main/README.md)

* This gives us a great IDE 

    * vs-code with Haskell support
    * A compiler (`ghc`), interpreter (`ghci`), Package managers (`stack` and `cabal`)
    * Some handy libraries and tools

# Problem definition for the rest of the workshop

Given a web site, we want to scrape it and find important web pages.

This involves a lot of figuring stuff out!

1. Learn Haskell

1. Download one web page

1. Extract links from a page, so we can find more pages to download

1. Once we're done, compute which ones are important

1. Make it all fast?


# Let's get started!

Open `Main.hs` in the src directory. It has the following contents:

~~~~ {.haskell}
module Main (main)
where

main = putStrLn "hello, world!"
~~~~

The suffix `.hs` is the standard for Haskell source files.

File names start with a capital letter, and everyone uses `CamelCase`.


# Building it

The package manager is set up to look for `Main.hs` in the src directory. 

This command will install it:

~~~~
stack install
~~~~

The generated output will reveal the name of the executable. Run it to see the output.

* stack is the name of the package manager. It takes care to automatically deal with dependencies on source files and packages.
* Everything required to set up your project is beyond scope of this introduction. 

# Checking in

Is everyone able to build and run their executable?


# Something a little more convenient

It's nice to have fast, native code at our fingertips.

But when *I'm* working, I expect a few things:

* I do lots of exploration.
* I make tons of mistakes.

For these circumstances, a full compiler can be a bit slow.

Instead, I sometimes use the interactive interpreter, `ghci`.

# Let's start GHCi

Easily done in vscode with extension `ghci-helper`:

 * vscode command: `Start GHCi using stack`


It will spin up ghci, load the current file, followed by a prompt:

~~~~
ghci>
~~~~



# Play around

The `ghci` interpreter evaluates expressions interactively.

Try it out:

~~~~ {.haskell}
2 + 2
~~~~

~~~~ {.haskell}
123456781234567812345678 * 87654321876543
~~~~

~~~~ {.haskell}
"foo" ++ "bar"
~~~~

(That `++` is the "append" operator.)



# Running our code in ghci

We defined a function named `main`, so let's invoke it:

~~~~{.haskell}
main
~~~~

Did that work for you?

What about this?

~~~~ {.haskell}
putStrLn "hi mom!"
~~~~

# Directives

All interpreter directives start with a "`:`" character.

Let's see what else `ghci` is up to:

* `:help` tells us what directives are available.
* `:reload` reloads the file we last `:load`ed.
* `:quit` exits back to the shell.

Abbreviation works, so `:r` is `:reload`

# Error messages

Error messages can be intimidating. Once you get used to them, they contain a lot of info. 

Explaination of common errors can be found at:

~~~~
https://errors.haskell.org/
~~~~

# Lists and strings

~~~~ {.haskell}
[1,2,3,4]
~~~~

~~~~ {.haskell}
['h','e','l','l','o']
~~~~

Double quotes are just syntactic sugar for the longer form:

~~~~ {.haskell}
"hello"
~~~~

What does this print?

~~~~ {.haskell}
"foo" == ['f','o','o']
~~~~


# Calling functions: 1

We use white space to separate a function from its argument:

~~~~ {.haskell}
head "foo"
~~~~

~~~~ {.haskell}
head [1,2,3]
~~~~

~~~~ {.haskell}
tail [1,2,3]
~~~~


# Calling functions: 2

If a function takes multiple arguments, we separate them with white
space:

~~~~ {.haskell}
min 3 4
~~~~

If an argument is a compound expression, wrap it in parentheses:

~~~~ {.haskell}
compare (3+5) (2+7)
~~~~

~~~~ {.haskell}
max (min 3 4) 5
~~~~


# Quick exercises: 1

Use `ghci` as a calculator.

The `**` operator performs exponentiation.

* If I invest 5 euro at 3% compound interest per year, how many
  euro's will I have after 10 years?


# Quick exercises: 2

The notation `['a'..'z']` generates a list from start to end,
inclusive.

The `sum` function adds the elements of a list.

* What is the sum of the numbers between 9 and 250, inclusive, *minus* 2?


# Quick exercises: 3

The `show` function renders a value as a string.  Try it!

~~~~ {.haskell}
show (1 == 2)
~~~~

The `length` function tells us how many elements are in a list.

~~~~ {.haskell}
length [1,2,3]
~~~~

* How many digits are in the product of all numbers between 0xBE and
  0xEF, inclusive?


# Defining a function

It is pretty simple to define a new function.

Open up your text editor, create a new file with a `.hs` extension,
and get writing!

~~~~ {.haskell}
isOdd x  =  (rem x 2) == 1
~~~~

* We start with the name of the function.
* Next come the names we want to give its parameter(s), separated by
  white space.
* After those come a single `=` character, with the *body* of the
  function following.

Load your source file into `ghci` and give `myOdd` a try.


# Making life more interesting

Now we can define very simple functions, but we're missing some
important building blocks for more fun.

So let's get to it!


# Conditional execution

Q: What does the familiar `if` look like in Haskell?

A: Familiar!

~~~~ {.haskell}
gcd a b = if b == 0
          then a
	  else gcd b (rem a b)
~~~~

We have the following elements:

* A Boolean expression

* `then` an expression that will be the result if the Boolean is
  `True`
* `else` an expression that will be the result if the Boolean is
  `False`


# Finally! A tiny bit about types

The two possible results of an `if` expression must have the same
type.

If `then` evaluates to a `String`, well `else` must too!

For instance, this makes no sense:

~~~~ {.haskell}
if True
then 3.14
else "wombat"
~~~~

We are forbidden from writing ill-typed expressions like this.


# What about else?

In imperative languages, we can usually leave out the `else` clause
after an `if`.

Not so in Haskell.

Why does this make sense for imperative languages, but not Haskell?


# A nearly trivial exercise

Write a function that appends `", world"` to its argument if the
argument is `"hello"`, or just returns its argument unmodified
otherwise.

* Remember, the "append" function is an operator named `++`.


# Lists in Haskell

We already know what a list looks like in Haskell:

~~~~ {.haskell}
[1,2,3]
~~~~

And of course there's the syntactic sugar for strings:

~~~~ {.haskell}
"foo" == ['f','o','o']
~~~~

But is this everything there is to know?


# List constructors

Supposing we want to construct a list from first principles.

* We write the *empty list* as `[]`.
* Given an existing list, we can add another element to the *front* of
  the list using the `:` operator.


# Type this into ghci

Add an element to an empty list:

~~~~ {.haskell}
1 : []
~~~~


# From single-element lists onwards

What about extending that list?

~~~~ {.haskell}
2 : (1 : [])
~~~~

You're probably guessing now that `[2,1]` is syntactic sugar for
`2:(1:[])`. And you're right!

What is the result of this expression?

~~~~ {.haskell}
5 : 8 : [] == [5,8]
~~~~


# Constructors

We refer to `[]` and `:` as *constructors*, because we use them to
construct lists.

When you create a list, the Haskell runtime has to remember which
constructors you used, and where.

So the value `[5,8]` is represented as:

* A `:` constructor, with `5` as its first parameter, and as its
  second ...
* Another `:` constructor, this time with `8` as its first parameter,
  and now as its second ...
* A `[]` constructor


# What did we see?

Depending on your background, I bet you're thinking something like
this:

* "Hey! Haskell lists look like singly linked lists!"
* "Hey! That looks just like lists built out of `cons` cells in Lisp!"

Right on.


# Why do we care about constructors?

Of course Haskell has to remember what a list is constructed of.

It also lets *us* inspect a list, to see which constructors were used.

How do we do this?

~~~~ {.haskell}
import Data.Char

isCapitalized name
  = case name of
      (first:rest) -> isUpper first
      []           -> False
~~~~


# Welcome to the case expression

A `case` expression allows us to *inspect* a structure to see how it
was constructed.

~~~~ {.haskell}
isCapitalized name
  = case name of
      []           -> False
      (first:rest) -> isUpper first
~~~~

* In between `case` and `of` is the expression we are inspecting.
* If the constructor used was the empty-list constructor `[]`, then
  clearly the `name` we're inspecting is empty, hence not capitalized.

If the constructor used was the "add to the front" `:` operator,
then things get more interesting.

* Whatever was the first parameter of the `:` constructor is bound
  to the name `first`.
* The second parameter of the `:` constructor (i.e. everything in the
  list after the first element) is bound to the name `rest`.
* The expression following the `->` is evaluated with these values.


# Pattern matching

The `case` expression performs what we call *pattern matching*.

* Patterns are checked from top to bottom.
* As soon as a match is found, its right hand side (after the `->`) is
  used as the result of the entire `case` expression.
* If no match succeeds, an exception is thrown.


# A worked example

Let's step through the machinery of what happens if we evaluate this
expression.

~~~~ {.haskell}
isCapitalized "Ann"
~~~~


# Whew! A few exercises!

Finally! We can write slightly more complex functions.

Now that you can inspect the front of a list, you should be able to
process an *entire* list recursively.

First, please write a function named `myLength` that computes the
number of elements in a list.

Next, write a function named `countCaps` that calculates the number of
capital letters in a string.

~~~~ {.haskell}
countCaps "Monkey Butter" == 2
~~~~


# Counting capital letters

Wow, that countCaps function was a pain, right?

Here's my definition that uses only the machinery we've learned so
far:

~~~~ {.haskell}
countCaps string =
  case string of
    []     -> 0
    (x:xs) -> if isUpper x
              then 1 + countCaps xs
              else countCaps xs
~~~~


# Huh.

I thought Haskell was all about concision!?


# Conciseness 1: top-level pattern matching

~~~~ {.haskell}
countCaps []     = 0
countCaps (x:xs) =
    if isUpper x
    then 1 + countCaps xs
    else countCaps xs
~~~~

We can define a function as a series of equations, each containing a
pattern match.

This is nice syntactic sugar for `case`.


# Conciseness 2: guards

~~~~ {.haskell}
countCaps []     = 0
countCaps (x:xs)
   | isUpper x    = 1 + countCaps xs
   | otherwise    = countCaps xs
~~~~

After each `|` is a *guard*.

* If a pattern matches, we evaluate each Boolean guard expression from
  top to bottom.
* When one succeeds, we evaluate the RHS as the body of the function.

(Yes, patterns in a `case` can have guards too.)


# Before

Like the original version, but with use of `case` stripped out:

~~~~ {.haskell}
countCaps xs =
  if null xs
  then 0 
  else if isUpper (head xs)
       then 1 + countCaps (tail xs)
       else countCaps (tail xs)
~~~~

# After

Both shorter and easier to follow:

~~~~ {.haskell}
countCaps []     = 0
countCaps (x:xs)
   | isUpper x    = 1 + countCaps xs
   | otherwise    = countCaps xs
~~~~


# Another approach

Write a new version of `countCaps` as follows:

* Write a function that goes through a list, and which generates a new
  list that contains only its capital letters.
* Use `length` to count the number of elements.

This should give the same result as your first function. Right?


# A change of specification

Suppose we want to count the number of lowercase letters in a string.

This seems almost the same as our function for counting uppercase
letters.

What can we do with this observation?


# Higher order functions

*Higher order function*: a function that accepts another function as a
parameter.

~~~~ {.haskell}
filter pred [] = []
filter pred (x:xs)
  | pred x     = x : filter pred xs
  | otherwise  =     filter pred xs
~~~~

How can we use this to define `countLowerCase`?


# Data in, data out

By now, we've seen several definitions like this:

~~~~ {.haskell}
countLowerCase string =
  length (filter isLower string)
~~~~

This is a recurring pattern:

* A function of one argument
* It's being fed the result of ...
* ... another function of one argument


# Function composition

Haskell doesn't limit us to giving functions alphanumeric names.

Here, we define a function named simply "`.`", which we can use as an
operator:

~~~~ {.haskell}
(f . g) x = f (g x)
~~~~

How to use this?

~~~~ {.haskell}
countLowerCase = length . filter isLower
~~~~


# Understanding composition

If that seemed hard to follow, let's make it clearer.

We'll plug the arguments into the RHS of our function definition:

~~~~ {.haskell}
(f . g) x = f (g x)
~~~~

We had `length` as the first argument to "`.`", and `filter isLower`
as the second:

~~~~ {.haskell}
(length . filter isLower) x 
  = length (filter isLower x)
~~~~


# Local variables

Inside an expression, we can introduce new variables using `let`.

~~~~ {.haskell}
let x = 2
    y = 4
in x + y
~~~~

* Local definitions come after the `let`.
* The expression where we use them comes after the `in`.


# White space

Haskell is sensitive to white space!

* A top-level definition starts in the leftmost column.
* After the beginning of a definition, if the next non-empty line is
  indented further, it is treated as a continuation of that
  definition.
* Never use tabs in your source files.

# White space and local variables

If you're defining local variables, they must start in the same
column.

This is good:

~~~~ {.haskell}
let x = 2
    y = 4
in x + y
~~~~

But this will lead to a compiler error:

~~~~ {.haskell}
let x = 2
      y = 4
in x + y
~~~~


# Composition exercise

Using function composition wherever you can, write a function that
accepts a string and returns a new string containing only the words
that begin with vowels.

* You'll want to play with the `words` and `unwords` functions before
  you start.

Example:

~~~~ {.haskell}
disemvowel "I think, therefore I am."
  == "I I am."
~~~~


# My solution

Here's how I wrote `disemvowel`:

~~~~ {.haskell}
disemvowel = 
  let isVowel c = toLower c `elem` "aeiou"
  in  unwords . filter (isVowel . head) . words
~~~~

Does this remind you of a Unix shell pipeline, only right-to-left?


# Problem definition, once again

Given a web site, we want to scrape it and find important web pages.

We're now Haskell experts, right?

* Download one web page


# Let's download a web page!

We'd really like to rely on a library to download a web page for
us.

At times like this, there's a very handy central repository of open
source Haskell software:

* [http://hackage.haskell.org](http://hackage.haskell.org/)
* (Everyone just calls it "Hackage")

Go there now!

Click on the
[Browse](https://hackage.haskell.org/packages/browse)
link at the top of the page to browse packages.

Alas, the list is overwhelmingly long, but we can find libraries for
all kinds of tasks if we're patient.

`search` can be used to restrict the list to find specific libraries 

* Search for `HTTP`

That still gives us 300+ packages to comb through, but at least it's
better than the 16,400 on the Packages web page.


# Short-cutting the search

We'll use the HTTP client library named `http-conduit`.

We can read about it online:

* [hackage.haskell.org/package/http-conduit](http://hackage.haskell.org/package/http-conduit)

That landing page for a package is intimidating, but look at the section labeled "Modules".

What do you see?


# Adding dependencies

Before we can use `http-conduit`, we must tell the package manager that we need it.

add `http-conduit` to the `dependencies` section in your `package.yaml` file:

~~~~
dependencies:
  - base >= 4.7 && < 5
  - http-conduit
~~~~

The next time you build your program, it figures out all the other libraries that
`http-conduit` depends on, and downloads, compiles, and installs
the whole lot.

Now also add the following dependencies, for you will need them later as well:

~~~~
  - bytestring
  - utf8-string
  - tagsoup
  - network-uri
  - containers
~~~~

# Instalation

Expect the build to take a few minutes and print a lot of output. 

Let's build the packages we specified in our `package.yaml` file:

~~~~
stack install
~~~~

This will install all specified packages and all of their dependencies as well.

# Reading docs: packages and modules

While we're waiting, let's try to figure out how we should use [`http-conduit`](https://hackage.haskell.org/package/http-conduit).

Remember the link to API documentation in the `Modules` section of the package's web
page? Click through to the API docs. 

(For now, ignore the reference to `stackage`)

An API page begins with a title that looks something like this:

~~~~
Network.HTTP.Simple
~~~~

This is the name of a *module*.

A module is a collection of related code.

A *package* is a collection of related modules.

(This will sound familiar if you know Python.)


# Reading docs: the rest

After the initial blurb, a module's docs consists of type signatures
and descriptions.

Here is a really simple type signature:

~~~~
foo :: String
~~~~

How the heck do we read this?

The *name* of the thing being defined comes before the `::`
characters.

Its *type* follows after the `::`.

This means "the value named `foo` has the type `String`".


# Haskell's type system

Up until now, we have not bothered talking about types or type
signatures.

Every expression and value in Haskell has a single type.

Those types can almost always be *inferred* automatically by the
compiler or interpreter.


# The most common basic types

* `Bool`
* `Int`
* `Char`
* `Double`

# A function signature

Here's another type signature:

~~~~
words :: String -> [String]
~~~~

Here we see a new symbol, `->`, which means "this is a function".

The type after the last `->` is the return type of the function.

All of its predecessors are argument types.

So this is a function that takes one `String` argument, and
returns... what?


# List notation

The notation `[a]` means "a list of values, all of some type `a`".

So `[String]` means "a list of values, all of type `String`".

`a` is a type variable, it can be substituted with any concrete type. Notice the lower case for type variables, upper case for concrete types. 


# Type synonyms

What's a `String`? 

* It's not special, just a *synonym* for `[Char]`, i.e. "a list of
  `Char`".
  
We can introduce new synonyms of our own.

~~~~ {.haskell}
type Dollars = Int
~~~~

A type synonym can be handy for documenting an intended use for an
existing type.


# Words

~~~~
words :: String -> [String]
~~~~

We can now read that this function accepts a string as argument, and
returns a list of strings.

From reading its name and type signature, can you guess what `words`
might do?


# Another signature

Tell me about this signature:

~~~~
mystery :: [String] -> String
~~~~

What are some reasonable possible behaviours for this function?


# Reading real-world docs

Here is the very first signature from `http-conduit`:

~~~~
httpBS :: MonadIO m => Request -> m (Response ByteString)
~~~~

This is more complex! How the heck do we read it?

The bits between `::` and '=>' are *constraints* on where we can use
`httpBS` - but let's ignore constraints for now.

* *Important*: it's often safe to gloss over things we don't (yet)
  understand.

We'll also ignore that mysterious lowercase `m` for a bit.

What can we tell about this function?


# Side note: Polymorphic types

In Haskell, a type can contain one or more type variables. 

`Response` is defined together with a type variable. Type variables start with a lowercase character.

~~~~
data Response body
~~~~

In our case, `body` is replaced by a concrete type:

~~~~
Response Bytestring
~~~~

So it is a `Response` of `Bytestring` 

# Bytestring

A `ByteString` is a blob of binary data.

Unlike `String`, it is not represented as a list, but as a packed
array.

However, it contains binary *bytes*, not text!

* Don't use `ByteString` for working with data that you have to
  manipulate as text.

# From binary to text

Now we have a `ByteString`, which we need to turn into text for
manipulating.

Let's cheat, and assume that all web pages are encoded in UTF-8.

# Let's play in ghci!

Does everyone have `http-conduit` installed now?

Add the import of the module to your file, and fire up `ghci`, so we can play with it:

~~~~
import           Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
~~~~

let's load a web page!

~~~~
httpBS "http://example.com/"
~~~~

What is that response? 

# It looks like a `Response Bytestring`

Take a good look at what just appeared as output. 

Prettyprinted, it looks like something like this: 

~~~~
Response 
  {responseStatus = Status {statusCode = 200, statusMessage = "OK"},
   responseVersion = HTTP/1.1,
   responseHeaders = [("Content-Encoding","gzip"),("Accept-Ranges","bytes"), 

   etc.
~~~~

Did you get a response? Yeah!

# Pure code

So far, all of the code we have written has been "pure".

* The behaviour of all of our functions has depended only on their
  inputs.
* All of our data is immutable.
* There's thus no way to change a global variable and modify the
  behaviour of a function.
  
# Impure code

And yet ... somehow we downloaded a web page!

* Web pages clearly are *not* pure.

So can we write code like this?

~~~~ {.haskell}
length (httpBS "http://x.org/")
~~~~

NO.

The type system segregates code that must be pure from code that may
have side effects ("impure" code).


# Are we stuck?

Well, let's look at a simpler example than `httpBS`.

Type this in `ghci`:

~~~~
:type readFile
~~~~

This will tell us what the type of `readFile` is.


# IO

The `:type` directive should print something like this:

~~~~ {.haskell}
readFile :: FilePath -> IO String
~~~~

Notice that `IO` on the result type? 

It means "this function may have side effects".

We often refer to impure functions, with `IO` in the result type, as
*actions*.

* This helps to distinguish them from pure functions.


# Mixing IO and other stuff

The type system keeps track of which functions have `IO` in their
types, and keeps us honest.

We can still mix pure and impure code in a natural way:

~~~~ {.haskell}
charCount fileName = do
  contents <- readFile fileName
  return (length contents)
~~~~


# "do" notation

Critical to what we just saw was the `do` keyword at the beginning of
the function definition.

This introduces a series of `IO` actions, one per line.


# Capturing the results of impure code


To capture the result of an `IO` action, we use `<-` instead of `=`.

~~~~ {.haskell}
contents <- readFile fileName
~~~~

The result (`contents`) is pure - it *does not have* the `IO` type.

This is how we supply pure code with data returned from impure code.


# The "return" action

This is *not* the `return` type you're used to!

It takes a *pure* value (without `IO` in its type), and *wraps* it
with the `IO` type.

Pure code can't call impure code, but it can thread data back into the
impure world using `return`.


# Haskell programs and IO

When you write a Haskell program, its entry point must be named
`main`.

The type of `main` must be:

~~~~ {.haskell}
main :: IO ()
~~~~

`()` is named "unit", and means more or less the same thing as `void`
in C or Java.

What this means is that *all* Haskell programs are impure!

# What is the type of the thing we download?

Try:

~~~~
:t httpBS "http://example.com"
~~~~

Note the type variable `m`. In our case we will use `IO` for it.

# Get the body from the Response
In the documentation you can find function definitions like: 

~~~~
getResponseBody :: Response a -> a
~~~~

In our case we have `Response Bytestring`, so this would become

~~~~
getResponseBody :: Response Bytestring -> Bytestring
~~~~

This can be used to get specific stuff from the response!

# Start to get downloading

~~~~
module Main (main)
where

import           Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

main = putStrLn "hello, world!"

download = do
   response <- httpBS "http://example.com" :: IO(Response B8.ByteString)
   let body = getResponseBody response
   return body
~~~~

Note that we have to tell explicitly that we want to use `IO` as concrete value for `m`, for other monadic types would fit too. (Bonus: Take a look at the error message if you leave the type annotation out.)

# Binary to text

Remember we were planning to cheat earlier?

We had this:

~~~~ {.haskell}
httpBS :: Request -> IO (Response ByteString)
~~~~

We need something whose result is an `IO String` instead.

How should that look?


# UTF-8 conversion

To do the conversion, let's grab a package named `utf8-string`.

Remember that we added it to our package.yaml file previously? Now we can use it. It contains a package named `Data.ByteString.UTF8`.

~~~~ {.haskell}
import Data.ByteString.UTF8
~~~~

It defines a function named `toString`:

~~~~ {.haskell}
toString :: ByteString -> String
~~~~


# UTF-8 conversion exercise

Write an action that downloads a URL and converts it from a
`ByteString` to a `String` using `toString`.

Write a type signature for the action.

* Haskell definitions usually don't require type signatures.
* Nevertheless, we write them for *documentation* on almost all
  top-level definitions.
* Vs-code suggests the type, and will warn you if a top-level signature is missing. 


# Downloading and saving a web page

Use your `download` function to save a local copy of the page you just
wrote.

~~~~ {.haskell}
saveAs :: String -> Int -> IO ()
~~~~

For simplicity, let's save the local files as names containing
numbers:

~~~~ {.haskell}
makeFileName :: Int -> FilePath
makeFileName k = "download-" ++ show k ++ ".html"
~~~~

To save a local copy of a file, you'll need the `writeFile` action.


# Shoveling through HTML

Two truisms:

* Most HTML in the wild is a mess.
* Even parsing well formed HTML is complicated.

So! Let's use another library.

The `tagsoup` package can parse arbitrarily messy HTML.

It will feed us a list of events, like a SAX parser.

We allready added it to our `package.yaml` file, so it is ready for us to use!

# Dealing with problems

Try this:

~~~~ {.haskell}
head [1]
~~~~

Now try this:

~~~~ {.haskell}
head []
~~~~


# Oops

If we pass an empty list, the `head` function throws an exception.

Suppose we need a version of `head` that will *not* throw an
exception.

~~~~ {.haskell}
safeHead :: [a] -> ????
~~~~

What should the `????` be?

Let's invent something.

~~~~ {.haskell}
safeHead (x:xs) = Some x
safeHead []     = None
~~~~


# Some? None?

* We're using a constructor named `Some` to capture the idea "we have
  a result".
  
* The constructor `None` indicates "we don't have a result here".

To bring these constructors into existence, we need to declare a new
type.

~~~~ {.haskell}
data Perhaps a = Some a
               | None
~~~~

The `|` character separates the constructors. We can read it as:

* The `Perhaps` type has two constructors:

* `Some` followed by a single argument

* or `None` with no arguments


# Maybe

Actually, Haskell already has a `Perhaps` type.

~~~~ {.haskell}
data Maybe a = Just a
             | Nothing
~~~~

The `a` is a *type parameter*, meaning that when we write this type,
we have to supply another type as a parameter:

* `Maybe Int`

* `Maybe String`


# Using constructors

If we want to construct a `Maybe Int` using the `Just` constructor, we
must pass it an `Int`.

~~~~ {.haskell}
Just 1  :: Maybe Int
Nothing :: Maybe Int
~~~~

This will not work, because the types don't match:

~~~~ {.haskell}
Just [1] :: Maybe String
~~~~


# Pattern matching over constructors

We can pattern match over the constructors for `Maybe` just as we did
for lists.

~~~~ {.haskell}
case foo of
  Just x  -> x
  Nothing -> bar
~~~~


# Tags

The [`tagsoup`](https://hackage.haskell.org/package/tagsoup-0.14.8/docs/Text-HTML-TagSoup.html) package defines the following type:

~~~~ {.haskell}
data Tag = TagOpen str [Attribute str]
         | TagClose str
         | TagText str
         | TagComment str
         | TagWarning str
         | TagPosition !Row !Column
~~~~

What do you think the constructors mean?


# Pattern matching on a Tag

Suppose we want to write a predicate that will tell is if a `Tag` is
an opening tag.

* What should the type of this function be?

* What should its body look like?


# Don't care!

Our first body looked like this:

~~~~ {.haskell}
isOpenTag (TagOpen x y)     = True
isOpenTag (TagClose x)      = False
isOpenTag (TagText x)       = False
isOpenTag (TagComment x)    = False
isOpenTag (TagWarning x)    = False
isOpenTag (TagPosition x y) = False
~~~~

Concise, but ugly. 

* We really only care about one constructor.

* We never use the variables `x` or `y` that we declare.


# The wild card pattern

We can write "I don't care what this pattern or variable is" using the
"`_`" character.

~~~~ {.haskell}
isOpenTag (TagOpen _ _) = True
isOpenTag  _            = False
~~~~

The wild card pattern always matches.

* Since we don't care about `x` or `y`, we can state that explicitly
  using `_`.
  
* Since we don't care about any constructor except `TagOpen`, we can
  match all the others using `_`.
  

# Just a quick question

Why don't we write the function like this?

~~~~ {.haskell}
isOpenTag  _            = False
isOpenTag (TagOpen _ _) = True
~~~~


# Extracting links from a web page

Suppose we have a page in memory already.

* Browse the `tagsoup` docs, in the [`Text.HTML.TagSoup`](https://hackage.haskell.org/package/tagsoup-0.14.8/docs/Text-HTML-TagSoup.html) module.

* Find a function that will parse a web page into a series of tags.


# Let's use it!

~~~~ {.haskell}
processPage url = do
  page <- download url
  return (parseTags page)
~~~~


# Tidying tags up

Parsed tags can contain a mixture of tag names.

~~~~
<A HREF="...">
~~~~

~~~~
<a hrEF="...">
~~~~

* Find a `tagsoup` function that will turn tag names and attributes to
  lower case.
  

# Canonical tags

Let's use our function to clean up the result of `parseTags`.

~~~~ {.haskell}
processPage url = do
  page <- download url
  return
    (canonicalizeTags
      (parseTags page))
~~~~


# Extracting links

We only care about open tags that are links, so `<a>` tags.

* How would we write the type of a function that will indicate whether
  a `Tag` is an open tag with the correct name?

* How would we use this function to extract only the open tags from a
  list of parsed tags?
  
  
# Whee!

This cascade is getting a bit ridiculous.

~~~~ {.haskell}
processPage url = do
  page <- download url
  return
    (filter (isTagOpenName "a")
      (canonicalizeTags
        (parseTags page)))
~~~~

Two observations:

* Our action is now mostly pure code.

* It sure looks like a pipeline.


# A rewriting exercise

Take this function and split it into pure and impure parts.

Write the pure part using function composition.

~~~~ {.haskell}
processPage url = do
  page <- download url
  return
    (filter (isTagOpenName "a")
      (canonicalizeTags
        (parseTags page)))
~~~~


# My solution

~~~~ {.haskell}
processPage url = do
  page <- download url
  return (process page)

process =
    filter (isTagOpenName "a") .
    canonicalizeTags .
    parseTags
~~~~


# More stuff to filter out

Let's skip `nofollow` links.

We want to get the `"rel"` attribute of a tag.

* Find a function that extracts an attribute from a tag.


# No following

~~~~ {.haskell}
nofollow tag = fromAttrib "rel" tag == "nofollow"
~~~~

~~~ {.haskell}
process =
    filter (not . nofollow) .
    filter (isTagOpenName "a") .
    canonicalizeTags .
    parseTags
~~~~


# We have a list of \<a> tags

How would we extract the `"href"` attribute from every element of the
list?


# Only non-empty \<a href\> tags

~~~~ {.haskell}
process =
    filter (not . null) .
    map (fromAttrib "href") .
    filter (not . nofollow) .
    filter (isTagOpenName "a") .
    canonicalizeTags .
    parseTags
~~~~


# Canonical URLs

Links can be absolute, relative, or invalid garbage, and we only want
valid-looking absolute links.

To properly create an absolute link, we need to know the absolute URL
of the page we're looking at.

~~~~ {.haskell}
canonicalizeLink :: String -> String -> Maybe String
~~~~


# Working with URIs

The `Network.URI` package contains some functions we might find handy.

~~~~ {.haskell}
parseURI :: String -> Maybe URI
parseURIReference :: String -> Maybe URI
uriToString id "" :: URI -> String
nonStrictRelativeTo :: URI -> URI -> Maybe URI
~~~~


# A monster of indentation

This is really hard to read!

~~~~ {.haskell}
import Network.URI

canon :: String -> String -> Maybe String
canon referer path =
  case parseURI referer of
    Nothing -> Nothing
    Just r  ->
      case parseURIReference path of
        Nothing -> Nothing
        Just p  -> Just (uriToString id (nonStrictRelativeTo p r) "")
~~~~

Surely there's a better way.


# Stair stepping

Notice that that function was a series of `case` inspections of
`Maybe` values?

Suppose we had a function that accepted a normal value, and returned a
`Maybe` value.

~~~~ {.haskell}
a -> Maybe b
~~~~

And suppose we had a concise syntax for writing an anonymous function.

~~~~ {.haskell}
\a -> "hi mom! " ++ a
~~~~

The `\` is pronounced "lambda".


# Observation

The `case` analysis is quite verbose. Suppose we had a function that
performed it, and called another function if our value was `Just`.

~~~~ {.haskell}
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind  Nothing      _     = Nothing
bind (Just value) action = action value
~~~~


# Using bind

How could we use this?

~~~~ {.haskell}
canon1  referer path =
  parseURI referer                `bind`
   \r -> parseURIReference path   `bind`
    \p -> Just (uriToString id (nonStrictRelativeTo p r) "")
~~~~

If we enclose a function name in backticks, we can use the function as
an infix operator.


# Reformatting the code

~~~~ {.haskell}
canon  referer path =
  parseURI referer         `bind` \r -> 
  parseURIReference path   `bind` \p -> 
  Just (uriToString id (nonStrictRelativeTo p r) "")
~~~~


# A built-in name for bind

The `>>=` operator is a more general version of our `bind` function.

~~~~ {.haskell}
canon  referer path =
  parseURI referer         >>= \r -> 
  parseURIReference path   >>= \p -> 
  Just (uriToString id (nonStrictRelativeTo p r) "")
~~~~


# Using syntactic sugar

Here's some tidier syntax that should look familiar.

~~~~ {.haskell}
canonicalize :: String -> String -> Maybe String

canonicalize referer path = do
  r <- parseURI referer
  p <- parseURIReference path
  return (uriToString id (nonStrictRelativeTo p r) "")
~~~~


# Nearly there

~~~~ {.haskell}
process url =
   map (canonicalize url) .
   filter (not . null) .
   map (fromAttrib "href") .
   filter (\t -> fromAttrib "rel" t /= "nofollow") .
   filter (isTagOpenName "a") .
   canonicalizeTags .
   parseTags
~~~~

One awkward thing: what is the type of this function?


# From [Maybe a] to [a]

Go to this web site:

* [haskell.org/hoogle](http://haskell.org/hoogle)

Type this into the search box:

~~~~ {.haskell}
[Maybe a] -> [a]
~~~~

What does the first result say?


# We're getting closer!

~~~~ {.haskell}
import Data.Maybe
import Network.URI

links url =
  catMaybes .
  map (canonicalize url) .
  filter (not . null) .
  map (fromAttrib "href") .
  filter (\t -> fromAttrib "rel" t /= "nofollow") .
  filter (isTagOpenName "a") .
  canonicalizeTags .
  parseTags
~~~~

# We're there!

In Vscode, a suggestion is done (blue wiggely line) to use `mapMaybe`.

Hoover over the blue line, select the blue dot and `apply hint "use mapMaybe"` 

~~~~{.haskell}
links :: String -> String -> [String]
links url =
    mapMaybe (canonicalizeLink url) . filter (not . null) .
    map (fromAttrib "href") .
    filter (not . nofollow) .
    filter (isTagOpenName "a") .
    canonicalizeTags .
    parseTags
~~~~


# From links to spidering

If we can download the links from one page, we can easily write a
spider to follow those links.

To keep things simple, let's set a limit on the number of pages we'll
download.

What information do we want to generate?

What do we need to track along the way?


# What we need to track

Here's the state we need to maintain:

* The number of pages we have downloaded

* A collection of pages we have seen links to, but haven't downloaded

* A collection of pages and their outbound links


# Tracking what we've seen

For any given page, we need to remember both it and all the pages it
links to.

One possibility for associating the two is a *tuple*:

~~~~ {.haskell}
("http://x.org/", ["http://microsoft.com/"])
~~~~

Tuples are useful any time we want mixed-type data without the hassle
of creating a new type.

Speaking of a new type, here's how we'd define one:

~~~~ {.haskell}
data Link = Link String [String]

-- Let's define some accessors, too.
linkFrom (Link url _) = url
linkTo (Link _ links) = links
~~~~


# Avoiding duplication

We don't want to visit any URL twice.

How do we avoid this?

~~~~ {.haskell}
visited url = elem url . map linkTo
~~~~

This function has a problem - what is that problem?


# Better performance

We really want a structure with a fast lookup operation.

What would you use in your language?


# Maps and importing

In Haskell, we have mutable hash tables, but we don't use them.

Instead, we use *immutable* key-value maps.

We must perform fancy module importing tricks because the `Data.Map`
module defines a lot of names that would otherwise overlap with
built-in names.

This means "only import the name `Map` from `Data.Map`":

~~~~ {.haskell}
import Data.Map (Map)
~~~~

And this means "import everything from `Data.Map`, but all those names
must be prefixed with `Map.`":

~~~~ {.haskell}
import qualified Data.Map as Map
~~~~


# What use is an immutable data structure?

Everyone knows how to add a key and value to a hash table, right?

And that seems like a fundamental operation.

What do we do with maps?

* Create a *new* map that is identical to the one we supply, with the
  requested element added.

How can this possibly work? Is it efficient?


# A fistful of dollars

Here's a surprisingly handy built-in operator:

~~~~ {.haskell}
f $ x = f x
~~~~

Why is this useful? Because it lets us eliminate parentheses.

Before:

~~~~ {.haskell}
explode k = error ("failed on " ++ show k)
~~~~

After:

~~~~ {.haskell}
explode k = error $ "failed on " ++ show k
~~~~


# Partial application

This is annoying to write:

~~~~ {.haskell}
increment k = 1 + k
~~~~

Almost as bad:

~~~~ {.haskell}
\k -> 1 + k
~~~~

Much handier, and identical:

~~~~ {.haskell}
(1+)
~~~~

In fact, this is valid:

~~~~ {.haskell}
increment = (1+)
~~~~

# Spidering, in all its glory

~~~~ {.haskell}
spider :: Int -> String -> IO (Map.Map String [String])
spider count url0 = go 0 Map.empty (Set.singleton url0)
  where
    go k seen queue0
        | k >= count = return seen
        | otherwise  =
      case Set.minView queue0 of
        Nothing -> return seen
        Just (url, queue) -> do
          request <- parseRequest  url
          page <- download request
          let ls       = links url page
              newSeen  = Map.insert url ls seen
              notSeen  = Set.fromList .
                         filter (`Map.notMember` newSeen) $ ls
              newQueue = queue `Set.union` notSeen
          go (k+1) newSeen newQueue

~~~~


# Where do we stand?

We can now:

* Download a web page

* Extract its links

* Spider out from there, without repeat visits

What remains?

* We could spider multiple pages concurrently

* Or we could compute which pages are "important"


# Fin

At this point, if we have miraculously not run out of time, we're
going on a choose-your-own-adventure session in Emacs.

Thanks for sticking with the slideshow so far!
