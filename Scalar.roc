interface Scalar
    exposes [Scalar, toU32, toCodePt, appendToUtf8, parseUtf8, fromCodePt, countUtf8Bytes, fromU32]
    imports [CodePt.{ CodePt }, CodePtInternal]

## A [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value) - that is,
## any [code point](./CodePt#CodePt) except for [high-surrogate](http://www.unicode.org/glossary/#high_surrogate_code_point)
## and [low-surrogate](http://www.unicode.org/glossary/#low_surrogate_code_point) code points.
Scalar := U32
    has [Eq, Hash]

toU32 : Scalar -> U32
toU32 = \@Scalar u32 -> u32

fromU32 : U32 -> Result Scalar [InvalidScalar]
fromU32 = \_u32 ->
    crash "TODO implement" # this can't just delegate to CodePt.fromU32; scalars are a subset of code points

toCodePt : Scalar -> CodePt
toCodePt = \@Scalar u32 ->
    CodePtInternal.fromU32Unchecked u32

## Convert a code point to a scalar value. This can fail if the given
## code point is
fromCodePt : CodePt -> Result Scalar [NonScalarCodePt]
fromCodePt = \codePt ->
    if CodePt.isValidScalar codePt then
        Ok (@Scalar (CodePt.toU32 codePt))
    else
        Err NonScalarCodePt

fromU32 : U32 -> Result Scalar [InvalidScalar]
fromU32 = \_u32 ->
    crash "TODO implement" # this can't just delegate to CodePt.fromU32; scalars are a subset of code points


fromStr : Str -> List Scalar
fromStr = \str ->
    crash "TODO implement" # https://www.roc-lang.org/builtins/Str#toScalars

appendToStr : Scalar, Str -> Str
appendToStr = \@Scalar u32, str ->
    when Str.appendScalar str u32 is
        Ok answer -> answer
        Err InvalidScalar ->
            u32str = Num.toStr u32

            crash "appendToStr received a Scalar value of \(u32str). This is an invalid Unicode scalar value, so it should not have been possible to obtain a `Scalar` which wraps it!"

walkStr : Str, state, (state, Scalar -> state) -> state

walkStrUntil : Str, state, (state, U32 -> [Break state, Continue state]) -> state

## If the string begins with a [Unicode code point](http://www.unicode.org/glossary/#code_point)
## equal to the given [U32], returns [Bool.true]. Otherwise returns [Bool.false].
##
## If the given string is empty, or if the given [U32] is not a valid
## code point, returns [Bool.false].
## ```
## expect Str.startsWithScalar "鹏 means 'roc'" 40527 # "鹏" is Unicode scalar 40527
## expect !Str.startsWithScalar "9" 9 # the Unicode scalar for "9" is 57, not 9
## expect !Str.startsWithScalar "" 40527
## ```
##
## ## Performance Details
##
## This runs slightly faster than [Str.startsWith], so
## if you want to check whether a string begins with something that's representable
## in a single code point, you can use (for example) `Str.startsWithScalar '鹏'`
## instead of `Str.startsWith "鹏"`. ('鹏' evaluates to the [U32] value `40527`.)
## This will not work for graphemes which take up multiple code points, however;
## `Str.startsWithScalar '👩‍👩‍👦‍👦'` would be a compiler error because 👩‍👩‍👦‍👦 takes up
## multiple code points and cannot be represented as a single [U32].
## You'd need to use `Str.startsWithScalar "🕊"` instead.
startsWithScalar : Str, U32 -> Bool

## Returns a [List] of the [Unicode scalar values](https://unicode.org/glossary/#unicode_scalar_value)
## in the given string.
##
## (Roc strings contain only scalar values, not [surrogate code points](https://unicode.org/glossary/#surrogate_code_point),
## so this is equivalent to returning a list of the string's [code points](https://unicode.org/glossary/#code_point).)
## ```
## expect Str.toScalars "Roc" == [82, 111, 99]
## expect Str.toScalars "鹏" == [40527]
## expect Str.toScalars "சி" == [2970, 3007]
## expect Str.toScalars "🐦" == [128038]
## expect Str.toScalars "👩‍👩‍👦‍👦" == [128105, 8205, 128105, 8205, 128102, 8205, 128102]
## expect Str.toScalars "I ♥ Roc" == [73, 32, 9829, 32, 82, 111, 99]
## expect Str.toScalars "" == []
## ```
toScalars : Str -> List U32

## Append a [U32] scalar to the given string. If the given scalar is not a valid
## unicode value, it returns [Err InvalidScalar].
## ```
## expect Str.appendScalar "H" 105 == Ok "Hi"
## expect Str.appendScalar "😢" 0xabcdef == Err InvalidScalar
## ```
appendScalar : Str, U32 -> Result Str [InvalidScalar]
appendScalar = \string, scalar ->
    if isValidScalar scalar then
        Ok (appendScalarUnsafe string scalar)
    else
        Err InvalidScalar

isValidScalar : U32 -> Bool
isValidScalar = \scalar ->
    scalar <= 0xD7FF || (scalar >= 0xE000 && scalar <= 0x10FFFF)

getScalarUnsafe : Str, Nat -> { scalar : U32, bytesParsed : Nat }

## Walks over the unicode [U32] values for the given [Str] and calls a function
## to update state for each.
## ```
## f : List U32, U32 -> List U32
## f = \state, scalar -> List.append state scalar
## expect Str.walkScalars "ABC" [] f == [65, 66, 67]
## ```
walkScalars : Str, state, (state, U32 -> state) -> state
walkScalars = \string, init, step ->
    walkScalarsHelp string init step 0 (Str.countUtf8Bytes string)

walkScalarsHelp : Str, state, (state, U32 -> state), Nat, Nat -> state
walkScalarsHelp = \string, state, step, index, length ->
    if index < length then
        { scalar, bytesParsed } = getScalarUnsafe string index
        newState = step state scalar

        walkScalarsHelp string newState step (index + bytesParsed) length
    else
        state


## Walks over the unicode [U32] values for the given [Str] and calls a function
## to update state for each.
## ```
## f : List U32, U32 -> [Break (List U32), Continue (List U32)]
## f = \state, scalar ->
##     check = 66
##     if scalar == check then
##         Break [check]
##     else
##         Continue (List.append state scalar)
## expect Str.walkScalarsUntil "ABC" [] f == [66]
## expect Str.walkScalarsUntil "AxC" [] f == [65, 120, 67]
## ```
walkScalarsUntil : Str, state, (state, U32 -> [Break state, Continue state]) -> state
walkScalarsUntil = \string, init, step ->
    walkScalarsUntilHelp string init step 0 (Str.countUtf8Bytes string)

walkScalarsUntilHelp : Str, state, (state, U32 -> [Break state, Continue state]), Nat, Nat -> state
walkScalarsUntilHelp = \string, state, step, index, length ->
    if index < length then
        { scalar, bytesParsed } = getScalarUnsafe string index

        when step state scalar is
            Continue newState ->
                walkScalarsUntilHelp string newState step (index + bytesParsed) length

            Break newState ->
                newState
    else
        state

