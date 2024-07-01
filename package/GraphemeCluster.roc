module [
    split,
    walk,
    walkGraphemeClusters,
]

import CodePoint exposing [CodePoint]
import InternalEmoji exposing [isPictographic]
import InternalCP
import InternalGBP exposing [GBP]

## Split a string into extended grapheme clusters
split : Str -> Result (List Str) _
split = \str ->
    walk str [] \acc, grapheme ->
        List.append acc grapheme

walk : Str, state, (state, Str -> state) -> Result state CodePoint.Utf8ParseErr
walk = \str, initialState, f ->
    bytes = Str.toUtf8 str
    walkGraphemeClusters bytes initialState \state, currentCluster ->
        currentStr =
            currentCluster
            |> CodePoint.toStr
            |> Result.mapErr \_ -> crash
                    """
                    This should never happen.
                    Since the code points came from a valid UTF-8 string,
                    they should be able to be converted back to a string.
                    """
            |> Result.withDefault ""
        f state currentStr

graphemeBreakingProperty : CodePoint -> GBP
graphemeBreakingProperty = InternalGBP.fromCP

# Define a type for grapheme cluster
GraphemeCluster : List CodePoint

# Function to iterate through grapheme clusters
walkGraphemeClusters : List U8, state, (state, GraphemeCluster -> state) -> Result state CodePoint.Utf8ParseErr
walkGraphemeClusters = \bytes, initialState, f ->
    CodePoint.walkUtf8 bytes { state: initialState, currentCluster: [], previousGBP: Other } \acc, cp ->
        gbp = graphemeBreakingProperty cp

        currentBreakingRule = breakingRule acc.currentCluster cp

        if shouldBreak currentBreakingRule then
            newState = f acc.state acc.currentCluster
            { state: newState, currentCluster: [cp], previousGBP: gbp }
        else
            { acc & currentCluster: List.append acc.currentCluster cp, previousGBP: gbp }
    |> Result.map \finalAcc ->
        if List.isEmpty finalAcc.currentCluster then
            finalAcc.state
        else
            f finalAcc.state finalAcc.currentCluster

shouldBreak : GBRule -> Bool
shouldBreak = \rule ->
    when rule is
        GBRule2
        | GBRule4
        | GBRule5
        | GBRule999 -> Bool.true

        GBRule1
        | GBRule3
        | GBRule6
        | GBRule7
        | GBRule8
        | GBRule9
        | GBRule9a
        | GBRule9b
        | GBRule9c
        | GBRule11
        | GBRule12 -> Bool.false

GBRule : [
    GBRule1,
    GBRule2,
    GBRule3,
    GBRule4,
    GBRule5,
    GBRule6,
    GBRule7,
    GBRule8,
    GBRule9,
    GBRule9a,
    GBRule9b,
    GBRule9c,
    GBRule11,
    GBRule12,
    GBRule999,
]

## Helper function to determine if we should break between two code points
## Implement the rules described at the TR9 section 3.1.1 Grapheme Cluster Boundary Rules
## See: https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules
breakingRule : GraphemeCluster, CodePoint -> GBRule
breakingRule = \currentCluster, nextCP ->

    previousCodePointRes = List.last currentCluster

    u32 = InternalCP.toU32 nextCP

    when previousCodePointRes is
        Err ListWasEmpty -> GBRule1
        Ok previousCP ->
            previousGBP = graphemeBreakingProperty previousCP
            nextGBP = graphemeBreakingProperty nextCP

            # The following pattern matching has many strange cases in the form of
            #            (_, _) if test -> rule
            # This is because an apparent compiler bug where rules with wildcards are not matched correctly.
            # Hopefully those will be fixed in the future and we can simplify this code
            when (previousGBP, nextGBP) is
                # =============================================================================
                # Do not break between a CR and LF. Otherwise, break before and after controls.
                # =============================================================================
                # GB3                                          CR  ×  LF
                (CR, LF) ->
                    GBRule3

                # GB4                         (Control | CR | LF)  ÷  _
                # (Control, _) -> GBRule4
                # (CR, _) -> GBRule4
                # (LF, _) -> GBRule4
                (_, _) if previousGBP == Control || previousGBP == CR || previousGBP == LF ->
                    GBRule4

                # GB5                                            _ ÷ (Control | CR | LF)
                # (_, Control) -> GBRule5
                # (_, CR) -> GBRule5
                # (_, LF) -> GBRule5
                (_, _) if nextGBP == Control || nextGBP == CR || nextGBP == LF ->
                    GBRule5

                # =============================================================================
                # Do not break Hangul syllable sequences.
                # =============================================================================
                # GB6
                (L, L)
                | (L, V)
                | (L, LV)
                | (L, LVT) ->
                    GBRule6

                # GB7
                (LV, V)
                | (LV, T)
                | (V, V)
                | (V, T) ->
                    GBRule7

                # GB8
                (LVT, T)
                | (T, T) ->
                    GBRule8

                # =============================================================================
                # Do not break before extending characters or ZWJ.
                # =============================================================================
                # GB9                                            _ ×  (Extend | ZWJ)
                # (_, Extend) -> GBRule9
                # (_, ZWJ) -> GBRule9
                (_, _) if nextGBP == Extend || nextGBP == ZWJ ->
                    GBRule9

                # GB9a                                           _ ×  SpacingMark
                # (_, SpacingMark) -> GBRule9a
                (_, _) if nextGBP == SpacingMark ->
                    GBRule9

                # GB9b                                    Prepend  ×  _
                # (Prepend, _) -> GBRule9b
                (_, _) if previousGBP == Prepend ->
                    GBRule9b

                # GB9c \p{InCB=Consonant} [ \p{InCB=Extend} \p{InCB=Linker} ]* \p{InCB=Linker} [ \p{InCB=Extend} \p{InCB=Linker} ]* × \p{InCB=Consonant}
                # See: https://www.unicode.org/reports/tr44/#Indic_Conjunct_Break
                # TODO: Implement this rule??? It's not clear how to implement it or even
                # if it's necessary since it doesn't seem to be used in the Unicode tests
                (_, _) if Bool.false ->
                    GBRule9c

                # =============================================================================
                # Do not break within emoji modifier sequences or emoji zwj sequences.
                # =============================================================================
                # GB11      \p{Extended_Pictographic} Extend* ZWJ  ×  \p{Extended_Pictographic}
                (ZWJ, _) if isEmojiModSeq currentCluster && isPictographic u32 ->
                    GBRule11

                # =============================================================================
                # Do not break within emoji flag sequences.
                # That is, do not break between regional indicator (RI) symbols if there is an
                # odd number of RI characters before the break point.
                # =============================================================================
                # GB12     sot (RI RI)* RI × RI
                # GB13     [^RI] (RI RI)* RI × RI
                # (RI, RI) if isOddRegionalIndicator currentCluster ->
                (RI, _) if isOddRegionalIndicator currentCluster && nextGBP == RI ->
                    GBRule12

                # =============================================================================
                # Otherwise, break everywhere.
                # =============================================================================
                # GB999                                        Any ÷ Any
                _ ->
                    GBRule999

isEmojiModSeq : GraphemeCluster -> Bool
isEmojiModSeq = \graphemes ->
    when graphemes is
        [a, ..] if isPictographic (InternalCP.toU32 a) -> Bool.true
        _ -> Bool.false

isOddRegionalIndicator : GraphemeCluster -> Bool
isOddRegionalIndicator = \graphemes ->
    isRegionalIndicator = \cp ->
        graphemeBreakingProperty cp == RI

    (List.len graphemes % 2 == 1)
    && List.all graphemes \cp -> isRegionalIndicator cp
