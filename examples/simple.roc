app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        unicode: "../package/main.roc", # use release URL (ends in tar.br) for local example, see github.com/roc/unicode/releases
    }
    imports [
        cli.Stdout,
        unicode.CodePoint,
    ]
    provides [main] to cli

word = "ẇ͓̞͒͟͡ǫ̠̠̉̏͠͡ͅr̬̺͚̍͛̔͒͢d̠͎̗̳͇͆̋̊͂͐"

maybeLength : Result Nat CodePoint.Utf8ParseErr
maybeLength = word |> Str.toUtf8 |> CodePoint.parseUtf8 |> Result.map List.len

main =
    when maybeLength is
        Ok count -> Stdout.line "\n\nThere are a total of \(Num.toStr count) code points in \(word)\n\n"
        Err _ -> crash "ERROR: Unable to parse \(word)!"

