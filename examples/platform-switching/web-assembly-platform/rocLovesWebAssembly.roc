app "rocLovesWebAssembly"
    packages { pf: "main.roc" }
    imports []
    provides [main] to pf

main =
    x: List I32
    x = [0]
    y = List.set x 0 12
    val =
        when List.get y 0 is
            Ok v -> v
            Err OutOfBounds -> -1
    val
