app "rocLovesPlatforms"
    packages { pf: "c-platform/main.roc" }
    # To switch platforms, comment-out the line above and un-comment one below.
    # packages { pf: "rust-platform/main.roc" }
    # packages { pf: "swift-platform/main.roc" }
    # packages { pf: "web-assembly-platform/main.roc" } # See ./web-assembly-platform/README.md
    # packages { pf: "zig-platform/main.roc" }
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
    valStr = Num.toStr val
    "Roc <3 Web Assembly!\nThe list has value: \(valStr)\n"