app "app"
    packages { pf: "platform.roc" }
    imports []
    provides [main] to pf

main = Foo "This is a test"
