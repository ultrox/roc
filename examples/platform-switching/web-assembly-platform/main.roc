platform "echo-in-web-assembly"
    requires {} { main : I32 }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : I32
mainForHost = main
