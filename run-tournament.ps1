param
(
    [Parameter(Mandatory)]
    [String]$Engine1,
    [Parameter(Mandatory)]
    [String]$Engine2
)

$cuteChessCli = "C:\Program Files (x86)\Cute Chess\cutechess-cli.exe"
$book = "C:\Users\jbhou\BanksiaGui-0.58-win64\bsg-engines\Cerebellum3Merge.bin"
$pgnout = "D:\cute-chess-data\tournament.pgn"

& $cuteChessCli `
    -engine cmd=$Engine1 proto=uci `
    -engine cmd=$Engine2 proto=uci `
    -each `
        tc=inf/5+0.1 `
        book=$book `
        bookdepth=5 `
    -rounds 100 -games 2 -repeat 2 -maxmoves 150 `
    -concurrency 4 `
    -ratinginterval 10 `
    -pgnout $pgnout
