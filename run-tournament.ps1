param
(
    [Parameter(Mandatory)]
    [String]$Engine1,
    [Parameter(Mandatory)]
    [String]$Engine2,
    [switch]$sprt = $false
)

$cuteChessCli = "C:\Program Files (x86)\Cute Chess\cutechess-cli.exe"
$book = "C:\Users\jbhou\BanksiaGui-0.58-win64\bsg-engines\Cerebellum3Merge.bin"
$pgnout = "D:\cute-chess-data\tournament.pgn"

$numRounds = 100

$sprtArgs = @()
if ($sprt) {
    $sprtArgs = @("-sprt", "elo0=0", "elo1=10", "alpha=0.05", "beta=0.05")
    $numRounds = 1000
}

& $cuteChessCli `
    -engine cmd=$Engine1 proto=uci `
    -engine cmd=$Engine2 proto=uci `
    -each `
        tc=inf/5+0.1 `
        book=$book `
        bookdepth=5 `
    -rounds $numRounds -games 2 -repeat 2 -maxmoves 150 `
    -concurrency 4 `
    -ratinginterval 10 `
    -pgnout $pgnout `
    @sprtArgs
