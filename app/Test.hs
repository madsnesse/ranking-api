module Test where
    
import Models
import Database
import Engine

main :: IO()
main = do
    _ <- createDatabaseTables
    let player1 = Player 1 "player1"
    let player2 = Player 2 "player2"
    let match = Match 1 1 2 5 0
    let league = League 1 "league1"
    addPlayerToLeague league player1
    addPlayerToLeague league player2
    addMatch league match
    printLeagueStatus 1

printLeagueStatus :: Int -> IO()
printLeagueStatus id = do
    league <- getLeague id
    players <- getPlayersInLeague id
    -- matches <- getMatchesInLeague id
    print league
    print players
    -- print matches