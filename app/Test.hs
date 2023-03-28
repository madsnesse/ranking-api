module Test where
    
import Models
import Database
import Engine

main :: IO()
main = do
    _ <- createDatabaseTables
    let player1 = Player 1 "player1"
    let player2 = Player 2 "player2"
    let match = Match 1 (playerId player1) (playerId player2) 5 0
    let league = League 1 "league1" 1
    _ <- savePlayer player1
    _ <- saveLeague league
    _ <- savePlayer player2
    _ <- addPlayerToLeague league player1
    _ <- addPlayerToLeague league player2
    _ <- addMatch league match
    printLeagueStatus 1

printLeagueStatus :: Int -> IO()
printLeagueStatus leagueId = do
    league <- getLeague leagueId
    players <- getPlayersInLeague leagueId
    -- matches <- getMatchesInLeague id
    print league
    print players
    -- print matches