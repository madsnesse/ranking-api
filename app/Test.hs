module Test where
    
import Models
import Database
import Engine
import Data.Aeson (encode)

main :: IO()
main = do
    _ <- createDatabaseTables
    player1 <- createPlayer "player1" "test@test.com"
    player2 <- createPlayer "player2" "test@test2.com"

    league <- createLeague "league1" (playerId player1)
    print league
    match <- createMatch (leagueId league) (playerId player1) (playerId player2) 10 0
    _ <- addPlayerToLeague league player1
    _ <- addPlayerToLeague league player2
    printLeagueStatus 1

printLeagueStatus :: Int -> IO()
printLeagueStatus leagueId = do
    league <- getLeague leagueId
    players <- getPlayersInLeague leagueId
    matches <- getMatchesInLeague leagueId
    print (encode league)
    print (encode players)
    print (encode matches)