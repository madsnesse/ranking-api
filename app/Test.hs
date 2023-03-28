module Test where
    
import Models
import Database
import Engine
import Data.Aeson (encode)

main :: IO()
main = do
    _ <- createDatabaseTables
    player1 <- createPlayer "player1"
    player2 <- createPlayer "player2"

    let league = League 1 "league1" 1
    let match = Match 1 1 (playerId player1) (playerId player2) 5 0
    _ <- saveLeague league
    _ <- addPlayerToLeague league player1
    _ <- addPlayerToLeague league player2
    _ <- addMatch league match
    printLeagueStatus 1

printLeagueStatus :: Int -> IO()
printLeagueStatus leagueId = do
    league <- getLeague leagueId
    players <- getPlayersInLeague leagueId
    matches <- getMatchesInLeague leagueId
    print (encode league)
    print (encode players)
    print (encode matches)