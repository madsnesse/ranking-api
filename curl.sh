# these need to be run individually assuming this is just run once it should work fine,
# but if you want to run it multiple times you need to change the ids of the players and leagues
# or delete te database and start over

echo -e "Create player 1"

curl --location 'localhost:8080/player' \
--header 'Content-Type: application/json' \
--data-raw '{
    "name":"Oliver",
    "email": "Milan.Berge@hotmail.com"
}'

echo -e "\nCreate player 2"

curl --location 'localhost:8080/player' \
--header 'Content-Type: application/json' \
--data-raw '{
    "name":"Steve",
    "email": "Kaya.Collins@gmail.com"
}'

echo -e "\nGet player 1"
curl --location 'localhost:8080/player/Milan.Berge@hotmail.com'

echo -e "\nGet player 2"
curl --location 'localhost:8080/player/Kaya.Collins@gmail.com'

echo -e "\nCreate league"
curl --location 'localhost:8080/league' \
--header 'Content-Type: application/json' \
--data '{
    "leagueName": "test league",
    "ownerId": 1
}'

echo -e "\nAdd players to league"
curl --location --request PUT 'localhost:8080/league/1' \
--header 'Content-Type: application/json' \
--data '{
    "players": [1, 2]
}'

echo -e "\nCreate match"
curl --location 'localhost:8080/match' \
--header 'Content-Type: application/json' \
--data '{
    "leagueId" : 1,
    "playerOne" : 1,
    "playerTwo" : 2,
    "scoreOne" : 10,
    "scoreTwo" : 4
}'

echo -e "\n Get league"
curl --location 'localhost:8080/league/1'