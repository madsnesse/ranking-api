# Ranking api

## Status: 
* What have you succeeded with so far.
    * Created a Controller module which is the entryway to the endpoints, mapping different paths/http methods to different functions in the Engine module
    * Engine module communicates with the database module
    * Database module does SQL queries using postgresql-simple
    * Created data classes representing the tables in the database i.e. the different entities in the application
    * some "helper" modules to separate some of the functions and make it more readable
* What is currently challenging in the project.
    * Need to implement remaining endpoints
    * Need to create the algorithm that calculates and updates ranking for players after a match is entered
    * Need to utilize more of the "advanced" functional programming techniques that have been covered in the course, I.E. rewrite parts that are already present
    * Want to see if there is a better way of handling DB connection than passing around the connection between functions
    * I have also got a problem with naming functions, since for instance I want to seperate the json request for creating a match from the match model, but they both use a field "leagueId". The way I have solved this for now is just using slightly different names, but I think I want to solve it differently by for instance splitting the program more up so that functions with the same name are not called in the same module. The way I have mitigated this temporarily is by giving the same functions slightly different names like "leagueId, league_id, leagueId' "


I struggled for a long time parsing between different types etc. Noe that I have a working ground to work from I think the remaining work will go a lot smoother and I will have some time to rewrite/refactor the parts I have implemented already.

## Development: 
### Requirements
Requires postgres, [stack](https://docs.haskellstack.org/en/stable/) and [docker](https://www.docker.com/)
### Building, running and testing
NOTE: zsh which is default on newer macos is not supported with stack, requires bash shell.

Start database using `docker-compose.yml` by running `docker-compose up -d` 

```
stack build
```

builds the application
then run 

```
stack run
```
 to run application

## Postman
I have created a postman collection in `postman_collection.json` that cna be used to test the api
### Adding dependencies:
add it under dependencies in package.yml, then run 
```
stack build
```

## Entity relations (preliminary)
<img src="ranking_api.png">