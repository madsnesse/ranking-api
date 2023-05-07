# Ranking api

## Development: 
### Requirements
Requires postgresql, [stack](https://docs.haskellstack.org/en/stable/) and [docker](https://www.docker.com/)
### Building, running and testing

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
I have created a postman collection in `postman_collection.json` that can be used to test the api

### Adding dependencies:
add it under dependencies in package.yml, then run 
```
stack build
```
