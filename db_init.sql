CREATE TABLE IF NOT EXISTS Player (
    id SERIAL PRIMARY KEY, 
    username TEXT,
    email TEXT
);
CREATE TABLE IF NOT EXISTS League (
    id SERIAL PRIMARY KEY,
    league_name TEXT,
    owner_id INTEGER,
    FOREIGN KEY (owner_id) REFERENCES Player(id)
);
CREATE TABLE IF NOT EXISTS PlayerLeague (
    player_id INTEGER,
    league_id INTEGER,
    rating INTEGER,
    PRIMARY KEY (player_id, league_id),
    FOREIGN KEY (player_id) REFERENCES Player(id),
    FOREIGN KEY (league_id) REFERENCES League(id)
);
CREATE TABLE IF NOT EXISTS Match (
    match_id SERIAL PRIMARY KEY,
    league_id INTEGER, 
    player_id_one INTEGER,
    player_id_two INTEGER, 
    score_one INTEGER, 
    score_two INTEGER,
    FOREIGN KEY (league_id) REFERENCES League(id)
);