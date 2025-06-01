library(DBI)
library(duckdb)

# Connect to (or create) the Auth_Users.duckdb file
con <- dbConnect(duckdb::duckdb(), dbdir = "Auth_Users.duckdb", read_only = FALSE)

# Create the 'users' table
# dbExecute(con, "
#   CREATE TABLE IF NOT EXISTS users (
#     id INTEGER PRIMARY KEY,
#     username TEXT,
#     password TEXT,
#     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
#   );
# ")

# Insert a new User
dbExecute(con, "
  INSERT INTO users (id, username, password) VALUES
  (2, ' ADD USER ', ' ADD PASSWORD ')
  ON CONFLICT DO NOTHING;
")

# View users
users_df <- dbGetQuery(con, "SELECT * FROM users;")
print(users_df)

# Disconnect
dbDisconnect(con, shutdown = TRUE)

