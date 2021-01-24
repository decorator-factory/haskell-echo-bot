# echo-bot

1. Create a `bot_config.json` file as a copy of `bot_config_template.json`
2. Put your token in the appropriate field
3. Run the bot with the `CONFIG_PATH` environment variable pointing to that file

# Running with `ghcid` for development:
```sh
# -r ≡ reload
# -c ≡ command to run
# --no-height-limit ≡ the error message/warning list can get arbitrarily large
CONFIG_PATH=bot_config.json ghcid -r -c "stack ghci --ghc-options -Wall" --no-height-limit
```
