# load all required packages
requiredpackages <- c("lintr", "ggplot2", "styler", "config", "twitteR", "tm", "tidyverse", 'tidytext')

for (package in requiredpackages) {
  if (!require(package, character.only = T)) {
    install.packages(package)
    library(package, character.only = T)
  }
}

# recovering twitter's api keys from config.yml
twitter_cfg <- config::get("AccessKey")

# twitter_cfg$access_token
# twitter_cfg$access_token_secret


# remember to keep your config.yml out of your vcs (eg: include it in your .gitignore)
# exemple how to get config from config.yml
# my_awesome_api_configuration <- config::get("my_awesome_api")
# my_awesome_api_configuration$url,
# my_awesome_api_configuration$api_key
