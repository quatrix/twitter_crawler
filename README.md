twitter_crawler
===============

This should (if finished) fetch your twitter friends (people you follow) to
the 6th degree.

Currently it's just the first thing I ever wrote in erlang, and it just
a simple Twitter API you can play with.

Twitter API Key
===============

For this to work you need to register an App and get an API key,
https://apps.twitter.com/

Then create a ```twitter.secret``` JSON file that looks like this:
```json
{
    "APIKey": "YOUR_API_KEY",
    "APISecret": "YOUR_API_SECRET"
}
```
