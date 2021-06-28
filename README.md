# Weather-server
This is a simple prototype of proxy server with weather forecasts caching

## Discription
This server connects to the [weatherapi](www.weatherapi.com) service (so you need to register an account and get the API key)

It provides one request: `localhost:"port"/"rootapi"/city/*city_name*/time/*time*`<br>
where `port` and `rootapi` are configurable parameters, `city_name` and `time` are request parameters

There are two processes in this app:
1. The server itself, which logic is:
* Server gets request
* If there are information in the cache then returns this<br>
 information otherwise send new request to API service
2. The cache filler function which logic is:
* Cache filler function takes list of locations from config
* Then it sends request for all of the cities from list
* Then it writes all of then to cache

## Building
1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack build`
3. Open `Weather-server.exe` file

## Configurating
After the `Weather-server.exe` file was launched for the first time, you must write the API key to the console<br>
and the configuration file will be generated. After that, you can manually change the settings in the `config.yaml` file.

There are next options that you can change:
```yaml
serverPort: 8080
apikey: yourkey
# time intervals between cache fillings (10^-6 seconds)
cacheFillingsInterval: 90000000
# maximum deviation between the cached time value of the weather forecast
# and the value of the time request parameter (seconds)
maxDeviation: 3600
locations:
- Rostov-on-Don
- Moscow
- Saint Petersburg
rootAPI: weatherapi
```
