# DEVNULL Org

DEVNULL Org is a company specialized in computer programming.
We use Haskell programming language and offer consulting services that include:

 - writing production level code
 - code optimization
 - debugging and fixing problems with execution speed and/or large memory consumption
 
 Additionally we work on tools that should solve some of the pain points various companies experience in their daily operations.
 
## Products

### Fakie - The ultimate API glue!
------------

#### Merge multiple API endpoints into results you control

How does it work you might ask. 
Well, it all starts with a configuration file. All you need to know in order to control the fakie api server is some JSON.
Configuration file determines what api endpoints you want to call and how to you want to map the results you get to the results
you actually want. 

When you call some external json api endpoint the result is obviously or hopefully json. You might want to specify your own
field names instead of ones you get externally because it is convenient to have this kind of control.
You can also choose which field you would like to keep and which ones to omit. Also you could convert types of fields i.e. Array to Object or
vice versa as well as group the results into your own endpoint that serves the data in the way it is convenient to you.

So if you need a flexible tool for working with and merging multiple api calls into a single or multiple responses Fakie is the thing to reach for.
