NOTE: This documentation has been put here because I couldn't find it anywhere else. I am not associated with Crunchbase in any way. I cannot help you with your Crunchbase API problems. If you need help try here: https://groups.google.com/forum/#!forum/crunchbase-api

CrunchBase API v1 Documentation
===============================

Overview
--------

The CrunchBase API provides [JSON](http://json.org/ "JSON") representations of the data found on CrunchBase.  The API currently supports three actions: "show", "search", and "list", which are each described below.

The `v/1/` components of the URLs below refer to the current version of the API, which is 1.

Show Entity
-----------

To retrieve information about a specific entity on CrunchBase, use a URL of the form:
 
    http://api.crunchbase.com/v/1/<namespace>/<permalink>.js
 
The available namespaces are:

* `company`
* `person`
* `financial-organization`
* `product`
* `service-provider`

`<permalink>` referrers to an entity's permalink as seen in the URL for its regular CrunchBase page.
 
Also, you can append a `callback` query param to have the result passed to a callback method of your choice.

### Examples

    api.crunchbase.com/v/1/company/facebook.js
    api.crunchbase.com/v/1/person/brad-fitzpatrick.js
    api.crunchbase.com/v/1/financial-organization/accel-partners.js?callback=callme

Search Queries
--------------

To retrieve a list entities that match a given search query, use URLs of the form:
 
    http://api.crunchbase.com/v/1/search.js?query=<keyword(s)>

The result is a hash containing the first 10 results:

    {"total": 1,
     "page": 1,
     "results":
      [{"name": "Speckly",
        "permalink": "speckly",
        "namespace": "company",
        "overview": "Speckly's mission is to make the process of finding media delivered through [BitTorrent](http://www.crunchbase.com/company/bittorrent) easy for everyone. Part of that equation is making BitTorrent searches relevant, fast, and easy to understand. ",
        "image":
         {"available_sizes":
           [[[150,
              61],
             "assets/images/resized/0002/2217/22217v1-max-150x150.png"],
            [[156,
              64],
             "assets/images/resized/0002/2217/22217v1-max-250x250.png"],
            [[156,
              64],
             "assets/images/resized/0002/2217/22217v1-max-450x450.png"]],
          "attribution": null}}]}
 
Note the `total` and `page` attributes of the hash. Total refers to the total number of results available for the given query.
 
To retrieve more than the first 10 results append the page parameter to your query. This request will retrieve results 11-20 for the query 'iphone':

    http://api.crunchbase.com/v/1/search.js?query=iphone&page=2

Also, you can append a `callback` query parameter to have the result passed to a callback method of you choice.
 
Note that this action may be useful if you want to "search by domain", as the search engine considers the entity homepage URLs when generating results. 

### Examples 

    api.crunchbase.com/v/1/search.js?query=techcrunch
    api.crunchbase.com/v/1/search.js?query=google.com&page=3
    api.crunchbase.com/v/1/search.js?query=reddit&callback=callme

List Entities
-------------

To retrieve a list of all of the entities in a certain namespace on CrunchBase, use a URL of the form:
 
    http://api.crunchbase.com/v/1/<plural-namespace>
 
The plural available namespaces are:

* `companies`
* `people`
* `financial-organizations`
* `products`
* `service-providers`

This action does not support JavaScript callbacks.

### Examples

    api.crunchbase.com/v/1/companies.js
    api.crunchbase.com/v/1/financial-organizations.js

Permalink Entity
----------------

Ever needed to find a corresponding CrunchBase page for a particular Company/Person/Financial Org name? Well, we now have an API for that which is easier to use than our Search API. 

### Syntax

For all entities except people:

    http://api.crunchbase.com/v/1/<plural entity namespace>/permalink?name=<entity name>

For people:

    http://api.crunchbase.com/v/1/people/permalink?first_name=<person first name>&last_name=<person last name>

### Examples

    http://api.crunchbase.com/v/1/companies/permalink?name=Google
    http://api.crunchbase.com/v/1/financial-organizations/permalink?name=Sequoia%20Capital
    http://api.crunchbase.com/v/1/products/permalink?name=iPhone
    http://api.crunchbase.com/v/1/people/permalink?first_name=Ron&last_name=Conway

### Returns

If the permalink is found, JSON will be returned in the following format (this one for companies):

    {"name": "Google", "crunchbase_url": "http://www.crunchbase.com/company/google", "permalink": "google"}

If a permalink is not found, the HTTP status code will be 404 and the JSON return will be (note that this is a 404 for companies):

    {"error": "Unknown company. Please see www.crunchbase.com/help/api for help."}

### Notes

The entity name (or for people, first and last name) is case-insensitive. Replace any spaces in the entity name with `%20`.

TechCrunch Posts API
--------------------

Want data on how many times a particular company has been written about on TechCrunch? Well, you can do that now as well. The syntax is very similar to the Permalink API, above. You start with an entity name.

### Syntax

For all entities except people:

    http://api.crunchbase.com/v/1/<plural entity namespace>/posts?name=<entity name>

For people:

    http://api.crunchbase.com/v/1/people/posts?first_name=<person first name>&last_name=<person last name>

### Examples:

    http://api.crunchbase.com/v/1/companies/posts?name=Google
    http://api.crunchbase.com/v/1/financial-organizations/posts?name=Sequoia%20Capital
    http://api.crunchbase.com/v/1/products/posts?name=iPhone
    http://api.crunchbase.com/v/1/people/posts?first_name=Ron&last_name=Conway

If any posts are found, JSON will be returned in the following format (this one for companies):

    {"posts_url": "http://www.crunchbase.com/company/google/posts", 
     "num_posts": 1174, 
     "name": "Google", 
     "crunchbase_url": "http://www.crunchbase.com/company/google", 
     "permalink": "google"}

If no posts are found, the HTTP status code will be 404 and the JSON return will be (note that this is a 404 for companies):

    {"error": "Unknown company. Please see www.crunchbase.com/help/api for help."}

## Notes

The entity name (or for people, first and last name) is case-insensitive. Replace any spaces in the entity name with `%20`.

Attribution
-----------

Please see the [Attribution](http://groups.google.com/group/crunchbase-api/web/attribution "Attribution") page, where we explain the requirements.

Sightings
---------

We get really excited when we see CrunchBase data being used in the wild. [See what's already been built](http://groups.google.com/group/crunchbase-api/web/sightings "See what's already been built") and [let us know](http://www.crunchbase.com/feedback/new "let us know") if you come across something new.