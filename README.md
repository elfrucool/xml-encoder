# XML ENCODE

Simple library for encoding xml and render it indented.

Use it when you have data and you want it to be converted to XML String

## USAGE

Given:

```elm
module MyModule where

import Html
import XmlNode as X

type alias Person =
    { name : String
    , age : Int
    , email : String
    , isAdmin : Bool
    }

personToXml : Person -> String
personToXml person =
    X.toString 0 <| -- 0 means indent starting from `0'
        X.element "Person"
            [ ("isAdmin", toString person.isAdmin) ]
            [ X.element "Name" [] [ X.text person.name ]
            , X.element "Age" [] [ X.text <| toString person.age ]
            , X.element "E-Mail" [] [ X.text person.email ]
            ]


-- somehwere in your code ...
    personToXml { name = "Alice", age = 20, email = "alice@example.com", isAdmin = True }
```

It will render:

```xml
<Person isAdmin="True">
  <Name>Alice</Name>
  <Age>20</Age>
  <E-Mail>alice@example.com</E-Mail>
</Person>
```
