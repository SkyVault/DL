# Package Evaluation Notes

PSDBC: wrapper around the JSDBC (java database)

psdbc/core:
    api layout interfaces
    core (people soft implementation)

    look at Connection class

    BINDS in sql basically a variable that is input
    STMT with out binds
    PREPARED STMT is with binds

    use executeQuery

    ResultSet, call next to get the first vall

    ```
    val resultSet = connection.statement.executeQuery("***", 2)
    while (resultSet.next()){ // iterates the rows
        resultSet.asInt(2 /* column */)
    }
    ```

psdbc/http:
    spoofs the

CLEAN:
    gateway = interface

use-case:

main is where everything gets tied together

IScript is a function that is defined in peoplesoft

moshi takes a class and turns it into json
