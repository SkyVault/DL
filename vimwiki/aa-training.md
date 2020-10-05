# AA Training

## First Session

### Building blocks of a degree planner report

Process Request

DP Progress report

left req groups - right requirements, view courses / req line

REQUIREMENT LINE

Course list
    single course/group or wild card

Requirements
    effective date = catalog term
    academic structure - for institutions and coders
    description fields - only report descr / long descr will display for student
    hide-status - turn off for limit req

Parameters
    print controll values - many unis skip printing to midigate text on the report
    req level and req line level

Line Item

Line Item Parameters
    display select line - indicates if courses can be selected

    Credit include mode - Include all stats = 100 line would not auto fall down into line
    4 without partition sharing found in parameters

    Sharing - course can satisfy multiple lines set as all stats lines in credit include mode

Line Item Detail
    Where the course list is attached
    Also if course list is a wild card, you can add parameters and say certain course lists shouldn't be included
    List Include Mode - N to say no
    Use connection types - Union / Intersect / Add / Sub

    Follows the Line Detail sequence number

Career Requirements
    Define when a group of requirements takes place,
    If someone starts a program later then someone else and his requirements are different, it wont effect the previous person who started the same program earlier

Pre-Condition Rules
    used at the req and line item req level for more complex coding
    So like minimum 2.00 to graduate but honer students need 3.00, so you can turn off lines using pre-conditions

    used to create exceptions

    Can turn on and off lines/groups etc using conditions

    Single translate
        Only use EQUAL or IN

    PluralTranslate Values
        Must use NOT EQUAL or NOT IN

Dynamic Condition
    Multiple pre-conditions using more than one connector type (or/and) etc...

    milestones:
        Thesis
        Portfolio
        Final Project
        Etc...

## Session 2

### TODO

* Use GP per Unit
* Effective Date for all requirements
* Path Planner DDoS
* Create ticket for that UI broken line glitch

Query Manager (Reporting Tools > Query > ...)
    Layout follows the request advicement report
    Can write queries using KEYPLN Grade etc...
    Not going to see the lines

    Selection Proc type

    Mode N or S
        N: Course could've been used, but not
        S: Was selected to satisfy a req

HOMEWORK:
    Path Planner for Political Sci Tech or another new one
