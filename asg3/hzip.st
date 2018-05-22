#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"RCSID := '$Id: hzip.st,v 1.10 2012-02-23 17:45:49-08 - - $'."

""
"Annie Shen (ashen7)"
"CMPS112 - Homework 3 - smalltalk hzip.st"
""

execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.


"Example from class course page - charfreq.st"
"Create ZeroArray Object"
"Will be used for frequency table & count frequency of input characters"
Object subclass: ZeroArry [
    "The main returning array of subclass"
    |theArray|

    ZeroArray class >> new [
        self shouldNotImplement.
    ]

    ZeroArray class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]

    "Create a new type Array with size of size"
    init: size [
        theArray := Array new: size.
    ]

    size [
       ^ theArray size.
    ]

    "The next location of index"
    at: index [
       ^ theArray at: index + 1.
    ]

    "The next location of index, but plus inserting a value"
    at: index put: value [
       ^ theArray at: index + 1 put: value.
    ]

    incr: index [
        "If the current index is NULL, then put a 0 in that index"
        (self at: index) isNil
            ifTrue: [ self at: index put: 0].
        self at: index put: (self at: index) + 1.
    ]


    keysAndValuesDo: aBlock [
       (0 to: (self size) - 1) do: [:index |
          aBlock value: index value: (self at: index).
       ]
    ]
]


"**************************************************************************"
"---------------------------------- Main ----------------------------------"

cmdline_opt := (Smalltalk getArgv: 1). "Start at 1, not 0 like most."
in_name := (Smalltalk getArgv: 2).
out_name := (Smalltalk getArgv: 3).

"If argument < 2, return error msg and exit the running program"
Smalltalk arguments < 2
ifTrue: [
    stderr << execname << 'ERROR: lacking command line arguments.' << nl.
    ObjectMemory quit.
]



"If the command line option is -t"
cmdline_opt =~ 't' ifMatched: [
freqtable := ZeroArray new: 256.
    [
        |input_file|
        "Reading the original file"
        input_file := FileStream open: in_name mode: FileStream read.
        input_file loadCharFreq: freqtable.
        "Close input_file after first time done reading"
        input_file close.

    "In terms of error in reading input_file, flush the program and exit"
    ] on: SystemExceptions.FileError do: [:signal |
        stdout flush.
        stderr << execname << 'ERROR: input file file error.' << nl.
        stderr flush.
    ].

    "Take sort_queue and turn into encoded table"
    "SortedCollection from example code in class folder"
    sort_queue := SortedCollection new.
    freqtable keysAndValuesDo:
].

"If the command line option is -c"
cmdline_opt =~ 'c' ifMatched: [


]
"If the command line option is -u"
cmdline_opt =~ 'u' ifMatched: [


]

stdout << 'Hello, World!' << nl << RCSID << nl.

status := 127.
stderr << execname << ': ' << 'Quitting with status ' << status << nl.
ObjectMemory quit: status.

"TEST: hzip.st"
