#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"RCSID := '$Id: hzip.st,v 1.10 2012-02-23 17:45:49-08 - - $'."

""
"Annie Shen (ashen7)"
"CMPS112 - Homework 3 - smalltalk hzip.st"
""


execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.


"**************************************************************************"
"------------------------------- File stuff -------------------------------"

"File load and read."
FileStream extend [
    |bitnum bitbuf|

    clearByte [
        bitnum := 8.
        bitbuf := 0.
    ]

    flushByte [
        |char|
        bitnum = 8 ifFalse: [
            char := Character value: bitbuf asInteger.
            self nextPut: char.
            self clearByte.
        ]
    ]

    writeBit: bit [
        bitbuf isNil ifTrue: [self clearByte].
        bitnum = 0 ifTrue: [self flushByte].
        bitbuf := bitbuf bitAt: bitnum put: bit.
        bitnum := bitnum - 1.
    ]

    "Reset the value of bitnum to 0"
    clearBits [
        bitnum := 0.
    ]


    fillByte [
        bitbuf := self next.
        bitnum := 0.
    ]

    "Go to next bit"
    nextBit [
        |b|
        bitnum isNil ifTrue: [self clearBits].
        bitnum = 0 ifTrue: [self fillBytes].
        bitbuf isNil ifTrue: [ ^ nil ].
        b := bitbuf value bitAt: bitnum.
        bitnum := bitnum - 1.
        ^ b.
    ]

    bitChar: char [
        |bit char_val bit_idx|

        char_val := char value.
        bit_idx := 8.
        [bit_idx ~= 0] whileTrue: [
            bit := char_val bitAt: bit_idx.
            bit_idx := bit_idx - 1.
            self writeBit: bit.
        ].

        (char_val = 0) | (char_val = 256) ifTrue: [
            self writeBit: (char_val bitAt: 9).
        ].
    ]

    loadCharFreq: freq [
        |char|

        [self atEnd not] whileTrue: [
            char := self next asInteger.
            freq incr: char.
        ].
        char = nil ifFalse: [
            freq incr: 256.
        ].
    ]

    loadTree [
        |stack pre_bit|

        stack := Stack new: 256.
        [self atEnd not] whileTrue: [
            pre_bit := self nextBit.
            pre_bit = 0 ifTrue: [
                |bit bit_char bit_idx leaf|

                bit_idx := 8.
                bit_char := 0.
                [bit_idx ~= 0] whileTrue: [
                    bit := self nextBit.
                    bit_char := bit_char bitAt: bit_idx put: bit.
                    bit_idx := bit_idx - 1.
                ].

                leaf := Leaf new: (bit_char asCharacter) count: 0.
                bit_char = 0 ifTrue: [
                    bit := self nextBit.
                    bit = 1 ifTrue: [
                        leaf := Leaf new: 256 count: 0.
                    ].
                ].

                stack push: leaf.
            ].

            pre_bit = 1 ifTrue: [
                |left right tree|

                right := stack pop.
                stack size = 0 ifTrue: [
                    self clearBits.
                    ^ right
                ].

                left := stack pop.
                tree := Tree new: left char count: 0 left: left right: right.
                stack push: tree.
            ].
        ].
    ]

    compress: output_file with: code [
        |key encode bit|

        [self atEnd not] whileTrue: [
            key := self next value.
            encode := code at: key.
            encode do: [:char |
                bit := char asBit.
                output_file writeBit: bit.
            ].
        ].

        encode := code at: 256.
        encode do: [:char |
            bit := char asBit.
            output_file writeBit: bit.
        ].
        output_file flushByte.
    ]


    decompress: output_file with: code [
        |bit tree|

        tree := code.
        bit := 0.
        [bit ~= nil] whileTrue: [
            bit := self nextBit.
            "If the bit == 0, go towards left"
            bit = 0 ifTrue: [
                tree := tree left.
            ].
            "If the bit == 1, go towards right"
            bit = 1 ifTrue: [
                tree := tree right.
            ].

            (tree isMemberOf: Leaf) ifTrue: [
                |char|
                tree char = 256 ifTrue: [ ^ char ].
                char := Character value: tree char asInteger.
                output_file nextPut: char.
                tree := code.
            ].
        ].
    ]
]


"Extend character to handle printing with ASCII codes"
Character extend [
    asBit [
        ^ self value bitAt: 1.
    ]

    isPrint [
        ^ (Character space < self) & (self <= $~)
    ]

    digits [
        ^ (self asInteger) > 15
    ]

    visible [
       self isPrint ifTrue: [^' ', self asString, ' ']
                    ifFalse: [ self digits
                        ifTrue: [((self asInteger) = 256)
                            ifTrue: [ ^ 'EOF' ]
                            ifFalse: [ ^ 'x', ((seld asInteger printStringRadix: 16) cpoyFrom: 4 )]
                        ]
                        ifFalse: [
                            ^ 'x0', ((self asInteger printStringRadix: 16) copyFrom: 4)
                        ]
                    ]
    ]
]


"**************************************************************************"
"----------------------- Stack & Array & Tree & Leaf ----------------------"

"Create Stack Object"
Object subclass: Stack [
    |theArray top item|

    Stack class >> new [
        self shouldNotImplement.
    ]

    Stack class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]

    init: size [
        top := 0.
        theArray := Array new: size.
    ]

    size [
        ^ top.
    ]

    "Take out and remove the item that's on the top of the stack.
     No need to specify which item."
    pop [
        top = 0 ifTrue: [
            stderr << execname << ': ERROR: pop - overflowing stack.' << nl.
            ^ nil
        ].
        item := theArray at: top.
        top := top - 1.
        ^ item
    ]

    "Insert a  specified item into stack"
    push: item [
        top := top + 1.
        top > self size ifTrue: [
            ^ stderr << execname << ': ERROR: push - overflowing stack.' << nl.
        ].
        theArray at: top put: item.
    ]
]


"Example code from class course page - charfreq.st
 Create ZeroArray Object
 Will be used for frequency table & count frequency of input characters"
Object subclass: ZeroArray [
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


"Example code from class course page - tree.st
 Create Leaf Object"
Object subclass: Leaf [
    |char count|
    char [ ^ char ]
    count [ ^ count ]

    Leaf class >> new [
        self shouldNotImplement
    ]

    Leaf class >> new: aChar count: aCount [
        |result|
        result := super new.
        result setChar: aChar andCount: aCount.
        ^result
    ]

    setChar: aChar andCount: aCount [
        char := aChar.
        count := aCount.
    ]

    <= other [
        ^ (count < other count)
        | ((count = other count) & (char <= other char))
    ]

    printBase: aStream [
        ^ aStream << self class << '(' << char << ',' << count
    ]

    printOn: aStream [
        (self printBase: aStream) << ')'.
    ]

    inorder: visitor prefix: string [
        visitor value: char value: string.
    ]

    "New - to be able to write to file"
    writeIn: outFile [
        outFile writeBit: (0 bitAt: 1).
        outFile bitChar: self char.
    ]
]

"Example code from class course page - tree.st
 Create Tree Object"
Leaf subclass: Tree [
   |left right|
   left [ ^ left ]
   right [ ^ right ]

    Tree class >> new: aChar count: aCount [
        self shouldNotImplement
    ]

    Tree class >> new: aChar count: aCount left: aLeft right: aRight [
        |result|
        result := super new: aChar count: aCount.
        result setLeft: aLeft andRight: aRight.
        ^ result
    ]

    setLeft: aLeft andRight: aRight [
        left := aLeft.
        right := aRight.
    ]

    printOn: aStream [
        (self printBase: aStream) << ',' << left << ',' << right << ')'.
    ]

    inorder: visitor prefix: string [
        left inorder: visitor prefix: string, '0'.
        right inorder: visitor prefix: string, '1'.
    ]

    "New - to be able to write to file"
    writeIn: outFile [
        left writeIn: outFile.
        right writeIn: outFile.
        outFile writeBit: (1 bitAt: 1).
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
    stderr << execname << ': ERROR: lacking command line arguments.' << nl.
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
        stderr << execname << ': ERROR: input file file error.' << nl.
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
