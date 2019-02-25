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
        bitnum := 8.
    ]

    "Go to next bit"
    nextBit [
        |bit|
        bitnum isNil ifTrue: [self clearBits].
        bitnum = 0 ifTrue: [self fillByte].
        bitbuf isNil ifTrue: [ ^ nil ].
        bit := bitbuf val bitAt: bitnum.
        bitnum := bitnum - 1.
        ^ bit.
    ]

    bitChar: char [
        |bit charval bitidx|

        charval := char value.
        bitidx := 8.
        [bitidx ~= 0] whileTrue: [
            bit := charval bitAt: bitidx.
            bitidx := bitidx - 1.
            self writeBit: bit.
        ].

        (charval = 0) | (charval = 256) ifTrue: [
            self writeBit: (charval bitAt: 9).
        ].
    ]

    loadCharFreq: freqtable [
        |char|

        [self atEnd not] whileTrue: [
            char := self next asInteger.
            freqtable incr: char.
        ].
        char = nil ifFalse: [
            freqtable incr: 256.
        ].
    ]

    loadTree [
        |stack preBit|
        stack := Stack new: 256.
        [self atEnd not] whileTrue: [
            preBit := self nextBit.
            preBit = 0 ifTrue: [
                |bit bitchar bitidx leaf|
                bitchar := 0.
                bitidx := 8.
                [bitidx ~= 0] whileTrue: [
                    bit := self nextBit.
                    bitchar := bitchar bitAt: bitidx put: bit.
                    bitidx := bitidx - 1.
                ].
                leaf := Leaf new: (bitchar asCharacter) count: 0.
                bitchar = 0 ifTrue: [
                    bit := self nextBit.
                    bit = 1 ifTrue: [
                        leaf := Leaf new: 256 count: 0.
                    ].
                ].
                stack push: leaf.
            ].

            preBit = 1 ifTrue: [
                |leftT rightT tree|

                rightT := stack pop.
                stack size = 0 ifTrue: [
                    self clearBits.
                    ^ rightT
                ].
                leftT := stack pop.
                tree := Tree new: leftT char
                count: 0 left: leftT right: rightT.
                stack push: tree.
            ].
        ].
    ]

    compress: outfile with: code [
        |key encode bit|

        [self atEnd not] whileTrue: [
            key := self next value.
            encode := code at: key.
            encode do: [:char |
                bit := char asBit.
                outfile writeBit: bit.
            ].
        ].
        encode := code at: 256.
        encode do: [:char |
            bit := char asBit.
            outfile writeBit: bit.
        ].
        outfile flushByte.
    ]

    decompress: outfile with: tree [
        |bit ctree|
        ctree := tree.
        bit := 0.
        [bit ~= nil] whileTrue: [
            bit := self nextBit.
            bit = 0 ifTrue: [
                ctree := tree left.
            ].
            bit = 1 ifTrue: [
                ctree := tree right.
            ].
            (ctree isMemberOf: Leaf) ifTrue: [
                |char|
                ctree char = 256 ifTrue: [ ^ char ].
                char := Character value: ctree char asInteger.
                outfile nextPut: char.
                ctree := tree.
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
                            ifFalse: [ ^ 'x', ((self asInteger printStringRadix: 16) copyFrom: 4 )]
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

    nl := Character nl.
    "Take out and remove the item that's on the top of the stack.
     No need to specify which item."
    pop [
        top = 0 ifTrue: [
            stderr << 'ERROR: pop - overflowing stack.' << nl.
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
            ^ stderr << 'ERROR: push - overflowing stack.' << nl.
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
        (self at: index) isNil ifTrue: [ self at: index put: 0].
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

    "Not sure what this does???!"
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
    writeIn: outfile [
        outfile writeBit: (0 bitAt: 1).
        outfile bitChar: self char.
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
    writeIn: outfile [
        left writeIn: outfile.
        right writeIn: outfile.
        outfile writeBit: (1 bitAt: 1).
    ]
]


"**************************************************************************"
"---------------------------------- Main ----------------------------------"
nl := Character nl.
cmdline := (Smalltalk getArgv: 1). "Start at 1, not 0 like most."
inputFileName := (Smalltalk getArgv: 2).
outputFileName := (Smalltalk getArgv: 3).

"If argument < 2, return error msg and exit the running program"
Smalltalk arguments < 2
ifTrue: [
    stderr << 'ERROR: lacking command line arguments.' << nl.
    ObjectMemory quit.
]

"If the command line option is -t OR -c"
cmdline =~ 't|c' ifMatched: [
freqtable := ZeroArray new: 257.
    [
        |infile|
        "Reading the original file"
        infile := FileStream open: inputFileName mode: FileStream read.
        infile loadCharFreq: freqtable.
        "Close input_file after first time done reading"
        infile close.

    "In terms of error in reading input_file, flush the program and exit"
    ] on: SystemExceptions.FileError do: [:signal |
        |errno|
        errno := File errno.
        stdout flush.
        stderr << 'ERROR: input file file error. ' << signal messageText << nl.
        stderr flush.
    ].
].

"Take care of queue and encoding table business
 Take queue and turn into encoded table
 SortedCollection from example code in class folder"
cmdline =~ 't|c' ifMatched: [
    pqueue := SortedCollection new.
    freqtable keysAndValuesDo: [:key :val |
        (val notNil and: [val > 0]) ifTrue: [
            newLeaf := Leaf new: (key asCharacter) count: val.
            pqueue add: newLeaf.
        ]
    ].
    "If user also wants to show debug table with -d, check here and print"
    cmdline =~ 'd' ifMatched: [
        pqueue inspect.
    ].
].


"Take care of queue and encoding tree business
 Take queue and turn into encoded Huffman tree
 SortedCollection from example code in class folder"
cmdline =~ 't|c' ifMatched: [
    [pqueue notEmpty] whileTrue: [
        leftT := pqueue removeFirst.
        pqueue size = 0 ifTrue: [^ leftT].
        rightT := pqueue removeFirst.
        newTree := Tree new: leftT char count: (leftT count + rightT count) left: leftT right: rightT.
        pqueue add: newTree.
    ].
    "Print out in stdout the tree for debugging purposes"
    cmdline =~ 'd' ifMatched: [
        stdout << leftT << nl.
    ].
].

"Travel through the tree (formed from above)
 and turn into an array"
cmdline =~ 't|c' ifMatched: [
    code := ZeroArray new: 257.
    tree = nil ifFalse: [
        tree inorder: [:char :encoding |
            code at: char asInteger put: encoding.
        ] prefix: ''.
    ].
].

"For option -t, forming frequency table with all that cool number stuff!"
cmdline =~ 't' ifMatched: [
    freqtable keysAndValuesDo: [:key :val |
        (val notNil and: [val > 0]) ifTrue: [
            |character|
            character := (key asCharacter visible).
            character asString size to: 2 do: [:skip|
                stdout << ' '
            ].
            stdout << character << ' '.
            val asString size to: 5 do: [:skip|
                stdout << ' '
            ].
            stdout << val << ' '.
            stdout << (code at: key) << nl.
        ]
    ]
].


"For option -c, compressing the input_file!"
cmdline =~ 'c' ifMatched: [
    [
        |infile outfile|
        infile := FileStream open: inputFileName mode: FileStream read.
        filenameOut = nil ifTrue: [
            outfile := stdout.
        ]
        ifFalse: [
            outfile := FileStream open: outputFileName mode: FileStream write.
        ].

        tree = nil ifFalse: [
            tree writeIn: outfile.
            outfile writeBit: (1 bitAt: 1).
            outfile flushByte.
            infile compress: outfile with: code.
        ].
        outfile close.
        infile close.
    ] on: SystemExceptions.FileError do: [:signal |
        |errno|
        errno := File errno.
        stdout flush.
        stderr << ': ERROR :' << signal messageText << nl.
        stderr flush.
    ].
].


"Decompress infile > outfile."
cmdline =~ 'u' ifMatched: [
    [
        |infile outfile|
        infile := FileStream open: inputFileName mode: FileStream read.
        tree := infile loadTree.
        tree = nil ifFalse: [
            outputFileName = nil ifTrue: [
                infile decompress: stdout with: tree.
            ]
            ifFalse: [
                outfile := FileStream open: outputFileName mode: FileStream write.
                infile decompress: outfile with: tree.
                outfile close.
            ].
        ].
        infile close.

    ] on: SystemExceptions.FileError do: [:signal |
        |errno|
        errno := File errno.
        stdout flush.
        stderr << 'ERROR : ' << signal messageText << nl.
        stderr flush.
    ].
    cmdline =~ 'd' ifMatched: [stdout << tree << nl.].
].
