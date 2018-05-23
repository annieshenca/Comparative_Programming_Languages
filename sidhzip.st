#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"Siddharth Verma"

execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.

"--==--==--==Tbh the important stuff happens here--==--==--"

"file reader, bit business and tree creation"
FileStream extend [
  |bitNumber bitBuffer|

  clearByte [
    bitNumber := 8.
    bitBuffer := 0.
  ]

  flushByte [
    |char|
    bitNumber = 8 ifFalse: [
      char := Character value: bitBuffer asInteger.
      self nextPut: char.
      self clearByte.
    ]
  ]

  writeBit: bit [
    bitBuffer isNil ifTrue: [self clearByte].
    bitNumber = 0 ifTrue: [self flushByte].
    bitBuffer := bitBuffer bitAt: bitNumber put: bit.
    bitNumber := bitNumber - 1.
  ]

  clearBits [
    bitNumber := 0.
  ]

  fillByte [
    bitBuffer := self next.
    bitNumber := 8.
  ]

  nextBit [
    |bit|
    bitNumber isNil ifTrue: [self clearBits].
    bitNumber = 0 ifTrue: [self fillByte].
    bitBuffer isNil ifTrue: [ ^ nil ].
    bit := bitBuffer value bitAt: bitNumber.
    bitNumber := bitNumber - 1.
    ^ bit.
  ]

  bitChar: char [
    |bit charVal bitIndex|
    charVal := char value.
    bitIndex := 8.
    [bitIndex ~= 0] whileTrue: [
      bit := charVal bitAt: bitIndex.
      bitIndex := bitIndex - 1.
      self writeBit: bit.
    ].
    (charVal = 0) | (charVal = 256) ifTrue: [
      self writeBit: (charVal bitAt: 9).
    ].
  ]

  loadCharFreq: freqtable [
    |ordChar|
    [self atEnd not] whileTrue: [
      ordChar := self next asInteger.
      freqtable incr: ordChar.
    ].
    ordChar = nil ifFalse: [
      freqtable incr: 256.
    ].
  ]

  loadTree [
    |stack preBit|
    stack := Stack new: 256.
    [self atEnd not] whileTrue: [
      preBit := self nextBit.
      preBit = 0 ifTrue: [
        |bit bitChar bitIndex newLeaf|
        bitChar := 0.
        bitIndex := 8.
        [bitIndex ~= 0] whileTrue: [
          bit := self nextBit.
          bitChar := bitChar bitAt: bitIndex put: bit.
          bitIndex := bitIndex - 1.
        ].
        newLeaf := Leaf new: (bitChar asCharacter) count: 0.
        bitChar = 0 ifTrue: [
          bit := self nextBit.
          bit = 1 ifTrue: [
            newLeaf := Leaf new: 256 count: 0.
          ].
        ].
        stack push: newLeaf.
      ].
      preBit = 1 ifTrue: [
        |ltree rtree newTree|
        rtree := stack pop.
        stack size = 0 ifTrue: [
          self clearBits.
          ^ rtree
        ].
        ltree := stack pop.
        newTree := Tree new: ltree char
        count: 0 left: ltree right: rtree.
        stack push: newTree.
      ].
    ].
  ]

  compressTo: outFile with: encodings [
    |key encode bit|
    [self atEnd not] whileTrue: [
      key := self next value.
      encode := encodings at: key.
      encode do: [:char |
        bit := char asBit.
        outFile writeBit: bit.
      ].
    ].
    encode := encodings at: 256.
    encode do: [:char |
      bit := char asBit.
      outFile writeBit: bit.
    ].
    outFile flushByte.
  ]

  deCompressTo: outFile with: tree [
    |bit currTree|
    currTree := tree.
    bit := 0.
    [bit ~= nil] whileTrue: [
      bit := self nextBit.
      bit = 0 ifTrue: [
        currTree := currTree left.
      ].
      bit = 1 ifTrue: [
        currTree := currTree right.
      ].
      (currTree isMemberOf: Leaf) ifTrue: [
        |char|
        currTree char = 256 ifTrue: [ ^ char ].
        char := Character value: currTree char asInteger.
        outFile nextPut: char.
        currTree := tree.
      ].
    ].
  ]
]

"extend character to handle printing wiht ascii codes"
Character extend [

  asBit [
    ^ self value bitAt: 1.
  ]

  isPrint [
    ^ (Character space < self) & (self <= $~)
  ]

  digits[
         ^ (self asInteger) > 15
         ]

  visible [
      self isPrint ifTrue: [^' ', self asString, ' ']
                   ifFalse: [ self digits
                      ifTrue:[((self asInteger) = 256)
                              ifTrue:[ ^ 'EOF']
                              ifFalse:[^ 'x',
                              ((self asInteger printStringRadix: 16)
                               copyFrom: 4 )]
                             ]
                      ifFalse:[

                             ^ 'x0',
                              ((self asInteger printStringRadix: 16)
                               copyFrom: 4)]
                     ]
  ]
]


"Create stack object"
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
  pop [
    top = 0 ifTrue: [
      stderr << 'stack underflow' << nl.
      ^ nil
    ].
    item := theArray at: top.
    top := top - 1.
    ^ item
  ]

  push: item [
    top := top + 1.
    top > self size ifTrue: [
      ^ stderr << 'stack overflow' << nl.
    ].
    theArray at: top put: item.
  ]
]

"Create ZeroArray object - will use for frequency table.
 note: taken from charfreq.st from Mackey"
Object subclass: ZeroArray [
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

  init: size [
    theArray := Array new: size.
  ]

  size [
    ^ theArray size.
  ]

  at: index [
    ^ theArray at: index + 1.
  ]

  at: index put: value [
    ^ theArray at: index + 1 put: value.
  ]

  incr: index [
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

"--==--==--==--==--==--==--Begin Tree Related Business--==--==--==--==--"

"Leaf object - encoding tree"
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

  writeIn: outFile [
    outFile writeBit: (0 bitAt: 1).
    outFile bitChar: self char.
  ]
]

"Treee data structure"
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

  writeIn: outFile [
    left writeIn: outFile.
    right writeIn: outFile.
    outFile writeBit: (1 bitAt: 1).
  ]
]

"==--==--==--==--==--Begin argument parsing + handling--==--==--==--==--=="

"get cli arguments
why cant smalltalk just start at the zeroth index like everyone??"

cliOptions := (Smalltalk getArgv: 1).
filenameIn := (Smalltalk getArgv: 2).
filenameOut := (Smalltalk getArgv: 3).

"error handle + usage statements"
Smalltalk arguments size < 2
ifTrue: [
  stderr << 'Usage: ' << execname
  << ' -dtcu inputfile [outputfile]' << nl.
  ObjectMemory quit.
].

"Handle options t or c"
cliOptions =~ 't|c' ifMatched: [
freqtable := ZeroArray new: 257.
  [
    |inFile|
    inFile := FileStream open: filenameIn
              mode: FileStream read.
    inFile loadCharFreq: freqtable.
    inFile close.
  ] on: SystemExceptions.FileError do: [:signal |
    |errno|
    errno := File errno.
    stdout flush.
    stderr << execname << ': ' << filenameIn << ': '
        << signal messageText << nl.
    stderr flush.
  ].
].

"pqueue -> encoded table"
cliOptions =~ 't|c' ifMatched: [
  pqueue := SortedCollection new.
  freqtable keysAndValuesDo: [:key :value |
    (value notNil and: [value > 0]) ifTrue: [
      newLeaf := Leaf new: (key asCharacter) count: value.
      pqueue add: newLeaf.
    ]
  ].
  cliOptions =~ 'd' ifMatched: [pqueue inspect.].
].

"pqueue -> enocoded tree"
cliOptions =~ 't|c' ifMatched: [
  [pqueue notEmpty] whileTrue: [
    tree := pqueue removeFirst.
    pqueue size = 0 ifTrue: [^ tree].
    rtree := pqueue removeFirst.
    newTree := Tree new: tree char
    count: (tree count + rtree count)
    left: tree right: rtree.
    pqueue add: newTree.
  ].
  cliOptions =~ 'd' ifMatched: [stdout << tree << nl.].
].

"traverse tree -> array"
cliOptions =~ 't|c' ifMatched: [
  encodings := ZeroArray new: 257.
  tree = nil ifFalse: [
    tree inorder: [:char :encoding |
      encodings at: char asInteger put: encoding.
    ] prefix: ''.
  ].
].

"frequency table and encoded table"
cliOptions =~ 't' ifMatched: [
  freqtable keysAndValuesDo: [:key :value |
    (value notNil and: [value > 0]) ifTrue: [
       |character|
       character := (key asCharacter visible).
       character asString size to: 2 do: [:skip| stdout << ' '].
       stdout << character << ' '.
       value asString size to: 5 do: [:skip| stdout << ' '].
       stdout << value << ' '.
       stdout << (encodings at: key) << nl.
    ]
  ]
].

"Compress inFile"
cliOptions =~ 'c' ifMatched: [
  [
    |inFile outFile|
    inFile := FileStream open: filenameIn
              mode: FileStream read.
    filenameOut = nil
    ifTrue: [
      outFile := stdout.
    ]
    ifFalse: [
      outFile := FileStream open: filenameOut
                mode: FileStream write.
    ].
    tree = nil ifFalse: [
      tree writeIn: outFile.
      outFile writeBit: (1 bitAt: 1).
      outFile flushByte.
      inFile compressTo: outFile with: encodings.
    ].
    outFile close.
    inFile close.
  ] on: SystemExceptions.FileError do: [:signal |
    |errno|
    errno := File errno.
    stdout flush.
    stderr << execname << ': ' << filenameOut << ': '
        << signal messageText << nl.
    stderr flush.
  ].
].

"Decompress inFile > outFile."
cliOptions =~ 'u' ifMatched: [
  [
    |inFile outFile|
    inFile := FileStream open: filenameIn
              mode: FileStream read.
    tree := inFile loadTree.
    tree = nil ifFalse: [
      filenameOut = nil
      ifTrue: [
        inFile deCompressTo: stdout with: tree.
      ]
      ifFalse: [
        outFile := FileStream open: filenameOut
                  mode: FileStream write.
        inFile deCompressTo: outFile with: tree.
        outFile close.
      ].
    ].
    inFile close.
  ] on: SystemExceptions.FileError do: [:signal |
    |errno|
    errno := File errno.
    stdout flush.
    stderr << execname << ': ' << filenameIn << ': '
        << signal messageText << nl.
    stderr flush.
  ].
  cliOptions =~ 'd' ifMatched: [stdout << tree << nl.].
].
