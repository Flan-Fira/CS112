#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.

"catfile.st"
FileStream extend [
    nl := Character nl.
    catFile [
        |count line|
        count := 0.
        [self atEnd not] whileTrue: [
            count := count + 1.
            count asString size to: 5 do: [:skip| stdout << ' '].
            stdout << count << ': ' << self nextLine << nl.
        ]
    ]
]


Character extend [
    isPrint [
        ^ (Character space < self) & (self <= $~)
    ]

    digits[
        ^ (self asInteger) > 15
    ]

    isVisible [
        self isPrint
            ifTrue: [^' ', self asString, ' ']
            ifFalse: [self digits
                ifTrue: [((self asInteger) = 256)
                    ifTrue: [ ^ 'EOF'] ifFalse:[^ 'x',
                        ((self asInteger printStringRadix: 16)
                        copyFrom: 4 )
                    ]
                ]
                ifFalse:[ ^ 'x0',
                    ((self asInteger printStringRadix: 16)
                    copyFrom: 4)]
                ]
    ]
]

"outbits.st"
Object subclass: OutBits [
    |bitIndex currentByte myStream|
    OutBits class >> new [
        self shouldNotImplement.
    ]
    OutBits class >> new: fileStream [
        |result|
        result := super new.
        result init: fileStream.
        ^result
    ]
    clearByte [
        bitIndex := 8.
        currentByte := 0.
    ]
    init: fileStream [
        myStream := fileStream.
        self clearByte.
    ]
    flushByte [
        myStream nextPutByte: currentByte.
        self clearByte.
    ]
    writeBit: bit [
        currentByte := currentByte bitAt: bitIndex put: bit.
        bitIndex := bitIndex - 1.
        bitIndex = 0 ifTrue: [self flushByte].
    ]
]

"bitstream.st"
Object subclass: BitStream [
    |bitIndex byte myStream|

    BitStream class >> new: fileStream [
        |result|
        result := super new.
        result init: fileStream.
        ^result
    ]

    init: fileStream [
        myStream := fileStream.
        bitIndex := 1.
    ]

    nextBit [
        bitIndex = 1 ifTrue: [
            byte := myStream next.
            bitIndex := 9.
        ].
        bitIndex := bitIndex - 1.
        ^byte value bitAt: bitIndex
    ]

    atEnd [
        ^bitIndex = 1 and: [myStream atEnd]
    ]
]

"charfreq.st"
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

"tree.st"
Object subclass: Leaf [
    |char count isLeaf|
    char [ ^ char ]
    count [ ^ count ]
    isLeaf [^ isLeaf]

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
        isLeaf := true.
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

    postorder: visitor [
        visitor value: char value: isLeaf.
    ]

]

"tree.st"
Leaf subclass: Tree [
    |left right|

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
        isLeaf := false.
    ]

    left  [ ^ left]
    right [ ^ right]

    printOn: aStream [
        (self printBase: aStream) << ',' << left << ',' << right << ')'.
    ]

    inorder: visitor prefix: string [
        left inorder: visitor prefix: string, '0'.
        right inorder: visitor prefix: string, '1'.
    ]

    postorder: visitor [
        left postorder: visitor .
        right postorder: visitor .
        visitor value: char value: isLeaf.
    ]

]




queue := SortedCollection new.
|decodingTree exitStatus|

freqtable := ZeroArray new: 257.
encodetable := ZeroArray new: 257.
freqtable at:256 put:1.

|inFile inFile2 outFile argumentCount|

argumentCount := 0.

optionD := false.
optionT := false.
optionC := false.
optionU := false.

Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |

    "options"
    (opt = $d) ifTrue: [ optionD := true. ].
    (opt = $t) ifTrue: [ optionT := true. ].
    (opt = $c) ifTrue: [ optionC := true. ].
    (opt = $u) ifTrue: [ optionU := true. ].

    (arg ~= nil) ifTrue: [
        argumentCount = 0
        ifTrue: [
            optionD ifTrue: [
                stdout << 'input filename: ' << arg <<nl.
            ].

            inFile := FileStream open: arg
                mode: FileStream read.
            inFile2 := FileStream open: arg
                mode: FileStream read.
        ]
        ifFalse: [
            optionD ifTrue: [
                stdout << 'output filename: ' << arg <<nl.
            ].
            outFile := FileStream open: arg
                mode: FileStream write.].

        argumentCount := argumentCount + 1.
    ].
].

optionU ifFalse: [
    "setup frequency table"
    [inFile atEnd not] whileTrue: [
        |ordChar|
        ordChar := inFile next asInteger.
        freqtable incr: ordChar.
    ].

    freqtable keysAndValuesDo: [:key :count |
        (count notNil and: [count > 0]) ifTrue: [
            queue add: (Leaf new: key count: count).
        ]
    ].

    [queue size > 1] whileTrue: [
        |leftchild rightchild|

        leftchild := queue removeFirst.
        rightchild := queue removeFirst.
        newtree := (Tree new: (leftchild char)
                    count: (leftchild count) +  (rightchild count)
                    left: leftchild right: rightchild).
        queue add: newtree.
    ].

    decodingTree := queue removeFirst.

    decodingTree inorder: [:char :string |
        encodetable at: char put: string.
    ] prefix: ''.

    binaryTree := ''.
    decodingTree postorder: [:char :isLeaf |
        isLeaf ifFalse: [binaryTree:=binaryTree,'1'.]
               ifTrue: [
                   bin := ''.
                   char = 256
                   ifTrue: [bin := '000000001']
                   ifFalse: [
                       char = 0
                       ifTrue: [bin := '000000000']
                       ifFalse: [
                           8 to:1 by: -1 do:[:i|
                               bin := bin,((char bitAt:i) printString).
                           ]
                       ].
                   ].
                   binaryTree := (binaryTree, '0', bin).
               ].
    ].

    optionT ifTrue: [
        encodetable keysAndValuesDo: [:key :count |
            (count notNil and: [count > 0]) ifTrue: [
                stdout <<  key asCharacter isVisible << '     '
                       << (freqtable at: key) << '    ' << count
                       << nl.
            ].
        ].
    ].

    optionC ifTrue: [
        outFile ~= nil ifTrue: [
            optionD ifTrue: [
            stdout << nl << 'optionC = '<<optionC
                   << '  output file = true' << nl.].

            "dump binary decoding tree"
            outBytes := OutBits new: outFile.
            1 to: (binaryTree size) by: 1 do: [:index |
                bit := ((binaryTree at: index) asInteger) - 48.
                outBytes writeBit: bit.
            ].

            outBytes writeBit: 1.

            [inFile2 atEnd not] whileTrue: [
                |ordChar2|
                ordChar2 := inFile2 next asInteger.
                code := (encodetable at: ordChar2).
                1 to: (code size) by: 1 do: [:index |
                    bit := ((code at: index) asInteger) - 48.
                    outBytes writeBit: bit.
                ].
            ].

            eof := (encodetable at: 256).
            1 to: (eof size) by: 1 do: [:index |
                bit := ((eof at: index) asInteger) - 48.
                outBytes writeBit: bit.].
            outBytes flushByte.
        ]

        ifFalse:[
            optionD ifTrue:[
                stdout << nl << 'optionC = ' << optionC
                       <<' no output file' << nl.].

                stdout << nl
                       << '----------encoded input in binary----------'
                       << nl.

                stdout << binaryTree << 1.

                [inFile2 atEnd not] whileTrue: [
                    |ordChar2|
                    ordChar2 := inFile2 next asInteger.
                    code := (encodetable at: ordChar2).
                    stdout <<code.
                ].
                eof := (encodetable at: 256).
                stdout << eof.
        ].
    ].
].

(optionU) ifTrue: [
    |outBytes isEOF stack|
    isEOF := false.

    optionD ifTrue: [ stdout << 'decompressing' << nl.].

    outBytes := OutBits new: outFile.
    stack := OrderedCollection new.
    fileBits := BitStream new: inFile.
    tree := false.
    newTree := ''.
    node := ''.

    [(fileBits atEnd not) & (isEOF not)] whileTrue: [
        treeNode := fileBits nextBit.
        leafChar := 0.

        tree ifFalse: [
            "create a leaf for input code"
            treeNode =0 ifTrue: [

                8 to:1 by:-1 do:[:i|
                    bit:=fileBits nextBit.
                    leafChar:=leafChar bitAt:i put:bit.
                ].

                leafChar = 0 ifTrue: [
                    specialbit := fileBits nextBit.
                    specialbit = 1 ifTrue: [
                        leafChar:=leafChar bitAt:9 put:1.
                    ].
                ].
            stack add: (Leaf new: leafChar count: '').
            ].

            "create tree"
            treeNode = 1 ifTrue: [
                stack size > 1 ifTrue: [
                    rightchild := stack removeLast.
                    leftchild := stack removeLast.

                    newtree := (Tree new: -1 count: ''
                                left: leftchild
                                right: rightchild).
                    stack add: newtree.
                ] ifFalse: [
                    tree := true.
                    newTree := stack removeLast.
                    node := newTree.
                ].
            ].
        ]

        ifTrue: [
            node isLeaf ifTrue: [
                ((node char) = 256) ifTrue: [
                    isEOF := true.
                ] ifFalse: [
                    stdout nextPut: (Character value: (node char)).
                    node:=newTree.].
            ] ifFalse: [
                treeNode = 0 ifTrue: [
                node := node left.

                node isLeaf ifTrue: [
                    isEOF not ifTrue: [
                        ((node char)=256) ifTrue: [
                            isEOF := true.
                    ] ifFalse: [
                        outFile = nil ifTrue: [
                            stdout nextPut:
                                (Character value: (node char)).
                        ]
                      ifFalse:[
                        8 to: 1 by: -1 do: [:index |
                            bit := (node char) bitAt: index.
                            outBytes writeBit: bit.
                        ].
                      ].
                      node := newTree.
                      ].
                    ].
                ]
           ] ifFalse:[
               node := node right.

               node isLeaf ifTrue: [
                   isEOF not ifTrue:[
                       ((node char)=256) ifTrue: [
                           isEOF := true.
                       ] ifFalse: [
                            outFile = nil ifTrue:[
                                stdout nextPut:
                                    (Character value: (node char)).
                            ]
                           ifFalse:[
                               8 to: 1 by: -1 do: [:index |
                                   bit := (node char) bitAt: index.
                                   outBytes writeBit: bit.
                               ].
                           ].
                           node:=newTree.
                         ].
                   ].
               ]
             ].
            ].
        ].
    ].
].

inFile close.
outFile ~= nil ifTrue:[ outFile close. ].
exitStatus:=0.
optionD ifTrue: [
    stdout << nl << 'Exit status: ' << exitStatus << nl.
    stdout << 'Done ' << nl.
].

