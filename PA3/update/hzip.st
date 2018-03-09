#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

execname := '$RCSfile: cmdline.st,v $' =~ '.*: (.*),v.*' at: 1.

nl := Character nl.

FileStream extend [
   nl := Character nl.
   catFile [
      |value textline|
      value := 0.
      [self atEnd not] whileTrue: [
         value := value + 1.
         textline :=self nextLine.
         value asString size to: 5 do: [:skip| stdout << ' '].
         stdout << value << ': ' << textline << nl.
      ]
   ]
]

"Array that is alter used for encode and freq table"
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



Object subclass: Leaf [
   |char value isLeaf|
   char [ ^ char ]
   value [ ^ value ]
   isLeaf [^ isLeaf]
   
   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar value: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.      
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      value := aCount.
      isLeaf:=true.
   ]

   <= other [
      ^ (value < other value)
      | ((value = other value) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << value
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   
   postorder: visitor [
      visitor  value: char value: isLeaf.
   ]
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar value: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar value: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar value: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      isLeaf:=false.
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
   "Had to add the postorder for -c"
   postorder: visitor[      
      left postorder: visitor .
      right postorder: visitor .  
      visitor value: char value:isLeaf.
   ]
]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^  self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

"Outputs bits to the specified outputfile"
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
      bitIndex = 0 ifTrue: [
        self flushByte].
   ]
]

"Object class: reads bits from inputfile"
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


"Where the truly program begins, before was just class defintions"
status := 0.
option_D := false.
option_C := false.
option_T := false.
option_U := false.


freqTable := ZeroArray new: 257.
freqTable at:256 put:1. "EOF"
priority := SortedCollection new.

"'global' varialbes used throughout program regardless of flags"
|readfile inputfile outputfile status|

argc := 0.


"https://stackoverflow.com/questions/41534812/gnu-smalltalk-command-line-arguments-parsing-explanation-documentation"
"Above url helped in parsing cmd line arguments"
Smalltalk arguments: '-t -d -c -u' do: [:opt :arg |

  (opt = $d) ifTrue: [ option_D := true. ].
  (opt = $t) ifTrue: [ option_T := true. ].
  (opt = $c) ifTrue: [ option_C := true. ].
  (opt = $u) ifTrue: [ option_U := true. ].



  (arg ~= nil) ifTrue: [
    "If arg ~= nill then the first arg after opt must be the inputfile, all other files are for output"
    argc = 0 
        ifTrue:[
          inputfile := FileStream open: arg
                    mode: FileStream read.
          readfile := FileStream open: arg
                    mode: FileStream read.
        ]
        ifFalse:[
            outputfile := FileStream open: arg
                       mode: FileStream write.
        ].
     argc := argc + 1.
   ].
].



"Followig code creates the encode table"
option_U ifFalse:[
    encode_table := ZeroArray new: 257.

    [inputfile atEnd not] whileTrue: [
       |refc|
       refc := inputfile next asInteger. 
       freqTable incr: refc.
    ].

    freqTable keysAndValuesDo: [:key :value |
       (value notNil and: [value > 0]) ifTrue: [
          new_leaf := Leaf new: key value: value. 
          priority add: new_leaf.
       ]
    ].

    [(priority size) > 1 ] whileTrue: [ 
       left := priority removeFirst.
       right := priority removeFirst.
       tree := ( Tree new: (left char)
                          value: (left value) + (right value)
                          left: left 
                          right: right).
       priority add: tree.
    ].

    tree := priority removeFirst.

    tree inorder: [:char :string | 
                    encode_table at: char put: string.
                  ] prefix: ''.
    bin_tree:=''.

    tree postorder: [:char :isLeaf |

       (isLeaf) ifFalse: [bin_tree := bin_tree,'1'.]
                ifTrue:[
                       |tro flo|
                       bin:=''.
                        tro := '000000001'.
                        flo := '000000000'.
                       (char = 256)
                            ifTrue:[bin:=tro.]
                            ifFalse:[
                               char=0 ifTrue:[bin:=flo.]
                                      ifFalse:[
                                         8 to:1 by: -1 do:[:i|
                                          bin := bin, ((char bitAt:i)
                                          printString).
                                       ]
                            ].               
                     ].
                     bin_tree := (bin_tree,'0',bin).
                ].
    ].
    "tricky printing function"
    (option_T) ifTrue:[
         encode_table keysAndValuesDo: [:key :value |
         (value notNil) ifTrue: [
            c := key asCharacter visible.
            (256 = key asInteger) ifTrue: [c := 'EOF'].
            (10 = key asInteger) ifTrue: [c := 'x0A'].
            (32 = key asInteger) ifTrue: [c := 'x20'].
            freq := freqTable at: key.
            freq_string := freq asString.
            stdout <<  c.
            1 to: 6-(c size) do: [:i | stdout << ' '].
            1 to: 5-(freq_string size) do: [:i | stdout << ' '].
            stdout << freq <<'    ' <<value.
            stdout << nl.
        ].
      ].
    ].

"Compression"
    option_C  ifTrue:[ 

    (outputfile ~= nil) 
    ifTrue:[
       
       out_bits := OutBits new: outputfile.    
       1 to: (bin_tree size) by: 1 do: [:index |
             bit:= ((bin_tree at: index) asInteger) - 48.
             out_bits writeBit: bit.
             ].

       out_bits writeBit: 1.
       [readfile atEnd not] whileTrue: [
          |c|
           c := readfile next asInteger.
           code:= (encode_table at: c). 
           1 to: (code size) by: 1 do: [:index |
             bit:= ((code at: index) asInteger) - 48.
             out_bits writeBit: bit.
              ].
           ].

       EOF := (encode_table at: 256).
       1 to: (EOF size) by: 1 do: [:index |
             bit:= ((EOF at: index) asInteger) - 48.
             out_bits writeBit: bit.].     
       out_bits flushByte.
      
    ]
    ifFalse:[
        
       stdout <<bin_tree<< 1.   
        
         [readfile atEnd not] whileTrue: [
               c := readfile next asInteger.
               code:= (encode_table at: c).
               stdout << code. 
            ].
          EOF := (encode_table at: 256).
          stdout<<EOF.
       ].    
     ].
].


"Decompression"
(option_U) ifTrue:[
  |out_bits stack tree_finished|
  stack := OrderedCollection new.

  EOF:=false.
  tree_finished := false.

  out_bits := OutBits new: outputfile.  
  file_bits := BitStream new: inputfile.
  
  decoding_tree:=''.
  node:=''.   
   
  "While the program has not reach end of file, get the bits from the inputfile "
  [EOF not] whileTrue: [
    next_node := file_bits nextBit.
    leafc := 0.
    "If the tree is not finished countie to make tree"
    (tree_finished) ifFalse:[
      "Inserts input to leaf if bit = 0"
      (next_node = 0) ifTrue:[
          8 to:1 by:-1 do:[:i|bit:=file_bits nextBit.
                              leafc:=leafc 
                              bitAt:i 
                              put:bit.
          ].

          (leafc = 0) ifTrue:[
            marker := file_bits nextBit.
            (marker = 1) ifTrue:[
              leafc := leafc bitAt:9 put:1.
            ].
          ].
          
          stack add: (Leaf new: leafc value: '').

        ].

        "builds tree from leaves" 
        (next_node = 1) ifTrue:[
                 
          (stack size > 1) ifTrue:[
          right := stack removeLast.
          left := stack removeLast.
           
          newtree := (Tree new: -1
                          value: ''
                          left: left
                          right: right).
            stack add: newtree.
           ] ifFalse:[
              tree_finished:=true.
             
              decoding_tree:=stack removeLast.
              node := decoding_tree.
           ].
        ].
     ]
     
    "If the tree has been built, then the data is decoded"
    ifTrue:[
        (node isLeaf) ifTrue:[
                (node char = 256) ifTrue:[
                    EOF := true.
                  ] ifFalse:[
                    stdout nextPut: (Character value: (node char)).
                    node := decoding_tree.
                  ].
        ] ifFalse:[
          (next_node=0)ifTrue:[
              node:=node left.

              node isLeaf ifTrue:[
              (EOF not) ifTrue:[  
                  ((node char) = 256) ifTrue:[
                  EOF:=true.


                  ] ifFalse:[

                    (outputfile = nil) ifTrue:[
                        stdout nextPut: (Character value: (node char)).
                    ] ifFalse:[
                        8 to: 1 by: -1 do: [:index |
                            bit := (node char) 
                            bitAt: index.
                            out_bits writeBit: bit.
                      ].
                    ].
                    node := decoding_tree.
                  ].
              ].
              ]
           ] ifFalse:[
               
              node:=node right.

              node isLeaf ifTrue:[
              (EOF not) ifTrue:[
                  ((node char) = 256) ifTrue:[
                      EOF:=true.
                  ] ifFalse:[
                    (outputfile = nil) ifTrue:[
                        stdout nextPut: (Character value: (node char)).
                    ]ifFalse:[
                        8 to: 1 by: -1 do: [:index |
                           bit:= (node char) bitAt: index.
                           out_bits writeBit: bit.
                        ].
                     ].
                    node:=decoding_tree.

                    ].
                ].
               ]
           ].
       ].
     ].
   ].
].  

"close file streams"
inputfile close.
(outputfile ~= nil) ifTrue:[
    outputfile close.
].
"quit with successful status of 0"
ObjectMemory quit: status.
