Red [
    Title: "Advent of Code 2024: Day 6"
    Author: "Serhii Olendarenko"
    Date: 2024-12-06
    SPDX-License-Identifier: "BSD-3-Clause"
    SPDX-FileCopyrightText: "© 2024 Serhii “GooRoo” Olendarenko"
]

files: [%data/task.example %data/task.data]

build-map: function [input] [
    the-map: copy []

    free: "."
    obstacle: "#"
    guard: charset "<>v^^"

    to-direction: function [ch] [
        switch ch [
            #"<"  ['left]
            #">"  ['right]
            #"v"  ['down]
            #"^^" ['up]
        ]
    ]

    r: 0
    direction: none
    repeat i length? input [
        l: copy []
        parse input/:i [
            some
                [ free (append l 'free)
                | obstacle (append l 'obst)
                | set g guard (
                    r: i
                    direction: to-direction g
                    append l 'guard
                  )
                ]
        ]
        append/only the-map l
    ]
    c: index? find the-map/:r 'guard

    w: length? the-map/1
    h: length? the-map

    reduce [the-map as-pair w h direction as-pair r c]
]

check-border: function [pos] [
    either any [
        zero? pos/1
        zero? pos/2
        pos/1 > size/1
        pos/2 > size/2
    ]['border][pos]
]
go-up:    function [pos] [check-border as-pair pos/1 - 1 pos/2    ]
go-down:  function [pos] [check-border as-pair pos/1 + 1 pos/2    ]
go-left:  function [pos] [check-border as-pair pos/1     pos/2 - 1]
go-right: function [pos] [check-border as-pair pos/1     pos/2 + 1]

turn: function [direction] [
    switch direction [
        up ['right]
        right ['down]
        down ['left]
        left ['up]
    ]
]

get-cell: function [pos [pair!]] [
    the-map/(pos/1)/(pos/2)
]

to-visit: function [direction [word!]] [
    switch direction [
        up ['visit-up]
        down ['visit-down]
        left ['visit-left]
        right ['visit-right]
    ]
]

visit-cell: function [pos [pair!] direction [word!]] [
    the-map/(pos/1)/(pos/2): (to-visit direction)
]

visited?: function [pos [pair!] direction [word!]] [
    equal? (get-cell pos) (to-visit direction)
]

advance: func [
    pos [pair!]
    direction [word!]
    /local
    new-pos
    next-cell
][
    ; print ["Visiting " pos]

    if visited? pos direction [
        print ["Loop at " pos " with " direction]
        return reduce ['loop direction]
    ]

    visit-cell pos direction
    loop 3 [
        go: switch direction [
            up ['go-up]
            down ['go-down]
            left ['go-left]
            right ['go-right]
        ]
        new-pos: apply :go [pos]
        if new-pos = 'border [return reduce ['end direction]]

        next-cell: get-cell new-pos

        switch/default next-cell [
            obst [direction: turn direction continue]
        ][
            return reduce [pos: new-pos direction]
        ]
    ]

    print "This should have never happened"
    quit
]

simulate: func [the-map start] [
    pos: start

    until [
        set [pos direction] do [advance pos direction]
        any [equal? pos 'end equal? pos 'loop]
    ]
    pos
]

count-visited: func [the-map] [
    total: 0
    visit: ['visit-up | 'visit-down | 'visit-left | 'visit-right]
    parse the-map [
        some [
            ahead block!
            into [some [visit (total: total + 1) | skip]]
        ]
    ]
    total
]

solve-first: func [file [file!]] [
    set [the-map size direction start] build-map read/lines file
    orig-direction: direction
    simulate the-map start
    count-visited the-map
]

solve-second: func [file [file!] /local total [integer!]] [
    set [orig-map size orig-direction orig-start] build-map read/lines file
    probe size

    total: 0
    repeat i size/1 [
        repeat j size/2 [
            if equal? the-map/:i/:j 'obst [continue]
            the-map: copy/deep orig-map
            the-map/:i/:j: 'obst
            direction: orig-direction
            if equal? (simulate the-map orig-start) 'loop [total: total + 1]
        ]
    ]
    total
]

results: copy []
foreach file files [
    print ["Reading from " file]
    append/only results reduce [file solve-first file]
    append/only results reduce [file solve-second file]
]
