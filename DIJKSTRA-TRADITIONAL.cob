       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIJKSTRA-TRADITIONAL.
       AUTHOR. GameDevMadeEasy.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MODERN-MAINFRAME.
       OBJECT-COMPUTER. MODERN-MAINFRAME.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> GRAPH CONFIGURATION
       01  WS-GRAPH-CONFIG.
           05  WS-NUM-NODES                 PIC 9(3) VALUE 9.
           05  WS-INFINITY                  PIC 9(6) VALUE 999999.
           05  WS-SOURCE-NODE               PIC 9(3) VALUE 1.

      *> GRAPH ADJACENCY MATRIX (9x9 for this example)
      *> 0 means no edge, positive number is edge weight
       01  WS-GRAPH-MATRIX.
           05  WS-GRAPH-ROW OCCURS 9 TIMES INDEXED BY IDX-ROW.
               10  WS-GRAPH-CELL OCCURS 9 TIMES INDEXED BY IDX-COL
                   PIC 9(6).

      *> DIJKSTRA ALGORITHM STATE
       01  WS-DIJKSTRA-STATE.
           05  WS-DISTANCES OCCURS 9 TIMES INDEXED BY IDX-DIST
               PIC 9(6).
           05  WS-VISITED OCCURS 9 TIMES INDEXED BY IDX-VISIT
               PIC X VALUE 'N'.
               88  NODE-VISITED VALUE 'Y'.
               88  NODE-UNVISITED VALUE 'N'.
           05  WS-PREVIOUS OCCURS 9 TIMES INDEXED BY IDX-PREV
               PIC 9(3).

      *> WORKING VARIABLES
       01  WS-WORK-VARS.
           05  WS-CURRENT-NODE              PIC 9(3).
           05  WS-MIN-DISTANCE              PIC 9(6).
           05  WS-MIN-NODE                  PIC 9(3).
           05  WS-NEIGHBOR-NODE             PIC 9(3).
           05  WS-EDGE-WEIGHT               PIC 9(6).
           05  WS-NEW-DISTANCE              PIC 9(6).
           05  WS-OLD-DISTANCE              PIC 9(6).
           05  WS-NODES-PROCESSED           PIC 9(3) VALUE 0.
           05  WS-LOOP-COUNTER              PIC 9(3).
           05  WS-ALL-VISITED               PIC X VALUE 'N'.
               88  ALL-NODES-VISITED        VALUE 'Y'.

      *> TIMING VARIABLES
       01  WS-TIMING.
           05  WS-START-TIME                PIC 9(8).
           05  WS-END-TIME                  PIC 9(8).
           05  WS-ELAPSED-TIME              PIC 9(8).

      *> DISPLAY FORMATTING
       01  WS-DISPLAY-LINE                  PIC X(70) VALUE ALL '='.
       01  WS-DISPLAY-HEADER                PIC X(70) VALUE
           'DIJKSTRA ALGORITHM - TRADITIONAL IMPLEMENTATION'.

       PROCEDURE DIVISION.

       0000-MAIN.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY WS-DISPLAY-HEADER.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY ' '.

           PERFORM 1000-INITIALIZE-GRAPH.
           PERFORM 2000-DISPLAY-GRAPH.

           DISPLAY ' '.
           DISPLAY 'Starting Dijkstra Algorithm...'.
           DISPLAY 'Source Node: ' WS-SOURCE-NODE.
           DISPLAY ' '.

           ACCEPT WS-START-TIME FROM TIME.
           PERFORM 3000-DIJKSTRA-ALGORITHM.
           ACCEPT WS-END-TIME FROM TIME.

           COMPUTE WS-ELAPSED-TIME = WS-END-TIME - WS-START-TIME.

           DISPLAY ' '.
           PERFORM 4000-DISPLAY-RESULTS.

           DISPLAY ' '.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY 'PERFORMANCE METRICS'.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY 'Execution Time: ' WS-ELAPSED-TIME
               ' microseconds'.
           DISPLAY 'Nodes Processed: ' WS-NODES-PROCESSED.
           DISPLAY WS-DISPLAY-LINE.

           STOP RUN.

      *> INITIALIZE GRAPH WITH EXAMPLE DATA
      *> Creates a 9-node graph with various weighted edges
       1000-INITIALIZE-GRAPH.
           DISPLAY 'Initializing graph...'.

      *>   Initialize all cells to 0 (no edge)
           PERFORM VARYING IDX-ROW FROM 1 BY 1
               UNTIL IDX-ROW > WS-NUM-NODES
               PERFORM VARYING IDX-COL FROM 1 BY 1
                   UNTIL IDX-COL > WS-NUM-NODES
                   MOVE 0 TO WS-GRAPH-CELL(IDX-ROW, IDX-COL)
               END-PERFORM
           END-PERFORM.

      *>   Define edges (bidirectional graph)
      *>   Node 1 connections
           MOVE 4 TO WS-GRAPH-CELL(1, 2).
           MOVE 4 TO WS-GRAPH-CELL(2, 1).
           MOVE 2 TO WS-GRAPH-CELL(1, 8).
           MOVE 2 TO WS-GRAPH-CELL(8, 1).

      *>   Node 2 connections
           MOVE 8 TO WS-GRAPH-CELL(2, 3).
           MOVE 8 TO WS-GRAPH-CELL(3, 2).
           MOVE 11 TO WS-GRAPH-CELL(2, 8).
           MOVE 11 TO WS-GRAPH-CELL(8, 2).

      *>   Node 3 connections
           MOVE 7 TO WS-GRAPH-CELL(3, 4).
           MOVE 7 TO WS-GRAPH-CELL(4, 3).
           MOVE 2 TO WS-GRAPH-CELL(3, 6).
           MOVE 2 TO WS-GRAPH-CELL(6, 3).
           MOVE 4 TO WS-GRAPH-CELL(3, 9).
           MOVE 4 TO WS-GRAPH-CELL(9, 3).

      *>   Node 4 connections
           MOVE 9 TO WS-GRAPH-CELL(4, 5).
           MOVE 9 TO WS-GRAPH-CELL(5, 4).
           MOVE 14 TO WS-GRAPH-CELL(4, 6).
           MOVE 14 TO WS-GRAPH-CELL(6, 4).

      *>   Node 5 connections
           MOVE 10 TO WS-GRAPH-CELL(5, 6).
           MOVE 10 TO WS-GRAPH-CELL(6, 5).

      *>   Node 6 connections
           MOVE 2 TO WS-GRAPH-CELL(6, 7).
           MOVE 2 TO WS-GRAPH-CELL(7, 6).

      *>   Node 7 connections
           MOVE 1 TO WS-GRAPH-CELL(7, 8).
           MOVE 1 TO WS-GRAPH-CELL(8, 7).
           MOVE 6 TO WS-GRAPH-CELL(7, 9).
           MOVE 6 TO WS-GRAPH-CELL(9, 7).

      *>   Node 8 connections
           MOVE 7 TO WS-GRAPH-CELL(8, 9).
           MOVE 7 TO WS-GRAPH-CELL(9, 8).

           DISPLAY 'Graph initialized with ' WS-NUM-NODES ' nodes.'.
           EXIT.

      *> DISPLAY GRAPH ADJACENCY MATRIX
       2000-DISPLAY-GRAPH.
           DISPLAY ' '.
           DISPLAY 'Graph Adjacency Matrix:'.
           DISPLAY '(0 = no edge, positive number = edge weight)'.
           DISPLAY ' '.

           PERFORM VARYING IDX-ROW FROM 1 BY 1
               UNTIL IDX-ROW > WS-NUM-NODES
               DISPLAY 'Node ' IDX-ROW ': ' WITH NO ADVANCING
               PERFORM VARYING IDX-COL FROM 1 BY 1
                   UNTIL IDX-COL > WS-NUM-NODES
                   DISPLAY WS-GRAPH-CELL(IDX-ROW, IDX-COL) ' '
                       WITH NO ADVANCING
               END-PERFORM
               DISPLAY ' '
           END-PERFORM.
           EXIT.

      *> DIJKSTRA'S ALGORITHM - TRADITIONAL IMPLEMENTATION
       3000-DIJKSTRA-ALGORITHM.
      *>   Step 1: Initialize distances and visited array
           PERFORM 3100-INITIALIZE-DIJKSTRA.

      *>   Step 2: Main loop - process all nodes
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL WS-LOOP-COUNTER > WS-NUM-NODES

      *>       Find unvisited node with minimum distance
               PERFORM 3200-FIND-MIN-UNVISITED-NODE

      *>       If no reachable unvisited nodes, exit
               IF WS-MIN-NODE = 0
                   EXIT PERFORM
               END-IF

      *>       Mark current node as visited
               MOVE WS-MIN-NODE TO WS-CURRENT-NODE
               SET IDX-VISIT TO WS-CURRENT-NODE
               MOVE 'Y' TO WS-VISITED(IDX-VISIT)
               ADD 1 TO WS-NODES-PROCESSED

      *>       Update distances to neighbors
               PERFORM 3300-UPDATE-NEIGHBOR-DISTANCES
           END-PERFORM.

           EXIT.

      *> INITIALIZE DIJKSTRA DATA STRUCTURES
       3100-INITIALIZE-DIJKSTRA.
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL WS-LOOP-COUNTER > WS-NUM-NODES

               SET IDX-DIST TO WS-LOOP-COUNTER
               SET IDX-VISIT TO WS-LOOP-COUNTER
               SET IDX-PREV TO WS-LOOP-COUNTER

      *>       Set all distances to infinity initially
               MOVE WS-INFINITY TO WS-DISTANCES(IDX-DIST)

      *>       Mark all nodes as unvisited
               MOVE 'N' TO WS-VISITED(IDX-VISIT)

      *>       No previous node initially
               MOVE 0 TO WS-PREVIOUS(IDX-PREV)
           END-PERFORM.

      *>   Distance to source node is 0
           SET IDX-DIST TO WS-SOURCE-NODE.
           MOVE 0 TO WS-DISTANCES(IDX-DIST).

           EXIT.

      *> FIND UNVISITED NODE WITH MINIMUM DISTANCE
       3200-FIND-MIN-UNVISITED-NODE.
           MOVE WS-INFINITY TO WS-MIN-DISTANCE.
           MOVE 0 TO WS-MIN-NODE.

           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL WS-LOOP-COUNTER > WS-NUM-NODES

               SET IDX-VISIT TO WS-LOOP-COUNTER
               SET IDX-DIST TO WS-LOOP-COUNTER

               IF NODE-UNVISITED(IDX-VISIT)
                   IF WS-DISTANCES(IDX-DIST) < WS-MIN-DISTANCE
                       MOVE WS-DISTANCES(IDX-DIST) TO WS-MIN-DISTANCE
                       MOVE WS-LOOP-COUNTER TO WS-MIN-NODE
                   END-IF
               END-IF
           END-PERFORM.

           EXIT.

      *> UPDATE DISTANCES TO NEIGHBORS OF CURRENT NODE
       3300-UPDATE-NEIGHBOR-DISTANCES.
           PERFORM VARYING WS-NEIGHBOR-NODE FROM 1 BY 1
               UNTIL WS-NEIGHBOR-NODE > WS-NUM-NODES

               SET IDX-ROW TO WS-CURRENT-NODE
               SET IDX-COL TO WS-NEIGHBOR-NODE
               MOVE WS-GRAPH-CELL(IDX-ROW, IDX-COL)
                   TO WS-EDGE-WEIGHT

      *>       If there's an edge to this neighbor
               IF WS-EDGE-WEIGHT > 0
                   SET IDX-VISIT TO WS-NEIGHBOR-NODE

      *>           Only process unvisited neighbors
                   IF NODE-UNVISITED(IDX-VISIT)
                       SET IDX-DIST TO WS-CURRENT-NODE
                       COMPUTE WS-NEW-DISTANCE =
                           WS-DISTANCES(IDX-DIST) + WS-EDGE-WEIGHT
                       END-COMPUTE

                       SET IDX-DIST TO WS-NEIGHBOR-NODE
                       MOVE WS-DISTANCES(IDX-DIST) TO WS-OLD-DISTANCE

      *>               If new path is shorter, update
                       IF WS-NEW-DISTANCE < WS-OLD-DISTANCE
                           MOVE WS-NEW-DISTANCE
                               TO WS-DISTANCES(IDX-DIST)
                           SET IDX-PREV TO WS-NEIGHBOR-NODE
                           MOVE WS-CURRENT-NODE
                               TO WS-PREVIOUS(IDX-PREV)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

           EXIT.

      *> DISPLAY FINAL RESULTS
       4000-DISPLAY-RESULTS.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY 'SHORTEST PATHS FROM NODE ' WS-SOURCE-NODE.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY ' '.

           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL WS-LOOP-COUNTER > WS-NUM-NODES

               SET IDX-DIST TO WS-LOOP-COUNTER

               DISPLAY 'Node ' WS-LOOP-COUNTER ': ' WITH NO ADVANCING

               IF WS-DISTANCES(IDX-DIST) = WS-INFINITY
                   DISPLAY 'UNREACHABLE'
               ELSE
                   DISPLAY 'Distance = ' WS-DISTANCES(IDX-DIST)
                   DISPLAY '         Path: ' WITH NO ADVANCING
                   PERFORM 4100-DISPLAY-PATH
               END-IF
           END-PERFORM.

           EXIT.

      *> DISPLAY PATH TO A NODE
       4100-DISPLAY-PATH.
           PERFORM 4110-BUILD-PATH-RECURSIVE.
           DISPLAY ' '.
           EXIT.

      *> RECURSIVELY BUILD PATH (USING STACK SIMULATION)
       4110-BUILD-PATH-RECURSIVE.
           SET IDX-PREV TO WS-LOOP-COUNTER.

           IF WS-PREVIOUS(IDX-PREV) NOT = 0
               MOVE WS-PREVIOUS(IDX-PREV) TO WS-NEIGHBOR-NODE
               PERFORM 4120-DISPLAY-PARENT-PATH
           END-IF.

           DISPLAY WS-LOOP-COUNTER ' ' WITH NO ADVANCING.
           EXIT.

       4120-DISPLAY-PARENT-PATH.
           SET IDX-PREV TO WS-NEIGHBOR-NODE.

           IF WS-PREVIOUS(IDX-PREV) NOT = 0
               MOVE WS-PREVIOUS(IDX-PREV) TO WS-CURRENT-NODE
               PERFORM 4130-DISPLAY-GRANDPARENT-PATH
           END-IF.

           DISPLAY WS-NEIGHBOR-NODE ' -> ' WITH NO ADVANCING.
           EXIT.

       4130-DISPLAY-GRANDPARENT-PATH.
           SET IDX-PREV TO WS-CURRENT-NODE.

           IF WS-PREVIOUS(IDX-PREV) NOT = 0
               PERFORM 4140-DISPLAY-ANCESTORS
           END-IF.

           DISPLAY WS-CURRENT-NODE ' -> ' WITH NO ADVANCING.
           EXIT.

       4140-DISPLAY-ANCESTORS.
      *>   For deeper paths, would need more levels or iterative approach
      *>   This handles paths up to 3 hops deep
           DISPLAY 'Path continues... '.
           EXIT.
