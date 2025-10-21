       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIJKSTRA-EVENTCHAINS.
       AUTHOR. GameDevMadeEasy.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MODERN-MAINFRAME.
       OBJECT-COMPUTER. MODERN-MAINFRAME.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> EVENT CONTEXT - Shared state for the algorithm
       01  WS-EVENT-CONTEXT.
           05  WS-NUM-NODES                 PIC 9(3) VALUE 9.
           05  WS-INFINITY                  PIC 9(6) VALUE 999999.
           05  WS-SOURCE-NODE               PIC 9(3) VALUE 1.
           05  WS-CURRENT-NODE              PIC 9(3).
           05  WS-NODES-PROCESSED           PIC 9(3) VALUE 0.
           05  WS-ALGORITHM-COMPLETE        PIC X VALUE 'N'.
               88  ALGORITHM-COMPLETE       VALUE 'Y'.
               88  ALGORITHM-INCOMPLETE     VALUE 'N'.

      *> GRAPH ADJACENCY MATRIX
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

      *> EVENT EXECUTION STATE
       01  WS-EVENT-RESULT.
           05  WS-EVENT-SUCCESS             PIC X VALUE 'Y'.
               88  EVENT-SUCCESS            VALUE 'Y'.
               88  EVENT-FAILURE            VALUE 'N'.
           05  WS-ERROR-MESSAGE             PIC X(80) VALUE SPACES.
           05  WS-CURRENT-EVENT-NAME        PIC X(30) VALUE SPACES.

      *> EVENTCHAIN STATE
       01  WS-EVENTCHAIN-STATE.
           05  WS-EVENTCHAIN-SUCCESS        PIC X VALUE 'Y'.
               88  EVENTCHAIN-SUCCESS       VALUE 'Y'.
               88  EVENTCHAIN-FAILURE       VALUE 'N'.
           05  WS-EVENTS-EXECUTED           PIC 9(3) VALUE 0.
           05  WS-EVENTS-FAILED             PIC 9(3) VALUE 0.

      *> MIDDLEWARE STATE
       01  WS-MIDDLEWARE-STATE.
           05  WS-LOG-ENABLED               PIC X VALUE 'N'.
               88  LOGGING-ON               VALUE 'Y'.
               88  LOGGING-OFF              VALUE 'N'.
           05  WS-TIMING-ENABLED            PIC X VALUE 'N'.
               88  TIMING-ON                VALUE 'Y'.
               88  TIMING-OFF               VALUE 'N'.
           05  WS-EVENT-START-TIME          PIC 9(8).
           05  WS-EVENT-END-TIME            PIC 9(8).
           05  WS-EVENT-DURATION            PIC 9(8).

      *> WORKING VARIABLES
       01  WS-WORK-VARS.
           05  WS-MIN-DISTANCE              PIC 9(6).
           05  WS-MIN-NODE                  PIC 9(3).
           05  WS-NEIGHBOR-NODE             PIC 9(3).
           05  WS-EDGE-WEIGHT               PIC 9(6).
           05  WS-NEW-DISTANCE              PIC 9(6).
           05  WS-OLD-DISTANCE              PIC 9(6).
           05  WS-LOOP-COUNTER              PIC 9(3).
           05  WS-ITERATION-COUNT           PIC 9(3) VALUE 0.

      *> TIMING VARIABLES
       01  WS-TIMING.
           05  WS-START-TIME                PIC 9(8).
           05  WS-END-TIME                  PIC 9(8).
           05  WS-ELAPSED-TIME              PIC 9(8).

      *> DISPLAY FORMATTING
       01  WS-DISPLAY-LINE                  PIC X(70) VALUE ALL '='.
       01  WS-DISPLAY-HEADER                PIC X(70) VALUE
           'DIJKSTRA ALGORITHM - EVENTCHAINS IMPLEMENTATION'.

       PROCEDURE DIVISION.

      *> MAIN PROGRAM
       0000-MAIN.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY WS-DISPLAY-HEADER.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY ' '.

           PERFORM 1000-INITIALIZE-GRAPH.
           PERFORM 2000-DISPLAY-GRAPH.

           DISPLAY ' '.
           DISPLAY 'Starting Dijkstra Algorithm (EventChains)...'.
           DISPLAY 'Source Node: ' WS-SOURCE-NODE.
           DISPLAY ' '.

           ACCEPT WS-START-TIME FROM TIME.
           PERFORM 3000-EXECUTE-DIJKSTRA-CHAIN.
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
           DISPLAY 'Events Executed: ' WS-EVENTS-EXECUTED.
           DISPLAY 'Events Failed: ' WS-EVENTS-FAILED.
           DISPLAY WS-DISPLAY-LINE.

           STOP RUN.

      *> INITIALIZE GRAPH (SAME AS TRADITIONAL)
       1000-INITIALIZE-GRAPH.
           DISPLAY 'Initializing graph...'.

           PERFORM VARYING IDX-ROW FROM 1 BY 1
               UNTIL IDX-ROW > WS-NUM-NODES
               PERFORM VARYING IDX-COL FROM 1 BY 1
                   UNTIL IDX-COL > WS-NUM-NODES
                   MOVE 0 TO WS-GRAPH-CELL(IDX-ROW, IDX-COL)
               END-PERFORM
           END-PERFORM.

      *>   Define edges (same graph structure)
           MOVE 4 TO WS-GRAPH-CELL(1, 2).
           MOVE 4 TO WS-GRAPH-CELL(2, 1).
           MOVE 2 TO WS-GRAPH-CELL(1, 8).
           MOVE 2 TO WS-GRAPH-CELL(8, 1).

           MOVE 8 TO WS-GRAPH-CELL(2, 3).
           MOVE 8 TO WS-GRAPH-CELL(3, 2).
           MOVE 11 TO WS-GRAPH-CELL(2, 8).
           MOVE 11 TO WS-GRAPH-CELL(8, 2).

           MOVE 7 TO WS-GRAPH-CELL(3, 4).
           MOVE 7 TO WS-GRAPH-CELL(4, 3).
           MOVE 2 TO WS-GRAPH-CELL(3, 6).
           MOVE 2 TO WS-GRAPH-CELL(6, 3).
           MOVE 4 TO WS-GRAPH-CELL(3, 9).
           MOVE 4 TO WS-GRAPH-CELL(9, 3).

           MOVE 9 TO WS-GRAPH-CELL(4, 5).
           MOVE 9 TO WS-GRAPH-CELL(5, 4).
           MOVE 14 TO WS-GRAPH-CELL(4, 6).
           MOVE 14 TO WS-GRAPH-CELL(6, 4).

           MOVE 10 TO WS-GRAPH-CELL(5, 6).
           MOVE 10 TO WS-GRAPH-CELL(6, 5).

           MOVE 2 TO WS-GRAPH-CELL(6, 7).
           MOVE 2 TO WS-GRAPH-CELL(7, 6).

           MOVE 1 TO WS-GRAPH-CELL(7, 8).
           MOVE 1 TO WS-GRAPH-CELL(8, 7).
           MOVE 6 TO WS-GRAPH-CELL(7, 9).
           MOVE 6 TO WS-GRAPH-CELL(9, 7).

           MOVE 7 TO WS-GRAPH-CELL(8, 9).
           MOVE 7 TO WS-GRAPH-CELL(9, 8).

           DISPLAY 'Graph initialized with ' WS-NUM-NODES ' nodes.'.
           EXIT.

      *> DISPLAY GRAPH (SAME AS TRADITIONAL)
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

      *> DIJKSTRA EVENTCHAIN - Main execution loop
       3000-EXECUTE-DIJKSTRA-CHAIN.
           DISPLAY 'Executing Dijkstra EventChain...'.
           DISPLAY ' '.

      *>   Initialize EventChain state
           MOVE 'Y' TO WS-EVENTCHAIN-SUCCESS.
           MOVE 0 TO WS-EVENTS-EXECUTED.
           MOVE 0 TO WS-EVENTS-FAILED.

      *>   EVENT 1: Initialize Algorithm
           MOVE 'InitializeAlgorithm' TO WS-CURRENT-EVENT-NAME.
           PERFORM 9100-MIDDLEWARE-WRAPPER.
           IF EVENT-FAILURE
               PERFORM 9900-HANDLE-EVENT-FAILURE
               GO TO 3000-EVENTCHAIN-END
           END-IF.

      *>   MAIN LOOP: Process each node (max iterations = num nodes)
           PERFORM VARYING WS-ITERATION-COUNT FROM 1 BY 1
               UNTIL WS-ITERATION-COUNT > WS-NUM-NODES
                   OR ALGORITHM-COMPLETE

      *>       EVENT 2: Find minimum unvisited node
               MOVE 'FindMinimumNode' TO WS-CURRENT-EVENT-NAME
               PERFORM 9100-MIDDLEWARE-WRAPPER
               IF EVENT-FAILURE
                   PERFORM 9900-HANDLE-EVENT-FAILURE
                   GO TO 3000-EVENTCHAIN-END
               END-IF

      *>       Check if we found a valid node
               IF WS-MIN-NODE = 0
                   MOVE 'Y' TO WS-ALGORITHM-COMPLETE
                   EXIT PERFORM
               END-IF

      *>       EVENT 3: Mark current node as visited
               MOVE WS-MIN-NODE TO WS-CURRENT-NODE
               MOVE 'MarkNodeVisited' TO WS-CURRENT-EVENT-NAME
               PERFORM 9100-MIDDLEWARE-WRAPPER
               IF EVENT-FAILURE
                   PERFORM 9900-HANDLE-EVENT-FAILURE
                   GO TO 3000-EVENTCHAIN-END
               END-IF

      *>       EVENT 4: Update neighbor distances
               MOVE 'UpdateNeighborDistances'
                   TO WS-CURRENT-EVENT-NAME
               PERFORM 9100-MIDDLEWARE-WRAPPER
               IF EVENT-FAILURE
                   PERFORM 9900-HANDLE-EVENT-FAILURE
                   GO TO 3000-EVENTCHAIN-END
               END-IF
           END-PERFORM.

       3000-EVENTCHAIN-END.
           EXIT.

      *> MIDDLEWARE WRAPPER - Wraps event execution
       9100-MIDDLEWARE-WRAPPER.
      *>   Pre-execution middleware
           IF LOGGING-ON
               PERFORM 9110-LOG-EVENT-START
           END-IF.

           IF TIMING-ON
               PERFORM 9120-START-TIMER
           END-IF.

      *>   CORE EVENT EXECUTION
           PERFORM 9200-DISPATCH-EVENT.

      *>   Post-execution middleware
           IF TIMING-ON
               PERFORM 9125-STOP-TIMER
           END-IF.

           IF LOGGING-ON
               PERFORM 9115-LOG-EVENT-END
           END-IF.

       9100-MIDDLEWARE-END.
           EXIT.

      *> MIDDLEWARE: Logging (Pre)
       9110-LOG-EVENT-START.
           DISPLAY '  [LOG] Starting: ' WS-CURRENT-EVENT-NAME.
           EXIT.

      *> MIDDLEWARE: Logging (Post)
       9115-LOG-EVENT-END.
           IF EVENT-SUCCESS
               DISPLAY '  [LOG] Completed: ' WS-CURRENT-EVENT-NAME
           ELSE
               DISPLAY '  [LOG] Failed: ' WS-CURRENT-EVENT-NAME
           END-IF.
           EXIT.

      *> MIDDLEWARE: Timing (Start)
       9120-START-TIMER.
           ACCEPT WS-EVENT-START-TIME FROM TIME.
           EXIT.

      *> MIDDLEWARE: Timing (Stop)
       9125-STOP-TIMER.
           ACCEPT WS-EVENT-END-TIME FROM TIME.
           COMPUTE WS-EVENT-DURATION =
               WS-EVENT-END-TIME - WS-EVENT-START-TIME.
           EXIT.

      *> EVENT DISPATCHER
       9200-DISPATCH-EVENT.
           EVALUATE WS-CURRENT-EVENT-NAME
               WHEN 'InitializeAlgorithm'
                   PERFORM 5100-EVENT-INITIALIZE
               WHEN 'FindMinimumNode'
                   PERFORM 5200-EVENT-FIND-MINIMUM
               WHEN 'MarkNodeVisited'
                   PERFORM 5300-EVENT-MARK-VISITED
               WHEN 'UpdateNeighborDistances'
                   PERFORM 5400-EVENT-UPDATE-NEIGHBORS
               WHEN OTHER
                   MOVE 'N' TO WS-EVENT-SUCCESS
                   MOVE 'Unknown event' TO WS-ERROR-MESSAGE
           END-EVALUATE.

           ADD 1 TO WS-EVENTS-EXECUTED.
           EXIT.

      *> EVENT 1: Initialize Algorithm
       5100-EVENT-INITIALIZE.
           MOVE 'Y' TO WS-EVENT-SUCCESS.
           MOVE SPACES TO WS-ERROR-MESSAGE.

           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL WS-LOOP-COUNTER > WS-NUM-NODES

               SET IDX-DIST TO WS-LOOP-COUNTER
               SET IDX-VISIT TO WS-LOOP-COUNTER
               SET IDX-PREV TO WS-LOOP-COUNTER

               MOVE WS-INFINITY TO WS-DISTANCES(IDX-DIST)
               MOVE 'N' TO WS-VISITED(IDX-VISIT)
               MOVE 0 TO WS-PREVIOUS(IDX-PREV)
           END-PERFORM.

           SET IDX-DIST TO WS-SOURCE-NODE.
           MOVE 0 TO WS-DISTANCES(IDX-DIST).

           EXIT.

      *> EVENT 2: Find Minimum Unvisited Node
       5200-EVENT-FIND-MINIMUM.
           MOVE 'Y' TO WS-EVENT-SUCCESS.
           MOVE SPACES TO WS-ERROR-MESSAGE.

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

      *> EVENT 3: Mark Node as Visited
       5300-EVENT-MARK-VISITED.
           MOVE 'Y' TO WS-EVENT-SUCCESS.
           MOVE SPACES TO WS-ERROR-MESSAGE.

           SET IDX-VISIT TO WS-CURRENT-NODE.
           MOVE 'Y' TO WS-VISITED(IDX-VISIT).
           ADD 1 TO WS-NODES-PROCESSED.

           EXIT.

      *> EVENT 4: Update Neighbor Distances
       5400-EVENT-UPDATE-NEIGHBORS.
           MOVE 'Y' TO WS-EVENT-SUCCESS.
           MOVE SPACES TO WS-ERROR-MESSAGE.

           PERFORM VARYING WS-NEIGHBOR-NODE FROM 1 BY 1
               UNTIL WS-NEIGHBOR-NODE > WS-NUM-NODES

               SET IDX-ROW TO WS-CURRENT-NODE
               SET IDX-COL TO WS-NEIGHBOR-NODE
               MOVE WS-GRAPH-CELL(IDX-ROW, IDX-COL)
                   TO WS-EDGE-WEIGHT

               IF WS-EDGE-WEIGHT > 0
                   SET IDX-VISIT TO WS-NEIGHBOR-NODE

                   IF NODE-UNVISITED(IDX-VISIT)
                       SET IDX-DIST TO WS-CURRENT-NODE
                       COMPUTE WS-NEW-DISTANCE =
                           WS-DISTANCES(IDX-DIST) + WS-EDGE-WEIGHT
                       END-COMPUTE

                       SET IDX-DIST TO WS-NEIGHBOR-NODE
                       MOVE WS-DISTANCES(IDX-DIST) TO WS-OLD-DISTANCE

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

      *> ERROR HANDLER
       9900-HANDLE-EVENT-FAILURE.
           ADD 1 TO WS-EVENTS-FAILED.
           MOVE 'N' TO WS-EVENTCHAIN-SUCCESS.

           DISPLAY '  [ERROR] Event failed: ' WS-CURRENT-EVENT-NAME.
           DISPLAY '  [ERROR] Message: ' WS-ERROR-MESSAGE.

           EXIT.

      *> DISPLAY RESULTS (SAME AS TRADITIONAL)
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

      *> DISPLAY PATH (SAME AS TRADITIONAL)
       4100-DISPLAY-PATH.
           PERFORM 4110-BUILD-PATH-RECURSIVE.
           DISPLAY ' '.
           EXIT.

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
           DISPLAY 'Path continues... '.
           EXIT.
