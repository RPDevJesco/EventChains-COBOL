       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIJKSTRA-COMPARISON.
       AUTHOR. GameDevMadeEasy.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MODERN-MAINFRAME.
       OBJECT-COMPUTER. MODERN-MAINFRAME.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> TEST CONFIGURATION
       01  WS-TEST-CONFIG.
           05  WS-NUM-ITERATIONS            PIC 9(4) VALUE 1000.
           05  WS-CURRENT-ITERATION         PIC 9(4).
           05  WS-NUM-NODES                 PIC 9(3) VALUE 9.
           05  WS-INFINITY                  PIC 9(6) VALUE 999999.
           05  WS-SOURCE-NODE               PIC 9(3) VALUE 1.

      *> GRAPH DATA
       01  WS-GRAPH-MATRIX.
           05  WS-GRAPH-ROW OCCURS 9 TIMES INDEXED BY IDX-ROW.
               10  WS-GRAPH-CELL OCCURS 9 TIMES INDEXED BY IDX-COL
                   PIC 9(6).

      *> DIJKSTRA STATE (SHARED BETWEEN IMPLEMENTATIONS)
       01  WS-DIJKSTRA-STATE.
           05  WS-DISTANCES OCCURS 9 TIMES INDEXED BY IDX-DIST
               PIC 9(6).
           05  WS-VISITED OCCURS 9 TIMES INDEXED BY IDX-VISIT
               PIC X VALUE 'N'.
               88  NODE-VISITED VALUE 'Y'.
               88  NODE-UNVISITED VALUE 'N'.
           05  WS-PREVIOUS OCCURS 9 TIMES INDEXED BY IDX-PREV
               PIC 9(3).

      *> TRADITIONAL IMPLEMENTATION METRICS
       01  WS-TRADITIONAL-METRICS.
           05  WS-TRAD-TOTAL-TIME           PIC 9(12) VALUE 0.
           05  WS-TRAD-MIN-TIME             PIC 9(8) VALUE 99999999.
           05  WS-TRAD-MAX-TIME             PIC 9(8) VALUE 0.
           05  WS-TRAD-AVG-TIME             PIC 9(8).
           05  WS-TRAD-CURRENT-TIME         PIC 9(8).

      *> EVENTCHAINS IMPLEMENTATION METRICS
       01  WS-EVENTCHAINS-METRICS.
           05  WS-EC-TOTAL-TIME             PIC 9(12) VALUE 0.
           05  WS-EC-MIN-TIME               PIC 9(8) VALUE 99999999.
           05  WS-EC-MAX-TIME               PIC 9(8) VALUE 0.
           05  WS-EC-AVG-TIME               PIC 9(8).
           05  WS-EC-CURRENT-TIME           PIC 9(8).

      *> COMPARISON RESULTS
       01  WS-COMPARISON.
           05  WS-TIME-DIFFERENCE           PIC S9(8).
           05  WS-PERCENTAGE-DIFF           PIC S9(5)V99.
           05  WS-FASTER-IMPL               PIC X(20).

      *> WORKING VARIABLES FOR ALGORITHM
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

      *> EVENTCHAINS SPECIFIC VARIABLES
       01  WS-EVENTCHAINS-STATE.
           05  WS-ALGORITHM-COMPLETE        PIC X VALUE 'N'.
               88  ALGORITHM-COMPLETE       VALUE 'Y'.
               88  ALGORITHM-INCOMPLETE     VALUE 'N'.
           05  WS-ITERATION-COUNT           PIC 9(3) VALUE 0.
           05  WS-EVENT-SUCCESS             PIC X VALUE 'Y'.
               88  EVENT-SUCCESS            VALUE 'Y'.
               88  EVENT-FAILURE            VALUE 'N'.
           05  WS-CURRENT-EVENT-NAME        PIC X(30) VALUE SPACES.

      *> TIMING VARIABLES
       01  WS-TIMING.
           05  WS-START-TIME                PIC 9(8).
           05  WS-END-TIME                  PIC 9(8).
           05  WS-ELAPSED-TIME              PIC 9(8).

      *> DISPLAY FORMATTING
       01  WS-DISPLAY-LINE                  PIC X(70) VALUE ALL '='.
       01  WS-DISPLAY-THIN-LINE             PIC X(70) VALUE ALL '-'.

       PROCEDURE DIVISION.

       0000-MAIN.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY ' DIJKSTRA ALGORITHM PERFORMANCE COMPARISON'.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY ' '.

           PERFORM 1000-INITIALIZE-GRAPH.

           DISPLAY 'Running performance comparison...'.
           DISPLAY 'Iterations per implementation: '
               WS-NUM-ITERATIONS.
           DISPLAY ' '.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY ' '.

      *>   RUN TRADITIONAL IMPLEMENTATION
           DISPLAY 'Testing Traditional Implementation...'.
           PERFORM 2000-TEST-TRADITIONAL-IMPL.
           DISPLAY 'Traditional implementation complete.'.
           DISPLAY ' '.

      *>   RUN EVENTCHAINS IMPLEMENTATION
           DISPLAY 'Testing EventChains Implementation...'.
           PERFORM 3000-TEST-EVENTCHAINS-IMPL.
           DISPLAY 'EventChains implementation complete.'.
           DISPLAY ' '.

      *>   CALCULATE AND DISPLAY RESULTS
           PERFORM 4000-CALCULATE-COMPARISON.
           PERFORM 5000-DISPLAY-RESULTS.

           STOP RUN.

      *> INITIALIZE GRAPH (SAME STRUCTURE AS OTHER PROGRAMS)
       1000-INITIALIZE-GRAPH.
           PERFORM VARYING IDX-ROW FROM 1 BY 1
               UNTIL IDX-ROW > WS-NUM-NODES
               PERFORM VARYING IDX-COL FROM 1 BY 1
                   UNTIL IDX-COL > WS-NUM-NODES
                   MOVE 0 TO WS-GRAPH-CELL(IDX-ROW, IDX-COL)
               END-PERFORM
           END-PERFORM.

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

           EXIT.

      *> TEST TRADITIONAL IMPLEMENTATION
       2000-TEST-TRADITIONAL-IMPL.
           PERFORM VARYING WS-CURRENT-ITERATION FROM 1 BY 1
               UNTIL WS-CURRENT-ITERATION > WS-NUM-ITERATIONS

               ACCEPT WS-START-TIME FROM TIME
               PERFORM 2100-RUN-TRADITIONAL-DIJKSTRA
               ACCEPT WS-END-TIME FROM TIME

               COMPUTE WS-TRAD-CURRENT-TIME =
                   WS-END-TIME - WS-START-TIME
               END-COMPUTE

               ADD WS-TRAD-CURRENT-TIME TO WS-TRAD-TOTAL-TIME

               IF WS-TRAD-CURRENT-TIME < WS-TRAD-MIN-TIME
                   MOVE WS-TRAD-CURRENT-TIME TO WS-TRAD-MIN-TIME
               END-IF

               IF WS-TRAD-CURRENT-TIME > WS-TRAD-MAX-TIME
                   MOVE WS-TRAD-CURRENT-TIME TO WS-TRAD-MAX-TIME
               END-IF
           END-PERFORM.

           COMPUTE WS-TRAD-AVG-TIME =
               WS-TRAD-TOTAL-TIME / WS-NUM-ITERATIONS.

           EXIT.

      *> RUN TRADITIONAL DIJKSTRA (INLINE)
       2100-RUN-TRADITIONAL-DIJKSTRA.
      *>   Initialize
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

      *>   Main loop
           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1
               UNTIL WS-LOOP-COUNTER > WS-NUM-NODES

               PERFORM 2110-FIND-MIN-UNVISITED

               IF WS-MIN-NODE = 0
                   EXIT PERFORM
               END-IF

               MOVE WS-MIN-NODE TO WS-CURRENT-NODE
               SET IDX-VISIT TO WS-CURRENT-NODE
               MOVE 'Y' TO WS-VISITED(IDX-VISIT)

               PERFORM 2120-UPDATE-NEIGHBORS
           END-PERFORM.

           EXIT.

       2110-FIND-MIN-UNVISITED.
           MOVE WS-INFINITY TO WS-MIN-DISTANCE.
           MOVE 0 TO WS-MIN-NODE.

           PERFORM VARYING WS-NEIGHBOR-NODE FROM 1 BY 1
               UNTIL WS-NEIGHBOR-NODE > WS-NUM-NODES
               SET IDX-VISIT TO WS-NEIGHBOR-NODE
               SET IDX-DIST TO WS-NEIGHBOR-NODE
               IF NODE-UNVISITED(IDX-VISIT)
                   IF WS-DISTANCES(IDX-DIST) < WS-MIN-DISTANCE
                       MOVE WS-DISTANCES(IDX-DIST) TO WS-MIN-DISTANCE
                       MOVE WS-NEIGHBOR-NODE TO WS-MIN-NODE
                   END-IF
               END-IF
           END-PERFORM.
           EXIT.

       2120-UPDATE-NEIGHBORS.
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

      *> TEST EVENTCHAINS IMPLEMENTATION
       3000-TEST-EVENTCHAINS-IMPL.
           PERFORM VARYING WS-CURRENT-ITERATION FROM 1 BY 1
               UNTIL WS-CURRENT-ITERATION > WS-NUM-ITERATIONS

               ACCEPT WS-START-TIME FROM TIME
               PERFORM 3100-RUN-EVENTCHAINS-DIJKSTRA
               ACCEPT WS-END-TIME FROM TIME

               COMPUTE WS-EC-CURRENT-TIME =
                   WS-END-TIME - WS-START-TIME
               END-COMPUTE

               ADD WS-EC-CURRENT-TIME TO WS-EC-TOTAL-TIME

               IF WS-EC-CURRENT-TIME < WS-EC-MIN-TIME
                   MOVE WS-EC-CURRENT-TIME TO WS-EC-MIN-TIME
               END-IF

               IF WS-EC-CURRENT-TIME > WS-EC-MAX-TIME
                   MOVE WS-EC-CURRENT-TIME TO WS-EC-MAX-TIME
               END-IF
           END-PERFORM.

           COMPUTE WS-EC-AVG-TIME =
               WS-EC-TOTAL-TIME / WS-NUM-ITERATIONS.

           EXIT.

      *> RUN EVENTCHAINS DIJKSTRA (INLINE)
       3100-RUN-EVENTCHAINS-DIJKSTRA.
           MOVE 'N' TO WS-ALGORITHM-COMPLETE.

      *>   EVENT 1: Initialize
           PERFORM 3110-EVENT-INITIALIZE.

      *>   Main loop
           PERFORM VARYING WS-ITERATION-COUNT FROM 1 BY 1
               UNTIL WS-ITERATION-COUNT > WS-NUM-NODES
                   OR ALGORITHM-COMPLETE

      *>       EVENT 2: Find minimum
               PERFORM 3120-EVENT-FIND-MINIMUM

               IF WS-MIN-NODE = 0
                   MOVE 'Y' TO WS-ALGORITHM-COMPLETE
                   EXIT PERFORM
               END-IF

      *>       EVENT 3: Mark visited
               MOVE WS-MIN-NODE TO WS-CURRENT-NODE
               PERFORM 3130-EVENT-MARK-VISITED

      *>       EVENT 4: Update neighbors
               PERFORM 3140-EVENT-UPDATE-NEIGHBORS
           END-PERFORM.

           EXIT.

       3110-EVENT-INITIALIZE.
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

       3120-EVENT-FIND-MINIMUM.
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

       3130-EVENT-MARK-VISITED.
           SET IDX-VISIT TO WS-CURRENT-NODE.
           MOVE 'Y' TO WS-VISITED(IDX-VISIT).
           EXIT.

       3140-EVENT-UPDATE-NEIGHBORS.
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

      *> CALCULATE COMPARISON METRICS
       4000-CALCULATE-COMPARISON.
           COMPUTE WS-TIME-DIFFERENCE =
               WS-EC-AVG-TIME - WS-TRAD-AVG-TIME.

           IF WS-TRAD-AVG-TIME > 0
               COMPUTE WS-PERCENTAGE-DIFF =
                   (WS-TIME-DIFFERENCE / WS-TRAD-AVG-TIME) * 100
               END-COMPUTE
           ELSE
               MOVE 0 TO WS-PERCENTAGE-DIFF
           END-IF.

           IF WS-TRAD-AVG-TIME < WS-EC-AVG-TIME
               MOVE 'Traditional' TO WS-FASTER-IMPL
           ELSE
               IF WS-TRAD-AVG-TIME > WS-EC-AVG-TIME
                   MOVE 'EventChains' TO WS-FASTER-IMPL
               ELSE
                   MOVE 'TIE' TO WS-FASTER-IMPL
               END-IF
           END-IF.

           EXIT.

      *> DISPLAY COMPARISON RESULTS
       5000-DISPLAY-RESULTS.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY ' PERFORMANCE COMPARISON RESULTS'.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY ' '.

           DISPLAY 'Test Configuration:'.
           DISPLAY '  Number of iterations: ' WS-NUM-ITERATIONS.
           DISPLAY '  Graph size: ' WS-NUM-NODES ' nodes'.
           DISPLAY '  Source node: ' WS-SOURCE-NODE.
           DISPLAY ' '.

           DISPLAY WS-DISPLAY-THIN-LINE.
           DISPLAY 'TRADITIONAL IMPLEMENTATION:'.
           DISPLAY WS-DISPLAY-THIN-LINE.
           DISPLAY '  Total time: ' WS-TRAD-TOTAL-TIME
               ' microseconds'.
           DISPLAY '  Average time: ' WS-TRAD-AVG-TIME
               ' microseconds'.
           DISPLAY '  Minimum time: ' WS-TRAD-MIN-TIME
               ' microseconds'.
           DISPLAY '  Maximum time: ' WS-TRAD-MAX-TIME
               ' microseconds'.
           DISPLAY ' '.

           DISPLAY WS-DISPLAY-THIN-LINE.
           DISPLAY 'EVENTCHAINS IMPLEMENTATION:'.
           DISPLAY WS-DISPLAY-THIN-LINE.
           DISPLAY '  Total time: ' WS-EC-TOTAL-TIME
               ' microseconds'.
           DISPLAY '  Average time: ' WS-EC-AVG-TIME
               ' microseconds'.
           DISPLAY '  Minimum time: ' WS-EC-MIN-TIME
               ' microseconds'.
           DISPLAY '  Maximum time: ' WS-EC-MAX-TIME
               ' microseconds'.
           DISPLAY ' '.

           DISPLAY WS-DISPLAY-THIN-LINE.
           DISPLAY 'COMPARISON ANALYSIS:'.
           DISPLAY WS-DISPLAY-THIN-LINE.
           DISPLAY '  Time difference: ' WS-TIME-DIFFERENCE
               ' microseconds'.
           DISPLAY '  Percentage difference: ' WS-PERCENTAGE-DIFF
               '%'.
           DISPLAY '  Faster implementation: ' WS-FASTER-IMPL.
           DISPLAY ' '.

           DISPLAY 'INTERPRETATION:'.
           IF WS-PERCENTAGE-DIFF < 5 AND WS-PERCENTAGE-DIFF > -5
               DISPLAY '  Performance is essentially equivalent.'
               DISPLAY '  Difference is within measurement variance.'
           ELSE
               IF WS-FASTER-IMPL = 'Traditional'
                   DISPLAY '  Traditional is faster by '
                       WS-PERCENTAGE-DIFF '%'
                   DISPLAY '  EventChains adds overhead from:'
                   DISPLAY '    - Event dispatching'
                   DISPLAY '    - Middleware wrapping'
                   DISPLAY '    - Additional function calls'
               ELSE
                   DISPLAY '  EventChains is faster by '
                       WS-PERCENTAGE-DIFF '%'
                   DISPLAY '  Unexpected result - possible compiler'
                   DISPLAY '  optimization of modular structure.'
               END-IF
           END-IF.

           DISPLAY ' '.
           DISPLAY WS-DISPLAY-LINE.
           EXIT.
