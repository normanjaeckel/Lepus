module AlgoTest exposing (suite)

import Algo
import Dict
import Expect
import Set
import Test exposing (..)


suite : Test
suite =
    let
        graph1 : Algo.Graph
        graph1 =
            Dict.fromList
                [ ( "P1", [ "A", "B" ] )
                , ( "P2", [ "B", "C" ] )
                , ( "P3", [ "A", "D" ] )
                , ( "P4", [ "D" ] )
                ]

        graph2 =
            Dict.fromList
                [ ( "P1", [ "Q3", "Q19", "Q11" ] )
                , ( "P2", [ "Q15", "Q14", "Q14" ] )
                , ( "P3", [ "Q11", "Q9", "Q10" ] )
                , ( "P4", [ "Q4", "Q16", "Q2" ] )
                , ( "P5", [ "Q10", "Q4", "Q15" ] )
                , ( "P6", [ "Q7", "Q19", "Q12" ] )
                , ( "P7", [ "Q14", "Q15", "Q11" ] )
                , ( "P8", [ "Q5", "Q10", "Q8" ] )
                , ( "P9", [ "Q16", "Q8", "Q20" ] )
                , ( "P10", [ "Q2", "Q16", "Q9" ] )
                , ( "P11", [ "Q20", "Q19", "Q6" ] )
                , ( "P12", [ "Q10", "Q10", "Q9" ] )
                , ( "P13", [ "Q4", "Q2", "Q17" ] )
                , ( "P14", [ "Q8", "Q20", "Q17" ] )
                , ( "P15", [ "Q1", "Q7", "Q19" ] )
                , ( "P16", [ "Q5", "Q13", "Q10" ] )
                , ( "P17", [ "Q19", "Q14", "Q10" ] )
                , ( "P18", [ "Q13", "Q13", "Q1" ] )
                , ( "P19", [ "Q18", "Q16", "Q5" ] )
                , ( "P20", [ "Q19", "Q19", "Q11" ] )
                ]

        graph3 =
            Dict.fromList
                [ ( "P01", [ "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P02", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12" ] )
                , ( "P03", [ "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P04", [ "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12" ] )
                , ( "P05", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P06", [ "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P07", [ "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P08", [ "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12" ] )
                , ( "P09", [ "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P10", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P11", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12" ] )
                , ( "P12", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12" ] )
                , ( "P13", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12" ] )
                , ( "P14", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P15", [ "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12" ] )
                , ( "P16", [ "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P17", [ "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12" ] )
                , ( "P18", [ "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P19", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12" ] )
                , ( "P20", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P21", [ "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12" ] )
                , ( "P22", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12" ] )
                , ( "P23", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P24", [ "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P25", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P26", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P27", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12" ] )
                , ( "P28", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P29", [ "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12" ] )
                , ( "P30", [ "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P31", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P32", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P33", [ "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P34", [ "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12" ] )
                , ( "P35", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12" ] )
                , ( "P36", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12" ] )
                , ( "P37", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P38", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12" ] )
                , ( "P39", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P40", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12" ] )
                , ( "P41", [ "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P42", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P43", [ "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P44", [ "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P45", [ "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12" ] )
                , ( "P46", [ "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12" ] )
                , ( "P47", [ "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P48", [ "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12" ] )
                , ( "P49", [ "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P50", [ "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P51", [ "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P52", [ "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P53", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12" ] )
                , ( "P54", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12" ] )
                , ( "P55", [ "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12" ] )
                , ( "P56", [ "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P57", [ "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P58", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P59", [ "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12" ] )
                , ( "P60", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P61", [ "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P62", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12" ] )
                , ( "P63", [ "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P64", [ "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P65", [ "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12" ] )
                , ( "P66", [ "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P67", [ "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P68", [ "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P69", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P70", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P71", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P72", [ "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P73", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12" ] )
                , ( "P74", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P75", [ "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q2-1", "Q2-2", "Q2-3", "Q2-4", "Q2-5", "Q2-6", "Q2-7", "Q2-8", "Q2-9", "Q2-10", "Q2-11", "Q2-12" ] )
                , ( "P76", [ "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                , ( "P77", [ "Q7-1", "Q7-2", "Q7-3", "Q7-4", "Q7-5", "Q7-6", "Q7-7", "Q7-8", "Q7-9", "Q7-10", "Q7-11", "Q7-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q6-1", "Q6-2", "Q6-3", "Q6-4", "Q6-5", "Q6-6", "Q6-7", "Q6-8", "Q6-9", "Q6-10", "Q6-11", "Q6-12" ] )
                , ( "P78", [ "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P79", [ "Q1-1", "Q1-2", "Q1-3", "Q1-4", "Q1-5", "Q1-6", "Q1-7", "Q1-8", "Q1-9", "Q1-10", "Q1-11", "Q1-12", "Q5-1", "Q5-2", "Q5-3", "Q5-4", "Q5-5", "Q5-6", "Q5-7", "Q5-8", "Q5-9", "Q5-10", "Q5-11", "Q5-12" ] )
                , ( "P80", [ "Q8-1", "Q8-2", "Q8-3", "Q8-4", "Q8-5", "Q8-6", "Q8-7", "Q8-8", "Q8-9", "Q8-10", "Q8-11", "Q8-12", "Q4-1", "Q4-2", "Q4-3", "Q4-4", "Q4-5", "Q4-6", "Q4-7", "Q4-8", "Q4-9", "Q4-10", "Q4-11", "Q4-12", "Q3-1", "Q3-2", "Q3-3", "Q3-4", "Q3-5", "Q3-6", "Q3-7", "Q3-8", "Q3-9", "Q3-10", "Q3-11", "Q3-12" ] )
                ]
    in
    describe "Main functions"
        [ test "Test run for graph 1" <|
            \_ ->
                Algo.run graph1 Dict.empty
                    |> Expect.equal
                        (Dict.fromList
                            [ ( "P1", "B" )
                            , ( "P2", "C" )
                            , ( "P3", "A" )
                            , ( "P4", "D" )
                            ]
                        )
        , test "Test run for graph 2" <|
            \_ ->
                Algo.run graph2 Dict.empty
                    |> Expect.equal
                        (Dict.fromList
                            [ ( "P1", "Q3" )
                            , ( "P10", "Q2" )
                            , ( "P11", "Q6" )
                            , ( "P12", "Q10" )
                            , ( "P13", "Q17" )
                            , ( "P14", "Q20" )
                            , ( "P15", "Q7" )
                            , ( "P16", "Q13" )
                            , ( "P17", "Q19" )
                            , ( "P18", "Q1" )
                            , ( "P19", "Q18" )
                            , ( "P2", "Q15" )
                            , ( "P20", "Q11" )
                            , ( "P3", "Q9" )
                            , ( "P4", "Q16" )
                            , ( "P5", "Q4" )
                            , ( "P6", "Q12" )
                            , ( "P7", "Q14" )
                            , ( "P8", "Q5" )
                            , ( "P9", "Q8" )
                            ]
                        )
        , test "Test run for graph 3" <|
            \_ ->
                Algo.run graph3 Dict.empty
                    |> Expect.all
                        [ Dict.toList >> List.length >> Expect.equal 80
                        , invertDict >> Dict.toList >> List.length >> Expect.equal 80
                        , Dict.toList >> List.foldl (\t s -> s |> Set.insert (Tuple.second t)) Set.empty >> Set.toList >> List.length >> Expect.equal 80
                        , Expect.equal
                            (Dict.fromList
                                [ ( "P01", "Q5-1" )
                                , ( "P02", "Q8-1" )
                                , ( "P03", "Q7-1" )
                                , ( "P04", "Q3-1" )
                                , ( "P05", "Q8-2" )
                                , ( "P06", "Q2-1" )
                                , ( "P07", "Q2-2" )
                                , ( "P08", "Q2-3" )
                                , ( "P09", "Q6-1" )
                                , ( "P10", "Q4-1" )
                                , ( "P11", "Q8-3" )
                                , ( "P12", "Q4-2" )
                                , ( "P13", "Q4-3" )
                                , ( "P14", "Q1-1" )
                                , ( "P15", "Q7-2" )
                                , ( "P16", "Q2-4" )
                                , ( "P17", "Q3-2" )
                                , ( "P18", "Q2-5" )
                                , ( "P19", "Q8-4" )
                                , ( "P20", "Q1-2" )
                                , ( "P21", "Q6-2" )
                                , ( "P22", "Q8-5" )
                                , ( "P23", "Q4-4" )
                                , ( "P24", "Q6-3" )
                                , ( "P25", "Q4-5" )
                                , ( "P26", "Q1-3" )
                                , ( "P27", "Q1-4" )
                                , ( "P28", "Q1-5" )
                                , ( "P29", "Q6-4" )
                                , ( "P30", "Q3-3" )
                                , ( "P31", "Q8-6" )
                                , ( "P32", "Q1-6" )
                                , ( "P33", "Q3-4" )
                                , ( "P34", "Q5-2" )
                                , ( "P35", "Q8-7" )
                                , ( "P36", "Q4-6" )
                                , ( "P37", "Q1-7" )
                                , ( "P38", "Q8-8" )
                                , ( "P39", "Q4-7" )
                                , ( "P40", "Q1-8" )
                                , ( "P41", "Q3-5" )
                                , ( "P42", "Q4-8" )
                                , ( "P43", "Q2-6" )
                                , ( "P44", "Q5-3" )
                                , ( "P45", "Q3-6" )
                                , ( "P46", "Q5-4" )
                                , ( "P47", "Q6-5" )
                                , ( "P48", "Q3-7" )
                                , ( "P49", "Q6-6" )
                                , ( "P50", "Q5-5" )
                                , ( "P51", "Q3-8" )
                                , ( "P52", "Q6-7" )
                                , ( "P53", "Q4-9" )
                                , ( "P54", "Q4-10" )
                                , ( "P55", "Q7-3" )
                                , ( "P56", "Q6-8" )
                                , ( "P57", "Q2-7" )
                                , ( "P58", "Q8-9" )
                                , ( "P59", "Q2-8" )
                                , ( "P60", "Q1-9" )
                                , ( "P61", "Q3-9" )
                                , ( "P62", "Q8-10" )
                                , ( "P63", "Q5-6" )
                                , ( "P64", "Q3-10" )
                                , ( "P65", "Q6-9" )
                                , ( "P66", "Q6-10" )
                                , ( "P67", "Q5-7" )
                                , ( "P68", "Q7-4" )
                                , ( "P69", "Q1-10" )
                                , ( "P70", "Q1-11" )
                                , ( "P71", "Q1-12" )
                                , ( "P72", "Q5-8" )
                                , ( "P73", "Q8-11" )
                                , ( "P74", "Q4-11" )
                                , ( "P75", "Q7-5" )
                                , ( "P76", "Q5-9" )
                                , ( "P77", "Q7-6" )
                                , ( "P78", "Q4-12" )
                                , ( "P79", "Q5-10" )
                                , ( "P80", "Q8-12" )
                                ]
                            )
                        ]
        ]


invertDict : Dict.Dict String String -> Dict.Dict String String
invertDict input =
    input
        |> Dict.toList
        |> List.map (\t -> ( Tuple.second t, Tuple.first t ))
        |> Dict.fromList
