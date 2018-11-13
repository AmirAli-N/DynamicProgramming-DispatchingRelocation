Imports System.Math
Imports System.IO
Imports System.Text
Imports DotNumerics.Optimization
Imports DotNumerics.LinearAlgebra
Imports DotNumerics.LinearAlgebra.CSLapack
Imports MathNet.Numerics.Integration
Imports MathNet.Numerics.Distributions
Imports MathNet.Numerics.SpecialFunctions
Imports System.Data
Public Class Form1
    Dim dynamic_ambulance_number As Integer = 17
    Dim next_action As List(Of Double)
    Dim fel As New List(Of List(Of Double))
    Dim call_num As Integer
    Dim current_event As Integer
    Dim clock As Double
    Dim arrival_clock As Double
    Dim locations As Integer
    Dim priority As Integer
    Dim i As Double
    Dim z As Double
    Public M_state As New List(Of IList(Of Integer))
    Public C_state As New List(Of IList(Of Double))
    Dim last_num_of_call As Integer
    Public last_origin(dynamic_ambulance_number - 1) As Integer
    'Dim lambda_of_l(5, 5) As Double
    Dim amb_list(dynamic_ambulance_number - 1) As Double
    Public amb_origin_base(dynamic_ambulance_number - 1) As Integer
    Dim closer_amb_distance As New List(Of List(Of Double))
    Dim row As Integer
    Public seed As Integer
    Public weed As Integer
    Public leed As Integer
    Public reed As Integer
    Public zeed As Integer
    Dim r1 As New Random(seed)
    Dim r2 As New Random(weed)
    Dim r3 As New Random(leed)
    Dim r4 As New Random(reed)
    Dim r5 As New Random(zeed)
    Dim random_num As New Random()
    Dim replications As Integer
    Dim visited_list As New List(Of List(Of String))
    Dim lambda_prime As New List(Of List(Of Double))
    Dim filepath As String
    Dim prob_priority As Double = 0.86
    Dim prob_scene_hospital As Double = 0.73
    Dim lambda_finish_at_scene As Double = 1 / 17.1
    Dim lambda_service_at_hospital As Double = 1 / 56.66
    Dim alpha() As Double = {1, 1, 1, 1, 1, 1, 1, 1} ' as in myopic policy
    Dim im_cost As Double
    Dim e_cost As Double
    Dim total_cost As New List(Of List(Of Double))
    Public c_of_r As New List(Of List(Of Double))
    Public c_of_r_loss As New List(Of List(Of Double))
    Dim policy As New Dictionary(Of String, Double)
    Dim value_err As Double
    Dim temp_value As Double
    Public number_lost_calls As New List(Of List(Of Double))
    Public number_lost_calls_pr As New List(Of List(Of Double))
    Dim avg_lost_calls As New List(Of Double)
    Dim avg_lost_calls_pr As New List(Of Double)
    Dim miss_call_num_lp As Double
    Dim miss_call_num_hp As Double
    Dim total_call_nom As Integer
    Dim total_hp_call_nom As Double
    Dim total_lp_call_nom As Double
    Public inner_iteration As Integer
    Dim reps As Integer
    Dim last_rep As Integer
    Dim temp_time As Double
    Dim temp_loss_time As Double
    Dim response_time As Double
    Dim lost_percent As Double
    Dim lost_percent_pr As Double
    Dim busy_time_fraction_list As New List(Of List(Of Double))
    Dim busy_time As Double
    Dim busy_time_start As Double
    Dim busy_time_end As Double
    Dim node_lambdas As New List(Of Double)
    Dim travel_lambdas As New List(Of List(Of Double))
    Dim node_distances As New List(Of List(Of Double))
    Dim ambulance_initial_location As New List(Of List(Of Integer))
    Dim factorl As New List(Of Double)
    Dim ambulance_potential_base As New List(Of Integer)
    Dim coverage As Double
    Dim time_interval As Double
    Dim final_coverage As New List(Of List(Of Double))
    Dim last_clock As Double
    Dim region_node_list As New List(Of List(Of Integer))
    Dim region_lambdas As New List(Of Double)
    Dim travel_lambdas_regions As New List(Of List(Of Double))
    Dim param_list As New List(Of List(Of Double))
    Dim all_states As New List(Of List(Of List(Of Integer)))
    Dim min_index As New Integer
    Dim integral_list As New List(Of List(Of Double))
    Dim arrival_list As New List(Of List(Of List(Of Double)))
    Dim cost_sb As New StringBuilder
    Dim iteration As Integer
    Dim best_miss_call As New List(Of Double)
    Dim best_resp_time As New List(Of Double)
    Dim amb_ini_base As New List(Of Integer)
    Dim hp_weight As Double = 1
    Dim Lp_weight As Double = 0.1
    Dim static_policy As String = "MCLP"
    Dim total_lp_call_nom_list As New List(Of List(Of Double))
    Dim total_hp_call_nom_list As New List(Of List(Of Double))
    Dim queue_time As New List(Of Double)
    Public Sub input_read()
        ''''''''''''''''''''''''''''''''''''''''reading call arrival lambdas'''''''''''''''''''''''''''''''''''
        Dim sr As StreamReader = New StreamReader("c:\users\snasrol\desktop\input\Node Lambdas for Calls.txt", True)
        Dim enumerator As Integer
        'node_lambdas.Add(0)
        While sr.Peek() <> -1
            enumerator += 1
            Dim first_line As String
            first_line = sr.ReadLine()
            If enumerator = Convert.ToInt32(first_line.Substring(0, first_line.IndexOf(vbTab))) Then
                node_lambdas.Add(Convert.ToDouble(first_line.Substring(first_line.IndexOf(vbTab) + 1)))
            Else
                While enumerator <> Convert.ToInt32(first_line.Substring(0, first_line.IndexOf(vbTab)))
                    node_lambdas.Add(0)
                    enumerator += 1
                End While
                node_lambdas.Add(Convert.ToDouble(first_line.Substring(first_line.IndexOf(vbTab) + 1)))
            End If
        End While
        '''''''''''''''''''''''''''''''''''''''''reading regions and their nodes''''''''''''''''''''''''''''''
        sr = New StreamReader("c:\users\snasrol\desktop\input\regions.txt", True)
        While sr.Peek <> -1
            Dim line As String = sr.ReadLine()
            Dim nodes As New List(Of Integer)
            Dim line_sub As String = line.Substring(line.IndexOf(vbTab) + 1, line.Count() - line.IndexOf(vbTab) - 1)
            For i As Integer = 0 To line_sub.Count() - 1 Step 1
                If line_sub.Contains(" ") Then
                    If line_sub(i) = " " Then
                        nodes.Add(Convert.ToInt32(line_sub.Substring(0, i)))
                        line_sub = line_sub.Remove(0, i + 1)
                        i = 0
                    End If
                Else
                    nodes.Add(Convert.ToInt32(line_sub.Substring(0, line_sub.Count())))
                    Exit For
                End If
            Next
            region_node_list.Add(nodes)
        End While
        ''''''''''''''''''''''''''''''''''reading lambdas of call arriving for regions''''''''''''''''''''
        For i As Integer = 0 To region_node_list.Count() - 1 Step 1
            Dim temp_lambda As New Double
            For j As Integer = 0 To region_node_list(i).Count() - 1 Step 1
                If node_lambdas(region_node_list(i).Item(j) - 1) <> 0 Then
                    temp_lambda = temp_lambda + 1 / node_lambdas(region_node_list(i).Item(j) - 1)
                End If
            Next
            region_lambdas.Add(temp_lambda)
        Next
        ''''''''''''''''''''''''''''''reading travel time lambdas'''''''''''''''''''''''''''''''
        sr = New StreamReader("c:\users\snasrol\desktop\input\Travel Table-Fixed.txt", True)
        While sr.Peek <> -1
            Dim line As String = sr.ReadLine()
            Dim first_row As New List(Of Double)
            For i As Integer = 1 To 168 Step 1
                If i = 168 Then
                    first_row.Add(Convert.ToDouble(line.Substring(0)))
                Else
                    first_row.Add(Convert.ToDouble(line.Substring(0, line.IndexOf(vbTab))))
                    line = line.Remove(0, line.IndexOf(vbTab) + 1)
                End If
            Next
            travel_lambdas.Add(first_row)
        End While
        ''''''''''''''''''''''''''''''''''''''''''''reading travel table based on region centers'''''''''''''''''''''''''
        Dim region_center As New List(Of Integer)
        sr = New StreamReader("c:\users\snasrol\desktop\input\Regions.txt")
        While sr.Peek <> -1
            Dim line As String = sr.ReadLine()
            Dim center As Integer = Convert.ToInt32(line.Substring(line.IndexOf("-") + 1, line.IndexOf(vbTab) - line.IndexOf("-") - 1))
            region_center.Add(center)
        End While
        For i As Integer = 0 To region_center.Count() - 1 Step 1
            Dim lambda_list As New List(Of Double)
            For j As Integer = 0 To region_center.Count() - 1 Step 1
                lambda_list.Add(travel_lambdas(region_center(i) - 1).Item(region_center(j) - 1))
            Next
            travel_lambdas_regions.Add(lambda_list)
        Next

        '''''''''''''''''''''''reading distance between each pair of nodes''''''''''''''''''''''''
        sr = New StreamReader("c:\users\snasrol\desktop\input\Node Distances.txt", True)
        While sr.Peek <> -1
            Dim line As String = sr.ReadLine()
            Dim first_row As New List(Of Double)
            For i As Integer = 1 To 168 Step 1
                If i = 168 Then
                    first_row.Add(Convert.ToDouble(line.Substring(0)))
                Else
                    first_row.Add(Convert.ToDouble(line.Substring(0, line.IndexOf(vbTab))))
                    line = line.Remove(0, line.IndexOf(vbTab) + 1)
                End If
            Next
            node_distances.Add(first_row)
        End While
        ''''''''''''''''''''''''''''''''''''''''reading ambulance initial location''''''''''''''''''''''''''''
        sr = New StreamReader("c:\users\snasrol\desktop\input\Ambulance Initial Location.txt", True)
        While sr.Peek <> -1
            Dim line As String = sr.ReadLine()
            Dim amb_ini_loc_tmp As New List(Of Integer)
            amb_ini_loc_tmp.Add(Convert.ToInt32(line.Substring(0, line.IndexOf(vbTab)))) 'location of base
            amb_ini_loc_tmp.Add(Convert.ToInt32(line.Substring(line.IndexOf(vbTab)))) 'number of ambs
            ambulance_initial_location.Add(amb_ini_loc_tmp)
        End While

        For t As Integer = 0 To 17 Step 1
            factorl.Add(factorial(t))
        Next
        ''''''''''''''''''''''''''''''''''''''''''reading potential amb bases '''''''''''''''''''''''''''''''''
        sr = New StreamReader("c:\users\snasrol\desktop\input\Potential Amb Base.txt", True)
        While sr.Peek <> -1
            Dim line As String = sr.ReadLine()
            ambulance_potential_base.Add(Convert.ToInt32(line))
        End While

        'For i As Integer = 0 To Me.TextBox2.Text - 1 Step 1
        '    sr = New StreamReader("C:\users\snasrol\desktop\output\Arrivals\" + i.ToString() + ".txt", True)
        '    Dim arrline As String
        '    Dim arr_list As New List(Of List(Of Double))
        '    While sr.Peek <> -1
        '        arrline = sr.ReadLine()
        '        Dim arrive As New List(Of Double)
        '        arrive.Add(arrline.Substring(0, arrline.IndexOf(vbTab)))
        '        arrive.Add(arrline.Substring(arrline.IndexOf(vbTab)))
        '        arr_list.Add(arrive)
        '    End While
        '    arrival_list.Add(arr_list)
        'Next
    End Sub
    Public Function expo_random_generator(ByVal lambda As Double)
        expo_random_generator = (-1 / lambda) * Math.Log(1 - r1.NextDouble())
    End Function
    Public Function normal_random_generator(ByVal mean As Double, stddev As Double)
        Return MathNet.Numerics.Distributions.Normal.Sample(r2, mean, stddev)
    End Function
    Private Sub closer_amb_initialization()
        For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
            Dim a As New List(Of Double)
            a.Add(0)
            a.Add(0)
            closer_amb_distance.Add(a)
        Next
    End Sub
    Private Sub amb_initial_state()
        Dim amb_state As New List(Of Integer)
        Dim inumerator As Integer = 0
        For t As Integer = 0 To ambulance_initial_location.Count() - 1 Step 1
            For k As Integer = 0 To ambulance_initial_location(t).Item(1) - 1 Step 1
                amb_state = New List(Of Integer)
                M_state.Add(amb_state) 'ambulance 2
                M_state(inumerator).Add(0)
                M_state(inumerator).Add(ambulance_initial_location(t).Item(0))
                amb_ini_base.Add(ambulance_initial_location(t).Item(0)) 'just to save the initial base of ambulance in a vector which its size is equal to the number of ambs
                last_origin(inumerator) = ambulance_initial_location(t).Item(0)
                amb_origin_base(inumerator) = ambulance_initial_location(t).Item(0)
                M_state(inumerator).Add(0)
                inumerator += 1
            Next
        Next
    End Sub
    Private Function call_generator() As Double
        Dim discount_factor As Double = 0.99999
        Dim call_reigon(167) As Double
        Dim t As Integer
        current_event = 0
        call_num += 1
        For q As Integer = 0 To node_lambdas.Count() - 1 Step 1
            If node_lambdas(q) <> 0 Then
                call_reigon(q) = expo_random_generator((1 / node_lambdas(q)))
            End If
        Next
        Dim calls As New List(Of Double)
        For q As Integer = 0 To call_reigon.Count() - 1 Step 1
            If call_reigon(q) <> 0 Then
                calls.Add(call_reigon(q))
            End If
        Next
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        arrival_clock = calls.Min()
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        For t = 0 To call_reigon.Count() - 1 Step 1
            If call_reigon(t) = arrival_clock Then
                locations = t
            End If
        Next
        If r3.NextDouble > prob_priority Then
            priority = 1
            total_hp_call_nom += 1 * priority_weight(priority) * Pow(discount_factor, clock + arrival_clock)
        Else
            priority = 0
            total_lp_call_nom += 1 * priority_weight(priority) * Pow(discount_factor, clock + arrival_clock)
        End If
        call_state_update(0, call_num, locations, priority, clock + arrival_clock)
        Return arrival_clock
    End Function
    Private Sub FEL_Builder(ByVal action As Integer, number_of_amb As Integer, ByVal destination As Integer)
        Dim B As List(Of Double) = New List(Of Double)
        B.Add(action)
        Select Case action
            Case 1
                B.Add(clock + call_generator()) ''done for the 168 nodes ''creating call arrival
            Case 2
                ''fixed travel times from "travel table-fixed.txt"
                B.Add(clock + travel_lambdas(last_origin(number_of_amb)).Item(destination))
                ''lambdas from "travel table.txt"
                'B.Add(clock + expo_random_generator(1 / travel_lambdas(last_origin(number_of_amb)).Item(destination))) ''amb dispatching
                amb_list(number_of_amb) = B(1)
            Case 3
                B.Add(clock + normal_random_generator(54.18, 15.8)) ''amb finishing on the scene
                amb_list(number_of_amb) = B(1)
            Case 4
                B.Add(clock + normal_random_generator(56.7, 13.6)) ''amb finishing at hospital
                amb_list(number_of_amb) = B(1)
            Case 5
                B.Add(clock + travel_lambdas(last_origin(number_of_amb)).Item(amb_origin_base(number_of_amb))) ''amb arriving to base
                amb_list(number_of_amb) = B(1)
        End Select
        fel.Add(B)
    End Sub
    Private Sub call_state_update(ByVal status As Integer, ByVal number_of_call As Integer, ByVal location_of_call As Integer, ByVal priority_of_call As Integer, ByVal time_of_creation As Double)
        Dim call_state As New List(Of Double)
        If number_of_call <= last_num_of_call Then
            If status = 1 And C_state(number_of_call).Item(0) <> 1 Then 'if the status of the call has changed from unassigned to assigned
                queue_time.Add(clock - C_state(number_of_call).Item(3)) 'add the queue time
            End If
            C_state(number_of_call).Item(0) = status
            C_state(number_of_call).Item(1) = location_of_call
            C_state(number_of_call).Item(2) = priority_of_call
        Else
            If status = 0 Then
                call_state.Add(status)
                call_state.Add(location_of_call)
                call_state.Add(priority_of_call)
                call_state.Add(time_of_creation)
                C_state.Add(call_state)
            End If
            last_num_of_call += 1
        End If
    End Sub
    Private Sub amb_state_update(ByVal number_of_amb As Integer, ByVal destination As Integer)
        If destination = 168 Then '''''''''''''''''''''''''''''''''''''''''''''ambulance(num_of_amb) is going to hospital ''done for 168 nodes
            M_state(number_of_amb).Item(0) = 3
            M_state(number_of_amb).Item(1) = last_origin(number_of_amb)
            Dim hospital As Integer = Int(r4.NextDouble() * 3)
            If hospital = 0 Then
                M_state(number_of_amb).Item(2) = 91
            ElseIf hospital = 1 Then
                M_state(number_of_amb).Item(2) = 37
            Else
                M_state(number_of_amb).Item(2) = 66
            End If
        End If
        If destination = 169 Then ''''''''''''''''''''''''''''''''''''''''''''ambulance(num_of_amb) is at the scene ''done for 168 nodes
            M_state(number_of_amb).Item(0) = 2
            M_state(number_of_amb).Item(1) = last_origin(number_of_amb)
            M_state(number_of_amb).Item(2) = last_origin(number_of_amb)
        End If
        If destination <= 167 Then '''''''''''''''''''''''''''''''''''''''''''ambulance(num_of_amb) dispatched ''done for 168 nodes
            M_state(number_of_amb).Item(0) = 1
            M_state(number_of_amb).Item(1) = last_origin(number_of_amb)
            M_state(number_of_amb).Item(2) = destination
        End If
        If destination = 170 Then ''''''''''''''''''''''''''''''''''''''''''''ambulance (number_of_amb) going to base ''done for 168 nodes
            M_state(number_of_amb).Item(0) = 4
            M_state(number_of_amb).Item(1) = last_origin(number_of_amb)
            M_state(number_of_amb).Item(2) = amb_origin_base(number_of_amb)
        End If
        If destination = 171 Then
            M_state(number_of_amb).Item(0) = 0 '''''''''''''''''''''''''''''ambulance (number_of_amb) is at the base
            M_state(number_of_amb).Item(1) = amb_origin_base(number_of_amb)
            M_state(number_of_amb).Item(2) = 0
        End If
        If destination >= 172 Then
            M_state(number_of_amb).Item(0) = 5
            M_state(number_of_amb).Item(1) = last_origin(number_of_amb)
            M_state(number_of_amb).Item(2) = destination - 172
        End If
    End Sub
    Private Sub main()
        Dim amb_num As Integer
        next_action = (From i In fel Order By i(1) Ascending Select i).First()
        clock = next_action(1)
        '''''''''''''''''''''''''''''''''''''''''''''''''''''busy time
        Dim all_amb_busy As Boolean
        For j As Integer = 0 To M_state.Count() - 1 Step 1
            If M_state(j).Item(0) = 0 Then
                all_amb_busy = False
                Exit For
            Else
                all_amb_busy = True
            End If
        Next
        If all_amb_busy = False And busy_time_start <> 0 Then
            busy_time_end = clock
            busy_time = busy_time + busy_time_end - busy_time_start
            busy_time_start = 0
        Else
            If all_amb_busy = True And busy_time_start = 0 Then
                busy_time_start = clock
            End If
        End If
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Select Case next_action(i)
            Case 1
                sampling_state(0, 50)
                call_arrival()
                'trace_current()
                'trace_fel()
            Case 2
                Dim rand As New Double
                rand = r5.NextDouble
                If rand > prob_scene_hospital Then 'deciding if ambulance stays on the scene
                    For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                        If clock = amb_list(t) Then
                            'trace_current()
                            ambulance_served_at_scene(t)
                            sampling_state(2, t)
                            'trace_fel()
                            amb_num = t
                            Exit For
                        End If
                    Next
                Else
                    For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1 'ambulance goes to hospital
                        If clock = amb_list(t) Then
                            'trace_current()
                            amb_to_hospital(t)
                            sampling_state(2, t)
                            'trace_fel()
                            amb_num = t
                            Exit For
                        End If
                    Next
                End If
                If C_state.Count() - 1 >= 1 Then
                    For t As Integer = 0 To C_state.Count() - 1 Step 1
                        If C_state(t).Item(1) = M_state(amb_num).Item(1) Then
                            If C_state(t).Item(3) < TextBox1.Text Then 'And C_state(t).Item(2) = 1 Then
                                lost_calls(reps, clock, C_state(t).Item(3), C_state(t).Item(2)) ''calculating waiting time for each call
                                If clock - C_state(t).Item(3) > 200 Then
                                    Dim ali = 1
                                End If
                            End If
                            C_state.RemoveAt(t)
                            Exit For
                        End If
                    Next
                End If
            Case 3
                For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                    If clock = amb_list(t) Then
                        'trace_current()
                        finished_at_scene(t)
                        sampling_state(3, t)
                        'trace_fel()
                        Exit For
                    End If
                Next
            Case 4
                For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                    If clock = amb_list(t) Then
                        'trace_current()
                        finished_at_hospital(t)
                        sampling_state(4, t)
                        'trace_fel()
                        Exit For
                    End If
                Next
            Case 5
                For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                    If clock = amb_list(t) Then
                        'trace_current()
                        arrives_at_base(t)
                        sampling_state(5, t)
                        'trace_fel()
                        Exit For
                    End If
                Next
        End Select
        fel.Remove(next_action)
    End Sub
    Private Sub call_arrival() 'being called when a call arrives
        Dim k As Integer ''as number of unassigned call in the queue
        Dim l As Integer ''as number of ambulance
        Dim min_distance As Double
        Dim dispatch_bool As Boolean = False
        FEL_Builder(1, vbNull, vbNull)
        total_call_nom += 1
        Dim PQueue As Boolean
        For Each t In C_state 'checks if there's a call in the queue
            If C_state(k).Item(0) = 1 Then
                PQueue = False
                k = k + 1
            Else
                If C_state(k).Item(3) <= clock Then
                    PQueue = True
                    Exit For
                End If
            End If
        Next
        ''k= number of the unassigned call
        If PQueue = True Then
            For Each t In M_state 'check if there's any ambulance available
                If M_state(l).Item(0) = 0 Then
                    closer_amb_distance(l).Item(0) = 0 ''number of the ambulance
                    closer_amb_distance(l).Item(1) = node_distances(M_state(l).Item(1)).Item(C_state(k).Item(1)) ''the distance between amb node and call node
                    l = l + 1
                Else
                    closer_amb_distance(l).Item(0) = 1 'differentiating between available and unavailab
                    closer_amb_distance(l).Item(1) = 1000
                    l = l + 1
                End If
            Next
            min_distance = (From z In closer_amb_distance Order By z(1) Ascending Select z(1)).ElementAt(0) ''finding the minimum distance
            For n As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                If closer_amb_distance(n).Item(1) = min_distance And closer_amb_distance(n).Item(0) = 0 Then
                    l = n
                    Exit For
                Else
                    If min_distance = 1000 Then
                        GoTo 0
                    End If
                End If
            Next
            For t As Integer = 0 To C_state.Count() - 1 Step 1
                If C_state(t).Item(0) = 0 And C_state(t).Item(3) <= clock Then 'if a call is unassigned
                    If C_state(t).Item(2) = 1 Then 'if that call is high priority
                        ambulance_dispatching(l, C_state(t).Item(1))
                        call_state_update(1, t, C_state(t).Item(1), C_state(t).Item(2), vbNull)
                        sampling_state(1, l)
                        dispatch_bool = True
                        Exit Sub
                    End If
                End If
            Next
            'if no high priority is found, dispatch to first unassigned call
            If dispatch_bool = False Then
                ambulance_dispatching(l, C_state(k).Item(1)) 'call to dispatch an ambulance
                call_state_update(1, k, C_state(k).Item(1), C_state(k).Item(2), vbNull) 'update the state of the call
                sampling_state(1, l)
            End If
0:      End If
    End Sub
    Private Sub ambulance_dispatching(ByVal number_of_amb As Integer, ByVal destination As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(1)
        amb_state_update(number_of_amb, destination)
        FEL_Builder(2, number_of_amb, destination)
        calculate_coverage()
    End Sub
    Private Sub ambulance_served_at_scene(ByVal number_of_amb As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(2)
        amb_state_update(number_of_amb, 169)
        FEL_Builder(3, number_of_amb, vbNull)
    End Sub
    Private Sub amb_to_hospital(ByVal number_of_amb As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(2)
        amb_state_update(number_of_amb, 168)
        FEL_Builder(4, number_of_amb, vbNull)
    End Sub
    Private Sub finished_at_scene(ByVal number_of_amb As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(2)
        Dim call_in_queue As Boolean = False
        Dim first_unassigned_call_ind As Integer
        Dim dispatch_amb As Boolean = False
        For t As Integer = 0 To C_state.Count() - 1 Step 1
            If C_state(t).Item(0) = 0 And C_state(t).Item(3) <= clock Then
                call_in_queue = True
                first_unassigned_call_ind = t
                If C_state(t).Item(2) = 1 Then 'dispatch for high priority
                    amb_state_update(number_of_amb, C_state(t).Item(1)) ''re assign after finishing
                    call_state_update(1, t, C_state(t).Item(1), C_state(t).Item(2), C_state(t).Item(3))
                    ambulance_dispatching(number_of_amb, C_state(t).Item(1))
                    dispatch_amb = True
                    Exit For
                End If
            End If
        Next
        If call_in_queue = True And dispatch_amb = False Then 'if no high priority call is found but there are calls in the queue
            amb_state_update(number_of_amb, C_state(first_unassigned_call_ind).Item(1)) ''re assign after finishing
            call_state_update(1, first_unassigned_call_ind, C_state(first_unassigned_call_ind).Item(1), C_state(first_unassigned_call_ind).Item(2), C_state(first_unassigned_call_ind).Item(3))
            ambulance_dispatching(number_of_amb, C_state(first_unassigned_call_ind).Item(1))
        End If
        If call_in_queue = False Then
            amb_state_update(number_of_amb, 170)
            amb_to_base(number_of_amb)
        End If
    End Sub
    Private Sub finished_at_hospital(ByVal number_of_amb As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(2)
        Dim call_in_queue As Boolean = False
        Dim first_unassigned_call_ind As Integer
        Dim dispatch_amb As Boolean = False
        For t As Integer = 0 To C_state.Count() - 1 Step 1
            If C_state(t).Item(0) = 0 And C_state(t).Item(3) <= clock Then
                call_in_queue = True
                first_unassigned_call_ind = t
                If C_state(t).Item(2) = 1 Then 'dispatch for high priority
                    amb_state_update(number_of_amb, C_state(t).Item(1)) ''re assign after finishing
                    call_state_update(1, t, C_state(t).Item(1), C_state(t).Item(2), C_state(t).Item(3))
                    ambulance_dispatching(number_of_amb, C_state(t).Item(1))
                    dispatch_amb = True
                    Exit For
                End If
            End If
        Next
        If call_in_queue = True And dispatch_amb = False Then 'if no high priority call is found but there are calls in the queue
            amb_state_update(number_of_amb, C_state(first_unassigned_call_ind).Item(1)) ''re assign after finishing
            call_state_update(1, first_unassigned_call_ind, C_state(first_unassigned_call_ind).Item(1), C_state(first_unassigned_call_ind).Item(2), C_state(first_unassigned_call_ind).Item(3))
            ambulance_dispatching(number_of_amb, C_state(first_unassigned_call_ind).Item(1))
        End If
        If call_in_queue = False Then
            amb_state_update(number_of_amb, 170)
            amb_to_base(number_of_amb)
        End If
    End Sub
    Private Sub amb_to_base(ByVal number_of_amb As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(1)
        amb_state_update(number_of_amb, 170)
        FEL_Builder(5, number_of_amb, vbNull)
    End Sub
    Private Sub arrives_at_base(ByVal number_of_amb As Integer)
        amb_state_update(number_of_amb, 171)
        calculate_coverage()
        '''''''''''''''''''''
        Dim call_in_queue As Boolean = False
        Dim first_unassigned_call_ind As Integer
        Dim dispatch_amb As Boolean = False
        For t As Integer = 0 To C_state.Count() - 1 Step 1
            If C_state(t).Item(0) = 0 And C_state(t).Item(3) <= clock Then
                call_in_queue = True
                first_unassigned_call_ind = t
                If C_state(t).Item(2) = 1 Then 'dispatch for high priority
                    'amb_state_update(number_of_amb, C_state(t).Item(1)) ''re assign after finishing
                    call_state_update(1, t, C_state(t).Item(1), C_state(t).Item(2), C_state(t).Item(3))
                    ambulance_dispatching(number_of_amb, C_state(t).Item(1))
                    dispatch_amb = True
                    Exit For
                End If
            End If
        Next
        If call_in_queue = True And dispatch_amb = False Then 'if no high priority call is found but there are calls in the queue
            amb_state_update(number_of_amb, C_state(first_unassigned_call_ind).Item(1)) ''re assign after finishing
            call_state_update(1, first_unassigned_call_ind, C_state(first_unassigned_call_ind).Item(1), C_state(first_unassigned_call_ind).Item(2), C_state(first_unassigned_call_ind).Item(3))
            ambulance_dispatching(number_of_amb, C_state(first_unassigned_call_ind).Item(1))
        End If
        'If call_in_queue = False Then
        'amb_state_update(number_of_amb, 170)
        'amb_to_base(number_of_amb)
        'End If
    End Sub
    Private Sub trace_fel()
        'Form2.DataGridView1.Rows.Add(1)
        Dim h As Integer
        Dim t As Integer = (From i In fel Order By i(0) Ascending Select i(0)).Count() - 1
        For k As Integer = 1 To t Step 1
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 1 Then
                Form2.DataGridView1.Item(2, row - 1).Value = Form2.DataGridView1.Item(2, row - 1).Value + " - " + "next call arrival" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 2 Then
                For n As Integer = 0 To dynamic_ambulance_number Step 1
                    If amb_list(n) = (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k) Then
                        h = n
                        Exit For
                    End If
                Next
                Form2.DataGridView1.Item(2, row - 1).Value = Form2.DataGridView1.Item(2, row - 1).Value + " - " + "ambulance " + h.ToString + " reaches the call scene" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 3 Then
                For n As Integer = 0 To dynamic_ambulance_number Step 1
                    If amb_list(n) = (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k) Then
                        h = n
                        Exit For
                    End If
                Next
                Form2.DataGridView1.Item(2, row - 1).Value = Form2.DataGridView1.Item(2, row - 1).Value + " - " + "ambulance " + h.ToString + " finishes serving the call at scene" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 4 Then
                For n As Integer = 0 To dynamic_ambulance_number Step 1
                    If amb_list(n) = (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k) Then
                        h = n
                        Exit For
                    End If
                Next
                Form2.DataGridView1.Item(2, row - 1).Value = Form2.DataGridView1.Item(2, row - 1).Value + " - " + "ambulance " + h.ToString + " finishes serving the call at hospital" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
            For n As Integer = 0 To dynamic_ambulance_number Step 1
                If amb_list(n) = (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k) Then
                    h = n
                    Exit For
                End If
            Next
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 5 Then
                Form2.DataGridView1.Item(2, row - 1).Value = Form2.DataGridView1.Item(2, row - 1).Value + " - " + "ambulance " + h.ToString + " arrives at base" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
        Next
        For k As Integer = 0 To dynamic_ambulance_number Step 1
            Form2.DataGridView1.Item(3, row - 1).Value = Form2.DataGridView1.Item(3, row - 1).Value + "(" + M_state(k).Item(0).ToString + ", " + M_state(k).Item(1).ToString + ", " + M_state(k).Item(2).ToString + ")" + Environment.NewLine
        Next
    End Sub
    Private Sub trace_calls()
        'Form2.DataGridView1.Rows.Add(1)
        Dim r As Integer
        For Each l In C_state
            If clock >= C_state(r).Item(3) Then
                Form2.DataGridView1.Item(4, row).Value = Form2.DataGridView1.Item(4, row).Value + "(" + C_state(r).Item(0).ToString + ", " + C_state(r).Item(1).ToString + ", " + C_state(r).Item(2).ToString + ")" + Environment.NewLine
                r = r + 1
            Else
                r = r + 1
            End If
        Next
    End Sub
    Private Sub trace_current()
        Dim n As Integer
        Form2.DataGridView1.Rows.Add(1)
        Form2.DataGridView1.Item(0, row).Value = clock.ToString()
        For k As Integer = 0 To dynamic_ambulance_number Step 1
            If amb_list(k) = clock Then
                n = k
                Exit For
            End If
        Next
        Select Case next_action(i)
            Case 1
                trace_calls()
                Form2.DataGridView1.Item(1, row).Value = "new call arrived"
            Case 2
                trace_calls()
                Form2.DataGridView1.Item(1, row).Value = "ambulance " + n.ToString + " reached the call scene"
            Case 3
                trace_calls()
                Form2.DataGridView1.Item(1, row).Value = "ambulance " + n.ToString + " finished at the scene"
            Case 4
                trace_calls()
                Form2.DataGridView1.Item(1, row).Value = "ambulance " + n.ToString + " finished at hospital"
            Case 5
                trace_calls()
                Form2.DataGridView1.Item(1, row).Value = "ambulance " + n.ToString + " arrived at base"
        End Select
        row += 1
    End Sub
    Private Sub BtStart_Click(sender As Object, e As EventArgs) Handles BtStart.Click
        input_read()
        last_rep = 1
        For t As Integer = 1 To Me.TextBox2.Text Step 1
            total_call_nom = 0
            total_hp_call_nom = 0
            total_lp_call_nom = 0
            miss_call_num_hp = 0
            miss_call_num_lp = 0
            temp_loss_time = 0
            temp_time = 0
            lost_percent = 0
            lost_percent_pr = 0
            busy_time = 0
            coverage = 0
            last_clock = 0
            time_interval = 0
            'Form2.DataGridView1.Rows.Add(1)
            'Form2.DataGridView1.Rows(row).DefaultCellStyle.BackColor = Color.Gray
            'Form2.DataGridView1.Item(2, row).Value = "Replication" + t.ToString
            'row = row + 1
            start_initialization()
            control_reps()
            busy_time = 0
            busy_time_start = 0
            busy_time_end = 0
            Me.Label4.Text = Me.Label4.Text + 1
            back_to_start_mode()
            queue_time.Clear()
            total_call_nom = 0
            total_hp_call_nom = 0
            total_lp_call_nom = 0
            miss_call_num_hp = 0
            miss_call_num_lp = 0
            temp_loss_time = 0
            temp_time = 0
            seed += 1
            weed += 1
            leed += 1
            reed += 1
            zeed += 1
            call_num = 0
            last_num_of_call = 0
        Next
        'Form2.Show()
    End Sub
    Private Sub start_initialization()
        'Form2.DataGridView1.Rows.Add(1)
        amb_initial_state()
        closer_amb_initialization()
        'lamda_of_l_initialization()
        FEL_Builder(1, vbNull, vbNull)
    End Sub
    Private Sub control_reps()
        While clock < Me.TextBox1.Text
            main()
            Me.Label2.Text = clock.ToString
            Application.DoEvents()
        End While
        reps = reps + 1
        lost_percent = (miss_call_num_hp + miss_call_num_lp) / (total_hp_call_nom + total_lp_call_nom)
        lost_percent_pr = miss_call_num_hp / (total_hp_call_nom + total_lp_call_nom)
        Dim busy_time_fraction As Double
        busy_time_fraction = busy_time / Me.TextBox1.Text
        Dim avg_resp_time As Double
        avg_resp_time = temp_time / (total_hp_call_nom + total_lp_call_nom)
        Dim avg_late_resp_time As Double
        avg_late_resp_time = temp_loss_time / (miss_call_num_hp + miss_call_num_lp)
        Dim avg_queue_time As Double
        avg_queue_time = queue_time.Sum() / (queue_time.Count() - 1)
        Dim sb As StringBuilder = New StringBuilder
        sb.Append(busy_time_fraction.ToString())
        Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Pi_Zero-Fraction of Busy Time_" + static_policy + "(" + hp_weight.ToString() + ", " + Lp_weight.ToString() + ").txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using

        sb = New StringBuilder
        sb.Append(lost_percent.ToString())
        Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Pi_Zero-Late Calls_" + static_policy + "(" + hp_weight.ToString() + ", " + Lp_weight.ToString() + ").txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
        sb = New StringBuilder
        sb.Append(lost_percent_pr.ToString())
        Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Pi_Zero_Late Calls_hp_" + static_policy + "(" + hp_weight.ToString() + ", " + Lp_weight.ToString() + ").txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using

        sb = New StringBuilder
        sb.Append(avg_resp_time.ToString())
        Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Pi_Zero-avg. Response Time_" + static_policy + "(" + hp_weight.ToString() + ", " + Lp_weight.ToString() + ").txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
        sb = New StringBuilder
        sb.Append(coverage.ToString() / Me.TextBox1.Text)
        Using writeresult As StreamWriter = New StreamWriter("c:\users\snasrol\desktop\output\Pi_Zero_Coverage_" + static_policy + "(" + hp_weight.ToString() + ", " + Lp_weight.ToString() + ").txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
        sb = New StringBuilder
        sb.Append(avg_late_resp_time)
        Using writeresult As StreamWriter = New StreamWriter("c:\users\snasrol\desktop\output\Pi_Zero_avg. Lost Calls Response Time_" + static_policy + "(" + hp_weight.ToString() + ", " + Lp_weight.ToString() + ").txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
        sb = New StringBuilder
        sb.Append(avg_queue_time)
        Using writeresult As StreamWriter = New StreamWriter("c:\users\snasrol\desktop\output\Pi_Zero_avg. Queue Time_" + static_policy + "(" + hp_weight.ToString() + ", " + Lp_weight.ToString() + ").txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
    End Sub
    Private Sub back_to_start_mode()
        fel.RemoveRange(0, fel.Count())
        C_state.RemoveRange(0, C_state.Count())
        M_state.RemoveRange(0, M_state.Count())
        clock = 0
        last_origin.Initialize()
        amb_list.Initialize()
        closer_amb_distance.RemoveRange(0, closer_amb_distance.Count())
        'closer_amb_initialization()
    End Sub
    Private Sub BtStop_Click(sender As Object, e As EventArgs) Handles BtStop.Click
        End
    End Sub
    Private Sub sampling_state(ByVal events As Integer, ByVal amb_number As Integer)
        Dim file_path As String = "C:\Users\snasrol\Desktop\output\State Complete Sample_" + static_policy + "(" + hp_weight.ToString() + ", " + Lp_weight.ToString() + ").txt"
        Dim sb As StringBuilder = New StringBuilder
        sb.Append("{" + events.ToString + ", [")
        For t = 0 To dynamic_ambulance_number - 1 Step 1
            sb.Append("(" + M_state(t).Item(0).ToString + ", " + M_state(t).Item(1).ToString + ", " + M_state(t).Item(2).ToString + ")")
        Next
        sb.Append("], [")
        For t = 0 To C_state.Count() - 1 Step 1
            If C_state(t).Item(3) <= clock Then
                sb.Append("(" + C_state(t).Item(0).ToString + ", " + C_state(t).Item(1).ToString + ", " + C_state(t).Item(2).ToString + ")")
            End If
        Next
        sb.Append("]}")
        sb.AppendLine("," + amb_number.ToString())
        Using outfile As StreamWriter = New StreamWriter(file_path, True)
            outfile.Write(sb.ToString())
        End Using
    End Sub
    Private Sub number_of_visits()
        Dim state_list As New List(Of String)
        Dim readfile_path As String = "C:\Users\snasrol\Desktop\output\State Complete Sample_" + static_policy + "(" + hp_weight.ToString + ", " + Lp_weight.ToString() + ").txt"
        Dim sr As String
        Using infile As StreamReader = New StreamReader(readfile_path, True)
1:          While infile.Peek() <> -1
                sr = infile.ReadLine()
                Dim visit_num As New Integer
                Dim visits As New List(Of String)
                state_list.Add(sr)
                For t As Integer = 0 To state_list.Count() - 1 Step 1
                    If sr = state_list(t) Then
                        visit_num = visit_num + 1
                    End If
                Next
                For t As Integer = 0 To visited_list.Count() - 1 Step 1
                    If visited_list(t).Item(0) = sr Then
                        visited_list(t).Item(1) = visit_num
                        GoTo 1
                    End If
                Next
                visits.Add(sr)
                visits.Add(visit_num)
                visited_list.Add(visits)
            End While
            Me.Label9.Text = state_list.Count() - 1
            Me.Label11.Text = visited_list.Count() - 1
        End Using
    End Sub
    Private Sub BtCreateVisitedStates_Click(sender As Object, e As EventArgs) Handles BtCreateVisitedStates.Click
        Dim writefile_path As String = "C:\Users\snasrol\Desktop\output\State Most Visited Sample.txt"
        Dim sb As StringBuilder = New StringBuilder
        number_of_visits()
        For t As Integer = 0 To visited_list.Count() - 1 Step 1
            sb.Append(visited_list(t).Item(1) + " - ")
            sb.AppendLine(visited_list(t).Item(0))
        Next
        Using writefile As StreamWriter = New StreamWriter(writefile_path, True)
            writefile.WriteLine(sb.ToString())
        End Using
    End Sub
    Private Sub random_sampling()
        Dim sample_size As Integer
        sample_size = Me.TextBox3.Text
        Dim readfilepath As String = "C:\Users\snasrol\Desktop\output\State Complete Sample.txt"
        Dim n As Integer
        Dim random_sample_list As New List(Of String)
        If sample_size <= Me.Label11.Text Then
            For t As Integer = 0 To sample_size Step 1
                Dim line_num As Integer
                line_num = random_num.Next(1, sample_size)
                Using sr As StreamReader = New StreamReader(readfilepath, True)
                    n = 0
                    Do While n < line_num And sr.Peek() > 0
                        sr.ReadLine()
                        n = n + 1
                    Loop
                    random_sample_list.Add(sr.ReadLine())

                End Using
            Next
            Dim writefilepath As String = "C:\Users\snasrol\Desktop\output\Random Sample.txt"
            Dim sb As StringBuilder = New StringBuilder
            For t As Integer = 0 To random_sample_list.Count() - 1 Step 1
                sb.AppendLine(random_sample_list(t))
            Next
            Using writefile As StreamWriter = New StreamWriter(writefilepath, True)
                writefile.WriteLine(sb.ToString())
            End Using
        Else
            MsgBox("Warning, Sample Size Should be Less than Number of New States")
        End If
    End Sub
    Private Sub BtRandomSample_Click(sender As Object, e As EventArgs) Handles BtRandomSample.Click
        random_sampling()
    End Sub
    Private Sub most_visited_sampling()
        Dim writefilepath As String = "C:\Users\snasrol\Desktop\output\visited Sample.txt"
        Dim sb As StringBuilder = New StringBuilder
        Dim num_of_all_state As Integer
        num_of_all_state = Me.Label9.Text
        For t As Integer = 0 To visited_list.Count() - 1 Step 1
            visited_list(t).Item(1) = Convert.ToDouble(visited_list(t).Item(1)) / num_of_all_state
        Next
        For t As Integer = 0 To visited_list.Count() - 1 Step 1
            If t + 1 = visited_list.Count() Then
                visited_list(t).Item(1) = 1
            Else
                visited_list(t + 1).Item(1) = Convert.ToDouble(visited_list(t + 1).Item(1)) + Convert.ToDouble(visited_list(t).Item(1))
            End If
        Next
        Dim sample_size As Integer
        sample_size = Me.TextBox3.Text - 1

        sb.AppendLine(visited_list(0).Item(0))

        If sample_size <= visited_list.Count() - 1 Then
            For t As Integer = 0 To sample_size - 1
                Dim temp_rand As Double
                temp_rand = random_num.NextDouble()
                For n As Integer = 0 To visited_list.Count() - 2 Step 1
                    If temp_rand < Convert.ToDouble(visited_list(n + 1).Item(1)) And temp_rand >= Convert.ToDouble(visited_list(n).Item(1)) Then
                        sb.AppendLine(visited_list(n + 1).Item(0))
                    End If
                Next
            Next
            Using writefile As StreamWriter = New StreamWriter(writefilepath, True)
                writefile.WriteLine(sb.ToString())
            End Using
        Else
            MsgBox("Warning, Sample Size Should be Less than Number of New States")
        End If
    End Sub
    Private Sub BtVisitedSample_Click(sender As Object, e As EventArgs) Handles BtVisitedSample.Click
        most_visited_sampling()
    End Sub
    Private Sub BtSimulate_Click(sender As Object, e As EventArgs) Handles BtSimulate.Click
        'For Form4.arrival_change = -0.1 To 0.1 Step 0.02
        best_miss_call = New List(Of Double)
        best_resp_time = New List(Of Double)
        Form4.seed = 0
        Form4.weed = 0
        Form4.leed = 0
        Form4.reed = 0
        Form4.zeed = 0
        'Dim sb As StringBuilder = New StringBuilder
        'sb.Append("arrival rate change=" + Form4.arrival_change.ToString())
        'Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Results_GD_R_A-Lost Calls.txt", True)
        'writeresult.WriteLine(sb.ToString())
        'End Using
        'Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Results_GD_R_A-Waiting Time.txt", True)
        'writeresult.WriteLine(sb.ToString())
        'End Using
        closer_amb_initialization()
        c_of_r.Clear()
        c_of_r_loss.Clear()
        number_lost_calls.Clear()
        final_coverage.Clear()
        number_lost_calls_pr.Clear()
        busy_time_fraction_list.Clear()
        total_hp_call_nom_list.clear()
        total_lp_call_nom_list.clear()
        Form4.input_read()
        Form4.Show()
        Application.DoEvents()
        Me.Hide()
        ''read each state in the sample
3:      phi_path()
        Dim random_sample As New List(Of String)
        Using sr As StreamReader = New StreamReader(filepath, True)
            While sr.Peek <> -1
                random_sample.Add(sr.ReadLine())
            End While
        End Using
        ''simulate each state by using class form4
        'Form4.input_read()

        Form4.r1 = New Random(Form4.seed)
        Form4.r2 = New Random(Form4.weed)
        Form4.r3 = New Random(Form4.leed)
        Form4.r4 = New Random(Form4.reed)
        Form4.r5 = New Random(Form4.zeed)
        For q As Integer = 0 To random_sample.Count() - 1 Step 1
            If q = 0 Then
                Form4.response_time_indicator = 1 'print response times
            Else
                Form4.response_time_indicator = 0 'do not print
            End If
            Form4.Label15.Text = Form4.Label15.Text + 1
            Form4.Label15.Refresh()
            Form4.Label17.Text = 0
            Form4.num_of_total_calls = New Integer
            Form4.state_FEL_building(random_sample(q))
            'Form4.closer_amb_initialization()
            Form4.control_replications_of_each_simulation(random_sample(q))
            Form4.seed = 0
            Form4.weed = 0
            Form4.leed = 0
            Form4.reed = 0
            Form4.zeed = 0
            final_coverage.Add(Form4.state_coverage) 'undiscounted
            c_of_r.Add(Form4.waiting_time)
            c_of_r_loss.Add(Form4.waiting_loss_time)
            number_lost_calls.Add(Form4.number_of_missed_calls)
            number_lost_calls_pr.Add(Form4.number_of_missed_calls_pr)
            busy_time_fraction_list.Add(Form4.busy_time_fraction) 'undiscounted
            total_hp_call_nom_list.add(Form4.total_hp_call_nom_list)
            total_lp_call_nom_list.add(Form4.total_lp_call_nom_list)
        Next
        'Form3.Show()
        'Exit Sub

        regression_solver()
        'matlab_min()
        iteration += 1

        '''''''''''''''''''''''''''''''''''''''''''''
        If stop_condition(alpha) < 5 Then
            policy.Clear()
            final_coverage.Clear()
            c_of_r.Clear()
            c_of_r_loss.Clear()
            number_lost_calls.Clear()
            number_lost_calls_pr.Clear()
            busy_time_fraction_list.Clear()
            total_hp_call_nom_list.clear()
            total_lp_call_nom_list.clear()
            GoTo 3
            'Else
            'Dim best_sb As StringBuilder = New StringBuilder
            'best_sb.Append(best_miss_call.Min())
            'Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Results_GD_R_A-Lost Calls.txt", True)
            'writeresult.WriteLine(best_sb.ToString())
            'End Using
            'best_sb = New StringBuilder
            'best_sb.Append(best_resp_time.Min())
            'Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Results_GD_R_A-Waiting Time.txt", True)
            'writeresult.WriteLine(best_sb.ToString())
            'End Using
            'Form3.Show()
            'Exit Sub
        End If
        'Next
    End Sub
    Public Sub phi_path()
        If Me.CheckBox1.Checked = True Then
            filepath = "C:\Users\snasrol\Desktop\output\Random Sample.txt"
        End If
        If Me.CheckBox2.Checked = True Then
            filepath = "C:\Users\snasrol\Desktop\output\visited Sample.txt"
        End If
    End Sub
    Public Function nodes_to_regions(ByVal state As List(Of List(Of Integer)))
        Dim new_state As New List(Of List(Of Integer))
        new_state = state.Select(Function(innerlist) innerlist.ToList).ToList
        For i As Integer = 1 To dynamic_ambulance_number Step 1
            For j As Integer = 0 To region_node_list.Count() - 1 Step 1
                If region_node_list(j).Contains(state(i).Item(1)) Then
                    new_state(i).Item(1) = j
                End If
            Next
        Next
        Return new_state
    End Function
    Public Function phi_one_numbers(ByVal state As List(Of List(Of Integer)))
        Dim phi_one_num As New Double
        For i As Integer = 1 To dynamic_ambulance_number
            If state(i).Item(0) = 1 Then
                If travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)) >= 8 Then
                    phi_one_num += 1
                End If
            End If
        Next
        Return phi_one_num
    End Function
    Public Function phi_one_time(ByVal state As List(Of List(Of Integer)))
        Dim phi_one_t As New Double
        For i As Integer = 1 To dynamic_ambulance_number
            If state(i).Item(0) = 1 Then
                If travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)) >= 8 Then
                    phi_one_t += travel_lambdas(state(i).Item(1)).Item(state(i).Item(2))
                End If
            End If
        Next
        Return phi_one_t
    End Function

    ''not used
    Public Function phi_two(ByVal state As List(Of List(Of Integer)))
        Dim num_of_assigned_calls_per_state As Integer
        If state.Count() > 5 Then
            For i As Integer = 5 To state.Count() - 1 Step 1
                If state(i).Item(0) = 1 Then
                    num_of_assigned_calls_per_state += 1
                End If
            Next
            Return num_of_assigned_calls_per_state
        Else
            Return 0
        End If
        num_of_assigned_calls_per_state = 0
    End Function
    Public Function phi_three(ByVal state As List(Of List(Of Integer)))
        Dim new_state As New List(Of List(Of Integer))
        new_state = nodes_to_regions(state)
        Dim num_of_available_ambs_per_state(13) As Integer
        Dim indicator As Integer = 0
        For i As Integer = 1 To dynamic_ambulance_number Step 1
            If new_state(i).Item(0) = 0 Then
                For j As Integer = 0 To 13 Step 1
                    If travel_lambdas_regions(j).Item(new_state(i).Item(1)) <= 8 Then
                        num_of_available_ambs_per_state(j) += 1
                    End If
                Next
            End If
        Next
        Dim phi_3_temp As New Double
        For k As Integer = 0 To 13 Step 1
            If num_of_available_ambs_per_state(k) = 0 Then
                phi_3_temp = phi_3_temp + region_lambdas(k)
            End If
        Next
        Return phi_3_temp
    End Function
    Public Function n_of_l_of_s(ByVal state As List(Of List(Of Integer)), ByVal region As Integer)
        Dim new_state As New List(Of List(Of Integer))
        new_state = nodes_to_regions(state)

        Dim available_ambs_per_state As New List(Of List(Of Integer))
        Dim indicator As Integer = 0
        For i As Integer = 0 To 13 Step 1
            Dim available_amb As New List(Of Integer)
            For k As Integer = 1 To dynamic_ambulance_number Step 1
                If new_state(k).Item(0) = 0 Then 'if ambulance is available
                    Dim amb_location As Double = new_state(k).Item(1)
                    If travel_lambdas_regions(i).Item(amb_location) <= 8 Then
                        available_amb.Add(k)
                    End If
                End If
            Next
            available_ambs_per_state.Add(available_amb)
        Next
        Return available_ambs_per_state(region)
    End Function
    Public Function phi_four(ByVal state As List(Of List(Of Integer)))
        Dim new_state As New List(Of List(Of Integer))
        new_state = nodes_to_regions(state)
        Dim phi_4 As Double
        Dim phi_4_final As Double

        For region_i As Integer = 0 To 13 Step 1
            Dim available_ambs As List(Of Integer) = n_of_l_of_s(new_state, region_i)
            If available_ambs.Count = 0 Then 'if there are available ambulances in region_i
                Dim lambda_l(13) As Double
                For k = 0 To 13 Step 1 'for all regions
                    For i As Integer = 0 To available_ambs.Count() - 1 Step 1 'for each ambulance that is available and close enough to region k
                        lambda_l(k) += region_lambdas(new_state(available_ambs(i)).Item(1))
                    Next
                Next
                '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''just to calculate the probabilities
                Dim lambda As Double
                For t As Integer = 0 To 13 Step 1
                    If region_lambdas(t) <> 0 Then
                        lambda += region_lambdas(t)
                    End If
                Next
                Dim prob(13) As Double
                For t As Integer = 0 To 13 Step 1
                    prob(t) = (region_lambdas(t)) / lambda
                Next
                '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''calculate service rate
                Dim miu As New List(Of Double)
                Dim counter(13) As Double
                For j As Integer = 0 To 13 Step 1
                    Dim temp As New Double
                    For t As Integer = 1 To dynamic_ambulance_number Step 1
                        If new_state(t).Item(0) = 0 Then
                            counter(j) += 1
                            temp += (travel_lambdas_regions(new_state(t).Item(1)).Item(j)) * prob(j)
                        End If
                    Next
                    temp += prob_scene_hospital * 56.7 + (1 - prob_scene_hospital) * 54.18
                    miu.Add(temp)
                Next

                Dim miu_harmonic As Double
                For l As Integer = 0 To 13 Step 1
                    If counter(l) > 0 Then
                        For t As Integer = 0 To Min(counter(l) - 1, 13) Step 1
                            miu_harmonic += 1 / miu(t)
                        Next
                        miu_harmonic = 1 / miu_harmonic
                    Else
                        miu_harmonic = 30
                    End If
                Next
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' calculate phi_4
                Dim rove As Double
                Dim erlang_c As Double
                For i As Integer = 0 To 13 Step 1
                    If counter(i) > 0 Then 'check the number of available servers for each region
                        If lambda_l(i) <> 0 Then 'if arrival rate for that region is not 0
                            rove = lambda_l(i) / miu_harmonic
                            Dim numerator As Double = counter(i) * Math.Pow(1 - rove, 2) + 2 * rove - (1 - rove) * Math.Sqrt(4 * counter(i) * rove + Math.Pow(counter(i), 2) * Math.Pow(1 - rove, 2))
                            Dim denuminator As Double = -counter(i) * rove * (1 - rove) + 2 * rove + rove * Math.Sqrt(4 * counter(i) * rove + Math.Pow(counter(i), 2) * Math.Pow(1 - rove, 2))
                            erlang_c = numerator / denuminator
                            phi_4 = (1 / region_lambdas(i)) * erlang_c
                        Else 'if arrival rate is 0
                            erlang_c = 0
                            phi_4 = erlang_c
                        End If
                    Else 'if there is no available ambulance
                        If lambda_l(i) <> 0 Then 'if arrival rate for that region is not 0 
                            rove = 1
                            Dim numerator As Double = counter(i) * Math.Pow(1 - rove, 2) + 2 * rove - (1 - rove) * Math.Sqrt(4 * counter(i) * rove + Math.Pow(counter(i), 2) * Math.Pow(1 - rove, 2))
                            Dim denuminator As Double = -counter(i) * rove * (1 - rove) + 2 * rove + rove * Math.Sqrt(4 * counter(i) * rove + Math.Pow(counter(i), 2) * Math.Pow(1 - rove, 2))
                            erlang_c = 1
                            phi_4 = erlang_c
                        Else 'if arrival rate is 0
                            erlang_c = 0
                            phi_4 = erlang_c
                        End If
                    End If
                Next
                If lambda_l(region_i) <> 0 Then
                    phi_4_final += (1 / lambda_l(region_i)) * (phi_4 + travel_lambdas_regions(region_i).Item(region_i))
                End If
            Else
                Dim lambda_l(13) As Double
                For k = 0 To 13 Step 1 'for all regions
                    For i As Integer = 0 To available_ambs.Count() - 1 Step 1 'for each ambulance that is available and close enough to region k
                        lambda_l(k) += region_lambdas(new_state(available_ambs(i)).Item(1))
                    Next
                Next
                If lambda_l(region_i) <> 0 Then
                    phi_4_final += (1 / lambda_l(region_i)) * travel_lambdas_regions(region_i).Item(region_i)
                End If
            End If
        Next

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Return phi_4_final
    End Function
    Public Function phi_five(ByVal state As List(Of List(Of Integer)))
        Dim new_state As New List(Of List(Of Integer))
        new_state = state.Select(Function(innerlist) innerlist.ToList).ToList
        For i As Integer = 1 To dynamic_ambulance_number Step 1
            If state(i).Item(0) = 0 Then

            End If
            If state(i).Item(0) = 1 Then
                new_state(i).Item(0) = 2
                new_state(i).Item(1) = state(i).Item(2)
            End If
            If state(i).Item(0) = 2 Then

            End If
            If state(i).Item(0) = 3 Then
                new_state(i).Item(1) = state(i).Item(2)
            End If
            If state(i).Item(0) = 4 Or state(i).Item(0) = 5 Then
                new_state(i).Item(0) = 0
                new_state(i).Item(1) = state(i).Item(2)
            End If
        Next
        Return phi_three(new_state)
    End Function
    Public Function phi_six(ByVal state As List(Of List(Of Integer)))
        Dim new_state As New List(Of List(Of Integer))
        new_state = state.Select(Function(innerlist) innerlist.ToList).ToList
        For i As Integer = 1 To dynamic_ambulance_number Step 1
            If state(i).Item(0) = 0 Then

            End If
            If state(i).Item(0) = 1 Then
                new_state(i).Item(0) = 2
                new_state(i).Item(1) = state(i).Item(2)
            End If
            If state(i).Item(0) = 2 Then

            End If
            If state(i).Item(0) = 3 Then
                new_state(i).Item(1) = state(i).Item(2)
            End If
            If state(i).Item(0) = 4 Or state(i).Item(0) = 5 Then
                new_state(i).Item(0) = 0
                new_state(i).Item(1) = state(i).Item(2)
            End If
        Next
        Return phi_four(new_state)
    End Function
    ''not used
    Public Function phi_four_topaloglu_way(ByVal state As List(Of List(Of Integer)))
        Dim new_state As New List(Of List(Of Integer))
        new_state = nodes_to_regions(state)

        Dim phi4 As New Double
        Dim lambda_l(13) As Double
        For l As Integer = 0 To 13 Step 1
            Dim miu As New Double
            Dim rove As New Double
            Dim available_ambs As List(Of Integer) = n_of_l_of_s(state, l)
            Dim temp_miu As New Double
            For i As Integer = 0 To available_ambs.Count() - 1 Step 1
                temp_miu += 1 / (travel_lambdas_regions(new_state(available_ambs(i)).Item(1)).Item(l))
                lambda_l(l) += region_lambdas(state(available_ambs(i)).Item(1))
            Next
            If available_ambs.Count() > 0 Then
                miu = 1 / temp_miu
                rove = lambda_l(l) * miu / available_ambs.Count()
                Dim numerator As Double = available_ambs.Count * Math.Pow(1 - rove, 2) + 2 * rove - (1 - rove) * Math.Sqrt(4 * available_ambs.Count() * rove + Math.Pow(available_ambs.Count(), 2) * Math.Pow(1 - rove, 2))
                Dim denuminator As Double = -available_ambs.Count() * rove * (1 - rove) + 2 * rove + rove * Math.Sqrt(4 * available_ambs.Count() * rove + Math.Pow(available_ambs.Count(), 2) * Math.Pow(1 - rove, 2))
                Dim erlang_c As Double = numerator / denuminator
                phi4 += lambda_l(l) * (erlang_c / (available_ambs.Count() / miu - lambda_l(l)))
            Else
                phi4 = 30
            End If
        Next
    End Function
    ''not used
    Public Function phi_five_future(ByVal state As List(Of List(Of Integer)), ByVal current_time As Double)
        Dim new_state As New List(Of List(Of Integer))
        new_state = state.Select(Function(innerlist) innerlist.ToList).ToList
        new_state = future_action_state_development(new_state, current_time)
        Return phi_three(new_state)
    End Function
    ''not used
    Public Function phi_six_future(ByVal state As List(Of List(Of Integer)), ByVal current_time As Double)
        Dim new_state As New List(Of List(Of Integer))
        new_state = state.Select(Function(innerlist) innerlist.ToList).ToList
        new_state = future_action_state_development(new_state, current_time)
        Return phi_four(new_state)
    End Function
    ''not used
    Public Function future_action_state_development(ByVal state As List(Of List(Of Integer)), ByVal current_time As Double)
        all_states = New List(Of List(Of List(Of Integer)))
        Dim temp_state As New List(Of Integer)
        Dim po_states As New List(Of List(Of Integer))
        'lamda_of_l_initialization()
        Dim sum_lambda As New List(Of Double)
        param_list = New List(Of List(Of Double))
        Dim p_list As New List(Of Double)
        min_index = New Integer
        ''create: all possible states regarding new call arrives

        For l As Integer = 0 To node_lambdas.Count() - 1 Step 1 'region
            If node_lambdas(l) > 0 Then 'if the region has a reasonable lambda then add the call from that region
                For n As Integer = 0 To 1 Step 1
                    temp_state = New List(Of Integer)
                    po_states = New List(Of List(Of Integer))
                    temp_state.Add(0)
                    temp_state.Add(l)
                    temp_state.Add(n)
                    po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                    po_states(0).Item(0) = 0 'event
                    po_states.Add(temp_state)
                    all_states.Add(po_states)

                    'add arrival time parameter
                    p_list = New List(Of Double)
                    p_list.Add(1 / node_lambdas(l))
                    If n = 0 Then
                        p_list.Add(prob_priority)
                    Else
                        p_list.Add(1 - prob_priority)
                    End If

                    param_list.Add(p_list)
                Next
                sum_lambda.Add(1 / node_lambdas(l))
            End If
        Next

        ''create: all possible states regarding ambulances reaching call locations



        For i As Integer = 1 To 17 Step 1
            If state(i).Item(0) = 1 Then
                po_states = New List(Of List(Of Integer))
                po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                po_states(0).Item(0) = 2 'event
                po_states(i).Item(0) = 2 'amb state is at the scene
                po_states(i).Item(1) = state(i).Item(2) 'amb destination is amb location at the scene
                all_states.Add(po_states)
                'add travel time parameter
                p_list = New List(Of Double)
                p_list.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2))) 'add travel times
                p_list.Add(Form4.M_state(i - 1).Item(3)) 'add start time of that travel time
                param_list.Add(p_list)

                po_states = New List(Of List(Of Integer))
                po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                po_states(0).Item(0) = 2 'event
                po_states(i).Item(0) = 3 'amb state is at the hospital
                po_states(i).Item(1) = state(i).Item(2) 'amb is located at the call scene
                po_states(i).Item(2) = 91 'amb is going to one of the hospitals
                all_states.Add(po_states)
                'add travel time parameter
                p_list = New List(Of Double)
                p_list.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)))
                p_list.Add(1 / 3)
                p_list.Add(Form4.M_state(i - 1).Item(3))
                param_list.Add(p_list)

                po_states = New List(Of List(Of Integer))
                po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                po_states(0).Item(0) = 2 'event
                po_states(i).Item(0) = 3 'amb state is at the hospital
                po_states(i).Item(1) = state(i).Item(2) 'amb is located at the call scene
                po_states(i).Item(2) = 37 'amb is going to one of the hospitals
                all_states.Add(po_states)
                'add travel time parameter
                p_list = New List(Of Double)
                p_list.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)))
                p_list.Add(1 / 3)
                p_list.Add(Form4.M_state(i - 1).Item(3))
                param_list.Add(p_list)

                po_states = New List(Of List(Of Integer))
                po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                po_states(0).Item(0) = 2 'event
                po_states(i).Item(0) = 3 'amb state is at the hospital
                po_states(i).Item(1) = state(i).Item(2) 'amb is located at the call scene
                po_states(i).Item(2) = 66 'amb is going to one of the hospitals
                all_states.Add(po_states)
                'add travel time parameter
                p_list = New List(Of Double)
                p_list.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)))
                p_list.Add(1 / 3)
                p_list.Add(Form4.M_state(i - 1).Item(3))
                param_list.Add(p_list)
            End If
        Next

        ''create: all possible states regarding ambualnces finishe their jobs

        'at the scene
        For i As Integer = 1 To 17 Step 1
            If state(i).Item(0) = 2 Then 'if amb is at the scene
                If state.Count() > 18 Then 'if there is a call
                    For j As Integer = 18 To state.Count() - 1 Step 1
                        If state(j).Item(0) = 0 Then 'call is unassigned
                            po_states = New List(Of List(Of Integer))
                            po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                            po_states(0).Item(0) = 9 'event is dispatching
                            po_states(i).Item(0) = 1 'amb state is dispatched
                            po_states(i).Item(2) = state(j).Item(1) 'amb destination is call location
                            po_states(j).Item(0) = 1 'call state is assigned
                            all_states.Add(po_states)
                            'add servie time parameters
                            p_list = New List(Of Double)
                            p_list.Add(54.18)
                            p_list.Add(15.18)
                            p_list.Add(Form4.M_state(i - 1).Item(3))
                            param_list.Add(p_list)

                        End If
                    Next
                Else 'no call in the queue
                    po_states = New List(Of List(Of Integer))
                    po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                    po_states(0).Item(0) = 3 'event is amb is finished
                    po_states(i).Item(0) = 3 'amb state is amb is finished
                    po_states(i).Item(2) = po_states(i).Item(1) 'amb loc is the same as amb des
                    all_states.Add(po_states)
                    'add servie time parameters
                    p_list = New List(Of Double)
                    p_list.Add(54.18)
                    p_list.Add(15.18)
                    p_list.Add(Form4.M_state(i - 1).Item(3))
                    param_list.Add(p_list)
                End If
            End If
        Next

        'ambulance finish at hospital
        For i As Integer = 1 To 17 Step 1
            If state(i).Item(0) = 3 Then 'if amb is at the hospital
                If state.Count() > 18 Then 'if there is a call
                    For j As Integer = 18 To state.Count() - 1 Step 1
                        If state(j).Item(0) = 0 Then 'call is unassigned
                            po_states = New List(Of List(Of Integer))
                            po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                            po_states(0).Item(0) = 9 'event is dispatching
                            po_states(i).Item(0) = 1 'amb state is dispatched
                            po_states(i).Item(2) = state(j).Item(1) 'amb destination is call location
                            po_states(j).Item(0) = 1 'call state is assigned
                            all_states.Add(po_states)
                            'add servie time parameters
                            p_list = New List(Of Double)
                            p_list.Add(56.7)
                            p_list.Add(13.6)
                            p_list.Add(Form4.M_state(i - 1).Item(3))
                            param_list.Add(p_list)

                        End If
                    Next
                Else 'no call in the queue
                    po_states = New List(Of List(Of Integer))
                    po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                    po_states(0).Item(0) = 4 'event is amb is finished
                    po_states(i).Item(0) = 3 'amb state is amb is finished
                    po_states(i).Item(2) = po_states(i).Item(1) 'amb loc is the same as amb des
                    all_states.Add(po_states)
                    'add servie time parameters
                    p_list = New List(Of Double)
                    p_list.Add(56.7)
                    p_list.Add(13.6)
                    p_list.Add(Form4.M_state(i - 1).Item(3))
                    param_list.Add(p_list)
                End If
            End If
        Next

        ''create: all possible states regarding ambulance arriving at the base

        For i As Integer = 1 To 17 Step 1
            If state(i).Item(0) = 4 Or state(i).Item(0) = 5 Then 'if amb is redeploying or reallocating
                po_states = New List(Of List(Of Integer))
                po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                po_states(0).Item(0) = 5 'event is amb  arrives to base
                po_states(i).Item(0) = 0 'amb state is at the base
                po_states(i).Item(1) = state(i).Item(2) 'amb loc is amb des
                po_states(i).Item(2) = 0
                all_states.Add(po_states)
                'add travel time parameter
                p_list = New List(Of Double)
                p_list.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)))
                p_list.Add(Form4.M_state(i - 1).Item(3))
                param_list.Add(p_list)
            End If
        Next
        Dim lambda_sum As Double
        For i As Integer = 0 To param_list.Count() - 1 Step 1
            If all_states(i).Item(0).Item(0) = 0 Then
                lambda_sum += param_list(i).Item(0)
            End If
        Next
        ''calculate: probabilities of transitions
        Dim final_prob As New List(Of Double)
        Dim min_del As Double = find_min_delta()
        Dim index As Integer = (From integ As List(Of Double) In integral_list Where integ.Item(0) = min_del Select integral_list.IndexOf(integ)).FirstOrDefault()
        Dim sum_times As Double
        For i As Integer = 0 To all_states.Count() - 1 Step 1
            If all_states(i).Item(0).Item(0) = 9 Or all_states(i).Item(0).Item(0) = 3 Or all_states(i).Item(0).Item(0) = 4 Then
                sum_times += current_time - param_list(i).Item(2)
            End If
        Next

        For i As Integer = 0 To all_states.Count() - 1

            If all_states(i).Item(0).Item(0) = 0 Then 'a call arrives, exp=min
                final_prob.Add(((param_list(i).Item(0)) / (lambda_sum)) * param_list(i).Item(1) * integral_list(index).Item(1))
            End If
            If all_states(i).Item(0).Item(0) = 9 Then 'an amb dispathces, service is over, norm=min
                If sum_times = 0 Then
                    final_prob.Add(integral_list(index).Item(2))
                Else
                    final_prob.Add(integral_list(index).Item(2) * (current_time - param_list(i).Item(2)) / (sum_times))
                End If
            End If
            If all_states(i).Item(0).Item(0) = 2 Then 'an amb reaches the scene, travel is over, constant=min
                If param_list(i).Count() > 2 Then
                    final_prob.Add((1 / 3) * 0.5 * Erfc((current_time - 56.7) / (13.6 * Math.Sqrt(2))) * prob_scene_hospital * param_list(i).Item(1) * integral_list(index).Item(3))
                Else
                    final_prob.Add(0.5 * Erfc((current_time - 54.18) / (15.8 * Math.Sqrt(2))) * (1 - prob_scene_hospital) * integral_list(index).Item(3))
                End If
            End If
            If all_states(i).Item(0).Item(0) = 3 Then 'an amb is finished on the scene, service is over, norm=min
                If sum_times = 0 Then
                    final_prob.Add(integral_list(index).Item(2))
                Else
                    final_prob.Add(integral_list(index).Item(2) * (current_time - param_list(i).Item(2)) / (sum_times))
                End If
            End If
            If all_states(i).Item(0).Item(0) = 4 Then 'an amb is finished at the hospital, service is over, norm=mi
                If sum_times = 0 Then
                    final_prob.Add(integral_list(index).Item(2))
                Else
                    final_prob.Add(integral_list(index).Item(2) * (current_time - param_list(i).Item(2)) / (sum_times))
                End If
            End If
            If all_states(i).Item(0).Item(0) = 5 Then 'an amb arrives at the base, travel is over, const=min
                If param_list(i).Count() > 1 Then
                    'final_prob.Add(param_list(i).Item(1) * integral_list(index).Item(3))
                    'Else
                    final_prob.Add(integral_list(index).Item(3))
                End If
            End If
        Next

        Dim future_index As Integer = final_prob.IndexOf(final_prob.Max())
        Return all_states(future_index)
    End Function
    ''not used
    Public Function factorial(num As Long)
        If num <= 1 Then
            Return (1)
        Else
            Return num * Factorial(num - 1)
        End If
    End Function
    ''not used
    Public Function find_min_delta()
        Dim min_delta = Double.PositiveInfinity
        For i As Integer = 0 To all_states.Count() - 1 Step 1
            If all_states(i).Item(0).Item(0) = 2 Or all_states(i).Item(0).Item(0) = 5 Then
                If param_list(i).Item(0) - param_list(i).Item(1) <= min_delta Then
                    min_delta = param_list(i).Item(0)
                End If
            End If
        Next
        Return min_delta
    End Function

    Public Sub one_step_simulation(ByVal state As List(Of List(Of Integer)), ByVal current_time As Double)
        Dim expected_cost_one_step_simu As Double
        For one_step_simu_rep As Integer = 0 To 4 Step 1
            Dim exp_residual_times As New List(Of List(Of Double))
            For i As Integer = 0 To node_lambdas.Count() - 1 Step 1
                Dim time_location As New List(Of Double)
                If node_lambdas(i) <> 0 Then
                    time_location.Add(expo_random_generator(1 / node_lambdas(i))) 'time
                    time_location.Add(i)
                    exp_residual_times.Add(time_location)
                End If
            Next
            Dim const_residual_times As New List(Of List(Of Double))
            For i As Integer = 1 To dynamic_ambulance_number Step 1
                Dim time_ambnum As New List(Of Double)
                If state(i).Item(0) = 1 Or state(i).Item(0) = 4 Or state(i).Item(0) = 5 Then
                    If Form4.amb_list(i - 1) <> 0 And travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)) - (current_time - Form4.amb_list(i - 1)) >= 0 Then
                        time_ambnum.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)) - (current_time - Form4.amb_list(i - 1)))
                        time_ambnum.Add(i) 'i=1,2,..
                        const_residual_times.Add(time_ambnum)
                    Else
                        time_ambnum.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)))
                        time_ambnum.Add(i) 'i=1,2,..
                        const_residual_times.Add(time_ambnum)
                    End If
                End If
            Next
            Dim norm_residual_times As New List(Of List(Of Double))
            For i As Integer = 1 To dynamic_ambulance_number Step 1
                Dim time_ambnum As New List(Of Double)
                If state(i).Item(0) = 2 Then
                    time_ambnum.Add((1 - r2.NextDouble()) * (1 - (1 / 2 * (1 + MathNet.Numerics.SpecialFunctions.Erf((current_time - amb_list(i - 1) - 54.18) / (Sqrt(2) * 15.18))))))
                    time_ambnum.Add(i) 'i=1,2,
                    norm_residual_times.Add(time_ambnum)
                End If
                If state(i).Item(0) = 3 Then
                    time_ambnum.Add((1 - r2.NextDouble()) * (1 - (1 / 2 * (1 + MathNet.Numerics.SpecialFunctions.Erf((current_time - amb_list(i - 1) - 56.7) / (Sqrt(2) * 13.6))))))
                    time_ambnum.Add(i)
                    norm_residual_times.Add(time_ambnum)
                End If
            Next
            Dim exp_min_time As Double = (From i In exp_residual_times Order By i(0) Ascending Select i).First().Item(0)
            Dim exp_min_index As Integer = (From i In exp_residual_times Order By i(0) Ascending Select i).First().Item(1)
            Dim const_min_time As Double
            Dim norm_min_time As Double
            Dim const_min_index As Integer
            Dim norm_min_index As Integer

            If const_residual_times.Count() > 0 Then
                const_min_time = (From i In const_residual_times Order By i(0) Ascending Select i).First().Item(0)
                const_min_index = (From i In const_residual_times Order By i(0) Ascending Select i).First().Item(1)
            End If
            If norm_residual_times.Count() > 0 Then
                norm_min_time = (From i In norm_residual_times Order By i(0) Ascending Select i).First().Item(0)
                norm_min_index = (From i In norm_residual_times Order By i(0) Ascending Select i).First().Item(1)
            End If

            Dim all_states As New List(Of List(Of List(Of Integer)))

            If (exp_min_time <= const_min_time And norm_min_time = 0) Or
                (exp_min_time <= norm_min_time And const_min_time = 0) Or
                (exp_min_time <= const_min_time And exp_min_time <= norm_min_time) Or
                (const_min_time = 0 And norm_min_time = 0) Then 'a call arrives sooner
                Dim new_state As List(Of List(Of Integer)) = state.Select(Function(innerlist) innerlist.ToList).ToList
                Dim call_state As New List(Of Integer)
                new_state(0).Item(0) = 0 'event call arrives

                call_state.Add(0) 'call status
                call_state.Add(exp_min_index) 'location

                If r3.NextDouble > prob_priority Then
                    priority = 1
                Else
                    priority = 0
                End If
                call_state.Add(priority) 'priority

                new_state.Add(call_state)
                all_states.Add(new_state)
            End If
            If ((const_min_time > 0) And ((const_min_time <= exp_min_time And norm_min_time = 0) Or
                (const_min_time <= exp_min_time And const_min_time <= norm_min_time))) Then 'a travel time finihes sooner
                Dim new_state As List(Of List(Of Integer)) = state.Select(Function(innerlist) innerlist.ToList).ToList
                If state(const_min_index).Item(0) = 1 Then 'if it was already at the scene
                    new_state(0).Item(0) = 2 'event amb reached
                    Dim rand = r5.NextDouble()
                    If rand > prob_scene_hospital Then
                        new_state(const_min_index).Item(0) = 2 ' amb is at the scene
                        new_state(const_min_index).Item(1) = state(const_min_index).Item(2) 'update origin
                        all_states.Add(new_state)
                    Else
                        new_state(const_min_index).Item(0) = 3 ' amb is going to hospital 91
                        new_state(const_min_index).Item(1) = state(const_min_index).Item(2) 'update origin
                        Dim hospital As Integer = Int(r4.NextDouble() * 3)
                        If hospital = 0 Then
                            new_state(const_min_index).Item(2) = 91
                        ElseIf hospital = 1 Then
                            new_state(const_min_index).Item(2) = 37
                        Else
                            new_state(const_min_index).Item(2) = 66
                        End If
                    End If
                    all_states.Add(new_state)
                End If
                If state(const_min_index).Item(0) = 4 Or state(const_min_index).Item(0) = 5 Then 'if it was already finished
                    new_state = state.Select(Function(innerlist) innerlist.ToList).ToList
                    new_state(0).Item(0) = 5 'event amb is at base
                    new_state(const_min_index).Item(0) = 0 'amb is available
                    new_state(const_min_index).Item(1) = state(const_min_index).Item(2) 'origin
                    new_state(const_min_index).Item(2) = 0 'destination
                    all_states.Add(new_state)
                End If
            End If
            If ((norm_min_time > 0) And ((norm_min_time <= exp_min_time And const_min_time = 0) Or
                (norm_min_time <= exp_min_time And norm_min_time <= const_min_time))) Then
                Dim new_state As List(Of List(Of Integer)) = state.Select(Function(innerlist) innerlist.ToList).ToList
                If state(norm_min_index).Item(0) = 2 Then 'if it was already serving at the scene
                    If state.Count() > dynamic_ambulance_number + 1 Then 'if there is a call
                        For j As Integer = dynamic_ambulance_number + 1 To state.Count() - 1 Step 1
                            If state(j).Item(0) = 0 Then 'if call was unassigned
                                new_state(0).Item(0) = 1 'amb is dispatched
                                new_state(norm_min_index).Item(0) = 1 'amb is dispatched
                                new_state(norm_min_index).Item(2) = state(j).Item(1) 'dest is call location
                                new_state(j).Item(0) = 1 'call is assigned
                                all_states.Add(new_state)
                                Exit For
                            Else
                                new_state = state.Select(Function(innerlist) innerlist.ToList).ToList
                                new_state(0).Item(0) = 3 'amb is finished
                                new_state(norm_min_index).Item(0) = 0 'amb is finished
                                new_state(norm_min_index).Item(2) = 0 'amb is available
                                all_states.Add(new_state)
                                Exit For
                            End If
                        Next
                    Else
                        new_state = state.Select(Function(innerlist) innerlist.ToList).ToList
                        new_state(0).Item(0) = 3 'amb is finished
                        new_state(norm_min_index).Item(0) = 0 'amb is finished
                        new_state(norm_min_index).Item(2) = 0 'amb is available
                        all_states.Add(new_state)
                    End If
                End If
                If state(norm_min_index).Item(0) = 3 Then
                    If state.Count() > dynamic_ambulance_number + 1 Then 'if there is a call
                        For j As Integer = dynamic_ambulance_number + 1 To state.Count() - 1 Step 1
                            If state(j).Item(0) = 0 Then 'if call was unassigned
                                new_state(0).Item(0) = 1 'amb is dispatched
                                new_state(norm_min_index).Item(0) = 1 'amb is dispatched
                                new_state(norm_min_index).Item(2) = state(j).Item(1) 'dest is call location
                                new_state(j).Item(0) = 1 'call is assigned
                                all_states.Add(new_state)
                                Exit For
                            Else
                                new_state = state.Select(Function(innerlist) innerlist.ToList).ToList
                                new_state(0).Item(0) = 3 'amb is finished
                                new_state(norm_min_index).Item(0) = 0 'amb is finished
                                new_state(norm_min_index).Item(2) = 0 'amb is available
                                all_states.Add(new_state)
                                Exit For
                            End If
                        Next
                    Else
                        new_state = state.Select(Function(innerlist) innerlist.ToList).ToList
                        new_state(0).Item(0) = 3 'amb is finished
                        new_state(norm_min_index).Item(0) = 0 'amb is finished
                        new_state(norm_min_index).Item(2) = 0 'amb is available
                        all_states.Add(new_state)
                    End If
                End If
            End If
            expected_cost_one_step_simu += expected_cost(all_states, current_time, Min(exp_min_time, Min(norm_min_time, const_min_time)))
        Next
        e_cost = expected_cost_one_step_simu / 5
    End Sub
    Public Sub action_state_development(ByVal state As List(Of List(Of Integer)), ByVal current_time As Double)
        all_states = New List(Of List(Of List(Of Integer)))
        Dim temp_state As New List(Of Integer)
        Dim po_states As New List(Of List(Of Integer))
        'lamda_of_l_initialization()
        Dim sum_lambda As New List(Of Double)
        param_list = New List(Of List(Of Double))
        Dim p_list As New List(Of Double)
        min_index = New Integer
        ''create: all possible states regarding new call arrives

        For l As Integer = 0 To node_lambdas.Count() - 1 Step 1 'region
            If node_lambdas(l) > 0 Then 'if the region has a reasonable lambda then add the call from that region
                For n As Integer = 0 To 1 Step 1
                    temp_state = New List(Of Integer)
                    po_states = New List(Of List(Of Integer))
                    temp_state.Add(0)
                    temp_state.Add(l)
                    temp_state.Add(n)
                    po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                    po_states(0).Item(0) = 0 'event
                    po_states.Add(temp_state)
                    all_states.Add(po_states)

                    'add arrival time parameter
                    p_list = New List(Of Double)
                    p_list.Add(1 / node_lambdas(l))
                    If n = 0 Then
                        p_list.Add(prob_priority)
                    Else
                        p_list.Add(1 - prob_priority)
                    End If

                    param_list.Add(p_list)
                Next
                sum_lambda.Add(1 / node_lambdas(l))
            End If
        Next

        ''create: all possible states regarding ambulances reaching call locations



        For i As Integer = 1 To dynamic_ambulance_number Step 1
            If state(i).Item(0) = 1 Then
                po_states = New List(Of List(Of Integer))
                po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                po_states(0).Item(0) = 2 'event
                po_states(i).Item(0) = 2 'amb state is at the scene
                po_states(i).Item(1) = state(i).Item(2) 'amb destination is amb location at the scene
                all_states.Add(po_states)
                'add travel time parameter
                p_list = New List(Of Double)
                p_list.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2))) 'add travel times
                p_list.Add(Form4.M_state(i - 1).Item(3)) 'add start time of that travel time
                param_list.Add(p_list)

                po_states = New List(Of List(Of Integer))
                po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                po_states(0).Item(0) = 2 'event
                po_states(i).Item(0) = 3 'amb state is at the hospital
                po_states(i).Item(1) = state(i).Item(2) 'amb is located at the call scene
                po_states(i).Item(2) = 91 'amb is going to one of the hospitals
                all_states.Add(po_states)
                'add travel time parameter
                p_list = New List(Of Double)
                p_list.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)))
                p_list.Add(1 / 3)
                p_list.Add(Form4.M_state(i - 1).Item(3))
                param_list.Add(p_list)

                po_states = New List(Of List(Of Integer))
                po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                po_states(0).Item(0) = 2 'event
                po_states(i).Item(0) = 3 'amb state is at the hospital
                po_states(i).Item(1) = state(i).Item(2) 'amb is located at the call scene
                po_states(i).Item(2) = 37 'amb is going to one of the hospitals
                all_states.Add(po_states)
                'add travel time parameter
                p_list = New List(Of Double)
                p_list.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)))
                p_list.Add(1 / 3)
                p_list.Add(Form4.M_state(i - 1).Item(3))
                param_list.Add(p_list)

                po_states = New List(Of List(Of Integer))
                po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                po_states(0).Item(0) = 2 'event
                po_states(i).Item(0) = 3 'amb state is at the hospital
                po_states(i).Item(1) = state(i).Item(2) 'amb is located at the call scene
                po_states(i).Item(2) = 66 'amb is going to one of the hospitals
                all_states.Add(po_states)
                'add travel time parameter
                p_list = New List(Of Double)
                p_list.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)))
                p_list.Add(1 / 3)
                p_list.Add(Form4.M_state(i - 1).Item(3))
                param_list.Add(p_list)
            End If
        Next

        ''create: all possible states regarding ambualnces finishe their jobs

        'at the scene
        For i As Integer = 1 To dynamic_ambulance_number Step 1
            If state(i).Item(0) = 2 Then 'if amb is at the scene
                If state.Count() > dynamic_ambulance_number + 1 Then 'if there is a call
                    For j As Integer = dynamic_ambulance_number + 1 To state.Count() - 1 Step 1
                        If state(j).Item(0) = 0 Then 'call is unassigned
                            po_states = New List(Of List(Of Integer))
                            po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                            po_states(0).Item(0) = 9 'event is dispatching
                            po_states(i).Item(0) = 1 'amb state is dispatched
                            po_states(i).Item(2) = state(j).Item(1) 'amb destination is call location
                            po_states(j).Item(0) = 1 'call state is assigned
                            all_states.Add(po_states)
                            'add servie time parameters
                            p_list = New List(Of Double)
                            p_list.Add(54.18)
                            p_list.Add(15.18)
                            p_list.Add(Form4.M_state(i - 1).Item(3))
                            param_list.Add(p_list)

                        End If
                    Next
                Else 'no call in the queue
                    po_states = New List(Of List(Of Integer))
                    po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                    po_states(0).Item(0) = 3 'event is amb is finished
                    po_states(i).Item(0) = 3 'amb state is amb is finished
                    po_states(i).Item(2) = po_states(i).Item(1) 'amb loc is the same as amb des
                    all_states.Add(po_states)
                    'add servie time parameters
                    p_list = New List(Of Double)
                    p_list.Add(54.18)
                    p_list.Add(15.18)
                    p_list.Add(Form4.M_state(i - 1).Item(3))
                    param_list.Add(p_list)
                End If
            End If
        Next

        'ambulance finish at hospital
        For i As Integer = 1 To 17 Step 1
            If state(i).Item(0) = 3 Then 'if amb is at the hospital
                If state.Count() > dynamic_ambulance_number + 1 Then 'if there is a call
                    For j As Integer = dynamic_ambulance_number + 1 To state.Count() - 1 Step 1
                        If state(j).Item(0) = 0 Then 'call is unassigned
                            po_states = New List(Of List(Of Integer))
                            po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                            po_states(0).Item(0) = 9 'event is dispatching
                            po_states(i).Item(0) = 1 'amb state is dispatched
                            po_states(i).Item(2) = state(j).Item(1) 'amb destination is call location
                            po_states(j).Item(0) = 1 'call state is assigned
                            all_states.Add(po_states)
                            'add servie time parameters
                            p_list = New List(Of Double)
                            p_list.Add(56.7)
                            p_list.Add(13.6)
                            p_list.Add(Form4.M_state(i - 1).Item(3))
                            param_list.Add(p_list)

                        End If
                    Next
                Else 'no call in the queue
                    po_states = New List(Of List(Of Integer))
                    po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                    po_states(0).Item(0) = 4 'event is amb is finished
                    po_states(i).Item(0) = 3 'amb state is amb is finished
                    po_states(i).Item(2) = po_states(i).Item(1) 'amb loc is the same as amb des
                    all_states.Add(po_states)
                    'add servie time parameters
                    p_list = New List(Of Double)
                    p_list.Add(56.7)
                    p_list.Add(13.6)
                    p_list.Add(Form4.M_state(i - 1).Item(3))
                    param_list.Add(p_list)
                End If
            End If
        Next

        ''create: all possible states regarding ambulance arriving at the base

        For i As Integer = 1 To dynamic_ambulance_number Step 1
            If state(i).Item(0) = 4 Or state(i).Item(0) = 5 Then 'if amb is redeploying or reallocating
                po_states = New List(Of List(Of Integer))
                po_states = state.Select(Function(innerlist) innerlist.ToList).ToList
                po_states(0).Item(0) = 5 'event is amb  arrives to base
                po_states(i).Item(0) = 0 'amb state is at the base
                po_states(i).Item(1) = state(i).Item(2) 'amb loc is amb des
                po_states(i).Item(2) = 0
                all_states.Add(po_states)
                'add travel time parameter
                p_list = New List(Of Double)
                p_list.Add(travel_lambdas(state(i).Item(1)).Item(state(i).Item(2)))
                p_list.Add(Form4.M_state(i - 1).Item(3))
                param_list.Add(p_list)
            End If
        Next
        Dim lambda_sum As Double
        For i As Integer = 0 To param_list.Count() - 1 Step 1
            If all_states(i).Item(0).Item(0) = 0 Then
                lambda_sum += param_list(i).Item(0)
            End If
        Next
        ''calculate: probabilities of transitions
        Dim final_prob As New List(Of Double)
        Dim min_del As Double = find_min_delta()
        Dim index As Integer = (From integ As List(Of Double) In integral_list Where integ.Item(0) = min_del Select integral_list.IndexOf(integ)).FirstOrDefault()
        Dim sum_times As Double
        For i As Integer = 0 To all_states.Count() - 1 Step 1
            If all_states(i).Item(0).Item(0) = 9 Or all_states(i).Item(0).Item(0) = 3 Or all_states(i).Item(0).Item(0) = 4 Then
                sum_times += current_time - param_list(i).Item(2)
            End If
        Next

        For i As Integer = 0 To all_states.Count() - 1

            If all_states(i).Item(0).Item(0) = 0 Then 'a call arrives, exp=min
                final_prob.Add(((param_list(i).Item(0)) / (lambda_sum)) * param_list(i).Item(1) * integral_list(index).Item(1))
            End If
            If all_states(i).Item(0).Item(0) = 9 Then 'an amb dispathces, service is over, norm=min
                If sum_times = 0 Then
                    final_prob.Add(integral_list(index).Item(2))
                Else
                    final_prob.Add(integral_list(index).Item(2) * (current_time - param_list(i).Item(2)) / (sum_times))
                End If
            End If
            If all_states(i).Item(0).Item(0) = 2 Then 'an amb reaches the scene, travel is over, constant=min
                If param_list(i).Count() > 2 Then
                    final_prob.Add((1 / 3) * 0.5 * Erfc((current_time - 56.7) / (13.6 * Math.Sqrt(2))) * prob_scene_hospital * param_list(i).Item(1) * integral_list(index).Item(3))
                Else
                    final_prob.Add(0.5 * Erfc((current_time - 54.18) / (15.8 * Math.Sqrt(2))) * (1 - prob_scene_hospital) * integral_list(index).Item(3))
                End If
            End If
            If all_states(i).Item(0).Item(0) = 3 Then 'an amb is finished on the scene, service is over, norm=min
                If sum_times = 0 Then
                    final_prob.Add(integral_list(index).Item(2))
                Else
                    final_prob.Add(integral_list(index).Item(2) * (current_time - param_list(i).Item(2)) / (sum_times))
                End If
            End If
            If all_states(i).Item(0).Item(0) = 4 Then 'an amb is finished at the hospital, service is over, norm=mi
                If sum_times = 0 Then
                    final_prob.Add(integral_list(index).Item(2))
                Else
                    final_prob.Add(integral_list(index).Item(2) * (current_time - param_list(i).Item(2)) / (sum_times))
                End If
            End If
            If all_states(i).Item(0).Item(0) = 5 Then 'an amb arrives at the base, travel is over, const=min
                If param_list(i).Count() > 1 Then
                    'final_prob.Add(param_list(i).Item(1) * integral_list(index).Item(3))
                    'Else
                    final_prob.Add(integral_list(index).Item(3))
                End If
            End If
        Next

        Dim sum As Double = final_prob.Sum()

        ''''calculate: final probability

        Dim probs_with_index As New List(Of List(Of Double))
        For i = 0 To final_prob.Count() - 1 Step 1
            Dim temp_probs_with_index As New List(Of Double)
            temp_probs_with_index.Add(final_prob(i))
            temp_probs_with_index.Add(i)
            probs_with_index.Add(temp_probs_with_index)
        Next
        probs_with_index = probs_with_index.OrderByDescending(Function(x) x(0)).ToList()

        Dim cumulative_prob As Double
        Dim index_list As New List(Of Integer)
        For i As Integer = 0 To probs_with_index.Count() - 1
            cumulative_prob = cumulative_prob + probs_with_index(i).Item(0)
            index_list.Add(probs_with_index(i).Item(1))
            If cumulative_prob >= 0.5 Then
                Exit For
            End If
        Next

        '' call cost module
        'e_cost = expected_cost(all_states, final_prob, index_list, current_time) 'expected cost with regards to possible states and probs
        'If inner_iteration = 1 Then
        '    cost_sb.Append(e_cost.ToString() + vbTab)
        'End If
    End Sub
    Public Function fixed_action(ByVal state As String, ByVal eve As Integer, ByVal wtf_finish As Integer, ByVal current_time As Double)
        'Dim write_cost As StreamWriter = New StreamWriter("c:\users\snasrol\desktop\Action costs3.txt", True)
        'cost_sb = New StringBuilder
        Dim int_state As New List(Of List(Of Integer))
        Dim int_temp_state As New List(Of Integer)
        int_temp_state.Add(Convert.ToInt32(state.ElementAt(1).ToString()))
        int_state.Add(int_temp_state)
        For x As Integer = state.IndexOf("[") To state.IndexOf("]")
            int_temp_state = New List(Of Integer)
            If state.ElementAt(x) = "(" Then
                int_temp_state.Add(Convert.ToInt32(state.ElementAt(x + 1).ToString()))
                int_temp_state.Add(Convert.ToInt32(state.Substring(state.IndexOf(",", x) + 2, state.IndexOf(",", state.IndexOf(",", x) + 1) - state.IndexOf(",", x) - 2)))
                int_temp_state.Add(Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", x)) + 2, state.IndexOf(")", x) - state.IndexOf(",", state.IndexOf(" ", x)) - 2)))
                int_state.Add(int_temp_state)
            End If
        Next
        For x As Integer = state.LastIndexOf("[") To state.LastIndexOf("]")
            int_temp_state = New List(Of Integer)
            If state.ElementAt(x) = "(" Then
                int_temp_state.Add(Convert.ToInt32(state.ElementAt(x + 1).ToString()))
                int_temp_state.Add(Convert.ToInt32(state.Substring(state.IndexOf(",", x) + 2, state.IndexOf(",", state.IndexOf(",", x) + 1) - state.IndexOf(",", x) - 2)))
                int_temp_state.Add(Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", x)) + 2, state.IndexOf(")", x) - state.IndexOf(",", state.IndexOf(" ", x)) - 2)))
                int_state.Add(int_temp_state)
            End If
        Next
        Dim new_int_state As New List(Of List(Of Integer))
        new_int_state = int_state.Select(Function(innerlist) innerlist.ToList).ToList

        Dim m As Integer
        Dim c As Integer
        Dim policy_evaluation As New Dictionary(Of String, Double)
        ''Count: number of calls in the state
        If int_state.Count() >= dynamic_ambulance_number + 1 Then
            For t As Integer = dynamic_ambulance_number + 1 To int_state.Count() - 1 Step 1
                c += 1
            Next
        End If

        ''count: number of ambulances
        m = dynamic_ambulance_number

        Dim dispatch_action As New List(Of List(Of Integer))
        Dim reallocate_action As New List(Of List(Of Integer))
        Dim redeploy_action As New List(Of List(Of Integer))

        ''if eve = 0 = call arrives, eve = 1 = ambuance finished
        ''''all possible action, combination of dispatching and reallocating

        'If iteration = 0 Then
        'GoTo 2
        'End If

        'GoTo 2 'run closest dispatch policy
        If eve = 0 Then
            Dim unassigned_call_first_ind As Integer = -1
            Dim dispatched As Boolean = False
            If c >= 1 Then
                For i As Integer = dynamic_ambulance_number + 1 To int_state.Count() - 1 Step 1
                    If int_state(i).Item(0) = 0 And Form4.c_state(i - (dynamic_ambulance_number + 1)).Item(3) <= current_time Then 'unassigned call and high priority
                        unassigned_call_first_ind = i
                        If int_state(i).Item(2) = 1 Then 'if call is high priority
                            For j As Integer = 1 To dynamic_ambulance_number Step 1
                                If int_state(j).Item(0) = 0 Then 'available ambulance
                                    dispatched = True
                                    Dim dispatch As New List(Of Integer)
                                    dispatch.Add(j - 1)
                                    dispatch.Add(i - (dynamic_ambulance_number + 1))
                                    dispatch_action.Add(dispatch)
                                    For k As Integer = 1 To dynamic_ambulance_number Step 1
                                        If int_state(k).Item(0) = 0 And k <> j Then 'availabe different ambulance
                                            ''For l = 0 To ambulance_potential_base.Count() - 1 Step 1 '(limiting the reallocation to the base that just emptied)
                                            If int_state(k).Item(1) <> int_state(j).Item(1) Then
                                                Dim allocation As New List(Of Integer)
                                                allocation.Add(j - 1)
                                                allocation.Add(k - 1)
                                                allocation.Add(int_state(j).Item(1))
                                                reallocate_action.Add(allocation)
                                            End If
                                            ''Next
                                        End If
                                    Next
                                End If
                            Next
                            Exit For 'just dispatch for the first unassigned call
                        End If
                        Exit For
                    End If
                Next
                If dispatched = False And unassigned_call_first_ind > -1 Then 'if no high priority call were in the queue
                    For j As Integer = 1 To dynamic_ambulance_number Step 1
                        If int_state(j).Item(0) = 0 Then 'available ambulance
                            Dim dispatch As New List(Of Integer)
                            dispatch.Add(j - 1)
                            dispatch.Add(unassigned_call_first_ind - (dynamic_ambulance_number + 1))
                            dispatch_action.Add(dispatch)
                            For k As Integer = 1 To dynamic_ambulance_number Step 1
                                If int_state(k).Item(0) = 0 And k <> j Then 'availabe different ambulance
                                    ''For l = 0 To ambulance_potential_base.Count() - 1 Step 1 '(limiting the reallocation to the base that just emptied)
                                    If int_state(k).Item(1) <> int_state(j).Item(1) Then
                                        Dim allocation As New List(Of Integer)
                                        allocation.Add(j - 1)
                                        allocation.Add(k - 1)
                                        allocation.Add(int_state(j).Item(1))
                                        reallocate_action.Add(allocation)
                                    End If
                                    ''Next
                                End If
                            Next
                        End If
                    Next
                End If
            End If
        End If

        GoTo 3 'not run closest dispatch policy


        ''Policy: Dispatch Closest

2:      If eve = 0 Then
            Dim unassigned_call_first_ind As Integer = -1
            Dim dispatched As Boolean = False
            If c >= 1 Then
                For i As Integer = (dynamic_ambulance_number + 1) To int_state.Count() - 1 Step 1
                    If int_state(i).Item(0) = 0 And Form4.c_state(i - (dynamic_ambulance_number + 1)).Item(3) <= current_time Then 'unassigned call
                        unassigned_call_first_ind = i
                        If int_state(i).Item(2) = 1 Then 'high priority call
                            For j As Integer = 1 To dynamic_ambulance_number Step 1
                                If int_state(j).Item(0) = 0 Then 'available ambulance
                                    closer_amb_distance(j - 1).Item(0) = 0
                                    closer_amb_distance(j - 1).Item(1) = node_distances(int_state(j).Item(1)).Item(int_state(i).Item(1))
                                Else
                                    closer_amb_distance(j - 1).Item(0) = 1
                                    closer_amb_distance(j - 1).Item(1) = 1000
                                End If
                            Next
                            Dim min_distance = (From z In closer_amb_distance Order By z(1) Ascending Select z(1)).ElementAt(0)
                            Dim dispatch As New List(Of Integer)
                            Dim n As Integer = 0
                            For n = 0 To dynamic_ambulance_number - 1 Step 1
                                If closer_amb_distance(n).Item(0) = 0 And closer_amb_distance(n).Item(1) = min_distance Then
                                    dispatch.Add(n)
                                    dispatch.Add(i - (dynamic_ambulance_number + 1))
                                    dispatch_action.Add(dispatch)
                                    dispatched = True
                                    'For k As Integer = 1 To dynamic_ambulance_number Step 1
                                    '    If int_state(k).Item(0) = 0 And k <> n + 1 Then 'availabe different ambulance
                                    '        If int_state(k).Item(1) <> int_state(n + 1).Item(1) Then
                                    '            Dim allocation As New List(Of Integer)
                                    '            allocation.Add(n)
                                    '            allocation.Add(k - 1)
                                    '            allocation.Add(int_state(n + 1).Item(1))
                                    '            reallocate_action.Add(allocation)
                                    '        End If
                                    '    End If
                                    'Next
                                    Exit For
                                End If
                            Next
                            Exit For 'just dispatch for the first unassigned call
                        End If
                        Exit For
                    End If
                Next
                If dispatched = False And unassigned_call_first_ind > -1 Then
                    For j As Integer = 1 To dynamic_ambulance_number Step 1
                        If int_state(j).Item(0) = 0 Then 'available ambulance
                            closer_amb_distance(j - 1).Item(0) = 0
                            closer_amb_distance(j - 1).Item(1) = node_distances(int_state(j).Item(1)).Item(int_state(unassigned_call_first_ind).Item(1))
                        Else
                            closer_amb_distance(j - 1).Item(0) = 1
                            closer_amb_distance(j - 1).Item(1) = 1000
                        End If
                    Next
                    Dim min_distance = (From z In closer_amb_distance Order By z(1) Ascending Select z(1)).ElementAt(0)
                    Dim dispatch As New List(Of Integer)
                    Dim n As Integer = 0
                    For n = 0 To dynamic_ambulance_number - 1 Step 1
                        If closer_amb_distance(n).Item(0) = 0 And closer_amb_distance(n).Item(1) = min_distance Then
                            dispatch.Add(n)
                            dispatch.Add(unassigned_call_first_ind - (dynamic_ambulance_number + 1))
                            dispatch_action.Add(dispatch)
                            dispatched = True
                            'For k As Integer = 1 To dynamic_ambulance_number Step 1
                            '    If int_state(k).Item(0) = 0 And k <> n + 1 Then 'availabe different ambulance
                            '        If int_state(k).Item(1) <> int_state(n + 1).Item(1) Then
                            '            Dim allocation As New List(Of Integer)
                            '            allocation.Add(n)
                            '            allocation.Add(k - 1)
                            '            allocation.Add(int_state(n + 1).Item(1))
                            '            reallocate_action.Add(allocation)
                            '        End If
                            '    End If
                            'Next
                            Exit For
                        End If
                    Next
                End If
            End If
        End If

        ''''all possible actions, combination of all redeployment
3:      Dim queue As Boolean = False
        Dim sent_ambulance As Boolean = False

        'GoTo 4 'running closest dispatch after redeployment

        If eve = 1 Then
            If c >= 1 Then
                Dim unassigned_first_call_ind As Integer = -1
                Dim dispatched As Boolean = False
                For i As Integer = (dynamic_ambulance_number + 1) To int_state.Count() - 1 Step 1
                    If int_state(i).Item(0) = 0 And Form4.c_state(i - (dynamic_ambulance_number + 1)).Item(3) <= current_time Then
                        unassigned_first_call_ind = i
                        queue = True
                        If int_state(i).Item(2) = 1 Then 'high priority call
                            Dim nchar As String = state.Substring(state.LastIndexOf(",") + 1) 'gets the number of the finished ambulance
                            Dim dispatch As New List(Of Integer)
                            For j As Integer = 1 To dynamic_ambulance_number Step 1 'create action for every available ambulance
                                If int_state(j).Item(0) = 0 Then
                                    dispatch = New List(Of Integer)
                                    dispatch.Add(j - 1)
                                    dispatch.Add(i - (dynamic_ambulance_number + 1))
                                    dispatch_action.Add(dispatch)
                                    redeploy_action.Add(dispatch)
                                    sent_ambulance = True
                                    dispatched = True
                                End If
                            Next
                            If sent_ambulance = False Then 'create dispatch action for the finished ambulance
                                Dim j As Integer = Convert.ToInt32(nchar.ToString()) + 1
                                dispatched = True
                                dispatch = New List(Of Integer)
                                dispatch.Add(j - 1)
                                dispatch.Add(i - (dynamic_ambulance_number + 1))
                                dispatch_action.Add(dispatch)
                            End If
                            Exit For
                        End If
                        Exit For
                    End If
                Next
                If dispatched = False And unassigned_first_call_ind > -1 Then
                    Dim nchar As String = state.Substring(state.LastIndexOf(",") + 1) 'gets the number of the finished ambulance
                    Dim dispatch As New List(Of Integer)
                    For j As Integer = 1 To dynamic_ambulance_number Step 1
                        If int_state(j).Item(0) = 0 Then
                            dispatch = New List(Of Integer)
                            dispatch.Add(j - 1)
                            dispatch.Add(unassigned_first_call_ind - (dynamic_ambulance_number + 1))
                            dispatch_action.Add(dispatch)
                            redeploy_action.Add(dispatch)
                            dispatched = True
                            sent_ambulance = True
                        End If
                    Next
                    If sent_ambulance = False Then
                        Dim j As Integer = Convert.ToInt32(nchar.ToString()) + 1
                        dispatched = True
                        dispatch = New List(Of Integer)
                        dispatch.Add(j - 1)
                        dispatch.Add(unassigned_first_call_ind - (dynamic_ambulance_number + 1))
                        dispatch_action.Add(dispatch)
                    End If
                End If
            End If
            If (queue = False Or sent_ambulance = True) And (wtf_finish <> 5 And wtf_finish <> 6) Then
                Dim schar As String = state.Substring(state.LastIndexOf(",") + 1) 'gets the number of the finished ambulance
                Dim amb_number As Integer = Convert.ToInt32(schar)
                If redeploy_action.Count() > 0 Then
                    For k As Integer = 0 To redeploy_action.Count() - 1 Step 1
                        If amb_number <> redeploy_action(k).Item(0) Then
                            For n As Integer = 0 To ambulance_potential_base.Count() - 1 Step 1
                                Dim redeploy As New List(Of Integer)
                                redeploy_action(k).Add(amb_number) 'adds the number of redeploying ambulance to the existing list of dispatching amb and call
                                'redeploy_action(k).Add(form4.amb_ini_base_list(amb_number))  'adds the number of base to the existing list of dispatching amb and call
                                redeploy.Add(ambulance_potential_base(n))
                            Next
                        Else
                            Continue For
                        End If
                    Next
                Else
                    For n As Integer = 0 To ambulance_potential_base.Count() - 1 Step 1
                        Dim redeploy As New List(Of Integer)
                        redeploy.Add(-1) 'it means there is no dispatching ambulance
                        redeploy.Add(-1) 'it means there is no dispatching call
                        redeploy.Add(amb_number)
                        'redeploy.Add(form4.amb_ini_base_list(amb_number))
                        redeploy.Add(ambulance_potential_base(n))
                        redeploy_action.Add(redeploy)
                    Next
                End If
            End If
        End If

        GoTo 5 'not running closest dispatch after redeployment

4:      If eve = 1 Then
            If c >= 1 Then
                Dim unassigned_first_call_ind As Integer = -1
                Dim dispatched As Boolean = False
                For i As Integer = (dynamic_ambulance_number + 1) To int_state.Count() - 1 Step 1
                    ''error
                    If int_state(i).Item(0) = 0 And Form4.c_state(i - (dynamic_ambulance_number + 1)).Item(3) <= current_time Then 'unassigned call
                        unassigned_first_call_ind = i
                        queue = True
                        If int_state(i).Item(2) = 1 Then 'high priority calls
                            Dim nchar As String = state.Substring(state.LastIndexOf(",") + 1) 'gets the number of the finished ambulance
                            For j As Integer = 1 To dynamic_ambulance_number Step 1
                                If int_state(j).Item(0) = 0 Or j = (Convert.ToInt32(nchar)) + 1 Then 'available ambulance
                                    closer_amb_distance(j - 1).Item(0) = 0
                                    closer_amb_distance(j - 1).Item(1) = node_distances(int_state(j).Item(1)).Item(int_state(i).Item(1))
                                Else
                                    closer_amb_distance(j - 1).Item(0) = 1
                                    closer_amb_distance(j - 1).Item(1) = 1000
                                End If
                            Next
                            Dim min_distance = (From z In closer_amb_distance Order By z(1) Ascending Select z(1)).ElementAt(0)
                            Dim dispatch As New List(Of Integer)
                            Dim schar As String = state.Substring(state.LastIndexOf(",") + 1) 'gets the number of the finished ambulance
                            For n = 0 To dynamic_ambulance_number - 1 Step 1
                                If closer_amb_distance(n).Item(0) = 0 And closer_amb_distance(n).Item(1) = min_distance Then
                                    If n <> Convert.ToInt32(schar.ToString()) Then
                                        dispatch.Add(n)
                                        dispatch.Add(i - (dynamic_ambulance_number + 1))
                                        dispatch_action.Add(dispatch)
                                        redeploy_action.Add(dispatch)
                                        sent_ambulance = True
                                        dispatched = True
                                        Exit For
                                    Else
                                        dispatch = New List(Of Integer)
                                        dispatch.Add(n)
                                        dispatch.Add(i - (dynamic_ambulance_number + 1))
                                        dispatch_action.Add(dispatch)
                                        dispatched = True
                                        Exit For
                                    End If
                                End If
                            Next
                            Exit For 'just dispatch for the first unassigned call
                        End If
                        Exit For
                    End If
                Next
                If dispatched = False And unassigned_first_call_ind > -1 Then 'if there is queue but no high priority calls
                    Dim nchar As String = state.Substring(state.LastIndexOf(",") + 1) 'gets the number of the finished ambulance
                    For j As Integer = 1 To dynamic_ambulance_number Step 1
                        If int_state(j).Item(0) = 0 Or j = (Convert.ToInt32(nchar)) + 1 Then 'available ambulance
                            closer_amb_distance(j - 1).Item(0) = 0
                            closer_amb_distance(j - 1).Item(1) = node_distances(int_state(j).Item(1)).Item(int_state(unassigned_first_call_ind).Item(1))
                        Else
                            closer_amb_distance(j - 1).Item(0) = 1
                            closer_amb_distance(j - 1).Item(1) = 1000
                        End If
                    Next
                    Dim min_distance = (From z In closer_amb_distance Order By z(1) Ascending Select z(1)).ElementAt(0)
                    Dim dispatch As New List(Of Integer)
                    Dim schar As String = state.Substring(state.LastIndexOf(",") + 1) 'gets the number of the finished ambulance
                    For n = 0 To dynamic_ambulance_number - 1 Step 1
                        If closer_amb_distance(n).Item(0) = 0 And closer_amb_distance(n).Item(1) = min_distance Then
                            If n <> Convert.ToInt32(schar.ToString()) Then
                                dispatch.Add(n)
                                dispatch.Add(unassigned_first_call_ind - (dynamic_ambulance_number + 1))
                                dispatch_action.Add(dispatch)
                                redeploy_action.Add(dispatch)
                                sent_ambulance = True
                                dispatched = True
                                Exit For
                            Else
                                dispatch = New List(Of Integer)
                                dispatch.Add(n)
                                dispatch.Add(unassigned_first_call_ind - (dynamic_ambulance_number + 1))
                                dispatch_action.Add(dispatch)
                                dispatched = True
                                Exit For
                            End If
                        End If
                    Next
                End If
            End If

            If (queue = False Or sent_ambulance = True) And (wtf_finish <> 5 And wtf_finish <> 6) Then
                ''Jagtenberg policy
                GoTo 6 'do not run Jagtenberg policy
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                Dim d() As Double = node_lambdas.ToArray()
                Dim sum As Double
                For i As Integer = 0 To d.Count() - 1 Step 1
                    If d(i) <> 0 Then
                        d(i) = 1 / d(i)
                        sum += d(i)
                    End If
                Next
                For i As Integer = 0 To d.Count() - 1 Step 1
                    d(i) = d(i) / sum
                Next
                Dim q As Double = 0.291
                Dim best_improvment As Double = 0
                Dim best_location As Integer = 0
                For j As Integer = 0 To ambulance_potential_base.Count() - 1 Step 1
                    Dim coverage_improvement As Double = 0
                    For i As Integer = 0 To 167 Step 1
                        Dim k As Integer = 0
                        If travel_lambdas(j).Item(i) < 8 Then
                            k += 1
                            For l As Integer = 1 To dynamic_ambulance_number Step 1
                                If int_state(l).Item(0) = 0 Then 'if amb is available
                                    k += 1
                                End If
                            Next
                            coverage_improvement += d(i) * (1 - q) * Pow(q, k - 1)
                        End If
                    Next
                    If coverage_improvement > best_improvment Then
                        best_location = j
                        best_improvment = coverage_improvement
                    End If
                Next
                Dim schar As String = state.Substring(state.LastIndexOf(",") + 1) 'gets the number of the finished ambulance
                Dim amb_number As Integer = Convert.ToInt32(schar)
                If redeploy_action.Count() > 0 Then
                    For k As Integer = 0 To redeploy_action.Count() - 1 Step 1
                        If amb_number <> redeploy_action(k).Item(0) Then
                            Dim redeploy As New List(Of Integer)
                            redeploy_action(k).Add(amb_number) 'adds the number of redeploying ambulance to the existing list of dispatching amb and call
                            redeploy.Add(best_location)
                        Else
                            Continue For
                        End If
                    Next
                Else
                    Dim redeploy As New List(Of Integer)
                    redeploy.Add(-1) 'it means there is no dispatching ambulance
                    redeploy.Add(-1) 'it means there is no dispatching call
                    redeploy.Add(amb_number)
                    redeploy.Add(best_location)
                    redeploy_action.Add(redeploy)
                End If
                GoTo 5 'do not run the general redeployment
                '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
6:              Dim tchar As String = state.Substring(state.LastIndexOf(",") + 1) 'gets the number of the finished ambulance
                Dim amb_numbert As Integer = Convert.ToInt32(tchar)
                If redeploy_action.Count() > 0 Then
                    For k As Integer = 0 To redeploy_action.Count() - 1 Step 1
                        If amb_numbert <> redeploy_action(k).Item(0) Then
                            'For n As Integer = 0 To ambulance_potential_base.Count() - 1 Step 1
                            Dim redeploy As New List(Of Integer)
                            redeploy_action(k).Add(amb_numbert) 'adds the number of redeploying ambulance to the existing list of dispatching amb and call
                            redeploy_action(k).Add(Form4.amb_ini_base_list(amb_numbert))  'adds the number of base to the existing list of dispatching amb and call
                            'redeploy_action(k).Add(ambulance_potential_base(n))
                            'Next
                        Else
                            Continue For
                        End If
                    Next
                Else
                    'For n As Integer = 0 To ambulance_potential_base.Count() - 1 Step 1
                    Dim redeploy As New List(Of Integer)
                    redeploy.Add(-1) 'it means there is no dispatching ambulance
                    redeploy.Add(-1) 'it means there is no dispatching call
                    redeploy.Add(amb_numbert)
                    redeploy.Add(Form4.amb_ini_base_list(amb_number))
                    'redeploy.Add(ambulance_potential_base(n))
                    redeploy_action.Add(redeploy)
                    'Next
                End If
            End If
        End If

        ''call appropriate module to create possible future state for dispatching and reallocation
5:      Dim to_cost As New List(Of Double)

        If c >= 1 And dispatch_action.Count() > 0 Then
            For i As Integer = 0 To dispatch_action.Count() - 1 Step 1
                new_int_state = int_state.Select(Function(innerlist) innerlist.ToList).ToList
                new_int_state(0).Item(0) = 1 'event is dispatching
                new_int_state(dispatch_action(i).Item(0) + 1).Item(0) = 1 'ambulance state is dispatched
                new_int_state(dispatch_action(i).Item(0) + 1).Item(2) = int_state(dispatch_action(i).Item(1) + (dynamic_ambulance_number + 1)).Item(1) 'ambulance destination is call location
                new_int_state(dispatch_action(i).Item(1) + (dynamic_ambulance_number + 1)).Item(0) = 1 'call state is assigned
                'action_state_development(new_int_state, current_time)
                one_step_simulation(new_int_state, current_time)
                If int_state(0).Item(0) = 0 Then
                    im_cost = immediate_cost(new_int_state, "A1", dispatch_action(i).Item(0) + 1, dispatch_action(i).Item(1) + 1, 10, current_time)
                Else
                    If int_state(0).Item(0) = 3 Or int_state(0).Item(0) = 4 Then
                        im_cost = immediate_cost(new_int_state, "A2", dispatch_action(i).Item(0) + 1, dispatch_action(i).Item(1) + 1, 10, current_time)
                    End If
                End If
                to_cost.Add(im_cost + e_cost)
                policy_evaluation.Add(("d," + (dispatch_action(i).Item(0)).ToString() + "," + (dispatch_action(i).Item(1)).ToString() + "," + "N" + "," + "N"), im_cost + e_cost) 'd is for dispatching
                For k As Integer = 0 To reallocate_action.Count() - 1 Step 1
                    If reallocate_action(k).Item(0) = dispatch_action(i).Item(0) Then
                        new_int_state = int_state.Select(Function(innerlist) innerlist.ToList).ToList
                        new_int_state(0).Item(0) = 1 'event is dispatching
                        new_int_state(dispatch_action(i).Item(0) + 1).Item(0) = 1 'amb state is dispatched
                        new_int_state(dispatch_action(i).Item(0) + 1).Item(2) = int_state(dispatch_action(i).Item(1) + (dynamic_ambulance_number + 1)).Item(1) 'amb destination is call location
                        new_int_state(reallocate_action(k).Item(1) + 1).Item(0) = 5 'amb state is going to base
                        new_int_state(reallocate_action(k).Item(1) + 1).Item(2) = int_state(dispatch_action(i).Item(0) + 1).Item(1) 'amb destination is base
                        new_int_state(dispatch_action(i).Item(1) + (dynamic_ambulance_number + 1)).Item(1) = 1 'call state is assigned
                        'action_state_development(new_int_state, current_time)
                        one_step_simulation(new_int_state, current_time)
                        im_cost = immediate_cost(new_int_state, "A1", dispatch_action(i).Item(0) + 1, dispatch_action(i).Item(1) + 1, 10, current_time)
                        to_cost.Add(im_cost + e_cost)
                        policy_evaluation.Add(("da," + (dispatch_action(i).Item(0)).ToString() + "," + (dispatch_action(i).Item(1)).ToString() + "," + reallocate_action(k).Item(1).ToString() + "," + int_state(dispatch_action(i).Item(0) + 1).Item(1).ToString()), im_cost + e_cost) 'da is for dispatching and reallocation
                    End If
                Next
            Next
        End If

        ''call appropriate module to create possible future state for redeployment

        If redeploy_action.Count() > 0 Then
            For i As Integer = 0 To redeploy_action.Count() - 1 Step 1
                If redeploy_action(i).Item(0) <> -1 And redeploy_action(i).Count > 2 Then
                    new_int_state = int_state.Select(Function(innerlist) innerlist.ToList).ToList
                    new_int_state(0).Item(0) = wtf_finish 'event
                    new_int_state(redeploy_action(i).Item(0) + 1).Item(0) = 1 'ambulance state is dispatched
                    new_int_state(redeploy_action(i).Item(0) + 1).Item(2) = int_state(redeploy_action(i).Item(1) + (dynamic_ambulance_number + 1)).Item(1) 'ambulance destination is call location
                    new_int_state(redeploy_action(i).Item(1) + (dynamic_ambulance_number + 1)).Item(0) = 1 'call state is assigned
                    new_int_state(redeploy_action(i).Item(2) + 1).Item(0) = 4    'amb state is going to its base
                    'new_int_state(redeploy_action(i).Item(2) + 1).Item(2) = Form4.amb_ini_base_list(redeploy_action(i).Item(2))
                    new_int_state(redeploy_action(i).Item(2) + 1).Item(2) = (redeploy_action(i).Item(3))
                    'action_state_development(new_int_state, current_time)
                    one_step_simulation(new_int_state, current_time)
                    im_cost = 0
                    to_cost.Add(im_cost + e_cost)
                    'policy_evaluation.Add(("rd," + (redeploy_action(i).Item(0)).ToString() + "," + (dispatch_action(i).Item(1)).ToString() + "," + (redeploy_action(i).Item(2)).ToString() + "," + Form4.amb_ini_base_list(redeploy_action(i).Item(2)).ToString()), im_cost + e_cost) 'rd is for redeployment and dispatching
                    policy_evaluation.Add(("rd," + (redeploy_action(i).Item(0)).ToString() + "," + (dispatch_action(i).Item(1)).ToString() + "," + (redeploy_action(i).Item(2)).ToString() + "," + (redeploy_action(i).Item(3)).ToString()), im_cost + e_cost)
                ElseIf redeploy_action(i).Item(0) = -1 Then
                    new_int_state = int_state.Select(Function(innerlist) innerlist.ToList).ToList
                    new_int_state(0).Item(0) = wtf_finish 'event
                    new_int_state(redeploy_action(i).Item(2) + 1).Item(0) = 4    'amb state is going to its base
                    'new_int_state(redeploy_action(i).Item(2) + 1).Item(2) = Form4.amb_ini_base_list(redeploy_action(i).Item(2))
                    new_int_state(redeploy_action(i).Item(2) + 1).Item(2) = (redeploy_action(i).Item(3))
                    'action_state_development(new_int_state, current_time)
                    one_step_simulation(new_int_state, current_time)
                    im_cost = 0
                    to_cost.Add(im_cost + e_cost)
                    'policy_evaluation.Add(("r," + "N" + "," + "N" + "," + (redeploy_action(i).Item(2)).ToString() + "," + Form4.amb_ini_base_list(redeploy_action(i).Item(2)).ToString()), im_cost + e_cost) 'r is for redeployment
                    policy_evaluation.Add(("r," + "N" + "," + "N" + "," + (redeploy_action(i).Item(2)).ToString() + "," + (redeploy_action(i).Item(3)).ToString()), im_cost + e_cost)
                End If
            Next
        End If

        Dim best_action As String
        If policy_evaluation.Count() >= 1 Then
            best_action = (From key In policy_evaluation Select key Where key.Value = policy_evaluation.Values.Min()).First.Key
            If policy.ContainsKey(state) = True Then
                policy(state) = policy_evaluation(best_action)
            Else
                policy.Add(state, policy_evaluation(best_action))
            End If
        Else
            best_action = "None"
        End If
        Return best_action
    End Function
    Public Function v_hat(ByVal state As List(Of List(Of Integer)), ByVal current_time As Double)
        Dim value_function As Double
        Dim phi1 As Double = phi_one_numbers(state)
        Dim phi2 As Double = phi_one_time(state)
        Dim phi3 As Double = phi_three(state)
        Dim phi4 As Double = phi_four(state)
        Dim phi5 As Double = phi_five(state)
        'Dim phi5 As Double = phi_five_future(state, current_time)
        Dim phi6 As Double = phi_six(state)
        'Dim phi6 As Double = phi_six_future(state, current_time)
        'Dim phi7 As Double = phi_four_topaloglu_way(state)

        value_function = alpha(0)
        value_function += alpha(1) * phi1
        value_function += alpha(2) * phi2
        value_function += alpha(3) * phi3
        value_function += alpha(4) * phi4
        value_function += alpha(5) * phi5
        value_function += alpha(6) * phi6
        'value_function += alpha(7) * phi7

        'If inner_iteration = 0 Then
        'cost_sb.Append(alpha(1).ToString() + "*" + phi1.ToString() + vbTab + alpha(2).ToString() + "*" + phi2.ToString() + vbTab + alpha(3).ToString + "*" + phi3.ToString() + vbTab + alpha(4).ToString() + "*" + phi4.ToString() + vbTab + alpha(5).ToString + "*" + phi5.ToString() + vbTab + alpha(6).ToString() + "*" + phi6.ToString() + vbTab + value_function.ToString() + vbTab)
        'End If
        'value_function += alpha(7) * phi_seven_mmc(state)
        If value_function.ToString() = "-1.#IND" Then
            Dim ali As Integer = 0
            ali = ali + 1
        End If
        Return value_function
    End Function
    Public Function expected_cost(ByVal future_states As List(Of List(Of List(Of Integer))), ByVal current_time As Double, ByVal residual_time As Double)
        Dim exp_cost As New Double
        For t As Integer = 0 To future_states.Count() - 1 Step 1
            'exp_cost = exp_cost + 0.9999999 * probs(t) * v_hat(future_states(t))
            'If index.Contains(t) = True Then
            'exp_cost = exp_cost + probs(t) * v_hat(future_states(t), current_time)
            exp_cost += v_hat(future_states(t), current_time)
            'End If
        Next
        'exp_cost = 0.9999999 * exp_cost
        Return Pow(0.99999, residual_time) * exp_cost / future_states.Count()
    End Function
    Public Function immediate_cost(ByVal state As List(Of List(Of Integer)), ByVal action_type As String, ByVal amb_num As Integer, ByVal call_number As Integer, ByVal base_num As Integer, ByVal current_time As Double)
        Dim h_cost() As Double = {100, 1000, 100, 1000}
        'lamda_of_l_initialization()
        Dim immed_cost As New Double
        'Dim call_pro As Integer = state(i).Item(2)
        If action_type = "A1" Then
            For i As Integer = (dynamic_ambulance_number + 1) To state.Count() - 1 Step 1
                If i = (dynamic_ambulance_number + 1) + call_number - 1 Then
                    Dim call_pro As Integer = state(i).Item(2)
                    If call_pro = 0 Then
                        immed_cost = h_cost(0) * (travel_lambdas(state(amb_num).Item(1)).Item(state(i).Item(1)) + Form4.c_state(call_number - 1).Item(3) - current_time)
                    End If
                    If call_pro = "1" Then
                        immed_cost = h_cost(1) * (travel_lambdas(state(amb_num).Item(1)).Item(state(i).Item(1)) + Form4.c_state(call_number - 1).Item(3) - current_time)
                    End If
                End If
            Next
        ElseIf action_type = "A2" Then
            For i As Integer = (dynamic_ambulance_number + 1) To state.Count() - 1 Step 1
                If i = (dynamic_ambulance_number + 1) + call_number - 1 Then
                    Dim call_pro As Integer = state(i).Item(2)
                    If call_pro = 0 Then
                        immed_cost = h_cost(2) * (travel_lambdas(state(amb_num).Item(1)).Item(state(i).Item(1)) + Form4.c_state(call_number - 1).Item(3) - current_time)
                    End If
                    If call_pro = "1" Then
                        immed_cost = h_cost(3) * (travel_lambdas(state(amb_num).Item(1)).Item(state(i).Item(1)) + Form4.c_state(call_number - 1).Item(3) - current_time)
                    End If
                End If
            Next
        End If
        'If inner_iteration = 1 Then
        '    cost_sb.Append(immed_cost.ToString() + vbTab)
        'End If
        Return immed_cost
    End Function
    Public Function stop_condition(ByVal new_alpha() As Double) As Integer
        phi_path()
        Dim random_sample As New List(Of String)
        Using sr As StreamReader = New StreamReader(filepath, True)
            While sr.Peek <> -1
                random_sample.Add(sr.ReadLine())
            End While
        End Using
        '''''''''''''''''''''''''''''''''''''''''''''' waiting time criteria
        Dim v_bar As Double
        Dim v_fin As Double
        'For q As Integer = 0 To c_of_r.Count() - 1 Step 1
        v_bar = New Double
        For t As Integer = 0 To c_of_r(0).Count() - 1 Step 1
            v_bar = v_bar + c_of_r(0).Item(t) / (total_hp_call_nom_list(0).Item(t) + total_lp_call_nom_list(0).Item(t))
        Next
        'v_fin += v_bar / c_of_r(q).Count()
        'Next

        ''''''''''''''''''''''''''''''''''''''''''''''' average lost calls
        Dim miss_call_bar As Double
        For t As Integer = 0 To number_lost_calls(0).Count() - 1 Step 1
            miss_call_bar = miss_call_bar + number_lost_calls(0).Item(t) / (total_hp_call_nom_list(0).Item(t) + total_lp_call_nom_list(0).Item(t))
        Next
        Dim miss_call_bar_pr As Double
        For t As Integer = 0 To number_lost_calls_pr(0).Count() - 1 Step 1
            miss_call_bar_pr = miss_call_bar_pr + number_lost_calls_pr(0).Item(t) / (total_hp_call_nom_list(0).Item(t) + total_lp_call_nom_list(0).Item(t))
        Next

        ''''''''''''''''''''''''''''''''''''''''''''''''fraction of busy time
        Dim busy_time_bar As Double
        For t As Integer = 0 To busy_time_fraction_list(0).Count() - 1 Step 1
            busy_time_bar = busy_time_bar + busy_time_fraction_list(0).Item(t)
        Next
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''coverage
        Dim output_coverage As Double
        For t As Integer = 0 To final_coverage(0).Count() - 1 Step 1
            output_coverage += final_coverage(0).Item(t)
        Next
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''lost calls waiting time
        Dim lost_waiting As Double
        For t As Integer = 0 To c_of_r_loss(0).Count() - 1 Step 1
            lost_waiting += c_of_r_loss(0).Item(t) / (number_lost_calls(0).Item(t))
        Next
        '''''''''''''''''''''''''''''''''''''''''''''''
        miss_call_bar = miss_call_bar / number_lost_calls(0).Count()
        miss_call_bar_pr = miss_call_bar_pr / number_lost_calls_pr(0).Count()
        value_err = v_bar / c_of_r(0).Count()
        'value_err = v_fin / c_of_r.Count()
        busy_time_bar = busy_time_bar / busy_time_fraction_list(0).Count()
        output_coverage = output_coverage / final_coverage(0).Count()
        lost_waiting = lost_waiting / c_of_r_loss(0).Count()
        '''''''''''''''''''''''''''''''''''''''''''''''''
        best_miss_call.Add(miss_call_bar)
        best_resp_time.Add(value_err)

        Dim sb As StringBuilder = New StringBuilder
        sb.Append(miss_call_bar.ToString())
        Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Results_GD_R_A-Late Calls.txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
        sb = New StringBuilder
        sb.Append(miss_call_bar_pr.ToString())
        Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Results_GD_R_A-Late Calls_pr.txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
        sb = New StringBuilder
        sb.Append(value_err.ToString())
        Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Results_GD_R_A-avg. Response Time.txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
        sb = New StringBuilder
        sb.Append(busy_time_bar.ToString())
        Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Results_GD_R_A-Fraction of Busy Time.txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
        sb = New StringBuilder
        sb.Append(output_coverage.ToString())
        Using writeresult As StreamWriter = New StreamWriter("C:\users\snasrol\desktop\output\Results_GD_R_A-Coverage.txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
        sb = New StringBuilder
        sb.Append(lost_waiting)
        Using writeresult As StreamWriter = New StreamWriter("C:\users\snasrol\desktop\output\Results_GD_R_A-avg. Response time of late calls.txt", True)
            writeresult.WriteLine(sb.ToString())
        End Using
        inner_iteration += 1
        Return inner_iteration
    End Function
    Public Sub regression_solver()
        phi_path()
        Dim random_sample As New List(Of String)
        Using sr As StreamReader = New StreamReader(filepath, True)
            While sr.Peek <> -1
                random_sample.Add(sr.ReadLine())
            End While
        End Using
        Dim i As Integer = c_of_r.Count()
        Dim j As Integer = TextBox2.Text
        Dim y As New Matrix(i * j, 1)
        For t As Integer = 0 To i - 1 Step 1
            For q As Integer = 0 To j - 1 Step 1
                y(t * j + q, 0) = c_of_r(t).Item(q)
                'y(t * j + q, 0) = c_of_r_loss(t).Item(q)
                'y(t * j + q, 0) = number_lost_calls(t).Item(q) + number_lost_calls_pr(t).Item(q)
            Next
        Next
        'changes with the number of Phi functions
        Dim x As New Matrix(i * j, 7)

        For t As Integer = 0 To i - 1 Step 1
            For q As Integer = 0 To j - 1 Step 1

                x(t * j + q, 0) = 1

                Dim state As New List(Of List(Of Integer))
                Dim int_temp_state As New List(Of Integer)
                int_temp_state.Add(Convert.ToInt32(random_sample(t).ElementAt(1).ToString()))
                state.Add(int_temp_state)
                For v As Integer = random_sample(t).IndexOf("[") To random_sample(t).IndexOf("]")
                    int_temp_state = New List(Of Integer)
                    If random_sample(t).ElementAt(v) = "(" Then
                        int_temp_state.Add(Convert.ToInt32(random_sample(t).ElementAt(v + 1).ToString()))
                        int_temp_state.Add(Convert.ToInt32(random_sample(t).Substring(random_sample(t).IndexOf(",", v) + 2, random_sample(t).IndexOf(",", random_sample(t).IndexOf(",", v) + 1) - random_sample(t).IndexOf(",", v) - 2)))
                        int_temp_state.Add(Convert.ToInt32(random_sample(t).Substring(random_sample(t).IndexOf(",", random_sample(t).IndexOf(" ", v)) + 2, random_sample(t).IndexOf(")", v) - random_sample(t).IndexOf(",", random_sample(t).IndexOf(" ", v)) - 2)))
                        state.Add(int_temp_state)
                    End If
                Next
                For v As Integer = random_sample(t).LastIndexOf("[") To random_sample(t).LastIndexOf("]")
                    int_temp_state = New List(Of Integer)
                    If random_sample(t).ElementAt(v) = "(" Then
                        int_temp_state.Add(Convert.ToInt32(random_sample(t).ElementAt(v + 1).ToString()))
                        int_temp_state.Add(Convert.ToInt32(random_sample(t).Substring(random_sample(t).IndexOf(",", v) + 2, random_sample(t).IndexOf(",", random_sample(t).IndexOf(",", v) + 1) - random_sample(t).IndexOf(",", v) - 2)))
                        int_temp_state.Add(Convert.ToInt32(random_sample(t).Substring(random_sample(t).IndexOf(",", random_sample(t).IndexOf(" ", v)) + 2, random_sample(t).IndexOf(")", v) - random_sample(t).IndexOf(",", random_sample(t).IndexOf(" ", v)) - 2)))
                        state.Add(int_temp_state)
                    End If
                Next

                x(t * j + q, 1) = phi_one_numbers(state)
                x(t * j + q, 2) = phi_one_time(state)
                x(t * j + q, 3) = phi_three(state)
                x(t * j + q, 4) = phi_four(state)
                x(t * j + q, 5) = phi_five(state)
                'x(t * j + q, 5) = phi_five_future(state, 0)
                x(t * j + q, 6) = phi_six(state)
                'x(t * j + q, 6) = phi_six_future(state, 0)
                'x(t * j + q, 7) = phi_four_topaloglu_way(state)
            Next
        Next

        Dim new_alpha As New Matrix(7, 1)
        Dim x_t As New Matrix(7, i * j)
        x_t = x.Transpose()
        Dim x_t_x As New Matrix(7, 7)
        x_t_x = x_t.Multiply(x)
        Dim x_inv As New Matrix(7, 7)
        Dim x_svd As New SingularValueDecomposition
        Dim x_s As New Matrix(7, 7)
        Dim x_u As New Matrix(7, 7)
        Dim x_v As New Matrix(7, 7)
        x_svd.ComputeSVD(x_t_x, x_s, x_u, x_v)
        x_v = x_v.Transpose()
        x_u = x_u.Transpose()
        For ind As Integer = 0 To 6 Step 1
            If x_s(ind, ind) > 0.0000001 Then
                x_s(ind, ind) = 1 / x_s(ind, ind)
            Else
                x_s(ind, ind) = 0
            End If
        Next
        x_inv = x_v.Multiply(x_s.Multiply(x_u))
        'x_inv = x_t_x.Inverse()
        Dim x_inv_x_t As New Matrix(7, i * j)
        x_inv_x_t = x_inv.Multiply(x_t)
        new_alpha = x_inv_x_t.Multiply(y)
        For t As Integer = 0 To 6
            alpha(t) = new_alpha(t, 0)
        Next

        'Dim alphp_file As StreamWriter = New StreamWriter("C:\users\snasrol\desktop\alpha.txt", True)
        'Dim sb As New StringBuilder
        'For t As Integer = 0 To 6
        'sb.Append(new_alpha(t, 0).ToString() + vbTab)
        'Next
        'Using alphp_file
        'alphp_file.WriteLine(sb)
        'End Using
    End Sub
    Public Sub lost_calls(ByVal reps As Integer, ByVal clock As Double, ByVal creation_time As Double, ByVal priority As Integer)
        Dim discount_factor As Double = 0.99999
        If last_rep = reps Then
            temp_time = temp_time + (clock - creation_time) * priority_weight(priority) * Pow(discount_factor, clock)
            'response_time = clock - creation_time
            If clock - creation_time > 8 Then
                temp_loss_time = temp_loss_time + (clock - creation_time) * priority_weight(priority) * Pow(discount_factor, clock)
                If priority = 1 Then
                    miss_call_num_hp += 1 * priority_weight(priority) * Pow(discount_factor, clock)
                Else
                    miss_call_num_lp += 1 * priority_weight(priority) * Pow(discount_factor, clock)
                End If
            End If
        Else
            temp_time = (clock - creation_time) * priority_weight(priority) * Pow(discount_factor, clock)
            'response_time = clock - creation_time
            last_rep = reps
            If clock - creation_time > 8 Then
                If priority = 1 Then
                    miss_call_num_hp = 1 * priority_weight(priority) * Pow(discount_factor, clock)
                Else
                    miss_call_num_lp = 1 * priority_weight(priority) * Pow(discount_factor, clock)
                End If
                temp_loss_time = (clock - creation_time) * priority_weight(priority) * Pow(discount_factor, clock)
            Else
                If priority = 1 Then
                    miss_call_num_hp = 0
                Else
                    miss_call_num_lp = 0
                End If
                temp_loss_time = 0
            End If
        End If

        'Dim sb As StringBuilder = New StringBuilder
        'sb.Append(response_time.ToString())
        'Using writeresult As StreamWriter = New StreamWriter("C:\Users\snasrol\Desktop\output\Pi_Zero-response times+" + reps.ToString() + ".txt", True)
        'writeresult.WriteLine(sb.ToString())
        ' End Using
    End Sub
    Public Sub calculate_coverage()
        Dim covered_nodes As New List(Of Integer)
        time_interval = clock - last_clock
        last_clock = clock
        For i As Integer = 0 To 167 Step 1
            For j As Integer = 0 To M_state.Count() - 1 Step 1
                If M_state(j).Item(0) = 0 Then 'amb is available
                    If travel_lambdas(M_state(j).Item(1)).Item(i) <= 8 Then 'amb is covering the node
                        If covered_nodes.Count() > 0 Then
                            If covered_nodes.Contains(i) = False Then
                                covered_nodes.Add(i)
                            End If
                        Else
                            covered_nodes.Add(i)
                        End If
                    End If
                End If
            Next
        Next
        coverage = coverage + covered_nodes.Count() * time_interval
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        input_read()
        Dim min_delta As New List(Of Double)
        For i As Integer = 0 To travel_lambdas.Count() - 1 Step 1 'counting the region for min_delta
            For j As Integer = travel_lambdas.Count() - 1 To 0 Step -1 'counting the region for min delta
                If min_delta.Count() > 0 And (min_delta.Contains(travel_lambdas(i).Item(j)) Or min_delta.Contains(travel_lambdas(j).Item(i))) Then 'min_delta sholud be new
                    Continue For
                Else
                    min_delta.Add(travel_lambdas(i).Item(j))
                End If
            Next
        Next

        Dim lambda As Double
        For i As Integer = 0 To node_lambdas.Count() - 1 Step 1
            If node_lambdas(i) <> 0 Then
                lambda += 1 / node_lambdas(i)
            End If
        Next

        Dim integ As List(Of Double)
        For t As Integer = 0 To min_delta.Count() - 1 Step 1
            integ = New List(Of Double)
            'when exp is minimum and we have normals and deltas
            integ.Add(min_delta(t))
            integ.Add(MathNet.Numerics.Integration.Integrate.OnClosedInterval(Function(x As Double) lambda * Math.Exp(-lambda * x) * (1 / 2) * MathNet.Numerics.SpecialFunctions.Erfc((x - (prob_scene_hospital * 56.7 + (1 - prob_scene_hospital) * 54.18)) / ((prob_scene_hospital * 13.6 + (1 - prob_scene_hospital) * 15.8) * Math.Sqrt(2))), 0, min_delta(t)))
            'when norm is minimum and we have exp and deltas
            integ.Add(MathNet.Numerics.Integration.Integrate.OnClosedInterval(Function(x As Double) (1 / ((prob_scene_hospital * 13.6 + (1 - prob_scene_hospital) * 15.8) * Math.Sqrt(2 * PI))) * Math.Exp(-Pow((x - (prob_scene_hospital * 56.7 + (1 - prob_scene_hospital) * 54.18)) / ((prob_scene_hospital * 13.6 + (1 - prob_scene_hospital) * 15.8) * Math.Sqrt(2)), 2)) * Math.Exp(-lambda * x), -100, min_delta(t)))
            'when deltas are minimum and we have exp and normals
            integ.Add(Math.Exp(-lambda * min_delta(t)) * (1 / 2) * MathNet.Numerics.SpecialFunctions.Erfc((min_delta(t) - (prob_scene_hospital * 56.7 + (1 - prob_scene_hospital) * 54.18)) / ((prob_scene_hospital * 13.6 + (1 - prob_scene_hospital) * 15.8) * Math.Sqrt(2))))
            integral_list.Add(integ)
        Next

        integ = New List(Of Double)
        'when exp in minimum and we have norm but with no delta
        integ.Add(Double.PositiveInfinity)
        integ.Add(MathNet.Numerics.Integration.Integrate.OnClosedInterval(Function(x As Double) lambda * Math.Exp(-lambda * x) * (1 / 2) * MathNet.Numerics.SpecialFunctions.Erfc((x - (prob_scene_hospital * 56.7 + (1 - prob_scene_hospital) * 54.18)) / ((prob_scene_hospital * 13.6 + (1 - prob_scene_hospital) * 15.8) * Math.Sqrt(2))), 0, 100))
        'when norm is minimum and we have exp but with no delta
        integ.Add(MathNet.Numerics.Integration.Integrate.OnClosedInterval(Function(x As Double) (1 / ((prob_scene_hospital * 13.6 + (1 - prob_scene_hospital) * 15.8) * Math.Sqrt(2 * PI))) * Math.Exp(-Pow((x - (prob_scene_hospital * 56.7 + (1 - prob_scene_hospital) * 54.18)) / ((prob_scene_hospital * 13.6 + (1 - prob_scene_hospital) * 15.8) * Math.Sqrt(2)), 2)) * Math.Exp(-lambda * x), -100, 100))
        integral_list.Add(integ)
    End Sub
    Public Function priority_weight(ByVal priority As Integer) As Double
        If priority = 1 Then
            Return hp_weight
        Else
            Return Lp_weight
        End If
    End Function
End Class
