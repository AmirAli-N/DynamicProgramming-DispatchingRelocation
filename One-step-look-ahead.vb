Imports System.Math
Imports System.IO
Imports System.Text
Imports MathNet.Numerics.Distributions
Public Class Form4
    Dim filepath As String
    Public M_state As New List(Of List(Of Integer))
    Dim last_origin() As Integer
    'Dim amb_origin_base(3) As Integer
    Public c_state As New List(Of List(Of Double))
    Dim last_num_of_call As Integer
    Dim fel As New List(Of List(Of Double))
    Dim clock As Double
    'Dim lambda_of_l(5, 5) As Double
    Public amb_list() As Double
    Public seed As Integer
    Public weed As Integer
    Public leed As Integer
    Public reed As Integer
    Public zeed As Integer
    Public r1 As New Random(seed)
    Public r2 As New Random(weed)
    Public r3 As New Random(leed)
    Public r4 As New Random(reed)
    Public r5 As New Random(zeed)
    Dim new_call_num As Integer
    Dim current_event As Integer
    Dim arrival_clock As Double
    Dim locations As Integer
    Dim priority As Integer
    Dim prob_priority As Double = 0.86
    Dim prob_scene_hospital As Double = 0.73
    Dim lambda_finish_at_scene As Double = 1 / 17.1
    Dim lambda_service_at_hospital As Double = 1 / 56.66
    Dim next_action As List(Of Double)
    Dim i As Double
    Dim row As Integer
    Dim closer_amb_distance As New List(Of List(Of Double))
    Dim begining_state As String
    Dim reps As Integer
    Dim last_replication As Integer
    Public waiting_time As List(Of Double)
    Public waiting_loss_time As List(Of Double)
    Dim temp_time As Double
    Dim temp_loss_time As Double
    Dim response_time As Double
    Public response_time_indicator As Integer
    Public arrival_change As Double
    Dim b_loop As Boolean
    Public miss_call_nom As Integer
    Public miss_call_num_pr As Integer
    Public number_of_missed_calls As New List(Of Double)
    Public number_of_missed_calls_pr As New List(Of Double)
    Public num_of_total_calls As Integer
    Public total_hp_call_nom_list As New List(Of Double)
    Public total_lp_call_nom_list As New List(Of Double)
    Dim busy_time_start As Double
    Dim busy_time_end As Double
    Dim busy_time As Double
    Public busy_time_fraction As New List(Of Double)
    Dim node_lambdas As New List(Of Double)
    Dim travel_lambdas As New List(Of List(Of Double))
    Dim node_distances As New List(Of List(Of Double))
    Dim ambulance_initial_location As New List(Of List(Of Integer))
    Public amb_ini_base_list As New List(Of Integer)
    Dim int_state As List(Of List(Of Integer))
    Dim coverage As Double
    Dim time_interval As Double
    Public state_coverage As New List(Of Double)
    Dim time_passed As New Double
    Dim last_clock As Double
    Dim arrival_list As New List(Of List(Of List(Of Double)))
    Dim miss_call_num_hp As Double
    Dim miss_call_num_lp As Double
    Dim hp_weight As Double = 1
    Dim lp_weight As Double = 0.1
    Dim total_hp_call_nom As Double
    Dim total_lp_call_nom As Double
    Dim dynamic_ambulance_number As Integer = 17

    Public Sub input_read()
        ''''''''''''''''''''''''''''''''''''''''reading call arrival lambdas'''''''''''''''''''''''''''''''''''
        Dim sr As StreamReader = New StreamReader("c:\users\snasrol\desktop\input\Node Lambdas for Calls.txt", True)
        Dim enumerator As Integer = 1
        'node_lambdas.Add(0)
        While sr.Peek() <> -1
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
            enumerator += 1
        End While

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
        For i As Integer = 0 To ambulance_initial_location.Count() - 1 Step 1
            For j As Integer = 0 To ambulance_initial_location(i).Item(1) Step 1
                amb_ini_base_list.Add(ambulance_initial_location(i).Item(0))
            Next
        Next
    End Sub
    Public Sub closer_amb_initialization()
        For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
            Dim a As New List(Of Double)
            a.Add(0)
            a.Add(0)
            closer_amb_distance.Add(a)
        Next
    End Sub
    Private Sub amb_iteration_initialization(ByVal state As String)
        Dim amb_state As New List(Of Integer)
        Dim amb_number As New Integer
        For t As Integer = 0 To state.IndexOf("]") Step 1 'ambulance state area
            If state.ElementAt(t) = "(" Then
                amb_state = New List(Of Integer)
                M_state.Add(amb_state)
                amb_number += 1
                Dim amb_string As String
                amb_string = ""
                For q As Integer = t + 1 To state.IndexOf("]") Step 1
                    If state.ElementAt(q) <> ")" Then
                        amb_string = amb_string + state.ElementAt(q)
                    Else
                        Exit For
                    End If
                Next
                M_state(amb_number - 1).Add(Convert.ToInt32(amb_string.Substring(0, amb_string.IndexOf(","))))
                M_state(amb_number - 1).Add(Convert.ToInt32(amb_string.Substring(amb_string.IndexOf(",") + 2, amb_string.LastIndexOf(",") - amb_string.IndexOf(",") - 2)))
                M_state(amb_number - 1).Add(Convert.ToInt32(amb_string.Substring(amb_string.LastIndexOf(",") + 2, amb_string.Count() - amb_string.LastIndexOf(",") - 2)))
                M_state(amb_number - 1).Add(0)
            End If
        Next
        amb_list = New Double(amb_number - 1) {}
        last_origin = New Integer(amb_number - 1) {}
        For t As Integer = 0 To amb_number - 1
            last_origin(t) = M_state(t).Item(1)
        Next
    End Sub
    Private Sub call_iteration_initialization(ByVal state As String)
        If c_state.Count() > 0 Then
            c_state.RemoveRange(0, c_state.Count() - 1)
        End If
        For t As Integer = state.IndexOf("]") + 1 To state.Count() - 1 Step 1
            Dim call_state As New List(Of Double)
            If state.ElementAt(t) = "]" Then
                Exit For
            Else
                If state.ElementAt(t) = "(" Then
                    Dim call_string As String
                    call_string = ""
                    For q As Integer = t + 1 To state.LastIndexOf("]") Step 1
                        If state.ElementAt(q) <> ")" Then
                            call_string = call_string + state.ElementAt(q)
                        Else
                            Exit For
                        End If
                    Next
                    call_state.Add(Convert.ToDouble(call_string.Substring(0, call_string.IndexOf(","))))
                    call_state.Add(Convert.ToDouble(call_string.Substring(call_string.IndexOf(",") + 2, call_string.LastIndexOf(",") - call_string.IndexOf(",") - 2)))
                    call_state.Add(Convert.ToDouble(call_string.Substring(call_string.LastIndexOf(",") + 2, call_string.Count() - call_string.LastIndexOf(",") - 2)))
                    call_state.Add(0)
                    c_state.Add(call_state)
                End If
            End If
        Next
        num_of_total_calls = c_state.Count()
        new_call_num = num_of_total_calls
    End Sub
    Private Sub amb_state_update(ByVal number_of_amb As Integer, ByVal destination As Integer, ByVal st_time As Double)
        If destination = 169 Then '''''''''''''''''''''''''''''''''''''''''''''ambulance(num_of_amb) is going to hospital
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
        If destination = 168 Then ''''''''''''''''''''''''''''''''''''''''''''ambulance(num_of_amb) is at the scene
            M_state(number_of_amb).Item(0) = 2
            M_state(number_of_amb).Item(1) = last_origin(number_of_amb)
            M_state(number_of_amb).Item(2) = last_origin(number_of_amb)
        End If
        If destination <= 167 Then '''''''''''''''''''''''''''''''''''''''''''ambulance(num_of_amb) dispatched
            M_state(number_of_amb).Item(0) = 1
            M_state(number_of_amb).Item(1) = last_origin(number_of_amb)
            M_state(number_of_amb).Item(2) = destination
        End If
        If destination >= 200 And destination < 400 Then ''''''''''''''''''''''''''''''''''''''''''''ambulance (number_of_amb) going to base
            M_state(number_of_amb).Item(0) = 4
            M_state(number_of_amb).Item(1) = last_origin(number_of_amb)
            M_state(number_of_amb).Item(2) = destination - 200
        End If
        If destination >= 800 Then
            M_state(number_of_amb).Item(0) = 0  '''''''''''''''''''''''''''''ambulance (number_of_amb) is at the base
            M_state(number_of_amb).Item(1) = destination - 800
            M_state(number_of_amb).Item(2) = 0
        End If
        If destination >= 400 And destination < 800 Then '''''''''''''''''''''''''ambulance is reallocating
            M_state(number_of_amb).Item(0) = 4
            M_state(number_of_amb).Item(1) = last_origin(number_of_amb)
            M_state(number_of_amb).Item(2) = destination - 400
        End If
        M_state(number_of_amb).Item(3) = clock
    End Sub
    Private Sub call_state_update(ByVal status As Integer, ByVal number_of_call As Integer, ByVal location_of_call As Integer, ByVal priority_of_call As Integer, ByVal time_of_creation As Double)
        Dim call_state As New List(Of Double)
        If number_of_call < c_state.Count() Then
            c_state(number_of_call).Item(0) = status
            c_state(number_of_call).Item(1) = location_of_call
            c_state(number_of_call).Item(2) = priority_of_call
        Else
            If status = 0 Then
                call_state.Add(status)
                call_state.Add(location_of_call)
                call_state.Add(priority_of_call)
                call_state.Add(time_of_creation)
                c_state.Add(call_state)
            End If
            last_num_of_call += 1
        End If
        'last_num_of_call = number_of_call
    End Sub
    Public Sub state_FEL_building(ByVal state As String)

        ''print replication propertise in datagridview
        'form3.DataGridView1.Rows.Add(1)
        'form3.DataGridView1.Rows(row).DefaultCellStyle.BackColor = Color.Gray
        'form3.DataGridView1.Item(2, row).Value = "State: " + state
        'row = row + 1

        ''iteration initialization
        amb_iteration_initialization(state)
        call_iteration_initialization(state)

        Dim best_action As String
        Dim wtf_finish As Integer = Convert.ToInt32(state.ElementAt(1).ToString())
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        If state.ElementAt(1) = "0" Or state.ElementAt(1) = "3" Or state.ElementAt(1) = "4" Or state.ElementAt(1) = "5" Or state.ElementAt(1) = "6" Then 'if there's need for an action in case of call arrival=0, amb finish at scene=3 and amb finish at hospital=4
            If state.ElementAt(1) = "0" Then 'if an action for dispatching is needed
                best_action = Form1.fixed_action(state, 0, 0, clock)
            End If
            If state.ElementAt(1) = "3" Or state.ElementAt(1) = "4" Then 'if an action after a finished amb is neede
                best_action = Form1.fixed_action(state, 1, wtf_finish, clock)
            End If
            If state.ElementAt(1) = "5" Or state.ElementAt(1) = "6" Then
                best_action = Form1.fixed_action(state, 1, wtf_finish, clock)
            End If
        Else
            FEL_Builder_iteration(1, vbNull, vbNull, vbNull)
            Dim loc_amb As New Integer
            Dim des_amb As New Integer
            Dim number_amb As New Integer
            For l As Integer = state.IndexOf("[") To state.IndexOf("]") Step 1
                If state.ElementAt(l) = "(" Then
                    number_amb += 1
                    If state.ElementAt(l + 1) = "1" Then ''if that amb is dispatched
                        loc_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        des_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(2, number_amb - 1, loc_amb, des_amb)
                    End If
                    If state.ElementAt(l + 1) = "2" Then ''if amb is at the scene
                        FEL_Builder_iteration(3, number_amb - 1, vbNull, vbNull)
                        Dim count As Integer = c_state.Count() - 1
                        For t As Integer = 0 To count Step 1
                            If c_state(t).Item(0) = 1 Then
                                If c_state(t).Item(1) = loc_amb Then
                                    c_state.RemoveAt(t)
                                    Exit For
                                End If
                            End If
                        Next
                    End If
                    If state.ElementAt(l + 1) = "3" Then ''if amb is at hospital
                        loc_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        des_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(4, number_amb - 1, loc_amb, vbNull)
                    End If
                    If state.ElementAt(l + 1) = "4" Then ''if amb is going to base
                        loc_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        des_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(5, number_amb - 1, loc_amb, des_amb)
                    End If
                    If state.ElementAt(l + 1) = "5" Then ''if amb is reallocating
                        FEL_Builder_iteration(6, number_amb - 1, loc_amb, des_amb)
                    End If
                End If
            Next
            Exit Sub
        End If
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        If best_action = "None" Then
            FEL_Builder_iteration(1, vbNull, vbNull, vbNull)
            Dim loc_amb As New Integer
            Dim des_amb As New Integer
            Dim number_amb As New Integer
            'Dim number_call As New Integer
            For l As Integer = state.IndexOf("[") To state.IndexOf("]") Step 1
                If state.ElementAt(l) = "(" Then
                    number_amb += 1
                    If state.ElementAt(l + 1) = "1" Then ''if that amb is dispatched
                        loc_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        des_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(2, number_amb - 1, loc_amb, des_amb)
                    End If
                    If state.ElementAt(l + 1) = "2" Then ''if amb is at the scene
                        FEL_Builder_iteration(3, number_amb - 1, vbNull, vbNull)
                        Dim count As Integer = c_state.Count() - 1
                        For t As Integer = 0 To count Step 1
                            If c_state(t).Item(0) = 1 Then
                                If c_state(t).Item(1) = loc_amb Then
                                    c_state.RemoveAt(t)
                                    Exit For
                                End If
                            End If
                        Next
                    End If
                    If state.ElementAt(l + 1) = "3" Then ''if amb is at hospital
                        loc_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        des_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(4, number_amb - 1, loc_amb, vbNull)
                    End If
                    If state.ElementAt(l + 1) = "4" Then ''if amb is going to base
                        loc_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        des_amb = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(5, number_amb - 1, loc_amb, des_amb)
                    End If
                    If state.ElementAt(l + 1) = "5" Then ''if amb is reallocating
                        FEL_Builder_iteration(6, number_amb - 1, loc_amb, des_amb)
                    End If
                End If
            Next
            Exit Sub
        End If
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        If best_action.Substring(0, best_action.IndexOf(",")) = "r" Then 'if the action is only redeployment
            best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "r" from best action string
            Dim amb_num As New Integer
            Dim base_num As New Integer
            Dim achar As String
            Dim number_of_comma As Integer = 0
            For t As Integer = 0 To best_action.Count() - 1
                If best_action.ElementAt(t) = "," Then 'counts number of commas
                    number_of_comma += 1
                    If number_of_comma = 2 Then 'marks the area of second comma in best_action
                        For q As Integer = t + 1 To best_action.Count() - 1
                            If best_action.ElementAt(q) = "," Then 'marks the area of third comma in best_action
                                achar = best_action.Substring(t, q - t)
                                Exit For
                            End If
                        Next
                    End If
                    Exit For
                End If
            Next
            amb_num = Convert.ToInt32(achar)
            Dim schar As String = best_action.Substring(best_action.LastIndexOf(",") + 1) 'marks the start of the third comma in best_action
            base_num = Convert.ToInt32(schar)
            Dim amb_number As New Integer
            Dim amb_loc As Integer
            For t As Integer = state.IndexOf("[") To state.IndexOf("]") Step 1
                If state.ElementAt(t) = "(" Then
                    amb_number += 1
                    If amb_number - 1 = amb_num Then
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                    End If
                End If
            Next

            FEL_Builder_iteration(6, amb_num, amb_loc, base_num) '' FEL building for the best action
            amb_state_update(amb_num, base_num + 200, 0) ''update amb status regarding the best action
            ''other FEL actions
            amb_number = New Integer
            amb_loc = New Integer
            Dim amb_des As New Integer
            FEL_Builder_iteration(1, vbNull, vbNull, vbNull)
            For l As Integer = 0 To state.IndexOf("]") Step 1 'ambuance state area
                If state.ElementAt(l) = "(" Then
                    amb_number += 1
                    If state.ElementAt(l + 1) = "1" Then ''if that amb is dispatched
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(2, amb_number - 1, amb_loc, amb_des)
                    End If
                    If state.ElementAt(l + 1) = "2" Then ''if amb is at the scene
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        FEL_Builder_iteration(3, amb_number - 1, vbNull, vbNull)
                        Dim count As Integer = c_state.Count() - 1
                        For t As Integer = 0 To count Step 1
                            If c_state(t).Item(0) = 1 Then
                                If c_state(t).Item(1) = amb_loc Then
                                    c_state.RemoveAt(t)
                                    Exit For
                                End If
                            End If
                        Next
                    End If
                    If state.ElementAt(l + 1) = "3" Then ''if amb is at hospital
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        FEL_Builder_iteration(4, amb_number - 1, amb_loc, vbNull)
                    End If
                    If state.ElementAt(l + 1) = "4" And amb_number - 1 <> amb_num Then ''if amb is going to base and is different from the amb of best action
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(5, amb_number - 1, amb_loc, amb_des)
                    End If
                    If state.ElementAt(l + 1) = "5" Then ''if amb is reallocating
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(6, amb_number - 1, amb_loc, amb_des)
                    End If
                End If
            Next
            Exit Sub
        End If
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        If best_action.Substring(0, best_action.IndexOf(",")) = "rd" Then 'if the action is redeployment and dispatching
            best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "rd" from best action string
            Dim amb_num As New Integer
            Dim call_num As New Integer
            Dim amb_num_2 As New Integer
            Dim base_num As New Integer
            Dim schar As String = best_action.Substring(0, best_action.IndexOf(",")) 'marks the area to the first comma
            amb_num = Convert.ToInt32(schar)
            Dim achar As String = best_action.Substring(best_action.LastIndexOf(",") + 1) 'marks the area from the third comma
            base_num = Convert.ToInt32(achar)
            Dim bchar As String
            Dim dchar As String
            Dim number_of_comma As Integer = 0
            For t As Integer = 0 To best_action.Count() - 1 Step 1
                If best_action.ElementAt(t) = "," Then
                    number_of_comma += 1
                End If
                If number_of_comma = 1 Then 'marks the area starting from the first comma
                    For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                        If best_action.ElementAt(q) = "," Then
                            bchar = best_action.Substring(t, q - t)
                            dchar = best_action.Substring(q + 1, best_action.LastIndexOf(",") - q - 1)
                            Exit For
                        End If
                    Next
                End If
            Next
            call_num = Convert.ToInt32(bchar)
            amb_num_2 = Convert.ToInt32(dchar)
            Dim amb_number As New Integer
            Dim call_number As New Integer
            Dim amb_loc As New Integer
            Dim amb_des As New Integer
            Dim call_loc As New Integer
            Dim call_pro As New Integer
            Dim amb_loc_2 As New Integer
            For t As Integer = state.IndexOf("[") To state.IndexOf("]") Step 1
                If state.ElementAt(t) = "(" Then
                    amb_number += 1
                    If amb_number - 1 = amb_num Then
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                        For q As Integer = state.LastIndexOf("[") To state.LastIndexOf("]") Step 1
                            If state.ElementAt(q) = "(" Then
                                call_number += 1
                                If call_number - 1 = call_num Then
                                    call_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", q) + 2, state.IndexOf(",", state.IndexOf(",", q) + 1) - state.IndexOf(",", q) - 2))
                                    call_pro = Convert.ToInt32(state.Substring(state.IndexOf(")", t) - 1, 1))
                                End If
                            End If
                        Next
                    End If
                    If amb_number - 1 = amb_num_2 Then
                        amb_loc_2 = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                    End If
                End If
            Next
            ''FEL best action development
            amb_number = New Integer
            FEL_Builder_iteration(2, amb_num, amb_loc, call_loc)
            FEL_Builder_iteration(6, amb_num_2, amb_loc_2, base_num)
            FEL_Builder_iteration(1, vbNull, vbNull, vbNull)
            amb_state_update(amb_num, call_loc, 0)
            amb_state_update(amb_num_2, base_num + 200, 0)
            call_state_update(1, call_num, call_loc, call_pro, 0)
            ''FEL other developments
            For l As Integer = 0 To state.IndexOf("]") Step 1 'ambuance state area
                If state.ElementAt(l) = "(" Then
                    amb_number += 1
                    If state.ElementAt(l + 1) = "1" Then ''if that amb is dispatched
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(2, amb_number - 1, amb_loc, amb_des)
                    End If
                    If state.ElementAt(l + 1) = "2" Then ''if amb is at the scene
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        FEL_Builder_iteration(3, amb_number - 1, vbNull, vbNull)
                        Dim count As Integer = c_state.Count() - 1
                        For t As Integer = 0 To count Step 1
                            If c_state(t).Item(0) = 1 Then
                                If c_state(t).Item(1) = amb_loc Then
                                    c_state.RemoveAt(t)
                                    Exit For
                                End If
                            End If
                        Next
                    End If
                    If state.ElementAt(l + 1) = "3" Then ''if amb is at hospital
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        FEL_Builder_iteration(4, amb_number - 1, amb_loc, vbNull)
                    End If
                    If state.ElementAt(l + 1) = "4" And amb_number - 1 <> amb_num Then ''if amb is going to base and is different from the amb of best action
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(5, amb_number - 1, amb_loc, amb_des)
                    End If
                    If state.ElementAt(l + 1) = "5" Then ''if amb is reallocating
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(6, amb_number - 1, amb_loc, amb_des)
                    End If
                End If
            Next
            Exit Sub
        End If
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        If best_action.Substring(0, best_action.IndexOf(",")) = "d" Then 'if best action is just dispatching
            best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "d" from best action string
            Dim amb_num As New Integer
            Dim call_num As New Integer
            Dim schar As String = best_action.Substring(0, best_action.IndexOf(",")) 'marks the area to the first comma in best_action
            amb_num = Convert.ToInt32(schar)
            Dim achar As String
            Dim number_of_comma As Integer = 0
            For t As Integer = 0 To best_action.Count() - 1 Step 1
                If best_action.ElementAt(t) = "," Then 'marks the area of the first comma
                    number_of_comma += 1
                End If
                If number_of_comma = 1 Then
                    For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                        If best_action.ElementAt(q) = "," Then 'marks the area of the second comma
                            achar = best_action.Substring(t, q - t)
                            Exit For
                        End If
                    Next
                End If
            Next
            call_num = Convert.ToInt32(achar)
            ''Fel building for the best action
            Dim amb_number As New Integer
            Dim call_number As New Integer
            Dim amb_loc As New Integer
            Dim amb_des As New Integer
            Dim call_loc As New Integer
            Dim call_pro As New Integer
            For t As Integer = state.IndexOf("[") To state.IndexOf("]") Step 1
                If state.ElementAt(t) = "(" Then
                    amb_number += 1
                    If amb_number - 1 = amb_num Then
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                        For q As Integer = state.LastIndexOf("[") To state.LastIndexOf("]") Step 1
                            If state.ElementAt(q) = "(" Then
                                call_number += 1
                                If call_number - 1 = call_num Then
                                    call_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", q) + 2, state.IndexOf(",", state.IndexOf(",", q) + 1) - state.IndexOf(",", q) - 2))
                                    call_pro = Convert.ToInt32(state.Substring(state.IndexOf(")", t) - 1, 1))
                                End If
                            End If
                        Next
                    End If
                End If
            Next
            FEL_Builder_iteration(2, amb_num, amb_loc, call_loc)
            amb_state_update(amb_num, call_loc, 0)
            call_state_update(1, call_num, call_loc, call_pro, 0)
            ''other FEL action
            FEL_Builder_iteration(1, vbNull, vbNull, vbNull)
            amb_number = New Integer
            For l As Integer = 0 To state.IndexOf("]") Step 1 'ambuance state area
                If state.ElementAt(l) = "(" Then
                    amb_number += 1
                    If state.ElementAt(l + 1) = "1" Then ''if that amb is dispatched
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(2, amb_number - 1, amb_loc, amb_des)
                    End If
                    If state.ElementAt(l + 1) = "2" Then ''if amb is at the scene
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        FEL_Builder_iteration(3, amb_number - 1, vbNull, vbNull)
                        For t As Integer = 0 To -1 + c_state.Count()
                            If c_state(t).Item(0) = 1 Then
                                If c_state(t).Item(1) = amb_loc Then
                                    c_state.RemoveAt(t)
                                    Exit For
                                End If
                            End If
                            If t = c_state.Count() - 1 Then
                                Exit For
                            End If
                        Next
                    End If
                    If state.ElementAt(l + 1) = "3" Then ''if amb is at hospital
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        FEL_Builder_iteration(4, amb_number - 1, amb_loc, vbNull)
                    End If
                    If state.ElementAt(l + 1) = "4" And amb_number - 1 <> amb_num Then ''if amb is going to base and is different from the amb of best action
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(5, amb_number - 1, amb_loc, amb_des)
                    End If
                    If state.ElementAt(l + 1) = "5" Then ''if amb is reallocating
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(6, amb_number - 1, amb_loc, amb_des)
                    End If
                End If
            Next
            Exit Sub
        End If
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        If best_action.Substring(0, best_action.IndexOf(",")) = "da" Then 'if best action is dispatching and reallocations
            best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "da" from best action string
            Dim amb_num As New Integer
            Dim call_num As New Integer
            Dim amb_num_2 As New Integer
            Dim base_num As New Integer
            Dim schar As String = best_action.Substring(0, best_action.IndexOf(",")) 'marks the area to the first comma
            amb_num = Convert.ToInt32(schar)
            Dim achar As String = best_action.Substring(best_action.LastIndexOf(",") + 1) 'marks the area from the third comma
            base_num = Convert.ToInt32(achar)
            Dim bchar As String
            Dim dchar As String
            Dim number_of_comma As Integer = 0
            For t As Integer = 0 To best_action.Count() - 1 Step 1
                If best_action.ElementAt(t) = "," Then
                    number_of_comma += 1
                End If
                If number_of_comma = 1 Then 'marks the area starting from the first comma
                    For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                        If best_action.ElementAt(q) = "," Then
                            bchar = best_action.Substring(t, q - t)
                            dchar = best_action.Substring(q + 1, best_action.LastIndexOf(",") - q - 1)
                            Exit For
                        End If
                    Next
                End If
            Next
            call_num = Convert.ToInt32(bchar)
            amb_num_2 = Convert.ToInt32(dchar)
            Dim amb_number As New Integer
            Dim call_number As New Integer
            Dim amb_loc As New Integer
            Dim amb_des As New Integer
            Dim call_loc As New Integer
            Dim call_pro As New Integer
            Dim amb_loc_2 As New Integer
            For t As Integer = state.IndexOf("[") To state.IndexOf("]") Step 1
                If state.ElementAt(t) = "(" Then
                    amb_number += 1
                    If amb_number - 1 = amb_num Then
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                        For q As Integer = state.LastIndexOf("[") To state.LastIndexOf("]") Step 1
                            If state.ElementAt(q) = "(" Then
                                call_number += 1
                                If call_number - 1 = call_num Then
                                    call_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", q) + 2, state.IndexOf(",", state.IndexOf(",", q) + 1) - state.IndexOf(",", q) - 2))
                                    call_pro = Convert.ToInt32(state.Substring(state.IndexOf(")", t) - 1, 1))
                                End If
                            End If
                        Next
                    End If
                    If amb_number - 1 = amb_num_2 Then
                        amb_loc_2 = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                    End If
                End If
            Next
            ''FEL best action development
            amb_number = New Integer
            FEL_Builder_iteration(2, amb_num, amb_loc, call_loc)
            FEL_Builder_iteration(6, amb_num_2, amb_loc_2, base_num)
            FEL_Builder_iteration(1, vbNull, vbNull, vbNull)
            amb_state_update(amb_num, call_loc, 0)
            amb_state_update(amb_num_2, base_num + 200, 0)
            call_state_update(1, call_num, call_loc, call_pro, 0)
            ''FEL other developments
            For l As Integer = 0 To state.IndexOf("]") Step 1 'ambuance state area
                If state.ElementAt(l) = "(" Then
                    amb_number += 1
                    If state.ElementAt(l + 1) = "1" Then ''if that amb is dispatched
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(2, amb_number - 1, amb_loc, amb_des)
                    End If
                    If state.ElementAt(l + 1) = "2" Then ''if amb is at the scene
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        FEL_Builder_iteration(3, amb_number - 1, vbNull, vbNull)
                        Dim count As Integer = c_state.Count() - 1
                        For t As Integer = 0 To count Step 1
                            If c_state(t).Item(0) = 1 Then
                                If c_state(t).Item(1) = amb_loc Then
                                    c_state.RemoveAt(t)
                                    Exit For
                                End If
                            End If
                        Next
                    End If
                    If state.ElementAt(l + 1) = "3" Then ''if amb is at hospital
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        FEL_Builder_iteration(4, amb_number - 1, amb_loc, vbNull)
                    End If
                    If state.ElementAt(l + 1) = "4" And amb_number - 1 <> amb_num Then ''if amb is going to base and is different from the amb of best action
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(5, amb_number - 1, amb_loc, amb_des)
                    End If
                    If state.ElementAt(l + 1) = "5" Then ''if amb is reallocating
                        amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", l) + 2, state.IndexOf(",", state.IndexOf(",", l) + 1) - state.IndexOf(",", l) - 2))
                        amb_des = Convert.ToInt32(state.Substring(state.IndexOf(",", state.IndexOf(" ", l)) + 2, state.IndexOf(")", l) - state.IndexOf(",", state.IndexOf(" ", l)) - 2))
                        FEL_Builder_iteration(6, amb_number - 1, amb_loc, amb_des)
                    End If
                End If
            Next
            Exit Sub
        End If
    End Sub
    Public Function expo_random_generator(ByVal lambda As Double)
        Dim rnd As Double = r1.NextDouble()
        expo_random_generator = (-1 / lambda) * Math.Log(1 - rnd)
    End Function
    Public Function normal_random_generator(ByVal mean As Double, stddev As Double)
        Return MathNet.Numerics.Distributions.Normal.Sample(r2, mean, stddev)
    End Function
    Private Function call_generator() As Double
        Dim discount_factor As Double = 0.99999
        Dim call_reigon(167) As Double
        Dim t As Integer
        new_call_num += 1
        current_event = 0
        For q As Integer = 0 To 167 Step 1
            If node_lambdas(q) <> 0 Then
                call_reigon(q) = expo_random_generator((1 / node_lambdas(q))) ' + arrival_change * (1 / node_lambdas(q)))
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
        For t = 0 To 167 Step 1
            If call_reigon(t) = arrival_clock Then
                locations = t
            End If
        Next
        If r3.NextDouble > prob_priority Then
            priority = 1
        Else
            priority = 0
        End If
        call_state_update(0, new_call_num, locations, priority, clock + arrival_clock)
        Return arrival_clock
    End Function
    Private Sub FEL_Builder_iteration(ByVal action As Integer, number_of_amb As Integer, ByVal origin As Integer, ByVal destination As Integer)
        Dim B As List(Of Double) = New List(Of Double)
        B.Add(action)
        Select Case action
            Case 1
                B.Add(clock + call_generator())
            Case 2
                B.Add(clock + travel_lambdas(origin).Item(destination))
                amb_list(number_of_amb) = B(1)
            Case 3
                B.Add(clock + normal_random_generator(54.18, 15.8))
                amb_list(number_of_amb) = B(1)
            Case 4
                B.Add(clock + normal_random_generator(56.7, 13.6))
                amb_list(number_of_amb) = B(1)
            Case 5
                B.Add(clock + travel_lambdas(origin).Item(destination))
                amb_list(number_of_amb) = B(1)
            Case 6
                B.Add(clock + travel_lambdas(origin).Item(destination))
                amb_list(number_of_amb) = B(1)
        End Select
        fel.Add(B)
    End Sub
    Private Sub simulation_of_state(ByVal state)
        Dim amb_num As Integer
        next_action = (From i In fel Order By i(1) Ascending Select i).First()
        clock = next_action(1)
        ''''''''''''''''''''''''''''busy time'''''''''''''''
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
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Select Case next_action(i)
            Case 1
                'sampling_state_iteration(0, 10)
                call_arrival()
                'trace_current(vbNull)
                'trace_fel()
            Case 2
                Dim rand As New Double
                rand = r5.NextDouble
                If rand > prob_scene_hospital Then
                    For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                        If clock = amb_list(t) Then
                            'trace_current(t)
                            ambulance_served_at_scene(t)
                            'sampling_state_iteration(2, t)
                            'trace_fel()
                            amb_num = t
                            Exit For
                        End If
                    Next
                Else
                    For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                        If clock = amb_list(t) Then
                            'trace_current(t)
                            amb_to_hospital(t)
                            'sampling_state_iteration(2, t)
                            'trace_fel()
                            amb_num = t
                            Exit For
                        End If
                    Next
                End If
                If c_state.Count() >= 1 Then
                    For t As Integer = 0 To c_state.Count() - 1 Step 1
                        If c_state(t).Item(1) = M_state(amb_num).Item(1) Then
                            If c_state(t).Item(3) < Form1.TextBox1.Text Then 'And c_state(t).Item(2) = 1 Then
                                calculating_waiting_time(clock, c_state(t).Item(3), reps, state, c_state(t).Item(2)) ''calculating waiting time for each call
                            End If
                            c_state.RemoveAt(t)
                            Exit For
                        End If
                    Next
                End If
            Case 3
                For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                    If clock = amb_list(t) Then
                        finished_at_scene(t)
                        'trace_current(t)
                        'sampling_state_iteration(3, t)
                        'trace_fel()
                        Exit For
                    End If
                Next
            Case 4
                For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                    If clock = amb_list(t) Then
                        finished_at_hospital(t)
                        'trace_current(t)
                        'sampling_state_iteration(4, t)
                        'trace_fel()
                        Exit For
                    End If
                Next
            Case 5
                For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                    If clock = amb_list(t) Then
                        'trace_current(t)
                        arrives_at_base(t)
                        'sampling_state_iteration(5, t)
                        'trace_fel()
                        Exit For
                    End If
                Next
            Case 6
                For t As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                    If clock = amb_list(t) Then
                        trace_current(t)
                        arrives_at_base(t)
                        'sampling_state_iteration(6, t)
                        trace_fel()
                        Exit For
                    End If
                Next
        End Select
        fel.Remove(next_action)
    End Sub
    Public Function sampling_state_iteration(ByVal events As Integer, ByVal amb_number As Integer)
        'Dim file_path As String = "C:\Users\snasrol\Desktop\output\State_ADP Complete Sample.txt"
        Dim sb As StringBuilder = New StringBuilder
        sb.Append("{" + events.ToString + ", [")
        For t = 0 To dynamic_ambulance_number - 1 Step 1
            sb.Append("(" + M_state(t).Item(0).ToString + ", " + M_state(t).Item(1).ToString + ", " + M_state(t).Item(2).ToString + ")")
        Next
        sb.Append("], [")
        For t = 0 To c_state.Count() - 1 Step 1
            If c_state(t).Item(3) <= clock Then
                sb.Append("(" + c_state(t).Item(0).ToString + ", " + c_state(t).Item(1).ToString + ", " + c_state(t).Item(2).ToString + ")")
            End If
        Next
        sb.Append("]}")
        sb.AppendLine("," + amb_number.ToString())
        'Using outfile As StreamWriter = New StreamWriter(file_path, True)
        'outfile.Write(sb.ToString())
        'End Using
        Return sb.ToString()
    End Function
    Private Sub call_arrival() ''mabey there is a best action in the policy
        If clock > Form1.TextBox1.Text Then
            Exit Sub
        End If
        num_of_total_calls += 1
        Dim state As String = sampling_state_iteration(0, 50)
        FEL_Builder_iteration(1, vbNull, vbNull, vbNull)
        Dim best_action As String = Form1.fixed_action(state, 0, 0, clock)
        If best_action <> "None" Then
            ''best dispatch and reallocate action
            If best_action.Substring(0, best_action.IndexOf(",")) = "da" Then 'if the action is dispatching and reallocation
                best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "da" from best action string
                Dim amb_num As New Integer
                Dim call_num As New Integer
                Dim amb_num_2 As New Integer
                Dim base_num As New Integer
                Dim schar As String = best_action.Substring(0, best_action.IndexOf(",")) 'marks the area to the first comma
                amb_num = Convert.ToInt32(schar)
                Dim achar As String = best_action.Substring(best_action.LastIndexOf(",") + 1) 'marks the area from the third comma
                base_num = Convert.ToInt32(achar)
                Dim bchar As String
                Dim dchar As String
                Dim number_of_comma As Integer = 0
                For t As Integer = 0 To best_action.Count() - 1 Step 1
                    If best_action.ElementAt(t) = "," Then
                        number_of_comma += 1
                    End If
                    If number_of_comma = 1 Then 'marks the area starting from the first comma
                        For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                            If best_action.ElementAt(q) = "," Then
                                bchar = best_action.Substring(t, q - t)
                                dchar = best_action.Substring(q + 1, best_action.LastIndexOf(",") - q - 1)
                                Exit For
                            End If
                        Next
                    End If
                Next
                call_num = Convert.ToInt32(bchar)
                amb_num_2 = Convert.ToInt32(dchar)
                Dim call_number As New Integer
                Dim call_loc As New Integer
                For t As Integer = state.LastIndexOf("[") To state.LastIndexOf("]") Step 1
                    If state.ElementAt(t) = "(" Then
                        call_number += 1
                        If call_number - 1 = call_num Then
                            call_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                        End If
                    End If
                Next
                ambulance_dispatching(amb_num, call_loc)
                call_state_update(1, call_num, c_state(call_num).Item(1), c_state(call_num).Item(2), c_state(call_num).Item(3))
                ambulance_reallocating(amb_num_2, base_num)
            End If
            ''best dispatch action''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            If best_action.Substring(0, best_action.IndexOf(",")) = "d" Then 'if the action is only dispatching
                best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "d" from best action string
                Dim amb_num As New Integer
                Dim call_num As New Integer
                Dim schar As String = best_action.Substring(0, best_action.IndexOf(",")) 'marks the area to the first comma
                amb_num = Convert.ToInt32(schar)
                Dim bchar As String
                Dim number_of_comma As Integer = 0
                For t As Integer = 0 To best_action.Count() - 1 Step 1
                    If best_action.ElementAt(t) = "," Then
                        number_of_comma += 1
                    End If
                    If number_of_comma = 1 Then 'marks the area starting from the first comma
                        For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                            If best_action.ElementAt(q) = "," Then
                                bchar = best_action.Substring(t, q - t)
                                Exit For
                            End If
                        Next
                    End If
                Next
                call_num = Convert.ToInt32(bchar)
                Dim call_number As New Integer
                Dim call_loc As New Integer
                For t As Integer = state.LastIndexOf("[") To state.LastIndexOf("]") Step 1
                    If state.ElementAt(t) = "(" Then
                        call_number += 1
                        If call_number - 1 = call_num Then
                            call_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                        End If
                    End If
                Next
                ambulance_dispatching(amb_num, call_loc)
                call_state_update(1, call_num, c_state(call_num).Item(1), c_state(call_num).Item(2), c_state(call_num).Item(3))
            End If
        End If
    End Sub
    Private Sub ambulance_dispatching(ByVal number_of_amb As Integer, ByVal destination As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(1)
        amb_state_update(number_of_amb, destination, clock)
        FEL_Builder_iteration(2, number_of_amb, M_state(number_of_amb).Item(1), destination)
        calculate_coverage()
    End Sub
    Private Sub ambulance_reallocating(ByVal number_of_amb As Integer, ByVal base As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(1)
        amb_state_update(number_of_amb, 400 + base, clock)
        FEL_Builder_iteration(6, number_of_amb, M_state(number_of_amb).Item(1), base)
        calculate_coverage()
    End Sub
    Private Sub ambulance_served_at_scene(ByVal number_of_amb As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(2)
        amb_state_update(number_of_amb, 168, clock)
        FEL_Builder_iteration(3, number_of_amb, M_state(number_of_amb).Item(1), vbNull)
    End Sub
    Private Sub amb_to_hospital(ByVal number_of_amb As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(2)
        amb_state_update(number_of_amb, 169, clock)
        FEL_Builder_iteration(4, number_of_amb, M_state(number_of_amb).Item(1), vbNull)
    End Sub
    Private Sub finished_at_scene(ByVal number_of_amb As Integer) ''mabey there is a best action in the policy
        last_origin(number_of_amb) = M_state(number_of_amb).Item(2)
        Dim state As String = sampling_state_iteration(3, number_of_amb)
        Dim best_action As String = Form1.fixed_action(state, 1, Convert.ToInt32(state.ElementAt(1).ToString()), clock)
        If best_action <> "None" Then
            If best_action.Substring(0, best_action.IndexOf(",")) = "rd" Then 'if the action is redeployment and dispatching
                best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "rd" from best action string
                Dim amb_num As New Integer
                Dim call_num As New Integer
                Dim amb_num_2 As New Integer
                Dim base_num As New Integer
                Dim schar As String = best_action.Substring(0, best_action.IndexOf(",")) 'marks the area to the first comma
                amb_num = Convert.ToInt32(schar)
                Dim achar As String = best_action.Substring(best_action.LastIndexOf(",") + 1) 'marks the area from the third comma
                base_num = Convert.ToInt32(achar)
                Dim bchar As String
                Dim dchar As String
                Dim number_of_comma As Integer = 0
                For t As Integer = 0 To best_action.Count() - 1 Step 1
                    If best_action.ElementAt(t) = "," Then
                        number_of_comma += 1
                    End If
                    If number_of_comma = 1 Then 'marks the area starting from the first comma
                        For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                            If best_action.ElementAt(q) = "," Then
                                bchar = best_action.Substring(t, q - t)
                                dchar = best_action.Substring(q + 1, best_action.LastIndexOf(",") - q - 1)
                                Exit For
                            End If
                        Next
                    End If
                Next
                call_num = Convert.ToInt32(bchar)
                amb_num_2 = Convert.ToInt32(dchar)
                Dim amb_number As New Integer
                Dim call_number As New Integer
                Dim amb_loc As New Integer
                Dim amb_des As New Integer
                Dim call_loc As New Integer
                Dim call_pro As New Integer
                Dim amb_loc_2 As New Integer
                For t As Integer = state.IndexOf("[") To state.IndexOf("]") Step 1
                    If state.ElementAt(t) = "(" Then
                        amb_number += 1
                        If amb_number - 1 = amb_num Then
                            amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                            For q As Integer = state.LastIndexOf("[") To state.LastIndexOf("]") Step 1
                                If state.ElementAt(q) = "(" Then
                                    call_number += 1
                                    If call_number - 1 = call_num Then
                                        call_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", q) + 2, state.IndexOf(",", state.IndexOf(",", q) + 1) - state.IndexOf(",", q) - 2))
                                        call_pro = Convert.ToInt32(state.Substring(state.IndexOf(")", t) - 1, 1))
                                    End If
                                End If
                            Next
                        End If
                        If amb_number - 1 = amb_num_2 Then
                            amb_loc_2 = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                        End If
                    End If
                Next
                amb_state_update(amb_num, call_loc, clock)
                call_state_update(1, call_num, c_state(call_num).Item(1), c_state(call_num).Item(2), c_state(call_num).Item(3))
                ambulance_dispatching(amb_num, call_loc)
                amb_state_update(amb_num_2, 200 + base_num, clock)
                amb_to_base(amb_num_2, base_num)
            End If
            ''best dispatch action''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            If best_action.Substring(0, best_action.IndexOf(",")) = "d" Then 'if the action is only dispatching
                best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "d" from best action string
                Dim amb_num As New Integer
                Dim call_num As New Integer
                Dim schar As String = best_action.Substring(0, best_action.IndexOf(",")) 'marks the area to the first comma
                amb_num = Convert.ToInt32(schar)
                Dim bchar As String
                Dim number_of_comma As Integer = 0
                For t As Integer = 0 To best_action.Count() - 1 Step 1
                    If best_action.ElementAt(t) = "," Then
                        number_of_comma += 1
                    End If
                    If number_of_comma = 1 Then 'marks the area starting from the first comma
                        For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                            If best_action.ElementAt(q) = "," Then
                                bchar = best_action.Substring(t, q - t)
                                Exit For
                            End If
                        Next
                    End If
                Next
                call_num = Convert.ToInt32(bchar)
                Dim call_number As New Integer
                Dim call_loc As New Integer
                For t As Integer = state.LastIndexOf("[") To state.LastIndexOf("]") Step 1
                    If state.ElementAt(t) = "(" Then
                        call_number += 1
                        If call_number - 1 = call_num Then
                            call_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                        End If
                    End If
                Next
                amb_state_update(amb_num, call_loc, clock)
                call_state_update(1, call_num, c_state(call_num).Item(1), c_state(call_num).Item(2), c_state(call_num).Item(3))
                ambulance_dispatching(amb_num, call_loc)
            End If
            ''best redeploy action''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            If best_action.Substring(0, best_action.IndexOf(",")) = "r" Then 'if the action is only redeployment
                best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "r" from best action string
                Dim amb_num_2 As New Integer
                Dim base_num As New Integer
                Dim achar As String = best_action.Substring(best_action.LastIndexOf(",") + 1) 'marks the area from the third comma
                base_num = Convert.ToInt32(achar)
                Dim dchar As String
                Dim number_of_comma As Integer = 0
                For t As Integer = 0 To best_action.Count() - 1 Step 1
                    If best_action.ElementAt(t) = "," Then
                        number_of_comma += 1
                    End If
                    If number_of_comma = 1 Then 'marks the area starting from the first comma
                        For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                            If best_action.ElementAt(q) = "," Then
                                dchar = best_action.Substring(q + 1, best_action.LastIndexOf(",") - q - 1)
                                Exit For
                            End If
                        Next
                    End If
                Next
                amb_num_2 = Convert.ToInt32(dchar)
                amb_state_update(amb_num_2, 200 + base_num, clock)
                amb_to_base(amb_num_2, base_num)
            End If
        End If
    End Sub
    Private Sub finished_at_hospital(ByVal number_of_amb As Integer) ''mabey there is a best action in the policy
        last_origin(number_of_amb) = M_state(number_of_amb).Item(2)
        Dim state As String = sampling_state_iteration(3, number_of_amb)
        Dim best_action As String = Form1.fixed_action(state, 1, Convert.ToInt32(state.ElementAt(1).ToString()), clock)
        If best_action <> "None" Then
            If best_action.Substring(0, best_action.IndexOf(",")) = "rd" Then 'if the action is redeployment and dispatching
                best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "rd" from best action string
                Dim amb_num As New Integer
                Dim call_num As New Integer
                Dim amb_num_2 As New Integer
                Dim base_num As New Integer
                Dim schar As String = best_action.Substring(0, best_action.IndexOf(",")) 'marks the area to the first comma
                amb_num = Convert.ToInt32(schar)
                Dim achar As String = best_action.Substring(best_action.LastIndexOf(",") + 1) 'marks the area from the third comma
                base_num = Convert.ToInt32(achar)
                Dim bchar As String
                Dim dchar As String
                Dim number_of_comma As Integer = 0
                For t As Integer = 0 To best_action.Count() - 1 Step 1
                    If best_action.ElementAt(t) = "," Then
                        number_of_comma += 1
                    End If
                    If number_of_comma = 1 Then 'marks the area starting from the first comma
                        For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                            If best_action.ElementAt(q) = "," Then
                                bchar = best_action.Substring(t, q - t)
                                dchar = best_action.Substring(q + 1, best_action.LastIndexOf(",") - q - 1)
                                Exit For
                            End If
                        Next
                    End If
                Next
                call_num = Convert.ToInt32(bchar)
                amb_num_2 = Convert.ToInt32(dchar)
                Dim amb_number As New Integer
                Dim call_number As New Integer
                Dim amb_loc As New Integer
                Dim amb_des As New Integer
                Dim call_loc As New Integer
                Dim call_pro As New Integer
                Dim amb_loc_2 As New Integer
                For t As Integer = state.IndexOf("[") To state.IndexOf("]") Step 1
                    If state.ElementAt(t) = "(" Then
                        amb_number += 1
                        If amb_number - 1 = amb_num Then
                            amb_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                            For q As Integer = state.LastIndexOf("[") To state.LastIndexOf("]") Step 1
                                If state.ElementAt(q) = "(" Then
                                    call_number += 1
                                    If call_number - 1 = call_num Then
                                        call_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", q) + 2, state.IndexOf(",", state.IndexOf(",", q) + 1) - state.IndexOf(",", q) - 2))
                                        call_pro = Convert.ToInt32(state.Substring(state.IndexOf(")", t) - 1, 1))
                                    End If
                                End If
                            Next
                        End If
                        If amb_number - 1 = amb_num_2 Then
                            amb_loc_2 = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                        End If
                    End If
                Next
                amb_state_update(amb_num, call_loc, clock)
                call_state_update(1, call_num, c_state(call_num).Item(1), c_state(call_num).Item(2), c_state(call_num).Item(3))
                ambulance_dispatching(amb_num, call_loc)
                amb_state_update(amb_num_2, 200 + base_num, clock)
                amb_to_base(amb_num_2, base_num)
            End If
            ''best dispatch action''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            If best_action.Substring(0, best_action.IndexOf(",")) = "d" Then 'if the action is only redeployment
                best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "d" from best action string
                Dim amb_num As New Integer
                Dim call_num As New Integer
                Dim schar As String = best_action.Substring(0, best_action.IndexOf(",")) 'marks the area to the first comma
                amb_num = Convert.ToInt32(schar)
                Dim bchar As String
                Dim number_of_comma As Integer = 0
                For t As Integer = 0 To best_action.Count() - 1 Step 1
                    If best_action.ElementAt(t) = "," Then
                        number_of_comma += 1
                    End If
                    If number_of_comma = 1 Then 'marks the area starting from the first comma
                        For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                            If best_action.ElementAt(q) = "," Then
                                bchar = best_action.Substring(t, q - t)
                                Exit For
                            End If
                        Next
                    End If
                Next
                call_num = Convert.ToInt32(bchar)
                Dim call_number As New Integer
                Dim call_loc As New Integer
                For t As Integer = state.LastIndexOf("[") To state.LastIndexOf("]") Step 1
                    If state.ElementAt(t) = "(" Then
                        call_number += 1
                        If call_number - 1 = call_num Then
                            call_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                        End If
                    End If
                Next
                amb_state_update(amb_num, call_loc, clock)
                call_state_update(1, call_num, c_state(call_num).Item(1), c_state(call_num).Item(2), c_state(call_num).Item(3))
                ambulance_dispatching(amb_num, call_loc)
            End If
            ''best redeploy action''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            If best_action.Substring(0, best_action.IndexOf(",")) = "r" Then 'if the action is only redeployment
                best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "r" from best action string
                Dim amb_num_2 As New Integer
                Dim base_num As New Integer
                Dim achar As String = best_action.Substring(best_action.LastIndexOf(",") + 1) 'marks the area from the third comma
                base_num = Convert.ToInt32(achar)
                Dim dchar As String
                Dim number_of_comma As Integer = 0
                For t As Integer = 0 To best_action.Count() - 1 Step 1
                    If best_action.ElementAt(t) = "," Then
                        number_of_comma += 1
                    End If
                    If number_of_comma = 1 Then 'marks the area starting from the first comma
                        For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                            If best_action.ElementAt(q) = "," Then
                                dchar = best_action.Substring(q + 1, best_action.LastIndexOf(",") - q - 1)
                                Exit For
                            End If
                        Next
                    End If
                Next
                amb_num_2 = Convert.ToInt32(dchar)
                amb_state_update(amb_num_2, 200 + base_num, clock)
                amb_to_base(amb_num_2, base_num)
            End If
        End If
    End Sub
    Private Sub amb_to_base(ByVal number_of_amb As Integer, ByVal base As Integer)
        last_origin(number_of_amb) = M_state(number_of_amb).Item(1)
        amb_state_update(number_of_amb, 200 + base, clock)
        FEL_Builder_iteration(5, number_of_amb, M_state(number_of_amb).Item(1), base)
    End Sub
    Private Sub arrives_at_base(ByVal number_of_amb As Integer)
        amb_state_update(number_of_amb, 800 + M_state(number_of_amb).Item(2), clock)
        calculate_coverage()
        '''''''''''''''''''''''''''''''''''''''''
        Dim state As String = sampling_state_iteration(3, number_of_amb)
        Dim best_action As String = Form1.fixed_action(state, 1, Convert.ToInt32(state.ElementAt(1).ToString()), clock)
        If best_action <> "None" Then
            ''best dispatch action
            If best_action.Substring(0, best_action.IndexOf(",")) = "d" Then 'if the action is only dispatching
                best_action = best_action.Remove(0, best_action.IndexOf(",") + 1) 'remove the "d" from best action string
                Dim amb_num As New Integer
                Dim call_num As New Integer
                Dim schar As String = best_action.Substring(0, best_action.IndexOf(",")) 'marks the area to the first comma
                amb_num = Convert.ToInt32(schar)
                Dim bchar As String
                Dim number_of_comma As Integer = 0
                For t As Integer = 0 To best_action.Count() - 1 Step 1
                    If best_action.ElementAt(t) = "," Then
                        number_of_comma += 1
                    End If
                    If number_of_comma = 1 Then 'marks the area starting from the first comma
                        For q As Integer = t + 1 To best_action.Count() - 1 Step 1
                            If best_action.ElementAt(q) = "," Then
                                bchar = best_action.Substring(t, q - t)
                                Exit For
                            End If
                        Next
                    End If
                Next
                call_num = Convert.ToInt32(bchar)
                Dim call_number As New Integer
                Dim call_loc As New Integer
                For t As Integer = state.LastIndexOf("[") To state.LastIndexOf("]") Step 1
                    If state.ElementAt(t) = "(" Then
                        call_number += 1
                        If call_number - 1 = call_num Then
                            call_loc = Convert.ToInt32(state.Substring(state.IndexOf(",", t) + 2, state.IndexOf(",", state.IndexOf(",", t) + 1) - state.IndexOf(",", t) - 2))
                        End If
                    End If
                Next
                amb_state_update(amb_num, call_loc, clock)
                call_state_update(1, call_num, c_state(call_num).Item(1), c_state(call_num).Item(2), c_state(call_num).Item(3))
                ambulance_dispatching(amb_num, call_loc)
            End If
        End If
    End Sub
    Private Sub trace_fel()
        'form3.DataGridView1.Rows.Add(1)
        Dim h As Integer
        Dim t As Integer = (From i In fel Order By i(0) Ascending Select i(0)).Count() - 1
        For k As Integer = 1 To t Step 1
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 1 Then
                Form3.DataGridView1.Item(2, row - 1).Value = Form3.DataGridView1.Item(2, row - 1).Value + " - " + "next call arrival" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 2 Then
                For n As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                    If amb_list(n) = (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k) Then
                        h = n
                        Exit For
                    End If
                Next
                Form3.DataGridView1.Item(2, row - 1).Value = Form3.DataGridView1.Item(2, row - 1).Value + " - " + "ambulance " + h.ToString + " reaches the call scene" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 3 Then
                For n As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                    If amb_list(n) = (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k) Then
                        h = n
                        Exit For
                    End If
                Next
                Form3.DataGridView1.Item(2, row - 1).Value = Form3.DataGridView1.Item(2, row - 1).Value + " - " + "ambulance " + h.ToString + " finishes serving the call at scene" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 4 Then
                For n As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                    If amb_list(n) = (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k) Then
                        h = n
                        Exit For
                    End If
                Next
                Form3.DataGridView1.Item(2, row - 1).Value = Form3.DataGridView1.Item(2, row - 1).Value + " - " + "ambulance " + h.ToString + " finishes serving the call at hospital" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
            For n As Integer = 0 To dynamic_ambulance_number - 1 Step 1
                If amb_list(n) = (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k) Then
                    h = n
                    Exit For
                End If
            Next
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 5 Then
                Form3.DataGridView1.Item(2, row - 1).Value = Form3.DataGridView1.Item(2, row - 1).Value + " - " + "ambulance " + h.ToString + " arrives at base" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
            If (From i In fel Order By i(1) Ascending Select i(0)).ElementAt(k) = 6 Then
                Form3.DataGridView1.Item(2, row - 1).Value = Form3.DataGridView1.Item(2, row - 1).Value + " - " + "ambulance " + h.ToString + " reallocated and arrives at base" + " In Time: " + (From i In fel Order By i(1) Ascending Select i(1)).ElementAt(k).ToString() + Environment.NewLine
            End If
        Next
        For k As Integer = 0 To 16 Step 1
            form3.DataGridView1.Item(3, row - 1).Value = form3.DataGridView1.Item(3, row - 1).Value + "(" + M_state(k).Item(0).ToString + ", " + M_state(k).Item(1).ToString + ", " + M_state(k).Item(2).ToString + ")" + Environment.NewLine
        Next
    End Sub
    Private Sub trace_calls()
        'form3.DataGridView1.Rows.Add(1)
        Dim r As Integer
        For Each l In c_state
            If clock >= c_state(r).Item(3) Then
                Form3.DataGridView1.Item(4, row).Value = Form3.DataGridView1.Item(4, row).Value + "(" + c_state(r).Item(0).ToString + ", " + c_state(r).Item(1).ToString + ", " + c_state(r).Item(2).ToString + ")" + Environment.NewLine
                r = r + 1
            Else
                r = r + 1
            End If
        Next
    End Sub
    Private Sub trace_current(ByVal amb_number As Integer)
        Dim n As Integer = amb_number
        Form3.DataGridView1.Rows.Add(1)
        Form3.DataGridView1.Item(0, row).Value = clock.ToString()
        Select Case next_action(i)
            Case 1
                trace_calls()
                Form3.DataGridView1.Item(1, row).Value = "new call arrived"
            Case 2
                trace_calls()
                Form3.DataGridView1.Item(1, row).Value = "ambulance " + n.ToString + " reached the call scene"
            Case 3
                trace_calls()
                Form3.DataGridView1.Item(1, row).Value = "ambulance " + n.ToString + " finished at the scene"
            Case 4
                trace_calls()
                Form3.DataGridView1.Item(1, row).Value = "ambulance " + n.ToString + " finished at hospital"
            Case 5
                trace_calls()
                Form3.DataGridView1.Item(1, row).Value = "ambulance " + n.ToString + " arrived at base"
            Case 6
                trace_calls()
                Form3.DataGridView1.Item(1, row).Value = "reallocated ambulance " + n.ToString() + " arrived at base"
        End Select
        row += 1
    End Sub
    Public Sub control_replications_of_each_simulation(ByVal state As String)
        last_replication = 1
        waiting_time = New List(Of Double)
        number_of_missed_calls = New List(Of Double)
        number_of_missed_calls_pr = New List(Of Double)
        state_coverage = New List(Of Double)
        waiting_loss_time = New List(Of Double)
        total_hp_call_nom_list = New List(Of Double)
        total_lp_call_nom_list = New List(Of Double)
        For t As Integer = 1 To Form1.TextBox2.Text Step 1
            'form3.DataGridView1.Rows.Add(1)
            'form3.DataGridView1.Rows(row).DefaultCellStyle.BackColor = Color.Gray
            'form3.DataGridView1.Item(2, row).Value = "Replication" + t.ToString
            'row = row + 1

            reps += 1

            control_simulation_time_of_each_state(state)

            state_coverage.Add(coverage / Form1.TextBox1.Text)
            waiting_time.Add(temp_time)
            waiting_loss_time.Add(temp_loss_time)
            number_of_missed_calls.Add((miss_call_num_hp + miss_call_num_lp))
            number_of_missed_calls_pr.Add(miss_call_num_hp)
            total_hp_call_nom_list.add(total_hp_call_nom)
            total_lp_call_nom_list.add(total_lp_call_nom)
            busy_time_fraction.Add(busy_time / Form1.TextBox1.Text)

            'Dim file_path As String = "C:\Users\snasrol\Desktop\output\Waiting time.txt"
            'Dim sb As StringBuilder = New StringBuilder
            'sb.AppendLine(reps.ToString() + vbTab + temp_time.ToString() + vbTab + (coverage / Form1.TextBox1.Text).ToString() + vbTab + (miss_call_nom / num_of_total_calls).ToString + vbTab + (busy_time / Form1.TextBox1.Text).ToString())
            'Using outfile As StreamWriter = New StreamWriter(file_path, True)
            'outfile.WriteAsync(sb.ToString())
            'End Using

            Me.Label17.Text = Me.Label17.Text + 1
            Me.Label17.Refresh()

            start_next_replication()

            If t < Form1.TextBox2.Text Then
                state_FEL_building(state)
            End If
            seed += 1
            weed += 1
            leed += 1
            reed += 1
            zeed += 1
        Next
        'form3.Show()
    End Sub
    Public Sub control_simulation_time_of_each_state(ByVal state As String)
        While clock < Form1.TextBox1.Text
            simulation_of_state(state)
            Me.Label2.Text = clock
            Me.Label2.Refresh()
            'Me.Refresh()
            'Application.DoEvents()
        End While

    End Sub
    Private Sub start_next_replication()
        fel.RemoveRange(0, fel.Count())
        c_state.RemoveRange(0, c_state.Count())
        M_state.RemoveRange(0, M_state.Count())
        clock = 0
        last_clock = 0
        last_origin.Initialize()
        amb_list.Initialize()
        'closer_amb_distance.RemoveRange(0, closer_amb_distance.Count())
        busy_time_start = New Double
        busy_time_end = New Double
        busy_time = 0
        time_interval = 0
        coverage = 0
        temp_time = 0
        temp_loss_time = 0
        num_of_total_calls = 0
        miss_call_num_hp = 0
        miss_call_num_lp = 0
        total_hp_call_nom = 0
        total_lp_call_nom = 0
        new_call_num = 0
        last_num_of_call = 0
    End Sub
    Public Sub calculating_waiting_time(ByVal clock As Double, ByVal call_creation_time As Double, ByVal replication As Integer, ByVal state As String, ByVal priority As Integer)
        Dim discount_factor As Double = 0.99999
        If last_replication = replication Then
            temp_time = temp_time + (clock - call_creation_time) * priority_weight(priority) * Pow(discount_factor, clock)
            If priority = 1 Then
                total_hp_call_nom += 1 * priority_weight(priority) * Pow(discount_factor, clock)
            Else
                total_lp_call_nom += 1 * priority_weight(priority) * Pow(discount_factor, clock)
            End If
            'response_time = clock - creation_time
            If clock - call_creation_time > 8 Then
                temp_loss_time = temp_loss_time + (clock - call_creation_time) * priority_weight(priority) * Pow(discount_factor, clock)
                If priority = 1 Then
                    miss_call_num_hp += 1 * priority_weight(priority) * Pow(discount_factor, clock)
                Else
                    miss_call_num_lp += 1 * priority_weight(priority) * Pow(discount_factor, clock)
                End If
            End If
        Else
            If priority = 1 Then
                total_hp_call_nom = 1 * priority_weight(priority) * Pow(discount_factor, clock)
            Else
                total_lp_call_nom = 1 * priority_weight(priority) * Pow(discount_factor, clock)
            End If
            temp_time = (clock - call_creation_time) * priority_weight(priority) * Pow(discount_factor, clock)
            'response_time = clock - creation_time
            last_replication = replication
            If clock - call_creation_time > 8 Then
                If priority = 1 Then
                    miss_call_num_hp = 1 * priority_weight(priority) * Pow(discount_factor, clock)
                Else
                    miss_call_num_lp = 1 * priority_weight(priority) * Pow(discount_factor, clock)
                End If
                temp_loss_time = (clock - call_creation_time) * priority_weight(priority) * Pow(discount_factor, clock)
            Else
                If priority = 1 Then
                    miss_call_num_hp = 0
                Else
                    miss_call_num_lp = 0
                End If
                temp_loss_time = 0
            End If
        End If
       
    End Sub
    Private Sub BtExit_Click(sender As Object, e As EventArgs) Handles BtExit.Click
        Me.Close()
        Form1.Show()
    End Sub
    Public Sub calculate_coverage() 'not discounted
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
        'time_passed += time_interval
    End Sub
    Public Function priority_weight(ByVal priority As Integer) As Double
        If priority = 1 Then
            Return hp_weight
        Else
            Return Lp_weight
        End If
    End Function
End Class
