myShapes model = [ dragon ( model.size )  |> move (20, 21) 
                 , roundedRect 4 120 2
                 |> filled grey |> move (80, 0)
                 , circle 2
                 |> filled darkGrey
                 |> move model.pos 
                 |> notifyMouseDown ChangeToDragging
                 ,
                 case model.dragState of 
                     Released -> group []
                     Dragging -> rect 185 125 
                         |> ghost 
                         |> notifyMouseMoveAt Drag 
                         |> notifyLeave ChangeToRelease
                         |> notifyMouseUp ChangeToRelease
                 ]                 

dragon n =   
    group <|
      ( arc
      )  
      ::    
      ( if n > 0 then         
        let           
          aChild = dragon (n-1)
        in         
          [aChild 
          |> rotate (degrees -90)
          |> (if ((modBy 8 n) == 1 || (modBy 8 n) == 2 || (modBy 8 n) == 0) && modBy 2 n == 1 then move ( (-4 * ( 2^( ((toFloat n+1)/2)-1))), 0) else move (0,0))
          |> (if ((modBy 8 n) == 1 || (modBy 8 n) == 2 || (modBy 8 n) == 0) && modBy 2 n == 0 then move ( (-4 * ( 2^( (toFloat n/2)-1))), 0) else move (0,0))
          |> (if ((modBy 8 n) == 4 || (modBy 8 n) == 5 || (modBy 8 n) == 6) && modBy 2 n == 1 then move ( (4 * ( 2^( ((toFloat n+1)/2)-1))), 0) else move (0,0))
          |> (if ((modBy 8 n) == 4 || (modBy 8 n) == 5 || (modBy 8 n) == 6) && modBy 2 n == 0 then move ( (4 * ( 2^( (toFloat n/2)-1))), 0) else move (0,0))
          |> (if ((modBy 8 n) == 2 || (modBy 8 n) == 3 || (modBy 8 n) == 4) && modBy 2 n == 1 then move (0, (-4*(2^(((toFloat n+1)/2)-1)))) else move (0,0))
          |> (if ((modBy 8 n) == 2 || (modBy 8 n) == 3 || (modBy 8 n) == 4) && modBy 2 n == 0 then move (0, (-4*(2^((toFloat n/2)-1)))) else move (0,0))
          |> (if ((modBy 8 n) == 6 || (modBy 8 n) == 7 || (modBy 8 n) == 0) && modBy 2 n == 1 then move (0, (4*(2^(((toFloat n+1)/2)-1)))) else move (0,0))
          |> (if ((modBy 8 n) == 6 || (modBy 8 n) == 7 || (modBy 8 n) == 0) && modBy 2 n == 0 then move (0, (4*(2^((toFloat n/2)-1)))) else move (0,0))
          ,aChild
          ]   
        else       
          []    
        )

arc = group [
      openPolygon [(0,0),(0,2),(-2,2)]
      |> outlined (solid 0.5) black
      ]

type DragState = Released | Dragging
type Msg = Tick Float GetKeyState | ChangeToRelease | ChangeToDragging | Drag (Float,Float)
getY (x,y) = y
checkPosition y = if y <= -45 then 0 else 
      if y <= -35 then 1 else 
      if y <= -25 then 2 else 
      if y <= -15 then 3 else 
      if y <= -5 then 4 else 
      if y <= 5 then 5 else 
      if y <= 15 then 6 else 
      if y <= 25 then 7 else 
      if y <= 35 then 8 else 
      if y <= 45 then 9 else 
      if y <= 55 then 10 else y

update msg model = case msg of                     
      Tick t _ -> { time = t, dragState = model.dragState, pos = model.pos, size = round (checkPosition (getY model.pos)) }
      
      ChangeToRelease -> { model | dragState = Released }
                                                      
      ChangeToDragging -> { model | dragState = Dragging }
                                        
      Drag (x, y) -> if y <= 55 && y >= -55 then { model | pos = (80 , y) } else { model | pos = model.pos }        
      
init = { time = 0, dragState = Released, pos = (80,-55), size = 0}
