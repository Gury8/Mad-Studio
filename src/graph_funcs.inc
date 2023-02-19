{-----------------------------------------------------------------------------
 Draw a line
 -----------------------------------------------------------------------------}
// http://www.efg2.com/Lab/Library/Delphi/Graphics/Bresenham.txt
procedure TfrmGraph.Line (xStart, yStart, xEnd, yEnd : integer);
 {Bresenham's Line Algorithm.  Byte, March 1988, pp. 249-253.}
   VAR
     a, b       :  INTEGER;  {displacements in x and y}
     d          :  INTEGER;  {decision variable}
     diag_inc   :  INTEGER;  {d's increment for diagonal steps}
     dx_diag    :  INTEGER;  {diagonal x step for next pixel}
     dx_nondiag :  INTEGER;  {nondiagonal x step for next pixel}
     dy_diag    :  INTEGER;  {diagonal y step for next pixel}
     dy_nondiag :  INTEGER;  {nondiagonal y step for next pixel}
     i          :  INTEGER;  {loop index}
     nondiag_inc:  INTEGER;  {d's increment for nondiagonal steps}
     swap       :  INTEGER;  {temporary variable for swap}
     x, y       :  INTEGER;  {current x and y coordinates}
 BEGIN
   x := xStart;              {line starting point}
   y := yStart;
   {Determine drawing direction and step to the next pixel.}
   a := xEnd - xStart;       {difference in x dimension}
   b := yEnd - yStart;       {difference in y dimension}
   {Determine whether end point lies to right or left of start point.}
   IF   a < 0                {drawing towards smaller x values?}
   THEN BEGIN
     a := -a;                {make 'a' positive}
     dx_diag := -1
   END
   ELSE
     dx_diag := 1;
   {Determine whether end point lies above or below start point.}
   IF   b < 0                {drawing towards smaller x values?}
   THEN BEGIN
     b := -b;                {make 'a' positive}
     dy_diag := -1
   END
   ELSE
     dy_diag := 1;
   {Identify octant containing end point.}
   IF   a < b
   THEN BEGIN
     swap := a;
     a := b;
     b := swap;
     dx_nondiag := 0;
     dy_nondiag := dy_diag
   END
   ELSE BEGIN
     dx_nondiag := dx_diag;
     dy_nondiag := 0
   END;
   d := b + b - a;            {initial value for d is 2*b - a}
   nondiag_inc := b + b;      {set initial d increment values}
   diag_inc    := b + b - a - a;
   FOR i := 0 TO a DO BEGIN   {draw the a+1 pixels}
     PlotEx(x,y);
     IF   d < 0               {is midpoint above the line?}
     THEN BEGIN               {step nondiagonally}
       x += dx_nondiag;
       y += dy_nondiag;
       d += nondiag_inc   {update decision variable}
     END
     ELSE BEGIN               {midpoint is above the line; step diagonally}
       x += dx_diag;
       y += dy_diag;
       d += diag_inc
     END
   END;
   if isLine then
     undoList.Add('0;0;0;0;21');
 END;

{-----------------------------------------------------------------------------
 Draw a rectangle
 -----------------------------------------------------------------------------}
procedure TfrmGraph.Rectangle(x1, y1, x2, y2 : integer);
begin
  //Line(x1, y1, x2 - 1, y1);
  //Line(x2 - 1, y1, x2 - 1, y2 - 1);
  //Line(x1, y2 - 1, x2 - 1, y2 - 1);
  //Line(x1, y1, x1, y2 - 1);
  isLine := false;
  if x1 > x2 then begin
    Line(x1 - 1, y1, x2 , y1);
    Line(x2, y1, x2, y2 - 1);
    Line(x1 - 1, y2 - 1, x2, y2 - 1);
    Line(x1 - 1, y1, x1 - 1, y2 - 1);
  end
  else begin
    Line(x1, y1, x2 - 1 , y1);
    Line(x2 - 1, y1, x2 - 1, y2 - 1);
    Line(x1, y2 - 1, x2 - 1, y2 - 1);
    Line(x1, y1, x1, y2 - 1);
  end;
//  refreshp;

  undoList.Add('0;0;0;0;21');
end;

{-----------------------------------------------------------------------------
 Draw filled rectangle
 -----------------------------------------------------------------------------}
procedure TfrmGraph.FillRectangle(x1, y1, x2, y2 : integer);
var
  y : integer;
begin
  isLine := false;
  if x1 > x2 then begin
    Line(x1 - 1, y1, x2 , y1);
    Line(x2, y1, x2, y2 - 1);
    Line(x1 - 1, y2 - 1, x2, y2 - 1);
    Line(x1 - 1, y1, x1 - 1, y2 - 1);
    for y := y1 + 1 to y2 - 1 do
      Line(x1 - 1, y, x2, y);
  end
  else begin
    Line(x1, y1, x2 - 1 , y1);
    Line(x2 - 1, y1, x2 - 1, y2 - 1);
    Line(x1, y2 - 1, x2 - 1, y2 - 1);
    Line(x1, y1, x1, y2 - 1);
    for y := y1 + 1 to y2 - 1 do
      Line(x1, y, x2 - 1, y);
  end;
//  refreshp;

  undoList.Add('0;0;0;0;21');
end;

//procedure TfrmGraph.Circle(xc, yc, r : integer);
//var
//  p,k,x,y : integer;
//begin
//p:=1-r;
//x:=0;
//y:=r;
//for k:=0 to y do begin
//  PlotEx(xc+x,yc+y);
//  PlotEx(xc-y,yc-x);
//  PlotEx(xc+y,yc-x);
//  PlotEx(xc-y,yc+x);
//  PlotEx(xc+y,yc+x);
//  PlotEx(xc-x,yc-y);
//  PlotEx(xc+x,yc-y);
//  PlotEx(xc-x,yc+y);
//if(p>0)then begin
// p:=p+2*(x+1)+1-2*(y+1);
// inc(x);
// dec(y);
//end
//else begin
// p:=p+2*(x+1)+1;
// inc(x);
//end
//end;
//refreshp;
//end;

{-----------------------------------------------------------------------------
 Draw a circle
 -----------------------------------------------------------------------------}
procedure TfrmGraph.Ellipse(x, y, r : integer);
var
  a, c : Integer;
//  swapX, swapY : integer;
begin
  // Check for radius of zero
  if r = 0 then begin
    PlotEx(x, y);
    refreshp;
    Exit;
  end;

  //if x > oldx then begin
  //  swapX := x;
  //  x := oldX;
  //  oldX := swapX;
  //end;
  //
  //if y > oldY then begin
  //  swapY := y;
  //  y := oldY;
  //  oldY := swapY;
  //end;

  isLine := false;

  // Circle algorithm
  c := 0; a := r - 1;
  while r >= c do begin
    PlotEx(x + c, y + r);
    PlotEx(x + c, y - r);
    PlotEx(x - c, y - r);
    PlotEx(x - c, y + r);
    PlotEx(x + r, y + c);
    PlotEx(x + r, y - c);
    PlotEx(x - r, y - c);
    PlotEx(x - r, y + c);
    Inc(c);
    a += 1 - c - c;
    if a >= 0 then Continue;
    Dec(r);
    a += r + r;
  end;

  //j := 1;
  //while j < r do begin
  //  i := j;
  //  c := 0; a := i - 1;
  //  while i >= c do begin
  //    PlotEx(x + c, y + i);
  //    PlotEx(x + c, y - i);
  //    PlotEx(x - c, y - i);
  //    PlotEx(x - c, y + i);
  //    PlotEx(x + i, y + c);
  //    PlotEx(x + i, y - c);
  //    PlotEx(x - i, y - c);
  //    PlotEx(x - i, y + c);
  //    Inc(c);
  //    a += 1 - c - c;
  //    if a >= 0 then Continue;
  //    Dec(i);
  //    a += i + i;
  //  end;
  //  Inc(j);
  //end;

//  refreshp;

  undoList.Add('0;0;0;0;21');
end;

{-----------------------------------------------------------------------------
 Draw filled circle
 -----------------------------------------------------------------------------}
// https://www.pascalgamedevelopment.com/showthread.php?30526-Draw-a-filled-circle
procedure TfrmGraph.FillCircle(xc, yc, r: longint);
var
  x, y, d: longint;
begin
  isLine := false;
  x := 0; y := r;
  d := 1 - r;
  while x < y do begin
    if d < 0 then
      d := d + x shl 1 + 3
    else begin
      d := d + x shl 1 - y shl 1 + 5;
      dec(y);
    end;
    Line(xc - x, yc - y, xc + x, yc - y);
    Line(xc - y, yc - x, xc + y, yc - x);
    Line(xc - y, yc + x, xc + y, yc + x);
    Line(xc - x, yc + y, xc + x, yc + y);
    inc(x);
  end;
  undoList.Add('0;0;0;0;21');
end;

{-----------------------------------------------------------------------------
 Draw a triangle
 -----------------------------------------------------------------------------}
procedure TfrmGraph.Triangle(x1, y1, x2, y2 : integer);
begin
  isLine := false;
  Line(x1, y1, x1 + ((x2 - x1) div 2), y2);
  Line(x1 + ((x2 - x1) div 2), y2, x2, y1);
  Line(x1, y1, x2, y1);
  undoList.Add('0;0;0;0;21');
end;

//procedure TfrmGraph.Flodfill(x, y : integer; f, o : integer);
//var
//  x, y : integer;
//begin
//  void flodfill(int x,int y,int f,int o)
//  {
//  int c;
//   c=getpixel(x,y);
//    if(c==o)
//    {
//    setcolor(f);
//    putpixel (x,y,f);
//     delay(10);
//     flodfill(x+1,y,f,o);
//     flodfill(x,y+1,f,o);
//     flodfill(x+1,y+1,f,o);
//     flodfill(x-1,y-1,f,o);
//     flodfill(x-1,y,f,o);
//     flodfill(x,y-1,f,o);
//     flodfill(x-1,y+1,f,o);
//     flodfill(x+1,y-1,f,o);
//    }
//  }
//end;

