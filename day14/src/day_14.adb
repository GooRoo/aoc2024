--  with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Day_14 is

   procedure Robot_Put_Image
      (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item : Robot) is
   begin
      Output.Put ("(");
      Output.Put (Item.Pos.X'Img);
      Output.Put (";");
      Output.Put (Item.Pos.Y'Img);
      Output.Put (") -> (");
      Output.Put (Item.Vel.DX'Img);
      Output.Put (";");
      Output.Put (Item.Vel.DY'Img);
      Output.Put (")");
   end Robot_Put_Image;

   procedure Print_Matrix (Item : Matrix) is
      package IO renames Ada.Text_IO;
      --  package Int_IO renames Ada.Integer_Text_IO;
   begin
      for Y in Y_Coordinate loop
         for X in X_Coordinate loop
            if Item (X, Y) = 0 then
               IO.Put (" ");
            else
               --  Int_IO.Put (Item (X, Y), Width => 0);
               IO.Put ("*");
            end if;
         end loop;
         IO.New_Line;
      end loop;
   end Print_Matrix;

   procedure Move (Self : in out Robot) is
   begin
      Self.Pos.X := @ + X_Coordinate'Mod (Self.Vel.DX);
      Self.Pos.Y := @ + Y_Coordinate'Mod (Self.Vel.DY);
   end Move;

   procedure Move_By (Self : in out Robot; Steps : Positive := 1) is
   begin
      Self.Pos.X := @ + X_Coordinate'Mod (Integer (Self.Vel.DX) * Steps);
      Self.Pos.Y := @ + Y_Coordinate'Mod (Integer (Self.Vel.DY) * Steps);
   end Move_By;

   function Read_From_File (Filename : String) return Robots is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;

      File : File_Type;
      Line : String (1 .. 100);
      Last : Natural;
      Result : Robots;

      P_Tag : constant String := "p=";
      V_Tag : constant String := "v=";
   begin
      Open (File, In_File, Filename);

      while not End_Of_File (File)
      loop
         Get_Line (File, Line, Last);

         declare
            P_Start : constant Natural := Index (Line (1 .. Last), P_Tag);
            V_Start : constant Natural := Index (Line (P_Start .. Last), V_Tag);

            P_Values : constant String := Trim (Line (1 + P_Tag'Length .. V_Start - 2), Ada.Strings.Both);
            V_Values : constant String := Trim (Line (V_Start + V_Tag'Length .. Last), Ada.Strings.Both);

            P_Comma : constant Natural := Index (P_Values, ",");
            V_Comma : constant Natural := Index (V_Values, ",");

            Pos_X : constant Natural := Natural'Value (P_Values (1 .. P_Comma - 1));
            Pos_Y : constant Natural := Natural'Value (P_Values (P_Comma + 1 .. P_Values'Last));
            Vel_X : constant Integer := Integer'Value (V_Values (1 .. V_Comma - 1));
            Vel_Y : constant Integer := Integer'Value (V_Values (V_Comma + 1 .. V_Values'Last));

            New_Particle : constant Robot := Robot'(
               Pos => (X => X_Coordinate (Pos_X), Y => Y_Coordinate (Pos_Y)),
               Vel => (DX => X_Movement (Vel_X), DY => Y_Movement (Vel_Y))
            );
         begin
            Robot_Vectors.Append (Result, New_Particle);
         exception
            when Constraint_Error =>
               Put_Line ("Error in line: " & Line (1 .. Last));
               Put_Line ("P_Values: '" & P_Values & "'");
               Put_Line ("V_Values: '" & V_Values & "'");
               Put_Line ("P_Comma: " & P_Comma'Image);
               Put_Line ("V_Comma: " & V_Comma'Image);
         end;

      end loop;

      Close (File);

      return Result;
   exception
      when others =>
         if Is_Open (File) then
            Close (File);
         end if;
         raise;
   end Read_From_File;

   procedure Simulate (Rs : in out Robots; Seconds : Positive) is
   begin
      for R of Rs
      loop
         Move_By (R, Seconds);
      end loop;
   end Simulate;

   procedure Fill_Matrix (Rs : Robots; M : in out Matrix) is
   begin
      for R of Rs
      loop
         M (R.Pos.X, R.Pos.Y) := @ + 1;
      end loop;
   end Fill_Matrix;

   function Count_Robots (Rs : Robots) return Quadrants is
      Result : Quadrants := [0, 0, 0, 0];
   begin
      for R of Rs
      loop
         if R.Pos.X < X_Coordinate'Mod (Width / 2) then
            if R.Pos.Y < Y_Coordinate'Mod (Height / 2) then
               Result (Top_Left) := @ + 1;
            elsif R.Pos.Y > Y_Coordinate'Mod (Height / 2) then
               Result (Bottom_Left) := @ + 1;
            end if;
         elsif R.Pos.X > X_Coordinate'Mod (Width / 2) then
            if R.Pos.Y < Y_Coordinate'Mod (Height / 2) then
               Result (Top_Right) := @ + 1;
            elsif R.Pos.Y > Y_Coordinate'Mod (Height / 2) then
               Result (Bottom_Right) := @ + 1;
            end if;
         end if;
      end loop;
      return Result;
   end Count_Robots;

   procedure Solve_First is
      package IO renames Ada.Text_IO;

      Data : Robots := Read_From_File ("data/task.data");
      Field : Matrix;
   begin
      Simulate (Data, 100);
      Fill_Matrix (Data, Field);
      --  Print_Matrix (Field);

      declare
         Counters : constant Quadrants := Count_Robots (Data);
         Safety_Factor : Natural;
      begin
         IO.Put_Line ("=====================");
         IO.Put_Line ("First task:");
         --  IO.Put_Line ("Top left: " & Counters (Top_Left)'Image);
         --  IO.Put_Line ("Top right: " & Counters (Top_Right)'Image);
         --  IO.Put_Line ("Bottom left: " & Counters (Bottom_Left)'Image);
         --  IO.Put_Line ("Bottom right: " & Counters (Bottom_Right)'Image);
         Safety_Factor := Counters (Top_Left) * Counters (Top_Right) * Counters (Bottom_Left) * Counters (Bottom_Right);
         IO.Put_Line ("Safety factor: " & Safety_Factor'Image);
      end;

   end Solve_First;

   procedure Solve_Second is
      package IO renames Ada.Text_IO;

      function Calculate_Variance (Rs : Robots; Get_Coordinate : access function (P : Point) return Natural) return Float is
         Sum : Float := 0.0;
         Mean : Float;
         Var_Sum : Float := 0.0;
         N : constant Float := Float (Rs.Length);
      begin
         for R of Rs loop
            Sum := @ + Float (Get_Coordinate (R.Pos));
         end loop;
         Mean := Sum / N;

         for R of Rs loop
            Var_Sum := @ + (Float (Get_Coordinate (R.Pos)) - Mean) ** 2;
         end loop;

         return Var_Sum / (N - 1.0);
      end Calculate_Variance;

      function Get_X (P : Point) return Natural is (Natural (P.X));
      function Get_Y (P : Point) return Natural is (Natural (P.Y));

      Data : Robots := Read_From_File ("data/task.data");
      Best_X_Time : Natural := 0;
      Best_Y_Time : Natural := 0;
      Best_X_Var : Float := Float'Last;
      Best_Y_Var : Float := Float'Last;
      Current_Var : Float;
      Max_Time : constant Natural := Natural'Max (Width, Height);
   begin
      for T in 0 .. Max_Time - 1
      loop
         Current_Var := Calculate_Variance (Data, Get_X'Access);
         if Current_Var < Best_X_Var then
            Best_X_Time := T;
            Best_X_Var := Current_Var;
         end if;

         Current_Var := Calculate_Variance (Data, Get_Y'Access);
         if Current_Var < Best_Y_Var then
            Best_Y_Time := T;
            Best_Y_Var := Current_Var;
         end if;

         Simulate (Data, 1);
      end loop;

      declare
         function Mod_Inverse (A, M : Natural) return Natural is
            T, New_T : Integer := 0;
            R, New_R : Integer := M;
            Old_T : Integer := 1;
            Old_R : Integer := A;
            Quotient : Integer;
         begin
            while New_R /= 0
            loop
               Quotient := Old_R / New_R;
               T := Old_T - Quotient * New_T;
               Old_T := New_T;
               New_T := T;
               R := Old_R - Quotient * New_R;
               Old_R := New_R;
               New_R := R;
            end loop;

            if Old_T < 0 then
               Old_T := @ + M;
            end if;

            return Natural (Old_T);
         end Mod_Inverse;

         Inverse : constant Natural := Mod_Inverse (Width, Height);
         Result : constant Natural := Best_X_Time +
                  ((Inverse * (Best_Y_Time - Best_X_Time)) mod Height) * Width;
      begin
         Data := Read_From_File ("data/task.data");
         Simulate (Data, Result);

         IO.Put_Line ("=====================");
         IO.Put_Line ("Second task:");

         declare
            Field : Matrix;
         begin
            Fill_Matrix (Data, Field);
            Print_Matrix (Field);
         end;

         IO.Put_Line ("At second: " & Result'Image);
      end;
   end Solve_Second;

end Day_14;
