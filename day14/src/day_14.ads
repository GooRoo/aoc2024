pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Strings.Text_Buffers;

package Day_14 is

   procedure Solve_First;
   procedure Solve_Second;

   Width : constant := 101;
   Height : constant := 103;

   type X_Coordinate is mod Width;
   type Y_Coordinate is mod Height;

   type Matrix is array (X_Coordinate, Y_Coordinate) of Integer
      with Default_Component_Value => 0;

   type X_Movement is range -Width .. Width;
   type Y_Movement is range -Height .. Height;

   type Point is record
      X : X_Coordinate;
      Y : Y_Coordinate;
   end record;

   type Velocity is record
      DX : X_Movement;
      DY : Y_Movement;
   end record;

   type Robot is record
      Pos : Point;
      Vel : Velocity;
   end record with Put_Image => Robot_Put_Image;

   type Quadrant is (Top_Left, Top_Right, Bottom_Left, Bottom_Right);

   type Quadrants is array (Quadrant) of Natural;

   procedure Move (Self : in out Robot);
   procedure Move_By (Self : in out Robot; Steps : Positive := 1);

   procedure Robot_Put_Image
      (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Item : Robot);

   package Robot_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural,
         Element_Type => Robot);

   subtype Robots is Robot_Vectors.Vector;

   function Read_From_File (Filename : String) return Robots;

   procedure Print_Matrix (Item : Matrix);

private
end Day_14;
