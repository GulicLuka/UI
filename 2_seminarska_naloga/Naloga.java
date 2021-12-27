import java.awt.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.ArrayList;

public class Naloga {
    public static void main(String[] args) throws FileNotFoundException{
        int[][] labirint;
        if(args.length > 0){
            labirint= readFile(args[0]); 
            drawOut(labirint);
        }
        else
            System.out.println("File missing");
    }

    public static int[][] readFile(String fileName) throws FileNotFoundException{
        Scanner sc = new Scanner(new File(fileName));
        ArrayList<ArrayList<Integer>> arrList = new ArrayList<>();
        while(sc.hasNextLine()){
            String[] poljaString = sc.nextLine().split(",");
            ArrayList<Integer> temp = new ArrayList<>();
            for(int i=0; i < poljaString.length; i++){
                temp.add(Integer.parseInt(poljaString[i]));
            }
            arrList.add(temp);
        }
        int[][] intArray = arrList.stream().map(  u  ->  u.stream().mapToInt(i->i).toArray()  ).toArray(int[][]::new);
        return intArray;
    }

    public static void drawOutPath(int[][] field, boolean[][] visited) {
        StdDraw.setCanvasSize(700, 700 );
        StdDraw.setFont(new Font(null, Font.PLAIN, (int) (16 - (double) (field.length / 8))));
        for (int i = 0; i < field.length; i++) {
            for (int j = 0; j < field[0].length; j++) {
                switch (field[i][j]) {
                    case -1 -> StdDraw.setPenColor(Color.BLACK);
                    case -2 -> StdDraw.setPenColor(Color.RED);
                    case -3 -> StdDraw.setPenColor(Color.YELLOW);
                    case -4 -> StdDraw.setPenColor(Color.GREEN);
                    default -> StdDraw.setPenColor(Color.WHITE);
                }
                if(field[i][j] == -3 && visited[i][j]) {
                    StdDraw.setPenColor(Color.PINK);
                }else if(field[i][j] >= 0 && visited[i][j]){
                    StdDraw.setPenColor(Color.BLUE);
                }
                StdDraw.filledSquare((double) j / (field.length - 1), 1- (double) i / (field.length -1), (double) 1 / (field.length - 1) * 0.5);
                StdDraw.setPenColor(Color.BLACK);
                StdDraw.text((double) j / (field.length - 1), 1 - (double) i / (field.length - 1), String.valueOf(field[i][j]));
            }
        }
    }
}
