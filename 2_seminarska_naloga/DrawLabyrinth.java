import edu.princeton.cs.introcs.StdDraw;

import java.awt.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.ArrayList;

public class DrawLabyrinth {
    public static void main(String[] args) throws FileNotFoundException{
        int[][] labirint;
        String file = "D:\\FRI_Ljubljana\\GitHub\\UmetnaInteligenca\\Seminarska2\\src\\labyrinths\\labyrinth_1.txt";
        readFile(file);
        //drawOut(labirint);
    }

    public static int[][] readFile(String fileName) throws FileNotFoundException{
        Scanner sc = new Scanner(new File(fileName));
        ArrayList<ArrayList<Integer>> arrList = new ArrayList<>();
        while(sc.hasNextLine()){
            String[] poljaString = sc.nextLine().split(",");
            ArrayList<Integer> temp = new ArrayList<>();
            for (String s : poljaString) {
                temp.add(Integer.parseInt(s));
            }
            arrList.add(temp);
        }
        return arrList.stream().map(u  ->  u.stream().mapToInt(i->i).toArray()).toArray(int[][]::new);
    }

    public static void drawOut(int[][] field) {
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
                StdDraw.filledSquare((double) j / (field.length - 1), 1- (double) i / (field.length -1),
                        (double) 1 / (field.length - 1) * 0.5);
                if (field[i][j] >= 0) {
                    StdDraw.setPenColor(Color.BLACK);
                    StdDraw.text((double) j / (field.length - 1), 1 - (double) i / (field.length - 1),
                            String.valueOf(field[i][j]));
                }
            }
        }
    }

    public static void drawOutPath(int[][] field, boolean[][] visited,int[] coords) {
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
                if(coords != null)
                    if(i == coords[0] && j == coords[1])
                        StdDraw.setPenColor(Color.DARK_GRAY);

                StdDraw.filledSquare((double) j / (field.length - 1), 1- (double) i / (field.length -1), (double) 1 / (field.length - 1) * 0.5);
                StdDraw.setPenColor(Color.BLACK);
                StdDraw.text((double) j / (field.length - 1), 1 - (double) i / (field.length - 1), String.valueOf(field[i][j]));
            }
        }
    }
    public static void drawOutPathBacktrack(int[][] field, boolean[][] visited,boolean[][] path) {
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
                }else if(field[i][j] >= 0 && visited[i][j] && path[i][j]){
                    StdDraw.setPenColor(Color.GRAY);
                } else if(field[i][j] >= 0 && visited[i][j]){
                    StdDraw.setPenColor(Color.BLUE);
                }

                StdDraw.filledSquare((double) j / (field.length - 1), 1- (double) i / (field.length -1), (double) 1 / (field.length - 1) * 0.5);
                StdDraw.setPenColor(Color.BLACK);
                StdDraw.text((double) j / (field.length - 1), 1 - (double) i / (field.length - 1), String.valueOf(field[i][j]));
            }
        }
    }
}
