import java.awt.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.ArrayList;
import java.util.Arrays;

public class Naloga {
    public static void main(String[] args) throws FileNotFoundException{
        String[] algoritmi = {"BFS", "DFS"};
        String[] labirinti = {"labyrinth_1", "labyrinth_2", "labyrinth_3", "labyrinth_4", "labyrinth_5", "labyrinth_6", "labyrinth_7", "labyrinth_8", "labyrinth_9"};
        Scanner sc  = new Scanner(System.in);
        do{
            System.out.println("Izberite preiskovalni algoritem:");
            for(int i=0; i < algoritmi.length; i++){
                System.out.println("(" + (i+1) + ") " + algoritmi[i]);
            }
            System.out.print("Vnesite stevilko algoritma katerega zelite pognati: ");
            int stAlg = sc.nextInt();
            System.out.println();
            System.out.println("Izbrali ste algoritem: " + algoritmi[stAlg - 1]);
            System.out.println();


            System.out.println("Izberite labirint nad katerim boste pognali algoritem:");
            for(int i=0; i < labirinti.length; i++){
                System.out.println("(" + (i+1) + ") " + labirinti[i]);
            }
            System.out.print("Vnesite stevilko algoritma katerega zelite pognati: ");
            int stlab = sc.nextInt();

            System.out.println();
            System.out.println("Izbrali ste labirint: " + labirinti[stlab - 1]);
            System.out.println();

            String file = labirinti[stlab - 1] + ".txt";
            int[][] grid = readFile(file);
            /*boolean[][] visited = new boolean[grid.length][grid[0].length];
            for(int i=0; i < visited.length; i++){
                Arrays.fill(visited[i], Boolean.FALSE);
            }*/
            

            switch(stAlg - 1){
                case 0:
                    //drawOut(grid);
                    BFS.setup(grid);
                    
                    break;
                case 1:
                    DFS.setup(grid);
                    break;
                default: System.out.println("Napacna vnesena steilka za izbiro algoritma: " + stAlg);

            }


            System.out.println("Ce zelite ponovno pognati pritisnite ( C )");
        } while(sc.next().toUpperCase().equals("C"));
        sc.close();
    }

    public static int[][] readFile(String fileName) throws FileNotFoundException{
        Scanner sc1 = new Scanner(new File(fileName));
        ArrayList<ArrayList<Integer>> arrList = new ArrayList<>();
        while(sc1.hasNextLine()){
            String[] poljaString = sc1.nextLine().split(",");
            ArrayList<Integer> temp = new ArrayList<>();
            for(int i=0; i < poljaString.length; i++){
                temp.add(Integer.parseInt(poljaString[i]));
            }
            arrList.add(temp);
        }
        int[][] intArray = arrList.stream().map(  u  ->  u.stream().mapToInt(i->i).toArray()  ).toArray(int[][]::new);
        sc1.close();
        return intArray;
    }

    public static void drawOut(int[][] field) {
        StdDraw.setCanvasSize(700, 700 );
        StdDraw.setFont(new Font(null, Font.PLAIN, (int) (16 - (double) (field.length / 8))));
        for (int i = 0; i < field.length; i++) {
            for (int j = 0; j < field[0].length; j++) {
                switch (field[i][j]) {
                    case -1:
                        StdDraw.setPenColor(Color.BLACK);
                        break;
                    case -2:
                        StdDraw.setPenColor(Color.RED);
                        break;
                    case -3:
                        StdDraw.setPenColor(Color.YELLOW);
                        break;
                    case -4:
                        StdDraw.setPenColor(Color.GREEN);
                        break;
                    default:
                        StdDraw.setPenColor(Color.WHITE);
                        break;
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
    
    public static void drawOutPath(int[][] field, boolean[][] visited) {
        StdDraw.setCanvasSize(700, 700 );
        StdDraw.setFont(new Font(null, Font.PLAIN, (int) (16 - (double) (field.length / 8))));
        for (int i = 0; i < field.length; i++) {
            for (int j = 0; j < field[0].length; j++) {
                switch (field[i][j]) {
                    case -1:
                        StdDraw.setPenColor(Color.BLACK);
                        break;
                    case -2:
                        StdDraw.setPenColor(Color.RED);
                        break;
                    case -3:
                        StdDraw.setPenColor(Color.YELLOW);
                        break;
                    case -4:
                        StdDraw.setPenColor(Color.GREEN);
                        break;
                    default:
                        StdDraw.setPenColor(Color.WHITE);
                        break;
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
