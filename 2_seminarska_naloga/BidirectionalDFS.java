import java.io.FileNotFoundException;
import java.util.*;

public class BidirectionalDFS{
    private static int ROW;
    private static int COL;
    private static int stZaklad;
    private static boolean zaklad;
    private static final String[] listSmeriNeba = {"ENWS", "NEWS", "WENS", "EWNS", "NWES", "WNES", "SNEW", "NSEW", "ESNW", "SENW", "NESW", "ENSW", "EWSN", "WESN", "SEWN", "ESWN", "WSEN", "SWEN", "SWNE", "WSNE", "NSWE", "SNWE", "WNSE", "NWSE"};

    private static int cena = 0;
    private static int premik = 0;
    private static final StringBuilder sb = new StringBuilder();
    private static final StringBuilder sbb = new StringBuilder();

    private static final int[] coords = new int[2];

    private static final HashMap<Integer, Pair> trea = new HashMap<>();


    private static class Pair {
        public int first;
        public int second;

        public Pair(int first, int second) {
            this.first = first;
            this.second = second;
        }
    }

    private static Boolean isValid(int[][] grid, boolean[][] vis, int row, int col, boolean prvi) {
        if (row < 0 || col < 0 || row >= ROW || col >= COL)
            return false;

        if(grid[row][col] == -1)
            return false;

        if(grid[row][col] == -4 && prvi)
            return false;

        if(grid[row][col] == -2 && !prvi)
            return false;

        return !vis[row][col];
    }


    private static void DFSsearch(int row, int col, int backRow, int backCol, int[][] grid, boolean[][] vis,boolean[][] visBack, String smer) {
        Stack<Pair> st = new Stack<>();
        Stack<Pair> stBack = new Stack<>();

        st.push(new Pair(row, col));
        stBack.push(new Pair(backRow, backCol));

        while (!st.empty()) {
            Pair curr = st.pop();
            Pair currBack = stBack.pop();

            row = curr.first;
            col = curr.second;

            backRow = currBack.first;
            backCol = currBack.second;

            if((!isValid(grid,vis, row, col, true)) && (!isValid(grid,visBack, backRow, backCol, false))) {
                continue;
            }else if((!isValid(grid,vis, row, col, true)) && (isValid(grid,visBack, backRow, backCol, false))) {
                if(grid[backRow][backCol] == -3 && !trea.isEmpty()) {
                    for (Map.Entry<Integer, Pair> entry : trea.entrySet()) {
                        if (entry.getValue().first == backRow && entry.getValue().second == backCol) {
                            trea.remove(entry.getKey());
                            break;
                        }
                    }
                }
                if(trea.isEmpty())
                    zaklad = true;

                if (grid[backRow][backCol] != -2 && grid[backRow][backCol] != -4) premik++;
                if (grid[backRow][backCol] >= 0) {
                    cena += grid[backRow][backCol];
                }
                visBack[backRow][backCol] = true;
                int adjxB = 0, adjyB = 0;

                int x = 0,y = 0;
                boolean konec = false;
                for(int i= 0; i< vis.length;i++){
                    for(int j = 0; j<vis[0].length; j++){
                        if(vis[i][j] && visBack[i][j]) {
                            x = i;
                            y = j;
                            konec = true;
                            break;
                        }
                    }
                    if (konec)
                        break;
                }
                if(konec && zaklad) {
                    sbb.append("(").append(backRow).append(",").append(backCol).append(")");
                    coords[0] = x;
                    coords[1] = y;
                    System.out.printf("Srecanje: (%d,%d)\n", x,y);
                    cena -= grid[x][y];
                    break;
                }
                sbb.append("(").append(backRow).append(",").append(backCol).append(")").append(" -> ");

                char[] chars = smer.toCharArray();
                for (char ch: chars) {
                    if(ch == 'E'){
                        adjxB = backRow;
                        adjyB = backCol + 1;
                    }else if(ch == 'W'){
                        adjxB = backRow;
                        adjyB = backCol - 1;
                    }else if(ch == 'S'){
                        adjxB = backRow+1;
                        adjyB = backCol;
                    }else if(ch == 'N'){
                        adjxB = backRow - 1;
                        adjyB = backCol;
                    }
                    stBack.push(new Pair(adjxB, adjyB));
                }
            }else if ((isValid(grid,vis, row, col, true)) && (!isValid(grid,visBack, backRow, backCol,false))) {
                if (grid[row][col] == -3 && !trea.isEmpty()) {
                    for (Map.Entry<Integer, Pair> entry : trea.entrySet()) {
                        if (entry.getValue().first == row && entry.getValue().second == col) {
                            trea.remove(entry.getKey());
                            break;
                        }
                    }
                }
                if(trea.isEmpty())
                    zaklad = true;

                if (grid[row][col] != -2 && grid[row][col] != -4) premik++;
                if (grid[row][col] >= 0){
                    cena += grid[row][col];
                }
                vis[row][col] = true;

                int x = 0,y = 0;
                boolean konec = false;
                for(int i= 0; i< vis.length;i++){
                    for(int j = 0; j<vis[0].length; j++){
                        if(vis[i][j] && visBack[i][j]) {
                            x = i;
                            y = j;
                            konec = true;
                            break;
                        }
                    }
                    if (konec)
                        break;
                }
                if(konec && zaklad) {
                    sb.append("(").append(row).append(",").append(col).append(")");
                    coords[0] = x;
                    coords[1] = y;
                    System.out.printf("Srecanje: (%d,%d)\n", x,y);
                    cena -= grid[x][y];
                    break;
                }
                sb.append("(").append(row).append(",").append(col).append(")").append(" -> ");

                char[] chars = smer.toCharArray();
                int adjx = 0, adjy = 0;
                for (char ch: chars) {
                    if(ch == 'E'){
                        adjx = row;
                        adjy = col + 1;
                    }else if(ch == 'W'){
                        adjx = row;
                        adjy = col - 1;
                    }else if(ch == 'S'){
                        adjx = row+1;
                        adjy = col;
                    }else if(ch == 'N'){
                        adjx = row - 1;
                        adjy = col;
                    }
                    st.push(new Pair(adjx, adjy));
                }
            }else{
                if(grid[row][col] == -3 && !trea.isEmpty()) {
                    for (Map.Entry<Integer, Pair> entry : trea.entrySet()) {
                        if (entry.getValue().first == row && entry.getValue().second == col) {
                            trea.remove(entry.getKey());
                            break;
                        }
                    }
                }

                if(grid[backRow][backCol] == -3 && !trea.isEmpty()) {
                    for (Map.Entry<Integer, Pair> entry : trea.entrySet()) {
                        if (entry.getValue().first == backRow && entry.getValue().second == backCol) {
                            trea.remove(entry.getKey());
                            break;
                        }
                    }
                }

                if(trea.isEmpty())
                    zaklad = true;

                if (grid[backRow][backCol] != -2 && grid[backRow][backCol] != -4) premik++;
                if (grid[backRow][backCol] >= 0) cena += grid[backRow][backCol];

                if (grid[row][col] != -2 && grid[row][col] != -4) premik++;
                if(grid[row][col] >= 0) cena += grid[row][col];

                vis[row][col] = true;
                visBack[backRow][backCol] = true;

                int x = 0,y = 0;
                boolean konec = false;
                for(int i= 0; i< vis.length;i++){
                    for(int j = 0; j<vis[0].length; j++){
                        if(vis[i][j] && visBack[i][j]) {
                            x = i;
                            y = j;
                            konec = true;
                            break;
                        }
                    }
                    if (konec)
                        break;
                }
                if(konec && zaklad) {
                    sb.append("(").append(row).append(",").append(col).append(")");
                    sbb.append("(").append(backRow).append(",").append(backCol).append(")");
                    coords[0] = x;
                    coords[1] = y;
                    System.out.printf("Srecanje: (%d,%d)\n", x,y);
                    cena -= grid[x][y];
                    break;
                }
                sb.append("(").append(row).append(",").append(col).append(")").append(" -> ");
                sbb.append("(").append(backRow).append(",").append(backCol).append(")").append(" -> ");

                int adjxB = 0, adjyB = 0;
                char[] chars = smer.toCharArray();
                for (char ch: chars) {
                    if(ch == 'E'){
                        adjxB = backRow;
                        adjyB = backCol + 1;
                    }else if(ch == 'W'){
                        adjxB = backRow;
                        adjyB = backCol - 1;
                    }else if(ch == 'S'){
                        adjxB = backRow+1;
                        adjyB = backCol;
                    }else if(ch == 'N'){
                        adjxB = backRow - 1;
                        adjyB = backCol;
                    }
                    stBack.push(new Pair(adjxB, adjyB));
                }

                int adjx = 0, adjy = 0;
                for (char ch: chars) {
                    if(ch == 'E'){
                        adjx = row;
                        adjy = col + 1;
                    }else if(ch == 'W'){
                        adjx = row;
                        adjy = col - 1;
                    }else if(ch == 'S'){
                        adjx = row+1;
                        adjy = col;
                    }else if(ch == 'N'){
                        adjx = row - 1;
                        adjy = col;
                    }
                    st.push(new Pair(adjx, adjy));
                }
            }
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        int[][] grid = DrawLabyrinth.readFile("D:\\FRI_Ljubljana\\GitHub\\UmetnaInteligenca\\Seminarska2\\src\\labyrinths\\labyrinth_4.txt");

        int stVozlisc = 0;
        boolean najden = false;
        int[] start= new int[2];
        int[] end= new int[2];
        for(int i = 0; i< grid.length; i++){
            for(int j = 0; j<grid[0].length; j++){
                if(grid[i][j] == -2 && !najden){
                    start[0] = i;
                    start[1] = j;
                    stVozlisc++;
                    najden = true;
                }else if(grid[i][j] == -3){
                    stZaklad++;
                    trea.put(stZaklad, new Pair(i,j));
                    stVozlisc++;
                }else if(grid[i][j] == -4 || grid[i][j] >= 0){
                    stVozlisc++;
                    if(grid[i][j] == -4){
                        end[0] = i;
                        end[1] = j;
                    }
                }
            }
        }
        int stObiskanih  = 0;
        ROW = grid.length;
        COL = grid[0].length;
        boolean[][] vis;
        boolean[][] visBack;
        Random random = new Random();
        int value = random.nextInt(listSmeriNeba.length-1);
        String smer = listSmeriNeba[value];

        smer = "ENWS";

        vis = new boolean[ROW][COL];
        visBack = new boolean[ROW][COL];
        DFSsearch(start[0], start[1],end[0], end[1], grid, vis,visBack, smer);
        for (boolean[] vi : vis) {
            for (boolean b : vi) {
                if (b)
                    stObiskanih++;
            }
        }
        StringBuilder smerObr =new StringBuilder(smer);
        smerObr.reverse();
        System.out.println("Smer: " + smerObr);
        System.out.println("Cena: " + cena);
        System.out.println("Premiki: " + premik);
        System.out.println("Stevilo vozlisc: " + stVozlisc);
        System.out.println("stevilo obiskanih: " + stObiskanih);
        System.out.println();
        System.out.println("Pot od začetka proti koncu:");
        System.out.println(sb);
        System.out.println();
        System.out.println("Pot od konca proti začetku:");
        System.out.println(sbb);

        boolean[][] izris = new boolean[ROW][COL];
        for(int i = 0; i< vis.length; i++){
            for(int j = 0; j<vis[0].length; j++) {
                if(vis[i][j] || visBack[i][j])
                    izris[i][j] = true;
            }
        }
        DrawLabyrinth.drawOutPath(grid, izris,coords);
        //DrawLabyrinth.drawOutPathDFS(grid,izris,endPath);
    }
}