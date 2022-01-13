import java.io.FileNotFoundException;
import java.util.*;

public class TreasureDFS {
    private static int ROW;
    private static int COL;
    private static int stZaklad;
    private static final String[] listSmeriNeba = {"ENWS", "NEWS", "WENS", "EWNS", "NWES", "WNES", "SNEW", "NSEW", "ESNW", "SENW", "NESW", "ENSW", "EWSN", "WESN", "SEWN", "ESWN", "WSEN", "SWEN", "SWNE", "WSNE", "NSWE", "SNWE", "WNSE", "NWSE"};

    private static boolean[][] obiskan;

    private static boolean[][] endPath;

    private static int premik;

    private static final StringBuilder sb = new StringBuilder();

    private static ArrayList<Obiskan> pot;


    private static class Pair {
        public int first;
        public int second;

        public Pair(int first, int second) {
            this.first = first;
            this.second = second;
        }
    }

    private static class Obiskan {
        private final int x;
        private final int y;
        private boolean pregledan;
        private final ArrayList<Pair> arr;

        public Obiskan(int x, int y, boolean pregledan, ArrayList<Pair> arr ) {
            this.x = x;
            this.y = y;
            this.pregledan = pregledan;
            this.arr = arr;
        }
        public int getX() {
            return x;
        }

        public int getY() {
            return y;
        }

        public boolean isPregledan() {
            return pregledan;
        }

        public void setPregledan(boolean pregledan) {
            this.pregledan = pregledan;
        }

        public ArrayList<Pair> getArr() {
            return arr;
        }

    }

    private static Boolean isValid(int[][] grid, boolean[][] vis, int row, int col) {
        if (row < 0 || col < 0 || row >= ROW || col >= COL)
            return false;

        if(grid[row][col] == -1)
            return false;

        return !vis[row][col];
    }


    private static void DFSsearch(int row, int col, int endX, int endY, int[][] grid, boolean[][] vis, String smer) {
        pot = new ArrayList<>();

        Stack<Pair> st = new Stack<>();
        st.push(new Pair(row, col));

        while (!st.empty()) {
            Pair curr = st.pop();

            row = curr.first;
            col = curr.second;

            if (!isValid(grid,vis, row, col))
                continue;

            if(grid[row][col] != -2) premik++;

            if(row == endX && col == endY) {
                sb.append("(").append(row).append(",").append(col).append(")");


                int a = row;
                int b = col;
                boolean viden;
                for (int i= 0; i< pot.size(); i++){
                    viden = false;
                    for (Obiskan obiskan : pot) {
                        for (int k = 0; k < 4; k++) {
                            if (obiskan.getArr().get(k).first == a && obiskan.getArr().get(k).second == b && !obiskan.isPregledan()) {
                                System.out.print("(" + a + "," + b + ")" + " -> ");
                                endPath[a][b] = true;
                                obiskan.setPregledan(true);
                                a = obiskan.getX();
                                b = obiskan.getY();
                                viden = true;
                            }
                        }
                        if (viden)
                            break;
                    }
                }
                st.clear();
                break;
            }

            vis[row][col] = true;
            obiskan[row][col] = true;


            sb.append("(").append(row).append(",").append(col).append(")").append(" -> ");

            ArrayList<Pair> arr = new ArrayList<>();
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
                arr.add(new Pair(adjx, adjy));
                st.push(new Pair(adjx, adjy));
            }
            pot.add(new Obiskan(row,col,false, arr));
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        int[][] grid = DrawLabyrinth.readFile("D:\\FRI_Ljubljana\\GitHub\\UmetnaInteligenca\\Seminarska2\\src\\labyrinths\\labyrinth_8.txt");

        int stVozlisc = 0;

        boolean najden = false;
        int[] start= new int[2];
        int[] end = new int[2];
        HashMap<Integer, Coordinates> treasure = new HashMap<>();
        for(int i = 0; i< grid.length; i++){
            for(int j = 0; j<grid[0].length; j++){
                if(grid[i][j] == -2 && !najden){
                    start[0] = i;
                    start[1] = j;
                    stVozlisc++;
                    najden = true;
                }else if(grid[i][j] == -3){
                    Coordinates c = new Coordinates(i,j);
                    stZaklad++;
                    treasure.put(stZaklad, c);
                    stVozlisc++;
                }else if(grid[i][j] == -4){
                    end[0] = i;
                    end[1] = j;
                    stVozlisc++;
                }else if(grid[i][j] >= 0){
                    stVozlisc++;
                }
            }
        }

        HashMap<Integer, Coordinates> treasureEnd = new HashMap<>();
        //Random treasures
        Random random = new Random();
        ArrayList<Integer> arr = new ArrayList<>();
        for(int i = 1; i <= treasure.size(); i++) {
            int value;
            do {
                value = random.nextInt(treasure.size());
                value += 1;
            }while(arr.contains(value));
            arr.add(value);
            treasureEnd.put(i, treasure.get(value));
        }
        ROW = grid.length;
        COL = grid[0].length;
        pot = new ArrayList<>();

        boolean[][] vis;
        String smerNeba = "ESWN";
        obiskan = new boolean[ROW][COL];
        boolean[][] visited = new boolean[ROW][COL];

        System.out.println("DFS");
        System.out.println("(Row,Col) -> (Row,Col) -> ...");
        System.out.println();


        for (Map.Entry<Integer, Coordinates> entry : treasureEnd.entrySet()) {
            vis = new boolean[ROW][COL];
            endPath = new boolean[ROW][COL];
            DFSsearch(start[0], start[1], entry.getValue().x, entry.getValue().y,grid, vis,smerNeba);
            start[0] =  entry.getValue().x;
            start[1] = entry.getValue().y;
            for(int i= 0; i< endPath.length; i++){
                for(int j= 0; j< endPath[0].length; j++){
                    if(endPath[i][j])
                        visited[i][j] = true;
                }
            }
        }
        int cenaPoti = 0;
        int cenaObiskanih = 0;
        int stObiskanih = 0;
        int stvozliscPoti = 0;
        vis = new boolean[ROW][COL];
        endPath = new boolean[ROW][COL];

        DFSsearch(start[0], start[1], end[0], end[1],grid, vis,smerNeba);
        for(int i= 0; i< endPath.length; i++){
            for(int j= 0; j< endPath[0].length; j++){
                if(endPath[i][j])
                    visited[i][j] = true;
            }
        }
        for(int i = 0; i< visited.length; i++){
            for(int j = 0; j< visited[i].length; j++) {
                if(visited[i][j]) {
                    if(grid[i][j] >= 0)
                        cenaPoti += grid[i][j];
                    stvozliscPoti++;
                }
                if(obiskan[i][j]) {
                    if(grid[i][j] >= 0)
                        cenaObiskanih += grid[i][j];
                    stObiskanih++;
                }
            }
        }
        System.out.println();
        StringBuilder smerObr =new StringBuilder(smerNeba);
        smerObr.reverse();
        System.out.println("Smer: " + smerObr);
        System.out.println("stevilo vozlisc: " + stVozlisc);
        System.out.println("stevilo vozlisc poti: "+ stvozliscPoti);
        System.out.println("stevilo obiskanih vozlisc: "+ stObiskanih);
        System.out.println("cena Obisanih: " + cenaObiskanih);
        System.out.println("cena poti: " + cenaPoti);
        System.out.println("premik: "+ premik);
        System.out.println();
        System.out.println("vsa obiskana pot:\n" +sb);
        DrawLabyrinth.drawOutPathBacktrack(grid, obiskan, visited);
    }

}