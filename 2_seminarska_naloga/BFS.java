import java.util.*;
 
class BFS{
     
    private static int ROW;
    private static int COL;
    private static int stZakladov = 0;
    private static boolean najden_zaklad = false;
    private static boolean[][] allVisited; 
    private static HashMap<String, Integer> PathValue = new HashMap<>();
    private static HashMap<String, Integer> PathMoves = new HashMap<>();
    private static HashMap<String, boolean[][]> PathVisited = new HashMap<>();
    private static ArrayList<int[]> obiskani_zakladi = new ArrayList<>();
    static int dRow[] = { -1, 0, 1, 0 };
    static int dCol[] = { 0, 1, 0, -1 };


    private static class pair {
        public int first;
        public int second;

        public pair(int first, int second) {
            this.first = first;
            this.second = second;
        }
    }

    private static Boolean isValid(int[][] grid, boolean[][] vis, int row, int col) {
        if (row < 0 || col < 0 || row >= ROW || col >= COL)
            return false;

        if(grid[row][col] == -1)
            return false;

        return !vis[row][col];
    }


    private static void BFSsearch(int row, int col, int[][] grid, boolean[][] vis) {
        int cena = 0;
        int premik = 0;
        int id = 1;
        najden_zaklad = false;
        StringBuilder sb = new StringBuilder();

        Queue<pair> queue = new LinkedList<>();
        queue.add(new pair(row, col));
        vis[row][col] = true;

        while (!queue.isEmpty()) {

            pair curr = queue.remove();;

            row = curr.first;
            col = curr.second;
            
            if(grid[row][col] >= 0 || grid[row][col] == -2){
                cena += grid[row][col];
                vis[row][col] = true;
                allVisited[row][col] = true;
            }

            int[] a = {row,col};
            if(grid[row][col] == -3 && !najden_zaklad){
                boolean notIn = true;
                for(int[] arr : obiskani_zakladi){
                    if(arr[0] == a[0] && arr[1] == a[1]){
                        notIn = false;
                        break;
                    }
                }
                if(notIn){
                    System.out.println("No. " + id + ": Position: [" + row + ", " + col + "]");
                    obiskani_zakladi.add(a);
                    vis = new boolean[grid.length][grid[0].length];
                    id++;
                    
                    if(obiskani_zakladi.size() == stZakladov){
                        System.out.println("Najdeni so vsi zakladi");
                        najden_zaklad = true;
                    }
                }
            }

            premik++;

            if(najden_zaklad && grid[row][col] == -4){
                System.out.println("No. " + id + ": Position: [" + row + ", " + col + "] CILJ");
                sb.append(row).append(":").append(col);
                PathValue.put(String.valueOf(sb), cena);
                PathMoves.put(String.valueOf(sb), premik);
                PathVisited.put(String.valueOf(sb), vis);
                queue.clear();
                return;
            }

            sb.append(row).append(":").append(col).append(" -> ");

            for(int i = 0; i < 4; i++)
            {
                int adjx = row + dRow[i];
                int adjy = col + dCol[i];
     
                if (isValid(grid, vis, adjx, adjy)) {
                    queue.add(new pair(adjx, adjy));
                    vis[adjx][adjy] = true;
                    allVisited[adjx][adjy] = true;
                }
            }

            if(queue.isEmpty()){
                System.out.println("Prazn QUEUE");
            }
        }
        System.out.println(sb.toString());
    }

    public static void setup(int[][] grid) {
        boolean najden = false;
        int[] start= new int[2];
        for(int i = 0; i< grid.length; i++){
            for(int j = 0; j<grid[0].length; j++){
                if(grid[i][j] == -2 && !najden){
                    start[0] = i;
                    start[1] = j;
                    najden = true;
                }else if(grid[i][j] == -3){
                    stZakladov++;
                }
            }
        }
        ROW = grid.length;
        COL = grid[0].length;
        boolean[][] vis;
        vis = new boolean[ROW][COL];
        allVisited = new boolean[ROW][COL];
        BFSsearch(start[0], start[1], grid, vis);
        Naloga.drawOutPath(grid, allVisited);
    }

}
