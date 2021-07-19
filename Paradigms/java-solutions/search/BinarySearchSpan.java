
//  Let: arr is Sorted = forall i, j in [0; args.length) : i < j --> arr[i] >= arr[j]
public class BinarySearchSpan {

    //Pred: 0 < args.length < (Integer.MAX_VALUE - 7) &&
    //      forall i, j in [1; args.length) : i < j --> arr[i] >= arr[j]
    //      forall i in [0; args.length) : (args[i] is Integer) && arg
    //Post: Answ == (i, len) : ( i == args.length - 1, len == 0  if (args.length == 1 || forall i in [1; args.length) : args[i] > args[0]) ||
    //      i == (j - 1), len == (k - i) : (forall j in [1, args.length) : (args[j] <= args[0] && args[j - 1] > args[0]),
    //      (args[k] < args[i] && args[k - 1] == args[i]))

    public static void main(final String[] args) {
        final int x = Integer.parseInt(args[0]);
        final int[] a = new int[args.length - 1];

        int i = 0;
        //i == 0 && args.length > 0
        //Inv: i' = i + 1
        while (i < args.length - 1) {
            a[i] = Integer.parseInt(args[i + 1]);
            i++;
            //i' = i + 1

        }
        //Pred: a is Sorted
        //Post: a.length <= MAX_VALUE - 8
        if (a.length > 0) {
            //a.length > 0
            final int l1 = iterBinSearch(a, x);
            final int r1 = recBinSearch(0, a.length, a, x);
            System.out.println(l1 + " " + (r1 - l1));
        } else {
            //a.length == 0
            System.out.println("0 0");
        }
    }

    //Pred: 0 <= l < r <= a.length
    //      a is Sorted
    //Post: Answ == i : ((i == a.length  && (forall i in [0; a.length) : a[i] > x || a[a.length - 1] == x)) ||
    //      (i = j : ( forall j in [1, a.length) : (a[j] < x && a[j - 1] == x)))
    static int recBinSearch(final int l, final int r, final int[] a, final int x) {
		final int cur = l + (r - l) / 2;
        //cur >= 0

        // a is Sorted
        if (r == l) {
            //Pred: l >= 0 && r >= 0 && r == l
            return r;
            //Post: Answ == r
        } else if (a[cur] >= x) {
            //Pred: cur >= 0 && (r - l) != 0 && a[cur] >= x
            return recBinSearch(cur + 1, r, a, x);
            //Post: l' = cur + 1 && r' = r --> l' >= 0 && r' >= 0 --> l' > l --> l' is closer to r'
        } else {
            //Pred: cur >= 0 && (r - l) != 0 && a[cur] < x
            return recBinSearch(l, cur, a, x);
            //Post: l' = l && r' = cur --> l' >= 0 && r' >= 0 --> r' < r --> r' is closer to l'
        }
    }

    //Pred: a is Sorted && a.lenght > 0
    //Post: Answ == i : (i == a.length && (forall i in [0; a.length) : a[i] > x || a.length == 1 && a[i] <= x) ||
    //      i == (j - 1) : (forall j in [1, a.length) : (a[j] <= x && a[j - 1] > x)
    static int iterBinSearch(final int[] a, final int x) {
        if (a.length == 0) {
            //Pred: a.length == 0 --> there are nothing to find
            return 0;
            //Post: Answ == 0
        } else {
            //a.lenght > 0
        }
        //a.lenght > 0
        int l = 0;
        int r = a.length;
        //a.lenght > 0 --> r > 0 && l == 0 && l < r
        int cur = 0;
        //a.lenght > 0 --> r > 0 && l == 0 && cur == 0 && l < r

        //Inv: l' < r' && r' >= 0 && l' >= 0
        while (r - l != 0) {
            cur = l + (r - l) / 2;
            //l >= 0 && r > 0 --> cur >= 0 && l < r
            //cur >= 0
            if (a[cur] > x) {
                //Pred: l <= cur <= r && a[cur] > x
                l = cur + 1;
                //Post: l' == cur + 1 && r' = r &&
                //l <= cur <= r -->(r' - l') < (r - l) (-->cycle gonna end)
            } else {
                //Pred: l <= cur <= r a[cur] <= x
                r = cur;
                //Post: l' == l && r' = cur &&
                //l <= cur <= r -->(r' - l') < (r - l) (-->cycle gonna end)
            }
            //Post: l <= cur <= r -->(r' - l') < (r - l) (-->cycle gonna end)
        }
        //Post: r == l == (a.length : forall i : a[i] > x || i : (a[i] <= x && a[i - 1] > x)
        return r;
        //Answ == r
    }
}
