load tsp.dat
x = tsp(:,1);
y = tsp(:,2);
figure(1);
plot(x, y, 'o');
%saveas(1, "tsp.png");
title("Original set");

load tsp_res.dat
x1 = tsp_res(:,1);
y1 = tsp_res(:,2);
figure(2);
plot(x1, y1, '-o');
%saveas(2, "tsp_res.png");
title("Shortest path");

load tsp_res_r.dat
x2 = tsp_res_r(:,1);
y2 = tsp_res_r(:,2);
figure(3);
plot(x2, y2, '-o');
%saveas(3, "tsp_res_r.png");
title("Shortest path (reversed)");

load tsp_res_p.dat
x3 = tsp_res_p(:,1);
y3 = tsp_res_p(:,2);
figure(4);
plot(x3, y3, '-o');
%saveas(4, "tsp_res_p.png");
title("Shortest path (parallel)");

load tsp_res_pr.dat
x4 = tsp_res_pr(:,1);
y4 = tsp_res_pr(:,2);
figure(5);
plot(x4, y4, '-o');
%saveas(5, "tsp_res_pr.png");
title("Shortest path (parallel reversed)");

pause()

