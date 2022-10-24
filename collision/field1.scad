$fs = 120;

$vpt = [0,1080,480];
$vpr = [-34,0,0];
$vpf = 45;

foul_line = 6;
plain_color = [1,1,1,0.8];
out_color = [1,0.75,0.75,0.8];
eat_color = [0.5,0.75,1,0.8];

// home, first, second, third, rubber
color([1,1,1]) scale([1,-1,-1]) rotate([90,0,0]) linear_extrude(1) {
    polygon([[-30,30*2+300],[30,30*2+300],[30,30+300],[0,300],[-30,30+300]]);
    polygon([[552-36*2,852],[552-36,852+36],[552,852],[552-36,852-36]]);
    polygon([[0,1404+36],[36,1404],[0,1404-36],[-36,1404]]);
    polygon([[-552+36*2,852],[-552+36,852+36],[-552,852],[-552+36,852-36]]);
    polygon([[-19,852],[19,852],[19,9+852],[-19,9+852]]);
}

// foul lines
color([1,1,1,0.5]) scale([1,-1,-1]) rotate([90,0,0]) linear_extrude(1) {
    polygon([[0,300],[-5000,300+5000],[-5000,300+5000+foul_line],[0,300+foul_line]]);
    polygon([[0,300],[5000,300+5000],[5000,300+5000+foul_line],[0,300+foul_line]]);
}

scale([0.1,0.1,-0.1]) translate([-23200,0,0]) {
    color(out_color) polyhedron([[12390,0,8700],[12400,0,8700],[12390,1350,8700],[12400,1350,8700]], [[0,1,3,2]]);
    color(out_color) polyhedron([[12390,0,8700],[12390,0,21700],[12390,1350,8700],[12390,1350,21700]], [[0,1,3,2]]);
    color(out_color) polyhedron([[12400,0,8700],[12400,0,21700],[12400,1350,8700],[12400,1350,21700]], [[0,1,3,2]]);
    color(out_color) polyhedron([[12390,0,21700],[12400,0,21700],[12390,1350,21700],[12400,1350,21700]], [[0,1,3,2]]);
    color(out_color) polyhedron([[12390,1350,8700],[12390,1350,21700],[12400,1350,8700],[12400,1350,21700]], [[0,1,3,2]]);
    color(out_color) polyhedron([[12400,0,21680],[22300,0,31450],[12400,1350,21680],[22300,1350,31450]], [[0,1,3,2]]);
    color(out_color) polyhedron([[12400,0,21680],[12400,0,21690],[12400,1350,21680],[12400,1350,21690]], [[0,1,3,2]]);
    color(out_color) polyhedron([[22300,0,31450],[22300,0,31460],[22300,1350,31450],[22300,1350,31460]], [[0,1,3,2]]);
    color(out_color) polyhedron([[12400,0,21690],[22300,0,31460],[12400,1350,21690],[22300,1350,31460]], [[0,1,3,2]]);
    color(out_color) polyhedron([[12400,1350,21680],[12400,1350,21690],[22300,1350,31450],[22300,1350,31460]], [[0,1,3,2]]);
    color(out_color) polyhedron([[14250,0,39418],[40000,0,13931],[14250,4150,39418],[40000,4150,13931]], [[0,1,3,2]]);
    color(out_color) polyhedron([[14250,0,39418],[14250,0,39428],[14250,4150,39418],[14250,4150,39428]], [[0,1,3,2]]);
    color(out_color) polyhedron([[40000,0,13931],[40000,0,13941],[40000,4150,13931],[40000,4150,13941]], [[0,1,3,2]]);
    color(out_color) polyhedron([[14250,0,39428],[40000,0,13941],[14250,4150,39428],[40000,4150,13941]], [[0,1,3,2]]);
    color(out_color) polyhedron([[14250,4150,39418],[14250,4150,39428],[40000,4150,13931],[40000,4150,13941]], [[0,1,3,2]]);
    color(out_color) polyhedron([[33700,0,25800],[42200,0,16660],[33700,1700,25800],[42200,1700,16660]], [[0,1,3,2]]);
    color(out_color) polyhedron([[33700,0,25800],[35700,0,28600],[33700,1700,25800],[35700,1700,28600]], [[0,1,3,2]]);
    color(out_color) polyhedron([[42200,0,16660],[42200,0,18610],[42200,1700,16660],[42200,1700,18610]], [[0,1,3,2]]);
    color(out_color) polyhedron([[35700,0,28600],[42200,0,18610],[35700,1700,28600],[42200,1700,18610]], [[0,1,3,2]]);
    color(out_color) polyhedron([[33700,1700,25800],[35700,1700,28600],[42200,1700,16660],[42200,1700,18610]], [[0,1,3,2]]);
    color(out_color) polyhedron([[24730,0,1930],[24731,0,1930],[24730,10,1930],[24731,10,1930]], [[0,1,3,2]]);
    color(out_color) polyhedron([[24730,0,1930],[36050,0,14510],[24730,10,1930],[36050,10,14510]], [[0,1,3,2]]);
    color(out_color) polyhedron([[24731,0,1930],[36051,0,14510],[24731,10,1930],[36051,10,14510]], [[0,1,3,2]]);
    color(out_color) polyhedron([[36050,0,14510],[36051,0,14510],[36050,10,14510],[36051,10,14510]], [[0,1,3,2]]);
    color(out_color) polyhedron([[24730,10,1930],[36050,10,14510],[24731,10,1930],[36051,10,14510]], [[0,1,3,2]]);
    color(out_color) polyhedron([[13960,0,4590],[18500,0,4590],[13960,5,4590],[18500,5,4590]], [[0,1,3,2]]);
    color(out_color) polyhedron([[13960,0,4590],[13960,0,4690],[13960,5,4590],[13960,5,4690]], [[0,1,3,2]]);
    color(out_color) polyhedron([[18500,0,4590],[18500,0,4690],[18500,5,4590],[18500,5,4690]], [[0,1,3,2]]);
    color(out_color) polyhedron([[13960,0,4690],[18500,0,4690],[13960,5,4690],[18500,5,4690]], [[0,1,3,2]]);
    color(out_color) polyhedron([[13960,5,4590],[13960,5,4690],[18500,5,4590],[18500,5,4690]], [[0,1,3,2]]);
    color(out_color) polyhedron([[19090,0,1880],[19190,0,1880],[19090,5,1880],[19190,5,1880]], [[0,1,3,2]]);
    color(out_color) polyhedron([[19090,0,1880],[18500,0,4560],[19090,5,1880],[18500,5,4560]], [[0,1,3,2]]);
    color(out_color) polyhedron([[19190,0,1880],[18600,0,4560],[19190,5,1880],[18600,5,4560]], [[0,1,3,2]]);
    color(out_color) polyhedron([[18500,0,4560],[18600,0,4560],[18500,5,4560],[18600,5,4560]], [[0,1,3,2]]);
    color(out_color) polyhedron([[19090,5,1880],[18500,5,4560],[19190,5,1880],[18600,5,4560]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
    color(plain_color) polyhedron([[0,0,0],[0,0,0],[0,0,0],[0,0,0]], [[0,1,3,2]]);
}
