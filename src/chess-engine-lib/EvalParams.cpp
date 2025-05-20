#include "EvalParams.h"

#include "Piece.h"
#include "RangePatches.h"

#include <format>
#include <ranges>
#include <sstream>

#include <cstring>

namespace {

constexpr EvalParamArray kDefaultParams = {
        0.209377f,    1.000000f,    0.762991f,    1.679964f,    6.320265f,    0.000000f,
        0.209377f,    1.076343f,    0.328532f,    0.188950f,    0.338195f,    1.209568f,
        1.000000f,    -0.014947f,   0.000000f,    -0.009408f,   0.001165f,    55.700275f,
        44.311245f,   144.191528f,  390.956207f,  388.698944f,  364.305145f,  402.948486f,
        579.690796f,  720.149658f,  1373.341675f, 1164.603027f, 0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    4.738749f,    145.870819f,
        3.904189f,    147.728836f,  13.198586f,   149.412842f,  -2.458300f,   138.690048f,
        15.509078f,   170.405029f,  66.562782f,   149.869324f,  66.415855f,   139.443115f,
        27.575520f,   136.997360f,  18.725500f,   139.402481f,  18.521502f,   140.184128f,
        29.318663f,   143.661011f,  27.863142f,   144.447937f,  35.321667f,   152.190704f,
        57.017868f,   142.196732f,  59.643661f,   135.912186f,  45.629330f,   133.519928f,
        35.392410f,   146.972824f,  42.455593f,   150.797729f,  54.911617f,   148.117172f,
        63.191830f,   138.488510f,  62.428162f,   144.004028f,  74.558495f,   143.871811f,
        63.697147f,   147.258789f,  47.550900f,   142.714157f,  60.298946f,   163.512100f,
        61.357391f,   162.039932f,  70.354790f,   162.259399f,  82.968857f,   139.967667f,
        100.165726f,  148.822159f,  97.309464f,   142.191147f,  75.990746f,   156.839813f,
        72.444115f,   146.082687f,  75.219002f,   170.354446f,  83.935516f,   183.861801f,
        102.878891f,  166.848358f,  83.513405f,   134.050629f,  140.148193f,  160.386963f,
        173.992859f,  182.478455f,  144.885101f,  151.074295f,  121.962059f,  161.174774f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    337.969177f,  368.577393f,  343.523621f,  359.913208f,
        331.861053f,  372.135559f,  341.737671f,  377.845856f,  358.744659f,  365.613312f,
        354.582397f,  356.452789f,  331.994507f,  357.555420f,  303.215485f,  319.311829f,
        348.597809f,  346.123138f,  353.828491f,  388.371094f,  368.906708f,  389.308075f,
        379.129700f,  389.367279f,  368.046143f,  378.361786f,  373.815338f,  370.092499f,
        347.026886f,  367.996674f,  365.296875f,  333.664612f,  365.018951f,  365.187225f,
        383.518036f,  386.837036f,  388.186066f,  390.956512f,  409.623688f,  398.725220f,
        411.160828f,  394.888245f,  391.689117f,  368.858368f,  385.684875f,  364.644745f,
        357.398621f,  368.942200f,  388.886902f,  382.112793f,  411.175720f,  394.740356f,
        428.485229f,  399.389862f,  412.831757f,  411.000183f,  425.807831f,  418.143860f,
        417.506104f,  399.976624f,  422.968201f,  383.271851f,  379.532532f,  371.269379f,
        414.446106f,  387.278595f,  421.252991f,  399.752686f,  454.203918f,  395.765778f,
        475.897064f,  405.881927f,  440.996368f,  405.726868f,  461.048157f,  409.970947f,
        423.686554f,  403.857819f,  426.262054f,  376.090851f,  389.145172f,  386.071350f,
        453.635773f,  391.155426f,  482.209229f,  388.245178f,  488.203247f,  396.261719f,
        535.301086f,  379.824066f,  497.438049f,  395.799042f,  457.107910f,  396.866516f,
        396.589813f,  367.146973f,  408.372772f,  382.350616f,  380.518799f,  403.081604f,
        436.645874f,  385.536316f,  462.263092f,  398.860657f,  470.521698f,  390.111053f,
        469.812469f,  380.399109f,  398.564392f,  387.002289f,  359.188477f,  390.701538f,
        241.210739f,  458.621796f,  333.837280f,  421.574127f,  366.132324f,  395.013153f,
        364.178375f,  417.445648f,  382.257538f,  414.758240f,  364.579803f,  393.308655f,
        523.896240f,  334.170776f,  201.281815f,  389.028748f,  338.961853f,  376.747803f,
        362.529755f,  370.486053f,  337.086212f,  387.334106f,  339.566284f,  401.408783f,
        340.026520f,  390.526093f,  332.812927f,  400.589294f,  342.666779f,  380.441803f,
        333.728027f,  350.870697f,  373.784790f,  379.291901f,  358.241364f,  376.893188f,
        380.501709f,  386.287445f,  349.354492f,  403.281403f,  359.827728f,  398.933075f,
        368.919464f,  388.816071f,  383.314758f,  378.540619f,  351.205017f,  336.492737f,
        351.444611f,  401.842316f,  376.389771f,  398.178436f,  364.226776f,  417.515076f,
        374.202515f,  403.560822f,  369.937195f,  411.823700f,  373.482971f,  399.084534f,
        379.430573f,  392.483246f,  364.748962f,  395.050903f,  365.014923f,  416.239258f,
        355.883392f,  404.492706f,  372.546906f,  410.625000f,  381.199249f,  422.403717f,
        395.381226f,  410.853668f,  367.438538f,  420.553436f,  365.215881f,  415.476654f,
        370.391968f,  403.964294f,  359.524078f,  398.851746f,  368.605560f,  415.111969f,
        394.112732f,  415.834473f,  422.223999f,  407.463531f,  400.075623f,  413.974854f,
        405.462006f,  412.620270f,  369.233368f,  423.353363f,  364.383545f,  407.849213f,
        376.222015f,  413.531677f,  389.496918f,  411.154816f,  397.207062f,  411.197296f,
        415.559204f,  394.924591f,  427.595856f,  408.821594f,  422.589783f,  423.673309f,
        436.373199f,  411.143524f,  398.399200f,  400.140869f,  367.618866f,  415.157410f,
        374.483246f,  423.843323f,  392.984436f,  409.640991f,  393.674438f,  418.533539f,
        389.840454f,  407.919006f,  434.905731f,  394.718628f,  362.689392f,  411.111664f,
        350.444397f,  387.248077f,  354.558746f,  398.013641f,  417.179474f,  411.189758f,
        347.715027f,  418.883179f,  293.003632f,  426.070831f,  348.156555f,  423.978363f,
        399.972961f,  395.041351f,  382.195557f,  390.603729f,  398.545258f,  384.143158f,
        589.983826f,  697.954590f,  588.928711f,  697.198364f,  583.542114f,  704.364990f,
        582.546753f,  701.600220f,  581.373962f,  698.370422f,  569.162964f,  701.967041f,
        555.747803f,  704.583557f,  554.543701f,  692.244080f,  559.654602f,  714.536438f,
        578.049500f,  706.948120f,  580.441040f,  707.894958f,  568.281433f,  709.121338f,
        571.807068f,  714.131226f,  570.039978f,  700.093506f,  558.077393f,  711.772278f,
        525.291260f,  719.445129f,  553.151917f,  720.303223f,  553.882507f,  717.713806f,
        554.722961f,  722.144348f,  559.887756f,  723.014587f,  574.392578f,  713.506836f,
        552.551270f,  720.017456f,  562.785156f,  715.377502f,  534.306946f,  713.108643f,
        571.592224f,  729.999634f,  564.490479f,  732.046570f,  566.653137f,  730.337219f,
        568.757385f,  731.568848f,  564.299438f,  729.691162f,  551.878113f,  740.722168f,
        548.125427f,  738.336304f,  531.456970f,  735.171570f,  574.025635f,  746.188538f,
        582.209595f,  739.438782f,  604.742920f,  731.809265f,  604.650696f,  729.645691f,
        603.555298f,  725.303955f,  587.235352f,  737.674072f,  586.364075f,  739.840088f,
        580.030518f,  740.197571f,  582.483398f,  752.572754f,  608.837830f,  735.174316f,
        616.537048f,  731.766724f,  617.546204f,  725.403076f,  651.498047f,  730.429749f,
        649.169434f,  729.757446f,  657.352112f,  722.330383f,  623.607727f,  745.779358f,
        601.553101f,  755.230530f,  590.985168f,  759.663757f,  626.086243f,  752.274170f,
        630.763367f,  746.938843f,  621.067871f,  752.170288f,  662.380676f,  724.344116f,
        675.429993f,  729.609253f,  656.095337f,  733.050598f,  609.942627f,  738.934143f,
        627.527100f,  741.942444f,  613.379761f,  748.366394f,  594.069580f,  754.513000f,
        603.992676f,  752.260498f,  599.472229f,  755.885742f,  656.849304f,  747.246582f,
        599.537476f,  757.947327f,  1404.205444f, 1081.944336f, 1371.701416f, 1139.756348f,
        1384.328247f, 1092.614502f, 1366.204224f, 1134.418091f, 1365.470581f, 1100.587402f,
        1344.504517f, 1098.835083f, 1322.874512f, 1081.805298f, 1332.235962f, 1186.815308f,
        1369.550049f, 1163.505737f, 1382.539429f, 1116.761230f, 1386.828369f, 1118.171021f,
        1387.133911f, 1093.298340f, 1378.160889f, 1110.709106f, 1392.397339f, 1037.365234f,
        1382.595581f, 1008.221619f, 1307.323608f, 1155.901245f, 1375.696533f, 1131.894897f,
        1375.735474f, 1154.684082f, 1370.156860f, 1175.013550f, 1368.807129f, 1156.672729f,
        1358.718018f, 1174.648315f, 1372.631348f, 1168.679077f, 1369.162476f, 1126.390625f,
        1366.665649f, 1115.676880f, 1377.590454f, 1183.507202f, 1367.552124f, 1166.578491f,
        1368.532837f, 1182.989014f, 1361.832520f, 1200.795044f, 1356.310669f, 1219.247925f,
        1350.007446f, 1222.968140f, 1361.511963f, 1195.119385f, 1346.281128f, 1263.105347f,
        1361.349854f, 1213.239990f, 1355.046997f, 1224.559082f, 1362.943115f, 1221.229980f,
        1348.707275f, 1255.176147f, 1363.555542f, 1239.026367f, 1366.208496f, 1241.418091f,
        1366.341309f, 1232.279907f, 1385.359375f, 1220.406494f, 1368.833374f, 1234.185425f,
        1370.518188f, 1188.704346f, 1361.951294f, 1233.832275f, 1341.426758f, 1288.099365f,
        1375.198975f, 1263.111206f, 1442.626709f, 1208.894287f, 1427.385498f, 1199.091064f,
        1411.602539f, 1162.554321f, 1347.201416f, 1244.152588f, 1353.268677f, 1238.129395f,
        1368.393555f, 1229.467407f, 1399.615356f, 1201.338013f, 1349.396973f, 1298.503418f,
        1452.553345f, 1153.286987f, 1439.333130f, 1134.932373f, 1482.284546f, 1115.075073f,
        1388.347656f, 1173.693481f, 1385.471680f, 1182.738037f, 1426.711060f, 1167.226562f,
        1361.128662f, 1220.671265f, 1357.362305f, 1251.141113f, 1387.548706f, 1240.352417f,
        1359.495728f, 1254.561157f, 1390.096436f, 1213.231689f, 56.974525f,   -28.899570f,
        26.742361f,   -8.382714f,   -41.499504f,  2.438910f,    -53.184818f,  -12.634027f,
        -37.106827f,  -27.585281f,  -54.370655f,  8.353681f,    31.132095f,   -1.904718f,
        58.223015f,   -23.065866f,  6.497669f,    -3.341665f,   -21.101454f,  2.279014f,
        -80.404800f,  5.147511f,    -131.557434f, 11.796460f,   -128.099747f, 6.397942f,
        -78.958008f,  6.062293f,    -13.908405f,  -5.259055f,   25.316095f,   -22.870134f,
        -6.134894f,   -25.969217f,  -65.196297f,  2.049640f,    -104.889069f, -5.210816f,
        -129.068604f, 5.918463f,    -135.276642f, 7.005167f,    -142.362183f, 9.620917f,
        -88.015068f,  -7.732456f,   -45.845501f,  -31.075245f,  -9.669835f,   -27.300224f,
        22.526731f,   -28.987507f,  -7.854955f,   -23.684536f,  -103.434044f, -1.549232f,
        -93.595329f,  0.059925f,    -102.910652f, -2.607859f,   -111.187149f, -10.232025f,
        -137.957108f, -15.329316f,  43.013214f,   -17.066429f,  18.509504f,   -5.521224f,
        -11.583697f,  -3.364150f,   4.801718f,    -8.251021f,   -32.080616f,  -9.398423f,
        -25.143679f,  -14.751506f,  3.024057f,    -20.141951f,  -75.120445f,  -19.120110f,
        40.305340f,   13.688290f,   93.598450f,   -6.258007f,   78.853500f,   -0.110749f,
        96.410255f,   -9.487206f,   -6.336550f,   -2.438577f,   138.983398f,  -17.680418f,
        -3.373664f,   17.653387f,   132.424713f,  -17.451845f,  96.115784f,   -15.038341f,
        73.610802f,   15.293553f,   148.905258f,  -3.289172f,   171.329987f,  -30.183088f,
        39.546112f,   -22.232864f,  38.712070f,   -20.622849f,  37.862106f,   34.747356f,
        153.687637f,  -22.240545f,  53.285160f,   -60.209442f,  233.481873f,  -43.612129f,
        183.366608f,  -11.490602f,  262.040161f,  -56.850204f,  258.771881f,  -102.339500f,
        96.077835f,   -2.981761f,   473.706421f,  10.058099f,   387.081055f,  -176.970505f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    57.670319f,   124.608170f,
        37.690628f,   106.585968f,  77.115013f,   85.258453f,   70.981392f,   99.738350f,
        79.548195f,   165.717682f,  154.402145f,  117.076668f,  145.166931f,  98.120659f,
        112.957336f,  120.016281f,  73.209900f,   125.961113f,  62.299751f,   125.448242f,
        75.922134f,   138.851395f,  87.224693f,   119.630356f,  91.379829f,   135.691650f,
        125.783424f,  135.250870f,  117.388924f,  128.289108f,  98.646606f,   118.240845f,
        81.475540f,   178.471054f,  78.180962f,   159.197510f,  106.162575f,  160.029846f,
        126.113060f,  133.098480f,  145.156708f,  130.328201f,  137.620667f,  162.659424f,
        74.811920f,   163.968704f,  80.922188f,   155.755096f,  119.380730f,  245.670685f,
        107.644257f,  274.034454f,  142.122452f,  216.956924f,  153.680832f,  211.447662f,
        198.432648f,  171.455261f,  233.302444f,  156.782288f,  184.550171f,  211.249268f,
        191.641708f,  177.561569f,  124.258858f,  401.616272f,  87.137589f,   424.934052f,
        143.270966f,  221.340530f,  191.529541f,  236.272446f,  257.014343f,  216.256271f,
        287.420105f,  193.965897f,  229.260330f,  167.497025f,  255.175964f,  223.106934f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        24.764824f,   79.545448f,   42.338100f,   73.352257f,   43.046970f,   75.706367f,
        68.265465f,   66.916122f,   80.513931f,   70.454620f,   103.745285f,  36.653450f,
        70.987801f,   53.128555f,   47.710838f,   54.815598f,   48.944828f,   92.600487f,
        69.013794f,   76.102959f,   58.800655f,   71.708496f,   35.007732f,   68.444191f,
        50.045208f,   76.888763f,   59.227402f,   79.297043f,   69.113159f,   74.479752f,
        74.974556f,   49.533733f,   114.869621f,  119.521446f,  127.866005f,  115.590790f,
        88.282204f,   102.384171f,  93.725159f,   96.900017f,   97.987999f,   93.306168f,
        102.679176f,  102.599335f,  76.980721f,   142.600922f,  83.060318f,   119.267929f,
        176.548721f,  187.300980f,  191.540222f,  178.899887f,  149.356705f,  164.311096f,
        158.920944f,  128.195938f,  181.817963f,  105.938759f,  191.996170f,  95.432121f,
        112.357979f,  161.211075f,  136.424881f,  156.158554f,  296.952759f,  250.649841f,
        271.555084f,  253.918976f,  250.067047f,  227.680801f,  247.625137f,  188.328842f,
        278.395050f,  143.785614f,  316.756226f,  128.074844f,  203.837631f,  196.009735f,
        204.278229f,  201.820801f,  454.175781f,  266.161621f,  430.946930f,  268.782898f,
        391.419037f,  253.073975f,  422.080200f,  218.856247f,  410.706818f,  171.927200f,
        325.715729f,  193.665466f,  165.348312f,  242.880432f,  244.708527f,  215.054672f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    12.649882f,   10.302147f,
        12.543474f,   -16.889202f,  23.896969f,   57.126282f,   15.472165f,   8.724533f,
        7.337094f,    21.236107f,   40.022903f,   18.190493f,   42.496544f,   2.374478f,
        27.074850f,   4.514232f,    14.557858f,   4.892654f,    0.000000f,    0.000000f,
        -14.508928f,  11.652245f,   -23.697897f,  -1.006187f,   -13.459570f,  -4.368816f,
        3.308576f,    -625.641541f, 32.829700f,   12.751536f,   33.203396f,   -1.721117f,
        16.491573f,   5.674189f,    9.848853f,    9.950441f,    -0.000000f,   0.000000f,
        -8.491116f,   7.760035f,    -13.197630f,  -0.422789f,   -2.567065f,   36.131298f,
        -0.366183f,   -73.285027f,  37.156120f,   72.358070f,   2.991902f,    -18.402315f,
        -37.710735f,  -30.153536f,  18.925791f,   8.200181f,    37.889118f,   -2.700435f,
        -282.508392f, 85.074203f,   -116.297287f, 41.710915f,   33.386597f,   -25.073984f,
        17.779099f,   -12.389237f,  0.000000f,    0.000000f,    2.705944f,    6.606321f,
        -3.225734f,   27.495470f,   -15.317623f,  52.187576f,   -22.482977f,  79.175140f,
        -445.967987f, 194.981262f,  -92.724579f,  33.307140f,   53.765831f,   -39.556225f,
        33.441338f,   -28.424438f,  0.000000f,    0.000000f,    -20.163742f,  29.213652f,
        -31.135168f,  54.916359f,   -68.603523f,  107.315376f,  -103.484726f, 158.690918f,
        319.263123f,  -215.461121f, 88.223770f,   -82.732277f,  -127.158409f, 80.870483f,
        -67.151291f,  31.340961f,   0.000000f,    0.000000f,    48.448959f,   -25.750860f,
        92.048889f,   -55.071800f,  205.898895f,  -179.499832f, 305.674683f,  -264.903656f,
        2.367158f,    -0.166281f,   0.000000f,    0.000000f,    -0.175594f,   2.429896f,
        5.406711f,    3.025739f,    3.130416f,    3.758424f,    2.415930f,    3.047190f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    -9.273084f,   -30.077717f,  -7.938403f,   -10.668716f,
        -6.408799f,   0.211878f,    -6.067870f,   2.268956f,    -3.717131f,   2.761829f,
        0.000000f,    0.000000f,    0.484041f,    -0.068398f,   -1.705470f,   -1.607636f,
        107.152420f,  -32.154282f,  9.582180f,    -34.021431f,  10.744215f,   -14.946447f,
        7.381150f,    -7.451790f,   4.463482f,    -1.476237f,   1.525777f,    1.811933f,
        0.000000f,    0.000000f,    -6.692753f,   0.305978f,    -1.971303f,   -4.249008f,
        14.166418f,   -15.634889f,  12.738251f,   -33.566002f,  8.414471f,    -16.242857f,
        6.854473f,    -9.557291f,   5.029870f,    -5.784403f,   -0.247629f,   -0.746153f,
        0.000000f,    0.000000f,    -0.556436f,   -1.542253f,   1.704464f,    -5.038418f,
        4.723783f,    -11.525443f,  2.136334f,    -134.707489f, 5.263465f,    -102.277176f,
        6.504786f,    -51.250320f,  3.727630f,    -36.366768f,  2.555811f,    -15.813128f,
        0.000000f,    0.000000f,    -0.320391f,   -2.877216f,   -2.528599f,   3.133627f,
        -4.115126f,   9.803563f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    6.430264f,    -0.868676f,   2.601942f,    1.481267f,
        1.843713f,    -0.057581f,   1.779256f,    0.371884f,    2.258776f,    0.395147f,
        5.968858f,    -2.109740f,   4.520441f,    1.450041f,    6.429832f,    12.431087f,
        -3.832668f,   5.835383f,    -4.095053f,   -3.530355f,   0.514402f,    1.189808f,
        -2.813584f,   -0.286812f,   4.823622f,    -5.040440f,   4.155324f,    -2.771536f,
        -3.811047f,   -1.533615f,   -2.829248f,   -4.711511f,   -12.471711f,  -6.573600f,
        0.154439f,    -7.671330f,   20.000000f,   10.000000f,   -2.613081f,   -8.984508f,
        -8.153668f,   -44.874054f,  6.936666f,    -19.242355f,  7.909854f,    5.115963f,
        -40.450302f,  -34.350388f,  -1.759705f,   -13.861781f,  5.039505f,    12.054765f,
        -41.930500f,  -31.338163f,  -15.109263f,  0.646675f,    3.760356f,    10.994230f,
        -50.445343f,  -7.084170f,   -3.419151f,   4.237572f,    2.920645f,    27.815483f,
        -68.288704f,  -23.523802f,  -45.550179f,  11.985205f,   20.446613f,   -12.531342f,
        -20.317337f,  -44.320473f,  27.349821f,   -36.909397f,  0.000000f,    0.000000f,
        30.350580f,   -42.763607f,  31.932123f,   -43.878979f,  44.231884f,   -47.780548f,
        70.672974f,   -59.085209f,  83.028046f,   -64.716858f,  140.174667f,  -85.184944f,
        222.082611f,  -111.746872f, 196.014069f,  -89.887718f,  0.000000f,    0.000000f,
        -55.516846f,  41.630886f,   -36.938335f,  36.985275f,   -28.967016f,  37.211945f,
        -37.561172f,  37.803486f,   -34.790154f,  41.980885f,   0.000000f,    0.000000f,
        0.000000f,    0.000000f,    5.362234f,    0.530777f,    60.267815f,   -39.186123f,
        130.572510f,  -78.725906f,  217.419785f,  -121.674667f, 316.562012f,  -166.821243f,
        -9.828558f,   3.064376f,    -13.739232f,  -46.566696f,  -20.357542f,  -50.729206f,
        -54.065037f,  -8.158860f,   -35.972618f,  7.168637f,    -15.941740f,  15.216648f,
        -8.448993f,   5.622088f,    -22.962299f,  33.869656f};

std::string evalCalcTToString(const EvalCalcT term) {
    return std::format("{:>6.1f}", term);
}

std::string taperedTermToString(const TaperedTerm& term) {
    return std::format("{{{:>6.1f}, {:>6.1f}}}", term.early, term.late);
}

std::string getPstPieceTypeName(int idx) {
    if (idx == EvalParams::kCandidatePassedPawnPstIdx) {
        return "Candidate";
    } else if (idx == EvalParams::kPassedPawnPstIdx) {
        return "Passed pawn";
    } else {
        return pieceToString((Piece)idx);
    }
}

std::string getTropismPieceTypeName(int idx) {
    if (idx == EvalParams::kDoubledPawnTropismIdx) {
        return "Doubled pawn";
    } else if (idx == EvalParams::kIsolatedPawnTropismIdx) {
        return "Isolated pawn";
    } else if (idx == EvalParams::kPassedPawnTropismIdx) {
        return "Passed pawn";
    } else if (idx == EvalParams::kCandidatePassedPawnTropismIdx) {
        return "Candidate";
    } else {
        return pieceToString((Piece)idx);
    }
}

std::string getAttackDefendPieceTypeName(int idx) {
    if (idx == EvalParams::kPassedPawnAttackDefendIdx) {
        return "Passed pawn";
    } else {
        return pieceToString((Piece)idx);
    }
}

void writeSquareTable(const EvalParams::SquareTable& squareTable, std::ostream& out) {
    for (int rank = kRanks - 1; rank >= 0; --rank) {
        out << "\t\t";
        for (int file = 0; file < kFiles; ++file) {
            const auto& value = squareTable[rank * kFiles + file];
            out << std::format("{}, ", taperedTermToString(value));
        }
        out << "\n";
    }
}

void writePieceSquareTables(
        const EvalParams::PieceSquareTables& pieceSquareTables, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < (int)pieceSquareTables.size(); ++pieceIdx) {
        out << "\t" << getPstPieceTypeName(pieceIdx) << ": {\n";
        writeSquareTable(pieceSquareTables[pieceIdx], out);
        out << "\t}\n";
    }
}

void writeTropismTable(
        const std::array<TaperedTerm, EvalParams::kNumTropismPieceTypes>& tropism,
        std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < (int)EvalParams::kNumTropismPieceTypes; ++pieceIdx) {
        out << std::format(
                "\t{}: {}\n",
                getTropismPieceTypeName(pieceIdx),
                taperedTermToString(tropism[pieceIdx]));
    }
}

template <std::size_t N>
std::string arrayToString(const std::array<TaperedTerm, N>& valueArray) {
    std::string inner =
            valueArray | std::ranges::views::transform(taperedTermToString) | joinToString(", ");
    return "{" + inner + "}";
}

template <std::size_t N>
std::string arrayToString(const std::array<EvalCalcT, N>& valueArray) {
    std::string inner =
            valueArray | std::ranges::views::transform(evalCalcTToString) | joinToString(", ");
    return "{" + inner + "}";
}

void writeTaperedTermPerPiece(
        const std::array<TaperedTerm, kNumPieceTypes>& terms, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        out << std::format(
                "\t{}: {}\n", pieceToString((Piece)pieceIdx), taperedTermToString(terms[pieceIdx]));
    }
}

}  // namespace

EvalParams EvalParams::getEmptyParams() {
    return EvalParams{};
}

EvalParams EvalParams::getDefaultParams() {
    return evalParamsFromArray(kDefaultParams);
}

EvalParamArray evalParamsToArray(const EvalParams& params) {
    EvalParamArray array;
    std::memcpy(array.data(), &params, sizeof(EvalParams));
    return array;
}

EvalParams evalParamsFromArray(const EvalParamArray& array) {
    EvalParams params = EvalParams::getEmptyParams();
    std::memcpy(&params, array.data(), sizeof(EvalParams));
    return params;
}

std::string evalParamsToString(const EvalParams& params) {
    std::ostringstream oss;

    oss << "phaseMaterialValues: {\n";
    for (int pieceIdx = 0; pieceIdx < (int)EvalParams::kNumPstPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {:>6.1f}\n",
                getPstPieceTypeName(pieceIdx),
                params.phaseMaterialValues[pieceIdx]);
    }
    oss << "}\n";

    oss << std::format(
            "\noppositeColoredBishopFactor:\n\t{}",
            arrayToString(params.oppositeColoredBishopFactor));

    oss << std::format("\nsingleMinorFactor:\n\t{}", evalCalcTToString(params.singleMinorFactor));
    oss << std::format("\ntwoKnightsFactor:\n\t{}", evalCalcTToString(params.twoKnightsFactor));
    oss << std::format("\nrookVsMinorFactor:\n\t{}", evalCalcTToString(params.rookVsMinorFactor));
    oss << std::format(
            "\nrookAndMinorVsRookFactor:\n\t{}",
            evalCalcTToString(params.rookAndMinorVsRookFactor));

    oss << std::format("\nhasUnstoppablePawn:\n\t{}", evalCalcTToString(params.hasUnstoppablePawn));

    oss << "\npieceValues: {\n";
    writeTaperedTermPerPiece(params.pieceValues, oss);
    oss << "}\n";

    oss << "\npieceSquareTables: {\n";
    writePieceSquareTables(params.pieceSquareTables, oss);
    oss << "}\n";

    oss << std::format(
            "\nprotectedPawnBonus:\n\t{}", taperedTermToString(params.protectedPawnBonus));

    oss << std::format(
            "\nprotectedPassedPawnBonus:\n\t{}",
            taperedTermToString(params.protectedPassedPawnBonus));

    oss << std::format(
            "\nconnectedPassedPawnBonus:\n\t{}",
            taperedTermToString(params.connectedPassedPawnBonus));

    oss << std::format(
            "\ndoubledPawnPenalty:\n\t{}", taperedTermToString(params.doubledPawnPenalty));

    oss << std::format(
            "\nisolatedPawnPenalty:\n\t{}", taperedTermToString(params.isolatedPawnPenalty));

    oss << std::format(
            "\nbishopPawnSameColorAdjustment:\n\t{}",
            arrayToString(params.bishopPawnSameColorAdjustment));

    oss << std::format(
            "\nbishopEnemyPawnSameColorAdjustment:\n\t{}",
            arrayToString(params.bishopEnemyPawnSameColorAdjustment));

    oss << std::format("\nbishopPairBonus:\n\t{}", taperedTermToString(params.bishopPairBonus));

    oss << std::format("\nknightPairBonus:\n\t{}", taperedTermToString(params.knightPairBonus));

    oss << std::format("\nrookPairBonus:\n\t{}", taperedTermToString(params.rookPairBonus));

    oss << std::format(
            "\nrookSemiOpenFileBonus:\n\t{}", taperedTermToString(params.rookSemiOpenFileBonus));

    oss << std::format("\nrookOpenFileBonus:\n\t{}", taperedTermToString(params.rookOpenFileBonus));

    oss << std::format("\nknightPawnAdjustment:\n\t{}", arrayToString(params.knightPawnAdjustment));

    oss << std::format("\nrookPawnAdjustment:\n\t{}", arrayToString(params.rookPawnAdjustment));

    oss << std::format("\nqueenPawnAdjustment:\n\t{}", arrayToString(params.queenPawnAdjustment));

    oss << std::format(
            "\nkingVirtualMobilityPenalty:\n\t{}",
            taperedTermToString(params.kingVirtualMobilityPenalty));

    oss << "\n\nmobilityBonus: {\n";
    writeTaperedTermPerPiece(params.mobilityBonus, oss);
    oss << "}\n";

    oss << "\n\nsafeMobilityAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                arrayToString(params.safeMobilityAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << "\nownKingTropism: {\n";
    writeTropismTable(params.ownKingTropism, oss);
    oss << "}\n";

    oss << "\nenemyKingTropism: {\n";
    writeTropismTable(params.enemyKingTropism, oss);
    oss << "}\n";

    oss << std::format("\ntempoBonus:\n\t{}", taperedTermToString(params.tempoBonus));

    oss << "\n\nattackDefendAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < (int)params.attackDefendAdjustment.size(); ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                getAttackDefendPieceTypeName(pieceIdx),
                arrayToString(params.attackDefendAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format("\ncontrolNearEnemyKing:\n\t{}", arrayToString(params.controlNearEnemyKing));

    oss << "\n\nkingAttackWeight: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.kingAttackWeight[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nnumKingAttackersAdjustment:\n\t{}",
            arrayToString(params.numKingAttackersAdjustment));

    oss << "\n\npiecePinnedAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.piecePinnedAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nkingOpenFileAdjustment:\n\t{}", taperedTermToString(params.kingOpenFileAdjustment));
    oss << std::format(
            "\nkingFlankOpenFileAdjustment:\n\t{}",
            taperedTermToString(params.kingFlankOpenFileAdjustment));
    oss << std::format(
            "\npassedPawnOutsideKingSquare:\n\t{}",
            taperedTermToString(params.passedPawnOutsideKingSquare));

    return oss.str();
}
