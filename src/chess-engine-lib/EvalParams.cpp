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
        0.209377f,    1.076343f,    0.328008f,    0.188586f,    0.337068f,    1.082230f,
        1.000000f,    -0.011780f,   0.000000f,    -0.009075f,   0.001102f,    60.563297f,
        43.347031f,   145.193665f,  389.137024f,  389.116760f,  363.617859f,  403.745361f,
        577.584595f,  721.568176f,  1371.622314f, 1163.454102f, 0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    4.087521f,    146.501495f,
        2.623493f,    149.153839f,  12.418391f,   150.149933f,  -3.239765f,   138.609406f,
        14.780332f,   170.963211f,  65.607529f,   150.561127f,  65.868446f,   140.127258f,
        26.810108f,   137.437302f,  17.897808f,   140.304398f,  17.066916f,   142.254578f,
        28.329729f,   144.771240f,  26.701141f,   145.578934f,  34.347820f,   153.316895f,
        55.709030f,   143.475296f,  58.648849f,   137.298859f,  44.753933f,   134.338150f,
        34.500069f,   147.756165f,  41.054073f,   152.492767f,  53.816593f,   149.406509f,
        62.043419f,   139.338455f,  61.475372f,   144.603531f,  73.242805f,   145.164642f,
        62.737648f,   148.446350f,  46.679119f,   143.598251f,  59.726974f,   163.847031f,
        59.974728f,   163.903198f,  69.123337f,   163.186829f,  81.444824f,   141.269394f,
        98.964073f,   149.974930f,  96.150406f,   143.018097f,  74.782654f,   158.126831f,
        71.670563f,   146.591476f,  74.585045f,   171.329056f,  81.611137f,   186.070358f,
        99.852638f,   168.608902f,  81.593864f,   133.918900f,  135.681458f,  159.347763f,
        172.355667f,  183.459549f,  144.879532f,  150.908600f,  120.985069f,  161.660217f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    334.053406f,  366.758514f,  341.674072f,  361.914673f,
        329.604645f,  373.688263f,  340.402924f,  377.898895f,  356.461945f,  366.400299f,
        352.787720f,  356.498016f,  329.636536f,  359.467590f,  301.323181f,  319.483551f,
        346.940216f,  345.192871f,  352.496185f,  388.752380f,  367.472198f,  389.897339f,
        377.409912f,  389.514374f,  366.395477f,  379.057068f,  372.152069f,  370.303772f,
        344.625000f,  369.531067f,  363.333008f,  335.006317f,  363.180115f,  365.599457f,
        381.969666f,  387.153839f,  386.496124f,  391.306152f,  407.358307f,  399.059265f,
        409.204437f,  395.189423f,  389.897400f,  369.068390f,  383.811035f,  364.644012f,
        355.509460f,  370.249451f,  387.080780f,  382.919312f,  409.286011f,  395.859253f,
        426.863678f,  399.548462f,  410.577637f,  411.202576f,  423.692474f,  418.477478f,
        415.842285f,  400.581512f,  419.511078f,  384.844757f,  377.261566f,  372.666962f,
        412.159241f,  387.766357f,  419.496857f,  400.116150f,  452.289856f,  395.981628f,
        474.002472f,  405.517334f,  439.195007f,  406.179047f,  459.021301f,  410.216064f,
        421.693298f,  404.328674f,  425.818848f,  375.005280f,  386.666199f,  387.773102f,
        452.198608f,  390.595245f,  479.701874f,  388.461426f,  486.325775f,  397.002045f,
        531.547363f,  381.386993f,  493.602783f,  396.606415f,  455.273987f,  396.690460f,
        391.719269f,  368.671539f,  403.874298f,  385.175293f,  380.988983f,  402.730896f,
        437.092163f,  385.378448f,  461.023743f,  399.166199f,  467.311523f,  392.358063f,
        468.350586f,  380.759064f,  395.373962f,  389.783264f,  364.329407f,  390.047180f,
        240.016266f,  457.834503f,  340.052704f,  419.769531f,  364.770294f,  396.180237f,
        365.215088f,  417.825897f,  380.217499f,  416.235687f,  365.765564f,  392.512604f,
        516.176880f,  336.308594f,  200.498901f,  387.587494f,  338.931152f,  377.812622f,
        361.415924f,  372.203430f,  336.481628f,  387.417633f,  339.244171f,  401.637512f,
        339.375366f,  391.867584f,  331.997223f,  401.705963f,  344.602173f,  379.268555f,
        332.415314f,  351.997253f,  373.040619f,  380.664734f,  357.437683f,  377.733246f,
        379.976349f,  387.121674f,  348.798462f,  403.848816f,  359.070007f,  399.915741f,
        368.272736f,  389.991669f,  382.634857f,  379.454346f,  349.985901f,  338.747772f,
        350.591919f,  402.550171f,  375.567383f,  398.920990f,  363.551849f,  417.837982f,
        373.565613f,  404.183044f,  369.349762f,  412.577576f,  372.922424f,  399.719940f,
        378.733215f,  393.581696f,  364.124084f,  396.646881f,  364.500183f,  417.108154f,
        356.047729f,  404.589325f,  371.822693f,  411.427429f,  380.428741f,  423.053894f,
        394.368042f,  412.109253f,  366.854340f,  421.335388f,  365.323395f,  416.047546f,
        369.744415f,  404.902527f,  357.901794f,  399.589600f,  367.844757f,  415.829834f,
        392.692261f,  417.097870f,  421.787231f,  407.858429f,  398.457062f,  414.977173f,
        404.104218f,  413.873169f,  368.400482f,  424.681671f,  365.043518f,  407.956848f,
        374.703400f,  414.523163f,  387.884460f,  412.302460f,  393.584961f,  412.776123f,
        414.200623f,  396.164459f,  428.241211f,  409.310181f,  423.535736f,  424.609283f,
        437.060242f,  411.734222f,  397.886139f,  400.609772f,  366.360901f,  415.821106f,
        375.988190f,  424.017365f,  392.611206f,  409.452087f,  394.794922f,  418.432526f,
        392.097229f,  408.148499f,  436.202820f,  394.771759f,  364.250854f,  411.564362f,
        345.115448f,  390.468658f,  354.595520f,  398.278473f,  407.349487f,  413.324646f,
        343.212402f,  420.397766f,  287.892853f,  427.644806f,  349.291473f,  425.085693f,
        387.108643f,  399.721588f,  384.365143f,  390.972748f,  397.147430f,  385.929199f,
        587.933289f,  698.913086f,  586.766968f,  698.510071f,  581.597168f,  705.561890f,
        580.506165f,  702.620117f,  579.324341f,  699.869751f,  567.077637f,  703.108643f,
        552.849487f,  706.767212f,  552.293030f,  693.283691f,  557.868225f,  715.847168f,
        576.134338f,  708.323608f,  578.365234f,  709.202881f,  566.096741f,  710.744324f,
        569.889709f,  714.934021f,  567.798462f,  701.531433f,  554.654785f,  713.351318f,
        525.174683f,  720.150024f,  550.168274f,  721.923828f,  552.055176f,  718.800232f,
        552.706360f,  723.410645f,  557.523621f,  724.606506f,  571.829895f,  715.054443f,
        550.686157f,  721.186707f,  560.204529f,  716.763062f,  531.225281f,  714.766113f,
        569.262878f,  731.478088f,  561.631775f,  734.047058f,  563.905701f,  732.292725f,
        566.271912f,  733.094421f,  562.470886f,  730.982666f,  550.660828f,  741.945801f,
        545.634949f,  739.032959f,  529.052673f,  736.402954f,  570.236511f,  748.416931f,
        580.273987f,  740.712769f,  601.948486f,  733.539673f,  603.414734f,  730.896423f,
        601.159302f,  727.229431f,  584.422546f,  739.529907f,  585.712769f,  741.470764f,
        577.387756f,  742.236755f,  580.059937f,  754.488281f,  607.678345f,  736.587463f,
        613.639709f,  733.751953f,  616.571533f,  726.707397f,  647.643799f,  733.179321f,
        642.657654f,  733.690063f,  655.098022f,  723.815430f,  624.732361f,  746.746338f,
        599.460632f,  756.949951f,  588.003906f,  761.538757f,  624.576172f,  753.553711f,
        628.185181f,  748.773315f,  619.848389f,  753.147034f,  658.837341f,  726.239258f,
        676.312439f,  729.769043f,  655.741272f,  734.251099f,  608.069702f,  740.259094f,
        620.720886f,  745.315186f,  614.219788f,  749.132935f,  592.753784f,  755.736755f,
        602.678833f,  753.561157f,  596.710754f,  757.991638f,  648.153687f,  750.927246f,
        601.782593f,  758.941467f,  1401.552490f, 1082.731934f, 1369.977173f, 1138.716553f,
        1382.682129f, 1092.222168f, 1364.356812f, 1133.842896f, 1364.381958f, 1096.791992f,
        1343.541748f, 1097.155884f, 1316.317993f, 1082.662964f, 1331.245361f, 1182.054688f,
        1367.776123f, 1161.181641f, 1380.368652f, 1117.726562f, 1385.390015f, 1116.096924f,
        1385.516724f, 1091.968018f, 1376.563232f, 1109.404663f, 1390.640869f, 1035.389160f,
        1382.911499f, 1000.801208f, 1302.335815f, 1166.415161f, 1373.553467f, 1131.147949f,
        1373.890991f, 1154.284302f, 1368.213745f, 1173.882690f, 1367.204956f, 1154.254150f,
        1357.405518f, 1171.753540f, 1371.145020f, 1167.359253f, 1367.645264f, 1126.224121f,
        1365.029907f, 1112.144775f, 1375.461548f, 1183.808472f, 1366.255493f, 1165.210571f,
        1366.557617f, 1181.916748f, 1359.533325f, 1200.598145f, 1354.269775f, 1219.544800f,
        1347.760742f, 1221.859985f, 1360.071655f, 1194.652710f, 1344.156006f, 1265.029785f,
        1358.543701f, 1210.036499f, 1353.151001f, 1223.348633f, 1361.076660f, 1220.364624f,
        1347.543091f, 1251.158081f, 1362.340332f, 1237.684448f, 1363.695679f, 1241.477051f,
        1364.682983f, 1230.912476f, 1383.586548f, 1219.791992f, 1367.456421f, 1232.084961f,
        1367.973389f, 1188.930664f, 1359.069946f, 1234.950562f, 1339.823242f, 1285.858887f,
        1376.517822f, 1259.733032f, 1442.945068f, 1206.398682f, 1423.080566f, 1203.343994f,
        1407.448730f, 1165.934448f, 1347.658081f, 1239.157959f, 1351.913940f, 1235.603638f,
        1367.743530f, 1225.442993f, 1396.438354f, 1201.798706f, 1349.534058f, 1292.830200f,
        1440.969360f, 1161.892700f, 1444.959839f, 1117.006958f, 1479.562500f, 1117.693848f,
        1392.179932f, 1166.654053f, 1382.791382f, 1184.515991f, 1421.652832f, 1169.924561f,
        1359.468628f, 1221.161255f, 1355.177368f, 1251.900269f, 1386.043579f, 1240.663574f,
        1365.047974f, 1249.867798f, 1387.470581f, 1215.777100f, 55.673615f,   -28.117315f,
        26.820709f,   -8.946693f,   -41.728340f,  2.926608f,    -52.738659f,  -12.481618f,
        -36.834358f,  -27.553032f,  -53.904858f,  8.310083f,    30.778637f,   -1.369062f,
        57.608192f,   -22.155725f,  6.674103f,    -3.449848f,   -21.318317f,  2.134555f,
        -79.464111f,  5.041321f,    -131.455017f, 12.037336f,   -126.857727f, 6.206445f,
        -78.760071f,  6.129614f,    -13.717296f,  -5.216728f,   25.263922f,   -22.505850f,
        -8.311407f,   -25.448708f,  -65.448074f,  1.994218f,    -103.807610f, -5.232527f,
        -127.918076f, 5.351357f,    -134.290314f, 6.723389f,    -142.677734f, 9.733865f,
        -87.128685f,  -7.925694f,   -44.350300f,  -31.489954f,  -8.595252f,   -28.761530f,
        22.481266f,   -29.502951f,  -8.116608f,   -23.290028f,  -103.329597f, -1.424057f,
        -91.441330f,  -0.390185f,   -101.975449f, -2.552046f,   -108.979286f, -10.621367f,
        -135.965393f, -16.117937f,  41.193153f,   -15.030490f,  16.717739f,   -5.587232f,
        -11.317531f,  -4.033874f,   -6.693757f,   -5.572491f,   -26.888332f,  -10.583862f,
        -32.134575f,  -13.698823f,  7.662514f,    -20.372835f,  -68.147095f,  -20.775723f,
        42.601059f,   13.898159f,   99.149239f,   -7.458015f,   76.573555f,   0.632307f,
        102.045815f,  -12.151053f,  -3.706476f,   -3.305728f,   153.884674f,  -18.563101f,
        -6.557108f,   17.742706f,   142.962860f,  -20.364582f,  113.215721f,  -20.666878f,
        81.870583f,   12.717854f,   151.430786f,  -4.338123f,   162.976517f,  -26.747215f,
        38.765472f,   -21.705933f,  42.155499f,   -22.265871f,  44.357525f,   33.527138f,
        153.199234f,  -22.130507f,  54.542965f,   -59.716125f,  188.842667f,  -32.510475f,
        154.747955f,  -5.244746f,   281.834167f,  -59.334553f,  251.404938f,  -102.675430f,
        130.044022f,  -11.288496f,  428.747162f,  22.024462f,   407.708405f,  -177.851181f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    56.512249f,   126.105644f,
        38.050171f,   108.449409f,  75.117943f,   87.772713f,   73.164360f,   98.930649f,
        78.822350f,   166.839172f,  153.612061f,  120.049629f,  145.509399f,  100.524689f,
        112.099777f,  125.207237f,  70.852104f,   126.926407f,  61.788902f,   127.898239f,
        72.832993f,   141.186508f,  86.672737f,   120.057655f,  91.178650f,   138.064499f,
        124.266556f,  136.974655f,  118.400574f,  130.519272f,  96.892258f,   120.994926f,
        79.778152f,   180.578583f,  76.994682f,   161.583603f,  105.753143f,  162.131500f,
        124.924004f,  135.515060f,  145.396149f,  132.348434f,  136.549454f,  165.134735f,
        75.428589f,   166.301697f,  83.945526f,   155.284607f,  121.332520f,  249.624924f,
        106.833977f,  274.023743f,  141.522125f,  219.059494f,  153.271027f,  215.255112f,
        196.328278f,  174.100830f,  230.618317f,  157.101913f,  183.474091f,  213.524292f,
        193.691895f,  177.505508f,  112.661369f,  420.011353f,  108.297630f,  428.381073f,
        138.619720f,  228.090500f,  191.555023f,  228.839966f,  258.935364f,  214.716370f,
        279.515411f,  197.726837f,  218.041550f,  180.373917f,  247.813248f,  231.435501f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        25.866085f,   77.895691f,   45.207428f,   69.885941f,   44.488354f,   72.994141f,
        68.147057f,   65.575050f,   77.691742f,   69.554771f,   106.065430f,  35.563377f,
        71.193443f,   52.574966f,   46.954308f,   54.547691f,   51.873837f,   89.202217f,
        75.669464f,   69.750969f,   63.371658f,   67.028305f,   38.332069f,   64.737358f,
        55.462673f,   72.229027f,   64.393219f,   74.889641f,   75.810890f,   68.974586f,
        80.797447f,   43.630844f,   116.799866f,  116.363922f,  131.860092f,  111.330414f,
        93.268379f,   97.176857f,   99.580826f,   91.149605f,   104.545486f,  87.783737f,
        108.552658f,  97.452614f,   81.512428f,   138.103302f,  88.073784f,   114.279716f,
        177.319122f,  185.419464f,  194.313950f,  175.609634f,  152.979050f,  160.157578f,
        164.623337f,  123.005325f,  186.618866f,  100.307671f,  196.356369f,  90.646942f,
        119.412163f,  156.028931f,  138.430252f,  152.892227f,  297.767334f,  249.235306f,
        273.093567f,  252.078873f,  252.333206f,  225.590225f,  249.435104f,  186.785187f,
        281.458527f,  139.729507f,  318.025970f,  125.688347f,  207.494354f,  192.767242f,
        205.007401f,  199.553497f,  453.395050f,  265.613770f,  429.678101f,  269.226562f,
        392.881317f,  252.556870f,  423.543121f,  216.958374f,  410.304016f,  172.358124f,
        331.118195f,  190.129761f,  165.099380f,  242.359543f,  247.366684f,  213.849747f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    13.087482f,   8.655989f,
        23.052126f,   58.009193f,   12.828939f,   13.390728f,   9.854548f,    17.614813f,
        40.570686f,   17.900305f,   42.654202f,   2.461928f,    26.867172f,   4.583480f,
        14.475964f,   5.104581f,    0.000000f,    0.000000f,    -14.927769f,  12.330573f,
        -23.655863f,  -1.547962f,   -14.300665f,  -6.426953f,   3.866193f,    -619.152527f,
        32.523109f,   12.602061f,   33.115829f,   -2.104885f,   16.229584f,   5.755186f,
        9.638103f,    10.171024f,   -0.000000f,   0.000000f,    -9.012076f,   8.341424f,
        -13.102200f,  -0.758075f,   -2.641225f,   32.454521f,   -0.353416f,   -77.004990f,
        37.227680f,   72.467278f,   3.472405f,    -18.478279f,  -37.838223f,  -30.410454f,
        18.927309f,   8.065634f,    37.695267f,   -2.802967f,   -285.919983f, 85.342056f,
        -116.107925f, 41.618042f,   32.768738f,   -24.778553f,  17.136639f,   -12.088976f,
        0.000000f,    0.000000f,    2.304451f,    7.392092f,    -3.775282f,   28.520403f,
        -16.030426f,  53.147350f,   -23.453283f,  81.682159f,   -441.473877f, 192.422989f,
        -94.576485f,  33.419403f,   52.583290f,   -39.492382f,  34.225212f,   -29.093567f,
        0.000000f,    0.000000f,    -19.824623f,  29.042471f,   -30.338154f,  54.156662f,
        -67.032883f,  106.129936f,  -100.292976f, 155.199036f,  319.273438f,  -211.617355f,
        86.794067f,   -80.723343f,  -126.214584f, 80.034996f,   -67.389305f,  30.924875f,
        0.000000f,    0.000000f,    47.781631f,   -24.417738f,  90.698029f,   -52.816177f,
        203.358444f,  -176.368362f, 300.789124f,  -253.474335f, 2.363739f,    -0.150783f,
        0.000000f,    0.000000f,    -0.198428f,   2.555548f,    5.358450f,    3.029869f,
        3.111690f,    3.750197f,    2.376972f,    3.120378f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        -9.108178f,   -30.085722f,  -7.711772f,   -10.590985f,  -6.161524f,   0.053987f,
        -5.664951f,   2.182161f,    -3.445077f,   2.595459f,    0.000000f,    0.000000f,
        0.303945f,    -0.355978f,   -3.253546f,   -1.498292f,   106.409569f,  -29.638285f,
        9.526237f,    -33.832603f,  10.757391f,   -15.134309f,  7.630887f,    -7.533637f,
        4.790431f,    -1.732493f,   1.954534f,    1.726391f,    0.000000f,    0.000000f,
        -6.208316f,   0.442114f,    -1.072606f,   -4.729187f,   13.831474f,   -15.638452f,
        12.646847f,   -32.828251f,  8.352793f,    -16.240774f,  6.762643f,    -9.332367f,
        4.968950f,    -5.698806f,   -0.222172f,   -0.767161f,   0.000000f,    0.000000f,
        -0.441844f,   -1.352003f,   1.570241f,    -4.697210f,   5.200654f,    -11.493258f,
        2.197022f,    -136.604904f, 5.182918f,    -101.336655f, 6.040205f,    -47.811783f,
        3.328042f,    -34.562656f,  2.559453f,    -14.873818f,  0.000000f,    0.000000f,
        -0.489519f,   -1.682172f,   -2.633943f,   4.120424f,    -3.842442f,   10.717678f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        6.450372f,    -0.879188f,   2.633566f,    1.470047f,    1.823425f,    -0.085315f,
        1.766351f,    0.392712f,    2.245134f,    0.458609f,    5.911775f,    -1.910818f,
        4.755805f,    1.194220f,    6.159865f,    12.633673f,   -3.804467f,   5.588072f,
        -4.009410f,   -3.581388f,   0.580247f,    1.121452f,    -2.824405f,   -0.261475f,
        4.847287f,    -5.054276f,   4.150414f,    -2.857911f,   -3.889671f,   -1.576343f,
        -2.617653f,   -4.936631f,   -12.380523f,  -6.696168f,   0.183982f,    -7.635507f,
        20.000000f,   10.000000f,   -2.800315f,   -8.401645f,   -8.254324f,   -44.469917f,
        6.718768f,    -18.700714f,  7.863876f,    5.113727f,    -41.078709f,  -34.050011f,
        -2.076724f,   -13.455810f,  5.102230f,    12.120109f,   -41.709763f,  -31.621685f,
        -15.175555f,  0.935218f,    3.778532f,    11.110825f,   -50.235966f,  -7.017627f,
        -3.557699f,   4.840312f,    3.060509f,    27.971371f,   -68.548233f,  -22.069857f,
        -45.404331f,  12.312299f,   23.184231f,   -15.823175f,  -20.777229f,  -44.221897f,
        29.718155f,   -39.636272f,  0.000000f,    0.000000f,    29.520617f,   -41.046127f,
        30.976528f,   -42.315220f,  43.999508f,   -46.393772f,  69.714989f,   -57.650848f,
        82.824234f,   -63.718292f,  141.641525f,  -84.579811f,  221.791489f,  -109.952057f,
        209.807755f,  -95.766586f,  0.000000f,    0.000000f,    -54.135204f,  40.184338f,
        -35.644104f,  35.303459f,   -27.839844f,  35.831509f,   -36.372925f,  36.332394f,
        -33.746796f,  40.617439f,   0.000000f,    0.000000f,    0.000000f,    0.000000f,
        4.994258f,    0.273304f,    58.824749f,   -37.718826f,  127.501122f,  -75.651398f,
        211.449387f,  -116.341125f, 307.007263f,  -159.175949f, -10.057157f,  2.955684f,
        -13.388169f,  -47.377266f,  -20.795740f,  -50.887608f,  -53.347973f,  -7.603198f,
        -33.670944f,  2.828870f,    -15.845585f,  15.356581f,   -8.436491f,   5.685603f,
        -22.615082f,  33.446903f};

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
