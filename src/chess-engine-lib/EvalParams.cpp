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
        0.209377f,    1.076343f,    0.289272f,    0.205206f,    0.440056f,    1.167871f,
        1.000000f,    -0.014401f,   0.000000f,    -0.030289f,   -0.002014f,   53.434738f,
        8.434715f,    157.220001f,  378.414612f,  384.850159f,  348.535339f,  419.725830f,
        572.239624f,  715.155945f,  1361.025879f, 1226.828491f, 0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    -26.901800f,  154.859573f,
        -28.557764f,  158.393356f,  -19.934332f,  162.151505f,  -36.722931f,  154.048508f,
        -20.648687f,  186.495193f,  31.511883f,   161.857132f,  30.135395f,   154.686249f,
        -8.067615f,   143.792160f,  -16.540142f,  150.800430f,  -13.024509f,  151.427368f,
        -4.485261f,   153.997284f,  -11.024583f,  164.629562f,  -4.822239f,   162.627579f,
        14.391195f,   158.112839f,  20.718084f,   148.474411f,  8.126451f,    145.469940f,
        1.719573f,    163.480209f,  9.035787f,    162.742737f,  18.085806f,   160.271820f,
        24.993715f,   158.036377f,  24.667124f,   162.346985f,  37.797073f,   155.874985f,
        24.124138f,   160.401031f,  8.770669f,    157.430237f,  28.893133f,   176.990265f,
        29.539164f,   178.213684f,  29.474165f,   170.213654f,  46.608761f,   158.329956f,
        63.592926f,   162.291565f,  54.573662f,   154.058868f,  39.679592f,   168.382233f,
        39.963375f,   160.956894f,  64.693985f,   184.813263f,  41.435665f,   203.319397f,
        70.890175f,   187.353821f,  22.106544f,   187.910751f,  69.996170f,   146.180511f,
        133.426712f,  180.331055f,  123.647774f,  181.058548f,  87.803970f,   182.161652f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    325.755005f,  337.882690f,  329.183685f,  371.562592f,
        322.549469f,  377.980164f,  348.515564f,  368.011566f,  344.572632f,  362.518402f,
        350.232269f,  351.417908f,  321.764008f,  365.098328f,  302.849762f,  282.049408f,
        341.881500f,  339.314636f,  349.293549f,  365.837463f,  355.181244f,  383.367920f,
        367.368896f,  383.382019f,  359.101410f,  376.073944f,  369.544067f,  365.852264f,
        346.837738f,  358.579834f,  358.965912f,  325.737579f,  351.458710f,  370.297455f,
        366.894073f,  373.100952f,  372.792023f,  382.994324f,  391.502014f,  395.874939f,
        401.029205f,  390.811127f,  378.127075f,  368.904968f,  378.588287f,  353.645691f,
        364.260284f,  369.464569f,  372.604706f,  370.683899f,  423.509430f,  384.690491f,
        415.560577f,  400.852234f,  400.346649f,  404.390076f,  410.758057f,  417.460205f,
        406.308228f,  402.358093f,  406.248688f,  378.345154f,  366.540375f,  372.640411f,
        395.387695f,  388.903290f,  405.153503f,  392.410217f,  436.142853f,  404.145599f,
        462.999969f,  400.097473f,  425.429779f,  401.739014f,  450.502930f,  398.717072f,
        415.108521f,  399.096527f,  418.940826f,  375.461456f,  372.648254f,  391.666016f,
        434.901794f,  384.914032f,  483.340790f,  379.162933f,  469.118378f,  394.020264f,
        530.747864f,  374.790802f,  467.317657f,  398.084625f,  451.683044f,  371.170013f,
        402.422485f,  362.497711f,  429.260742f,  369.528625f,  375.062653f,  395.178314f,
        423.093048f,  382.708618f,  459.784058f,  402.930481f,  456.487061f,  379.358673f,
        421.759247f,  382.336670f,  417.592285f,  386.707550f,  367.705597f,  372.184845f,
        192.112732f,  407.265961f,  280.889740f,  433.839508f,  350.155151f,  402.132721f,
        410.008636f,  413.386383f,  338.600525f,  414.597931f,  359.478546f,  400.200562f,
        524.214539f,  316.749603f,  319.508545f,  290.317535f,  325.544739f,  380.077606f,
        339.640747f,  407.044769f,  320.773499f,  410.100494f,  318.038971f,  414.331299f,
        333.155762f,  403.042969f,  315.826996f,  417.693390f,  298.957794f,  406.227081f,
        297.795227f,  382.603516f,  356.845245f,  393.351990f,  340.329987f,  395.600037f,
        357.605164f,  407.867859f,  333.395752f,  418.387024f,  348.591095f,  406.351746f,
        348.555206f,  406.777985f,  365.204620f,  393.150574f,  338.885498f,  353.769928f,
        330.995361f,  417.391418f,  367.066010f,  415.438904f,  354.632355f,  431.235992f,
        358.990723f,  421.142212f,  356.273315f,  426.905151f,  357.498260f,  420.152313f,
        360.001770f,  410.416138f,  354.443237f,  405.688721f,  342.711487f,  428.457672f,
        348.294586f,  426.772003f,  357.506531f,  428.250977f,  367.005859f,  438.428650f,
        374.690948f,  426.242126f,  355.770538f,  430.297302f,  356.854401f,  424.761139f,
        351.946228f,  401.078674f,  337.425598f,  423.613678f,  355.731110f,  433.697174f,
        369.763123f,  439.994873f,  402.535583f,  425.987000f,  385.036224f,  427.859650f,
        378.795929f,  438.407074f,  359.418030f,  426.742493f,  352.718079f,  425.019318f,
        358.029083f,  421.641754f,  362.480682f,  438.789642f,  366.763489f,  434.861206f,
        401.023224f,  421.148499f,  396.643036f,  431.581299f,  429.361359f,  428.528412f,
        459.703369f,  421.110199f,  384.215820f,  416.160339f,  370.388184f,  418.729919f,
        356.060059f,  444.053619f,  388.317017f,  426.395966f,  354.575195f,  452.256134f,
        385.268646f,  418.576752f,  445.313293f,  414.647125f,  315.416687f,  442.407990f,
        321.487122f,  401.812836f,  355.200104f,  420.319611f,  361.191986f,  449.141510f,
        321.702026f,  438.886566f,  325.490875f,  443.862000f,  343.172028f,  440.196198f,
        364.533203f,  416.370911f,  322.462769f,  406.130920f,  433.506958f,  403.215332f,
        583.532471f,  689.057922f,  578.883850f,  691.996460f,  577.400696f,  695.399963f,
        572.701904f,  692.335632f,  573.361145f,  690.489868f,  562.956055f,  695.682129f,
        550.445007f,  695.177063f,  546.947083f,  682.116211f,  557.582397f,  706.484253f,
        574.471313f,  696.282166f,  570.139832f,  708.191040f,  560.319885f,  703.869324f,
        561.118103f,  711.540222f,  571.145508f,  690.329895f,  550.707214f,  704.262268f,
        515.610046f,  716.101318f,  558.287231f,  711.068359f,  557.469666f,  714.614624f,
        552.041016f,  715.882324f,  549.226501f,  720.060852f,  559.353577f,  711.377319f,
        541.191650f,  718.313293f,  555.516846f,  707.040955f,  531.505981f,  714.256165f,
        559.303162f,  729.526306f,  557.950500f,  732.670593f,  556.674194f,  732.899231f,
        542.432007f,  735.343018f,  552.719971f,  727.453491f,  554.757996f,  735.611633f,
        534.638733f,  744.526611f,  523.198669f,  732.579285f,  558.564941f,  746.919189f,
        582.520203f,  734.419373f,  597.234253f,  726.917664f,  589.613953f,  728.406433f,
        590.518738f,  722.325134f,  573.027100f,  733.893982f,  578.310242f,  739.987366f,
        552.771790f,  738.456848f,  570.627258f,  749.495422f,  603.418152f,  737.173645f,
        609.478027f,  733.120605f,  609.217102f,  725.010132f,  631.794434f,  726.123718f,
        647.661987f,  725.849426f,  649.688660f,  727.667603f,  605.291870f,  749.052673f,
        593.222351f,  752.391174f,  581.819397f,  758.322815f,  628.697632f,  742.980652f,
        626.474487f,  739.605408f,  604.157227f,  753.552856f,  658.691956f,  721.821289f,
        669.817688f,  727.723572f,  610.042725f,  743.026672f,  600.614380f,  740.360596f,
        629.801697f,  737.551392f,  600.617554f,  745.921570f,  582.024292f,  747.483032f,
        585.234314f,  752.886475f,  572.702576f,  755.987610f,  624.425232f,  749.269470f,
        633.308716f,  737.892090f,  1387.614990f, 1121.727173f, 1362.875000f, 1174.427490f,
        1359.167480f, 1180.869629f, 1350.784546f, 1179.777466f, 1345.564575f, 1172.983887f,
        1316.110840f, 1180.505981f, 1323.902222f, 1135.896240f, 1355.989746f, 1127.225342f,
        1368.446655f, 1181.528076f, 1381.247559f, 1146.665771f, 1376.900513f, 1166.056519f,
        1367.921509f, 1172.210571f, 1362.159424f, 1196.183105f, 1366.145996f, 1137.725342f,
        1382.391968f, 1081.973633f, 1325.743652f, 1160.617188f, 1368.835571f, 1169.330933f,
        1369.065552f, 1217.742310f, 1350.596924f, 1236.654419f, 1358.768799f, 1219.873291f,
        1355.801514f, 1228.815918f, 1354.765869f, 1245.876953f, 1367.231934f, 1196.831055f,
        1342.350952f, 1253.212769f, 1363.828491f, 1244.716309f, 1360.307495f, 1238.873413f,
        1357.141846f, 1247.836548f, 1352.842896f, 1277.356567f, 1350.285278f, 1294.657104f,
        1347.950439f, 1278.862549f, 1357.994141f, 1253.703613f, 1345.667969f, 1297.193359f,
        1355.375854f, 1272.016602f, 1345.553223f, 1290.961426f, 1356.102051f, 1284.114990f,
        1349.538208f, 1314.791382f, 1354.810059f, 1310.525757f, 1360.857300f, 1307.852173f,
        1367.764160f, 1283.912476f, 1382.799194f, 1267.490112f, 1375.822510f, 1259.274048f,
        1374.163330f, 1234.863525f, 1357.846802f, 1307.576782f, 1322.404297f, 1362.683594f,
        1369.381958f, 1330.992798f, 1426.618896f, 1282.498291f, 1445.218872f, 1197.819824f,
        1392.414307f, 1251.659790f, 1356.618896f, 1264.649170f, 1347.158447f, 1283.863403f,
        1367.857178f, 1269.913818f, 1364.140015f, 1314.497559f, 1347.752441f, 1350.558960f,
        1414.971313f, 1276.887817f, 1404.765381f, 1224.784912f, 1463.267090f, 1195.870117f,
        1397.873901f, 1229.739746f, 1398.476685f, 1218.103271f, 1397.716309f, 1252.477173f,
        1342.638672f, 1314.841553f, 1342.786743f, 1328.942261f, 1451.431641f, 1225.227905f,
        1439.695312f, 1226.101807f, 1361.409790f, 1291.444946f, 62.573441f,   -21.554073f,
        25.794666f,   -9.823074f,   -41.874691f,  5.930475f,    -56.066086f,  -5.362117f,
        -38.430840f,  -19.543320f,  -51.939068f,  10.281198f,   27.205975f,   4.521285f,
        59.183022f,   -14.129595f,  15.857275f,   -13.004870f,  -19.839657f,  -3.145851f,
        -67.379295f,  -0.889235f,   -137.697556f, 14.782084f,   -123.037125f, 5.905639f,
        -79.497437f,  4.697479f,    -6.192287f,   -7.445868f,   25.936169f,   -18.334467f,
        -22.471878f,  -22.426548f,  -63.175400f,  1.927696f,    -120.396637f, 4.116307f,
        -116.631081f, 2.375324f,    -138.402100f, 7.942324f,    -122.883224f, 4.358536f,
        -83.354599f,  -8.029469f,   -48.091614f,  -27.636620f,  -2.189015f,   -12.588163f,
        -1.856959f,   -19.444557f,  -58.883530f,  -9.174749f,   -73.236420f,  -7.803506f,
        -75.007309f,  -8.972227f,   -83.971817f,  -2.861740f,   -46.124413f,  -23.612921f,
        -118.554955f, -18.667725f,  30.153076f,   -7.701854f,   60.457779f,   -15.846788f,
        -16.064138f,  -2.610904f,   56.822186f,   -24.786966f,  -10.849893f,  -7.224929f,
        16.004240f,   -12.112442f,  31.679689f,   -22.909657f,  -102.042503f, -9.116964f,
        55.323708f,   -2.694709f,   116.416771f,  -12.014240f,  71.660019f,   -3.666490f,
        72.078629f,   -7.576032f,   108.663521f,  -30.613323f,  196.240631f,  -21.537992f,
        102.858971f,  3.099440f,    137.160782f,  -13.259955f,  127.311783f,  -14.354298f,
        93.103561f,   7.999971f,    129.422150f,  -2.465499f,   124.285767f,  -29.936749f,
        18.280952f,   -16.584253f,  211.212921f,  -105.501160f, 88.154366f,   35.223679f,
        88.188744f,   27.983503f,   214.057388f,  -92.161308f,  196.744873f,  -52.061691f,
        103.166252f,  -6.622799f,   0.372592f,    11.465233f,   104.574593f,  -53.587917f,
        49.457870f,   -21.979723f,  116.582802f,  -18.635410f,  876.252075f,  -320.718201f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.810053f,    120.607315f,
        -13.793588f,  130.984833f,  24.652424f,   108.033134f,  3.707147f,    149.591904f,
        43.152065f,   122.838379f,  92.504196f,   113.340736f,  76.563004f,   105.149086f,
        54.283321f,   104.669144f,  15.612775f,   138.084183f,  6.708207f,    144.592331f,
        16.324499f,   134.845917f,  29.828495f,   143.890305f,  13.390067f,   157.669571f,
        56.441910f,   127.744881f,  61.370243f,   131.628006f,  24.917057f,   114.845627f,
        32.588707f,   196.287598f,  31.139349f,   171.631073f,  61.921818f,   169.119751f,
        70.079926f,   138.986893f,  99.520348f,   142.454407f,  79.775681f,   164.026398f,
        45.867203f,   153.191071f,  44.399460f,   143.783875f,  77.019310f,   259.163910f,
        55.555344f,   262.541962f,  91.035439f,   233.249908f,  108.246666f,  214.529861f,
        133.484924f,  191.624603f,  180.166992f,  145.531479f,  97.626678f,   187.606720f,
        146.032257f,  183.059158f,  52.778885f,   460.980255f,  127.982384f,  376.597900f,
        113.591293f,  299.672424f,  165.118378f,  270.838776f,  231.571487f,  204.529419f,
        215.509338f,  201.393463f,  124.400673f,  205.810089f,  208.932495f,  256.605133f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        -10.871196f,  102.051628f,  9.050614f,    81.133904f,   -8.817271f,   85.344154f,
        26.745230f,   87.779602f,   18.783709f,   93.396187f,   61.853165f,   44.753647f,
        36.662556f,   55.221146f,   3.279348f,    55.334064f,   19.976730f,   95.662720f,
        63.894955f,   71.634911f,   4.004271f,    93.380020f,   10.182350f,   72.240089f,
        3.441981f,    78.864723f,   23.478014f,   75.356468f,   54.064232f,   78.312424f,
        39.310120f,   62.233700f,   90.177673f,   133.608658f,  120.747696f,  114.910233f,
        62.786983f,   110.509033f,  75.807800f,   95.475670f,   79.593102f,   93.966415f,
        97.746254f,   108.189629f,  54.068905f,   148.629959f,  60.879436f,   124.940849f,
        178.987701f,  200.935715f,  182.705078f,  197.799042f,  160.983139f,  170.907532f,
        158.393311f,  133.151794f,  173.624207f,  121.763016f,  156.934906f,  125.575798f,
        121.980797f,  179.618088f,  141.900269f,  163.191666f,  310.982727f,  272.042511f,
        301.446503f,  280.204926f,  273.269409f,  233.072128f,  268.666901f,  207.886536f,
        309.964996f,  167.687973f,  324.489319f,  153.243866f,  252.280777f,  211.482437f,
        206.459976f,  226.039261f,  538.882324f,  307.123901f,  564.737854f,  285.330902f,
        441.890808f,  307.081268f,  469.302002f,  234.029663f,  480.878906f,  200.352768f,
        437.407959f,  200.878357f,  157.365646f,  311.917389f,  180.693954f,  299.041901f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    15.995199f,   9.218275f,
        26.251064f,   42.760502f,   15.550387f,   22.078360f,   9.258748f,    16.668148f,
        -0.319864f,   -0.316068f,   -0.183558f,   -0.168217f,   -0.068560f,   0.076273f,
        27.272194f,   12.391487f,   38.635990f,   -3.703240f,   26.400440f,   -1.934814f,
        13.363708f,   -1.281385f,   0.000000f,    0.000000f,    -14.488242f,  0.409596f,
        -26.552896f,  -8.681839f,   -46.705334f,  24.381340f,   -60.136536f,  -235.762512f,
        33.564507f,   2.692131f,    36.153027f,   -13.971533f,  18.980637f,   -3.506297f,
        9.101913f,    5.616842f,    -0.000000f,   0.000000f,    -12.805017f,  6.739514f,
        -19.460924f,  11.147652f,   -39.339848f,  87.491295f,   -55.938190f,  -60.000492f,
        33.437695f,   78.394173f,   0.360917f,    -13.480748f,  -63.262901f,  -9.893426f,
        21.407200f,   3.644926f,    38.037533f,   0.910959f,    18.719826f,   15.560489f,
        30.923983f,   -24.297028f,  -18.210588f,  -53.416611f,  8.088325f,    17.334364f,
        -220.412674f, 74.712524f,   -104.855751f, 30.021255f,   28.704042f,   -27.929953f,
        7.845366f,    -9.569007f,   0.000000f,    0.000000f,    1.331635f,    9.901533f,
        1.187176f,    32.059830f,   -7.010911f,   66.921410f,   -26.276588f,  124.684158f,
        -457.947998f, 188.411438f,  -136.294312f, 48.296375f,   27.931808f,   -29.541040f,
        27.231268f,   -29.111547f,  0.000000f,    0.000000f,    -12.179718f,  27.624092f,
        -20.158928f,  53.574612f,   -63.209229f,  110.068367f,  -181.515533f, 252.874908f,
        141.422806f,  -65.718369f,  73.959167f,   -84.805115f,  -78.478828f,  18.312731f,
        -51.894444f,  0.194867f,    0.000000f,    0.000000f,    53.745148f,   -26.323214f,
        103.988800f,  -49.868523f,  244.716110f,  -196.683517f, 544.368347f,  -554.398376f,
        2.348424f,    -0.286663f,   0.000000f,    0.000000f,    -0.356763f,   2.637790f,
        5.216510f,    3.704036f,    3.236266f,    3.775397f,    2.261218f,    1.497835f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    -11.471704f,  -29.887945f,  -10.567382f,  -5.625606f,
        -8.525598f,   2.688660f,    -7.461721f,   3.690645f,    -4.887341f,   5.115874f,
        0.000000f,    0.000000f,    -0.019455f,   1.328997f,    -3.649420f,   -1.986652f,
        71.098465f,   -20.735052f,  13.039860f,   -37.875904f,  12.092216f,   -13.523175f,
        10.276090f,   -8.031583f,   7.561790f,    -2.836464f,   5.432786f,    -0.975800f,
        0.000000f,    0.000000f,    -9.092605f,   -0.233079f,   -1.821573f,   -6.520579f,
        25.044355f,   -17.696148f,  14.728454f,   -36.048031f,  9.409523f,    -14.427297f,
        7.795051f,    -10.139802f,  5.640620f,    -5.544425f,   0.807529f,    -2.311257f,
        0.000000f,    0.000000f,    -1.738300f,   -1.084060f,   -0.092905f,   -3.490236f,
        -0.284832f,   -7.969565f,   0.263611f,    -110.816299f, 3.850776f,    -51.956493f,
        4.423283f,    -34.592461f,  4.486696f,    -27.340931f,  1.520459f,    -6.429058f,
        0.000000f,    0.000000f,    -1.427672f,   6.089958f,    -3.924454f,   17.800962f,
        -1.738794f,   21.182980f,   0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    6.882706f,    -0.620651f,   2.231821f,    1.433192f,
        2.406496f,    -0.217716f,   2.045115f,    0.818669f,    3.275468f,    -3.389531f,
        6.376971f,    -0.850521f,   4.351185f,    2.379820f,    7.314596f,    13.037576f,
        -1.319218f,   7.244008f,    -2.958874f,   -4.304744f,   0.759196f,    1.630784f,
        -2.529541f,   -0.539501f,   5.076118f,    -5.077440f,   4.406502f,    -5.722279f,
        -2.086706f,   -2.459658f,   -0.505389f,   -6.648451f,   -7.777756f,   -7.207444f,
        1.286669f,    -8.910544f,   20.000000f,   10.000000f,   -1.382460f,   -7.465575f,
        -8.353492f,   -44.474033f,  8.142933f,    -19.583761f,  8.088311f,    4.196855f,
        -41.702801f,  -34.547386f,  -3.647498f,   -10.410069f,  4.544261f,    11.300138f,
        -43.078991f,  -34.749004f,  -14.017091f,  -3.762661f,   4.004437f,    9.287892f,
        -53.384666f,  -4.685743f,   -6.539225f,   4.545390f,    3.287230f,    32.361725f,
        -66.077492f,  -36.556297f,  -50.331955f,  17.405788f,   0.000000f,    0.000000f,
        33.395336f,   -46.795906f,  33.593876f,   -45.235355f,  42.249775f,   -46.238514f,
        72.137787f,   -59.881256f,  91.098923f,   -68.725594f,  153.084991f,  -90.741333f,
        253.384491f,  -112.656235f, 156.800781f,  -65.480721f,  0.000000f,    0.000000f,
        -56.848042f,  44.944820f,   -37.679321f,  37.876301f,   -32.654770f,  38.838543f,
        -39.358479f,  38.140015f,   -41.406372f,  54.058697f,   0.000000f,    0.000000f,
        0.000000f,    0.000000f,    6.203648f,    0.498146f,    66.524620f,   -41.611641f,
        137.429642f,  -81.295349f,  229.048065f,  -124.726685f, 310.923859f,  -169.794937f,
        -10.594360f,  3.711615f,    -18.819874f,  -49.967670f,  -22.758699f,  -46.290184f,
        -53.036427f,  -14.438382f,  -35.193661f,  -6.063499f,   -15.987329f,  12.671880f,
        -10.286136f,  6.559554f,    -19.805605f,  35.308270f};

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
            "\npasserMechanicalObstructionFactor:\n\t{}",
            taperedTermToString(params.passerMechanicalObstructionFactor));

    oss << std::format(
            "\npasserDynamicObstructionFactor:\n\t{}",
            taperedTermToString(params.passerDynamicObstructionFactor));

    oss << std::format(
            "\npasserSacrificialOstructionFactor:\n\t{}",
            taperedTermToString(params.passerSacrificialOstructionFactor));

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

    oss << std::format(
            "\nownRookBehindPassedPawn:\n\t{}",
            taperedTermToString(params.ownRookBehindPassedPawn));

    oss << std::format(
            "\nownRookInFrontOfPassedPawn:\n\t{}",
            taperedTermToString(params.ownRookInFrontOfPassedPawn));

    oss << std::format(
            "\nenemyRookBehindPassedPawn:\n\t{}",
            taperedTermToString(params.enemyRookBehindPassedPawn));

    oss << std::format(
            "\nenemyRookInFrontOfPassedPawn:\n\t{}",
            taperedTermToString(params.enemyRookInFrontOfPassedPawn));

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
                pieceToString((Piece)pieceIdx),
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
