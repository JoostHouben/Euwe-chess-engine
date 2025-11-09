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
        0.209377f,    1.076343f,    0.308827f,    0.225177f,    0.378976f,    1.000000f,
        -0.011059f,   -0.000000f,   -0.016250f,   -0.003909f,   80.412956f,   -17.454321f,
        161.923981f,  328.124695f,  421.654572f,  289.275360f,  458.890533f,  439.383789f,
        787.124695f,  1080.388062f, 1312.905762f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -47.213196f,  161.497406f,  -45.592075f,
        155.889664f,  -42.646397f,  169.433853f,  -54.397861f,  172.731354f,  -46.858749f,
        173.257034f,  -1.177469f,   169.418381f,  1.300182f,    157.591568f,  -26.708334f,
        143.924194f,  -39.538921f,  153.207306f,  -36.284260f,  153.887924f,  -31.209652f,
        169.036606f,  -30.629467f,  181.539688f,  -28.867529f,  171.639328f,  -7.288515f,
        166.372177f,  -4.740595f,   153.521255f,  -16.960522f,  150.365875f,  -28.438509f,
        162.276367f,  -16.364897f,  165.016327f,  -12.088998f,  167.760513f,  -4.013669f,
        167.610397f,  0.507557f,    161.619217f,  9.731815f,    160.009628f,  -9.444455f,
        170.033615f,  -12.939041f,  158.169891f,  -5.095757f,   175.432693f,  -2.410949f,
        172.712463f,  2.242137f,    165.343964f,  4.787511f,    177.110123f,  26.102959f,
        164.242676f,  25.559586f,   169.525818f,  5.878963f,    169.888504f,  -0.364020f,
        163.097977f,  3.057463f,    217.686508f,  -2.202029f,   232.054337f,  12.458032f,
        194.329849f,  47.477173f,   105.944939f,  58.926556f,   177.268784f,  68.086685f,
        192.720123f,  140.745239f,  185.846909f,  46.204006f,   155.111252f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    229.415726f,  392.842285f,  289.916901f,  392.360474f,  274.752258f,
        419.889008f,  289.750427f,  411.647034f,  285.071899f,  395.799713f,  297.767456f,
        403.579346f,  280.021667f,  395.202271f,  249.530319f,  374.111572f,  293.144287f,
        367.181122f,  309.644470f,  407.064789f,  303.913208f,  398.591125f,  322.581299f,
        413.397430f,  317.204834f,  409.255005f,  301.347107f,  409.481720f,  301.455597f,
        416.559662f,  302.156677f,  370.642395f,  300.673981f,  370.534851f,  317.655304f,
        422.980835f,  328.095093f,  416.012878f,  336.822937f,  431.620209f,  347.490356f,
        422.818939f,  329.606354f,  411.055328f,  328.397675f,  412.750397f,  302.166595f,
        386.488708f,  324.542542f,  410.968323f,  333.535309f,  413.538513f,  356.979980f,
        433.152985f,  348.935974f,  438.252899f,  356.021942f,  444.505493f,  348.117645f,
        431.705200f,  346.199829f,  425.200775f,  315.526794f,  421.384125f,  337.743103f,
        423.814484f,  359.477814f,  442.394196f,  363.242126f,  437.477936f,  367.226868f,
        448.239929f,  365.006897f,  428.997467f,  378.829407f,  432.091309f,  352.807617f,
        429.816589f,  347.308289f,  411.206696f,  329.501709f,  417.505127f,  359.683319f,
        428.742157f,  376.783325f,  421.942352f,  379.861938f,  438.234711f,  422.089478f,
        421.617004f,  407.078857f,  436.391693f,  381.626892f,  425.701294f,  283.695496f,
        413.867645f,  317.050629f,  452.301422f,  337.830078f,  439.826935f,  374.342743f,
        429.637268f,  407.855103f,  417.616425f,  354.514526f,  425.456940f,  387.569580f,
        421.791473f,  314.554718f,  428.453796f,  338.188293f,  392.667542f,  201.602737f,
        395.913666f,  254.681824f,  421.388123f,  348.191406f,  429.664795f,  296.204407f,
        453.698547f,  321.153137f,  444.675293f,  463.321106f,  392.303284f,  414.047089f,
        368.838287f,  163.483002f,  326.667725f,  264.713318f,  432.320465f,  261.001801f,
        440.711700f,  270.262268f,  448.312469f,  261.228668f,  449.323792f,  253.610687f,
        466.155579f,  263.265900f,  448.748810f,  263.612762f,  435.314941f,  237.572556f,
        427.326782f,  280.958313f,  443.251312f,  279.681061f,  432.582947f,  294.501282f,
        439.464417f,  276.084442f,  466.542816f,  284.982086f,  457.592072f,  296.731567f,
        454.626495f,  298.095581f,  439.464386f,  291.207092f,  393.064056f,  284.280273f,
        444.456268f,  294.469849f,  445.354584f,  293.733307f,  472.175903f,  296.798004f,
        465.694214f,  299.242096f,  469.815460f,  292.183533f,  458.024994f,  306.105133f,
        447.261230f,  285.209595f,  441.087372f,  268.143799f,  458.239471f,  288.555573f,
        458.938477f,  298.292450f,  466.600159f,  311.044922f,  471.018951f,  306.659332f,
        477.038361f,  301.269226f,  460.353149f,  309.252533f,  449.773895f,  291.911804f,
        459.982971f,  271.009735f,  472.006409f,  293.739441f,  472.868408f,  314.222900f,
        467.312134f,  336.556702f,  456.522614f,  318.858917f,  476.400238f,  312.574127f,
        474.118988f,  296.475616f,  467.064880f,  300.466370f,  463.307465f,  296.624542f,
        464.434082f,  318.317657f,  474.967133f,  324.194427f,  453.099274f,  349.139465f,
        461.150879f,  330.670013f,  461.053619f,  402.583466f,  457.830475f,  358.345428f,
        473.724945f,  328.924286f,  456.483856f,  300.531799f,  449.935425f,  310.571564f,
        463.928833f,  328.048035f,  471.088562f,  298.795471f,  470.062195f,  328.829803f,
        461.405701f,  310.686035f,  470.161469f,  356.559540f,  443.483429f,  251.930145f,
        461.632690f,  281.154114f,  460.491486f,  223.222595f,  484.838440f,  284.401611f,
        472.448456f,  279.567505f,  482.184143f,  282.088531f,  470.010559f,  243.335403f,
        469.386871f,  422.438477f,  411.414246f,  240.657303f,  476.835144f,  444.906494f,
        758.067261f,  438.369873f,  761.639893f,  436.704285f,  774.553650f,  439.553223f,
        769.575806f,  440.967255f,  770.675659f,  436.582184f,  773.765320f,  414.125519f,
        775.175903f,  423.284973f,  753.563477f,  424.085754f,  773.879578f,  422.822205f,
        778.989075f,  409.861389f,  792.411987f,  437.573578f,  769.169006f,  431.394409f,
        779.091797f,  443.341797f,  754.035828f,  432.071838f,  760.289551f,  385.133575f,
        790.582031f,  423.849945f,  781.304443f,  435.613281f,  784.036926f,  426.221466f,
        775.890320f,  423.957245f,  787.626648f,  423.121979f,  783.032349f,  408.563324f,
        787.414307f,  436.400085f,  780.375916f,  403.890625f,  779.005798f,  422.198181f,
        809.538635f,  416.695129f,  801.819519f,  429.263062f,  799.080994f,  441.222046f,
        797.597534f,  438.729828f,  806.432861f,  436.100464f,  796.553162f,  438.781250f,
        800.737732f,  410.509705f,  804.932312f,  436.183655f,  816.509705f,  432.129822f,
        812.847351f,  441.322266f,  812.472351f,  445.487701f,  813.087341f,  456.538971f,
        808.070068f,  459.853668f,  809.733032f,  450.516052f,  802.864258f,  447.407257f,
        811.732117f,  454.191132f,  818.502502f,  473.731140f,  805.976990f,  461.719055f,
        810.507019f,  473.281372f,  806.047180f,  495.846313f,  802.337158f,  521.925232f,
        791.302551f,  491.424286f,  799.468323f,  484.604431f,  798.186279f,  452.719574f,
        821.792786f,  476.182831f,  812.462708f,  470.354584f,  818.873718f,  490.302338f,
        813.462708f,  501.067322f,  806.809631f,  497.669861f,  800.196228f,  489.894775f,
        805.347656f,  494.206940f,  803.669495f,  461.495972f,  807.212341f,  474.337891f,
        807.326233f,  469.768982f,  811.971680f,  468.579376f,  808.356812f,  500.960938f,
        812.181641f,  516.343689f,  806.731140f,  426.661133f,  830.403870f,  515.309448f,
        799.488464f,  1091.604736f, 1235.100830f, 1077.838135f, 1266.736694f, 1076.152222f,
        1260.350220f, 1080.860596f, 1237.853149f, 1084.612915f, 1228.365112f, 1040.228638f,
        1277.778076f, 1015.395447f, 1336.329224f, 1016.631104f, 1277.207764f, 1082.651001f,
        1254.935913f, 1090.190796f, 1253.621460f, 1092.070435f, 1241.440796f, 1086.108887f,
        1256.492065f, 1081.358154f, 1273.957520f, 1086.866211f, 1225.188477f, 1083.910767f,
        1139.091431f, 1043.284790f, 1297.253052f, 1079.997437f, 1274.150513f, 1089.698608f,
        1271.940552f, 1075.911987f, 1307.836548f, 1074.195312f, 1311.611450f, 1083.609131f,
        1294.701172f, 1074.579590f, 1327.044922f, 1093.679688f, 1266.632812f, 1068.833862f,
        1351.776489f, 1076.300049f, 1313.931274f, 1098.827026f, 1299.716064f, 1076.069092f,
        1349.688721f, 1059.368042f, 1398.779907f, 1068.377441f, 1354.891479f, 1068.330566f,
        1362.400269f, 1077.853394f, 1350.035522f, 1067.221924f, 1396.476196f, 1068.436279f,
        1378.905518f, 1077.076782f, 1329.887207f, 1059.047363f, 1395.019897f, 1074.579224f,
        1378.947021f, 1060.634521f, 1408.291138f, 1046.636597f, 1415.218994f, 1059.265503f,
        1422.879272f, 1075.932495f, 1381.575439f, 1073.105957f, 1351.831421f, 1081.937134f,
        1335.486572f, 1046.859009f, 1405.636108f, 1063.435181f, 1395.954712f, 1058.299683f,
        1463.812744f, 1087.182007f, 1411.992676f, 1118.437500f, 1332.186646f, 1072.782593f,
        1420.171387f, 1066.411987f, 1355.127441f, 1049.430542f, 1385.285522f, 1074.299561f,
        1387.448608f, 1034.817993f, 1452.226196f, 1039.977295f, 1455.961670f, 1080.592041f,
        1433.316650f, 1131.353638f, 1374.960571f, 1137.435425f, 1352.238403f, 1074.124390f,
        1336.407104f, 1086.772827f, 1332.524170f, 1093.902710f, 1327.977173f, 1094.944458f,
        1380.086548f, 1090.688843f, 1384.712158f, 1100.875732f, 1419.940796f, 1104.944946f,
        1352.260132f, 1117.368530f, 1339.379883f, 63.874348f,   -40.427460f,  26.333586f,
        -18.523830f,  -31.836178f,  -1.846057f,   -55.960354f,  -16.307974f,  -28.349213f,
        -32.079029f,  -55.454651f,  1.263430f,    27.730129f,   -9.764366f,   52.456821f,
        -19.695812f,  27.149240f,   -35.144489f,  -8.218278f,   -11.329773f,  -83.665230f,
        3.302535f,    -117.488426f, 6.445898f,    -127.022743f, 15.318475f,   -67.250748f,
        -2.668272f,   -13.423372f,  -12.429271f,  22.652306f,   -32.033211f,  -9.445594f,
        -34.725155f,  -47.792732f,  -13.016010f,  -118.927879f, 0.402146f,    -126.234650f,
        5.744503f,    -172.476410f, 22.840698f,   -118.033356f, 7.366044f,    -67.803322f,
        -9.982666f,   -37.908699f,  -29.661440f,  -35.625610f,  -21.814264f,  -74.767525f,
        -9.748679f,   -57.387497f,  -8.926126f,   -148.824539f, 12.837277f,   -113.945641f,
        6.697222f,    -145.863556f, 19.600000f,   -92.111252f,  -5.450216f,   -127.276878f,
        -13.373658f,  18.822966f,   -21.099293f,  -38.516365f,  9.529786f,    32.004322f,
        -8.611079f,   -80.776405f,  24.512482f,   -115.213470f, 21.133038f,   -40.802788f,
        11.335975f,   -113.098938f, 27.652548f,   -55.893215f,  -20.021118f,  -18.707682f,
        -5.602057f,   47.951183f,   5.039021f,    52.309204f,   17.462366f,   24.320206f,
        18.748489f,   67.265579f,   0.718271f,    9.699364f,    38.701443f,   27.859135f,
        26.665640f,   32.674934f,   -8.136745f,   -29.624722f,  22.153858f,   12.739102f,
        26.601881f,   45.835838f,   18.668386f,   112.520439f,  9.514459f,    155.204300f,
        -7.793301f,   273.188934f,  -78.630341f,  172.531067f,  11.714004f,   -59.527538f,
        -23.672186f,  -149.615509f, -11.050200f,  216.139740f,  -72.911423f,  93.106094f,
        5.572750f,    78.463936f,   19.683632f,   -7.295587f,   17.702105f,   71.094604f,
        42.734543f,   -24.487572f,  -13.459625f,  152.281326f,  -139.481735f, 0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -29.215851f,  109.632256f,  -10.898806f,
        71.893127f,   5.325773f,    88.608070f,   -1.139472f,   140.524109f,  -2.153089f,
        122.465157f,  60.375107f,   108.477409f,  50.792213f,   82.713577f,   42.649471f,
        43.876930f,   -17.236031f,  131.650513f,  4.099178f,    104.398849f,  13.128595f,
        98.249809f,   -2.557949f,   110.778114f,  13.279663f,   106.384651f,  43.185692f,
        99.902618f,   70.889244f,   66.212097f,   20.990507f,   90.244560f,   18.036919f,
        169.051407f,  27.194992f,   144.890656f,  24.331642f,   136.848251f,  41.898323f,
        129.230621f,  56.663982f,   126.286064f,  54.939384f,   143.666626f,  89.382446f,
        130.008102f,  35.315758f,   123.593834f,  35.053150f,   209.774887f,  43.108017f,
        210.379440f,  46.286442f,   230.200516f,  72.062073f,   162.908905f,  116.704575f,
        142.202362f,  132.036911f,  155.975189f,  81.882149f,   158.355072f,  110.083984f,
        156.770279f,  94.335899f,   418.036102f,  24.234711f,   390.285156f,  39.412750f,
        383.964813f,  57.083775f,   352.315125f,  166.946106f,  191.944031f,  95.612770f,
        177.650345f,  153.257309f,  195.280853f,  155.145279f,  143.434280f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    -67.707832f,
        107.915802f,  -48.183140f,  98.323418f,   -68.477158f,  92.884483f,   -61.887497f,
        41.294582f,   -92.235229f,  80.061371f,   2.156715f,    55.892315f,   -44.995083f,
        64.429604f,   -75.571945f,  74.095680f,   -41.442112f,  107.212547f,  -44.492962f,
        121.696991f,  -39.610378f,  95.415619f,   -72.620674f,  109.190216f,  -75.997574f,
        115.647865f,  -57.727112f,  104.793045f,  14.563309f,   75.097969f,   -23.908813f,
        76.989761f,   29.900021f,   156.687210f,  48.392132f,   136.889252f,  1.894067f,
        125.758011f,  10.016458f,   115.366714f,  17.944405f,   112.539215f,  -0.083206f,
        143.485901f,  0.378476f,    162.999786f,  -7.407817f,   134.182663f,  106.977562f,
        224.655045f,  108.043083f,  207.644547f,  95.249504f,   181.022095f,  105.369499f,
        148.061218f,  120.765839f,  133.672379f,  75.712662f,   174.936066f,  97.915642f,
        173.937698f,  56.521141f,   182.623764f,  214.285156f,  292.252777f,  222.784744f,
        297.603241f,  227.245544f,  255.016968f,  186.901169f,  228.720688f,  203.704224f,
        185.487686f,  292.107910f,  163.472107f,  121.980423f,  252.473328f,  130.029358f,
        241.479904f,  432.252167f,  320.960449f,  435.077637f,  327.766876f,  356.286499f,
        340.888397f,  422.820099f,  259.250610f,  378.405457f,  269.515045f,  252.533737f,
        242.260605f,  144.749390f,  303.176453f,  4.138278f,    352.209595f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    9.655078f,    7.247312f,    8.974731f,
        56.052986f,   5.365614f,    -12.852739f,  3.429337f,    21.074362f,   -0.398073f,
        -0.322408f,   -0.289420f,   -0.176946f,   -0.172869f,   0.048201f,    32.493027f,
        14.811010f,   34.582943f,   0.971891f,    19.913906f,   -0.309707f,   12.072655f,
        -9.105736f,   0.000000f,    0.000000f,    -12.898877f,  7.084666f,    -25.108620f,
        6.625401f,    -38.987492f,  2.442841f,    -78.072983f,  87.885544f,   25.139494f,
        0.058306f,    25.909227f,   -3.153020f,   13.250717f,   -2.025135f,   8.152024f,
        -5.460071f,   0.000000f,    0.000000f,    -9.785422f,   4.234311f,    -18.882751f,
        4.334150f,    -30.906004f,  29.904491f,   -48.510532f,  83.733376f,   31.063517f,
        68.886658f,   -5.115782f,   -0.170438f,   -13.896008f,  -41.171535f,  15.937536f,
        1.802506f,    33.207886f,   0.783532f,    11.537465f,   19.488554f,   33.117069f,
        -33.303795f,  4.705044f,    -68.486427f,  -2.816320f,   20.255678f,   -171.561035f,
        73.517044f,   -119.285789f, 30.628035f,   17.293207f,   -14.594324f,  -10.041989f,
        -0.104394f,   0.000000f,    0.000000f,    -13.932468f,  23.395611f,   -15.087392f,
        44.091812f,   -23.338312f,  55.183136f,   -33.363075f,  96.918564f,   -279.829132f,
        122.140045f,  -116.553627f, 40.646084f,   19.369818f,   -32.448349f,  9.291245f,
        -20.822176f,  0.000000f,    0.000000f,    -8.634049f,   16.595760f,   -12.267843f,
        35.246685f,   -37.137119f,  86.465614f,   -30.181089f,  79.162033f,   7.516586f,
        -44.493591f,  49.499580f,   -113.828163f, -112.765968f, 32.750553f,   -36.935856f,
        -11.341792f,  0.000000f,    0.000000f,    79.598091f,   -29.287344f,  147.476578f,
        -64.300407f,  271.213257f,  -176.208710f, 329.586121f,  -152.518448f, -0.188957f,
        -0.535289f,   5.285484f,    0.973909f,    3.262770f,    1.869742f,    1.617399f,
        2.281352f,    -4.598658f,   -39.402668f,  -2.751006f,   -16.857145f,  -1.621533f,
        -7.036618f,   -1.974611f,   -1.438368f,   0.000000f,    0.000000f,    2.591510f,
        -0.828979f,   3.362159f,    1.734011f,    15.509988f,   -9.217737f,   -52.006165f,
        -1.003039f,   12.186792f,   -48.332916f,  9.940920f,    -22.414530f,  6.435610f,
        -10.953130f,  3.340228f,    -5.018238f,   0.000000f,    0.000000f,    -7.476057f,
        4.914144f,    -11.244683f,  6.690252f,    -3.303079f,   1.700348f,    16.737753f,
        -7.386901f,   14.888161f,   -42.441975f,  9.213818f,    -15.217538f,  7.818496f,
        -8.891167f,   6.707756f,    -6.008304f,   0.000000f,    0.000000f,    1.146016f,
        1.862860f,    -0.796535f,   3.115655f,    -4.072497f,   4.635800f,    2.174723f,
        -0.380351f,   -3.092404f,   -74.721458f,  -2.914968f,   -55.568348f,  2.467548f,
        -37.283257f,  -0.245323f,   -4.373200f,   0.000000f,    0.000000f,    -0.426522f,
        5.754639f,    -1.080206f,   11.172441f,   -3.873011f,   19.201523f,   -4.535117f,
        21.382025f,   5.621593f,    -1.047754f,   2.211556f,    2.044665f,    2.464547f,
        -0.308947f,   1.529886f,    -0.285056f,   1.463032f,    0.164829f,    4.154473f,
        -1.919161f,   4.431711f,    2.014823f,    8.913421f,    11.402205f,   -0.355626f,
        5.141657f,    -2.287160f,   -3.412381f,   0.700157f,    -0.802985f,   -2.782133f,
        -1.488652f,   3.078783f,    -5.518761f,   3.818156f,    -11.805265f,  -1.301656f,
        -5.027556f,   -0.688782f,   -4.640752f,   -7.486928f,   -5.621869f,   -0.930965f,
        -2.513974f,   20.000000f,   10.000000f,   0.781101f,    -7.043667f,   -8.768914f,
        -42.337967f,  9.479958f,    -17.820400f,  7.921009f,    0.635145f,    -33.860180f,
        -26.390270f,  -1.165364f,   -15.891221f,  1.926964f,    13.338269f,   -43.012882f,
        -30.190662f,  -15.305948f,  -2.729789f,   6.329795f,    9.620350f,    -50.356705f,
        -10.371560f,  -3.616051f,   2.817048f,    3.111054f,    34.931667f,   -57.964535f,
        -42.697674f,  -45.574806f,  5.570175f,    0.000000f,    0.000000f,    -8.144269f,
        -6.777083f,   -7.500835f,   -5.659959f,   3.912041f,    -6.006593f,   10.753245f,
        -8.276286f,   24.617039f,   -14.000747f,  59.054951f,   -20.380726f,  76.826530f,
        -8.716131f,   167.623123f,  -36.816769f,  3.938804f,    -1.762387f,   82.345047f,
        -9.261501f,   40.687935f,   -7.445706f,   45.417721f,   11.101181f,   37.000751f,
        14.240237f,   0.130049f,    -1.741371f,   0.207628f,    -0.677995f,   0.303638f,
        -0.171088f,   0.482680f,    0.366493f,    0.727220f,    0.389138f,    1.000000f,
        1.000000f,    2.217151f,    2.694721f,    -12.766429f,  3.917675f,    -26.746548f,
        -36.362137f,  -22.125725f,  -58.862915f,  -80.383369f,  -13.953377f,  -22.425261f,
        -24.098818f,  -8.447374f,   5.438633f,    -2.784477f,   1.389958f,    -18.713186f,
        36.786774f,   0.000000f,    0.000000f,    -3.188546f,   -1.230379f,   -10.878311f,
        -7.541315f,   -20.522896f,  -10.124166f,  -30.338669f,  -21.238016f,  -61.745365f,
        17.990772f,   -107.475395f, 61.901497f,   0.000000f,    0.000000f,    -20.265484f,
        -8.766071f,   -52.804287f,  -13.093475f,  -78.997665f,  -16.047379f,  -155.166763f,
        52.408638f,   -152.562637f, 36.641510f,   -218.798981f, 125.426094f,  0.361345f,
        -1.221358f,   1.822060f,    -0.771326f,   -14.252586f,  -8.740335f,   -7.193029f,
        -9.462902f,   25.488573f,   9.783204f};

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
            "\npasserSacrificialObstructionFactor:\n\t{}",
            taperedTermToString(params.passerSacrificialObstructionFactor));

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

    oss << "\n\nmobilityBonus: {\n";
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.mobilityBonus[pieceIdx - 1]));
    }
    oss << "}\n";

    oss << "\n\nsafeMobilityAdjustment: {\n";
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                arrayToString(params.safeMobilityAdjustment[pieceIdx - 1]));
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
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.kingAttackWeight[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nattackersMinusDefendersFactor:\n\t{}",
            arrayToString(params.attackersMinusDefendersFactor));

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

    oss << std::format(
            "\ndefendedChecksAdjustment:\n\t{}", arrayToString(params.defendedChecksAdjustment));

    oss << std::format(
            "\nundefendedChecksAdjustment:\n\t{}",
            arrayToString(params.undefendedChecksAdjustment));

    oss << std::format(
            "\ncentralPotentialHoleAdjustment:\n\t{}",
            taperedTermToString(params.centralPotentialHoleAdjustment));
    oss << std::format(
            "\nwingPotentialHoleAdjustment:\n\t{}",
            taperedTermToString(params.wingPotentialHoleAdjustment));

    oss << std::format(
            "\ncentralHoleAdjustment:\n\t{}", taperedTermToString(params.centralHoleAdjustment));
    oss << std::format(
            "\nwingHoleAdjustment:\n\t{}", taperedTermToString(params.wingHoleAdjustment));

    oss << std::format(
            "\nknightOnOutpostAdjustment:\n\t{}",
            taperedTermToString(params.knightOnOutpostAdjustment));

    return oss.str();
}
