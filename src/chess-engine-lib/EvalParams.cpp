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
        0.209377f,    1.076343f,    0.309840f,    0.159573f,    0.309500f,    1.000000f,
        -0.000000f,   -0.000000f,   -0.018401f,   0.000637f,    32.736382f,   -26.008022f,
        143.401566f,  332.089355f,  351.503418f,  320.018616f,  397.054626f,  445.094299f,
        677.088135f,  1164.756348f, 1088.177246f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -40.898289f,  143.737305f,  -60.237633f,
        143.901947f,  -60.891544f,  147.563553f,  -74.637367f,  139.465103f,  -64.336540f,
        159.810211f,  -20.376997f,  149.181183f,  -8.462970f,   142.772232f,  -32.070515f,
        137.089233f,  -32.234432f,  141.735947f,  -44.501923f,  136.314957f,  -43.288445f,
        143.130798f,  -48.442627f,  149.929459f,  -39.985565f,  147.167572f,  -17.933727f,
        139.296463f,  -12.075961f,  136.607803f,  -18.006058f,  136.517212f,  -17.302591f,
        145.863602f,  -17.666628f,  144.635803f,  -21.646133f,  148.392746f,  -18.749352f,
        140.256607f,  -10.039629f,  130.407822f,  -4.157259f,   144.969223f,  -12.200633f,
        149.810211f,  -18.084440f,  145.701630f,  -6.849205f,   166.549347f,  2.339614f,
        143.645020f,  -7.313745f,   152.214996f,  -4.517366f,   140.712128f,  8.480530f,
        140.219086f,  20.111444f,   133.597855f,  7.304649f,    148.947235f,  -5.659108f,
        153.289169f,  11.609679f,   168.261856f,  3.572763f,    160.980881f,  20.247343f,
        172.032333f,  12.676826f,   128.890549f,  39.217934f,   125.703484f,  76.829269f,
        169.828796f,  96.927452f,   157.598724f,  34.579853f,   169.183838f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    282.643402f,  281.337524f,  294.614380f,  315.874695f,  290.904785f,
        348.851868f,  297.287231f,  351.805298f,  294.309296f,  349.274628f,  304.089874f,
        329.974365f,  282.765167f,  320.564514f,  246.207703f,  325.522858f,  291.611176f,
        326.516083f,  318.987946f,  336.645966f,  303.362091f,  343.434937f,  324.803986f,
        350.955566f,  320.425079f,  350.145538f,  308.840393f,  361.414185f,  299.587311f,
        360.351318f,  298.564911f,  316.593353f,  299.483124f,  327.081909f,  327.767517f,
        346.648163f,  330.500427f,  340.512268f,  345.881287f,  363.876068f,  348.951996f,
        361.037354f,  334.438995f,  336.947693f,  337.163147f,  323.296600f,  300.617249f,
        325.738007f,  334.454590f,  360.499847f,  367.976715f,  345.107635f,  367.460999f,
        354.115479f,  351.943634f,  358.144104f,  360.704254f,  368.981781f,  354.309235f,
        361.193787f,  340.901093f,  361.445007f,  331.682373f,  349.457458f,  342.539337f,
        360.403107f,  354.014496f,  365.545197f,  372.427460f,  362.339264f,  370.472717f,
        370.832397f,  371.351349f,  358.743347f,  380.339722f,  357.177216f,  355.594299f,
        354.235413f,  358.729065f,  353.061951f,  331.122467f,  369.646301f,  360.630615f,
        375.761658f,  390.235870f,  350.198456f,  390.263550f,  351.608490f,  434.574677f,
        344.443512f,  400.857788f,  350.947693f,  385.997070f,  354.336700f,  319.060760f,
        354.694946f,  312.592834f,  376.339600f,  339.263184f,  364.578827f,  365.413269f,
        359.233307f,  355.528290f,  367.469849f,  381.925720f,  362.931427f,  380.013794f,
        341.513489f,  354.291931f,  353.561249f,  325.800262f,  363.516205f,  186.575455f,
        324.542236f,  275.095673f,  374.305237f,  284.735535f,  399.519165f,  343.236450f,
        378.513550f,  324.079590f,  376.244232f,  469.736877f,  366.670135f,  184.085403f,
        378.755859f,  143.403519f,  213.805511f,  272.091888f,  384.550781f,  318.724945f,
        376.502472f,  296.799316f,  387.351440f,  290.271973f,  391.778839f,  289.857452f,
        385.960419f,  295.721924f,  384.804840f,  311.379486f,  382.503235f,  302.765778f,
        337.147125f,  316.194519f,  390.643127f,  305.371521f,  376.710754f,  323.023102f,
        394.740753f,  306.405212f,  394.778107f,  318.068817f,  396.021149f,  326.002350f,
        388.005493f,  334.883789f,  381.372070f,  329.086792f,  346.938263f,  306.617889f,
        370.998413f,  326.844421f,  393.596802f,  323.581696f,  398.795105f,  328.300201f,
        397.153381f,  330.088593f,  408.298340f,  328.499603f,  396.111084f,  330.389648f,
        382.829620f,  320.457611f,  394.613373f,  321.750977f,  391.380829f,  332.711792f,
        391.758453f,  324.473816f,  406.614502f,  341.456329f,  407.131989f,  343.967773f,
        406.622406f,  329.926178f,  401.666840f,  345.468872f,  392.667480f,  331.381500f,
        393.185242f,  310.782990f,  411.243317f,  323.272034f,  408.632141f,  340.227539f,
        409.910065f,  361.131348f,  400.995178f,  355.536102f,  409.278473f,  344.629333f,
        408.099426f,  327.724640f,  408.146057f,  313.283875f,  400.301849f,  320.812469f,
        407.823975f,  327.475616f,  414.409424f,  333.208801f,  404.088959f,  362.233917f,
        399.996674f,  351.346924f,  414.468323f,  430.211487f,  382.314758f,  376.275818f,
        400.140839f,  366.104126f,  386.834198f,  301.242554f,  400.955383f,  321.369202f,
        412.931824f,  334.127197f,  418.865082f,  303.796234f,  409.035278f,  367.013672f,
        391.391022f,  327.693481f,  407.201813f,  362.455353f,  388.401825f,  311.515045f,
        371.373383f,  285.030151f,  411.780701f,  302.080963f,  410.936096f,  299.286041f,
        409.114014f,  314.003174f,  408.206329f,  291.520325f,  415.780273f,  268.022858f,
        407.284943f,  319.849060f,  375.338257f,  368.904419f,  380.387970f,  455.972992f,
        653.303040f,  448.573914f,  654.897522f,  444.274170f,  665.269897f,  444.137848f,
        660.671936f,  445.573395f,  656.837830f,  438.314178f,  667.239563f,  429.705627f,
        663.497864f,  431.901733f,  645.286438f,  446.815979f,  658.105591f,  438.456390f,
        656.329529f,  431.372742f,  668.668457f,  436.657562f,  667.644470f,  431.193237f,
        671.208191f,  438.994965f,  664.360168f,  431.903137f,  655.007324f,  389.425293f,
        674.173462f,  425.381409f,  677.369934f,  437.735748f,  662.194946f,  425.030823f,
        678.257019f,  427.938049f,  673.370667f,  422.605743f,  673.766663f,  410.860901f,
        683.054749f,  417.147339f,  681.745483f,  431.413635f,  661.767822f,  433.759247f,
        687.690979f,  425.753754f,  694.370483f,  426.066711f,  693.550598f,  428.260254f,
        689.579224f,  425.318726f,  695.645081f,  419.379181f,  695.861328f,  442.448608f,
        693.911133f,  415.290253f,  698.208191f,  440.052612f,  702.985901f,  431.689758f,
        705.169067f,  449.963043f,  693.882324f,  469.982361f,  687.365967f,  454.056580f,
        695.916138f,  460.282135f,  697.079224f,  421.167114f,  705.533569f,  444.352509f,
        697.644226f,  429.908417f,  709.992859f,  465.332947f,  695.772278f,  466.733673f,
        691.817932f,  481.670319f,  687.984619f,  489.318329f,  682.977478f,  490.539429f,
        689.785645f,  499.137695f,  682.713013f,  494.267334f,  694.180603f,  451.380249f,
        706.038208f,  463.074463f,  707.893921f,  488.989990f,  693.470581f,  503.175659f,
        694.247925f,  475.189941f,  699.851074f,  503.348389f,  688.462463f,  466.448029f,
        701.094971f,  469.569580f,  698.793457f,  474.360260f,  683.560364f,  467.279724f,
        696.320190f,  480.298889f,  692.360229f,  455.543976f,  700.371216f,  444.726898f,
        706.247375f,  445.533630f,  710.604004f,  423.368042f,  718.719238f,  512.661011f,
        688.542175f,  1179.166382f, 1008.680847f, 1167.210205f, 1022.976379f, 1158.140015f,
        1037.201782f, 1162.419556f, 1022.607117f, 1170.058838f, 1013.350830f, 1134.808472f,
        1019.786499f, 1147.359253f, 923.870056f,  1129.293335f, 1008.664734f, 1172.287354f,
        1057.765259f, 1178.019775f, 1019.065918f, 1179.235474f, 1037.755127f, 1172.729614f,
        1042.345337f, 1167.354980f, 1049.877930f, 1167.326416f, 1014.014404f, 1166.156860f,
        966.905396f,  1124.244019f, 1084.754517f, 1174.246094f, 1060.323242f, 1168.775146f,
        1076.638794f, 1162.453613f, 1077.660889f, 1161.826904f, 1082.593262f, 1164.455566f,
        1076.500854f, 1163.082275f, 1075.829346f, 1169.891479f, 1051.156982f, 1158.645996f,
        1073.407104f, 1166.367432f, 1105.651001f, 1162.149048f, 1100.893188f, 1162.819092f,
        1122.684937f, 1148.459351f, 1130.553589f, 1165.269897f, 1111.313721f, 1160.735840f,
        1128.926880f, 1154.286377f, 1118.093018f, 1149.634521f, 1156.085938f, 1175.851807f,
        1082.283325f, 1159.316162f, 1122.628052f, 1139.330078f, 1186.276245f, 1139.561768f,
        1155.861816f, 1145.544067f, 1169.654907f, 1151.976074f, 1171.222534f, 1140.131592f,
        1183.098389f, 1156.831177f, 1146.405762f, 1177.835327f, 1107.985352f, 1148.255981f,
        1133.261475f, 1118.593750f, 1202.557495f, 1140.374390f, 1182.197021f, 1131.231567f,
        1217.859741f, 1195.309570f, 1155.116699f, 1216.764160f, 1082.802734f, 1155.469971f,
        1178.286499f, 1153.456543f, 1133.857300f, 1138.240356f, 1150.744385f, 1163.438110f,
        1152.686279f, 1146.235474f, 1172.438232f, 1117.371460f, 1225.510742f, 1135.163086f,
        1175.639038f, 1171.528687f, 1132.611694f, 1190.429810f, 1122.537109f, 1164.138184f,
        1119.551147f, 1193.052124f, 1089.055054f, 1155.685059f, 1168.205322f, 1128.559814f,
        1187.548584f, 1118.056396f, 1222.986084f, 1186.122192f, 1164.523315f, 1195.041626f,
        1141.191650f, 1211.395264f, 1132.872681f, 59.678677f,   -50.743809f,  24.245447f,
        -19.363394f,  -38.522583f,  6.362813f,    -51.822891f,  -15.848815f,  -36.716423f,
        -16.300335f,  -55.816883f,  12.091426f,   25.734241f,   -4.056672f,   47.562347f,
        -16.641378f,  24.837420f,   -36.834003f,  -24.069735f,  -5.313059f,   -81.811325f,
        5.855956f,    -100.891930f, 2.857228f,    -93.750916f,  4.645496f,    -61.894600f,
        2.251159f,    -5.750559f,   -9.894892f,   22.740831f,   -27.986008f,  -30.924553f,
        -26.746668f,  -40.638920f,  -10.889601f,  -114.468872f, 4.674455f,    -111.216667f,
        5.173675f,    -118.206062f, 9.868751f,    -110.219261f, 10.965809f,   -58.478573f,
        -11.503994f,  -36.596153f,  -26.123871f,  -10.923305f,  -30.637253f,  -14.668834f,
        -17.000519f,  -56.476246f,  0.297242f,    -117.336685f, 9.632929f,    -85.195404f,
        3.932366f,    -130.832382f, 12.455312f,   -70.371971f,  -13.513103f,  -86.882599f,
        -35.912876f,  17.241701f,   -28.551346f,  0.826628f,    -5.048874f,   -16.610971f,
        -1.399196f,   -58.777969f,  11.783990f,   -46.550377f,  10.660368f,   -8.455500f,
        -3.691409f,   -26.968756f,  -17.648975f,  -59.391758f,  -22.076818f,  15.532847f,
        8.842030f,    -0.467388f,   23.225330f,   3.725459f,    14.215349f,   14.676474f,
        3.646495f,    75.309212f,   -12.738324f,  37.116722f,   25.259813f,   57.320847f,
        10.569829f,   23.928385f,   -15.685581f,  -1.447901f,   -7.772207f,   103.862267f,
        -1.207351f,   63.325470f,   6.715244f,    72.081062f,   -2.922665f,   -15.211838f,
        1.794212f,    92.523521f,   -20.767952f,  26.517319f,   38.438755f,   149.122375f,
        -86.800674f,  66.435913f,   -53.460854f,  225.955017f,  -88.129662f,  113.761879f,
        -8.354775f,   105.631035f,  -22.173426f,  20.903580f,   -35.662815f,  181.905197f,
        -58.361019f,  -81.196785f,  56.120293f,   162.346130f,  -142.204941f, 0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    23.484179f,   82.604866f,   32.160583f,
        55.070103f,   25.283733f,   73.709618f,   31.846689f,   143.599564f,  40.105396f,
        107.136955f,  100.702705f,  55.060341f,   118.122284f,  57.812576f,   74.753479f,
        77.194633f,   49.206085f,   86.703781f,   17.099451f,   90.932983f,   28.444094f,
        105.538589f,  55.396679f,   55.842068f,   31.067650f,   98.220558f,   74.186035f,
        86.035194f,   115.321350f,  44.406269f,   78.706657f,   63.518986f,   68.616913f,
        140.767303f,  51.788898f,   127.117752f,  58.820248f,   130.183731f,  75.320015f,
        101.275902f,  95.703911f,   101.160149f,  109.295807f,  111.013359f,  97.176544f,
        104.148117f,  95.393639f,   89.966980f,   49.071442f,   255.795883f,  85.121246f,
        234.501755f,  98.893837f,   185.658859f,  119.690063f,  152.671402f,  135.427658f,
        126.213806f,  162.156204f,  142.829346f,  148.155579f,  106.452377f,  191.723618f,
        113.843407f,  134.830902f,  310.647797f,  70.321609f,   307.072449f,  80.121078f,
        280.686523f,  88.351463f,   268.410309f,  207.705368f,  180.059219f,  181.834915f,
        158.798019f,  194.961243f,  153.006195f,  149.139191f,  192.949982f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    -37.247078f,
        83.660301f,   -83.521217f,  99.653389f,   -66.867569f,  85.120819f,   -85.799011f,
        35.600544f,   -78.824890f,  88.627029f,   -21.808643f,  61.906071f,   -10.905979f,
        53.523228f,   -45.190647f,  68.097755f,   -18.759666f,  92.692238f,   -44.621410f,
        84.458351f,   -46.154144f,  90.978760f,   -69.395851f,  65.466705f,   -64.821953f,
        77.056450f,   -11.898496f,  78.222832f,   16.079025f,   60.195934f,   1.543657f,
        64.677559f,   48.824947f,   125.538689f,  39.817863f,   112.674049f,  1.288792f,
        107.840561f,  16.964430f,   83.090599f,   -9.644467f,   92.006256f,   6.648168f,
        111.493874f,  4.847161f,    135.576859f,  20.703926f,   114.175758f,  117.237106f,
        196.714294f,  107.166290f,  175.725281f,  91.644196f,   150.040192f,  100.130791f,
        107.912491f,  115.202919f,  110.217102f,  120.365219f,  125.235695f,  34.211853f,
        171.159622f,  62.472897f,   160.060135f,  217.445572f,  266.380768f,  241.855942f,
        259.722046f,  258.673645f,  213.301056f,  214.592026f,  189.565796f,  285.731384f,
        154.840286f,  331.245056f,  143.724197f,  228.905029f,  188.093628f,  146.099380f,
        216.771240f,  452.676849f,  250.889114f,  426.686493f,  304.253906f,  388.847382f,
        301.526062f,  429.445129f,  223.562180f,  400.931000f,  206.751526f,  257.183258f,
        223.278976f,  177.379654f,  239.030350f,  271.520325f,  242.756302f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    9.756256f,    7.576623f,    25.747154f,
        60.195347f,   28.820280f,   -3.041744f,   3.968162f,    15.912487f,   -0.396687f,
        -0.337150f,   -0.269120f,   -0.171304f,   -0.184780f,   0.083141f,    34.180775f,
        10.823113f,   36.265957f,   1.276067f,    23.910042f,   3.388284f,    14.732830f,
        2.925642f,    0.000000f,    0.000000f,    -12.775789f,  4.900457f,    -24.856188f,
        2.173280f,    -37.418766f,  40.821869f,   -23.063000f,  34.900639f,   28.759346f,
        -0.270591f,   29.757765f,   -10.662509f,  14.848912f,   1.385890f,    10.082822f,
        0.662219f,    0.000000f,    0.000000f,    -10.860303f,  1.449187f,    -19.895731f,
        2.576080f,    -37.025509f,  43.462513f,   -39.212223f,  26.752314f,   32.374405f,
        49.843006f,   3.302912f,    -10.260291f,  -22.991692f,  -24.896782f,  19.746218f,
        0.485402f,    36.481308f,   -0.797132f,   18.075548f,   19.452122f,   28.650660f,
        -30.726698f,  14.999352f,   -65.968399f,  -2.399822f,   20.162727f,   -117.368042f,
        44.411537f,   -147.392639f, 44.435619f,   5.392758f,    -23.186480f,  -1.017987f,
        -5.384778f,   0.000000f,    0.000000f,    -5.052297f,   14.829707f,   -11.868268f,
        40.705742f,   -17.327288f,  65.158813f,   -20.899267f,  94.668587f,   -245.671875f,
        101.563072f,  -142.376038f, 36.437004f,   30.858355f,   -35.834152f,  28.619883f,
        -29.623734f,  0.000000f,    0.000000f,    -2.774972f,   21.504177f,   -21.276264f,
        55.392857f,   -39.110977f,  99.318222f,   -31.496140f,  85.492943f,   50.425220f,
        -124.763100f, 166.155533f,  -220.371033f, -117.029625f, 29.086348f,   -85.778198f,
        29.987753f,   0.000000f,    0.000000f,    64.152435f,   -7.702049f,   180.130386f,
        -124.564606f, 285.129150f,  -188.677048f, 343.718109f,  -171.885300f, -0.407425f,
        1.914354f,    4.694272f,    0.864391f,    3.587711f,    2.123876f,    1.964655f,
        1.030465f,    -7.440023f,   -41.323135f,  -5.352159f,   -17.649925f,  -3.923748f,
        -5.491945f,   -2.120508f,   -2.421488f,   0.000000f,    0.000000f,    1.863887f,
        0.636963f,    7.442518f,    3.261068f,    12.880199f,   -1.991265f,   -0.126950f,
        -11.186316f,  6.713049f,    -32.754490f,  6.018395f,    -11.044180f,  6.967651f,
        -8.434746f,   3.402451f,    -1.500627f,   0.000000f,    0.000000f,    -3.135373f,
        2.041598f,    -7.759135f,   3.064038f,    -9.422456f,   3.016994f,    6.315463f,
        -2.973117f,   11.234898f,   -39.291866f,  10.992847f,   -15.265981f,  8.012561f,
        -5.539876f,   5.759984f,    -2.741845f,   0.000000f,    0.000000f,    -1.066589f,
        0.706133f,    -4.946196f,   4.985934f,    -6.049829f,   2.279297f,    1.303659f,
        -5.847381f,   -16.541487f,  -116.784195f, 2.748720f,    -83.794167f,  3.578630f,
        -59.697464f,  0.358881f,    -12.913654f,  0.000000f,    0.000000f,    -2.903243f,
        10.233212f,   -7.704023f,   24.439411f,   -6.798151f,   24.976263f,   -6.638518f,
        27.335688f,   5.687097f,    -1.325537f,   1.890909f,    1.686077f,    1.066853f,
        -1.083748f,   2.559144f,    -1.039996f,   3.023083f,    -1.672140f,   5.774092f,
        -1.946759f,   4.521993f,    0.675097f,    6.688177f,    11.028355f,   -5.516587f,
        7.622503f,    -2.192013f,   -3.141561f,   0.680008f,    -0.252251f,   -3.295681f,
        -0.245109f,   4.750193f,    -5.578649f,   3.721417f,    -6.420937f,   -0.348916f,
        -3.757679f,   -0.640553f,   -4.718258f,   -7.677814f,   -5.616205f,   -1.036498f,
        -5.090006f,   20.000000f,   10.000000f,   -0.595861f,   -6.364735f,   -7.631346f,
        -38.394779f,  8.164284f,    -16.283092f,  8.210544f,    1.238002f,    -33.349522f,
        -28.684277f,  -0.167637f,   -13.681533f,  2.613246f,    12.425442f,   -41.290424f,
        -27.805710f,  -12.938294f,  -5.161816f,   6.129926f,    8.696460f,    -55.166855f,
        -6.709400f,   -5.459514f,   -2.589453f,   0.918751f,    34.961781f,   -61.949806f,
        -34.494221f,  -43.225800f,  -0.810231f,   0.000000f,    0.000000f,    -9.234606f,
        -8.438522f,   -15.167058f,  -4.767815f,   -7.872918f,   -5.474653f,   -1.749719f,
        -8.094325f,   20.850321f,   -14.432904f,  59.061024f,   -24.031345f,  124.514610f,
        -34.790405f,  265.610931f,  -75.945946f,  1.085310f,    -1.061903f,   71.684280f,
        -0.643873f,   38.850388f,   -1.679199f,   45.135727f,   2.887153f,    37.267204f,
        3.979628f,    0.239460f,    -6.783036f,   0.373409f,    -2.415806f,   0.449662f,
        -0.945386f,   0.591465f,    0.329032f,    0.823560f,    0.318046f,    1.000000f,
        1.000000f,    1.083749f,    10.644354f,   -10.560200f,  5.329753f,    -18.228773f,
        -49.523495f,  -26.971380f,  -40.199112f,  -57.787792f,  -22.311157f,  -29.158703f,
        -10.823745f,  -7.895623f,   7.019897f,    -3.868589f,   0.563994f,    -18.732122f,
        27.047461f,   0.000000f,    0.000000f,    -1.934828f,   -3.205529f,   -8.307086f,
        -10.102524f,  -26.846432f,  -7.275216f,   -40.297535f,  -1.123904f,   -73.730362f,
        29.370453f,   -111.904816f, 72.063599f,   0.000000f,    0.000000f,    -21.774622f,
        -5.185627f,   -54.839558f,  -5.712668f,   -92.290001f,  4.080645f,    -153.837494f,
        67.089569f,   -225.095673f, 145.368439f,  -267.731171f, 180.283646f,  -5.138618f,
        2.489718f,    -5.909228f,   -6.554196f,   26.973936f,   5.886049f};

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
            "\npotentialHoleAdjustment:\n\t{}",
            taperedTermToString(params.potentialHoleAdjustment));
    oss << std::format("\nholeAdjustment:\n\t{}", taperedTermToString(params.holeAdjustment));
    oss << std::format(
            "\nknightOnOutpostAdjustment:\n\t{}",
            taperedTermToString(params.knightOnOutpostAdjustment));

    return oss.str();
}
