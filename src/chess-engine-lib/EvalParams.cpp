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
            0.209377f,    1.076343f,    0.322013f,    0.164933f,    0.276038f,    1.000000f,
            -0.000000f,   -0.000000f,   -0.011230f,   0.000876f,    46.446068f,   -15.571608f,
            143.801483f,  330.309662f,  347.297729f,  303.909058f,  403.323242f,  434.020477f,
            690.874023f,  1128.676514f, 1110.934570f, 0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    -39.067139f,  141.800385f,  -42.048103f,
            141.870178f,  -41.010143f,  146.931610f,  -57.197586f,  148.075607f,  -36.148113f,
            163.141891f,  0.856664f,    145.854889f,  5.877968f,    142.359955f,  -29.100515f,
            134.797256f,  -31.754507f,  139.460953f,  -31.895000f,  136.230118f,  -29.424623f,
            142.578186f,  -35.099186f,  155.269363f,  -24.991621f,  151.419708f,  -4.156009f,
            139.314560f,  -5.700959f,   137.748306f,  -16.584005f,  134.801880f,  -15.223896f,
            144.362274f,  -11.130855f,  148.476501f,  -12.724066f,  150.146118f,  -8.594905f,
            148.237961f,  0.650843f,    137.306747f,  4.179076f,    147.110657f,  -8.430228f,
            151.620544f,  -15.878152f,  143.624893f,  -4.361261f,   160.717575f,  4.001467f,
            147.094803f,  3.680949f,    153.482986f,  4.528784f,    148.789307f,  19.279331f,
            148.603867f,  27.600981f,   135.508087f,  6.929196f,    152.986511f,  -4.059320f,
            151.993118f,  17.812309f,   161.396469f,  5.630202f,    166.527405f,  28.340279f,
            160.427765f,  9.304206f,    144.119690f,  50.575314f,   124.831573f,  72.925819f,
            175.565292f,  83.010864f,   166.538620f,  42.497784f,   161.593796f,  0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    271.967773f,  283.603973f,  290.177032f,  314.548462f,  293.566345f,
            334.167694f,  297.166748f,  343.160980f,  289.543701f,  346.402954f,  299.691223f,
            330.747314f,  281.584839f,  311.206909f,  234.640442f,  334.495483f,  292.143463f,
            322.517517f,  301.923737f,  336.641693f,  298.152740f,  344.911499f,  322.050842f,
            346.256744f,  320.952972f,  339.823029f,  303.094147f,  356.486603f,  299.836609f,
            356.145599f,  295.637817f,  319.307129f,  291.886017f,  327.230225f,  324.396576f,
            342.621368f,  326.949982f,  335.840240f,  344.982910f,  355.924286f,  348.753571f,
            355.740295f,  332.980255f,  330.518707f,  331.469574f,  322.387756f,  295.141174f,
            332.972229f,  328.881104f,  353.492493f,  364.641327f,  340.739105f,  370.292969f,
            351.076904f,  352.840942f,  354.537018f,  358.745636f,  367.450775f,  354.108826f,
            359.641052f,  331.069611f,  362.407745f,  326.568542f,  345.267334f,  334.724182f,
            364.247437f,  350.650635f,  362.119873f,  387.083710f,  353.838257f,  379.185059f,
            366.678680f,  373.234009f,  355.984222f,  381.926727f,  354.552643f,  355.907745f,
            351.871796f,  351.582397f,  351.061188f,  334.809357f,  365.537476f,  359.649872f,
            372.525024f,  404.086945f,  344.461060f,  399.662537f,  346.458038f,  445.368866f,
            339.750977f,  416.573761f,  344.626953f,  392.020233f,  339.574524f,  306.875885f,
            355.413116f,  310.953766f,  378.935272f,  340.132568f,  362.565063f,  362.472015f,
            355.860565f,  359.548798f,  357.641357f,  378.101013f,  351.264648f,  393.217316f,
            335.827881f,  354.907806f,  345.950714f,  336.631531f,  358.892090f,  126.971230f,
            374.268219f,  242.542038f,  383.777222f,  300.216858f,  395.851471f,  370.183380f,
            365.710999f,  311.357361f,  377.860870f,  421.859039f,  375.306458f,  203.504044f,
            367.776459f,  166.027481f,  242.484192f,  262.057800f,  382.322601f,  305.143738f,
            380.731293f,  281.118286f,  395.360260f,  275.226593f,  397.543243f,  274.656097f,
            397.558563f,  278.322052f,  390.646667f,  286.159576f,  389.326111f,  307.969086f,
            350.839355f,  296.681732f,  395.341492f,  291.673035f,  385.020325f,  309.528595f,
            399.370361f,  291.968689f,  400.614441f,  301.951355f,  403.934845f,  315.525391f,
            391.553619f,  316.766144f,  389.244995f,  317.521088f,  350.663666f,  292.182495f,
            372.570251f,  312.660645f,  399.699127f,  307.483490f,  404.703735f,  311.126434f,
            404.772949f,  314.733368f,  413.829132f,  312.483368f,  399.052155f,  314.885010f,
            383.557739f,  300.598389f,  405.631073f,  306.953888f,  397.059296f,  323.165985f,
            397.643250f,  307.686951f,  414.937042f,  322.401001f,  412.538666f,  327.093018f,
            415.108063f,  312.700806f,  408.701447f,  326.221161f,  399.195679f,  311.735474f,
            402.899353f,  293.360748f,  420.596100f,  308.136597f,  413.214264f,  324.993225f,
            418.270844f,  348.416992f,  405.640350f,  336.108429f,  414.254761f,  326.138702f,
            414.358521f,  311.554840f,  413.292572f,  293.708984f,  407.917725f,  306.106140f,
            417.062927f,  314.049530f,  419.292786f,  321.340668f,  407.955933f,  343.094940f,
            407.981628f,  342.909424f,  419.621185f,  400.852753f,  388.522339f,  362.260712f,
            403.854645f,  348.294464f,  394.679138f,  289.934143f,  406.342987f,  306.741211f,
            416.699310f,  318.761597f,  422.094604f,  274.013245f,  420.985626f,  347.542358f,
            395.488281f,  319.087799f,  411.253693f,  326.481140f,  399.835907f,  289.905731f,
            385.961243f,  263.243134f,  418.737885f,  273.945465f,  417.379089f,  289.246307f,
            417.554810f,  300.532074f,  413.946320f,  291.881073f,  416.743317f,  283.604218f,
            413.470093f,  338.245422f,  370.013275f,  326.510834f,  377.977844f,  445.906647f,
            666.540039f,  437.499573f,  668.209167f,  433.523834f,  678.833862f,  433.155762f,
            674.527466f,  435.166992f,  668.585754f,  426.983459f,  682.256836f,  421.417267f,
            674.587524f,  420.564301f,  661.340454f,  435.471344f,  672.050720f,  426.915710f,
            671.196594f,  417.937561f,  685.475647f,  422.635620f,  685.337158f,  422.959137f,
            683.275879f,  424.082397f,  681.953186f,  422.736572f,  667.744019f,  372.878754f,
            690.589355f,  413.666626f,  689.615417f,  425.362183f,  671.852539f,  414.608307f,
            691.628662f,  417.155762f,  684.614319f,  407.664642f,  689.534851f,  397.336121f,
            698.474121f,  414.062500f,  689.901306f,  423.570984f,  674.002380f,  418.744171f,
            703.980164f,  405.773682f,  708.973328f,  410.084045f,  708.270630f,  429.242950f,
            700.814819f,  413.880188f,  707.978088f,  412.122681f,  707.636780f,  446.762207f,
            699.131104f,  405.108704f,  709.666687f,  426.167450f,  716.719360f,  418.492584f,
            716.697998f,  432.363220f,  709.586792f,  452.678497f,  702.621826f,  438.883087f,
            711.418823f,  445.188629f,  713.607300f,  418.888794f,  713.425598f,  427.774475f,
            713.303894f,  417.856049f,  723.953186f,  449.511841f,  710.770020f,  447.438110f,
            707.227234f,  467.875641f,  700.423706f,  480.868256f,  693.427124f,  482.904846f,
            701.448303f,  484.138519f,  699.288269f,  464.032715f,  712.045898f,  435.278168f,
            721.900513f,  456.694855f,  718.789673f,  471.190125f,  707.284607f,  489.172638f,
            707.376465f,  462.040131f,  713.381531f,  491.504761f,  704.469177f,  440.800842f,
            716.364807f,  460.187561f,  710.981018f,  455.306946f,  699.188965f,  464.920593f,
            708.665039f,  468.680267f,  704.541870f,  439.496033f,  715.080200f,  441.069183f,
            715.738281f,  455.440460f,  718.375122f,  399.059296f,  736.158691f,  470.915222f,
            709.737427f,  1135.809937f, 1042.651733f, 1132.635742f, 1043.479492f, 1124.518066f,
            1056.072998f, 1126.312256f, 1051.432129f, 1131.766479f, 1046.727295f, 1105.150513f,
            1055.262451f, 1115.111694f, 974.477539f,  1086.582764f, 1040.338989f, 1128.578369f,
            1102.595581f, 1136.738892f, 1053.601685f, 1141.005493f, 1067.616455f, 1136.824829f,
            1069.451172f, 1132.430908f, 1072.163818f, 1133.496826f, 1037.487549f, 1129.684448f,
            990.086365f,  1088.979370f, 1099.581299f, 1135.259888f, 1083.004150f, 1132.797607f,
            1097.301392f, 1123.598145f, 1107.573853f, 1125.591553f, 1102.995605f, 1127.282104f,
            1101.211426f, 1129.159180f, 1096.441772f, 1137.684448f, 1064.477417f, 1129.440186f,
            1083.116577f, 1129.500000f, 1125.561646f, 1130.686646f, 1121.859375f, 1128.519653f,
            1138.670044f, 1113.160522f, 1152.666260f, 1129.195557f, 1129.711548f, 1128.794312f,
            1144.998535f, 1118.054932f, 1133.194824f, 1116.739868f, 1173.954102f, 1142.571167f,
            1103.631958f, 1123.571655f, 1150.695923f, 1099.806641f, 1207.767334f, 1101.163818f,
            1181.968628f, 1115.936035f, 1181.435303f, 1119.515503f, 1178.824463f, 1104.913452f,
            1196.337158f, 1114.941040f, 1176.102905f, 1134.682617f, 1140.655762f, 1123.034546f,
            1137.736938f, 1082.312256f, 1221.915405f, 1097.560913f, 1210.552368f, 1101.220459f,
            1228.587891f, 1163.948364f, 1166.200928f, 1185.796509f, 1093.656860f, 1118.079102f,
            1213.404541f, 1117.349121f, 1145.644897f, 1098.177368f, 1177.904053f, 1127.107666f,
            1168.235352f, 1117.111694f, 1190.520264f, 1095.400024f, 1227.973511f, 1110.842896f,
            1169.674683f, 1129.661255f, 1146.168823f, 1155.900269f, 1152.545166f, 1103.803101f,
            1168.486816f, 1161.468262f, 1104.689697f, 1106.804443f, 1205.104980f, 1101.874878f,
            1201.522339f, 1090.051025f, 1228.048340f, 1138.846313f, 1188.119507f, 1152.032959f,
            1147.651978f, 1190.336426f, 1127.036499f, 60.378944f,   -53.402721f,  23.330259f,
            -20.477064f,  -37.582088f,  5.378673f,    -52.192131f,  -16.550652f,  -38.264580f,
            -15.451624f,  -55.383850f,  11.670866f,   25.649521f,   -2.678418f,   49.201981f,
            -18.668301f,  24.922356f,   -36.989937f,  -20.652491f,  -7.109841f,   -83.148575f,
            4.817484f,    -108.067101f, 5.319282f,    -88.907143f,  4.148846f,    -60.811382f,
            1.405175f,    -6.156512f,   -9.559527f,   22.180967f,   -26.697447f,  -31.772928f,
            -28.013758f,  -34.962765f,  -11.663289f,  -124.999001f, 9.198456f,    -101.145004f,
            3.703023f,    -117.285011f, 10.596098f,   -98.538795f,  8.453328f,    -56.543907f,
            -12.087737f,  -34.273022f,  -26.743742f,  -16.698246f,  -23.432808f,  -15.401448f,
            -17.348803f,  -40.267097f,  -1.424197f,   -108.815628f, 8.059130f,    -88.545387f,
            5.383781f,    -121.961830f, 9.987494f,    -68.281570f,  -13.382787f,  -72.540009f,
            -35.681503f,  11.056844f,   -26.873854f,  -25.694242f,  5.203858f,    -27.053167f,
            -0.121543f,   -59.148380f,  11.144918f,   -64.974289f,  15.409813f,   -16.654051f,
            -0.604148f,   -26.028149f,  -18.535843f,  -58.893600f,  -21.671698f,  34.117908f,
            6.062004f,    4.925934f,    20.095034f,   -6.167594f,   16.748039f,   10.945082f,
            1.393773f,    57.236012f,   -8.503860f,   81.917099f,   23.493252f,   84.523834f,
            5.333983f,    38.142994f,   -18.746958f,  8.084662f,    3.213395f,    115.092575f,
            -3.894188f,   68.716400f,   6.615585f,    104.071899f,  -17.013098f,  1.020553f,
            1.203590f,    78.220055f,   -17.977764f,  21.097422f,   40.360218f,   161.241089f,
            -75.333633f,  59.795502f,   -66.894531f,  210.073761f,  -73.127808f,  120.637283f,
            -1.012208f,   121.137558f,  -40.138756f,  54.323917f,   -33.075916f,  287.767334f,
            -84.085510f,  -132.229385f, 70.416336f,   264.765350f,  -169.233032f, 0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    38.390350f,   65.504280f,   55.179371f,
            47.489899f,   44.072060f,   66.880341f,   60.914570f,   142.877792f,  64.508102f,
            100.050003f,  120.849640f,  44.956894f,   134.256683f,  55.768696f,   82.721382f,
            54.837391f,   62.352577f,   84.880173f,   21.383833f,   92.666374f,   45.389320f,
            86.397392f,   80.326332f,   45.346268f,   51.711418f,   96.315125f,   99.271690f,
            73.580315f,   130.648438f,  37.021660f,   86.174927f,   53.874958f,   72.351669f,
            131.049072f,  59.609684f,   127.787682f,  67.166077f,   124.650146f,  86.511833f,
            102.424583f,  113.263184f,  100.864601f,  125.346085f,  103.690445f,  95.863625f,
            108.801048f,  97.665634f,   80.357162f,   56.891380f,   244.289658f,  89.548271f,
            244.826401f,  115.595116f,  190.355881f,  122.830200f,  165.344040f,  140.039627f,
            122.953659f,  154.933578f,  153.430313f,  156.092972f,  106.033684f,  193.356445f,
            113.407944f,  152.663635f,  294.111267f,  99.846436f,   313.272339f,  89.399872f,
            280.022888f,  121.100418f,  263.282043f,  208.382523f,  179.873123f,  159.298447f,
            174.297913f,  207.910446f,  169.600800f,  169.882187f,  181.887970f,  0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    -42.877239f,
            90.618469f,   -46.277054f,  99.599197f,   -40.705959f,  83.553200f,   -42.334423f,
            51.551422f,   -4.623332f,   78.752892f,   12.147768f,   61.867924f,   9.131976f,
            54.184883f,   -52.808937f,  68.995628f,   -24.566751f,  93.401741f,   -23.067360f,
            91.081139f,   -13.708574f,  90.301086f,   -32.284077f,  70.684280f,   -22.379948f,
            76.740608f,   5.695698f,    82.704666f,   27.214903f,   65.542809f,   4.951579f,
            60.567631f,   43.459782f,   125.605141f,  49.784214f,   119.638191f,  7.449749f,
            116.715172f,  50.075768f,   88.355515f,   18.872759f,   97.933807f,   23.088453f,
            118.627075f,  23.460686f,   139.505173f,  25.505556f,   113.995422f,  121.778877f,
            193.981735f,  108.192558f,  183.558105f,  104.138000f,  156.464584f,  112.514526f,
            125.351524f,  126.661362f,  129.059875f,  122.901161f,  130.211853f,  30.333187f,
            183.039566f,  73.249283f,   157.271133f,  218.488632f,  266.889465f,  225.480286f,
            268.094360f,  261.675507f,  210.518021f,  208.536942f,  193.296921f,  273.751770f,
            161.285980f,  326.573181f,  139.044281f,  228.172089f,  187.889221f,  149.214844f,
            215.325134f,  438.784790f,  256.060547f,  422.500580f,  310.812988f,  379.876404f,
            308.035828f,  409.882172f,  233.686157f,  355.680725f,  223.239731f,  267.141144f,
            217.876923f,  159.807617f,  247.304199f,  251.403198f,  246.772278f,  0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
            0.000000f,    0.000000f,    0.000000f,    11.175270f,   6.763124f,    26.194849f,
            60.580700f,   19.925011f,   -4.538902f,   7.265619f,    15.378041f,   -0.392583f,
            -0.320635f,   -0.271864f,   -0.169535f,   -0.152345f,   0.055076f,    29.818766f,
            13.990967f,   36.658978f,   4.404080f,    25.909771f,   5.007109f,    14.612461f,
            6.009663f,    0.000000f,    0.000000f,    -14.584571f,  3.789985f,    -27.251841f,
            1.339232f,    -31.377634f,  29.342180f,   -25.315916f,  26.241995f,   24.121944f,
            1.773326f,    29.348845f,   -9.471146f,   14.848095f,   1.812808f,    9.605654f,
            2.268050f,    0.000000f,    0.000000f,    -10.824108f,  -0.404582f,   -18.533865f,
            1.607531f,    -27.180180f,  23.993643f,   -40.590275f,  19.955091f,   33.705498f,
            48.749001f,   3.725417f,    -12.634501f,  -26.431204f,  -26.055590f,  20.102364f,
            -0.282322f,   35.622482f,   0.674476f,    21.537422f,   17.922844f,   27.632452f,
            -28.110903f,  20.248104f,   -69.061638f,  -3.358869f,   19.661963f,   -158.908813f,
            59.243885f,   -154.735794f, 44.850113f,   -7.445776f,   -21.623835f,  -3.049961f,
            -5.780337f,   0.000000f,    0.000000f,    -5.509961f,   12.783757f,   -14.659213f,
            39.346085f,   -19.746307f,  63.165844f,   -27.749701f,  96.853508f,   -271.909821f,
            112.766159f,  -153.565689f, 43.763752f,   25.052759f,   -31.847263f,  28.255648f,
            -27.866486f,  0.000000f,    0.000000f,    -1.811917f,   21.861620f,   -19.923794f,
            55.916904f,   -36.120789f,  98.175064f,   -21.520386f,  74.532707f,   127.403603f,
            -201.166656f, 177.865082f,  -229.819809f, -111.255341f, 20.752407f,   -88.268211f,
            29.179386f,   0.000000f,    0.000000f,    62.915882f,   -8.102000f,   181.593506f,
            -131.052902f, 280.148804f,  -193.179413f, 326.495178f,  -164.688812f, -0.102913f,
            2.612341f,    4.762234f,    0.999819f,    3.800316f,    1.785733f,    1.808313f,
            1.085320f,    -8.557076f,   -37.157261f,  -5.973352f,   -15.825059f,  -4.887527f,
            -5.299136f,   -2.600863f,   -2.158881f,   0.000000f,    0.000000f,    1.824789f,
            -0.193407f,   3.482228f,    3.312310f,    11.054883f,   -1.542987f,   23.450825f,
            -12.402836f,  6.108370f,    -35.011078f,  6.216243f,    -12.629194f,  7.199022f,
            -9.221994f,   2.923020f,    -2.026145f,   0.000000f,    0.000000f,    -4.091473f,
            0.986939f,    -6.590200f,   1.005861f,    -8.869242f,   1.127640f,    -4.898212f,
            -0.372536f,   11.904341f,   -39.597069f,  11.227859f,   -16.339804f,  8.356578f,
            -6.919658f,   5.817650f,    -4.131580f,   0.000000f,    0.000000f,    -0.879185f,
            0.412759f,    -5.920585f,   4.751679f,    -8.354996f,   3.170543f,    -0.286943f,
            -5.078396f,   -16.207363f,  -116.872871f, 1.562882f,    -86.322609f,  2.777889f,
            -56.375244f,  0.162075f,    -13.898317f,  0.000000f,    0.000000f,    -1.737571f,
            8.493451f,    -6.440609f,   24.843048f,   -4.618966f,   23.852312f,   -4.621881f,
            25.245859f,   5.655525f,    -1.184150f,   1.731761f,    1.746329f,    1.422411f,
            -1.142568f,   2.586843f,    -1.409193f,   2.317635f,    -0.907857f,   5.256070f,
            -2.297823f,   4.333015f,    0.881474f,    6.827013f,    11.000802f,   -5.811783f,
            8.446181f,    -2.418647f,   -2.998832f,   0.460573f,    0.173822f,    -2.889215f,
            -0.661795f,   4.810465f,    -5.596266f,   3.409844f,    -5.168496f,   -1.108296f,
            -2.924654f,   -1.150028f,   -4.537907f,   -7.876050f,   -5.552407f,   -1.092127f,
            -5.329252f,   20.000000f,   10.000000f,   -0.675204f,   -6.376261f,   -6.446273f,
            -40.132637f,  8.820564f,    -17.146437f,  7.474189f,    3.980757f,    -38.161949f,
            -28.347363f,  -1.105955f,   -11.345076f,  3.160786f,    12.743253f,   -39.231445f,
            -28.859013f,  -13.443365f,  -4.639574f,   6.392211f,    9.045169f,    -52.672276f,
            -8.392138f,   -4.191095f,   -2.699052f,   1.692433f,    32.293007f,   -59.304546f,
            -41.890018f,  -40.923038f,  -10.552378f,  0.000000f,    0.000000f,    -10.077003f,
            -9.058027f,   -16.508942f,  -5.241137f,   -11.973256f,  -5.198739f,   -1.227216f,
            -9.179997f,   17.774534f,   -13.252072f,  60.810059f,   -25.389851f,  126.459587f,
            -34.694824f,  200.741226f,  -54.349003f,  3.994859f,    -1.661026f,   69.349022f,
            -0.121902f,   40.782993f,   -1.941175f,   45.162693f,   3.284390f,    33.611923f,
            3.144504f,    0.274121f,    -6.162456f,   0.423796f,    -2.260917f,   0.510210f,
            -1.062925f,   0.655169f,    0.337468f,    0.869118f,    0.402670f,    1.000000f,
            1.000000f,    0.891899f,    9.248528f,    -12.052072f,  6.183721f,    -14.019821f,
            -50.360588f,  -25.720739f,  -42.674511f,  -60.132225f,  -24.551502f,  -27.909494f,
            -7.529463f,   -6.157408f,   6.777866f,    -2.989229f,   0.075252f,    -15.655160f,
            25.996162f,   0.000000f,    0.000000f,    -1.292409f,   -4.272915f,   -7.128247f,
            -11.574593f,  -27.959955f,  -7.290893f,   -37.707916f,  -6.388620f,   -69.596153f,
            25.594025f,   -109.467720f, 69.989487f,   0.000000f,    0.000000f,    -21.080307f,
            -5.584234f,   -53.949329f,  -6.934841f,   -97.744637f,  5.607420f,    -150.435196f,
            57.405838f,   -216.009644f, 143.434097f,  -274.612152f, 167.651993f, -9.003812f,
            0.246650f };

    std::string evalCalcTToString(const EvalCalcT term) {
        return std::format("{:>6.1f}", term);
    }

    std::string taperedTermToString(const TaperedTerm& term) {
        return std::format("{{{:>6.1f}, {:>6.1f}}}", term.early, term.late);
    }

    std::string getPstPieceTypeName(int idx) {
        if (idx == EvalParams::kCandidatePassedPawnPstIdx) {
            return "Candidate";
        }
        else if (idx == EvalParams::kPassedPawnPstIdx) {
            return "Passed pawn";
        }
        else {
            return pieceToString((Piece)idx);
        }
    }

    std::string getTropismPieceTypeName(int idx) {
        if (idx == EvalParams::kDoubledPawnTropismIdx) {
            return "Doubled pawn";
        }
        else if (idx == EvalParams::kIsolatedPawnTropismIdx) {
            return "Isolated pawn";
        }
        else if (idx == EvalParams::kPassedPawnTropismIdx) {
            return "Passed pawn";
        }
        else if (idx == EvalParams::kCandidatePassedPawnTropismIdx) {
            return "Candidate";
        }
        else {
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

    return oss.str();
}
