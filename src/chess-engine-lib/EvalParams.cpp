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
        0.209377f,    1.076343f,    0.322363f,    0.193860f,    0.393714f,    4.322596f,
        1.000000f,    -0.009334f,   0.000000f,    -0.041586f,   -0.000916f,   75.907898f,
        -10.768785f,  163.170090f,  360.206055f,  361.525421f,  337.501160f,  404.158997f,
        511.132751f,  681.575562f,  1263.360229f, 1180.211182f, 0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    -41.868961f,  162.061325f,
        -43.676235f,  160.208115f,  -36.499763f,  167.574493f,  -55.240696f,  170.098572f,
        -35.585403f,  193.026260f,  10.275764f,   167.449478f,  12.613271f,   159.885284f,
        -27.362799f,  151.570007f,  -32.202301f,  157.390015f,  -32.100849f,  155.389877f,
        -23.639753f,  161.294556f,  -30.190878f,  171.371048f,  -24.263294f,  169.987854f,
        -0.028526f,   161.851334f,  0.491947f,    153.250259f,  -12.165019f,  152.555511f,
        -16.584230f,  168.345383f,  -7.437564f,   167.622955f,  -2.506459f,   168.466522f,
        4.837327f,    163.360458f,  4.688076f,    166.186737f,  13.301115f,   167.035751f,
        3.445413f,    167.906082f,  -13.089081f,  163.503006f,  7.811731f,    182.145828f,
        8.584966f,    178.022705f,  4.070054f,    173.706421f,  19.961283f,   162.959641f,
        34.139072f,   169.975159f,  33.142159f,   158.043091f,  18.808084f,   171.402878f,
        14.359334f,   169.179184f,  39.671574f,   189.404572f,  7.801063f,    201.365356f,
        36.580353f,   201.472824f,  5.983835f,    184.081970f,  37.755119f,   179.373520f,
        94.810608f,   195.171936f,  90.453491f,   193.131027f,  58.291035f,   187.608932f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    298.693665f,  337.608582f,  317.737671f,  341.244781f,
        315.216583f,  362.498627f,  328.207916f,  357.619080f,  330.091797f,  340.891663f,
        338.250427f,  331.073944f,  313.199707f,  341.905121f,  281.132812f,  294.536621f,
        323.742401f,  343.662689f,  330.384247f,  343.681854f,  335.926117f,  355.382690f,
        351.783417f,  357.782227f,  346.503754f,  354.476685f,  359.235352f,  336.762787f,
        337.532196f,  335.386658f,  333.343140f,  324.021118f,  332.157715f,  343.685547f,
        350.346527f,  354.487793f,  356.633545f,  357.872681f,  376.389984f,  369.265076f,
        382.208313f,  362.259552f,  362.246338f,  346.910187f,  361.638763f,  335.996490f,
        339.302643f,  347.846466f,  354.546631f,  357.553162f,  397.245819f,  360.447571f,
        393.410004f,  372.982452f,  379.382141f,  375.969604f,  388.143982f,  388.153778f,
        381.019043f,  377.047333f,  376.429474f,  361.022766f,  351.803680f,  346.096375f,
        360.613770f,  374.229431f,  383.387756f,  375.074066f,  410.432892f,  380.686768f,
        419.763977f,  378.945740f,  395.846222f,  375.250702f,  406.877258f,  371.445404f,
        388.929749f,  375.131042f,  391.042694f,  360.453003f,  351.409546f,  368.934113f,
        388.307220f,  376.177155f,  439.064362f,  362.245819f,  435.023682f,  364.438477f,
        499.227295f,  352.454468f,  426.180328f,  368.720764f,  415.514801f,  354.285828f,
        378.518951f,  339.992462f,  367.348938f,  372.131439f,  358.078003f,  369.251617f,
        408.945068f,  355.493347f,  426.998871f,  383.217041f,  406.476105f,  361.049042f,
        412.131897f,  356.837921f,  402.377350f,  354.865784f,  355.235809f,  356.491730f,
        144.579010f,  410.545532f,  275.812958f,  400.579865f,  366.439087f,  390.120697f,
        360.116516f,  399.033447f,  354.355682f,  382.997253f,  420.273468f,  374.315430f,
        476.479218f,  299.838043f,  287.974579f,  210.464188f,  318.733032f,  364.135193f,
        333.128845f,  384.696289f,  312.670349f,  395.758270f,  313.053040f,  390.766632f,
        316.767029f,  392.109589f,  312.441711f,  395.352509f,  292.584290f,  403.188293f,
        329.338074f,  337.331360f,  338.125946f,  387.531525f,  327.043030f,  387.841003f,
        344.068573f,  395.708801f,  322.178467f,  404.931946f,  335.817596f,  390.300079f,
        342.390869f,  387.566742f,  353.806427f,  382.333832f,  335.967560f,  363.591949f,
        328.870941f,  389.365295f,  351.582886f,  403.298401f,  342.514496f,  412.992554f,
        345.452881f,  406.507660f,  344.573822f,  412.987091f,  348.770874f,  400.074310f,
        349.263947f,  394.113037f,  346.161011f,  387.078522f,  329.803131f,  411.290527f,
        335.611328f,  411.418732f,  346.153442f,  408.957825f,  356.145172f,  419.725159f,
        361.401428f,  411.048798f,  344.147339f,  412.606506f,  363.148834f,  399.195709f,
        342.317291f,  391.459595f,  321.431152f,  413.435730f,  340.466370f,  417.382355f,
        357.254395f,  424.853180f,  381.910217f,  411.406891f,  368.103363f,  409.780365f,
        361.967926f,  424.404694f,  344.428223f,  411.931671f,  336.293915f,  411.706879f,
        346.043854f,  403.163727f,  342.813873f,  429.107910f,  354.387177f,  418.985291f,
        376.396454f,  411.348877f,  366.620636f,  416.025726f,  409.106537f,  411.963989f,
        422.226959f,  407.457092f,  371.787994f,  399.663849f,  334.766663f,  400.934387f,
        338.750885f,  429.285675f,  362.619476f,  415.515808f,  340.867798f,  428.931152f,
        378.168610f,  405.040466f,  380.464264f,  406.623840f,  350.677856f,  411.203278f,
        321.529602f,  376.444641f,  351.458771f,  402.271454f,  326.807404f,  432.871460f,
        298.739990f,  422.833282f,  345.040985f,  421.281708f,  335.191986f,  417.998230f,
        345.320831f,  401.616852f,  376.803284f,  371.387817f,  411.608887f,  390.895020f,
        519.346191f,  661.236267f,  513.558411f,  664.704041f,  513.641541f,  666.906921f,
        509.962799f,  658.955627f,  512.505737f,  657.503174f,  504.780396f,  665.687622f,
        492.643036f,  667.519775f,  488.669769f,  653.264465f,  504.433228f,  669.928467f,
        510.530914f,  661.605469f,  504.259979f,  673.513855f,  496.607666f,  675.576355f,
        498.242462f,  678.691040f,  513.471130f,  659.048950f,  477.935944f,  675.264526f,
        476.249084f,  673.129578f,  501.323273f,  674.686157f,  497.909576f,  679.545715f,
        490.569733f,  687.366455f,  498.952118f,  679.223816f,  491.652710f,  681.747742f,
        486.953125f,  684.638611f,  494.698486f,  675.950500f,  478.609863f,  686.599426f,
        510.467834f,  692.153687f,  492.562805f,  700.130005f,  495.827545f,  699.999817f,
        482.934052f,  697.388550f,  490.105103f,  694.632935f,  488.645996f,  704.701294f,
        477.634003f,  712.155884f,  473.703705f,  695.835266f,  507.705933f,  708.362915f,
        519.207336f,  701.915527f,  538.161194f,  691.545837f,  526.524658f,  695.043701f,
        526.220093f,  690.509277f,  517.735291f,  703.605469f,  518.166321f,  698.747620f,
        498.305420f,  706.161743f,  526.115540f,  705.294983f,  543.405273f,  702.275085f,
        563.208801f,  693.734863f,  557.513062f,  684.848572f,  571.350586f,  688.304321f,
        569.717163f,  696.728088f,  589.106018f,  686.903259f,  547.813843f,  711.919128f,
        535.378906f,  714.239258f,  528.328613f,  719.841248f,  572.229492f,  703.477600f,
        574.569458f,  697.714783f,  561.270874f,  709.401428f,  590.516235f,  687.372253f,
        603.763916f,  687.435181f,  550.948303f,  708.051514f,  548.512390f,  699.514465f,
        542.033630f,  705.815979f,  544.523132f,  703.392029f,  531.824768f,  704.738586f,
        534.890991f,  711.612854f,  515.965454f,  717.580750f,  513.456116f,  722.913269f,
        590.881226f,  696.269897f,  1279.910278f, 1092.930420f, 1273.514648f, 1097.605225f,
        1263.015869f, 1124.133667f, 1258.233643f, 1136.412842f, 1261.190918f, 1110.332886f,
        1237.192749f, 1120.691406f, 1229.282349f, 1064.250977f, 1242.355713f, 1088.640259f,
        1278.601318f, 1111.864014f, 1283.549072f, 1098.189941f, 1277.372559f, 1138.119141f,
        1268.332275f, 1136.512451f, 1266.827759f, 1146.021973f, 1263.518066f, 1109.890625f,
        1286.974243f, 1041.889526f, 1242.315430f, 1120.832886f, 1267.748901f, 1134.407471f,
        1269.922729f, 1174.413818f, 1258.208496f, 1176.624878f, 1262.245972f, 1169.025391f,
        1263.490723f, 1160.901367f, 1256.756592f, 1198.377930f, 1272.584229f, 1154.674561f,
        1252.949707f, 1206.284180f, 1264.259277f, 1192.344604f, 1258.441650f, 1200.011597f,
        1254.707886f, 1206.578857f, 1255.918823f, 1220.004028f, 1255.907349f, 1220.343262f,
        1245.948853f, 1238.542358f, 1262.850098f, 1199.734009f, 1245.787109f, 1250.135742f,
        1261.612793f, 1205.698364f, 1246.078979f, 1233.441406f, 1244.631958f, 1255.933716f,
        1238.572632f, 1266.064087f, 1241.517090f, 1260.781128f, 1259.637939f, 1243.698242f,
        1255.625000f, 1240.215210f, 1268.973022f, 1235.588501f, 1280.909546f, 1202.434937f,
        1278.815063f, 1182.430054f, 1252.692627f, 1258.038452f, 1225.268311f, 1288.499878f,
        1248.026367f, 1291.169556f, 1275.625854f, 1258.026001f, 1301.605591f, 1212.843262f,
        1257.584839f, 1259.033447f, 1264.386230f, 1186.294556f, 1247.091797f, 1215.170776f,
        1261.726196f, 1228.298950f, 1236.932373f, 1283.959106f, 1245.704590f, 1279.757690f,
        1245.821167f, 1260.913330f, 1245.952637f, 1267.606445f, 1313.324707f, 1189.462158f,
        1273.654419f, 1177.361450f, 1324.933105f, 1131.816162f, 1255.192017f, 1242.395874f,
        1194.279053f, 1308.441650f, 1253.317139f, 1266.210571f, 1279.190430f, 1244.744751f,
        1317.884521f, 1186.376343f, 1248.935181f, 1256.618286f, 70.294594f,   -36.171219f,
        20.602201f,   -2.953568f,   -41.501274f,  7.353073f,    -57.338734f,  -2.333272f,
        -36.804420f,  -18.404982f,  -55.591713f,  10.674540f,   23.964973f,   4.518752f,
        57.796860f,   -14.816866f,  20.745829f,   -16.051935f,  -18.155727f,  -5.967564f,
        -71.596313f,  4.682520f,    -122.952835f, 12.987884f,   -109.469971f, 6.954401f,
        -71.411743f,  4.612129f,    -2.065244f,   -11.450080f,  24.320356f,   -19.053755f,
        -32.435860f,  -19.066668f,  -59.059452f,  0.368641f,    -111.126823f, 5.879366f,
        -105.885635f, 6.245593f,    -115.296425f, 6.488594f,    -114.167824f, 7.627332f,
        -72.825974f,  -6.319213f,   -47.009716f,  -30.655209f,  10.899048f,   -24.207705f,
        4.558523f,    -22.964413f,  -60.466438f,  -5.040845f,   -63.739391f,  -5.059637f,
        -85.576439f,  -3.354768f,   -96.615196f,  4.039898f,    -60.327190f,  -15.881845f,
        -94.963547f,  -30.817629f,  70.281372f,   -23.915712f,  70.105087f,   -19.287552f,
        35.383007f,   -11.685843f,  39.764214f,   -21.067625f,  -26.765711f,  -4.009674f,
        54.168442f,   -22.982410f,  24.024773f,   -22.877522f,  -111.027367f, -10.769092f,
        86.427048f,   -15.500687f,  118.612488f,  -15.476319f,  83.038490f,   -12.738937f,
        100.431427f,  -17.686890f,  179.483734f,  -49.345356f,  134.666107f,  -13.482797f,
        64.249992f,   4.468191f,    42.274738f,   -2.128522f,   145.784958f,  -37.831703f,
        130.773315f,  -14.591113f,  130.482452f,  -17.892054f,  144.029175f,  -43.677917f,
        149.884064f,  -59.097885f,  268.232819f,  -129.737045f, 126.706795f,  19.494038f,
        80.615089f,   -1.760749f,   68.967056f,   -5.960818f,   153.877472f,  -46.842731f,
        86.429138f,   -6.722803f,   7.433144f,    -21.188644f,  84.109802f,   -74.203056f,
        123.592339f,  -48.039417f,  -1.971619f,   -1.775462f,   672.264709f,  -220.999207f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    1.853619f,    116.225395f,
        -4.898964f,   122.634842f,  25.163448f,   104.907082f,  18.469112f,   186.586517f,
        39.334774f,   148.928574f,  85.129601f,   111.481888f,  72.483864f,   110.265549f,
        12.612512f,   133.857986f,  13.461662f,   140.087753f,  -12.590940f,  139.964981f,
        12.181164f,   141.709396f,  19.888781f,   158.701859f,  3.975359f,    162.943878f,
        54.138634f,   140.695007f,  63.287594f,   120.452751f,  10.328484f,   126.174538f,
        40.181274f,   186.317810f,  20.299898f,   175.956635f,  44.031818f,   172.077484f,
        52.790440f,   153.340179f,  78.810867f,   160.588745f,  58.574696f,   173.764282f,
        65.643929f,   147.602097f,  52.040409f,   145.297211f,  64.362457f,   273.617004f,
        65.123947f,   276.132233f,  75.194473f,   241.026031f,  92.782578f,   230.345001f,
        113.297707f,  212.127228f,  133.813126f,  162.493744f,  84.297401f,   196.833481f,
        150.399094f,  180.307251f,  78.596817f,   316.678802f,  64.267250f,   382.198975f,
        87.101311f,   295.973511f,  133.474289f,  290.940094f,  184.033096f,  245.953186f,
        115.649513f,  197.744827f,  155.814743f,  195.219910f,  178.815598f,  244.454468f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        -40.577496f,  107.081902f,  -31.527338f,  94.724968f,   -32.633450f,  87.827583f,
        -2.738907f,   85.330139f,   -26.454889f,  103.031815f,  27.821556f,   54.496212f,
        17.603241f,   56.126251f,   -53.253117f,  78.022346f,   -14.028928f,  104.844032f,
        39.219711f,   73.507515f,   -16.561985f,  105.277206f,  -23.241064f,  79.426216f,
        -19.642378f,  84.182320f,   -12.117964f,  77.921707f,   23.654087f,   78.582222f,
        -4.727489f,   75.632553f,   58.721371f,   135.112762f,  84.304298f,   113.139992f,
        23.675291f,   114.526665f,  47.179062f,   100.035141f,  45.895817f,   96.513580f,
        65.303589f,   110.135994f,  29.800634f,   141.880249f,  27.432257f,   122.551872f,
        151.054688f,  202.767548f,  145.818695f,  191.893997f,  116.627556f,  176.072495f,
        116.536186f,  135.818130f,  141.595749f,  123.282990f,  126.375137f,  133.640289f,
        84.739014f,   175.972641f,  99.680504f,   163.612946f,  260.001099f,  274.971405f,
        255.266693f,  279.478363f,  251.412140f,  227.202606f,  233.990982f,  204.027634f,
        270.936981f,  168.650177f,  271.959290f,  159.979019f,  228.760300f,  206.087708f,
        174.736084f,  221.214539f,  488.852936f,  295.821014f,  471.707184f,  293.293060f,
        397.973969f,  312.994568f,  432.927338f,  220.045822f,  396.664642f,  208.754532f,
        330.321503f,  232.323425f,  206.749969f,  264.689972f,  155.372482f,  296.203186f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    13.536598f,   7.639970f,
        30.051441f,   45.449059f,   18.822935f,   13.279656f,   6.820857f,    18.527502f,
        -0.345964f,   -0.339846f,   -0.188755f,   -0.180249f,   -0.103377f,   0.092608f,
        28.028973f,   9.782303f,    32.681786f,   -0.020351f,   21.248270f,   0.785483f,
        11.730489f,   -1.193577f,   0.000000f,    0.000000f,    -11.768860f,  -3.460818f,
        -25.642788f,  -7.711613f,   -36.227390f,  49.413319f,   -109.675346f, 129.994415f,
        35.304192f,   -5.753369f,   33.297123f,   -16.353109f,  14.865569f,   -2.407811f,
        8.943663f,    0.602419f,    -0.000000f,   0.000000f,    -11.034590f,  0.768472f,
        -20.133047f,  -2.903527f,   -34.944313f,  54.582298f,   -72.099251f,  30.119837f,
        27.951208f,   68.304207f,   0.762191f,    -8.975205f,   -49.750347f,  -10.210931f,
        19.158440f,   2.490625f,    34.865139f,   2.854622f,    19.047432f,   17.283289f,
        23.789022f,   -24.824306f,  -0.123031f,   -61.375008f,  4.855143f,    19.818678f,
        -104.068535f, 35.269222f,   -119.363266f, 29.908409f,   32.930130f,   -34.333107f,
        8.034413f,    -11.071324f,  0.000000f,    0.000000f,    4.816823f,    7.490092f,
        1.962602f,    33.723022f,   -3.469373f,   58.910530f,   -17.310165f,  110.783562f,
        -292.690613f, 124.488640f,  -140.741257f, 43.280460f,   22.105745f,   -29.709135f,
        28.747446f,   -32.053192f,  0.000000f,    0.000000f,    -11.903049f,  29.030592f,
        -17.695248f,  53.573002f,   -52.696815f,  103.454697f,  -126.001221f, 169.501999f,
        85.364883f,   -79.898216f,  119.544556f,  -137.369888f, -67.499603f,  9.311884f,
        -69.821541f,  14.148758f,   0.000000f,    0.000000f,    57.613708f,   -22.394142f,
        122.688431f,  -76.605659f,  255.512039f,  -202.269226f, 464.947998f,  -411.961975f,
        0.000000f,    0.000000f,    0.235579f,    2.721258f,    5.005955f,    2.812208f,
        3.461263f,    3.103747f,    2.504285f,    -0.621333f,   0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        -7.305284f,   -39.778446f,  -7.301428f,   -9.666065f,   -5.792879f,   -2.233498f,
        -5.172054f,   1.461245f,    -2.110745f,   2.390335f,    0.000000f,    0.000000f,
        2.869530f,    -0.262613f,   -9.892711f,   -1.571302f,   -20.113176f,  -4.531155f,
        15.286526f,   -36.021320f,  12.793261f,   -9.992052f,   12.323991f,   -5.985721f,
        9.814015f,    -1.384154f,   7.418643f,    -0.504986f,   0.000000f,    0.000000f,
        -3.003433f,   -1.108023f,   1.741832f,    -4.633827f,   20.528891f,   -12.786544f,
        13.048333f,   -36.150730f,  9.838600f,    -11.419958f,  8.519604f,    -8.610643f,
        6.412505f,    -4.158329f,   1.973217f,    -2.079875f,   0.000000f,    0.000000f,
        -1.464672f,   -0.525013f,   0.136467f,    -2.522846f,   2.081051f,    -9.788527f,
        5.509398f,    -118.379631f, 7.499650f,    -78.612694f,  5.778535f,    -42.721836f,
        3.484048f,    -22.085161f,  1.926211f,    -2.585466f,   0.000000f,    0.000000f,
        -3.407943f,   14.708588f,   -4.740726f,   19.553852f,   -2.680649f,   26.087601f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        6.941758f,    -1.540939f,   1.355004f,    1.762840f,    1.814162f,    -0.716511f,
        3.060158f,    0.192197f,    2.908136f,    -3.848885f,   5.786827f,    -1.157563f,
        4.468934f,    1.143266f,    7.834093f,    11.679782f,   -1.555229f,   6.239015f,
        -2.212877f,   -4.604185f,   0.303273f,    1.108347f,    -2.496490f,   -0.471759f,
        4.218682f,    -5.251935f,   4.523530f,    -7.487044f,   -0.411474f,   -4.240996f,
        -0.041589f,   -6.566933f,   -7.151298f,   -6.349540f,   1.198372f,    -8.944246f,
        20.000000f,   10.000000f,   -0.300853f,   -7.468019f,   -6.062694f,   -42.098129f,
        9.256735f,    -18.892515f,  7.336816f,    3.492683f,    -42.490055f,  -32.573444f,
        -2.562555f,   -10.802804f,  4.339715f,    10.177169f,   -40.169785f,  -35.908772f,
        -13.483666f,  -5.741348f,   6.005486f,    8.042859f,    -53.603703f,  -4.531090f,
        -7.067132f,   0.114708f,    4.787760f,    31.328281f,   -63.157158f,  -40.920536f,
        -46.844505f,  14.228507f,   0.000000f,    0.000000f,    62.664139f,   -55.327526f,
        57.275158f,   -51.389221f,  55.714558f,   -50.498768f,  64.071281f,   -55.498413f,
        77.918480f,   -62.908642f,  120.403099f,  -75.922478f,  179.497757f,  -85.609604f,
        188.234573f,  -75.517105f,  0.000000f,    0.000000f,    -93.021812f,  49.770481f,
        -68.297913f,  42.301033f,   -71.977264f,  45.264309f,   -80.072723f,  45.042919f,
        -80.333595f,  48.802273f,   0.000000f,    0.000000f,    0.000000f,    0.000000f,
        15.527846f,   4.848150f,    117.924866f,  -43.489979f,  231.648224f,  -90.940956f,
        357.261871f,  -141.455627f, 515.700623f,  -200.117874f, -10.615908f,  0.198817f,
        -20.393768f,  -49.719700f,  -29.311068f,  -42.252796f,  -41.467197f,  -22.582554f,
        -24.188255f,  -29.957226f,  -10.007284f,  11.425837f,   -7.968351f,   5.651613f,
        -19.138439f,  30.726507f,   6.641151f,    1.863730f,    4.729122f,    0.958982f,
        -2.499092f,   -7.655365f,   -18.917322f,  -4.614372f,   -27.422956f,  -8.017842f,
        -60.125237f,  21.576738f,   -104.509438f, 68.317223f,   17.485184f,   4.426315f,
        -0.296810f,   -4.840450f,   -40.616367f,  -1.581732f,   -77.038567f,  6.375960f,
        -144.679001f, 78.913887f,   -276.535034f, 207.953934f,  -289.305756f, 191.614975f};

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

    oss << std::format(
            "\ndefendedChecksAdjustment:\n\t{}", arrayToString(params.defendedChecksAdjustment));

    oss << std::format(
            "\nundefendedChecksAdjustment:\n\t{}",
            arrayToString(params.undefendedChecksAdjustment));

    return oss.str();
}
