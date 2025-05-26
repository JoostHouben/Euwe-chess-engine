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
        0.209377f,    1.076343f,    0.294897f,    0.211547f,    0.442624f,    1.088191f,
        1.000000f,    -0.008761f,   -0.009009f,   -0.036673f,   -0.000557f,   62.682232f,
        2.365654f,    161.895355f,  380.518829f,  376.830231f,  350.893616f,  415.132294f,
        566.948303f,  714.817993f,  1352.000854f, 1250.150757f, 0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    -31.261465f,  158.679916f,
        -35.060402f,  164.191208f,  -26.052055f,  166.018204f,  -41.827999f,  158.618668f,
        -23.798351f,  188.297531f,  26.541548f,   166.403564f,  25.283581f,   159.808533f,
        -13.625507f,  150.073929f,  -22.511314f,  153.931503f,  -19.791424f,  156.218048f,
        -12.163075f,  159.465042f,  -16.820532f,  167.892807f,  -10.980929f,  166.649826f,
        7.112895f,    163.453247f,  13.493709f,   152.911652f,  0.451561f,    151.693375f,
        -4.261565f,   166.820923f,  2.742771f,    166.792847f,  10.732826f,   165.122269f,
        19.538776f,   160.098785f,  19.421249f,   164.802582f,  30.319824f,   160.535843f,
        15.715537f,   166.259003f,  1.262505f,    162.855286f,  21.615801f,   181.380646f,
        22.668123f,   183.481354f,  22.791948f,   175.279266f,  40.631569f,   161.078690f,
        56.298969f,   168.690659f,  46.709591f,   159.134445f,  31.248177f,   175.105499f,
        33.415939f,   166.865448f,  62.262444f,   185.767731f,  26.738316f,   212.297852f,
        60.356121f,   194.495056f,  16.091288f,   195.683762f,  60.577541f,   145.180679f,
        129.305923f,  180.693344f,  122.974052f,  182.064941f,  84.112465f,   181.771362f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    343.966766f,  324.131500f,  333.942383f,  365.525909f,
        328.693237f,  373.589813f,  357.852051f,  357.551727f,  348.007263f,  358.743774f,
        361.472961f,  337.538635f,  325.471130f,  358.283905f,  319.263763f,  283.469910f,
        345.581970f,  336.086609f,  344.448639f,  366.909790f,  358.759399f,  374.937531f,
        372.220856f,  374.750610f,  362.720215f,  370.482574f,  379.292480f,  357.035309f,
        351.085663f,  362.176514f,  363.235565f,  329.212799f,  350.671661f,  372.191559f,
        369.918243f,  362.733459f,  375.643066f,  372.050812f,  398.187469f,  387.550781f,
        402.326111f,  381.174591f,  380.831390f,  360.820770f,  380.109375f,  346.330444f,
        362.421631f,  363.935516f,  377.013794f,  370.708099f,  418.167969f,  378.132721f,
        416.048706f,  390.474243f,  400.665894f,  394.866943f,  407.404510f,  408.901978f,
        406.696930f,  389.946991f,  395.890442f,  373.770203f,  368.511810f,  363.744659f,
        389.735504f,  386.138916f,  404.163696f,  392.542267f,  437.463196f,  397.706238f,
        454.843384f,  392.855652f,  425.221893f,  391.767456f,  436.051971f,  387.768921f,
        413.902710f,  391.310059f,  411.533905f,  368.899231f,  383.237823f,  383.455536f,
        434.158417f,  381.310425f,  477.456573f,  369.735077f,  464.133301f,  381.135284f,
        528.915527f,  368.698792f,  468.244995f,  379.385742f,  430.627380f,  369.419006f,
        407.075623f,  355.474548f,  405.041229f,  375.417664f,  382.960815f,  384.228455f,
        438.588013f,  371.834198f,  448.213348f,  389.847900f,  442.964508f,  374.689362f,
        414.474274f,  369.791473f,  432.200653f,  379.396088f,  368.290100f,  366.441803f,
        203.235672f,  406.311188f,  263.430237f,  434.606964f,  368.200897f,  403.569702f,
        390.521118f,  416.906647f,  348.923950f,  401.893829f,  408.381531f,  378.087952f,
        512.271301f,  302.698486f,  308.501770f,  275.712311f,  327.199219f,  385.964966f,
        339.446930f,  404.237274f,  325.023834f,  404.932526f,  322.523193f,  408.946136f,
        335.118774f,  397.069824f,  322.231476f,  409.283417f,  294.301788f,  408.580780f,
        297.413269f,  383.131165f,  357.258698f,  390.180359f,  340.189453f,  392.643280f,
        359.477203f,  401.354248f,  335.336975f,  414.980133f,  350.687195f,  402.690857f,
        352.670471f,  402.560669f,  368.531281f,  389.582886f,  347.878204f,  360.745789f,
        333.006287f,  409.617432f,  366.825684f,  411.707611f,  356.843018f,  423.323822f,
        359.605347f,  414.003754f,  357.954498f,  421.418915f,  360.498688f,  412.927032f,
        361.309570f,  402.217590f,  358.964539f,  401.861786f,  344.934814f,  425.839020f,
        355.412720f,  420.301361f,  358.412415f,  422.078705f,  367.527130f,  434.595917f,
        373.518311f,  425.109467f,  356.951355f,  427.082947f,  361.836609f,  415.281158f,
        355.189514f,  399.537476f,  344.244781f,  414.558441f,  353.748535f,  431.109009f,
        371.706909f,  435.234802f,  398.340820f,  426.003845f,  386.095306f,  423.372955f,
        381.495117f,  436.235352f,  361.098114f,  420.474060f,  356.887604f,  417.960358f,
        364.392944f,  409.204102f,  361.306427f,  433.916656f,  365.835754f,  432.662537f,
        392.921448f,  422.271118f,  393.508087f,  425.080170f,  429.464081f,  420.387390f,
        460.615265f,  413.364014f,  390.286652f,  414.531830f,  359.944885f,  415.254547f,
        349.738922f,  443.030304f,  390.506470f,  422.720001f,  361.491241f,  444.484772f,
        381.500427f,  421.331848f,  421.028412f,  416.659393f,  320.257935f,  441.708191f,
        327.242889f,  396.629150f,  357.321594f,  424.927307f,  367.620911f,  441.918335f,
        292.824158f,  438.603882f,  371.706024f,  425.707581f,  340.515656f,  433.184540f,
        330.600098f,  420.332703f,  363.407043f,  398.021484f,  453.474762f,  397.733582f,
        575.516235f,  690.317932f,  570.741211f,  694.564697f,  571.038513f,  696.164429f,
        565.743591f,  693.687622f,  569.947083f,  690.107117f,  561.081238f,  697.858643f,
        549.205322f,  693.548889f,  542.758423f,  682.580811f,  555.567444f,  703.730225f,
        569.402832f,  692.867737f,  558.311646f,  711.044189f,  552.672485f,  705.230896f,
        555.348816f,  712.911316f,  566.817139f,  695.415894f,  535.724792f,  707.241577f,
        501.181000f,  715.687500f,  555.311035f,  706.282959f,  551.532104f,  712.807678f,
        547.504150f,  718.610168f,  541.093323f,  722.452820f,  551.350586f,  713.897949f,
        535.600952f,  720.448608f,  547.181946f,  712.341614f,  523.036316f,  720.700195f,
        559.374207f,  728.296936f,  550.047852f,  732.802002f,  548.605835f,  736.810059f,
        537.530579f,  735.996460f,  546.149353f,  729.541443f,  549.096375f,  735.206665f,
        529.132751f,  745.061340f,  520.353760f,  730.574829f,  560.958191f,  743.162659f,
        583.247925f,  733.716187f,  591.900452f,  728.729492f,  582.432373f,  728.968994f,
        581.895996f,  721.790833f,  566.150818f,  732.643250f,  572.412476f,  738.775452f,
        543.438049f,  741.352539f,  575.431580f,  744.359924f,  599.123108f,  736.883057f,
        615.866577f,  729.787415f,  605.842773f,  722.827393f,  639.047607f,  721.498169f,
        641.332214f,  724.008301f,  650.429260f,  725.361816f,  593.239441f,  747.848083f,
        591.755798f,  748.563232f,  577.825562f,  754.973206f,  622.847351f,  741.361572f,
        631.995972f,  736.971252f,  612.540283f,  749.076111f,  637.953735f,  724.720459f,
        671.708252f,  719.247131f,  618.981995f,  735.760742f,  598.790955f,  732.906372f,
        629.341797f,  733.046204f,  598.158264f,  736.928833f,  600.117004f,  735.681519f,
        590.076721f,  746.570618f,  556.479431f,  756.715088f,  610.549377f,  748.653931f,
        610.980530f,  740.378296f,  1371.578003f, 1161.989990f, 1357.114746f, 1191.434814f,
        1352.113647f, 1185.848633f, 1344.898682f, 1207.481445f, 1342.230591f, 1189.250977f,
        1323.533447f, 1193.515381f, 1327.476074f, 1142.165039f, 1339.054932f, 1215.138794f,
        1364.010986f, 1207.619995f, 1373.048096f, 1167.911865f, 1367.498169f, 1186.923096f,
        1358.190918f, 1195.295166f, 1353.903198f, 1217.677124f, 1356.100098f, 1166.691040f,
        1379.494751f, 1102.064453f, 1325.564331f, 1149.865723f, 1362.137817f, 1196.676514f,
        1358.462036f, 1244.268311f, 1343.147705f, 1250.436035f, 1351.585938f, 1237.354492f,
        1344.077515f, 1251.683350f, 1344.070557f, 1274.436279f, 1357.749146f, 1217.752075f,
        1335.802368f, 1252.451050f, 1353.444092f, 1255.426392f, 1356.430542f, 1254.798218f,
        1344.234619f, 1274.416260f, 1345.295532f, 1300.421875f, 1340.774780f, 1317.014648f,
        1334.872925f, 1318.065186f, 1346.250000f, 1287.062744f, 1338.274658f, 1305.398926f,
        1343.626343f, 1292.174438f, 1335.744141f, 1314.130005f, 1341.132446f, 1313.900635f,
        1333.874268f, 1349.593872f, 1329.646973f, 1342.496826f, 1349.606934f, 1324.365967f,
        1352.163574f, 1314.688843f, 1370.548950f, 1284.426025f, 1366.009399f, 1282.242920f,
        1361.895386f, 1261.007812f, 1337.330444f, 1347.613159f, 1321.189575f, 1357.360474f,
        1344.811035f, 1354.767944f, 1371.588989f, 1345.958618f, 1412.541016f, 1220.769897f,
        1359.413208f, 1324.597290f, 1354.938354f, 1266.280029f, 1341.943848f, 1293.771606f,
        1347.669800f, 1303.440063f, 1347.962280f, 1343.225708f, 1314.243408f, 1388.545654f,
        1367.700562f, 1324.675659f, 1370.274048f, 1273.536621f, 1446.167236f, 1216.228394f,
        1385.827515f, 1235.432495f, 1401.429932f, 1224.996826f, 1384.811157f, 1267.787598f,
        1311.209229f, 1357.369873f, 1347.459106f, 1342.349121f, 1404.284546f, 1288.941162f,
        1441.576294f, 1222.359619f, 1303.448486f, 1369.023926f, 67.331543f,   -24.414557f,
        23.038746f,   -6.324785f,   -42.717648f,  6.438921f,    -59.136402f,  -0.514375f,
        -40.600967f,  -15.210546f,  -53.320740f,  11.739555f,   26.239290f,   4.295192f,
        59.523849f,   -10.786356f,  17.780365f,   -14.619526f,  -14.027339f,  -5.183062f,
        -68.134766f,  3.839960f,    -135.201126f, 15.133856f,   -120.190758f, 5.980837f,
        -75.400764f,  5.400376f,    -5.944468f,   -8.746371f,   29.792391f,   -21.115641f,
        -17.267641f,  -23.176483f,  -63.779343f,  0.363133f,    -102.693604f, 1.169401f,
        -101.603111f, 2.047119f,    -135.672958f, 8.419370f,    -120.975937f, 5.357437f,
        -73.500328f,  -10.118927f,  -42.586014f,  -31.153023f,  0.624697f,    -10.687307f,
        9.799312f,    -21.102112f,  -71.244919f,  -6.607388f,   -71.400887f,  -5.108460f,
        -84.908325f,  -6.726416f,   -86.254890f,  -1.447119f,   -51.554386f,  -21.167969f,
        -102.373077f, -22.682756f,  33.359715f,   -3.787498f,   53.382637f,   -12.504563f,
        23.713121f,   -12.931900f,  47.048664f,   -22.772068f,  -2.326812f,   -8.193587f,
        18.873674f,   -14.060139f,  35.162128f,   -20.163576f,  -99.740135f,  -13.486408f,
        121.153404f,  -23.576160f,  132.374405f,  -14.900584f,  106.196739f,  -12.149826f,
        103.310493f,  -16.671080f,  127.874001f,  -33.731522f,  130.786667f,  -5.702274f,
        48.520107f,   12.822542f,   104.819603f,  -5.831691f,   179.419708f,  -25.258684f,
        83.874863f,   2.430159f,    86.132378f,   5.584253f,    156.357132f,  -33.158733f,
        25.583675f,   -20.565941f,  234.108307f,  -114.563362f, 90.207306f,   25.731934f,
        46.228546f,   42.698223f,   192.590378f,  -80.813957f,  256.614990f,  -74.872894f,
        65.965508f,   0.813984f,    56.747234f,   -9.278645f,   75.263016f,   -36.731144f,
        109.281334f,  -37.180412f,  35.398117f,   2.183836f,    989.998901f,  -338.452667f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    11.608172f,   107.910271f,
        -20.267208f,  132.010345f,  24.469707f,   112.826828f,  -6.829280f,   156.436584f,
        36.615849f,   118.921120f,  86.602600f,   118.149124f,  65.667221f,   111.913216f,
        38.192745f,   119.203392f,  10.311313f,   139.325272f,  -1.262460f,   142.662827f,
        11.066691f,   135.444260f,  13.855447f,   155.913498f,  1.114767f,    162.016190f,
        47.949192f,   134.613708f,  50.394829f,   133.268906f,  12.601875f,   123.498596f,
        27.193644f,   192.224823f,  29.806974f,   168.731827f,  50.768867f,   172.377853f,
        58.170689f,   144.832794f,  85.525597f,   151.354050f,  56.705223f,   171.005981f,
        27.258902f,   159.326889f,  48.010616f,   152.169815f,  88.292290f,   279.703522f,
        54.150810f,   258.017822f,  81.714241f,   241.402527f,  99.302956f,   216.820099f,
        118.381081f,  206.440506f,  159.182617f,  149.318237f,  104.434242f,  180.351440f,
        153.470367f,  172.208221f,  75.802383f,   310.332489f,  59.782795f,   403.441864f,
        96.250031f,   310.428925f,  147.743057f,  277.099884f,  218.888748f,  202.566330f,
        198.020187f,  198.217850f,  139.801620f,  201.847870f,  224.714493f,  236.412476f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        -19.058594f,  105.574455f,  6.204095f,    74.400391f,   -15.868220f,  85.899689f,
        31.088854f,   80.728592f,   -6.869637f,   101.195786f,  56.793663f,   44.224041f,
        44.287888f,   52.179291f,   -17.871059f,  61.967319f,   11.410254f,   97.978760f,
        67.921616f,   69.594795f,   7.483665f,    94.241417f,   6.678784f,    71.417252f,
        -9.089748f,   77.419571f,   7.422765f,    75.421364f,   37.334934f,   84.858955f,
        26.625622f,   65.843079f,   88.830795f,   130.760513f,  114.643494f,  110.347931f,
        54.502808f,   111.964958f,  71.863190f,   90.208580f,   79.205170f,   91.298805f,
        95.264130f,   107.735970f,  70.287895f,   141.122025f,  57.535408f,   123.958931f,
        179.100128f,  203.231567f,  185.826614f,  194.595612f,  164.205032f,  168.516006f,
        158.626801f,  130.588791f,  177.082855f,  117.125572f,  165.171143f,  124.212433f,
        125.662148f,  176.269379f,  150.307281f,  157.972427f,  315.402924f,  271.789368f,
        302.840820f,  276.870850f,  281.687103f,  228.214996f,  273.679199f,  206.364136f,
        317.433258f,  160.100754f,  340.702606f,  145.241547f,  247.993942f,  210.652496f,
        210.544418f,  219.640518f,  533.020386f,  304.127502f,  540.347107f,  296.449280f,
        473.796356f,  297.437653f,  480.117676f,  220.685638f,  515.505371f,  179.267960f,
        444.999359f,  204.667938f,  242.887817f,  276.982269f,  172.295074f,  300.328705f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    15.547349f,   10.547941f,
        31.903151f,   44.557667f,   7.808328f,    26.035372f,   9.992668f,    15.523027f,
        -0.329135f,   -0.320901f,   -0.196942f,   -0.165254f,   -0.077576f,   0.095340f,
        28.374109f,   11.732932f,   35.792248f,   -1.520542f,   24.533716f,   -1.148846f,
        13.388613f,   -0.865592f,   0.000000f,    0.000000f,    -15.177210f,  1.236414f,
        -25.919489f,  -7.720091f,   -46.977180f,  40.476963f,   -94.727715f,  -49.498924f,
        39.293068f,   -1.013814f,   32.996113f,   -14.236329f,  17.120365f,   -2.812384f,
        8.815392f,    5.322675f,    -0.000000f,   0.000000f,    -14.959566f,  9.775561f,
        -18.195644f,  13.317310f,   -38.716728f,  92.052567f,   -89.267097f,  102.372101f,
        32.454678f,   75.553558f,   1.635960f,    -15.271439f,  -62.288940f,  -9.254653f,
        20.500132f,   4.493259f,    37.774525f,   0.665411f,    20.037777f,   15.377128f,
        30.321163f,   -27.312061f,  -5.011730f,   -57.242619f,  7.579222f,    17.026190f,
        -240.604874f, 75.682991f,   -130.204178f, 35.698750f,   22.717575f,   -27.280481f,
        11.982452f,   -10.779858f,  0.000000f,    0.000000f,    -0.119711f,   10.438669f,
        -0.479739f,   32.689434f,   -10.036662f,  68.919594f,   -29.577593f,  134.102219f,
        -469.923859f, 189.960251f,  -142.343979f, 49.315498f,   30.594332f,   -31.286530f,
        29.366755f,   -30.683773f,  0.000000f,    0.000000f,    -9.825130f,   27.024057f,
        -18.122934f,  54.494392f,   -58.283417f,  111.828323f,  -185.580414f, 260.455566f,
        170.125275f,  -63.025414f,  96.084999f,   -98.138176f,  -70.359482f,  17.399111f,
        -67.831352f,  18.722866f,   0.000000f,    0.000000f,    51.312618f,   -17.782429f,
        109.015434f,  -55.252293f,  250.206757f,  -210.894501f, 571.803711f,  -612.074402f,
        -0.412006f,   -0.155419f,   0.000000f,    0.000000f,    -0.629255f,   3.586172f,
        5.141637f,    3.523435f,    3.065675f,    3.550779f,    2.585437f,    -0.686468f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    -9.492600f,   -30.670959f,  -8.517539f,   -5.982730f,
        -7.488751f,   1.186550f,    -6.057686f,   3.483999f,    -3.257034f,   3.049546f,
        0.000000f,    0.000000f,    0.699939f,    -2.957643f,   -5.478600f,   -7.186368f,
        96.022751f,   -34.716095f,  14.783716f,   -39.105701f,  13.289091f,   -16.466028f,
        9.998734f,    -7.092559f,   8.504217f,    -3.528834f,   6.116701f,    -1.558923f,
        0.000000f,    0.000000f,    -2.554584f,   -2.311558f,   2.030383f,    -6.493758f,
        24.813509f,   -19.315948f,  11.619482f,   -29.560961f,  6.892337f,    -9.750872f,
        6.977997f,    -8.515257f,   4.392774f,    -5.733418f,   0.821186f,    -1.936191f,
        0.000000f,    0.000000f,    -1.544329f,   -1.040168f,   1.613698f,    -4.766519f,
        1.750612f,    -9.084577f,   1.755317f,    -107.616814f, 5.839248f,    -47.706219f,
        4.641103f,    -30.238747f,  3.949985f,    -22.626724f,  0.676801f,    1.004786f,
        0.000000f,    0.000000f,    -4.037141f,   15.740670f,   -4.515681f,   23.207315f,
        -2.967254f,   27.292038f,   0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    6.810789f,    -0.949351f,   2.017349f,    1.672756f,
        2.294242f,    -0.318224f,   2.532631f,    0.297113f,    3.535240f,    -4.495473f,
        5.830631f,    -0.993638f,   4.303589f,    2.044664f,    7.639621f,    12.721010f,
        -1.073582f,   5.999949f,    -2.672699f,   -4.552716f,   0.684708f,    0.943912f,
        -2.619174f,   -0.380186f,   4.739916f,    -5.470683f,   4.477645f,    -8.575820f,
        -2.175007f,   -2.394595f,   -0.089569f,   -7.021277f,   -8.456010f,   -6.776738f,
        1.857495f,    -7.915867f,   20.000000f,   10.000000f,   -0.676159f,   -7.404325f,
        -6.467684f,   -44.492279f,  8.907411f,    -19.486298f,  6.808731f,    4.697170f,
        -48.086250f,  -30.770405f,  -4.651392f,   -9.516119f,   4.577340f,    12.158888f,
        -41.117794f,  -33.809673f,  -14.543301f,  -0.579684f,   4.546464f,    9.058964f,
        -55.234764f,  -4.147323f,   -7.824038f,   7.360506f,    3.871830f,    35.533146f,
        -68.622726f,  -25.267860f,  -49.478012f,  27.864281f,   0.000000f,    0.000000f,
        38.764778f,   -54.478046f,  33.865635f,   -51.135098f,  32.793064f,   -49.275002f,
        47.607136f,   -57.622910f,  60.193562f,   -66.036308f,  102.114311f,  -80.676605f,
        180.184921f,  -91.662651f,  86.256912f,   -47.908958f,  0.000000f,    0.000000f,
        -60.709686f,  50.018475f,   -34.967693f,  41.923168f,   -36.530285f,  42.983456f,
        -45.119457f,  43.771053f,   -47.702625f,  47.176563f,   0.000000f,    0.000000f,
        0.000000f,    0.000000f,    3.124534f,    3.791681f,    71.836655f,   -44.254166f,
        150.532776f,  -89.818687f,  246.583221f,  -141.193207f, 348.160553f,  -194.341980f,
        -11.155155f,  0.258264f,    -19.522240f,  -54.489174f,  -23.451571f,  -55.637798f,
        -45.763332f,  -23.907560f,  -26.038923f,  -34.253643f,  -13.752463f,  11.684249f,
        -9.382195f,   5.907933f,    -22.134502f,  35.816734f,   6.641151f,    1.863730f,
        3.356889f,    1.118602f,    -3.364241f,   -6.452435f,   -23.436453f,  -2.759473f,
        -27.993561f,  -12.393256f,  -50.516800f,  11.932226f,   -94.350281f,  53.466106f,
        17.485184f,   4.426315f,    -0.630158f,   -5.594659f,   -42.024281f,  -2.456740f,
        -71.035332f,  -0.292317f,   -157.813400f, 77.201874f,   -275.597656f, 195.153305f,
        -261.003693f, 180.171097f};

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

    oss << std::format(
            "\ndefendedChecksAdjustment:\n\t{}", arrayToString(params.defendedChecksAdjustment));

    oss << std::format(
            "\nundefendedChecksAdjustment:\n\t{}",
            arrayToString(params.undefendedChecksAdjustment));

    return oss.str();
}
