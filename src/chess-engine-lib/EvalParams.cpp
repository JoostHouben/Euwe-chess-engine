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
        0.209377f,    1.076343f,    0.294976f,    0.211565f,    0.443170f,    1.083434f,
        1.000000f,    -0.009400f,   -0.009001f,   -0.034193f,   -0.000005f,   60.817123f,
        2.640882f,    162.277664f,  380.379272f,  377.782990f,  350.960114f,  415.780823f,
        567.181030f,  715.307190f,  1354.679321f, 1248.075317f, 0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    -30.688128f,  159.074799f,
        -34.488632f,  164.483307f,  -25.471292f,  166.367584f,  -41.298206f,  159.109085f,
        -23.451828f,  188.404449f,  26.473806f,   166.629425f,  25.353016f,   160.137787f,
        -13.403670f,  150.539124f,  -22.037092f,  154.463867f,  -19.399359f,  156.739441f,
        -11.904769f,  159.972992f,  -16.395306f,  168.270935f,  -10.663417f,  166.881775f,
        7.604958f,    163.588043f,  13.753942f,   153.202606f,  0.755486f,    152.121445f,
        -3.927995f,   167.131989f,  2.990535f,    167.121536f,  10.999048f,   165.558182f,
        19.799864f,   160.589233f,  19.690166f,   165.162354f,  30.653273f,   160.908875f,
        15.920150f,   166.782196f,  1.525772f,    163.364731f,  21.859030f,   181.747360f,
        22.657852f,   184.158905f,  22.923227f,   175.643158f,  40.740536f,   161.603729f,
        56.566521f,   169.212021f,  46.892803f,   159.810165f,  31.485474f,   175.593491f,
        33.583317f,   167.300354f,  62.388950f,   186.302567f,  26.863714f,   211.910019f,
        60.498665f,   194.806778f,  16.183346f,   196.440475f,  60.929436f,   145.007462f,
        129.321289f,  181.513000f,  122.115585f,  183.052673f,  83.859039f,   182.451202f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    344.399261f,  325.698059f,  333.694489f,  367.036407f,
        328.627441f,  374.476196f,  357.639008f,  358.177216f,  347.894714f,  359.104065f,
        361.124512f,  338.176453f,  325.400116f,  359.632812f,  318.286316f,  284.682617f,
        344.976318f,  337.477325f,  345.028687f,  367.521210f,  358.946930f,  375.625275f,
        372.153320f,  375.560699f,  362.609833f,  371.389557f,  378.760345f,  357.959961f,
        350.639893f,  362.891571f,  362.802582f,  330.387634f,  350.485046f,  372.858246f,
        369.831238f,  363.909637f,  375.472870f,  373.131409f,  398.056610f,  388.507812f,
        402.249939f,  382.121399f,  380.831177f,  361.560242f,  380.151093f,  347.314484f,
        362.347076f,  365.061462f,  376.596191f,  371.656708f,  418.026520f,  379.105499f,
        415.705872f,  391.692719f,  400.345856f,  395.869415f,  407.314880f,  409.816986f,
        406.653320f,  390.785065f,  396.182953f,  374.947845f,  368.152679f,  364.838165f,
        389.548462f,  386.694305f,  403.616791f,  393.456512f,  436.648041f,  398.904022f,
        454.787048f,  393.884460f,  424.933167f,  393.055145f,  436.190521f,  389.120270f,
        413.440338f,  392.499176f,  411.106537f,  369.882996f,  383.014313f,  384.281097f,
        434.036713f,  382.288116f,  476.920349f,  370.568726f,  463.816498f,  382.417236f,
        529.793457f,  369.539734f,  467.725739f,  380.518829f,  431.195282f,  370.189880f,
        405.354858f,  357.036530f,  405.343933f,  375.865173f,  383.151550f,  384.863617f,
        438.204590f,  372.726776f,  447.941254f,  390.791351f,  444.733337f,  375.296906f,
        416.031342f,  369.507294f,  430.530762f,  380.611084f,  372.256866f,  366.240356f,
        204.934677f,  406.695343f,  259.845642f,  436.100311f,  373.697906f,  402.840424f,
        390.887970f,  418.066345f,  355.701904f,  400.155640f,  398.758606f,  381.650238f,
        501.306305f,  305.139832f,  307.169006f,  276.612335f,  326.995819f,  386.952515f,
        339.801697f,  405.187988f,  325.043365f,  405.539368f,  322.713593f,  409.723999f,
        334.887665f,  397.700684f,  321.924652f,  410.076599f,  294.593262f,  409.467926f,
        297.277710f,  384.386047f,  357.497192f,  391.343323f,  340.337769f,  393.219696f,
        359.828857f,  401.848053f,  335.392639f,  415.605133f,  350.920410f,  403.218445f,
        352.153778f,  403.450500f,  368.569824f,  390.060516f,  347.878143f,  361.895905f,
        333.013397f,  410.128937f,  367.016510f,  412.398071f,  357.060913f,  424.069824f,
        359.817566f,  414.740448f,  358.010559f,  421.953522f,  360.731598f,  413.343445f,
        361.540161f,  402.672089f,  359.164948f,  402.695282f,  344.794891f,  426.580475f,
        355.931610f,  420.812195f,  358.601715f,  422.606720f,  367.689392f,  435.234161f,
        373.789215f,  425.793884f,  357.113525f,  427.733612f,  362.551086f,  415.783722f,
        355.178589f,  400.534576f,  344.439240f,  415.148010f,  353.774994f,  431.970520f,
        371.448975f,  435.970306f,  398.457642f,  426.473694f,  385.752747f,  424.361664f,
        381.668488f,  436.871429f,  361.080811f,  421.514374f,  356.596558f,  419.187195f,
        364.528198f,  409.866913f,  361.378143f,  434.661774f,  365.536713f,  433.388336f,
        392.180237f,  423.213684f,  393.958984f,  425.663635f,  428.566986f,  421.708862f,
        460.573975f,  414.207764f,  390.310242f,  414.945343f,  362.669098f,  414.906921f,
        350.102478f,  443.346191f,  390.844971f,  423.019348f,  360.600769f,  445.102631f,
        378.270569f,  423.307861f,  421.615417f,  417.150055f,  322.196106f,  442.118408f,
        328.289307f,  395.502258f,  357.159363f,  425.060333f,  370.977661f,  441.120544f,
        293.183411f,  439.228363f,  374.276611f,  425.614807f,  339.418213f,  434.187744f,
        319.457520f,  423.797363f,  362.746185f,  398.824158f,  449.569672f,  398.433197f,
        575.870422f,  690.733459f,  571.121460f,  695.169800f,  571.414978f,  696.596680f,
        566.031616f,  694.021912f,  570.159485f,  690.306335f,  561.026794f,  698.226013f,
        549.003723f,  694.241638f,  543.075195f,  683.229492f,  555.909790f,  704.203491f,
        569.521973f,  693.735596f,  558.781006f,  711.557678f,  553.295654f,  705.669678f,
        555.973694f,  713.236084f,  567.122742f,  696.085938f,  535.808533f,  707.714783f,
        501.339142f,  716.639954f,  555.128296f,  707.168579f,  551.750061f,  713.588135f,
        547.663025f,  719.229797f,  541.529480f,  723.102539f,  552.022827f,  714.279175f,
        536.464722f,  720.156982f,  547.585449f,  712.450745f,  523.374878f,  720.946838f,
        559.085022f,  729.090454f,  549.694519f,  733.714050f,  548.336853f,  737.564209f,
        537.530762f,  736.627502f,  546.240112f,  729.941589f,  549.299133f,  735.706543f,
        530.024170f,  745.493530f,  520.120911f,  731.436707f,  560.928162f,  743.672852f,
        583.159180f,  734.599182f,  591.371887f,  729.482483f,  583.221069f,  728.986267f,
        582.192017f,  722.517456f,  566.373413f,  732.999451f,  571.895020f,  739.696167f,
        542.822998f,  742.427856f,  574.040100f,  745.181335f,  599.539795f,  737.205627f,
        616.033813f,  730.114075f,  606.943665f,  723.215271f,  638.313354f,  722.870178f,
        641.387329f,  724.730347f,  648.918091f,  726.908569f,  593.279297f,  748.626892f,
        591.066895f,  749.086365f,  577.165100f,  755.571899f,  622.383911f,  741.949402f,
        632.361450f,  737.532410f,  612.305664f,  749.788208f,  642.850769f,  723.570740f,
        675.301147f,  719.030762f,  620.104919f,  736.027649f,  598.933044f,  733.337708f,
        629.989380f,  733.149475f,  598.935364f,  737.392517f,  600.305237f,  736.219727f,
        590.192139f,  747.239380f,  556.567871f,  757.301331f,  613.182129f,  748.476135f,
        609.948486f,  741.307922f,  1374.805054f, 1158.489502f, 1359.689209f, 1190.421509f,
        1354.559937f, 1183.738159f, 1347.542236f, 1205.248169f, 1344.931641f, 1186.706665f,
        1326.668213f, 1189.033936f, 1327.851074f, 1141.988892f, 1343.037109f, 1216.538574f,
        1366.363281f, 1206.148315f, 1375.619019f, 1166.152344f, 1370.202515f, 1185.356934f,
        1360.970947f, 1193.759521f, 1356.536255f, 1217.001587f, 1358.137207f, 1165.443970f,
        1381.199829f, 1100.320679f, 1328.805908f, 1146.837280f, 1364.597900f, 1194.803467f,
        1360.921143f, 1242.921997f, 1345.845337f, 1248.232056f, 1354.463013f, 1235.295532f,
        1346.847168f, 1250.025146f, 1346.852783f, 1273.338501f, 1360.489014f, 1217.104248f,
        1338.820801f, 1248.161133f, 1355.756226f, 1253.467407f, 1359.045898f, 1252.187744f,
        1346.963989f, 1271.768677f, 1348.479492f, 1296.805908f, 1343.350708f, 1315.364502f,
        1337.827148f, 1316.094849f, 1348.719116f, 1288.088257f, 1341.055298f, 1303.955566f,
        1346.164795f, 1289.979370f, 1338.530884f, 1310.844604f, 1344.601318f, 1309.845459f,
        1336.769897f, 1347.168701f, 1332.197510f, 1339.584473f, 1352.600830f, 1322.566406f,
        1354.754028f, 1314.041992f, 1373.173218f, 1281.605347f, 1368.158569f, 1280.177124f,
        1364.851562f, 1256.324585f, 1340.061523f, 1345.317139f, 1325.377563f, 1351.158325f,
        1349.816895f, 1350.614014f, 1373.496094f, 1345.512207f, 1415.337524f, 1217.039917f,
        1361.667358f, 1323.929688f, 1357.379639f, 1261.888062f, 1346.869629f, 1286.609375f,
        1350.120850f, 1300.933105f, 1350.069702f, 1341.496704f, 1314.929199f, 1387.313354f,
        1367.634644f, 1325.114990f, 1376.949463f, 1270.334595f, 1451.033203f, 1206.838257f,
        1389.386597f, 1230.865479f, 1405.248657f, 1221.654297f, 1385.116333f, 1267.088013f,
        1312.295166f, 1355.519165f, 1349.560425f, 1340.683960f, 1408.972168f, 1284.478882f,
        1441.804077f, 1222.258667f, 1305.698975f, 1365.606201f, 64.939087f,   -24.437710f,
        22.020155f,   -6.702758f,   -42.752396f,  6.560041f,    -58.249123f,  -0.609756f,
        -39.330627f,  -15.078996f,  -52.940113f,  11.702255f,   25.593557f,   3.863709f,
        56.936615f,   -10.942957f,  15.780871f,   -15.245796f,  -13.969156f,  -5.306598f,
        -67.185760f,  4.079074f,    -133.440247f, 15.249143f,   -117.417885f, 6.198556f,
        -73.750328f,  5.579896f,    -5.060076f,   -8.900596f,   28.396885f,   -21.751198f,
        -17.315243f,  -23.978676f,  -61.460564f,  -0.125010f,   -100.004204f, 1.403646f,
        -97.641106f,  2.265431f,    -132.969147f, 8.968231f,    -118.698036f, 5.772081f,
        -71.886551f,  -10.088165f,  -43.814659f,  -31.289597f,  -2.331308f,   -10.827285f,
        10.436854f,   -20.732746f,  -68.235123f,  -6.280299f,   -63.640629f,  -5.475164f,
        -79.981468f,  -6.176116f,   -83.926102f,  -1.022575f,   -48.902607f,  -21.489330f,
        -102.853287f, -23.216198f,  32.621082f,   -4.765327f,   52.611973f,   -11.999960f,
        26.318880f,   -12.369388f,  47.199814f,   -21.630232f,  1.908749f,    -7.498601f,
        24.365133f,   -14.254667f,  27.471996f,   -18.366093f,  -97.783661f,  -14.595146f,
        121.666214f,  -23.706417f,  131.151138f,  -14.792420f,  112.453163f,  -13.192354f,
        109.602287f,  -17.182276f,  134.287796f,  -34.343109f,  132.386765f,  -5.299368f,
        50.147053f,   12.424313f,   110.032486f,  -7.898591f,   192.191895f,  -29.821787f,
        79.524223f,   3.027062f,    81.252937f,   6.376310f,    155.620316f,  -33.614445f,
        26.005547f,   -20.397827f,  225.774506f,  -113.797852f, 127.327530f,  17.477076f,
        38.148346f,   40.072739f,   248.086761f,  -93.898071f,  248.059250f,  -74.041618f,
        69.157799f,   0.230419f,    57.816814f,   -9.094748f,   80.280846f,   -37.252583f,
        79.625504f,   -31.649998f,  42.580963f,   0.270627f,    998.849060f,  -341.983673f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    14.253705f,   106.307243f,
        -19.225090f,  131.712204f,  24.599054f,   111.701881f,  -5.082323f,   153.087631f,
        37.717976f,   116.263855f,  86.740326f,   116.788887f,  65.864311f,   111.023796f,
        38.631626f,   118.737694f,  12.415921f,   137.698166f,  -0.736888f,   142.203979f,
        11.016439f,   134.735703f,  14.896775f,   154.447876f,  0.761668f,    160.988831f,
        49.691395f,   132.847733f,  50.603088f,   132.154724f,  11.894826f,   123.014954f,
        26.988316f,   192.683395f,  29.916958f,   167.947586f,  50.693066f,   171.292099f,
        57.600891f,   144.393341f,  85.213921f,   150.570526f,  56.973362f,   170.267288f,
        25.627592f,   158.687241f,  48.656361f,   151.212784f,  89.847206f,   279.071198f,
        53.260563f,   257.940216f,  80.893951f,   239.364731f,  97.970871f,   216.230652f,
        117.563629f,  206.412582f,  159.276550f,  148.292297f,  113.724640f,  176.722336f,
        153.123642f,  173.359802f,  73.478920f,   313.960999f,  59.353390f,   399.552399f,
        103.532715f,  261.875031f,  144.777863f,  276.357635f,  215.737335f,  201.112259f,
        193.920135f,  196.295273f,  139.665771f,  203.202652f,  228.663773f,  225.220703f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        -18.010382f,  105.558762f,  7.146909f,    74.371246f,   -14.384128f,  85.313217f,
        32.209980f,   78.965431f,   -6.484409f,   101.220909f,  57.918228f,   43.816841f,
        44.592903f,   52.217434f,   -16.759157f,  62.303467f,   12.365822f,   98.405663f,
        68.931290f,   70.296196f,   9.420106f,    94.125717f,   9.005950f,    70.896774f,
        -7.901588f,   76.803474f,   8.769964f,    74.595612f,   39.123062f,   84.286880f,
        28.618618f,   65.741165f,   90.097328f,   130.323074f,  116.447151f,  109.924408f,
        55.824905f,   111.950157f,  73.894157f,   89.758209f,   81.523216f,   90.718422f,
        97.353554f,   107.220352f,  73.195961f,   140.481232f,  58.989700f,   123.812210f,
        181.184891f,  202.688278f,  187.520523f,  194.055359f,  165.458649f,  168.458328f,
        160.408157f,  130.125183f,  178.796371f,  116.923019f,  168.029556f,  123.688255f,
        128.864502f,  175.499619f,  150.823792f,  158.218246f,  316.679230f,  271.677490f,
        305.691559f,  276.597351f,  282.975739f,  227.966797f,  275.310852f,  206.045639f,
        318.396820f,  160.600418f,  343.465637f,  145.333710f,  246.333557f,  211.452576f,
        210.856155f,  220.104630f,  531.699768f,  304.770660f,  539.877625f,  297.072601f,
        476.041962f,  297.123901f,  482.104309f,  220.486526f,  521.809570f,  178.137482f,
        447.447510f,  205.303909f,  246.921585f,  276.011658f,  174.874283f,  299.853790f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    15.554724f,   10.662051f,
        31.665174f,   44.623020f,   6.760353f,    26.075603f,   10.141294f,   15.482363f,
        -0.329261f,   -0.319015f,   -0.197313f,   -0.165344f,   -0.084815f,   0.098269f,
        27.869413f,   11.924322f,   35.757458f,   -1.371875f,   24.545563f,   -1.050617f,
        13.388550f,   -0.690889f,   0.000000f,    0.000000f,    -15.294339f,  1.235433f,
        -26.080791f,  -7.803834f,   -47.394680f,  41.798737f,   -88.281715f,  -97.656944f,
        38.984360f,   -0.958726f,   33.120197f,   -14.353688f,  17.145817f,   -2.809236f,
        8.881753f,    5.273441f,    -0.000000f,   0.000000f,    -14.933272f,  9.456378f,
        -18.368696f,  13.514176f,   -38.995266f,  92.070976f,   -83.713310f,  60.047081f,
        32.378593f,   75.577034f,   1.816204f,    -15.119501f,  -61.976395f,  -9.271462f,
        20.520420f,   4.387483f,    37.903706f,   0.481778f,    20.052422f,   15.077958f,
        30.790773f,   -27.628872f,  -5.940003f,   -56.914280f,  7.409318f,    16.940472f,
        -241.135071f, 75.840347f,   -130.142380f, 35.526306f,   22.370577f,   -27.182037f,
        11.949778f,   -10.833912f,  0.000000f,    0.000000f,    0.431870f,    10.346597f,
        -0.272681f,   32.492069f,   -9.839361f,   68.785034f,   -29.559530f,  135.532364f,
        -472.400238f, 190.802490f,  -141.055481f, 48.961300f,   31.324856f,   -31.645527f,
        29.797106f,   -30.878048f,  0.000000f,    0.000000f,    -10.109044f,  27.008869f,
        -18.491909f,  54.696911f,   -58.086693f,  111.189774f,  -185.031113f, 258.973938f,
        181.382614f,  -68.305466f,  97.103271f,   -98.164574f,  -71.009926f,  18.749474f,
        -67.505318f,  18.744507f,   0.000000f,    0.000000f,    51.174767f,   -17.892609f,
        109.060272f,  -55.851105f,  248.773148f,  -209.194305f, 569.285034f,  -607.692932f,
        0.000000f,    0.000000f,    -0.634232f,   3.555114f,    5.133502f,    3.511007f,
        3.060750f,    3.568280f,    2.575608f,    -0.660526f,   0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        -9.581265f,   -30.709208f,  -8.613455f,   -5.915541f,   -7.546202f,   1.099167f,
        -6.166494f,   3.434779f,    -3.302766f,   2.882154f,    0.000000f,    0.000000f,
        0.669572f,    -3.085094f,   -6.986086f,   -7.124352f,   95.439484f,   -34.751015f,
        14.701268f,   -39.013134f,  13.191113f,   -16.506590f,  9.884469f,    -7.167289f,
        8.422940f,    -3.522031f,   6.085015f,    -1.586389f,   0.000000f,    0.000000f,
        -2.508381f,   -2.270037f,   1.996757f,    -6.568850f,   24.156992f,   -19.292377f,
        11.912487f,   -29.678640f,  7.070137f,    -9.901136f,   7.034487f,    -8.641287f,
        4.433893f,    -5.788674f,   0.848413f,    -2.046728f,   0.000000f,    0.000000f,
        -1.643324f,   -1.009398f,   1.549107f,    -4.827145f,   1.439159f,    -9.015787f,
        1.779646f,    -107.187935f, 5.669124f,    -47.534218f,  4.289817f,    -29.371178f,
        3.817399f,    -22.088144f,  0.535124f,    1.634720f,    0.000000f,    0.000000f,
        -4.108400f,   16.129272f,   -4.630641f,   24.191273f,   -2.893876f,   28.177595f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        6.754020f,    -1.005675f,   1.916517f,    1.614515f,    2.234151f,    -0.385934f,
        2.381508f,    0.265876f,    3.430084f,    -4.720749f,   5.683722f,    -1.042262f,
        4.230939f,    2.000234f,    7.555316f,    12.697673f,   -1.319880f,   6.092254f,
        -2.611664f,   -4.558058f,   0.716332f,    0.940034f,    -2.600587f,   -0.385249f,
        4.772781f,    -5.494966f,   4.479489f,    -8.470465f,   -2.135281f,   -2.397400f,
        0.016795f,    -7.045762f,   -8.455427f,   -6.758076f,   2.213667f,    -7.970308f,
        20.000000f,   10.000000f,   -0.671493f,   -7.532896f,   -6.319156f,   -44.664467f,
        8.943734f,    -19.601805f,  6.851574f,    4.637834f,    -47.906921f,  -30.672361f,
        -4.621399f,   -9.567876f,   4.574003f,    12.007783f,   -41.239365f,  -33.862343f,
        -14.410889f,  -0.824872f,   4.372681f,    9.080774f,    -55.961029f,  -3.787909f,
        -7.849267f,   7.273794f,    3.845678f,    35.445522f,   -68.669319f,  -25.500423f,
        -49.554977f,  26.984182f,   0.000000f,    0.000000f,    37.858616f,   -54.188370f,
        33.019085f,   -51.004314f,  31.864494f,   -49.336449f,  46.320175f,   -57.695129f,
        59.219376f,   -66.163643f,  99.765869f,   -80.657639f,  177.824982f,  -91.966537f,
        81.459106f,   -45.610844f,  0.000000f,    0.000000f,    -59.130127f,  50.039604f,
        -33.620876f,  41.623650f,   -35.047855f,  43.021049f,   -43.653049f,  43.956154f,
        -46.126717f,  47.627346f,   0.000000f,    0.000000f,    0.000000f,    0.000000f,
        2.671582f,    3.634412f,    69.849297f,   -44.149368f,  147.649597f,  -89.717621f,
        241.001038f,  -140.461563f, 342.311066f,  -193.865707f, -11.182647f,  0.237949f,
        -19.508127f,  -54.695259f,  -24.134130f,  -55.607975f,  -45.785442f,  -23.930620f,
        -24.551764f,  -37.769333f,  -13.339035f,  11.986656f,   -9.280603f,   5.859504f,
        -22.258375f,  35.941891f,   6.641151f,    1.863730f,    3.523339f,    1.122622f,
        -3.121153f,   -6.288151f,   -22.943949f,  -2.624938f,   -27.570412f,  -12.345243f,
        -50.240299f,  11.991314f,   -95.129173f,  56.076138f,   17.485184f,   4.426315f,
        0.063117f,    -5.538268f,   -40.959305f,  -2.675487f,   -71.919762f,  0.260941f,
        -154.417267f, 76.593819f,   -294.101379f, 211.736984f,  -265.738281f, 182.117676f};

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
