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
        0.209377f,    1.076343f,    0.298593f,    0.208367f,    0.361808f,    1.256754f,
        1.000000f,    -0.004605f,   0.000000f,    -0.031249f,   -0.002046f,   83.460716f,
        30.974611f,   159.419693f,  410.391449f,  407.297363f,  376.157043f,  416.969879f,
        601.474426f,  747.006470f,  1382.466797f, 1289.845093f, 0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    -7.981252f,   155.172119f,
        -7.366622f,   164.045990f,  1.008801f,    162.012619f,  -13.869894f,  161.881500f,
        5.224667f,    189.868759f,  53.360424f,   166.134506f,  56.318726f,   154.382416f,
        13.036353f,   148.925797f,  5.030245f,    152.494080f,  7.024651f,    157.003403f,
        17.160240f,   157.573029f,  11.911692f,   165.674179f,  22.524448f,   170.741287f,
        38.948910f,   160.564468f,  47.058220f,   149.459473f,  28.698301f,   148.237656f,
        21.033663f,   161.099930f,  31.160118f,   163.812668f,  41.707588f,   161.776596f,
        46.100319f,   160.226761f,  49.230721f,   158.426346f,  59.049934f,   159.389282f,
        52.758404f,   162.624954f,  30.666767f,   157.935364f,  47.778641f,   178.040802f,
        44.956367f,   185.627274f,  53.898415f,   183.028824f,  69.055725f,   159.589523f,
        88.531593f,   168.550293f,  78.384819f,   155.477127f,  63.897976f,   172.227417f,
        61.177650f,   159.542831f,  76.239372f,   170.222366f,  69.689461f,   190.943466f,
        86.385590f,   184.386032f,  85.767136f,   171.821121f,  116.835014f,  158.151489f,
        157.835510f,  175.389908f,  150.155914f,  176.120514f,  110.034927f,  167.804382f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    323.457031f,  392.598816f,  360.075531f,  378.314362f,
        343.410278f,  405.300842f,  374.924591f,  400.610962f,  369.519012f,  382.694977f,
        380.920349f,  363.263794f,  354.682892f,  388.968262f,  320.017181f,  369.074860f,
        352.308258f,  359.323822f,  367.676147f,  412.516693f,  388.946320f,  404.192657f,
        400.399048f,  404.736206f,  390.861176f,  397.829071f,  405.039948f,  387.578949f,
        376.688477f,  380.826233f,  389.275574f,  375.282379f,  377.235168f,  381.860809f,
        403.910858f,  394.808838f,  407.452362f,  408.030670f,  424.249298f,  422.810303f,
        433.819153f,  414.112091f,  410.925110f,  391.218109f,  410.516479f,  375.451721f,
        377.197815f,  394.641449f,  406.255798f,  399.285095f,  439.061676f,  398.998260f,
        444.560913f,  417.735992f,  432.922180f,  429.118927f,  443.756683f,  433.815277f,
        433.975159f,  429.590576f,  433.408722f,  408.291016f,  397.360046f,  389.405609f,
        422.109009f,  407.667145f,  434.184692f,  422.607513f,  462.548767f,  418.446075f,
        497.182495f,  420.423431f,  464.243652f,  423.597412f,  485.334778f,  426.268677f,
        445.310669f,  418.748566f,  461.138702f,  395.782990f,  399.119537f,  415.094299f,
        465.658112f,  405.407104f,  498.710388f,  409.863342f,  505.048401f,  414.522827f,
        558.578003f,  406.126953f,  504.630157f,  417.017242f,  473.887238f,  395.874084f,
        390.359711f,  397.607513f,  425.444153f,  392.151886f,  408.951324f,  414.590790f,
        467.064728f,  403.406708f,  470.021118f,  416.704742f,  481.116364f,  414.203857f,
        464.524445f,  400.480255f,  443.961578f,  405.693573f,  413.338287f,  401.823273f,
        226.801270f,  428.909424f,  379.841522f,  416.441986f,  401.314514f,  404.746674f,
        441.339691f,  423.794830f,  390.563446f,  438.433807f,  380.196564f,  411.526062f,
        581.944458f,  304.245605f,  212.461975f,  377.642761f,  349.162720f,  391.046265f,
        370.002167f,  395.105164f,  349.247772f,  403.822357f,  345.740265f,  413.204742f,
        356.737762f,  409.858643f,  343.551117f,  413.658112f,  340.110077f,  390.800201f,
        326.159332f,  374.306824f,  375.823792f,  406.918823f,  368.307587f,  388.687103f,
        385.641663f,  402.927582f,  360.087006f,  420.739899f,  376.397949f,  410.206207f,
        374.987640f,  404.525909f,  396.854279f,  389.702057f,  358.513123f,  360.663055f,
        353.321594f,  415.068146f,  387.510681f,  419.391846f,  380.455627f,  425.208557f,
        386.002014f,  419.331757f,  383.698395f,  423.269409f,  386.337830f,  408.351837f,
        391.762421f,  408.519257f,  383.214050f,  405.725494f,  371.173859f,  426.183044f,
        369.733459f,  425.709503f,  381.881348f,  428.339325f,  385.213379f,  437.602325f,
        401.371643f,  425.572906f,  379.355438f,  434.018097f,  380.764130f,  427.598541f,
        380.704529f,  402.833557f,  368.397491f,  412.633148f,  382.633667f,  428.826416f,
        400.688904f,  432.608063f,  430.129852f,  422.629272f,  411.402222f,  430.818604f,
        406.869751f,  432.927673f,  381.256042f,  428.112701f,  383.152252f,  416.706390f,
        381.305786f,  412.827698f,  406.716675f,  427.799042f,  389.690704f,  428.593262f,
        427.573822f,  423.458252f,  412.668488f,  427.505981f,  456.270416f,  428.670135f,
        466.926147f,  417.046356f,  413.503448f,  408.987396f,  372.103851f,  433.152618f,
        378.930634f,  440.408356f,  401.070068f,  429.506226f,  383.920502f,  438.639008f,
        394.333344f,  416.529205f,  442.157593f,  414.161774f,  366.141418f,  433.911469f,
        369.689148f,  389.502411f,  385.307953f,  429.486450f,  406.132690f,  432.031708f,
        349.959229f,  437.175171f,  369.113312f,  429.202667f,  391.830566f,  424.580078f,
        443.644562f,  414.516724f,  404.491943f,  394.238953f,  423.939026f,  395.366302f,
        609.817505f,  727.405579f,  608.541687f,  723.209534f,  605.921387f,  732.426514f,
        601.889160f,  729.261230f,  605.763123f,  723.483154f,  590.857849f,  732.923523f,
        577.172363f,  728.632812f,  582.353333f,  714.585388f,  578.167542f,  740.006653f,
        600.520813f,  735.705078f,  598.752197f,  736.743347f,  585.800293f,  743.335510f,
        595.546265f,  740.570984f,  597.545105f,  727.713379f,  567.815125f,  739.759888f,
        522.518311f,  753.908630f,  597.499695f,  735.509949f,  595.188416f,  732.395020f,
        573.749878f,  751.569763f,  569.011047f,  753.952087f,  588.732361f,  743.034485f,
        575.913574f,  740.095398f,  582.284546f,  741.539246f,  564.721802f,  736.890747f,
        581.327698f,  764.646729f,  582.429382f,  764.496094f,  588.423340f,  758.958435f,
        581.104736f,  760.928528f,  593.832153f,  754.600037f,  575.837463f,  761.109802f,
        559.917419f,  768.368042f,  562.243835f,  760.553223f,  591.437073f,  775.342773f,
        612.438904f,  764.788574f,  624.536987f,  755.423706f,  632.091370f,  753.385620f,
        623.846069f,  756.661865f,  603.021423f,  756.792236f,  598.851868f,  772.666443f,
        615.633179f,  760.280823f,  600.789551f,  779.257080f,  631.193542f,  766.004517f,
        638.951904f,  759.790344f,  650.735291f,  746.728333f,  684.126892f,  747.443237f,
        661.762878f,  755.365356f,  666.347839f,  763.925232f,  628.652527f,  776.264709f,
        626.768555f,  779.352173f,  614.282227f,  785.147461f,  660.327148f,  770.118286f,
        660.355042f,  773.330139f,  628.855164f,  783.435547f,  659.135925f,  761.953125f,
        697.850159f,  751.720276f,  669.273254f,  759.135742f,  638.388794f,  766.676270f,
        658.199707f,  768.603455f,  638.089050f,  770.879395f,  639.640991f,  769.754639f,
        631.079163f,  774.012451f,  609.904114f,  785.131165f,  644.879822f,  776.209961f,
        608.552917f,  788.288818f,  1417.305054f, 1199.282471f, 1385.110352f, 1246.646973f,
        1390.583740f, 1197.620483f, 1373.454346f, 1247.683838f, 1368.644531f, 1222.274414f,
        1364.320435f, 1198.673218f, 1307.499756f, 1253.146606f, 1381.075439f, 1207.333862f,
        1383.183350f, 1256.764526f, 1397.733398f, 1221.493164f, 1398.703003f, 1220.646606f,
        1394.054321f, 1207.879150f, 1386.709473f, 1249.104492f, 1389.984375f, 1191.205444f,
        1387.812012f, 1158.211914f, 1341.483521f, 1259.968506f, 1399.731323f, 1218.299927f,
        1386.183716f, 1273.849609f, 1372.392334f, 1300.534180f, 1379.230469f, 1270.152100f,
        1372.179321f, 1299.694092f, 1375.957520f, 1319.963989f, 1381.839111f, 1276.589478f,
        1370.953613f, 1279.715210f, 1382.977783f, 1309.590576f, 1380.436646f, 1295.576050f,
        1373.953979f, 1311.657471f, 1373.168091f, 1337.614868f, 1368.173584f, 1364.359009f,
        1366.966919f, 1358.523804f, 1381.438232f, 1322.550049f, 1360.619263f, 1356.449951f,
        1364.468140f, 1340.373169f, 1370.315186f, 1345.340454f, 1381.375854f, 1349.964233f,
        1355.904541f, 1418.450684f, 1381.622314f, 1368.668823f, 1375.415527f, 1400.074341f,
        1381.465576f, 1356.783081f, 1398.278076f, 1354.919556f, 1382.183105f, 1332.511108f,
        1391.602905f, 1310.270020f, 1364.672119f, 1393.347168f, 1362.569214f, 1395.092773f,
        1387.879883f, 1393.550171f, 1458.274536f, 1332.697632f, 1474.097656f, 1269.078003f,
        1408.546021f, 1310.344604f, 1364.476807f, 1341.381714f, 1363.001587f, 1355.528931f,
        1388.464233f, 1340.181885f, 1402.106934f, 1354.533569f, 1367.844849f, 1406.727783f,
        1422.664429f, 1339.676636f, 1425.167358f, 1307.392944f, 1481.530396f, 1267.782104f,
        1392.367798f, 1318.733154f, 1387.372192f, 1340.594727f, 1427.201660f, 1300.107422f,
        1329.462158f, 1410.742310f, 1393.861572f, 1362.082520f, 1428.755615f, 1334.156128f,
        1379.346436f, 1364.574829f, 1348.774414f, 1417.264282f, 57.837830f,   -28.248436f,
        27.715097f,   -8.936336f,   -41.960533f,  6.102252f,    -56.020641f,  -1.264771f,
        -42.392235f,  -20.242502f,  -57.221775f,  11.770200f,   31.106365f,   -0.421192f,
        59.594360f,   -23.460272f,  -1.500102f,   -9.665091f,   -14.322089f,  -4.392024f,
        -69.308022f,  2.407557f,    -131.640320f, 12.712309f,   -121.053871f, 7.538695f,
        -78.074821f,  7.546291f,    -13.367278f,  -2.793544f,   28.259146f,   -21.562906f,
        -35.274601f,  -19.857267f,  -84.139534f,  -1.433430f,   -115.618889f, -1.433606f,
        -131.852539f, 5.653846f,    -123.868240f, 3.390865f,    -139.340118f, 9.926499f,
        -66.536125f,  -14.141872f,  -49.207531f,  -31.014349f,  -7.450795f,   -17.972132f,
        22.859953f,   -26.892826f,  -29.492590f,  -15.195065f,  -88.592171f,  -4.334544f,
        -104.239960f, -2.522965f,   -93.111519f,  -2.129166f,   -97.236153f,  -12.916639f,
        -126.897530f, -23.366316f,  -0.756785f,   -8.209523f,   79.364960f,   -25.289471f,
        -5.673178f,   -4.677161f,   10.134330f,   -11.081767f,  -17.189968f,  -13.068057f,
        -9.303842f,   -17.440599f,  24.596769f,   -21.611511f,  -92.353554f,  -16.521263f,
        38.016987f,   13.367643f,   113.491287f,  -6.964252f,   85.542961f,   -3.076119f,
        111.556999f,  -23.986721f,  41.836708f,   -8.507339f,   180.833282f,  -20.493547f,
        11.953950f,   15.039312f,   208.930817f,  -44.002789f,  148.461960f,  -32.882248f,
        89.760239f,   19.698755f,   81.401726f,   9.490510f,    71.014626f,   -2.002334f,
        101.131371f,  -34.591839f,  76.886688f,   -21.758774f,  -5.950140f,   27.102037f,
        109.230499f,  -16.072922f,  220.829025f,  -62.859745f,  220.705566f,  -78.081207f,
        153.175491f,  -11.916529f,  173.896561f,  -14.589675f,  82.900925f,   -44.439095f,
        10.643536f,   -34.802307f,  142.104797f,  -18.166243f,  391.722748f,  -191.627991f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    28.489763f,   126.317390f,
        -13.171693f,  140.331787f,  24.050524f,   136.445618f,  18.363882f,   118.503647f,
        41.601311f,   122.870857f,  99.737442f,   148.070404f,  88.738342f,   122.719193f,
        62.607647f,   133.014389f,  20.959606f,   159.451233f,  31.739040f,   160.306274f,
        23.419754f,   154.562653f,  25.315006f,   176.202362f,  29.867811f,   175.589539f,
        50.327698f,   163.753006f,  75.614395f,   141.577576f,  30.450766f,   144.996994f,
        66.745697f,   196.208771f,  50.842358f,   180.496216f,  74.198326f,   175.127090f,
        73.123238f,   178.031952f,  97.563950f,   164.334106f,  79.705177f,   181.006241f,
        46.998737f,   175.989227f,  39.641979f,   188.147751f,  93.536560f,   300.994171f,
        63.431454f,   310.715546f,  108.718315f,  233.567444f,  103.489342f,  257.988892f,
        152.137848f,  207.882416f,  187.700623f,  166.812027f,  117.594757f,  246.265869f,
        171.646561f,  203.498993f,  111.016777f,  285.707947f,  53.185307f,   434.512054f,
        103.470940f,  286.781036f,  177.060608f,  248.224258f,  267.614471f,  214.469727f,
        192.719818f,  263.362946f,  236.842911f,  188.506531f,  225.548431f,  209.986465f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        47.873878f,   73.178207f,   61.334068f,   61.825104f,   43.353226f,   72.693398f,
        55.094723f,   72.611519f,   83.991516f,   80.395691f,   101.088921f,  39.393353f,
        100.077187f,  38.564804f,   47.545734f,   56.483925f,   66.517784f,   84.860779f,
        92.666519f,   61.412815f,   71.078934f,   68.917870f,   62.126869f,   61.443893f,
        58.784500f,   74.717674f,   78.659500f,   74.996109f,   96.689682f,   67.268539f,
        95.495247f,   51.069038f,   129.032669f,  116.940620f,  141.514191f,  106.928627f,
        104.317421f,  100.540695f,  120.458580f,  85.667854f,   123.123825f,  87.830360f,
        122.376114f,  100.061516f,  100.827057f,  130.126282f,  98.798775f,   120.824341f,
        199.763580f,  189.464157f,  207.756073f,  185.371872f,  168.996521f,  161.066910f,
        185.583939f,  125.143425f,  197.244446f,  98.382904f,   191.964630f,  99.073448f,
        174.804688f,  153.461121f,  175.733185f,  148.757019f,  320.655609f,  248.671783f,
        285.306244f,  268.559753f,  267.862671f,  230.779388f,  273.036072f,  190.158264f,
        311.827209f,  136.135239f,  342.982117f,  127.669952f,  252.503693f,  191.386963f,
        204.990707f,  208.294159f,  460.034485f,  279.794312f,  469.219177f,  295.649719f,
        378.230103f,  296.967010f,  436.767975f,  223.983887f,  383.934143f,  198.022827f,
        392.075836f,  185.359909f,  195.380630f,  258.776520f,  243.252502f,  244.044540f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    14.280543f,   10.905450f,
        10.054164f,   69.204941f,   8.895201f,    19.096739f,   15.334110f,   12.738427f,
        24.477304f,   24.181646f,   41.166435f,   2.401997f,    27.881359f,   3.648755f,
        14.392492f,   6.273845f,    0.000000f,    0.000000f,    -18.215162f,  7.054730f,
        -35.018181f,  1.274690f,    -34.788380f,  6.097174f,    -49.941742f,  -215.849548f,
        23.755392f,   16.698460f,   33.470963f,   -2.974562f,   15.993737f,   4.936944f,
        7.924431f,    11.356398f,   -0.000000f,   0.000000f,    -12.588601f,  5.847313f,
        -22.192877f,  13.618231f,   -16.708364f,  10.973735f,   -16.248999f,  -241.102936f,
        34.191246f,   83.863693f,   0.892575f,    -19.062771f,  -51.178181f,  -19.326290f,
        19.531557f,   12.859901f,   37.529202f,   0.027442f,    -292.305176f, 119.504997f,
        -122.314964f, 45.797932f,   27.087885f,   -19.004311f,  2.990160f,    -4.269757f,
        0.000000f,    0.000000f,    -2.285882f,   11.014387f,   -4.607213f,   36.163486f,
        -16.910091f,  60.738110f,   -36.271561f,  108.198593f,  -517.807190f, 212.137680f,
        -144.187561f, 53.299927f,   29.507927f,   -27.731318f,  33.830902f,   -27.870718f,
        0.000000f,    0.000000f,    -9.846741f,   23.992212f,   -21.474594f,  51.989655f,
        -55.560833f,  106.084999f,  -133.805328f, 204.819702f,  404.346741f,  -331.636932f,
        71.479584f,   -77.433487f,  -79.370293f,  22.138571f,   -52.586910f,  -10.798629f,
        0.000000f,    0.000000f,    25.112799f,   19.677586f,   74.481804f,   -5.708169f,
        189.577362f,  -136.856689f, 417.089966f,  -485.651093f, 2.173160f,    0.114394f,
        0.000000f,    0.000000f,    -0.527947f,   2.440063f,    5.661768f,    3.139741f,
        2.956587f,    3.642948f,    2.458528f,    1.035815f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        -12.322384f,  -27.452381f,  -9.691269f,   -6.453400f,   -7.484188f,   -1.349002f,
        -5.876655f,   1.081877f,    -4.267275f,   2.479959f,    0.000000f,    0.000000f,
        -1.671792f,   -3.423035f,   -1.931767f,   -6.906826f,   87.557693f,   -34.932987f,
        11.651179f,   -37.203613f,  11.224659f,   -15.747227f,  7.606238f,    -7.844730f,
        6.297994f,    -2.396564f,   2.814448f,    1.028512f,    0.000000f,    0.000000f,
        -3.661835f,   0.396553f,    3.245892f,    -4.484330f,   24.870386f,   -19.414001f,
        11.745167f,   -35.961674f,  5.448007f,    -15.727482f,  5.048510f,    -8.338324f,
        3.430392f,    -4.854366f,   -1.092947f,   -0.322354f,   0.000000f,    0.000000f,
        -2.210078f,   0.026185f,    0.154281f,    -4.127088f,   4.296644f,    -10.225636f,
        0.125794f,    -131.884949f, 1.888379f,    -69.996231f,  2.596013f,    -40.135662f,
        3.728827f,    -30.641024f,  0.593950f,    -9.824345f,   0.000000f,    0.000000f,
        -1.454950f,   -1.160164f,   -3.879215f,   7.855868f,    -3.882041f,   18.521988f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        6.531910f,    -1.021687f,   1.626106f,    1.156218f,    1.480145f,    -0.270156f,
        1.407112f,    0.415560f,    2.862033f,    -2.287051f,   5.693301f,    -2.212798f,
        4.990355f,    1.340869f,    6.193045f,    13.283972f,   -0.564255f,   5.951558f,
        -3.264493f,   -4.029655f,   1.139551f,    1.218804f,    -2.623172f,   -0.128051f,
        4.885772f,    -5.248683f,   3.758163f,    -4.865573f,   -2.536356f,   -1.894009f,
        -1.171480f,   -6.062824f,   -11.995502f,  -7.815558f,   1.707363f,    -9.590507f,
        20.000000f,   10.000000f,   -1.507702f,   -8.459846f,   -8.478421f,   -45.986263f,
        9.440317f,    -21.922895f,  5.726242f,    4.989628f,    -43.515362f,  -34.413193f,
        -5.124997f,   -13.205223f,  5.047799f,    12.780763f,   -44.726028f,  -32.393047f,
        -15.304029f,  0.364256f,    2.765474f,    10.367698f,   -59.492359f,  -3.653539f,
        -7.066264f,   5.307820f,    5.326268f,    25.855076f,   -62.960213f,  -40.484444f,
        -42.789520f,  4.398513f,    0.000000f,    0.000000f,    11.593657f,   -38.368382f,
        9.073562f,    -36.665710f,  25.182404f,   -39.461670f,  51.720638f,   -52.336327f,
        70.108307f,   -60.027344f,  115.538513f,  -71.646614f,  212.240128f,  -96.851128f,
        224.865738f,  -102.628654f, 0.000000f,    0.000000f,    -28.059330f,  36.518757f,
        -8.986190f,   27.922558f,   -0.146655f,   31.184259f,   -7.015280f,   32.002083f,
        -7.588386f,   46.612839f,   0.000000f,    0.000000f,    0.000000f,    0.000000f,
        -5.501234f,   1.156404f,    22.262846f,   -32.727798f,  58.610363f,   -64.475235f,
        109.605713f,  -99.549927f,  182.044510f,  -142.388290f, -10.719177f,  1.793822f,
        -13.130959f,  -51.428619f,  -19.837446f,  -55.175194f,  -52.029282f,  -26.779221f,
        -16.961458f,  -16.573992f,  -17.742514f,  16.266985f,   -11.686368f,  7.483015f,
        -26.793509f,  33.667511f};

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
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
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
