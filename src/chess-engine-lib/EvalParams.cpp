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
        0.209377f,    1.076343f,    0.333067f,    0.191856f,    0.332423f,    1.057246f,
        1.000000f,    -0.003396f,   -0.009253f,   -0.014366f,   0.002018f,    53.439747f,
        40.553093f,   148.755310f,  389.041779f,  380.860260f,  360.921539f,  400.694550f,
        564.665283f,  721.599548f,  1372.245117f, 1144.780029f, 0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    2.215646f,    149.321014f,
        -0.771325f,   153.775787f,  8.800369f,    153.303207f,  -5.362406f,   141.830139f,
        13.655074f,   172.568161f,  63.051674f,   154.700546f,  62.947880f,   144.844803f,
        22.398941f,   143.150345f,  15.771363f,   141.746307f,  13.855305f,   145.321121f,
        24.515005f,   149.358536f,  24.278488f,   148.173370f,  32.281952f,   156.875076f,
        53.237881f,   147.127243f,  55.870480f,   140.169724f,  40.819191f,   138.822113f,
        32.714020f,   149.288605f,  38.593483f,   155.006912f,  49.897888f,   153.568680f,
        60.310402f,   141.098038f,  60.225517f,   146.575241f,  71.274307f,   148.019913f,
        59.300568f,   152.679291f,  43.243401f,   147.634674f,  56.643795f,   167.020569f,
        56.449009f,   167.322052f,  66.899353f,   167.918060f,  79.180153f,   142.985580f,
        96.704514f,   155.371155f,  93.156067f,   147.660355f,  71.757042f,   163.332275f,
        70.065300f,   150.104126f,  75.907593f,   169.391235f,  74.139542f,   191.333054f,
        96.790794f,   172.519928f,  78.143181f,   141.420593f,  132.008469f,  164.009583f,
        173.880432f,  182.589325f,  144.800064f,  150.743484f,  121.539261f,  159.853683f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    349.344940f,  364.523224f,  342.016663f,  355.700806f,
        331.267670f,  368.441559f,  342.386627f,  367.794006f,  355.792755f,  358.158844f,
        356.370392f,  341.780884f,  329.455414f,  348.034088f,  307.232788f,  324.547638f,
        346.597717f,  342.954498f,  345.307343f,  390.016541f,  368.521606f,  379.592834f,
        379.127747f,  380.035492f,  366.075531f,  371.703308f,  377.576874f,  359.745880f,
        345.769623f,  369.150421f,  362.554810f,  332.546753f,  359.215515f,  365.944885f,
        382.434784f,  378.444244f,  387.018829f,  380.696411f,  411.481659f,  391.404785f,
        408.557678f,  386.495453f,  389.889984f,  360.240692f,  383.099457f,  356.339111f,
        349.395416f,  362.762085f,  388.353546f,  382.508820f,  405.396820f,  387.008057f,
        425.133820f,  390.810150f,  409.867584f,  402.947021f,  422.515747f,  410.919220f,
        414.922943f,  389.220276f,  418.768921f,  376.761658f,  378.021515f,  359.960815f,
        405.264099f,  382.977448f,  417.762634f,  398.722778f,  452.650848f,  387.728485f,
        473.653137f,  399.395203f,  439.504944f,  397.030518f,  456.389984f,  399.930481f,
        419.234161f,  396.116760f,  422.836975f,  367.242310f,  395.955170f,  374.902130f,
        448.753571f,  387.701111f,  475.528809f,  379.805420f,  482.675201f,  385.155548f,
        527.328979f,  376.011902f,  501.507538f,  381.596954f,  437.006744f,  396.742004f,
        394.331177f,  360.046204f,  388.033691f,  384.145050f,  382.288391f,  391.371918f,
        446.003662f,  376.153656f,  454.933563f,  386.423126f,  447.144379f,  388.696472f,
        458.358124f,  372.382874f,  408.172607f,  379.256561f,  367.533081f,  387.368958f,
        246.338913f,  461.157501f,  321.364777f,  417.860504f,  380.507080f,  392.842499f,
        360.589355f,  411.942688f,  391.369690f,  402.758026f,  400.960571f,  376.732117f,
        556.966858f,  310.669708f,  175.134354f,  384.438934f,  334.031921f,  386.466888f,
        358.370575f,  369.780121f,  334.078491f,  383.212585f,  336.032745f,  399.643250f,
        332.302979f,  387.790924f,  330.072632f,  396.106934f,  336.660156f,  381.408203f,
        327.158936f,  350.471954f,  370.211578f,  378.355286f,  353.503052f,  375.629456f,
        378.008514f,  381.426727f,  345.821381f,  401.892700f,  356.305481f,  397.981323f,
        366.463928f,  387.051910f,  380.675385f,  377.827972f,  351.875275f,  342.312988f,
        348.467834f,  395.798859f,  372.198547f,  396.260437f,  360.693665f,  412.414154f,
        370.421997f,  399.526001f,  366.712799f,  409.242920f,  371.987762f,  393.722351f,
        375.850311f,  386.604462f,  363.115753f,  395.199585f,  362.848969f,  415.860199f,
        356.315979f,  400.807556f,  369.477753f,  406.669647f,  377.153809f,  420.107971f,
        391.099243f,  411.690277f,  363.170685f,  420.456299f,  365.803528f,  409.267609f,
        368.258881f,  403.091034f,  362.038483f,  390.928467f,  362.343781f,  415.208435f,
        389.667114f,  413.228546f,  414.833893f,  408.470764f,  396.240326f,  413.057465f,
        407.617218f,  410.355621f,  365.370544f,  421.025787f,  363.628418f,  402.770172f,
        377.235260f,  405.173157f,  384.673401f,  408.211792f,  392.950958f,  411.155609f,
        402.785675f,  397.600250f,  421.472687f,  404.081329f,  409.456146f,  419.660461f,
        433.195648f,  406.064941f,  395.607727f,  399.934937f,  360.261292f,  412.907104f,
        364.133087f,  424.655975f,  387.169159f,  408.376495f,  391.982300f,  414.568451f,
        380.186066f,  412.462097f,  407.125610f,  398.754028f,  349.265533f,  416.648621f,
        344.474670f,  387.372284f,  343.582642f,  406.091614f,  429.341003f,  399.149170f,
        318.853333f,  418.394745f,  329.739990f,  409.396454f,  347.032288f,  417.956573f,
        358.590332f,  401.411255f,  402.399445f,  388.456543f,  396.673248f,  381.425232f,
        573.170227f,  702.275391f,  572.383240f,  702.472595f,  567.622192f,  707.626709f,
        566.199219f,  703.455383f,  567.601929f,  699.268616f,  555.534790f,  705.705750f,
        543.268982f,  706.602844f,  541.380249f,  695.131287f,  546.426086f,  715.964172f,
        563.154602f,  708.109863f,  560.955627f,  711.217346f,  552.443176f,  710.133850f,
        558.103088f,  715.127563f,  557.151917f,  706.108215f,  539.448975f,  713.936462f,
        500.889374f,  721.632385f,  539.620117f,  719.242004f,  537.240723f,  718.543335f,
        541.575500f,  726.306946f,  543.432983f,  725.379395f,  560.310730f,  714.478149f,
        540.214600f,  721.464172f,  549.364319f,  718.801819f,  518.509155f,  717.748413f,
        561.026306f,  730.313171f,  543.632324f,  734.768311f,  549.709045f,  733.292297f,
        554.907410f,  731.289917f,  549.341370f,  731.125916f,  537.072327f,  741.356689f,
        537.763428f,  738.893188f,  519.358826f,  735.316101f,  565.281006f,  743.639893f,
        570.117676f,  740.951599f,  586.305237f,  734.491089f,  586.454712f,  730.352600f,
        586.579773f,  724.592957f,  571.774353f,  739.514832f,  574.487610f,  739.630676f,
        563.278748f,  744.448914f,  572.431335f,  749.824036f,  595.792480f,  735.664612f,
        609.258606f,  729.797913f,  603.550415f,  724.683716f,  646.279785f,  727.364258f,
        631.459778f,  732.364563f,  646.872742f,  722.366943f,  601.415649f,  746.594971f,
        586.169006f,  754.030212f,  575.880859f,  757.459534f,  605.099121f,  753.700439f,
        622.777588f,  745.586975f,  616.460510f,  748.265137f,  634.712463f,  727.790222f,
        655.111877f,  727.917480f,  661.798889f,  725.101807f,  593.697876f,  738.943176f,
        609.958252f,  743.446655f,  602.228638f,  743.526062f,  594.618103f,  747.853699f,
        593.644165f,  750.206482f,  581.919922f,  756.147644f,  633.820129f,  750.140625f,
        568.160400f,  764.969727f,  1395.713623f, 1085.566528f, 1370.051147f, 1127.481934f,
        1382.748779f, 1059.562866f, 1364.960693f, 1120.496094f, 1367.261719f, 1069.430054f,
        1353.760986f, 1068.449585f, 1319.697632f, 1045.348511f, 1313.119873f, 1239.762207f,
        1371.451904f, 1153.295898f, 1381.149292f, 1099.163818f, 1384.754395f, 1100.373901f,
        1385.342896f, 1071.444702f, 1376.588013f, 1090.280029f, 1390.197388f, 1013.474548f,
        1381.832642f, 993.820435f,  1311.037720f, 1084.619629f, 1374.627930f, 1126.114502f,
        1373.622192f, 1136.027344f, 1371.626831f, 1146.000610f, 1369.532715f, 1129.156250f,
        1356.107544f, 1149.122803f, 1371.145752f, 1150.439819f, 1367.374634f, 1104.456543f,
        1367.730591f, 1080.245850f, 1376.507812f, 1159.425293f, 1372.541260f, 1136.975464f,
        1366.810303f, 1158.257812f, 1364.125610f, 1179.460449f, 1356.686890f, 1197.008423f,
        1346.902100f, 1215.032837f, 1361.206665f, 1185.958984f, 1346.991699f, 1238.327393f,
        1359.546631f, 1188.038940f, 1355.681396f, 1199.970947f, 1359.955933f, 1199.508911f,
        1348.443970f, 1240.572632f, 1358.835938f, 1223.738159f, 1369.890869f, 1212.679810f,
        1365.567139f, 1227.774902f, 1383.945190f, 1203.293091f, 1369.847778f, 1210.347656f,
        1371.375732f, 1171.084473f, 1357.699951f, 1225.654297f, 1352.410522f, 1248.410767f,
        1380.529541f, 1232.259399f, 1440.210571f, 1192.166870f, 1424.790771f, 1172.661499f,
        1407.064209f, 1172.683838f, 1349.794067f, 1221.003540f, 1356.550171f, 1213.753906f,
        1361.887817f, 1221.628906f, 1404.630127f, 1181.215820f, 1343.615479f, 1281.815430f,
        1440.209106f, 1147.719727f, 1440.712646f, 1123.442261f, 1494.605469f, 1076.493164f,
        1385.561890f, 1142.965942f, 1390.679321f, 1167.140259f, 1430.301270f, 1142.692993f,
        1347.920288f, 1210.181152f, 1381.036865f, 1213.251831f, 1376.271484f, 1243.908569f,
        1378.750732f, 1210.927612f, 1362.712524f, 1234.407471f, 60.852230f,   -32.658508f,
        26.079466f,   -7.472332f,   -41.899139f,  1.754938f,    -54.426041f,  -9.625490f,
        -37.182461f,  -26.090736f,  -55.471817f,  7.822602f,    31.945463f,   -4.994428f,
        60.404213f,   -22.054798f,  4.165506f,    -2.335362f,   -17.761189f,  2.246483f,
        -82.525726f,  9.132215f,    -131.404129f, 11.720247f,   -126.458694f, 5.758772f,
        -79.334297f,  7.298450f,    -14.738308f,  -5.185463f,   24.363659f,   -23.277216f,
        -10.568143f,  -24.274023f,  -61.951767f,  -1.852200f,   -91.182808f,  -8.240071f,
        -125.457733f, 6.899367f,    -138.994415f, 7.248713f,    -149.981186f, 11.656230f,
        -84.078423f,  -8.572304f,   -46.167110f,  -31.574038f,  -30.790710f,  -21.430115f,
        35.847763f,   -30.980621f,  -24.308889f,  -21.762217f,  -104.820564f, 1.008041f,
        -111.246895f, 3.251106f,    -107.936058f, -2.324813f,   -118.715973f, -8.398063f,
        -133.614197f, -16.983891f,  44.739422f,   -15.360461f,  7.375826f,    -1.053796f,
        7.922067f,    -9.804401f,   -12.091797f,  -4.803457f,   -44.719486f,  -7.643800f,
        -16.033760f,  -20.860476f,  -0.969445f,   -17.366533f,  -96.146591f,  -19.534830f,
        114.724403f,  -9.737665f,   116.157181f,  -11.603857f,  107.256905f,  -6.590164f,
        122.614105f,  -17.346987f,  7.716633f,    -6.050469f,   90.109169f,   -7.259981f,
        -37.881382f,  20.586533f,   80.236206f,   -9.177361f,   142.288406f,  -29.289808f,
        78.522766f,   5.873971f,    121.080017f,  0.761694f,    167.052704f,  -26.922058f,
        25.947519f,   -20.475613f,  20.901772f,   -20.231441f,  113.567978f,  12.946128f,
        162.256729f,  -20.493515f,  102.578789f,  -62.654083f,  266.041870f,  -64.566193f,
        133.615845f,  1.114768f,    262.948669f,  -62.798225f,  205.368362f,  -86.725563f,
        150.269638f,  -15.862679f,  383.912384f,  38.070873f,   344.770721f,  -164.717148f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    65.629784f,   112.328148f,
        33.882530f,   105.237053f,  79.068398f,   86.963127f,   67.452644f,   95.339432f,
        72.586777f,   163.745117f,  152.412292f,  121.222313f,  139.993088f,  105.856789f,
        100.946899f,  134.613968f,  71.908463f,   128.590317f,  59.106434f,   123.605072f,
        73.225166f,   139.616135f,  74.295715f,   129.280060f,  85.977448f,   135.532944f,
        122.817688f,  140.793640f,  112.459419f,  128.712357f,  90.715454f,   127.087807f,
        78.133850f,   174.123657f,  77.131989f,   155.282394f,  95.398186f,   163.534683f,
        118.005295f,  138.025330f,  133.744431f,  137.304962f,  122.688126f,  168.903290f,
        60.840542f,   168.205856f,  86.866081f,   162.003922f,  125.630951f,  268.588257f,
        104.619720f,  269.805756f,  132.937225f,  222.319061f,  144.415192f,  217.485397f,
        188.078583f,  183.912323f,  202.998596f,  162.066452f,  181.723999f,  208.555283f,
        193.110397f,  172.243118f,  142.981812f,  340.288940f,  65.930908f,   421.835266f,
        127.721825f,  214.415497f,  189.793304f,  220.102249f,  240.390259f,  218.014282f,
        270.229736f,  192.070984f,  235.074875f,  170.951080f,  249.157471f,  212.137054f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        13.083276f,   85.994888f,   41.576309f,   69.905243f,   39.059731f,   79.294769f,
        69.276421f,   65.038506f,   65.006409f,   77.482567f,   101.274231f,  38.227013f,
        83.159584f,   52.238766f,   36.200523f,   60.375965f,   42.043236f,   97.814560f,
        69.737602f,   77.964127f,   61.669861f,   76.887131f,   33.925621f,   71.175621f,
        44.292774f,   79.713066f,   48.022110f,   84.390297f,   64.976242f,   81.657059f,
        69.531624f,   54.371632f,   109.403275f,  124.517319f,  121.717827f,  120.738594f,
        83.026520f,   108.698837f,  92.760353f,   96.480530f,   96.323654f,   96.182365f,
        94.096237f,   108.755539f,  91.308815f,   140.290329f,  81.055779f,   121.746475f,
        169.575150f,  197.830154f,  191.133041f,  185.351349f,  146.390167f,  170.333069f,
        157.149536f,  131.949097f,  181.818634f,  108.021202f,  191.052597f,  100.627266f,
        111.684151f,  166.175156f,  134.646866f,  160.006149f,  289.453430f,  262.431946f,
        265.748291f,  260.456390f,  249.013306f,  234.694031f,  245.479080f,  192.451157f,
        276.572174f,  143.317459f,  322.361267f,  126.808311f,  190.570267f,  202.705734f,
        197.546555f,  204.665390f,  425.463501f,  278.557861f,  412.893982f,  286.662994f,
        396.813568f,  259.371124f,  416.612579f,  215.322495f,  421.833069f,  161.711365f,
        320.965485f,  199.025620f,  183.615067f,  234.210342f,  234.105240f,  220.003067f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    12.100220f,   11.540203f,
        14.633443f,   -18.524879f,  28.550148f,   57.977215f,   8.003514f,    12.273988f,
        7.965772f,    20.504097f,   40.066700f,   20.319775f,   40.021812f,   5.924749f,
        25.430813f,   6.243839f,    14.431930f,   6.228679f,    0.000000f,    0.000000f,
        -15.255136f,  12.868870f,   -22.941614f,  -1.051880f,   -12.244720f,  5.568432f,
        -15.942997f,  -580.065491f, 36.706635f,   12.378496f,   30.223106f,   0.122807f,
        14.743217f,   7.679020f,    9.598755f,    10.398994f,   -0.000000f,   0.000000f,
        -10.058694f,  9.398904f,    -11.537377f,  -1.081510f,   -1.129447f,   32.951630f,
        -14.824292f,  -40.924923f,  37.055611f,   71.580040f,   4.252628f,    -20.541195f,
        -30.765282f,  -33.585533f,  19.946260f,   3.548486f,    39.036797f,   -0.412744f,
        13.236338f,   11.756087f,   31.752516f,   -15.143168f,  16.041376f,   -43.273251f,
        -0.878269f,   -3.012104f,   -287.433319f, 81.014236f,   -128.174896f, 46.204102f,
        31.133493f,   -24.618099f,  20.924240f,   -12.988170f,  0.000000f,    0.000000f,
        0.958493f,    6.884900f,    -5.155409f,   26.946821f,   -18.853495f,  51.752766f,
        -25.649603f,  82.572624f,   -444.247131f, 190.547760f,  -90.980080f,  30.257462f,
        56.516937f,   -42.036346f,  33.998688f,   -28.935366f,  0.000000f,    0.000000f,
        -19.674986f,  28.275551f,   -30.407301f,  53.718029f,   -62.538494f,  103.116264f,
        -92.356606f,  147.826492f,  309.769531f,  -192.344879f, 85.609261f,   -82.395592f,
        -134.612289f, 91.195824f,   -76.812988f,  41.955841f,   0.000000f,    0.000000f,
        51.333790f,   -24.868059f,  100.522774f,  -64.845909f,  209.964539f,  -190.029343f,
        303.445251f,  -288.282501f, 2.324946f,    -0.119229f,   0.000000f,    0.000000f,
        -0.488846f,   3.099444f,    5.390994f,    2.854973f,    2.967661f,    3.883248f,
        2.555698f,    2.917472f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    -7.611045f,   -31.234688f,
        -6.097276f,   -11.340790f,  -5.302298f,   -1.654565f,   -4.861317f,   2.015867f,
        -2.443856f,   1.186125f,    0.000000f,    0.000000f,    0.941711f,    -3.809163f,
        -0.816097f,   -6.785508f,   110.235344f,  -44.765305f,  10.425294f,   -34.472878f,
        10.943131f,   -16.626383f,  6.384377f,    -6.189271f,   4.672674f,    -1.158896f,
        1.748695f,    1.588619f,    0.000000f,    0.000000f,    -0.483220f,   -1.973822f,
        1.157763f,    -4.803585f,   12.532578f,   -16.496214f,  12.043342f,   -28.167963f,
        7.423780f,    -11.843494f,  7.093190f,    -8.359973f,   4.595218f,    -5.752647f,
        -0.257842f,   -0.298486f,   0.000000f,    0.000000f,    -0.716330f,   -1.741413f,
        3.032411f,    -7.021079f,   5.836039f,    -12.946527f,  3.405297f,    -137.346542f,
        5.636199f,    -90.946632f,  5.610713f,    -44.381920f,  2.367157f,    -31.241110f,
        1.457040f,    -6.168107f,   0.000000f,    0.000000f,    -2.401293f,   5.372710f,
        -2.511628f,   7.475649f,    -4.833681f,   15.641183f,   0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    6.388531f,    -1.146192f,
        2.328751f,    1.830685f,    1.650091f,    -0.030665f,   1.714082f,    0.265407f,
        2.463169f,    0.059738f,    5.437133f,    -2.024346f,   4.594463f,    1.088801f,
        6.707160f,    12.135397f,   -3.818922f,   4.775615f,    -4.151098f,   -3.732767f,
        0.529874f,    1.497155f,    -2.965151f,   -0.211134f,   4.580374f,    -4.843907f,
        3.873297f,    -2.526750f,   -4.157706f,   -1.680580f,   -2.887282f,   -4.879714f,
        -12.895775f,  -6.765831f,   0.676508f,    -6.728948f,   20.000000f,   10.000000f,
        -2.239970f,   -8.855842f,   -6.860123f,   -45.975372f,  7.467971f,    -19.821121f,
        6.819904f,    4.921039f,    -45.284710f,  -32.065208f,  -2.339269f,   -13.170496f,
        5.262675f,    12.529057f,   -38.974834f,  -31.700060f,  -15.248524f,  3.082728f,
        3.042202f,    10.915597f,   -54.338116f,  -6.124984f,   -4.985388f,   7.125689f,
        4.012843f,    26.219744f,   -68.368309f,  -20.709093f,  -44.298126f,  16.774981f,
        17.002096f,   -10.430028f,  -21.008230f,  -36.393936f,  21.144716f,   -29.108435f,
        0.000000f,    0.000000f,    40.152805f,   -50.592262f,  41.475449f,   -52.602455f,
        54.476940f,   -57.034199f,  79.238823f,   -67.591415f,  92.624382f,   -74.842529f,
        144.037827f,  -93.106552f,  211.202026f,  -114.501160f, 234.136353f,  -110.762726f,
        0.000000f,    0.000000f,    -71.426445f,  50.802010f,   -50.153393f,  44.400063f,
        -43.912956f,  46.676159f,   -54.378624f,  46.936874f,   -48.358582f,  45.081825f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    9.908704f,    0.017765f,
        80.983780f,   -47.863312f,  166.895691f,  -96.348022f,  263.990265f,  -149.041290f,
        386.734161f,  -204.093536f, -10.099331f,  1.700433f,    -12.968026f,  -46.354080f,
        -15.788649f,  -55.861496f,  -44.956509f,  -17.117868f,  -34.329166f,  9.894779f,
        -15.888137f,  14.791816f,   -8.206136f,   5.147186f,    -23.475843f,  33.319195f};

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
