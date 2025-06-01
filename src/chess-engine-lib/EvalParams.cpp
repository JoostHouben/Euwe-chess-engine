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
        0.209377f,    1.076343f,    0.332062f,    0.189779f,    0.386821f,    1.000000f,
        -0.012864f,   0.000000f,    -0.039580f,   0.000059f,    70.351227f,   -15.062983f,
        164.067200f,  349.509979f,  363.228210f,  340.771973f,  403.015747f,  508.008118f,
        672.185059f,  1265.435181f, 1138.841919f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -44.755383f,  163.744797f,  -47.460163f,
        160.884598f,  -39.778820f,  166.275787f,  -57.702087f,  169.875656f,  -39.882900f,
        193.180206f,  4.626213f,    167.868286f,  8.113437f,    161.085587f,  -29.831282f,
        151.335800f,  -34.891537f,  158.350952f,  -35.945930f,  156.881226f,  -27.506958f,
        161.781235f,  -34.509220f,  172.875183f,  -29.154739f,  170.963257f,  -6.032743f,
        163.614212f,  -4.575036f,   154.699707f,  -15.339327f,  152.304871f,  -20.647623f,
        169.901627f,  -11.508142f,  168.405685f,  -6.900945f,   169.575287f,  -0.507111f,
        165.504059f,  -0.291692f,   168.225952f,  6.485542f,    168.475800f,  -2.579009f,
        168.790894f,  -16.014652f,  164.143890f,  3.973763f,    182.788513f,  6.116879f,
        177.773560f,  -1.551011f,   173.657074f,  14.615534f,   163.766159f,  28.489321f,
        170.701523f,  27.732006f,   158.212280f,  13.714347f,   171.812149f,  9.594480f,
        169.268890f,  32.982666f,   195.464630f,  -1.119711f,   203.204971f,  29.042866f,
        202.538757f,  -0.990130f,   186.948685f,  26.422087f,   176.616318f,  80.461395f,
        195.581985f,  81.618217f,   196.395828f,  49.086761f,   192.057465f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    287.405029f,  347.484467f,  308.809174f,  341.863739f,  308.552185f,
        363.858032f,  318.144745f,  358.523682f,  320.512390f,  344.356659f,  327.671448f,
        333.503387f,  304.688599f,  343.087555f,  259.828705f,  301.096558f,  312.566589f,
        344.649567f,  325.954132f,  337.609985f,  327.415344f,  354.407471f,  341.307068f,
        359.490692f,  336.267670f,  356.482880f,  348.231689f,  340.462769f,  333.634583f,
        332.678101f,  323.798950f,  323.101532f,  321.255188f,  351.577118f,  340.403992f,
        356.535858f,  345.335175f,  361.682922f,  366.005920f,  369.969147f,  371.383484f,
        363.419525f,  351.258484f,  350.025330f,  350.993958f,  339.386475f,  329.770264f,
        352.961945f,  346.221497f,  355.377075f,  386.762482f,  359.443604f,  382.430359f,
        373.713715f,  369.434082f,  375.087280f,  377.106506f,  387.292511f,  371.061798f,
        380.286346f,  366.563049f,  362.997528f,  340.853333f,  352.196716f,  349.573761f,
        374.958771f,  371.496613f,  375.034454f,  401.879944f,  380.361542f,  407.010712f,
        380.025787f,  381.075134f,  378.904297f,  395.148071f,  372.484131f,  376.954468f,
        376.593842f,  378.382843f,  361.691162f,  338.839264f,  370.096405f,  377.435547f,
        376.293884f,  427.886475f,  362.167725f,  423.419189f,  365.262604f,  483.308411f,
        355.267029f,  409.864563f,  370.838531f,  404.532501f,  356.440125f,  359.106873f,
        345.589661f,  360.060059f,  368.037018f,  342.347595f,  370.871887f,  391.476318f,
        359.302338f,  422.461090f,  381.170044f,  395.745453f,  362.578461f,  394.200165f,
        361.576904f,  378.390411f,  361.558502f,  330.405396f,  369.559967f,  128.244690f,
        415.518646f,  268.588593f,  400.575043f,  350.209778f,  391.876251f,  331.521698f,
        407.049225f,  337.178864f,  388.833618f,  421.270752f,  382.132568f,  509.876984f,
        293.889984f,  293.034180f,  202.207550f,  324.598785f,  352.078278f,  334.331451f,
        387.074158f,  317.136169f,  391.029480f,  317.632416f,  390.476440f,  319.295959f,
        394.656799f,  316.086182f,  393.892548f,  293.814606f,  397.112030f,  335.897705f,
        339.236450f,  343.672333f,  382.706696f,  331.395630f,  383.285797f,  347.538239f,
        394.266113f,  326.576752f,  402.237457f,  338.846924f,  389.077515f,  343.382446f,
        390.114136f,  356.046722f,  381.653778f,  341.500702f,  359.677917f,  331.412689f,
        389.020691f,  354.763763f,  403.440735f,  345.183746f,  411.984375f,  348.700745f,
        406.154938f,  347.654968f,  413.923981f,  350.850708f,  400.747253f,  350.759430f,
        391.318390f,  350.470093f,  384.873352f,  332.842468f,  412.719666f,  337.862000f,
        411.328033f,  349.226562f,  407.138184f,  358.538513f,  419.408478f,  365.296509f,
        409.044739f,  348.505280f,  409.902130f,  366.387299f,  400.914551f,  343.525238f,
        396.711304f,  325.909851f,  409.281158f,  344.527466f,  413.433746f,  359.524963f,
        424.421417f,  383.493439f,  410.758331f,  372.014771f,  410.595123f,  365.911346f,
        425.968964f,  345.853271f,  412.152191f,  338.175018f,  408.444305f,  344.638641f,
        406.132599f,  342.629852f,  427.888092f,  359.024841f,  415.823730f,  374.907135f,
        412.028473f,  364.542450f,  416.372192f,  413.141205f,  413.548431f,  422.705261f,
        408.406647f,  374.054626f,  397.836792f,  335.897766f,  397.590698f,  352.039307f,
        423.697144f,  366.248505f,  413.048767f,  343.924103f,  424.462585f,  384.732056f,
        401.203094f,  381.373352f,  406.009460f,  354.579010f,  407.470398f,  326.240692f,
        381.813721f,  372.136383f,  396.866455f,  324.404449f,  435.712952f,  303.849182f,
        419.875427f,  338.486694f,  421.574982f,  347.398590f,  417.935425f,  351.239105f,
        399.780487f,  388.831116f,  360.678131f,  402.104340f,  387.354858f,  516.372681f,
        650.834290f,  510.317566f,  655.672363f,  509.699829f,  657.085449f,  507.382843f,
        649.188232f,  509.726440f,  647.603577f,  501.300629f,  656.216614f,  490.661285f,
        658.018433f,  487.287720f,  643.265442f,  499.155121f,  660.670593f,  507.546356f,
        654.891541f,  502.406952f,  665.466553f,  493.164032f,  664.731018f,  495.006256f,
        669.059937f,  509.559723f,  651.324158f,  475.233704f,  666.313171f,  483.069855f,
        664.431213f,  498.761810f,  663.350403f,  494.261230f,  671.979370f,  484.867188f,
        678.783875f,  496.981750f,  669.651489f,  486.302582f,  674.259644f,  484.292023f,
        675.156006f,  488.170685f,  667.649719f,  477.877350f,  674.507507f,  506.595367f,
        683.764343f,  488.167786f,  690.992249f,  490.656799f,  690.560303f,  475.594666f,
        690.308899f,  485.851105f,  683.675171f,  486.526428f,  694.578125f,  482.569824f,
        700.374756f,  472.562164f,  684.885986f,  506.473297f,  698.620850f,  511.596069f,
        693.135925f,  529.979126f,  685.117798f,  522.002014f,  686.340332f,  527.509460f,
        680.369019f,  517.678406f,  693.359436f,  514.647766f,  691.887878f,  490.098602f,
        697.975769f,  523.997925f,  694.262024f,  538.835083f,  692.804932f,  556.478149f,
        685.284668f,  553.313599f,  677.053406f,  571.749756f,  678.101257f,  565.846497f,
        685.511841f,  571.458923f,  681.231445f,  551.791687f,  699.510193f,  531.808594f,
        704.783936f,  522.731995f,  711.642700f,  567.685181f,  693.408813f,  572.267334f,
        685.596008f,  555.074097f,  701.221924f,  595.838196f,  677.024170f,  594.360474f,
        679.532227f,  534.083618f,  704.171692f,  540.892761f,  691.249207f,  537.835388f,
        696.605225f,  534.518982f,  696.345764f,  526.329285f,  694.019226f,  525.118103f,
        704.272034f,  514.145996f,  706.142822f,  504.487915f,  715.912109f,  591.822327f,
        684.157959f,  1281.201660f, 1054.432373f, 1274.024536f, 1051.622314f, 1264.312012f,
        1085.922852f, 1261.320190f, 1090.852051f, 1264.186768f, 1070.309204f, 1233.910034f,
        1091.935181f, 1240.982056f, 1014.259949f, 1249.853638f, 1041.730835f, 1275.468994f,
        1084.062622f, 1285.034180f, 1057.853760f, 1278.538208f, 1102.229370f, 1270.458252f,
        1096.908081f, 1268.142944f, 1106.107666f, 1266.048828f, 1068.721191f, 1292.870239f,
        993.416565f,  1245.114136f, 1088.975952f, 1268.726440f, 1091.616943f, 1271.437256f,
        1133.036011f, 1259.720825f, 1137.386353f, 1263.639771f, 1129.265381f, 1265.895996f,
        1121.853638f, 1258.884033f, 1152.489624f, 1276.101074f, 1106.004883f, 1263.236084f,
        1147.359375f, 1263.064819f, 1156.143188f, 1257.501953f, 1159.098633f, 1257.648560f,
        1158.604248f, 1258.136963f, 1178.051147f, 1255.995972f, 1178.374878f, 1248.020386f,
        1194.295654f, 1265.058716f, 1159.388062f, 1246.817139f, 1217.045288f, 1264.058594f,
        1161.894531f, 1248.980835f, 1191.395752f, 1248.676636f, 1208.561035f, 1245.529785f,
        1209.993164f, 1247.646606f, 1216.086426f, 1256.959229f, 1206.457642f, 1264.201294f,
        1183.789551f, 1270.611938f, 1188.979736f, 1281.492554f, 1172.390869f, 1278.039795f,
        1146.945435f, 1249.706665f, 1225.512207f, 1229.921631f, 1241.914429f, 1250.705444f,
        1250.766846f, 1279.451538f, 1217.586182f, 1313.499878f, 1154.323364f, 1268.039673f,
        1197.619751f, 1267.742920f, 1142.595581f, 1239.210938f, 1189.255127f, 1257.988403f,
        1193.436157f, 1231.716553f, 1251.445679f, 1250.979736f, 1233.718018f, 1263.511475f,
        1206.071533f, 1263.220581f, 1202.104248f, 1310.894287f, 1168.282227f, 1273.477661f,
        1150.743164f, 1330.628052f, 1089.421387f, 1263.473267f, 1197.137573f, 1189.950073f,
        1278.183228f, 1271.342163f, 1207.177612f, 1306.697876f, 1167.864624f, 1278.784790f,
        1181.897461f, 1251.729370f, 1214.518555f, 66.027466f,   -33.871723f,  19.904385f,
        -3.016543f,   -40.810169f,  7.651756f,    -56.857658f,  -3.785627f,   -36.005390f,
        -20.256330f,  -53.063717f,  8.382058f,    22.725737f,   6.294006f,    52.199818f,
        -11.103857f,  23.841595f,   -16.236650f,  -20.945892f,  -5.423137f,   -66.197716f,
        2.499984f,    -120.252510f, 12.723612f,   -107.431618f, 6.183033f,    -69.044601f,
        4.785284f,    0.201975f,    -11.038551f,  23.797085f,   -18.018381f,  -34.712887f,
        -17.270210f,  -53.621990f,  0.917702f,    -117.387329f, 8.071685f,    -101.114655f,
        4.760748f,    -116.444946f, 7.721368f,    -107.958702f, 7.390476f,    -70.522865f,
        -6.806279f,   -44.004787f,  -31.793915f,  1.617144f,    -19.017134f,  -1.060118f,
        -22.297798f,  -70.575768f,  -4.123937f,   -59.619762f,  -6.945396f,   -73.403015f,
        -5.017753f,   -93.975861f,  4.569441f,    -50.163338f,  -18.161272f,  -86.057343f,
        -33.772758f,  61.412178f,   -22.263453f,  80.754707f,   -24.751570f,  17.934023f,
        -8.976460f,   33.797390f,   -20.634096f,  -19.642448f,  -4.676329f,   61.112095f,
        -25.416624f,  24.175070f,   -23.192873f,  -120.305710f, -9.879018f,   80.316940f,
        -9.708657f,   94.560440f,   -11.143610f,  85.359222f,   -17.451036f,  73.235260f,
        -10.730902f,  197.094940f,  -55.846443f,  182.869812f,  -21.286385f,  54.241421f,
        9.098446f,    41.223358f,   -3.222961f,   160.585800f,  -38.045826f,  112.188972f,
        -8.616776f,   144.843887f,  -28.428225f,  104.094734f,  -33.165005f,  121.487846f,
        -49.503918f,  299.788116f,  -134.807724f, 84.326111f,   26.386604f,   66.500366f,
        6.526861f,    154.035904f,  -39.014622f,  157.510086f,  -41.720955f,  90.282257f,
        -10.707127f,  -40.734470f,  -16.312313f,  109.603340f,  -89.122215f,  180.107315f,
        -62.490162f,  97.140564f,   -15.009281f,  707.553345f,  -231.182632f, 0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -10.496933f,  111.773567f,  -6.488624f,
        119.978653f,  24.533440f,   93.283607f,   24.393431f,   185.029968f,  41.401352f,
        133.834320f,  84.509605f,   98.522079f,   77.058739f,   99.155540f,   17.221233f,
        120.944580f,  12.427701f,   131.339005f,  -14.872430f,  129.709259f,  7.351349f,
        135.174301f,  22.083887f,   141.698898f,  12.067674f,   147.845123f,  51.458359f,
        132.308472f,  56.728371f,   108.460960f,  1.307813f,    120.125549f,  36.414101f,
        178.584320f,  16.216995f,   170.385376f,  45.654922f,   166.998199f,  55.128197f,
        141.762375f,  80.460396f,   147.532394f,  59.283184f,   163.071854f,  63.340546f,
        135.888351f,  53.210648f,   128.295074f,  60.014080f,   272.815674f,  66.027969f,
        266.412445f,  80.108246f,   225.301529f,  94.325325f,   223.597321f,  113.352333f,
        200.067902f,  137.099014f,  150.722900f,  78.909546f,   186.070023f,  145.871246f,
        174.962265f,  61.686829f,   366.858917f,  52.337891f,   399.785400f,  74.529518f,
        288.273285f,  118.959625f,  308.767181f,  188.268600f,  243.796875f,  124.163528f,
        171.485138f,  172.624161f,  186.551697f,  173.046555f,  224.231491f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    -43.673161f,
        108.177399f,  -33.498661f,  95.030327f,   -39.901379f,  87.625389f,   -2.177596f,
        85.834106f,   -33.516575f,  106.004288f,  26.358311f,   52.710640f,   18.134071f,
        53.765465f,   -51.937840f,  77.439575f,   -12.770425f,  103.298965f,  34.747868f,
        71.300842f,   -21.389496f,  106.195213f,  -26.773647f,  80.347908f,   -23.418571f,
        86.508484f,   -11.403582f,  76.502258f,   23.301567f,   77.651215f,   -11.728967f,
        74.316498f,   53.065796f,   136.992020f,  84.270706f,   113.493813f,  23.265430f,
        111.821960f,  43.451012f,   98.718811f,   43.020004f,   97.444336f,   60.835728f,
        111.239883f,  21.786158f,   143.897751f,  29.816526f,   121.143402f,  147.203476f,
        201.414398f,  142.490189f,  191.615784f,  114.773361f,  174.594559f,  111.430084f,
        136.011475f,  137.021683f,  123.580391f,  123.313087f,  134.025787f,  78.290367f,
        174.934967f,  87.428474f,   165.570251f,  242.318054f,  278.444824f,  252.195786f,
        279.438812f,  244.430603f,  224.974396f,  227.030258f,  202.939041f,  259.029083f,
        172.878677f,  268.927124f,  162.268921f,  230.253891f,  203.341843f,  156.832611f,
        224.103973f,  480.303406f,  291.746735f,  466.649261f,  292.693939f,  390.760071f,
        314.446838f,  419.067963f,  218.616791f,  377.944458f,  222.628113f,  347.195801f,
        224.303574f,  190.550446f,  268.054291f,  150.755737f,  295.665710f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    13.362108f,   7.044587f,    30.125023f,
        46.392544f,   24.099360f,   7.973538f,    5.709999f,    19.137394f,   -0.355573f,
        -0.347134f,   -0.192405f,   -0.186310f,   -0.123593f,   0.086926f,    28.252821f,
        7.808675f,    33.234497f,   -1.375386f,   21.481133f,   -0.607461f,   11.353603f,
        -1.385804f,   0.000000f,    0.000000f,    -11.481277f,  -1.600598f,   -23.162981f,
        -8.346756f,   -37.203114f,  57.123573f,   -110.220444f, 180.420822f,  35.761368f,
        -5.613240f,   34.193787f,   -16.386274f,  14.918061f,   -2.652721f,   8.391068f,
        1.219829f,    0.000000f,    0.000000f,    -11.119617f,  2.775580f,    -18.557653f,
        -5.788019f,   -37.212578f,  60.864868f,   -81.860886f,  91.001076f,   27.409847f,
        68.495651f,   1.630077f,    -10.623604f,  -48.557034f,  -9.425673f,   18.988354f,
        2.986380f,    33.698811f,   3.718263f,    19.015522f,   16.467016f,   28.445189f,
        -26.078613f,  -1.823624f,   -60.686497f,  4.586039f,    20.562784f,   -106.334251f,
        36.375050f,   -107.962181f, 23.925831f,   40.329620f,   -38.771923f,  11.702320f,
        -13.280622f,  0.000000f,    0.000000f,    7.071741f,    5.960080f,    4.339719f,
        32.205204f,   -0.475309f,   57.586266f,   -14.122539f,  109.440842f,  -285.909302f,
        120.072960f,  -143.842972f, 43.173611f,   20.020008f,   -29.242468f,  27.666615f,
        -31.588081f,  0.000000f,    0.000000f,    -11.852645f,  29.574558f,   -14.428392f,
        51.425362f,   -48.986835f,  99.804062f,   -117.801674f, 156.200226f,  44.521881f,
        -45.861961f,  116.633766f,  -139.843903f, -73.754776f,  11.690370f,   -75.019478f,
        20.442049f,   0.000000f,    0.000000f,    59.692287f,   -24.624649f,  119.823860f,
        -71.663322f,  250.822952f,  -178.055359f, 455.556610f,  -374.904816f, 0.354875f,
        2.695845f,    4.778477f,    2.820532f,    3.369511f,    2.829150f,    2.399941f,
        0.003350f,    -3.671248f,   -44.364822f,  -5.149975f,   -12.357667f,  -2.862991f,
        -5.037512f,   -2.817238f,   -1.832860f,   0.000000f,    0.000000f,    3.248981f,
        -2.344545f,   6.697568f,    -2.410944f,   -5.090278f,   -3.340035f,   -53.693062f,
        -3.583233f,   7.292088f,    -36.387878f,  5.510221f,    -9.407866f,   5.344571f,
        -6.159760f,   2.425582f,    -1.019376f,   0.000000f,    0.000000f,    -7.590824f,
        0.879375f,    -11.745734f,  0.493505f,    -6.905567f,   -2.721659f,   9.123135f,
        -10.497899f,  10.322508f,   -33.604774f,  7.584933f,    -10.374605f,  5.772132f,
        -5.563854f,   4.698083f,    -1.182033f,   0.000000f,    0.000000f,    -0.384364f,
        1.872548f,    -2.862393f,   1.711896f,    0.059237f,    -1.084061f,   2.515941f,
        -7.635860f,   -0.583491f,   -114.090637f, 4.634130f,    -64.887802f,  2.655282f,
        -31.228542f,  2.175938f,    -22.085794f,  0.000000f,    0.000000f,    -2.196850f,
        5.120965f,    -5.322970f,   16.287981f,   -6.681225f,   21.926476f,   -5.706376f,
        27.419682f,   6.868357f,    -1.549674f,   1.399841f,    1.526871f,    1.789340f,
        -0.685413f,   2.889719f,    0.198505f,    2.408646f,    -2.662261f,   6.145825f,
        -1.565318f,   4.240861f,    1.167677f,    7.228270f,    11.809772f,   -1.847479f,
        7.216302f,    -1.942375f,   -4.700136f,   0.350271f,    1.230396f,    -2.208528f,
        -0.913281f,   4.330601f,    -5.178733f,   4.385299f,    -7.459709f,   -0.126166f,
        -4.369447f,   0.265504f,    -6.607521f,   -6.505609f,   -6.510161f,   0.958147f,
        -9.023257f,   20.000000f,   10.000000f,   -0.596424f,   -7.527312f,   -5.843102f,
        -42.002586f,  8.859897f,    -18.810617f,  7.704319f,    3.398234f,    -38.681671f,
        -33.798996f,  -2.265290f,   -10.037651f,  4.746714f,    8.874035f,    -40.489952f,
        -34.648655f,  -12.943549f,  -6.177051f,   5.962850f,    7.401908f,    -52.239746f,
        -5.917722f,   -7.017942f,   -0.826561f,   5.039282f,    31.132921f,   -62.235832f,
        -43.645752f,  -48.073395f,  15.524276f,   0.000000f,    0.000000f,    64.423187f,
        -56.297115f,  58.293194f,   -51.933437f,  57.409676f,   -50.827896f,  64.305054f,
        -54.795116f,  78.020172f,   -62.351543f,  112.467964f,  -73.888824f,  186.102219f,
        -86.972855f,  228.084869f,  -82.044662f,  -94.410446f,  50.230736f,   -68.552597f,
        42.193134f,   -73.969070f,  45.775093f,   -81.256874f,  44.452751f,   -82.747108f,
        47.982761f,   0.000000f,    0.000000f,    16.321049f,   5.389641f,    120.984016f,
        -43.957436f,  234.964722f,  -91.409386f,  363.482056f,  -143.232407f, 526.215271f,
        -202.748978f, -9.153873f,   -0.367549f,   -20.696037f,  -49.957203f,  -32.108871f,
        -41.501015f,  -46.650852f,  -17.724075f,  -31.575594f,  -21.317953f,  -8.162850f,
        11.029043f,   -8.022294f,   5.871128f,    -19.295519f,  30.297707f,   0.000000f,
        0.000000f,    -1.897416f,   -1.463151f,   -8.903700f,   -10.160986f,  -24.693535f,
        -6.381551f,   -30.060068f,  -14.060596f,  -65.036758f,  18.561697f,   -111.674774f,
        64.264473f,   0.000000f,    0.000000f,    -16.370167f,  -9.233534f,   -55.679192f,
        -6.961246f,   -91.957687f,  0.757729f,    -155.322922f, 71.993744f,   -332.653351f,
        251.303314f,  -319.710083f, 195.284393f};

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
