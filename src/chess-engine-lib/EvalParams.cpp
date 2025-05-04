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
        1.076343f,    0.306639f,    0.249490f,    0.438312f,    0.978836f,    1.000000f,
        -0.026879f,   -0.006773f,   -0.034015f,   0.003464f,    136.400986f,  23.940813f,
        179.383636f,  437.160278f,  425.003754f,  383.565063f,  450.773834f,  607.450439f,
        793.560730f,  1429.719971f, 1347.516235f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -18.448158f,  176.059753f,  -20.015133f,
        178.614014f,  -8.826793f,   183.407654f,  -20.726009f,  179.100204f,  -4.957111f,
        206.654282f,  51.942383f,   183.390717f,  47.970490f,   170.780518f,  9.811850f,
        162.682312f,  -8.538301f,   175.026749f,  -3.549155f,   173.877655f,  6.161968f,
        179.622528f,  4.814027f,    188.403229f,  12.630987f,   189.321808f,  34.346687f,
        178.427338f,  37.393047f,   171.424591f,  24.577765f,   166.417038f,  11.348741f,
        186.402649f,  20.273985f,   188.045883f,  34.337185f,   180.653229f,  40.828560f,
        183.449432f,  42.847752f,   184.413055f,  57.365170f,   180.288971f,  46.143471f,
        185.716049f,  24.583990f,   177.266434f,  37.264587f,   201.096390f,  42.019489f,
        205.722672f,  52.918514f,   198.485519f,  69.022675f,   186.588470f,  87.464378f,
        188.414719f,  80.899033f,   178.599380f,  51.617657f,   200.646088f,  55.875774f,
        182.650162f,  76.159782f,   219.700012f,  39.827473f,   230.753738f,  85.758995f,
        213.449875f,  113.000298f,  185.660934f,  149.542862f,  202.341980f,  176.063324f,
        198.166046f,  140.671371f,  188.648422f,  130.280121f,  191.863434f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    375.268799f,  403.178925f,  379.434479f,  390.183044f,  368.032440f,
        412.967468f,  388.028076f,  428.234222f,  384.204681f,  413.272339f,  401.272003f,
        393.238739f,  374.503632f,  397.272705f,  327.221375f,  397.189514f,  387.343231f,
        400.879517f,  374.432465f,  430.865784f,  405.077759f,  427.950470f,  424.246521f,
        417.809906f,  417.028290f,  413.899597f,  424.277344f,  410.573730f,  398.488647f,
        401.507050f,  399.845612f,  405.616638f,  396.432098f,  403.035583f,  427.677643f,
        419.409088f,  433.477631f,  423.457520f,  448.473358f,  442.929657f,  458.339539f,
        434.860657f,  438.960999f,  407.463013f,  434.328979f,  398.974823f,  407.540192f,
        383.590881f,  432.081635f,  410.548157f,  491.320892f,  409.229248f,  480.807312f,
        436.268738f,  465.745026f,  445.880615f,  474.396210f,  453.525848f,  468.854156f,
        439.367706f,  466.684784f,  429.520416f,  421.024078f,  420.677948f,  451.662323f,
        417.567780f,  472.110901f,  428.869476f,  497.063690f,  432.242462f,  530.871582f,
        443.010406f,  490.460632f,  449.857849f,  510.163757f,  444.237732f,  481.861816f,
        443.607422f,  484.773743f,  416.024872f,  433.132538f,  417.878906f,  496.299805f,
        416.196808f,  524.205627f,  424.827515f,  546.813416f,  426.537994f,  591.077759f,
        422.521545f,  569.421082f,  429.891174f,  500.273407f,  414.371277f,  455.590515f,
        415.326050f,  472.214081f,  405.165497f,  421.971649f,  430.103760f,  529.948975f,
        411.577362f,  526.068481f,  443.268250f,  505.173645f,  444.120026f,  559.101440f,
        398.932343f,  477.929352f,  423.693268f,  413.666016f,  405.057251f,  276.799866f,
        441.081390f,  454.154388f,  429.242523f,  352.415253f,  445.814392f,  446.286011f,
        440.690491f,  468.637085f,  432.761200f,  392.348328f,  415.604126f,  498.394379f,
        344.433624f,  211.624603f,  363.614532f,  338.741943f,  430.918701f,  387.016663f,
        424.288574f,  353.890869f,  442.958679f,  362.107788f,  440.291931f,  357.910370f,
        443.732361f,  347.640015f,  451.226501f,  353.160461f,  437.961670f,  331.350159f,
        421.643280f,  395.024139f,  425.087128f,  375.324707f,  424.830780f,  400.779907f,
        430.479645f,  368.086060f,  452.335358f,  380.908081f,  448.002289f,  394.556793f,
        438.785461f,  403.306000f,  425.092865f,  363.693298f,  392.984406f,  365.422821f,
        439.653717f,  395.768280f,  445.145996f,  388.960968f,  460.030334f,  392.854248f,
        460.932251f,  387.487579f,  461.802277f,  398.269318f,  442.822876f,  392.566101f,
        447.095001f,  388.076172f,  447.156586f,  367.814392f,  459.766449f,  399.061676f,
        441.718536f,  392.215637f,  461.539764f,  401.434479f,  467.990021f,  418.353729f,
        455.836639f,  384.938812f,  463.453552f,  386.823425f,  460.067230f,  382.705444f,
        453.093903f,  370.222931f,  450.847504f,  390.411926f,  464.563080f,  410.454651f,
        457.624268f,  449.870483f,  455.644714f,  427.238556f,  457.807587f,  419.982483f,
        461.023560f,  392.682892f,  466.314362f,  396.972351f,  439.895874f,  403.722626f,
        453.926025f,  416.373810f,  461.773560f,  415.612030f,  468.083252f,  436.592560f,
        454.432587f,  430.791077f,  451.013092f,  463.202942f,  474.022614f,  465.229980f,
        458.378082f,  430.706848f,  444.848480f,  398.983490f,  457.943756f,  401.915039f,
        469.086334f,  407.960205f,  460.035461f,  401.410156f,  472.396545f,  420.746857f,
        459.820496f,  427.487518f,  452.748322f,  382.054260f,  461.680481f,  384.875000f,
        421.742340f,  458.917511f,  437.507019f,  386.465820f,  468.301483f,  397.410553f,
        456.911987f,  421.489532f,  452.643707f,  402.212860f,  460.949341f,  435.375397f,
        429.914337f,  429.254120f,  445.970032f,  406.220490f,  428.615021f,  617.297852f,
        778.696411f,  616.634338f,  771.627563f,  610.806519f,  782.098999f,  607.933411f,
        778.512024f,  606.067810f,  774.093506f,  596.563660f,  781.214905f,  576.002502f,
        783.572021f,  565.928223f,  777.869690f,  594.740845f,  787.400330f,  603.259338f,
        781.593994f,  605.661438f,  789.132019f,  601.554077f,  785.603210f,  600.068359f,
        789.328674f,  604.361389f,  771.522339f,  580.177490f,  789.338928f,  528.292236f,
        806.029663f,  602.779907f,  789.042419f,  615.585327f,  777.038513f,  596.209106f,
        791.988403f,  586.797424f,  798.486328f,  602.682373f,  785.726440f,  579.679138f,
        795.243408f,  607.184448f,  779.335999f,  560.596069f,  796.654724f,  600.439514f,
        810.310059f,  608.944641f,  801.749451f,  590.187927f,  809.999023f,  606.941223f,
        798.763916f,  599.813904f,  800.996582f,  578.020691f,  818.162476f,  594.569763f,
        816.528076f,  573.776917f,  809.287720f,  619.753967f,  812.992310f,  630.711609f,
        803.697449f,  646.957031f,  800.954529f,  642.918945f,  798.448364f,  619.787231f,
        807.661560f,  620.678711f,  803.489014f,  645.069519f,  802.063660f,  628.945679f,
        808.903015f,  646.738586f,  817.992188f,  658.315308f,  805.413147f,  652.605652f,
        806.884888f,  666.871216f,  796.732483f,  715.793152f,  786.411072f,  696.850037f,
        794.830627f,  707.675842f,  794.977295f,  685.260559f,  805.755371f,  658.997559f,
        819.885986f,  633.261902f,  831.063599f,  669.981689f,  819.525024f,  685.451416f,
        810.543457f,  679.240051f,  821.089233f,  701.963562f,  795.308655f,  705.879211f,
        802.116333f,  721.849670f,  800.785950f,  658.790894f,  806.727295f,  690.149719f,
        798.531372f,  681.527283f,  808.937073f,  658.881653f,  814.900269f,  638.961060f,
        831.696533f,  657.233704f,  825.677734f,  679.221252f,  822.965271f,  669.185242f,
        822.621338f,  1445.809814f, 1302.626709f, 1427.582764f, 1305.200439f, 1434.573975f,
        1255.638672f, 1421.060181f, 1291.972656f, 1424.082153f, 1266.592651f, 1393.797485f,
        1266.210693f, 1334.428101f, 1230.192749f, 1434.345825f, 1185.047485f, 1441.362671f,
        1296.750366f, 1452.632324f, 1264.603638f, 1444.670654f, 1289.133423f, 1437.436890f,
        1286.100220f, 1438.717529f, 1291.416748f, 1457.332886f, 1195.873535f, 1420.494751f,
        1173.364380f, 1345.645020f, 1377.577881f, 1426.424194f, 1360.036987f, 1427.656738f,
        1356.723022f, 1428.003662f, 1345.822266f, 1425.339722f, 1331.412476f, 1427.071899f,
        1340.616821f, 1426.784180f, 1355.736694f, 1432.884399f, 1320.604004f, 1398.703003f,
        1404.267090f, 1418.966553f, 1400.012085f, 1441.973389f, 1319.630127f, 1419.269165f,
        1365.268555f, 1414.588989f, 1401.770630f, 1420.745605f, 1392.995605f, 1413.305542f,
        1407.174316f, 1417.904785f, 1392.503662f, 1409.385010f, 1418.822388f, 1424.687744f,
        1385.446167f, 1415.375000f, 1397.726807f, 1420.935669f, 1413.627197f, 1401.393433f,
        1472.721924f, 1427.330078f, 1430.732178f, 1428.370361f, 1442.576660f, 1438.412231f,
        1412.035645f, 1438.587280f, 1434.412109f, 1417.686768f, 1423.048828f, 1416.037720f,
        1424.425781f, 1418.432129f, 1439.213623f, 1409.071777f, 1473.848511f, 1446.767456f,
        1427.188599f, 1499.521729f, 1408.918213f, 1490.650391f, 1380.230347f, 1474.390991f,
        1356.083984f, 1418.119995f, 1413.953735f, 1383.173462f, 1455.098022f, 1431.074829f,
        1410.860596f, 1413.381714f, 1477.075317f, 1396.071289f, 1490.402710f, 1479.778198f,
        1420.404053f, 1568.257568f, 1262.127563f, 1554.026978f, 1279.850220f, 1411.036499f,
        1430.480225f, 1465.079712f, 1396.916260f, 1433.518311f, 1409.166992f, 1404.201050f,
        1455.996704f, 1440.401001f, 1400.375488f, 1473.729126f, 1402.385742f, 1563.977661f,
        1331.684326f, 1450.690186f, 1399.861084f, 60.076653f,   -20.501505f,  34.100216f,
        -16.723347f,  -45.491955f,  5.155171f,    -41.584949f,  -15.370773f,  -31.581196f,
        -26.819674f,  -52.760983f,  10.697564f,   32.681065f,   -0.075379f,   71.309792f,
        -24.313238f,  15.484492f,   -26.177359f,  -12.019530f,  -8.233316f,   -76.365326f,
        -2.277034f,   -159.260330f, 16.965158f,   -143.246597f, 11.714845f,   -91.902374f,
        5.229101f,    -22.490171f,  -5.056995f,   26.286167f,   -27.400478f,  -42.186844f,
        -13.129237f,  -72.353577f,  -4.194317f,   -121.557678f, 0.309134f,    -144.884384f,
        10.127767f,   -152.830505f, 7.389328f,    -174.365005f, 13.351604f,   -98.264160f,
        -9.220447f,   -66.287720f,  -29.312250f,  -63.592003f,  -1.684005f,   -55.522358f,
        -9.211045f,   -80.557259f,  -3.665046f,   -81.821381f,  -6.978088f,   -119.480598f,
        -0.479784f,   -140.401611f, 6.170927f,    -113.581612f, -18.325449f,  -117.721237f,
        -25.076532f,  41.684013f,   -26.404190f,  40.399605f,   -13.470729f,  -23.932493f,
        -0.270221f,   -15.277694f,  -11.060257f,  -50.001614f,  -4.071422f,   79.014526f,
        -41.135376f,  12.245856f,   -15.840873f,  -114.297974f, -15.768616f,  22.805433f,
        6.534937f,    65.727142f,   2.239435f,    92.332291f,   -6.215933f,   20.768435f,
        3.259599f,    76.462189f,   -23.388868f,  181.259613f,  -22.464083f,  165.911835f,
        -13.393349f,  187.023834f,  -45.051384f,  122.831261f,  -49.710220f,  101.833900f,
        5.427445f,    127.257332f,  8.528005f,    134.186920f,  -19.309458f,  140.534927f,
        -48.274887f,  100.009743f,  -27.040831f,  90.584175f,   19.122641f,   136.914673f,
        -15.766434f,  267.023193f,  -90.449356f,  132.824478f,  -10.086967f,  186.936020f,
        -61.783806f,  183.230362f,  -11.185556f,  155.101013f,  -59.616047f,  261.215698f,
        -53.576157f,  273.007660f,  -59.725330f,  -340.501617f, -13.404168f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    24.922306f,   84.360588f,   20.623468f,
        83.726646f,   24.715248f,   86.730644f,   -13.996565f,  85.950249f,   12.793661f,
        103.167931f,  73.791748f,   57.704735f,   49.791046f,   63.805065f,   47.730625f,
        46.171215f,   38.643028f,   108.844963f,  65.217300f,   88.834625f,   38.639774f,
        85.231354f,   43.788349f,   66.966301f,   36.154102f,   78.812988f,   44.734898f,
        85.619827f,   61.371410f,   78.050430f,   67.061104f,   64.733528f,   108.940453f,
        141.198929f,  111.024895f,  123.359268f,  88.594093f,   110.742065f,  93.160484f,
        101.222168f,  84.467308f,   103.038528f,  97.773178f,   117.984848f,  81.364136f,
        136.433640f,  84.440331f,   130.175262f,  185.836731f,  208.327286f,  182.175919f,
        203.022858f,  151.096542f,  181.743729f,  160.912491f,  147.723846f,  177.747482f,
        120.139153f,  217.403214f,  103.637718f,  138.987747f,  166.219971f,  139.449326f,
        175.073532f,  307.436096f,  282.148041f,  267.272797f,  294.568024f,  274.978973f,
        240.924545f,  244.671585f,  217.400314f,  282.309692f,  159.800705f,  365.864868f,
        130.626251f,  246.025406f,  203.652161f,  205.300186f,  231.155685f,  511.156372f,
        308.333618f,  472.691284f,  305.477539f,  417.952454f,  299.669708f,  470.402618f,
        222.436188f,  441.470245f,  197.470383f,  294.884705f,  246.401199f,  224.502731f,
        264.438690f,  223.221527f,  280.996277f,  0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    16.482292f,   7.109357f,    20.639137f,   57.727268f,   13.004807f,
        18.227802f,   10.061378f,   21.306547f,   35.560631f,   18.501762f,   43.545944f,
        0.400457f,    31.139029f,   -0.403121f,   15.938110f,   -0.085658f,   0.000000f,
        0.000000f,    -18.100033f,  2.182612f,    -35.990719f,  9.323370f,    -41.312016f,
        -2.689314f,   3.374097f,    -536.367493f, 35.811180f,   1.176358f,    35.540966f,
        -10.912848f,  21.312220f,   -4.223512f,   10.876839f,   4.218741f,    -0.000000f,
        0.000000f,    -13.691085f,  3.806641f,    -21.597759f,  15.378715f,   -22.151394f,
        0.267603f,    6.345127f,    -281.479431f, 32.623508f,   87.258598f,   -3.329883f,
        -12.299100f,  -47.076191f,  -39.280899f,  22.039320f,   9.700173f,    39.600586f,
        -1.553521f,   -363.958496f, 125.160141f,  -146.827698f, 57.436184f,   30.089090f,
        -20.895626f,  15.299546f,   -10.741710f,  0.000000f,    0.000000f,    1.279128f,
        11.115057f,   -6.402257f,   33.753227f,   -18.091633f,  63.063473f,   -39.326035f,
        146.624725f,  -521.333496f, 228.938889f,  -147.597946f, 60.402443f,   37.142685f,
        -26.590115f,  36.885460f,   -29.417498f,  0.000000f,    0.000000f,    -11.325729f,
        22.467411f,   -28.012362f,  56.226360f,   -54.240772f,  105.176628f,  -91.522270f,
        153.768967f,  468.525604f,  -436.514191f, 57.140774f,   -79.703346f,  -102.113068f,
        26.586649f,   -74.011475f,  7.612613f,    0.000000f,    0.000000f,    37.413216f,
        5.004012f,    111.711662f,  -28.376871f,  216.848129f,  -138.704697f, 362.869812f,
        -407.433105f, 2.172799f,    0.256017f,    0.000000f,    0.000000f,    -0.871278f,
        1.951045f,    6.126445f,    4.200503f,    2.945931f,    4.708860f,    2.338929f,
        2.875772f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -13.754089f,  -37.395164f,  -9.823144f,
        -13.196869f,  -7.272439f,   -5.530744f,   -6.406291f,   -0.906219f,   -4.618371f,
        -0.005006f,   0.000000f,    0.000000f,    3.724893f,    -2.037147f,   10.848323f,
        -7.093455f,   63.435635f,   -22.867010f,  17.494474f,   -37.801388f,  15.778581f,
        -14.592526f,  12.301581f,   -7.471244f,   8.801614f,    -1.056419f,   8.029238f,
        -0.931613f,   0.000000f,    0.000000f,    -3.343216f,   1.963820f,    6.226449f,
        -7.046361f,   25.826172f,   -20.218149f,  7.184750f,    -32.990273f,  4.795274f,
        -12.169400f,  5.253032f,    -6.055581f,   3.219678f,    -3.396826f,   -1.390074f,
        0.968456f,    0.000000f,    0.000000f,    -3.772702f,   1.209185f,    -0.557448f,
        -4.417694f,   6.690823f,    -13.153469f,  -1.864064f,   -144.094299f, -0.400090f,
        -54.777363f,  3.800975f,    -47.216850f,  1.384103f,    -20.469332f,  -0.770612f,
        3.661544f,    0.000000f,    0.000000f,    -2.774531f,   14.339108f,   -2.176744f,
        17.973396f,   -2.584388f,   24.630222f,   0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    7.212594f,    -0.972383f,   2.571312f,
        1.005241f,    2.149802f,    -0.404017f,   3.102427f,    -0.492872f,   1.674903f,
        1.076641f,    6.802568f,    -2.201382f,   4.720970f,    1.946992f,    8.413211f,
        13.897808f,   -3.929289f,   -4.557363f,   -1.152921f,   2.394116f,    -3.409801f,
        -0.762719f,   3.991938f,    -4.717765f,   3.249434f,    -4.655615f,   -3.859483f,
        -2.192244f,   -1.328570f,   -6.560865f,   -12.172760f,  -8.470062f,   20.000000f,
        10.000000f,   -0.567746f,   -7.731483f,   -9.255570f,   -47.659885f,  10.336613f,
        -22.090805f,  7.517536f,    6.135644f,    -46.911114f,  -30.446760f,  -3.596662f,
        -11.403247f,  4.483356f,    13.328605f,   -42.527500f,  -36.301548f,  -15.509683f,
        -2.708396f,   5.689499f,    10.815009f,   -53.706852f,  -6.238203f,   -5.915776f,
        6.048203f,    5.065859f,    27.873493f,   -60.966034f,  -64.937904f,  -43.208076f,
        -12.068816f,  0.000000f,    0.000000f,    -25.467299f,  -21.947956f,  -25.812618f,
        -21.340672f,  -7.739991f,   -25.922535f,  10.922400f,   -38.025684f,  39.549412f,
        -47.597519f,  81.080460f,   -65.299973f,  181.036911f,  -92.248260f,  278.834686f,
        -100.917229f, 0.000000f,    0.000000f,    25.789515f,   21.456329f,   52.839546f,
        8.745702f,    52.642342f,   17.105492f,   46.431778f,   15.366912f,   46.228222f,
        32.211777f,   0.000000f,    0.000000f,    0.000000f,    0.000000f,    -24.771540f,
        0.267077f,    -54.876797f,  -15.568769f,  -65.145546f,  -33.942783f,  -43.216721f,
        -60.139805f,  -19.964273f,  -82.836655f,  -13.910690f,  5.616528f,    -20.945808f,
        -54.811623f,  -26.042685f,  -51.505871f,  -63.920547f,  -12.658294f,  -25.323023f,
        -35.251453f,  -14.824677f,  16.424063f,   -9.496721f,   7.799864f,    -23.371479f,
        33.914192f};

std::string evalCalcTToString(const EvalCalcT term) {
    return std::format("{:>6.1f}", term);
}

std::string taperedTermToString(const TaperedTerm& term) {
    return std::format("{{{:>6.1f}, {:>6.1f}}}", term.early, term.late);
}

std::string getPstPieceTypeName(int idx) {
    if (idx == EvalParams::kPassedPawnPstIdx) {
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
