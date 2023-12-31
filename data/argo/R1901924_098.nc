CDF       
      	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       	DATE_TIME         N_PROF        N_PARAM       N_LEVELS  !   N_CALIB       	N_HISTORY            	   title         Argo float vertical profile    institution       BODC   source        
Argo float     history       04-Jun-2023 08:16:38Zcreation      
references        (http://www.argodatamgt.org/Documentation   comment       bThis netCDF file is generated using BODC's argoReader and netCDF writer software (argo@bodc.ac.uk)     user_manual_version       3.4    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    <H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    <X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    <\   REFERENCE_DATE_TIME                	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    <`   DATE_CREATION                  	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    <p   DATE_UPDATE                	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    <�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    <�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  <�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  <�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  =   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        =H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    =L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    =P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     =T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    =t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    =x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     =|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     =�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     =�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    =�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
_FillValue        A.�~       
resolution        >�E�vQ�        =�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    =�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        =�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            =�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            =�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    >   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    >   VERTICAL_SAMPLING_SCHEME                   	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    >   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ?   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ?   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ?   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ?   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                    	valid_max         @�p        axis      Z      
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  ?    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  G�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  P(   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  X�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  Z�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                    	valid_max         @�p        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  _   PSAL_ADJUSTED            
      
   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  g�   TEMP_ADJUSTED            
      
   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  p    PRES_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PRES_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 $  x�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PSAL_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 $  z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   standard_name         TEMP_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 $  |�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PRES_ADJUSTED_ERROR    units         decibar    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �     PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PSAL_ADJUSTED_ERROR    units         psu    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         TEMP_ADJUSTED_ERROR    units         degree_Celsius     conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    source_name       	PARAMETER      conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	             	long_name         'Calibration equation for this parameter    source_name       SCIENTIFIC_CALIB_EQUATION      
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	             	long_name         *Calibration coefficients for this equation     source_name       SCIENTIFIC_CALIB_COEFFICIENT   
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	             	long_name         .Comment applying to this parameter calibration     source_name       SCIENTIFIC_CALIB_COMMENT   
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	            	long_name         Date of calibration    source_name       SCIENTIFIC_CALIB_DATE      conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     source_name       HISTORY_INSTITUTION    conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    source_name       HISTORY_STEP   conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    source_name       HISTORY_SOFTWARE   conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     source_name       HISTORY_SOFTWARE_RELEASE   conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      source_name       HISTORY_REFERENCE      conventions       Institution dependent      
_FillValue                  �  �(   HISTORY_DATE                     	long_name         #Date the history record was created    source_name       HISTORY_DATE   conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_ACTION                       	long_name         Action performed on data   source_name       HISTORY_ACTION     conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   source_name       HISTORY_PARAMETER      conventions       Argo reference table 3     
_FillValue                  0  �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   source_name       HISTORY_START_PRES     units         decibar    
_FillValue        G�O�        �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    source_name       HISTORY_STOP_PRES      units         decibar    
_FillValue        G�O�        �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    source_name       HISTORY_PREVIOUS_VALUE     
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   source_name       HISTORY_QCTEST     conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0  �tArgo profile    3.1 1.2 19500101000000  20230604081645  20230604081645  1901924 Argo UK                                                         Jon Turton                                                      PSAL            TEMP            PRES               bA   BO  165542                          2B  A   APEX                            8981                            2.13.1.1                        846 @�0L��/�1   @�0L��/��C�^��@D��҈�p1   GPS     Primary sampling: mixed                                                                                                                                                                                                                                            9A   A   A   @�{@�
=A (�A Q�A@z�A`  A�
A�A�  A��
A�{A�  A�A�  B 33B�B  B  B��B(  B/��B8
=B?��BG�BO�
BW�B`{Bg�HBo�HBw�HB�B�
=B�
=B�\B���B���B���B���B�
=B�B�  B�B�
=B�
=B���B��B���B��B���B�  B�
=B���B���B���B�B�B�  B�B�  B�B���B���C C�C��C�RC��C
  C  CC�qC�CC�C�C  C�C  C��C!��C#��C%��C(  C*  C+�qC.�C/�qC1��C4  C5�qC7�qC9�qC<�C>�C@CA��CC��CE��CG��CI�RCK�RCM��CO�3CQ�RCS�RCV  CX  CY�RC[��C]�qC_�qCa�RCc��Cf�Cg��Ci�qCk�qCnCp�Cr  Ct�CvCx�Cz�C|C}�qC�qC�  C�C�HC�HC��C��C�HC�  C�  C���C��qC�  C��qC���C��)C��)C��qC�  C��C��C�  C��C�  C���C�HC��qC�  C�HC�  C��C��C��qC��qC�HC��C��C�HC���C��C��C��C���C��C�C��C��C�HC��qC�  C���C���C��C�HC��qC���C���C���C��qC��C��C��C���C��qC���C���C��C��C���C�  C��C�HC�HC��C�  C��qC���C��qC��C��C�HC�HC��C��qC�  C���C���C���C���C��C��C��C��C��qC�HC�HC���C���C�HC�HC���C��qC�  C��C�C�fC��C��qC��qC�HC�  C�  C���C���C��C��qC���C���C���C�  C�  C�  C��)C���C�  C���C���C���C��qD �HD�D� D  D� D�qD~�D �D��DHD\D�\D� D�\D~�D�\D\D��D	~�D	�\D
��D �D~�D��D�HDHD~D�qD\D  D��D �D� D�qD|�D�qD� DHD\D�\D� D�D��D�D~�D�\D��D  D�HD�\D~D��D� D��D\DHD�HD  D|�D��D��D�\D~�D��D \D!HD!� D!�D"~D"�\D#� D$  D$~D$�D%~�D& �D&��D'�D'��D( �D(\D)  D)�HD*  D*~�D+  D+��D,�D,�HD,��D-~D.HD.��D.�\D/\D/�D0\D1�D1� D1�\D2\D2��D3� D4�D4�HD5HD5\D5�\D6��D7HD7��D8  D8~D9  D9� D9�\D:\D;  D;��D<HD<��D=  D=~�D>  D>\D>�\D?~�D?��D@\DA  DA\DA�DB~�DB�\DC��DD�DD� DE  DE��DE��DF~�DF�\DG}qDG�qDH\DI �DI\DJ  DJ��DK �DK��DK��DL~�DM  DM~�DM��DN\DO�DO\DO�qDP~�DP��DQ~DRHDR�HDS  DS\DS��DT\DT�\DU�HDV�DV��DW�DW�3DX�DX� DX�\DY\DY�\DZ��DZ�\D[� D\HD\}qD\��D]��D^�D^�HD_ �D_~D_��D`\D`�qDa~Da�qDb|�Db�qDc}qDc�\Dd� Dd�De��De�\Df~�Dg  Dg�HDh�Dh\Dh��Di}qDi�\Dj��Dj�\Dk� Dl3Dl��DmHDm��Dn �Dn��Dn��Do}qDp  Dp��Dp�\Dq~�Dr  Dr~Dr�\Ds� Ds��Dt��Du3Du��Dv  Dv\Dw  Dw��Dx�Dx��Dx�D|�D~{�D�]�D��\D�D�J�D���D��D��RD�7\D�,�D�o
D��RD�7\D�z=D���D��fD�<{D�~D���D�G\D��
D�T�D���D�>fD���D�;�D��{D�;3D��=D�NfD��RD�C3D��3D�K�D��)D�>D¾D�9�D�ɚD�\D�J=D�q�D�=D� �D�7�D�r=D���B�aB��B�-B��B�?B��B��B��B��B�*B�*B��B�2B��B��B�rB�B�$B�*B��B��B�B�jB��B��BB�BGB{B�BgB9BMB3B�BEB�B^B	BtB�B�BB�B3B�B�BuBABAB�BB3BB�B�B;B �B OBuB�B�HB�BB�(B��B��B��B�BB�B �B�B�B�BUB OB B��B�B�JB��B�B�cB+B�B�LB�BɠB�"B�~B�<B҉BބB�KB�B�!B�qB�)B��B�B\BaB�B�BbBDBB�fB�ZB�B�BݘB�:B��B��B�hB��B��B��B��B~�BwBs�BncBm�Bi�BgBa|B_�Ba�BeBoBw�BwfB{�B~�BHB�B�OB��B��B��B��B�B��B��B~BB}B{�BvBt9Bp�Bl"BbNBZBXyBW�BVSBT�BRBH�BD�BAoB<jB72B2B.}B)*B&�B"NB�BWB�B�BBBB6B�B�B��B��B�B�`B��B�sB��B�jB�EB��B��B�9B��B�OB�=B�*B�8B��B��B��B��B�B�KB��B��B�MB�-B�By$Bq�Bl�Bj0Bd�BZ7BP.BG�BC�B>�B<�B;B:�B:*B7�B.IB'�BQB BdB	�B�B�B?B�B�B �B �BB+B�B�B�B�B�B	lB�B
�zB
�B
�B
��B
�IB
ۦB
�EB
��B
��B
āB
� B
�]B
�lB
��B
��B
��B
�IB
��B
�*B
�fB
��B
��B
�&B
��B
��B
��B
��B
��B
�{B
�B
~]B
|�B
y�B
v�B
t�B
r�B
oiB
l"B
jB
g�B
gB
dZB
b�B
`�B
^�B
Z�B
S�B
L�B
I�B
H1B
C�B
C-B
AUB
@�B
?�B
=B
=�B
<�B
9�B
4�B
/iB
+QB
&�B
"�B
�B
�B
�B
�B
\B
�B
�B
�B
B
B
�B
B
{B
�B
uB
�B
 �B
 4B	��B	��B	��B	�B	�jB	�B	�DB	��B	��B	��B	�nB	�B	�B	�MB	�3B	�B	�aB	�B	�vB	��B	�"B	�hB	�B	ܬB	��B	�B	��B	�B	ǔB	�B	�HB	�IB	��B	�QB	��B	�yB	�
B	��B	�uB	��B	�hB	�HB	��B	��B	�B	��B	�B	��B	��B	��B	�B	�4B	��B	��B	�;B	��B	�gB	��B	��B	��B	�rB	��B	�pB	��B	�,B	��B	��B	�B	�B	�B	�nB	�B	��B	�\B	�B	��B	��B	��B	�B	�_B	�B	��B	��B	�QB	�1B	�CB	�B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	�B	��B	�2B	��B	�{B	�B	�[B	�{B	��B	��B	�aB	�:B	�B	��B	�#B	��B	��B	��B	�aB	�4B	~BB	}�B	��B	�9B	�mB	�SB	�mB	�B	��B	�=B	��B	��B	�B	��B	�DB	�B	�B	�PB	�6B	��B	��B	�HB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�=B	�CB	�pB	��B	�ZB	�B	��B	��B	��B	��B	��B	��B	�IB	�}B	��B	��B	��B	�iB	�/B	��B	��B	�"B	�aB	��B	�JB	�{B	�_B	��B	ɠB	�dB	��B	�~B	��B	�7B	�/B	�B	�FB	�KB	�B
tB
�B
�B
�B

B
&2B
0�B
6+B
B�B
P�B
[	B
hsB
i*B
qAB
x8B
~�B
��B
��B
��B
�B
��B
ǔB
�?B
�B
�wB
�zB �Au$�Au�Au+Au6zAuE9AuP�AuiDAuxlAu��Au�7Au�MAun�AudZAul�Au|Au��Au��Au��Au��Au��Au��Au�RAu�Au�Au��Au�
Au�`Au�Au�`Au�Au��Au��Au��Au�MAu��Au�2Au��AuZ�Au(At�gAt�IAt�xAt�.Atl"AtS�At:*At8At4�At6�At6At@OAtC-AtE�At@OAt7LAt�As��As�>As�As�vAs��As�Ask�Asg�AsV�AsMAsL0AsA�As.IAs(�As/�As($As�As
=Ara�Ap�ApY�An�AnoAmqvAl�HAl��AkC�AjߤAh�Ae6zAc��Ab��Aa8Aa�A`��A`�IA`�fA`YKA_�_A^m]A]8�A]:�A]_A]�A]��A\�9A\�.A\c A[�&A[H�AY҉AXeAW��AW�mAW�XAVe�AS�CARZ�AP��AO�cAO�AO��AO�SAO6zAN�:ANDgAM��AM�}AM-wAL�+ALS�AK��AL�AL�AK��AKc�AJ��AKYAK)_AK)_AK$AK	AJ��AJ��AJ'RAI�AI�jAIl�AH��AH�AHXAH%FAG��AG�~AGQ�AF��AE��AE�AD��AD�[AD��AD��ADeACeAB��ABhsAA�AA1�A@��A@bNA?�/A?��A>��A>��A>j�A=��A=/�A<��A<��A<M�A;��A;eA:s�A9�}A98A8YA7�A6�mA6k�A5�A5:�A4$A3�+A3H�A36zA2ݘA2��A2E9A2�A1�AA1�A1��A1�MA1I�A0��A0�A.҉A.tTA.o�A-�A-M�A,��A,.IA+��A+L�A*dZA)J#A(�@A(:�A'��A'�{A'n/A'd�A'_A'
=A&U2A%�eA$<�A#�A"ZA"�A!�HA!�qA!YKA!)�A �A +�A �A 
�A .IA [�A bNA GEA �A�tAl�A��Ag8A�AB�A��AiDA:�A��A��A�sA�A��A%A!�A��AffA�Ae�A֡A�dA(�A�HA�A�A{�AںA��A3�A�A��AQA�A�kAA
�XA
S�A	�)A	�A�A�A��A��AMjA�A֡A��A��A�0A��AeA��A=A�AA��Ad�A+A1�AA�A �h@��@�M@���@�=@�V�@�/@�?}@��@��@�  @��j@�@�c @���@�ݘ@�[W@��@��@�o@�)�@��@�Dg@���@��@�z@���@�o @�6�@�dZ@�@��@��@�@�-@��@�k@�A�@�E9@�>�@�+@�M�@��u@�2�@�l�@��;@�ߤ@��@�?�@��&@��@׮@�2a@ϓ@���@��@�W?@�(@Ȯ}@�5?@��K@�U�@��?@��@��@ńM@�q@�C�@�)_@�G�@���@��@�`B@�@�7@���@��4@�RT@�7L@���@���@�v�@��@� �@���@���@�~�@�p�@�s�@���@�+@�u�@�4@�n/@��o@��@��H@�B�@�Z�@���@�F�@���@�A�@�,=@�9X@�E�@�E�@��}@�m]@�zx@��$@�p�@�=@��m@�C�@�X�@��s@��X@�?}@���@��b@�Xy@��@��@��@��@���@���@��h@�j@�	l@���@�͟@�_@� i@�m�@��@�+@�V@���@�33@� \@��8@��Q@�!-@��]@���@��?@���@���@���@��e@���@��@���@��+@��@��
@���@�˒@�u�@�B�@�>�@�5�@�@�M@�@��X@��V@��@��f@�|@�{J@�c@�m]@�X�@�H�@�/�@�6z@�A�@�@��2@��[@��)@��U@�v�@�d�@�R�@�!@���@��@@�a�@�;@�W�@��@���@�c�@��@��L@�a@�ѷ@���@�0�@�u@���@�@�S�@���@�w�@}&�@{��@x�4@w�{@v{@uV@sA�@nW�@l�?@h�U@c�}@\u�@Yk�@U�@R#:@O�	@N�@Kv`@I<6@H��@F͟@D4n@B\�@A�X@@!@>��@<�|@:($@7Y@4�e@1��@0��@/W?@.�@-rG11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�{@�
=A (�A Q�A@z�A`  A�
A�A�  A��
A�{A�  A�A�  B 33B�B  B  B��B(  B/��B8
=B?��BG�BO�
BW�B`{Bg�HBo�HBw�HB�B�
=B�
=B�\B���B���B���B���B�
=B�B�  B�B�
=B�
=B���B��B���B��B���B�  B�
=B���B���B���B�B�B�  B�B�  B�B���B���C C�C��C�RC��C
  C  CC�qC�CC�C�C  C�C  C��C!��C#��C%��C(  C*  C+�qC.�C/�qC1��C4  C5�qC7�qC9�qC<�C>�C@CA��CC��CE��CG��CI�RCK�RCM��CO�3CQ�RCS�RCV  CX  CY�RC[��C]�qC_�qCa�RCc��Cf�Cg��Ci�qCk�qCnCp�Cr  Ct�CvCx�Cz�C|C}�qC�qC�  C�C�HC�HC��C��C�HC�  C�  C���C��qC�  C��qC���C��)C��)C��qC�  C��C��C�  C��C�  C���C�HC��qC�  C�HC�  C��C��C��qC��qC�HC��C��C�HC���C��C��C��C���C��C�C��C��C�HC��qC�  C���C���C��C�HC��qC���C���C���C��qC��C��C��C���C��qC���C���C��C��C���C�  C��C�HC�HC��C�  C��qC���C��qC��C��C�HC�HC��C��qC�  C���C���C���C���C��C��C��C��C��qC�HC�HC���C���C�HC�HC���C��qC�  C��C�C�fC��C��qC��qC�HC�  C�  C���C���C��C��qC���C���C���C�  C�  C�  C��)C���C�  C���C���C���C��qD �HD�D� D  D� D�qD~�D �D��DHD\D�\D� D�\D~�D�\D\D��D	~�D	�\D
��D �D~�D��D�HDHD~D�qD\D  D��D �D� D�qD|�D�qD� DHD\D�\D� D�D��D�D~�D�\D��D  D�HD�\D~D��D� D��D\DHD�HD  D|�D��D��D�\D~�D��D \D!HD!� D!�D"~D"�\D#� D$  D$~D$�D%~�D& �D&��D'�D'��D( �D(\D)  D)�HD*  D*~�D+  D+��D,�D,�HD,��D-~D.HD.��D.�\D/\D/�D0\D1�D1� D1�\D2\D2��D3� D4�D4�HD5HD5\D5�\D6��D7HD7��D8  D8~D9  D9� D9�\D:\D;  D;��D<HD<��D=  D=~�D>  D>\D>�\D?~�D?��D@\DA  DA\DA�DB~�DB�\DC��DD�DD� DE  DE��DE��DF~�DF�\DG}qDG�qDH\DI �DI\DJ  DJ��DK �DK��DK��DL~�DM  DM~�DM��DN\DO�DO\DO�qDP~�DP��DQ~DRHDR�HDS  DS\DS��DT\DT�\DU�HDV�DV��DW�DW�3DX�DX� DX�\DY\DY�\DZ��DZ�\D[� D\HD\}qD\��D]��D^�D^�HD_ �D_~D_��D`\D`�qDa~Da�qDb|�Db�qDc}qDc�\Dd� Dd�De��De�\Df~�Dg  Dg�HDh�Dh\Dh��Di}qDi�\Dj��Dj�\Dk� Dl3Dl��DmHDm��Dn �Dn��Dn��Do}qDp  Dp��Dp�\Dq~�Dr  Dr~Dr�\Ds� Ds��Dt��Du3Du��Dv  Dv\Dw  Dw��Dx�Dx��Dx�D|�D~{�D�]�D��\D�D�J�D���D��D��RD�7\D�,�D�o
D��RD�7\D�z=D���D��fD�<{D�~D���D�G\D��
D�T�D���D�>fD���D�;�D��{D�;3D��=D�NfD��RD�C3D��3D�K�D��)D�>D¾D�9�D�ɚD�\D�J=D�q�D�=D� �D�7�D�r=D���B�B��B��B��B�%B��B�bB��B��B�8B�[B��B�$B�_B��B�lB�B��B�B��B��B�'B�hB��B�B �B�BPB�B�BmB"BAB.B�B�BSBB	�B�B�B�BuB)BpB�B�BrBDB,B�BB?B1B�B@B^B �B 1B�B�B��B�OB�RB�B��B��B�sB�$B vB�B!B�B�B�B�BHB��B��B��B�B\B?B	OB��B�B�6B�7B�*B��B��B޼B��B�IB�B�cB�<B�kB1B�B�B_BB�B�B�B��B�B��B�bB��BؘB�EB��B�FB�IB��B�B��B�0Bw�Bu2Bn�Bn�Bj�Bh�BbNB_�Ba�BesBpxBx�Bw9B{�B~�BVB�B��B��B��B�eB�+B��B�tB�uB~�B}�B|�Bv�Bt�Br0Bn�Bc�BZoBX�BW�BV�BU�BTtBI�BE�BB�B>9B8<B3$B/�B)�B("B#TBB�B�BByBBB	�B2B�uB��B��B��B�tBعB�-B�;B��B�qB��B�uB��B�B��B��B��B��B��B�YB�aB�jB�'B��B��B�jB�cB�pBz�Br�Bm@Bk�Bg*B]BQ�BH�BEB?oB<�B;3B:�B:�B9�B0!B+NBHBB&B
B	?BeB�B8B�BB �B�B�B�BBjB�B|BaB�B
��B
�(B
�0B
�B
��B
��B
��B
�6B
��B
ŗB
¹B
��B
��B
�]B
�bB
��B
��B
�
B
��B
��B
��B
�B
��B
�4B
��B
��B
��B
��B
�BB
�/B
(B
~B
z�B
w�B
vCB
thB
p�B
mGB
j�B
h�B
hB
eB
c#B
aCB
`�B
]OB
WB
M�B
KDB
I(B
DlB
C�B
A�B
AGB
@�B
<�B
=vB
>^B
;�B
7+B
1QB
-JB
'�B
$2B
!cB
B
�B
�B
�B
�B
^B
�B
	VB
�B
^B
�B
�B
AB
�B
�B
*B
 :B
 dB
 �B	�}B	��B	��B	�B	�B	�B	��B	�iB	��B	��B	�1B	�RB	�DB	�BB	�B	�YB	�%B	�:B	�_B	��B	ޜB	�B	�|B	�MB	��B	ԢB	��B	�(B	��B	�UB	��B	��B	�HB	�B	��B	�_B	��B	�B	�FB	��B	�3B	�
B	��B	��B	�3B	��B	�hB	�B	��B	�yB	��B	� B	��B	��B	��B	�B	��B	�/B	��B	��B	��B	��B	��B	�B	�.B	��B	�B	�ZB	�OB	�UB	��B	�:B	�-B	�/B	��B	�*B	�9B	�XB	�B	��B	�sB	��B	�8B	�-B	�3B	��B	��B	��B	�B	��B	��B	�&B	�rB	�SB	��B	�\B	��B	�kB	��B	��B	�^B	�IB	��B	��B	��B	�}B	��B	�B	�;B	��B	�2B	��B	�B	�WB	�+B	��B	��B	�DB	~�B	}�B	��B	�YB	��B	�\B	�yB	�6B	��B	�8B	��B	��B	�B	��B	�cB	�zB	�QB	�aB	�RB	�B	��B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�GB	��B	�<B	��B	��B	�	B	�oB	�.B	��B	��B	�B	�IB	�&B	�$B	��B	�B	��B	��B	�B	��B	�%B	�uB	�jB	��B	��B	��B	��B	ÑB	�yB	�B	��B	̲B	�B	̢B	�<B	�QB	�SB	�B	�wB	�|B	��B
�B
�B
�B
�B
2B
&\B
0�B
69B
C B
P�B
[B
h�B
iIB
qWB
xCB
B
��B
��B
��B
�-B
�	B
ǡB
�FB
�B
�zB
��B �Au$�Au�Au+Au6zAuE9AuP�AuiDAuxlAu��Au�7Au�MAun�AudZAul�Au|Au��Au��Au��Au��Au��Au��Au�RAu�Au�Au��Au�
Au�`Au�Au�`Au�Au��Au��Au��Au�MAu��Au�2Au��AuZ�Au(At�gAt�IAt�xAt�.Atl"AtS�At:*At8At4�At6�At6At@OAtC-AtE�At@OAt7LAt�As��As�>As�As�vAs��As�Ask�Asg�AsV�AsMAsL0AsA�As.IAs(�As/�As($As�As
=Ara�Ap�ApY�An�AnoAmqvAl�HAl��AkC�AjߤAh�Ae6zAc��Ab��Aa8Aa�A`��A`�IA`�fA`YKA_�_A^m]A]8�A]:�A]_A]�A]��A\�9A\�.A\c A[�&A[H�AY҉AXeAW��AW�mAW�XAVe�AS�CARZ�AP��AO�cAO�AO��AO�SAO6zAN�:ANDgAM��AM�}AM-wAL�+ALS�AK��AL�AL�AK��AKc�AJ��AKYAK)_AK)_AK$AK	AJ��AJ��AJ'RAI�AI�jAIl�AH��AH�AHXAH%FAG��AG�~AGQ�AF��AE��AE�AD��AD�[AD��AD��ADeACeAB��ABhsAA�AA1�A@��A@bNA?�/A?��A>��A>��A>j�A=��A=/�A<��A<��A<M�A;��A;eA:s�A9�}A98A8YA7�A6�mA6k�A5�A5:�A4$A3�+A3H�A36zA2ݘA2��A2E9A2�A1�AA1�A1��A1�MA1I�A0��A0�A.҉A.tTA.o�A-�A-M�A,��A,.IA+��A+L�A*dZA)J#A(�@A(:�A'��A'�{A'n/A'd�A'_A'
=A&U2A%�eA$<�A#�A"ZA"�A!�HA!�qA!YKA!)�A �A +�A �A 
�A .IA [�A bNA GEA �A�tAl�A��Ag8A�AB�A��AiDA:�A��A��A�sA�A��A%A!�A��AffA�Ae�A֡A�dA(�A�HA�A�A{�AںA��A3�A�A��AQA�A�kAA
�XA
S�A	�)A	�A�A�A��A��AMjA�A֡A��A��A�0A��AeA��A=A�AA��Ad�A+A1�AA�A �h@��@�M@���@�=@�V�@�/@�?}@��@��@�  @��j@�@�c @���@�ݘ@�[W@��@��@�o@�)�@��@�Dg@���@��@�z@���@�o @�6�@�dZ@�@��@��@�@�-@��@�k@�A�@�E9@�>�@�+@�M�@��u@�2�@�l�@��;@�ߤ@��@�?�@��&@��@׮@�2a@ϓ@���@��@�W?@�(@Ȯ}@�5?@��K@�U�@��?@��@��@ńM@�q@�C�@�)_@�G�@���@��@�`B@�@�7@���@��4@�RT@�7L@���@���@�v�@��@� �@���@���@�~�@�p�@�s�@���@�+@�u�@�4@�n/@��o@��@��H@�B�@�Z�@���@�F�@���@�A�@�,=@�9X@�E�@�E�@��}@�m]@�zx@��$@�p�@�=@��m@�C�@�X�@��s@��X@�?}@���@��b@�Xy@��@��@��@��@���@���@��h@�j@�	l@���@�͟@�_@� i@�m�@��@�+@�V@���@�33@� \@��8@��Q@�!-@��]@���@��?@���@���@���@��e@���@��@���@��+@��@��
@���@�˒@�u�@�B�@�>�@�5�@�@�M@�@��X@��V@��@��f@�|@�{J@�c@�m]@�X�@�H�@�/�@�6z@�A�@�@��2@��[@��)@��U@�v�@�d�@�R�@�!@���@��@@�a�@�;@�W�@��@���@�c�@��@��L@�a@�ѷ@���@�0�@�u@���@�@�S�@���@�w�@}&�@{��@x�4@w�{@v{@uV@sA�@nW�@l�?@h�U@c�}@\u�@Yk�@U�@R#:@O�	@N�@Kv`@I<6@H��@F͟@D4n@B\�@A�X@@!@>��@<�|@:($@7Y@4�e@1��@0��@/W?@.�@-rG11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP, where dP is SURFACE PRESSURE from current cycle.                                                                                                                                                                                     N/A                                                                                                                                                                                                                                                             PSAL_ADJUSTED = celltm_sbe41(PSAL,TEMP,PRES,e_time,alpha,tau)                                                                                                                                                                                                   dP = 0                                                                                                                                                                                                                                                          N/A                                                                                                                                                                                                                                                             e_time assumes 0.09dBar/s ascent rate, alpha=0.141, tau=6.68s in continuous profile mode, or alpha=0.0267, tau=18.6s if in spot sampling mode                                                                                                                   null                                                                                                                                                                                                                                                            N/A                                                                                                                                                                                                                                                             Salinity corrected for Cell Thermal Mass (CTM), Johnson et al.(2007), JAOT & effects of pressure adjustments                                                                                                                                                    20230604081441              20230604081441  BO  BO  BO  ARGQARGQARGQRTSPPREXRTQC1.0 2.0 2.0                                                                                                                                                                                                 202306040814412023060408144120230604081444  CV  CV  QCP$                                                G�O�G�O�@�{G�O�G�O�D���H!��H!��H!��                                14778366        