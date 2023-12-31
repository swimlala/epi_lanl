CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:28:43Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7T   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    88   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8D   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8H   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8P   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8X   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8`   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8d   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8l   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9l   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  =   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  A�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Bt   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  F   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  I�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  N   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  O    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  R�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  V(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  W   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  Z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  [�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  _    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    _P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    bP   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    eP   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  hP   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    h|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    h�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    h�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         h�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         h�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        h�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    h�Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130111011  20190523124446  1514_5041_233                   2C  D   APEX                            2041                            062805                          846 @�V�W?�1   @�V�W?�@1���l�D�c/333331   GPS     Primary sampling: mixed [deeper than nominal 500dbar: discrete; nominal 500dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   Bi33Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�@ D�3D��D!�3D.s3D:�3DG�3DT3D`33Dl��Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bh  Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��D� D��D!� D.` D:� DG� DT  D`  Dl��Dys31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A��AΟ�A΁A�bNA�9XA�ĜA�JA���A��A�\)A�Q�A�VA��wA�p�A�VA�hsA�/A��/A�JA�|�A��PA�ȴA�A��A��^A�Q�A�S�A��`A�JA���A��HA�1A��HA��TA�%A���A��A�jA���A�n�A�bA�%A�jA�A�A��A�{A�A��A��A��PA��RA��-A���A��A���A�I�A�A~9XA}�FA}t�A}ƨA{�Ax��AuO�Aq`BAo�An��Ak�^AeC�Aa�A]�TAY��AWVAV�DAT~�ASoAQC�AO��ANM�AM��AI�hAGXAF �AD1A=��A<{A;�-A;33A:ĜA9�PA9�A8r�A7%A3XA1�-A1�A05?A.I�A-7LA,��A,-A+A*��A*~�A(r�A'G�A&��A%oA#;dA!�A �RA7LA��A�wA��AA�A�;AdZA�A��AbNA��A��A��A�
A��A�;A��A�A��Av�A^5A=qAƨA
��A
1'A
(�A
{A	��A��A��A"�A�jA1'AO�AbNA/A�9A�Ap�A ��@�S�@�M�@���@�{@�7L@���@�@�ȴ@�&�@��@�@띲@�=q@��@�ƨ@�J@�x�@�@�o@��/@��T@�9X@�5?@��@؃@�ƨ@�@֗�@�5?@���@Ӿw@�ƨ@��#@�%@˅@�@�"�@ʇ+@�^5@��#@� �@ǅ@�t�@���@��@�9X@�33@°!@�E�@��@�33@�n�@��T@��@��@���@��@���@��@��@�  @�Z@��9@�Ĝ@���@�&�@� �@��!@��F@���@�S�@��
@�@�\)@���@�@zJ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�O�A��AΟ�A΁A�bNA�9XA�ĜA�JA���A��A�\)A�Q�A�VA��wA�p�A�VA�hsA�/A��/A�JA�|�A��PA�ȴA�A��A��^A�Q�A�S�A��`A�JA���A��HA�1A��HA��TA�%A���A��A�jA���A�n�A�bA�%A�jA�A�A��A�{A�A��A��A��PA��RA��-A���A��A���A�I�A�A~9XA}�FA}t�A}ƨA{�Ax��AuO�Aq`BAo�An��Ak�^AeC�Aa�A]�TAY��AWVAV�DAT~�ASoAQC�AO��ANM�AM��AI�hAGXAF �AD1A=��A<{A;�-A;33A:ĜA9�PA9�A8r�A7%A3XA1�-A1�A05?A.I�A-7LA,��A,-A+A*��A*~�A(r�A'G�A&��A%oA#;dA!�A �RA7LA��A�wA��AA�A�;AdZA�A��AbNA��A��A��A�
A��A�;A��A�A��Av�A^5A=qAƨA
��A
1'A
(�A
{A	��A��A��A"�A�jA1'AO�AbNA/A�9A�Ap�A ��@�S�@�M�@���@�{@�7L@���@�@�ȴ@�&�@��@�@띲@�=q@��@�ƨ@�J@�x�@�@�o@��/@��T@�9X@�5?@��@؃@�ƨ@�@֗�@�5?@���@Ӿw@�ƨ@��#@�%@˅@�@�"�@ʇ+@�^5@��#@� �@ǅ@�t�@���@��@�9X@�33@°!@�E�@��@�33@�n�@��T@��@��@���@��@���@��@��@�  @�Z@��9@�Ĝ@���@�&�@� �@��!@��F@���@�S�@��
@�@�\)@���@�@zJ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�BF�Bk�Bn�B{�B�=B�1B~�Bz�B^5B��B��BG�BbB�TB��B�B�qB�3B�B�PB��B�-BhsB6FBB�B�mB�B�B��B��B�BhB�BB�XB��B�B��B�FB�uB��B�wB�BhsBF�B �B
�B
ǮB
�9B
�B
��B
�7B
p�B
XB
F�B
%�B	��B	�mB	�HB	�`B	��B	�B	��B	�LB	��B	�JB	�B	l�B	?}B	&�B	B�HB��B��BĜB�^B�!B�B��B��B�bB�7B�By�Bz�B{�B|�B|�B{�B|�B{�B{�B� B|�B�B|�B~�B�+B�B�+B�B�B�+B�Bz�Bz�Bx�By�Bx�Bv�Bm�Bn�BdZB`BB]/B]/B]/B\)B]/B\)BW
BS�BN�BO�BO�BS�BS�BS�BW
B]/B^5B`BBdZBhsBl�Bp�Bp�Bo�Bn�Bo�Bm�Bm�Bm�Bk�BiyBffBiyBk�Bk�BjBiyBhsBiyBk�Bk�BhsBffBcTBaHB_;B]/BZBYBXBVBVBT�BR�BQ�BO�BM�BL�BL�BM�BM�BN�BO�BM�BL�BL�BO�BL�BN�BP�BS�BXB^5BbNBe`BiyBo�Bp�Bx�B�1B�JB�\B�hB�{B�{B�uB�hB�hB�oB��B��B��B�^B�wB�}B�qB�dB�qBǮB��B��B�B�5B��B	
=B	�B	�LB	�XB	��B	�B	��B
JB
\B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�5BG�Bl�Bn�B|�B�DB�=B� B|�BhsBB��BO�B�B�ZB��B&�BƨB�-BhB�bB��B�qBn�B?}B
=B��B�yB�B�B��B�B�B�B�B�^B�}B�/B��B�dB�{B��B��B�?Bm�BN�B/B
�B
��B
�XB
�'B
�B
�uB
u�B
\)B
M�B
1'B	��B	�sB	�NB	�`B	��B	�B	�#B	��B	��B	�bB	�=B	{�B	F�B	-B	
=B�fB��B��BƨB�wB�3B�B��B��B��B�JB�+B�1B~�B}�B}�B~�B~�B}�B}�B�B�DB�B�B�B�B�7B�B�7B�B�%B�7B�+B}�B{�B{�B}�B|�Bx�Bq�Bo�BgmBe`B`BB^5B^5B_;B`BB`BB\)BXBS�BVBS�BVBT�BVBXB^5B_;BaHBffBk�Bo�Bp�Bp�Bq�Br�Bq�Bp�Bo�Bo�Bn�Bl�BjBk�Bm�Bm�Bm�Bm�BjBm�Bo�Bm�Bl�BjBffBdZBbNB`BB^5B\)BZBYBXBVBT�BS�BS�BR�BO�BO�BO�BN�BO�BP�BM�BM�BN�BP�BQ�BQ�BR�BVBYB^5BcTBffBjBr�Bq�By�B�=B�\B�hB�oB��B��B��B�{B�oB�oB��B��B��B�^B��B��B�}B�dB�qBǮB��B��B�B�5B��B	DB	�B	�RB	�XB	��B	�B	��B
JB
\B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<T��<u<D��<D��<#�
<#�
<u<T��<#�
<#�
<#�
<e`B<u<49X<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<D��<�o<D��<D��<49X<#�
<T��<T��<#�
<#�
<49X<e`B<#�
<#�
<#�
<#�
<#�
<D��<T��<e`B<#�
<#�
<D��<�t�<D��<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181658262011121816582620111218165826  AO  ARGQ                                                                        20111130111011  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130111011  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165826  IP                  G�O�G�O�G�O�                