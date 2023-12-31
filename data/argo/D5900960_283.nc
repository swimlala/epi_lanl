CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:28:51Z UW 3.1 conversion   
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
resolution        =���   axis      Z        8  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  =�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  @�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     8  A�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  D�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  G�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  H�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  L�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  SD   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  WL   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     8  X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  [T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    [�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ^�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    a�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  d�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    d�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    d�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    d�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    d�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  d�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    e    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    e   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    e   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         e$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         e(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        e,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    e0Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL              A   AO  20111130111555  20190523124446  1514_5041_283                   2C  D   APEX                            2041                            062805                          846 @ծ�\���1   @ծ�\���@0߾vȴ9�c���
=q1   GPS     Primary sampling: mixed [deeper than nominal 500dbar: discrete; nominal 500dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   B͙�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� D��D�fD!�fD.l�D;  DGs3DS��D`FfDl�3Dy��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�  B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���D�fD�3D!�3D.Y�D:��DG` DSٚD`33Dl� Dyy�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A���A�x�A�hsA��A�VA�5?A��HA���A�C�A�p�A���A�G�A�7LA�r�A��A��DA�1'A�  A���A���A���A��-A��A��A��PA�Q�A�?}A���A�~�A���A��`A��A�VA��-A�`BA���A���A��DA�VA�M�A��wA�\)A�\)A��A�{A�$�A��7A�ĜA��!A���A��\A�"�A�dZA�=qA�`BA�I�AzJAxȴAw�AsS�Ap{Ail�Ac�
A`�+A[hsAX�AV �AS��AO��AN�AM7LAJ��AJ1AI��AH�RAFVAD1'AC&�AB��AAS�A@JA;�TA9?}A7\)A6 �A4=qA2��A0~�A.Q�A+;dA*$�A(�HA'�#A%�
A$�A#?}A!�^A �DA (�A��A�PA5?A��A�A�A�uA�;A�Av�Ax�A
=A��A�;A��A��A��A�HAdZA�mAl�AQ�A7LA�!AhsA
v�A	��A�yA�A��AjA�7A��A�AO�A ��A v�A r�A ��A �\A 1@���@�K�@�ff@�5?@�$�@���@��@���@��j@�Z@��@��P@�S�@�o@��H@��+@�{@�/@���@�33@���@�-@�/@�z�@��@�X@�@�+@���@�M�@�@�x�@�@�33@��@�9@�@�dZ@�z�@�@�%@�Ĝ@�b@�dZ@�/@۶F@۾w@���@�ƨ@�dZ@�dZ@�=q@ؼj@� �@���@�-@ȣ�@�ȴ@��R@�Z@��F@�M�@���@�A�@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�{A���A�x�A�hsA��A�VA�5?A��HA���A�C�A�p�A���A�G�A�7LA�r�A��A��DA�1'A�  A���A���A���A��-A��A��A��PA�Q�A�?}A���A�~�A���A��`A��A�VA��-A�`BA���A���A��DA�VA�M�A��wA�\)A�\)A��A�{A�$�A��7A�ĜA��!A���A��\A�"�A�dZA�=qA�`BA�I�AzJAxȴAw�AsS�Ap{Ail�Ac�
A`�+A[hsAX�AV �AS��AO��AN�AM7LAJ��AJ1AI��AH�RAFVAD1'AC&�AB��AAS�A@JA;�TA9?}A7\)A6 �A4=qA2��A0~�A.Q�A+;dA*$�A(�HA'�#A%�
A$�A#?}A!�^A �DA (�A��A�PA5?A��A�A�A�uA�;A�Av�Ax�A
=A��A�;A��A��A��A�HAdZA�mAl�AQ�A7LA�!AhsA
v�A	��A�yA�A��AjA�7A��A�AO�A ��A v�A r�A ��A �\A 1@���@�K�@�ff@�5?@�$�@���@��@���@��j@�Z@��@��P@�S�@�o@��H@��+@�{@�/@���@�33@���@�-@�/@�z�@��@�X@�@�+@���@�M�@�@�x�@�@�33@��@�9@�@�dZ@�z�@�@�%@�Ĝ@�b@�dZ@�/@۶F@۾w@���@�ƨ@�dZ@�dZ@�=q@ؼj@� �@���@�-@ȣ�@�ȴ@��R@�Z@��F@�M�@���@�A�@���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B.B6FBI�BN�BW
B`BBe`BcTBffBdZBe`Be`BaHB`BB_;B^5B\)B^5BbNBVBQ�BT�BVBS�BN�BM�BJ�BL�BE�B7LB)�B�BbB+B%BB�B�ZB�BȴB��B� BO�B>wB+B�BDB
��B
�yB
ÖB
��B
�oB
�B
n�B
]/B
<jB
hB
+B	��B	�#B	�}B	��B	q�B	cTB	C�B	2-B	%�B	�B	bB	
=B	B��B��B��B�B�fB�;B�/B�#B�B��BĜB�}B�dB�XB�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�bB�\B�VB�PB�\B�uB��B��B��B��B��B�!B�3B�LB�RBĜBƨB��B��B��BŢBǮBĜBɺB��B��BǮB��B�B��B��B��B��B��B��B��B��B�'B�XB�}BȴBɺB��B�B�BB�B�B�B��B�B��B��B��B	B	B	B	  B	B	B	B	B		7B	
=B	+B	B	B	bB	�B	�B	!�B	#�B	#�B	%�B	(�B	(�B	 �B	oB	bB	\B	B	B	+B	1B	JB	VB	PB	uB	�B	�B	 �B	�B	�B	!�B	&�B	>wB	v�B	��B	�RB	��B	�ZB	�B
JB
VB
PB
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B.B8RBL�BS�B]/BdZBiyBp�Bn�BgmBgmBiyBdZBaHBcTBbNB`BBbNBe`BXBR�BXBZBT�BO�BM�BM�BQ�BH�B:^B.B"�BuB	7B1B%B�B�fB�)B��B�3B�PBW
BC�B/B�B\BB
�B
��B
�B
��B
�DB
w�B
cTB
J�B
{B

=B	��B	�HB	��B	��B	x�B	n�B	I�B	7LB	,B	"�B	{B	JB		7B	B��B��B��B�B�NB�5B�;B�)B�#B��BŢB�}B�wB�XB�3B�-B�B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�\B�bB�oB��B��B��B��B��B��B�-B�9B�RB�RBƨB��B��B��B��BǮBɺBǮB��B��B��B��BƨB�-B�B��B��B��B��B��B��B��B�'B�^B�}BɺB��B��B�B�;B�B�B��B��B��B��B��B��B	B	B	B	B	B	%B	B	%B	DB	DB	
=B	%B	B	\B	�B	�B	"�B	$�B	$�B	'�B	)�B	,B	&�B	{B	{B	uB	%B	B	1B	1B	VB	\B	PB	uB	�B	�B	 �B	 �B	�B	"�B	&�B	>wB	v�B	��B	�RB	��B	�`B	��B
PB
VB
PB
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<u<D��<#�
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
<49X<#�
<#�
<#�
<49X<T��<�o<49X<#�
<#�
<#�
<#�
<49X<e`B<e`B<49X<#�
<49X<D��<#�
<�C�<#�
<#�
<49X<49X<u<e`B<49X<e`B<49X<#�
<49X<T��<#�
<#�
<49X<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181658402011121816584020111218165840  AO  ARGQ                                                                        20111130111555  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130111555  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165840  IP                  G�O�G�O�G�O�                