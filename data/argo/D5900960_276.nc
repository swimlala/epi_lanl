CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:28:50Z UW 3.1 conversion   
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
resolution        =���   axis      Z        T  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  =�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  @�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  A�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  E(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  H|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  IT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  L�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  M�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  P�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  T(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  U    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  XT   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  Y,   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  \�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    \�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    _�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    b�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  e�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    e�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    e�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    e�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    e�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  e�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    f,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    f<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    f@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         fP   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         fT   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        fX   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    f\Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL              A   AO  20111130111509  20190523124446  1514_5041_276                   2C  D   APEX                            2041                            062805                          846 @բ�^Р
1   @բ�^Р
@1m�����cw�E���1   GPS     Primary sampling: mixed [deeper than nominal 500dbar: discrete; nominal 500dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   C� C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�33DٚD� D!�3D.Y�D:��DG�3DS�3D`�fDl��Dx� D���D�6fD�9�D���D�	�D�@ D�vfD���D��D�9�D�c3Dǩ�D���D�33D�\�D��fD�fD�6fD�s3D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   C  C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3D��D` D!�3D.9�D:��DGs3DS�3D`ffDlٚDx� D���D�&fD�)�D���D���D�0 D�ffD���D�	�D�)�D�S3DǙ�D��D�#3D�L�D��fD��fD�&fD�c3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�dZA��`A�l�A���A��A���A�O�A��^A��`A��TA�M�A���A���A�A�;dA��#A�A�A���A�l�A���A���A�ȴA���A�+A�  A���A��A��A�r�A���A���A��^A�(�A�`BA���A��^A��mA�VA}�AwO�AsG�Ao�^Ak/AhĜAf�AfVAdbAa�#A^�/AZ��AX$�AVffAS�TAQ�wAOK�AL�`AI��AE��ABn�AA
=A?��A?��A?
=A=S�A;t�A:�`A:{A8�yA8(�A6��A5`BA5&�A4�jA2r�A/�A-
=A,A+��A+�A*ffA(�A'C�A%|�A$��A%G�A%33A$(�A"�A!+A�TA+A�/A�A��A��A%AQ�A$�A�A�Az�A��A�A+An�AA�A(�A&�At�A�A��A�
A�RA?}A�yA�AXAM�AdZA	�A�;A�FAl�A9XA7LA�A�A �`A7LA Q�@�;d@��^@��9@��@�v�@���@�X@��@�+@�o@���@��@�P@�;d@��y@�ȴ@��@�@�K�@�j@��@�+@�R@�+@�$�@��^@�V@�dZ@�ȴ@�R@ꗍ@�V@�x�@�V@�A�@��m@�l�@��@�M�@�7@�D@㕁@�\)@�
=@���@�
=@�G�@��y@�o@�\)@�E�@ЋD@Ǿw@�+@�1@��@��#@�33@��@��@�\)@�V@��j@��@xr�@r�@j-@aX@Vv�@K�m@EV@?��@;��@4��@/K�@)x�@#��@�-@�w@-@+@dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�dZA��`A�l�A���A��A���A�O�A��^A��`A��TA�M�A���A���A�A�;dA��#A�A�A���A�l�A���A���A�ȴA���A�+A�  A���A��A��A�r�A���A���A��^A�(�A�`BA���A��^A��mA�VA}�AwO�AsG�Ao�^Ak/AhĜAf�AfVAdbAa�#A^�/AZ��AX$�AVffAS�TAQ�wAOK�AL�`AI��AE��ABn�AA
=A?��A?��A?
=A=S�A;t�A:�`A:{A8�yA8(�A6��A5`BA5&�A4�jA2r�A/�A-
=A,A+��A+�A*ffA(�A'C�A%|�A$��A%G�A%33A$(�A"�A!+A�TA+A�/A�A��A��A%AQ�A$�A�A�Az�A��A�A+An�AA�A(�A&�At�A�A��A�
A�RA?}A�yA�AXAM�AdZA	�A�;A�FAl�A9XA7LA�A�A �`A7LA Q�@�;d@��^@��9@��@�v�@���@�X@��@�+@�o@���@��@�P@�;d@��y@�ȴ@��@�@�K�@�j@��@�+@�R@�+@�$�@��^@�V@�dZ@�ȴ@�R@ꗍ@�V@�x�@�V@�A�@��m@�l�@��@�M�@�7@�D@㕁@�\)@�
=@���@�
=@�G�@��y@�o@�\)@�E�@ЋD@Ǿw@�+@�1@��@��#@�33@��@��@�\)@�V@��j@��@xr�@r�@j-@aX@Vv�@K�m@EV@?��@;��@4��@/K�@)x�@#��@�-@�w@-@+@dZ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�mB��B�jB�oBdZBO�BE�BK�BI�BYBN�BM�BJ�B@�B:^B33B/B/BC�BYBG�B9XB2-BJB
�B
�B
�B
�dB
��B
�=B
{�B
cTB
S�B
;dB
2-B
�B
�B	��B	ƨB	�B	�1B	k�B	]/B	N�B	H�B	9XB	(�B	hB	B��B��B��B��B�B�5B��BÖB�LB�9B�jB�jB�dB�}BƨBƨBŢBŢBŢBÖBŢBĜB��B�jBBB�wB�wB�^B�RB�XB�FB�?B�XB��B�B�)B�BB�HB�`B�B�B�HB�B�)B�)B�/B�HB�NB�NB�HB�5B�B��B��B��B�#B�mB�B	PB	uB	JB	B��B�B�fB��B��B��B�yB�`B�B	
=B		7B	hB	�B��B��B	�B	uB	hB	DB	1B	B	B	B��B��B��B	B	  B	+B		7B		7B	JB	hB	"�B	,B	,B	%�B	%�B	/B	1'B	5?B	5?B	6FB	7LB	@�B	E�B	F�B	G�B	G�B	L�B	M�B	Q�B	T�B	VB	VB	YB	[#B	ZB	T�B	S�B	S�B	bNB	ffB	iyB	bNB	XB	H�B	=qB	=qB	q�B	��B	�'B	ŢB	��B	�HB	�B	��B
%B
\B
�B
�B
#�B
'�B
1'B
:^B
>wB
I�B
P�B
VB
XB
]/B
aHB
ffB
k�B
p�B
w�B
|�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B��B��BhsBW
BH�BN�BK�B\)BN�BP�BM�BA�B=qB6FB0!B2-BG�B_;BK�B=qB<jBoB
��B
�B
�;B
ÖB
��B
�VB
�B
gmB
\)B
@�B
6FB
#�B
�B
1B	��B	�9B	�oB	p�B	aHB	O�B	M�B	=qB	/B	�B	1B	B	B	  B��B�B�fB�B��B�dB�RB�qB�wB��BĜBȴBɺBɺBȴBɺBǮBƨBŢBŢBÖBƨBĜB�}B�}B�qB�jB�qB�^B�RB�RB��B�#B�;B�ZB�ZB�mB�B�B�`B�B�5B�5B�5B�TB�ZB�TB�TB�HB�;B��B��B��B�5B�fB�B	PB	�B	\B	1B	B�B�mB��B��B��B�B�fB�B	JB	DB	{B	�B	  B��B	�B	�B	uB	PB		7B	%B	B	B��B��B��B	B	B	1B	
=B	
=B	JB	hB	!�B	-B	0!B	&�B	'�B	/B	2-B	6FB	6FB	7LB	9XB	A�B	E�B	F�B	H�B	I�B	M�B	N�B	R�B	VB	W
B	W
B	ZB	\)B	[#B	VB	T�B	S�B	dZB	hsB	l�B	gmB	]/B	I�B	>wB	>wB	q�B	��B	�'B	ŢB	��B	�HB	�B	��B
%B
\B
�B
�B
#�B
(�B
1'B
:^B
?}B
J�B
P�B
VB
XB
]/B
aHB
ffB
k�B
p�B
w�B
|�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<49X<#�
<#�
<#�
<D��<#�
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
<T��<49X<#�
<#�
<D��<D��<#�
<#�
<49X<#�
<D��<#�
<#�
<#�
<D��<�C�<T��<D��<T��<#�
<#�
<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<D��<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181658382011121816583820111218165838  AO  ARGQ                                                                        20111130111509  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130111509  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165838  IP                  G�O�G�O�G�O�                