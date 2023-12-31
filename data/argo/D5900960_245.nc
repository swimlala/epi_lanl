CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:28:45Z UW 3.1 conversion   
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
_FillValue                    =x   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >x   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    Bt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ct   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Gp   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    Kl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ll   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    Ph   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Qh   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ud   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    Y`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ^\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  cX   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    c�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    f�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    i�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  l�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    l�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    l�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    l�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    l�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  l�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    m   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    m   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    m   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         m(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         m,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        m0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    m4Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130111128  20190523124446  1514_5041_245                   2C  D   APEX                            2041                            062805                          846 @�l��1   @�l��@1������c��Q�1   GPS     Primary sampling: mixed [deeper than nominal 500dbar: discrete; nominal 500dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� D�3D,�D"33D.��D:��DG��DS��D`33Dl� Dx� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @s33@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD�fD  D"&fD.� D:� DG� DS��D`  Dl�3Dx�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��
A��
A���A���A���A���A���A�ȴA�ȴA�ƨA���A�Aɺ^Aɴ9Aɴ9Aɩ�Aɗ�Aɉ7A�v�A�t�A�l�A�bNA�O�A�O�A�O�A�O�A�O�A�;dA�(�A��A���Aȏ\A�ffA�A�A��A��A��Aƙ�A�`BA��A�  A�"�A�  A�ĜA�C�A��A��`A���A���A���A� �A��`A���A�A�A���A��PA�VA�&�A�oA�%A���A��A�x�A� �A�t�A�O�A�A�G�A���A�p�A��PA���A��A�-A�I�A���A�bA�ZA��A���A�1'A�
=A���A�^5A�I�A�=qA�l�A�&�A��;A|  Aw�#At�\ArĜAn��AkO�Ai�FAe�A^Q�AY\)ASƨAO�PAKoAH~�AG�ADffA>ZA=K�A;�PA:�\A:A�A9��A5�A25?A0��A/�#A/`BA.��A. �A-\)A,I�A+VA)�hA)&�A(bNA&��A&z�A&�A%�A$��A#�A#��A#oA"�uA"JA!�FA!`BA!/A �`A ��A A�A {At�A��A=qA�TA��AG�A�A�A�A^5A�mA��A��A/A�A�DA�A�wA��A��AjA�7A�jA��A?}A�jA  A�^A
ȴA	�;A	��A	`BA�AK�A�AbA�A J@��\@�E�@���@��9@���@��@�~�@�=q@�`B@�ƨ@�1@��`@�F@�=q@��`@��@�\@�@�@��@�^5@�j@�+@�$�@�O�@�9@�ȴ@ܬ@��@؛�@�C�@��@���@�/@�b@ӥ�@�|�@�;d@�o@��@�=q@��@�5?@���@�&�@��@ˮ@��@ʗ�@�M�@�x�@ț�@� �@�  @ǝ�@��@��y@ƸR@�^5@�{@���@ź^@��T@�Q�@��;@�G�@�$�@��@���@���@�x�@���@�E�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��
A��
A���A���A���A���A���A�ȴA�ȴA�ƨA���A�Aɺ^Aɴ9Aɴ9Aɩ�Aɗ�Aɉ7A�v�A�t�A�l�A�bNA�O�A�O�A�O�A�O�A�O�A�;dA�(�A��A���Aȏ\A�ffA�A�A��A��A��Aƙ�A�`BA��A�  A�"�A�  A�ĜA�C�A��A��`A���A���A���A� �A��`A���A�A�A���A��PA�VA�&�A�oA�%A���A��A�x�A� �A�t�A�O�A�A�G�A���A�p�A��PA���A��A�-A�I�A���A�bA�ZA��A���A�1'A�
=A���A�^5A�I�A�=qA�l�A�&�A��;A|  Aw�#At�\ArĜAn��AkO�Ai�FAe�A^Q�AY\)ASƨAO�PAKoAH~�AG�ADffA>ZA=K�A;�PA:�\A:A�A9��A5�A25?A0��A/�#A/`BA.��A. �A-\)A,I�A+VA)�hA)&�A(bNA&��A&z�A&�A%�A$��A#�A#��A#oA"�uA"JA!�FA!`BA!/A �`A ��A A�A {At�A��A=qA�TA��AG�A�A�A�A^5A�mA��A��A/A�A�DA�A�wA��A��AjA�7A�jA��A?}A�jA  A�^A
ȴA	�;A	��A	`BA�AK�A�AbA�A J@��\@�E�@���@��9@���@��@�~�@�=q@�`B@�ƨ@�1@��`@�F@�=q@��`@��@�\@�@�@��@�^5@�j@�+@�$�@�O�@�9@�ȴ@ܬ@��@؛�@�C�@��@���@�/@�b@ӥ�@�|�@�;d@�o@��@�=q@��@�5?@���@�&�@��@ˮ@��@ʗ�@�M�@�x�@ț�@� �@�  @ǝ�@��@��y@ƸR@�^5@�{@���@ź^@��T@�Q�@��;@�G�@�$�@��@���@���@�x�@���@�E�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB~�B}�B|�B|�B}�B� B~�B� B� B�B�B�B�B�B�%B�1B�1B�=B�bB��B��B��B��B�'B��B��BBÖBĜB�ZBBE�B_;B��B�LBȴB��B�BB��BBBB��B�NB�9B��B��B��B�DB2-B�BƨB��BXB+B<jB!�B&�BA�BH�BK�BK�BJ�BI�BG�BL�BjB�7B�7B�Bw�BdZBO�BN�BM�BG�B;dB,B!�B�BoB
=B
��B
�fB
��B
�9B
{�B
YB
D�B
oB	�B	�LB	��B	�B	t�B	XB	D�B	7LB	�B��B�fB��B�?B�B�B��B��B��B�oB�{B�uB�hB�JB�JB�1B�+B�+B�%B�%B�%B�B�B�B�B�B�B�+B�1B�7B�=B�hB�BBÖBB��B��BBÖBÖBƨB�B�BB�mB�B��B��B��B	  B	  B	B	%B	oB	�B	�B	�B	bB	1B	B��B�B�ZB�;B�/B�;B�BB�BB�HB�HB�ZB�mB�B�yB�fB�ZB�;B�B��B��B�wB�?B�'B�3B�3B�RBB��B��B��B��BɺBÖB��B�hB��B��B��B�!B�RBƨB��B��B��B��B��B��B�;B�BB�NB�NB�;B�5B�TB�mB�B�B�B�B�B�B�B�sB�B�B�B�B��B��B��B��B��B	  B��B��B��B��B	  B��B��B��B��B��B��B	B	\B	\)B	�\B	�B	ȴB	�B	�B	��B
B
DB
\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B~�B}�B|�B|�B}�B� B~�B� B� B�B�B�B�B�B�%B�1B�1B�=B�bB��B��B��B��B�-B��B��BBÖBĜB�ZBBE�B`BB��B�RBȴB��B�TB��BBB	7B��B�fB�FB��B��B�B��B>wB��B��B��B]/B.BG�B'�B-BB�BI�BK�BK�BK�BK�BN�BT�Bp�B�DB�PB�1B}�BiyBS�BQ�BO�BL�BB�B0!B$�B�B{BPB  B
�B
��B
�wB
�B
\)B
O�B
�B
B	��B	��B	�+B	{�B	^5B	G�B	=qB	&�B	%B�B�
B��B�9B�!B�B��B��B��B��B�{B�{B��B��B�JB�DB�1B�1B�1B�7B�+B�+B�+B�B�B�B�1B�7B�=B�JB�oB�BÖBŢBÖB��BBBĜBĜBǮB�B�NB�B��B��B��B	  B	B	B	B	+B	uB	�B	�B	�B	�B	JB	+B��B��B�mB�HB�BB�NB�TB�NB�TB�ZB�`B�B�B�B�sB�`B�ZB�;B�BÖBÖB�RB�-B�9B�?B�XBÖB��B��B��B��B��B��B��B�uB��B��B��B�!B�FBƨB�
B��B��B��B��B��B�NB�TB�TB�mB�HB�;B�TB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	B��B��B��B	  B	  B��B��B��B��B��B��B	%B	\B	\)B	�\B	�B	ȴB	�B	�B	��B
B
DB
\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<D��<�9X<e`B<D��<49X<T��<#�
<#�
<e`B<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<49X<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<49X<#�
<u<T��<���<T��<49X<#�
<49X<49X<#�
<49X<u<T��<e`B<T��<e`B<49X<#�
<e`B<���<#�
<#�
<#�
<#�
<#�
<u<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181658292011121816582920111218165829  AO  ARGQ                                                                        20111130111128  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130111128  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165829  IP                  G�O�G�O�G�O�                