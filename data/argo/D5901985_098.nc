CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   r   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2014-02-08T00:58:49Z creation;2015-04-24T01:37:44Z conversion to V3.1;2016-11-17T02:42:00Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ;d   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ;�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  =�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  >   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  ?�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  @P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  B   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  B�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  DT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  D�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  t  F�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  G   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  H�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  J�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  L\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    L�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    R�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    X�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ^�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    _   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    _   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    _   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    _   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  _    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    _`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    _p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         _�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         _�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        _�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20140208005849  20161129234514  5901985 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               bA   JA  P7_97922_098                    2C  D   PROVOR                          09027                           5815A03                         841 @��;�䱀1   @��?M� @5���v��cܓt�j1   ARGOS   A   A   B   Primary sampling: discrete [SBE41CP pumping for 19 seconds per measurements]                                                                                                                                                                                       ?   @�33A!��Ac33A�  A�ffA���B33B"  B1��BF��B\ffBm33B�ffB�33B���B���B�ffB���B���B�ffB�ffB���B�ffB�  B�  C33C�C��CL�C�fC�3C L�C%��C*� C/�fC4�3C9ffC>��CC�fCHL�CRffC\L�Cf��CpL�Cz33C��C���C�Y�C�ffC�Y�C�� C�� C��C�ffC��C��C�33C��CÙ�C�ffC�ffC�ffC�s3C�ffC�33C��fC��C�L�C�33C�ٚD  D��D9�D,�D�D�3D�3D%  D*,�D/3D49�D933D>fDC3DH,�DM  DR,�DW,�D\&fDa@ Df  DkfDo�3Du,�Dz33D�L�D�|�D�� D�3D�P D�� D��fD�fD�\�D���D�� D�  D�C3DԜ�D���D�fD�@ D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?��@�ffA#33Ad��A���A�33A�B��B"ffB2  BG33B\��Bm��B���B�ffB���B�  B���B�  B�  Bș�Bҙ�B�  B♚B�33B�33CL�C33C�fCffC  C��C ffC%�fC*��C0  C4��C9� C>�3CD  CHffCR� C\ffCf�3CpffCzL�C�&fC�ٚC�ffC�s3C�ffC���C���C��C�s3C�&fC��C�@ C��CæfC�s3C�s3C�s3C׀ C�s3C�@ C��3C�&fC�Y�C�@ C��fDfD�3D@ D33D3D��D��D%fD*33D/�D4@ D99�D>�DC�DH33DM&fDR33DW33D\,�DaFfDf&fDk�Do��Du33Dz9�D�P D�� D��3D�fD�S3D��3D�ɚD��D�` D���D��3D�#3D�FfDԠ D�� D��D�C3D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A��
A���A��RA��-A��9A��!A��A���A��+A�ffA�7LA�1'A�bA��A���A�|�A��A��A�C�A�5?A�-A��mA�oA��A�1'A�Q�A��A��jA�%A��A��PA�VA�{A��A���A�bNA��`A��A���A�C�A���A���A���A~��AydZAk�Ab~�AP�AJ=qAC33A=t�A5�mA2~�A*��A&~�A%%A �!AG�A��An�A��A��A�A�PA��A�`@�r�@��;@�K�@��y@���@�z�@϶F@�7L@�9X@�"�@���@���@�M�@�o@��R@�bN@��m@���@�=q@�`B@��P@�O�@�V@���@�dZ@�{@�Z@�&�@��@��@{C�@rn�@a��@\z�@TZ@D�D@;C�@4�@4(�@-�-@(�9@ Ĝ@�#@��@��@1'@
�211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A��
A���A��RA��-A��9A��!A��A���A��+A�ffA�7LA�1'A�bA��A���A�|�A��A��A�C�A�5?A�-A��mA�oA��A�1'A�Q�A��A��jA�%A��A��PA�VA�{A��A���A�bNA��`A��A���A�C�A���A���A���A~��AydZAk�Ab~�AP�AJ=qAC33A=t�A5�mA2~�A*��A&~�A%%A �!AG�A��An�A��A��A�A�PA��A�`@�r�@��;@�K�@��y@���@�z�@϶F@�7L@�9X@�"�@���@���@�M�@�o@��R@�bN@��m@���@�=q@�`B@��P@�O�@�V@���@�dZ@�{@�Z@�&�@��@��@{C�@rn�@a��@\z�@TZ@D�D@;C�@4�@4(�@-�-@(�9@ Ĝ@�#@��@��@1'@
�211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B^5B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BB�LB�-B�-B�BffB_;BQ�BM�BL�B7LB!�BJBBB��B�fB��B�RB�VB�BffB.BuB�B
��B
e`B
H�B
�B	�^B	|�B	�B	B�sB�
BǮB�dB�B��B��B�PB�1B� Bv�Bn�BbNB`BB`BBcTBgmBx�B�B�7B��B�=B�7B��BȴB�B��BB�ZB�ZB	{B	[#B	}�B	�B	�bB	��B	��B	�B	�FB	�wB	��B	�B	�BB	�mB	�B	��B	��B
B
bB
�B
'�B
33B
A�B
J�B
N�B
P�B
YB
^5B
e`B
m�B
n�B
q�B
v�B
}�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BB�LB�-B�-B�BffB_;BQ�BM�BL�B72B!�B0BB �B��B�fB��B�RB�VB�BffB.B[B�B
��B
e`B
H�B
�B	�^B	|�B	�B	B�XB�
BǔB�dB�B��B�sB�PB�1B�Bv�Bn}BbNB`BB`BBcTBgRBx�B�B�7B��B�#B�B��BȴB�B��B�uB�ZB�ZB	{B	[	B	}�B	�B	�HB	��B	��B	�B	�+B	�wB	οB	��B	�'B	�mB	�B	��B	��B
�B
HB
�B
'�B
33B
A�B
J�B
N�B
P�B
YB
^5B
e`B
m�B
n�B
q�B
v�B
}�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-PO1+PO2,  where PO1 is PRESSURE OFFSET from this cycle and PO2 is PRESSURE OFFSET from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    PO1=0.0(dbar); PO2=0.1(dbar)                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported PRESSURE OFFSET                                                                                                                                                                                                              None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201402210018052014022100180520140221001805201608161405402016081614054020160816140540JA  ARFMdecpP7_h                                                                20140208005830  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20140208005849  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20140208005851  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20140208005855  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20140208005856  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20140208005856  QCP$                G�O�G�O�G�O�               0JA  ARGQpump1.0                                                                 20140208005856  CF  PSAL            ?   ?   ?�                  JA  ARGQpump1.0                                                                 20140208005856  CF  TEMP            ?   ?   ?�                  JA  ARUP                                                                        20140208012238                      G�O�G�O�G�O�                JA  ARFMdecpP7_h                                                                20140210185701  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.6                                                                 20140210190533  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_b                                                                20140210190534  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19c                                                                20140210190539  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8d                                                                20140210190539  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16b                                                                20140210190540  QCP$                G�O�G�O�G�O�               0JA  ARGQpump1.0                                                                 20140210190540  CF  PSAL            ?   ?   ?�                  JA  ARGQpump1.0                                                                 20140210190540  CF  TEMP            ?   ?   ?�                  JA  ARUP                                                                        20140210192043                      G�O�G�O�G�O�                JA      jafc1.0                                                                 20150424013744                      G�O�G�O�G�O�                JA  ARUP                                                                        20150503004512                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20140220151805  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20140220151805  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2014V1                                                       20160816050540  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20161129234514                      G�O�G�O�G�O�                