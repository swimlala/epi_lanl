CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   H   N_CALIB       	N_HISTORY             	   history       d2007-05-31T16:17:56Z creation; 2007-10-15T17:19:56Z updated; 2018-09-18T10:20:36Z converted from 2.2   title         Argo float vertical profile    institution       AOML   source        
Argo float     
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      
_FillValue               conventions       Argo reference table 1          6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       standard_name         time   axis      T      
resolution        >�E�vQ�        8   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        8   LATITUDE               	long_name         &Latitude of the station, best estimate     units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        standard_name         latitude   axis      Y           8   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        standard_name         	longitude      axis      X           8$   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8,   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    80   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    88   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8<   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8@   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z           8D   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  9d   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        9�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  :�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        ;   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        <4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  =T   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        =�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  >�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ?   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        @$   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  AD   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  H  B�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        B�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    DD   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    GD   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    JD   SCIENTIFIC_CALIB_DATE               	             
_FillValue               	long_name         Date of calibration    conventions       YYYYMMDDHHMISS        ,  MD   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    N�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    N�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    N�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    N�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  N�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    O$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    O4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    O8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         OH   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         OL   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        OP   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    OT   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    Mp   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        Np   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     Nt   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     N�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     N�Argo profile    3.1 1.2 19500101000000  5901274 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               #A   AO  20070531161756  20180918102151  1484_59057_035                  2C  D   846 @�z(3� 1   @�z�� @'u?�   �dd@   1   ARGOS   A   A   A   @���@�33A6ffAt��A�33A���A�  A�ffB��BffB0ffBD��BX��Bm33B�  B�33B���B�  B�ffB�  B�ffB�ffB�33B�  B�33B���B�  C�3C��CffC� C#L�C+L�C3��C>�CHL�CR� C]� Ch� Cs�fC�@ C�33C�33C�L�C�� C��C���C�@ C��3Cə�C�� Cޙ�C�33C��DFfD  D3DFfD�D&Y�D.� D7�fDAfDKfDU�fD`��Dl�Dx�D�VfD��D�  D�s3111111111111111111111111111111111111111111111111111111111111111111111111@�  @陙A1��Ap  A���A�fgAٙ�A�  B��B33B/33BC��BW��Bl  B�ffB���B�  B�ffB���B�ffB���B���Bϙ�B�ffB晙B�33B�ffCffCL�C�C33C#  C+  C3� C=��CH  CR33C]33Ch33Cs��C��C��C��C�&gC���C��gC�fgC��C���C�s4Cә�C�s4C��C��4D33D��D  D33DgD&FgD.��D7�3D@�3DJ�3DUs3D`�gDlgDw��D�L�D�3D�fD�i�111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�A�A�-A��A־wA֥�A֛�A֕�A֏\A֏\A֍PA֋DA֍PA֏\A֋DAև+A֋DA֍PA֍PAօA�z�A�x�A�XA�ffA��A���A�G�A��RA���A��Az1'A^��AT�yAIVA<�jA:1'A4��A2��A1��A0=qA+��A+G�A'��A%�mA"�yA bA
=A9XAI�A7LA%A-A
1'A1'A�Ap�@�r�@�+@�
=@�-@�t�@Гu@�hs@���@��/@��@��h@��@��!@��9@�
=@��111111111111111111111111111111111111111111111111111111111111111111111111A�p�A�A�A�-A��A־wA֥�A֛�A֕�A֏\A֏\A֍PA֋DA֍PA֏\A֋DAև+A֋DA֍PA֍PAօA�z�A�x�A�XA�ffA��A���A�G�A��RA���A��Az1'A^��AT�yAIVA<�jA:1'A4��A2��A1��A0=qA+��A+G�A'��A%�mA"�yA bA
=A9XAI�A7LA%A-A
1'A1'A�Ap�@�r�@�+@�
=@�-@�t�@Гu@�hs@���@��/@��@��h@��@��!@��9@�
=@��111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
R�B
P�B
Q�B
O�B
N�B
N�B
O�B
N�B
N�B
O�B
O�B
P�B
P�B
Q�B
P�B
P�B
P�B
Q�B
S�B
S�B
T�B
VB
dZB
��B
�-B
uB	�B	��B	��B	�-B	�hB	H�B	R�B	H�B	DB	7LB	�B	�3B	��B
O�B
Q�B
�uB
��B
��B
��B
��B
��B
�hB
�B
y�B
q�B
iyB
]/B
Q�B
K�B
F�B
=qB
2-B
.B
"�B
�B
�B
�B
�B
�B
 �B
!�B
%�B
,B
33B
7LB
=q111111111111111111111111111111111111111111111111111111111111111111111111B
SCB
Q#B
R$B
PbB
OB
O	B
PB
OB
N�B
PB
PB
QB
QB
RB
QB
QB
QB
RB
T!B
T&B
U&B
VTB
etB
��B
�IB
"7B	��B

�B	�!B	�wB	�FB	L/B	V�B	K�B	JB	8�B	��B	��B	��B
P�B
RCB
�HB
�B
�pB
�ZB
�MB
�$B
�$B
��B
zUB
r4B
jB
]�B
R^B
L9B
G)B
>FB
2�B
.�B
#rB
 6B
%B
)B
B
.B
!LB
"0B
&TB
,dB
3�B
7�B
=�111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��$<y��<#�
<l�<#�
<#�
<#��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.3 dbar                                                                                                                                                                                                                        none                                                                                                                                                                                                                                                            CTM alpha = 0.0267 & tau = 18.6 s with error equal to the correction                                                                                                                                                                                            Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  200708311135212007083111352120070831113521  Primary sampling: discrete [discrete spot sampling by SBE-41 at reported pressures]                                                                                                                                                                                APEX                            2575                            101805                          AO  ARGQ                                                                        20070531161756  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20070531161756  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20070831113521  QC  PRES            @���D�s3@                  PM  ARSQCTM V1.1                                                                20070831113521  QC  PSAL            @���D�s3@                  PM  ARSQOWGUV1.0                                                                20180918102151  IP                  G�O�G�O�G�O�                