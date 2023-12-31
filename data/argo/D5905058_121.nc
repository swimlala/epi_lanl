CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-29T12:36:37Z creation;2019-01-29T12:36:40Z conversion to V3.1;2019-12-23T06:07:50Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۨ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20190129123637  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               yA   JA  I2_0675_121                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @أ��z�1   @أ�O���@7�ڹ�Y��c3MjO1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A!��AA��Aa��A�  A�33A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ӅA	A+\)AK\)Ak\)A��HA�{A�{A��HA��HA��HA��HA��HBp�B
p�Bp�Bp�B"p�B*p�B2p�B:p�BBp�BJp�BRp�BZp�Bbp�Bjp�Brp�Bzp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC �)C�)C�)C�)C�)C
��C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C ��C"�)C$�)C&�)C(�)C*�)C,�)C.�)C0�)C2�)C4�)C6�)C8�)C:�)C<�)C>�)C@�)CB�)CD�)CF�)CH�)CJ�)CL�)CN�)CP�)CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn�)Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�Z�C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�AHC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�ND '
D �
D'
D�
D'
D�
D'
D�
D'
D�
D-qD�
D'
D�
D'
D�
D'
D�
D	'
D	�
D
'
D
�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D �D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
D '
D �
D!'
D!�
D"'
D"�
D#'
D#�
D$'
D$�
D%'
D%�
D&'
D&�
D''
D'�
D('
D(�
D)'
D)�
D*'
D*�
D+'
D+�
D, �D,�
D-'
D-�
D.'
D.�
D/'
D/�
D0'
D0�
D1'
D1�
D2'
D2�
D3'
D3�
D4'
D4�
D5'
D5�
D6'
D6�
D7'
D7�
D8'
D8�
D9'
D9�
D:'
D:�
D;'
D;�
D<'
D<�
D='
D=�
D>'
D>�
D?'
D?�
D@'
D@�
DA'
DA�
DB'
DB�
DC'
DC�qDD'
DD�
DE'
DE�
DF'
DF�
DG'
DG�
DH'
DH�
DI'
DI�
DJ'
DJ�
DK'
DK�
DL'
DL�
DM'
DM�
DN'
DN�
DO'
DO�
DP'
DP�
DQ'
DQ�
DR'
DR�
DS'
DS�
DT'
DT�
DU'
DU�
DV'
DV�
DW'
DW�
DX'
DX�
DY'
DY�
DZ'
DZ�
D['
D[�
D\'
D\�
D]'
D]�
D^'
D^�
D_'
D_�
D`'
D`�
Da'
Da�
Db'
Db�
Dc'
Dc�
Dd'
Dd�
De'
De�
Df'
Df�
Dg'
Dg�
Dh'
Dh�
Di'
Di�
Dj'
Dj�
Dk'
Dk�
Dl'
Dl�
Dm'
Dm�
Dn'
Dn�
Do'
Do�
Dp'
Dp�
Dq'
Dq�
Dr'
Dr�
Ds'
Ds�
Dt'
Dt�
Du'
Du�
Dv'
Dv�
Dw'
Dw�
Dx'
Dx�
Dy'
Dy�
Dz'
Dz�
D{'
D{�
D|'
D|�
D}'
D}�
D~'
D~�
D'
D�
D��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D�D�ӅD��D�S�DÓ�D�ӅD��D�S�Dē�D�ӅD��D�S�Dœ�D�ӅD��D�S�DƓ�D�ӅD��D�S�DǓ�D�ӅD��D�S�Dȓ�D�ӅD��D�S�Dɓ�D�ӅD��D�S�Dʓ�D�ӅD��D�S�D˓�D�ӅD��D�S�D̓�D�ӅD��D�S�D͓�D�ӅD��D�S�DΓ�D�ӅD��D�S�Dϓ�D�ӅD��D�S�DГ�D�ӅD��D�S�Dѓ�D�ӅD��D�S�Dғ�D�ӅD��D�S�Dӓ�D�ӅD��D�S�Dԓ�D�ӅD��D�S�DՓ�D�ӅD��D�S�D֓�D�ӅD��D�S�Dז�D�ӅD��D�S�Dؓ�D�ӅD��D�S�Dٓ�D�ӅD��D�S�Dړ�D�ӅD��D�S�Dۓ�D�ӅD��D�S�Dܓ�D�ӅD��D�S�Dݓ�D�ӅD��D�S�Dޓ�D�ӅD��D�S�Dߓ�D�ӅD��D�S�D���D�ӅD��D�S�DᓅD�ӅD��D�S�DⓅD�ӅD��D�S�D㓅D�ָD��D�S�D䓅D�ӅD��D�S�D哅D�ӅD��D�S�D擅D�ӅD��D�S�D瓅D�ӅD��D�S�D蓅D�ӅD��D�S�D铅D�ӅD��D�S�D꓅D�ӅD��D�S�D듅D�ӅD��D�S�D쓅D�ӅD��D�S�D퓅D�ӅD��D�S�DD�ӅD��D�S�DD�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�V�D��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�K�A�M�A�O�A�Q�A�ZA�bNA�bNA�^5A�p�A�t�A�v�A�x�A�z�A�|�A�|�A�|�A�~�A�~�A��A��A�~�A�~�A��A��A��A��A��A��A��A��A��A��A��PA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��\A��PA�ZA�ZA� �A��A��A�n�A���A��+A��A��A��A���A���A�dZA�x�A��A��mA��DA�A�A�A�l�A�9XA��A� �A�33A��A��TA��9A��DA���A���A���A�S�A��mA���A�7LA���A���A�1'A��A���A� �A�
=A�"�A�M�A�O�A��`A���A�K�A�&�A�ZA�JA���A�jA��mA���A�&�A���A��A���A��yA�hsA��A��A�~�A�I�A���A��-A���A��A�(�A��TA��mA��A~��A|ȴAy�hAxĜAw��Au;dArĜAo�7Ak�Aj �Ai�Ah��AghsAe�Ac��Ab�9A`-A\Q�AZ��AZ  AYt�AX�uAYAWO�ATr�ASG�AQ�;ANffAM��AK�AH�/AFbNACl�AA��A@��A?C�A>E�A:��A8n�A8Q�A6��A5��A3�A2�RA17LA0M�A.��A-O�A,�A+\)A*ĜA*M�A)%A(9XA'��A(JA'��A'A&ffA%�hA%?}A$�HA$�uA$�A#
=A!"�A bNAA�A
=AhsA%A��AE�A7LA��A$�A/A�9A�A?}A��AS�AA33A�A��A�A��A/AI�A�A&�A
^5A	��A	C�A��A1AXA�HA�DA-A  A��A?}A�/AQ�A�AĜAjA �A33A I�@���@���@��T@� �@�x�@�
=@�@��@���@�-@��@�C�@�5?@���@띲@�Ĝ@�l�@�=q@���@��y@�J@�hs@��H@�@���@۶F@ڏ\@�`B@��T@ԣ�@�|�@�@�  @���@��@ʰ!@�7L@��@�{@�I�@��H@��@�&�@�l�@��@�v�@��@��^@��@��@�K�@�33@�~�@�{@�{@��@�G�@���@�33@���@�=q@�p�@�%@�z�@� �@��;@�@��+@�$�@�hs@�7L@���@��D@�A�@��
@���@�dZ@�ȴ@���@�$�@�`B@�V@��9@��u@�Q�@�ƨ@�o@��+@��@���@�O�@��u@���@�\)@�33@��H@�V@�J@��h@�?}@�r�@�b@��;@�t�@�@���@��@�@���@�X@�7L@���@��j@��@�1'@�b@��w@�\)@�o@�ȴ@��\@�M�@�^5@�E�@�O�@���@��@��w@�K�@�33@�@�
=@���@���@��@��h@�G�@��/@�9X@��@�l�@�K�@�33@�;d@�@�V@���@��7@�p�@�/@��@��D@�bN@�1'@�  @��
@��
@��
@���@��@��P@�S�@�"�@�"�@��y@��!@���@�K�@�t�@�l�@��+@�x�@��T@���@�V@��@���@�O�@���@�/@��@�`B@�G�@�V@��`@�Ĝ@��@�Q�@�I�@�b@��;@��;@��
@�\)@�;d@���@��@���@��+@��+@��+@��+@�~�@�ff@�-@���@��@��^@�`B@�7L@�?}@��`@��9@��u@�Z@�ƨ@�o@�ȴ@��!@���@���@�ff@�^5@�E�@�5?@��@��@��#@�@��@�%@���@���@��@�j@�Z@�9X@�b@�  @��@���@��@�S�@�;d@�o@�@���@��!@��\@�ff@�5?@���@��^@��h@�p�@�O�@�O�@�/@��@��@���@���@�bN@�9X@��@�1@��@��m@���@��@�|�@�\)@�33@�@���@�^5@�5?@�@��T@�@���@�hs@��@���@���@�Q�@�(�@�@K�@~�@~v�@~E�@~{@}��@}O�@}?}@|�@|�@|��@|Z@|9X@|(�@{��@{t�@{C�@{33@{o@{@z��@z�\@z�\@z^5@y��@y��@x��@x �@w�@v�y@vff@u@uO�@uO�@u/@t�j@t�D@tZ@s�
@s��@s33@r��@rM�@r�@q�#@q��@qX@q�@p��@p��@p�@p �@o\)@n�R@n5?@m�@m��@m�h@mp�@mp�@m/@lj@k�m@k�
@kƨ@k"�@j��@j��@jM�@i��@i��@i%@h��@hQ�@g��@g��@f�R@e�@e��@e?}@d�j@dj@c��@cƨ@cS�@c"�@co@b�H@b�\@bM�@a��@a�7@a7L@`�u@`1'@_�w@_;d@^�R@^v�@^V@^5?@]�T@]��@]p�@]`B@]�@\�j@\��@\(�@[�F@[�@[S�@Z�H@ZM�@Y��@X��@X�@XA�@W�;@W�P@V��@VE�@U�T@Up�@T��@TZ@T�@S�F@SdZ@S33@R�H@Rn�@R�@Q��@Q&�@P��@PA�@P  @O��@O|�@OK�@O�@N�y@Nff@N@M@Mp�@M?}@L�j@Lj@L9X@Kƨ@KdZ@Ko@J��@J��@J~�@JM�@JJ@I��@I7L@H�`@H�u@HA�@Hb@G��@G��@G\)@F��@FV@E�@E��@E�-@Ep�@E?}@EV@D��@Dz�@DZ@DI�@D9X@D1@C��@Ct�@CC�@Co@B��@B��@B�\@BM�@A�@A�^@A�7@Ahs@AG�@@��@@bN@@  @?�w@?K�@?�@>�@>ff@>V@>{@=@=�@=?}@<�/@<z�@<(�@;��@;ƨ@;t�@;33@;@:�H@:��@:�!@:^5@:�@9�@9x�@9G�@8��@8�9@8 �@7��@7��@7�w@7|�@7\)@7K�@7�@7
=@6��@6�@6�R@6�+@6V@6{@5��@5��@5O�@4�@4�@49X@3�m@3�
@3�F@3�@333@3@2��@2M�@2J@1��@1�@1��@1��@1x�@17L@0Ĝ@01'@/�w@/��@/;d@.��@.�y@.�R@.�+@.ff@.ff@.ff@.V@.@-��@-@-`B@-/@,��@,�@,�@,��@,��@,z�@,j@,�@+�
@+ƨ@+��@+�@+�@+t�@+dZ@+C�@+33@+33@+@*�!@*^5@*-@)��@)�#@)X@)G�@)7L@)&�@)%@(r�@(Q�@(A�@(b@(  @'�w@'�@'�P@'l�@'+@'
=@&��@&�@&�R@&��@&ff@&@%@%�-@%�@%V@$�@$�j@$�D@$I�@$(�@$(�@#�m@#�F@#��@#C�@#o@#@"�H@"�\@"=q@!�#@!X@ ��@ Ĝ@ ��@ bN@ 1'@�@��@�P@;d@ȴ@��@��@V@@@�h@�@�/@��@�@I�@(�@1@�m@�
@��@dZ@dZ@33@�@��@�\@M�@=q@�@�#@�@��@��@&�@%@�`@�9@�u@�@�@r�@Q�@1'@b@�;@��@��@��@��@��@��@K�@�y@�R@�+@V@5?@{@��@�@O�@�@��@�j@�D@�@�m@�F@��@�@dZ@C�@@�!@~�@n�@=q@��@�7@hs@G�@&�@%@��@��@r�@Q�@1'@1'@b@�@�w@\)@+@�y@�+@v�@V@{@{@@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�K�A�M�A�O�A�Q�A�ZA�bNA�bNA�^5A�p�A�t�A�v�A�x�A�z�A�|�A�|�A�|�A�~�A�~�A��A��A�~�A�~�A��A��A��A��A��A��A��A��A��A��A��PA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��\A��PA�ZA�ZA� �A��A��A�n�A���A��+A��A��A��A���A���A�dZA�x�A��A��mA��DA�A�A�A�l�A�9XA��A� �A�33A��A��TA��9A��DA���A���A���A�S�A��mA���A�7LA���A���A�1'A��A���A� �A�
=A�"�A�M�A�O�A��`A���A�K�A�&�A�ZA�JA���A�jA��mA���A�&�A���A��A���A��yA�hsA��A��A�~�A�I�A���A��-A���A��A�(�A��TA��mA��A~��A|ȴAy�hAxĜAw��Au;dArĜAo�7Ak�Aj �Ai�Ah��AghsAe�Ac��Ab�9A`-A\Q�AZ��AZ  AYt�AX�uAYAWO�ATr�ASG�AQ�;ANffAM��AK�AH�/AFbNACl�AA��A@��A?C�A>E�A:��A8n�A8Q�A6��A5��A3�A2�RA17LA0M�A.��A-O�A,�A+\)A*ĜA*M�A)%A(9XA'��A(JA'��A'A&ffA%�hA%?}A$�HA$�uA$�A#
=A!"�A bNAA�A
=AhsA%A��AE�A7LA��A$�A/A�9A�A?}A��AS�AA33A�A��A�A��A/AI�A�A&�A
^5A	��A	C�A��A1AXA�HA�DA-A  A��A?}A�/AQ�A�AĜAjA �A33A I�@���@���@��T@� �@�x�@�
=@�@��@���@�-@��@�C�@�5?@���@띲@�Ĝ@�l�@�=q@���@��y@�J@�hs@��H@�@���@۶F@ڏ\@�`B@��T@ԣ�@�|�@�@�  @���@��@ʰ!@�7L@��@�{@�I�@��H@��@�&�@�l�@��@�v�@��@��^@��@��@�K�@�33@�~�@�{@�{@��@�G�@���@�33@���@�=q@�p�@�%@�z�@� �@��;@�@��+@�$�@�hs@�7L@���@��D@�A�@��
@���@�dZ@�ȴ@���@�$�@�`B@�V@��9@��u@�Q�@�ƨ@�o@��+@��@���@�O�@��u@���@�\)@�33@��H@�V@�J@��h@�?}@�r�@�b@��;@�t�@�@���@��@�@���@�X@�7L@���@��j@��@�1'@�b@��w@�\)@�o@�ȴ@��\@�M�@�^5@�E�@�O�@���@��@��w@�K�@�33@�@�
=@���@���@��@��h@�G�@��/@�9X@��@�l�@�K�@�33@�;d@�@�V@���@��7@�p�@�/@��@��D@�bN@�1'@�  @��
@��
@��
@���@��@��P@�S�@�"�@�"�@��y@��!@���@�K�@�t�@�l�@��+@�x�@��T@���@�V@��@���@�O�@���@�/@��@�`B@�G�@�V@��`@�Ĝ@��@�Q�@�I�@�b@��;@��;@��
@�\)@�;d@���@��@���@��+@��+@��+@��+@�~�@�ff@�-@���@��@��^@�`B@�7L@�?}@��`@��9@��u@�Z@�ƨ@�o@�ȴ@��!@���@���@�ff@�^5@�E�@�5?@��@��@��#@�@��@�%@���@���@��@�j@�Z@�9X@�b@�  @��@���@��@�S�@�;d@�o@�@���@��!@��\@�ff@�5?@���@��^@��h@�p�@�O�@�O�@�/@��@��@���@���@�bN@�9X@��@�1@��@��m@���@��@�|�@�\)@�33@�@���@�^5@�5?@�@��T@�@���@�hs@��@���@���@�Q�@�(�@�@K�@~�@~v�@~E�@~{@}��@}O�@}?}@|�@|�@|��@|Z@|9X@|(�@{��@{t�@{C�@{33@{o@{@z��@z�\@z�\@z^5@y��@y��@x��@x �@w�@v�y@vff@u@uO�@uO�@u/@t�j@t�D@tZ@s�
@s��@s33@r��@rM�@r�@q�#@q��@qX@q�@p��@p��@p�@p �@o\)@n�R@n5?@m�@m��@m�h@mp�@mp�@m/@lj@k�m@k�
@kƨ@k"�@j��@j��@jM�@i��@i��@i%@h��@hQ�@g��@g��@f�R@e�@e��@e?}@d�j@dj@c��@cƨ@cS�@c"�@co@b�H@b�\@bM�@a��@a�7@a7L@`�u@`1'@_�w@_;d@^�R@^v�@^V@^5?@]�T@]��@]p�@]`B@]�@\�j@\��@\(�@[�F@[�@[S�@Z�H@ZM�@Y��@X��@X�@XA�@W�;@W�P@V��@VE�@U�T@Up�@T��@TZ@T�@S�F@SdZ@S33@R�H@Rn�@R�@Q��@Q&�@P��@PA�@P  @O��@O|�@OK�@O�@N�y@Nff@N@M@Mp�@M?}@L�j@Lj@L9X@Kƨ@KdZ@Ko@J��@J��@J~�@JM�@JJ@I��@I7L@H�`@H�u@HA�@Hb@G��@G��@G\)@F��@FV@E�@E��@E�-@Ep�@E?}@EV@D��@Dz�@DZ@DI�@D9X@D1@C��@Ct�@CC�@Co@B��@B��@B�\@BM�@A�@A�^@A�7@Ahs@AG�@@��@@bN@@  @?�w@?K�@?�@>�@>ff@>V@>{@=@=�@=?}@<�/@<z�@<(�@;��@;ƨ@;t�@;33@;@:�H@:��@:�!@:^5@:�@9�@9x�@9G�@8��@8�9@8 �@7��@7��@7�w@7|�@7\)@7K�@7�@7
=@6��@6�@6�R@6�+@6V@6{@5��@5��@5O�@4�@4�@49X@3�m@3�
@3�F@3�@333@3@2��@2M�@2J@1��@1�@1��@1��@1x�@17L@0Ĝ@01'@/�w@/��@/;d@.��@.�y@.�R@.�+@.ff@.ff@.ff@.V@.@-��@-@-`B@-/@,��@,�@,�@,��@,��@,z�@,j@,�@+�
@+ƨ@+��@+�@+�@+t�@+dZ@+C�@+33@+33@+@*�!@*^5@*-@)��@)�#@)X@)G�@)7L@)&�@)%@(r�@(Q�@(A�@(b@(  @'�w@'�@'�P@'l�@'+@'
=@&��@&�@&�R@&��@&ff@&@%@%�-@%�@%V@$�@$�j@$�D@$I�@$(�@$(�@#�m@#�F@#��@#C�@#o@#@"�H@"�\@"=q@!�#@!X@ ��@ Ĝ@ ��@ bN@ 1'@�@��@�P@;d@ȴ@��@��@V@@@�h@�@�/@��@�@I�@(�@1@�m@�
@��@dZ@dZ@33@�@��@�\@M�@=q@�@�#@�@��@��@&�@%@�`@�9@�u@�@�@r�@Q�@1'@b@�;@��@��@��@��@��@��@K�@�y@�R@�+@V@5?@{@��@�@O�@�@��@�j@�D@�@�m@�F@��@�@dZ@C�@@�!@~�@n�@=q@��@�7@hs@G�@&�@%@��@��@r�@Q�@1'@1'@b@�@�w@\)@+@�y@�+@v�@V@{@{@@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�RB�RB�RB�RB�RB�RB�LB�RB�RB�LB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�RB�LB�LB�LB�LB�LB�LB�LB�LB�LB�RB�RB�RB�RB�RB�RB�XB�XB�^B�^B�dBÖBB��B��B�B2-B?}BB�BO�BQ�BR�BM�BJ�BF�BF�BE�BF�BG�BE�BD�BE�BJ�BW
BaHBbNB^5B[#BZBXBR�BO�B@�BC�BB�B>wB:^B33B �B	7BB��B�B�B�yB�fB�
B�qB��B��B��B�\Bx�BiyBP�BJ�BE�B>wB-BhB
�B
�sB
�5B
��B
�RB
�B
��B
��B
�\B
|�B
m�B
cTB
R�B
G�B
:^B
0!B
�B
%B	��B	��B	�fB	�B	��B	��B	��B	�{B	�VB	�%B	w�B	gmB	^5B	G�B	&�B	�B	�B	�B	
=B	�B	�B	VB	hB	
=B�B��B�B�TB��BŢB�dB�?B�B��B��B�VB�PB�hB�\B�%B�B{�Bw�Br�Bk�BhsBdZBbNB`BB]/B\)B[#B^5B^5B^5BYBT�BR�BQ�BP�BN�BK�BL�BI�BE�BC�B@�B<jB9XB8RB6FB49B33B1'B0!B/B.B/B-B/B-B,B-B,B-B,B,B,B+B,B+B+B(�B)�B)�B)�B)�B)�B)�B)�B)�B)�B'�B'�B(�B'�B)�B(�B)�B%�B%�B%�B(�B(�B$�B&�B&�B(�B)�B)�B&�B'�B&�B+B33B+B&�B'�B'�B+B)�B-B+B,B,B,B,B-B2-B1'B2-B33B6FB8RB9XB<jB>wB@�BD�BJ�BL�BM�BP�BT�BXB[#B\)B\)B\)B^5B_;B_;BbNBcTBcTBbNBffBk�Bl�Bn�Bo�Br�Bs�Bt�Bu�Bv�By�B{�B|�B� B� B�B�B�B�%B�%B�1B�=B�DB�VB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�!B�'B�-B�?B�FB�XB�dBBɺB��B��B��B�B�#B�5B�BB�TB�`B�sB�B�B��B��B��B	  B	B	%B		7B	PB	{B	�B	�B	�B	�B	!�B	%�B	(�B	,B	-B	.B	1'B	49B	6FB	8RB	=qB	A�B	B�B	C�B	B�B	C�B	C�B	C�B	F�B	J�B	L�B	M�B	P�B	W
B	ZB	]/B	_;B	bNB	dZB	e`B	e`B	gmB	iyB	jB	l�B	m�B	o�B	r�B	u�B	y�B	�B	�%B	�DB	�DB	�1B	�VB	�bB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�?B	�FB	�FB	�RB	�RB	�XB	�^B	�dB	�jB	�}B	��B	��B	��B	B	B	ÖB	ĜB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�TB	�TB	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
JB
DB
JB
PB
VB
VB
VB
\B
\B
\B
\B
\B
bB
hB
hB
hB
oB
oB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�$B�$B�*B�*B�0B�aB�[B͟B��B�|B1�B?HBB[BO�BQ�BR�BM�BJ�BFtBFtBEmBFtBGzBEmBDgBEmBJ�BV�BaBbB]�BZ�BY�BW�BR�BO�B@OBCaBB[B>BB:*B2�B �B	B�B��B�|B�iB�DB�2BּB�<B��B�_B�MB�(Bx�BiDBP�BJ�BESB>BB,�B4B
�B
�>B
�B
ϫB
�B
��B
��B
�~B
�(B
|�B
mCB
c B
R�B
GzB
:B
/�B
dB
�B	��B	��B	�B	յB	�OB	��B	�YB	�FB	�B	��B	w�B	g8B	^B	GzB	&�B	]B	EB	SB	
	B	dB	EB	B	4B	
	B�OB�B�UB�B��B�SB�B��B��B��B�qB�B�B�B�B��B��B{�Bw�BraBk6Bh$BdBa�B_�B\�B[�BZ�B]�B]�B^BX�BT�BR�BQ�BP�BN�BK�BL�BIlBESBCGB@4B<B9	B8B5�B3�B2�B0�B/�B.�B-�B.�B,�B.�B,�B+�B,�B+�B,�B+�B+�B+�B*�B+�B*�B*�B(�B)�B)�B)�B)�B)�B)�B)�B)�B)�B'�B'�B(�B'�B)�B(�B)�B%�B%�B%�B(�B(�B$�B&�B&�B(�B)�B)�B&�B'�B&�B*�B2�B*�B&�B'�B'�B*�B)�B,�B*�B+�B+�B+�B+�B,�B1�B0�B1�B2�B5�B8B9	B<B>(B@4BDMBJrBL~BM�BP�BT�BW�BZ�B[�B[�B[�B]�B^�B^�Ba�BcBb�Ba�BfBk6Bl=BnIBoOBraBshBtnButBvzBy�B{�B|�B�B�B��B��B��B��B��B��B��B��B�B� B�&B�2B�9B�?B�WB�OB�vB��B��B��B��B��B��B��B��B��B��B�	B�B�AB�lBϑBЗBԯB��B��B��B��B�B�B�$B�6B�IB�nB�zB��B��B	�B	�B	�B	B	,B	?B	KB	WB	pB	!|B	%�B	(�B	+�B	,�B	-�B	0�B	3�B	5�B	8B	=B	A;B	BAB	CGB	B'B	CGB	CGB	CGB	F?B	JrB	L~B	M�B	P�B	V�B	Y�B	\�B	^�B	a�B	dB	eB	eB	gB	iB	j0B	l=B	mCB	oOB	raB	utB	y�B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�2B	�9B	�KB	�WB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�.B	�4B	�;B	�;B	�AB	�AB	�GB	�MB	�YB	�?B	�fB	�lB	�rB	�rB	�xB	�~B	ϑB	ѝB	ңB	өB	өB	өB	��B	ԯB	ԯB	յB	֡B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�$B	�*B	�*B	�*B	�*B	�0B	�6B	�=B	�=B	�=B	�=B	�CB	�CB	�CB	�IB	�IB	�OB	�OB	�[B	�[B	�aB	�GB	�|B	�hB	�hB	�hB	�hB	�hB	�hB	�tB	�tB	�zB	��B	�fB	��B	��B	��B	�rB	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B

�B

�B

�B
�B

�B
�B
B
�B
B
�B
B
B
B
B
B
B
B
B
B
B
 B
 B
&B
&B
B
,B
B
2B
2B
2B
B
YB
?B
EB
KB
1B
KB
KB
QB
QB
QB
QB
QB
7B
7B
WB
WB
]B
]B
dB
dB
jB
jB
jB
jB
jB
pB
pB
VB
pB
 vB
 \B
!|B
!|B
!|B
!|B
"hB
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$tB
%zB
%�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
*�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
/�B
/�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
2�B
2�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
8B
8B
8B
8B
8B
9	B
9	B
9	B
9	B
9	B
9	B
9	B
9�B
:B
:B
:B
:B
:B
:�B
;B
<B
<B
<B
<B
=B
=B
="B
="B
>(B
>(B
>B
>(B
?.B
?.B
?.B
@4B
@4B
@4B
@4B
@4B
@4B
@4B
A B
A;B
A;B
BAB
BAB
BAB
CGB
CGB
CGB
CGB
CGB
CGB
DMB
DMB
DMB
DMB
D3B
DMB
DMB
DMB
ESB
E9B
ESB
ESB
F?B
FYB
G_B
G_B
G_B
HfB
HfB
HfB
HfB
HfB
IlB
I�B
JrB
J�B
JrB
JrB
J�B
J�B
JrB
KxB
K^B
JrB
JrB
JrB
KxB
KxB
KxB
KxB
KxB
KxB
KxB
KxB
L~B
L~B
L�B
M�B
L~B
L~B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
_B
^�B
^�B
^�B
^�B
_B
^�B
^�B
^�B
`B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
a�B
a�B
cB
cB
cB
cB
cB
cB
dB
dB
dB
dB
d�B
eB
eB
eB
eB
fB
fB
fB
e�B
fB
gB
gB
gB
gB
gB
gB
h$B
h$B
h$B
h>B
h$B
i*B
i*B
i*B
i*B
i*B
i*B
i*B
j0B
j0B
jKB
jB
k6B
k6B
k6B
kQB
k6B
k6B
k6B
k6B
k61111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.61(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902040041062019020400410620190204004106201902050032202019020500322020190205003220JA  ARFMdecpA19c                                                                20190129213621  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190129123637  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190129123638  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190129123639  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190129123639  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190129123639  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190129123639  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190129123639  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190129123640  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190129123640                      G�O�G�O�G�O�                JA  ARUP                                                                        20190129125725                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190129153924  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20190129153924  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20190129153924  CV  LATITUDE        G�O�G�O�A�Z                JM  ARGQJMQC2.0                                                                 20190129153924  CV  LONGITUDE       G�O�G�O���d                JM  ARCAJMQC2.0                                                                 20190203154106  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190203154106  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190204153220  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                