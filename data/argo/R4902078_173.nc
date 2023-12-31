CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-01-21T10:00:41Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݌   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20200121100041  20200121100041  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @��׌� 1   @����B@-���
=q�d{\(�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ D�|�D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@ӅA	A)AIAiA��HA��HA��HA��HA��HA��HA��HA��HBp�B

>Bp�Bp�B"p�B*p�B2p�B:p�BBp�BJp�BRp�BZp�Bbp�Bjp�Brp�Bzp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC �)C�)C�)C�)C�)C
�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C �)C"�)C$�)C&�)C(�)C*�)C,�)C.�)C0�)C2��C4�)C6�)C8�)C:�)C<�)C>�)C@�)CB�)CD�)CF�)CH�)CJ�)CL�)CN�)CP�)CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn�)Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�Z�C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�Z�C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�ND '
D �
D'
D�
D'
D�
D'
D�
D'
D�
D'
D�
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
D'
D�
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
D,'
D,�
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
DC�
DD'
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
D��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D�D�ӅD��D�S�DÓ�D�ӅD��D�S�Dē�D�ӅD��D�S�Dœ�D�ӅD��D�S�DƓ�D�ӅD��D�S�DǓ�D�ӅD��D�S�Dȓ�D�ӅD��D�S�Dɓ�D�ӅD��D�S�Dʓ�D�ӅD��D�S�D˓�D�ӅD��D�S�D̓�D�ӅD��D�S�D͓�D�ӅD��D�S�DΓ�D�ӅD��D�S�Dϓ�D�ӅD��D�S�DГ�D�ӅD��D�S�Dѓ�D�ӅD��D�S�Dғ�D�ӅD��D�S�DӐRD�ӅD��D�S�Dԓ�D�ӅD��D�S�DՓ�D�ӅD��D�S�D֓�D�ӅD��D�S�Dד�D�ӅD��D�S�Dؓ�D�ӅD��D�S�Dٓ�D�ӅD��D�S�Dړ�D�ӅD��D�S�Dۓ�D�ӅD��D�S�Dܓ�D�ӅD��D�S�Dݓ�D�ӅD��D�S�Dޓ�D�ӅD��D�S�Dߓ�D�ӅD��D�S�D���D�ӅD��D�S�DᓅD�ӅD��D�S�DⓅD�ӅD��D�S�D㓅D�ӅD��D�S�D䓅D�ӅD��D�S�D哅D�ӅD��D�S�D擅D�ӅD��D�S�D瓅D�ӅD��D�S�D蓅D�ӅD��D�S�D铅D�ӅD��D�S�D꓅D�ӅD��D�S�D듅D�ӅD��D�S�D쓅D�ӅD��D�S�D퓅D�ӅD��D�S�DD�ӅD��D�S�DD�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ָD�&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AЬAЬAЩ�AЧ�AС�AС�AУ�AЩ�Aа!AЮAЬAа!Aа!AЮAЮAв-AиRAк^Aк^Aк^AмjA�ȴA���AмjAк^AХ�AЏ\A�z�A�jA�ffA�ffA�dZA�^5A�\)A�\)A�ZA�S�A�O�A�M�A�5?A�{A���A��A��AϾwAρAζFAʼjA�M�Aĕ�A��RA���A��wA��A�ĜA�|�A��A��A�S�A���A� �A�p�A��A��A�5?A���A�(�A��A���A��hA��A��A���A��A��/A��DA��HA��RA���A�=qA�ffA��uA���A�^A{�Aw��Ap��An�Ae�AaO�A]�AV�AU7LARZAJ�AEO�AD�DAC�AA&�A?��A??}A=�^A<ȴA<VA;�A6�yA6�RA8��A8VA7�FA7�A6��A6�A6�A5�A3�A3�A3\)A3C�A2r�A1�#A0�+A.bA-K�A-33A-
=A,�yA-+A,VA+\)A+
=A+�A*��A)��A)S�A(�/A'�A( �A(r�A(n�A'�#A'C�A%+A$1'A"��A"��A"JA!|�A �A�A/AA�AVAjAt�A�`A�\A�Al�A�/A  A�A�7A��A5?AA�TA��A�AS�A��A�9AA�AA��AS�A��A��A�hA
=A�AVA��A`BAS�A&�A�A�`AbA�Al�A�`A�DA^5A{A�AG�A�A��Av�A��A�A
jA	A	�PA	�hA	��A	hsA	XA	33A�jA1A��A�!A�A�wAS�A+A��A��A��A�jA�9Ar�AQ�AbNA9XA �A �A �A�A�-A��A�A��A��AG�A�A
=A �A �jA �uA z�A jA A�@��
@�v�@�/@��/@��D@��D@��
@�dZ@�V@�n�@�{@��/@��D@�S�@��R@�{@�V@�F@�M�@���@�7@���@��m@�S�@�o@��@�O�@�Ĝ@��@�\)@��@�n�@��#@�O�@��`@�bN@�(�@�1@�\)@��y@�v�@���@��#@���@噚@��@�A�@�b@�w@�\)@��@�V@�^@�@�j@�Q�@�I�@߅@�"�@ݺ^@�%@��`@�(�@�+@���@�n�@�M�@��@٩�@ى7@�X@��/@�(�@ׅ@���@ְ!@�ff@�-@��@թ�@Դ9@��@�~�@�=q@��@�{@���@���@�X@�Ĝ@�I�@ϥ�@�o@�@�G�@�V@���@̓u@�A�@��H@�V@�@ə�@��`@�I�@�b@�|�@�+@���@�~�@Ų-@ŉ7@�x�@�7L@��@�V@���@�A�@öF@�;d@�$�@���@���@��@��F@�
=@�~�@�-@�@�x�@�V@�bN@�9X@� �@�b@�C�@��@���@�-@�hs@�V@��9@�j@��@���@��w@��w@���@�l�@�S�@�+@�@�V@���@��@�Ĝ@�9X@��
@�|�@���@���@���@���@���@���@�ff@��@�z�@�1'@��@�\)@��y@�~�@�-@��T@��h@�/@��@�9X@���@�"�@�ȴ@�v�@��@��@���@�x�@�G�@�G�@�?}@��/@�z�@�Q�@��;@�|�@�dZ@�C�@�
=@�ȴ@���@�E�@�@��h@�x�@�hs@��@���@�b@��F@�|�@�t�@�l�@�C�@��H@�M�@��^@�`B@�7L@��@���@�(�@��@��
@���@�"�@���@���@���@���@�=q@��-@��7@�p�@�X@�?}@�&�@��`@�r�@�A�@� �@�  @��m@���@�|�@�C�@�o@���@���@�~�@�=q@�@�@�`B@���@�Ĝ@�j@��
@�dZ@�"�@��!@�n�@�-@�J@��^@��@�X@���@�9X@��w@�dZ@��@��@��\@��@���@�p�@�&�@��/@���@�r�@�bN@�A�@��F@�l�@��@��@��!@�n�@�J@���@�@��^@��-@�p�@���@��@�Q�@�1'@���@��F@�l�@�
=@��\@�~�@�n�@�V@�E�@�-@�@�x�@�?}@�Ĝ@�r�@��m@��@�K�@��@���@�E�@��@��@��#@���@��h@�`B@�G�@�%@��@�z�@�I�@�ƨ@��P@�t�@�dZ@�;d@��y@�v�@�=q@�-@�-@�$�@��@��#@��-@���@��7@�X@�G�@���@�Ĝ@�A�@�@~ȴ@~@}@}�-@}p�@}O�@}�@|�/@|��@|�j@|z�@{�@z^5@zM�@z=q@zJ@yx�@xQ�@w�w@w\)@v��@v��@v$�@u@t�@tI�@s�
@s�@sC�@r��@r�@qx�@p�`@pr�@pb@p  @o�w@o��@ol�@o
=@n�y@n��@n�+@n5?@m�-@mO�@l�/@l�@kdZ@ko@j��@j~�@j-@i&�@h�u@hQ�@g��@g|�@g;d@f�@f5?@e�@e�T@e`B@eV@d�/@d��@dj@dZ@d9X@d(�@d(�@d(�@d(�@c�F@c33@bn�@b=q@a�#@aX@a�@`�9@`r�@` �@_;d@^��@^5?@^{@]�@]��@]�h@]`B@]O�@\��@\�/@\�@\9X@[ƨ@[t�@[o@Z�!@Z^5@Y��@Y��@Yhs@Y7L@X�`@X��@X �@W�P@V�y@Vv�@U�T@U�h@T�@Tz�@S�
@SdZ@So@R�H@R~�@RJ@Q�7@Q&�@P�9@P �@P  @P  @P  @O�;@OK�@N�y@Nȴ@Nȴ@Nv�@M�T@M�@L��@L�D@L(�@L�@L�@L1@K�F@Kt�@Kt�@KS�@KC�@K33@Jn�@J�@I��@I�^@I��@Ihs@I%@H�@Hr�@HQ�@HA�@Hb@G�@G��@G�@G\)@G+@G�@F�y@F��@Fff@FV@F{@E�T@E��@E��@EO�@EV@D��@D9X@D�@D1@C�m@C�
@CS�@B��@B�!@BM�@Ax�@AG�@AG�@AG�@A&�@@�`@@��@@�@@bN@?�;@?;d@>��@>��@>V@>{@=@=�h@=p�@=?}@=/@=V@<��@<Z@;�m@;��@;t�@;dZ@;S�@;33@:��@:��@:^5@:M�@:-@:J@9�#@9�^@9��@9G�@9&�@9%@8��@8Ĝ@8r�@8Q�@8  @7�w@7|�@7\)@7�@6�@6�R@6�+@6{@5p�@4��@4�@4�/@4��@4�@4j@4�@3�
@3��@3"�@2��@2n�@2-@1�@1��@1X@17L@1�@0Ĝ@0Q�@/��@/K�@.�y@.ȴ@.��@.v�@.5?@.$�@.{@-��@-/@-V@,�@,�/@,��@,�j@,z�@,�@+�m@+ƨ@+�F@+��@+��@+@*-@*J@*J@)��@)�@)��@)�^@)�^@)��@)��@)��@)�7@)G�@(bN@'�;@'�@'|�@'\)@'
=@&�+@&V@&V@&5?@%@%�h@%�@%�@%p�@%`B@$��@#�m@#��@#t�@#dZ@#C�@#"�@#@"��@"M�@"=q@"-@"�@"J@!�#@!�7@ ��@ �@  �@�;@\)@K�@+@+@
=@�+@v�@ff@5?@@�@�T@�-@�@/@��@�/@��@�@j@(�@�@��@�
@ƨ@ƨ@�F@��@�@t�@t�@S�@33@o@�H@�\@n�@=q@J@��@��@�7@x�@7L@%@�`@�9@��@�u@r�@bN@A�@ �@  @��@�w@|�@K�@K�@;d@�@ȴ@ff@$�@@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AЬAЬAЩ�AЧ�AС�AС�AУ�AЩ�Aа!AЮAЬAа!Aа!AЮAЮAв-AиRAк^Aк^Aк^AмjA�ȴA���AмjAк^AХ�AЏ\A�z�A�jA�ffA�ffA�dZA�^5A�\)A�\)A�ZA�S�A�O�A�M�A�5?A�{A���A��A��AϾwAρAζFAʼjA�M�Aĕ�A��RA���A��wA��A�ĜA�|�A��A��A�S�A���A� �A�p�A��A��A�5?A���A�(�A��A���A��hA��A��A���A��A��/A��DA��HA��RA���A�=qA�ffA��uA���A�^A{�Aw��Ap��An�Ae�AaO�A]�AV�AU7LARZAJ�AEO�AD�DAC�AA&�A?��A??}A=�^A<ȴA<VA;�A6�yA6�RA8��A8VA7�FA7�A6��A6�A6�A5�A3�A3�A3\)A3C�A2r�A1�#A0�+A.bA-K�A-33A-
=A,�yA-+A,VA+\)A+
=A+�A*��A)��A)S�A(�/A'�A( �A(r�A(n�A'�#A'C�A%+A$1'A"��A"��A"JA!|�A �A�A/AA�AVAjAt�A�`A�\A�Al�A�/A  A�A�7A��A5?AA�TA��A�AS�A��A�9AA�AA��AS�A��A��A�hA
=A�AVA��A`BAS�A&�A�A�`AbA�Al�A�`A�DA^5A{A�AG�A�A��Av�A��A�A
jA	A	�PA	�hA	��A	hsA	XA	33A�jA1A��A�!A�A�wAS�A+A��A��A��A�jA�9Ar�AQ�AbNA9XA �A �A �A�A�-A��A�A��A��AG�A�A
=A �A �jA �uA z�A jA A�@��
@�v�@�/@��/@��D@��D@��
@�dZ@�V@�n�@�{@��/@��D@�S�@��R@�{@�V@�F@�M�@���@�7@���@��m@�S�@�o@��@�O�@�Ĝ@��@�\)@��@�n�@��#@�O�@��`@�bN@�(�@�1@�\)@��y@�v�@���@��#@���@噚@��@�A�@�b@�w@�\)@��@�V@�^@�@�j@�Q�@�I�@߅@�"�@ݺ^@�%@��`@�(�@�+@���@�n�@�M�@��@٩�@ى7@�X@��/@�(�@ׅ@���@ְ!@�ff@�-@��@թ�@Դ9@��@�~�@�=q@��@�{@���@���@�X@�Ĝ@�I�@ϥ�@�o@�@�G�@�V@���@̓u@�A�@��H@�V@�@ə�@��`@�I�@�b@�|�@�+@���@�~�@Ų-@ŉ7@�x�@�7L@��@�V@���@�A�@öF@�;d@�$�@���@���@��@��F@�
=@�~�@�-@�@�x�@�V@�bN@�9X@� �@�b@�C�@��@���@�-@�hs@�V@��9@�j@��@���@��w@��w@���@�l�@�S�@�+@�@�V@���@��@�Ĝ@�9X@��
@�|�@���@���@���@���@���@���@�ff@��@�z�@�1'@��@�\)@��y@�~�@�-@��T@��h@�/@��@�9X@���@�"�@�ȴ@�v�@��@��@���@�x�@�G�@�G�@�?}@��/@�z�@�Q�@��;@�|�@�dZ@�C�@�
=@�ȴ@���@�E�@�@��h@�x�@�hs@��@���@�b@��F@�|�@�t�@�l�@�C�@��H@�M�@��^@�`B@�7L@��@���@�(�@��@��
@���@�"�@���@���@���@���@�=q@��-@��7@�p�@�X@�?}@�&�@��`@�r�@�A�@� �@�  @��m@���@�|�@�C�@�o@���@���@�~�@�=q@�@�@�`B@���@�Ĝ@�j@��
@�dZ@�"�@��!@�n�@�-@�J@��^@��@�X@���@�9X@��w@�dZ@��@��@��\@��@���@�p�@�&�@��/@���@�r�@�bN@�A�@��F@�l�@��@��@��!@�n�@�J@���@�@��^@��-@�p�@���@��@�Q�@�1'@���@��F@�l�@�
=@��\@�~�@�n�@�V@�E�@�-@�@�x�@�?}@�Ĝ@�r�@��m@��@�K�@��@���@�E�@��@��@��#@���@��h@�`B@�G�@�%@��@�z�@�I�@�ƨ@��P@�t�@�dZ@�;d@��y@�v�@�=q@�-@�-@�$�@��@��#@��-@���@��7@�X@�G�@���@�Ĝ@�A�@�@~ȴ@~@}@}�-@}p�@}O�@}�@|�/@|��@|�j@|z�@{�@z^5@zM�@z=q@zJ@yx�@xQ�@w�w@w\)@v��@v��@v$�@u@t�@tI�@s�
@s�@sC�@r��@r�@qx�@p�`@pr�@pb@p  @o�w@o��@ol�@o
=@n�y@n��@n�+@n5?@m�-@mO�@l�/@l�@kdZ@ko@j��@j~�@j-@i&�@h�u@hQ�@g��@g|�@g;d@f�@f5?@e�@e�T@e`B@eV@d�/@d��@dj@dZ@d9X@d(�@d(�@d(�@d(�@c�F@c33@bn�@b=q@a�#@aX@a�@`�9@`r�@` �@_;d@^��@^5?@^{@]�@]��@]�h@]`B@]O�@\��@\�/@\�@\9X@[ƨ@[t�@[o@Z�!@Z^5@Y��@Y��@Yhs@Y7L@X�`@X��@X �@W�P@V�y@Vv�@U�T@U�h@T�@Tz�@S�
@SdZ@So@R�H@R~�@RJ@Q�7@Q&�@P�9@P �@P  @P  @P  @O�;@OK�@N�y@Nȴ@Nȴ@Nv�@M�T@M�@L��@L�D@L(�@L�@L�@L1@K�F@Kt�@Kt�@KS�@KC�@K33@Jn�@J�@I��@I�^@I��@Ihs@I%@H�@Hr�@HQ�@HA�@Hb@G�@G��@G�@G\)@G+@G�@F�y@F��@Fff@FV@F{@E�T@E��@E��@EO�@EV@D��@D9X@D�@D1@C�m@C�
@CS�@B��@B�!@BM�@Ax�@AG�@AG�@AG�@A&�@@�`@@��@@�@@bN@?�;@?;d@>��@>��@>V@>{@=@=�h@=p�@=?}@=/@=V@<��@<Z@;�m@;��@;t�@;dZ@;S�@;33@:��@:��@:^5@:M�@:-@:J@9�#@9�^@9��@9G�@9&�@9%@8��@8Ĝ@8r�@8Q�@8  @7�w@7|�@7\)@7�@6�@6�R@6�+@6{@5p�@4��@4�@4�/@4��@4�@4j@4�@3�
@3��@3"�@2��@2n�@2-@1�@1��@1X@17L@1�@0Ĝ@0Q�@/��@/K�@.�y@.ȴ@.��@.v�@.5?@.$�@.{@-��@-/@-V@,�@,�/@,��@,�j@,z�@,�@+�m@+ƨ@+�F@+��@+��@+@*-@*J@*J@)��@)�@)��@)�^@)�^@)��@)��@)��@)�7@)G�@(bN@'�;@'�@'|�@'\)@'
=@&�+@&V@&V@&5?@%@%�h@%�@%�@%p�@%`B@$��@#�m@#��@#t�@#dZ@#C�@#"�@#@"��@"M�@"=q@"-@"�@"J@!�#@!�7@ ��@ �@  �@�;@\)@K�@+@+@
=@�+@v�@ff@5?@@�@�T@�-@�@/@��@�/@��@�@j@(�@�@��@�
@ƨ@ƨ@�F@��@�@t�@t�@S�@33@o@�H@�\@n�@=q@J@��@��@�7@x�@7L@%@�`@�9@��@�u@r�@bN@A�@ �@  @��@�w@|�@K�@K�@;d@�@ȴ@ff@$�@@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	ǮB	ǮB	ƨB	ƨB	ƨB	ƨB	ŢB	ŢB	ƨB	ŢB	ŢB	ŢB	ƨB	ŢB	ŢB	ŢB	ƨB	ǮB	ƨB	ƨB	ȴB	�B	�BB	�mB	��B
B
%B
+B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
1B
+B
1B
JB
VB
\B
hB
uB
{B
�B
m�B
�BB%�Bv�B�B�oB�uB��B��B��B�-B�#B��B
=B��BVBDB  B�B�B�B�yB��B�?B��B��Bz�B1'B
�;B
ǮB
�!B
��B
q�B
ZB
B�B
'�B
�B
%B	�B	ɺB	�-B	�%B	jB	T�B	/B	)�B	�B��B�yB�sB�B��B	JB	�B	'�B	0!B	0!B	1'B	,B	Q�B	}�B	}�B	~�B	� B	�B	�7B	�DB	��B	�FB	�dB	�qB	��B	�#B	�B	�B	�`B	��B	��B
  B
B
�B
#�B
&�B
-B
/B
.B
,B
,B
/B
1'B
8RB
A�B
F�B
D�B
C�B
=qB
>wB
J�B
`BB
`BB
^5B
[#B
T�B
N�B
J�B
R�B
ZB
W
B
ZB
ZB
W
B
S�B
W
B
VB
W
B
VB
XB
XB
[#B
]/B
\)B
\)B
\)B
[#B
\)B
[#B
[#B
YB
YB
W
B
VB
S�B
P�B
P�B
M�B
J�B
H�B
H�B
G�B
E�B
D�B
?}B
;dB
<jB
@�B
C�B
C�B
B�B
B�B
B�B
B�B
A�B
@�B
?}B
=qB
9XB
7LB
6FB
7LB
8RB
:^B
9XB
:^B
8RB
33B
+B
+B
1'B
/B
.B
-B
,B
,B
,B
,B
,B
-B
.B
1'B
2-B
33B
49B
5?B
49B
1'B
-B
,B
+B
,B
/B
/B
.B
.B
/B
.B
.B
.B
.B
,B
+B
+B
+B
+B
)�B
(�B
(�B
(�B
,B
/B
-B
,B
)�B
)�B
+B
(�B
#�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
,B
-B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
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
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
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
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
I�B
J�B
J�B
J�B
J�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
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
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
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
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
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
]/B
]/B
]/B
]/B
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
`BB
`BB
`BB
`BB
`BB
`BB
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
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
dZB
dZB
e`B
e`B
e`B
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
ffB
ffB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�cB	�cB	�]B	�]B	�]B	�]B	�WB	�WB	�]B	�WB	�WB	�WB	�]B	�WB	�WB	�WB	�]B	�cB	�]B	�]B	�iB	��B	��B	�"B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
B
*B
0B
[B
]FB
��B�Bf~Bt�B�$B�*B�OB�UB�sB��B��B�B��B��B�B��B�B�YB�@B�MB�.BĳB��B�sB�BBj�B �B
��B
�cB
��B
�<B
a_B
I�B
2DB
�B
HB	��B	�@B	�oB	��B	u�B	Z4B	D�B	�B	�B	
UB�B�.B�(B�@B�B��B	[B	�B	�B	�B	 �B	�B	A�B	m�B	m�B	n�B	o�B	s�B	x�B	z�B	�gB	��B	�B	�&B	�8B	��B	�:B	�4B	�B	�B	�B	�B	��B
	OB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
(B
1>B
6]B
4QB
3KB
-&B
.,B
:vB
O�B
O�B
M�B
J�B
D�B
>�B
:vB
B�B
I�B
F�B
I�B
I�B
F�B
C�B
F�B
E�B
F�B
E�B
G�B
G�B
J�B
L�B
K�B
K�B
K�B
J�B
K�B
J�B
J�B
H�B
H�B
F�B
E�B
C�B
@�B
@�B
=�B
:vB
8iB
8iB
7cB
5WB
4QB
/2B
+B
,B
08B
3KB
3KB
2DB
2DB
2DB
2DB
1>B
08B
/2B
-&B
)B
'B
%�B
'B
(B
*B
)B
*B
(B
"�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
#�B
$�B
#�B
 �B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
zB
sB
mB
sB
sB
mB
mB
gB
gB
mB
mB
mB
mB
gB
aB

UB

UB
[B
[B
aB
aB
[B
[B
[B
gB
gB
gB
aB
[B
aB
gB
aB
[B
[B

UB

UB

UB

UB
aB
gB
aB

UB

UB

UB

UB
	OB
	OB

UB

UB

UB

UB
[B
[B

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB

UB
	OB
	OB
HB
BB
HB
BB
HB
HB
HB
HB
HB
HB
HB
BB
HB
	OB
	OB
	OB
	OB
	OB
	OB
HB
	OB
	OB
	OB

UB

UB

UB
	OB
HB
BB
	OB
HB
BB

UB

UB
	OB
HB
HB
BB
	OB
HB
BB
BB
HB
HB
BB
<B
<B
<B
BB
BB
BB
BB
	OB

UB

UB

UB
[B

UB

UB

UB

UB
[B
[B
[B
[B
[B
[B
[B
aB
gB
gB
gB
gB
aB
aB
mB
mB
sB
sB
sB
sB
sB
sB
zB
zB
zB
zB
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
'B
'B
'B
'B
'B
(B
)B
)B
)B
)B
)B
)B
)B
*B
*B
*B
*B
*B
+B
+B
,B
,B
,B
,B
,B
,B
,B
-&B
.,B
.,B
.,B
.,B
/2B
/2B
/2B
08B
08B
1>B
1>B
1>B
1>B
1>B
1>B
1>B
1>B
1>B
2DB
2DB
3KB
3KB
3KB
3KB
3KB
3KB
3KB
3KB
3KB
4QB
4QB
4QB
5WB
6]B
6]B
6]B
6]B
6]B
6]B
6]B
7cB
7cB
7cB
7cB
7cB
8iB
8iB
8iB
8iB
8iB
9oB
8iB
8iB
9oB
:vB
:vB
:vB
:vB
9oB
:vB
:vB
:vB
:vB
:vB
;|B
:vB
;|B
;|B
;|B
;|B
<�B
<�B
<�B
<�B
<�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
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
B�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
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
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
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
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
RB
RB
RB
RB
RB
RB
RB
RB
RB
RB
S	B
S	B
S	B
S	B
S	B
S	B
S	B
TB
TB
TB
TB
TB
TB
TB
TB
TB
UB
TB
TB
UB
UB
UB
UB
UB
UB
UB
UB
VB
VB
VB
VB
VB
VB
VB
W"B
W"B
X(B
X(B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Y.B
Z4B
Z4B
[:B
[:B
[:B
[:B
[:B
[:B
[:B
\@B
\@B
\@B
\@B
]FB
]FB
]FB
]FB
]FB
]FB
]FB
]FB
^MB
^MB
^MB
^MB
^MB
^MB
^MB
^MB
^MB
^MB
^MB
^MB
_SB
_SB
_SB
`YB
`YB
`YB
`YB
`YB
`YB
`YB
a_B
a_B
beB
beB
beB
beB
beB
beB
beB
beB
beB
ckB
ckB
ckB
ckB
dqB
dqB
dqB
dqB
dqB
dqB
exB
exB
f~B
f~B
f~B
f~B
f~B
f~B
f~B
f~B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.61 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20200121100041              20200121100041  AO  ARCAADJP                                                                    20200121100041    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20200121100041    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200121100041  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200121100041  QCF$                G�O�G�O�G�O�0               