CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-04-10T10:00:35Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20200410100035  20200410100035  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @�ֱ���1   @�ـ��n@+PbM���do��$�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D��3D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�3D�0 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@ӅA	A)AIAiA��HA��HA��HA��HA��HA��HA��HA��HBp�B
p�Bp�Bp�B"p�B*p�B2p�B:p�BBp�BJp�BRp�BZp�Bbp�Bjp�Brp�Bzp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC �)C�)C�)C�)C�)C
�)C�)C�)C��C�)C�)C�)C�)C�)C�)C�)C �)C"�)C$�)C&�)C(�)C*�)C,�)C.�)C0�)C2�)C4�)C6�)C8�)C:�)C<�)C>�)C@�)CB�)CD�)CF�)CH�)CJ�)CL�)CN�)CP�)CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn�)Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�Z�C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�ND '
D �
D'
D�pD'
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
DX�pDY'
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
D��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD�RD�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D�D�ӅD��D�S�DÓ�D�ӅD��D�S�Dē�D�ӅD��D�S�Dœ�D�ӅD��D�S�DƓ�D�ӅD��D�S�DǓ�D�ӅD��D�S�Dȓ�D�ӅD��D�S�Dɓ�D�ӅD��D�S�Dʓ�D�ӅD��D�S�D˓�D�ӅD��D�S�D̓�D�ӅD��D�S�D͓�D�ӅD��D�S�DΓ�D�ӅD��D�S�Dϓ�D�ӅD��D�S�DГ�D�ӅD��D�S�Dѓ�D�ӅD��D�S�Dғ�D�ӅD��D�S�Dӓ�D�ӅD��D�S�Dԓ�D�ӅD��D�S�DՓ�D�ָD��D�S�D֓�D�ӅD��D�S�Dד�D�ӅD��D�S�Dؓ�D�ӅD��D�S�Dٓ�D�ӅD��D�S�Dړ�D�ӅD��D�S�Dۓ�D�ӅD��D�S�Dܓ�D�ӅD��D�S�Dݓ�D�ӅD��D�S�Dޓ�D�ӅD��D�S�Dߓ�D�ӅD��D�S�D���D�ӅD��D�S�DᓅD�ӅD��D�S�DⓅD�ӅD��D�S�D㓅D�ӅD��D�S�D䓅D�ӅD��D�S�D哅D�ӅD��D�S�D擅D�ӅD��D�V�D疸D�ӅD��D�S�D蓅D��RD��D�S�D铅D�ӅD��D�S�DꖸD�ӅD��D�S�D듅D�ӅD��D�S�D쓅D�ӅD��D�S�D퓅D�ӅD��D�S�DD�ӅD��D�S�DD�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�V�D���D�ӅD��D�C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�E�A�A�A�1Aգ�A���A�z�A�t�A�l�A�bNA�^5A�Q�A�E�A�5?A�&�A��A�1A��TA�9XAҬA�dZA�{A��;AѴ9Aѝ�A�t�A�5?A�
=Aв-A�"�Aϧ�A�K�A�+A��A���AΩ�A�?}A�5?A�r�A�ZA��Aǡ�A���A� �A�1'A�l�A�A��A��/A���A���A���A�hsA�~�A��RA�ZA��-A���A��A���A��^A��A�bNA��PA�A��-A��FA�~�A��A�JA�/A��TA��A�^5A�K�A�%A|{Aq�TAl�jAhr�AedZAb�A_t�AY�TAUG�ATjAR�RAOXAI��AG�FAGAFACoAA��A?�hA?"�A>ȴA>Q�A<ȴA;��A;A9�A7�FA4^5A1ƨA1%A.�A-��A-t�A,�A+G�A*��A*5?A(M�A(�A'S�A&ĜA%�TA%�A%+A$�A#"�A"�DA"=qA"  A!��A!�^A ��A   A�AA�AƨA�RAn�A��A-A��A�9A�wA
=Ar�AK�A�+A�AM�A��A�A|�A�AffAK�A��A�A�AVA~�A~�A�A|�AjA�TA|�A|�A�7A�PA��AA  A�#A�PA7LA�/A�!A��A�+Az�AE�A{A�A��A��AS�AA��A��A33AAA�A1'A�hA
��A
VA	��A	\)A	�A�A�DAz�A{A�
A��A��A��A�HA��AbNA5?AJA��Al�A"�A�/A�\A5?A�^A\)AVA��AbNA  A��A��AXA ��A �uA jA 1'A $�A �A A   A   @��F@�t�@��y@�~�@�M�@��T@���@�9X@��w@�|�@�
=@�v�@���@�X@���@�j@� �@��
@�33@���@���@�{@�x�@�%@��u@��@�?}@��@��m@�o@��H@�h@�bN@�33@�n�@�J@�-@�X@���@�D@��m@�ȴ@��@��#@��@�h@�@�&�@�r�@�F@�33@⟾@��@�^@�(�@��@�@��H@޸R@�v�@��@�hs@��@�"�@�5?@���@�x�@�V@؋D@��;@ׅ@�S�@�C�@�;d@�
=@�M�@�@Ցh@ԓu@���@ӕ�@�+@�^5@Ѳ-@�/@��`@У�@�1@�;d@Ο�@��@��@�Z@�b@�1@��;@�K�@��y@�ȴ@�5?@ɩ�@�7L@Ȭ@�Z@�I�@�  @ǍP@�dZ@�K�@�;d@���@š�@��@Ĵ9@�r�@�bN@�1'@���@�t�@�@���@�5?@��h@�V@���@�Q�@���@�ƨ@���@��@��\@�5?@��@��h@�7L@���@�j@�(�@���@��F@��@�C�@�ȴ@��+@�V@��^@��7@�7L@���@�9X@��@�dZ@�o@��@��\@�V@�-@��-@��@��@�Ĝ@���@�(�@�ƨ@��w@��w@��@���@��@��R@�v�@�V@�{@��@��@��@���@��`@�Ĝ@�Ĝ@���@�bN@�9X@�|�@��H@��\@�ff@��@��#@���@��7@�`B@�&�@���@���@�b@��
@�t�@��@��@�ȴ@��R@��!@�n�@���@�O�@���@��9@���@�I�@� �@��@���@�o@���@�v�@�^5@�5?@��@��h@�7L@�V@��j@��D@�A�@��@��;@���@�C�@��@��@��+@�M�@���@�`B@��@�9X@��;@��@��@�t�@�\)@�\)@�33@���@�v�@�V@���@�X@�7L@�%@��@���@�r�@��;@�dZ@��y@���@��@���@�x�@�/@��@���@��j@��@�Q�@�1'@�b@��@�ƨ@���@�dZ@��y@���@�ȴ@���@�~�@�n�@�E�@���@���@�x�@���@��
@���@�C�@��@��y@�~�@�$�@���@�V@��@���@�bN@�(�@�1@��@�ƨ@�\)@�@��H@���@�M�@��@���@���@�O�@��@���@��D@�j@� �@��@�t�@��!@�n�@�M�@�=q@�$�@��@�x�@�?}@��@���@�A�@��;@��;@��F@�t�@�dZ@�dZ@�\)@�S�@��H@���@�v�@�V@�5?@��@�J@���@��@���@��j@��D@�9X@� �@�1@�|�@��@�o@�
=@���@���@���@�v�@�5?@���@���@�x�@�O�@�/@�Ĝ@��@�1'@��@|�@;d@+@�@~ȴ@}�T@}�@|�@{ƨ@{dZ@{33@z��@z~�@z�@yx�@y%@xr�@x1'@w|�@w
=@vȴ@v5?@u�@u�-@uO�@t�@t�@s��@s"�@r��@rn�@q��@qX@q�@p��@pr�@o��@n�@n$�@m�@l��@lI�@l9X@l(�@l1@k"�@j�\@jM�@i�^@i��@iX@i&�@h��@hr�@g��@gK�@f�y@f�R@f{@ep�@e?}@e/@d��@d�@c��@cC�@c33@co@b�@b��@b~�@ahs@`��@`r�@_�;@_l�@^��@^E�@]p�@\��@\z�@[��@[�@[C�@Zn�@ZJ@Y��@Yx�@X�@X �@W�w@W�P@WK�@V�@V5?@U��@U��@UO�@T�/@Tj@TI�@S��@Sƨ@S��@S�@SC�@R�H@R^5@R-@R�@Q��@Q��@Q��@Q�@P��@PA�@O�@O�P@O+@O�@N�R@Nv�@N$�@Mp�@L�@L�@Kƨ@Kt�@J�@J�!@J~�@J=q@I�#@Ihs@I�@HĜ@H�u@H �@G�w@Gl�@G;d@G�@F��@FV@E�@E�-@E��@E/@D�/@D�@DZ@D1@C@B=q@BJ@A��@Ahs@@��@@�@@Q�@@ �@?�;@?�w@?�@?�@?�P@?K�@>��@>v�@>{@=@=p�@=V@<�j@<��@;�m@;"�@:�H@:��@:M�@9��@9�#@9�7@9�@8�9@8bN@81'@8b@8  @7|�@7\)@7+@6�@6��@6ff@5�@5�h@4��@4�@4Z@41@3�F@3��@3�@3t�@3"�@2n�@2^5@2�@1�7@1G�@1�@0��@0�@0A�@0 �@/��@/|�@/\)@/+@.�@.�R@.5?@.@-�T@-�@,��@,��@,�@+��@+�F@+t�@+@*��@*��@*�\@*~�@*~�@*n�@*M�@)�@)X@(Ĝ@(r�@(A�@( �@'|�@';d@'�@'
=@'
=@'
=@&�@&V@&$�@%�@%�-@%p�@%O�@%?}@%�@%�@%V@$��@$�/@$�@$Z@$9X@#��@#�
@#S�@#S�@#C�@#o@"�H@"�\@"M�@"-@!��@!�#@!�^@!X@!%@ ��@ ��@�@�P@l�@+@ȴ@V@$�@{@{@{@@�T@�-@��@��@�h@�@V@�@�j@�j@�D@�@�m@ƨ@��@�@dZ@o@@��@�!@�!@�!@n�@M�@M�@M�@M�@-@�^@��@��@x�@hs@G�@�@�`@�9@�@bN@1'@�@�@�;@\)@
=@�@�R@�+@$�@�@@��@��@O�@/@V@�@�@�@�/@��@��@I�@�@1@��@�
@�
@ƨ@ƨ@ƨ@�F@��@�@t�@S�@33@@�@��@��@~�@M�@=q@�@�@��@X@&�@%@�`@��@�@�@r�@1'@b@�@�;@�w@|�@\)@K�@\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�E�A�A�A�1Aգ�A���A�z�A�t�A�l�A�bNA�^5A�Q�A�E�A�5?A�&�A��A�1A��TA�9XAҬA�dZA�{A��;AѴ9Aѝ�A�t�A�5?A�
=Aв-A�"�Aϧ�A�K�A�+A��A���AΩ�A�?}A�5?A�r�A�ZA��Aǡ�A���A� �A�1'A�l�A�A��A��/A���A���A���A�hsA�~�A��RA�ZA��-A���A��A���A��^A��A�bNA��PA�A��-A��FA�~�A��A�JA�/A��TA��A�^5A�K�A�%A|{Aq�TAl�jAhr�AedZAb�A_t�AY�TAUG�ATjAR�RAOXAI��AG�FAGAFACoAA��A?�hA?"�A>ȴA>Q�A<ȴA;��A;A9�A7�FA4^5A1ƨA1%A.�A-��A-t�A,�A+G�A*��A*5?A(M�A(�A'S�A&ĜA%�TA%�A%+A$�A#"�A"�DA"=qA"  A!��A!�^A ��A   A�AA�AƨA�RAn�A��A-A��A�9A�wA
=Ar�AK�A�+A�AM�A��A�A|�A�AffAK�A��A�A�AVA~�A~�A�A|�AjA�TA|�A|�A�7A�PA��AA  A�#A�PA7LA�/A�!A��A�+Az�AE�A{A�A��A��AS�AA��A��A33AAA�A1'A�hA
��A
VA	��A	\)A	�A�A�DAz�A{A�
A��A��A��A�HA��AbNA5?AJA��Al�A"�A�/A�\A5?A�^A\)AVA��AbNA  A��A��AXA ��A �uA jA 1'A $�A �A A   A   @��F@�t�@��y@�~�@�M�@��T@���@�9X@��w@�|�@�
=@�v�@���@�X@���@�j@� �@��
@�33@���@���@�{@�x�@�%@��u@��@�?}@��@��m@�o@��H@�h@�bN@�33@�n�@�J@�-@�X@���@�D@��m@�ȴ@��@��#@��@�h@�@�&�@�r�@�F@�33@⟾@��@�^@�(�@��@�@��H@޸R@�v�@��@�hs@��@�"�@�5?@���@�x�@�V@؋D@��;@ׅ@�S�@�C�@�;d@�
=@�M�@�@Ցh@ԓu@���@ӕ�@�+@�^5@Ѳ-@�/@��`@У�@�1@�;d@Ο�@��@��@�Z@�b@�1@��;@�K�@��y@�ȴ@�5?@ɩ�@�7L@Ȭ@�Z@�I�@�  @ǍP@�dZ@�K�@�;d@���@š�@��@Ĵ9@�r�@�bN@�1'@���@�t�@�@���@�5?@��h@�V@���@�Q�@���@�ƨ@���@��@��\@�5?@��@��h@�7L@���@�j@�(�@���@��F@��@�C�@�ȴ@��+@�V@��^@��7@�7L@���@�9X@��@�dZ@�o@��@��\@�V@�-@��-@��@��@�Ĝ@���@�(�@�ƨ@��w@��w@��@���@��@��R@�v�@�V@�{@��@��@��@���@��`@�Ĝ@�Ĝ@���@�bN@�9X@�|�@��H@��\@�ff@��@��#@���@��7@�`B@�&�@���@���@�b@��
@�t�@��@��@�ȴ@��R@��!@�n�@���@�O�@���@��9@���@�I�@� �@��@���@�o@���@�v�@�^5@�5?@��@��h@�7L@�V@��j@��D@�A�@��@��;@���@�C�@��@��@��+@�M�@���@�`B@��@�9X@��;@��@��@�t�@�\)@�\)@�33@���@�v�@�V@���@�X@�7L@�%@��@���@�r�@��;@�dZ@��y@���@��@���@�x�@�/@��@���@��j@��@�Q�@�1'@�b@��@�ƨ@���@�dZ@��y@���@�ȴ@���@�~�@�n�@�E�@���@���@�x�@���@��
@���@�C�@��@��y@�~�@�$�@���@�V@��@���@�bN@�(�@�1@��@�ƨ@�\)@�@��H@���@�M�@��@���@���@�O�@��@���@��D@�j@� �@��@�t�@��!@�n�@�M�@�=q@�$�@��@�x�@�?}@��@���@�A�@��;@��;@��F@�t�@�dZ@�dZ@�\)@�S�@��H@���@�v�@�V@�5?@��@�J@���@��@���@��j@��D@�9X@� �@�1@�|�@��@�o@�
=@���@���@���@�v�@�5?@���@���@�x�@�O�@�/@�Ĝ@��@�1'@��@|�@;d@+@�@~ȴ@}�T@}�@|�@{ƨ@{dZ@{33@z��@z~�@z�@yx�@y%@xr�@x1'@w|�@w
=@vȴ@v5?@u�@u�-@uO�@t�@t�@s��@s"�@r��@rn�@q��@qX@q�@p��@pr�@o��@n�@n$�@m�@l��@lI�@l9X@l(�@l1@k"�@j�\@jM�@i�^@i��@iX@i&�@h��@hr�@g��@gK�@f�y@f�R@f{@ep�@e?}@e/@d��@d�@c��@cC�@c33@co@b�@b��@b~�@ahs@`��@`r�@_�;@_l�@^��@^E�@]p�@\��@\z�@[��@[�@[C�@Zn�@ZJ@Y��@Yx�@X�@X �@W�w@W�P@WK�@V�@V5?@U��@U��@UO�@T�/@Tj@TI�@S��@Sƨ@S��@S�@SC�@R�H@R^5@R-@R�@Q��@Q��@Q��@Q�@P��@PA�@O�@O�P@O+@O�@N�R@Nv�@N$�@Mp�@L�@L�@Kƨ@Kt�@J�@J�!@J~�@J=q@I�#@Ihs@I�@HĜ@H�u@H �@G�w@Gl�@G;d@G�@F��@FV@E�@E�-@E��@E/@D�/@D�@DZ@D1@C@B=q@BJ@A��@Ahs@@��@@�@@Q�@@ �@?�;@?�w@?�@?�@?�P@?K�@>��@>v�@>{@=@=p�@=V@<�j@<��@;�m@;"�@:�H@:��@:M�@9��@9�#@9�7@9�@8�9@8bN@81'@8b@8  @7|�@7\)@7+@6�@6��@6ff@5�@5�h@4��@4�@4Z@41@3�F@3��@3�@3t�@3"�@2n�@2^5@2�@1�7@1G�@1�@0��@0�@0A�@0 �@/��@/|�@/\)@/+@.�@.�R@.5?@.@-�T@-�@,��@,��@,�@+��@+�F@+t�@+@*��@*��@*�\@*~�@*~�@*n�@*M�@)�@)X@(Ĝ@(r�@(A�@( �@'|�@';d@'�@'
=@'
=@'
=@&�@&V@&$�@%�@%�-@%p�@%O�@%?}@%�@%�@%V@$��@$�/@$�@$Z@$9X@#��@#�
@#S�@#S�@#C�@#o@"�H@"�\@"M�@"-@!��@!�#@!�^@!X@!%@ ��@ ��@�@�P@l�@+@ȴ@V@$�@{@{@{@@�T@�-@��@��@�h@�@V@�@�j@�j@�D@�@�m@ƨ@��@�@dZ@o@@��@�!@�!@�!@n�@M�@M�@M�@M�@-@�^@��@��@x�@hs@G�@�@�`@�9@�@bN@1'@�@�@�;@\)@
=@�@�R@�+@$�@�@@��@��@O�@/@V@�@�@�@�/@��@��@I�@�@1@��@�
@�
@ƨ@ƨ@ƨ@�F@��@�@t�@S�@33@@�@��@��@~�@M�@=q@�@�@��@X@&�@%@�`@��@�@�@r�@1'@b@�@�;@�w@|�@\)@K�@\)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�LBȴB�
B�#B�fB�B�B	%B	6FB	VB	�B	��B	�B
�B
0!B
D�B
R�B
n�B
��B
�!B
ŢB
�B
��BB�B"�B�B�B"�B'�B!�BuB
��B
�B
�yB
�5B
��B
�B
�/B
�5B
�'B
�3B
�^B
��B
jB
=qB
�B	��B	��B	�9B	��B	�hB	�B	t�B	aHB	VB	T�B	M�B	K�B	M�B	L�B	M�B	N�B	S�B	VB	ZB	ZB	[#B	\)B	`BB	bNB	`BB	`BB	e`B	q�B	y�B	z�B	�B	�=B	��B	�LB	�RB	�}B	��B	�}B	��B	��B	�NB	�B	��B	��B
1B
PB
oB
�B
�B
�B
�B
�B
�B
oB
{B
�B
�B
�B
�B
#�B
-B
)�B
A�B
>wB
<jB
K�B
D�B
E�B
33B
"�B
�B
9XB
E�B
>wB
:^B
>wB
C�B
@�B
?}B
B�B
B�B
?}B
A�B
9XB
5?B
49B
7LB
:^B
<jB
?}B
D�B
J�B
K�B
L�B
O�B
R�B
R�B
S�B
R�B
S�B
VB
XB
XB
ZB
[#B
ZB
YB
T�B
O�B
P�B
Q�B
O�B
P�B
M�B
K�B
K�B
K�B
J�B
K�B
K�B
J�B
J�B
J�B
J�B
I�B
F�B
G�B
I�B
J�B
J�B
J�B
J�B
J�B
I�B
H�B
F�B
E�B
D�B
D�B
B�B
A�B
A�B
@�B
>wB
>wB
=qB
>wB
=qB
<jB
<jB
=qB
=qB
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
?}B
>wB
=qB
<jB
<jB
>wB
=qB
<jB
<jB
;dB
;dB
;dB
:^B
:^B
9XB
8RB
8RB
8RB
8RB
7LB
6FB
33B
0!B
-B
,B
+B
)�B
+B
)�B
(�B
(�B
)�B
)�B
)�B
)�B
(�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
&�B
&�B
%�B
%�B
%�B
$�B
$�B
#�B
#�B
#�B
#�B
"�B
"�B
!�B
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
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
)�B
+B
+B
+B
+B
,B
,B
,B
-B
-B
.B
/B
/B
/B
/B
/B
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
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
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
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
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
9XB
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
A�B
B�B
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
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
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
J�B
J�B
J�B
K�B
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
M�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
O�B
P�B
Q�B
Q�B
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
T�B
VB
VB
VB
T�B
VB
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
VB
VB
VB
W
B
W
B
W
B
XB
XB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
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
^5B
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
aHB
aHB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
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
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
jB
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
l�B
l�B
l�B
l�B
l�B
m�B
l�B
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
p�B
p�B
p�B
q�B
q�B
q�B
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
w�B
w�B
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
x�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�%B
�%B
�%B
�%B
�%B
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
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�1B
�1B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�UB�OB�UB�[B�sB�zB�zB�zB�zB�zB�zB�zB�zB�sB�mB�mB�aB�OB�OB�gB��B��B��B��B��B��B��B��B�B�iBƿB��B�B�FB�kB��B	%�B	E�B	p�B	�vB	�eB
sB
�B
4QB
B�B
^MB
�[B
��B
�WB
�SB
�~B
��BmB�BmBgB�B�B�B*B
�B
�kB
�.B
��B
íB
��B
��B
��B
��B
��B
�B
��B
Z4B
-&B
gB	�B	��B	��B	�zB	�B	q�B	dqB	P�B	E�B	D�B	=�B	;|B	=�B	<�B	=�B	>�B	C�B	E�B	I�B	I�B	J�B	K�B	O�B	RB	O�B	O�B	UB	a_B	i�B	j�B	p�B	y�B	�zB	�B	�B	�2B	�>B	�2B	��B	íB	�B	�4B	�B	�B	��B	�B
$B
BB
[B
gB
aB
[B
HB
$B
0B

UB
aB
gB
gB
�B
�B
�B
1>B
.,B
,B
;|B
4QB
5WB
"�B
�B
gB
)B
5WB
.,B
*B
.,B
3KB
08B
/2B
2DB
2DB
/2B
1>B
)B
$�B
#�B
'B
*B
,B
/2B
4QB
:vB
;|B
<�B
?�B
B�B
B�B
C�B
B�B
C�B
E�B
G�B
G�B
I�B
J�B
I�B
H�B
D�B
?�B
@�B
A�B
?�B
@�B
=�B
;|B
;|B
;|B
:vB
;|B
;|B
:vB
:vB
:vB
:vB
9oB
6]B
7cB
9oB
:vB
:vB
:vB
:vB
:vB
9oB
8iB
6]B
5WB
4QB
4QB
2DB
1>B
1>B
08B
.,B
.,B
-&B
.,B
-&B
,B
,B
-&B
-&B
/2B
08B
08B
08B
08B
08B
08B
08B
/2B
.,B
-&B
,B
,B
.,B
-&B
,B
,B
+B
+B
+B
*B
*B
)B
(B
(B
(B
(B
'B
%�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
zB
zB
zB
zB
zB
sB
sB
sB
sB
sB
sB
sB
sB
sB
sB
mB
mB
mB
gB
gB
gB
aB
aB
aB
aB
aB
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
aB
aB
aB
aB
aB
[B
aB
aB
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
[B
aB
aB
aB
aB
aB
aB
aB
aB
aB
gB
gB
gB
gB
gB
gB
gB
gB
gB
mB
mB
mB
mB
mB
sB
sB
sB
sB
sB
sB
sB
sB
sB
sB
zB
zB
zB
�B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
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
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
'B
'B
'B
'B
'B
(B
(B
(B
(B
(B
)B
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
+B
+B
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
08B
08B
08B
08B
08B
08B
08B
08B
08B
08B
08B
08B
1>B
1>B
1>B
2DB
2DB
1>B
2DB
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
5WB
5WB
5WB
6]B
6]B
6]B
6]B
6]B
6]B
6]B
6]B
6]B
7cB
7cB
8iB
8iB
8iB
8iB
8iB
9oB
9oB
9oB
9oB
:vB
:vB
:vB
:vB
:vB
:vB
:vB
;|B
;|B
;|B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
?�B
?�B
@�B
@�B
@�B
?�B
@�B
A�B
A�B
B�B
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
E�B
E�B
E�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
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
M�B
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
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
RB
RB
S	B
S	B
S	B
TB
TB
TB
TB
TB
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
W"B
X(B
X(B
X(B
Y.B
Y.B
Y.B
Y.B
Y.B
Z4B
Z4B
Z4B
[:B
Z4B
[:B
[:B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
\@B
]FB
\@B
]FB
]FB
]FB
]FB
^MB
^MB
^MB
^MB
_SB
_SB
_SB
_SB
_SB
`YB
`YB
`YB
a_B
a_B
a_B
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
exB
exB
exB
exB
exB
exB
f~B
f~B
f~B
f~B
f~B
f~B
g�B
g�B
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
h�B
i�B
i�B
j�B
j�B
j�B
j�B
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
n�B
n�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
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
u�B
u�B
u�B
u�B
u�B
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
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
w�B
w�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
}B
{�B
{�B
}B
}B
}B
}B
}B
~B
~B
~B
~B
~B
B
B
B
B
B
B
B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�$B
�$B
�$B
�$B
�$B
�*B
�*B
�*B
�*B
�*B
�*B
�*B
�0B
�0B
�0B
�0B
�0B
�0B
�0B
�6B
�61111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.61 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20200410100035              20200410100035  AO  ARCAADJP                                                                    20200410100035    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20200410100035    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200410100035  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200410100035  QCF$                G�O�G�O�G�O�0               