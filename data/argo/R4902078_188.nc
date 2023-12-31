CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T12:00:33Z creation      
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
resolution        =���   axis      Z        |  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  `(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ̐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619120033  20200619120033  4902078 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5439                            2B  A   NAVIS_A                         0460                            011514                          863 @�"Y�@�1   @�"Zq�)p@*�M����d�� ě�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      �A   A   A   @9��@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @`��@��R@ӅA	A)AIAiA��HA��HA��HA��HA��HA��HA��HA��HBp�B
p�B
>Bp�B"p�B*p�B2p�B:p�BBp�BJp�BRp�BZp�Bbp�Bjp�Brp�Bzp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�B�8RB�8RB�k�B�8RB�8RB�8RB�8RB�B�B�8RB�8RB�8RB�8RB�8RC �)C�)C�)C�)C�)C
�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C �)C"�)C$�)C&�)C(�)C*�)C,�)C.�)C0�)C2�)C4�)C6�)C8�)C:�)C<�)C>�)C@�)CB�)CD�)CF�)CH�)CJ�)CL�)CN�)CP�)CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn�)Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�Z�C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�ND '
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
D@�pDA'
DA�
DB-pDB�
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
D��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D��RD�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D�D�ӅD��D�S�DÓ�D�ӅD��D�S�Dē�D�ӅD��D�S�Dœ�D�ӅD��D�S�DƓ�D�ӅD��D�S�DǓ�D�ӅD��D�S�Dȓ�D�ӅD��D�S�Dɓ�D�ӅD��D�S�Dʓ�D�ӅD��D�S�D˓�D�ӅD��D�S�D̓�D�ӅD��D�S�D͓�D�ӅD��D�S�DΓ�D�ӅD��D�S�Dϓ�D�ӅD��D�S�DГ�D�ӅD��D�S�Dѓ�D�ӅD��D�S�Dғ�D�ӅD��D�S�Dӓ�D�ӅD��D�S�Dԓ�D�ӅD��D�S�DՓ�D�ӅD��D�S�D֓�D�ӅD��D�S�Dד�D�ӅD��D�S�Dؓ�D�ӅD��D�S�Dٓ�D�ӅD��D�S�Dړ�D�ӅD��D�S�Dۓ�D�ӅD��D�S�Dܓ�D�ӅD��D�S�Dݓ�D�ӅD��D�S�Dޓ�D�ӅD��D�S�Dߓ�D�ӅD��D�S�D���D�ӅD��D�S�DᓅD�ӅD��D�S�DⓅD�ӅD��D�S�D㓅D�ӅD��D�S�D䓅D�ӅD��D�S�D哅D�ӅD��D�S�D擅D�ӅD��D�S�D瓅D�ӅD��D�S�D蓅D�ӅD��D�S�D铅D�ӅD��D�S�D꓅D�ӅD��D�S�D듅D�ӅD��D�S�D쓅D�ӅD��D�S�D퓅D�ӅD��D�S�DD�ӅD��D�S�DD�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�Å1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA���A���A��
A��A��/A��`A���A��#A��yA��
A��HA��HA��yA��`A��yA��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��mA��HA��;A��/A��/A��#A��#A���A�ĜAٗ�AظRA�&�A���A�/A˥�A��A��AŴ9A�+A���A�$�A�hsA�t�A�ZA�bNA�M�A�M�A���A�ĜA�O�A��A�XA��DA��wA���A�I�A���A��A��A�  A�?}A���A�-A�z�A� �A�A�A�&�A�{A��A�9XA�r�Az-Au�FAn�DAh��Ab�uA`$�A_p�A]�
A[�AY`BAW��AV��ASAO��AN9XAMl�AK��AJ=qAHVAFz�AE\)AD��AD�RAD1ABffAAO�A=XA;�A:��A:1A8�!A7\)A6~�A4ȴA3�FA3A2I�A1�FA1�A09XA/`BA,�RA+7LA*��A*ĜA)7LA'�A&r�A%�;A$��A$9XA#�FA#7LA!C�A!�A �yA ��A ffAl�AXAXAA�`A��A��A-A�AbNA�A�PA7LAoA�RA~�A=qA�A|�A`BA;dA33A��AȴA-A��AXA;dAVA�`An�A�A�hAx�AXAK�A?}AVA�A�A�A&�A��A��AM�AA��Ap�AO�A7LA�/A�+A�9AZA��A��AC�AȴA�A^5A(�A�A%A�\AA�FA��A��Ax�A\)A/AoA
�yA
��A
�A	�-A	�A��A�DA=qA  A�hAhsA��A�A%A�A�jA�Az�AA�A �A�AƨA�A�7Ap�A33A��AI�A�A��Al�A
=A r�A =qA J@��@���@��^@�p�@���@���@�  @�
=@�ȴ@�`B@�A�@��@���@�V@���@���@��;@�;d@��H@�n�@��@�x�@��@�I�@�b@�  @�P@�\@��@��^@�7@���@� �@�@���@�E�@�p�@�O�@��`@�  @�S�@�+@��@�hs@�7L@�j@��;@�ƨ@㝲@�"�@�n�@��@�p�@��@�9@�r�@�9X@߅@�M�@�&�@ܼj@܃@�r�@�1@�33@��y@�V@��#@ٙ�@ى7@�/@���@�9X@���@�\)@��@���@���@�Ĝ@�1@�;d@���@�ȴ@���@ҟ�@҇+@�ff@�M�@�X@�j@���@�C�@���@�v�@��@�&�@˅@�@ʸR@�E�@�{@��#@�O�@ȼj@�bN@��m@��;@���@ǍP@�C�@�E�@���@š�@�x�@�/@ēu@��@�;d@���@¸R@+@�@��@�j@��
@��@�l�@�+@�o@�@��@�M�@��@��@� �@���@���@�C�@�ȴ@�=q@���@�O�@���@��@�Q�@�b@��@��@��H@�v�@�^5@�V@���@���@�p�@��@��@�b@���@�+@��y@���@�ff@�{@��T@��h@�X@�V@���@���@�r�@��@�dZ@�"�@��@��!@�v�@�=q@��@���@�%@���@���@���@��@���@��@���@�\)@�@���@��R@���@�~�@�=q@�{@��@��h@�&�@��@���@��9@�Q�@�b@���@�C�@�
=@���@��!@�V@��@���@�X@��@��j@�z�@�1'@� �@�b@�1@�  @��@��;@���@�@���@�~�@��h@��@��u@��;@���@�dZ@�"�@��R@�^5@�-@��@���@��@�hs@���@�r�@�I�@��w@�;d@���@�M�@�5?@��@���@�`B@�G�@�%@�Ĝ@���@�1'@��w@��P@�l�@�S�@��@��@���@�ȴ@���@�n�@�V@���@�`B@�7L@�%@��9@�Z@�9X@�(�@�b@��;@�dZ@�+@��@��+@�E�@�{@��#@��h@��@��/@���@�Z@�  @���@��m@��F@���@�|�@�33@��R@��+@��@�@��h@��7@��@�x�@�`B@��9@�A�@�1'@�(�@��@���@�C�@��!@�=q@��#@��^@���@�x�@�7L@��@���@���@�bN@��;@��@���@�l�@�
=@���@�M�@�J@�@��@���@���@�Q�@�  @�ƨ@��P@�S�@�+@���@��R@�E�@��T@���@��h@�X@���@��@�Q�@�(�@��@~{@|��@|Z@|1@{�m@{��@{dZ@{C�@z�H@z-@y��@y�^@yX@y�@xbN@w�@w
=@vV@u@u`B@tz�@s�m@s��@st�@sdZ@sS�@sC�@s@r��@r=q@rJ@q�#@q��@qX@q%@pQ�@pb@o�;@o�@o�@nE�@m�T@m@m��@m�@m`B@l�/@l(�@k�
@kƨ@j�H@j�!@jn�@jJ@i��@i�7@i7L@h�`@hbN@g�@g\)@g�@f�R@f�+@e��@e�-@eO�@d�@dj@c��@c��@c"�@b�!@bM�@bJ@a�#@a��@`�@_��@_+@^��@^ff@^E�@]��@]?}@]/@\9X@[�@[o@Z��@Z=q@Y��@Y�#@Yx�@X��@X1'@X �@X �@X  @W��@W�@W|�@W;d@Vȴ@Vv�@V5?@V@U�@U�T@U�@T��@S��@S�@R�H@R��@Rn�@RJ@QX@P��@P�u@P�u@P�u@P �@O�w@O��@O�P@O�P@O+@N5?@M�T@M�-@M�h@MO�@M/@M/@L��@Lz�@K��@K�@K"�@JM�@I�#@Ihs@H�`@H�u@HbN@H1'@G�@Gl�@G;d@F�+@F$�@E@E`B@E?}@E?}@D�@DI�@D1@C�m@C�m@C�m@C�m@C�
@C�F@C��@C@B�!@BM�@BJ@A�@A��@A��@AX@A%@@Ĝ@@�@@A�@?��@?�P@?K�@>��@>��@=�T@=�h@=p�@=/@<�@<��@<Z@<9X@;��@;ƨ@;t�@;dZ@;dZ@;dZ@:�H@:��@:�@9�#@9�^@9�^@9��@9��@9�7@97L@8��@8bN@7�@7;d@6��@6�R@6�+@65?@5�@5�-@5/@4�@4(�@3t�@3S�@3C�@3C�@2��@2n�@2^5@2M�@2-@2�@1��@1��@1G�@0�`@0Ĝ@0�9@0Q�@/|�@.�@.��@.V@.$�@-@-O�@,��@,�D@,I�@,1@+ƨ@+t�@*�@*-@)��@*=q@)x�@)%@(Ĝ@(r�@( �@'�@'�;@'��@'�@'K�@'+@'
=@&�@&�R@&�+@&ff@&$�@%��@%/@$�/@$Z@$�@$1@#�
@#ƨ@#�F@#��@#��@#S�@#o@#@"�H@"��@"M�@"=q@"-@"J@!��@!��@!�@!��@!�^@!��@!X@!%@ Q�@�@��@l�@�@ȴ@v�@5?@�@��@�h@`B@O�@/@�@V@�@�j@z�@(�@��@�
@ƨ@ƨ@�@C�@"�@o@�@�H@��@��@��@�\@~�@n�@=q@��@��@��@hs@X@��@�`@��@�9@��@Ĝ@Ĝ@�u@�@Q�@ �@|�@K�@��@E�@$�@�@�h@`B@�@�-@@��@�@`B@?}@�@��@�@�@�@�@��@�D@�@dZ@C�@33@o@�@�H@��@�!@�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ƨA���A���A��
A��A��/A��`A���A��#A��yA��
A��HA��HA��yA��`A��yA��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A��mA��HA��;A��/A��/A��#A��#A���A�ĜAٗ�AظRA�&�A���A�/A˥�A��A��AŴ9A�+A���A�$�A�hsA�t�A�ZA�bNA�M�A�M�A���A�ĜA�O�A��A�XA��DA��wA���A�I�A���A��A��A�  A�?}A���A�-A�z�A� �A�A�A�&�A�{A��A�9XA�r�Az-Au�FAn�DAh��Ab�uA`$�A_p�A]�
A[�AY`BAW��AV��ASAO��AN9XAMl�AK��AJ=qAHVAFz�AE\)AD��AD�RAD1ABffAAO�A=XA;�A:��A:1A8�!A7\)A6~�A4ȴA3�FA3A2I�A1�FA1�A09XA/`BA,�RA+7LA*��A*ĜA)7LA'�A&r�A%�;A$��A$9XA#�FA#7LA!C�A!�A �yA ��A ffAl�AXAXAA�`A��A��A-A�AbNA�A�PA7LAoA�RA~�A=qA�A|�A`BA;dA33A��AȴA-A��AXA;dAVA�`An�A�A�hAx�AXAK�A?}AVA�A�A�A&�A��A��AM�AA��Ap�AO�A7LA�/A�+A�9AZA��A��AC�AȴA�A^5A(�A�A%A�\AA�FA��A��Ax�A\)A/AoA
�yA
��A
�A	�-A	�A��A�DA=qA  A�hAhsA��A�A%A�A�jA�Az�AA�A �A�AƨA�A�7Ap�A33A��AI�A�A��Al�A
=A r�A =qA J@��@���@��^@�p�@���@���@�  @�
=@�ȴ@�`B@�A�@��@���@�V@���@���@��;@�;d@��H@�n�@��@�x�@��@�I�@�b@�  @�P@�\@��@��^@�7@���@� �@�@���@�E�@�p�@�O�@��`@�  @�S�@�+@��@�hs@�7L@�j@��;@�ƨ@㝲@�"�@�n�@��@�p�@��@�9@�r�@�9X@߅@�M�@�&�@ܼj@܃@�r�@�1@�33@��y@�V@��#@ٙ�@ى7@�/@���@�9X@���@�\)@��@���@���@�Ĝ@�1@�;d@���@�ȴ@���@ҟ�@҇+@�ff@�M�@�X@�j@���@�C�@���@�v�@��@�&�@˅@�@ʸR@�E�@�{@��#@�O�@ȼj@�bN@��m@��;@���@ǍP@�C�@�E�@���@š�@�x�@�/@ēu@��@�;d@���@¸R@+@�@��@�j@��
@��@�l�@�+@�o@�@��@�M�@��@��@� �@���@���@�C�@�ȴ@�=q@���@�O�@���@��@�Q�@�b@��@��@��H@�v�@�^5@�V@���@���@�p�@��@��@�b@���@�+@��y@���@�ff@�{@��T@��h@�X@�V@���@���@�r�@��@�dZ@�"�@��@��!@�v�@�=q@��@���@�%@���@���@���@��@���@��@���@�\)@�@���@��R@���@�~�@�=q@�{@��@��h@�&�@��@���@��9@�Q�@�b@���@�C�@�
=@���@��!@�V@��@���@�X@��@��j@�z�@�1'@� �@�b@�1@�  @��@��;@���@�@���@�~�@��h@��@��u@��;@���@�dZ@�"�@��R@�^5@�-@��@���@��@�hs@���@�r�@�I�@��w@�;d@���@�M�@�5?@��@���@�`B@�G�@�%@�Ĝ@���@�1'@��w@��P@�l�@�S�@��@��@���@�ȴ@���@�n�@�V@���@�`B@�7L@�%@��9@�Z@�9X@�(�@�b@��;@�dZ@�+@��@��+@�E�@�{@��#@��h@��@��/@���@�Z@�  @���@��m@��F@���@�|�@�33@��R@��+@��@�@��h@��7@��@�x�@�`B@��9@�A�@�1'@�(�@��@���@�C�@��!@�=q@��#@��^@���@�x�@�7L@��@���@���@�bN@��;@��@���@�l�@�
=@���@�M�@�J@�@��@���@���@�Q�@�  @�ƨ@��P@�S�@�+@���@��R@�E�@��T@���@��h@�X@���@��@�Q�@�(�@��@~{@|��@|Z@|1@{�m@{��@{dZ@{C�@z�H@z-@y��@y�^@yX@y�@xbN@w�@w
=@vV@u@u`B@tz�@s�m@s��@st�@sdZ@sS�@sC�@s@r��@r=q@rJ@q�#@q��@qX@q%@pQ�@pb@o�;@o�@o�@nE�@m�T@m@m��@m�@m`B@l�/@l(�@k�
@kƨ@j�H@j�!@jn�@jJ@i��@i�7@i7L@h�`@hbN@g�@g\)@g�@f�R@f�+@e��@e�-@eO�@d�@dj@c��@c��@c"�@b�!@bM�@bJ@a�#@a��@`�@_��@_+@^��@^ff@^E�@]��@]?}@]/@\9X@[�@[o@Z��@Z=q@Y��@Y�#@Yx�@X��@X1'@X �@X �@X  @W��@W�@W|�@W;d@Vȴ@Vv�@V5?@V@U�@U�T@U�@T��@S��@S�@R�H@R��@Rn�@RJ@QX@P��@P�u@P�u@P�u@P �@O�w@O��@O�P@O�P@O+@N5?@M�T@M�-@M�h@MO�@M/@M/@L��@Lz�@K��@K�@K"�@JM�@I�#@Ihs@H�`@H�u@HbN@H1'@G�@Gl�@G;d@F�+@F$�@E@E`B@E?}@E?}@D�@DI�@D1@C�m@C�m@C�m@C�m@C�
@C�F@C��@C@B�!@BM�@BJ@A�@A��@A��@AX@A%@@Ĝ@@�@@A�@?��@?�P@?K�@>��@>��@=�T@=�h@=p�@=/@<�@<��@<Z@<9X@;��@;ƨ@;t�@;dZ@;dZ@;dZ@:�H@:��@:�@9�#@9�^@9�^@9��@9��@9�7@97L@8��@8bN@7�@7;d@6��@6�R@6�+@65?@5�@5�-@5/@4�@4(�@3t�@3S�@3C�@3C�@2��@2n�@2^5@2M�@2-@2�@1��@1��@1G�@0�`@0Ĝ@0�9@0Q�@/|�@.�@.��@.V@.$�@-@-O�@,��@,�D@,I�@,1@+ƨ@+t�@*�@*-@)��@*=q@)x�@)%@(Ĝ@(r�@( �@'�@'�;@'��@'�@'K�@'+@'
=@&�@&�R@&�+@&ff@&$�@%��@%/@$�/@$Z@$�@$1@#�
@#ƨ@#�F@#��@#��@#S�@#o@#@"�H@"��@"M�@"=q@"-@"J@!��@!��@!�@!��@!�^@!��@!X@!%@ Q�@�@��@l�@�@ȴ@v�@5?@�@��@�h@`B@O�@/@�@V@�@�j@z�@(�@��@�
@ƨ@ƨ@�@C�@"�@o@�@�H@��@��@��@�\@~�@n�@=q@��@��@��@hs@X@��@�`@��@�9@��@Ĝ@Ĝ@�u@�@Q�@ �@|�@K�@��@E�@$�@�@�h@`B@�@�-@@��@�@`B@?}@�@��@�@�@�@�@��@�D@�@dZ@C�@33@o@�@�H@��@�!@�!1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�!B	�'B	�!B	�'B	�3B	�!B	�'B	�3B	�!B	�'B	�'B	�-B	�-B	�3B	�?B	�9B	�FB	�LB	�FB	�FB	�FB	�FB	�LB	�^B	�XB	�^B	�jB	�jB	�jB	�jB	�wB	B	B	B	��B	��B	��B	��B	��B	��B	��B	�}B	�}B	�jB	�RB	�?B	�B	��B	�^B	�dB	��B
%B
6FB
��B
�B
�3B
��B
��B5?B@�BQ�BZB_;Bm�B� B�Bt�BZB@�B33B�B%B
��B
�B
��B
�RB
��B
�VB
x�B
l�B
T�B
E�B
9XB
$�B
\B	�B	�B	�qB	�B	�VB	~�B	z�B	o�B	bNB	]/B	W
B	Q�B	I�B	N�B	YB	ZB	YB	\)B	bNB	gmB	k�B	m�B	n�B	o�B	q�B	q�B	v�B	u�B	q�B	q�B	u�B	|�B	�B	�=B	�VB	�\B	�bB	�bB	�oB	�{B	�{B	��B	�B	�B	�'B	�?B	�-B	�'B	�'B	�9B	�?B	�RB	�wB	��B	ÖB	ĜB	��B	��B	��B	�B	�ZB	�B	�B	�B	�yB	�yB	�yB	�yB	�B	�B	�B	��B
B
	7B
hB
�B
�B
�B
 �B
#�B
%�B
)�B
-B
1'B
33B
49B
7LB
8RB
<jB
@�B
C�B
F�B
G�B
G�B
F�B
F�B
E�B
A�B
=qB
9XB
7LB
7LB
8RB
;dB
<jB
>wB
@�B
A�B
@�B
>wB
D�B
C�B
B�B
B�B
C�B
C�B
F�B
F�B
G�B
G�B
H�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
G�B
H�B
E�B
D�B
E�B
E�B
E�B
G�B
H�B
H�B
G�B
E�B
D�B
D�B
C�B
C�B
B�B
B�B
C�B
C�B
C�B
C�B
B�B
B�B
B�B
B�B
B�B
A�B
A�B
@�B
?}B
?}B
>wB
=qB
=qB
<jB
<jB
;dB
:^B
:^B
9XB
8RB
7LB
7LB
7LB
6FB
6FB
6FB
6FB
6FB
5?B
5?B
49B
33B
33B
2-B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
1'B
0!B
/B
.B
-B
,B
-B
-B
-B
,B
,B
,B
,B
+B
+B
)�B
(�B
(�B
'�B
'�B
&�B
&�B
%�B
%�B
$�B
$�B
#�B
#�B
#�B
"�B
"�B
"�B
"�B
"�B
!�B
!�B
!�B
!�B
!�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
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
!�B
 �B
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
"�B
"�B
#�B
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
$�B
#�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
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
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
-B
-B
-B
-B
-B
.B
.B
-B
-B
.B
-B
-B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
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
33B
49B
49B
49B
49B
49B
49B
49B
49B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
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
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
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
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
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
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
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
O�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
S�B
T�B
T�B
T�B
T�B
VB
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
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
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
aHB
bNB
bNB
cTB
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
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
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
o�B
o�B
o�B
n�B
o�B
o�B
o�B
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
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
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
|�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�+B
�+B
�+B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�DB
�DB
�=B
�DB
�DB
�JB
�JB
�PB
�PB
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
�VB
�\B
�\B
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�,B	�DB	�DB	�DB	�>B	�>B	�8B	�>B	�8B	�8B	�8B	�2B	�2B	�B	�B	��B	��B	��B	�B	�B	��B	��B
%�B
�UB
��B
��B
§B
�B$�B08BA�BI�BN�B]FBo�Bp�BdqBI�B08B"�B6B
��B
�B
�SB
��B
�B
��B
~B
h�B
\@B
D�B
5WB
)B
�B	�B	�SB	ŹB	�&B	��B	~B	n�B	j�B	_SB	RB	L�B	F�B	A�B	9oB	>�B	H�B	I�B	H�B	K�B	RB	W"B	[:B	]FB	^MB	_SB	a_B	a_B	f~B	exB	a_B	a_B	exB	l�B	q�B	y�B	~B	B	�B	�B	�$B	�0B	�0B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�8B	�KB	�QB	��B	��B	�vB	ŹB	�B	�4B	�4B	�4B	�.B	�.B	�.B	�.B	�4B	�MB	�kB	�B	�B	��B
B
BB
[B
gB
zB
�B
�B
�B
�B
 �B
"�B
#�B
'B
(B
,B
08B
3KB
6]B
7cB
7cB
6]B
6]B
5WB
1>B
-&B
)B
'B
'B
(B
+B
,B
.,B
08B
1>B
08B
.,B
4QB
3KB
2DB
2DB
3KB
3KB
6]B
6]B
7cB
7cB
8iB
7cB
8iB
8iB
8iB
8iB
8iB
8iB
8iB
8iB
8iB
7cB
7cB
8iB
5WB
4QB
5WB
5WB
5WB
7cB
8iB
8iB
7cB
5WB
4QB
4QB
3KB
3KB
2DB
2DB
3KB
3KB
3KB
3KB
2DB
2DB
2DB
2DB
2DB
1>B
1>B
08B
/2B
/2B
.,B
-&B
-&B
,B
,B
+B
*B
*B
)B
(B
'B
'B
'B
%�B
%�B
%�B
%�B
%�B
$�B
$�B
#�B
"�B
"�B
!�B
!�B
!�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
zB
zB
zB
zB
zB
sB
sB
sB
mB
mB
mB
mB
mB
mB
mB
mB
gB
mB
gB
gB
gB
gB
aB
aB
aB

UB
	OB
	OB
HB
HB
HB
HB
	OB
	OB
	OB
	OB
	OB
HB
HB
HB
HB
HB
HB
HB
HB
HB
	OB
	OB
	OB

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
[B
[B
[B
[B
[B
[B
aB
aB
aB
gB
gB
gB
mB
mB
mB
mB
mB
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
sB
sB
sB
zB
zB
zB
zB
zB
zB
zB
�B
�B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
'B
'B
'B
'B
'B
'B
'B
'B
(B
(B
(B
)B
)B
)B
)B
)B
*B
*B
*B
*B
+B
+B
+B
+B
+B
+B
+B
+B
,B
-&B
-&B
-&B
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
1>B
1>B
1>B
1>B
1>B
2DB
3KB
3KB
3KB
3KB
4QB
4QB
4QB
4QB
4QB
5WB
5WB
5WB
5WB
5WB
6]B
6]B
6]B
7cB
7cB
8iB
9oB
9oB
:vB
:vB
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
;|B
;|B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
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
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
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
RB
RB
RB
P�B
RB
RB
S	B
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
W"B
W"B
W"B
X(B
X(B
X(B
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
Z4B
Z4B
Z4B
Z4B
Z4B
Z4B
Z4B
[:B
\@B
\@B
\@B
]FB
]FB
^MB
^MB
^MB
^MB
^MB
^MB
_SB
_SB
_SB
_SB
_SB
_SB
_SB
_SB
_SB
^MB
_SB
_SB
_SB
_SB
_SB
_SB
_SB
_SB
_SB
`YB
`YB
a_B
a_B
a_B
a_B
a_B
a_B
a_B
beB
beB
ckB
ckB
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
dqB
exB
exB
exB
exB
exB
f~B
f~B
f~B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
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
o�B
o�B
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
p�B
p�B
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
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
z�B
z�B
y�B
z�B
z�B
{�B
{�B
}B
}B
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
~B
B
B
B
B
B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      surface_pressure=-0.61 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     salinity_offset = -0.0159092                                                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                      PSAL ADJUST [dd mm yyyy N S_off stddev] 15 06 2019 151 -0.0159092 0.0003 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                        20200619120033              20200619120033  AO  ARCAADJP                                                                    20200619120033    IP                G�O�G�O�G�O�                AO  ARCAADJS                                                                    20200619120033    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619120033  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619120033  QCF$                G�O�G�O�G�O�0               