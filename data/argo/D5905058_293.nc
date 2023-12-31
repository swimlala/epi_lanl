CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-02-02T12:50:24Z creation;2021-02-02T12:50:26Z conversion to V3.1;2023-06-29T05:47:21Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        x  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  M\   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p,   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �d   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210202125024  20230705041505  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              %A   JA  I2_0675_293                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�[Q�t� 1   @�[T��� @6�1&�y�b�U2a|1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=�fD>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@ָRA	A)AIAiA��HA��HA��HA��HA��HA��HA��HA��HBp�B
p�Bp�Bp�B"p�B*p�B2p�B:p�BBp�BJp�BRp�BZp�Bbp�Bjp�Brp�Bzp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC �)C�)C�)C�)C�)C
�)C�)C�)C�)C�)C�)C��C�)C�)C�)C�)C �)C"�)C$��C&�)C(�)C*�)C,�)C.�)C0�)C2�)C4�)C6�)C8�)C:�)C<�)C>�)C@�)CB�)CD�)CF�)CH�)CJ�)CL�)CN�)CP�)CR�)CT�)CV�)CX�)CZ�)C\�)C^�)C`�)Cb�)Cd�)Cf�)Ch�)Cj�)Cl�)Cn�)Cp�)Cr�)Ct�)Cv�)Cx�)Cz�)C|�)C~�)C�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�NC�ND '
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
D��D'
D�
D'
D�
D'
D�qD	'
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
D=-qD=�qD>'
D>�
D?'
D?�
D@'
D@��DA'
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
Db �Db�
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
D��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D�D�ӅD��D�S�DÓ�D�ӅD��D�S�Dē�D�ӅD��D�S�Dœ�D�ӅD��D�S�DƓ�D�ӅD��D�S�DǓ�D�ӅD��D�S�Dȓ�D�ӅD��D�S�Dɓ�D�ӅD��D�S�Dʓ�D�ӅD��D�S�D˓�D�ӅD��D�S�D̓�D�ӅD��D�S�D͓�D�ӅD��D�S�DΓ�D�ӅD��D�S�Dϓ�D�ӅD��D�S�DГ�D�ӅD��D�S�Dѓ�D�ӅD��D�S�Dғ�D�ӅD��D�S�Dӓ�D�ӅD��D�S�Dԓ�D�ӅD��D�S�DՓ�D�ӅD��D�S�D֓�D�ӅD��D�S�Dד�D�ӅD��D�S�Dؓ�D�ӅD��D�S�Dٓ�D�ӅD��D�S�Dړ�D�ӅD��D�S�Dۓ�D�ӅD��D�S�Dܓ�D�ӅD��D�S�Dݓ�D�ӅD��D�S�Dޓ�D�ӅD��D�S�Dߓ�D�ӅD��D�S�D���D�ӅD��D�S�DᓅD�ӅD��D�S�DⓅD�ӅD��D�S�D㓅D�ӅD��D�S�D䓅D�ӅD��D�S�D哅D�ӅD��D�V�D擅D�ӅD��D�S�D瓅D�ӅD��D�S�D蓅D�ӅD��D�S�D铅D�ӅD��D�S�D꓅D�ָD��D�S�D듅D�ӅD��D�S�D쓅D�ӅD��D�S�D퓅D�ӅD��D�S�DD�ӅD��D�S�DD��RD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D�D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D�ӅD��D�S�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A��!A��9A��9A��9A��-A��9A��FA��RA��RA��FA��9A��-A��-A��-A��9A��FA��FA��RA��RA��RA��RA��^A��RA��jA��jA��9A��9A��RA���A��hA��\A�v�A�t�A�t�A�v�A�~�A�z�A�v�A�l�A�1'A��^A�  A��TA�M�A�I�A�JA��A�jA�t�A�E�A�E�A���A��A���A�-A��;A�7LA��^A��`A�ĜA��A�`BA���A�oA���A�M�A�ZA�ƨA���A�=qA�|�A��A�?}A��`A���A��\A��DA�9XA��hA�ĜA�  A���A�|�A��;A�ffA�bNA���A��A�A��uA�
=A��A�ffA�VA�v�A�bNA��;A�ƨA��`A�n�A�oA��!A���A��^A��!A���A�S�A���A��DA���A���A��A�dZA�-A~�uAx�RAt�DAr�Ap��Ao��Ak�TAit�Af�HAa�A^^5A[`BAX�9AU�AP5?AM33ALQ�AKdZAK�AJZAI"�AG��AE�^AC�-AB �A@$�A>r�A<(�A:�\A:I�A9XA7�A5�#A533A4-A1��A0{A/�;A.��A.bNA.M�A-|�A,�uA,1'A*��A)7LA(bNA'"�A%��A$�HA#�A#%A"�RA" �A ��A�^AhsA�A1AK�AZAp�A �A�/A�;A��A%A�uA�PA$�A��A�;A"�A%A�FAQ�A��A
r�A	�A	�
A	��A	dZA��A�;A&�AVA��A�9A�A�AA�A�7A ��@���@�~�@��/@��@�E�@�(�@�~�@�D@���@�Z@�@���@��^@홚@�1'@�p�@�@噚@�Ĝ@�P@�7L@ޟ�@��#@��`@�(�@�5?@��/@�r�@׶F@�^5@���@�Z@�
=@љ�@Л�@Ϯ@�p�@̬@�r�@�  @�K�@�o@�v�@�O�@�A�@ǥ�@�\)@�^5@��/@��@�dZ@���@�~�@�V@�A�@��;@�dZ@��h@��j@�1@��P@�S�@���@���@�^5@��T@���@�O�@���@�|�@�33@�@��7@��@��@��w@�|�@���@���@��h@�hs@�X@�?}@�%@�Z@��
@�~�@�v�@��@�7L@��u@���@�;d@�+@�ȴ@��\@�ff@��^@�7L@��@��@��j@�9X@��@��y@��\@�5?@���@���@���@��@�X@�&�@��`@��j@��u@�(�@�ƨ@�|�@�33@���@��@�@�@���@�G�@�&�@��@��/@���@���@� �@��
@��;@��F@���@�t�@�S�@�
=@���@���@��@��y@��@���@��!@�ff@��@�@�@�/@��`@���@�Ĝ@���@�9X@���@��P@�l�@�;d@���@�E�@���@���@��-@���@��7@�`B@�/@�V@��`@���@�r�@�9X@���@�ƨ@��P@�S�@�C�@�33@��@�
=@�@��@�ȴ@���@�5?@��@��^@���@�x�@�G�@�%@��/@�Ĝ@�Ĝ@��9@��@���@�z�@��@��F@�\)@�+@�o@���@��!@���@�~�@�ff@�=q@��@�J@�@���@��^@��7@�hs@��@��j@��u@�r�@�1'@��;@��@���@�\)@��@���@���@���@��\@�=q@��T@�@���@�?}@���@��@���@���@��@�r�@�bN@�9X@�1@��;@�l�@�C�@�o@��R@��+@�ff@�M�@��@���@��^@��@�x�@�G�@���@�r�@���@�dZ@�C�@�+@�@���@��H@��R@�=q@�@��-@��-@���@�`B@�?}@��@�Ĝ@�j@�  @��w@�l�@�;d@�+@��@���@�^5@�$�@��^@�`B@�&�@�%@��`@�Ĝ@��9@���@��D@�9X@�  @�@��@�P@K�@+@~�+@~E�@}`B@}�@|��@|z�@|(�@{��@{�F@{t�@z�H@z=q@y�@y��@yX@x�9@xQ�@xA�@w�;@w�P@w
=@v��@v�+@vv�@vff@v$�@v@u�@u�-@u�@u�@t��@t��@tz�@s�F@sC�@s"�@r��@r�\@rn�@q�#@qX@pĜ@p  @o��@ol�@oK�@n�y@nE�@nE�@n5?@n$�@m�T@m�@mO�@mV@l�j@lj@kƨ@j�H@j~�@j=q@j-@j�@i��@i�^@iX@h�u@hb@g�;@g�@g��@g��@g�P@g�@f�+@f�+@fE�@ep�@d��@d�/@d�/@d�j@d��@dZ@d1@c�F@c@b��@b^5@a�#@a��@a�7@ahs@aX@a&�@`Ĝ@`1'@_�@_;d@_�@^�@^ff@^{@]�@]��@]�-@]/@\z�@\�@[�F@[S�@[33@[o@Z�H@Z��@Z��@Z�@Y��@Y�^@Y��@Yx�@X��@XQ�@W�P@WK�@WK�@W+@V��@V�+@V$�@U��@UO�@T��@T�j@T(�@S�m@S�
@Sƨ@Sƨ@Sƨ@Sƨ@Sƨ@S��@St�@SdZ@SC�@So@R�@R�H@R��@Rn�@RJ@Q�#@QG�@Q�@P��@P��@PA�@O�;@O��@O\)@O
=@N�R@Nff@M��@M?}@MV@L�@L�@LZ@L9X@K��@J�@J�H@J�!@J�@I��@I�7@IG�@H��@H �@G�@GK�@Fȴ@F��@F$�@E�T@E�-@Ep�@E`B@EO�@D�D@C�m@C��@CdZ@C"�@C@B��@Bn�@A��@@��@@Ĝ@@�@@Q�@@b@?��@?|�@?;d@>�y@>�+@>$�@>@=��@=�@<��@<�j@;�F@;t�@;C�@;"�@:�!@:M�@:J@9��@9�@8�9@8b@7K�@6ff@6E�@6@5��@5@5�-@5O�@5�@4�@4I�@3�
@3�@3dZ@333@3@2��@2��@2n�@2M�@2=q@2-@1��@1�@1��@1��@1�7@1hs@1X@1G�@1&�@0��@0�9@0�@01'@0 �@0  @/��@/�@/\)@/+@/�@.��@.�+@.v�@.E�@-�@-@-�h@-`B@-O�@-?}@-�@,��@,�/@,�D@,Z@,I�@,�@+ƨ@+�@+"�@*��@*~�@*=q@*J@)��@)X@)&�@(��@(�u@(A�@'��@'|�@';d@'+@'\)@'+@&�R@&V@%�-@%�h@%?}@$��@$��@$j@$I�@$�@#�
@#t�@#o@"��@"�\@"n�@"J@!�^@!�7@!x�@!X@!G�@ ��@ ��@ �u@ r�@ A�@�w@|�@\)@;d@�@�y@��@�+@v�@5?@$�@{@{@@@�@�-@p�@`B@O�@�@V@��@�j@9X@�@��@�
@�F@�@�@�@�@dZ@33@�@��@^5@^5@=q@J@�#@��@��@��@x�@x�@x�@x�@hs@x�@X@G�@�@�@�@%@�`@��@Q�@1'@ �@b@b@ �@b@�@�;@�@l�@;d@�@�y@ȴ@�R@��@ff@E�@$�@�T@��@�h@/@�j@�@j@1@��@33@"�@o@�@��@�!@�!@�\@=q@��@�#@x�@X@&�@%@�`@��@�@r�@bN@bN@A�@ �@  @�@��@��@;d@�@�y@�@�@�R@v�@$�@�@�-@�@?}@��@�/@�j@�@��@z�@I�@(�@1@�F@��@�@t�@dZ@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A��!A��9A��9A��9A��-A��9A��FA��RA��RA��FA��9A��-A��-A��-A��9A��FA��FA��RA��RA��RA��RA��^A��RA��jA��jA��9A��9A��RA���A��hA��\A�v�A�t�A�t�A�v�A�~�A�z�A�v�A�l�A�1'A��^A�  A��TA�M�A�I�A�JA��A�jA�t�A�E�A�E�A���A��A���A�-A��;A�7LA��^A��`A�ĜA��A�`BA���A�oA���A�M�A�ZA�ƨA���A�=qA�|�A��A�?}A��`A���A��\A��DA�9XA��hA�ĜA�  A���A�|�A��;A�ffA�bNA���A��A�A��uA�
=A��A�ffA�VA�v�A�bNA��;A�ƨA��`A�n�A�oA��!A���A��^A��!A���A�S�A���A��DA���A���A��A�dZA�-A~�uAx�RAt�DAr�Ap��Ao��Ak�TAit�Af�HAa�A^^5A[`BAX�9AU�AP5?AM33ALQ�AKdZAK�AJZAI"�AG��AE�^AC�-AB �A@$�A>r�A<(�A:�\A:I�A9XA7�A5�#A533A4-A1��A0{A/�;A.��A.bNA.M�A-|�A,�uA,1'A*��A)7LA(bNA'"�A%��A$�HA#�A#%A"�RA" �A ��A�^AhsA�A1AK�AZAp�A �A�/A�;A��A%A�uA�PA$�A��A�;A"�A%A�FAQ�A��A
r�A	�A	�
A	��A	dZA��A�;A&�AVA��A�9A�A�AA�A�7A ��@���@�~�@��/@��@�E�@�(�@�~�@�D@���@�Z@�@���@��^@홚@�1'@�p�@�@噚@�Ĝ@�P@�7L@ޟ�@��#@��`@�(�@�5?@��/@�r�@׶F@�^5@���@�Z@�
=@љ�@Л�@Ϯ@�p�@̬@�r�@�  @�K�@�o@�v�@�O�@�A�@ǥ�@�\)@�^5@��/@��@�dZ@���@�~�@�V@�A�@��;@�dZ@��h@��j@�1@��P@�S�@���@���@�^5@��T@���@�O�@���@�|�@�33@�@��7@��@��@��w@�|�@���@���@��h@�hs@�X@�?}@�%@�Z@��
@�~�@�v�@��@�7L@��u@���@�;d@�+@�ȴ@��\@�ff@��^@�7L@��@��@��j@�9X@��@��y@��\@�5?@���@���@���@��@�X@�&�@��`@��j@��u@�(�@�ƨ@�|�@�33@���@��@�@�@���@�G�@�&�@��@��/@���@���@� �@��
@��;@��F@���@�t�@�S�@�
=@���@���@��@��y@��@���@��!@�ff@��@�@�@�/@��`@���@�Ĝ@���@�9X@���@��P@�l�@�;d@���@�E�@���@���@��-@���@��7@�`B@�/@�V@��`@���@�r�@�9X@���@�ƨ@��P@�S�@�C�@�33@��@�
=@�@��@�ȴ@���@�5?@��@��^@���@�x�@�G�@�%@��/@�Ĝ@�Ĝ@��9@��@���@�z�@��@��F@�\)@�+@�o@���@��!@���@�~�@�ff@�=q@��@�J@�@���@��^@��7@�hs@��@��j@��u@�r�@�1'@��;@��@���@�\)@��@���@���@���@��\@�=q@��T@�@���@�?}@���@��@���@���@��@�r�@�bN@�9X@�1@��;@�l�@�C�@�o@��R@��+@�ff@�M�@��@���@��^@��@�x�@�G�@���@�r�@���@�dZ@�C�@�+@�@���@��H@��R@�=q@�@��-@��-@���@�`B@�?}@��@�Ĝ@�j@�  @��w@�l�@�;d@�+@��@���@�^5@�$�@��^@�`B@�&�@�%@��`@�Ĝ@��9@���@��D@�9X@�  @�@��@�P@K�@+@~�+@~E�@}`B@}�@|��@|z�@|(�@{��@{�F@{t�@z�H@z=q@y�@y��@yX@x�9@xQ�@xA�@w�;@w�P@w
=@v��@v�+@vv�@vff@v$�@v@u�@u�-@u�@u�@t��@t��@tz�@s�F@sC�@s"�@r��@r�\@rn�@q�#@qX@pĜ@p  @o��@ol�@oK�@n�y@nE�@nE�@n5?@n$�@m�T@m�@mO�@mV@l�j@lj@kƨ@j�H@j~�@j=q@j-@j�@i��@i�^@iX@h�u@hb@g�;@g�@g��@g��@g�P@g�@f�+@f�+@fE�@ep�@d��@d�/@d�/@d�j@d��@dZ@d1@c�F@c@b��@b^5@a�#@a��@a�7@ahs@aX@a&�@`Ĝ@`1'@_�@_;d@_�@^�@^ff@^{@]�@]��@]�-@]/@\z�@\�@[�F@[S�@[33@[o@Z�H@Z��@Z��@Z�@Y��@Y�^@Y��@Yx�@X��@XQ�@W�P@WK�@WK�@W+@V��@V�+@V$�@U��@UO�@T��@T�j@T(�@S�m@S�
@Sƨ@Sƨ@Sƨ@Sƨ@Sƨ@S��@St�@SdZ@SC�@So@R�@R�H@R��@Rn�@RJ@Q�#@QG�@Q�@P��@P��@PA�@O�;@O��@O\)@O
=@N�R@Nff@M��@M?}@MV@L�@L�@LZ@L9X@K��@J�@J�H@J�!@J�@I��@I�7@IG�@H��@H �@G�@GK�@Fȴ@F��@F$�@E�T@E�-@Ep�@E`B@EO�@D�D@C�m@C��@CdZ@C"�@C@B��@Bn�@A��@@��@@Ĝ@@�@@Q�@@b@?��@?|�@?;d@>�y@>�+@>$�@>@=��@=�@<��@<�j@;�F@;t�@;C�@;"�@:�!@:M�@:J@9��@9�@8�9@8b@7K�@6ff@6E�@6@5��@5@5�-@5O�@5�@4�@4I�@3�
@3�@3dZ@333@3@2��@2��@2n�@2M�@2=q@2-@1��@1�@1��@1��@1�7@1hs@1X@1G�@1&�@0��@0�9@0�@01'@0 �@0  @/��@/�@/\)@/+@/�@.��@.�+@.v�@.E�@-�@-@-�h@-`B@-O�@-?}@-�@,��@,�/@,�D@,Z@,I�@,�@+ƨ@+�@+"�@*��@*~�@*=q@*J@)��@)X@)&�@(��@(�u@(A�@'��@'|�@';d@'+@'\)@'+@&�R@&V@%�-@%�h@%?}@$��@$��@$j@$I�@$�@#�
@#t�@#o@"��@"�\@"n�@"J@!�^@!�7@!x�@!X@!G�@ ��@ ��@ �u@ r�@ A�@�w@|�@\)@;d@�@�y@��@�+@v�@5?@$�@{@{@@@�@�-@p�@`B@O�@�@V@��@�j@9X@�@��@�
@�F@�@�@�@�@dZ@33@�@��@^5@^5@=q@J@�#@��@��@��@x�@x�@x�@x�@hs@x�@X@G�@�@�@�@%@�`@��@Q�@1'@ �@b@b@ �@b@�@�;@�@l�@;d@�@�y@ȴ@�R@��@ff@E�@$�@�T@��@�h@/@�j@�@j@1@��@33@"�@o@�@��@�!@�!@�\@=q@��@�#@x�@X@&�@%@�`@��@�@r�@bN@bN@A�@ �@  @�@��@��@;d@�@�y@�@�@�R@v�@$�@�@�-@�@?}@��@�/@�j@�@��@z�@I�@(�@1@�F@��@�@t�@dZ@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BcTBcTBcTBcTBcTBcTBdZBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBdZBcTBcTBdZBdZBcTBdZBdZBdZBdZBdZBe`BdZBdZBgmBhsBhsBk�Bk�Bk�BjBhsBiyBiyBjBx�B�DB��B��BƨB�HB�mB�B��B
=B	7B1BVB	7B��B��B��B��BDB�B&�B-B/B1'B0!B/B-B,B)�B(�B'�B$�B!�B�B{BhBVBJBBB��B�B�B�B�B�`B�
B�qB�?B�B��B��B��B��B��B�%Bp�BN�B-B�B�B�BoB+B
�B
�NB
��B
�3B
�B
��B
�DB
~�B
{�B
m�B
@�B
2-B
hB	�B	�#B	��B	ÖB	�B	��B	�+B	hsB	R�B	@�B	/B	�B	B�B�B�`B�ZB�BB�#B��B��BB�XB�'B��B��B��B��B��B�oB�\B�JB�7B�B|�B{�By�Bw�Bv�Bv�Br�Bo�Bn�BgmBdZBdZB`BB^5B]/B[#BYBXBW
BR�BQ�BP�BN�BL�BJ�BG�BE�BB�BA�BC�BI�BH�BG�BB�B>wB9XB8RB33B1'B0!B/B0!B/B.B.B.B.B-B,B,B+B,B(�B(�B)�B)�B)�B)�B(�B(�B'�B(�B)�B(�B+B,B,B-B.B.B-B/B1'B1'B2-B1'B33B7LB8RB8RB:^B:^B?}BA�BB�BD�BG�BJ�BI�BJ�BK�BK�BL�BP�BR�BT�BVBXBYBYBZB[#B\)B\)B_;BaHBcTBdZBffBgmBk�Bn�Bp�Bt�Bz�B|�B|�B|�B|�B|�B|�B~�B�B�B�B�B�=B�=B�=B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�FB�^B��BǮBɺB��B��B��B��B�B�B�B�#B�5B�NB�`B�mB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	+B	+B		7B	JB	VB	\B	bB	bB	hB	hB	�B	�B	�B	 �B	!�B	"�B	$�B	'�B	(�B	(�B	(�B	)�B	)�B	)�B	+B	-B	0!B	1'B	33B	9XB	<jB	<jB	=qB	>wB	A�B	F�B	F�B	H�B	I�B	K�B	S�B	W
B	YB	ZB	[#B	[#B	\)B	^5B	`BB	bNB	e`B	ffB	hsB	k�B	m�B	p�B	r�B	r�B	t�B	u�B	v�B	v�B	x�B	y�B	z�B	~�B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�=B	�DB	�JB	�JB	�PB	�VB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�9B	�FB	�LB	�XB	�^B	�^B	�dB	�qB	�}B	�}B	��B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�/B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�TB	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
	7B
	7B
	7B

=B

=B
DB
DB
JB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
hB
hB
oB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
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
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
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
/B
/B
/B
0!B
0!B
0!B
0!B
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
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
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
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
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
H�B
H�B
H�B
I�B
I�B
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
K�B
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
P�B
P�B
P�B
Q�B
Q�B
Q�B
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
XB
XB
XB
XB
XB
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
]/B
]/B
^5B
^5B
_;B
`BB
`BB
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
aHB
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
ffB
ffB
ffB
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
gmB
hsB
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
jB
iyB
iyB
iyB
iyB
iyB
hsB
hsB
iyB
iyB
iyB
jB
jB
iyB
jB
jB
jB
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
n�B
n�B
n�B
n�B
n�B
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
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bc Bc BcBc Bc Bc Bd&Bc Bc Bc Bc Bc Bc Bc Bc Bc Bc Bd&Bc Bc Bd&Bd&Bc Bd&Bd&Bd&Bd&Bd@Be,Bd@BdZBgRBhXBhsBkQBkQBkQBjKBhXBi_Bi�Bk6BzB�PB�eB̳B�B�|B��B�BB�B�B^B�BB�B��B��B�JB�B�B(sB.IB0�B2�B33B3�B.�B,�B*eB)�B)_B&B"�B)B�BoB�B�B�BuB�$B��B�B��B�B�mB�eB�(B�zB��B��B��B��B�B��B�lBu�BR�B.�B�BxB�B{B	�B
�nB
�B
��B
��B
�cB
�kB
��B
�OB
�B
rGB
CGB
8B
�B	�'B	��B	�:B	�zB	�UB	��B	�B	k�B	V�B	DB	3�B	# B	B�B�QB��B�`B��B��B�MB�BāB��B�3B�DB�NB�1B�
B��B��B�HB��B��B��B}VB|�Bz*BxBw�Bw�BshBqvBp;Bh�Be�Be�BabB_;B^B[�BZBYBXBS[BR�BQ�BO�BM�BK�BI7BF�BC{BA�BDMBJXBJ#BI7BDB?�B:�B:�B4�B2�B0�B0;B0oB/B.IB.}B.�B.�B-�B,�B,�B,B,�B)�B*KB*�B*�B+B*�B)�B)yB)B*0B+B*0B,qB,�B,WB-wB.}B.IB.B0�B2GB2B2�B2-B4�B8�B8�B8�B:�B;B@ BA�BB�BESBHBKxBJrBKxBLJBLdBM�BQBR�BUBV9BXBYeBY�BZ�B[WB\]B\�B_�Ba�Bc�Bd�Bf�Bh$Bk�Bn�Bp�Bu�B{0B}"B}B|�B|�B|�B}BB�B� B�[B��B�=B�XB��B��B��B�gB��B��B��B��B��B��B��B��B��B�B�`B��B�5B�hB��B��B��B�zBɺB̳B��B�B��B��B��B�B�=B�jB�hB�`B�mB�eB�kB�wB�iB��B�B�B��B��B��B��B	 B	B	-B	9B	�B	B		B	0B	"B	(B	HB	B	NB	�B	mB	_B	�B	 �B	!�B	"�B	$�B	'�B	(�B	(�B	(�B	)�B	)�B	)�B	+B	,�B	/�B	1'B	3MB	9>B	<6B	<6B	=VB	>wB	A�B	FtB	F�B	H�B	I�B	K�B	S�B	V�B	X�B	Y�B	Z�B	[	B	\B	^B	`'B	b4B	eFB	fLB	hXB	kkB	mwB	p�B	r|B	r|B	t�B	u�B	v�B	v�B	x�B	y�B	z�B	~�B	��B	��B	��B	��B	��B	��B	�B	�	B	��B	�B	�B	�0B	�PB	�VB	�\B	�@B	�MB	�gB	�YB	�_B	�eB	�kB	�qB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	�B	�B	�2B	�$B	�*B	�*B	�JB	�qB	�HB	�cB	��B	�uB	�aB	�gB	āB	�mB	�tB	�tB	ƎB	�zB	ǔB	ȴB	˒B	̳B	͹B	οB	ϫB	бB	��B	ҽB	��B	��B	��B	��B	�$B	�B	�7B	�/B	��B	�B	�B	��B	�B	�-B	�HB	�:B	�FB	�B	�2B	�RB	�>B	�XB	�sB	�yB	�eB	�qB	�wB	�cB	�cB	�}B	�B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
1B
	B
�B
	B
	�B

#B
)B
)B
JB
B
"B
"B
<B
<B
B
B
(B
(B
BB
.B
.B
HB
NB
hB
oB
@B
@B
@B
@B
@B
FB
aB
{B
gB
SB
SB
9B
9B
SB
SB
�B
_B
_B
�B
eB
kB
QB
kB
kB
kB
qB
�B
�B
�B
�B
�B
�B
pB
�B
�B
pB
�B
 �B
!�B
!�B
"�B
"�B
"�B
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
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
+�B
+�B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
0B
1B
1'B
1�B
1�B
1�B
2B
1�B
2�B
2�B
3B
4B
4B
4B
5B
5B
5B
5%B
6B
5�B
6+B
6+B
7B
7B
72B
8B
8B
8B
9>B
9$B
9$B
9>B
9>B
:B
:*B
:B
:*B
:*B
;0B
;0B
;dB
;JB
<6B
<6B
<6B
<6B
=<B
=VB
=qB
>]B
>BB
?HB
?HB
?HB
?HB
?HB
@OB
@OB
@iB
AUB
AUB
B[B
BuB
CaB
C{B
C�B
DMB
DgB
DgB
D�B
DgB
ESB
E�B
F�B
F�B
F�B
F�B
G�B
GzB
H�B
H�B
H�B
HfB
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
JrB
J�B
J�B
K^B
K�B
K�B
KxB
K�B
KxB
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
N�B
N�B
N�B
N�B
N�B
N�B
OvB
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
Q�B
Q�B
Q�B
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
T�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
\B
\�B
\�B
\�B
]B
\�B
\�B
^B
]�B
_B
_�B
_�B
_�B
_�B
_�B
`B
`B
`B
_�B
`B
aB
`�B
aB
aB
aB
aB
`�B
aB
aB
bB
a�B
a�B
a�B
bB
bB
bB
bB
c B
cB
c B
c B
c B
cB
c B
cB
cB
cB
c�B
d&B
dB
dB
eB
eB
e,B
eB
f2B
f2B
f2B
f2B
f2B
fB
fB
f2B
fB
fB
gB
gRB
g8B
gRB
h>B
h>B
h>B
h>B
h$B
h>B
iDB
iDB
iDB
iDB
iDB
iDB
iDB
iDB
j0B
iDB
iDB
i_B
i_B
i*B
h$B
h>B
iDB
iDB
i*B
jB
jeB
iDB
jKB
jKB
jeB
kkB
kQB
kQB
kQB
kQB
kQB
l"B
l=B
l=B
lWB
l=B
lqB
lWB
lqB
m]B
m]B
m]B
ncB
nIB
nIB
ncB
ncB
oiB
oiB
oiB
oiB
poB
poB
poB
poB
pUB
poB
q[B
qvB
qvB
qvB
qvB
r|B
r|B
raB
raB
raB
ra111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.61(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202102080032312021020800323120210208003231202306231727292023062317272920230623172729202102090022462021020900224620210209002246  JA  ARFMdecpA19c                                                                20210202215014  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210202125024  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210202125025  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210202125025  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210202125026  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210202125026  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210202125026  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210202125026  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210202125026  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210202125026                      G�O�G�O�G�O�                JA  ARUP                                                                        20210202125233                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20210202153514  CV  JULD            G�O�G�O�F�ڎ                JM  ARGQJMQC2.0                                                                 20210202153514  CV  JULD_LOCATION   G�O�G�O�F�ڻ                JM  ARGQJMQC2.0                                                                 20210202153514  CV  LATITUDE        G�O�G�O�A��/                JM  ARCAJMQC2.0                                                                 20210207153231  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210207153231  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20210208152246  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082729  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041505                      G�O�G�O�G�O�                