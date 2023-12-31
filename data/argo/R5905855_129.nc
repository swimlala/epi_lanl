CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-08-25T21:42:21Z creation;2022-08-25T21:42:21Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220825214221  20220825215658  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�骏�@1   @����@.#S����c˅�Q�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BY33B_33Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C833C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�C3Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBY{B_{Bg�HBo�HBw�HBz�B��B��B��B��B��B��B��B��B��B�#�B�#�B��qB��B��B��B��B��B��B��B�#�B�#�B׽qB��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�C�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC8+�C9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]޸C_�RCa�RCc�RCe�RCh�Ci�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D)zD)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�B=D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D��=D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D¿
D��
D�?
D�
Dÿ
D��
D�?
D�
DĿ
D��
D�?
D�
Dſ
D��
D�?
D�
Dƿ
D��
D�?
D�
Dǿ
D��
D�?
D�
Dȿ
D��
D�?
D�
Dɿ
D��
D�B=D�
Dʿ
D��
D�?
D�
D˿
D��
D�?
D�
D̿
D��
D�?
D�
DͿ
D��
D�?
D�
Dο
D��
D�?
D�
DϿ
D��
D�?
D�
Dп
D��
D�?
D�
Dѿ
D��
D�?
D�
Dҿ
D��
D�?
D�
Dӿ
D��
D�?
D�
DԿ
D��
D�?
D�
Dտ
D��
D�?
D�
Dֿ
D��
D�?
D�
D׿
D��
D�?
D�
Dؿ
D��
D�?
D�
Dٿ
D��
D�?
D�
Dڿ
D��
D�?
D�
Dۿ
D��
D�?
D�
Dܿ
D��
D�?
D�
Dݿ
D��
D�?
D�
D޿
D��
D�?
D�
D߿
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D�
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�{�D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�iDA�poA�t�A�uZA�v`A�xA�y>A�zA�y�A�y�A�{A�~�Aց�AւAփAփGAփGAփAքAփ�AօAքA�x8A�g�A�Z�A�7�A�	A�یA�w�A�ffA�(A�@�Aґ�A���A� �A�ΥA�~(A�.IA��8A���A�X�A�4AʖSA�̘A�3�AƉA�gmA�h>A��]A��A�o�A�[WA���A���A��/A�f�A��DA��iA� �A�A�}VA��	A��A�/�A���A�7�A���A���A�&�A���A�|�A��A���A���A�;dA�OBA��;A�IA��A�CaA��0A��xA��\A���A�]�A�f2A��]A��A�;�A�A��A��A�{�Ap;A{PHAw�]Av��Ar|�Al��Aj!�Ae��A_��A\�{AZ��AW��AT�	AO�pAL;AJAH�mAFM�AB�LAA�A?(�A=�9A<��A;aA:n�A7�+A5@OA4A21'A1VA18A/�A,�AA,�A+$A*��A*B[A(�}A'N<A&ffA$��A"�fA"l�A"F�A!+�A _�A�KA��A�$A-�A�A�A`�A��A,�A7LA}VA!�A�=A��AA�A�A�#AĜA��AB[A��A
�A+kAS&A�A�A$tA��A��A�_A#�A��A.IA�A&�A�'A��AZ�A�Ai�A#�A
��A
($A	!�A�RAIRAݘA�DA�VA~�AYA��A�OAg8AJA��A`�AK�A ��A `�A dZA &@���@�H�@�Mj@�V@�^5@���@���@��A@���@���@��v@���@��@�c�@���@��@�Z�@�:�@���@�֡@�Q@��@�S&@��[@���@�'�@��'@�?�@�u@�@��?@���@�m]@�4@���@�V@�J@�F@�"@��@�!@��@�	@���@���@��@��6@�͟@��9@ݳ�@ܐ.@�]d@���@ܰ!@�i�@�Dg@��@ڷ�@��d@�~(@�`�@�J@۷@�'�@��@ؕ@��@ׯ�@�S&@���@�0U@խC@�33@ԇ�@Ӳ-@�%F@��`@ҹ�@��X@Ҟ�@�Xy@��#@�  @��@�S�@й�@�kQ@�ϫ@��@���@�خ@υ�@Ο�@��]@�w2@�2a@��@ʏ\@���@���@ǲ�@�=�@�҉@��p@�tT@ż@Ş�@��@ļj@��@��@â�@��@��5@�Ɇ@��@�H@���@��V@�T�@���@�e�@�:*@��@�ff@��7@���@� �@�ѷ@��n@�Z@��@�
=@�kQ@���@�-@��
@��@��@�i�@�*�@��
@���@�L�@��D@�($@���@���@�O@��@��@��}@�PH@�1�@�x@��z@���@�6z@��@�خ@�P�@��@��.@�@�@���@���@��/@�W�@��@�&�@��A@�V�@��>@���@���@�\�@�A�@�@���@��@�GE@�!�@���@�U2@��=@���@���@��@�a�@��@���@��@��r@��=@�G�@�A�@��@�h
@�7�@��'@��@��@���@��r@�%�@��C@��z@���@�&@���@���@��*@��"@�O�@���@�r�@�7@� �@��K@�}�@�IR@�&@��|@�i�@��@��@�@���@�4@���@��\@�Z�@�1�@��@���@���@�Mj@��@��@�~(@�J@��h@�?}@���@���@��A@�#:@�ƨ@��S@�V@���@�q@�M@�+k@��@��@��@��@�\)@�9�@�V@���@�/�@��@��#@�s@�f�@���@�m�@��@��w@�j@�rG@�'�@�Ĝ@�֡@���@�z�@�Ov@��@�ݘ@���@�9�@��@��v@�{�@�PH@��a@��@�[�@�p;@�a|@��@��@�\)@�Ɇ@��o@��@��f@�e�@�=@��@���@�H�@�	@�
�@���@��~@�s�@�9�@���@���@�j@�V@�6�@��@���@��@�[W@�(�@��U@�kQ@�M@���@��-@���@�o�@�E9@� \@��"@��e@�n�@�Xy@�*�@��@�p�@�S�@�A�@���@�n�@�O@�G@��@��j@��V@�iD@�q@��K@�͟@��e@��@�GE@��@~�b@~5?@}�)@}G�@|z�@{��@{33@z�s@z��@z:*@y��@x�$@xz�@x?�@w�@w!-@v� @u��@uO�@t�5@t��@t�@t�@tz�@r�s@q��@p��@pc�@o�@nߤ@n��@n@�@m��@l�E@lC-@k�[@kdZ@j��@j?@i�@iQ�@i*0@i;@hѷ@h�@g|�@f��@f�@e�-@e�n@ehs@e!�@e�@dz�@c�&@c�@c�@b~�@a�@a\�@aIR@`�v@`�9@`��@`Q�@_خ@_�	@_]�@_4�@^�R@]�)@]�@\ی@\�p@\�?@\��@\֡@\��@\Q�@[�@Z�@Z��@Z��@Zh
@Z$�@Y�9@Y(�@X�@X��@Xg8@X2�@W�0@W@O@W&@W�@Vc @V�@U��@T�K@T~(@T1@S��@R�8@R��@Rd�@RR�@Q�n@Pr�@P �@O˒@Ov`@Oa@O)_@N�\@N�A@Nn�@N^5@N@�@M�T@M��@M�'@M-w@L�f@L�v@L�_@L�@Lz�@L7�@K�@K��@K��@K�@J��@I�z@I��@I/@H�U@HD�@G�+@G�g@G�@GY@F��@F@Ehs@D�5@DM@C��@C��@Cj�@CMj@C�@B��@B{@A8�@@�`@@�@@��@@`�@@<�@@'R@?ݘ@?a@>�c@>��@>Ta@>J@=��@=f�@=+�@<��@<�/@<��@<��@<H@<�@<�@;��@;�@;��@;e�@:�@:�@:��@:3�@:�@9�>@9�@9�"@9w2@9Vm@9!�@8�z@8|�@7��@7�K@7��@7P�@7�@6͟@6l�@6C�@5�n@5N<@5@@4��@4�@4�@4�U@4l"@49X@3��@3�@@3;d@3(@2��@2�@2��@2ff@2
�@1��@1��@1Y�@1&�@1�@1;@0�5@0Ɇ@0�@0�D@0l"@0<�@/�@/��@/b�@/F�@/1�@.ff@.�@-�o@-�=@-B�@,�@,|�@,1'@+��@+��@+dZ@+�@*��@*�6@*�r@*V@*5?@*e@)�>@)�-@)c�@(�P@(�?@(`�@((�@(  @'�0@'�4@&�"@&�m@&�h@&��@&)�@%��@%��@%<6@$�@$�I@$r�@$"h@#�@#��@#a@"�@"��@"�@!�T@!�@!w2@!S&@!5�@ �P@ ��@ ��@ ��@ ,=@ 1@ƨ@��@iD@O@'�@��@}V@R�@�@�9@�C@��@x�@J�@�@��@�@�v@Ɇ@�U@Ĝ@Ĝ@y>@˒@]�@RT@/�@�@�,@�<@z@J�@	@�o@��@��@��@��@m]@IR@q@�@�e@Z@�A@خ@��@�@��@iD@)_@Y@�@ i@��@�2@�@͟@��@��@�\@{�@p;@?@��@�"@ \@��@�@��@r�@[�@:�@"h@  @�q@{J@K�@.I@)_@�@�c@��@�+@l�@Q@�@��@��@�'@��@��@S&@@@��@��@�@��@Z@/�@�@x@�@ݘ@�@@C�@"�@�,@s�@L0@+k@�@�@�H@�~@Vm@<6@��@�[@�O@��@bN@:�@@��@� @�@��@�V@�:@��@K�@9�@'�@�@
��@
��@o@Y@
ں@
�}@
�6@
��@
i�@
E�@
B[@
($@
u@	��@	��@	o @	&�@	%@��@��@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�iDA�poA�t�A�uZA�v`A�xA�y>A�zA�y�A�y�A�{A�~�Aց�AւAփAփGAփGAփAքAփ�AօAքA�x8A�g�A�Z�A�7�A�	A�یA�w�A�ffA�(A�@�Aґ�A���A� �A�ΥA�~(A�.IA��8A���A�X�A�4AʖSA�̘A�3�AƉA�gmA�h>A��]A��A�o�A�[WA���A���A��/A�f�A��DA��iA� �A�A�}VA��	A��A�/�A���A�7�A���A���A�&�A���A�|�A��A���A���A�;dA�OBA��;A�IA��A�CaA��0A��xA��\A���A�]�A�f2A��]A��A�;�A�A��A��A�{�Ap;A{PHAw�]Av��Ar|�Al��Aj!�Ae��A_��A\�{AZ��AW��AT�	AO�pAL;AJAH�mAFM�AB�LAA�A?(�A=�9A<��A;aA:n�A7�+A5@OA4A21'A1VA18A/�A,�AA,�A+$A*��A*B[A(�}A'N<A&ffA$��A"�fA"l�A"F�A!+�A _�A�KA��A�$A-�A�A�A`�A��A,�A7LA}VA!�A�=A��AA�A�A�#AĜA��AB[A��A
�A+kAS&A�A�A$tA��A��A�_A#�A��A.IA�A&�A�'A��AZ�A�Ai�A#�A
��A
($A	!�A�RAIRAݘA�DA�VA~�AYA��A�OAg8AJA��A`�AK�A ��A `�A dZA &@���@�H�@�Mj@�V@�^5@���@���@��A@���@���@��v@���@��@�c�@���@��@�Z�@�:�@���@�֡@�Q@��@�S&@��[@���@�'�@��'@�?�@�u@�@��?@���@�m]@�4@���@�V@�J@�F@�"@��@�!@��@�	@���@���@��@��6@�͟@��9@ݳ�@ܐ.@�]d@���@ܰ!@�i�@�Dg@��@ڷ�@��d@�~(@�`�@�J@۷@�'�@��@ؕ@��@ׯ�@�S&@���@�0U@խC@�33@ԇ�@Ӳ-@�%F@��`@ҹ�@��X@Ҟ�@�Xy@��#@�  @��@�S�@й�@�kQ@�ϫ@��@���@�خ@υ�@Ο�@��]@�w2@�2a@��@ʏ\@���@���@ǲ�@�=�@�҉@��p@�tT@ż@Ş�@��@ļj@��@��@â�@��@��5@�Ɇ@��@�H@���@��V@�T�@���@�e�@�:*@��@�ff@��7@���@� �@�ѷ@��n@�Z@��@�
=@�kQ@���@�-@��
@��@��@�i�@�*�@��
@���@�L�@��D@�($@���@���@�O@��@��@��}@�PH@�1�@�x@��z@���@�6z@��@�خ@�P�@��@��.@�@�@���@���@��/@�W�@��@�&�@��A@�V�@��>@���@���@�\�@�A�@�@���@��@�GE@�!�@���@�U2@��=@���@���@��@�a�@��@���@��@��r@��=@�G�@�A�@��@�h
@�7�@��'@��@��@���@��r@�%�@��C@��z@���@�&@���@���@��*@��"@�O�@���@�r�@�7@� �@��K@�}�@�IR@�&@��|@�i�@��@��@�@���@�4@���@��\@�Z�@�1�@��@���@���@�Mj@��@��@�~(@�J@��h@�?}@���@���@��A@�#:@�ƨ@��S@�V@���@�q@�M@�+k@��@��@��@��@�\)@�9�@�V@���@�/�@��@��#@�s@�f�@���@�m�@��@��w@�j@�rG@�'�@�Ĝ@�֡@���@�z�@�Ov@��@�ݘ@���@�9�@��@��v@�{�@�PH@��a@��@�[�@�p;@�a|@��@��@�\)@�Ɇ@��o@��@��f@�e�@�=@��@���@�H�@�	@�
�@���@��~@�s�@�9�@���@���@�j@�V@�6�@��@���@��@�[W@�(�@��U@�kQ@�M@���@��-@���@�o�@�E9@� \@��"@��e@�n�@�Xy@�*�@��@�p�@�S�@�A�@���@�n�@�O@�G@��@��j@��V@�iD@�q@��K@�͟@��e@��@�GE@��@~�b@~5?@}�)@}G�@|z�@{��@{33@z�s@z��@z:*@y��@x�$@xz�@x?�@w�@w!-@v� @u��@uO�@t�5@t��@t�@t�@tz�@r�s@q��@p��@pc�@o�@nߤ@n��@n@�@m��@l�E@lC-@k�[@kdZ@j��@j?@i�@iQ�@i*0@i;@hѷ@h�@g|�@f��@f�@e�-@e�n@ehs@e!�@e�@dz�@c�&@c�@c�@b~�@a�@a\�@aIR@`�v@`�9@`��@`Q�@_خ@_�	@_]�@_4�@^�R@]�)@]�@\ی@\�p@\�?@\��@\֡@\��@\Q�@[�@Z�@Z��@Z��@Zh
@Z$�@Y�9@Y(�@X�@X��@Xg8@X2�@W�0@W@O@W&@W�@Vc @V�@U��@T�K@T~(@T1@S��@R�8@R��@Rd�@RR�@Q�n@Pr�@P �@O˒@Ov`@Oa@O)_@N�\@N�A@Nn�@N^5@N@�@M�T@M��@M�'@M-w@L�f@L�v@L�_@L�@Lz�@L7�@K�@K��@K��@K�@J��@I�z@I��@I/@H�U@HD�@G�+@G�g@G�@GY@F��@F@Ehs@D�5@DM@C��@C��@Cj�@CMj@C�@B��@B{@A8�@@�`@@�@@��@@`�@@<�@@'R@?ݘ@?a@>�c@>��@>Ta@>J@=��@=f�@=+�@<��@<�/@<��@<��@<H@<�@<�@;��@;�@;��@;e�@:�@:�@:��@:3�@:�@9�>@9�@9�"@9w2@9Vm@9!�@8�z@8|�@7��@7�K@7��@7P�@7�@6͟@6l�@6C�@5�n@5N<@5@@4��@4�@4�@4�U@4l"@49X@3��@3�@@3;d@3(@2��@2�@2��@2ff@2
�@1��@1��@1Y�@1&�@1�@1;@0�5@0Ɇ@0�@0�D@0l"@0<�@/�@/��@/b�@/F�@/1�@.ff@.�@-�o@-�=@-B�@,�@,|�@,1'@+��@+��@+dZ@+�@*��@*�6@*�r@*V@*5?@*e@)�>@)�-@)c�@(�P@(�?@(`�@((�@(  @'�0@'�4@&�"@&�m@&�h@&��@&)�@%��@%��@%<6@$�@$�I@$r�@$"h@#�@#��@#a@"�@"��@"�@!�T@!�@!w2@!S&@!5�@ �P@ ��@ ��@ ��@ ,=@ 1@ƨ@��@iD@O@'�@��@}V@R�@�@�9@�C@��@x�@J�@�@��@�@�v@Ɇ@�U@Ĝ@Ĝ@y>@˒@]�@RT@/�@�@�,@�<@z@J�@	@�o@��@��@��@��@m]@IR@q@�@�e@Z@�A@خ@��@�@��@iD@)_@Y@�@ i@��@�2@�@͟@��@��@�\@{�@p;@?@��@�"@ \@��@�@��@r�@[�@:�@"h@  @�q@{J@K�@.I@)_@�@�c@��@�+@l�@Q@�@��@��@�'@��@��@S&@@@��@��@�@��@Z@/�@�@x@�@ݘ@�@@C�@"�@�,@s�@L0@+k@�@�@�H@�~@Vm@<6@��@�[@�O@��@bN@:�@@��@� @�@��@�V@�:@��@K�@9�@'�@�@
��@
��@o@Y@
ں@
�}@
�6@
��@
i�@
E�@
B[@
($@
u@	��@	��@	o @	&�@	%@��@��@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�LB
�2B
��B
��B
�B
��B
�B
��B
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�fB
�B
�B
�B
��B
��B
��B
�
B
�B
�B
�>BB# BaBc BabBV�BL�BSuB^B`BabBe�Bw2B��B��B��B�kB�B�2B�B�/B��B��BdB5�BA�BLJBi*BnBb�Bi�B��B�hB��B��B\B?cBK�BK�BD�B4TBpB�BqB�B�B	B�B��B�zB�QB��B��B̈́B��B�'B�Bg�B1�B B
�zB
�B
��B
f�B
N"B
'RB
pB
	RB	��B	�xB	өB	��B	��B	��B	v�B	]dB	LJB	C�B	7�B	-)B	!�B	�B	B	�B	hB	�B	�B	QB	aB	�B	�B	�B	%B�wB	aB	 �B	�B	&B	EB	�B	
�B	�B	�B	-B	 iB	�B	bB	9B	CB	$�B	)�B	0�B	2�B	2B	0�B	.�B	9�B	I�B	O�B	R�B	QhB	NVB	J#B	F�B	Q�B	XB	W�B	X_B	XB	W�B	W�B	WsB	W�B	[�B	gRB	n}B	kkB	h
B	n}B	x�B	B	�4B	��B	�-B	� B	�B	��B	��B	��B	�0B	�rB	��B	� B	��B	��B	�:B	��B	�@B	�uB	��B	�uB	��B	�oB	��B	��B	�{B	��B	��B	�8B	�
B	�mB	�2B	��B	�_B	�3B	��B	�B	�RB	��B	�cB	��B	�dB	�OB	�}B	��B	��B	�B	��B	�MB	ɺB	��B	ɆB	�RB	�7B	ȴB	ʦB	�)B	��B	��B	�B	��B	��B	�<B	��B	�oB	�GB	�-B	�AB	��B	�B	��B	��B	��B	ªB	�.B	�B	�XB	ʌB	��B	��B	ʦB	ɠB	�^B	ʦB	�RB	�xB	ɠB	��B	͟B	οB	�VB	̈́B	��B	�HB	�]B	��B	�B	��B	�B	�@B	��B	��B	�dB	��B	ܬB	�B	ޞB	�B	ޞB	�;B	��B	ބB	��B	�;B	ߊB	��B	�B	�NB	�B	�,B	�B	��B	�@B	�B	�B	�8B	�mB	��B	��B	�B	��B	��B	�hB	�B	ٚB	ּB	յB	�B	��B	՛B	�mB	�
B	��B	�B	�_B	ٴB	�B	ٴB	یB	ߊB	�B	��B	ޞB	�VB	�B	�VB	ߊB	��B	�|B	��B	�,B	�B	�%B	�B	��B
�B
B	��B	�jB	��B
�B
�B
aB
�B
�B
B
�B
�B
[B
uB
�B
�B
AB
�B
AB
B
oB
B
B
UB
[B
GB
B
�B
B	�HB
 OB
oB
 �B
 iB
�B
3B
�B
�B
�B
zB
SB
�B
B
�B
�B
SB
mB
mB
�B
�B
B
�B
tB
SB
�B
�B

rB
6B
�B
VB
�B
�B
�B
�B
PB
\B
�B
@B
�B
 B
B
B
B
4B
�B
:B
�B
{B
�B
�B
�B
_B
�B
�B
�B
EB
�B
�B
�B
�B
B
�B
�B
	B
)B
]B
CB
�B
�B
/B
~B
B
5B
OB
�B
�B
B
!B
VB
;B
�B
 B
 'B
 \B
 �B
 �B
 �B
!|B
!�B
!�B
"�B
"hB
"�B
"�B
"�B
# B
#:B
#nB
#�B
#TB
#:B
#TB
$ZB
$tB
#�B
#�B
#�B
#�B
$�B
$tB
$�B
$tB
$�B
%�B
&�B
%�B
'mB
'RB
'�B
(>B
(�B
(XB
)B
)�B
)�B
)�B
*B
)�B
)DB
'8B
&�B
(
B
'�B
'�B
'8B
&fB
$�B
$&B
$�B
$�B
%B
&B
'B
&fB
&B
&fB
&fB
&�B
'mB
'RB
'�B
(XB
(�B
)�B
*KB
*B
*eB
+QB
+�B
+�B
,�B
-�B
.cB
/B
/�B
/�B
/�B
0!B
0oB
0oB
0�B
1'B
1vB
1�B
1�B
2B
2�B
2�B
2�B
2�B
4B
4�B
4�B
4�B
4�B
4�B
5B
5ZB
5tB
5ZB
5tB
5�B
5�B
6FB
7B
6�B
6�B
7B
7�B
88B
8lB
8lB
8lB
8�B
9$B
9�B
9�B
9�B
9�B
9�B
:*B
:^B
:DB
9�B
9�B
9�B
9rB
9�B
:^B
9�B
88B
88B
9>B
9>B
9	B
9>B
9>B
:xB
:�B
;�B
;�B
<�B
=B
=�B
>B
>(B
>BB
>BB
>�B
?B
?�B
@OB
A B
A B
AoB
A�B
BB
BB
B[B
B�B
CaB
C�B
C�B
DB
D�B
D�B
D�B
D�B
D�B
E9B
E�B
F�B
G+B
G�B
G�B
H�B
H�B
IB
I7B
I�B
J�B
KDB
K�B
K�B
L�B
L�B
L�B
MB
M6B
MPB
M�B
M�B
M�B
N<B
NVB
O(B
OvB
O\B
OB
O�B
O�B
O�B
P�B
Q B
Q�B
Q�B
RTB
R�B
RoB
RTB
R�B
S@B
S&B
S@B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
T�B
UgB
UgB
UgB
U�B
VB
VB
V9B
VSB
V9B
V�B
V�B
V�B
V�B
V�B
W�B
XyB
XyB
X�B
YB
YKB
YeB
YB
Y�B
Y�B
ZB
ZB
ZQB
ZkB
Z�B
Z�B
[#B
[WB
[=B
[qB
[�B
[�B
\�B
]~B
]~B
]�B
^B
^B
^B
^OB
^�B
_B
_VB
_�B
_�B
_�B
`B
`'B
`\B
`\B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
abB
abB
aHB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
b�B
b�B
cB
c B
c�B
c�B
c�B
dZB
d@B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
e�B
fLB
ffB
f�B
f�B
gB
gB
gRB
gmB
gmB
g�B
g�B
g�B
h$B
h>B
hXB
h�B
h�B
i*B
iDB
i*B
jeB
jB
j�B
j�B
k6B
kQB
k�B
lB
lB
lWB
l�B
m)B
mwB
mwB
m�B
m�B
m�B
m�B
nB
n/B
n�B
o B
o B
oOB
oiB
o�B
o�B
o�B
pUB
pUB
poB
poB
q'B
qvB
q�B
q�B
rB
rGB
rGB
r�B
r�B
s3B
sMB
shB
s�B
tTB
tTB
t�B
t�B
t�B
uB
u?B
uZB
uZB
u�B
vB
vB
v`B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
xB
xB
xB
x8B
xRB
xlB
xlB
xlB
x�B
x�B
xlB
xRB
x�B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zxB
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{0B
z�B
{B
{0B
{B
z�B
{0B
{JB
{�B
|6B
|6B
|PB
|PB
|PB
|PB
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}�B
}�B
}�B
~B
~B
~B
~BB
~wB
~�B
B
HB
�B
�B
�B
�B
�B
�OB
��B
��B
��B
��B
�oB
��B
��B
��B
�B
�AB
��B
��B
��B
��B
�B
�GB
�{B
�{B
�{B
�{B
��B
��B
��B
�{B
��B
�B
�B
�-B
�GB
��B
��B
��B
�B
�MB
�gB
��B
��B
��B
��B
�B
�B
�9B
�SB
�SB
�mB
��B
��B
�%B
��B
��B
�B
�?B
��B
��B
�_B
��B
��B
�_B
�zB
��B
��B
��B
�B
�1B
��B
��B
�fB
�fB
��B
��B
��B
��B
��B
�l11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�LB
�2B
��B
��B
�B
��B
�B
��B
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�fB
�B
�B
�B
��B
��B
��B
�
B
�B
�B
�>BB# BaBc BabBV�BL�BSuB^B`BabBe�Bw2B��B��B��B�kB�B�2B�B�/B��B��BdB5�BA�BLJBi*BnBb�Bi�B��B�hB��B��B\B?cBK�BK�BD�B4TBpB�BqB�B�B	B�B��B�zB�QB��B��B̈́B��B�'B�Bg�B1�B B
�zB
�B
��B
f�B
N"B
'RB
pB
	RB	��B	�xB	өB	��B	��B	��B	v�B	]dB	LJB	C�B	7�B	-)B	!�B	�B	B	�B	hB	�B	�B	QB	aB	�B	�B	�B	%B�wB	aB	 �B	�B	&B	EB	�B	
�B	�B	�B	-B	 iB	�B	bB	9B	CB	$�B	)�B	0�B	2�B	2B	0�B	.�B	9�B	I�B	O�B	R�B	QhB	NVB	J#B	F�B	Q�B	XB	W�B	X_B	XB	W�B	W�B	WsB	W�B	[�B	gRB	n}B	kkB	h
B	n}B	x�B	B	�4B	��B	�-B	� B	�B	��B	��B	��B	�0B	�rB	��B	� B	��B	��B	�:B	��B	�@B	�uB	��B	�uB	��B	�oB	��B	��B	�{B	��B	��B	�8B	�
B	�mB	�2B	��B	�_B	�3B	��B	�B	�RB	��B	�cB	��B	�dB	�OB	�}B	��B	��B	�B	��B	�MB	ɺB	��B	ɆB	�RB	�7B	ȴB	ʦB	�)B	��B	��B	�B	��B	��B	�<B	��B	�oB	�GB	�-B	�AB	��B	�B	��B	��B	��B	ªB	�.B	�B	�XB	ʌB	��B	��B	ʦB	ɠB	�^B	ʦB	�RB	�xB	ɠB	��B	͟B	οB	�VB	̈́B	��B	�HB	�]B	��B	�B	��B	�B	�@B	��B	��B	�dB	��B	ܬB	�B	ޞB	�B	ޞB	�;B	��B	ބB	��B	�;B	ߊB	��B	�B	�NB	�B	�,B	�B	��B	�@B	�B	�B	�8B	�mB	��B	��B	�B	��B	��B	�hB	�B	ٚB	ּB	յB	�B	��B	՛B	�mB	�
B	��B	�B	�_B	ٴB	�B	ٴB	یB	ߊB	�B	��B	ޞB	�VB	�B	�VB	ߊB	��B	�|B	��B	�,B	�B	�%B	�B	��B
�B
B	��B	�jB	��B
�B
�B
aB
�B
�B
B
�B
�B
[B
uB
�B
�B
AB
�B
AB
B
oB
B
B
UB
[B
GB
B
�B
B	�HB
 OB
oB
 �B
 iB
�B
3B
�B
�B
�B
zB
SB
�B
B
�B
�B
SB
mB
mB
�B
�B
B
�B
tB
SB
�B
�B

rB
6B
�B
VB
�B
�B
�B
�B
PB
\B
�B
@B
�B
 B
B
B
B
4B
�B
:B
�B
{B
�B
�B
�B
_B
�B
�B
�B
EB
�B
�B
�B
�B
B
�B
�B
	B
)B
]B
CB
�B
�B
/B
~B
B
5B
OB
�B
�B
B
!B
VB
;B
�B
 B
 'B
 \B
 �B
 �B
 �B
!|B
!�B
!�B
"�B
"hB
"�B
"�B
"�B
# B
#:B
#nB
#�B
#TB
#:B
#TB
$ZB
$tB
#�B
#�B
#�B
#�B
$�B
$tB
$�B
$tB
$�B
%�B
&�B
%�B
'mB
'RB
'�B
(>B
(�B
(XB
)B
)�B
)�B
)�B
*B
)�B
)DB
'8B
&�B
(
B
'�B
'�B
'8B
&fB
$�B
$&B
$�B
$�B
%B
&B
'B
&fB
&B
&fB
&fB
&�B
'mB
'RB
'�B
(XB
(�B
)�B
*KB
*B
*eB
+QB
+�B
+�B
,�B
-�B
.cB
/B
/�B
/�B
/�B
0!B
0oB
0oB
0�B
1'B
1vB
1�B
1�B
2B
2�B
2�B
2�B
2�B
4B
4�B
4�B
4�B
4�B
4�B
5B
5ZB
5tB
5ZB
5tB
5�B
5�B
6FB
7B
6�B
6�B
7B
7�B
88B
8lB
8lB
8lB
8�B
9$B
9�B
9�B
9�B
9�B
9�B
:*B
:^B
:DB
9�B
9�B
9�B
9rB
9�B
:^B
9�B
88B
88B
9>B
9>B
9	B
9>B
9>B
:xB
:�B
;�B
;�B
<�B
=B
=�B
>B
>(B
>BB
>BB
>�B
?B
?�B
@OB
A B
A B
AoB
A�B
BB
BB
B[B
B�B
CaB
C�B
C�B
DB
D�B
D�B
D�B
D�B
D�B
E9B
E�B
F�B
G+B
G�B
G�B
H�B
H�B
IB
I7B
I�B
J�B
KDB
K�B
K�B
L�B
L�B
L�B
MB
M6B
MPB
M�B
M�B
M�B
N<B
NVB
O(B
OvB
O\B
OB
O�B
O�B
O�B
P�B
Q B
Q�B
Q�B
RTB
R�B
RoB
RTB
R�B
S@B
S&B
S@B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
T�B
UgB
UgB
UgB
U�B
VB
VB
V9B
VSB
V9B
V�B
V�B
V�B
V�B
V�B
W�B
XyB
XyB
X�B
YB
YKB
YeB
YB
Y�B
Y�B
ZB
ZB
ZQB
ZkB
Z�B
Z�B
[#B
[WB
[=B
[qB
[�B
[�B
\�B
]~B
]~B
]�B
^B
^B
^B
^OB
^�B
_B
_VB
_�B
_�B
_�B
`B
`'B
`\B
`\B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
abB
abB
aHB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bhB
bhB
b�B
b�B
b�B
cB
c B
c�B
c�B
c�B
dZB
d@B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
e�B
fLB
ffB
f�B
f�B
gB
gB
gRB
gmB
gmB
g�B
g�B
g�B
h$B
h>B
hXB
h�B
h�B
i*B
iDB
i*B
jeB
jB
j�B
j�B
k6B
kQB
k�B
lB
lB
lWB
l�B
m)B
mwB
mwB
m�B
m�B
m�B
m�B
nB
n/B
n�B
o B
o B
oOB
oiB
o�B
o�B
o�B
pUB
pUB
poB
poB
q'B
qvB
q�B
q�B
rB
rGB
rGB
r�B
r�B
s3B
sMB
shB
s�B
tTB
tTB
t�B
t�B
t�B
uB
u?B
uZB
uZB
u�B
vB
vB
v`B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
xB
xB
xB
x8B
xRB
xlB
xlB
xlB
x�B
x�B
xlB
xRB
x�B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
zB
zxB
z�B
z�B
z�B
z�B
z�B
{B
{B
{B
{0B
z�B
{B
{0B
{B
z�B
{0B
{JB
{�B
|6B
|6B
|PB
|PB
|PB
|PB
|PB
|jB
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}�B
}�B
}�B
~B
~B
~B
~BB
~wB
~�B
B
HB
�B
�B
�B
�B
�B
�OB
��B
��B
��B
��B
�oB
��B
��B
��B
�B
�AB
��B
��B
��B
��B
�B
�GB
�{B
�{B
�{B
�{B
��B
��B
��B
�{B
��B
�B
�B
�-B
�GB
��B
��B
��B
�B
�MB
�gB
��B
��B
��B
��B
�B
�B
�9B
�SB
�SB
�mB
��B
��B
�%B
��B
��B
�B
�?B
��B
��B
�_B
��B
��B
�_B
�zB
��B
��B
��B
�B
�1B
��B
��B
�fB
�fB
��B
��B
��B
��B
��B
�l11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220825214219  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220825214221  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220825214221  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220825214221                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220826064226  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220826064226  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220825215658                      G�O�G�O�G�O�                