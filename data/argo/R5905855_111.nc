CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:30:36Z creation;2022-06-04T19:30:37Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604193036  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               oA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ټqCQ�n1   @ټq���@-� ě���c��t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A���A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B̙�B���B�  B�  B�  B�  B�ffB�ffB�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C�C33C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<33C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� DyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@~{@�
=@�
=A�A?�A_�A�A��\A��\A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B�#�B��>B��B��B��B��B�W
B��>B��B��B��B��B̊>BνqB��B��B��B��B�W
B�W
B��B��B��B��B��B��C޸C�RC�RC�RC	�RC�RC�RC�C+�C�RC�RC޸C�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC<+�C=޸C?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~DyzDy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
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
D�?
D�
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
D��p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��[A��9A��yA��A��A�ٴA��/A�ޞA��A��A���A��A��A��A��A��2A��`A��A��A��fA��>A��KA��A��A��A��A��KA��A��WA��/A�� A���A���A��AA��GA��QA��>A��A��A͛qA� �A�,AÊ=A�;�A�*eA�L�A���A�:*A��,A� A�՛A�*�A���A�bA���A��xA���A��xA�C�A�!�A��A�A�jA��A�K�A�	�A���A��eA���A�D�A��A�)�A�5A|S�Ax@OAw�As�VAoAm�1Ah��Aeu�A_|A[��AW��AT$tAPANOvAL��AG�MAE �AB�HAA*�A=5�A;_A9�[A7��A6FA5�OA4H�A2>�A0HA.�A-"�A-��A-)_A-_A,jA)��A'��A'e�A&��A$��A"�wA!�A v`A ��A!&�A!��A#�A ��A�A�IA��Au�A�A�AoiAzA�ASA�PA�3A��Ab�A�A�A��A�A��A�)A�#A�mA�PA�$A��AL0AϫA��A�vA�9AxA?�A�A/AN<AA�|A�]A�A��A��A�A��AS�A�)A�A
|A	��A	T�A��AtTA��A'RA�`A�A!-A��A��A�\A�A��A-�A�@AخA-�AD�A�AیAw2A?ATaA6A�6AaA��A��Ad�A��A�gA`�Ac�AoiA�TA�<A|�A+�A�A1'A6zAxA�4A �A N�A o @��A eA +A A J@�خ@��P@���@�A�@�O@���@��@�V@�V@��@���@�8�@���@��@��K@�rG@�W?@��@�{�@�:*@��@��H@��@@��"@�4�@���@�	@���@�o@�r@�)_@��3@���@�֡@��@�خ@�b@�!@��@��,@��@��@���@�{J@��|@��@�A @莊@�J�@�%F@���@�O@�˒@�hs@���@��o@�>B@���@�Q@�;�@�(�@��@��)@ߥ@�A @޲�@�hs@���@�{�@��@��N@�]�@ڿ�@�q@��@���@ٞ�@�F@��@�!�@��@���@է�@�+@Ԅ�@��}@��@�~@ѱ[@ѕ�@�IR@�bN@Ϗ�@�_p@�C�@��@ι�@�A�@���@͠'@�U�@��@̍�@�ݘ@��M@ʏ\@�_�@�3�@�@Ɏ"@�#�@� i@��y@ȱ�@�z�@�-@��@�c@�:�@��P@Ơ�@�I�@�	@��Z@�u�@��E@�`�@�J@��6@Ð�@�hs@�!�@º�@�?�@�-�@�e@��0@�e�@��[@�@��:@�"�@�	l@���@���@�2�@��@�k�@���@�E�@��r@���@���@�ی@�A�@���@�Q�@��@��$@�_�@���@�4�@���@��@�l"@�
�@���@�x�@�9�@��@��@�1@��^@�IR@��B@�[�@�	@���@���@�x�@��@���@�ی@���@��A@�h�@�C�@�7@���@�� @��z@���@�L�@�,�@� \@��@���@��}@�S�@��+@�<6@�S@���@�?�@��&@�RT@�q@���@��@�f�@�4�@�@@���@��@�I�@�,=@��:@���@��U@�r�@��@��z@���@�zx@�O@� \@��@��@��5@���@��x@�3�@��g@���@�g�@�@O@��!@�Y@�� @��)@��>@��m@��N@�%@��'@���@�E�@��@�Z�@�9�@���@� �@�ƨ@��7@�ی@�͟@�y>@�-�@��@��-@���@�S�@�:�@��v@��@�n�@�0U@�J@���@�)_@��c@���@���@�|@��@�ߤ@�҉@���@�:*@�~@�@�b@��Q@���@��4@�/�@��@���@��.@�	@���@���@��@�C�@��@��@���@�5�@��!@�a|@�1�@��@���@��@@���@�O@�+@��@��?@���@��4@��h@�/�@��W@�ϫ@���@���@�*0@���@��K@��@��E@��@�[�@�Q@�*�@�4@��9@��@�~�@�@��@���@���@�q@��@��@��@�a@�8�@��f@���@�q�@�Z�@�2�@��]@��d@�y�@��@��D@�oi@�Z@�J�@�6@�1@��@��@��*@��	@�b�@�J#@�;d@�҉@�q@���@��K@��@��$@�&�@�ȴ@���@�oi@�	@�@�k@U�@~�r@~0U@~�@}��@|�5@|V�@{�	@{=@z��@zp;@y�=@yq@x�O@x�.@w�W@w��@w�@v��@v8�@uQ�@t�$@t��@t[�@s��@s��@s.I@r��@r�@qc@q%@p��@p�D@p@o��@o�:@o+@n�y@n	@m�=@m�@l�@l��@lw�@k�@kb�@j�h@jn�@j�@iϫ@i��@i+@h�U@g��@f͟@fW�@f@e��@ep�@eX@e&�@d��@d`�@c��@cF�@b��@a�N@`�/@`g8@`�@_�[@_33@^�@^�X@^��@^{�@^	@]^�@\��@\�$@[�+@[�0@[��@[U�@[(@ZE�@Z	@YT�@X�@Xu�@X  @W��@Wn/@WS�@W8@Vu%@V_@U��@Uzx@T��@Th�@T<�@S�]@Sƨ@SRT@R�x@Q�D@Q��@Q�M@Q��@Q�@P��@P��@P�Y@Pl"@P:�@O�&@O�6@O��@O�	@OiD@N��@NR�@N@M�@M�-@M�7@M@@L�u@L�o@L@K�@K��@K�;@K�:@J�@JkQ@J5?@I@I}�@I*0@I�@H�	@H�@Hc�@G�r@G�a@G��@G��@G�$@G�	@Gg�@F��@FW�@F0U@E�j@E��@EO�@Dی@D|�@D>B@D�@C�q@CJ#@B�"@B�b@B@�@A�@A��@A|@Aa�@@�@@j@@�@?�+@?��@?"�@>�c@>��@>�L@>u%@>L0@>�@=�9@=c@=F@=@@<�@<r�@;��@;��@:�@:$�@9�@9@9x�@9O�@9?}@9(�@8�f@8��@89X@7ݘ@7�@7C�@7o@6ȴ@65?@5w2@4��@4��@4��@4A�@3�g@3U�@3
=@2��@2�r@2l�@2
�@1G�@1+�@0��@0�@0(�@/��@/�@.�]@.�b@.v�@.
�@-��@-�C@-��@-S&@,�@,��@+�@+�[@+��@+'�@*�@*;�@*e@)�@)\�@(�@(��@(j@(,=@(�@'�@'ݘ@'�g@'�@'y�@'�@&�L@&�F@&�+@&s�@&Ta@&@�@&4@%��@%^�@%Vm@%[W@%`B@%Vm@%/@%&�@%q@%@@%�@$�@$�p@$��@$�e@$��@$��@$j@$6@$�@#��@#��@#��@#�q@#C�@#�@"�H@"��@"c @"{@!�#@!��@!�@!`B@!IR@!:�@!�@ ��@ �@ �9@ ��@ m�@ >B@  �@ �@˒@��@{J@J#@�@�!@p;@5?@�@�>@@o @��@�z@w�@`�@6@ݘ@j�@C�@�@�@�x@i�@�@��@��@j@?}@(�@@@��@�o@U2@6@��@��@��@{J@n/@!-@��@�,@�'@�R@��@^5@�@a�@G�@0�@�`@w�@�I@��@�@g8@N�@C-@%�@�@ݘ@�@,�@�@��@�Z@��@�@:�@%F@�f@��@��@z�@r�@j@Q�@@��@�@��@X�@@��@��@�\@�\@\�@!�@��@�H@�C@\�@Dg@@�	@�p@��@7�@�@�0@�V@v`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��[A��9A��yA��A��A�ٴA��/A�ޞA��A��A���A��A��A��A��A��2A��`A��A��A��fA��>A��KA��A��A��A��A��KA��A��WA��/A�� A���A���A��AA��GA��QA��>A��A��A͛qA� �A�,AÊ=A�;�A�*eA�L�A���A�:*A��,A� A�՛A�*�A���A�bA���A��xA���A��xA�C�A�!�A��A�A�jA��A�K�A�	�A���A��eA���A�D�A��A�)�A�5A|S�Ax@OAw�As�VAoAm�1Ah��Aeu�A_|A[��AW��AT$tAPANOvAL��AG�MAE �AB�HAA*�A=5�A;_A9�[A7��A6FA5�OA4H�A2>�A0HA.�A-"�A-��A-)_A-_A,jA)��A'��A'e�A&��A$��A"�wA!�A v`A ��A!&�A!��A#�A ��A�A�IA��Au�A�A�AoiAzA�ASA�PA�3A��Ab�A�A�A��A�A��A�)A�#A�mA�PA�$A��AL0AϫA��A�vA�9AxA?�A�A/AN<AA�|A�]A�A��A��A�A��AS�A�)A�A
|A	��A	T�A��AtTA��A'RA�`A�A!-A��A��A�\A�A��A-�A�@AخA-�AD�A�AیAw2A?ATaA6A�6AaA��A��Ad�A��A�gA`�Ac�AoiA�TA�<A|�A+�A�A1'A6zAxA�4A �A N�A o @��A eA +A A J@�خ@��P@���@�A�@�O@���@��@�V@�V@��@���@�8�@���@��@��K@�rG@�W?@��@�{�@�:*@��@��H@��@@��"@�4�@���@�	@���@�o@�r@�)_@��3@���@�֡@��@�خ@�b@�!@��@��,@��@��@���@�{J@��|@��@�A @莊@�J�@�%F@���@�O@�˒@�hs@���@��o@�>B@���@�Q@�;�@�(�@��@��)@ߥ@�A @޲�@�hs@���@�{�@��@��N@�]�@ڿ�@�q@��@���@ٞ�@�F@��@�!�@��@���@է�@�+@Ԅ�@��}@��@�~@ѱ[@ѕ�@�IR@�bN@Ϗ�@�_p@�C�@��@ι�@�A�@���@͠'@�U�@��@̍�@�ݘ@��M@ʏ\@�_�@�3�@�@Ɏ"@�#�@� i@��y@ȱ�@�z�@�-@��@�c@�:�@��P@Ơ�@�I�@�	@��Z@�u�@��E@�`�@�J@��6@Ð�@�hs@�!�@º�@�?�@�-�@�e@��0@�e�@��[@�@��:@�"�@�	l@���@���@�2�@��@�k�@���@�E�@��r@���@���@�ی@�A�@���@�Q�@��@��$@�_�@���@�4�@���@��@�l"@�
�@���@�x�@�9�@��@��@�1@��^@�IR@��B@�[�@�	@���@���@�x�@��@���@�ی@���@��A@�h�@�C�@�7@���@�� @��z@���@�L�@�,�@� \@��@���@��}@�S�@��+@�<6@�S@���@�?�@��&@�RT@�q@���@��@�f�@�4�@�@@���@��@�I�@�,=@��:@���@��U@�r�@��@��z@���@�zx@�O@� \@��@��@��5@���@��x@�3�@��g@���@�g�@�@O@��!@�Y@�� @��)@��>@��m@��N@�%@��'@���@�E�@��@�Z�@�9�@���@� �@�ƨ@��7@�ی@�͟@�y>@�-�@��@��-@���@�S�@�:�@��v@��@�n�@�0U@�J@���@�)_@��c@���@���@�|@��@�ߤ@�҉@���@�:*@�~@�@�b@��Q@���@��4@�/�@��@���@��.@�	@���@���@��@�C�@��@��@���@�5�@��!@�a|@�1�@��@���@��@@���@�O@�+@��@��?@���@��4@��h@�/�@��W@�ϫ@���@���@�*0@���@��K@��@��E@��@�[�@�Q@�*�@�4@��9@��@�~�@�@��@���@���@�q@��@��@��@�a@�8�@��f@���@�q�@�Z�@�2�@��]@��d@�y�@��@��D@�oi@�Z@�J�@�6@�1@��@��@��*@��	@�b�@�J#@�;d@�҉@�q@���@��K@��@��$@�&�@�ȴ@���@�oi@�	@�@�k@U�@~�r@~0U@~�@}��@|�5@|V�@{�	@{=@z��@zp;@y�=@yq@x�O@x�.@w�W@w��@w�@v��@v8�@uQ�@t�$@t��@t[�@s��@s��@s.I@r��@r�@qc@q%@p��@p�D@p@o��@o�:@o+@n�y@n	@m�=@m�@l�@l��@lw�@k�@kb�@j�h@jn�@j�@iϫ@i��@i+@h�U@g��@f͟@fW�@f@e��@ep�@eX@e&�@d��@d`�@c��@cF�@b��@a�N@`�/@`g8@`�@_�[@_33@^�@^�X@^��@^{�@^	@]^�@\��@\�$@[�+@[�0@[��@[U�@[(@ZE�@Z	@YT�@X�@Xu�@X  @W��@Wn/@WS�@W8@Vu%@V_@U��@Uzx@T��@Th�@T<�@S�]@Sƨ@SRT@R�x@Q�D@Q��@Q�M@Q��@Q�@P��@P��@P�Y@Pl"@P:�@O�&@O�6@O��@O�	@OiD@N��@NR�@N@M�@M�-@M�7@M@@L�u@L�o@L@K�@K��@K�;@K�:@J�@JkQ@J5?@I@I}�@I*0@I�@H�	@H�@Hc�@G�r@G�a@G��@G��@G�$@G�	@Gg�@F��@FW�@F0U@E�j@E��@EO�@Dی@D|�@D>B@D�@C�q@CJ#@B�"@B�b@B@�@A�@A��@A|@Aa�@@�@@j@@�@?�+@?��@?"�@>�c@>��@>�L@>u%@>L0@>�@=�9@=c@=F@=@@<�@<r�@;��@;��@:�@:$�@9�@9@9x�@9O�@9?}@9(�@8�f@8��@89X@7ݘ@7�@7C�@7o@6ȴ@65?@5w2@4��@4��@4��@4A�@3�g@3U�@3
=@2��@2�r@2l�@2
�@1G�@1+�@0��@0�@0(�@/��@/�@.�]@.�b@.v�@.
�@-��@-�C@-��@-S&@,�@,��@+�@+�[@+��@+'�@*�@*;�@*e@)�@)\�@(�@(��@(j@(,=@(�@'�@'ݘ@'�g@'�@'y�@'�@&�L@&�F@&�+@&s�@&Ta@&@�@&4@%��@%^�@%Vm@%[W@%`B@%Vm@%/@%&�@%q@%@@%�@$�@$�p@$��@$�e@$��@$��@$j@$6@$�@#��@#��@#��@#�q@#C�@#�@"�H@"��@"c @"{@!�#@!��@!�@!`B@!IR@!:�@!�@ ��@ �@ �9@ ��@ m�@ >B@  �@ �@˒@��@{J@J#@�@�!@p;@5?@�@�>@@o @��@�z@w�@`�@6@ݘ@j�@C�@�@�@�x@i�@�@��@��@j@?}@(�@@@��@�o@U2@6@��@��@��@{J@n/@!-@��@�,@�'@�R@��@^5@�@a�@G�@0�@�`@w�@�I@��@�@g8@N�@C-@%�@�@ݘ@�@,�@�@��@�Z@��@�@:�@%F@�f@��@��@z�@r�@j@Q�@@��@�@��@X�@@��@��@�\@�\@\�@!�@��@�H@�C@\�@Dg@@�	@�p@��@7�@�@�0@�V@v`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	i�B	i�B	i�B	i�B	i�B	i�B	i�B	jB	jB	i�B	i�B	i�B	i�B	jB	j0B	i�B	j0B	jKB	j0B	jKB	jKB	jB	jB	i�B	iyB	iyB	i�B	i�B	i�B	iyB	i�B	i_B	iDB	i�B	jB	mCB	n�B	pB	q�B	x�B	�zB
�B
_�B
gB
��B
��B
ÖB
�B
�?B
��B
�B
�fB
�=B
�B
�3B
�HB
�RB
�B
��B
�`B
�B
̘B
�KB
��B
��B
v�B
\]B
QB
H�B
<�B
-]B
�B	��B	�B	ңB	�DB	��B	�RB	��B	��B	uB	Z7B	IB	9>B	-)B	$�B	B	�B	<B	%B��B�rB�hB�"B�B�B�mB��B�5B�B��B�B�[B	B	VB	!�B	1AB	 �B	&�B	*B	*eB	# B	KB	�B	.B	�B	(�B	<PB	a�B	^jB	NB	N�B	V�B	_�B	d�B	d�B	h�B	oB	s�B	{B	}qB	��B	}"B	�uB	�?B	��B	�	B	�DB	�6B	�gB	��B	�jB	��B	��B	�ZB	�B	�@B	�B	�B	�MB	�B	�TB	�B	�AB	�GB	��B	�zB	�hB	��B	��B	�yB	�TB	��B	�`B	��B	�B	�,B	�B	��B	�B	��B	��B	��B	��B	�OB	�-B	�GB	�gB	�dB	�:B	�B	�\B	��B	�{B	�B	��B	�B	�B	�.B
B
oB
�B
�B
bB
VB
B
TB
vB

�B
�B
	�B
B
dB
�B
�B
	�B
DB
B
�B
�B
�B
JB
	�B
\B
PB
�B
�B
�B
�B
	B
�B
�B
B
YB
�B
7B
!B
 �B
"�B
!-B
�B
5B
�B
�B
B
"�B
#�B
#�B
#:B
"�B
"�B
# B
#B
#B
!�B
"B
!HB
 'B
;B
�B
1B
B
�B
"B
bB
oB
�B
�B
�B
�B
B
�B
EB
�B
YB
�B
SB
B
{B
�B
B
B
�B
HB
PB
�B
�B
�B
�B
�B
	B
	7B
	RB
	lB
	�B
	�B
xB
�B
B
B
�B
�B
dB
0B
�B
�B
�B
xB
�B

	B
	�B
	lB
	B
�B
�B
�B
�B
�B
1B
�B
	�B
	�B
	RB
	RB
	7B
	B
�B
�B
�B
�B
�B
�B
	B

XB
	�B

#B

XB

=B

�B

XB

#B

rB

rB

�B

�B

�B
)B
^B
^B
^B

�B

�B

=B

=B
	�B

	B
	�B
	�B
	�B
	�B
	�B

	B
	�B
	�B
	�B
	�B
	�B
	7B
�B
�B
_B
B
�B
�B
+B
�B
?B
_B
�B
�B
B
�B
�B
�B
EB
EB
�B
_B
�B
tB
KB
	�B

=B
B
�B
�B
�B
�B
xB

�B
	lB
�B
�B
	B
	B
	B

#B
B
~B
B
6B
�B
<B
�B
�B
pB
VB
<B
"B
VB
�B
B
B
�B
�B
�B
�B
B
\B
NB
�B
TB
�B
oB
�B
�B
&B
�B
�B
�B
�B
�B
FB
FB
B
�B
FB
�B
�B
[B
uB
�B
�B
,B
aB
aB
{B
�B
�B
�B
2B
�B
�B
�B
�B
�B
�B
SB
YB
sB
�B
�B
7B
�B
�B
�B
�B
=B
�B
=B
�B
�B
�B
�B
B
�B
jB
OB
�B
B
�B
�B
�B
B
!B
�B
jB
5B
5B
 'B
!B
!�B
"�B
#�B
#�B
#TB
#�B
#�B
#�B
#�B
#�B
#�B
$B
$�B
$�B
$�B
$ZB
$@B
#�B
#TB
#nB
#TB
#:B
# B
#B
"�B
#TB
#nB
#�B
#�B
$&B
$tB
$�B
$�B
%zB
%zB
%`B
%�B
&�B
'�B
(>B
($B
(>B
(>B
(>B
(�B
)B
)yB
)�B
)�B
)�B
)�B
,B
,WB
,�B
,�B
-B
-]B
-�B
.�B
.�B
.�B
/ B
/iB
0oB
0�B
0�B
0�B
1'B
1AB
1[B
1�B
1�B
2B
2-B
1�B
1AB
1�B
2B
1�B
2B
2aB
3�B
4B
4B
4TB
4�B
4�B
4�B
4�B
4�B
5tB
5tB
5�B
5�B
5�B
6B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6zB
6�B
6�B
6�B
6�B
7LB
7�B
8lB
9rB
9�B
9�B
9�B
9�B
:DB
:�B
:�B
;dB
;�B
;�B
<B
=<B
=VB
="B
=<B
=VB
=�B
=�B
>(B
>�B
?B
>�B
>�B
?B
?�B
?�B
?�B
?�B
?}B
@OB
@4B
@�B
@�B
AB
A B
AoB
A�B
BAB
B[B
B�B
B�B
CGB
CGB
CGB
D3B
D�B
D�B
D�B
EB
ESB
E�B
E�B
E�B
F%B
F�B
F�B
FtB
G_B
G_B
G_B
G�B
G�B
G�B
HB
HB
H1B
HB
HfB
H�B
IB
IlB
J=B
J�B
J�B
J�B
J�B
J�B
J�B
J=B
J=B
J�B
K)B
KDB
K^B
K)B
J�B
K^B
K�B
K�B
L0B
L�B
L�B
L�B
MB
L�B
M6B
MjB
N�B
O�B
O�B
O\B
P.B
PbB
P}B
PbB
PbB
P�B
Q B
QB
QNB
Q�B
Q�B
R:B
RB
RB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S�B
S�B
S�B
S�B
S�B
TB
TaB
TFB
TFB
TFB
TaB
T�B
U2B
UMB
UgB
UgB
UgB
U�B
VSB
VmB
VmB
V�B
V�B
V�B
V�B
W
B
W?B
WYB
W�B
W�B
W�B
XB
XEB
X_B
XyB
X�B
XEB
X�B
X�B
Y1B
Y1B
YB
ZB
ZB
ZB
ZB
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[qB
[�B
[�B
\]B
\�B
\�B
\�B
]/B
]/B
]/B
]/B
]IB
]~B
]�B
^B
^5B
^OB
^jB
^�B
^�B
_�B
_�B
`B
`'B
`\B
`�B
`�B
aB
abB
a�B
a|B
a�B
bhB
bhB
b�B
b�B
cB
c�B
d&B
d&B
d@B
dZB
d�B
eB
eB
eB
e,B
e�B
e�B
fLB
ffB
ffB
f�B
g8B
gmB
gmB
g�B
h
B
h>B
hsB
h�B
h�B
h�B
i*B
iB
iB
i*B
i_B
i�B
j0B
j0B
jKB
jeB
jB
j�B
j�B
k�B
l�B
m)B
mCB
mCB
m]B
m�B
m�B
m�B
m�B
m�B
n/B
nB
n/B
nIB
ncB
n}B
n�B
n�B
n}B
o B
oOB
oOB
o5B
o�B
o�B
pB
p!B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q'B
qAB
qvB
q�B
q�B
r-B
rGB
rGB
r�B
r�B
r�B
r�B
shB
shB
s�B
s�B
s�B
tB
t9B
tTB
t�B
t�B
u%B
u%B
u?B
u�B
v+B
v+B
v`B
vzB
v�B
v�B
w�B
w�B
w�B
x8B
xlB
xlB
xlB
x�B
x�B
x�B
x�B
x�B
yXB
y�B
y�B
y�B
zDB
zxB
z�B
z�B
zxB
zxB
z�B
{B
{0B
z�B
z�B
z�B
z^B
{0B
{�B
|B
|B
|B
|B
|6B
|6B
|PB
|6B
{�B
{B
{B
z�B
z�B
z�B
z�B
z�B
{0B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|B
|6B
|jB
|�B
}B
}"B
}qB
}�B
}�B
~]B
~BB
~wB
~]B
~wB
~�B
~�B
cB
HB
HB
.B
}B
�B
�B
�B
�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	i�B	i�B	i�B	i�B	i�B	i�B	i�B	jB	jB	i�B	i�B	i�B	i�B	jB	j0B	i�B	j0B	jKB	j0B	jKB	jKB	jB	jB	i�B	iyB	iyB	i�B	i�B	i�B	iyB	i�B	i_B	iDB	i�B	jB	mCB	n�B	pB	q�B	x�B	�zB
�B
_�B
gB
��B
��B
ÖB
�B
�?B
��B
�B
�fB
�=B
�B
�3B
�HB
�RB
�B
��B
�`B
�B
̘B
�KB
��B
��B
v�B
\]B
QB
H�B
<�B
-]B
�B	��B	�B	ңB	�DB	��B	�RB	��B	��B	uB	Z7B	IB	9>B	-)B	$�B	B	�B	<B	%B��B�rB�hB�"B�B�B�mB��B�5B�B��B�B�[B	B	VB	!�B	1AB	 �B	&�B	*B	*eB	# B	KB	�B	.B	�B	(�B	<PB	a�B	^jB	NB	N�B	V�B	_�B	d�B	d�B	h�B	oB	s�B	{B	}qB	��B	}"B	�uB	�?B	��B	�	B	�DB	�6B	�gB	��B	�jB	��B	��B	�ZB	�B	�@B	�B	�B	�MB	�B	�TB	�B	�AB	�GB	��B	�zB	�hB	��B	��B	�yB	�TB	��B	�`B	��B	�B	�,B	�B	��B	�B	��B	��B	��B	��B	�OB	�-B	�GB	�gB	�dB	�:B	�B	�\B	��B	�{B	�B	��B	�B	�B	�.B
B
oB
�B
�B
bB
VB
B
TB
vB

�B
�B
	�B
B
dB
�B
�B
	�B
DB
B
�B
�B
�B
JB
	�B
\B
PB
�B
�B
�B
�B
	B
�B
�B
B
YB
�B
7B
!B
 �B
"�B
!-B
�B
5B
�B
�B
B
"�B
#�B
#�B
#:B
"�B
"�B
# B
#B
#B
!�B
"B
!HB
 'B
;B
�B
1B
B
�B
"B
bB
oB
�B
�B
�B
�B
B
�B
EB
�B
YB
�B
SB
B
{B
�B
B
B
�B
HB
PB
�B
�B
�B
�B
�B
	B
	7B
	RB
	lB
	�B
	�B
xB
�B
B
B
�B
�B
dB
0B
�B
�B
�B
xB
�B

	B
	�B
	lB
	B
�B
�B
�B
�B
�B
1B
�B
	�B
	�B
	RB
	RB
	7B
	B
�B
�B
�B
�B
�B
�B
	B

XB
	�B

#B

XB

=B

�B

XB

#B

rB

rB

�B

�B

�B
)B
^B
^B
^B

�B

�B

=B

=B
	�B

	B
	�B
	�B
	�B
	�B
	�B

	B
	�B
	�B
	�B
	�B
	�B
	7B
�B
�B
_B
B
�B
�B
+B
�B
?B
_B
�B
�B
B
�B
�B
�B
EB
EB
�B
_B
�B
tB
KB
	�B

=B
B
�B
�B
�B
�B
xB

�B
	lB
�B
�B
	B
	B
	B

#B
B
~B
B
6B
�B
<B
�B
�B
pB
VB
<B
"B
VB
�B
B
B
�B
�B
�B
�B
B
\B
NB
�B
TB
�B
oB
�B
�B
&B
�B
�B
�B
�B
�B
FB
FB
B
�B
FB
�B
�B
[B
uB
�B
�B
,B
aB
aB
{B
�B
�B
�B
2B
�B
�B
�B
�B
�B
�B
SB
YB
sB
�B
�B
7B
�B
�B
�B
�B
=B
�B
=B
�B
�B
�B
�B
B
�B
jB
OB
�B
B
�B
�B
�B
B
!B
�B
jB
5B
5B
 'B
!B
!�B
"�B
#�B
#�B
#TB
#�B
#�B
#�B
#�B
#�B
#�B
$B
$�B
$�B
$�B
$ZB
$@B
#�B
#TB
#nB
#TB
#:B
# B
#B
"�B
#TB
#nB
#�B
#�B
$&B
$tB
$�B
$�B
%zB
%zB
%`B
%�B
&�B
'�B
(>B
($B
(>B
(>B
(>B
(�B
)B
)yB
)�B
)�B
)�B
)�B
,B
,WB
,�B
,�B
-B
-]B
-�B
.�B
.�B
.�B
/ B
/iB
0oB
0�B
0�B
0�B
1'B
1AB
1[B
1�B
1�B
2B
2-B
1�B
1AB
1�B
2B
1�B
2B
2aB
3�B
4B
4B
4TB
4�B
4�B
4�B
4�B
4�B
5tB
5tB
5�B
5�B
5�B
6B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6zB
6�B
6�B
6�B
6�B
7LB
7�B
8lB
9rB
9�B
9�B
9�B
9�B
:DB
:�B
:�B
;dB
;�B
;�B
<B
=<B
=VB
="B
=<B
=VB
=�B
=�B
>(B
>�B
?B
>�B
>�B
?B
?�B
?�B
?�B
?�B
?}B
@OB
@4B
@�B
@�B
AB
A B
AoB
A�B
BAB
B[B
B�B
B�B
CGB
CGB
CGB
D3B
D�B
D�B
D�B
EB
ESB
E�B
E�B
E�B
F%B
F�B
F�B
FtB
G_B
G_B
G_B
G�B
G�B
G�B
HB
HB
H1B
HB
HfB
H�B
IB
IlB
J=B
J�B
J�B
J�B
J�B
J�B
J�B
J=B
J=B
J�B
K)B
KDB
K^B
K)B
J�B
K^B
K�B
K�B
L0B
L�B
L�B
L�B
MB
L�B
M6B
MjB
N�B
O�B
O�B
O\B
P.B
PbB
P}B
PbB
PbB
P�B
Q B
QB
QNB
Q�B
Q�B
R:B
RB
RB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S�B
S�B
S�B
S�B
S�B
TB
TaB
TFB
TFB
TFB
TaB
T�B
U2B
UMB
UgB
UgB
UgB
U�B
VSB
VmB
VmB
V�B
V�B
V�B
V�B
W
B
W?B
WYB
W�B
W�B
W�B
XB
XEB
X_B
XyB
X�B
XEB
X�B
X�B
Y1B
Y1B
YB
ZB
ZB
ZB
ZB
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[qB
[�B
[�B
\]B
\�B
\�B
\�B
]/B
]/B
]/B
]/B
]IB
]~B
]�B
^B
^5B
^OB
^jB
^�B
^�B
_�B
_�B
`B
`'B
`\B
`�B
`�B
aB
abB
a�B
a|B
a�B
bhB
bhB
b�B
b�B
cB
c�B
d&B
d&B
d@B
dZB
d�B
eB
eB
eB
e,B
e�B
e�B
fLB
ffB
ffB
f�B
g8B
gmB
gmB
g�B
h
B
h>B
hsB
h�B
h�B
h�B
i*B
iB
iB
i*B
i_B
i�B
j0B
j0B
jKB
jeB
jB
j�B
j�B
k�B
l�B
m)B
mCB
mCB
m]B
m�B
m�B
m�B
m�B
m�B
n/B
nB
n/B
nIB
ncB
n}B
n�B
n�B
n}B
o B
oOB
oOB
o5B
o�B
o�B
pB
p!B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q'B
qAB
qvB
q�B
q�B
r-B
rGB
rGB
r�B
r�B
r�B
r�B
shB
shB
s�B
s�B
s�B
tB
t9B
tTB
t�B
t�B
u%B
u%B
u?B
u�B
v+B
v+B
v`B
vzB
v�B
v�B
w�B
w�B
w�B
x8B
xlB
xlB
xlB
x�B
x�B
x�B
x�B
x�B
yXB
y�B
y�B
y�B
zDB
zxB
z�B
z�B
zxB
zxB
z�B
{B
{0B
z�B
z�B
z�B
z^B
{0B
{�B
|B
|B
|B
|B
|6B
|6B
|PB
|6B
{�B
{B
{B
z�B
z�B
z�B
z�B
z�B
{0B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|B
|6B
|jB
|�B
}B
}"B
}qB
}�B
}�B
~]B
~BB
~wB
~]B
~wB
~�B
~�B
cB
HB
HB
.B
}B
�B
�B
�B
�411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105251  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193036  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193037  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193037                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043044  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043044  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                