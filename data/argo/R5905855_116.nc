CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:31:36Z creation;2022-06-04T19:31:36Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604193136  20220610161506  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               tA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��&�1   @����Pg@,�=p��
�c�9XbN1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C��C��C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@33CB  CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDyfDy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBxG�B�W
B��qB��qB��B��B��B��B��B��B��B��B��B�W
B�#�B��B��qB��B��B��B��B��B��B��B��B��B��B��B��B�W
B�W
B��>B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC��C�C�RC�RC!޸C#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC@+�CA�RCC޸CE޸CG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D zD ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI�zDI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx�zDyzDy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
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
D�=D�
D��
D�?
D�
D�
D��
D�;�D�
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
D�=D�
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
D�=D�?
D�
D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�D3A�I�A��NA�zA�[WA�IA�?A�;�A�4nA�2aA�,qA�'A�&LA�&A�%zA�$�A�%zA�&A�$�A�$A�#�A�!�A�!�A�#�A�&A�)_A�+�A�*�A�)�A�.�A�YAǒ�A�s�A��A���A��oA�ʌA�qAA��YA���A���A�]�A�A�A�s�A�*�A��DA��KA��5A�O�A��3A�MjA��A�A���A���A�#�A�OvA��A���A���A���A���A��A��VA�A�@A��A�A�0UA��tA�`A�;�A�>�A�L�A�ƨA��A�@�A�'RA{�)AxAq+�Ag�.AaYA]'RA\AZ�AXq�AU��AT�ARRTAP7LAK��AD֡ACv`AA�KA==�A:MA8�'A7VmA5L�A3��A3��A3u�A3.�A2�A0�A/�A,�A*��A'�A&�6A'��A$�gA%�A%�A$e,A �A!w2A �BA �A �A!N�A#��A$�fA#�A!�3A!�A!ffA � Aq�A9XAjA�dAC�A+�A�>A˒AA�A�
APHA	A�kA[WA+kA�WAIRA��A��A2aAL0A��A��A�A��A� AoiAu�A��Au�A��AiDA*�AGA��A($Ae,ACA�A!-A �A$tA
��A
��A
�A
j�A
A	��A	��A	��A	.�A	�A	�A�kAj�A,=A�A�9A�4A�mA�DA��A�:A[WA�A��A$tA�A�A�4Aa�A$tA��A�9A}�A�Al�A ��A J�A J@��@��4@�#:@�J#@��o@��@�S�@���@���@��w@�w2@���@�{J@���@�@�Y�@�@� �@���@�%@�B�@�_p@�z�@�Ov@��@�/�@�r�@�kQ@�J�@��@�6@�?}@�|�@��@��@�x@�^@�X@�33@�n@�9@�tT@�9@߫�@߸�@��)@��@�c @�L0@�~�@�m]@�'�@�M�@�`B@ܑ @�@�!�@�U2@ٴ�@�hs@���@�D�@؄�@��@؞@ּj@�	�@���@��@�r�@��@ӭC@��@ң�@�!@ѐ�@�Z�@�@�b@��D@�P�@�zx@�&@΄�@͎�@��@���@ˊ	@�(@�+k@��+@��
@ɪ�@Ƀ{@�ȴ@�-@��@��T@Ǘ�@�zx@��@Ǵ�@�Y�@��@��@��U@�bN@��9@Ł�@�9�@Ĩ�@Ê�@�ی@�@��@�s�@���@�� @�g8@�C�@�G@�rG@��K@��Y@�2�@���@�s�@�Ĝ@�)�@���@�<6@��@���@�:�@�x@��@��y@��,@���@��6@�q@�@��@��C@��f@�l�@��@���@���@���@�7�@��*@�e�@��@�h
@�>B@��m@�]�@��@���@���@��@�+k@���@�X�@�ȴ@��z@���@�Q�@�Ft@�&�@�@��t@��{@�P�@�%F@���@�h�@���@�\)@�F�@�8@��@��B@���@�Z@���@�H�@�/�@��F@��+@�t�@��@���@�Z�@�*�@�|@��@�V�@��S@�'�@��@��O@���@���@���@�iD@�
=@���@�h
@�2�@���@���@�'�@�ѷ@��L@�{�@�c�@�M@��9@���@�0�@��"@���@�Z@��@���@���@���@�w2@�L�@�1�@���@��,@���@�4n@��@��@���@�!-@��@��@�Ov@�{@��@���@�RT@�@��@���@��@���@���@�E9@���@��)@�a|@�#:@�@��*@�,�@���@�ی@���@�u�@�5?@� �@�_p@���@��x@�h
@�4n@��@��@��@�c@��@���@�q@���@�s@�)_@��@��'@�?�@�خ@��K@���@���@�v`@�H�@�*0@��"@��U@�n�@�#:@��H@�S�@�2a@�%@��F@�j@�0U@���@�l�@�A�@��@��y@��D@�M@���@��"@�Q�@��c@��s@���@�[�@�@��W@��H@��w@���@��P@��@��@�<�@�#:@�-�@��@��^@���@�`B@�'�@��@��O@�s�@�I�@�$@���@��S@���@���@�qv@�\�@�F�@��@���@��@��1@���@�g8@�D�@�1�@�(�@�@�@+@~��@~@�@}��@|��@|��@{�F@z�@yA @w��@w9�@w$t@v�b@v$�@u%@tr�@s� @sa@s�@rq�@re@q��@q�@o�@@o�@n�H@n��@n� @nxl@m�3@m@l��@lZ@k�a@k8@jz@j:*@j�@ju@i@ij@h�/@hXy@h�@g�F@gx@g_p@g8@f��@f��@f)�@e�@e�-@e�"@e%@d�o@c�r@c]�@b��@b��@b.�@a�@a}�@a+�@`��@`h�@`:�@_��@_��@_j�@_�@^�H@^�!@^R�@^&�@^@]��@]IR@\l"@[�g@[��@Z�@Z�@ZC�@Y�Z@Y�H@Yp�@Y�@X�@X�)@X��@X��@X~(@XS�@W��@WP�@V�@V6�@U}�@U0�@U�@T�@T'R@T1@Sݘ@S��@S�F@S�@St�@S�@Rs�@R@Q�#@Q��@Q�M@Qe,@QG�@P��@P��@Pr�@P-�@O�@O�[@O�k@O_p@O&@Nu%@Mԕ@M�h@Mp�@M\�@M8�@LtT@K��@J�]@J��@J� @J5?@I�#@I��@I?}@H��@H~@G�$@G]�@F��@F=q@E�j@E�3@E��@E<6@E#�@E@@D��@Dy>@DS�@Db@CZ�@B�M@B��@B�r@B6�@A�T@A�@A��@A(�@@��@@��@@��@@<�@@M@@b@?˒@?J#@?+@?�@>��@>��@>��@=�-@=|@=L�@<��@<��@<e�@<K^@<(�@;�}@;{J@;dZ@:�B@:p;@:L0@:)�@9��@9�h@9`B@9+�@9�@8�P@8��@8�o@8:�@8@7��@7X�@7)_@6��@6�@6!�@5�D@5��@5�n@5f�@5�@4`�@4!@4�@3��@3s@3�@2��@2Ta@2�@1�T@1��@1rG@1T�@1�@0�_@0<�@/��@/ƨ@/qv@/,�@.�y@.}V@-��@-�=@-��@-Y�@,�P@,�@,w�@,g8@,(�@+��@+��@+�:@+�@+o@*��@*~�@*.�@)�.@)�H@)�"@)rG@)IR@(�U@(��@(��@(�.@(y>@(]d@(Q�@((�@'�]@'�F@'a@'$t@&��@&q�@&�@%�z@%��@%e,@%7L@$�K@$Ɇ@$��@$!@#��@#��@#�F@#x@#,�@"��@"}V@"E�@"�@!�T@!�M@!B�@!�@ ��@ y>@ H@ ~@��@�$@@O@33@/�@�@�8@�'@H�@��@��@c�@S&@L�@q@�.@Ft@b@�+@��@|�@$t@��@��@p;@8�@��@ϫ@p�@e,@Q�@&�@#�@�@�@�f@��@�E@�O@|�@C-@  @��@��@��@�@dZ@O@;d@.I@�@͟@s�@i�@q�@�@��@��@u�@c�@hs@7L@�@��@�U@��@�.@Xy@<�@�@�@�@��@s@&@�8@�h@�@s�@B[@-@J@�>@�@�@�@��@IR@-w@@�@�@@@;@��@֡@�z@|�@c�@H@"h@�&@�6@��@j�@b�@Z�@\)@U�@O@+@�c@�!@��@_�@GE@-@	@_@��@�@�C@��@X@A @8�@4@(�@	l@�@�@��@z�@C-@�@�&@�q@qv@@O@�@
ߤ@
�'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�D3A�I�A��NA�zA�[WA�IA�?A�;�A�4nA�2aA�,qA�'A�&LA�&A�%zA�$�A�%zA�&A�$�A�$A�#�A�!�A�!�A�#�A�&A�)_A�+�A�*�A�)�A�.�A�YAǒ�A�s�A��A���A��oA�ʌA�qAA��YA���A���A�]�A�A�A�s�A�*�A��DA��KA��5A�O�A��3A�MjA��A�A���A���A�#�A�OvA��A���A���A���A���A��A��VA�A�@A��A�A�0UA��tA�`A�;�A�>�A�L�A�ƨA��A�@�A�'RA{�)AxAq+�Ag�.AaYA]'RA\AZ�AXq�AU��AT�ARRTAP7LAK��AD֡ACv`AA�KA==�A:MA8�'A7VmA5L�A3��A3��A3u�A3.�A2�A0�A/�A,�A*��A'�A&�6A'��A$�gA%�A%�A$e,A �A!w2A �BA �A �A!N�A#��A$�fA#�A!�3A!�A!ffA � Aq�A9XAjA�dAC�A+�A�>A˒AA�A�
APHA	A�kA[WA+kA�WAIRA��A��A2aAL0A��A��A�A��A� AoiAu�A��Au�A��AiDA*�AGA��A($Ae,ACA�A!-A �A$tA
��A
��A
�A
j�A
A	��A	��A	��A	.�A	�A	�A�kAj�A,=A�A�9A�4A�mA�DA��A�:A[WA�A��A$tA�A�A�4Aa�A$tA��A�9A}�A�Al�A ��A J�A J@��@��4@�#:@�J#@��o@��@�S�@���@���@��w@�w2@���@�{J@���@�@�Y�@�@� �@���@�%@�B�@�_p@�z�@�Ov@��@�/�@�r�@�kQ@�J�@��@�6@�?}@�|�@��@��@�x@�^@�X@�33@�n@�9@�tT@�9@߫�@߸�@��)@��@�c @�L0@�~�@�m]@�'�@�M�@�`B@ܑ @�@�!�@�U2@ٴ�@�hs@���@�D�@؄�@��@؞@ּj@�	�@���@��@�r�@��@ӭC@��@ң�@�!@ѐ�@�Z�@�@�b@��D@�P�@�zx@�&@΄�@͎�@��@���@ˊ	@�(@�+k@��+@��
@ɪ�@Ƀ{@�ȴ@�-@��@��T@Ǘ�@�zx@��@Ǵ�@�Y�@��@��@��U@�bN@��9@Ł�@�9�@Ĩ�@Ê�@�ی@�@��@�s�@���@�� @�g8@�C�@�G@�rG@��K@��Y@�2�@���@�s�@�Ĝ@�)�@���@�<6@��@���@�:�@�x@��@��y@��,@���@��6@�q@�@��@��C@��f@�l�@��@���@���@���@�7�@��*@�e�@��@�h
@�>B@��m@�]�@��@���@���@��@�+k@���@�X�@�ȴ@��z@���@�Q�@�Ft@�&�@�@��t@��{@�P�@�%F@���@�h�@���@�\)@�F�@�8@��@��B@���@�Z@���@�H�@�/�@��F@��+@�t�@��@���@�Z�@�*�@�|@��@�V�@��S@�'�@��@��O@���@���@���@�iD@�
=@���@�h
@�2�@���@���@�'�@�ѷ@��L@�{�@�c�@�M@��9@���@�0�@��"@���@�Z@��@���@���@���@�w2@�L�@�1�@���@��,@���@�4n@��@��@���@�!-@��@��@�Ov@�{@��@���@�RT@�@��@���@��@���@���@�E9@���@��)@�a|@�#:@�@��*@�,�@���@�ی@���@�u�@�5?@� �@�_p@���@��x@�h
@�4n@��@��@��@�c@��@���@�q@���@�s@�)_@��@��'@�?�@�خ@��K@���@���@�v`@�H�@�*0@��"@��U@�n�@�#:@��H@�S�@�2a@�%@��F@�j@�0U@���@�l�@�A�@��@��y@��D@�M@���@��"@�Q�@��c@��s@���@�[�@�@��W@��H@��w@���@��P@��@��@�<�@�#:@�-�@��@��^@���@�`B@�'�@��@��O@�s�@�I�@�$@���@��S@���@���@�qv@�\�@�F�@��@���@��@��1@���@�g8@�D�@�1�@�(�@�@�@+@~��@~@�@}��@|��@|��@{�F@z�@yA @w��@w9�@w$t@v�b@v$�@u%@tr�@s� @sa@s�@rq�@re@q��@q�@o�@@o�@n�H@n��@n� @nxl@m�3@m@l��@lZ@k�a@k8@jz@j:*@j�@ju@i@ij@h�/@hXy@h�@g�F@gx@g_p@g8@f��@f��@f)�@e�@e�-@e�"@e%@d�o@c�r@c]�@b��@b��@b.�@a�@a}�@a+�@`��@`h�@`:�@_��@_��@_j�@_�@^�H@^�!@^R�@^&�@^@]��@]IR@\l"@[�g@[��@Z�@Z�@ZC�@Y�Z@Y�H@Yp�@Y�@X�@X�)@X��@X��@X~(@XS�@W��@WP�@V�@V6�@U}�@U0�@U�@T�@T'R@T1@Sݘ@S��@S�F@S�@St�@S�@Rs�@R@Q�#@Q��@Q�M@Qe,@QG�@P��@P��@Pr�@P-�@O�@O�[@O�k@O_p@O&@Nu%@Mԕ@M�h@Mp�@M\�@M8�@LtT@K��@J�]@J��@J� @J5?@I�#@I��@I?}@H��@H~@G�$@G]�@F��@F=q@E�j@E�3@E��@E<6@E#�@E@@D��@Dy>@DS�@Db@CZ�@B�M@B��@B�r@B6�@A�T@A�@A��@A(�@@��@@��@@��@@<�@@M@@b@?˒@?J#@?+@?�@>��@>��@>��@=�-@=|@=L�@<��@<��@<e�@<K^@<(�@;�}@;{J@;dZ@:�B@:p;@:L0@:)�@9��@9�h@9`B@9+�@9�@8�P@8��@8�o@8:�@8@7��@7X�@7)_@6��@6�@6!�@5�D@5��@5�n@5f�@5�@4`�@4!@4�@3��@3s@3�@2��@2Ta@2�@1�T@1��@1rG@1T�@1�@0�_@0<�@/��@/ƨ@/qv@/,�@.�y@.}V@-��@-�=@-��@-Y�@,�P@,�@,w�@,g8@,(�@+��@+��@+�:@+�@+o@*��@*~�@*.�@)�.@)�H@)�"@)rG@)IR@(�U@(��@(��@(�.@(y>@(]d@(Q�@((�@'�]@'�F@'a@'$t@&��@&q�@&�@%�z@%��@%e,@%7L@$�K@$Ɇ@$��@$!@#��@#��@#�F@#x@#,�@"��@"}V@"E�@"�@!�T@!�M@!B�@!�@ ��@ y>@ H@ ~@��@�$@@O@33@/�@�@�8@�'@H�@��@��@c�@S&@L�@q@�.@Ft@b@�+@��@|�@$t@��@��@p;@8�@��@ϫ@p�@e,@Q�@&�@#�@�@�@�f@��@�E@�O@|�@C-@  @��@��@��@�@dZ@O@;d@.I@�@͟@s�@i�@q�@�@��@��@u�@c�@hs@7L@�@��@�U@��@�.@Xy@<�@�@�@�@��@s@&@�8@�h@�@s�@B[@-@J@�>@�@�@�@��@IR@-w@@�@�@@@;@��@֡@�z@|�@c�@H@"h@�&@�6@��@j�@b�@Z�@\)@U�@O@+@�c@�!@��@_�@GE@-@	@_@��@�@�C@��@X@A @8�@4@(�@	l@�@�@��@z�@C-@�@�&@�q@qv@@O@�@
ߤ@
�'11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	\)B	ZkB	[�B	ZQB	ZkB	ZB	ZB	Z7B	ZQB	ZkB	ZkB	ZkB	Z�B	Z�B	[=B	[qB	[�B	[�B	\xB	\xB	\�B	\�B	\�B	]~B	^OB	_�B	`�B	a�B	bNB	gB	�B
�B0;BՁB��B�RB�JB��B�B��B��B��B��BBB�B�B�BB�B!-B$�B$�B$&B#�B!�B!BB�B�B�B�QB��B��B�@B��B{�BpBabB\�BEB&LBmB
��B
��B
�~B
=VB
$@B	��B	��B	��B	�+B	raB	\�B	W�B	O�B	H�B	;�B	4TB	,WB	!�B	$B	  B	 iB�]B	 B��B	SB	VB	qB	$�B	-)B	/�B	33B	?HB	LdB	KDB	W�B	W�B	>BB	2�B	M�B	F?B	OvB	f�B	s�B	R�B	iDB	^�B	h�B	lB	~wB	��B	�=B	רB	��B	˒B	�/B	�xB	�VB	��B	�+B	�_B	̘B	�"B	�NB	��B	�B	�0B	�B	�sB	��B	�JB	��B	��B
oB	�(B	��B	��B	��B	�OB	��B	ܬB	�B	�XB	ݘB	�ZB	�B	�B	�B	�B	�RB	�sB	��B	�DB	��B	�B	��B	�`B	�-B	�B	�\B	� B	�B	�*B	�RB	�B	�B	��B	�MB	�B	�B	�B	�`B	�2B	�8B	�	B	��B	�lB	�B	�zB	�	B	�B	�JB	��B	�lB	��B	��B	��B	�?B	��B	��B	�^B	��B	��B	�(B	��B	��B	�9B	�|B	�GB	��B	��B	�B	�B	��B	�B	��B	׍B	�B	�fB	�fB	�-B	ބB	�B	��B	ݲB	�pB	�hB	�qB	�RB	��B	ܬB	�/B	ܬB	��B	�OB	�hB	��B	��B	�"B	�yB	�B	�TB	�B	��B	�B	�B	��B	�eB	��B	�kB	׍B	�sB	��B	�5B	��B	�B	�B	�,B	��B	�_B	�B	��B	�`B	�B	��B	��B	�vB	�B	��B	��B	�TB	�B	�B	�B	�ZB	�FB	��B	�B	�`B	�RB	��B	��B	�B	�8B	��B	�B	�*B	��B	��B	�B	�oB	�'B	�B	�B	�B	��B	��B	�B	�iB	�iB	�iB	�OB	��B	�B	�B	�B	�B	��B	�tB	��B	�LB	��B	�lB	�B	��B	�B	�B	��B	��B	��B	�JB	�B	��B	��B	�B	�B	��B	��B	�dB	�dB	��B	��B	��B	�PB	�PB	��B	�<B	��B	��B	��B	��B	��B	�wB	��B	��B	�B	�B	�B	�HB	�}B	��B	��B	��B	��B	��B	�cB	�.B	��B	��B	�B	�B
 OB
 �B
 �B
�B
oB
�B
�B
�B
oB
oB
AB
AB
'B
AB
�B
aB
{B
�B
MB
YB
fB
	�B
	�B
	�B

	B
	�B
	lB
	RB
	7B
	lB
	�B
	�B
B
B
B
\B
\B
�B
�B
�B
�B
�B
\B
\B
�B
�B
}B
�B
 B
�B
�B
TB
�B
�B
@B
�B
aB
�B
B
MB
�B
�B
�B
eB
�B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
/B
dB
~B
~B
�B
�B
�B
�B
~B
�B
�B
 \B
!bB
!�B
!�B
!|B
"�B
"�B
"hB
"4B
"4B
"�B
#�B
$&B
$@B
$B
$�B
%,B
%`B
%`B
%zB
%�B
%�B
%�B
&fB
&�B
&�B
&�B
&�B
&�B
'B
&�B
'B
'RB
&�B
&�B
&�B
'B
'RB
'�B
'�B
'�B
'�B
'�B
(�B
*�B
+B
+QB
+QB
+�B
+�B
,B
,WB
,�B
-CB
-CB
-]B
-�B
-�B
-�B
./B
.IB
.}B
.�B
.�B
/5B
/OB
/OB
/5B
/�B
0UB
0;B
0;B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
1'B
0�B
0�B
1'B
1[B
1�B
1�B
1�B
2GB
2�B
2�B
3�B
49B
4�B
5B
5tB
6+B
6FB
6`B
6�B
6�B
6�B
6�B
6�B
7B
7B
6�B
7B
7LB
72B
7B
7LB
7LB
7�B
7�B
7fB
7�B
8RB
88B
8B
7�B
7B
8B
8lB
9	B
9�B
9�B
:�B
:�B
;JB
;dB
;0B
;�B
;dB
;�B
;�B
=�B
=�B
=�B
=�B
=�B
=�B
>]B
>�B
>�B
?�B
@iB
AB
A�B
B'B
B�B
B�B
B�B
B[B
BAB
B'B
B�B
B�B
B�B
C-B
B�B
C{B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
DMB
D�B
EB
E9B
E�B
E�B
E�B
FB
FYB
F�B
F�B
F�B
G+B
GEB
GzB
G�B
G�B
G�B
HB
G�B
HKB
HfB
I7B
I�B
I�B
J=B
JrB
J�B
KB
J�B
KDB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L~B
L�B
MB
M�B
M�B
M�B
N"B
N�B
NpB
N�B
N�B
N�B
N�B
N�B
O(B
O�B
O�B
O�B
O�B
PB
O�B
PB
PbB
P�B
P�B
P�B
QB
Q4B
Q4B
QNB
QNB
Q�B
RoB
R�B
R�B
R�B
RoB
S&B
S�B
T�B
T�B
T�B
T�B
UB
UMB
UMB
U�B
VB
VSB
VSB
V�B
WYB
W�B
W�B
W�B
XB
XB
W�B
X+B
XyB
X_B
XyB
X�B
Y1B
YeB
YB
Y�B
Y�B
Y�B
ZB
ZQB
ZQB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
[qB
[�B
[�B
[�B
[qB
[�B
\B
\B
\]B
\�B
\xB
\�B
\�B
\�B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
_;B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`BB
`\B
`�B
aB
`�B
`�B
`�B
aB
aHB
bB
bB
b4B
b�B
b�B
b�B
c B
c:B
cTB
cnB
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
eFB
e`B
ezB
e�B
ffB
f�B
f�B
f�B
gRB
gmB
g�B
g�B
g�B
h$B
hsB
hsB
hsB
iB
i_B
i_B
i�B
i�B
i�B
j0B
j0B
j0B
j�B
j�B
j�B
j�B
j�B
kB
kB
kB
k�B
k�B
k�B
k�B
l=B
l=B
l�B
l�B
m)B
m)B
mCB
mwB
m�B
n�B
n�B
n�B
o B
oB
oOB
oOB
o�B
pB
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qAB
q�B
rB
rGB
r-B
rGB
r|B
r�B
r�B
r�B
r�B
sB
sB
s3B
s�B
t�B
t�B
t�B
t�B
uB
utB
u�B
vzB
vzB
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
xB
xB
xB
xB
xB
xB
xB
xB
x8B
x8B
y	B
yXB
y	B
yXB
y�B
y�B
y�B
y�B
zB
zDB
z�B
z�B
z�B
{JB
{�B
{�B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}VB
}�B
}�B
}�B
~B
~B
~(B
~]B
~]B
~�B
~�B
~�B
}B
}B
�B
�B
�B
�B
� B
�B
�B
�4B
�iB
��B
��B
��B
�UB
�UB
��B
��B
��B
��B
��B
��B
��B
�'B
�[B
��B
��B
��B
��B
�-B
�GB
�aB
�aB
�{B
��B
��B
��B
��B
��B
��B
��B
�3B
�gB
��B
��B
��B
�B
�SB
��B
��B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	\)B	ZkB	[�B	ZQB	ZkB	ZB	ZB	Z7B	ZQB	ZkB	ZkB	ZkB	Z�B	Z�B	[=B	[qB	[�B	[�B	\xB	\xB	\�B	\�B	\�B	]~B	^OB	_�B	`�B	a�B	bNB	gB	�B
�B0;BՁB��B�RB�JB��B�B��B��B��B��BBB�B�B�BB�B!-B$�B$�B$&B#�B!�B!BB�B�B�B�QB��B��B�@B��B{�BpBabB\�BEB&LBmB
��B
��B
�~B
=VB
$@B	��B	��B	��B	�+B	raB	\�B	W�B	O�B	H�B	;�B	4TB	,WB	!�B	$B	  B	 iB�]B	 B��B	SB	VB	qB	$�B	-)B	/�B	33B	?HB	LdB	KDB	W�B	W�B	>BB	2�B	M�B	F?B	OvB	f�B	s�B	R�B	iDB	^�B	h�B	lB	~wB	��B	�=B	רB	��B	˒B	�/B	�xB	�VB	��B	�+B	�_B	̘B	�"B	�NB	��B	�B	�0B	�B	�sB	��B	�JB	��B	��B
oB	�(B	��B	��B	��B	�OB	��B	ܬB	�B	�XB	ݘB	�ZB	�B	�B	�B	�B	�RB	�sB	��B	�DB	��B	�B	��B	�`B	�-B	�B	�\B	� B	�B	�*B	�RB	�B	�B	��B	�MB	�B	�B	�B	�`B	�2B	�8B	�	B	��B	�lB	�B	�zB	�	B	�B	�JB	��B	�lB	��B	��B	��B	�?B	��B	��B	�^B	��B	��B	�(B	��B	��B	�9B	�|B	�GB	��B	��B	�B	�B	��B	�B	��B	׍B	�B	�fB	�fB	�-B	ބB	�B	��B	ݲB	�pB	�hB	�qB	�RB	��B	ܬB	�/B	ܬB	��B	�OB	�hB	��B	��B	�"B	�yB	�B	�TB	�B	��B	�B	�B	��B	�eB	��B	�kB	׍B	�sB	��B	�5B	��B	�B	�B	�,B	��B	�_B	�B	��B	�`B	�B	��B	��B	�vB	�B	��B	��B	�TB	�B	�B	�B	�ZB	�FB	��B	�B	�`B	�RB	��B	��B	�B	�8B	��B	�B	�*B	��B	��B	�B	�oB	�'B	�B	�B	�B	��B	��B	�B	�iB	�iB	�iB	�OB	��B	�B	�B	�B	�B	��B	�tB	��B	�LB	��B	�lB	�B	��B	�B	�B	��B	��B	��B	�JB	�B	��B	��B	�B	�B	��B	��B	�dB	�dB	��B	��B	��B	�PB	�PB	��B	�<B	��B	��B	��B	��B	��B	�wB	��B	��B	�B	�B	�B	�HB	�}B	��B	��B	��B	��B	��B	�cB	�.B	��B	��B	�B	�B
 OB
 �B
 �B
�B
oB
�B
�B
�B
oB
oB
AB
AB
'B
AB
�B
aB
{B
�B
MB
YB
fB
	�B
	�B
	�B

	B
	�B
	lB
	RB
	7B
	lB
	�B
	�B
B
B
B
\B
\B
�B
�B
�B
�B
�B
\B
\B
�B
�B
}B
�B
 B
�B
�B
TB
�B
�B
@B
�B
aB
�B
B
MB
�B
�B
�B
eB
�B
B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
/B
dB
~B
~B
�B
�B
�B
�B
~B
�B
�B
 \B
!bB
!�B
!�B
!|B
"�B
"�B
"hB
"4B
"4B
"�B
#�B
$&B
$@B
$B
$�B
%,B
%`B
%`B
%zB
%�B
%�B
%�B
&fB
&�B
&�B
&�B
&�B
&�B
'B
&�B
'B
'RB
&�B
&�B
&�B
'B
'RB
'�B
'�B
'�B
'�B
'�B
(�B
*�B
+B
+QB
+QB
+�B
+�B
,B
,WB
,�B
-CB
-CB
-]B
-�B
-�B
-�B
./B
.IB
.}B
.�B
.�B
/5B
/OB
/OB
/5B
/�B
0UB
0;B
0;B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
1'B
0�B
0�B
1'B
1[B
1�B
1�B
1�B
2GB
2�B
2�B
3�B
49B
4�B
5B
5tB
6+B
6FB
6`B
6�B
6�B
6�B
6�B
6�B
7B
7B
6�B
7B
7LB
72B
7B
7LB
7LB
7�B
7�B
7fB
7�B
8RB
88B
8B
7�B
7B
8B
8lB
9	B
9�B
9�B
:�B
:�B
;JB
;dB
;0B
;�B
;dB
;�B
;�B
=�B
=�B
=�B
=�B
=�B
=�B
>]B
>�B
>�B
?�B
@iB
AB
A�B
B'B
B�B
B�B
B�B
B[B
BAB
B'B
B�B
B�B
B�B
C-B
B�B
C{B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
DMB
D�B
EB
E9B
E�B
E�B
E�B
FB
FYB
F�B
F�B
F�B
G+B
GEB
GzB
G�B
G�B
G�B
HB
G�B
HKB
HfB
I7B
I�B
I�B
J=B
JrB
J�B
KB
J�B
KDB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L~B
L�B
MB
M�B
M�B
M�B
N"B
N�B
NpB
N�B
N�B
N�B
N�B
N�B
O(B
O�B
O�B
O�B
O�B
PB
O�B
PB
PbB
P�B
P�B
P�B
QB
Q4B
Q4B
QNB
QNB
Q�B
RoB
R�B
R�B
R�B
RoB
S&B
S�B
T�B
T�B
T�B
T�B
UB
UMB
UMB
U�B
VB
VSB
VSB
V�B
WYB
W�B
W�B
W�B
XB
XB
W�B
X+B
XyB
X_B
XyB
X�B
Y1B
YeB
YB
Y�B
Y�B
Y�B
ZB
ZQB
ZQB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
[qB
[�B
[�B
[�B
[qB
[�B
\B
\B
\]B
\�B
\xB
\�B
\�B
\�B
]�B
]�B
]�B
^OB
^�B
^�B
^�B
_;B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
`BB
`\B
`�B
aB
`�B
`�B
`�B
aB
aHB
bB
bB
b4B
b�B
b�B
b�B
c B
c:B
cTB
cnB
c�B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
eFB
e`B
ezB
e�B
ffB
f�B
f�B
f�B
gRB
gmB
g�B
g�B
g�B
h$B
hsB
hsB
hsB
iB
i_B
i_B
i�B
i�B
i�B
j0B
j0B
j0B
j�B
j�B
j�B
j�B
j�B
kB
kB
kB
k�B
k�B
k�B
k�B
l=B
l=B
l�B
l�B
m)B
m)B
mCB
mwB
m�B
n�B
n�B
n�B
o B
oB
oOB
oOB
o�B
pB
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qAB
q�B
rB
rGB
r-B
rGB
r|B
r�B
r�B
r�B
r�B
sB
sB
s3B
s�B
t�B
t�B
t�B
t�B
uB
utB
u�B
vzB
vzB
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
xB
xB
xB
xB
xB
xB
xB
xB
x8B
x8B
y	B
yXB
y	B
yXB
y�B
y�B
y�B
y�B
zB
zDB
z�B
z�B
z�B
{JB
{�B
{�B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}VB
}�B
}�B
}�B
~B
~B
~(B
~]B
~]B
~�B
~�B
~�B
}B
}B
�B
�B
�B
�B
� B
�B
�B
�4B
�iB
��B
��B
��B
�UB
�UB
��B
��B
��B
��B
��B
��B
��B
�'B
�[B
��B
��B
��B
��B
�-B
�GB
�aB
�aB
�{B
��B
��B
��B
��B
��B
��B
��B
�3B
�gB
��B
��B
��B
�B
�SB
��B
��B
��B
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105252  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193136  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193136  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193136                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043144  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043144  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161506                      G�O�G�O�G�O�                