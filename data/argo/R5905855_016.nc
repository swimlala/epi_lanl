CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:13:31Z creation;2022-06-04T19:13:31Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191331  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�͘_��1   @�͘�y�@.������d
=p��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B ffB��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bo��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C33C�fC  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�3D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @G�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A��\B G�Bz�B�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBgz�Boz�Bwz�B�HB��B��B��B��B��B��B��B��B��B��B�#�B��qB��B��B�#�B��BýqBǽqB��B��B��B��B��B��B��B�#�B�W
B�qB�qB��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC+�C޸C�RC�RC�RC�RC޸C!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC@�CA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC|�C~�C�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D zD ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
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
D���D�?
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
D�=D�?
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
D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��DA��QA���A���A��A��oA��;A��iA��5A��A���A�уAݸ�Aݨ�A�b�A�~A���A�p�A��AۭA�y>A�R�A���A��A���A�
rA��A�P}A�DA�t�A�7�A�F�A�#A�бA͓@A�kQA��Aʟ�Aʄ�A�B�A��#A�sA�x�A���A�TaA���A�:^A��}A�.A��NA�1�A���A��'A�U�A�ΥA�U�A���A�.A��DA�poA�B�A�� A�^jA��A��A�EA�X�A�W�A��/A� �A��A���A�'�A���A�>wA��A��$A��A�R�A�#nA�MA���A�`�A�A��A�T�A���A�:^A��A�6�A�M�A��gA���A�e�A���A���A���A���A���Az��Aw�Ar_Ao��Al�DAi��Ag8�Ad��Ac�zAa�MA_ffA]�,A[��AY�AW*0AS��AM�pAJ�AG��AD$�AB��A?�:A<]dA7sA4�yA4rGA1�|A0�EA1	lA0��A1�A0�hA-
=A+�A(�EA'1'A%�A&(A$�A#~(A"�A!��A J�Ax�A�rAO�AA A@OA:*A��A�dA�4A�'A�AA��AA�A�'A�hA�
A�OA��A�AhsA�A|AJAVA�]A��Am]A|�A�5A�KARTAffA�A�A
��A	�A�ZAzxA	  A	�NA	�MA	�OA	<6A	L0A	A��A��A�[A��A��A�A�A�fAW?A%FAA?A�A��A9�A~(A��A��A~�A�AGEA��AzA��A ��A 4A �@�RT@��@��0@��@�D�@��	@��@��c@�/�@��	@���@�N<@��D@��@��@�PH@�C@�6z@��@��@�$@��@�)_@���@�IR@�p;@�"�@�E�@��@�q@勬@�h
@�7�@� �@��@���@��@���@ව@�(�@�o @ަL@�`�@�K^@��@�IR@��@�!@��E@�ff@��>@٥@َ"@��@ع�@�h�@��3@�|@�!-@֚@�E�@�e�@Ԛ�@�l"@�0U@���@��T@�� @��d@ө*@��@�/�@�G@ѹ�@���@Сb@�V�@�9X@��@ϱ[@�n/@�&�@��@ζ�@�9X@�|@̆Y@�˒@˕�@�(@ʀ�@�E�@ɹ�@���@ȅ�@��@Ǔ�@�6z@�(�@Ʃ�@�bN@�=q@Ō~@ħ�@�YK@�4n@���@â�@�q@�҉@u@�Ov@�J@��^@�w2@�C�@��@��@�V@���@�Q�@���@�H�@��E@��@���@�`�@��}@�P�@��@��4@�ݘ@���@��M@��{@�p�@�k�@�e�@�,�@��.@�*�@��S@�e�@�ں@�Ft@�	@��4@���@��'@�q�@��+@��@��@@�'�@��K@���@���@�z�@��@���@�J@�[W@�(@��/@���@�C�@�{@��@���@�o@��@�b@��P@�e�@��@���@�z�@�'R@���@��k@��@���@�S�@�~@��@�v`@���@�|�@�u%@�9X@���@�~�@�t�@�Z�@���@���@�|�@�!@���@�&@� i@�ѷ@�~�@�-�@�!�@��@��@���@��@���@�N�@�H�@�6�@�@���@�m]@�X@�C�@��@��c@���@�~�@�oi@�Q�@�!@���@��w@���@�IR@�;@��]@���@��@�)�@�_@���@�_p@��K@���@�r�@�\�@�%�@��@���@���@�`B@��@�҉@���@�~(@��@�ƨ@�dZ@���@��@�@���@�S�@�ߤ@��h@�bN@���@���@�Vm@��@���@�|�@�8�@�@��@��6@���@�k�@�a@�Vm@�&@��@���@�n�@�M@���@���@���@�g�@�X�@���@�i�@�8�@���@��#@��a@��0@���@�4�@�Ɇ@��1@�h
@�Ft@�)�@�@���@��X@�e,@�ߤ@�bN@�1@���@�U�@��@��!@�1�@���@��9@���@�iD@�@O@�9�@�+@��@���@�0U@��Q@��	@�;d@�Ɇ@�g8@��@���@���@�Mj@�4�@�;d@�(�@��@��]@��@�H@���@��@�e�@�@��e@�~(@�Q@�?�@��@�4@~�8@~��@}�@}B�@|�.@|9X@{��@{�@z{�@z �@yF@x��@xXy@xb@wa@v��@v��@v�X@v��@v{@u�X@u;@t��@t2�@s��@s$t@r�@r�B@r�1@rTa@q�@q/@p��@p��@pS�@p,=@o�
@oiD@n��@nq�@n@m�@mA @l֡@l�O@lPH@l�@k��@kH�@jں@j�b@j8�@i�@iQ�@hu�@h/�@g�m@g�k@f�@fOv@e��@e��@ec�@d�@d��@dm�@d�@c{J@c;d@c�@b�X@bJ�@a�T@a�h@a4@a�@`�@`�O@`�o@`?�@`�@_��@_qv@_A�@^�c@^Z�@^5?@^ �@]�"@]:�@\��@\c�@\PH@\,=@[��@[� @[��@Z��@Z�@ZkQ@Y�9@Y�C@Yx�@Y0�@Y�@X��@Xw�@W�+@W��@WE9@V�H@V��@VYK@V$�@U��@U�M@U@T�@T��@TN�@T�@S�;@S�w@S�$@Sx@S�@R�,@R��@R@�@Ru@Q��@Q/@P��@P��@P��@P��@P��@Pg8@P7�@O��@OiD@O6z@N�"@NV@N_@M��@M}�@M@Lu�@K�@K��@KF�@K.I@K$t@K"�@K
=@J�,@J�x@JV@I�o@I��@I�@I�@H�j@HI�@H �@H@G�g@Gl�@G@F��@FQ@E��@Ep�@E4@EV@D�v@D��@D��@DU2@C�;@C{J@C;d@B��@B��@BOv@A��@A�t@ADg@A/@A*0@A�@@��@@�I@@��@@7�@?�+@?�:@?�@>��@>R�@>6�@>@=��@=�>@=�9@=�H@=o @<��@<��@<K^@<�@;��@;�4@:��@:ȴ@:�}@:q�@:@9�@9�d@9�~@9N<@9/@8�f@8��@8`�@8-�@87@7�6@7��@7dZ@7,�@6��@6�+@6kQ@6Ov@6	@5�N@5o @4��@4��@47�@3��@333@2�<@2n�@24@1�#@1�"@0��@0|�@0>B@/�]@/�@/P�@/Y@.��@.��@.��@.W�@.�@-�>@-�d@-%@,�u@,_@,	�@+��@+�P@+O@++@*ȴ@*-@)zx@)5�@);@(��@(��@(q@'�]@'��@'��@'$t@&��@&��@&��@&��@&��@&_�@%@%p�@%a�@%<6@%-w@$�K@$�I@$u�@$V�@$�@#�]@#�&@#��@#U�@"�@"�A@"��@"{�@"}V@"xl@"J@!��@!��@!�C@!8�@ ی@ �.@ 9X@�@�6@��@S�@"�@�@��@�@5?@@�@�d@��@|@A @�@��@��@@�@x@4�@S@͟@��@M�@�@��@?}@�f@�D@-�@M@�@�W@��@.I@��@��@�@�r@n�@W�@3�@�-@j@A @�@��@[�@6@"h@�@�
@˒@��@�k@a@@O@'�@o@�@�X@�}@��@p;@h
@M�@#:@�@��@�>@�@�S@�@c�@-w@ѷ@�@Xy@I�@H@K^@7�@�W@� @��@qv@33@�@�@��@�\@^5@�@��@�@F@�@��@�e@��@oi@_@9X@�;@��@iD@>�@�@
��@
�'@
��@
d�@
1�@

�@	��@	�@	a�@	<6@	�@�@֡@�?@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��DA��QA���A���A��A��oA��;A��iA��5A��A���A�уAݸ�Aݨ�A�b�A�~A���A�p�A��AۭA�y>A�R�A���A��A���A�
rA��A�P}A�DA�t�A�7�A�F�A�#A�бA͓@A�kQA��Aʟ�Aʄ�A�B�A��#A�sA�x�A���A�TaA���A�:^A��}A�.A��NA�1�A���A��'A�U�A�ΥA�U�A���A�.A��DA�poA�B�A�� A�^jA��A��A�EA�X�A�W�A��/A� �A��A���A�'�A���A�>wA��A��$A��A�R�A�#nA�MA���A�`�A�A��A�T�A���A�:^A��A�6�A�M�A��gA���A�e�A���A���A���A���A���Az��Aw�Ar_Ao��Al�DAi��Ag8�Ad��Ac�zAa�MA_ffA]�,A[��AY�AW*0AS��AM�pAJ�AG��AD$�AB��A?�:A<]dA7sA4�yA4rGA1�|A0�EA1	lA0��A1�A0�hA-
=A+�A(�EA'1'A%�A&(A$�A#~(A"�A!��A J�Ax�A�rAO�AA A@OA:*A��A�dA�4A�'A�AA��AA�A�'A�hA�
A�OA��A�AhsA�A|AJAVA�]A��Am]A|�A�5A�KARTAffA�A�A
��A	�A�ZAzxA	  A	�NA	�MA	�OA	<6A	L0A	A��A��A�[A��A��A�A�A�fAW?A%FAA?A�A��A9�A~(A��A��A~�A�AGEA��AzA��A ��A 4A �@�RT@��@��0@��@�D�@��	@��@��c@�/�@��	@���@�N<@��D@��@��@�PH@�C@�6z@��@��@�$@��@�)_@���@�IR@�p;@�"�@�E�@��@�q@勬@�h
@�7�@� �@��@���@��@���@ව@�(�@�o @ަL@�`�@�K^@��@�IR@��@�!@��E@�ff@��>@٥@َ"@��@ع�@�h�@��3@�|@�!-@֚@�E�@�e�@Ԛ�@�l"@�0U@���@��T@�� @��d@ө*@��@�/�@�G@ѹ�@���@Сb@�V�@�9X@��@ϱ[@�n/@�&�@��@ζ�@�9X@�|@̆Y@�˒@˕�@�(@ʀ�@�E�@ɹ�@���@ȅ�@��@Ǔ�@�6z@�(�@Ʃ�@�bN@�=q@Ō~@ħ�@�YK@�4n@���@â�@�q@�҉@u@�Ov@�J@��^@�w2@�C�@��@��@�V@���@�Q�@���@�H�@��E@��@���@�`�@��}@�P�@��@��4@�ݘ@���@��M@��{@�p�@�k�@�e�@�,�@��.@�*�@��S@�e�@�ں@�Ft@�	@��4@���@��'@�q�@��+@��@��@@�'�@��K@���@���@�z�@��@���@�J@�[W@�(@��/@���@�C�@�{@��@���@�o@��@�b@��P@�e�@��@���@�z�@�'R@���@��k@��@���@�S�@�~@��@�v`@���@�|�@�u%@�9X@���@�~�@�t�@�Z�@���@���@�|�@�!@���@�&@� i@�ѷ@�~�@�-�@�!�@��@��@���@��@���@�N�@�H�@�6�@�@���@�m]@�X@�C�@��@��c@���@�~�@�oi@�Q�@�!@���@��w@���@�IR@�;@��]@���@��@�)�@�_@���@�_p@��K@���@�r�@�\�@�%�@��@���@���@�`B@��@�҉@���@�~(@��@�ƨ@�dZ@���@��@�@���@�S�@�ߤ@��h@�bN@���@���@�Vm@��@���@�|�@�8�@�@��@��6@���@�k�@�a@�Vm@�&@��@���@�n�@�M@���@���@���@�g�@�X�@���@�i�@�8�@���@��#@��a@��0@���@�4�@�Ɇ@��1@�h
@�Ft@�)�@�@���@��X@�e,@�ߤ@�bN@�1@���@�U�@��@��!@�1�@���@��9@���@�iD@�@O@�9�@�+@��@���@�0U@��Q@��	@�;d@�Ɇ@�g8@��@���@���@�Mj@�4�@�;d@�(�@��@��]@��@�H@���@��@�e�@�@��e@�~(@�Q@�?�@��@�4@~�8@~��@}�@}B�@|�.@|9X@{��@{�@z{�@z �@yF@x��@xXy@xb@wa@v��@v��@v�X@v��@v{@u�X@u;@t��@t2�@s��@s$t@r�@r�B@r�1@rTa@q�@q/@p��@p��@pS�@p,=@o�
@oiD@n��@nq�@n@m�@mA @l֡@l�O@lPH@l�@k��@kH�@jں@j�b@j8�@i�@iQ�@hu�@h/�@g�m@g�k@f�@fOv@e��@e��@ec�@d�@d��@dm�@d�@c{J@c;d@c�@b�X@bJ�@a�T@a�h@a4@a�@`�@`�O@`�o@`?�@`�@_��@_qv@_A�@^�c@^Z�@^5?@^ �@]�"@]:�@\��@\c�@\PH@\,=@[��@[� @[��@Z��@Z�@ZkQ@Y�9@Y�C@Yx�@Y0�@Y�@X��@Xw�@W�+@W��@WE9@V�H@V��@VYK@V$�@U��@U�M@U@T�@T��@TN�@T�@S�;@S�w@S�$@Sx@S�@R�,@R��@R@�@Ru@Q��@Q/@P��@P��@P��@P��@P��@Pg8@P7�@O��@OiD@O6z@N�"@NV@N_@M��@M}�@M@Lu�@K�@K��@KF�@K.I@K$t@K"�@K
=@J�,@J�x@JV@I�o@I��@I�@I�@H�j@HI�@H �@H@G�g@Gl�@G@F��@FQ@E��@Ep�@E4@EV@D�v@D��@D��@DU2@C�;@C{J@C;d@B��@B��@BOv@A��@A�t@ADg@A/@A*0@A�@@��@@�I@@��@@7�@?�+@?�:@?�@>��@>R�@>6�@>@=��@=�>@=�9@=�H@=o @<��@<��@<K^@<�@;��@;�4@:��@:ȴ@:�}@:q�@:@9�@9�d@9�~@9N<@9/@8�f@8��@8`�@8-�@87@7�6@7��@7dZ@7,�@6��@6�+@6kQ@6Ov@6	@5�N@5o @4��@4��@47�@3��@333@2�<@2n�@24@1�#@1�"@0��@0|�@0>B@/�]@/�@/P�@/Y@.��@.��@.��@.W�@.�@-�>@-�d@-%@,�u@,_@,	�@+��@+�P@+O@++@*ȴ@*-@)zx@)5�@);@(��@(��@(q@'�]@'��@'��@'$t@&��@&��@&��@&��@&��@&_�@%@%p�@%a�@%<6@%-w@$�K@$�I@$u�@$V�@$�@#�]@#�&@#��@#U�@"�@"�A@"��@"{�@"}V@"xl@"J@!��@!��@!�C@!8�@ ی@ �.@ 9X@�@�6@��@S�@"�@�@��@�@5?@@�@�d@��@|@A @�@��@��@@�@x@4�@S@͟@��@M�@�@��@?}@�f@�D@-�@M@�@�W@��@.I@��@��@�@�r@n�@W�@3�@�-@j@A @�@��@[�@6@"h@�@�
@˒@��@�k@a@@O@'�@o@�@�X@�}@��@p;@h
@M�@#:@�@��@�>@�@�S@�@c�@-w@ѷ@�@Xy@I�@H@K^@7�@�W@� @��@qv@33@�@�@��@�\@^5@�@��@�@F@�@��@�e@��@oi@_@9X@�;@��@iD@>�@�@
��@
�'@
��@
d�@
1�@

�@	��@	�@	a�@	<6@	�@�@֡@�?@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	jeB	j�B	kB	kB	kB	k6B	k6B	kB	kB	j�B	i�B	i*B	hXB	h
B	jB	xB	��B	��B	�sB	��B	�zB	��B	��B	�xB	��B	��B
,B
!�B
8�B
|�B
��B
� B
�2B
�TB
� B
��B
��B
~BB
��B
�SB
�"B
�B�B
��B
�&B
��B
�QB
�4B
ĜB
ŢB�BuB�BB,�B:*BL�B]�B�RB�-B��B��B��B��B�OB�fB�8BΥB̳B� B�dB��B�\B��B�+B�B�DBt�Bg�B[WB8lB�B
�cB
�?B
��B
�B
��B
��B
�5B
�IB
��B
w2B
X�B
G�B
G_B
'mB
�B
FB
<B	��B	ބB	ĜB	�aB	�B	��B	v�B	gRB	a�B	[qB	RB	N�B	F�B	9�B	/OB	�B	zB�2B�B�B��B�aB�B�B�IB��B��B�xB�,B�oB��B��B�#B��B��B��BǮBՁB�qB� B�OB�;B�|B�B�LB�6B	 B	�B		B	MB	WB	-CB	1vB	%�B	+�B	88B	:^B	G�B	O(B	e,B	p�B	uB	xB	x�B	}�B	��B	��B	��B	�_B	��B	��B	�'B	�oB	�HB	��B	��B	�8B	�zB	�B	��B	�B	��B	��B	�/B	��B	��B	�B	��B	��B	��B	�PB	�0B	��B	�;B	�B	�B	��B	��B	�pB	��B	�CB	��B	�/B	��B	��B	�fB	��B	��B	�oB	��B	�B	өB	��B	��B	��B	�^B	�6B	�B	ϑB	��B	ңB	�[B	��B	�MB	�2B	�B	�B	��B	�B	�yB	�+B	��B	�SB	�TB	��B	�B	�B	ѝB	�hB	уB	�}B	�B	�B	ԕB	��B	ҽB	��B	�uB	�B	�uB	�B	��B	��B	�FB	�TB	�BB	�VB	��B	�vB	�\B	��B	��B	ϫB	�B	��B	ΊB	�B	ˬB	ʦB	�B	�dB	�0B	ˬB	��B	˒B	˒B	�B	�B	̳B	�\B	�bB	�4B	�,B	רB	��B	��B	�yB	��B	��B	��B	�/B	�'B	��B	�BB	�'B	�BB	�BB	�BB	�'B	�B	ߤB	�VB	��B	ޞB	ބB	�B	��B	�OB	�B	��B	�OB	��B	�B	޸B	�!B	�;B	�B	��B	�|B	�@B	�tB	�B	�TB	��B	�2B	�>B	�B	��B	�>B	��B	��B	�B	�B	�B	�B	��B	�kB	�WB	�B	�CB	��B	��B	�IB	�cB	�cB	��B	�B	�IB	��B	�)B	�)B	�]B	��B	��B	��B	��B	�5B	�B	��B	��B	�B	� B	�}B	�cB	�B	��B	� B	��B	��B	�B	�!B	��B	�!B	��B	�B	��B	�AB	��B	�3B	�MB	�hB	�B	�B	�B	�9B	�TB	�B	��B	�zB	��B	��B	�B	�LB	��B	��B	��B	��B	��B	��B	�*B	�xB	��B	�B	��B	��B	��B	��B	�PB	��B	�"B	�"B	�qB	��B	��B	�BB	��B	�cB	��B	��B
 iB
 �B
 �B
 B
�B
[B
3B
9B
�B
�B
%B
tB
�B
�B
�B
�B
B
�B
�B
+B
�B
�B
	B
	B
	7B
	7B
	RB
	RB
	�B
	�B

=B

�B

�B
B
^B
DB
B
xB
xB
�B
JB
JB
dB
�B
�B
dB
JB
JB
�B
�B
B
B
B
�B
�B
�B
"B
B
�B
�B
�B
.B
�B
 B
TB
oB
B
 B
�B
�B
,B
B
�B
B
�B

B
YB
�B
_B
B
�B
B
�B
�B
qB
�B
�B
�B
OB
�B
�B
�B
 'B
 �B
 �B
 �B
!-B
!HB
!�B
"NB
"hB
!�B
"hB
!�B
"4B
"NB
"�B
"�B
"�B
"�B
#:B
#TB
"�B
"�B
# B
#�B
$&B
$&B
$&B
$�B
%B
%B
%�B
&�B
'8B
'�B
(>B
(�B
(�B
*eB
+B
+B
+B
+kB
,"B
+�B
+�B
,�B
-CB
,�B
,�B
-)B
-)B
-�B
-�B
.IB
./B
./B
/B
/�B
/�B
0UB
0�B
0�B
0�B
1B
1�B
1�B
2GB
2�B
2�B
2�B
3�B
4�B
4�B
5B
5ZB
5?B
5�B
6+B
6�B
7B
72B
7�B
8B
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9	B
9rB
:B
:xB
:�B
;0B
;JB
;B
;B
;�B
;�B
<B
<B
<B
<B
<6B
<�B
=<B
="B
=B
<�B
=VB
=qB
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>(B
>wB
>�B
>�B
>�B
?B
?}B
?�B
?�B
@iB
@OB
@OB
@iB
@�B
@�B
@�B
@�B
AB
A;B
A�B
A�B
A�B
BB
BB
B'B
B[B
B�B
B�B
CB
CGB
CGB
C{B
C�B
C�B
C�B
DB
DMB
DgB
D�B
D�B
EB
E9B
ESB
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
GB
GB
G_B
GzB
G�B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
H�B
IB
IB
I7B
I�B
J#B
JXB
JXB
KB
KDB
KDB
KDB
K^B
KxB
K�B
LB
LdB
L~B
L�B
L~B
L�B
MjB
M�B
NB
N<B
N<B
NVB
N�B
N�B
O(B
O(B
OB
OBB
O�B
O�B
PB
PbB
P�B
Q B
Q4B
QNB
Q�B
Q�B
Q�B
Q�B
RoB
R�B
R�B
S&B
S@B
SuB
S�B
S�B
TFB
T,B
T,B
TFB
T�B
T�B
T�B
T�B
T�B
UMB
U�B
V9B
V9B
VSB
VSB
V�B
VmB
V�B
V�B
V�B
W$B
WsB
W�B
W�B
WsB
XB
XEB
X_B
XEB
X�B
X�B
X�B
X�B
YKB
YB
YB
Y�B
ZB
Z7B
ZB
ZB
ZkB
Z�B
Z�B
Z�B
[#B
[qB
[WB
[qB
[�B
[�B
[�B
\CB
\)B
\�B
\�B
]�B
]�B
]�B
^5B
^OB
^�B
^�B
_;B
_pB
_�B
_�B
`BB
`\B
`�B
`�B
`�B
`�B
aB
aB
`�B
a�B
b4B
bhB
b�B
b�B
cB
c B
c B
cnB
dB
d�B
d�B
d�B
e,B
eB
e`B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
h$B
h$B
h>B
hsB
hsB
h�B
h�B
iB
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jB
jB
jB
jB
j�B
kB
k�B
k�B
k�B
k�B
l"B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
m)B
mCB
m]B
m�B
m�B
m�B
nB
ncB
n�B
o B
o5B
oOB
oiB
o�B
o�B
pB
p;B
p�B
p�B
qAB
q�B
qvB
q�B
qvB
q�B
r-B
r-B
r|B
r|B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
tB
tTB
t�B
u%B
uB
u%B
uZB
uZB
utB
utB
u�B
u�B
u�B
u�B
u�B
u�B
vB
vFB
vFB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
w2B
w2B
wLB
w�B
w�B
xB
x8B
xRB
xRB
x8B
xRB
x�B
x�B
x�B
y	B
y>B
yrB
y�B
y�B
y�B
y�B
zDB
z^B
z^B
z�B
z�B
{0B
{JB
{JB
{B
{�B
|B
|jB
|PB
|�B
|�B
}B
}B
}<B
}�B
}�B
}�B
}�B
}�B
~]B
~�B
~�B
~�B
~�B
~�B
B
.B
c11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	jeB	j�B	kB	kB	kB	k6B	k6B	kB	kB	j�B	i�B	i*B	hXB	h
B	jB	xB	��B	��B	�sB	��B	�zB	��B	��B	�xB	��B	��B
,B
!�B
8�B
|�B
��B
� B
�2B
�TB
� B
��B
��B
~BB
��B
�SB
�"B
�B�B
��B
�&B
��B
�QB
�4B
ĜB
ŢB�BuB�BB,�B:*BL�B]�B�RB�-B��B��B��B��B�OB�fB�8BΥB̳B� B�dB��B�\B��B�+B�B�DBt�Bg�B[WB8lB�B
�cB
�?B
��B
�B
��B
��B
�5B
�IB
��B
w2B
X�B
G�B
G_B
'mB
�B
FB
<B	��B	ބB	ĜB	�aB	�B	��B	v�B	gRB	a�B	[qB	RB	N�B	F�B	9�B	/OB	�B	zB�2B�B�B��B�aB�B�B�IB��B��B�xB�,B�oB��B��B�#B��B��B��BǮBՁB�qB� B�OB�;B�|B�B�LB�6B	 B	�B		B	MB	WB	-CB	1vB	%�B	+�B	88B	:^B	G�B	O(B	e,B	p�B	uB	xB	x�B	}�B	��B	��B	��B	�_B	��B	��B	�'B	�oB	�HB	��B	��B	�8B	�zB	�B	��B	�B	��B	��B	�/B	��B	��B	�B	��B	��B	��B	�PB	�0B	��B	�;B	�B	�B	��B	��B	�pB	��B	�CB	��B	�/B	��B	��B	�fB	��B	��B	�oB	��B	�B	өB	��B	��B	��B	�^B	�6B	�B	ϑB	��B	ңB	�[B	��B	�MB	�2B	�B	�B	��B	�B	�yB	�+B	��B	�SB	�TB	��B	�B	�B	ѝB	�hB	уB	�}B	�B	�B	ԕB	��B	ҽB	��B	�uB	�B	�uB	�B	��B	��B	�FB	�TB	�BB	�VB	��B	�vB	�\B	��B	��B	ϫB	�B	��B	ΊB	�B	ˬB	ʦB	�B	�dB	�0B	ˬB	��B	˒B	˒B	�B	�B	̳B	�\B	�bB	�4B	�,B	רB	��B	��B	�yB	��B	��B	��B	�/B	�'B	��B	�BB	�'B	�BB	�BB	�BB	�'B	�B	ߤB	�VB	��B	ޞB	ބB	�B	��B	�OB	�B	��B	�OB	��B	�B	޸B	�!B	�;B	�B	��B	�|B	�@B	�tB	�B	�TB	��B	�2B	�>B	�B	��B	�>B	��B	��B	�B	�B	�B	�B	��B	�kB	�WB	�B	�CB	��B	��B	�IB	�cB	�cB	��B	�B	�IB	��B	�)B	�)B	�]B	��B	��B	��B	��B	�5B	�B	��B	��B	�B	� B	�}B	�cB	�B	��B	� B	��B	��B	�B	�!B	��B	�!B	��B	�B	��B	�AB	��B	�3B	�MB	�hB	�B	�B	�B	�9B	�TB	�B	��B	�zB	��B	��B	�B	�LB	��B	��B	��B	��B	��B	��B	�*B	�xB	��B	�B	��B	��B	��B	��B	�PB	��B	�"B	�"B	�qB	��B	��B	�BB	��B	�cB	��B	��B
 iB
 �B
 �B
 B
�B
[B
3B
9B
�B
�B
%B
tB
�B
�B
�B
�B
B
�B
�B
+B
�B
�B
	B
	B
	7B
	7B
	RB
	RB
	�B
	�B

=B

�B

�B
B
^B
DB
B
xB
xB
�B
JB
JB
dB
�B
�B
dB
JB
JB
�B
�B
B
B
B
�B
�B
�B
"B
B
�B
�B
�B
.B
�B
 B
TB
oB
B
 B
�B
�B
,B
B
�B
B
�B

B
YB
�B
_B
B
�B
B
�B
�B
qB
�B
�B
�B
OB
�B
�B
�B
 'B
 �B
 �B
 �B
!-B
!HB
!�B
"NB
"hB
!�B
"hB
!�B
"4B
"NB
"�B
"�B
"�B
"�B
#:B
#TB
"�B
"�B
# B
#�B
$&B
$&B
$&B
$�B
%B
%B
%�B
&�B
'8B
'�B
(>B
(�B
(�B
*eB
+B
+B
+B
+kB
,"B
+�B
+�B
,�B
-CB
,�B
,�B
-)B
-)B
-�B
-�B
.IB
./B
./B
/B
/�B
/�B
0UB
0�B
0�B
0�B
1B
1�B
1�B
2GB
2�B
2�B
2�B
3�B
4�B
4�B
5B
5ZB
5?B
5�B
6+B
6�B
7B
72B
7�B
8B
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9	B
9rB
:B
:xB
:�B
;0B
;JB
;B
;B
;�B
;�B
<B
<B
<B
<B
<6B
<�B
=<B
="B
=B
<�B
=VB
=qB
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>(B
>wB
>�B
>�B
>�B
?B
?}B
?�B
?�B
@iB
@OB
@OB
@iB
@�B
@�B
@�B
@�B
AB
A;B
A�B
A�B
A�B
BB
BB
B'B
B[B
B�B
B�B
CB
CGB
CGB
C{B
C�B
C�B
C�B
DB
DMB
DgB
D�B
D�B
EB
E9B
ESB
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
GB
GB
G_B
GzB
G�B
G�B
G�B
H1B
H�B
H�B
H�B
H�B
H�B
IB
IB
I7B
I�B
J#B
JXB
JXB
KB
KDB
KDB
KDB
K^B
KxB
K�B
LB
LdB
L~B
L�B
L~B
L�B
MjB
M�B
NB
N<B
N<B
NVB
N�B
N�B
O(B
O(B
OB
OBB
O�B
O�B
PB
PbB
P�B
Q B
Q4B
QNB
Q�B
Q�B
Q�B
Q�B
RoB
R�B
R�B
S&B
S@B
SuB
S�B
S�B
TFB
T,B
T,B
TFB
T�B
T�B
T�B
T�B
T�B
UMB
U�B
V9B
V9B
VSB
VSB
V�B
VmB
V�B
V�B
V�B
W$B
WsB
W�B
W�B
WsB
XB
XEB
X_B
XEB
X�B
X�B
X�B
X�B
YKB
YB
YB
Y�B
ZB
Z7B
ZB
ZB
ZkB
Z�B
Z�B
Z�B
[#B
[qB
[WB
[qB
[�B
[�B
[�B
\CB
\)B
\�B
\�B
]�B
]�B
]�B
^5B
^OB
^�B
^�B
_;B
_pB
_�B
_�B
`BB
`\B
`�B
`�B
`�B
`�B
aB
aB
`�B
a�B
b4B
bhB
b�B
b�B
cB
c B
c B
cnB
dB
d�B
d�B
d�B
e,B
eB
e`B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
g�B
g�B
h$B
h$B
h>B
hsB
hsB
h�B
h�B
iB
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jB
jB
jB
jB
j�B
kB
k�B
k�B
k�B
k�B
l"B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
m)B
mCB
m]B
m�B
m�B
m�B
nB
ncB
n�B
o B
o5B
oOB
oiB
o�B
o�B
pB
p;B
p�B
p�B
qAB
q�B
qvB
q�B
qvB
q�B
r-B
r-B
r|B
r|B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
tB
tTB
t�B
u%B
uB
u%B
uZB
uZB
utB
utB
u�B
u�B
u�B
u�B
u�B
u�B
vB
vFB
vFB
vFB
vzB
v�B
v�B
v�B
v�B
v�B
w2B
w2B
wLB
w�B
w�B
xB
x8B
xRB
xRB
x8B
xRB
x�B
x�B
x�B
y	B
y>B
yrB
y�B
y�B
y�B
y�B
zDB
z^B
z^B
z�B
z�B
{0B
{JB
{JB
{B
{�B
|B
|jB
|PB
|�B
|�B
}B
}B
}<B
}�B
}�B
}�B
}�B
}�B
~]B
~�B
~�B
~�B
~�B
~�B
B
.B
c11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105229  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191331  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191331  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191331                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041338  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041338  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                