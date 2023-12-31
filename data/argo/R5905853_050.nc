CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:31:38Z creation;2022-06-04T17:31:38Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173138  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               2A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�!񑢳�1   @�!�'O@.�l�C���c I�^5?1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�ffB�  B�  B�  B�  B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�ffB���B�ffB�ffB�  B�  B�  B�  C   C  C  C33C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&L�C'�fC*  C,33C-� C0  C1�fC4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�C3D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @$z�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��qB��B�W
B��B��B��B��B��B��B�W
B��>BýqB��B��B��B��B��B��B�W
B�qB�W
B�W
B��B��B��B��B��C�RC�RC+�C޸C	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC$�C&EC'޸C)�RC,+�C-xRC/�RC1޸C3�RC5޸C7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC\�C]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��\C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
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
D�B=D��=D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�B=Dׂ=D׿
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
D���D�?
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
D�8�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��MA��A��AA���A��|A��MA��A��A���A��`A��A��KA��WA���A��WA���A���A�� A���A��A��AA��A��A��A��A��A���A���A��/A��WA��)A��TA���A��|A���A���A�HAɡbA�b�Aɷ�A�1A��`A�5?A�Y�A���A�R�A�d�A���A���A��A�?HA���A�!bA���A�hA��6A�z�A�"4A�z�A�A�ܒA��A���A��)A���A�ŢA���A��:A���A��A���A���A��A�~A�eA�ҽA��KA��A���A���A��hA�x8A�A��A~��A|v�Av�Ao�oAh�.A_%FAZ��AU�AQ�AMW?AL��AL��AJ�5AI��AIF�AI�AG1'AEOAA�LA@T�A?+kA=�sA<C�A;6A9��A9!A8p;A6�A5��A5��A5��A4�'A4?�A3�A2��A3�A2��A1��A1w2A0��A/�qA.?}A-(�A,�A,�mA,�MA+�A*�_A)y>A)1'A)R�A(��A(&�A'7LA&ȴA&A%/�A%MjA$�jA$A�A#�nA#��A#=qA"�~A"_pA"T�A";A ��A�)A[WA{A�A��A�A�"A7LA͟A��AN<A�A��A�VAC-A(A�]A�0A��A8�A�A��A�cA0�AA�_A	lA��A��A^5A��A)�A�<A�dA�SAS&A(�A��Ah�A�.A\�A�A��A�Ay�AߤAVmA�A[�A�Ab�A�]A�A$tA�A��ARTA�A
֡A
��A
��A
X�A	�A	l"A	\)A	�A��A�A��AO�A�A9XA�yA��AoAȴA�sA��A{A��A҉A�-Av`A��An�A�A��A&�A �|A ��A �@���@���@�"�@�J�@�Ɇ@��d@�E�@�c�@�}V@��`@�n�@��@��s@�"h@�K�@���@��+@�~�@��@�PH@�l�@���@�zx@�bN@흲@���@� @줩@�Ov@�H@�F�@�z@���@�@�%F@�4@�"@��p@��@�(�@䲖@�N<@�j@�GE@���@�X@�p;@��B@�?@��m@�s�@��@�^5@���@�*0@��@�S�@�@پw@��@�U2@�$�@��@�+@ָR@�kQ@�
�@�e,@�Ɇ@�ff@�C�@��@�C�@�p;@�@�qv@а!@�Z�@Ϟ�@�@΁o@� �@͆�@�0�@̡b@�H@˳�@�!�@̄�@�:*@ˉ7@��@ʢ4@��#@�%F@ț�@�x@ư�@�@Ń{@Ű�@œ@��	@ĂA@���@�N<@��K@���@��S@��@���@��{@�O@���@�ں@���@�?�@��@�X@��z@�u%@�5?@��@�qv@�Dg@��P@��A@��@��@�(�@��@�g8@��@�{J@�f�@�:�@��s@���@�1'@��Q@���@�u�@��5@��U@��u@�g8@��@�W?@�� @�@��f@��@�V@���@�%@�z@��@�@O@�7L@���@��@�:�@��@��a@���@�1�@���@�Ov@��+@��3@�s�@���@��r@�a|@�w�@� �@�c@�s@�S&@� \@�%@��b@�u%@�a|@�(�@��@��N@���@��n@�[W@�+�@��@��L@�oi@�C�@��D@��X@�&�@��@��u@�($@�ϫ@��*@��'@�=�@��]@���@�c�@�7�@���@��n@��@�ں@���@�xl@�,=@���@���@���@�o�@��@���@�~(@��T@�c�@�@O@�V@��p@���@�6�@��@���@��C@�m]@�"�@���@��I@��@�bN@�C-@�'R@��@���@��@���@���@�Ta@���@���@�33@��@��p@�d�@�YK@�!@��@���@�]�@�j�@�O@���@�ѷ@�}V@��=@�5�@�+�@�O�@�X�@�N<@�+@��@�~(@�9X@��@���@�e,@�J�@�%F@��@��@��2@��@��@�2�@��}@���@�RT@��@���@�J�@�O@��F@�9X@�zx@�
=@�Y�@��@@��a@���@��@��=@�X@�%@���@��@��#@��=@�IR@���@�ں@��O@���@�S�@�%�@�@���@���@�dZ@�+�@�@�S@�͟@���@�`�@�!�@��@��@���@�[W@�(@�͟@���@�m�@�Ov@�;�@�-�@��@��Q@��@�w2@�<6@��@���@���@�r�@�#:@�m@�V@!-@~�@~_�@}�@}+�@|��@|y>@{�r@{��@{�@z($@y�T@yo @x�@xV�@w��@w=@v��@u�)@u�@u�@ua�@u@tC-@s�}@s�@s i@r�A@rJ�@r@q�N@q��@qY�@p�U@p�@o>�@n��@n��@m�@m:�@l2�@kt�@kb�@k��@l,=@l�@k�*@k��@j��@i��@i`B@h�[@h��@h�@g_p@g8@f�!@f!�@fJ�@e�)@d��@d6@c�w@c�F@cC�@b�@bW�@b��@bH�@`�5@`�@_�@_t�@_(@^��@^�b@^ȴ@^�B@^\�@]ԕ@]�~@]p�@]V@\��@\��@\C-@[��@[��@[!-@Zߤ@Z��@Z��@Z#:@Y�Z@Y�o@Y�3@YJ�@X�p@Xq@XS�@X*�@W��@W��@W�P@W/�@W4�@W9�@Wo@V�H@Vq�@VC�@VO@Vu@U�#@U�=@U�@Uu�@UX@U=�@U&�@T�@T��@T�O@T��@T6@T  @S�w@S|�@S�@R�@R�1@RZ�@Q�@Qp�@P��@P�.@Pm�@PN�@PG@O��@OY@N��@NW�@N.�@M�@M��@MF@L�@L�O@Lq@LPH@L%�@K�K@K
=@J�]@J�}@JM�@I�j@Ix�@I*0@H�[@H>B@H�@G�a@Go�@GS@F�@F�@F��@F��@FJ�@E�@E�^@E:�@D�@D�.@Dw�@D?�@C��@Cl�@C�@C,�@B��@B��@B:*@A��@A�~@Aa�@A \@@�P@@��@@��@@�@?��@?
=@>��@>Ov@>�@=�9@=�~@=hs@<�@<�I@<S�@;��@;�K@;��@;RT@:�2@:��@:s�@:GE@9�T@9��@9�h@9/@8�/@8Z@7��@7O@7�@6�H@6q�@6B[@5�@5��@5�=@5IR@4�?@4�.@4"h@3�q@3dZ@34�@3,�@2�@2M�@1�n@1/@1#�@1q@1�@0��@0�@0Ɇ@0z�@0bN@0M@0	�@/��@/C@.�2@.��@.}V@.�@-�n@-s�@- \@,��@,�_@,7@+a@+@O@+C�@*��@*��@*��@*@)s�@)hs@)+�@(u�@(7�@'�@'��@'1�@'Y@'S@&�@&�'@&�1@&M�@&.�@%�@%��@%�h@%?}@$��@$Ĝ@$��@$�@#��@#�V@#��@#�{@#v`@#;d@"�B@"E�@"
�@!��@!�Z@!��@!0�@ ��@ �?@ ��@ �@ ��@ Q�@��@�@��@C�@�F@R�@:*@�D@�@��@x�@o @f�@X@+�@�@֡@�j@��@u�@c�@Q�@A�@/�@�@��@��@/�@�M@�]@�L@Ov@+k@
�@�d@�C@k�@-w@��@�O@u�@�@�6@�0@�	@dZ@F�@@��@^5@1�@ԕ@x�@q@��@~(@D�@@��@�a@x@P�@J#@&@��@�H@�B@��@R�@	@�@�d@�t@��@?}@!�@�@�@��@(�@ݘ@��@��@C�@@�@�6@��@Q@#:@�@�9@�3@��@G�@�@�@�@��@�?@��@��@��@�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��MA��A��AA���A��|A��MA��A��A���A��`A��A��KA��WA���A��WA���A���A�� A���A��A��AA��A��A��A��A��A���A���A��/A��WA��)A��TA���A��|A���A���A�HAɡbA�b�Aɷ�A�1A��`A�5?A�Y�A���A�R�A�d�A���A���A��A�?HA���A�!bA���A�hA��6A�z�A�"4A�z�A�A�ܒA��A���A��)A���A�ŢA���A��:A���A��A���A���A��A�~A�eA�ҽA��KA��A���A���A��hA�x8A�A��A~��A|v�Av�Ao�oAh�.A_%FAZ��AU�AQ�AMW?AL��AL��AJ�5AI��AIF�AI�AG1'AEOAA�LA@T�A?+kA=�sA<C�A;6A9��A9!A8p;A6�A5��A5��A5��A4�'A4?�A3�A2��A3�A2��A1��A1w2A0��A/�qA.?}A-(�A,�A,�mA,�MA+�A*�_A)y>A)1'A)R�A(��A(&�A'7LA&ȴA&A%/�A%MjA$�jA$A�A#�nA#��A#=qA"�~A"_pA"T�A";A ��A�)A[WA{A�A��A�A�"A7LA͟A��AN<A�A��A�VAC-A(A�]A�0A��A8�A�A��A�cA0�AA�_A	lA��A��A^5A��A)�A�<A�dA�SAS&A(�A��Ah�A�.A\�A�A��A�Ay�AߤAVmA�A[�A�Ab�A�]A�A$tA�A��ARTA�A
֡A
��A
��A
X�A	�A	l"A	\)A	�A��A�A��AO�A�A9XA�yA��AoAȴA�sA��A{A��A҉A�-Av`A��An�A�A��A&�A �|A ��A �@���@���@�"�@�J�@�Ɇ@��d@�E�@�c�@�}V@��`@�n�@��@��s@�"h@�K�@���@��+@�~�@��@�PH@�l�@���@�zx@�bN@흲@���@� @줩@�Ov@�H@�F�@�z@���@�@�%F@�4@�"@��p@��@�(�@䲖@�N<@�j@�GE@���@�X@�p;@��B@�?@��m@�s�@��@�^5@���@�*0@��@�S�@�@پw@��@�U2@�$�@��@�+@ָR@�kQ@�
�@�e,@�Ɇ@�ff@�C�@��@�C�@�p;@�@�qv@а!@�Z�@Ϟ�@�@΁o@� �@͆�@�0�@̡b@�H@˳�@�!�@̄�@�:*@ˉ7@��@ʢ4@��#@�%F@ț�@�x@ư�@�@Ń{@Ű�@œ@��	@ĂA@���@�N<@��K@���@��S@��@���@��{@�O@���@�ں@���@�?�@��@�X@��z@�u%@�5?@��@�qv@�Dg@��P@��A@��@��@�(�@��@�g8@��@�{J@�f�@�:�@��s@���@�1'@��Q@���@�u�@��5@��U@��u@�g8@��@�W?@�� @�@��f@��@�V@���@�%@�z@��@�@O@�7L@���@��@�:�@��@��a@���@�1�@���@�Ov@��+@��3@�s�@���@��r@�a|@�w�@� �@�c@�s@�S&@� \@�%@��b@�u%@�a|@�(�@��@��N@���@��n@�[W@�+�@��@��L@�oi@�C�@��D@��X@�&�@��@��u@�($@�ϫ@��*@��'@�=�@��]@���@�c�@�7�@���@��n@��@�ں@���@�xl@�,=@���@���@���@�o�@��@���@�~(@��T@�c�@�@O@�V@��p@���@�6�@��@���@��C@�m]@�"�@���@��I@��@�bN@�C-@�'R@��@���@��@���@���@�Ta@���@���@�33@��@��p@�d�@�YK@�!@��@���@�]�@�j�@�O@���@�ѷ@�}V@��=@�5�@�+�@�O�@�X�@�N<@�+@��@�~(@�9X@��@���@�e,@�J�@�%F@��@��@��2@��@��@�2�@��}@���@�RT@��@���@�J�@�O@��F@�9X@�zx@�
=@�Y�@��@@��a@���@��@��=@�X@�%@���@��@��#@��=@�IR@���@�ں@��O@���@�S�@�%�@�@���@���@�dZ@�+�@�@�S@�͟@���@�`�@�!�@��@��@���@�[W@�(@�͟@���@�m�@�Ov@�;�@�-�@��@��Q@��@�w2@�<6@��@���@���@�r�@�#:@�m@�V@!-@~�@~_�@}�@}+�@|��@|y>@{�r@{��@{�@z($@y�T@yo @x�@xV�@w��@w=@v��@u�)@u�@u�@ua�@u@tC-@s�}@s�@s i@r�A@rJ�@r@q�N@q��@qY�@p�U@p�@o>�@n��@n��@m�@m:�@l2�@kt�@kb�@k��@l,=@l�@k�*@k��@j��@i��@i`B@h�[@h��@h�@g_p@g8@f�!@f!�@fJ�@e�)@d��@d6@c�w@c�F@cC�@b�@bW�@b��@bH�@`�5@`�@_�@_t�@_(@^��@^�b@^ȴ@^�B@^\�@]ԕ@]�~@]p�@]V@\��@\��@\C-@[��@[��@[!-@Zߤ@Z��@Z��@Z#:@Y�Z@Y�o@Y�3@YJ�@X�p@Xq@XS�@X*�@W��@W��@W�P@W/�@W4�@W9�@Wo@V�H@Vq�@VC�@VO@Vu@U�#@U�=@U�@Uu�@UX@U=�@U&�@T�@T��@T�O@T��@T6@T  @S�w@S|�@S�@R�@R�1@RZ�@Q�@Qp�@P��@P�.@Pm�@PN�@PG@O��@OY@N��@NW�@N.�@M�@M��@MF@L�@L�O@Lq@LPH@L%�@K�K@K
=@J�]@J�}@JM�@I�j@Ix�@I*0@H�[@H>B@H�@G�a@Go�@GS@F�@F�@F��@F��@FJ�@E�@E�^@E:�@D�@D�.@Dw�@D?�@C��@Cl�@C�@C,�@B��@B��@B:*@A��@A�~@Aa�@A \@@�P@@��@@��@@�@?��@?
=@>��@>Ov@>�@=�9@=�~@=hs@<�@<�I@<S�@;��@;�K@;��@;RT@:�2@:��@:s�@:GE@9�T@9��@9�h@9/@8�/@8Z@7��@7O@7�@6�H@6q�@6B[@5�@5��@5�=@5IR@4�?@4�.@4"h@3�q@3dZ@34�@3,�@2�@2M�@1�n@1/@1#�@1q@1�@0��@0�@0Ɇ@0z�@0bN@0M@0	�@/��@/C@.�2@.��@.}V@.�@-�n@-s�@- \@,��@,�_@,7@+a@+@O@+C�@*��@*��@*��@*@)s�@)hs@)+�@(u�@(7�@'�@'��@'1�@'Y@'S@&�@&�'@&�1@&M�@&.�@%�@%��@%�h@%?}@$��@$Ĝ@$��@$�@#��@#�V@#��@#�{@#v`@#;d@"�B@"E�@"
�@!��@!�Z@!��@!0�@ ��@ �?@ ��@ �@ ��@ Q�@��@�@��@C�@�F@R�@:*@�D@�@��@x�@o @f�@X@+�@�@֡@�j@��@u�@c�@Q�@A�@/�@�@��@��@/�@�M@�]@�L@Ov@+k@
�@�d@�C@k�@-w@��@�O@u�@�@�6@�0@�	@dZ@F�@@��@^5@1�@ԕ@x�@q@��@~(@D�@@��@�a@x@P�@J#@&@��@�H@�B@��@R�@	@�@�d@�t@��@?}@!�@�@�@��@(�@ݘ@��@��@C�@@�@�6@��@Q@#:@�@�9@�3@��@G�@�@�@�@��@�?@��@��@��@�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	>�B	>wB	>�B	>wB	>�B	>�B	>�B	>�B	>�B	>�B	>(B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	="B	=<B	<�B	=VB	=<B	="B	<�B	;B�B�}B�dB	sB	L�B	g�B	oiB	x�B	�uB	�<B	��B	ƨB
"NB
YB
uB
�B
�B
��B
�B
�MB�B5BIB,�BL�BI�B0�B)�B_Bq'BqABgBV�BCB3MB5�BdB�B
�B
��B
�B
��B
U�B
B	��B	��B	��B	�HB	��B	��B	e�B	L�B	<�B	-�B	$�B	�B	�B�wB	�B	)�B	=�B	@iB	@iB	JXB	F�B	6zB	-wB	*�B	.�B	:�B	=B	WsB	_�B	f�B	d&B	l�B	vzB	��B	��B	�IB	�B	�hB	˒B	��B	�XB	��B	��B	�*B	��B	�B	��B	��B
AB
�B
�B

rB
YB
MB
PB
�B
9B
�B
�B
 �B
"4B
+�B
-�B
.cB
-�B
.�B
-wB
/5B
0;B
6`B
6�B
0�B
4�B
6�B
88B
<jB
<6B
9rB
9�B
<6B
;dB
;B
;B
<jB
@iB
?B
=VB
?�B
BB
CB
F�B
GEB
E�B
D�B
F�B
E9B
G�B
F�B
G�B
IRB
K^B
J#B
O�B
RoB
T�B
]�B
a�B
[#B
_�B
`vB
a�B
abB
_;B
^�B
\�B
\CB
^jB
Z�B
X�B
W�B
W
B
V�B
T�B
RoB
QNB
OB
M�B
MB
L~B
KDB
KxB
M�B
S�B
SB
QB
O�B
P�B
R B
O�B
OvB
N�B
N<B
K�B
IB
FYB
G�B
GzB
EB
HfB
G_B
D�B
AoB
D3B
D�B
C�B
>�B
4nB
.B
,"B
)�B
+�B
*�B
)_B
(�B
)�B
,�B
1�B
0�B
./B
+�B
*KB
'�B
#TB
!�B
 \B
�B
IB
B
�B
]B
=B
�B
eB
�B
�B
�B
�B
�B
@B
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
yB
YB
gB
B
:B
}B
�B
�B
\B
<B
JB
	�B
	lB
	�B

#B
	RB
�B
B
?B
mB
�B
YB
�B
B
�B
�B
SB
�B
mB
MB
aB
AB
�B
'B
�B
�B
 �B
 OB	�BB	�B	�BB	��B	��B	��B	��B	�B	�dB	��B	��B	�(B	�(B
uB
�B
B
mB
�B
B
�B
�B
MB
�B	�.B	�B	��B
�B
B
B
tB
�B
�B
�B
�B
SB
�B
�B
�B
GB
�B
B
�B
YB
%B
�B
B
�B
�B
�B
�B
�B
�B
	7B
�B
�B
�B
�B
zB
YB
tB
KB
	B
	B

#B
	�B

	B

	B

rB

XB

�B

�B
)B
�B
�B
�B
�B
(B
BB
VB
�B
6B
B
JB

�B
dB
�B
0B
DB

�B

=B
	�B

#B

XB
�B
�B
~B
�B
�B
�B
"B
�B
�B
�B
�B
�B
B
�B
&B
[B
�B
�B
�B
�B
�B
�B
�B
,B
gB
$B
?B
sB
�B
�B
$B
�B
9B
�B
�B
SB
B
mB
�B
�B
�B
$B
�B
YB
_B
+B
�B
B
EB
_B
yB
yB
�B
�B
B
�B
eB
�B
�B
B
kB
7B
QB
kB
B
	B
�B
�B
#B
�B
�B
�B
�B
qB
�B
�B
�B
QB
7B
7B
7B
�B
7B
�B
�B
QB
�B
�B
�B
)B
�B
�B
�B
]B
B
�B
qB
B
�B
 \B
!bB
"�B
"�B
"hB
!�B
!�B
"�B
"4B
# B
#�B
$ZB
&�B
&�B
&�B
&�B
'B
'8B
&�B
&�B
&�B
'8B
'B
'mB
'B
&�B
#�B
 �B
;B
"NB
&LB
(>B
)�B
*�B
+B
+�B
+�B
,=B
,�B
-�B
-�B
-�B
-�B
-�B
.B
.cB
.�B
.�B
.�B
.�B
/ B
/�B
/�B
/iB
/iB
/�B
/�B
0UB
0�B
0�B
0�B
1'B
0�B
1AB
1AB
1vB
1�B
1�B
1�B
1�B
1�B
2GB
2GB
2aB
2�B
2�B
2�B
3B
3B
3�B
3�B
3�B
4B
4B
4�B
4�B
4�B
4�B
5%B
5ZB
5ZB
5�B
6B
5tB
5�B
6`B
6�B
6�B
6�B
7B
7�B
7�B
8B
7�B
7�B
7fB
7�B
8B
8�B
8�B
8�B
8�B
8�B
8�B
9$B
:DB
:�B
;B
;dB
;JB
:�B
9�B
9$B
8�B
9>B
:^B
<6B
=<B
=�B
=�B
>(B
=�B
=VB
<�B
=B
=<B
<�B
=B
=�B
>]B
?�B
?�B
>�B
>�B
>�B
?�B
?HB
?B
>�B
A;B
A�B
@�B
?�B
@�B
@�B
BuB
C�B
C�B
D�B
E�B
F%B
F?B
F%B
F%B
F�B
G+B
G_B
G_B
GB
GB
G�B
G�B
G�B
G�B
H�B
HfB
H�B
H�B
H�B
HfB
H�B
IlB
J�B
J�B
KB
KDB
K)B
K^B
K�B
K�B
K�B
LB
LdB
L~B
L�B
LdB
L0B
LJB
LJB
LdB
LdB
L0B
L�B
M�B
M�B
M�B
N"B
N<B
NpB
N�B
N�B
OB
N�B
N�B
OB
OBB
O\B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
QB
Q B
QhB
Q�B
Q�B
RTB
RoB
R�B
R�B
R�B
R�B
S&B
S&B
S@B
S�B
S�B
TFB
T,B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UgB
UMB
T�B
T�B
U2B
UMB
U�B
U�B
U�B
V�B
W$B
WYB
W�B
W�B
X+B
XB
XEB
XEB
X�B
XyB
X�B
X�B
YB
Y1B
Y1B
Y1B
Y�B
Y�B
ZB
Z7B
ZkB
Z�B
[	B
[=B
[WB
[�B
\B
\]B
\�B
\�B
]/B
]/B
]dB
]�B
^B
^�B
^�B
_�B
_�B
_�B
_�B
`'B
`�B
`vB
`�B
`�B
aHB
aHB
a�B
b4B
b�B
b�B
b�B
b�B
cTB
dB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d&B
dB
dtB
d�B
e`B
e�B
e�B
fB
fB
fB
e�B
ffB
gmB
gmB
g�B
gmB
g�B
hXB
h
B
h
B
h>B
h>B
gB
g�B
g�B
h
B
h�B
h�B
h�B
iB
iDB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
j0B
j0B
jB
j�B
j�B
kB
k�B
lB
l=B
lWB
lWB
lWB
lqB
l�B
m)B
mCB
mCB
m)B
m�B
m�B
m�B
m�B
nB
n/B
nIB
n}B
n�B
o B
oB
o5B
o�B
o�B
pB
pUB
p�B
p�B
q'B
qAB
q'B
qAB
q�B
q�B
q�B
q�B
q�B
rGB
r-B
rGB
rGB
rGB
rGB
rGB
r�B
r�B
sB
s3B
s3B
s�B
s�B
s�B
t9B
tTB
tnB
t�B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
vB
v+B
v`B
v�B
v�B
v�B
wLB
w�B
xB
x8B
x�B
x�B
y	B
y>B
y>B
y�B
y�B
y�B
y�B
zB
zB
zB
z^B
z�B
z�B
z�B
{B
z�B
{0B
{�B
{�B
{�B
{�B
|B
|�B
|�B
|�B
}B
}<B
}qB
}�B
}�B
}�B
~(B
~BB
~wB
~wB
~�B
~�B
.B
HB
.B
�B
}B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	>�B	>wB	>�B	>wB	>�B	>�B	>�B	>�B	>�B	>�B	>(B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	=�B	="B	=<B	<�B	=VB	=<B	="B	<�B	;B�B�}B�dB	sB	L�B	g�B	oiB	x�B	�uB	�<B	��B	ƨB
"NB
YB
uB
�B
�B
��B
�B
�MB�B5BIB,�BL�BI�B0�B)�B_Bq'BqABgBV�BCB3MB5�BdB�B
�B
��B
�B
��B
U�B
B	��B	��B	��B	�HB	��B	��B	e�B	L�B	<�B	-�B	$�B	�B	�B�wB	�B	)�B	=�B	@iB	@iB	JXB	F�B	6zB	-wB	*�B	.�B	:�B	=B	WsB	_�B	f�B	d&B	l�B	vzB	��B	��B	�IB	�B	�hB	˒B	��B	�XB	��B	��B	�*B	��B	�B	��B	��B
AB
�B
�B

rB
YB
MB
PB
�B
9B
�B
�B
 �B
"4B
+�B
-�B
.cB
-�B
.�B
-wB
/5B
0;B
6`B
6�B
0�B
4�B
6�B
88B
<jB
<6B
9rB
9�B
<6B
;dB
;B
;B
<jB
@iB
?B
=VB
?�B
BB
CB
F�B
GEB
E�B
D�B
F�B
E9B
G�B
F�B
G�B
IRB
K^B
J#B
O�B
RoB
T�B
]�B
a�B
[#B
_�B
`vB
a�B
abB
_;B
^�B
\�B
\CB
^jB
Z�B
X�B
W�B
W
B
V�B
T�B
RoB
QNB
OB
M�B
MB
L~B
KDB
KxB
M�B
S�B
SB
QB
O�B
P�B
R B
O�B
OvB
N�B
N<B
K�B
IB
FYB
G�B
GzB
EB
HfB
G_B
D�B
AoB
D3B
D�B
C�B
>�B
4nB
.B
,"B
)�B
+�B
*�B
)_B
(�B
)�B
,�B
1�B
0�B
./B
+�B
*KB
'�B
#TB
!�B
 \B
�B
IB
B
�B
]B
=B
�B
eB
�B
�B
�B
�B
�B
@B
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
yB
YB
gB
B
:B
}B
�B
�B
\B
<B
JB
	�B
	lB
	�B

#B
	RB
�B
B
?B
mB
�B
YB
�B
B
�B
�B
SB
�B
mB
MB
aB
AB
�B
'B
�B
�B
 �B
 OB	�BB	�B	�BB	��B	��B	��B	��B	�B	�dB	��B	��B	�(B	�(B
uB
�B
B
mB
�B
B
�B
�B
MB
�B	�.B	�B	��B
�B
B
B
tB
�B
�B
�B
�B
SB
�B
�B
�B
GB
�B
B
�B
YB
%B
�B
B
�B
�B
�B
�B
�B
�B
	7B
�B
�B
�B
�B
zB
YB
tB
KB
	B
	B

#B
	�B

	B

	B

rB

XB

�B

�B
)B
�B
�B
�B
�B
(B
BB
VB
�B
6B
B
JB

�B
dB
�B
0B
DB

�B

=B
	�B

#B

XB
�B
�B
~B
�B
�B
�B
"B
�B
�B
�B
�B
�B
B
�B
&B
[B
�B
�B
�B
�B
�B
�B
�B
,B
gB
$B
?B
sB
�B
�B
$B
�B
9B
�B
�B
SB
B
mB
�B
�B
�B
$B
�B
YB
_B
+B
�B
B
EB
_B
yB
yB
�B
�B
B
�B
eB
�B
�B
B
kB
7B
QB
kB
B
	B
�B
�B
#B
�B
�B
�B
�B
qB
�B
�B
�B
QB
7B
7B
7B
�B
7B
�B
�B
QB
�B
�B
�B
)B
�B
�B
�B
]B
B
�B
qB
B
�B
 \B
!bB
"�B
"�B
"hB
!�B
!�B
"�B
"4B
# B
#�B
$ZB
&�B
&�B
&�B
&�B
'B
'8B
&�B
&�B
&�B
'8B
'B
'mB
'B
&�B
#�B
 �B
;B
"NB
&LB
(>B
)�B
*�B
+B
+�B
+�B
,=B
,�B
-�B
-�B
-�B
-�B
-�B
.B
.cB
.�B
.�B
.�B
.�B
/ B
/�B
/�B
/iB
/iB
/�B
/�B
0UB
0�B
0�B
0�B
1'B
0�B
1AB
1AB
1vB
1�B
1�B
1�B
1�B
1�B
2GB
2GB
2aB
2�B
2�B
2�B
3B
3B
3�B
3�B
3�B
4B
4B
4�B
4�B
4�B
4�B
5%B
5ZB
5ZB
5�B
6B
5tB
5�B
6`B
6�B
6�B
6�B
7B
7�B
7�B
8B
7�B
7�B
7fB
7�B
8B
8�B
8�B
8�B
8�B
8�B
8�B
9$B
:DB
:�B
;B
;dB
;JB
:�B
9�B
9$B
8�B
9>B
:^B
<6B
=<B
=�B
=�B
>(B
=�B
=VB
<�B
=B
=<B
<�B
=B
=�B
>]B
?�B
?�B
>�B
>�B
>�B
?�B
?HB
?B
>�B
A;B
A�B
@�B
?�B
@�B
@�B
BuB
C�B
C�B
D�B
E�B
F%B
F?B
F%B
F%B
F�B
G+B
G_B
G_B
GB
GB
G�B
G�B
G�B
G�B
H�B
HfB
H�B
H�B
H�B
HfB
H�B
IlB
J�B
J�B
KB
KDB
K)B
K^B
K�B
K�B
K�B
LB
LdB
L~B
L�B
LdB
L0B
LJB
LJB
LdB
LdB
L0B
L�B
M�B
M�B
M�B
N"B
N<B
NpB
N�B
N�B
OB
N�B
N�B
OB
OBB
O\B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
QB
Q B
QhB
Q�B
Q�B
RTB
RoB
R�B
R�B
R�B
R�B
S&B
S&B
S@B
S�B
S�B
TFB
T,B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
UgB
UMB
T�B
T�B
U2B
UMB
U�B
U�B
U�B
V�B
W$B
WYB
W�B
W�B
X+B
XB
XEB
XEB
X�B
XyB
X�B
X�B
YB
Y1B
Y1B
Y1B
Y�B
Y�B
ZB
Z7B
ZkB
Z�B
[	B
[=B
[WB
[�B
\B
\]B
\�B
\�B
]/B
]/B
]dB
]�B
^B
^�B
^�B
_�B
_�B
_�B
_�B
`'B
`�B
`vB
`�B
`�B
aHB
aHB
a�B
b4B
b�B
b�B
b�B
b�B
cTB
dB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d&B
dB
dtB
d�B
e`B
e�B
e�B
fB
fB
fB
e�B
ffB
gmB
gmB
g�B
gmB
g�B
hXB
h
B
h
B
h>B
h>B
gB
g�B
g�B
h
B
h�B
h�B
h�B
iB
iDB
i_B
iyB
i�B
i�B
i�B
i�B
i�B
j0B
j0B
jB
j�B
j�B
kB
k�B
lB
l=B
lWB
lWB
lWB
lqB
l�B
m)B
mCB
mCB
m)B
m�B
m�B
m�B
m�B
nB
n/B
nIB
n}B
n�B
o B
oB
o5B
o�B
o�B
pB
pUB
p�B
p�B
q'B
qAB
q'B
qAB
q�B
q�B
q�B
q�B
q�B
rGB
r-B
rGB
rGB
rGB
rGB
rGB
r�B
r�B
sB
s3B
s3B
s�B
s�B
s�B
t9B
tTB
tnB
t�B
t�B
t�B
u%B
u�B
u�B
u�B
u�B
vB
v+B
v`B
v�B
v�B
v�B
wLB
w�B
xB
x8B
x�B
x�B
y	B
y>B
y>B
y�B
y�B
y�B
y�B
zB
zB
zB
z^B
z�B
z�B
z�B
{B
z�B
{0B
{�B
{�B
{�B
{�B
|B
|�B
|�B
|�B
}B
}<B
}qB
}�B
}�B
}�B
~(B
~BB
~wB
~wB
~�B
~�B
.B
HB
.B
�B
}B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104903  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173138  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173138  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173138                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023145  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023145  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                