CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:30:11Z creation;2022-06-04T19:30:11Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604193011  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               mA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٷi�N�1   @ٷj ���@-��+�d��$�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B���B���B�33B�  B���B���B�  B�33B�  B���B���B�  B�  B�  B�33B�ffB�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�3D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@~{@�
=@�
=A�AA�A_�A�A�A�A�A�AЏ\A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B�#�B��>B��>B�#�B��B��>B��qB��B�#�B��B��qB��qB��B��B��B�#�B�W
B��B��B�#�B��B��B��B�qB��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�C�C�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCN�CO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF��DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�=D�B=D�
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
D��
D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AЎ�AЖ�AЕAЕ�AИ�AКAИ�AИ�AИ_AЛ=AНIAН�AОAП�AР'AПVAО�AР\AЛ�AЛ=AЍ�A�`vA�9�A��A��AϼAϡ�A�tA�.}A�PA��A��,A���A��A���A�
�A�2�Aǌ�Aƞ�Aø�A�z�A���A�T�A�K�A��PA���A���A�xA�A���A��A��yA� �A���A���A��A��
A���A��DA���A��A�c�A���A��5A�I�A�S�A�jA��A~�A|�4Ay�!ArS�Am(�Ah��Ae��Ac�+A`VmA]��AZ��AVg�ASP�AR�APIRAN�OAL`BAHJAFz�AE�4ACD�A?T�A>�A<($A9�AA8s�A7��A7+A6%�A6�A4֡A2m]A1�A1zA0R�A/U2A.�RA.�hA.��A.8�A-��A,�A+)�A(�A&XyA%xA#x�A"�A"�oA!��A!҉A!ԕA!�A!��A!��A H�A�KA��A��Al"A��Aa|A�A��A(�Av�A(�A�A�LA�AG�AA�A��AQA�5AݘA�.A��AuAn�A��AoiA�AA�A��A33A��AA�A�A�Au�A
��A	��A��A1A�A��A�AAO�A�A��Ao�A��AN<AA8�A��A�AE9A=A�A��A�CA��A��A��A�SA�~AS�A��A�A͟A��A��A`�AںAaA��Ar�AJ�AA �]A �bA TaA {@��P@�y>@�P�@��u@�1�@���@�H�@�zx@��M@�u�@�c @�?�@��@�@�iD@��@�ȴ@��@�F@�~�@�(@�*�@�tT@�@�\)@��@�7L@� i@�	@��@���@��@��@�0�@ꖼ@��6@��@�Ta@�w@�f@�M@��3@�	l@�@���@��P@��@�M@���@��B@�b�@�Ĝ@��z@�/�@��f@�ȴ@���@ޓu@�b@�|�@ܚ�@�ƨ@�e�@�U2@��@��@���@ٲ�@��@���@ذ�@�z@�c�@�?@׺^@�A�@֚�@��@ԝI@�8�@��X@�m�@�X@Ѐ�@��@ϑh@��@�kQ@��@�s�@̦L@�bN@���@��@ʇ+@��@ȸR@Ȁ�@Ǒh@ư�@��@ť@�m]@�O@Č@�!@õt@�[W@���@���@�o�@�!-@�ȴ@�~�@�_�@�*�@��9@���@���@�?}@���@��m@��@���@�ƨ@�?}@�'�@�&�@�'�@�,�@�@���@���@�1�@��@���@���@��@�2a@�%@���@�oi@�6@��-@�@@�_@�4n@�*�@��@��@��	@��@��+@���@���@���@���@��:@�{J@�@O@�@���@��@���@�N�@�ϫ@�u�@�J#@���@�q@��)@��t@��C@��@���@��g@�X@��"@���@�&�@�ԕ@��V@��8@��@���@�֡@�-�@��'@���@��1@�{�@�a|@�~@��N@�[W@��@�p;@�J@���@���@���@�f�@�&@��m@���@�,=@�G@�  @���@���@�:�@��`@��@��4@�l"@�M�@�.�@���@�|@�J�@��@���@��h@�C-@���@��@���@��$@��@�}V@�?�@�_@��3@�G�@��@��I@�E�@��7@�Y@�ی@��$@��+@�5?@�ԕ@�j�@�S�@�@@��)@��F@�`�@��@���@��#@���@���@��"@�dZ@��@���@���@�i�@�B[@��@�ƨ@��~@��@��s@���@�C-@�'R@��
@���@�7L@���@���@���@�kQ@�_@���@�k�@��P@��b@�]d@�<�@��@��j@��H@��{@�T�@�9�@���@�ff@�	�@��=@�+�@��<@�l"@�<�@�1@���@��q@�+@��|@���@���@��@�\�@�7�@�u@���@�K�@�J�@�4�@�ی@���@��.@�n�@�?@�4@��K@�w2@���@�u%@�bN@�@�@�($@�_@��@��P@�"�@��m@��@�Z@��]@��'@�j@�A @��@��y@���@�9X@�7@�{@��]@��@���@���@���@�^�@�7L@���@���@���@��I@��\@�h�@�C�@��@
=@~�r@}�@}o @}+�@|ی@|��@|A�@|G@{J#@z�@z)�@y�-@y�@y�h@yc@yf�@x�@x��@xN�@w��@w��@w"�@v�H@v��@v	@uc@uB�@t�v@toi@s�Q@s$t@ri�@q��@q\�@q!�@p�j@p9X@ot�@n��@n?@m�@m��@mw2@ma�@mVm@m+@lXy@k�{@k9�@k!-@kS@j�M@j͟@j�}@j�L@j��@jxl@j	@h��@g�;@f�,@f	@d�5@d1'@c�@@b��@b�\@b@�@a�@a�H@a�@`�v@`�@`'R@_��@_�@_�@_�@_
=@_@^�@^��@^R�@]��@^�@]��@]��@]?}@\�@\PH@\�@[�r@[�@[� @[e�@Zff@Z+k@Y�d@Y(�@X��@XK^@W�+@W�@W�0@W|�@WE9@V�,@VO@U�Z@U�.@U@Up�@Uq@T��@T��@T�@TQ�@S�A@SA�@R�}@Q�#@Q�7@Q:�@P�9@Poi@Pr�@PA�@OP�@N�!@N?@M�j@M5�@L|�@LPH@Kݘ@Kl�@K"�@Jȴ@J^5@J4@J@I��@IX@H�)@HH@G�*@G�@@G�{@F��@F@�@F�@F
�@E��@E�@E�j@E�@E�@D֡@D�u@DA�@C��@C�a@C�f@C8@B��@Bl�@B&�@A�D@A��@A�@A�=@AF@@�@@?�@@�@?�;@?�	@?l�@?�@>��@>a|@>0U@>!�@=�@=`B@<��@<��@<~(@;��@;��@;�@:u%@:J@9��@9ϫ@9�@9��@9�@8�j@8oi@8?�@7��@7�@6��@6��@6c @6?@6	@5�@5ԕ@5�^@5��@5V@4�@4G@3�@3Mj@3@O@36z@3Y@2�@2Ta@1ԕ@1u�@1&�@1;@0��@0��@0Ft@0�@/�@/�@/�6@/�$@/�$@/{J@//�@.��@.��@.�\@.^5@.J@-��@-J�@-q@-+@,�@,�z@,m�@,$@+�@+�k@+\)@+U�@+@*��@*kQ@*!�@)�d@)��@)�S@)�@)O�@(�@(A�@("h@(	�@'�+@'��@'�@@'��@'j�@'(@&�!@&�1@&n�@&.�@%�#@%��@%��@%��@%j@%#�@%	l@$��@$7�@#��@#qv@#C�@#�@"�}@"��@"h
@"J�@"�@!�)@!�z@!5�@ ��@ ��@ ��@ ��@ �O@ �E@ �@�g@�	@iD@9�@!-@C@��@��@xl@Q@$�@{@�@�@hs@q@�@��@�@�@M@�+@��@�[@�	@9�@
=@��@��@��@��@�F@�@��@�=@O�@Dg@2a@�@��@��@m�@N�@(�@�@��@"�@��@��@}V@Z�@GE@+k@��@�=@Vm@-w@V@�K@�@�9@>B@,=@�m@��@iD@O@4�@�@��@d�@E�@($@	@	@�@��@��@c@k�@Y�@+�@��@��@�)@�@��@g8@,=@��@�[@�f@�@8@o@ȴ@�x@u%@�@
�@��@�)@�j@�9@�z@��@��@�@w2@Dg@+@�9@I�@�@��@�*@�:@�f@�{@iD@6z@�@
�y@
��@
u%@
e@	��@	�@	�"@	��@	��@	}�@	rG@	f�@	N<@�|@�?@��@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AЎ�AЖ�AЕAЕ�AИ�AКAИ�AИ�AИ_AЛ=AНIAН�AОAП�AР'AПVAО�AР\AЛ�AЛ=AЍ�A�`vA�9�A��A��AϼAϡ�A�tA�.}A�PA��A��,A���A��A���A�
�A�2�Aǌ�Aƞ�Aø�A�z�A���A�T�A�K�A��PA���A���A�xA�A���A��A��yA� �A���A���A��A��
A���A��DA���A��A�c�A���A��5A�I�A�S�A�jA��A~�A|�4Ay�!ArS�Am(�Ah��Ae��Ac�+A`VmA]��AZ��AVg�ASP�AR�APIRAN�OAL`BAHJAFz�AE�4ACD�A?T�A>�A<($A9�AA8s�A7��A7+A6%�A6�A4֡A2m]A1�A1zA0R�A/U2A.�RA.�hA.��A.8�A-��A,�A+)�A(�A&XyA%xA#x�A"�A"�oA!��A!҉A!ԕA!�A!��A!��A H�A�KA��A��Al"A��Aa|A�A��A(�Av�A(�A�A�LA�AG�AA�A��AQA�5AݘA�.A��AuAn�A��AoiA�AA�A��A33A��AA�A�A�Au�A
��A	��A��A1A�A��A�AAO�A�A��Ao�A��AN<AA8�A��A�AE9A=A�A��A�CA��A��A��A�SA�~AS�A��A�A͟A��A��A`�AںAaA��Ar�AJ�AA �]A �bA TaA {@��P@�y>@�P�@��u@�1�@���@�H�@�zx@��M@�u�@�c @�?�@��@�@�iD@��@�ȴ@��@�F@�~�@�(@�*�@�tT@�@�\)@��@�7L@� i@�	@��@���@��@��@�0�@ꖼ@��6@��@�Ta@�w@�f@�M@��3@�	l@�@���@��P@��@�M@���@��B@�b�@�Ĝ@��z@�/�@��f@�ȴ@���@ޓu@�b@�|�@ܚ�@�ƨ@�e�@�U2@��@��@���@ٲ�@��@���@ذ�@�z@�c�@�?@׺^@�A�@֚�@��@ԝI@�8�@��X@�m�@�X@Ѐ�@��@ϑh@��@�kQ@��@�s�@̦L@�bN@���@��@ʇ+@��@ȸR@Ȁ�@Ǒh@ư�@��@ť@�m]@�O@Č@�!@õt@�[W@���@���@�o�@�!-@�ȴ@�~�@�_�@�*�@��9@���@���@�?}@���@��m@��@���@�ƨ@�?}@�'�@�&�@�'�@�,�@�@���@���@�1�@��@���@���@��@�2a@�%@���@�oi@�6@��-@�@@�_@�4n@�*�@��@��@��	@��@��+@���@���@���@���@��:@�{J@�@O@�@���@��@���@�N�@�ϫ@�u�@�J#@���@�q@��)@��t@��C@��@���@��g@�X@��"@���@�&�@�ԕ@��V@��8@��@���@�֡@�-�@��'@���@��1@�{�@�a|@�~@��N@�[W@��@�p;@�J@���@���@���@�f�@�&@��m@���@�,=@�G@�  @���@���@�:�@��`@��@��4@�l"@�M�@�.�@���@�|@�J�@��@���@��h@�C-@���@��@���@��$@��@�}V@�?�@�_@��3@�G�@��@��I@�E�@��7@�Y@�ی@��$@��+@�5?@�ԕ@�j�@�S�@�@@��)@��F@�`�@��@���@��#@���@���@��"@�dZ@��@���@���@�i�@�B[@��@�ƨ@��~@��@��s@���@�C-@�'R@��
@���@�7L@���@���@���@�kQ@�_@���@�k�@��P@��b@�]d@�<�@��@��j@��H@��{@�T�@�9�@���@�ff@�	�@��=@�+�@��<@�l"@�<�@�1@���@��q@�+@��|@���@���@��@�\�@�7�@�u@���@�K�@�J�@�4�@�ی@���@��.@�n�@�?@�4@��K@�w2@���@�u%@�bN@�@�@�($@�_@��@��P@�"�@��m@��@�Z@��]@��'@�j@�A @��@��y@���@�9X@�7@�{@��]@��@���@���@���@�^�@�7L@���@���@���@��I@��\@�h�@�C�@��@
=@~�r@}�@}o @}+�@|ی@|��@|A�@|G@{J#@z�@z)�@y�-@y�@y�h@yc@yf�@x�@x��@xN�@w��@w��@w"�@v�H@v��@v	@uc@uB�@t�v@toi@s�Q@s$t@ri�@q��@q\�@q!�@p�j@p9X@ot�@n��@n?@m�@m��@mw2@ma�@mVm@m+@lXy@k�{@k9�@k!-@kS@j�M@j͟@j�}@j�L@j��@jxl@j	@h��@g�;@f�,@f	@d�5@d1'@c�@@b��@b�\@b@�@a�@a�H@a�@`�v@`�@`'R@_��@_�@_�@_�@_
=@_@^�@^��@^R�@]��@^�@]��@]��@]?}@\�@\PH@\�@[�r@[�@[� @[e�@Zff@Z+k@Y�d@Y(�@X��@XK^@W�+@W�@W�0@W|�@WE9@V�,@VO@U�Z@U�.@U@Up�@Uq@T��@T��@T�@TQ�@S�A@SA�@R�}@Q�#@Q�7@Q:�@P�9@Poi@Pr�@PA�@OP�@N�!@N?@M�j@M5�@L|�@LPH@Kݘ@Kl�@K"�@Jȴ@J^5@J4@J@I��@IX@H�)@HH@G�*@G�@@G�{@F��@F@�@F�@F
�@E��@E�@E�j@E�@E�@D֡@D�u@DA�@C��@C�a@C�f@C8@B��@Bl�@B&�@A�D@A��@A�@A�=@AF@@�@@?�@@�@?�;@?�	@?l�@?�@>��@>a|@>0U@>!�@=�@=`B@<��@<��@<~(@;��@;��@;�@:u%@:J@9��@9ϫ@9�@9��@9�@8�j@8oi@8?�@7��@7�@6��@6��@6c @6?@6	@5�@5ԕ@5�^@5��@5V@4�@4G@3�@3Mj@3@O@36z@3Y@2�@2Ta@1ԕ@1u�@1&�@1;@0��@0��@0Ft@0�@/�@/�@/�6@/�$@/�$@/{J@//�@.��@.��@.�\@.^5@.J@-��@-J�@-q@-+@,�@,�z@,m�@,$@+�@+�k@+\)@+U�@+@*��@*kQ@*!�@)�d@)��@)�S@)�@)O�@(�@(A�@("h@(	�@'�+@'��@'�@@'��@'j�@'(@&�!@&�1@&n�@&.�@%�#@%��@%��@%��@%j@%#�@%	l@$��@$7�@#��@#qv@#C�@#�@"�}@"��@"h
@"J�@"�@!�)@!�z@!5�@ ��@ ��@ ��@ ��@ �O@ �E@ �@�g@�	@iD@9�@!-@C@��@��@xl@Q@$�@{@�@�@hs@q@�@��@�@�@M@�+@��@�[@�	@9�@
=@��@��@��@��@�F@�@��@�=@O�@Dg@2a@�@��@��@m�@N�@(�@�@��@"�@��@��@}V@Z�@GE@+k@��@�=@Vm@-w@V@�K@�@�9@>B@,=@�m@��@iD@O@4�@�@��@d�@E�@($@	@	@�@��@��@c@k�@Y�@+�@��@��@�)@�@��@g8@,=@��@�[@�f@�@8@o@ȴ@�x@u%@�@
�@��@�)@�j@�9@�z@��@��@�@w2@Dg@+@�9@I�@�@��@�*@�:@�f@�{@iD@6z@�@
�y@
��@
u%@
e@	��@	�@	�"@	��@	��@	}�@	rG@	f�@	N<@�|@�?@��@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�\B�BB�\B��B�\B�\B�\B��B��B�\B�\B�(B�(B��B��B��B��B��B��B�pB��B��B�?B�B~(By�Bu�Bp!Bg8Bd�BuZB��B��B��B�B	[B	�.B;�B?�B,B�BKBv�Bx8BcnBD�B*B!�BK^B�VB�rB~]Bx�By	B�Br�BS&B8RB^B
��B
ܬB
�NB
�B
��B
\xB
!�B
 B
oB	�DB	�aB	��B	�xB	��B	��B	�B	r-B	a|B	T�B	F%B	2�B	%�B	"�B	VB	]B	MB	VB		lB	�B	 4B�B��B�B�B��B�B�\B�B��B��B�B�B��B	�B	�B	xB	 �B	3�B	G�B	LJB	EB	;B	,�B	%�B	!�B	&�B	:xB	EB	PB	X+B	\�B	bhB	jeB	u�B	|�B	z�B	��B	�	B	�B	�B	�vB	��B	�+B	��B	��B	�LB	�FB	�>B	�kB	��B	�fB	��B	�lB	��B	��B	��B	�dB	�B	�B	��B	�zB	��B	��B	āB	�mB	��B	��B	��B	��B	�XB	�aB	�/B	�B	�kB	�QB	��B	��B	��B	�"B	��B	��B	�B	ϑB	�	B	��B	��B	�SB	� B	�B	��B	��B	��B	��B	�XB	�rB	�*B	�0B	��B
)B
�B
vB
B
[B

B
�B
:B
�B
TB
B
[B
B
[B
�B
 B
�B
B
�B
PB
"B
�B
�B
B
�B

=B
�B	��B
 �B
SB
B
�B
�B
DB
�B
�B
�B
�B
BB
B
�B
B
0B
B
"B
�B
�B
�B
jB
dB
xB
~B
 B
�B
�B
�B
�B

B
2B
�B
�B
 B
�B
bB
HB
�B
�B
JB
dB
jB
pB
�B
BB
BB
�B
B
�B
�B
�B
^B
�B
JB
0B
0B
B
B
xB
�B
�B
vB
�B
�B
�B
�B
0B

=B
	�B
B

=B
�B
^B
B

�B

rB
	�B
	�B
�B
1B
�B
	B
�B
�B
B
�B
%B
�B
�B
B
%B
�B
�B
�B
�B
�B
�B
�B
�B
KB
fB
�B
B
�B
+B
�B
KB
�B
�B
�B
B
B
�B
1B
B
1B
1B
KB
KB
�B
	B

	B

�B

�B

�B

rB

XB

#B

#B

=B
	�B

XB

�B
�B
�B
xB
�B
�B
�B
�B
�B
B
B
6B
PB
6B
PB
�B
�B
�B
�B
B
BB
BB
�B
\B
.B
}B
�B
�B
�B
�B
�B
�B
NB
B
�B
�B
4B
�B
�B
bB
�B
�B
4B
NB
hB
�B
�B
�B
�B
�B
:B
�B
�B
uB
�B
,B
B
B
�B
2B
�B
FB
�B
�B
FB
�B
B
�B
�B
gB
gB
gB
gB
�B
YB
YB

B

B
mB
�B
�B
�B
�B
eB
B
�B
�B
#B
#B
�B
�B
�B
	B
�B
�B
�B
�B
B
B
QB
�B
kB
�B
�B
�B
�B
=B
=B
=B
WB
=B
qB
qB
�B
)B
CB
�B
�B
�B
/B
B
~B
IB
�B
B
B
�B
�B
VB
 \B
 �B
 �B
!�B
"�B
"B
"�B
!�B
"NB
"NB
"4B
"B
"NB
"B
"hB
"�B
"�B
#B
#�B
$B
$�B
$�B
%�B
%�B
%�B
&fB
&2B
&�B
'RB
'�B
(�B
(�B
*KB
+QB
+�B
+�B
+�B
,=B
,qB
-B
-�B
.IB
.�B
/ B
/OB
0;B
0oB
0UB
0!B
0!B
0oB
0�B
0;B
0B
0!B
0�B
2B
49B
4B
4TB
4�B
5%B
5%B
5?B
5ZB
5ZB
5�B
6B
5�B
5�B
6B
5�B
5�B
5�B
6FB
6FB
6FB
6`B
7�B
7�B
7�B
7�B
7�B
8B
8�B
9	B
8�B
8�B
8B
7�B
7fB
7�B
7�B
8�B
8lB
8B
8B
8B
8RB
8lB
8lB
8�B
9�B
:B
:DB
:*B
:^B
:�B
:�B
:�B
;�B
<B
<B
<6B
<PB
<�B
=VB
=�B
>BB
>]B
>wB
>�B
?B
?cB
?�B
@B
@�B
@�B
AB
AB
AB
A;B
A�B
BAB
BB
BAB
B[B
B[B
BAB
BuB
BuB
BuB
BAB
B'B
A�B
AUB
A B
@�B
@B
?�B
?�B
@ B
@B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
BAB
BAB
BAB
BAB
BB
BB
A�B
BAB
CaB
D3B
D�B
EB
D�B
EB
E�B
FB
F%B
FB
E�B
F%B
F%B
F%B
FYB
FYB
FYB
F�B
F�B
F�B
GB
G_B
G�B
G�B
H�B
HKB
HfB
I7B
IRB
I�B
J	B
J	B
J=B
JXB
J�B
KB
K�B
L0B
L0B
LJB
L�B
L�B
L~B
L~B
MjB
MjB
M�B
M�B
NpB
N�B
N�B
O(B
O\B
OvB
O�B
O�B
PHB
PB
PbB
P�B
QNB
Q�B
RoB
RTB
RTB
S[B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TaB
TaB
T�B
T�B
UB
UgB
U�B
U�B
VB
VB
V9B
VSB
VSB
V�B
V�B
WYB
W�B
W�B
W�B
XB
XB
X_B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YB
YKB
Y�B
Y�B
Z�B
[#B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]B
]/B
^B
^B
^5B
^5B
^OB
^jB
^�B
_pB
`BB
_�B
_�B
_�B
`'B
`'B
`'B
`B
`B
`�B
`�B
`�B
a-B
aHB
abB
a|B
a�B
b4B
bhB
b�B
bhB
b�B
b�B
c B
cTB
c�B
c�B
dB
d&B
dZB
dZB
d�B
d�B
e,B
ezB
e�B
e�B
f2B
e�B
fB
ffB
f�B
g8B
gB
gB
f�B
g8B
gmB
g�B
g�B
hsB
i*B
i�B
jKB
jB
j�B
j�B
j�B
l=B
lqB
l"B
lWB
k�B
lB
l=B
lB
k�B
k�B
l=B
l�B
m]B
m]B
m�B
nB
nB
m�B
mwB
m�B
n/B
nIB
n/B
n�B
n�B
oOB
p!B
poB
p�B
p�B
poB
pUB
p;B
pUB
p�B
qB
p;B
pB
pB
p!B
p!B
p!B
p;B
pUB
p�B
p�B
qB
p�B
q[B
q�B
q�B
rB
r|B
r�B
r�B
r�B
r�B
r�B
s3B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tTB
tnB
t�B
t�B
t�B
t�B
uB
u%B
u%B
uZB
utB
utB
u�B
u�B
v�B
v�B
wB
wB
wLB
wLB
wLB
w�B
w�B
xB
x8B
xlB
x�B
y	B
y>B
yXB
y�B
y�B
y�B
y�B
y�B
zxB
z�B
z�B
z�B
z�B
{B
{JB
{JB
{JB
{dB
{�B
{�B
{�B
{�B
|B
|PB
|�B
}B
}qB
}�B
}�B
}�B
}�B
~(B
~(B
~�B
HB
}B
�B
}B
}B
� B
�B
� B
� B
�4B
�4B
�4B
�iB
�iB
��B
��B
��B
��B
�oB
�oB
�oB
��B
��B
�B
�B
�'B
�AB
�AB
�B
�'B
��B
��B
�AB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�\B�BB�\B��B�\B�\B�\B��B��B�\B�\B�(B�(B��B��B��B��B��B��B�pB��B��B�?B�B~(By�Bu�Bp!Bg8Bd�BuZB��B��B��B�B	[B	�.B;�B?�B,B�BKBv�Bx8BcnBD�B*B!�BK^B�VB�rB~]Bx�By	B�Br�BS&B8RB^B
��B
ܬB
�NB
�B
��B
\xB
!�B
 B
oB	�DB	�aB	��B	�xB	��B	��B	�B	r-B	a|B	T�B	F%B	2�B	%�B	"�B	VB	]B	MB	VB		lB	�B	 4B�B��B�B�B��B�B�\B�B��B��B�B�B��B	�B	�B	xB	 �B	3�B	G�B	LJB	EB	;B	,�B	%�B	!�B	&�B	:xB	EB	PB	X+B	\�B	bhB	jeB	u�B	|�B	z�B	��B	�	B	�B	�B	�vB	��B	�+B	��B	��B	�LB	�FB	�>B	�kB	��B	�fB	��B	�lB	��B	��B	��B	�dB	�B	�B	��B	�zB	��B	��B	āB	�mB	��B	��B	��B	��B	�XB	�aB	�/B	�B	�kB	�QB	��B	��B	��B	�"B	��B	��B	�B	ϑB	�	B	��B	��B	�SB	� B	�B	��B	��B	��B	��B	�XB	�rB	�*B	�0B	��B
)B
�B
vB
B
[B

B
�B
:B
�B
TB
B
[B
B
[B
�B
 B
�B
B
�B
PB
"B
�B
�B
B
�B

=B
�B	��B
 �B
SB
B
�B
�B
DB
�B
�B
�B
�B
BB
B
�B
B
0B
B
"B
�B
�B
�B
jB
dB
xB
~B
 B
�B
�B
�B
�B

B
2B
�B
�B
 B
�B
bB
HB
�B
�B
JB
dB
jB
pB
�B
BB
BB
�B
B
�B
�B
�B
^B
�B
JB
0B
0B
B
B
xB
�B
�B
vB
�B
�B
�B
�B
0B

=B
	�B
B

=B
�B
^B
B

�B

rB
	�B
	�B
�B
1B
�B
	B
�B
�B
B
�B
%B
�B
�B
B
%B
�B
�B
�B
�B
�B
�B
�B
�B
KB
fB
�B
B
�B
+B
�B
KB
�B
�B
�B
B
B
�B
1B
B
1B
1B
KB
KB
�B
	B

	B

�B

�B

�B

rB

XB

#B

#B

=B
	�B

XB

�B
�B
�B
xB
�B
�B
�B
�B
�B
B
B
6B
PB
6B
PB
�B
�B
�B
�B
B
BB
BB
�B
\B
.B
}B
�B
�B
�B
�B
�B
�B
NB
B
�B
�B
4B
�B
�B
bB
�B
�B
4B
NB
hB
�B
�B
�B
�B
�B
:B
�B
�B
uB
�B
,B
B
B
�B
2B
�B
FB
�B
�B
FB
�B
B
�B
�B
gB
gB
gB
gB
�B
YB
YB

B

B
mB
�B
�B
�B
�B
eB
B
�B
�B
#B
#B
�B
�B
�B
	B
�B
�B
�B
�B
B
B
QB
�B
kB
�B
�B
�B
�B
=B
=B
=B
WB
=B
qB
qB
�B
)B
CB
�B
�B
�B
/B
B
~B
IB
�B
B
B
�B
�B
VB
 \B
 �B
 �B
!�B
"�B
"B
"�B
!�B
"NB
"NB
"4B
"B
"NB
"B
"hB
"�B
"�B
#B
#�B
$B
$�B
$�B
%�B
%�B
%�B
&fB
&2B
&�B
'RB
'�B
(�B
(�B
*KB
+QB
+�B
+�B
+�B
,=B
,qB
-B
-�B
.IB
.�B
/ B
/OB
0;B
0oB
0UB
0!B
0!B
0oB
0�B
0;B
0B
0!B
0�B
2B
49B
4B
4TB
4�B
5%B
5%B
5?B
5ZB
5ZB
5�B
6B
5�B
5�B
6B
5�B
5�B
5�B
6FB
6FB
6FB
6`B
7�B
7�B
7�B
7�B
7�B
8B
8�B
9	B
8�B
8�B
8B
7�B
7fB
7�B
7�B
8�B
8lB
8B
8B
8B
8RB
8lB
8lB
8�B
9�B
:B
:DB
:*B
:^B
:�B
:�B
:�B
;�B
<B
<B
<6B
<PB
<�B
=VB
=�B
>BB
>]B
>wB
>�B
?B
?cB
?�B
@B
@�B
@�B
AB
AB
AB
A;B
A�B
BAB
BB
BAB
B[B
B[B
BAB
BuB
BuB
BuB
BAB
B'B
A�B
AUB
A B
@�B
@B
?�B
?�B
@ B
@B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
BAB
BAB
BAB
BAB
BB
BB
A�B
BAB
CaB
D3B
D�B
EB
D�B
EB
E�B
FB
F%B
FB
E�B
F%B
F%B
F%B
FYB
FYB
FYB
F�B
F�B
F�B
GB
G_B
G�B
G�B
H�B
HKB
HfB
I7B
IRB
I�B
J	B
J	B
J=B
JXB
J�B
KB
K�B
L0B
L0B
LJB
L�B
L�B
L~B
L~B
MjB
MjB
M�B
M�B
NpB
N�B
N�B
O(B
O\B
OvB
O�B
O�B
PHB
PB
PbB
P�B
QNB
Q�B
RoB
RTB
RTB
S[B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TaB
TaB
T�B
T�B
UB
UgB
U�B
U�B
VB
VB
V9B
VSB
VSB
V�B
V�B
WYB
W�B
W�B
W�B
XB
XB
X_B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YB
YKB
Y�B
Y�B
Z�B
[#B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]B
]/B
^B
^B
^5B
^5B
^OB
^jB
^�B
_pB
`BB
_�B
_�B
_�B
`'B
`'B
`'B
`B
`B
`�B
`�B
`�B
a-B
aHB
abB
a|B
a�B
b4B
bhB
b�B
bhB
b�B
b�B
c B
cTB
c�B
c�B
dB
d&B
dZB
dZB
d�B
d�B
e,B
ezB
e�B
e�B
f2B
e�B
fB
ffB
f�B
g8B
gB
gB
f�B
g8B
gmB
g�B
g�B
hsB
i*B
i�B
jKB
jB
j�B
j�B
j�B
l=B
lqB
l"B
lWB
k�B
lB
l=B
lB
k�B
k�B
l=B
l�B
m]B
m]B
m�B
nB
nB
m�B
mwB
m�B
n/B
nIB
n/B
n�B
n�B
oOB
p!B
poB
p�B
p�B
poB
pUB
p;B
pUB
p�B
qB
p;B
pB
pB
p!B
p!B
p!B
p;B
pUB
p�B
p�B
qB
p�B
q[B
q�B
q�B
rB
r|B
r�B
r�B
r�B
r�B
r�B
s3B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tTB
tnB
t�B
t�B
t�B
t�B
uB
u%B
u%B
uZB
utB
utB
u�B
u�B
v�B
v�B
wB
wB
wLB
wLB
wLB
w�B
w�B
xB
x8B
xlB
x�B
y	B
y>B
yXB
y�B
y�B
y�B
y�B
y�B
zxB
z�B
z�B
z�B
z�B
{B
{JB
{JB
{JB
{dB
{�B
{�B
{�B
{�B
|B
|PB
|�B
}B
}qB
}�B
}�B
}�B
}�B
~(B
~(B
~�B
HB
}B
�B
}B
}B
� B
�B
� B
� B
�4B
�4B
�4B
�iB
�iB
��B
��B
��B
��B
�oB
�oB
�oB
��B
��B
�B
�B
�'B
�AB
�AB
�B
�'B
��B
��B
�AB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105251  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193011  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193011  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193011                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043019  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043019  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                