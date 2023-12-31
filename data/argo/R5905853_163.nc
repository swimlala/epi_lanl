CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-07-28T21:52:22Z creation;2023-07-28T21:52:23Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230728215222  20230728215427  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�=�io{1   @�=�� �.@1�fffff�c�(�\1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffBB�  B�  B�33C 33C��C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"L�C#�fC%�fC'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CK�fCN  CP33CQ�fCS�fCU�fCX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp33Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@~{@�
=@�
=A�A?�A_�A�A�A�A�A��\A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBpG�Bw�HB�HB��B��qB��B��B��B��B��B��B��B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�W
B�>B��B��B�#�C +�C�C޸C�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC �C"EC#޸C%޸C'޸C)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCH�CI�RCK޸CM�RCP+�CQ޸CS޸CU޸CW�RCY�RC[�RC]޸C_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCp+�Cq�RCs�RCu�RCw޸Cy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ�zDJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
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
D�=D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
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
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
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
D��D��
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
D�b=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%zA�)�A�-�A�.A�/OA�.A�.�A�0UA�1[A�1[A�3hA�1�A�4nA�4nA�5tA�5�A�6�A�8�A�9�A�9$A�7LA�6�A�6FA�:^A�9�A�A�A�?A�=A�ӏA͝IA���A��A��BA̳hA�v�A��A˝�A�&�AʍPA�R�A�DgA��^A��@A��3A��VA�U2A�,�A���A�[#A�_A��aA�+6A���A��A�i�A��+A�{A��A���A��A�C�A��6A�s�A�U�A��)A���A�kA�A�(�A�+�A��RA�6FA��A�t�A��vA�[�A��<A���A��sA�[�A���A��5A�s�A��+A���A�+A�4�A�/A�GA�{A�IA���A�C�A�eA�?}A��A~<6AxIRAr�[Aq!�Ao�An?Ah��Acu%A^MASs�AP�\AL��AG�AFW?AD�BAAXyA=4A:�A6A3�A1�A1!-A/M�A-tTA,��A,�uA+r�A*y�A)�A'�@A'(A&Z�A%�>A%�'A%��A%w�A#��A!m�A A�kA�9A��A�6A�MA�~A	A=�A��A*�A;A��A%A�$A2aA�yA�qAZ�A<6A��A�A
�A"hA	�A
��A	2�AGEAx�A�FAaA�A�yA��A($AX�A�
A��A��A�_A n�A ?�A �@��@��/@��@��@���@�҉@�/@�A $tA �7A ��A ��A �+@�J�@��@��@�(�@���@��[@�>B@�خ@�|@�K�@�C@��9@�g8@��@���@��@�c @��D@���@��@��@�v`@���@�B�@��@��@�Dg@�E�@�2a@�B�@���@�!@���@��f@��g@��@�ȴ@��`@��"@��2@�>B@���@�Ft@�1@���@��@癚@�G�@�+@��@�Q�@�1�@�u@���@��@�x�@俱@��@�U�@�@���@⟾@�_@�g8@�GE@�4@�k�@���@�U�@ބ�@��@�+@�~(@�Dg@�ƨ@�͟@�d�@��@�E9@�O�@�O�@��@�a|@ռ�@կ�@�\�@��2@���@ӖS@�u�@�F�@��y@��)@�-@�5�@о@�bN@�?�@�0U@��@��}@�n/@�;d@�v`@ϊ�@ϩ�@�1�@�E9@��@�B�@�=@�@�+�@�p;@��@��`@�%@�8�@��K@�S&@�
=@�	@���@�
�@��g@ǭC@ǈf@�}�@�y�@�]�@� \@���@ƕ�@�GE@�"h@��@�]�@�!�@��c@ę1@�  @Ã{@�Z�@�8@���@��	@�&@� i@���@���@�ff@�x@��9@��q@���@��6@�0U@�_@���@�*0@���@�_@�^�@���@���@��m@��@�֡@�d�@�Q�@�GE@��j@��@���@�J�@��@���@���@���@���@�x�@���@��u@�=q@�P�@��h@�y>@�S�@�C�@�8�@��.@��	@�,�@�&@��5@��'@�p;@��@���@�c�@��@��z@�^5@��@�x@�?}@�=@�4�@���@�@�@��@��a@���@��n@�}�@��@�h
@�9X@�,=@�	@��4@�E9@�Y@��X@�� @�S�@��@��}@���@���@���@�  @��w@�Q�@�:�@�.I@��@��p@��@��@��q@��@�f�@�Q�@�8�@�;@���@�ѷ@���@�E�@�-@���@���@�s�@�)_@��c@���@�[�@�{@���@���@�z@�oi@�h�@�6@� �@��T@��q@�{J@�N<@���@�]d@��@�G@���@���@��:@�e,@��@��@��h@�� @��Y@�~�@�p;@�U2@�6@���@�iD@�%F@��@��s@�Ɇ@���@��@��a@�S&@��@� i@��2@��[@���@��@���@�f�@�+@��H@���@�[�@�
�@���@�|�@�1�@�}V@�GE@�e@��o@���@���@��@�c�@� \@�ߤ@���@���@���@��Y@�~(@�r�@�j@�V�@�4n@���@�;d@��	@��@��@�~�@�^5@�&�@�{J@�Dg@�2a@�@�Ɇ@�m�@�J@��=@�=�@��@���@��@�E�@��@��d@��@�Z�@��@� i@��@���@�E�@�GE@�خ@�Vm@���@���@�Q�@��@�U�@��|@���@�H�@��@��o@�خ@�e�@�9�@�C@���@���@�ȴ@��@��@�c�@�-�@��@~)�@|��@|Ĝ@|�e@|q@|b@{�w@{��@{��@{g�@{>�@{
=@z�@z��@z�+@zd�@z#:@z@y�H@y^�@xr�@w�+@w�@w�$@wS�@v��@u�@t�@t��@tm�@t[�@t2�@t/�@t-�@t/�@t*�@t(�@t!@s�@sƨ@s��@s�	@s�@sb�@s@O@s)_@s"�@s�@r��@r�c@r��@r��@r��@r��@r1�@q�)@q��@q%@p��@p[�@o�6@o�q@o��@oqv@oK�@n�}@m?}@lXy@l*�@l7@k�W@k�@@kK�@k)_@j�}@i��@i��@i�M@io @ia�@i�@h�j@hV�@g�@g�@f($@ee,@d�4@c�;@c��@b�@b
�@a|@aB�@a�@`�p@`��@`�o@_�A@_6z@_�@^�@^�@^� @]�D@]j@\��@\��@\��@[��@[�@Z�@Z�@Z��@ZkQ@Y��@X@W��@V��@U�.@U�'@U�~@Ua�@U+�@U	l@T�@T�E@T��@T�@Toi@T1@R��@Ra|@Re@Q��@Qe,@P�@PĜ@P��@P7�@O�6@O�@O��@O@O@N�@N{�@NE�@NC�@N)�@M��@M�@LN�@K�}@K��@K$t@J�'@J1�@J!�@Je@J_@I�@I��@I=�@H�f@H�@H�@G�F@G_p@F�h@FC�@Fu@Ef�@D�P@D�@D�e@D<�@C�6@C��@C��@Cj�@CF�@CY@B��@Bc @BM�@B5?@A�9@A��@AL�@A&�@@�@@�Y@?��@?O@?�@>�!@>��@>_�@>)�@>_@=�D@=�o@=�z@<:�@;�@;ݘ@;�Q@;�}@;��@;�0@;�k@;��@;�4@;\)@;>�@;"�@:�c@:�,@:��@:~�@:p;@:i�@:^5@:-@9�#@9}�@9�@8�@8u�@7�m@7˒@7��@6�H@6�A@6�@5��@5Vm@4�@4��@44n@4	�@3�
@3a@2�R@2��@2_�@2L0@2($@1��@1`B@0�@0�@0�@0��@0w�@09X@/�&@/��@/�V@/�f@/Z�@.�@.6�@-��@-x�@-X@-:�@,�f@,��@,H@,(�@,$@,7@,�@,b@+�@+�@+�a@+qv@+�@*�@*�B@*�L@*��@*z@*n�@*Ta@)��@)m]@)�@(4n@'�K@'t�@'o@&�!@&��@&a|@&H�@&($@&�@%x�@%<6@%%@$��@$[�@$N�@#��@#l�@#E9@"�H@"^5@"3�@"�@!�o@!��@!��@!��@!p�@!Dg@ �f@ c�@ �@��@��@�*@��@�f@U�@,�@@�b@��@Q@&�@��@`B@@�@�@~�@X�@1�@&@�@��@�2@��@GE@�@�9@��@��@|@�@	l@�P@�|@��@y>@:�@�@ƨ@qv@a@�@@�@ߤ@�F@�D@@�n@��@��@��@|@j@�P@e�@7�@�@�w@e�@'�@��@�2@҉@��@B[@��@��@�z@�@��@V@�j@�@2�@��@��@�0@�@iD@J#@@��@h
@	@�>@�#@��@��@�@��@��@��@��@IR@�@�@�$@�@��@�Y@l"@N�@'R@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%zA�)�A�-�A�.A�/OA�.A�.�A�0UA�1[A�1[A�3hA�1�A�4nA�4nA�5tA�5�A�6�A�8�A�9�A�9$A�7LA�6�A�6FA�:^A�9�A�A�A�?A�=A�ӏA͝IA���A��A��BA̳hA�v�A��A˝�A�&�AʍPA�R�A�DgA��^A��@A��3A��VA�U2A�,�A���A�[#A�_A��aA�+6A���A��A�i�A��+A�{A��A���A��A�C�A��6A�s�A�U�A��)A���A�kA�A�(�A�+�A��RA�6FA��A�t�A��vA�[�A��<A���A��sA�[�A���A��5A�s�A��+A���A�+A�4�A�/A�GA�{A�IA���A�C�A�eA�?}A��A~<6AxIRAr�[Aq!�Ao�An?Ah��Acu%A^MASs�AP�\AL��AG�AFW?AD�BAAXyA=4A:�A6A3�A1�A1!-A/M�A-tTA,��A,�uA+r�A*y�A)�A'�@A'(A&Z�A%�>A%�'A%��A%w�A#��A!m�A A�kA�9A��A�6A�MA�~A	A=�A��A*�A;A��A%A�$A2aA�yA�qAZ�A<6A��A�A
�A"hA	�A
��A	2�AGEAx�A�FAaA�A�yA��A($AX�A�
A��A��A�_A n�A ?�A �@��@��/@��@��@���@�҉@�/@�A $tA �7A ��A ��A �+@�J�@��@��@�(�@���@��[@�>B@�خ@�|@�K�@�C@��9@�g8@��@���@��@�c @��D@���@��@��@�v`@���@�B�@��@��@�Dg@�E�@�2a@�B�@���@�!@���@��f@��g@��@�ȴ@��`@��"@��2@�>B@���@�Ft@�1@���@��@癚@�G�@�+@��@�Q�@�1�@�u@���@��@�x�@俱@��@�U�@�@���@⟾@�_@�g8@�GE@�4@�k�@���@�U�@ބ�@��@�+@�~(@�Dg@�ƨ@�͟@�d�@��@�E9@�O�@�O�@��@�a|@ռ�@կ�@�\�@��2@���@ӖS@�u�@�F�@��y@��)@�-@�5�@о@�bN@�?�@�0U@��@��}@�n/@�;d@�v`@ϊ�@ϩ�@�1�@�E9@��@�B�@�=@�@�+�@�p;@��@��`@�%@�8�@��K@�S&@�
=@�	@���@�
�@��g@ǭC@ǈf@�}�@�y�@�]�@� \@���@ƕ�@�GE@�"h@��@�]�@�!�@��c@ę1@�  @Ã{@�Z�@�8@���@��	@�&@� i@���@���@�ff@�x@��9@��q@���@��6@�0U@�_@���@�*0@���@�_@�^�@���@���@��m@��@�֡@�d�@�Q�@�GE@��j@��@���@�J�@��@���@���@���@���@�x�@���@��u@�=q@�P�@��h@�y>@�S�@�C�@�8�@��.@��	@�,�@�&@��5@��'@�p;@��@���@�c�@��@��z@�^5@��@�x@�?}@�=@�4�@���@�@�@��@��a@���@��n@�}�@��@�h
@�9X@�,=@�	@��4@�E9@�Y@��X@�� @�S�@��@��}@���@���@���@�  @��w@�Q�@�:�@�.I@��@��p@��@��@��q@��@�f�@�Q�@�8�@�;@���@�ѷ@���@�E�@�-@���@���@�s�@�)_@��c@���@�[�@�{@���@���@�z@�oi@�h�@�6@� �@��T@��q@�{J@�N<@���@�]d@��@�G@���@���@��:@�e,@��@��@��h@�� @��Y@�~�@�p;@�U2@�6@���@�iD@�%F@��@��s@�Ɇ@���@��@��a@�S&@��@� i@��2@��[@���@��@���@�f�@�+@��H@���@�[�@�
�@���@�|�@�1�@�}V@�GE@�e@��o@���@���@��@�c�@� \@�ߤ@���@���@���@��Y@�~(@�r�@�j@�V�@�4n@���@�;d@��	@��@��@�~�@�^5@�&�@�{J@�Dg@�2a@�@�Ɇ@�m�@�J@��=@�=�@��@���@��@�E�@��@��d@��@�Z�@��@� i@��@���@�E�@�GE@�خ@�Vm@���@���@�Q�@��@�U�@��|@���@�H�@��@��o@�خ@�e�@�9�@�C@���@���@�ȴ@��@��@�c�@�-�@��@~)�@|��@|Ĝ@|�e@|q@|b@{�w@{��@{��@{g�@{>�@{
=@z�@z��@z�+@zd�@z#:@z@y�H@y^�@xr�@w�+@w�@w�$@wS�@v��@u�@t�@t��@tm�@t[�@t2�@t/�@t-�@t/�@t*�@t(�@t!@s�@sƨ@s��@s�	@s�@sb�@s@O@s)_@s"�@s�@r��@r�c@r��@r��@r��@r��@r1�@q�)@q��@q%@p��@p[�@o�6@o�q@o��@oqv@oK�@n�}@m?}@lXy@l*�@l7@k�W@k�@@kK�@k)_@j�}@i��@i��@i�M@io @ia�@i�@h�j@hV�@g�@g�@f($@ee,@d�4@c�;@c��@b�@b
�@a|@aB�@a�@`�p@`��@`�o@_�A@_6z@_�@^�@^�@^� @]�D@]j@\��@\��@\��@[��@[�@Z�@Z�@Z��@ZkQ@Y��@X@W��@V��@U�.@U�'@U�~@Ua�@U+�@U	l@T�@T�E@T��@T�@Toi@T1@R��@Ra|@Re@Q��@Qe,@P�@PĜ@P��@P7�@O�6@O�@O��@O@O@N�@N{�@NE�@NC�@N)�@M��@M�@LN�@K�}@K��@K$t@J�'@J1�@J!�@Je@J_@I�@I��@I=�@H�f@H�@H�@G�F@G_p@F�h@FC�@Fu@Ef�@D�P@D�@D�e@D<�@C�6@C��@C��@Cj�@CF�@CY@B��@Bc @BM�@B5?@A�9@A��@AL�@A&�@@�@@�Y@?��@?O@?�@>�!@>��@>_�@>)�@>_@=�D@=�o@=�z@<:�@;�@;ݘ@;�Q@;�}@;��@;�0@;�k@;��@;�4@;\)@;>�@;"�@:�c@:�,@:��@:~�@:p;@:i�@:^5@:-@9�#@9}�@9�@8�@8u�@7�m@7˒@7��@6�H@6�A@6�@5��@5Vm@4�@4��@44n@4	�@3�
@3a@2�R@2��@2_�@2L0@2($@1��@1`B@0�@0�@0�@0��@0w�@09X@/�&@/��@/�V@/�f@/Z�@.�@.6�@-��@-x�@-X@-:�@,�f@,��@,H@,(�@,$@,7@,�@,b@+�@+�@+�a@+qv@+�@*�@*�B@*�L@*��@*z@*n�@*Ta@)��@)m]@)�@(4n@'�K@'t�@'o@&�!@&��@&a|@&H�@&($@&�@%x�@%<6@%%@$��@$[�@$N�@#��@#l�@#E9@"�H@"^5@"3�@"�@!�o@!��@!��@!��@!p�@!Dg@ �f@ c�@ �@��@��@�*@��@�f@U�@,�@@�b@��@Q@&�@��@`B@@�@�@~�@X�@1�@&@�@��@�2@��@GE@�@�9@��@��@|@�@	l@�P@�|@��@y>@:�@�@ƨ@qv@a@�@@�@ߤ@�F@�D@@�n@��@��@��@|@j@�P@e�@7�@�@�w@e�@'�@��@�2@҉@��@B[@��@��@�z@�@��@V@�j@�@2�@��@��@�0@�@iD@J#@@��@h
@	@�>@�#@��@��@�@��@��@��@��@IR@�@�@�$@�@��@�Y@l"@N�@'R@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
/iB
/5B
/5B
/5B
/iB
/5B
/B
/B
/B
/B
/OB
/B
/5B
/5B
/B
/B
/B
/B
/B
/B
/OB
/OB
/iB
/OB
/5B
/B
/B
.IB
=�B
e�B
y$B
�B
��B
�MB
�)B
��B
��B
�B
��B
�[B
z�B
|�B
tTB
e,B
�RB
��B
�+B
��B
��B
�B
�$B+BuBB+�B=�BB�BLdBjBo5BqBq�B{dBz*Bu?Bq'BT{B5�B:�B#�B<BKB�B	�B�B�B�BzB�B3B�B{B
��B
��B
��B
�yB
ȀB
��B
��B
�	B
cnB
/5B
BB	ޞB	�AB	��B	.B	iDB	S&B	JXB	E�B	>]B	/ B	%�B		�BޞB�B�BżB��B��B	B�RB�BB�OB�*B�UB��B�4B�B��B�eB�B��B�wB�B�0B��B�(B�"B�jB�xB�2B��B��B	 �B��B��B�$B��B��B��B�fB�B�B��B	
XB��B��B��B	 OB	B�B�B��B�B�QB��B	 �B	�B	 �B�qB��B�B�JB�rB�$B�8B�^B��B��B�XB�>B��B�3B�B�LB��B��B�zB��B	�B	�B	&B	 vB	A�B	Q B	^5B	c�B	f�B	ezB	dB	nIB	s�B	t�B	x�B	~�B	�aB	�9B	��B	�YB	�zB	��B	�_B	��B	��B	��B	�zB	�+B	�	B	��B	�B	�gB	��B	��B	�7B	�B	��B	�pB	�|B	�RB	��B	��B	��B	��B	��B	��B	�cB	��B	�oB	��B	�B	�5B	�B	�B	�fB	��B	�?B	��B	��B	�RB	�$B	�^B	�PB	�B	��B	�;B	��B	��B	��B	ǔB	�lB	��B	�PB	�"B	�bB	ңB	ңB	�B	�pB	͟B	̘B	��B	�6B	��B	��B	�~B	ΥB	ϫB	��B	� B	��B	��B	ѷB	��B	�oB	��B	�B	�SB	�9B	�B	�mB	�mB	רB	��B	��B	یB	��B	�B	��B	ܒB	ߤB	� B	�LB	�B	�B	�/B	�B	��B	�B	�/B	��B	�'B	��B	�B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�6B	�6B	�B	�B	��B	�qB	��B	�B	�B	�wB	�B	��B	��B	�B	�}B	��B	��B	�B	��B	��B	�>B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�nB	�TB	�B	�nB	�LB	�XB	��B	�(B
 iB
B
�B
[B
B
�B
�B
�B
�B
B
B
�B
[B
�B
�B
oB
UB
�B
�B
-B
{B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
mB
B
�B
�B
B
B
B
9B
B
B
�B
�B
B
KB
KB
fB
fB
�B
	B
�B
	7B
	lB

�B
�B
B
�B
�B
PB
PB
B
PB
B
�B
JB
�B
�B
^B
)B

�B

�B
0B
�B
�B
0B
�B
�B
�B
0B
B
�B
�B
�B
~B
B
�B
�B
VB
�B
BB
B
�B
�B
:B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
,B
aB
�B
�B
�B
�B
�B
�B
9B
�B
�B
�B
�B
�B

B
?B
+B
�B
�B
KB
1B
1B
B
7B
QB
qB
	B
	B
	B
�B
WB
�B
�B
�B
~B
�B
B
�B
OB
jB
jB
5B
 B
�B
 BB
 �B
 �B
 �B
!B
!�B
!�B
"B
"4B
!�B
!�B
"B
!�B
!�B
!�B
!�B
!�B
"NB
"�B
#B
"�B
#�B
$@B
$ZB
$�B
%zB
%`B
%,B
%zB
%�B
&2B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
*eB
*B
*KB
*�B
,B
,�B
,�B
-wB
-�B
.B
.}B
./B
.B
-�B
-�B
-�B
/iB
/iB
0�B
1�B
3�B
3�B
4TB
4�B
5%B
5�B
5�B
6B
6zB
6�B
7fB
8B
8�B
9>B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
<B
<�B
<�B
="B
=B
="B
="B
="B
="B
="B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=qB
=qB
=qB
=VB
=<B
=B
=B
=B
<�B
<�B
<�B
=qB
=�B
>B
>(B
>]B
>]B
>wB
>BB
>]B
?.B
?.B
?.B
?HB
?HB
?}B
?�B
?�B
@4B
@�B
@�B
@�B
@�B
@�B
@�B
AB
AUB
AoB
A�B
B'B
B[B
B�B
B�B
BuB
CB
C-B
C{B
C-B
C-B
B�B
B�B
B�B
B�B
B'B
BB
A�B
A�B
A�B
A�B
A�B
AUB
A;B
@�B
AUB
A�B
A�B
AoB
AUB
AUB
A�B
C-B
C-B
DMB
D�B
EB
EB
E9B
EmB
E�B
E�B
E�B
E�B
E�B
E�B
FB
GzB
G�B
I�B
KDB
LB
M�B
N�B
OBB
O�B
PbB
P�B
P�B
QB
Q�B
R B
RTB
RTB
RTB
RoB
R�B
S�B
S�B
SuB
SuB
S�B
S�B
S�B
SuB
S�B
S�B
SuB
S�B
S�B
S�B
TB
TB
T,B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
VB
VmB
V�B
V�B
W$B
WYB
WYB
W�B
WYB
WsB
W?B
W�B
W�B
W�B
W�B
X_B
YB
ZQB
Z�B
[#B
[�B
[�B
[�B
\B
\)B
\)B
\B
\B
^B
^5B
^5B
^B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^5B
^�B
^�B
^�B
_VB
_�B
_�B
`B
`�B
`�B
aHB
a�B
b�B
b�B
c B
cnB
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
e,B
e�B
fB
fLB
ffB
fLB
fLB
f�B
f�B
f�B
gB
f�B
gB
gRB
hXB
hsB
h�B
h�B
h�B
iB
i�B
jB
i�B
i�B
i�B
jB
jB
j0B
j0B
jKB
j�B
j�B
j�B
kB
k6B
kB
kB
kB
kB
k6B
kQB
k�B
lqB
l�B
m)B
mwB
m�B
nB
n/B
nIB
ncB
ncB
oOB
o�B
o�B
pUB
poB
pUB
p�B
q'B
qAB
q�B
rGB
rGB
raB
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
uB
u?B
vB
v+B
vzB
v�B
v�B
w�B
xB
x8B
y�B
y�B
z*B
z^B
z^B
z�B
z�B
z�B
z�B
{JB
{dB
{�B
{�B
{�B
|B
|�B
|jB
|jB
|jB
|�B
|�B
|�B
|�B
}VB
}�B
}�B
~]B
~BB
~]B
~(B
~�B
~�B
HB
cB
cB
}B
cB
cB
HB
� B
�iB
��B
��B
��B
�;B
��B
��B
��B
��B
�[B
��B
�-B
�GB
�GB
�-B
�GB
��B
�B
�MB
��B
��B
��B
��B
�B
�B
�9B
�mB
��B
�%B
�tB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�EB
�_B
��B
��B
��B
��B
��B
�B
�1B
�fB
�KB
�K1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
/iB
/5B
/5B
/5B
/iB
/5B
/B
/B
/B
/B
/OB
/B
/5B
/5B
/B
/B
/B
/B
/B
/B
/OB
/OB
/iB
/OB
/5B
/B
/B
.IB
=�B
e�B
y$B
�B
��B
�MB
�)B
��B
��B
�B
��B
�[B
z�B
|�B
tTB
e,B
�RB
��B
�+B
��B
��B
�B
�$B+BuBB+�B=�BB�BLdBjBo5BqBq�B{dBz*Bu?Bq'BT{B5�B:�B#�B<BKB�B	�B�B�B�BzB�B3B�B{B
��B
��B
��B
�yB
ȀB
��B
��B
�	B
cnB
/5B
BB	ޞB	�AB	��B	.B	iDB	S&B	JXB	E�B	>]B	/ B	%�B		�BޞB�B�BżB��B��B	B�RB�BB�OB�*B�UB��B�4B�B��B�eB�B��B�wB�B�0B��B�(B�"B�jB�xB�2B��B��B	 �B��B��B�$B��B��B��B�fB�B�B��B	
XB��B��B��B	 OB	B�B�B��B�B�QB��B	 �B	�B	 �B�qB��B�B�JB�rB�$B�8B�^B��B��B�XB�>B��B�3B�B�LB��B��B�zB��B	�B	�B	&B	 vB	A�B	Q B	^5B	c�B	f�B	ezB	dB	nIB	s�B	t�B	x�B	~�B	�aB	�9B	��B	�YB	�zB	��B	�_B	��B	��B	��B	�zB	�+B	�	B	��B	�B	�gB	��B	��B	�7B	�B	��B	�pB	�|B	�RB	��B	��B	��B	��B	��B	��B	�cB	��B	�oB	��B	�B	�5B	�B	�B	�fB	��B	�?B	��B	��B	�RB	�$B	�^B	�PB	�B	��B	�;B	��B	��B	��B	ǔB	�lB	��B	�PB	�"B	�bB	ңB	ңB	�B	�pB	͟B	̘B	��B	�6B	��B	��B	�~B	ΥB	ϫB	��B	� B	��B	��B	ѷB	��B	�oB	��B	�B	�SB	�9B	�B	�mB	�mB	רB	��B	��B	یB	��B	�B	��B	ܒB	ߤB	� B	�LB	�B	�B	�/B	�B	��B	�B	�/B	��B	�'B	��B	�B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�6B	�6B	�B	�B	��B	�qB	��B	�B	�B	�wB	�B	��B	��B	�B	�}B	��B	��B	�B	��B	��B	�>B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�nB	�TB	�B	�nB	�LB	�XB	��B	�(B
 iB
B
�B
[B
B
�B
�B
�B
�B
B
B
�B
[B
�B
�B
oB
UB
�B
�B
-B
{B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
mB
B
�B
�B
B
B
B
9B
B
B
�B
�B
B
KB
KB
fB
fB
�B
	B
�B
	7B
	lB

�B
�B
B
�B
�B
PB
PB
B
PB
B
�B
JB
�B
�B
^B
)B

�B

�B
0B
�B
�B
0B
�B
�B
�B
0B
B
�B
�B
�B
~B
B
�B
�B
VB
�B
BB
B
�B
�B
:B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
,B
aB
�B
�B
�B
�B
�B
�B
9B
�B
�B
�B
�B
�B

B
?B
+B
�B
�B
KB
1B
1B
B
7B
QB
qB
	B
	B
	B
�B
WB
�B
�B
�B
~B
�B
B
�B
OB
jB
jB
5B
 B
�B
 BB
 �B
 �B
 �B
!B
!�B
!�B
"B
"4B
!�B
!�B
"B
!�B
!�B
!�B
!�B
!�B
"NB
"�B
#B
"�B
#�B
$@B
$ZB
$�B
%zB
%`B
%,B
%zB
%�B
&2B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
*eB
*B
*KB
*�B
,B
,�B
,�B
-wB
-�B
.B
.}B
./B
.B
-�B
-�B
-�B
/iB
/iB
0�B
1�B
3�B
3�B
4TB
4�B
5%B
5�B
5�B
6B
6zB
6�B
7fB
8B
8�B
9>B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
<B
<�B
<�B
="B
=B
="B
="B
="B
="B
="B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=qB
=qB
=qB
=VB
=<B
=B
=B
=B
<�B
<�B
<�B
=qB
=�B
>B
>(B
>]B
>]B
>wB
>BB
>]B
?.B
?.B
?.B
?HB
?HB
?}B
?�B
?�B
@4B
@�B
@�B
@�B
@�B
@�B
@�B
AB
AUB
AoB
A�B
B'B
B[B
B�B
B�B
BuB
CB
C-B
C{B
C-B
C-B
B�B
B�B
B�B
B�B
B'B
BB
A�B
A�B
A�B
A�B
A�B
AUB
A;B
@�B
AUB
A�B
A�B
AoB
AUB
AUB
A�B
C-B
C-B
DMB
D�B
EB
EB
E9B
EmB
E�B
E�B
E�B
E�B
E�B
E�B
FB
GzB
G�B
I�B
KDB
LB
M�B
N�B
OBB
O�B
PbB
P�B
P�B
QB
Q�B
R B
RTB
RTB
RTB
RoB
R�B
S�B
S�B
SuB
SuB
S�B
S�B
S�B
SuB
S�B
S�B
SuB
S�B
S�B
S�B
TB
TB
T,B
T�B
T�B
T�B
U2B
U�B
U�B
U�B
VB
VmB
V�B
V�B
W$B
WYB
WYB
W�B
WYB
WsB
W?B
W�B
W�B
W�B
W�B
X_B
YB
ZQB
Z�B
[#B
[�B
[�B
[�B
\B
\)B
\)B
\B
\B
^B
^5B
^5B
^B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^B
^5B
^�B
^�B
^�B
_VB
_�B
_�B
`B
`�B
`�B
aHB
a�B
b�B
b�B
c B
cnB
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
e,B
e�B
fB
fLB
ffB
fLB
fLB
f�B
f�B
f�B
gB
f�B
gB
gRB
hXB
hsB
h�B
h�B
h�B
iB
i�B
jB
i�B
i�B
i�B
jB
jB
j0B
j0B
jKB
j�B
j�B
j�B
kB
k6B
kB
kB
kB
kB
k6B
kQB
k�B
lqB
l�B
m)B
mwB
m�B
nB
n/B
nIB
ncB
ncB
oOB
o�B
o�B
pUB
poB
pUB
p�B
q'B
qAB
q�B
rGB
rGB
raB
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
uB
u?B
vB
v+B
vzB
v�B
v�B
w�B
xB
x8B
y�B
y�B
z*B
z^B
z^B
z�B
z�B
z�B
z�B
{JB
{dB
{�B
{�B
{�B
|B
|�B
|jB
|jB
|jB
|�B
|�B
|�B
|�B
}VB
}�B
}�B
~]B
~BB
~]B
~(B
~�B
~�B
HB
cB
cB
}B
cB
cB
HB
� B
�iB
��B
��B
��B
�;B
��B
��B
��B
��B
�[B
��B
�-B
�GB
�GB
�-B
�GB
��B
�B
�MB
��B
��B
��B
��B
�B
�B
�9B
�mB
��B
�%B
�tB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�EB
�_B
��B
��B
��B
��B
��B
�B
�1B
�fB
�KB
�K1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230728215212  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230728215222  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230728215223  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230728215223                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230728215224  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230728215224  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230728215427                      G�O�G�O�G�O�                