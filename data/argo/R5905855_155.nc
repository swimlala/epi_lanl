CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-05-14T03:44:10Z creation;2023-05-14T03:44:12Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230514034410  20230514035855  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�*��i1   @�+ ��@0G�z��c����S�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�33B˙�B�  B�  B�  B�ffBߙ�B���B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB�CD  CF  CH  CI�fCL  CN  CP  CR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�3D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B�W
B��B��B��B��B�#�B�#�B��B��qB��B��B��B�#�Bˊ>B��B��B��B�W
Bߊ>B�qB��B��B��B��B��B��B��C޸C�RC�RC�RC	�RC�RC�C�C�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC6�C7�RC9�RC;�RC=�RC?�RCB�CC�RCE�RCG�RCI޸CK�RCM�RCO�RCQ�RCT�CU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCv�Cw�RCy޸C{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D�zDzD~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DMzDM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D�=D�?
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
D�5p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aѷ�A�\)A��A�y	A�=A��|A��
A�ܒA�خA�רA���A��gA��BA���AϓuA�y>A�f2A�XA�@�A�;0A�6�A�,�A�!-A��A��A���A��|A��A��yA��<A��^AΝ�A�v�A�[#A�>BA�Aͮ}A�yrA�xlA�u�A�r�A�g�A�<6A�*�A�%�A��A�0!A�&A��UA��A��-A�<6A��A�]�A��BA�G�A���A���A�S�A��A�u�A���A��HA��A��aA�A�A���A���A�{�A��A�IA���A�*0A�MA�S�A��A�!bA��A���A�.IA��qA��.A�m�A��,A�A� �A�Q�A�7�A�DA�}"A�@OA��~A�aA�7�A�iDA�CA�q�A�OBA��kA�(XA�Q�A}9XAz�Aw��At:�AotTAnu�AfFAb`BA_��A[�+AZiDAYxAV�AS�cAR	�AO�AM��AI��AG&�AAqA="hA;�6A:S&A8�:A4��A4U2A5�A3��A2�A-�mA,� A+�A)��A(��A(��A(�A'�A&	A$`�A"��A!:*A ��AϫA�9A��A�	A�SA�oAcA`BA��A/�A��A6A	A�`A[WA��A��AW�A�A�A
��A��A�Ab�A�AY�A�$A�A��AxlAHA4A�A8�A~Ay�A��AGA�A��A"hA��Aw2A*�A�LA��A��A]�A �@�C�@��@�k�@���@���@���@���@�v�@�2�@�_p@�\�@�^@�S�@�`�@�m�@��@��@�'�@�{�@�[@��@�F@�@��@�	@�ȴ@�'R@�z@�"�@�/@���@�@�S�@�u@鰊@���@�a�@腈@���@�J�@�
=@�h�@�3�@�X@�C�@��,@��@�F@���@�_@��>@�
�@ߥ@���@�q�@��@��@�h
@��+@ہ�@� i@پw@�"�@�ی@��U@�tT@��@�J@��@�G�@ֵ�@�9X@��@�@��2@�Ft@ӌ~@�
�@ѹ�@Ѥ@@�Z�@���@ж�@а!@б�@Ч@�oi@ϑh@�4n@�  @���@��z@�v`@�/�@�ں@̯O@�_@�خ@��@ʇ+@�M@�
�@��@�t�@��`@�M�@��@�L�@�ں@�L0@��
@�m]@��@�r�@��@�B�@��E@�3�@�ϫ@�ی@�S�@�#:@�0�@�u%@��@��w@���@��4@�F�@�+@���@�q@��@��C@���@��@��@���@��E@���@�(�@���@�}�@���@��'@�n/@��@���@��@��O@���@�'R@��+@�H�@�S@��@�$t@��@�\�@�RT@���@�֡@��r@��n@�o @���@�:*@�RT@�+@���@��m@�d�@�@�@��@���@��h@�j@��@��@��*@�k�@��p@�J�@��4@�Dg@�,�@�@��z@�H@�4n@�$�@���@��N@�@@��L@�~(@�8�@���@�N<@���@�{@��*@�%F@��r@�tT@�]d@��@�RT@��@�xl@�Ta@�@���@��@�]d@���@�l�@�@��.@�R�@�e@��+@��@��@�J�@��	@���@��|@���@�m�@�A�@�*�@��@�u@��=@���@�~(@��)@���@�x@�G�@�C@���@���@�q�@�	�@�خ@��@�u@���@��]@��@��#@���@��@�j�@���@���@�}V@�Q@��@���@��@�_p@�*0@�Y@���@�Ĝ@�`�@��6@��~@�qv@�&�@���@�V@�>B@�	@���@�y�@�T�@�E9@�o@���@�U2@��>@�S�@�"�@��@��@���@���@���@�IR@�m�@��@�u@��t@���@��$@�e�@���@�]d@�$�@��@�O@�,=@�%�@���@��@��q@���@�B�@��B@��@�u�@�R�@�6�@�2�@�.�@�O@�7@��@��@� �@��@�X@��@�֡@��@��b@�h�@�($@��m@��z@��k@�x@�@O@��H@�Q@��T@��@�c@�m]@�S&@��@��@��x@�Ov@�Ov@�C�@��@�rG@�K�@�J#@�9�@��@�ȴ@�?�@�y�@�5�@��@��@��@���@�u%@�4n@�@~��@|��@{S@y��@yc�@x��@x��@x9X@x�@w�*@v�y@v3�@uc@tĜ@tS�@s�{@r��@r��@r{�@r1�@q�X@qIR@p�D@p�@o6z@n�2@n�X@n��@n�+@ni�@m�-@m5�@mV@m�@l�P@l��@l�9@lz�@lPH@l4n@l(�@l'R@l�@l�@k�@k��@j��@j�x@i��@ic�@iL�@h�.@h>B@hM@g�]@g��@gt�@g+@f��@f�1@f�@e��@e�@e2a@d��@doi@d	�@c�@c��@c�@cJ#@b�}@b�@a�#@a<6@`�|@`�$@`�Y@`r�@`Z@`H@`C-@`?�@`:�@_K�@^h
@]��@\�_@\��@\4n@[�&@[@O@Z�]@Z�}@Zs�@Zff@ZL0@Y��@Yo @Y�@X�I@W�F@W|�@W�4@Wy�@WC�@V�c@V�}@Vh
@V1�@V+k@V �@U��@U��@U�j@U��@U��@U��@U�@U��@U8�@T�@T�@T|�@S�+@S;d@S�@RYK@Q��@Qhs@P��@P��@O��@O��@Oa@N�@N��@Ov`@N��@N�F@Nu%@M�@M&�@L��@LA�@K�a@K�@KZ�@KiD@KC@J��@Jh
@I�j@I�@H�@H�@Hw�@Gt�@F�@F�6@FJ�@FO@E��@E��@E�d@E�-@EX@D�?@D7�@D~@C�W@C�@C�;@C]�@C@B�A@A��@A�~@Ak�@ADg@A8�@A7L@A2a@A�@@Xy@@	�@?�a@?��@?dZ@>��@>�1@>��@>��@>v�@>q�@>kQ@>R�@>�@=�^@=Vm@=%F@<�@<C-@<-�@<b@;�@:�,@:��@:� @:GE@:O@9�@9Vm@9�@8��@8tT@8U2@7ݘ@7{J@7g�@7>�@6��@6҉@6��@6u%@6c @6�@5��@58�@5+@5�@4��@4_@49X@3��@3��@3��@3�@2�c@2��@2�\@2!�@1�S@1#�@0�@0'R@/˒@/��@/S�@/C@.��@.ߤ@.�x@.?@-�@-�@-��@-Q�@,~(@,$@,-�@,!@+�W@+��@*�y@*�'@*�@*� @*xl@*R�@)��@)��@(�f@(��@(M@(�@'�[@'t�@'S@&�\@&_�@&GE@&6�@&O@&�@&
�@%��@%�@%7L@%@@$�v@$�@$z�@$y>@$w�@$9X@#�@#\)@#;d@#8@#
=@"�<@"&�@!��@!��@!�h@!:�@ �@ ��@ `�@ 7�@�]@�}@��@�@@��@�4@�@�R@�1@Z�@L0@�@��@�@�~@A @�`@��@�@w�@Xy@M@I�@<�@�@�@�@t�@F�@,�@�@��@ں@�+@c @W�@Q@Q@Ov@J�@3�@@ �@��@�>@�@u�@�@�/@ی@�@$@� @��@v`@C@�R@�+@kQ@M�@&�@�@��@�t@�"@X@:�@�@�$@�@��@H@>B@<�@�@G@�]@�@�f@t�@\)@8@&@��@n�@O@�@�H@��@8�@�@��@�/@�.@`�@<�@,=@'R@7@�@�@�W@��@l�@�8@�@��@�@�F@� @�r@l�@L0@@�@:*@e@�#@�t@�=@��@x�@e,@+@�@y>@6@@�m@��@�$@�:@��@�P@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aѷ�A�\)A��A�y	A�=A��|A��
A�ܒA�خA�רA���A��gA��BA���AϓuA�y>A�f2A�XA�@�A�;0A�6�A�,�A�!-A��A��A���A��|A��A��yA��<A��^AΝ�A�v�A�[#A�>BA�Aͮ}A�yrA�xlA�u�A�r�A�g�A�<6A�*�A�%�A��A�0!A�&A��UA��A��-A�<6A��A�]�A��BA�G�A���A���A�S�A��A�u�A���A��HA��A��aA�A�A���A���A�{�A��A�IA���A�*0A�MA�S�A��A�!bA��A���A�.IA��qA��.A�m�A��,A�A� �A�Q�A�7�A�DA�}"A�@OA��~A�aA�7�A�iDA�CA�q�A�OBA��kA�(XA�Q�A}9XAz�Aw��At:�AotTAnu�AfFAb`BA_��A[�+AZiDAYxAV�AS�cAR	�AO�AM��AI��AG&�AAqA="hA;�6A:S&A8�:A4��A4U2A5�A3��A2�A-�mA,� A+�A)��A(��A(��A(�A'�A&	A$`�A"��A!:*A ��AϫA�9A��A�	A�SA�oAcA`BA��A/�A��A6A	A�`A[WA��A��AW�A�A�A
��A��A�Ab�A�AY�A�$A�A��AxlAHA4A�A8�A~Ay�A��AGA�A��A"hA��Aw2A*�A�LA��A��A]�A �@�C�@��@�k�@���@���@���@���@�v�@�2�@�_p@�\�@�^@�S�@�`�@�m�@��@��@�'�@�{�@�[@��@�F@�@��@�	@�ȴ@�'R@�z@�"�@�/@���@�@�S�@�u@鰊@���@�a�@腈@���@�J�@�
=@�h�@�3�@�X@�C�@��,@��@�F@���@�_@��>@�
�@ߥ@���@�q�@��@��@�h
@��+@ہ�@� i@پw@�"�@�ی@��U@�tT@��@�J@��@�G�@ֵ�@�9X@��@�@��2@�Ft@ӌ~@�
�@ѹ�@Ѥ@@�Z�@���@ж�@а!@б�@Ч@�oi@ϑh@�4n@�  @���@��z@�v`@�/�@�ں@̯O@�_@�خ@��@ʇ+@�M@�
�@��@�t�@��`@�M�@��@�L�@�ں@�L0@��
@�m]@��@�r�@��@�B�@��E@�3�@�ϫ@�ی@�S�@�#:@�0�@�u%@��@��w@���@��4@�F�@�+@���@�q@��@��C@���@��@��@���@��E@���@�(�@���@�}�@���@��'@�n/@��@���@��@��O@���@�'R@��+@�H�@�S@��@�$t@��@�\�@�RT@���@�֡@��r@��n@�o @���@�:*@�RT@�+@���@��m@�d�@�@�@��@���@��h@�j@��@��@��*@�k�@��p@�J�@��4@�Dg@�,�@�@��z@�H@�4n@�$�@���@��N@�@@��L@�~(@�8�@���@�N<@���@�{@��*@�%F@��r@�tT@�]d@��@�RT@��@�xl@�Ta@�@���@��@�]d@���@�l�@�@��.@�R�@�e@��+@��@��@�J�@��	@���@��|@���@�m�@�A�@�*�@��@�u@��=@���@�~(@��)@���@�x@�G�@�C@���@���@�q�@�	�@�خ@��@�u@���@��]@��@��#@���@��@�j�@���@���@�}V@�Q@��@���@��@�_p@�*0@�Y@���@�Ĝ@�`�@��6@��~@�qv@�&�@���@�V@�>B@�	@���@�y�@�T�@�E9@�o@���@�U2@��>@�S�@�"�@��@��@���@���@���@�IR@�m�@��@�u@��t@���@��$@�e�@���@�]d@�$�@��@�O@�,=@�%�@���@��@��q@���@�B�@��B@��@�u�@�R�@�6�@�2�@�.�@�O@�7@��@��@� �@��@�X@��@�֡@��@��b@�h�@�($@��m@��z@��k@�x@�@O@��H@�Q@��T@��@�c@�m]@�S&@��@��@��x@�Ov@�Ov@�C�@��@�rG@�K�@�J#@�9�@��@�ȴ@�?�@�y�@�5�@��@��@��@���@�u%@�4n@�@~��@|��@{S@y��@yc�@x��@x��@x9X@x�@w�*@v�y@v3�@uc@tĜ@tS�@s�{@r��@r��@r{�@r1�@q�X@qIR@p�D@p�@o6z@n�2@n�X@n��@n�+@ni�@m�-@m5�@mV@m�@l�P@l��@l�9@lz�@lPH@l4n@l(�@l'R@l�@l�@k�@k��@j��@j�x@i��@ic�@iL�@h�.@h>B@hM@g�]@g��@gt�@g+@f��@f�1@f�@e��@e�@e2a@d��@doi@d	�@c�@c��@c�@cJ#@b�}@b�@a�#@a<6@`�|@`�$@`�Y@`r�@`Z@`H@`C-@`?�@`:�@_K�@^h
@]��@\�_@\��@\4n@[�&@[@O@Z�]@Z�}@Zs�@Zff@ZL0@Y��@Yo @Y�@X�I@W�F@W|�@W�4@Wy�@WC�@V�c@V�}@Vh
@V1�@V+k@V �@U��@U��@U�j@U��@U��@U��@U�@U��@U8�@T�@T�@T|�@S�+@S;d@S�@RYK@Q��@Qhs@P��@P��@O��@O��@Oa@N�@N��@Ov`@N��@N�F@Nu%@M�@M&�@L��@LA�@K�a@K�@KZ�@KiD@KC@J��@Jh
@I�j@I�@H�@H�@Hw�@Gt�@F�@F�6@FJ�@FO@E��@E��@E�d@E�-@EX@D�?@D7�@D~@C�W@C�@C�;@C]�@C@B�A@A��@A�~@Ak�@ADg@A8�@A7L@A2a@A�@@Xy@@	�@?�a@?��@?dZ@>��@>�1@>��@>��@>v�@>q�@>kQ@>R�@>�@=�^@=Vm@=%F@<�@<C-@<-�@<b@;�@:�,@:��@:� @:GE@:O@9�@9Vm@9�@8��@8tT@8U2@7ݘ@7{J@7g�@7>�@6��@6҉@6��@6u%@6c @6�@5��@58�@5+@5�@4��@4_@49X@3��@3��@3��@3�@2�c@2��@2�\@2!�@1�S@1#�@0�@0'R@/˒@/��@/S�@/C@.��@.ߤ@.�x@.?@-�@-�@-��@-Q�@,~(@,$@,-�@,!@+�W@+��@*�y@*�'@*�@*� @*xl@*R�@)��@)��@(�f@(��@(M@(�@'�[@'t�@'S@&�\@&_�@&GE@&6�@&O@&�@&
�@%��@%�@%7L@%@@$�v@$�@$z�@$y>@$w�@$9X@#�@#\)@#;d@#8@#
=@"�<@"&�@!��@!��@!�h@!:�@ �@ ��@ `�@ 7�@�]@�}@��@�@@��@�4@�@�R@�1@Z�@L0@�@��@�@�~@A @�`@��@�@w�@Xy@M@I�@<�@�@�@�@t�@F�@,�@�@��@ں@�+@c @W�@Q@Q@Ov@J�@3�@@ �@��@�>@�@u�@�@�/@ی@�@$@� @��@v`@C@�R@�+@kQ@M�@&�@�@��@�t@�"@X@:�@�@�$@�@��@H@>B@<�@�@G@�]@�@�f@t�@\)@8@&@��@n�@O@�@�H@��@8�@�@��@�/@�.@`�@<�@,=@'R@7@�@�@�W@��@l�@�8@�@��@�@�F@� @�r@l�@L0@@�@:*@e@�#@�t@�=@��@x�@e,@+@�@y>@6@@�m@��@�$@�:@��@�P@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
pB
�B
�B
pB
�B
OB
�B
�B
�B
#B
'B
+B
/ B
3�B
49B
1�B
/�B
-CB
)�B
)B
(>B
&2B
$@B
 �B
VB
�B
qB
�B

B
�B
�B
pB
�B
-B	��B	�zB	�8B	�jB	��B	��B	�B	�5B	�kB	�QB	�jB	޸B	��B
#B
<�B
K^B
h�B
��B
�PB
�HB
�0B
ÖB
��BMB�B#nB'�B,�B;�B:�B9XBJ�BR BO�BMBJ#BF?BB�BB[BB�B@iB@ B8�B;dBX_BfBh>B^�BN<BF�B>BB1�B%zB�B�B
�B
��B
��B
�oB
�5B
�{B
�B
y	B
jB
W$B
OvB
A�B
_B
�B	�B	�B	�_B	�DB	�CB	�B	w�B	h�B	bhB	[qB	R�B	K�B	DMB	:�B	2aB	#B	B	3B��B��B��B�B��B�vB	$�B	+�B	0�B��B		B	bB	�B	4B	�B	"B	~B	�B	0B	�B	[B��B��B��B��B�3B�fB�$B��B�B		lB	�B�"BөB�gB�_B�sB��B�BބB�yB��B�MB��B�'B��B��B��B�>B�B��B��B�lB�B�dB	 �B	UB	�B	MB	�B	�B	�B	�B	(�B	/�B	0;B	0!B	*�B	(XB	+�B	)B	)*B	 �B	�B	�B	 'B	#�B	*�B	*KB	%�B	=B	sB	�B	�B��B	_B	D�B	X�B	dB	ezB	iDB	d�B	j�B	c�B	aHB	_�B	]�B	b�B	gB	ncB	n}B	oB	oB	qvB	s�B	tTB	|B	�OB	~�B	}qB	}B	}B	~�B	�B	��B	�%B	�	B	�B	�vB	�KB	��B	�GB	��B	�1B	��B	��B	�7B	�fB	�zB	�+B	��B	��B	��B	�	B	�xB	��B	��B	��B	�gB	��B	��B	�+B	��B	�WB	��B	�/B	��B	�QB	�KB	��B	��B	�qB	��B	��B	�dB	�IB	�OB	��B	�B	��B	��B	��B	�XB	��B	�KB	��B	�B	��B	�=B	��B	��B	�B	��B	�B	�aB	�B	�B	�?B	�B	�fB	�	B	��B	��B	�0B	��B	��B	�HB	�4B	�oB	��B	�?B	��B	��B	�zB	�B	ɠB	�XB	ʌB	�DB	�dB	�6B	��B	�(B	��B	ӏB	�aB	ՁB	��B	֡B	�mB	ևB	��B	�B	�kB	��B	��B	ܒB	�WB	�]B	ܬB	ݘB	�HB	�B	� B	�ZB	��B	�FB	�LB	�B	�qB	�B	�CB	�B	�5B	�B	� B	�`B	��B	��B	��B
 OB
{B
�B
�B
�B
�B
3B
�B
MB
9B
SB
�B
�B
�B
�B
�B
�B
�B
�B
�B

XB

�B
�B
B
4B
�B
TB
�B
�B
B
B
vB
�B
 B
hB
NB
�B
�B
�B
TB
�B
�B
@B
�B
�B
�B
�B
mB
�B
$B
+B
_B
_B
EB
_B
B
1B
B
�B
B
�B
B
7B
QB
B
QB
#B
�B
�B
�B
�B
IB
dB
�B
5B
�B
�B
"hB
$ZB
'�B
*0B
+�B
+�B
,=B
,"B
+�B
,B
,�B
-wB
-�B
-�B
.B
.IB
.cB
.IB
.IB
/ B
/�B
/�B
0UB
1vB
2B
1�B
2|B
3MB
3�B
3�B
3�B
3�B
3�B
3�B
3�B
3MB
33B
2�B
3�B
3B
2�B
3MB
3hB
3MB
2|B
2B
0�B
0;B
/�B
2�B
1�B
2-B
1�B
1[B
0�B
0UB
0�B
1AB
2GB
2�B
2�B
2�B
3�B
6FB
7fB
7�B
7fB
7�B
8B
9	B
9�B
:�B
;B
;�B
;�B
<B
<B
<B
<jB
=qB
>(B
>�B
>�B
>�B
>�B
?B
?}B
?}B
?cB
?HB
?B
>�B
>�B
?�B
@iB
A;B
@�B
@�B
A B
A�B
B[B
B�B
B�B
B�B
CGB
C�B
DB
C�B
C�B
C{B
B�B
B'B
A�B
B'B
B�B
C�B
DB
D�B
EB
D�B
DgB
B�B
?�B
=<B
;B
;�B
;�B
;�B
<PB
<6B
<jB
<�B
="B
=qB
>B
>(B
>�B
?cB
?}B
@4B
@iB
@�B
@�B
A;B
AUB
B�B
B�B
B�B
C-B
C-B
C-B
DMB
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
H�B
J=B
LdB
L�B
L�B
L�B
MPB
M�B
MB
M�B
M�B
O(B
O�B
O�B
O�B
PHB
P�B
P}B
P�B
QB
QB
Q B
QNB
Q�B
RB
R�B
S@B
S@B
S[B
S[B
SuB
S�B
TFB
TaB
T�B
T{B
T�B
T�B
T{B
T�B
T�B
T�B
T{B
TB
T�B
TFB
TaB
SuB
S�B
S�B
S�B
T�B
TaB
T�B
TFB
T{B
T{B
TaB
T�B
T�B
T�B
TaB
T�B
T�B
T�B
T�B
UB
U�B
U�B
VB
VB
V�B
V�B
V�B
X�B
Y�B
ZQB
ZkB
Z�B
Z�B
[=B
[#B
Z�B
Z�B
Z�B
Z�B
Z�B
Z7B
ZB
ZQB
Z�B
ZQB
ZQB
ZB
ZB
Z�B
[�B
]~B
]�B
^OB
^�B
^5B
]/B
]/B
]~B
]IB
]�B
^B
^�B
_B
_B
_�B
`\B
`'B
`BB
`'B
_�B
`�B
aB
abB
bB
b4B
bhB
b�B
b�B
b�B
b�B
c�B
dtB
eB
e`B
f�B
g�B
gB
f�B
gRB
g�B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
iyB
iyB
i�B
jB
i�B
jKB
j�B
kB
kB
kB
kB
kB
j�B
kQB
k�B
k�B
k�B
l"B
l�B
lWB
lqB
l�B
mwB
m]B
mCB
m�B
mwB
m�B
nIB
ncB
n�B
n�B
n�B
n�B
n�B
n�B
oB
oB
o5B
o5B
oiB
o�B
o�B
p!B
pUB
p�B
qAB
r-B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tB
t�B
t�B
t�B
tTB
tnB
t�B
t�B
u?B
utB
u�B
v+B
vzB
vFB
vzB
vzB
v�B
v�B
v�B
wB
wLB
xB
x8B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z^B
zxB
{0B
{dB
{�B
{�B
|B
|B
|�B
|�B
|�B
|�B
|�B
|�B
}B
|�B
}B
}"B
}VB
}VB
}qB
}�B
}�B
}�B
}�B
~B
~(B
~�B
~�B
~�B
~�B
~�B
.B
cB
�B
�B
�OB
��B
��B
��B
��B
�;B
�UB
�UB
�UB
�;B
�;B
��B
��B
�AB
��B
��B
��B
�-B
�GB
�aB
��B
��B
�B
�3B
�MB
��B
��B
��B
�gB
��B
��B
��B
�9B
�SB
�mB
�mB
��B
��B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�?B
�YB
�tB
�YB
�YB
�tB
��B
��B
�B
��B
�EB
��B
��B
��B
�B
�fB
��B
��B
�B
�B
�7B
�RB
�RB
��B
��B
��B
�	B
�	B
�XB
�rB
�rB
��B
��B
��B
��B
��B
��B
�)B
�DB
�DB
�^B
�^B
�DB
�xB
�B
�dB
��B
��B
��B
�PB
��B
��B
��B
�<B
�pB
��B
��B
��B
��B
��B
��B
��B
��B
�BB
��B
��B
��B
��B
�.B
�B
�B
�.B
�HB
�bB
�HB
�bB
��B
��B
��B
� B
� B
�B
�hB
�hB
� B
�TB
�oB
��B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
pB
�B
�B
pB
�B
OB
�B
�B
�B
#B
'B
+B
/ B
3�B
49B
1�B
/�B
-CB
)�B
)B
(>B
&2B
$@B
 �B
VB
�B
qB
�B

B
�B
�B
pB
�B
-B	��B	�zB	�8B	�jB	��B	��B	�B	�5B	�kB	�QB	�jB	޸B	��B
#B
<�B
K^B
h�B
��B
�PB
�HB
�0B
ÖB
��BMB�B#nB'�B,�B;�B:�B9XBJ�BR BO�BMBJ#BF?BB�BB[BB�B@iB@ B8�B;dBX_BfBh>B^�BN<BF�B>BB1�B%zB�B�B
�B
��B
��B
�oB
�5B
�{B
�B
y	B
jB
W$B
OvB
A�B
_B
�B	�B	�B	�_B	�DB	�CB	�B	w�B	h�B	bhB	[qB	R�B	K�B	DMB	:�B	2aB	#B	B	3B��B��B��B�B��B�vB	$�B	+�B	0�B��B		B	bB	�B	4B	�B	"B	~B	�B	0B	�B	[B��B��B��B��B�3B�fB�$B��B�B		lB	�B�"BөB�gB�_B�sB��B�BބB�yB��B�MB��B�'B��B��B��B�>B�B��B��B�lB�B�dB	 �B	UB	�B	MB	�B	�B	�B	�B	(�B	/�B	0;B	0!B	*�B	(XB	+�B	)B	)*B	 �B	�B	�B	 'B	#�B	*�B	*KB	%�B	=B	sB	�B	�B��B	_B	D�B	X�B	dB	ezB	iDB	d�B	j�B	c�B	aHB	_�B	]�B	b�B	gB	ncB	n}B	oB	oB	qvB	s�B	tTB	|B	�OB	~�B	}qB	}B	}B	~�B	�B	��B	�%B	�	B	�B	�vB	�KB	��B	�GB	��B	�1B	��B	��B	�7B	�fB	�zB	�+B	��B	��B	��B	�	B	�xB	��B	��B	��B	�gB	��B	��B	�+B	��B	�WB	��B	�/B	��B	�QB	�KB	��B	��B	�qB	��B	��B	�dB	�IB	�OB	��B	�B	��B	��B	��B	�XB	��B	�KB	��B	�B	��B	�=B	��B	��B	�B	��B	�B	�aB	�B	�B	�?B	�B	�fB	�	B	��B	��B	�0B	��B	��B	�HB	�4B	�oB	��B	�?B	��B	��B	�zB	�B	ɠB	�XB	ʌB	�DB	�dB	�6B	��B	�(B	��B	ӏB	�aB	ՁB	��B	֡B	�mB	ևB	��B	�B	�kB	��B	��B	ܒB	�WB	�]B	ܬB	ݘB	�HB	�B	� B	�ZB	��B	�FB	�LB	�B	�qB	�B	�CB	�B	�5B	�B	� B	�`B	��B	��B	��B
 OB
{B
�B
�B
�B
�B
3B
�B
MB
9B
SB
�B
�B
�B
�B
�B
�B
�B
�B
�B

XB

�B
�B
B
4B
�B
TB
�B
�B
B
B
vB
�B
 B
hB
NB
�B
�B
�B
TB
�B
�B
@B
�B
�B
�B
�B
mB
�B
$B
+B
_B
_B
EB
_B
B
1B
B
�B
B
�B
B
7B
QB
B
QB
#B
�B
�B
�B
�B
IB
dB
�B
5B
�B
�B
"hB
$ZB
'�B
*0B
+�B
+�B
,=B
,"B
+�B
,B
,�B
-wB
-�B
-�B
.B
.IB
.cB
.IB
.IB
/ B
/�B
/�B
0UB
1vB
2B
1�B
2|B
3MB
3�B
3�B
3�B
3�B
3�B
3�B
3�B
3MB
33B
2�B
3�B
3B
2�B
3MB
3hB
3MB
2|B
2B
0�B
0;B
/�B
2�B
1�B
2-B
1�B
1[B
0�B
0UB
0�B
1AB
2GB
2�B
2�B
2�B
3�B
6FB
7fB
7�B
7fB
7�B
8B
9	B
9�B
:�B
;B
;�B
;�B
<B
<B
<B
<jB
=qB
>(B
>�B
>�B
>�B
>�B
?B
?}B
?}B
?cB
?HB
?B
>�B
>�B
?�B
@iB
A;B
@�B
@�B
A B
A�B
B[B
B�B
B�B
B�B
CGB
C�B
DB
C�B
C�B
C{B
B�B
B'B
A�B
B'B
B�B
C�B
DB
D�B
EB
D�B
DgB
B�B
?�B
=<B
;B
;�B
;�B
;�B
<PB
<6B
<jB
<�B
="B
=qB
>B
>(B
>�B
?cB
?}B
@4B
@iB
@�B
@�B
A;B
AUB
B�B
B�B
B�B
C-B
C-B
C-B
DMB
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
H�B
J=B
LdB
L�B
L�B
L�B
MPB
M�B
MB
M�B
M�B
O(B
O�B
O�B
O�B
PHB
P�B
P}B
P�B
QB
QB
Q B
QNB
Q�B
RB
R�B
S@B
S@B
S[B
S[B
SuB
S�B
TFB
TaB
T�B
T{B
T�B
T�B
T{B
T�B
T�B
T�B
T{B
TB
T�B
TFB
TaB
SuB
S�B
S�B
S�B
T�B
TaB
T�B
TFB
T{B
T{B
TaB
T�B
T�B
T�B
TaB
T�B
T�B
T�B
T�B
UB
U�B
U�B
VB
VB
V�B
V�B
V�B
X�B
Y�B
ZQB
ZkB
Z�B
Z�B
[=B
[#B
Z�B
Z�B
Z�B
Z�B
Z�B
Z7B
ZB
ZQB
Z�B
ZQB
ZQB
ZB
ZB
Z�B
[�B
]~B
]�B
^OB
^�B
^5B
]/B
]/B
]~B
]IB
]�B
^B
^�B
_B
_B
_�B
`\B
`'B
`BB
`'B
_�B
`�B
aB
abB
bB
b4B
bhB
b�B
b�B
b�B
b�B
c�B
dtB
eB
e`B
f�B
g�B
gB
f�B
gRB
g�B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
iyB
iyB
i�B
jB
i�B
jKB
j�B
kB
kB
kB
kB
kB
j�B
kQB
k�B
k�B
k�B
l"B
l�B
lWB
lqB
l�B
mwB
m]B
mCB
m�B
mwB
m�B
nIB
ncB
n�B
n�B
n�B
n�B
n�B
n�B
oB
oB
o5B
o5B
oiB
o�B
o�B
p!B
pUB
p�B
qAB
r-B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
tB
t�B
t�B
t�B
tTB
tnB
t�B
t�B
u?B
utB
u�B
v+B
vzB
vFB
vzB
vzB
v�B
v�B
v�B
wB
wLB
xB
x8B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z^B
zxB
{0B
{dB
{�B
{�B
|B
|B
|�B
|�B
|�B
|�B
|�B
|�B
}B
|�B
}B
}"B
}VB
}VB
}qB
}�B
}�B
}�B
}�B
~B
~(B
~�B
~�B
~�B
~�B
~�B
.B
cB
�B
�B
�OB
��B
��B
��B
��B
�;B
�UB
�UB
�UB
�;B
�;B
��B
��B
�AB
��B
��B
��B
�-B
�GB
�aB
��B
��B
�B
�3B
�MB
��B
��B
��B
�gB
��B
��B
��B
�9B
�SB
�mB
�mB
��B
��B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�?B
�YB
�tB
�YB
�YB
�tB
��B
��B
�B
��B
�EB
��B
��B
��B
�B
�fB
��B
��B
�B
�B
�7B
�RB
�RB
��B
��B
��B
�	B
�	B
�XB
�rB
�rB
��B
��B
��B
��B
��B
��B
�)B
�DB
�DB
�^B
�^B
�DB
�xB
�B
�dB
��B
��B
��B
�PB
��B
��B
��B
�<B
�pB
��B
��B
��B
��B
��B
��B
��B
��B
�BB
��B
��B
��B
��B
�.B
�B
�B
�.B
�HB
�bB
�HB
�bB
��B
��B
��B
� B
� B
�B
�hB
�hB
� B
�TB
�oB
��B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230514034355  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230514034410  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230514034411  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230514034412                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230514034412  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230514034412  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230514035855                      G�O�G�O�G�O�                