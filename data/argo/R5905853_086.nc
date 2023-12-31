CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:38:12Z creation;2022-06-04T17:38:12Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604173812  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               VA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�|j��/�1   @�|kj �.@.�1&�x��c��"��`1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�33B�  B���B���C   C�fC  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۃ3D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@~{@�
=@�
=A�A?�A_�A�A�A�A�A��\A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB`G�Bg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��qB��B��B��B�#�B�W
B��qB��qB��B��B��B��B��B��B��B��B��B�W
B��B�#�B��B��>B��qB��C޸C�RC�RC�RC	�RC�RC�RC�RC�RC�C�RC�RC�RC�RC�RC�RC!�RC#�RC%޸C'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCt�Cu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~DzD~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
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
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
Dۂ=Dۿ
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
D��=D��=D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�;�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�xA��A�A��A�A��A��A��A��A��A��A��A�xA�CA��A�IA��A�!A��A��A� �A�"4A�#nA�$@A�%FA�#�A�#�A�%FA�&�A�$A�MA�DA�	7A�fA�A���A̔FA�e`A�9XA��AˬA�rGA�Q�A�rGA�"A�#:A���A���A�jKA��A�aA�!-A��vA�W�A�ncA�xlA�`�A�GzA���A��A���A��A�p;A�YA���A��A�~�A��A���A��EA�A�A��0A�֡A��A�~�A��A���A�l"A�~A�w�A�49A��2A�_A�}�A�Y�A�̘A��A���A���A�a�A���A��A���A��A���A�kQA��"A�v`A�0�A~�A{��Av�fAt	An��Aj\�Ah�Ae��Ab�yA^?}AY��AW�AT�'AQL0AP�rAO�6AN�AL�AI�nAG�WAD��AB��A@�fA?S�A=�!A<X�A:�9A:Y�A8�hA6��A5t�A3�A2^�A1�bA1&A0N�A0��A/�ZA.��A+�=A*�\A*�A(�#A'l�A&ݘA&4A%P�A$4A#c A"�A"($A!��A ��A tTAl�A��A�+A6�A�A�5A�@A��Au�A�Ac�AE�A<�A�A\�A�FA7LA�HA�$A|�AMjA$�AE�A�A��A:�A��A�AOA~(A�'AA�AK^A/Aw2AMA� A�A_A_�A
�EA	��A	-A�A��A\�A+A�]A��Al�A�A�Aq�AA��AI�A!A!-A��A��Aq�A%�AoAL�A	A��A[WA�A p;A _pA �VA X�@�2a@�"�@��	@�xl@��Z@���@�x@�YK@�خ@��F@���@�h
@��{@�K�@��@��1@���@���@�@�@���@��@�˒@�P�@��@��@�H@�~�@��m@�#:@빌@�@�}�@��@颜@�!@�@�.@�|�@��+@��@���@�s�@�-�@�@��#@�6z@��&@�0@�f@�c�@��@�s�@���@�)�@�Dg@ރ@ݤ@@��?@�o @��@ڃ�@��@���@��6@�Z�@�-w@��|@ֲ�@�($@�;@�0U@�%�@�G@ӌ~@��@�]d@�dZ@�@�d�@�n/@���@�!�@ͮ@͈f@��@̉�@�@˅@ʾ@�p;@�z�@�e�@�x@ɹ�@�j�@��@Ȱ!@�@��a@��f@�h�@�#:@Ŏ"@�E9@�(@�D�@��N@ñ[@Ê�@�U�@� \@��s@´9@�_@��+@��n@��@�xl@�Ĝ@��@�Ĝ@��N@�4@�֡@�  @��@��{@�J�@� i@��c@�C-@���@���@�q�@�$�@�G�@���@���@�m�@� �@���@�j@�_p@�zx@�S�@��!@���@��y@�:�@�D�@�I�@�ƨ@�l"@���@�ϫ@��o@�;@���@�N�@�G�@�Z@��@�e�@�S�@���@�i�@� �@��d@���@�Mj@�*0@��	@��<@�s�@�-@���@��@���@�n/@�$t@���@���@�w�@�Ft@��
@��:@�H�@��@���@���@�YK@�|�@�!�@��<@�i�@�R�@�{@��@��C@�N<@��|@��@�bN@��@�@���@��C@�|�@�-w@�C@��@�(�@��@�V@���@��W@�Z@�Z@�N�@�D�@���@�RT@��@�bN@�|@���@��@��@�O@�I�@��}@��[@���@���@�~@��=@��@�K^@��A@���@�Z�@���@���@�j@��@���@���@���@�ϫ@��:@��@���@���@�[�@�%�@�
�@��@��N@���@�c�@�F�@�0�@��@�u�@�L0@�7@���@�?}@��@�҉@�q�@��g@��@�a�@�0�@��`@���@�>B@���@���@�hs@�C@���@�7�@��@��V@�N<@���@���@�~�@�1@�˒@��t@���@�33@��@���@��v@�ȴ@���@���@�a|@�PH@�7�@���@��a@���@�v`@�Y�@�+�@��@���@���@�r�@�&�@���@��=@�t�@�o@��F@�?�@�	@���@��@���@�@O@���@�a|@�8�@��]@���@���@���@��~@�qv@�O@�7L@�V@�֡@�tT@�S�@�R�@�$�@��j@���@�G�@��@���@��@���@�H@�;@iD@�@~YK@~
�@}��@}+�@|�z@{�&@{��@{F�@z��@z�F@z\�@y��@x��@x:�@w�
@wiD@v�2@vff@v+k@u�j@u�@uDg@t��@t]d@t�@s��@r�s@q��@pɆ@pb@o�@o~�@n�]@n҉@n��@nd�@nGE@nJ@mIR@l��@lj@k�@k|�@k8@j�H@j�<@j��@j�@j�X@j��@j��@j=q@i�M@i�@h��@hoi@hQ�@h7@g��@gn/@f��@f�b@f�b@f@�@fu@e��@e<6@eV@d�@d�9@cݘ@c��@c�:@cg�@b�@bJ�@bu@a�t@`�@`�.@` �@_�f@_�@^�@^M�@^u@]ϫ@]��@]w2@]5�@\�5@\��@\C-@[�$@[K�@Z�@Z\�@Z �@Y�^@Y#�@X�@X>B@X�@WU�@Vں@V��@VH�@U�@Us�@U�@Tu�@S�&@SZ�@S�@R��@R�x@R?@Q��@Q��@QrG@Q!�@P�@P�_@P �@O�@N�@N��@NJ�@M��@Mp�@M \@L��@L�4@K��@Kg�@K;d@J�"@J��@J{@I�N@I�X@I-w@Hѷ@H�Y@H<�@G�
@Gv`@G,�@FM�@E��@Eu�@E5�@D�@D�D@DM@C�0@CC�@C�@B�<@BW�@B{@A�j@Ax�@A�@@ѷ@@�u@@U2@@�@?��@?�w@?��@?J#@? i@>�m@>�!@>-@=��@<�|@<�e@<Xy@<>B@;�A@;�@;J#@;33@; i@:��@:\�@:�@9ԕ@9�C@9�h@9��@9L�@8�|@8�j@8c�@7�@7n/@7J#@7.I@6͟@6n�@6@�@6�@5�@5u�@4ی@4��@4��@4w�@4$@3��@3Mj@2�@2�'@2�F@2&�@1��@1�'@1s�@12a@0��@0U2@0�@/�@/�A@/��@/�@/�&@/�
@/�6@/��@/��@/n/@/)_@/�@.��@.��@.GE@.4@-�@-��@-Vm@,�f@,�O@,��@,A�@+�@+�@+��@+�:@+P�@+�@*��@*�A@*?@*�@*	@)�@)�"@)G�@)�@(�@(|�@(Ft@(M@'خ@'��@'6z@'�@' i@&ں@&��@&?@%��@%�@%|@%N<@$�	@$�@$<�@$!@#�W@#x@#l�@#X�@"�R@"p;@"R�@!�d@!��@!��@!J�@ �|@ �o@ 7@ �@��@�m@�*@~�@e�@+@�@@ں@��@;�@�@�T@��@\�@#�@@@@��@�@�@��@��@_@@�Q@��@S�@�@�@\�@��@��@Y�@/@�	@��@�4@�Y@V�@�@�6@�@@X�@+@ߤ@��@��@��@��@�1@Z�@e@�C@u�@e,@�@Ĝ@�@y>@h�@]d@Q�@C-@'R@7@�@�@��@� @�@�@@�{@o�@.I@�@��@�}@i�@{@��@�~@o @O�@IR@0�@0�@/@ \@�@;@�|@�p@~(@M@1'@*�@�@ƨ@F�@�@��@�@�m@��@}V@^5@8�@!�@��@��@hs@(�@��@�@�/@�E@��@y>@`�@9X@$@	�@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�xA��A�A��A�A��A��A��A��A��A��A��A�xA�CA��A�IA��A�!A��A��A� �A�"4A�#nA�$@A�%FA�#�A�#�A�%FA�&�A�$A�MA�DA�	7A�fA�A���A̔FA�e`A�9XA��AˬA�rGA�Q�A�rGA�"A�#:A���A���A�jKA��A�aA�!-A��vA�W�A�ncA�xlA�`�A�GzA���A��A���A��A�p;A�YA���A��A�~�A��A���A��EA�A�A��0A�֡A��A�~�A��A���A�l"A�~A�w�A�49A��2A�_A�}�A�Y�A�̘A��A���A���A�a�A���A��A���A��A���A�kQA��"A�v`A�0�A~�A{��Av�fAt	An��Aj\�Ah�Ae��Ab�yA^?}AY��AW�AT�'AQL0AP�rAO�6AN�AL�AI�nAG�WAD��AB��A@�fA?S�A=�!A<X�A:�9A:Y�A8�hA6��A5t�A3�A2^�A1�bA1&A0N�A0��A/�ZA.��A+�=A*�\A*�A(�#A'l�A&ݘA&4A%P�A$4A#c A"�A"($A!��A ��A tTAl�A��A�+A6�A�A�5A�@A��Au�A�Ac�AE�A<�A�A\�A�FA7LA�HA�$A|�AMjA$�AE�A�A��A:�A��A�AOA~(A�'AA�AK^A/Aw2AMA� A�A_A_�A
�EA	��A	-A�A��A\�A+A�]A��Al�A�A�Aq�AA��AI�A!A!-A��A��Aq�A%�AoAL�A	A��A[WA�A p;A _pA �VA X�@�2a@�"�@��	@�xl@��Z@���@�x@�YK@�خ@��F@���@�h
@��{@�K�@��@��1@���@���@�@�@���@��@�˒@�P�@��@��@�H@�~�@��m@�#:@빌@�@�}�@��@颜@�!@�@�.@�|�@��+@��@���@�s�@�-�@�@��#@�6z@��&@�0@�f@�c�@��@�s�@���@�)�@�Dg@ރ@ݤ@@��?@�o @��@ڃ�@��@���@��6@�Z�@�-w@��|@ֲ�@�($@�;@�0U@�%�@�G@ӌ~@��@�]d@�dZ@�@�d�@�n/@���@�!�@ͮ@͈f@��@̉�@�@˅@ʾ@�p;@�z�@�e�@�x@ɹ�@�j�@��@Ȱ!@�@��a@��f@�h�@�#:@Ŏ"@�E9@�(@�D�@��N@ñ[@Ê�@�U�@� \@��s@´9@�_@��+@��n@��@�xl@�Ĝ@��@�Ĝ@��N@�4@�֡@�  @��@��{@�J�@� i@��c@�C-@���@���@�q�@�$�@�G�@���@���@�m�@� �@���@�j@�_p@�zx@�S�@��!@���@��y@�:�@�D�@�I�@�ƨ@�l"@���@�ϫ@��o@�;@���@�N�@�G�@�Z@��@�e�@�S�@���@�i�@� �@��d@���@�Mj@�*0@��	@��<@�s�@�-@���@��@���@�n/@�$t@���@���@�w�@�Ft@��
@��:@�H�@��@���@���@�YK@�|�@�!�@��<@�i�@�R�@�{@��@��C@�N<@��|@��@�bN@��@�@���@��C@�|�@�-w@�C@��@�(�@��@�V@���@��W@�Z@�Z@�N�@�D�@���@�RT@��@�bN@�|@���@��@��@�O@�I�@��}@��[@���@���@�~@��=@��@�K^@��A@���@�Z�@���@���@�j@��@���@���@���@�ϫ@��:@��@���@���@�[�@�%�@�
�@��@��N@���@�c�@�F�@�0�@��@�u�@�L0@�7@���@�?}@��@�҉@�q�@��g@��@�a�@�0�@��`@���@�>B@���@���@�hs@�C@���@�7�@��@��V@�N<@���@���@�~�@�1@�˒@��t@���@�33@��@���@��v@�ȴ@���@���@�a|@�PH@�7�@���@��a@���@�v`@�Y�@�+�@��@���@���@�r�@�&�@���@��=@�t�@�o@��F@�?�@�	@���@��@���@�@O@���@�a|@�8�@��]@���@���@���@��~@�qv@�O@�7L@�V@�֡@�tT@�S�@�R�@�$�@��j@���@�G�@��@���@��@���@�H@�;@iD@�@~YK@~
�@}��@}+�@|�z@{�&@{��@{F�@z��@z�F@z\�@y��@x��@x:�@w�
@wiD@v�2@vff@v+k@u�j@u�@uDg@t��@t]d@t�@s��@r�s@q��@pɆ@pb@o�@o~�@n�]@n҉@n��@nd�@nGE@nJ@mIR@l��@lj@k�@k|�@k8@j�H@j�<@j��@j�@j�X@j��@j��@j=q@i�M@i�@h��@hoi@hQ�@h7@g��@gn/@f��@f�b@f�b@f@�@fu@e��@e<6@eV@d�@d�9@cݘ@c��@c�:@cg�@b�@bJ�@bu@a�t@`�@`�.@` �@_�f@_�@^�@^M�@^u@]ϫ@]��@]w2@]5�@\�5@\��@\C-@[�$@[K�@Z�@Z\�@Z �@Y�^@Y#�@X�@X>B@X�@WU�@Vں@V��@VH�@U�@Us�@U�@Tu�@S�&@SZ�@S�@R��@R�x@R?@Q��@Q��@QrG@Q!�@P�@P�_@P �@O�@N�@N��@NJ�@M��@Mp�@M \@L��@L�4@K��@Kg�@K;d@J�"@J��@J{@I�N@I�X@I-w@Hѷ@H�Y@H<�@G�
@Gv`@G,�@FM�@E��@Eu�@E5�@D�@D�D@DM@C�0@CC�@C�@B�<@BW�@B{@A�j@Ax�@A�@@ѷ@@�u@@U2@@�@?��@?�w@?��@?J#@? i@>�m@>�!@>-@=��@<�|@<�e@<Xy@<>B@;�A@;�@;J#@;33@; i@:��@:\�@:�@9ԕ@9�C@9�h@9��@9L�@8�|@8�j@8c�@7�@7n/@7J#@7.I@6͟@6n�@6@�@6�@5�@5u�@4ی@4��@4��@4w�@4$@3��@3Mj@2�@2�'@2�F@2&�@1��@1�'@1s�@12a@0��@0U2@0�@/�@/�A@/��@/�@/�&@/�
@/�6@/��@/��@/n/@/)_@/�@.��@.��@.GE@.4@-�@-��@-Vm@,�f@,�O@,��@,A�@+�@+�@+��@+�:@+P�@+�@*��@*�A@*?@*�@*	@)�@)�"@)G�@)�@(�@(|�@(Ft@(M@'خ@'��@'6z@'�@' i@&ں@&��@&?@%��@%�@%|@%N<@$�	@$�@$<�@$!@#�W@#x@#l�@#X�@"�R@"p;@"R�@!�d@!��@!��@!J�@ �|@ �o@ 7@ �@��@�m@�*@~�@e�@+@�@@ں@��@;�@�@�T@��@\�@#�@@@@��@�@�@��@��@_@@�Q@��@S�@�@�@\�@��@��@Y�@/@�	@��@�4@�Y@V�@�@�6@�@@X�@+@ߤ@��@��@��@��@�1@Z�@e@�C@u�@e,@�@Ĝ@�@y>@h�@]d@Q�@C-@'R@7@�@�@��@� @�@�@@�{@o�@.I@�@��@�}@i�@{@��@�~@o @O�@IR@0�@0�@/@ \@�@;@�|@�p@~(@M@1'@*�@�@ƨ@F�@�@��@�@�m@��@}V@^5@8�@!�@��@��@hs@(�@��@�@�/@�E@��@y>@`�@9X@$@	�@�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
^�B
^�B
^5B
^B
^5B
^5B
^B
^B
^B
^B
^B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_pB
_;B
_B
^�B
^�B
^jB
_;B
`vB
`�B
`�B
`�B
aB
a|B
aHB
aB
aB
`�B
_pB
[=B
W�B
TaB
N�B
KxB
AUB
=B
UB
k�B
vzB
v�B
�.B
�,B
��B
�B
��B
��B
��B
�6B
��B�B(sBU�B}VB�rB�KB�)B��B��B�Bq[Bo�Bj�B�"B�'B��B�hB��B��B�B��BvBi�Bd�BdBbhB_!BBuB/B�B
�]B
յB
� B
�B
�vB
�B
{�B
p�B
]/B
W�B
OB
>]B
,�B
 �B
^B	�qB	�gB	��B	��B	��B	z�B	j�B	NpB	9�B	(
B	�B	�B	�B	
	B	zB	 OB�VB��B��B�ZB�B�%B�XB�$B��B�6B��B��B	-B	�B	aB	(B	MB	�B	E�B	TFB	Z�B	bB	bhB	b4B	k6B	sB	t�B	v�B	|�B	�[B	�zB	�bB	�B	��B	��B	�B	�|B	��B	��B	��B	��B	��B	�vB	��B	��B	�nB	��B	��B	�RB	��B	� B	�uB	�MB	�B	��B	��B	�+B	��B	�B	��B	��B	�?B	��B	�B	��B	�"B	��B	�RB	�_B	�B	�1B	�B	�7B	�6B	ϫB	�B	�=B	��B	�B	��B	�jB	�PB	��B	�BB	��B	�BB	��B	��B	��B	��B	�OB	�B	�aB	�B	ˬB	�PB	�6B	ʦB	�7B	�XB	�NB	�hB	ѝB	��B	�TB	�oB	�_B	��B	�hB	�\B	��B	�B	��B	ޞB	�B	ٚB	�YB	�sB	��B	�[B	��B	��B	�4B	� B	��B	��B	��B	�jB	��B	�6B	�B	��B	��B	ʦB	�=B	��B	�"B	̘B	�B	�dB	�PB	�B	�}B	�(B	͹B	�B	��B	�B	��B	��B	�B	�B	�_B	ؓB	��B	�
B	՛B	�_B	��B	�5B	�B	�]B	�=B	ںB	��B	�B	��B	�eB	�+B	��B	��B	�7B	�_B	��B	�eB	�eB	�eB	�eB	�B	�KB	�B	�B	�CB	�CB	�]B	�)B	��B	��B	�]B	��B	�xB	��B	��B	��B	��B	�B	�IB	��B	�B	�|B	��B	�B	�B	�B	�B	��B	��B	�sB	�kB	�kB	�QB	�B	�B	�=B	��B	�]B	�]B	��B	�B	�B	��B	��B	�aB	��B	�B	�MB	�-B	�vB	�]B	�B	��B	�}B	�wB	�B	�B	�eB	�B	�eB	�B	�B	�=B	�B	�B	�_B	��B	��B	�DB	�yB	�B	�B	��B	�}B	�vB	�B	�LB	�FB	��B	�"B	�B	�BB	�]B	��B	��B	�XB	�*B	��B	��B	�B	�fB	�+B	��B	�MB	�B	�B	��B	�aB	�B	�B	�9B	�9B	�B	�B	�tB	�B	�FB	�B	�2B	�LB	��B	��B	��B	��B	��B	��B	�>B	�XB	��B	��B	��B	�B	��B	�DB	��B	��B	�B	�B	��B	��B	�<B	��B	��B	��B	��B	�}B
  B	��B
  B
 �B
 �B
 �B	��B	�]B	�B	�dB	��B	��B
gB
�B
B
�B
�B
�B
B
�B
UB	��B	�<B	�"B	��B	�B	��B	��B	�B	�dB	�BB
^B
�B
	lB
�B
�B
�B
�B
B
�B
�B
�B
MB
?B
$B
�B
�B
�B
�B
�B
B
�B
1B
�B
�B
B
B
�B
kB
�B
xB
B
�B
OB
�B
B
;B
VB
�B
�B
�B
�B
 B
 �B
 �B
!B
!HB
!HB
!�B
"�B
"�B
#nB
#B
#�B
$B
#�B
$�B
$�B
$�B
$�B
$�B
%B
%�B
%�B
&B
&2B
&fB
&fB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
(
B
(>B
($B
(�B
)_B
)B
)*B
)DB
)�B
)�B
)_B
)�B
*eB
+B
+B
+B
+6B
+B
+�B
,qB
,qB
,�B
-)B
-CB
-wB
./B
.B
-�B
./B
.�B
.�B
/�B
/�B
0!B
/�B
0�B
0oB
0�B
0!B
0UB
1�B
1vB
2-B
2�B
2�B
2|B
3B
49B
4B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5tB
5tB
5�B
6B
5�B
6+B
6B
6FB
6`B
6FB
6+B
5tB
5%B
5%B
5?B
7B
6�B
6�B
7LB
8B
8�B
:DB
;B
:DB
9�B
:�B
:�B
:�B
:�B
<B
<B
=�B
>wB
>�B
>(B
>�B
?B
?HB
?}B
@B
@4B
@OB
@�B
@�B
A�B
B[B
A�B
B'B
B'B
B�B
B�B
B�B
B�B
B�B
C�B
DB
C�B
DB
D�B
D�B
D�B
EB
FB
E�B
E�B
F?B
F�B
F�B
GEB
G_B
G_B
GEB
G_B
G_B
G�B
G�B
G�B
G�B
G�B
HB
H�B
I7B
I�B
JXB
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L~B
LJB
L�B
MB
MPB
M�B
M�B
M�B
NB
N<B
NpB
NB
N�B
N"B
NpB
N�B
OB
OvB
O\B
O�B
O�B
O�B
P�B
PbB
P�B
QhB
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S&B
S�B
SuB
S�B
S�B
TFB
T�B
UMB
U�B
U�B
VB
VB
VSB
VmB
V�B
W
B
W?B
WsB
W�B
WsB
W�B
X_B
X+B
X�B
X_B
X�B
YB
Y1B
YKB
YKB
Y�B
Y�B
Y�B
Z7B
Z�B
[�B
[WB
[�B
[�B
\CB
\]B
\�B
\�B
\xB
]/B
]/B
]dB
]~B
]�B
]�B
]�B
]�B
^5B
^jB
^�B
^�B
_pB
_pB
_�B
_�B
`B
`B
`BB
`\B
`�B
abB
aHB
abB
abB
a�B
a�B
bhB
b�B
b�B
b�B
c B
cnB
cnB
c�B
c�B
c�B
dZB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
ezB
ezB
e�B
e�B
fB
fB
f2B
fLB
ffB
f�B
f�B
f�B
gB
gRB
gmB
gmB
g�B
g�B
h
B
hXB
hsB
h�B
h�B
h�B
h�B
iB
i*B
iyB
i�B
i�B
i�B
jKB
jeB
jB
kB
kB
kB
k6B
kkB
k�B
k�B
l=B
l"B
l=B
l�B
l�B
mwB
mB
mwB
m�B
m�B
m�B
n�B
n�B
n�B
o�B
oB
oOB
o�B
o�B
p!B
poB
p�B
poB
poB
p�B
p�B
p�B
qAB
qAB
q'B
qvB
q�B
rB
q�B
rGB
rGB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
sMB
s�B
s�B
s�B
tB
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v+B
v`B
v`B
v�B
v�B
v�B
v�B
w2B
w�B
w�B
w�B
xB
xB
xB
xB
w�B
xRB
xlB
y	B
x�B
y	B
y�B
y�B
y�B
y�B
zB
zB
zB
zDB
zDB
zDB
z^B
z^B
zDB
z�B
zxB
z�B
z�B
z�B
z�B
{0B
{B
{JB
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}B
}�B
}�B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
HB
cB
HB
�B
�B
�OB
�4B
�iB
��B
��B
��B
��B
��B
� B
� B
�;B
�oB
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
^�B
^�B
^5B
^B
^5B
^5B
^B
^B
^B
^B
^B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_pB
_;B
_B
^�B
^�B
^jB
_;B
`vB
`�B
`�B
`�B
aB
a|B
aHB
aB
aB
`�B
_pB
[=B
W�B
TaB
N�B
KxB
AUB
=B
UB
k�B
vzB
v�B
�.B
�,B
��B
�B
��B
��B
��B
�6B
��B�B(sBU�B}VB�rB�KB�)B��B��B�Bq[Bo�Bj�B�"B�'B��B�hB��B��B�B��BvBi�Bd�BdBbhB_!BBuB/B�B
�]B
յB
� B
�B
�vB
�B
{�B
p�B
]/B
W�B
OB
>]B
,�B
 �B
^B	�qB	�gB	��B	��B	��B	z�B	j�B	NpB	9�B	(
B	�B	�B	�B	
	B	zB	 OB�VB��B��B�ZB�B�%B�XB�$B��B�6B��B��B	-B	�B	aB	(B	MB	�B	E�B	TFB	Z�B	bB	bhB	b4B	k6B	sB	t�B	v�B	|�B	�[B	�zB	�bB	�B	��B	��B	�B	�|B	��B	��B	��B	��B	��B	�vB	��B	��B	�nB	��B	��B	�RB	��B	� B	�uB	�MB	�B	��B	��B	�+B	��B	�B	��B	��B	�?B	��B	�B	��B	�"B	��B	�RB	�_B	�B	�1B	�B	�7B	�6B	ϫB	�B	�=B	��B	�B	��B	�jB	�PB	��B	�BB	��B	�BB	��B	��B	��B	��B	�OB	�B	�aB	�B	ˬB	�PB	�6B	ʦB	�7B	�XB	�NB	�hB	ѝB	��B	�TB	�oB	�_B	��B	�hB	�\B	��B	�B	��B	ޞB	�B	ٚB	�YB	�sB	��B	�[B	��B	��B	�4B	� B	��B	��B	��B	�jB	��B	�6B	�B	��B	��B	ʦB	�=B	��B	�"B	̘B	�B	�dB	�PB	�B	�}B	�(B	͹B	�B	��B	�B	��B	��B	�B	�B	�_B	ؓB	��B	�
B	՛B	�_B	��B	�5B	�B	�]B	�=B	ںB	��B	�B	��B	�eB	�+B	��B	��B	�7B	�_B	��B	�eB	�eB	�eB	�eB	�B	�KB	�B	�B	�CB	�CB	�]B	�)B	��B	��B	�]B	��B	�xB	��B	��B	��B	��B	�B	�IB	��B	�B	�|B	��B	�B	�B	�B	�B	��B	��B	�sB	�kB	�kB	�QB	�B	�B	�=B	��B	�]B	�]B	��B	�B	�B	��B	��B	�aB	��B	�B	�MB	�-B	�vB	�]B	�B	��B	�}B	�wB	�B	�B	�eB	�B	�eB	�B	�B	�=B	�B	�B	�_B	��B	��B	�DB	�yB	�B	�B	��B	�}B	�vB	�B	�LB	�FB	��B	�"B	�B	�BB	�]B	��B	��B	�XB	�*B	��B	��B	�B	�fB	�+B	��B	�MB	�B	�B	��B	�aB	�B	�B	�9B	�9B	�B	�B	�tB	�B	�FB	�B	�2B	�LB	��B	��B	��B	��B	��B	��B	�>B	�XB	��B	��B	��B	�B	��B	�DB	��B	��B	�B	�B	��B	��B	�<B	��B	��B	��B	��B	�}B
  B	��B
  B
 �B
 �B
 �B	��B	�]B	�B	�dB	��B	��B
gB
�B
B
�B
�B
�B
B
�B
UB	��B	�<B	�"B	��B	�B	��B	��B	�B	�dB	�BB
^B
�B
	lB
�B
�B
�B
�B
B
�B
�B
�B
MB
?B
$B
�B
�B
�B
�B
�B
B
�B
1B
�B
�B
B
B
�B
kB
�B
xB
B
�B
OB
�B
B
;B
VB
�B
�B
�B
�B
 B
 �B
 �B
!B
!HB
!HB
!�B
"�B
"�B
#nB
#B
#�B
$B
#�B
$�B
$�B
$�B
$�B
$�B
%B
%�B
%�B
&B
&2B
&fB
&fB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
(
B
(>B
($B
(�B
)_B
)B
)*B
)DB
)�B
)�B
)_B
)�B
*eB
+B
+B
+B
+6B
+B
+�B
,qB
,qB
,�B
-)B
-CB
-wB
./B
.B
-�B
./B
.�B
.�B
/�B
/�B
0!B
/�B
0�B
0oB
0�B
0!B
0UB
1�B
1vB
2-B
2�B
2�B
2|B
3B
49B
4B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5tB
5tB
5�B
6B
5�B
6+B
6B
6FB
6`B
6FB
6+B
5tB
5%B
5%B
5?B
7B
6�B
6�B
7LB
8B
8�B
:DB
;B
:DB
9�B
:�B
:�B
:�B
:�B
<B
<B
=�B
>wB
>�B
>(B
>�B
?B
?HB
?}B
@B
@4B
@OB
@�B
@�B
A�B
B[B
A�B
B'B
B'B
B�B
B�B
B�B
B�B
B�B
C�B
DB
C�B
DB
D�B
D�B
D�B
EB
FB
E�B
E�B
F?B
F�B
F�B
GEB
G_B
G_B
GEB
G_B
G_B
G�B
G�B
G�B
G�B
G�B
HB
H�B
I7B
I�B
JXB
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L~B
LJB
L�B
MB
MPB
M�B
M�B
M�B
NB
N<B
NpB
NB
N�B
N"B
NpB
N�B
OB
OvB
O\B
O�B
O�B
O�B
P�B
PbB
P�B
QhB
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S&B
S�B
SuB
S�B
S�B
TFB
T�B
UMB
U�B
U�B
VB
VB
VSB
VmB
V�B
W
B
W?B
WsB
W�B
WsB
W�B
X_B
X+B
X�B
X_B
X�B
YB
Y1B
YKB
YKB
Y�B
Y�B
Y�B
Z7B
Z�B
[�B
[WB
[�B
[�B
\CB
\]B
\�B
\�B
\xB
]/B
]/B
]dB
]~B
]�B
]�B
]�B
]�B
^5B
^jB
^�B
^�B
_pB
_pB
_�B
_�B
`B
`B
`BB
`\B
`�B
abB
aHB
abB
abB
a�B
a�B
bhB
b�B
b�B
b�B
c B
cnB
cnB
c�B
c�B
c�B
dZB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
ezB
ezB
e�B
e�B
fB
fB
f2B
fLB
ffB
f�B
f�B
f�B
gB
gRB
gmB
gmB
g�B
g�B
h
B
hXB
hsB
h�B
h�B
h�B
h�B
iB
i*B
iyB
i�B
i�B
i�B
jKB
jeB
jB
kB
kB
kB
k6B
kkB
k�B
k�B
l=B
l"B
l=B
l�B
l�B
mwB
mB
mwB
m�B
m�B
m�B
n�B
n�B
n�B
o�B
oB
oOB
o�B
o�B
p!B
poB
p�B
poB
poB
p�B
p�B
p�B
qAB
qAB
q'B
qvB
q�B
rB
q�B
rGB
rGB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
sMB
s�B
s�B
s�B
tB
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v+B
v`B
v`B
v�B
v�B
v�B
v�B
w2B
w�B
w�B
w�B
xB
xB
xB
xB
w�B
xRB
xlB
y	B
x�B
y	B
y�B
y�B
y�B
y�B
zB
zB
zB
zDB
zDB
zDB
z^B
z^B
zDB
z�B
zxB
z�B
z�B
z�B
z�B
{0B
{B
{JB
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}B
}�B
}�B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
HB
cB
HB
�B
�B
�OB
�4B
�iB
��B
��B
��B
��B
��B
� B
� B
�;B
�oB
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104919  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173812  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173812  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173812                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023819  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023819  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                