CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:42:51Z creation;2022-06-04T17:42:52Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604174251  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               pA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ٽ�w`1   @ٽ�r�J�@.���"���cSC��%1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Br  Bw33B��B�ffB�  B���B���B�33B�  B�  B�  B�33B�33B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  CL�C��C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D߼�D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @1G�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBq�HBw{Bz�B�W
B��B��>B��qB�#�B��B��B��B�#�B�#�B��B��B��B�#�B��qB��qB��B��B��B��B��B��B��B�#�B�#�B��B��B��B��B��B��B��C޸C�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RCEC�C�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3޸C5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC\�C^�C_�RCa�RCc�RCe�RCh�Ci�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D��D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'�zD(zD(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR��DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Du~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D���D��
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
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D߻�D��
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
D�up1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`A�>wA���AͥFA�K)A��A̩*Ȃ�A̋DA�{JA�l�A�W�A�D3A�6�A� �A�4A�;A��TA���A˽�A��aA˩*A˙�A˖Aˎ�AˋxA�z�A�K)A�_Aʻ�A�c�A��A�уA�MA�LdA��A�Y�A�:^A���A���A��rA�4�A���A�gmA��GA�͟A���A�4A�Z�A�8A��,A�A�}VA���A�Q�A��jA��fA�XA��A��A���A�ܒA��A�/�A���A���A��?A��A���A�j�A�cTA~��A{�QAy��AxAt��Ar�HArXyAo�Am��Alc AkZAiJ#AcRTAZJ�AU��AT~AQY�AL�AGX�AFE�AD��AD
=ACffABT�AAMjAA'RA@�A:��A8A5��A3�^A3h
A2�jA1�WA/�A/]dA/A/)_A/&�A/�A.g8A-�A+�A*D�A)E�A'S&A&��A(qA*)_A*�A*�IA*�A(��A&��A#p�A"Q�A�AA�A'RAr�Av`AR�A��A�A��A�A�Av�A��AG�A��AA��A>�A��A��A�ZA�uAd�A��A;A+A
��A
�A
 �A	�CA	�A	�A	�"A	�A	~A�$A�+AA�A�XAoiA�A�MA�~A�A��A	S�A��Au�A�A[�A��A��A�4APHA+�A�PA��A� A�A{�AVA�cA��A8�A�.A��A��A�mA�'A��A�AsA�A �I@��@���@��a@��Q@�^�@�x@�J#@��
@�8@�y>@�z@��q@��@���@�^5@��A@�k�@���@��z@�7@��T@���@���@�x@��@�#�@��@��B@��D@�C@�@���@�L@�E�@�q�@@@�'R@�a�@��@�j�@�*�@�hs@�E9@�@O@�=@�4@�)_@�N�@�/@啁@�R@��@�4�@⒣@�:@�)�@�9�@� i@ަL@��@��@ۆ�@�N�@ٌ~@��@�H�@��+@�zx@�<6@��@��@�-@դ@@��H@�/�@Ӑ�@��@�%�@�1@�#�@�e�@�GE@�	@Ϡ�@�X�@�/@��@�S@���@�ߤ@΂A@��@͠�@�Mj@���@��@���@�f�@�@��@ʸR@�r�@�M�@�{@���@ɠ�@�T�@��@��H@�D�@��@ǹ�@�4@Ʋ�@�c�@���@ŋ�@��@Ĵ9@đ�@�q@�ƨ@�F�@¿�@�D�@�
�@��@�6z@��I@��T@���@�U�@��@���@�,�@��@�~(@�K^@��+@��#@��3@�@O@��@� i@�V�@���@�@�oi@� �@��@��@��@��Y@� �@���@�1�@�1�@���@�q@��Z@�_@�M@�x@��-@�l�@�IR@�.I@��@��y@���@�l�@���@��S@��@��m@�;�@��@@�S�@�S@�Ĝ@���@�&�@��K@�[W@�6z@��@�Ĝ@��A@��@���@�Q�@��@���@�)�@���@��[@�}�@�$t@��[@�?@��@�<�@��@��@�˒@�u@���@�F�@��m@��+@�O@��
@�O@��@��@�6@��n@�Dg@�ی@�PH@��@��r@��Q@��F@�B�@�%@���@���@�}V@�a|@�}V@��}@�v�@��@���@�W?@�Z�@�2a@��1@��@�@���@���@���@�~�@�t�@�\�@�;d@���@��!@�
�@��z@�qv@��f@�YK@��A@��@�w2@��@�E�@�-@��@�+�@�/@��"@�@��m@�z�@���@�Z�@��@��F@���@�~�@�q@�n�@�:�@���@�j�@�!�@��K@�ѷ@��L@�c�@�7�@��o@�o�@�:�@�#�@��@�ߤ@�oi@�8�@��@���@�|�@�O@��@��@���@�z@�kQ@��;@�]�@�'�@��'@���@�u�@�E�@�GE@�($@�ϫ@���@�>�@�@�ȴ@��+@�M@��-@���@�`B@�!�@��[@���@�\�@�&�@��@���@�e�@��@��x@�!@� �@��Q@��H@���@�A @��	@���@�h�@��@��@���@��$@�e�@�Q�@�IR@�A @�!-@�ں@�Ĝ@���@�GE@�3�@�*@"�@~�L@~@}�@}zx@}�@|�4@|_@|/�@{��@{dZ@z�"@zں@z�1@z!�@y*0@x�@x�@x�z@xw�@x[�@x	�@w��@w$t@v�]@v��@v1�@v�@u��@u2a@u!�@u�@t�o@t�@s�W@s��@s|�@sU�@r�@r8�@r�@q��@qf�@p�@pXy@p6@o�a@o8@n��@np;@n$�@m��@m��@mc�@m�@l��@l�e@lN�@l�@k��@k\)@j�@j�r@jR�@i��@io @iq@h��@h@g�4@gA�@f�y@f��@e�^@e5�@d��@d�@c��@cO@b�"@b�H@b��@b�F@bW�@b8�@a�)@au�@aq@a@@`��@`��@`e�@`2�@_�@_��@_�P@^�@^}V@^c @^ �@]Vm@\�e@\�@[��@[��@[S�@Z��@Z��@Zl�@Z($@Y�@Y�@X�@X��@X>B@W�}@W�	@WS�@V��@Vu@U�T@U��@UB�@T�/@T�D@T2�@S��@SH�@R��@ROv@Q�3@QN<@Q�@P�)@P,=@O�}@OMj@N�8@N~�@N@M�h@M�@L�@L�e@Lq@L,=@K�
@K�	@K@O@Jxl@J#:@I�D@I�@Iu�@I7L@H�f@H��@H�I@H_@G��@Gg�@F�'@FM�@F@E@Ee,@D�@DV�@D�@C��@Cg�@C1�@B��@B��@A��@A��@Af�@A7L@A \@@�O@@~@?�;@?e�@>�y@>:*@=��@=�@<��@<l"@<2�@;y�@;Mj@; i@:�!@:u%@:a|@:5?@9�Z@9�d@9�=@9F@8��@8��@8V�@8�@7�[@7(@6�]@6��@6W�@6($@6�@5��@5��@5&�@4�5@4Ĝ@4tT@4A�@3��@3C�@2�"@2�h@2E�@1�@1�-@1rG@1A @0�@0�@0��@0|�@01@/�a@/��@/�@.�s@.��@.��@.�A@.l�@.@-�T@-��@-�"@-O�@-+@,��@,�u@,S�@,~@+�W@+�g@+��@+��@+e�@+K�@+&@*�H@*�}@*�@*^5@)��@)hs@)+�@(��@(�v@(�O@(e�@(%�@'�]@'��@'�P@'t�@'�@&�@&�\@&Z�@&3�@&{@%�z@%�t@%��@%\�@$��@$�e@$m�@$[�@$"h@#��@#��@#��@#v`@#O@#;d@#8@#1�@"�"@"��@"��@"?@!��@!��@!�@!�S@!<6@ �	@ �@ �j@ ��@ e�@�;@�:@iD@C�@=@o@�@��@GE@��@u�@p�@e,@��@�_@e�@*�@�
@��@n/@P�@�@�2@�@d�@($@�@�j@�~@Vm@��@�@g8@I�@b@��@�W@˒@��@W?@�@��@@�@5?@��@�=@rG@X@ \@�`@�)@��@Ft@M@��@�k@9�@��@��@\�@	@��@�-@�n@�@Q�@/@�@��@�E@�e@��@S�@*�@M@�m@��@C�@ߤ@�@c @L0@C�@�@u@��@��@��@hs@S&@0�@�P@ی@�e@��@g8@(�@$@M@M@��@�K@��@4�@(@�@
�c@
��@
~�@
V@	ԕ@	�3@	��@	��@	��@	}�@	zx@	4@��@�[@�9@�e@|�@]d@:�@-�@�@�
@�k@A�@�@��@�A@c @C�@�@�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`A�>wA���AͥFA�K)A��A̩*Ȃ�A̋DA�{JA�l�A�W�A�D3A�6�A� �A�4A�;A��TA���A˽�A��aA˩*A˙�A˖Aˎ�AˋxA�z�A�K)A�_Aʻ�A�c�A��A�уA�MA�LdA��A�Y�A�:^A���A���A��rA�4�A���A�gmA��GA�͟A���A�4A�Z�A�8A��,A�A�}VA���A�Q�A��jA��fA�XA��A��A���A�ܒA��A�/�A���A���A��?A��A���A�j�A�cTA~��A{�QAy��AxAt��Ar�HArXyAo�Am��Alc AkZAiJ#AcRTAZJ�AU��AT~AQY�AL�AGX�AFE�AD��AD
=ACffABT�AAMjAA'RA@�A:��A8A5��A3�^A3h
A2�jA1�WA/�A/]dA/A/)_A/&�A/�A.g8A-�A+�A*D�A)E�A'S&A&��A(qA*)_A*�A*�IA*�A(��A&��A#p�A"Q�A�AA�A'RAr�Av`AR�A��A�A��A�A�Av�A��AG�A��AA��A>�A��A��A�ZA�uAd�A��A;A+A
��A
�A
 �A	�CA	�A	�A	�"A	�A	~A�$A�+AA�A�XAoiA�A�MA�~A�A��A	S�A��Au�A�A[�A��A��A�4APHA+�A�PA��A� A�A{�AVA�cA��A8�A�.A��A��A�mA�'A��A�AsA�A �I@��@���@��a@��Q@�^�@�x@�J#@��
@�8@�y>@�z@��q@��@���@�^5@��A@�k�@���@��z@�7@��T@���@���@�x@��@�#�@��@��B@��D@�C@�@���@�L@�E�@�q�@@@�'R@�a�@��@�j�@�*�@�hs@�E9@�@O@�=@�4@�)_@�N�@�/@啁@�R@��@�4�@⒣@�:@�)�@�9�@� i@ަL@��@��@ۆ�@�N�@ٌ~@��@�H�@��+@�zx@�<6@��@��@�-@դ@@��H@�/�@Ӑ�@��@�%�@�1@�#�@�e�@�GE@�	@Ϡ�@�X�@�/@��@�S@���@�ߤ@΂A@��@͠�@�Mj@���@��@���@�f�@�@��@ʸR@�r�@�M�@�{@���@ɠ�@�T�@��@��H@�D�@��@ǹ�@�4@Ʋ�@�c�@���@ŋ�@��@Ĵ9@đ�@�q@�ƨ@�F�@¿�@�D�@�
�@��@�6z@��I@��T@���@�U�@��@���@�,�@��@�~(@�K^@��+@��#@��3@�@O@��@� i@�V�@���@�@�oi@� �@��@��@��@��Y@� �@���@�1�@�1�@���@�q@��Z@�_@�M@�x@��-@�l�@�IR@�.I@��@��y@���@�l�@���@��S@��@��m@�;�@��@@�S�@�S@�Ĝ@���@�&�@��K@�[W@�6z@��@�Ĝ@��A@��@���@�Q�@��@���@�)�@���@��[@�}�@�$t@��[@�?@��@�<�@��@��@�˒@�u@���@�F�@��m@��+@�O@��
@�O@��@��@�6@��n@�Dg@�ی@�PH@��@��r@��Q@��F@�B�@�%@���@���@�}V@�a|@�}V@��}@�v�@��@���@�W?@�Z�@�2a@��1@��@�@���@���@���@�~�@�t�@�\�@�;d@���@��!@�
�@��z@�qv@��f@�YK@��A@��@�w2@��@�E�@�-@��@�+�@�/@��"@�@��m@�z�@���@�Z�@��@��F@���@�~�@�q@�n�@�:�@���@�j�@�!�@��K@�ѷ@��L@�c�@�7�@��o@�o�@�:�@�#�@��@�ߤ@�oi@�8�@��@���@�|�@�O@��@��@���@�z@�kQ@��;@�]�@�'�@��'@���@�u�@�E�@�GE@�($@�ϫ@���@�>�@�@�ȴ@��+@�M@��-@���@�`B@�!�@��[@���@�\�@�&�@��@���@�e�@��@��x@�!@� �@��Q@��H@���@�A @��	@���@�h�@��@��@���@��$@�e�@�Q�@�IR@�A @�!-@�ں@�Ĝ@���@�GE@�3�@�*@"�@~�L@~@}�@}zx@}�@|�4@|_@|/�@{��@{dZ@z�"@zں@z�1@z!�@y*0@x�@x�@x�z@xw�@x[�@x	�@w��@w$t@v�]@v��@v1�@v�@u��@u2a@u!�@u�@t�o@t�@s�W@s��@s|�@sU�@r�@r8�@r�@q��@qf�@p�@pXy@p6@o�a@o8@n��@np;@n$�@m��@m��@mc�@m�@l��@l�e@lN�@l�@k��@k\)@j�@j�r@jR�@i��@io @iq@h��@h@g�4@gA�@f�y@f��@e�^@e5�@d��@d�@c��@cO@b�"@b�H@b��@b�F@bW�@b8�@a�)@au�@aq@a@@`��@`��@`e�@`2�@_�@_��@_�P@^�@^}V@^c @^ �@]Vm@\�e@\�@[��@[��@[S�@Z��@Z��@Zl�@Z($@Y�@Y�@X�@X��@X>B@W�}@W�	@WS�@V��@Vu@U�T@U��@UB�@T�/@T�D@T2�@S��@SH�@R��@ROv@Q�3@QN<@Q�@P�)@P,=@O�}@OMj@N�8@N~�@N@M�h@M�@L�@L�e@Lq@L,=@K�
@K�	@K@O@Jxl@J#:@I�D@I�@Iu�@I7L@H�f@H��@H�I@H_@G��@Gg�@F�'@FM�@F@E@Ee,@D�@DV�@D�@C��@Cg�@C1�@B��@B��@A��@A��@Af�@A7L@A \@@�O@@~@?�;@?e�@>�y@>:*@=��@=�@<��@<l"@<2�@;y�@;Mj@; i@:�!@:u%@:a|@:5?@9�Z@9�d@9�=@9F@8��@8��@8V�@8�@7�[@7(@6�]@6��@6W�@6($@6�@5��@5��@5&�@4�5@4Ĝ@4tT@4A�@3��@3C�@2�"@2�h@2E�@1�@1�-@1rG@1A @0�@0�@0��@0|�@01@/�a@/��@/�@.�s@.��@.��@.�A@.l�@.@-�T@-��@-�"@-O�@-+@,��@,�u@,S�@,~@+�W@+�g@+��@+��@+e�@+K�@+&@*�H@*�}@*�@*^5@)��@)hs@)+�@(��@(�v@(�O@(e�@(%�@'�]@'��@'�P@'t�@'�@&�@&�\@&Z�@&3�@&{@%�z@%�t@%��@%\�@$��@$�e@$m�@$[�@$"h@#��@#��@#��@#v`@#O@#;d@#8@#1�@"�"@"��@"��@"?@!��@!��@!�@!�S@!<6@ �	@ �@ �j@ ��@ e�@�;@�:@iD@C�@=@o@�@��@GE@��@u�@p�@e,@��@�_@e�@*�@�
@��@n/@P�@�@�2@�@d�@($@�@�j@�~@Vm@��@�@g8@I�@b@��@�W@˒@��@W?@�@��@@�@5?@��@�=@rG@X@ \@�`@�)@��@Ft@M@��@�k@9�@��@��@\�@	@��@�-@�n@�@Q�@/@�@��@�E@�e@��@S�@*�@M@�m@��@C�@ߤ@�@c @L0@C�@�@u@��@��@��@hs@S&@0�@�P@ی@�e@��@g8@(�@$@M@M@��@�K@��@4�@(@�@
�c@
��@
~�@
V@	ԕ@	�3@	��@	��@	��@	}�@	zx@	4@��@�[@�9@�e@|�@]d@:�@-�@�@�
@�k@A�@�@��@�A@c @C�@�@�T1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�8B�mB��B�B�B�B�B�_B�=B�CB�B�B��B��B�AB�B�B�RB��B	UB	 �B	�B	{B	�B	B	B	 vB	2�B	?�B	T�B	i*B	�B	�B
:xB
��B
��B
�8B\�By�B��B�GB�xB��B�@B�xB�uB�oB��B�"B�B�B��B�B�`B��B��B��B��B�BB~(Bh�B:xB
��B
�fB
��B
�gB
xlB
h
B
S�B
 vB	��B	�gB	��B	�DB	��B	��B	�WB	��B	��B	��B	�UB	|PB	wB	l�B	HB	1AB	+�B	B	�B�2B�[B�B��B�B��B�B�B�B�DB�'BޞBؓB�KB�EB�nB��B	MB	�B	B	
B	+�B	1'B	5�B	.B	�B	SB		7B	0B	t�B	�B	��B	��B	��B	�B	�#B	ezB	^�B	I�B	;JB	.�B	 vB	�B	B	"NB	 BB	 �B	($B	0!B	,�B	:�B	B�B	a�B	p�B	z�B	��B	jB	iDB	_pB	\CB	gmB	r�B	oB	J�B	./B	@�B	M�B	\�B	e�B	lWB	s�B	{B	~�B	��B	�1B	��B	�uB	�9B	��B	��B	��B	�*B	��B	�3B	�{B	�aB	�2B	ӏB	�TB	�"B	��B	՛B	�2B	՛B	յB	�B	ٚB	�#B	ߊB	�B	��B	��B	�mB	��B	�2B	خB	�BB	�B	�B	�0B	�yB	��B	�8B	�FB	��B	��B	�8B	�0B	�B	�oB	�B	�B	�B	�}B	�wB	�B	�B	�B	�B	�B	�}B	�B	�aB	�'B	�!B	�B	�B	��B	�*B	�"B	�CB	�B	��B	޸B	��B	�IB	ܬB	�hB	�B	��B	�B	��B	��B	��B	�B	�@B	�&B	�B	�B	�
B	�QB	��B	�eB	�B	�hB	��B	�B	�B	��B	��B	��B	�B	�4B	�4B	�B	�LB	�,B	�FB	�B	��B	�
B	�B	��B	�'B	��B	�;B	�B	�5B	�B	��B	�'B	��B	�B	�B	�B	�vB	��B	�MB	�B	��B	�%B	�ZB	��B	��B	��B	��B	�2B	��B	��B	�2B	��B	�%B	��B	��B	��B	��B	�	B	�rB	�XB	��B	�XB	�XB	�$B	��B	�8B	�8B	��B	�lB	��B	��B	��B	��B	�8B	��B	�$B	��B	�>B	�$B	�$B	�>B	�	B	��B	�>B	�rB	�rB	��B	�rB	�	B	�RB	�	B	�$B	�rB	��B	�B	��B	�*B	�*B	�xB	�DB	��B	�JB	��B	�B	��B	�<B	�(B
 �B
 �B
 OB	�}B	��B
 �B	��B
  B
 �B
oB
�B
�B
UB
oB
;B
 �B
 �B
 B
 �B
�B
�B
[B
'B
�B
{B
{B
aB
aB
GB
�B
�B
aB
-B
-B
�B
[B
[B
�B
�B
 B
�B
B
 OB
 4B	��B
 4B
 �B
UB
�B
�B
�B
�B
MB
tB
�B
%B
�B
�B
�B
B
B
B
YB
?B
YB
�B
B
B
EB
zB
zB
�B
�B
�B
	B
	�B

=B

�B
�B
�B
�B
�B
BB
 B
�B
[B
�B
aB
aB
aB
aB
aB
�B
�B
B
B
�B
MB
�B
�B
9B
�B
�B
oB
�B
,B
aB
�B
�B
�B
�B

B
EB
�B
�B
QB
B
B
�B
eB
�B
�B
�B
jB
jB
!B
�B
�B
�B
�B
�B
 BB
 'B
 'B
!HB
!HB
!HB
!bB
!HB
!�B
!�B
!�B
"�B
"�B
"�B
#:B
#�B
#�B
#�B
#nB
$tB
%FB
%`B
&2B
&LB
&�B
'RB
'8B
($B
(XB
(�B
)yB
)�B
)DB
)_B
*�B
*�B
*�B
+B
+kB
+�B
,B
,B
,�B
,WB
,WB
-)B
-�B
-�B
.}B
.cB
.�B
.�B
.�B
/OB
/�B
/�B
0oB
0�B
0�B
0�B
1AB
1[B
1�B
2|B
2�B
3�B
3�B
4B
4�B
4�B
4�B
5B
5tB
5�B
5�B
5tB
5tB
5�B
6B
6`B
6`B
6�B
6�B
7�B
7�B
7�B
8B
88B
8B
7�B
7�B
7fB
7fB
7�B
8B
8�B
8�B
9	B
9	B
9$B
9XB
9>B
9>B
9$B
9�B
9�B
9�B
:*B
:�B
:xB
:�B
:�B
:�B
;0B
;B
;�B
;�B
;�B
<B
<6B
<jB
<�B
<�B
<�B
="B
=qB
=�B
=qB
=�B
=�B
>(B
>BB
>wB
>�B
>�B
>�B
?.B
?}B
?�B
?�B
@ B
@OB
@OB
@OB
@�B
A�B
BAB
B�B
B�B
B�B
C{B
C{B
C{B
C�B
C{B
C�B
C{B
C�B
DB
DMB
DMB
DMB
D�B
D�B
E�B
F?B
FYB
F?B
F�B
F�B
F�B
G+B
GzB
G_B
G_B
GEB
GEB
G�B
G�B
H1B
H�B
H�B
H�B
IB
IlB
I�B
I�B
J=B
J�B
J�B
KxB
K�B
KDB
K^B
KxB
K�B
K�B
L~B
L�B
L�B
MB
M�B
M�B
N"B
N<B
NpB
N�B
OB
OvB
O�B
O�B
O�B
P}B
P�B
P�B
Q B
QB
Q4B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
SB
S[B
S�B
S�B
S�B
S�B
T,B
T�B
UB
U�B
U�B
U�B
VB
VmB
V�B
V�B
W$B
WsB
W�B
W�B
XEB
X�B
X�B
Y1B
YKB
YKB
Y�B
Z�B
Z�B
Z�B
[	B
[qB
\]B
\�B
\�B
]IB
]~B
^OB
^OB
^�B
^�B
^�B
_B
_!B
_pB
_pB
_VB
^�B
^�B
^�B
_;B
_;B
_�B
_�B
`B
`'B
`�B
`�B
`�B
`�B
abB
a�B
a�B
a�B
b4B
bNB
b�B
cB
c B
c:B
c�B
c�B
c�B
dB
c�B
dZB
dtB
d�B
dtB
d�B
d�B
eB
ezB
e�B
e�B
e�B
e�B
e�B
f2B
f2B
fLB
ffB
f�B
f�B
f�B
gRB
gRB
g�B
g�B
g�B
g�B
h
B
h
B
h$B
h
B
hsB
h�B
h�B
h�B
iDB
i�B
i�B
i�B
jB
j0B
j�B
j�B
j�B
kB
kB
kB
k�B
k�B
k�B
k�B
l"B
l"B
lWB
lqB
l�B
l�B
l�B
mB
m]B
mwB
m�B
m�B
m�B
m�B
n/B
n/B
nIB
nIB
nIB
n}B
n�B
n�B
o B
o5B
oiB
o�B
oiB
o�B
pB
o�B
p!B
p!B
p!B
p�B
p�B
p�B
q'B
qB
q'B
qAB
q[B
q�B
raB
r�B
r�B
r�B
s3B
s3B
s�B
s�B
tB
tB
tnB
tTB
t�B
t�B
uB
uZB
uZB
u%B
uZB
u�B
vB
vzB
v�B
v�B
v�B
wB
w2B
w2B
wLB
wLB
w�B
w�B
xRB
xlB
x�B
x�B
y	B
y$B
y>B
y�B
y�B
y�B
y�B
zDB
z^B
zxB
z�B
{0B
{B
{�B
{�B
|B
|jB
|PB
|jB
|PB
|6B
|B
|B
|6B
|�B
}"B
|�B
|�B
|�B
}"B
}<B
}qB
}�B
~(B
~]B
~wB
~wB
~]B
~�B
~�B
B
HB
}B
cB
�B
�B
�B
� B
�4B
�iB
��B
�B
�B
�B
��B
�B
� B
�oB
��B
�B
�'B
�AB
��B
��B
�B
��B
�aB
�aB
��B
��B
��B
��B
��B
��B
�3B
�MB
��B
��B
��B
�9B
�9B
�B
��B
��B
�tB
��B
��B
�+B
�EB
�EB
�zB
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�8B�mB��B�B�B�B�B�_B�=B�CB�B�B��B��B�AB�B�B�RB��B	UB	 �B	�B	{B	�B	B	B	 vB	2�B	?�B	T�B	i*B	�B	�B
:xB
��B
��B
�8B\�By�B��B�GB�xB��B�@B�xB�uB�oB��B�"B�B�B��B�B�`B��B��B��B��B�BB~(Bh�B:xB
��B
�fB
��B
�gB
xlB
h
B
S�B
 vB	��B	�gB	��B	�DB	��B	��B	�WB	��B	��B	��B	�UB	|PB	wB	l�B	HB	1AB	+�B	B	�B�2B�[B�B��B�B��B�B�B�B�DB�'BޞBؓB�KB�EB�nB��B	MB	�B	B	
B	+�B	1'B	5�B	.B	�B	SB		7B	0B	t�B	�B	��B	��B	��B	�B	�#B	ezB	^�B	I�B	;JB	.�B	 vB	�B	B	"NB	 BB	 �B	($B	0!B	,�B	:�B	B�B	a�B	p�B	z�B	��B	jB	iDB	_pB	\CB	gmB	r�B	oB	J�B	./B	@�B	M�B	\�B	e�B	lWB	s�B	{B	~�B	��B	�1B	��B	�uB	�9B	��B	��B	��B	�*B	��B	�3B	�{B	�aB	�2B	ӏB	�TB	�"B	��B	՛B	�2B	՛B	յB	�B	ٚB	�#B	ߊB	�B	��B	��B	�mB	��B	�2B	خB	�BB	�B	�B	�0B	�yB	��B	�8B	�FB	��B	��B	�8B	�0B	�B	�oB	�B	�B	�B	�}B	�wB	�B	�B	�B	�B	�B	�}B	�B	�aB	�'B	�!B	�B	�B	��B	�*B	�"B	�CB	�B	��B	޸B	��B	�IB	ܬB	�hB	�B	��B	�B	��B	��B	��B	�B	�@B	�&B	�B	�B	�
B	�QB	��B	�eB	�B	�hB	��B	�B	�B	��B	��B	��B	�B	�4B	�4B	�B	�LB	�,B	�FB	�B	��B	�
B	�B	��B	�'B	��B	�;B	�B	�5B	�B	��B	�'B	��B	�B	�B	�B	�vB	��B	�MB	�B	��B	�%B	�ZB	��B	��B	��B	��B	�2B	��B	��B	�2B	��B	�%B	��B	��B	��B	��B	�	B	�rB	�XB	��B	�XB	�XB	�$B	��B	�8B	�8B	��B	�lB	��B	��B	��B	��B	�8B	��B	�$B	��B	�>B	�$B	�$B	�>B	�	B	��B	�>B	�rB	�rB	��B	�rB	�	B	�RB	�	B	�$B	�rB	��B	�B	��B	�*B	�*B	�xB	�DB	��B	�JB	��B	�B	��B	�<B	�(B
 �B
 �B
 OB	�}B	��B
 �B	��B
  B
 �B
oB
�B
�B
UB
oB
;B
 �B
 �B
 B
 �B
�B
�B
[B
'B
�B
{B
{B
aB
aB
GB
�B
�B
aB
-B
-B
�B
[B
[B
�B
�B
 B
�B
B
 OB
 4B	��B
 4B
 �B
UB
�B
�B
�B
�B
MB
tB
�B
%B
�B
�B
�B
B
B
B
YB
?B
YB
�B
B
B
EB
zB
zB
�B
�B
�B
	B
	�B

=B

�B
�B
�B
�B
�B
BB
 B
�B
[B
�B
aB
aB
aB
aB
aB
�B
�B
B
B
�B
MB
�B
�B
9B
�B
�B
oB
�B
,B
aB
�B
�B
�B
�B

B
EB
�B
�B
QB
B
B
�B
eB
�B
�B
�B
jB
jB
!B
�B
�B
�B
�B
�B
 BB
 'B
 'B
!HB
!HB
!HB
!bB
!HB
!�B
!�B
!�B
"�B
"�B
"�B
#:B
#�B
#�B
#�B
#nB
$tB
%FB
%`B
&2B
&LB
&�B
'RB
'8B
($B
(XB
(�B
)yB
)�B
)DB
)_B
*�B
*�B
*�B
+B
+kB
+�B
,B
,B
,�B
,WB
,WB
-)B
-�B
-�B
.}B
.cB
.�B
.�B
.�B
/OB
/�B
/�B
0oB
0�B
0�B
0�B
1AB
1[B
1�B
2|B
2�B
3�B
3�B
4B
4�B
4�B
4�B
5B
5tB
5�B
5�B
5tB
5tB
5�B
6B
6`B
6`B
6�B
6�B
7�B
7�B
7�B
8B
88B
8B
7�B
7�B
7fB
7fB
7�B
8B
8�B
8�B
9	B
9	B
9$B
9XB
9>B
9>B
9$B
9�B
9�B
9�B
:*B
:�B
:xB
:�B
:�B
:�B
;0B
;B
;�B
;�B
;�B
<B
<6B
<jB
<�B
<�B
<�B
="B
=qB
=�B
=qB
=�B
=�B
>(B
>BB
>wB
>�B
>�B
>�B
?.B
?}B
?�B
?�B
@ B
@OB
@OB
@OB
@�B
A�B
BAB
B�B
B�B
B�B
C{B
C{B
C{B
C�B
C{B
C�B
C{B
C�B
DB
DMB
DMB
DMB
D�B
D�B
E�B
F?B
FYB
F?B
F�B
F�B
F�B
G+B
GzB
G_B
G_B
GEB
GEB
G�B
G�B
H1B
H�B
H�B
H�B
IB
IlB
I�B
I�B
J=B
J�B
J�B
KxB
K�B
KDB
K^B
KxB
K�B
K�B
L~B
L�B
L�B
MB
M�B
M�B
N"B
N<B
NpB
N�B
OB
OvB
O�B
O�B
O�B
P}B
P�B
P�B
Q B
QB
Q4B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
SB
S[B
S�B
S�B
S�B
S�B
T,B
T�B
UB
U�B
U�B
U�B
VB
VmB
V�B
V�B
W$B
WsB
W�B
W�B
XEB
X�B
X�B
Y1B
YKB
YKB
Y�B
Z�B
Z�B
Z�B
[	B
[qB
\]B
\�B
\�B
]IB
]~B
^OB
^OB
^�B
^�B
^�B
_B
_!B
_pB
_pB
_VB
^�B
^�B
^�B
_;B
_;B
_�B
_�B
`B
`'B
`�B
`�B
`�B
`�B
abB
a�B
a�B
a�B
b4B
bNB
b�B
cB
c B
c:B
c�B
c�B
c�B
dB
c�B
dZB
dtB
d�B
dtB
d�B
d�B
eB
ezB
e�B
e�B
e�B
e�B
e�B
f2B
f2B
fLB
ffB
f�B
f�B
f�B
gRB
gRB
g�B
g�B
g�B
g�B
h
B
h
B
h$B
h
B
hsB
h�B
h�B
h�B
iDB
i�B
i�B
i�B
jB
j0B
j�B
j�B
j�B
kB
kB
kB
k�B
k�B
k�B
k�B
l"B
l"B
lWB
lqB
l�B
l�B
l�B
mB
m]B
mwB
m�B
m�B
m�B
m�B
n/B
n/B
nIB
nIB
nIB
n}B
n�B
n�B
o B
o5B
oiB
o�B
oiB
o�B
pB
o�B
p!B
p!B
p!B
p�B
p�B
p�B
q'B
qB
q'B
qAB
q[B
q�B
raB
r�B
r�B
r�B
s3B
s3B
s�B
s�B
tB
tB
tnB
tTB
t�B
t�B
uB
uZB
uZB
u%B
uZB
u�B
vB
vzB
v�B
v�B
v�B
wB
w2B
w2B
wLB
wLB
w�B
w�B
xRB
xlB
x�B
x�B
y	B
y$B
y>B
y�B
y�B
y�B
y�B
zDB
z^B
zxB
z�B
{0B
{B
{�B
{�B
|B
|jB
|PB
|jB
|PB
|6B
|B
|B
|6B
|�B
}"B
|�B
|�B
|�B
}"B
}<B
}qB
}�B
~(B
~]B
~wB
~wB
~]B
~�B
~�B
B
HB
}B
cB
�B
�B
�B
� B
�4B
�iB
��B
�B
�B
�B
��B
�B
� B
�oB
��B
�B
�'B
�AB
��B
��B
�B
��B
�aB
�aB
��B
��B
��B
��B
��B
��B
�3B
�MB
��B
��B
��B
�9B
�9B
�B
��B
��B
�tB
��B
��B
�+B
�EB
�EB
�zB
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104930  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174251  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174252  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174252                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024259  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024259  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                