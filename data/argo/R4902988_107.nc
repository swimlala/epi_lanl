CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-09-15T06:47:49Z creation;2022-09-15T06:47:50Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220915064749  20220915070333  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               kA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��Ȱ��B1   @����4@9�hr�!�c��7Kƨ1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A!��A@  A`  A�  A�  A�  A���A���A�  A�  A�  B   B  B  BffB ffB(  B0  B8  B@ffBG��BP  BX  B`ffBhffBp  Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C�fC�fC�fC�fC	�fC  C  C  C�C  C�fC  C�fC  C  C   C"�C$�C&  C(�C*�C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C��3C��3C��3C�  C�  C��C�  C�  C��3C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D�fD  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/�fD0fD0�fD1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DEy�DF  DF�fDG  DG� DG��DH� DIfDI�fDJ  DJ� DKfDK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\y�D]  D]� D]��D^y�D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dg��Dh� Di  Diy�Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDufDu� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{�fD|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D���D�<�D�|�D�� D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D���D�@ DÀ D�� D�  D�@ DĀ D�� D���D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�C3D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D���D�@ DҀ D��3D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ D�|�D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ Dܼ�D�  D�@ D݀ D�� D�  D�<�D�|�D޼�D�  D�@ D߀ D�� D�  D�@ D�� D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D��3D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�3D�@ D� D�� D�  D�<�D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D���D�@ D�vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�
=@�
=A!�A?�A_�A�A�A�A��\A��\A�A�A�A�B�HB�HBG�B G�B'�HB/�HB7�HB@G�BGz�BO�HBW�HB`G�BhG�Bo�HBwz�Bz�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�qB��B��B��C޸C޸C޸C޸C	޸C�RC�RC�RC�C�RC޸C�RC޸C�RC�RC�RC"�C$�C%�RC(�C*�C+�RC-�RC0�C1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCr�Cs�RCu�RCw�RCy�RC{�RC~�C�RC��)C��)C��)C��)C��)C��)C��\C��)C��)C��C��)C��\C��\C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��\C��\C��\C��\C��\C��)C��)C��)C��C��)C��)C��C��C��C��)C��)C��)C��)C��)C��)C��\C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��\C��)C��)C��)C��)C��C��)C��)C��)C��C��C��)C��)C��)C��\C��\C��\C��)C��)C��C��)C��)C��\C��\C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��\C��)C��C��)C��)C��C��)C��)C��)C��)C��)C��)D ~D �D~D�D~DzD~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D�zD�D~D�D�zD�D~D�D~DzD~D�D~D�D~D�D~D�D~D�D~D�Dw�D�D~D�D~DzD~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$��D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-�zD-�D.~D.�D/�zD0zD0�zD0�D1~D2zD2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9w�D9��D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD��DEw�DE�DF�zDF�DG~DG��DH~DIzDI�zDI�DJ~DKzDK~DK�DL~DMzDM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR��DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY��DZ~DZ�D[~D[�D\w�D\�D]~D]��D^w�D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~DezDe~De�Df~Df�Dg~Dg��Dh~Dh�Diw�Di�Dj~Dj��Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt�zDuzDu~Du�Dv~Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~D{zD{�zD{�D|~D|�D}~D}�D~~D~�D~D�D�?
D�
D���D���D�?
D�
D��
D��
D�?
D�
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
D���D���D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�;�D�
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
D���D��
D�?
D�
D��
D��
D�?
D�
D��
D��
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
D���D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
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
D��=D��
D�?
D�
D��
D���D�;�D�{�D��
D�=D�?
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
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
D�
D��
D��
D�?
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
D���D�?
D�
Dÿ
D��
D�?
D�
DĿ
D���D�?
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
D�B=D�
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
D���D�?
D�
D��=D��
D�;�D�
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
D�{�Dٿ
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
Dܻ�D��
D�?
D�
Dݿ
D��
D�;�D�{�D޻�D��
D�?
D�
D߿
D��
D�?
D�
D��D���D�?
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
D�{�D�
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
D�B=D�
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
D�=D�?
D��=D�
D��
D�?
D�=D��=D��
D�?
D�
D�
D�=D�?
D�
D�
D��
D�;�D�
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
D��
D���D�?
D�
D��
D���D�?
D�up111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ZA��A���Aح�A؝A،�A؀ A�uZA�r|A�jA�e`A�`�A�]/A�T�A�D3A�:�A�2-A�(�A�!�A�uA��A׌~A�u�A���A̒�Aɟ�A��A��AŹ�A��MA�MA�WsA���A���A��.A��A�pA�`vA�K^A�B�A�!bA��xA���A��TA���A��<A��A�خA���A��A�$�A��FA�ΥA��A�uA���A��2A���A��CA�*eA��JA�ҽA��vA��7A���A���A�xA��sA�A��UA��KA�qA��A�xA���A���A�ϫA�EA���A�~A���A���A�/�A��EA��$A���A���A��A���A��A��]A��A�e�A��A�E�Ap�A~aA|MA{�jAz?}Ax��Ax%FAv�Av
�At�6As�3Aq�HAo�Anl�Ak�~Ah�Ag>BAeOAc��Ab(�Aau�Aa1'Aa�AaSA`��A_6zA]�KA\��A[ƨAZ֡AX��AV`BAU��AUB[AT�HATW?AOdZAK�	AJ��AIJ�AH_AG��AF�'AF9XAE'�AB�AAAA|�AA  A@MjA?��A>m]A>A=�A=��A=u�A<�A:7LA9@A7�6A7J�A7uA6ɆA6�kA5�PA2�ZA2G�A2�A1n/A0� A.��A-�A,�A+�gA+1�A)ݘA(��A'ϫA&��A%.IA$�A#QA"�A"qA��AaA2aAYA��A�LAa�AA�uAaAIRAA!�A�*A�A�FA"�A��A��A��A5?A!�Ax�Ae�AIRASA͟A�gA�cA�A`BA��A_A	�A�A�AjAIRAA�A�TA��A�oA�CA�Al�AC�A�A��A iA Q�A @�]�@�Z�@���@��@���@�C�@��@�|�@�O@�@���@��@�#�@�ߤ@��@�@�(�@��@�c�@��@��@��@�J�@�b@�8�@�O@�P�@��)@���@�~(@ղ�@Ժ�@���@ЦL@�Dg@�(�@�	l@�S�@�p�@�7�@�Xy@���@ƋD@�b@��d@�	l@ą�@ģ�@�u@��@�}V@�V@��A@��@�e�@��9@�V@�ݘ@�j@��@�@�@���@��f@�j@��@�3�@�}�@��K@��@�c@��@���@�IR@���@�K^@��@��$@��O@��W@�#:@�~(@���@�e,@���@�Q�@��9@�*0@��[@��h@���@�y>@�ff@�(�@�;@�;@��@��:@�Dg@�҉@��@��#@���@�w2@�"�@�ں@�$@��}@��w@���@�[�@�خ@�x�@��@���@���@�� @��A@�h�@�L0@�	@��@���@���@���@�Ta@�4n@�0U@�2�@�1�@�6�@�Ft@�@�@���@�ی@�_@��@���@�w2@�;@�M�@��P@���@���@��@�ѷ@�?�@���@���@��[@��h@�}�@�s@�p�@�e�@�A @�C@��"@���@�@�@���@�v`@���@���@�K^@��@���@���@���@�^�@�<6@���@�}V@�`�@�<�@��r@��T@��=@�w2@�dZ@�[W@�[W@�[W@�Y�@�W?@�S�@�'�@���@�[�@��@��@���@�33@���@��?@���@�� @��A@�g8@���@�S&@��@���@Y@~i�@~0U@}��@}�@}IR@|��@|u�@|c�@{��@{P�@{.I@{@zTa@x��@x��@xe�@x(�@w�@w�F@w��@we�@v��@v�b@v�A@vxl@vTa@vO@u��@u�@t��@t�@sj�@r�'@ri�@r��@s;d@s{J@s��@s|�@r��@q��@qo @q	l@p�D@o�@n�@n1�@n6�@nu%@o@n�2@n�L@nM�@m�j@m��@l�|@l��@l��@k��@k�@j��@j�'@j�1@j5?@j)�@j�@jJ@je@i}�@i@@h�@h��@hj@h(�@g��@g�m@g�V@g+@e�z@eA @dI�@c�*@c6z@b�@b�@b@a�j@aO�@`��@`|�@_�r@_$t@]}�@\�	@\�@\Ĝ@\��@\��@\��@\��@\j@\K^@\:�@\@[b�@Z��@Z�!@Z�x@Z��@Zs�@Zl�@Z^5@ZL0@ZO@Y�)@Y�@Y�n@Y��@YrG@Y\�@Y�@X�.@X�@W��@Wa@W,�@V��@V��@Vں@V�B@V�6@V\�@U�9@U\�@T��@TbN@TM@T$@T�@S�A@S�6@S��@S�V@SZ�@S@R��@Rߤ@R�'@Ru%@R.�@Q�@P�P@P��@Pe�@P@O�Q@O�w@O��@OY@N8�@M�@M�@M�@M��@M�S@Mhs@M%F@L��@L��@L`�@LK^@K�@K�P@K)_@J��@Jxl@J!�@I�@Izx@H��@H�@Gn/@GE9@G)_@F�@E:�@D��@Dr�@D[�@D~@C��@Ct�@CS�@C=@B�"@B��@Bp;@B^5@B.�@A�@A@A��@A[W@A-w@@�@@Q�@?�k@?F�@>��@>l�@>C�@=�@=�7@=q@<�E@<��@<A�@;ݘ@;�	@:�s@:$�@8ѷ@82�@8$@8G@7�{@7;d@7.I@6��@6p;@6_@5��@5e,@52a@5<6@5IR@55�@50�@5 \@5+@5q@5@5!�@5%F@4�`@4��@4�@4��@4S�@3��@3P�@3!-@2�B@2�\@2E�@21�@2�@1��@1��@1�@1IR@0�K@0��@0PH@/��@/>�@/+@/.I@/,�@/�@/ i@.��@.��@.��@.�]@.YK@-c@-Vm@,�@,Q�@+��@+�&@+��@+��@+�;@+�;@+ݘ@+ݘ@+��@+�@+@+@+�@+o@+�@+S@+S@+�@+�@*��@*�'@*q�@*$�@)�.@* �@)��@)��@)}�@)[W@)�@(�`@(�@(�/@(�@(��@(��@(�e@(��@(��@(z�@(6@(	�@'�@'�@@&z@%c@%o @%f�@%e,@%8�@$�j@$��@$/�@#��@#�@#F�@"�]@"�1@"M�@!ϫ@!��@!`B@!@ ��@ �p@ ��@ 	�@��@��@t�@O@�@��@��@��@:*@
�@�D@�@ԕ@��@�X@u�@\�@J�@5�@�j@S�@�@��@�K@��@~�@U�@/�@�@�@��@�}@��@Q@8�@5?@+k@�T@��@|@5�@��@�)@�p@�U@�@�Y@[�@Xy@Q�@<�@*�@@�}@�V@�k@��@��@qv@]�@P�@��@�B@�X@�m@�@�@�}@��@zx@��@�@�e@�z@��@��@�I@�@�Y@�Y@�o@|�@q@j@j@l"@oi@C-@� @e�@(@��@�@��@��@1�@��@c@hs@4@@�P@��@��@�@�o@�@y>@q@2�@�@��@o�@!-@S@�y@�X@ȴ@~�@#:@�@u@��@�o@��@�@�9@�3@�^@��@�n@��@��@Vm@0�@+�@-w@��@��@�[@��@Ɇ@��@�)@�)@Ɇ@��@��@��@��@N�@��@�:@X�@4�@o@
�M@
��@
��@
Ta@	ԕ@	��@	�X@	��@	�7@	X@	@�`@�$@��@�@�u@��@�D@�@�@~(@|�@oi@Q�@K^@9X@$@�@��@��@g�@O@9�@$t@�@�@�b@{�@V@�@�@�@��@�9@�M@J�@(�@�@�|@�/@��@��@�$@�O@�z@�@]d@G@E9@�]@��@��@�@��@�1@q�@($@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ZA��A���Aح�A؝A،�A؀ A�uZA�r|A�jA�e`A�`�A�]/A�T�A�D3A�:�A�2-A�(�A�!�A�uA��A׌~A�u�A���A̒�Aɟ�A��A��AŹ�A��MA�MA�WsA���A���A��.A��A�pA�`vA�K^A�B�A�!bA��xA���A��TA���A��<A��A�خA���A��A�$�A��FA�ΥA��A�uA���A��2A���A��CA�*eA��JA�ҽA��vA��7A���A���A�xA��sA�A��UA��KA�qA��A�xA���A���A�ϫA�EA���A�~A���A���A�/�A��EA��$A���A���A��A���A��A��]A��A�e�A��A�E�Ap�A~aA|MA{�jAz?}Ax��Ax%FAv�Av
�At�6As�3Aq�HAo�Anl�Ak�~Ah�Ag>BAeOAc��Ab(�Aau�Aa1'Aa�AaSA`��A_6zA]�KA\��A[ƨAZ֡AX��AV`BAU��AUB[AT�HATW?AOdZAK�	AJ��AIJ�AH_AG��AF�'AF9XAE'�AB�AAAA|�AA  A@MjA?��A>m]A>A=�A=��A=u�A<�A:7LA9@A7�6A7J�A7uA6ɆA6�kA5�PA2�ZA2G�A2�A1n/A0� A.��A-�A,�A+�gA+1�A)ݘA(��A'ϫA&��A%.IA$�A#QA"�A"qA��AaA2aAYA��A�LAa�AA�uAaAIRAA!�A�*A�A�FA"�A��A��A��A5?A!�Ax�Ae�AIRASA͟A�gA�cA�A`BA��A_A	�A�A�AjAIRAA�A�TA��A�oA�CA�Al�AC�A�A��A iA Q�A @�]�@�Z�@���@��@���@�C�@��@�|�@�O@�@���@��@�#�@�ߤ@��@�@�(�@��@�c�@��@��@��@�J�@�b@�8�@�O@�P�@��)@���@�~(@ղ�@Ժ�@���@ЦL@�Dg@�(�@�	l@�S�@�p�@�7�@�Xy@���@ƋD@�b@��d@�	l@ą�@ģ�@�u@��@�}V@�V@��A@��@�e�@��9@�V@�ݘ@�j@��@�@�@���@��f@�j@��@�3�@�}�@��K@��@�c@��@���@�IR@���@�K^@��@��$@��O@��W@�#:@�~(@���@�e,@���@�Q�@��9@�*0@��[@��h@���@�y>@�ff@�(�@�;@�;@��@��:@�Dg@�҉@��@��#@���@�w2@�"�@�ں@�$@��}@��w@���@�[�@�خ@�x�@��@���@���@�� @��A@�h�@�L0@�	@��@���@���@���@�Ta@�4n@�0U@�2�@�1�@�6�@�Ft@�@�@���@�ی@�_@��@���@�w2@�;@�M�@��P@���@���@��@�ѷ@�?�@���@���@��[@��h@�}�@�s@�p�@�e�@�A @�C@��"@���@�@�@���@�v`@���@���@�K^@��@���@���@���@�^�@�<6@���@�}V@�`�@�<�@��r@��T@��=@�w2@�dZ@�[W@�[W@�[W@�Y�@�W?@�S�@�'�@���@�[�@��@��@���@�33@���@��?@���@�� @��A@�g8@���@�S&@��@���@Y@~i�@~0U@}��@}�@}IR@|��@|u�@|c�@{��@{P�@{.I@{@zTa@x��@x��@xe�@x(�@w�@w�F@w��@we�@v��@v�b@v�A@vxl@vTa@vO@u��@u�@t��@t�@sj�@r�'@ri�@r��@s;d@s{J@s��@s|�@r��@q��@qo @q	l@p�D@o�@n�@n1�@n6�@nu%@o@n�2@n�L@nM�@m�j@m��@l�|@l��@l��@k��@k�@j��@j�'@j�1@j5?@j)�@j�@jJ@je@i}�@i@@h�@h��@hj@h(�@g��@g�m@g�V@g+@e�z@eA @dI�@c�*@c6z@b�@b�@b@a�j@aO�@`��@`|�@_�r@_$t@]}�@\�	@\�@\Ĝ@\��@\��@\��@\��@\j@\K^@\:�@\@[b�@Z��@Z�!@Z�x@Z��@Zs�@Zl�@Z^5@ZL0@ZO@Y�)@Y�@Y�n@Y��@YrG@Y\�@Y�@X�.@X�@W��@Wa@W,�@V��@V��@Vں@V�B@V�6@V\�@U�9@U\�@T��@TbN@TM@T$@T�@S�A@S�6@S��@S�V@SZ�@S@R��@Rߤ@R�'@Ru%@R.�@Q�@P�P@P��@Pe�@P@O�Q@O�w@O��@OY@N8�@M�@M�@M�@M��@M�S@Mhs@M%F@L��@L��@L`�@LK^@K�@K�P@K)_@J��@Jxl@J!�@I�@Izx@H��@H�@Gn/@GE9@G)_@F�@E:�@D��@Dr�@D[�@D~@C��@Ct�@CS�@C=@B�"@B��@Bp;@B^5@B.�@A�@A@A��@A[W@A-w@@�@@Q�@?�k@?F�@>��@>l�@>C�@=�@=�7@=q@<�E@<��@<A�@;ݘ@;�	@:�s@:$�@8ѷ@82�@8$@8G@7�{@7;d@7.I@6��@6p;@6_@5��@5e,@52a@5<6@5IR@55�@50�@5 \@5+@5q@5@5!�@5%F@4�`@4��@4�@4��@4S�@3��@3P�@3!-@2�B@2�\@2E�@21�@2�@1��@1��@1�@1IR@0�K@0��@0PH@/��@/>�@/+@/.I@/,�@/�@/ i@.��@.��@.��@.�]@.YK@-c@-Vm@,�@,Q�@+��@+�&@+��@+��@+�;@+�;@+ݘ@+ݘ@+��@+�@+@+@+�@+o@+�@+S@+S@+�@+�@*��@*�'@*q�@*$�@)�.@* �@)��@)��@)}�@)[W@)�@(�`@(�@(�/@(�@(��@(��@(�e@(��@(��@(z�@(6@(	�@'�@'�@@&z@%c@%o @%f�@%e,@%8�@$�j@$��@$/�@#��@#�@#F�@"�]@"�1@"M�@!ϫ@!��@!`B@!@ ��@ �p@ ��@ 	�@��@��@t�@O@�@��@��@��@:*@
�@�D@�@ԕ@��@�X@u�@\�@J�@5�@�j@S�@�@��@�K@��@~�@U�@/�@�@�@��@�}@��@Q@8�@5?@+k@�T@��@|@5�@��@�)@�p@�U@�@�Y@[�@Xy@Q�@<�@*�@@�}@�V@�k@��@��@qv@]�@P�@��@�B@�X@�m@�@�@�}@��@zx@��@�@�e@�z@��@��@�I@�@�Y@�Y@�o@|�@q@j@j@l"@oi@C-@� @e�@(@��@�@��@��@1�@��@c@hs@4@@�P@��@��@�@�o@�@y>@q@2�@�@��@o�@!-@S@�y@�X@ȴ@~�@#:@�@u@��@�o@��@�@�9@�3@�^@��@�n@��@��@Vm@0�@+�@-w@��@��@�[@��@Ɇ@��@�)@�)@Ɇ@��@��@��@��@N�@��@�:@X�@4�@o@
�M@
��@
��@
Ta@	ԕ@	��@	�X@	��@	�7@	X@	@�`@�$@��@�@�u@��@�D@�@�@~(@|�@oi@Q�@K^@9X@$@�@��@��@g�@O@9�@$t@�@�@�b@{�@V@�@�@�@��@�9@�M@J�@(�@�@�|@�/@��@��@�$@�O@�z@�@]d@G@E9@�]@��@��@�@��@�1@q�@($@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B8�B8�B8�B9	B9�B9XB9>B9rB9rB:*B;B;�B<6B<�B<�B=�B>(B>�B>�B=�B<6B5tBB�jB�B��B��B��B��B��B�B}"Bu�Bm�BnIBf�Bc:Bb�Bb�BcnBb�Ba�BaHB`'B^�B^�BX�BU�BR�BG�BCGB>�B9XB0�BB�B�B��B��B��B�9B�VB�[B�B��B��B�7B|�Bk�Be,B\CBN�BESB<jB8�B33B)�BpB_BB�PB��B�-B�B�B�[B�CB��B��BzxBU�BB�B:�B3B,�B�B�B9B�B
�PB
�aB
�B
�B
�\B
רB
бB
�B
��B
��B
��B
��B
��B
y$B
n�B
a�B
[�B
Y�B
YB
XEB
X�B
V9B
G�B
=B
1�B
*�B
kB
�B
�B
�B	��B	��B	�=B	�{B	ЗB	�)B	�%B	ÖB	�]B	��B	�XB	��B	�B	��B	��B	�ZB	��B	��B	��B	�WB	��B	��B	��B	�.B	��B	��B	�+B	�%B	��B	�GB	��B	yXB	s�B	r�B	qAB	k�B	iyB	b�B	a�B	]B	[�B	W�B	R�B	NVB	L�B	HfB	C�B	BuB	@ B	?cB	:B	7B	6�B	6FB	6+B	5�B	4�B	3�B	1�B	1�B	,"B	(
B	$�B	 �B	�B	�B	~B	�B	)B	=B	�B	B	�B	�B	{B	[B	�B	�B	�B	1B	tB	[B	 �B	 4B�"B�<B�dB��B��B�DB��B�tB��B�B��B�B�3B�B��B�-B�OB��B�B�CB�cB��B�6B�B��B�B�RB�2B��B�zB�&B�&B�&B�B�B�B� B��B�B�B��B�B�|B�NB�B�HB�B�nB��B� B��B�B��B�B�`B�2B�B��B�B�B�B�B�|B�B��B��B�dB��B	�B	B	B	�B	B		B	)B	�B	B	6B	\B	 B	 B	4B	�B	B	gB	SB	�B	YB	WB	�B	 'B	"B	"hB	#TB	$@B	%B	$�B	(�B	)�B	-B	0�B	2-B	4B	5ZB	8�B	9�B	<PB	=<B	>wB	=qB	IB	J	B	I�B	J�B	M6B	MjB	N�B	Q B	QhB	RoB	S@B	T{B	UgB	Z�B	_VB	`�B	a-B	f�B	hXB	jB	k�B	mwB	n/B	o B	o�B	p�B	q[B	r�B	s�B	{�B	��B	��B	��B	��B	��B	��B	��B	��B	�BB	�TB	��B	��B	�1B	��B	��B	�VB	��B	��B	��B	��B	�QB	��B	��B	��B	��B	�}B	ǔB	�NB	ңB	�&B	�B	�MB	ևB	�mB	�mB	��B	��B	�B	�B	�WB	�)B	�B	�B	�B	��B	�8B	�yB	�B	�B	�B	��B	�B	��B	��B	��B	��B
 �B
�B
AB
aB
�B
�B
YB
�B
	�B

�B
�B
�B
�B
�B
pB
B
bB
�B
�B
�B
�B
�B
2B
2B
�B
�B
/B
�B
�B
;B
$�B
(�B
*�B
,�B
2�B
6`B
6FB
7�B
?cB
A�B
A�B
BB
B'B
B[B
B�B
B�B
CaB
CGB
CB
CB
C�B
DB
D�B
ESB
HB
I�B
K)B
M6B
OB
SB
UB
X+B
\�B
^�B
bB
ffB
gB
h
B
h�B
h�B
j�B
k6B
kkB
m)B
r�B
v�B
w�B
x�B
y�B
z�B
|B
}B
}qB
~B
}�B
}�B
}�B
}�B
}<B
}VB
}�B
}�B
}�B
�OB
�B
� B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�\B
�hB
�B
��B
��B
��B
�_B
�QB
��B
��B
�5B
��B
��B
�>B
�sB
��B
��B
�*B
�_B
�yB
��B
�B
�B
��B
�WB
�]B
��B
��B
�/B
�IB
�IB
�cB
��B
��B
�B
��B
��B
��B
�B
��B
��B
�?B
�FB
�B
��B
�RB
��B
�>B
�rB
��B
��B
��B
�jB
�qB
��B
�.B
�cB
��B
��B
�OB
��B
��B
��B
��B
B
��B
��B
�GB
��B
āB
�B
�B
ǮB
�fB
�RB
��B
�	B
�=B
�xB
̈́B
�pB
ΥB
ΥB
οB
�(B
�vB
�.B
�bB
� B
ѷB
ѷB
҉B
�[B
�FB
ԯB
��B
�mB
��B
�+B
�B
�	B
�xB
ܬB
��B
�B
�HB
�hB
�B
�B
� B
�B
�tB
�B
��B
�zB
��B
�B
�B
��B
�B
��B
�sB
�B
��B
�*B
�B
�"B
�B
�)B
�/B
�IB
�5B
�B
�B
�B
�B
�-B
��B
�hB
��B
��B
�lB
��B
�	B
��B
��B
�*B
�DB
�0B
��B
�6B
��B
�VB
��B
��B
�qB
��B
��B
��B
��B
��B
��B
��B
��B
�]B
�]B
�wB
��B
�.B B �BB�B�B[BuB�B�BGBaB�B�BgBSBtBBBBB_BzB_BzBzB_B�B	�B	�B
�BxB�B0B�B0BB0BBB�B~BjBjBPBjB�B�B�B�B�B�B�BVB�BBB�B�B�BB}B�B�B�B�B BBB BNBNB�B�B�B B{B�B�B�B�B�B�B�BYB�BEB�B1BB�B�B�B=B�B�B�B]B/BdB~B�BBjB�B�B!B�B�B�B B BB BB \B �B �B �B �B!�B"NB"�B#B#B#:B#nB#�B#�B$B$&B$ZB$tB$�B%,B%,B%,B%,B%�B%�B&B&�B'B'8B'B'B'RB'mB'�B'�B'�B'�B(
B($B(�B(�B(�B(�B(�B)B)B)B)�B)�B)�B)�B)�B*0B)�B*0B+�B,qB,�B,�B,�B,�B,�B,�B,�B-B-B-)B-)B-CB-CB-CB-CB-)B-CB.B.�B/ B/B/OB/OB/�B0;B0�B1'B1AB1�B1�B1�B2-B2aB2|B2�B2�B2�B2�B2�B3hB3�B4B4nB4�B4�B4�B4�B5ZB5�B5�B6B6B6+B6FB6FB6`B6`B6zB6�B6�B6�B6�B72B7LB7LB72B7�B7�B7�B7�B7�B7�B7�B7�B7�B8B8B88B88B8lB9XB9�B9�B:B:*B:^B:�B:�B;JB<B<B<6B<6B<PB<�B=B=VB=�B=�B=�B=�B=�B=�B=�B=�B=�B=�B>B>(B>BB>]B>wB>�B>�B?B?�B?�B?�B?�B@ B@OB@�B@�BA BAoBA�BA�BA�BA�BBuBB�BB�BB�BCGBCaBCaBC�BC�BC�BC�BC�BC�BD�BE�BFYBFtBFtBF�BF�BF�BF�BGEBG�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B8�B8�B8�B9	B9�B9XB9>B9rB9rB:*B;B;�B<6B<�B<�B=�B>(B>�B>�B=�B<6B5tBB�jB�B��B��B��B��B��B�B}"Bu�Bm�BnIBf�Bc:Bb�Bb�BcnBb�Ba�BaHB`'B^�B^�BX�BU�BR�BG�BCGB>�B9XB0�BB�B�B��B��B��B�9B�VB�[B�B��B��B�7B|�Bk�Be,B\CBN�BESB<jB8�B33B)�BpB_BB�PB��B�-B�B�B�[B�CB��B��BzxBU�BB�B:�B3B,�B�B�B9B�B
�PB
�aB
�B
�B
�\B
רB
бB
�B
��B
��B
��B
��B
��B
y$B
n�B
a�B
[�B
Y�B
YB
XEB
X�B
V9B
G�B
=B
1�B
*�B
kB
�B
�B
�B	��B	��B	�=B	�{B	ЗB	�)B	�%B	ÖB	�]B	��B	�XB	��B	�B	��B	��B	�ZB	��B	��B	��B	�WB	��B	��B	��B	�.B	��B	��B	�+B	�%B	��B	�GB	��B	yXB	s�B	r�B	qAB	k�B	iyB	b�B	a�B	]B	[�B	W�B	R�B	NVB	L�B	HfB	C�B	BuB	@ B	?cB	:B	7B	6�B	6FB	6+B	5�B	4�B	3�B	1�B	1�B	,"B	(
B	$�B	 �B	�B	�B	~B	�B	)B	=B	�B	B	�B	�B	{B	[B	�B	�B	�B	1B	tB	[B	 �B	 4B�"B�<B�dB��B��B�DB��B�tB��B�B��B�B�3B�B��B�-B�OB��B�B�CB�cB��B�6B�B��B�B�RB�2B��B�zB�&B�&B�&B�B�B�B� B��B�B�B��B�B�|B�NB�B�HB�B�nB��B� B��B�B��B�B�`B�2B�B��B�B�B�B�B�|B�B��B��B�dB��B	�B	B	B	�B	B		B	)B	�B	B	6B	\B	 B	 B	4B	�B	B	gB	SB	�B	YB	WB	�B	 'B	"B	"hB	#TB	$@B	%B	$�B	(�B	)�B	-B	0�B	2-B	4B	5ZB	8�B	9�B	<PB	=<B	>wB	=qB	IB	J	B	I�B	J�B	M6B	MjB	N�B	Q B	QhB	RoB	S@B	T{B	UgB	Z�B	_VB	`�B	a-B	f�B	hXB	jB	k�B	mwB	n/B	o B	o�B	p�B	q[B	r�B	s�B	{�B	��B	��B	��B	��B	��B	��B	��B	��B	�BB	�TB	��B	��B	�1B	��B	��B	�VB	��B	��B	��B	��B	�QB	��B	��B	��B	��B	�}B	ǔB	�NB	ңB	�&B	�B	�MB	ևB	�mB	�mB	��B	��B	�B	�B	�WB	�)B	�B	�B	�B	��B	�8B	�yB	�B	�B	�B	��B	�B	��B	��B	��B	��B
 �B
�B
AB
aB
�B
�B
YB
�B
	�B

�B
�B
�B
�B
�B
pB
B
bB
�B
�B
�B
�B
�B
2B
2B
�B
�B
/B
�B
�B
;B
$�B
(�B
*�B
,�B
2�B
6`B
6FB
7�B
?cB
A�B
A�B
BB
B'B
B[B
B�B
B�B
CaB
CGB
CB
CB
C�B
DB
D�B
ESB
HB
I�B
K)B
M6B
OB
SB
UB
X+B
\�B
^�B
bB
ffB
gB
h
B
h�B
h�B
j�B
k6B
kkB
m)B
r�B
v�B
w�B
x�B
y�B
z�B
|B
}B
}qB
~B
}�B
}�B
}�B
}�B
}<B
}VB
}�B
}�B
}�B
�OB
�B
� B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�\B
�hB
�B
��B
��B
��B
�_B
�QB
��B
��B
�5B
��B
��B
�>B
�sB
��B
��B
�*B
�_B
�yB
��B
�B
�B
��B
�WB
�]B
��B
��B
�/B
�IB
�IB
�cB
��B
��B
�B
��B
��B
��B
�B
��B
��B
�?B
�FB
�B
��B
�RB
��B
�>B
�rB
��B
��B
��B
�jB
�qB
��B
�.B
�cB
��B
��B
�OB
��B
��B
��B
��B
B
��B
��B
�GB
��B
āB
�B
�B
ǮB
�fB
�RB
��B
�	B
�=B
�xB
̈́B
�pB
ΥB
ΥB
οB
�(B
�vB
�.B
�bB
� B
ѷB
ѷB
҉B
�[B
�FB
ԯB
��B
�mB
��B
�+B
�B
�	B
�xB
ܬB
��B
�B
�HB
�hB
�B
�B
� B
�B
�tB
�B
��B
�zB
��B
�B
�B
��B
�B
��B
�sB
�B
��B
�*B
�B
�"B
�B
�)B
�/B
�IB
�5B
�B
�B
�B
�B
�-B
��B
�hB
��B
��B
�lB
��B
�	B
��B
��B
�*B
�DB
�0B
��B
�6B
��B
�VB
��B
��B
�qB
��B
��B
��B
��B
��B
��B
��B
��B
�]B
�]B
�wB
��B
�.B B �BB�B�B[BuB�B�BGBaB�B�BgBSBtBBBBB_BzB_BzBzB_B�B	�B	�B
�BxB�B0B�B0BB0BBB�B~BjBjBPBjB�B�B�B�B�B�B�BVB�BBB�B�B�BB}B�B�B�B�B BBB BNBNB�B�B�B B{B�B�B�B�B�B�B�BYB�BEB�B1BB�B�B�B=B�B�B�B]B/BdB~B�BBjB�B�B!B�B�B�B B BB BB \B �B �B �B �B!�B"NB"�B#B#B#:B#nB#�B#�B$B$&B$ZB$tB$�B%,B%,B%,B%,B%�B%�B&B&�B'B'8B'B'B'RB'mB'�B'�B'�B'�B(
B($B(�B(�B(�B(�B(�B)B)B)B)�B)�B)�B)�B)�B*0B)�B*0B+�B,qB,�B,�B,�B,�B,�B,�B,�B-B-B-)B-)B-CB-CB-CB-CB-)B-CB.B.�B/ B/B/OB/OB/�B0;B0�B1'B1AB1�B1�B1�B2-B2aB2|B2�B2�B2�B2�B2�B3hB3�B4B4nB4�B4�B4�B4�B5ZB5�B5�B6B6B6+B6FB6FB6`B6`B6zB6�B6�B6�B6�B72B7LB7LB72B7�B7�B7�B7�B7�B7�B7�B7�B7�B8B8B88B88B8lB9XB9�B9�B:B:*B:^B:�B:�B;JB<B<B<6B<6B<PB<�B=B=VB=�B=�B=�B=�B=�B=�B=�B=�B=�B=�B>B>(B>BB>]B>wB>�B>�B?B?�B?�B?�B?�B@ B@OB@�B@�BA BAoBA�BA�BA�BA�BBuBB�BB�BB�BCGBCaBCaBC�BC�BC�BC�BC�BC�BD�BE�BFYBFtBFtBF�BF�BF�BF�BGEBG�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220915064734  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220915064749  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220915064750  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220915064750                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220915154755  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220915154755  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220915070333                      G�O�G�O�G�O�                