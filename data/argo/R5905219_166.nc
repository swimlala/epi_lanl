CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-10-18T06:52:29Z creation;2022-10-18T06:52:30Z conversion to V3.1      
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
resolution        =���   axis      Z        x  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  `0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     x  ̀   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20221018065229  20221031181502  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @�����G1   @��F*@2��Q��c��G�{1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD�CF  CH  CJ  CL  CN�CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&�fD'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8� D9  D9�fD:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?�fD@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL�fDMfDM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D D��3D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D̼�D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D��3D�  D�@ DЀ D�� D�  D�<�Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�<�DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D޼�D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�3D�C3D��fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
>@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A��\A�\)A�\)A�\)A�\)B�B	�B�B�B!G�B)�B1�B9�BAG�BI�BQ�BY�Ba�Bi�Bq�By�B��
B��
B��
B�=pB���B��
B��
B��
B��
B��
B��
B�
=B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B�
=B�
=B��
B��B��
B��
B��
B��
B��
B��
C k�Ck�Ck�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C Q�C"Q�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8k�C:k�C<Q�C>k�C@k�CBk�CD�CFk�CHk�CJk�CLk�CN�CP�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�B�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�(�C�5�C�5�C�5�C�(�C�(�C�5�C�(�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�B�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D�GD�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D !GD ��D!�D!��D"�D"��D#�D#�{D$�D$��D%�D%��D&�D&�GD'!GD'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8!GD8��D9�D9�GD:�D:��D;�D;�GD<�D<��D=�D=��D>�D>��D?�D?�GD@�D@�{DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL!GDL�GDM!GDM��DN�DN��DO�DO��DP!GDP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De!GDe��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn!GDn��Do�Do�GDp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�qD�MqD��qD�ФD��D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�
>D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD���D�ФD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD�ФD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD��D�MqD��qD��>D�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD���D��qD�
>D�MqD��qD��qD�qD�MqD��qD��qD��D�MqD��qD��qD�qD�MqD���D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD�ФD�qD�MqD��qD��qD�qD�MqD��>D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��>D��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��>D�
>D�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD��qD�qD�MqD��qD�ФD�qD�MqDqD�ФD�qD�MqDÍqD��qD�qD�MqDčqD��qD�qD�MqDōqD��qD�qD�MqDƍqD��qD�qD�MqDǍqD��qD�qD�MqDȍqD��qD�qD�MqDɍqD��qD�qD�MqDʍqD��qD�qD�MqDˍqD��qD�qD�MqD̍qD��>D�qD�MqD͍qD��qD�qD�MqD΍qD��qD�qD�MqDύqD�ФD�qD�MqDЍqD��qD�qD�J>DэqD��qD�qD�MqDҍqD��qD�qD�MqDӍqD��qD�qD�J>DԍqD��qD�qD�MqDՍqD��qD�qD�MqD֍qD��qD�qD�MqD׍qD��qD�qD�MqD؍qD��qD�qD�MqDٍqD��qD�qD�MqDڍqD��qD�qD�MqDۍqD��qD�qD�MqD܍qD��qD�qD�MqDݍqD��qD�qD�MqDލqD��>D�qD�MqDߍqD��qD�qD�MqD��qD��qD�qD�MqD�qD��qD�qD�MqD�qD�ФD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��>D�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�P�D�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD��qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD�qD�MqD�qD��qD��D�MqD�qD��qD�qD�MqD��qD��>D�qD�MqD��qD��qD��D�P�D���D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�{Aߺ^Aߥ�Aߗ�AߋDA߉7A߃A�x�A�v�A�p�A�jA�hsA�dZA�`BA�\)A�XA�S�A�Q�A�M�A�K�A�A�A�?}A�=qA�9XA�7LA�5?A�/A�-A�"�A�VA�`BA�Q�A�VA۾wA�p�Aا�A��AծA��A�oAѕ�A�Aϩ�A�A���A��A�bNA���A�n�Aİ!AËDAA��-A�5?A� �A��7A�p�A�{A�t�A��mA�K�A��wA�+A�C�A�(�A��-A���A�?}A���A���A�
=A��RA��-A��A�ȴA�C�A��A��uA�{A�jA���A�&�A�
=A�-A�v�A��!A�5?A�%A�jA���A���A��A��mA���A�ĜA��+A���A�t�A�dZA�"�A��A���A�7LA�v�A��A�K�A� �A|�Az�HAw�Apv�An�/Am��AkƨAh��Ag7LAfVAd�Ab�jAb�Aap�A\Q�AY��AX��AX�9AXz�AVbNAS�TAQ�AP�AP(�AO�^AO%AL�DAK�AIO�AG�;AE��AC��AA�TA@�yA?�A>�jA<E�A9��A7O�A4ĜA3�hA3+A2ZA0��A/��A/"�A-�wA,ȴA+oA*bA)%A&��A%A%G�A$�yA#��A"r�A"�RA"JA!�A!`BA ��A�A\)A�`A��A^5A�hA�RA%A�A\)A�RAAoA�jA  A|�Av�A�PAĜA$�AXAoA
=A��A��A��A�A��A�7A
ZA
(�A	�wA	A�!AbNAS�A��A{A�A��A=qA�!Az�A�7AO�A�A ��A I�A $�@��w@�K�@�33@��+@��@�X@�Ĝ@���@��@�dZ@���@��T@���@�?}@�@���@�$�@��T@�p�@�ƨ@�C�@��@��#@�@���@��@��y@�ff@�%@��y@��#@�@�9X@�v�@��`@�(�@���@ۥ�@���@�1@���@��y@�n�@�J@ٲ-@�x�@�O�@�&�@��@�Ĝ@�b@��;@�+@�%@Դ9@ԛ�@ԛ�@�j@�S�@�~�@Л�@�  @Ͼw@ϥ�@ύP@�dZ@�"�@��H@�ȴ@ΰ!@Ώ\@��@��@�(�@˅@���@ʇ+@�^5@�E�@�J@�`B@ț�@�Q�@��m@�\)@ƸR@���@ũ�@�p�@�V@�Ĝ@�A�@�ƨ@�;d@°!@��@��@�@��h@�x�@��@�j@��+@�%@�v�@�$�@�E�@�5?@�V@�^5@�@��@�"�@��P@���@��H@��\@�E�@���@�~�@��@��@���@�t�@�S�@�+@���@� �@�5?@���@�"�@�J@�p�@��j@�r�@�1@�ƨ@�ƨ@��@�|�@���@�n�@�M�@��@���@���@���@��@��@���@���@�(�@�1@��
@��@�K�@�
=@���@�~�@�=q@��-@��@�p�@�&�@��@���@�Ĝ@���@�A�@�b@�1@�1@�  @��@��;@��
@�dZ@���@�v�@�@��^@�`B@�%@��@���@��@�|�@�;d@���@���@���@��!@���@���@��+@��@���@��h@��@�`B@�G�@��@��@�I�@� �@��@��;@�l�@�ȴ@�v�@�5?@��@��-@�G�@���@�j@�A�@�(�@��@�b@�  @��@��;@��F@���@���@��P@�S�@�
=@��!@�V@�5?@�@���@��h@�`B@�V@��@��9@��@�bN@��@��;@���@�;d@�
=@��H@���@�v�@�V@�J@���@���@��7@��@�p�@�O�@�/@�%@���@���@��@���@���@���@���@��u@��@�1'@�1@���@���@�"�@��R@�v�@�=q@��@��@���@�V@�Ĝ@��u@�j@�I�@���@���@���@�\)@�"�@�ȴ@�M�@��@�@��@��-@�G�@�%@���@�z�@� �@�b@��@��F@���@�
=@���@�J@���@��^@��@�?}@���@�Ĝ@���@�z�@�Z@|�@+@~ȴ@~{@}p�@|��@|�D@|(�@{t�@z��@z�!@z=q@y��@yG�@xbN@wl�@w+@v�@v��@vv�@u�T@uO�@t�@t1@sdZ@s"�@r~�@r�@rJ@q��@qX@p��@p�@p1'@o\)@nȴ@n�R@n�R@nff@m@m�@l��@l(�@k�F@k��@k�@kdZ@k33@j�H@j��@j��@j~�@j^5@j-@i�#@i��@i&�@h1'@h  @g�P@g�@f��@fV@f5?@f5?@f$�@f$�@e�@e�T@e��@e�-@eO�@d�j@dj@c�m@c�F@c��@c�@cC�@c33@c"�@co@co@c@b��@bn�@ax�@a%@`��@`�u@`b@_�@_�;@_�;@_��@^�y@^ff@]@]?}@\�@\��@\�j@\�j@\9X@[S�@Z�@Z��@Z�!@Z�\@ZM�@Y�@Y&�@X1'@Wl�@W+@W�@Vv�@U�T@UO�@T�@T��@Tz�@Tj@TZ@T9X@S��@So@Rn�@Q��@Qhs@QG�@P��@P�9@P�u@P1'@P  @O�;@O�w@Ol�@O�@O
=@N��@N�y@N��@N@M�-@M�@MO�@M/@M�@MV@L�/@K��@Ko@J�@J��@Jn�@J�@IX@HA�@G�P@G�@G
=@F�y@F��@F�+@FV@F5?@E�@E�h@EO�@EV@D��@D�j@Dz�@C��@C�@CS�@C33@Co@B�H@B�!@B��@B�\@B^5@B=q@A��@A��@AG�@A%@@�`@@Q�@?�w@?|�@?;d@?+@?
=@>ȴ@>ff@=��@=p�@=`B@=?}@=V@<�@<z�@<9X@;�
@;��@;t�@;C�@;"�@:�@:��@:�\@:=q@9�@9��@9��@9�7@9X@9%@8�9@8A�@81'@8b@7�;@7K�@6��@6�@6ȴ@6�R@6$�@5�T@5�-@5�h@5`B@5?}@4�@49X@4(�@4�@3ƨ@3��@3�@3dZ@3C�@2��@2^5@1��@1G�@1%@0��@0�u@01'@0  @/�w@/�w@/��@/l�@.ȴ@.ff@.{@-�@-@-�@-V@,�D@,I�@+��@+ƨ@+�F@+t�@+33@+o@*�@*��@*-@*�@)��@)��@)�#@)x�@(��@(�9@(�@(A�@( �@'�@'�w@'|�@&�y@&ȴ@&��@&E�@&{@%@%p�@%p�@%p�@%/@$��@$�@$z�@$(�@#�
@#��@#�@#C�@"�@"�\@"M�@"J@!�@!�#@!��@!�^@!�^@!�7@!7L@ Ĝ@ �u@ Q�@�;@\)@K�@;d@
=@�y@��@V@5?@{@�T@��@�h@�@�@p�@?}@V@�@��@�D@Z@�m@33@o@�@��@�!@^5@�@�#@��@��@��@�9@��@��@��@��@Q�@  @�;@�@��@v�@ff@V@E�@E�@5?@$�@�-@��@�@`B@`B@O�@/@��@�/@�/@��@��@�@j@I�@9X@(�@�@�m@t�@o@^5@�^@�^@��@�7@7L@�9@�@Q�@�@\)@+@�@��@�@�@�@ȴ@��@V@E�@@�T@�T@@�h@O�@/@V@�@��@�j@�D@z�@j@Z@�@�
@�
@�@C�@33@33@33@33@C�@33@"�@o@
�H@
~�@
M�@
-@	��@	��@	�^@	��@	hs@	7L@	&�@	7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�{Aߺ^Aߥ�Aߗ�AߋDA߉7A߃A�x�A�v�A�p�A�jA�hsA�dZA�`BA�\)A�XA�S�A�Q�A�M�A�K�A�A�A�?}A�=qA�9XA�7LA�5?A�/A�-A�"�A�VA�`BA�Q�A�VA۾wA�p�Aا�A��AծA��A�oAѕ�A�Aϩ�A�A���A��A�bNA���A�n�Aİ!AËDAA��-A�5?A� �A��7A�p�A�{A�t�A��mA�K�A��wA�+A�C�A�(�A��-A���A�?}A���A���A�
=A��RA��-A��A�ȴA�C�A��A��uA�{A�jA���A�&�A�
=A�-A�v�A��!A�5?A�%A�jA���A���A��A��mA���A�ĜA��+A���A�t�A�dZA�"�A��A���A�7LA�v�A��A�K�A� �A|�Az�HAw�Apv�An�/Am��AkƨAh��Ag7LAfVAd�Ab�jAb�Aap�A\Q�AY��AX��AX�9AXz�AVbNAS�TAQ�AP�AP(�AO�^AO%AL�DAK�AIO�AG�;AE��AC��AA�TA@�yA?�A>�jA<E�A9��A7O�A4ĜA3�hA3+A2ZA0��A/��A/"�A-�wA,ȴA+oA*bA)%A&��A%A%G�A$�yA#��A"r�A"�RA"JA!�A!`BA ��A�A\)A�`A��A^5A�hA�RA%A�A\)A�RAAoA�jA  A|�Av�A�PAĜA$�AXAoA
=A��A��A��A�A��A�7A
ZA
(�A	�wA	A�!AbNAS�A��A{A�A��A=qA�!Az�A�7AO�A�A ��A I�A $�@��w@�K�@�33@��+@��@�X@�Ĝ@���@��@�dZ@���@��T@���@�?}@�@���@�$�@��T@�p�@�ƨ@�C�@��@��#@�@���@��@��y@�ff@�%@��y@��#@�@�9X@�v�@��`@�(�@���@ۥ�@���@�1@���@��y@�n�@�J@ٲ-@�x�@�O�@�&�@��@�Ĝ@�b@��;@�+@�%@Դ9@ԛ�@ԛ�@�j@�S�@�~�@Л�@�  @Ͼw@ϥ�@ύP@�dZ@�"�@��H@�ȴ@ΰ!@Ώ\@��@��@�(�@˅@���@ʇ+@�^5@�E�@�J@�`B@ț�@�Q�@��m@�\)@ƸR@���@ũ�@�p�@�V@�Ĝ@�A�@�ƨ@�;d@°!@��@��@�@��h@�x�@��@�j@��+@�%@�v�@�$�@�E�@�5?@�V@�^5@�@��@�"�@��P@���@��H@��\@�E�@���@�~�@��@��@���@�t�@�S�@�+@���@� �@�5?@���@�"�@�J@�p�@��j@�r�@�1@�ƨ@�ƨ@��@�|�@���@�n�@�M�@��@���@���@���@��@��@���@���@�(�@�1@��
@��@�K�@�
=@���@�~�@�=q@��-@��@�p�@�&�@��@���@�Ĝ@���@�A�@�b@�1@�1@�  @��@��;@��
@�dZ@���@�v�@�@��^@�`B@�%@��@���@��@�|�@�;d@���@���@���@��!@���@���@��+@��@���@��h@��@�`B@�G�@��@��@�I�@� �@��@��;@�l�@�ȴ@�v�@�5?@��@��-@�G�@���@�j@�A�@�(�@��@�b@�  @��@��;@��F@���@���@��P@�S�@�
=@��!@�V@�5?@�@���@��h@�`B@�V@��@��9@��@�bN@��@��;@���@�;d@�
=@��H@���@�v�@�V@�J@���@���@��7@��@�p�@�O�@�/@�%@���@���@��@���@���@���@���@��u@��@�1'@�1@���@���@�"�@��R@�v�@�=q@��@��@���@�V@�Ĝ@��u@�j@�I�@���@���@���@�\)@�"�@�ȴ@�M�@��@�@��@��-@�G�@�%@���@�z�@� �@�b@��@��F@���@�
=@���@�J@���@��^@��@�?}@���@�Ĝ@���@�z�@�Z@|�@+@~ȴ@~{@}p�@|��@|�D@|(�@{t�@z��@z�!@z=q@y��@yG�@xbN@wl�@w+@v�@v��@vv�@u�T@uO�@t�@t1@sdZ@s"�@r~�@r�@rJ@q��@qX@p��@p�@p1'@o\)@nȴ@n�R@n�R@nff@m@m�@l��@l(�@k�F@k��@k�@kdZ@k33@j�H@j��@j��@j~�@j^5@j-@i�#@i��@i&�@h1'@h  @g�P@g�@f��@fV@f5?@f5?@f$�@f$�@e�@e�T@e��@e�-@eO�@d�j@dj@c�m@c�F@c��@c�@cC�@c33@c"�@co@co@c@b��@bn�@ax�@a%@`��@`�u@`b@_�@_�;@_�;@_��@^�y@^ff@]@]?}@\�@\��@\�j@\�j@\9X@[S�@Z�@Z��@Z�!@Z�\@ZM�@Y�@Y&�@X1'@Wl�@W+@W�@Vv�@U�T@UO�@T�@T��@Tz�@Tj@TZ@T9X@S��@So@Rn�@Q��@Qhs@QG�@P��@P�9@P�u@P1'@P  @O�;@O�w@Ol�@O�@O
=@N��@N�y@N��@N@M�-@M�@MO�@M/@M�@MV@L�/@K��@Ko@J�@J��@Jn�@J�@IX@HA�@G�P@G�@G
=@F�y@F��@F�+@FV@F5?@E�@E�h@EO�@EV@D��@D�j@Dz�@C��@C�@CS�@C33@Co@B�H@B�!@B��@B�\@B^5@B=q@A��@A��@AG�@A%@@�`@@Q�@?�w@?|�@?;d@?+@?
=@>ȴ@>ff@=��@=p�@=`B@=?}@=V@<�@<z�@<9X@;�
@;��@;t�@;C�@;"�@:�@:��@:�\@:=q@9�@9��@9��@9�7@9X@9%@8�9@8A�@81'@8b@7�;@7K�@6��@6�@6ȴ@6�R@6$�@5�T@5�-@5�h@5`B@5?}@4�@49X@4(�@4�@3ƨ@3��@3�@3dZ@3C�@2��@2^5@1��@1G�@1%@0��@0�u@01'@0  @/�w@/�w@/��@/l�@.ȴ@.ff@.{@-�@-@-�@-V@,�D@,I�@+��@+ƨ@+�F@+t�@+33@+o@*�@*��@*-@*�@)��@)��@)�#@)x�@(��@(�9@(�@(A�@( �@'�@'�w@'|�@&�y@&ȴ@&��@&E�@&{@%@%p�@%p�@%p�@%/@$��@$�@$z�@$(�@#�
@#��@#�@#C�@"�@"�\@"M�@"J@!�@!�#@!��@!�^@!�^@!�7@!7L@ Ĝ@ �u@ Q�@�;@\)@K�@;d@
=@�y@��@V@5?@{@�T@��@�h@�@�@p�@?}@V@�@��@�D@Z@�m@33@o@�@��@�!@^5@�@�#@��@��@��@�9@��@��@��@��@Q�@  @�;@�@��@v�@ff@V@E�@E�@5?@$�@�-@��@�@`B@`B@O�@/@��@�/@�/@��@��@�@j@I�@9X@(�@�@�m@t�@o@^5@�^@�^@��@�7@7L@�9@�@Q�@�@\)@+@�@��@�@�@�@ȴ@��@V@E�@@�T@�T@@�h@O�@/@V@�@��@�j@�D@z�@j@Z@�@�
@�
@�@C�@33@33@33@33@C�@33@"�@o@
�H@
~�@
M�@
-@	��@	��@	�^@	��@	hs@	7L@	&�@	7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�}B�qB�qB�qB�jB�jB�jB�jB�jB�jB�dB�dB�dB�dB�dB�dB�dB�^B�^B�XB�XB�RB�RB�RB�RB�RB�LB�FB�FB�9B�B�?B��BÖBȴB�/B�BB�B$�BR�B2-Bw�B�Bn�BaHBcTB�%BɺB��B�uB�+Br�B{�B��B�FBÖB�TB�B�B�B�B�`B�ZB�BB��B��B��B��B��B��B��B��B�+B|�Bu�Br�Bo�BjBcTB]/BT�BP�BB�B)�B�B��B�B�HB�B��B��B�LB��B�Bp�BL�BB�B9XB-B�BDBB
��B
�BB
��B
�B
dZB
I�B
:^B
%�B	��B	�B	�sB	�BB	��B	ƨB	��B	�RB	�B	��B	��B	�=B	t�B	q�B	p�B	q�B	jB	]/B	Q�B	G�B	F�B	T�B	P�B	E�B	F�B	:^B	0!B	-B	%�B	�B	�B	{B	VB	+B��B�B�B�`B�TB�HB�/B�B�
B��B��B��BǮBÖBȴB��B��B��B��BǮB�#B�B�B��B��B	  B��B��B��B��B��B��B	B	B	%B	1B	JB	PB	VB	\B	VB	PB	PB	bB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	#�B	(�B	/B	2-B	9XB	8RB	8RB	49B	33B	5?B	7LB	7LB	5?B	5?B	49B	2-B	7LB	8RB	:^B	<jB	>wB	A�B	B�B	B�B	A�B	A�B	D�B	F�B	G�B	G�B	F�B	F�B	E�B	C�B	A�B	@�B	@�B	:^B	8RB	6FB	33B	-B	(�B	&�B	$�B	$�B	%�B	'�B	&�B	'�B	)�B	'�B	&�B	(�B	)�B	2-B	6FB	9XB	;dB	A�B	K�B	P�B	Q�B	S�B	VB	VB	VB	XB	YB	XB	XB	W
B	ZB	[#B	\)B	^5B	aHB	cTB	e`B	gmB	hsB	k�B	l�B	m�B	o�B	p�B	p�B	p�B	p�B	r�B	v�B	y�B	y�B	}�B	� B	� B	�B	�B	�B	�+B	�1B	�7B	�DB	�VB	�VB	�VB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�'B	�9B	�?B	�!B	�B	��B	�B	�?B	�3B	�3B	�9B	�FB	�jB	�FB	�3B	�!B	�B	�B	�'B	�-B	�?B	�LB	�LB	�LB	�RB	�}B	��B	B	ĜB	ŢB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�/B	�;B	�;B	�HB	�NB	�TB	�TB	�ZB	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
+B
+B
1B
1B

=B
DB
JB
PB
PB
PB
VB
VB
\B
\B
bB
hB
oB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
#�B
%�B
%�B
%�B
&�B
&�B
'�B
(�B
)�B
+B
,B
,B
,B
,B
-B
.B
/B
/B
0!B
1'B
2-B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
@�B
A�B
A�B
B�B
B�B
C�B
D�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
YB
XB
YB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
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
��B
��B
��B
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333  B�}B�qB�qB�qB�jB�jB�jB�jB�jB�jB�dB�dB�dB�dB�dB�dB�dB�^B�^B�XB�XB�RB�RB�RB�RB�RB�LB�FB�FB�9B�B�?B��BÖBȴB�/B�BB�B$�BR�B2-Bw�B�Bn�BaHBcTB�%BɺB��B�uB�+Br�B{�B��B�FBÖB�TB�B�B�B�B�`B�ZB�BB��B��B��B��B��B��B��B��B�+B|�Bu�Br�Bo�BjBcTB]/BT�BP�BB�B)�B�B��B�B�HB�B��B��B�LB��B�Bp�BL�BB�B9XB-B�BDBB
��B
�BB
��B
�B
dZB
I�B
:^B
%�B	��B	�B	�sB	�BB	��B	ƨB	��B	�RB	�B	��B	��B	�=B	t�B	q�B	p�B	q�B	jB	]/B	Q�B	G�B	F�B	T�B	P�B	E�B	F�B	:^B	0!B	-B	%�B	�B	�B	{B	VB	+B��B�B�B�`B�TB�HB�/B�B�
B��B��B��BǮBÖBȴB��B��B��B��BǮB�#B�B�B��B��B	  B��B��B��B��B��B��B	B	B	%B	1B	JB	PB	VB	\B	VB	PB	PB	bB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	#�B	(�B	/B	2-B	9XB	8RB	8RB	49B	33B	5?B	7LB	7LB	5?B	5?B	49B	2-B	7LB	8RB	:^B	<jB	>wB	A�B	B�B	B�B	A�B	A�B	D�B	F�B	G�B	G�B	F�B	F�B	E�B	C�B	A�B	@�B	@�B	:^B	8RB	6FB	33B	-B	(�B	&�B	$�B	$�B	%�B	'�B	&�B	'�B	)�B	'�B	&�B	(�B	)�B	2-B	6FB	9XB	;dB	A�B	K�B	P�B	Q�B	S�B	VB	VB	VB	XB	YB	XB	XB	W
B	ZB	[#B	\)B	^5B	aHB	cTB	e`B	gmB	hsB	k�B	l�B	m�B	o�B	p�B	p�B	p�B	p�B	r�B	v�B	y�B	y�B	}�B	� B	� B	�B	�B	�B	�+B	�1B	�7B	�DB	�VB	�VB	�VB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�'B	�9B	�?B	�!B	�B	��B	�B	�?B	�3B	�3B	�9B	�FB	�jB	�FB	�3B	�!B	�B	�B	�'B	�-B	�?B	�LB	�LB	�LB	�RB	�}B	��B	B	ĜB	ŢB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�/B	�;B	�;B	�HB	�NB	�TB	�TB	�ZB	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
+B
+B
1B
1B

=B
DB
JB
PB
PB
PB
VB
VB
\B
\B
bB
hB
oB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
#�B
%�B
%�B
%�B
&�B
&�B
'�B
(�B
)�B
+B
,B
,B
,B
,B
-B
.B
/B
/B
0!B
1'B
2-B
2-B
33B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
@�B
A�B
A�B
B�B
B�B
C�B
D�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
YB
XB
YB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
|�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�7B
�7B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�\B
�\B
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
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
��B
��B
��B
��333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20221018155219  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221018065229  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221018065230  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221018065230                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221018155235  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221018155235  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221018070542                      G�O�G�O�G�O�                JA  ARFMdecpA19c                                                                20221018155219  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221031171524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221031171525  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221031171525                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221101021530  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221101021530  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20221031181502                      G�O�G�O�G�O�                