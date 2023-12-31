CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-04-14T00:37:51Z creation;2020-04-14T00:37:53Z conversion to V3.1      
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
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  s(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ː   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200414003751  20200414005544  1902338                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   NAVIS_A                         0922                            ARGO                            863 @��z�H1   @�������C��
=q@DL�C��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@�CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DDy�DE  DE� DE��DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  Dy�D��D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�C3DԀ D�� D�  D�@ DՀ Dռ�D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڃ3D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@<(�@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH�\BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��HB�{B�{B�{C 
=C
=C
=C
=C
=C

=C
=C
=C
=C
=C
=C
=C
=C
=C
=C
=C 
=C"
=C$
=C&
=C(
=C*
=C,
=C.
=C0
=C2
=C4
=C6#�C8
=C:
=C<
=C>
=C@#�CB#�CD
=CF
=CH
=CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ
=C\
=C^
=C`
=Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp
=Cr
=Cs�Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D|)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DC�)DD|)DE�DE��DE�)DF|)DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D|)D�)D�>D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD��D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�~D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�{D�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�D{DԁHD��HD�HD�AHDՁHDվD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�D{Dڄ{D��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��{D�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A��
A��
A��A��A��A��A��#A��#A��#A��/A��/A��;A��;A��;A��HA��HA��HA��TA��HA��;A��;A��/A��/A��/A��/A��#A��/A��#A���A���A���A���A�=qA�-A�1A�A��#A�A�A�-A{K�Ax1'Au�FAp��Am��Al�yAl�Ak�Ak��Ak&�Aj�9Ajr�AjA�Ai��AiXAh^5Ag�7Af��Af=qAe��AdffAb��Aa��A_hsA^ĜA^E�A^�A]��A]VA\�HA\��A\M�AZ��AY�PAYVAX�RAX�+AXr�AXffAXM�AW�wAW+AV�AU��AT��AS�AR�DAR{AQS�AP�`AP�9AP�DAP1'AOdZAO�AN��ANM�AN  AN  AM��AMK�AL~�AJ��AI�mAI��AIO�AH�AH��AHI�AGAF�AF�AE�wAE&�AD�9AC�wAC&�AB(�AA�AA"�A@�RA@ZA>�+A:bA: �A:E�A:�/A:~�A8�uA7A6��A7�A8E�A7�#A7�PA7t�A7K�A7&�A6�A5��A3��A1��A1|�A.ĜA.VA-�
A-l�A-�A-|�A-�A+�mA*�/A+�A++A+�A+A+33A*bNA)�
A)��A*��A*�/A*��A+;dA++A)��A)K�A)hsA)l�A)�hA)oA(A�A'�PA&�9A&�jA'�A&��A&�A%��A%�#A%|�A$(�A#��A#�A#hsA"�`A"�RA"��A"�/A#oA#/A#7LA#K�A#�
A#?}A!VA��A"�A
=A�9A~�A9XA��A��A (�A A�A =qA`BA��A��A��A1A��A�A`BAr�A|�A7LA��AZA�A�^AXAA�DA(�A��A��AA��A
=A��Az�A{A�A�
A��A\)A+A%An�A�mA\)Av�A�A��AAoA�+AZAA�A��A��A�^A�7A+A
I�A	A	x�A	%A��A$�A��A�mAA��A��A��A�PA\)AG�AAz�A�AAl�A��A=qA�A  AƨA7LA�A�AffAI�A5?A�A�7A�A �A �DA E�A �@��P@��H@�p�@�(�@�o@���@�n�@�%@�Z@�\)@�5?@�`B@�/@�dZ@��@�7L@���@�9@�@�A�@��
@�w@�
=@���@��H@��@���@�(�@�33@�-@�h@��/@�D@�\)@�~�@��@��;@�+@ް!@�`B@�b@١�@ו�@�$�@�Z@�o@�V@���@��@�~�@˝�@ʟ�@�@�7L@�A�@�1'@��@ǶF@ǍP@�|�@�l�@�l�@�C�@�ȴ@š�@���@�  @�n�@��@�bN@��;@�"�@�M�@��@���@�Ĝ@��F@�V@���@��@��D@�b@��F@�+@��\@�hs@�%@�Ĝ@�1'@�l�@���@���@�r�@�  @��;@��H@��T@���@�-@�~�@���@��H@��y@���@��@���@��/@�%@�A�@�33@�M�@�O�@��`@�j@� �@�ƨ@�33@��!@�E�@��h@�O�@�/@�?}@�&�@���@�Ĝ@�A�@�+@��@�$�@���@�G�@��`@�9X@��
@�|�@�K�@�"�@�ȴ@��+@�5?@���@��@���@�p�@�V@�Ĝ@���@��@���@���@���@���@��u@�r�@�bN@�ƨ@�\)@�+@���@��\@�J@�%@��`@���@��D@�Q�@��@�ȴ@�@���@�@���@�`B@�&�@�V@�bN@�1'@��@���@���@�l�@���@��\@�v�@�n�@�ff@�ff@�V@�5?@�$�@�-@�$�@��@��j@�dZ@��\@�J@��@���@��-@��7@�p�@�`B@�G�@�7L@�/@�V@���@���@�j@�I�@�9X@�b@��
@��F@���@��P@�t�@�;d@�o@��@��@��R@���@�^5@�-@��#@��@��m@���@�C�@���@�v�@�V@�^5@�ff@�ff@�^5@�V@�V@�E�@�{@���@��-@�X@�G�@���@\)@~�+@~5?@~$�@}�@{�m@y7L@x�`@x��@w�@v�+@v@u�-@u�-@u�-@u�-@u�-@u�-@u�-@u�-@u�-@u��@t�@s33@q�#@o
=@nE�@m/@l(�@jn�@i��@h  @gK�@g
=@f�@f�R@f�+@f$�@e�T@e��@e?}@d�D@cS�@b�H@bn�@bJ@a�^@`��@`r�@_�;@_��@_|�@_�P@_��@_�P@_\)@_;d@_;d@_+@_;d@_;d@_+@_�@_�@^�@^��@^��@^�+@^�+@^�+@^�+@^�+@^��@^��@^��@^ff@]�-@]p�@\�/@\Z@\I�@[��@[�F@[o@Z��@Z��@Z^5@Z�@ZJ@ZJ@Y�#@Y��@Y��@Y�7@Y&�@Y&�@Y&�@Y&�@Y�@Y�@Y�@X��@XĜ@X�@Xb@W�@W+@W
=@VV@V$�@Up�@T�@T�D@Tz�@Tj@T9X@S�m@S��@SS�@S@R��@R-@Q��@Q��@Qhs@Q�@P�9@P�9@PĜ@P�9@P�9@P�9@P��@P�@P�@PbN@O�@O�w@O�@N�@N�R@N�R@Nȴ@N�R@N�+@N�+@Nv�@Nff@Nff@Nff@NV@Nff@Nv�@N�+@NV@N5?@N{@M��@M�@L�@LI�@L�@K��@K��@K�@Kt�@KdZ@I��@H �@F��@E@E/@D��@C�@B^5@BJ@A�@A�@A�#@A��@Ahs@A�7@A��@A�7@Ax�@Ax�@A��@BJ@B�@A��@A��@A��@A��@A�#@A��@BJ@B-@A��@>�@>$�@<�j@;C�@;"�@:�@:��@:n�@:M�@:J@9�@9�#@9�#@9�#@9��@9x�@9�@8��@81'@8  @8b@7�P@7|�@7|�@7��@7��@8 �@8Ĝ@9x�@9�@9��@:-@:=q@9X@8�9@8bN@8A�@81'@8 �@8  @7��@7��@7�;@9G�@9�@8  @8�9@8��@9&�@97L@9X@97L@8�`@8��@8bN@8A�@7|�@6E�@5�@4��@2��@2-@2-@1�^@1x�@1x�@1��@1��@1��@1��@1�7@1�7@1�7@1�7@1�7@1�7@1�7@1�7@1x�@1x�@1x�@1��@1G�@1&�@17L@1&�@1%@1%@1%@1G�@1hs@1�@1&�@1hs@2-@2M�@2M�@2n�@2~�@2�\@2��@2�@3o@2��@2��@2��@2��@2��@2��@2�!@2��@2~�@2�!@2��@2�H@2�H@2��@2�!@2��@2��@2�\@2�\@2~�@2n�@2^5@2^5@2M�@2=q@2-@2�@2�@2J@1��@1�#@1�#@1��@1�^@1�^@1�^@1��@1��@1��@1��@1��@1�7@1x�@1x�@1x�@1X@1X@1&�@1�@1%@1%@0��@0��@1&�@1G�@1G�@1G�@17L@17L@1&�@1�@0�`@0r�@0A�@01'@01'@0A�@0r�@0�u@0�@0�9@0Ĝ@0Ĝ@0r�@0A�@0 �@0 �@0b@0b@0  @0  @/�@/�;@/�;@/�@/�@/�@/�@/l�@/K�@/;d@/�@/
=@.�@.ȴ@.�R@.��@.v�@.5?@.{@.{@.{@.@.@-�T@-��@-@-@-@-��@-��@-�@-p�@-`B@-O�@-O�@-?}@-/@-V@,�@,��@,�@,z�@,I�@,I�@,9X@,(�@,(�@,�@,1@+��@+�m@+�
@+ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A��
A��
A��A��A��A��A��#A��#A��#A��/A��/A��;A��;A��;A��HA��HA��HA��TA��HA��;A��;A��/A��/A��/A��/A��#A��/A��#A���A���A���A���A�=qA�-A�1A�A��#A�A�A�-A{K�Ax1'Au�FAp��Am��Al�yAl�Ak�Ak��Ak&�Aj�9Ajr�AjA�Ai��AiXAh^5Ag�7Af��Af=qAe��AdffAb��Aa��A_hsA^ĜA^E�A^�A]��A]VA\�HA\��A\M�AZ��AY�PAYVAX�RAX�+AXr�AXffAXM�AW�wAW+AV�AU��AT��AS�AR�DAR{AQS�AP�`AP�9AP�DAP1'AOdZAO�AN��ANM�AN  AN  AM��AMK�AL~�AJ��AI�mAI��AIO�AH�AH��AHI�AGAF�AF�AE�wAE&�AD�9AC�wAC&�AB(�AA�AA"�A@�RA@ZA>�+A:bA: �A:E�A:�/A:~�A8�uA7A6��A7�A8E�A7�#A7�PA7t�A7K�A7&�A6�A5��A3��A1��A1|�A.ĜA.VA-�
A-l�A-�A-|�A-�A+�mA*�/A+�A++A+�A+A+33A*bNA)�
A)��A*��A*�/A*��A+;dA++A)��A)K�A)hsA)l�A)�hA)oA(A�A'�PA&�9A&�jA'�A&��A&�A%��A%�#A%|�A$(�A#��A#�A#hsA"�`A"�RA"��A"�/A#oA#/A#7LA#K�A#�
A#?}A!VA��A"�A
=A�9A~�A9XA��A��A (�A A�A =qA`BA��A��A��A1A��A�A`BAr�A|�A7LA��AZA�A�^AXAA�DA(�A��A��AA��A
=A��Az�A{A�A�
A��A\)A+A%An�A�mA\)Av�A�A��AAoA�+AZAA�A��A��A�^A�7A+A
I�A	A	x�A	%A��A$�A��A�mAA��A��A��A�PA\)AG�AAz�A�AAl�A��A=qA�A  AƨA7LA�A�AffAI�A5?A�A�7A�A �A �DA E�A �@��P@��H@�p�@�(�@�o@���@�n�@�%@�Z@�\)@�5?@�`B@�/@�dZ@��@�7L@���@�9@�@�A�@��
@�w@�
=@���@��H@��@���@�(�@�33@�-@�h@��/@�D@�\)@�~�@��@��;@�+@ް!@�`B@�b@١�@ו�@�$�@�Z@�o@�V@���@��@�~�@˝�@ʟ�@�@�7L@�A�@�1'@��@ǶF@ǍP@�|�@�l�@�l�@�C�@�ȴ@š�@���@�  @�n�@��@�bN@��;@�"�@�M�@��@���@�Ĝ@��F@�V@���@��@��D@�b@��F@�+@��\@�hs@�%@�Ĝ@�1'@�l�@���@���@�r�@�  @��;@��H@��T@���@�-@�~�@���@��H@��y@���@��@���@��/@�%@�A�@�33@�M�@�O�@��`@�j@� �@�ƨ@�33@��!@�E�@��h@�O�@�/@�?}@�&�@���@�Ĝ@�A�@�+@��@�$�@���@�G�@��`@�9X@��
@�|�@�K�@�"�@�ȴ@��+@�5?@���@��@���@�p�@�V@�Ĝ@���@��@���@���@���@���@��u@�r�@�bN@�ƨ@�\)@�+@���@��\@�J@�%@��`@���@��D@�Q�@��@�ȴ@�@���@�@���@�`B@�&�@�V@�bN@�1'@��@���@���@�l�@���@��\@�v�@�n�@�ff@�ff@�V@�5?@�$�@�-@�$�@��@��j@�dZ@��\@�J@��@���@��-@��7@�p�@�`B@�G�@�7L@�/@�V@���@���@�j@�I�@�9X@�b@��
@��F@���@��P@�t�@�;d@�o@��@��@��R@���@�^5@�-@��#@��@��m@���@�C�@���@�v�@�V@�^5@�ff@�ff@�^5@�V@�V@�E�@�{@���@��-@�X@�G�@���@\)@~�+@~5?@~$�@}�@{�m@y7L@x�`@x��@w�@v�+@v@u�-@u�-@u�-@u�-@u�-@u�-@u�-@u�-@u�-@u��@t�@s33@q�#@o
=@nE�@m/@l(�@jn�@i��@h  @gK�@g
=@f�@f�R@f�+@f$�@e�T@e��@e?}@d�D@cS�@b�H@bn�@bJ@a�^@`��@`r�@_�;@_��@_|�@_�P@_��@_�P@_\)@_;d@_;d@_+@_;d@_;d@_+@_�@_�@^�@^��@^��@^�+@^�+@^�+@^�+@^�+@^��@^��@^��@^ff@]�-@]p�@\�/@\Z@\I�@[��@[�F@[o@Z��@Z��@Z^5@Z�@ZJ@ZJ@Y�#@Y��@Y��@Y�7@Y&�@Y&�@Y&�@Y&�@Y�@Y�@Y�@X��@XĜ@X�@Xb@W�@W+@W
=@VV@V$�@Up�@T�@T�D@Tz�@Tj@T9X@S�m@S��@SS�@S@R��@R-@Q��@Q��@Qhs@Q�@P�9@P�9@PĜ@P�9@P�9@P�9@P��@P�@P�@PbN@O�@O�w@O�@N�@N�R@N�R@Nȴ@N�R@N�+@N�+@Nv�@Nff@Nff@Nff@NV@Nff@Nv�@N�+@NV@N5?@N{@M��@M�@L�@LI�@L�@K��@K��@K�@Kt�@KdZ@I��@H �@F��@E@E/@D��@C�@B^5@BJ@A�@A�@A�#@A��@Ahs@A�7@A��@A�7@Ax�@Ax�@A��@BJ@B�@A��@A��@A��@A��@A�#@A��@BJ@B-@A��@>�@>$�@<�j@;C�@;"�@:�@:��@:n�@:M�@:J@9�@9�#@9�#@9�#@9��@9x�@9�@8��@81'@8  @8b@7�P@7|�@7|�@7��@7��@8 �@8Ĝ@9x�@9�@9��@:-@:=q@9X@8�9@8bN@8A�@81'@8 �@8  @7��@7��@7�;@9G�@9�@8  @8�9@8��@9&�@97L@9X@97L@8�`@8��@8bN@8A�@7|�@6E�@5�@4��@2��@2-@2-@1�^@1x�@1x�@1��@1��@1��@1��@1�7@1�7@1�7@1�7@1�7@1�7@1�7@1�7@1x�@1x�@1x�@1��@1G�@1&�@17L@1&�@1%@1%@1%@1G�@1hs@1�@1&�@1hs@2-@2M�@2M�@2n�@2~�@2�\@2��@2�@3o@2��@2��@2��@2��@2��@2��@2�!@2��@2~�@2�!@2��@2�H@2�H@2��@2�!@2��@2��@2�\@2�\@2~�@2n�@2^5@2^5@2M�@2=q@2-@2�@2�@2J@1��@1�#@1�#@1��@1�^@1�^@1�^@1��@1��@1��@1��@1��@1�7@1x�@1x�@1x�@1X@1X@1&�@1�@1%@1%@0��@0��@1&�@1G�@1G�@1G�@17L@17L@1&�@1�@0�`@0r�@0A�@01'@01'@0A�@0r�@0�u@0�@0�9@0Ĝ@0Ĝ@0r�@0A�@0 �@0 �@0b@0b@0  @0  @/�@/�;@/�;@/�@/�@/�@/�@/l�@/K�@/;d@/�@/
=@.�@.ȴ@.�R@.��@.v�@.5?@.{@.{@.{@.@.@-�T@-��@-@-@-@-��@-��@-�@-p�@-`B@-O�@-O�@-?}@-/@-V@,�@,��@,�@,z�@,I�@,I�@,9X@,(�@,(�@,�@,1@+��@+�m@+�
@+ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�bB�bB�bB�bB�bB�bB�bB�bB�bB�\B�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�hB�hB�hB�uB��B�uB�uB�{B�oB�VB�B_;BR�BC�B=qB1'B0!B/B-B+B+B'�B&�B&�B&�B$�B�B�B{BbBJBB��B�B�NB�)B�B�B�B��B��B��B��B�qB�B��B��B��B��B��B��B��B��B�uB�VB�B}�Bv�Bu�Bq�Bn�Bm�Bl�Bl�BiyBiyBhsBhsBgmBffBffBaHB[#BD�B=qBA�B@�B=qB;dB9XB33B-B#�B�B�B�BVB1B��B��B��B�B�yB�#B��B��B��B��B��B�uB�B�B��B��B��B��B��B��B��B��B�bBy�B[#BYB6FB.B)�B%�B'�B'�B$�B�B
=B�B�B(�B+B#�B �B�B�B)�B/B2-B8RB9XB-B%�B'�B(�B-B)�B �B�BbBhB�B�BoBbBbBPB
��B
��B
��B
��B
�B
�B
�B
��B
��BB+BDB�BhB
��B
�fB
�NB
�;B
�#B
�B
��B
�B
�`BBDBJB%B
��B
�B
�B
�B
�B
�B
�B
�sB
�HB
�5B
�/B
�B
�B
��B
��B
��B
��B
ɺB
ƨB
��B
�jB
�RB
�LB
�9B
�3B
�!B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�oB
�VB
�=B
�7B
�%B
�B
�B
�B
� B
}�B
|�B
{�B
v�B
p�B
m�B
l�B
ffB
dZB
aHB
aHB
_;B
^5B
^5B
]/B
]/B
[#B
ZB
YB
T�B
Q�B
N�B
L�B
H�B
E�B
F�B
G�B
G�B
E�B
B�B
A�B
@�B
@�B
?}B
>wB
=qB
:^B
9XB
7LB
5?B
49B
2-B
1'B
/B
,B
(�B
'�B
(�B
&�B
%�B
#�B
!�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
oB
oB
oB
VB
+B
B
B
B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�sB	�fB	�NB	�5B	�#B	��B	��B	ɺB	ĜB	B	�wB	�dB	�FB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�bB	�PB	�JB	�1B	�B	� B	|�B	z�B	x�B	v�B	u�B	u�B	v�B	x�B	v�B	u�B	u�B	s�B	r�B	o�B	k�B	iyB	iyB	hsB	e`B	ffB	iyB	l�B	p�B	r�B	s�B	r�B	o�B	ffB	dZB	ffB	gmB	dZB	`BB	[#B	YB	XB	W
B	W
B	T�B	T�B	W
B	VB	VB	W
B	YB	]/B	]/B	_;B	_;B	^5B	_;B	]/B	]/B	]/B	^5B	^5B	]/B	\)B	\)B	]/B	^5B	^5B	aHB	cTB	cTB	ffB	hsB	n�B	q�B	v�B	}�B	}�B	~�B	~�B	� B	�B	�B	�B	�=B	�DB	�JB	�JB	�VB	�\B	�oB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�9B	�FB	�FB	�RB	�XB	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�dB	�dB	�jB	�qB	�jB	�wB	��B	��B	�}B	�}B	�}B	��B	ƨB	ƨB	ǮB	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�;B	�HB	�TB	�TB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
1B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
VB
uB
�B
�B
�B
�B
�B
�B
!�B
#�B
$�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
+B
+B
+B
+B
+B
+B
+B
-B
-B
-B
/B
0!B
2-B
33B
5?B
6FB
8RB
:^B
;dB
<jB
<jB
=qB
>wB
?}B
A�B
C�B
D�B
F�B
F�B
G�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
S�B
S�B
T�B
T�B
T�B
S�B
S�B
S�B
Q�B
Q�B
P�B
P�B
O�B
N�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
S�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
YB
YB
W
B
R�B
S�B
P�B
P�B
P�B
P�B
P�B
P�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
XB
[#B
\)B
]/B
^5B
_;B
_;B
`BB
aHB
bNB
e`B
hsB
k�B
m�B
o�B
o�B
q�B
p�B
p�B
p�B
o�B
o�B
p�B
q�B
s�B
u�B
y�B
}�B
|�B
�B
�%B
�1B
�1B
�7B
�JB
�bB
�hB
�hB
�oB
�hB
�bB
�bB
�\B
�PB
�PB
�PB
�VB
�oB
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
�B
�B
�B
�B
�-B
�3B
�9B
�?B
�FB
�LB
�RB
�^B
�jB
�wB
��B
��B
B
ÖB
ĜB
ĜB
ĜB
ŢB
ƨB
ȴB
ɺB
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
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�
B
�
B
�B
�B
�B
�#B
�)B
�)B
�/B
�/B
�5B
�5B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�BB
�HB
�NB
�NB
�NB
�TB
�fB
�sB
�sB
�B
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
��B  BBBBBBBBBBBBBB%B%B%B%B+B+B1B	7B	7B
=BDBJBJBJBJBPBPBPBVBVBVB\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�bB�bB�bB�bB�bB�bB�bB�bB�bB�\B�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�bB�hB�hB�hB�uB��B�uB�uB�{B�oB�VB�B_;BR�BC�B=qB1'B0!B/B-B+B+B'�B&�B&�B&�B$�B�B�B{BbBJBB��B�B�NB�)B�B�B�B��B��B��B��B�qB�B��B��B��B��B��B��B��B��B�uB�VB�B}�Bv�Bu�Bq�Bn�Bm�Bl�Bl�BiyBiyBhsBhsBgmBffBffBaHB[#BD�B=qBA�B@�B=qB;dB9XB33B-B#�B�B�B�BVB1B��B��B��B�B�yB�#B��B��B��B��B��B�uB�B�B��B��B��B��B��B��B��B��B�bBy�B[#BYB6FB.B)�B%�B'�B'�B$�B�B
=B�B�B(�B+B#�B �B�B�B)�B/B2-B8RB9XB-B%�B'�B(�B-B)�B �B�BbBhB�B�BoBbBbBPB
��B
��B
��B
��B
�B
�B
�B
��B
��BB+BDB�BhB
��B
�fB
�NB
�;B
�#B
�B
��B
�B
�`BBDBJB%B
��B
�B
�B
�B
�B
�B
�B
�sB
�HB
�5B
�/B
�B
�B
��B
��B
��B
��B
ɺB
ƨB
��B
�jB
�RB
�LB
�9B
�3B
�!B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�oB
�VB
�=B
�7B
�%B
�B
�B
�B
� B
}�B
|�B
{�B
v�B
p�B
m�B
l�B
ffB
dZB
aHB
aHB
_;B
^5B
^5B
]/B
]/B
[#B
ZB
YB
T�B
Q�B
N�B
L�B
H�B
E�B
F�B
G�B
G�B
E�B
B�B
A�B
@�B
@�B
?}B
>wB
=qB
:^B
9XB
7LB
5?B
49B
2-B
1'B
/B
,B
(�B
'�B
(�B
&�B
%�B
#�B
!�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
oB
oB
oB
VB
+B
B
B
B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�sB	�fB	�NB	�5B	�#B	��B	��B	ɺB	ĜB	B	�wB	�dB	�FB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�bB	�PB	�JB	�1B	�B	� B	|�B	z�B	x�B	v�B	u�B	u�B	v�B	x�B	v�B	u�B	u�B	s�B	r�B	o�B	k�B	iyB	iyB	hsB	e`B	ffB	iyB	l�B	p�B	r�B	s�B	r�B	o�B	ffB	dZB	ffB	gmB	dZB	`BB	[#B	YB	XB	W
B	W
B	T�B	T�B	W
B	VB	VB	W
B	YB	]/B	]/B	_;B	_;B	^5B	_;B	]/B	]/B	]/B	^5B	^5B	]/B	\)B	\)B	]/B	^5B	^5B	aHB	cTB	cTB	ffB	hsB	n�B	q�B	v�B	}�B	}�B	~�B	~�B	� B	�B	�B	�B	�=B	�DB	�JB	�JB	�VB	�\B	�oB	�oB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�9B	�FB	�FB	�RB	�XB	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�^B	�dB	�dB	�jB	�qB	�jB	�wB	��B	��B	�}B	�}B	�}B	��B	ƨB	ƨB	ǮB	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�;B	�HB	�TB	�TB	�ZB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
1B
	7B

=B
DB
DB
DB
JB
JB
JB
JB
VB
uB
�B
�B
�B
�B
�B
�B
!�B
#�B
$�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
+B
+B
+B
+B
+B
+B
+B
-B
-B
-B
/B
0!B
2-B
33B
5?B
6FB
8RB
:^B
;dB
<jB
<jB
=qB
>wB
?}B
A�B
C�B
D�B
F�B
F�B
G�B
H�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
S�B
S�B
T�B
T�B
T�B
S�B
S�B
S�B
Q�B
Q�B
P�B
P�B
O�B
N�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
S�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
YB
YB
W
B
R�B
S�B
P�B
P�B
P�B
P�B
P�B
P�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
VB
XB
[#B
\)B
]/B
^5B
_;B
_;B
`BB
aHB
bNB
e`B
hsB
k�B
m�B
o�B
o�B
q�B
p�B
p�B
p�B
o�B
o�B
p�B
q�B
s�B
u�B
y�B
}�B
|�B
�B
�%B
�1B
�1B
�7B
�JB
�bB
�hB
�hB
�oB
�hB
�bB
�bB
�\B
�PB
�PB
�PB
�VB
�oB
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
�B
�B
�B
�B
�-B
�3B
�9B
�?B
�FB
�LB
�RB
�^B
�jB
�wB
��B
��B
B
ÖB
ĜB
ĜB
ĜB
ŢB
ƨB
ȴB
ɺB
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
�B
�B
�B
�B
�
B
�
B
�
B
�
B
�
B
�
B
�B
�B
�B
�#B
�)B
�)B
�/B
�/B
�5B
�5B
�;B
�;B
�;B
�;B
�;B
�;B
�;B
�BB
�HB
�NB
�NB
�NB
�TB
�fB
�sB
�sB
�B
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
��B  BBBBBBBBBBBBBB%B%B%B%B+B+B1B	7B	7B
=BDBJBJBJBJBPBPBPBVBVBVB\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20200414093748  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200414003751  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200414003752  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200414003752  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200414003753  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200414003753  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200414003753  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200414003753  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200414003753  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200414003753                      G�O�G�O�G�O�                JA  ARUP                                                                        20200414005544                      G�O�G�O�G�O�                