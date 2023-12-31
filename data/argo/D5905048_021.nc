CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-26T00:35:24Z creation;2016-07-26T00:35:26Z conversion to V3.1;2019-12-19T08:30:54Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160726003524  20200116201517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_021                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @׾6$8!�1   @׾6β@�@3M5�Xy>�d�1���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP�\BX(�B`�\Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�G�B�G�B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
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
=C6
=C8
=C:
=C<
=C>
=C@
=CB
=CD
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
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�~D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�>D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��{D�ǮD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ffA�M�A�33A�+A�$�A�$�A�$�A��A��A��A��A��A�{A�{A�oA��/A�jA�{A�ĜAٗ�A���A�dZA��yA��TA�A�A���A��HAՓuA�G�A�&�A�^5A��A��#Aϕ�A�I�Aɟ�A�
=A�dZA�l�A��-A�^5A�S�A��/A��A�?}A�{A�`BA�7LA���A�ĜA�jA�^5A�\)A�E�A���A��+A��
A�VA�(�A��A��A�E�A�{A���A���A�$�A�oA�A��A��A�ȴA��hA���A�x�A��+A�A�C�A��A�bA��A�t�A�9XA�G�A�%A�I�A�1A���A��jA���A��mA�S�A�p�A��FA�
=A��!A�~�A�E�A�A���A��+A���A�x�A��TA��!A�A���A�-A~�uA}�;A|��Ax�uAu��As?}Aq�^Ap  An��Am��AlA�Aj�!Ai�;Ai
=Ag��AfjAc�;Ab=qA`�A^v�A\ĜAZ1AX�+AV��AR�HAQ\)AP1AO33AM�AKp�AI��AG;dADZABĜAA��A@A�A?�;A=��A<ȴA<��A<bA:��A:�A9��A8~�A7��A5\)A3�A2�HA2n�A1��A1�A0��A/|�A-S�A+�A*1'A(��A'�A&A�A%�7A$E�A#�;A#&�A"�A"�jA"jA!ƨA!�A �uAAoA��A��A&�A  Ap�AjA��A��AQ�A�A�uA�
A�A��A�A��A�AE�A��A{AVAI�AJA\)A�RAr�A1A	G�A~�A�A�!AbNAI�A�AȴA��A��@��
@��#@�hs@���@�x�@�@��;@��
@��\@��m@�
=@�E�@��@�x�@��@�o@���@�1@�33@陚@�9X@�M�@��@䛦@�\)@�-@�&�@�bN@�A�@�(�@�~�@�/@�I�@�l�@���@���@���@ؓu@�I�@�+@�n�@�@�%@�z�@�dZ@��@�X@�  @��@ͺ^@�V@͡�@́@�?}@�ȴ@Ο�@Ώ\@�~�@͑h@�Z@�r�@���@�b@�dZ@��H@Ɂ@���@��y@Ƨ�@ÍP@���@�9X@���@��\@��@�X@��@��@��D@��/@��j@�dZ@��!@��\@�@���@�X@�7L@��`@�I�@��@���@�+@��@�5?@�j@�S�@���@�9X@�ƨ@���@�v�@��y@��@�v�@�E�@�-@�{@�@���@�`B@�?}@��@��@�A�@��F@�b@��;@��@�S�@�33@�
=@�n�@�n�@���@�$�@�?}@��@�  @��w@��;@�I�@�`B@�/@�7L@���@���@�hs@��@��R@��\@�@��^@�7L@�A�@���@��P@�C�@�n�@�=q@�n�@�o@�+@�;d@�dZ@��w@�ƨ@���@�;d@��@�
=@�ff@�@�7L@��@�%@�O�@�O�@�Z@���@�K�@��@�E�@�M�@��@�z�@��@�?}@���@�+@���@�{@��@�%@�X@��@��#@��@���@�1'@��;@�\)@��!@���@��@�t�@�ƨ@��;@���@�t�@�K�@�o@���@�ff@�=q@���@�x�@�?}@�%@���@� �@���@�K�@���@��@���@�v�@�V@�J@���@���@�hs@��@���@� �@��
@�l�@�+@��@���@�V@�5?@��@�J@��#@���@�hs@�/@�V@��u@�9X@���@��@�t�@�33@�o@��y@���@��!@�V@���@���@��^@���@�X@�bN@�I�@�I�@�I�@�I�@�1'@�1@���@���@�l�@�;d@�o@��H@���@�ff@�-@��@�@��@�`B@�X@�?}@�%@�Ĝ@���@�Q�@�1@��@��
@���@���@�|�@�S�@�"�@���@���@�M�@�@��^@�hs@��@�A�@��@��\@��#@�@���@�hs@��@���@��j@��D@�9X@�(�@�(�@� �@�b@�@�w@�P@l�@;d@+@~�@~��@~ff@~$�@}�@{ƨ@z��@z=q@y��@y��@yX@yX@yG�@y7L@y&�@x�`@xb@w�@w
=@vE�@v5?@u�T@uO�@tz�@t(�@t1@s�F@s�@so@r=q@q��@qX@q&�@p�9@o�@o;d@o
=@n�y@n��@nE�@m@lZ@k��@k�
@k�@j��@j^5@j�@i�#@i&�@hr�@h �@h �@hb@g��@gK�@fȴ@fV@e�@e/@d��@dZ@cƨ@c33@c@bn�@a�^@ahs@a&�@`��@`��@`b@_�w@_|�@_�@_
=@^ȴ@^ff@^{@]�T@]��@]p�@]�@\�j@\z�@\Z@\9X@\�@[��@["�@Z��@Zn�@Z-@Z�@Y�#@Y��@Y�7@YX@X�`@X��@XA�@X �@W�@W\)@W
=@Vv�@V{@U@U��@U`B@T�/@T�@T�@S��@R�@R��@R�\@Q�@Q��@Qx�@P�`@P�9@P�9@P�9@P�@P  @O�@O|�@Nȴ@M�@M�@MV@M�@L�@L��@L�D@LZ@K�
@K�@K33@K@J�H@J~�@Jn�@J^5@JM�@J=q@J�@I�@I�^@I��@I�7@IG�@I�@H�u@H��@H�@HA�@H1'@H1'@H  @G��@G+@F��@E@E`B@D��@DI�@D�@C�
@C�F@C��@CdZ@CS�@C33@B��@BM�@AG�@@�9@@��@@�@@b@?�@?��@?�P@?K�@?+@>��@>��@>V@=��@=�@=V@<��@<�@;ƨ@;�@;S�@:�H@:�!@:^5@:J@9�#@9�^@9x�@97L@8Ĝ@8bN@8bN@8A�@8  @7�;@7�w@7\)@7+@6�@6ȴ@6�+@6ff@5@5V@4��@4�@4j@4I�@4�@3��@3o@2�\@2�@1��@1�7@1hs@1&�@0�`@0Ĝ@0��@0Q�@0  @/��@/�P@/;d@.��@.�R@.E�@.@-@-O�@,�@,z�@,Z@,9X@+��@+ƨ@+��@+"�@*��@*�\@*=q@*�@)�#@)�7@)X@)G�@)7L@)%@(��@(  @'��@'��@'��@'��@'��@'|�@'+@&�R@&v�@&@%p�@%`B@%O�@%?}@$��@$�/@$�j@$�@$z�@$I�@$I�@$I�@#ƨ@#dZ@#"�@"�H@"�\@"J@!��@!X@!7L@!�@ �@ A�@�;@\)@;d@+@+@�@�@
=@��@�@��@V@$�@�T@�-@��@?}@�j@�D@I�@1@�
@�@dZ@dZ@S�@"�@�\@^5@=q@�@�#@��@�7@x�@x�@x�@hs@7L@&�@�`@�9@�u@bN@bN@bN@Q�@A�@A�@A�@A�@A�@A�@ �@��@��@l�@K�@+@��@�@�R@ff@E�@5?@5?@5?@$�@{@�@�h@�@/@�@�/@�/@�/@�@j@9X@�@1@��@��@�
@�@dZ@S�@S�@o@�H@��@M�@��@��@�@�@��@��@�@��@��@��@X@��@��@Ĝ@�9@��@�u@1'@�;@�w@|�@;d@;d@;d@;d@�@
=@��@�y@�y@�@��@��@�+@ff@5?@�T@�h@p�@`B@`B@O�@/@�/@�@��@�D@j@I�@9X@(�@�@�@1@��@�m@ƨ@�F@�F@�F@��@t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ffA�M�A�33A�+A�$�A�$�A�$�A��A��A��A��A��A�{A�{A�oA��/A�jA�{A�ĜAٗ�A���A�dZA��yA��TA�A�A���A��HAՓuA�G�A�&�A�^5A��A��#Aϕ�A�I�Aɟ�A�
=A�dZA�l�A��-A�^5A�S�A��/A��A�?}A�{A�`BA�7LA���A�ĜA�jA�^5A�\)A�E�A���A��+A��
A�VA�(�A��A��A�E�A�{A���A���A�$�A�oA�A��A��A�ȴA��hA���A�x�A��+A�A�C�A��A�bA��A�t�A�9XA�G�A�%A�I�A�1A���A��jA���A��mA�S�A�p�A��FA�
=A��!A�~�A�E�A�A���A��+A���A�x�A��TA��!A�A���A�-A~�uA}�;A|��Ax�uAu��As?}Aq�^Ap  An��Am��AlA�Aj�!Ai�;Ai
=Ag��AfjAc�;Ab=qA`�A^v�A\ĜAZ1AX�+AV��AR�HAQ\)AP1AO33AM�AKp�AI��AG;dADZABĜAA��A@A�A?�;A=��A<ȴA<��A<bA:��A:�A9��A8~�A7��A5\)A3�A2�HA2n�A1��A1�A0��A/|�A-S�A+�A*1'A(��A'�A&A�A%�7A$E�A#�;A#&�A"�A"�jA"jA!ƨA!�A �uAAoA��A��A&�A  Ap�AjA��A��AQ�A�A�uA�
A�A��A�A��A�AE�A��A{AVAI�AJA\)A�RAr�A1A	G�A~�A�A�!AbNAI�A�AȴA��A��@��
@��#@�hs@���@�x�@�@��;@��
@��\@��m@�
=@�E�@��@�x�@��@�o@���@�1@�33@陚@�9X@�M�@��@䛦@�\)@�-@�&�@�bN@�A�@�(�@�~�@�/@�I�@�l�@���@���@���@ؓu@�I�@�+@�n�@�@�%@�z�@�dZ@��@�X@�  @��@ͺ^@�V@͡�@́@�?}@�ȴ@Ο�@Ώ\@�~�@͑h@�Z@�r�@���@�b@�dZ@��H@Ɂ@���@��y@Ƨ�@ÍP@���@�9X@���@��\@��@�X@��@��@��D@��/@��j@�dZ@��!@��\@�@���@�X@�7L@��`@�I�@��@���@�+@��@�5?@�j@�S�@���@�9X@�ƨ@���@�v�@��y@��@�v�@�E�@�-@�{@�@���@�`B@�?}@��@��@�A�@��F@�b@��;@��@�S�@�33@�
=@�n�@�n�@���@�$�@�?}@��@�  @��w@��;@�I�@�`B@�/@�7L@���@���@�hs@��@��R@��\@�@��^@�7L@�A�@���@��P@�C�@�n�@�=q@�n�@�o@�+@�;d@�dZ@��w@�ƨ@���@�;d@��@�
=@�ff@�@�7L@��@�%@�O�@�O�@�Z@���@�K�@��@�E�@�M�@��@�z�@��@�?}@���@�+@���@�{@��@�%@�X@��@��#@��@���@�1'@��;@�\)@��!@���@��@�t�@�ƨ@��;@���@�t�@�K�@�o@���@�ff@�=q@���@�x�@�?}@�%@���@� �@���@�K�@���@��@���@�v�@�V@�J@���@���@�hs@��@���@� �@��
@�l�@�+@��@���@�V@�5?@��@�J@��#@���@�hs@�/@�V@��u@�9X@���@��@�t�@�33@�o@��y@���@��!@�V@���@���@��^@���@�X@�bN@�I�@�I�@�I�@�I�@�1'@�1@���@���@�l�@�;d@�o@��H@���@�ff@�-@��@�@��@�`B@�X@�?}@�%@�Ĝ@���@�Q�@�1@��@��
@���@���@�|�@�S�@�"�@���@���@�M�@�@��^@�hs@��@�A�@��@��\@��#@�@���@�hs@��@���@��j@��D@�9X@�(�@�(�@� �@�b@�@�w@�P@l�@;d@+@~�@~��@~ff@~$�@}�@{ƨ@z��@z=q@y��@y��@yX@yX@yG�@y7L@y&�@x�`@xb@w�@w
=@vE�@v5?@u�T@uO�@tz�@t(�@t1@s�F@s�@so@r=q@q��@qX@q&�@p�9@o�@o;d@o
=@n�y@n��@nE�@m@lZ@k��@k�
@k�@j��@j^5@j�@i�#@i&�@hr�@h �@h �@hb@g��@gK�@fȴ@fV@e�@e/@d��@dZ@cƨ@c33@c@bn�@a�^@ahs@a&�@`��@`��@`b@_�w@_|�@_�@_
=@^ȴ@^ff@^{@]�T@]��@]p�@]�@\�j@\z�@\Z@\9X@\�@[��@["�@Z��@Zn�@Z-@Z�@Y�#@Y��@Y�7@YX@X�`@X��@XA�@X �@W�@W\)@W
=@Vv�@V{@U@U��@U`B@T�/@T�@T�@S��@R�@R��@R�\@Q�@Q��@Qx�@P�`@P�9@P�9@P�9@P�@P  @O�@O|�@Nȴ@M�@M�@MV@M�@L�@L��@L�D@LZ@K�
@K�@K33@K@J�H@J~�@Jn�@J^5@JM�@J=q@J�@I�@I�^@I��@I�7@IG�@I�@H�u@H��@H�@HA�@H1'@H1'@H  @G��@G+@F��@E@E`B@D��@DI�@D�@C�
@C�F@C��@CdZ@CS�@C33@B��@BM�@AG�@@�9@@��@@�@@b@?�@?��@?�P@?K�@?+@>��@>��@>V@=��@=�@=V@<��@<�@;ƨ@;�@;S�@:�H@:�!@:^5@:J@9�#@9�^@9x�@97L@8Ĝ@8bN@8bN@8A�@8  @7�;@7�w@7\)@7+@6�@6ȴ@6�+@6ff@5@5V@4��@4�@4j@4I�@4�@3��@3o@2�\@2�@1��@1�7@1hs@1&�@0�`@0Ĝ@0��@0Q�@0  @/��@/�P@/;d@.��@.�R@.E�@.@-@-O�@,�@,z�@,Z@,9X@+��@+ƨ@+��@+"�@*��@*�\@*=q@*�@)�#@)�7@)X@)G�@)7L@)%@(��@(  @'��@'��@'��@'��@'��@'|�@'+@&�R@&v�@&@%p�@%`B@%O�@%?}@$��@$�/@$�j@$�@$z�@$I�@$I�@$I�@#ƨ@#dZ@#"�@"�H@"�\@"J@!��@!X@!7L@!�@ �@ A�@�;@\)@;d@+@+@�@�@
=@��@�@��@V@$�@�T@�-@��@?}@�j@�D@I�@1@�
@�@dZ@dZ@S�@"�@�\@^5@=q@�@�#@��@�7@x�@x�@x�@hs@7L@&�@�`@�9@�u@bN@bN@bN@Q�@A�@A�@A�@A�@A�@A�@ �@��@��@l�@K�@+@��@�@�R@ff@E�@5?@5?@5?@$�@{@�@�h@�@/@�@�/@�/@�/@�@j@9X@�@1@��@��@�
@�@dZ@S�@S�@o@�H@��@M�@��@��@�@�@��@��@�@��@��@��@X@��@��@Ĝ@�9@��@�u@1'@�;@�w@|�@;d@;d@;d@;d@�@
=@��@�y@�y@�@��@��@�+@ff@5?@�T@�h@p�@`B@`B@O�@/@�/@�@��@�D@j@I�@9X@(�@�@�@1@��@�m@ƨ@�F@�F@�F@��@t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B6FB6FB5?B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB6FBB�B��B��BBJB
=B+BB�B�B�B��B  BBB$�B>wB@�B;dBS�Bs�B��B�BB�#B�TB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��BBB��B�B�fB�ZB�yB�TB�)B�#B�ZB�B�)B��B�^B�BiyBn�BZBcTBiyBbNBS�BL�B8RB'�B�B��B��B�qB�B�bBo�B^5BD�B1'BoB
��B
��B
�B
�yB
�;B
ÖB
�'B
��B
��B
��B
�DB
�B
}�B
w�B
p�B
l�B
k�B
T�B
F�B
7LB
0!B
%�B
�B
{B
PB
B	��B	��B	�B	�ZB	��B	ÖB	�XB	��B	��B	�1B	{�B	o�B	\)B	M�B	D�B	>wB	9XB	(�B	"�B	uB	1B��B��B��B��B��B��B��B�B�B�B�sB�TB�HB�#B��B��B��B��B��BƨBB�jB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�\B�hB�1B�+B�B�B�B� B� B}�B}�Bz�By�Bv�Bt�Bu�B{�B�B�{B�B�LB�^B�wBƨB��BɺBǮB�dB��B��B��B�uB�uB�hB�uB�uB��B�oB�DB�=B�\B�uB�PB�PB�\B�VB�DB�JB�\B�\B�\B�PB�DB�PB�uB��B��B��B�bB�\B�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�FB�dB��BƨBȴBɺBǮBÖBŢBȴB��B�#B�)B�B�B��B��B��B��B��B	+B		7B	
=B	JB	\B	uB	�B	�B	�B	bB	bB	{B	�B	�B	�B	�B	�B	"�B	+B	/B	/B	49B	5?B	8RB	8RB	?}B	A�B	B�B	D�B	F�B	G�B	H�B	H�B	I�B	L�B	M�B	T�B	W
B	W
B	W
B	]/B	cTB	hsB	iyB	n�B	o�B	o�B	r�B	t�B	v�B	z�B	� B	�B	}�B	{�B	� B	� B	� B	~�B	�B	�B	�B	�%B	�DB	�=B	�1B	�+B	�B	�B	�+B	�DB	��B	��B	��B	��B	��B	�{B	�JB	�\B	�bB	�hB	�hB	�hB	�bB	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	�B	�B	�'B	�3B	�?B	�XB	�XB	�^B	�XB	�XB	�dB	�qB	��B	ÖB	ƨB	ĜB	��B	B	ƨB	ǮB	��B	��B	ȴB	ɺB	��B	�B	�/B	�)B	�#B	�#B	�)B	�5B	�`B	�`B	�ZB	�ZB	�TB	�NB	�NB	�TB	�ZB	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
VB
\B
\B
\B
\B
\B
hB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
;dB
;dB
<jB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
@�B
@�B
@�B
A�B
B�B
B�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
S�B
T�B
T�B
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
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
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
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
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
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
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
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
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
t�B
t�B
u�B
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
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
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
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B6zB6zB5ZB6FB6FB6FB6`B6FB6FB6FB6FB6`B6`B7�B7�BFYB�WB��B�B�BxB�B�B��B�5B�B��B �BMB�B'�BAUBD�BBABYBy	B��B�oBǮB��B�zB�CB�B�"B �B�B��B�qB�cB��B�B�B�qB��B��B �BMB�B�jB�B��B�B�"B�B�VBݲB�LBچB�jB��B�]B��Bl"BpoB[�Bd�Bj�Bc�BU�BO�B;dB+B"�B��B�[B� B�iB�aBraBa�BG+B4nB�B
��B
�tB
��B
�B
� B
�tB
��B
�B
��B
�B
��B
�9B
HB
y�B
rB
o B
o�B
X_B
IlB
9XB
2-B
'�B
/B
9B
�B
3B	�.B	�lB	��B	�RB	�B	��B	�B	��B	��B	��B	~�B	s�B	^B	OvB	FB	@�B	<PB	+�B	%�B	�B	
#B	 �B�wB��B�"B�B�FB��B�B�[B��B�B�B��B�/B��BѝBϑB͟B�0BȚB�9B�wB�RB��B��B��B�$B�LB��B��B�!B�B�IB��B��B�B��B��B��B�@B�,B��B�1B�tB�B�B�B� BcB~�B{�B{�Bw�Bu�Bw�B|jB�GB�uB��B��B��B�}B�zB˒B�DB�)B�wB��B�7B�B��B��B��B��B�FB��B�uB��B�rB�B�B��B��B�}B��B��B��B��B��B�bB�"B�dB�pB�FB��B��B��B� B�.B�[B�FB�B��B��B�B��B��B�vB�BB�\B�|B�nB� B�RB��B��B��B��B�B�AB�EBɺBʌB�fBāB��BȀB�B��BۦB�B��B�B�ZB�rB��B��B	�B		�B	
�B	PB	HB	,B	?B	yB	
B	B	�B	B	B	B	B	�B	�B	"�B	+QB	/�B	/�B	4nB	5�B	8�B	88B	?�B	A�B	B�B	D�B	F�B	HB	IB	IlB	J�B	M6B	MPB	UMB	WsB	W�B	W$B	]B	cnB	h�B	i�B	n�B	o�B	o�B	r�B	t�B	v�B	z�B	�OB	��B	~BB	{�B	� B	�B	�B	.B	�AB	�[B	�B	�?B	��B	��B	��B	�_B	�B	��B	��B	��B	��B	��B	�B	�~B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�,B	�WB	��B	��B	��B	��B	�'B	�MB	��B	�rB	��B	��B	��B	��B	�B	�qB	�iB	��B	�EB	�B	��B	��B	�B	��B	�)B	�DB	ȚB	�B	�B	��B	ݘB	ܒB	�qB	�=B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�TB	�B	�*B	�}B	�B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�<B	�BB	�HB
 4B
 4B
B
 B
 B
;B
AB
AB
AB
GB
aB
GB
gB
SB
mB
YB
_B
_B
EB
KB
KB
1B
KB
	lB
	lB
	lB

rB

�B
xB
xB
xB
xB
~B
JB
jB
jB
�B
�B
pB
vB
vB
�B
�B
�B
hB
NB
NB
hB
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
+B
B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
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
!B
 �B
!�B
!�B
!�B
!�B
"B
#B
#�B
#�B
$B
$&B
$�B
%�B
%�B
&B
&B
&2B
'RB
)B
(�B
)*B
)DB
*B
*B
*0B
*0B
+6B
,"B
+�B
,"B
,B
,=B
-CB
-CB
-CB
-)B
-)B
-CB
-CB
./B
.B
./B
./B
/B
/5B
0;B
0UB
0UB
0;B
0;B
1AB
1AB
1AB
1[B
2GB
2-B
3MB
3MB
3MB
33B
3MB
3MB
3MB
3MB
3hB
4nB
5ZB
5ZB
6FB
6`B
6`B
7fB
7fB
7fB
7�B
8lB
8lB
8lB
8�B
8�B
9rB
9rB
9XB
9rB
9XB
9XB
9rB
:xB
;�B
;B
;�B
;B
;B
<�B
;B
;dB
;B
<�B
<jB
=qB
=�B
=�B
=�B
=�B
=�B
>�B
>�B
@iB
@�B
@�B
A�B
B�B
B�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
C{B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
C{B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
MB
L�B
MB
M�B
MB
M�B
M�B
M�B
NB
N�B
N�B
O�B
O�B
O�B
O�B
O�B
QB
Q B
P�B
Q B
Q B
Q B
RB
RB
RB
RB
RB
SB
R�B
S&B
S&B
TB
TB
TB
T�B
TB
U2B
UB
V9B
V9B
V9B
W
B
W$B
W$B
W$B
X+B
XB
X+B
X+B
X+B
X+B
XB
Y1B
YB
YKB
Y1B
Z7B
Z7B
Z7B
[#B
[#B
[=B
[=B
[=B
[=B
[WB
\)B
\CB
]/B
]IB
]IB
]IB
^OB
^5B
^OB
^OB
^jB
_pB
_;B
_;B
_;B
_;B
`BB
`BB
`\B
`vB
`\B
`vB
`vB
aHB
a-B
abB
aHB
abB
aHB
abB
abB
abB
bNB
bhB
b�B
bNB
cTB
cnB
c�B
cnB
d�B
dtB
dZB
dtB
d�B
ezB
e�B
e�B
f�B
ffB
fLB
ffB
ffB
fLB
ffB
f�B
g�B
gmB
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
iyB
iyB
i�B
iyB
i�B
iyB
i�B
j�B
j�B
j�B
jeB
j�B
j�B
jeB
jB
jB
jB
j�B
k�B
k�B
l�B
l�B
l�B
lqB
l�B
l�B
mwB
m�B
m�B
m�B
mwB
mwB
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n}B
n}B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
p�B
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
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
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
t�B
t�B
u�B
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
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
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
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607300036372016073000363720160730003637201806221259542018062212595420180622125954201804050658392018040506583920180405065839  JA  ARFMdecpA19c                                                                20160726093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160726003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160726003524  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160726003524  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160726003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160726003525  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160726003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160726003525  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160726003525  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160726003526                      G�O�G�O�G�O�                JA  ARUP                                                                        20160726011748                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160726153323  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20160729153637  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160729153637  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215839  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622035954  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201517                      G�O�G�O�G�O�                