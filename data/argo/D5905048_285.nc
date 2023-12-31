CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-26T00:35:34Z creation;2018-09-26T00:35:39Z conversion to V3.1;2019-12-19T07:27:59Z update;     
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
resolution        =���   axis      Z        X  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oT   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  s,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     X  �l   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �L   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �TArgo profile    3.1 1.2 19500101000000  20180926003534  20200116231516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_285                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؄5��� 1   @؄6��O�@4���U�=�dX1���.1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D���D�<�D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ Dؼ�D���D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�C3D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B �\B'B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
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
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D|)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�>D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�>D�~D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD��D�>D́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HDؾD��D�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�{D�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�{D�D{D��{D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AݬAݕ�A݉7A݅A�z�A�x�A�v�A�t�A�r�A�r�A�r�A�p�A�p�A�r�A�r�A�p�A�r�A�hsA��Aں^A�I�AՑhA�A�A�1A��A�{A�-A��#Aº^A��A��A�A���A���A�oA�K�A��wA�/A��^A�hsA�\)A��A��!A�-A��A���A�jA�/A��A�JA��HA�1'A��PA�bA�~�A��wA� �A���A�A���A�p�A��A�dZA��A��/A�A���A��A�p�A���A�(�A���A�I�A�A���A��9A��9A��A���A���A��A�~�A�r�A��wA���A��yA��A���A�K�A��7A�ƨA�
=A���A�oA��+A�I�A��DA�|�A��-A�\)A��-A�?}A��7A���A�S�A/A}C�Az��Aw"�At��Ar��An��AlAkAi��Ac7LAa��A`�yA]��AZ$�ATAR�AN�AL�ALbNAJȴAIdZAG�-AE�-ADv�AB��AA��AA"�A@�\A?�wA>��A=&�A;�A;`BA;
=A:�HA:�!A:  A8�RA89XA7p�A5��A4r�A2bNA/�A-��A+��A+`BA*��A)��A(�A'33A&��A%�A%�A$-A$�A#��A"bNA!�A �+A  �A�;A��A�A`BAVAI�A��A��AĜA�AM�A|�A��A`BAVA�A�7AdZA~�AO�A��A5?A$�Az�A�A��A�#A��A�wA�A
=qA	�A
{A	��A	XA��Ax�A�DAffA�TA r�@�x�@�&�@�dZ@�\@���@��;@�@�@��@�9X@��m@�+@�Ĝ@�J@�I�@�t�@�M�@�/@��@�A�@�b@��@ݲ-@��@�bN@��@ۍP@�C�@۝�@�|�@���@�hs@�`B@�bN@��@�X@�r�@���@�  @�1@��@�n�@��T@�V@���@�r�@�  @�{@��@�X@���@�j@Ɨ�@�%@�z�@�b@å�@�\)@�S�@�S�@�S�@�C�@�@�^5@�{@��T@���@��@���@�l�@�A�@�%@���@���@��j@��R@�
=@�1'@�\)@�"�@��@�$�@���@��@��w@��@��!@�ff@�@�7L@��@�%@��u@��@��@�@���@�=q@��@�x�@�G�@��@���@���@���@�j@�  @��@��@�~�@��@��-@��@��/@���@�Z@� �@�9X@�Z@�b@��m@���@���@�l�@�o@�ȴ@��!@�V@��@���@���@�X@�V@��j@��@�(�@�t�@��@��@��R@���@���@���@�^5@�5?@��@���@��7@�7L@��@���@���@�bN@�  @��F@�t�@�S�@�C�@���@��\@�^5@�E�@��@��#@��-@��-@���@��@���@���@�r�@�Z@� �@��;@��w@��@�C�@�@��R@��\@�M�@�J@���@���@�hs@�O�@�?}@�?}@�&�@���@��D@�A�@��@�1@��
@���@�|�@�;d@��H@��!@��+@�ff@�5?@��@�@�X@�%@��`@�Ĝ@��u@�r�@�9X@�1@���@��@�;d@�ȴ@��\@�ff@�ff@�^5@�V@�5?@�{@���@�J@�J@��#@�?}@��@���@�z�@��9@��
@�dZ@�C�@��;@��w@�S�@���@���@�v�@�~�@�n�@�5?@��@��#@���@��-@��9@�r�@��@� �@���@�"�@���@��\@�-@��@��T@��^@�X@��j@��@�j@�9X@���@�C�@��@���@��H@���@�V@�@��@��@���@�{@��#@���@�X@���@���@�r�@��@��@��m@�ƨ@��P@�S�@�K�@�"�@��@�
=@�@���@�-@���@���@�G�@��@��@���@�Ĝ@��9@���@��u@�z�@�Q�@�b@�  @�  @�;@|�@
=@~��@~v�@~5?@}�@}�h@}/@}?}@}�@}V@|�/@|�j@|��@|��@|Z@|(�@|(�@|1@{�
@{t�@{33@z�H@z��@zn�@y��@y��@yx�@y&�@xbN@w�;@w��@v��@v5?@v{@u�T@u��@t��@t1@s�
@s��@so@s�@s��@s��@rn�@q��@qx�@qG�@p��@pb@o��@nȴ@n{@m�-@mp�@l�j@lz�@l��@l�@lj@lj@l(�@k��@kS�@j~�@iX@i�@h�`@h�`@h�u@hQ�@g�@gK�@g+@fff@e@e��@e�@e�@e�@e�@dz�@c�
@c��@c��@c33@b�H@b-@a�^@ahs@a&�@`�`@`bN@_�;@_|�@_K�@^��@^ff@^@]@]?}@\�@\��@\��@\9X@\1@[�F@[S�@Z��@Z^5@ZJ@Yhs@X�9@X�u@Xr�@XA�@W|�@W\)@W�@V�R@VV@U�@U�@T��@T�D@T1@S��@SS�@S33@So@R��@R��@R-@Q�@QX@Q�@P��@P�u@P�u@PbN@PbN@PA�@PQ�@O�@O��@O;d@O+@O�@O
=@N�@N��@Nv�@N5?@M�T@M��@M�-@M�-@M�h@MO�@L��@L��@L�D@Lj@K��@K�F@Ko@Jn�@I%@HQ�@G�@G�w@G��@G|�@G\)@F�@Fv�@F{@E�T@E��@E/@EV@D�/@D��@D9X@C��@B��@B��@B�!@B�!@B�!@B=q@A��@AG�@A�@A�@A�@@�`@@�@?�@?�@>�@>��@>ff@>5?@>5?@>5?@=�@=�h@=�@=�@=`B@<��@<I�@<(�@;�m@;ƨ@;�@;S�@;@:n�@9�#@9��@9��@9��@9�7@9�7@9X@9�@8��@8Q�@7�@7�;@7�P@6��@6�@6ȴ@6ff@5@5?}@4�/@4��@4(�@3�m@3dZ@2�@2��@2�!@2~�@2n�@2-@1�7@1�@0Ĝ@01'@/�;@/�P@/;d@.��@.�@.�R@.��@.��@.V@.$�@.@-�T@-�@-�@,�/@,�j@,z�@,9X@,�@+�m@+ƨ@+��@+33@*�@*�H@*��@*��@*^5@*-@)��@)��@)��@)hs@(��@(�@(r�@(Q�@(1'@( �@(  @'�w@'�P@'\)@'+@'+@'�@&ȴ@&ȴ@&ȴ@&��@&�+@&�+@&�+@&V@&{@&@&@%�@%�T@%@%@%�@$��@$��@$z�@$j@#�m@#�F@#33@"��@"~�@"=q@"�@!�@!�#@!��@!��@!X@!�@!%@ �`@ Ĝ@ �9@ �@ Q�@�w@\)@K�@+@��@ȴ@�R@��@��@�+@ff@ff@V@{@��@`B@/@��@�@�D@z�@Z@j@Z@(�@��@��@ƨ@33@�H@n�@�@�@��@��@x�@hs@G�@��@��@Ĝ@�u@1'@  @�@�w@��@|�@\)@+@�y@�R@�+@v�@E�@{@�T@��@@��@�@V@��@�D@�D@I�@�
@�F@��@33@@�H@�!@~�@M�@�@�^@��@�7@X@G�@7L@�@%@�9@�u@r�@r�@r�@b@�@�;@��@��@l�@+@�@�R@��@{@�@�-@`B@O�@O�@O�@O�@?}@?}@?}@/@V@V@��@�@��@�D@Z@9X@�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AݬAݕ�A݉7A݅A�z�A�x�A�v�A�t�A�r�A�r�A�r�A�p�A�p�A�r�A�r�A�p�A�r�A�hsA��Aں^A�I�AՑhA�A�A�1A��A�{A�-A��#Aº^A��A��A�A���A���A�oA�K�A��wA�/A��^A�hsA�\)A��A��!A�-A��A���A�jA�/A��A�JA��HA�1'A��PA�bA�~�A��wA� �A���A�A���A�p�A��A�dZA��A��/A�A���A��A�p�A���A�(�A���A�I�A�A���A��9A��9A��A���A���A��A�~�A�r�A��wA���A��yA��A���A�K�A��7A�ƨA�
=A���A�oA��+A�I�A��DA�|�A��-A�\)A��-A�?}A��7A���A�S�A/A}C�Az��Aw"�At��Ar��An��AlAkAi��Ac7LAa��A`�yA]��AZ$�ATAR�AN�AL�ALbNAJȴAIdZAG�-AE�-ADv�AB��AA��AA"�A@�\A?�wA>��A=&�A;�A;`BA;
=A:�HA:�!A:  A8�RA89XA7p�A5��A4r�A2bNA/�A-��A+��A+`BA*��A)��A(�A'33A&��A%�A%�A$-A$�A#��A"bNA!�A �+A  �A�;A��A�A`BAVAI�A��A��AĜA�AM�A|�A��A`BAVA�A�7AdZA~�AO�A��A5?A$�Az�A�A��A�#A��A�wA�A
=qA	�A
{A	��A	XA��Ax�A�DAffA�TA r�@�x�@�&�@�dZ@�\@���@��;@�@�@��@�9X@��m@�+@�Ĝ@�J@�I�@�t�@�M�@�/@��@�A�@�b@��@ݲ-@��@�bN@��@ۍP@�C�@۝�@�|�@���@�hs@�`B@�bN@��@�X@�r�@���@�  @�1@��@�n�@��T@�V@���@�r�@�  @�{@��@�X@���@�j@Ɨ�@�%@�z�@�b@å�@�\)@�S�@�S�@�S�@�C�@�@�^5@�{@��T@���@��@���@�l�@�A�@�%@���@���@��j@��R@�
=@�1'@�\)@�"�@��@�$�@���@��@��w@��@��!@�ff@�@�7L@��@�%@��u@��@��@�@���@�=q@��@�x�@�G�@��@���@���@���@�j@�  @��@��@�~�@��@��-@��@��/@���@�Z@� �@�9X@�Z@�b@��m@���@���@�l�@�o@�ȴ@��!@�V@��@���@���@�X@�V@��j@��@�(�@�t�@��@��@��R@���@���@���@�^5@�5?@��@���@��7@�7L@��@���@���@�bN@�  @��F@�t�@�S�@�C�@���@��\@�^5@�E�@��@��#@��-@��-@���@��@���@���@�r�@�Z@� �@��;@��w@��@�C�@�@��R@��\@�M�@�J@���@���@�hs@�O�@�?}@�?}@�&�@���@��D@�A�@��@�1@��
@���@�|�@�;d@��H@��!@��+@�ff@�5?@��@�@�X@�%@��`@�Ĝ@��u@�r�@�9X@�1@���@��@�;d@�ȴ@��\@�ff@�ff@�^5@�V@�5?@�{@���@�J@�J@��#@�?}@��@���@�z�@��9@��
@�dZ@�C�@��;@��w@�S�@���@���@�v�@�~�@�n�@�5?@��@��#@���@��-@��9@�r�@��@� �@���@�"�@���@��\@�-@��@��T@��^@�X@��j@��@�j@�9X@���@�C�@��@���@��H@���@�V@�@��@��@���@�{@��#@���@�X@���@���@�r�@��@��@��m@�ƨ@��P@�S�@�K�@�"�@��@�
=@�@���@�-@���@���@�G�@��@��@���@�Ĝ@��9@���@��u@�z�@�Q�@�b@�  @�  @�;@|�@
=@~��@~v�@~5?@}�@}�h@}/@}?}@}�@}V@|�/@|�j@|��@|��@|Z@|(�@|(�@|1@{�
@{t�@{33@z�H@z��@zn�@y��@y��@yx�@y&�@xbN@w�;@w��@v��@v5?@v{@u�T@u��@t��@t1@s�
@s��@so@s�@s��@s��@rn�@q��@qx�@qG�@p��@pb@o��@nȴ@n{@m�-@mp�@l�j@lz�@l��@l�@lj@lj@l(�@k��@kS�@j~�@iX@i�@h�`@h�`@h�u@hQ�@g�@gK�@g+@fff@e@e��@e�@e�@e�@e�@dz�@c�
@c��@c��@c33@b�H@b-@a�^@ahs@a&�@`�`@`bN@_�;@_|�@_K�@^��@^ff@^@]@]?}@\�@\��@\��@\9X@\1@[�F@[S�@Z��@Z^5@ZJ@Yhs@X�9@X�u@Xr�@XA�@W|�@W\)@W�@V�R@VV@U�@U�@T��@T�D@T1@S��@SS�@S33@So@R��@R��@R-@Q�@QX@Q�@P��@P�u@P�u@PbN@PbN@PA�@PQ�@O�@O��@O;d@O+@O�@O
=@N�@N��@Nv�@N5?@M�T@M��@M�-@M�-@M�h@MO�@L��@L��@L�D@Lj@K��@K�F@Ko@Jn�@I%@HQ�@G�@G�w@G��@G|�@G\)@F�@Fv�@F{@E�T@E��@E/@EV@D�/@D��@D9X@C��@B��@B��@B�!@B�!@B�!@B=q@A��@AG�@A�@A�@A�@@�`@@�@?�@?�@>�@>��@>ff@>5?@>5?@>5?@=�@=�h@=�@=�@=`B@<��@<I�@<(�@;�m@;ƨ@;�@;S�@;@:n�@9�#@9��@9��@9��@9�7@9�7@9X@9�@8��@8Q�@7�@7�;@7�P@6��@6�@6ȴ@6ff@5@5?}@4�/@4��@4(�@3�m@3dZ@2�@2��@2�!@2~�@2n�@2-@1�7@1�@0Ĝ@01'@/�;@/�P@/;d@.��@.�@.�R@.��@.��@.V@.$�@.@-�T@-�@-�@,�/@,�j@,z�@,9X@,�@+�m@+ƨ@+��@+33@*�@*�H@*��@*��@*^5@*-@)��@)��@)��@)hs@(��@(�@(r�@(Q�@(1'@( �@(  @'�w@'�P@'\)@'+@'+@'�@&ȴ@&ȴ@&ȴ@&��@&�+@&�+@&�+@&V@&{@&@&@%�@%�T@%@%@%�@$��@$��@$z�@$j@#�m@#�F@#33@"��@"~�@"=q@"�@!�@!�#@!��@!��@!X@!�@!%@ �`@ Ĝ@ �9@ �@ Q�@�w@\)@K�@+@��@ȴ@�R@��@��@�+@ff@ff@V@{@��@`B@/@��@�@�D@z�@Z@j@Z@(�@��@��@ƨ@33@�H@n�@�@�@��@��@x�@hs@G�@��@��@Ĝ@�u@1'@  @�@�w@��@|�@\)@+@�y@�R@�+@v�@E�@{@�T@��@@��@�@V@��@�D@�D@I�@�
@�F@��@33@@�H@�!@~�@M�@�@�^@��@�7@X@G�@7L@�@%@�9@�u@r�@r�@r�@b@�@�;@��@��@l�@+@�@�R@��@{@�@�-@`B@O�@O�@O�@O�@?}@?}@?}@/@V@V@��@�@��@�D@Z@9X@�@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
p�B
q�B
s�B
r�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
w�B
y�B
�JB�B^5BffBR�B��B�}B�VB��B�5B�B/B$�B%BVB,BT�BXBdZBiyBk�BXBk�B� B�+B�=B�hB�uB��B�hB�+Bs�BdZBffBbNBVB@�BO�BO�BL�BE�B@�BC�BA�B7LB.B(�B�B%BJBJBB��B�B�TBǮBB�B��B�=Bo�Bl�B_;BE�B�B
�ZB
��B
��B
�NB
�)B
�TB
�TB
�;B
�#B
�5B
��B
ĜB
��B
��B
�{B
�DB
�PB
{�B
`BB
[#B
YB
;dB
(�B
&�B
oB	��B	��B	�NB	�}B	��B	�FB	��B	[#B	R�B	_;B	7LB	uB��BɺB�B�FB�qB�?B��B��B��B�-B�B�dBBB�jB�?B�'B�3BĜBĜBŢB��B�XB�B�^B�!B��B��B��B�bB��B��B�3B�9B�B�B��B�3B�-B�B��B�!B�B��B��B��B��B��B��B��B��B�uB�%B��B��B�%B�PB�B�B�B�B�%B�oB�{B��B�hB�bB��B�B�9B�}BĜB��B��B��B�B�B��B�/B�fB�`B�HB�5B��BɺB�B��B��B�oB}�Bo�B�%B�=B�B�VB�B�DB�7B�=B~�Bt�Bm�Bo�Bw�Bv�Bw�B|�B~�B~�Bw�Bx�B� B�B�=B�1B�PB��B��B��B��B��B��B��B��B�'B�-B�dB�jB�RB�^BÖBÖB��B��BƨB�XB�B�B�LB�3B��B�B�XB�XB�dB�jB��B��B�}B�qB�^B�wB�}B�wB�}BB��B�5B�mB�B��B�B�sB�/B��B��B�/B�NB�NB�NB�`B�TB�`B�B�B��B��B��B��B	  B��B��B	B	+B	JB	VB	oB	�B	�B	�B	�B	 �B	 �B	"�B	#�B	&�B	%�B	%�B	'�B	-B	.B	7LB	:^B	<jB	>wB	C�B	E�B	F�B	I�B	J�B	I�B	I�B	L�B	P�B	W
B	XB	]/B	`BB	cTB	cTB	e`B	gmB	iyB	jB	jB	l�B	p�B	s�B	v�B	x�B	w�B	w�B	y�B	z�B	~�B	~�B	�B	�B	�1B	�1B	�7B	�=B	�PB	�bB	�oB	�uB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�'B	�3B	�?B	�LB	�RB	�^B	�dB	�dB	�^B	�XB	�jB	�qB	�}B	��B	��B	��B	B	��B	��B	ĜB	ĜB	ŢB	ĜB	ŢB	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�B	�B	�/B	�B	�#B	�HB	�mB	�mB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B	��B	��B	��B	��B
  B
  B
B
B
%B
+B
+B
1B
1B
1B
+B
+B
	7B

=B

=B
	7B
	7B
	7B
DB
JB
JB
JB
JB
\B
VB
\B
\B
bB
bB
oB
uB
uB
{B
uB
uB
oB
uB
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
!�B
 �B
 �B
�B
�B
#�B
$�B
$�B
#�B
#�B
"�B
#�B
#�B
"�B
#�B
&�B
&�B
%�B
(�B
(�B
&�B
&�B
'�B
)�B
'�B
&�B
&�B
&�B
(�B
(�B
(�B
'�B
'�B
(�B
)�B
(�B
'�B
(�B
)�B
)�B
+B
-B
,B
,B
-B
,B
,B
,B
,B
-B
-B
-B
0!B
0!B
/B
-B
0!B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
2-B
33B
49B
33B
33B
2-B
33B
33B
49B
49B
6FB
7LB
6FB
7LB
7LB
8RB
7LB
7LB
8RB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
=qB
=qB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
;dB
9XB
:^B
<jB
>wB
>wB
>wB
@�B
>wB
?}B
@�B
A�B
C�B
B�B
C�B
B�B
C�B
C�B
B�B
C�B
F�B
H�B
G�B
G�B
E�B
D�B
F�B
F�B
H�B
G�B
F�B
E�B
E�B
E�B
F�B
G�B
H�B
I�B
I�B
I�B
H�B
H�B
J�B
J�B
I�B
H�B
G�B
J�B
J�B
K�B
J�B
J�B
J�B
J�B
J�B
M�B
N�B
O�B
O�B
N�B
N�B
N�B
N�B
N�B
O�B
Q�B
P�B
O�B
Q�B
Q�B
P�B
O�B
P�B
Q�B
R�B
Q�B
R�B
R�B
R�B
T�B
T�B
T�B
T�B
S�B
Q�B
R�B
S�B
S�B
T�B
VB
VB
W
B
W
B
XB
XB
YB
XB
XB
YB
YB
XB
XB
YB
ZB
ZB
YB
[#B
ZB
[#B
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
\)B
\)B
[#B
]/B
^5B
^5B
^5B
_;B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
_;B
`BB
aHB
`BB
`BB
aHB
aHB
`BB
_;B
aHB
aHB
aHB
aHB
aHB
`BB
`BB
^5B
`BB
aHB
aHB
`BB
aHB
`BB
aHB
cTB
cTB
dZB
dZB
e`B
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
cTB
e`B
gmB
gmB
ffB
gmB
gmB
hsB
hsB
hsB
gmB
hsB
gmB
ffB
ffB
gmB
hsB
hsB
hsB
iyB
iyB
jB
jB
iyB
iyB
iyB
iyB
hsB
ffB
hsB
hsB
jB
k�B
k�B
k�B
k�B
l�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
m�B
l�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
n�B
m�B
n�B
p�B
p�B
o�B
n�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
r�B
s�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
s�B
u�B
u�B
u�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
w�B
x�B
x�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
p�B
q�B
s�B
r�B
s�B
s�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
xB
z�B
�}B"�Bc�BmB]IB�B�aB�mB�B�tB�B0�B'�BBoB.�BVBY�Be�Bj�Bl�B[#BmwB��B��B�DB��B�,B�
B� B�fButBf�Bh$Bd@BY1BC�BQ�BQNBN<BG_BBABD�BB�B9	B/�B+B5B
=B<B�B�B��B�B��B�=B��B�;B��B�JBrGBm�B`�BH�B�B
��B
�B
�YB
�B
ބB
�B
�B
��B
��B
�VB
�\B
�B
��B
��B
��B
�B
�VB
}�B
cnB
]/B
[#B
?.B
,�B
)�B
�B	�B	��B	�zB	��B	�qB	�fB	�~B	a�B	U2B	`�B	;�B	_B��B�dB��B��B�wB�LB�B�6B�8B��B�;B��BÖB�aB��B��B��B��B�B�B��B�B�xB��B�B��B�QB��B��B�B��B�B��B�%B�OB��B��B��B�3B�;B�*B�oB��B�tB�BB��B�tB�hB��B��B��B�B��B��B�/B�B��B��B�YB�mB��B�_B�B�2B�B��B��B�|B�cB�TB�cB�B��B��B�:B�yB�	B�,BݘB�fB��B�4B�;BЗB˒B�-B��B��B��B�BsB��B�)B��B��B�SB��B��B��B�Bv+BoOBp�BxlBw�Bx�B}qBHBHBx�By�B��B�{B�rB��B��B��B��B�HB��B�XB��B�IB�hB��B��B�JB��B�	B��B��B�3B�AB��B�_B��B�/B�!B��B��B�QB�B��B��B��B��B��B��B��B��B��B��B��B��B�4B�GB��B��B��B�/B�tB�B�B��B�NB�dB�~B�B�B��B��B�B�B��B��B�B�?B�$B�B	 4B�VB�}B	gB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	"�B	$&B	'B	&fB	&fB	(XB	-CB	.�B	7�B	:�B	<�B	>�B	C{B	E�B	F�B	I�B	J�B	I�B	I�B	MB	Q B	W
B	X_B	]IB	`vB	cnB	c�B	e�B	g�B	i�B	j�B	kB	l�B	p�B	s�B	v�B	x�B	w�B	w�B	zB	{B	B	HB	�[B	�9B	�KB	�fB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�
B	�$B	�*B	�"B	�=B	�"B	�)B	�OB	�GB	�[B	�hB	�tB	�LB	��B	�xB	�dB	�B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	ĶB	ĶB	żB	ĶB	żB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�(B	�.B	�:B	�2B	�
B	�B	�B	�7B	�1B	�1B	�7B	�#B	�=B	�QB	�mB	�7B	�KB	�KB	�/B	ٚB	�qB	�HB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	� B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	��B	��B	�B	�(B	�.B
 B
  B
 B
  B	�B	�BB	�PB	�BB
 4B
 OB
-B
3B
?B
EB
+B
KB
KB
1B
_B
_B
	RB

#B

XB
	lB
	lB
	lB
^B
dB
dB
dB
dB
\B
pB
\B
vB
}B
}B
oB
�B
�B
{B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
~B
 �B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
!�B
 �B
 �B
�B
!B
#�B
$�B
$�B
#�B
#�B
#B
#�B
#�B
# B
$B
&�B
'B
%�B
(�B
)B
'B
'B
'�B
)�B
($B
'B
'B
'B
(�B
)B
)B
($B
($B
)B
*B
)B
(
B
)*B
*B
*0B
+B
-B
,"B
,=B
-)B
,=B
,=B
,=B
,=B
-CB
-CB
-CB
0;B
0;B
/B
-]B
0!B
/5B
/OB
/OB
/5B
/OB
/OB
/OB
0UB
0UB
2GB
3MB
49B
3MB
3MB
2aB
3MB
3hB
4TB
4TB
6+B
72B
6FB
7LB
7fB
8RB
7�B
7fB
8RB
;dB
;B
<jB
<�B
<�B
<�B
<�B
<jB
=qB
=�B
>wB
=�B
=�B
<�B
<�B
<�B
<�B
<�B
<�B
;�B
;�B
9�B
:xB
<�B
>�B
>�B
>�B
@�B
>�B
?�B
@�B
A�B
C�B
B�B
C�B
B�B
C�B
C�B
B�B
C�B
F�B
H�B
G�B
G�B
E�B
D�B
F�B
F�B
H�B
G�B
F�B
E�B
E�B
E�B
F�B
G�B
H�B
I�B
I�B
I�B
H�B
H�B
J�B
J�B
I�B
H�B
G�B
J�B
J�B
K�B
J�B
J�B
J�B
J�B
J�B
M�B
N�B
O�B
O�B
N�B
N�B
N�B
N�B
OB
O�B
RB
Q B
PB
RB
Q�B
QB
P.B
QB
RB
R�B
RB
SB
S&B
S&B
T�B
UB
UB
UB
S�B
R:B
S&B
TB
T,B
T�B
VB
VB
W
B
W$B
X+B
XB
X�B
XB
X+B
Y1B
Y1B
XEB
X+B
Y1B
ZB
Z7B
Y1B
[=B
ZB
[#B
Z7B
ZQB
[=B
\)B
\CB
\CB
\CB
\CB
\CB
\CB
\CB
\CB
[WB
]IB
^B
^OB
^OB
_;B
^5B
^OB
_VB
_VB
_VB
`'B
`BB
_VB
`'B
aHB
`\B
`BB
aHB
aHB
`\B
_VB
aHB
aHB
aHB
a-B
abB
`BB
`\B
^OB
`\B
abB
abB
`vB
aHB
`vB
a|B
cnB
cTB
dtB
dtB
e`B
dtB
dZB
dZB
ezB
e`B
ezB
e`B
ezB
e`B
dtB
c�B
ezB
gmB
g�B
f�B
g�B
gmB
hsB
hXB
hsB
g�B
hsB
g�B
f�B
f�B
g�B
h�B
h�B
h�B
iyB
iyB
jB
jB
iyB
i�B
i�B
iyB
h�B
f�B
h�B
h�B
j�B
k�B
k�B
k�B
k�B
l�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
m�B
l�B
m�B
m�B
m�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
n�B
n�B
m�B
n�B
p�B
p�B
o�B
n�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
s�B
r�B
s�B
s�B
s�B
s�B
r�B
s�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
s�B
u�B
u�B
u�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
v�B
w�B
x�B
x�B
y�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%zx<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809300038092018093000380920180930003809201809300200262018093002002620180930020026201810010026232018100100262320181001002623  JA  ARFMdecpA19c                                                                20180926093525  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180926003534  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180926003537  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180926003537  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180926003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180926003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180926003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180926003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180926003538  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180926003539                      G�O�G�O�G�O�                JA  ARUP                                                                        20180926005607                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180926153840  CV  JULD            G�O�G�O�F�!�                JM  ARCAJMQC2.0                                                                 20180929153809  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180929153809  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180929170026  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180930152623  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231516                      G�O�G�O�G�O�                