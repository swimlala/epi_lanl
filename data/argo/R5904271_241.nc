CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-11-14T11:00:20Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `p   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɸ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ͠   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20191114110020  20191114110020  5904271 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  4744                            2B  A   NAVIS_A                         0188                            052512                          863 @���;*&�1   @�����pr@/p��
=q�dM�-V1   GPS     Primary sampling: mixed [deep: discrete, shallow: continuous]                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`��Bg��Bp  Bx  B�  B�  B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @AG�@��
@��
A�A!�AA�Aa�A���A���A���A���A���A���A���A���B z�Bz�Bz�Bz�B z�B(z�B0z�B8z�B@z�BHz�BPz�BX�GBaG�Bh{Bpz�Bxz�B�=qB�=qB�p�B�p�B�=qB�
>B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C88RC:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D ��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�HD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�@�D���D��
D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D� �D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D� �D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D��
D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D�D���D��D�C�DÃ�D���D��D�C�Dă�D���D��D�C�DŃ�D���D��D�C�Dƃ�D���D��D�C�Dǃ�D���D��D�C�Dȃ�D���D��D�C�DɃ�D���D��D�C�Dʃ�D���D��D�C�D˃�D���D��D�C�D̃�D���D��D�C�D̓�D���D��D�C�D΃�D���D��D�C�Dσ�D���D��D�C�DЃ�D���D��D�C�Dу�D���D��D�C�D҃�D���D��D�C�DӃ�D���D��D�C�Dԃ�D���D��D�C�DՃ�D���D��D�C�Dփ�D���D��D�C�D׃�D���D��D�C�D؃�D���D��D�C�Dك�D���D��D�C�Dڃ�D���D��D�C�Dۃ�D���D��D�C�D܃�D���D��D�C�D݃�D���D��D�C�Dރ�D���D��D�C�D߃�D���D��D�C�D���D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D���D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D��D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�C�D���D���D��D�G
D�}q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��HA��A���A�JA�JA���A���A�$�A�C�A�A�A�-A�?}A�XA�\)A�Q�A�?}A��A�bA�A���A���A���A��A��A���A�;dA��TAٓuA�jA��mA��A՛�A��`A�+Aκ^A��TA���AǁAƺ^A�=qA���A��uA�+A��jA�+A��A�t�A���A�ȴA��A��A�&�A���A���A���A�A�A���A��RA�+A�$�A�\)A���A�v�A�x�A��A���A���A���A�
=A��FA�bA�
=A�A��FA� �A~ZAy�7Au;dAp�AkO�Ai�Ai�^Ah�`Ah^5AfffAd �Ab~�A_O�A\�DAY��AX9XAU�#AT�/AR��APALjAK�AJ(�AI"�AH$�AF��AD��AChsAAdZA?�#A=�A: �A7dZA5l�A4��A4~�A4 �A3;dA2{A1`BA0��A/�A-�^A,VA*�yA)dZA(��A'�^A'%A&�\A&VA&$�A%C�A$ĜA$z�A$ffA$=qA#�A#�-A#�A"v�A ��A   AA�A1A�mA�wA�hA�A��A�A;dA�At�A�A
=Av�A�A�wA�+AVA�mA�uA��A��A%A	��A	\)AVA�^A�hAp�At�Al�A;dA��A �A	hsA	��A	�A	`BAK�AQ�A+A�A��Ap�AĜA�-A�At�A�`A��A��A�A�AO�A E�@��w@�C�@�C�@��@�oA 9XA"�A ĜA �+A ^5A Q�@�~�@�%@�j@���@�/@���@��@��9@��@��y@�$�@��^@�Ĝ@��\@��@��@� �@�@�@�E�@�$�@�J@�@��@���@�h@�Ĝ@�ƨ@@�O�@�j@� �@��m@띲@��@�+@�v�@�5?@陚@�j@�@�"�@�V@�`B@��@�9@� �@��
@�F@�"�@�v�@�@��`@��@�I�@�1@ߕ�@�"�@��@އ+@�5?@ݡ�@�`B@�A�@ە�@�\)@ڸR@�~�@�`B@���@�Q�@��@�|�@և+@�V@��@�hs@�`B@�?}@��@�1@�@��@�/@�Ĝ@Ѓ@�  @ϝ�@�C�@���@Ώ\@�E�@�@Ͳ-@�7L@�Ĝ@�  @˶F@�|�@�~�@�=q@��@�G�@��`@ȴ9@��m@Ə\@���@��`@�bN@��;@���@��
@�l�@�33@���@���@�n�@��@���@�hs@��@���@��@�I�@���@��@�K�@���@���@�v�@��@��@�Ĝ@�Z@���@��P@�K�@�
=@��@��\@�V@���@��-@���@�A�@�dZ@��y@��R@�J@��T@���@�x�@�7L@���@��;@�C�@��+@�V@��@��h@���@���@�I�@�  @��
@���@�
=@�=q@�{@�{@�{@���@�p�@��@�bN@���@�t�@�"�@��y@��\@���@��h@�hs@�%@���@���@�b@���@�C�@�
=@��@��@�~�@��T@���@�`B@�?}@�/@��@���@��@��D@�j@�bN@�Z@�1'@���@��
@��
@�S�@�+@�o@�
=@���@�v�@�J@��T@���@�`B@�/@���@��@�I�@�b@��@��;@���@��w@�t�@�"�@���@���@���@�n�@�E�@�$�@�J@���@�O�@���@��j@�r�@��m@��P@�o@���@��+@�M�@�-@�@��#@���@�p�@�O�@�/@��`@�z�@�Z@�Q�@�  @�|�@�K�@���@���@��+@�-@���@���@���@�7L@�Ĝ@�bN@� �@��w@���@���@���@�t�@�S�@���@��@�v�@�@�O�@�V@��/@���@� �@��@�ƨ@�l�@�;d@���@���@�~�@�M�@���@���@���@�O�@�%@��@��`@���@�j@� �@�b@���@�l�@�S�@�
=@���@���@�-@��T@��^@�x�@�/@��j@�j@�(�@���@�ƨ@�"�@�~�@�$�@���@���@��7@��@�p�@��@���@���@��@� �@���@��
@��w@��w@���@�l�@�
=@���@���@��\@�V@�-@�J@��#@��^@��@�hs@��@��@���@���@�Z@�A�@�1@�w@l�@+@~��@~5?@~@}�@}�@}O�@}?}@}/@|�@|�@|I�@|9X@|1@{�F@{��@{t�@{o@z�H@z��@z=q@y�@y��@xĜ@x��@xr�@xb@w�w@w�@v�+@u�@up�@t��@t�@tI�@t�@s�
@s��@st�@sC�@r�@r-@q��@q�@pĜ@o�w@o|�@o�@n�y@n�@nȴ@nȴ@n��@n@m�h@m�@l�@l�@l�/@lj@l�@k�F@k�@kt�@kt�@kS�@k33@j�@j�!@j~�@i�#@i��@iX@hĜ@hĜ@hr�@hb@g�w@gl�@f��@f�+@fV@f{@e��@e`B@d�@c��@c��@cS�@b�H@b�\@bM�@a�@ax�@a�@`Ĝ@`1'@_�;@_��@^��@^v�@^{@]�@\��@\j@\I�@\9X@\�@[��@[ƨ@[dZ@["�@Zn�@Y��@YX@X��@X��@X�u@Xr�@XbN@X1'@Xb@X  @W��@W\)@V��@V�R@V�+@V{@U�-@Up�@T��@T�D@S��@Sƨ@S��@S�@SS�@S@R��@R�!@R=q@Q�^@Qx�@Q7L@P��@P�@PbN@P1'@P  @O��@O��@O;d@N�@Nv�@N5?@N@M��@M��@Mp�@M�@L�/@L�@L�D@LI�@K�F@K33@Ko@J�H@J��@JJ@I�7@IX@I%@H��@H�u@HbN@Hb@Gl�@G+@F�@F��@F{@E@E�h@Ep�@E?}@D��@D��@D(�@C��@C�F@CS�@C"�@B�H@B��@BJ@A�7@A&�@@��@@�@@�@@bN@@Q�@?�@?|�@?
=@>ff@=�T@=�@<�j@<�@<��@<�@;ƨ@;�@;S�@;"�@:�@:��@:~�@:-@9��@9X@8�`@8r�@8A�@8b@7�w@7+@6�y@6�@6ff@6$�@5@5�h@5?}@4��@4�@4j@3�m@3ƨ@3�@3"�@2�!@2J@1�^@1X@0��@0  @/�@/�P@/l�@.ȴ@-�T@-�h@-p�@-`B@-?}@,�@,�j@,��@,�D@,j@,1@+�F@+��@+dZ@*��@*M�@*�@*J@)��@)�@)��@)X@)&�@)�@(�`@(Ĝ@(�9@(bN@'��@';d@'�@&��@&��@&�y@&�@&��@&5?@&{@%�-@%�@%O�@%V@$�D@$(�@#�
@#�@#@"�@"�H@"��@"�\@"n�@"M�@"J@!��@!x�@!�@!%@!%@!%@!%@ �`@ ��@ ��@ Ĝ@ ��@ ��@ �u@ �u@ r�@ bN@ Q�@ Q�@ A�@  �@��@�P@K�@+@+@�@
=@��@ff@5?@�T@�-@��@p�@�@��@�/@j@9X@1@dZ@@��@�!@~�@�@��@��@x�@X@7L@%@�`@��@bN@  @�@��@�w@�@��@�P@\)@+@��@�@�R@��@�+@v�@5?@{@�-@O�@�@��@��@�@��@��@��@j@(�@(�@�@1@�
@��@��@��@t�@C�@"�@o@�@��@��@�!@n�@M�@-@�@�@��@�7@hs@G�@&�@�@�`@�9@�u@r�@A�@b@�@�@�P@|�@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��A��HA��A���A�JA�JA���A���A�$�A�C�A�A�A�-A�?}A�XA�\)A�Q�A�?}A��A�bA�A���A���A���A��A��A���A�;dA��TAٓuA�jA��mA��A՛�A��`A�+Aκ^A��TA���AǁAƺ^A�=qA���A��uA�+A��jA�+A��A�t�A���A�ȴA��A��A�&�A���A���A���A�A�A���A��RA�+A�$�A�\)A���A�v�A�x�A��A���A���A���A�
=A��FA�bA�
=A�A��FA� �A~ZAy�7Au;dAp�AkO�Ai�Ai�^Ah�`Ah^5AfffAd �Ab~�A_O�A\�DAY��AX9XAU�#AT�/AR��APALjAK�AJ(�AI"�AH$�AF��AD��AChsAAdZA?�#A=�A: �A7dZA5l�A4��A4~�A4 �A3;dA2{A1`BA0��A/�A-�^A,VA*�yA)dZA(��A'�^A'%A&�\A&VA&$�A%C�A$ĜA$z�A$ffA$=qA#�A#�-A#�A"v�A ��A   AA�A1A�mA�wA�hA�A��A�A;dA�At�A�A
=Av�A�A�wA�+AVA�mA�uA��A��A%A	��A	\)AVA�^A�hAp�At�Al�A;dA��A �A	hsA	��A	�A	`BAK�AQ�A+A�A��Ap�AĜA�-A�At�A�`A��A��A�A�AO�A E�@��w@�C�@�C�@��@�oA 9XA"�A ĜA �+A ^5A Q�@�~�@�%@�j@���@�/@���@��@��9@��@��y@�$�@��^@�Ĝ@��\@��@��@� �@�@�@�E�@�$�@�J@�@��@���@�h@�Ĝ@�ƨ@@�O�@�j@� �@��m@띲@��@�+@�v�@�5?@陚@�j@�@�"�@�V@�`B@��@�9@� �@��
@�F@�"�@�v�@�@��`@��@�I�@�1@ߕ�@�"�@��@އ+@�5?@ݡ�@�`B@�A�@ە�@�\)@ڸR@�~�@�`B@���@�Q�@��@�|�@և+@�V@��@�hs@�`B@�?}@��@�1@�@��@�/@�Ĝ@Ѓ@�  @ϝ�@�C�@���@Ώ\@�E�@�@Ͳ-@�7L@�Ĝ@�  @˶F@�|�@�~�@�=q@��@�G�@��`@ȴ9@��m@Ə\@���@��`@�bN@��;@���@��
@�l�@�33@���@���@�n�@��@���@�hs@��@���@��@�I�@���@��@�K�@���@���@�v�@��@��@�Ĝ@�Z@���@��P@�K�@�
=@��@��\@�V@���@��-@���@�A�@�dZ@��y@��R@�J@��T@���@�x�@�7L@���@��;@�C�@��+@�V@��@��h@���@���@�I�@�  @��
@���@�
=@�=q@�{@�{@�{@���@�p�@��@�bN@���@�t�@�"�@��y@��\@���@��h@�hs@�%@���@���@�b@���@�C�@�
=@��@��@�~�@��T@���@�`B@�?}@�/@��@���@��@��D@�j@�bN@�Z@�1'@���@��
@��
@�S�@�+@�o@�
=@���@�v�@�J@��T@���@�`B@�/@���@��@�I�@�b@��@��;@���@��w@�t�@�"�@���@���@���@�n�@�E�@�$�@�J@���@�O�@���@��j@�r�@��m@��P@�o@���@��+@�M�@�-@�@��#@���@�p�@�O�@�/@��`@�z�@�Z@�Q�@�  @�|�@�K�@���@���@��+@�-@���@���@���@�7L@�Ĝ@�bN@� �@��w@���@���@���@�t�@�S�@���@��@�v�@�@�O�@�V@��/@���@� �@��@�ƨ@�l�@�;d@���@���@�~�@�M�@���@���@���@�O�@�%@��@��`@���@�j@� �@�b@���@�l�@�S�@�
=@���@���@�-@��T@��^@�x�@�/@��j@�j@�(�@���@�ƨ@�"�@�~�@�$�@���@���@��7@��@�p�@��@���@���@��@� �@���@��
@��w@��w@���@�l�@�
=@���@���@��\@�V@�-@�J@��#@��^@��@�hs@��@��@���@���@�Z@�A�@�1@�w@l�@+@~��@~5?@~@}�@}�@}O�@}?}@}/@|�@|�@|I�@|9X@|1@{�F@{��@{t�@{o@z�H@z��@z=q@y�@y��@xĜ@x��@xr�@xb@w�w@w�@v�+@u�@up�@t��@t�@tI�@t�@s�
@s��@st�@sC�@r�@r-@q��@q�@pĜ@o�w@o|�@o�@n�y@n�@nȴ@nȴ@n��@n@m�h@m�@l�@l�@l�/@lj@l�@k�F@k�@kt�@kt�@kS�@k33@j�@j�!@j~�@i�#@i��@iX@hĜ@hĜ@hr�@hb@g�w@gl�@f��@f�+@fV@f{@e��@e`B@d�@c��@c��@cS�@b�H@b�\@bM�@a�@ax�@a�@`Ĝ@`1'@_�;@_��@^��@^v�@^{@]�@\��@\j@\I�@\9X@\�@[��@[ƨ@[dZ@["�@Zn�@Y��@YX@X��@X��@X�u@Xr�@XbN@X1'@Xb@X  @W��@W\)@V��@V�R@V�+@V{@U�-@Up�@T��@T�D@S��@Sƨ@S��@S�@SS�@S@R��@R�!@R=q@Q�^@Qx�@Q7L@P��@P�@PbN@P1'@P  @O��@O��@O;d@N�@Nv�@N5?@N@M��@M��@Mp�@M�@L�/@L�@L�D@LI�@K�F@K33@Ko@J�H@J��@JJ@I�7@IX@I%@H��@H�u@HbN@Hb@Gl�@G+@F�@F��@F{@E@E�h@Ep�@E?}@D��@D��@D(�@C��@C�F@CS�@C"�@B�H@B��@BJ@A�7@A&�@@��@@�@@�@@bN@@Q�@?�@?|�@?
=@>ff@=�T@=�@<�j@<�@<��@<�@;ƨ@;�@;S�@;"�@:�@:��@:~�@:-@9��@9X@8�`@8r�@8A�@8b@7�w@7+@6�y@6�@6ff@6$�@5@5�h@5?}@4��@4�@4j@3�m@3ƨ@3�@3"�@2�!@2J@1�^@1X@0��@0  @/�@/�P@/l�@.ȴ@-�T@-�h@-p�@-`B@-?}@,�@,�j@,��@,�D@,j@,1@+�F@+��@+dZ@*��@*M�@*�@*J@)��@)�@)��@)X@)&�@)�@(�`@(Ĝ@(�9@(bN@'��@';d@'�@&��@&��@&�y@&�@&��@&5?@&{@%�-@%�@%O�@%V@$�D@$(�@#�
@#�@#@"�@"�H@"��@"�\@"n�@"M�@"J@!��@!x�@!�@!%@!%@!%@!%@ �`@ ��@ ��@ Ĝ@ ��@ ��@ �u@ �u@ r�@ bN@ Q�@ Q�@ A�@  �@��@�P@K�@+@+@�@
=@��@ff@5?@�T@�-@��@p�@�@��@�/@j@9X@1@dZ@@��@�!@~�@�@��@��@x�@X@7L@%@�`@��@bN@  @�@��@�w@�@��@�P@\)@+@��@�@�R@��@�+@v�@5?@{@�-@O�@�@��@��@�@��@��@��@j@(�@(�@�@1@�
@��@��@��@t�@C�@"�@o@�@��@��@�!@n�@M�@-@�@�@��@�7@hs@G�@&�@�@�`@�9@�u@r�@A�@b@�@�@�P@|�@|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	JB	DB	JB	uB	�B	�B	�B	uB	{B	!�B	1'B	0!B	'�B	0!B	@�B	E�B	I�B	J�B	L�B	M�B	M�B	L�B	M�B	M�B	N�B	P�B	YB	dZB	o�B	�%B	�VB	�VB	�bB	�bB	�VB	��B	��B	��B
B
VB
�B
%�B
�B
B
P�B
hsB
y�B
ȴB
��B
��BPB&�B"�B%�B.B1'B0!B"�B�B{BDBB
��B
�fB
�B
��B
��B
��B
�uB
�B
m�B
ZB
:^B
$�B
�B
+B	�B	�#B	ǮB	�!B	��B	�DB	�B	�B	z�B	v�B	n�B	k�B	e`B	P�B	D�B	=qB	49B	+B	$�B	�B	
=B��B��B��B�B�B�B�B�sB�ZB�;B�#B��BȴBƨBȴB��B��B��B��B��B�B��BɺBɺB��B��B��B�B�fB�fB�sB�B�B��B	B		7B	JB	bB	bB	uB	�B	�B	�B	DB	oB	!�B	33B	7LB	8RB	49B	/B	(�B	'�B	#�B	#�B	"�B	"�B	$�B	+B	C�B	>wB	5?B	.B	,B	.B	-B	/B	0!B	8RB	>wB	B�B	E�B	F�B	J�B	P�B	aHB	l�B	�B	�1B	�1B	�1B	~�B	~�B	�=B	�DB	�=B	�%B	��B	�B	�B	�B	�B	�-B	�-B	��B	��B	~�B	v�B	u�B	w�B	�B	�B	�PB	��B	�B	�'B	�-B	�-B	�3B	�3B	�!B	�B	�'B	�LB	B	ĜB	ŢB	ŢB	ǮB	ǮB	ȴB	ɺB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�
B	�B	��B	�
B	�B	�#B	�#B	�#B	�)B	�#B	�#B	�#B	�#B	�)B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�5B	�BB	�HB	�NB	�HB	�NB	�TB	�TB	�TB	�ZB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�yB	�sB	�sB	�sB	�sB	�sB	�yB	�sB	�sB	�sB	�mB	�fB	�fB	�sB	�B	�B	�B	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B

=B
DB
DB
JB
VB
VB
VB
VB
PB
PB
VB
VB
VB
bB
bB
bB
bB
bB
hB
hB
hB
bB
hB
hB
hB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
 �B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
'�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
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
/B
0!B
/B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
/B
/B
/B
/B
.B
.B
.B
.B
.B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
5?B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
7LB
7LB
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
9XB
9XB
:^B
:^B
:^B
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
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
R�B
R�B
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
YB
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
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
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
dZB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
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
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
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
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	JB	DB	JB	uB	�B	�B	�B	uB	{B	!�B	1'B	0!B	'�B	0!B	@�B	E�B	I�B	J�B	L�B	M�B	M�B	L�B	M�B	M�B	N�B	P�B	YB	dZB	o�B	�%B	�VB	�VB	�bB	�bB	�VB	��B	��B	��B
B
VB
�B
%�B
�B
B
P�B
hsB
y�B
ȴB
��B
��BPB&�B"�B%�B.B1'B0!B"�B�B{BDBB
��B
�fB
�B
��B
��B
��B
�uB
�B
m�B
ZB
:^B
$�B
�B
+B	�B	�#B	ǮB	�!B	��B	�DB	�B	�B	z�B	v�B	n�B	k�B	e`B	P�B	D�B	=qB	49B	+B	$�B	�B	
=B��B��B��B�B�B�B�B�sB�ZB�;B�#B��BȴBƨBȴB��B��B��B��B��B�B��BɺBɺB��B��B��B�B�fB�fB�sB�B�B��B	B		7B	JB	bB	bB	uB	�B	�B	�B	DB	oB	!�B	33B	7LB	8RB	49B	/B	(�B	'�B	#�B	#�B	"�B	"�B	$�B	+B	C�B	>wB	5?B	.B	,B	.B	-B	/B	0!B	8RB	>wB	B�B	E�B	F�B	J�B	P�B	aHB	l�B	�B	�1B	�1B	�1B	~�B	~�B	�=B	�DB	�=B	�%B	��B	�B	�B	�B	�B	�-B	�-B	��B	��B	~�B	v�B	u�B	w�B	�B	�B	�PB	��B	�B	�'B	�-B	�-B	�3B	�3B	�!B	�B	�'B	�LB	B	ĜB	ŢB	ŢB	ǮB	ǮB	ȴB	ɺB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�
B	�B	��B	�
B	�B	�#B	�#B	�#B	�)B	�#B	�#B	�#B	�#B	�)B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�5B	�BB	�HB	�NB	�HB	�NB	�TB	�TB	�TB	�ZB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�yB	�sB	�sB	�sB	�sB	�sB	�yB	�sB	�sB	�sB	�mB	�fB	�fB	�sB	�B	�B	�B	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B

=B
DB
DB
JB
VB
VB
VB
VB
PB
PB
VB
VB
VB
bB
bB
bB
bB
bB
hB
hB
hB
bB
hB
hB
hB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
 �B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
'�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
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
/B
0!B
/B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
/B
/B
/B
/B
.B
.B
.B
.B
.B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
5?B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
7LB
7LB
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
9XB
9XB
:^B
:^B
:^B
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
=qB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
C�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
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
R�B
R�B
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
YB
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
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
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
dZB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
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
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
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
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
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
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
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
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20191114110020                              AO  ARCAADJP                                                                    20191114110020    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191114110020  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191114110020  QCF$                G�O�G�O�G�O�0               