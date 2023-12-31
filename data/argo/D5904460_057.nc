CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-22T19:18:11Z AOML 3.0 creation; 2016-08-07T21:17:38Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150622191811  20160807141738  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               9A   AO  5285_8895_057                   2C  D   APEX                            6487                            072314                          846 @�ZX�"�1   @�ZY-��
@-L������c���S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    9A   B   B   @�ff@�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB933B?��BH  BP  BX  Ba��BfffBo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dyl�D��D�C3D�y�D�ɚD�	�D�L�D�i�D���D���D�<�D���DǶfD�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@��HA	p�A+
>AIp�Aip�A��RA��RA��RA��RAĸRAԸRA�RA��RB\)B
\)B\)B\)B"\)B*\)B2B;�\BA��BJ\)BR\)BZ\)Bc��BhBq��Bz\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C �
C�
C�
C�
C�
C
�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C �
C"�
C$�
C&�
C(�
C*�
C,�
C.�
C0�
C2�
C4�
C6�
C8�
C:�
C<�
C>�
C@�
CB�
CD�
CF�
CH�
CJ�
CL�
CN�
CP�
CR�
CT�
CV�
CX�
CZ�
C\�
C^�
C`�
Cb�
Cd�
Cf�
Ch�
Cj�
Cl�
Cn�
Cp�
Cr�
Ct�
Cv�
Cx�
Cz�
C|�
C~�
C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�XRC�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�C�K�D %�D ��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D	%�D	��D
%�D
��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D%�D��D %�D ��D!%�D!��D"%�D"��D#%�D#��D$%�D$��D%%�D%��D&%�D&��D'%�D'��D(%�D(��D)%�D)��D*%�D*��D+%�D+��D,%�D,��D-%�D-��D.%�D.��D/%�D/��D0%�D0��D1%�D1��D2%�D2��D3%�D3��D4%�D4��D5%�D5��D6%�D6��D7%�D7��D8%�D8��D9%�D9��D:%�D:��D;%�D;��D<%�D<��D=%�D=��D>%�D>��D?%�D?��D@%�D@��DA%�DA��DB%�DB��DC%�DC��DD%�DD��DE%�DE��DF%�DF��DG%�DG��DH%�DH��DI%�DI��DJ%�DJ��DK%�DK��DL%�DL��DM%�DM��DN%�DN��DO%�DO��DP%�DP��DQ%�DQ��DR%�DR��DS%�DS�]DT%�DT��DU%�DU��DV%�DV��DW%�DW��DX%�DX��DY%�DY��DZ%�DZ��D[%�D[��D\%�D\��D]%�D]��D^%�D^��D_%�D_��D`%�D`��Da%�Da��Db%�Db��Dc%�Dc��Dd%�Dd��De%�De��Df%�Df��Dg%�Dg��Dh%�Dh��Di%�Di��Dj%�Dj��Dk%�Dk��Dl%�Dl��Dm%�Dm��Dn%�Dn��Do%�Do��Dp%�Dp��Dq%�Dq��Dr%�Dr��Ds%�Ds��Dt%�Dt��Dy��D��D�VD��{D��{D�{D�_�D�|{D��{D���D�O�D��{D��GD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�+A�-A�+A�-A�-A�-A�-A�C�A�C�A�A�A�A�A�A�A�?}A�?}A�A�A�A�A�A�A�E�A�E�A�G�A�/AլA�S�A���A�p�A�A�+Aʗ�A�\)Aț�A��#A�hsA��HA�/Ař�A�7LAēuA�l�A�n�A��A�1A�VA� �A�\)A��hA��;A���A�`BA�\)A�I�A���A��A�K�A���A�{A�VA��FA��jA�hsA��-A�`BA���A���A�bA��uA�ffA���A�ĜA�ĜA��FA��A��A���A�+A��A��A�~�A�O�A��jA�dZA�M�A�ȴA�A�&�A|1'Av�+Ap�Ak+Ac�^Aa�Aa`BA^��A\A�AY�TAVbAP��AO��AM�FAKoAF�+AD�HABĜA@��A>�yA>��A@{AA�AAA@VA>ȴA=�#A<�yA=7LA<��A;p�A:�!A:JA9��A8ZA8{A7��A6v�A6bA5�;A5O�A4M�A3"�A2I�A0�A0=qA/hsA.z�A-��A-;dA,�A+��A+;dA*ȴA)��A)+A(�DA(M�A'��A'�A&�/A&VA%�A%l�A$�A$$�A#ƨA#/A"�A!��A!S�A �A�mA�7AC�A�DAM�A�
A33A��AƨA%AbA�A�A�A?}A�uAbNA��A�A�RAffA�#A��At�A7LA�A%A�A��A9XA  A�
A��A`BA"�AoA��AVA�A�A�RAz�A��A�hA%A�jA�#A
�A
�\A
I�A	�mA	\)A��A��Az�A�AXA"�A��A~�AbNA(�A1'A9XAJA�A�7AXA
=A�DAM�A �A��A�#A�PAK�A��A�\Av�AjAE�A�A�;A�^A��A�7Ap�AK�A33A ��A �jA z�A {@�
=@��@�p�@��@�I�@�@�n�@�/@��j@�(�@��+@�5?@�{@���@�%@��@���@�z�@�r�@�1@�ȴ@��@�Ĝ@�Z@���@��m@��@��@�`B@�bN@�+@��@陚@�?}@���@�@���@�@�p�@�7L@�Z@���@�dZ@�^5@��@�X@��@���@��@�bN@�  @���@�V@��@��/@�A�@۶F@�-@�x�@�7L@�(�@�l�@��y@֗�@�=q@�@�x�@�/@��@ԋD@�I�@��@��@ӶF@�33@җ�@�$�@�O�@�r�@�9X@Ϯ@ΰ!@�5?@ͩ�@�X@���@�A�@�9X@�1'@���@�;d@��@���@�n�@�$�@�@ə�@�X@�7L@���@�bN@���@��H@�$�@�?}@ě�@�9X@å�@ÍP@Å@�t�@�K�@�ff@���@�G�@���@��u@� �@��@��@��;@���@�33@�=q@���@�Ĝ@���@��u@�bN@��;@�l�@�ȴ@�=q@���@�hs@��@��u@�(�@���@��@���@�@�{@���@�O�@��@��`@�j@�ƨ@�+@��H@���@���@��R@��!@���@�~�@�5?@��T@�x�@�9X@��
@�ƨ@��F@���@���@�dZ@��+@���@�O�@�G�@��@��/@�z�@��w@���@���@��h@�X@��@�%@��@�Q�@��m@��w@��@���@��P@�l�@�K�@��@���@�x�@�O�@�X@�X@�7L@���@��D@�r�@�9X@��@�"�@�ȴ@�=q@�@��@���@���@���@�1'@��@�+@�@���@���@�V@�-@���@���@��@��@���@��u@�Q�@��@�\)@�+@���@���@���@��+@�^5@�-@���@��#@��-@�x�@�"�@��@���@�@��-@y��@o�w@gK�@_
=@V�y@N�@EV@;��@5��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�+A�+A�-A�+A�-A�-A�-A�-A�C�A�C�A�A�A�A�A�A�A�?}A�?}A�A�A�A�A�A�A�E�A�E�A�G�A�/AլA�S�A���A�p�A�A�+Aʗ�A�\)Aț�A��#A�hsA��HA�/Ař�A�7LAēuA�l�A�n�A��A�1A�VA� �A�\)A��hA��;A���A�`BA�\)A�I�A���A��A�K�A���A�{A�VA��FA��jA�hsA��-A�`BA���A���A�bA��uA�ffA���A�ĜA�ĜA��FA��A��A���A�+A��A��A�~�A�O�A��jA�dZA�M�A�ȴA�A�&�A|1'Av�+Ap�Ak+Ac�^Aa�Aa`BA^��A\A�AY�TAVbAP��AO��AM�FAKoAF�+AD�HABĜA@��A>�yA>��A@{AA�AAA@VA>ȴA=�#A<�yA=7LA<��A;p�A:�!A:JA9��A8ZA8{A7��A6v�A6bA5�;A5O�A4M�A3"�A2I�A0�A0=qA/hsA.z�A-��A-;dA,�A+��A+;dA*ȴA)��A)+A(�DA(M�A'��A'�A&�/A&VA%�A%l�A$�A$$�A#ƨA#/A"�A!��A!S�A �A�mA�7AC�A�DAM�A�
A33A��AƨA%AbA�A�A�A?}A�uAbNA��A�A�RAffA�#A��At�A7LA�A%A�A��A9XA  A�
A��A`BA"�AoA��AVA�A�A�RAz�A��A�hA%A�jA�#A
�A
�\A
I�A	�mA	\)A��A��Az�A�AXA"�A��A~�AbNA(�A1'A9XAJA�A�7AXA
=A�DAM�A �A��A�#A�PAK�A��A�\Av�AjAE�A�A�;A�^A��A�7Ap�AK�A33A ��A �jA z�A {@�
=@��@�p�@��@�I�@�@�n�@�/@��j@�(�@��+@�5?@�{@���@�%@��@���@�z�@�r�@�1@�ȴ@��@�Ĝ@�Z@���@��m@��@��@�`B@�bN@�+@��@陚@�?}@���@�@���@�@�p�@�7L@�Z@���@�dZ@�^5@��@�X@��@���@��@�bN@�  @���@�V@��@��/@�A�@۶F@�-@�x�@�7L@�(�@�l�@��y@֗�@�=q@�@�x�@�/@��@ԋD@�I�@��@��@ӶF@�33@җ�@�$�@�O�@�r�@�9X@Ϯ@ΰ!@�5?@ͩ�@�X@���@�A�@�9X@�1'@���@�;d@��@���@�n�@�$�@�@ə�@�X@�7L@���@�bN@���@��H@�$�@�?}@ě�@�9X@å�@ÍP@Å@�t�@�K�@�ff@���@�G�@���@��u@� �@��@��@��;@���@�33@�=q@���@�Ĝ@���@��u@�bN@��;@�l�@�ȴ@�=q@���@�hs@��@��u@�(�@���@��@���@�@�{@���@�O�@��@��`@�j@�ƨ@�+@��H@���@���@��R@��!@���@�~�@�5?@��T@�x�@�9X@��
@�ƨ@��F@���@���@�dZ@��+@���@�O�@�G�@��@��/@�z�@��w@���@���@��h@�X@��@�%@��@�Q�@��m@��w@��@���@��P@�l�@�K�@��@���@�x�@�O�@�X@�X@�7L@���@��D@�r�@�9X@��@�"�@�ȴ@�=q@�@��@���@���@���@�1'@��@�+@�@���@���@�V@�-@���@���@��@��@���@��u@�Q�@��@�\)@�+@���@���@���@��+@�^5@�-@���@��#@��-G�O�@�"�@��@���@�@��-@y��@o�w@gK�@_
=@V�y@N�@EV@;��@5��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	�?B	�jB
�B
z�BYB�qBoB2-B@�BL�B^5BiyBp�B}�B��B�?B��B�ZB  B
=BVB�B)�B8RB9XB49B)�B�BDBDB\BB��B��B�VBiyB_;BaHBcTB_;BVBJ�B>wB:^B6FB.B �BJB��B�B�B�B�9By�BP�B�B
�wB
t�B
J�B
8RB
�B	�B	�dB	�hB	e`B	6FB	&�B	 �B	JB��B�B�HB�B�B�
B�)B��B	%B	{B	�B	]/B	��B	��B
1B
hB
PB
VB
�B
.B
T�B
dZB
ffB
e`B
m�B
iyB
hsB
iyB
jB
l�B
q�B
s�B
v�B
w�B
q�B
jB
gmB
gmB
e`B
bNB
aHB
aHB
`BB
`BB
aHB
aHB
bNB
aHB
`BB
_;B
^5B
]/B
\)B
\)B
[#B
[#B
YB
W
B
T�B
S�B
R�B
S�B
S�B
S�B
R�B
R�B
Q�B
N�B
M�B
K�B
I�B
F�B
C�B
?}B
;dB
7LB
2-B
2-B
33B
2-B
6FB
6FB
49B
2-B
2-B
2-B
49B
33B
49B
7LB
8RB
8RB
7LB
8RB
8RB
9XB
:^B
;dB
;dB
:^B
:^B
9XB
8RB
7LB
7LB
5?B
49B
5?B
5?B
33B
33B
33B
49B
49B
2-B
0!B
2-B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
7LB
7LB
8RB
8RB
8RB
7LB
6FB
5?B
5?B
2-B
33B
5?B
7LB
7LB
6FB
6FB
6FB
6FB
6FB
6FB
5?B
5?B
49B
49B
49B
49B
49B
49B
33B
33B
2-B
2-B
1'B
0!B
.B
-B
-B
,B
)�B
(�B
&�B
%�B
#�B
"�B
"�B
!�B
 �B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
hB
bB
\B
VB
PB
DB
	7B
1B
+B
+B
%B
%B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B
	7B
	7B
	7B
1B
1B
JB
PB
PB
PB
PB
PB
JB
JB
DB
JB
PB
VB
VB
VB
PB
JB
JB
JB
JB
JB
JB
PB
PB
PB
VB
VB
VB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
\B
\B
bB
bB
hB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
oB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
&�B
-B
2-B
7LB
<jB
B�B
G�B
K�B
Q�B
XB
^5B
bN1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	�\B	�\B	�ZB	�\B	�ZB	�ZB	�\B	�ZB	�YB	�[B	�[B	�[B	�[B	�YB	�[B	�[B	�YB	�[B	�[B	�[B	�[B	�dB	��B	��B	�B	�FB
mB
z�BX�B�AB@B1�B@RBL�B^BiFBppB}�B��B�B͞B�)B��B
B&B�B)�B8"B9'B4	B)�B|BBB,B�B��B͞B�#BiBB_BaBcB_BU�BJ�B>=B:&B6B-�B �BB��B�B�UB��B�By�BP�BbB
�?B
t�B
J�B
8B
LB	�IB	�4B	�8B	e0B	6B	&�B	 �B	B��B�rB�B��B��B��B��B��B	�B	IB	OB	\�B	�ZB	ΟB
�B
.B
B
B
SB
-�B
T�B
dB
f(B
e"B
mRB
i<B
h2B
i<B
j@B
lMB
qnB
svB
v�B
w�B
qjB
j?B
g/B
g-B
e"B
bB
aB
aB
`B
`B
aB
aB
bB
a
B
`B
^�B
]�B
\�B
[�B
[�B
Z�B
Z�B
X�B
V�B
T�B
S�B
R�B
S�B
S�B
S�B
R�B
R�B
Q�B
N�B
M�B
K�B
IxB
FgB
CVB
?=B
;%B
7
B
1�B
1�B
2�B
1�B
6B
6B
3�B
1�B
1�B
1�B
3�B
2�B
3�B
7B
8B
8B
7B
8B
8B
9B
:B
;#B
;$B
:B
:B
9B
8B
7
B
7
B
4�B
3�B
4�B
4�B
2�B
2�B
2�B
3�B
3�B
1�B
/�B
1�B
6B
7B
7	B
7	B
7
B
7B
8B
7
B
7	B
8B
8B
8B
7
B
6B
4�B
4�B
1�B
2�B
4�B
7	B
7	B
6B
6B
6B
6B
6B
6B
4�B
4�B
3�B
3�B
3�B
3�B
3�B
3�B
2�B
2�B
1�B
1�B
0�B
/�B
-�B
,�B
,�B
+�B
)�B
(�B
&�B
%�B
#�B
"�B
"�B
!�B
 ~B
B
|B
|B
zB
 �B
|B
oB
iB
]B
dB
tB
uB
pB
dB
^B
RB
EB
>B
=B
>B
7B
3B
-B
&B
B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
�B
�B
�B
�B
�B
B
B
B
B
B
	B
B
B

�B
B
B
B
B
B
B
B
B
B
B
B
B
B

B
	B
B
B
B
B
	B
B
B
B
B

B
B
B
B
B
B
B
B
B
"B
B
B
B
B
B
B
$B
$B
!B
B
'B
-B
/B
2B
0B
1B
3B
5B
@B
AB
?B
AB
@B
CB
AB
HB
LB
SB
WB
TB
UB
_B
fB
dG�O�B
xB
"�B
&�B
,�B
1�B
7B
< B
BHB
GgB
KB
Q�B
W�B
]�B
b1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.59 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417382016080714173820160807141738  AO  ARCAADJP                                                                    20150622191811    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150622191811  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150622191811  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141738  IP                  G�O�G�O�G�O�                