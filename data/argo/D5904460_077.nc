CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-05T09:16:15Z AOML 3.0 creation; 2016-08-07T21:17:41Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20151005091615  20160807141741  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               MA   AO  5285_8895_077                   2C  D   APEX                            6487                            072314                          846 @�t��m?�1   @�t�/hQ�@+������c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    MA   B   B   @�  @�  A   A   A@  A`  A�  A���A�33A�  A�  A�  A�  A�B ��B  B  B��B��B(  B0  B8  B@��BF  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy��D��3D�S3D��3D�� D��D�<�D��3D��fD���D�Y�D�� DǬ�D͠ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�ffA33A+33AK33Ak33A���A�fgA���A���Ař�Aՙ�A噚A�34B��B
��B��BfgB"fgB*��B2��B:��BC��BH��BRfgBZ��Bb��Bj��Br��Bz��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC �3C�3C�3C�3C�3C
�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C �3C"�3C$�3C&�3C(�3C*�3C,�3C.�3C0�3C2�3C4�3C6�3C8�3C:�3C<�3C>�3C@�3CB�3CD�3CF��CH�3CJ�3CL�3CN�3CP�3CR�3CT�3CV�3CX�3CZ�3C\�3C^�3C`�3Cb�3Cd��Cf�3Ch�3Cj�3Cl�3Cn�3Cp�3Cr�3Ct�3Cv�3Cx�3Cz�3C|�3C~�3C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�D ,�D ��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D	,�D	��D
,�D
��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D,�D��D ,�D ��D!,�D!��D",�D"��D#,�D#��D$,�D$��D%,�D%��D&,�D&��D',�D'��D(,�D(��D),�D)��D*,�D*��D+,�D+��D,,�D,��D-,�D-��D.,�D.��D/,�D/��D0,�D0��D1,�D1��D2,�D2��D3,�D3��D4,�D4��D5,�D5��D6,�D6��D7,�D7��D8,�D8��D9,�D9��D:,�D:��D;,�D;��D<,�D<��D=,�D=��D>,�D>��D?,�D?��D@,�D@��DA,�DA��DB,�DB��DC,�DC��DD,�DD��DE,�DE��DF,�DF��DG,�DG��DH,�DH��DI,�DI��DJ,�DJ��DK,�DK��DL,�DL��DM,�DM��DN,�DN��DO,�DO��DP,�DP��DQ,�DQ��DR,�DR��DS,�DS��DT,�DT��DU,�DU��DV,�DV��DW,�DW��DX,�DX��DY,�DY��DZ,�DZ��D[,�D[��D\,�D\��D],�D]��D^,�D^��D_,�D_��D`,�D`��Da,�Da��Db,�Db��Dc,�Dc��Dd,�Dd��De,�De��Df,�Df��Dg,�Dg��Dh,�Dh��Di,�Di��Dj,�Dj��Dk,�Dk��Dl,�Dl��Dm,�Dm��Dn,�Dn��Do,�Do��Dp,�Dp��Dq,�Dq��Dr,�Dr��Ds,�Ds��Dt,�Dt��Dy��D�	�D�i�D���D��fD�33D�S3D���D���D�3D�p D��fD��3DͶf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�FA�FA�FA�RA�RAᛦA�ZA��#A�7LA�Q�Aއ+A�v�Aܟ�A�9XAղ-A��HAϏ\A�oA�bNA�(�A�{Aɺ^A�/A�;dA�jA�VA�+A���A�?}A�O�A��A���A��!A�{A�VA�r�A�  A��
A�\)A���A��hA�^5A�C�A���A��9A�t�A�ZA���A��A���A�\)A�t�A�n�A�x�A��A��TA��9A�;dA�9XA�{A��A��A�%A�33A��TA��\A���A�"�A���A���A��FA��+A�A���A�S�AXAy�#AvVAq�FAjjAg�wAf-AcdZAa`BA`JA]ƨAW�AS��AQ�wAP{AKƨAHA�AE�-AD�9AB�/A?dZA=��A=�A<��A:��A7��A6  A4�!A3�^A1�TA0-A/l�A.�yA.z�A-+A+�FA+A)?}A'S�A&��A&��A%�A$�yA"ȴA!/A ȴA A�A��AJA�9A�9A��A`BAO�A�`AC�A��A7LA9XA�HAAr�A��AXA-AbNA	�A	;dA	+A	hsA	��A	�7A	+A	hsAĜA	�hA
A	S�A��AdZAXA��A$�A  A�AVA�A�A�A%A��A�A��AA?}A+Av�AXAdZA��A�A|�Ap�AG�A;dA;dA?}A��AjAJA&�A�AoA%A ��A ��A bNA 1'@��@��^@��`@�S�@�V@��@��@��m@� �@�j@�"�@�ȴ@�!@�\@�$�@��@�bN@�
=@��@홚@�h@�?}@�@�1@��;@��
@�@�+@�!@�{@��@�`B@��@�b@�"�@�~�@���@�h@��/@�1@�F@��@㕁@�K�@�^5@���@��@�z�@�1@�dZ@�@�n�@���@ݺ^@ݡ�@�X@�(�@ۥ�@���@��@�?}@ؼj@�\)@���@�O�@�z�@� �@�b@��m@�
=@�=q@�`B@�?}@�z�@�K�@�o@Χ�@͙�@̬@���@˅@�;d@��H@ʧ�@�~�@�p�@�G�@���@��;@�@ƸR@�E�@��@���@�x�@���@Ĵ9@ċD@�bN@�9X@Å@�33@�
=@+@��#@�x�@�hs@���@�@���@�&�@��j@�Q�@��@�bN@�t�@��!@��y@���@�v�@�@���@���@���@��h@��@�G�@��u@�A�@��w@�|�@���@�^5@��#@���@�X@�7L@�/@�%@��9@�r�@�(�@��;@��F@��@�;d@��R@�{@��-@�?}@��@�O�@�V@��D@��@��@���@���@�n�@���@�@�X@���@��@�j@�I�@� �@��
@�@��+@�5?@��^@��-@���@�`B@�V@��@��j@�j@���@�
=@��@���@�@���@�j@��w@�t�@�S�@�C�@�"�@���@�5?@��T@���@��@�`B@�?}@���@���@�bN@��
@��@�|�@�\)@�33@��@��H@��R@�~�@�@���@�O�@��@��`@���@�j@�b@���@�K�@�"�@���@��R@��@��7@�O�@��@�%@��@��/@��j@��@�Q�@�(�@��@��@��P@�;d@�o@���@�5?@�$�@��@���@�`B@���@���@�1'@�(�@�  @��;@�ƨ@�dZ@�+@�o@�
=@�o@���@���@��+@�V@�$�@�=q@�@���@�hs@���@���@��@�z�@�(�@�t�@�;d@�o@��@��R@��+@�M�@���@��^@��@�O�@�?}@���@�Ĝ@��@��u@��D@�z�@�bN@�(�@��@��P@��@�V@��@��9@t�@kC�@dz�@[�
@Tz�@J��@B-@8�@2=q@+��@%��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�A�FA�FA�FA�RA�RAᛦA�ZA��#A�7LA�Q�Aއ+A�v�Aܟ�A�9XAղ-A��HAϏ\A�oA�bNA�(�A�{Aɺ^A�/A�;dA�jA�VA�+A���A�?}A�O�A��A���A��!A�{A�VA�r�A�  A��
A�\)A���A��hA�^5A�C�A���A��9A�t�A�ZA���A��A���A�\)A�t�A�n�A�x�A��A��TA��9A�;dA�9XA�{A��A��A�%A�33A��TA��\A���A�"�A���A���A��FA��+A�A���A�S�AXAy�#AvVAq�FAjjAg�wAf-AcdZAa`BA`JA]ƨAW�AS��AQ�wAP{AKƨAHA�AE�-AD�9AB�/A?dZA=��A=�A<��A:��A7��A6  A4�!A3�^A1�TA0-A/l�A.�yA.z�A-+A+�FA+A)?}A'S�A&��A&��A%�A$�yA"ȴA!/A ȴA A�A��AJA�9A�9A��A`BAO�A�`AC�A��A7LA9XA�HAAr�A��AXA-AbNA	�A	;dA	+A	hsA	��A	�7A	+A	hsAĜA	�hA
A	S�A��AdZAXA��A$�A  A�AVA�A�A�A%A��A�A��AA?}A+Av�AXAdZA��A�A|�Ap�AG�A;dA;dA?}A��AjAJA&�A�AoA%A ��A ��A bNA 1'@��@��^@��`@�S�@�V@��@��@��m@� �@�j@�"�@�ȴ@�!@�\@�$�@��@�bN@�
=@��@홚@�h@�?}@�@�1@��;@��
@�@�+@�!@�{@��@�`B@��@�b@�"�@�~�@���@�h@��/@�1@�F@��@㕁@�K�@�^5@���@��@�z�@�1@�dZ@�@�n�@���@ݺ^@ݡ�@�X@�(�@ۥ�@���@��@�?}@ؼj@�\)@���@�O�@�z�@� �@�b@��m@�
=@�=q@�`B@�?}@�z�@�K�@�o@Χ�@͙�@̬@���@˅@�;d@��H@ʧ�@�~�@�p�@�G�@���@��;@�@ƸR@�E�@��@���@�x�@���@Ĵ9@ċD@�bN@�9X@Å@�33@�
=@+@��#@�x�@�hs@���@�@���@�&�@��j@�Q�@��@�bN@�t�@��!@��y@���@�v�@�@���@���@���@��h@��@�G�@��u@�A�@��w@�|�@���@�^5@��#@���@�X@�7L@�/@�%@��9@�r�@�(�@��;@��F@��@�;d@��R@�{@��-@�?}@��@�O�@�V@��D@��@��@���@���@�n�@���@�@�X@���@��@�j@�I�@� �@��
@�@��+@�5?@��^@��-@���@�`B@�V@��@��j@�j@���@�
=@��@���@�@���@�j@��w@�t�@�S�@�C�@�"�@���@�5?@��T@���@��@�`B@�?}@���@���@�bN@��
@��@�|�@�\)@�33@��@��H@��R@�~�@�@���@�O�@��@��`@���@�j@�b@���@�K�@�"�@���@��R@��@��7@�O�@��@�%@��@��/@��j@��@�Q�@�(�@��@��@��P@�;d@�o@���@�5?@�$�@��@���@�`B@���@���@�1'@�(�@�  @��;@�ƨ@�dZ@�+@�o@�
=@�o@���@���@��+@�V@�$�@�=q@�@���@�hs@���@���@��@�z�@�(�@�t�@�;d@�o@��@��R@��+@�M�@���@��^@��@�O�@�?}@���@�Ĝ@��@��u@��D@�z�@�bN@�(�@��@��PG�O�@�V@��@��9@t�@kC�@dz�@[�
@Tz�@J��@B-@8�@2=q@+��@%��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJ�BJ�BJ�BJ�BJ�BK�Bm�B��B��B	W
B	�B	�dB	�B
bB
�B
�BF�B��B�RB�B1B-BD�BS�B�7B��B�FB��B��B��B��B��B��B�B�B�/B�ZB�mB�sB�B��B�B�sB�/B��B��B��B�uB�JBx�BR�B=qB)�B%B�;B��B��B�Bv�B\)B2-B{B
��B
��B"�B�B�BoB
��B
ȴB
�LB
�B
��B
�B
��B
�B
R�B
&�B
1B	�BB	�B	��B	�JB	w�B	iyB	^5B	J�B	#�B		7B��B�B�)BȴB�jB�FB�FB�jB��BÖB��B��B�B��B�5B�mB�B	DB	�B	�B	�B	.B	5?B	2-B	33B	<jB	F�B	hsB	l�B	v�B	t�B	�B	�JB	�bB	��B	�hB	�JB	��B	��B	�{B	�oB	�bB	�\B	�VB	�VB	�+B	z�B	jB	`BB	[#B	dZB	`BB	R�B	G�B	G�B	K�B	YB	jB	o�B	s�B	z�B	y�B	�=B	�{B	��B	��B	�bB	�+B	�B	�B	�=B	��B	�B	�'B	�9B	�jB	��B	��B	ÖB	ɺB	�B	�TB	�ZB	�HB	�5B	�ZB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�mB	�mB	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�sB	�TB	�)B	�B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�#B	�)B	�)B	�)B	�#B	�#B	�B	�#B	�#B	�)B	�)B	�)B	�/B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�BB	�;B	�;B	�;B	�5B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�ZB	�`B	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
+B
1B
1B
1B
	7B
	7B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
	7B
1B
1B
+B
	7B

=B

=B
DB
DB
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
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
bB
bB
\B
\B
\B
bB
bB
bB
hB
hB
oB
uB
uB
uB
oB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
%�B
'�B
,B
1'B
8RB
@�B
D�B
I�B
O�B
W
B
[#B
aHB
e`B
k�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  BJ�BJ�BJ�BJ�BJ�BK�Bm[B�~B��B	V�B	��B	�;B	�}B
5B
}B
�RBFpB�sB�B�|B�B,�BDaBS�B��B��B�B�MBˈBӿBΜBʅBʄB��B��B��B�B�2B�:B�cB�B�gB�5B��BӽB�JB��B�9B�Bx�BR�B=0B)�B�B��B�FB�jB��Bv�B[�B1�B9B
��B
�}B"�BpBdB,B
��B
�sB
�
B
��B
��B
��B
�kB
��B
R�B
&�B
�B	�B	��B	�ZB	�B	w�B	i:B	]�B	J�B	#�B	�B��B�WB��B�|B�1B�B�B�.B�NB�\BʄBѯB��BӻB��B�/B�eB	B	ZB	_B	dB	-�B	4�B	1�B	2�B	<&B	FhB	h/B	lDB	v�B	tvB	��B	�B	�B	�9B	� B	� B	�8B	�>B	�3B	�(B	�B	�B	�B	�B	��B	z�B	j3B	_�B	Z�B	dB	_�B	R�B	GfB	GgB	K~B	X�B	j7B	oVB	skB	z�B	y�B	��B	�/B	�6B	�AB	�B	��B	��B	��B	��B	�@B	��B	��B	��B	�B	�6B	�5B	�HB	�kB	նB	�B	�B	��B	��B	�B	�*B	�,B	�*B	�1B	�8B	�=B	�?B	�?B	�1B	�B	�B	�1B	�ZB	�hB	�lB	�lB	�tB	�yB	�nB	�YB	�;B	�"B	�B	��B	մB	ϏB	�lB	�~B	ӪB	џB	џB	ҧB	өB	ќB	ϐB	ЖB	ϏB	͂B	ώB	ώB	ϐB	ϏB	ЕB	ҥB	ӨB	өB	ԬB	ԮB	԰B	ԮB	ԱB	԰B	ղB	��B	��B	׿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�	B	�	B	�B	�B	�	B	�B	�B	�!B	�B	�9B	�MB	�KB	�GB	�DB	�FB	�JB	�cB	�\B	�\B	�wB	�pB	�hB	�aB	�uB	�{B	�~B	�{B	�|B	�vB	�uB	�vB	�vB	�wB	�vB	�vB	�vB	�zB	�|B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
�B
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
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
B
�B
�B
 B
B
B
	B
B
B
B
B
B
B
B
B
!B
"B
 B
B
B
B
 B
 B
B
B
$B
'B
,B
3B
6B
8B
9B
>B
>B
EB
EB
LB
IB
KB
KB
LB
RB
HB
OB
UB
VB
VB
RB
UB
VB
WB
]B
^B
\B
\B
VB
TB
WB
]B
\B
]B
\B
^B
[B
[B
[B
]B
cB
aB
hB
 oB
 rB
 pB
!uB
!vB
!vB
!sB
"yB
!tB
"}B
"}B
"~B
"|B
"~B
"|B
"}B
"|B
#�B
#�G�O�B
'�B
+�B
0�B
7�B
@,B
DDB
IbB
O�B
V�B
Z�B
`�B
eB
k-B
oE1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.7 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417412016080714174120160807141741  AO  ARCAADJP                                                                    20151005091615    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151005091615  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151005091615  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141741  IP                  G�O�G�O�G�O�                