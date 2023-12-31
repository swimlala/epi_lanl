CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-05T12:35:25Z creation;2018-05-05T12:35:29Z conversion to V3.1;2019-12-23T06:22:28Z update;     
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
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ۨ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20180505123525  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               :A   JA  I2_0675_058                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�`WgY  1   @�`X @7�l�!-�b����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�ffA33A'33AG33Ag33A���A���A���A���AÙ�Aә�A㙚A�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3CnY�Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�3D�D��D�D��D�D��D�D��D�D��D�D�3D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6#3D6�3D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD�њD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfDfD��fD�fD�NfDÎfD��fD�fD�NfDĎfD��fD�fD�NfDŎfD��fD�fD�NfDƎfD��fD�fD�NfDǎfD��fD�fD�NfDȎfD��fD�fD�NfDɎfD��fD�fD�NfDʎfD��fD�fD�NfDˎfD��fD�fD�NfD̎fD��fD�fD�NfD͎fD��fD�fD�NfDΎfD��fD�fD�NfDώfD��fD�fD�NfDЎfD��fD�fD�NfDюfD��fD�fD�NfDҎfD��fD�fD�NfDӎfD��fD�fD�NfDԎfD��fD�fD�NfDՎfD��fD�fD�NfD֎fD��fD�fD�NfD׎fD��fD�fD�NfD؎fD��fD�fD�NfDَfD��fD�fD�NfDڎfD��fD�fD�NfDێfD��fD�fD�NfD܎fD��fD�fD�NfDݎfD��fD�fD�NfDގfD��fD�fD�NfDߎfD��fD�fD�NfD��fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD��fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD��fD��fD�fD�NfD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�M�A��A��9A��+A�v�A�\)A�C�A�33A��A���A��;A���A�A�A��HA���A��A�33A��A�ƨA���A�"�A�oA�oA�VA�1A���A�`BA�M�A�"�A���A� �A��A�-A��TA��-A�=qA���A��DA�ZA�
=A��;A���A�ƨA��-A���A���A��A�ZA�/A�{A���A���A�;dA�-A��A�n�A���A�1'A���A��mA�XA�I�A��!A�^5A�
=A��A�z�A���A�^5A��A�A���A���A��uA���A���A��9A�ƨA���A��-A���A��\A�z�A�p�A�l�A�hsA�^5A��A��HA�
=A��A��+A��A�M�A�C�A�{A��A��A�bNA��mA��;A��mA�&�A�;dA�r�A���A��^A�&�A���A���A���A�t�A�{A~v�A}l�A{�Ayt�Av^5Ar��Ap�!Ap �An��AmC�Ak�FAkl�Aj��Ai�-Ahn�Af�/Ad��AbQ�A`v�A]�mAWl�AU|�AT�jAS�mAQ�;APA�AOG�ANM�AL(�AJ~�AI�7AH��AH  AG7LAF�AF�DADĜAA��A>��A>Q�A=�FA<�RA;ƨA;33A:Q�A8-A6�jA5��A4~�A3dZA2�\A1A01A,��A)l�A'A%�
A%/A$5?A#��A#|�A"ȴA!�A n�A 1AXA9XA|�AVA��A�A$�A�A�A&�A�`A��AQ�A��A~�A�PA7LAVAr�AK�Ar�A�TA�9Ax�A��A  A��A{AVA��A\)A
�+A	��A	�A�A9XA�A��A;dA�AG�An�A$�A��AAM�A��A;dA ZA�^A�AZA j@���@��R@���@��@�A�@�"�@�r�@��@�I�@�n�@�@�C�@�~�@�v�@��@���@�ff@�dZ@�@�v�@��@�X@�@��@��@�=q@��D@���@�@ݙ�@��@�%@��@ӥ�@щ7@��@�7L@��;@�\)@�V@�hs@ȴ9@�|�@�V@�@���@ě�@��@��@�%@�I�@�n�@�x�@�G�@���@���@���@��-@��/@���@���@��@��@��F@�ȴ@�J@��7@�%@��9@�C�@��@���@���@�r�@�\)@��H@���@�5?@���@�?}@��9@�bN@�1@��@��@�;d@��y@�~�@�$�@�=q@�M�@�=q@��T@���@��7@�O�@��`@�Q�@��@��
@��w@��
@���@��!@��@�`B@�O�@��@��u@�(�@��@��m@��@�l�@�\)@�C�@�o@���@�=q@�$�@�@�{@�@�@�@���@���@���@���@�/@���@��D@�1'@� �@��@��@��w@���@���@��@�;d@�33@��@��!@��+@�^5@�E�@�$�@��@���@�/@���@��`@�I�@��w@��P@�\)@�K�@�33@�o@��@���@�~�@���@��@�O�@�&�@���@��9@���@��@�bN@� �@��;@���@�o@���@���@���@��\@�v�@�=q@��#@�hs@��@��@��D@�j@�9X@���@�t�@�33@�@���@��@�ȴ@�~�@�5?@���@��h@�7L@���@��D@�9X@���@��P@�\)@�o@���@�v�@�^5@�V@�E�@�J@��T@���@�hs@�X@���@���@��@�Ĝ@���@��D@�r�@�Z@�1'@�1'@�(�@�1'@�(�@��@���@��;@���@�t�@�+@��@��R@���@�v�@�ff@�n�@�E�@�{@��-@���@��h@�`B@�&�@��@��@���@���@��@��@��@�r�@�Z@�9X@��
@��P@�|�@�S�@�C�@�+@��@��!@��\@�v�@�ff@�=q@�-@�$�@�{@���@�@���@��7@�X@�V@��@��@��j@�j@�Q�@�b@�@+@~��@~ȴ@~5?@}�@}V@|�@|��@|�@|9X@{��@{ƨ@{S�@{33@{o@z��@zM�@y��@y�7@x�9@x �@w��@w;d@v�y@vv�@v$�@u��@up�@t�@t�@tz�@tI�@t1@tZ@s�@r�H@r�\@r-@q�7@pĜ@p�u@pr�@pr�@p1'@o�P@o+@n��@n�+@nv�@nV@nff@n��@n��@n5?@m��@m/@lz�@lZ@l�@kS�@j~�@jJ@i�#@i7L@h�u@h1'@g�@f��@e�@e�h@e?}@d�/@d��@d�D@dz�@dj@c��@c�F@cS�@co@b�H@b�\@b=q@bJ@a�^@a�@`�`@`r�@` �@_�@_|�@_�@^�@^��@^{@]�@]��@]p�@\�j@\�@[�F@[S�@Z�H@Z~�@ZJ@Y�#@Y&�@X��@X�u@X �@Wl�@W
=@V�R@Vff@V5?@V@U�T@U@U�-@U�@T��@T��@T9X@S�m@SS�@So@R�@Rn�@Q�7@QX@Q7L@P�9@PbN@P �@P  @P  @O�;@O��@Ol�@OK�@N�@N��@N��@Nv�@N{@M�T@M��@M��@MV@L�/@L��@MV@M/@L��@L�@Lj@L(�@K��@Kt�@KdZ@J�@Jn�@JJ@I�7@I�@H�u@H  @G�@G�@Gl�@G�@G
=@F��@Fȴ@Fff@F{@E�@E�-@E`B@E/@E�@D�@D��@D9X@C��@C��@CC�@B�!@B��@B~�@BM�@B=q@A��@A��@A�^@A�7@A&�@A%@@��@@��@@��@@�u@@1'@@  @?�w@?�P@?l�@?\)@?;d@?+@>��@>�+@>@=��@=�@=p�@=`B@<��@<��@<j@<I�@<1@;ƨ@;dZ@;"�@:��@:��@:�\@:�@9�^@9hs@9G�@9&�@9�@8��@8��@8 �@7�@7��@7�P@7;d@6�y@6�+@6ff@6$�@5�@5@5�-@5�@5/@5V@4�/@4��@4��@4z�@49X@3�m@3��@3C�@333@333@3@2��@2�\@2^5@1��@1�^@1hs@0��@0A�@0 �@01'@/�@/�w@/�P@/K�@.�@.5?@-p�@-�@-�-@-�@,�j@,�@,�D@,9X@,�@+��@+��@*�@*�H@*�H@*��@*�!@*n�@*-@*J@)��@)�#@)��@)hs@)G�@)%@(�u@(r�@(Q�@(A�@( �@'�;@'��@'�;@'�w@'��@'�P@'|�@'\)@&��@&��@&E�@&{@%�T@%��@%�-@%�@%O�@%?}@%/@%�@%V@$�@$��@$�@$9X@$(�@#�
@#�F@#t�@"�@"�!@"�\@"=q@!�#@!�7@!x�@!X@!�@ ��@ ��@ ��@ bN@ b@��@��@|�@l�@;d@
=@�y@ȴ@��@��@�+@V@5?@@�@��@�h@�h@�@O�@�@��@�@��@j@9X@�@1@��@�m@�
@��@�@C�@�H@�!@~�@-@�@��@hs@&�@�`@�9@r�@bN@bN@bN@Q�@�;@��@K�@��@ȴ@�R@�R@��@v�@{@��@p�@?}@�@�@�j@�D@j@9X@(�@�@��@ƨ@�F@��@dZ@33@@��@�\@n�@-@�@��@X@7L@�@��@��@�@r�@bN@1'@b@�@�@��@l�@;d@�@�@
=@��@�@ȴ@��@v�@ff@ff@V@E�@$�@{@{@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�M�A��A��9A��+A�v�A�\)A�C�A�33A��A���A��;A���A�A�A��HA���A��A�33A��A�ƨA���A�"�A�oA�oA�VA�1A���A�`BA�M�A�"�A���A� �A��A�-A��TA��-A�=qA���A��DA�ZA�
=A��;A���A�ƨA��-A���A���A��A�ZA�/A�{A���A���A�;dA�-A��A�n�A���A�1'A���A��mA�XA�I�A��!A�^5A�
=A��A�z�A���A�^5A��A�A���A���A��uA���A���A��9A�ƨA���A��-A���A��\A�z�A�p�A�l�A�hsA�^5A��A��HA�
=A��A��+A��A�M�A�C�A�{A��A��A�bNA��mA��;A��mA�&�A�;dA�r�A���A��^A�&�A���A���A���A�t�A�{A~v�A}l�A{�Ayt�Av^5Ar��Ap�!Ap �An��AmC�Ak�FAkl�Aj��Ai�-Ahn�Af�/Ad��AbQ�A`v�A]�mAWl�AU|�AT�jAS�mAQ�;APA�AOG�ANM�AL(�AJ~�AI�7AH��AH  AG7LAF�AF�DADĜAA��A>��A>Q�A=�FA<�RA;ƨA;33A:Q�A8-A6�jA5��A4~�A3dZA2�\A1A01A,��A)l�A'A%�
A%/A$5?A#��A#|�A"ȴA!�A n�A 1AXA9XA|�AVA��A�A$�A�A�A&�A�`A��AQ�A��A~�A�PA7LAVAr�AK�Ar�A�TA�9Ax�A��A  A��A{AVA��A\)A
�+A	��A	�A�A9XA�A��A;dA�AG�An�A$�A��AAM�A��A;dA ZA�^A�AZA j@���@��R@���@��@�A�@�"�@�r�@��@�I�@�n�@�@�C�@�~�@�v�@��@���@�ff@�dZ@�@�v�@��@�X@�@��@��@�=q@��D@���@�@ݙ�@��@�%@��@ӥ�@щ7@��@�7L@��;@�\)@�V@�hs@ȴ9@�|�@�V@�@���@ě�@��@��@�%@�I�@�n�@�x�@�G�@���@���@���@��-@��/@���@���@��@��@��F@�ȴ@�J@��7@�%@��9@�C�@��@���@���@�r�@�\)@��H@���@�5?@���@�?}@��9@�bN@�1@��@��@�;d@��y@�~�@�$�@�=q@�M�@�=q@��T@���@��7@�O�@��`@�Q�@��@��
@��w@��
@���@��!@��@�`B@�O�@��@��u@�(�@��@��m@��@�l�@�\)@�C�@�o@���@�=q@�$�@�@�{@�@�@�@���@���@���@���@�/@���@��D@�1'@� �@��@��@��w@���@���@��@�;d@�33@��@��!@��+@�^5@�E�@�$�@��@���@�/@���@��`@�I�@��w@��P@�\)@�K�@�33@�o@��@���@�~�@���@��@�O�@�&�@���@��9@���@��@�bN@� �@��;@���@�o@���@���@���@��\@�v�@�=q@��#@�hs@��@��@��D@�j@�9X@���@�t�@�33@�@���@��@�ȴ@�~�@�5?@���@��h@�7L@���@��D@�9X@���@��P@�\)@�o@���@�v�@�^5@�V@�E�@�J@��T@���@�hs@�X@���@���@��@�Ĝ@���@��D@�r�@�Z@�1'@�1'@�(�@�1'@�(�@��@���@��;@���@�t�@�+@��@��R@���@�v�@�ff@�n�@�E�@�{@��-@���@��h@�`B@�&�@��@��@���@���@��@��@��@�r�@�Z@�9X@��
@��P@�|�@�S�@�C�@�+@��@��!@��\@�v�@�ff@�=q@�-@�$�@�{@���@�@���@��7@�X@�V@��@��@��j@�j@�Q�@�b@�@+@~��@~ȴ@~5?@}�@}V@|�@|��@|�@|9X@{��@{ƨ@{S�@{33@{o@z��@zM�@y��@y�7@x�9@x �@w��@w;d@v�y@vv�@v$�@u��@up�@t�@t�@tz�@tI�@t1@tZ@s�@r�H@r�\@r-@q�7@pĜ@p�u@pr�@pr�@p1'@o�P@o+@n��@n�+@nv�@nV@nff@n��@n��@n5?@m��@m/@lz�@lZ@l�@kS�@j~�@jJ@i�#@i7L@h�u@h1'@g�@f��@e�@e�h@e?}@d�/@d��@d�D@dz�@dj@c��@c�F@cS�@co@b�H@b�\@b=q@bJ@a�^@a�@`�`@`r�@` �@_�@_|�@_�@^�@^��@^{@]�@]��@]p�@\�j@\�@[�F@[S�@Z�H@Z~�@ZJ@Y�#@Y&�@X��@X�u@X �@Wl�@W
=@V�R@Vff@V5?@V@U�T@U@U�-@U�@T��@T��@T9X@S�m@SS�@So@R�@Rn�@Q�7@QX@Q7L@P�9@PbN@P �@P  @P  @O�;@O��@Ol�@OK�@N�@N��@N��@Nv�@N{@M�T@M��@M��@MV@L�/@L��@MV@M/@L��@L�@Lj@L(�@K��@Kt�@KdZ@J�@Jn�@JJ@I�7@I�@H�u@H  @G�@G�@Gl�@G�@G
=@F��@Fȴ@Fff@F{@E�@E�-@E`B@E/@E�@D�@D��@D9X@C��@C��@CC�@B�!@B��@B~�@BM�@B=q@A��@A��@A�^@A�7@A&�@A%@@��@@��@@��@@�u@@1'@@  @?�w@?�P@?l�@?\)@?;d@?+@>��@>�+@>@=��@=�@=p�@=`B@<��@<��@<j@<I�@<1@;ƨ@;dZ@;"�@:��@:��@:�\@:�@9�^@9hs@9G�@9&�@9�@8��@8��@8 �@7�@7��@7�P@7;d@6�y@6�+@6ff@6$�@5�@5@5�-@5�@5/@5V@4�/@4��@4��@4z�@49X@3�m@3��@3C�@333@333@3@2��@2�\@2^5@1��@1�^@1hs@0��@0A�@0 �@01'@/�@/�w@/�P@/K�@.�@.5?@-p�@-�@-�-@-�@,�j@,�@,�D@,9X@,�@+��@+��@*�@*�H@*�H@*��@*�!@*n�@*-@*J@)��@)�#@)��@)hs@)G�@)%@(�u@(r�@(Q�@(A�@( �@'�;@'��@'�;@'�w@'��@'�P@'|�@'\)@&��@&��@&E�@&{@%�T@%��@%�-@%�@%O�@%?}@%/@%�@%V@$�@$��@$�@$9X@$(�@#�
@#�F@#t�@"�@"�!@"�\@"=q@!�#@!�7@!x�@!X@!�@ ��@ ��@ ��@ bN@ b@��@��@|�@l�@;d@
=@�y@ȴ@��@��@�+@V@5?@@�@��@�h@�h@�@O�@�@��@�@��@j@9X@�@1@��@�m@�
@��@�@C�@�H@�!@~�@-@�@��@hs@&�@�`@�9@r�@bN@bN@bN@Q�@�;@��@K�@��@ȴ@�R@�R@��@v�@{@��@p�@?}@�@�@�j@�D@j@9X@(�@�@��@ƨ@�F@��@dZ@33@@��@�\@n�@-@�@��@X@7L@�@��@��@�@r�@bN@1'@b@�@�@��@l�@;d@�@�@
=@��@�@ȴ@��@v�@ff@ff@V@E�@$�@{@{@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�=B�=B�PB�DB�PB�PB�VB�hB��B��B��B�BƨB�B!�BH�BVB[#B^5B_;B`BBdZBdZBdZBcTBcTBcTBgmBgmBjBq�Br�Bv�Bz�B}�B� B�7B�PB�bB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�B�B�B�%B�1B�VB��B��B�dB{�Bq�B�XBĜBȴB��B��B��B�B�B��B��B��B��B��B��BŢB�qB��Bu�BhB�mB�XB��B�Bo�BVBC�B'�B�B	7B
��B
�B
�ZB
��B
�^B
��B
�7B
v�B
k�B
VB
E�B
6FB
.B
 �B
bB	��B	�ZB	��B	��B	ĜB	�RB	�B	��B	��B	��B	�VB	�B	t�B	ffB	YB	H�B	(�B	�B	uB	PB	B��B��B�B�B�BB�)B�B�B��B��B��BɺB�XB��B��B��B�hB�=B�+B�B|�Bv�Br�Bm�BiyBffBhsBbNBXBC�B6FB/B-B,B+B)�B)�B,B(�B(�B,B.B0!B2-B49B7LB7LB6FB7LB9XB;dB>wBB�BB�BB�B>wB<jB;dB:^B7LB33B2-B0!B)�B)�B)�B%�B&�B$�B2-B2-B49B49B5?B5?B5?B49B49B33B33B2-B/B.B.B.B/B.B/B33BS�Bx�Bx�Bq�Bm�Bm�Bo�Bo�Bn�Bk�BffB^5BXBN�BM�BP�BR�BW
B\)B`BB_;BZB[#B^5B`BBn�Bw�Bw�Bu�Bq�Bo�BiyBffBe`B`BBZBXBR�BL�BK�BH�BC�BC�BE�BG�BH�BJ�BK�BK�BN�BM�BQ�BR�BS�BT�BYBYBYBZB]/B^5B_;BaHBcTBbNBdZBffBjBn�Br�Bu�By�B{�B�B�B�B�+B�=B�VB�\B�bB�hB��B��B��B��B��B��B��B��B��B��B�B�9B�RB�RB�jB�wB�}B��BĜB��B��B��B��B�B�#B�)B�NB�ZB�`B�sB�B��B��B��B��B	  B	B	B	B	B	B	B		7B	bB	oB	�B	�B	�B	�B	�B	!�B	%�B	(�B	+B	.B	/B	/B	2-B	49B	5?B	5?B	6FB	7LB	7LB	:^B	<jB	>wB	@�B	B�B	C�B	E�B	F�B	L�B	N�B	P�B	W
B	]/B	aHB	dZB	dZB	e`B	ffB	gmB	k�B	l�B	q�B	u�B	v�B	x�B	|�B	}�B	� B	�B	�B	�B	�%B	�1B	�PB	�bB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�?B	�LB	�XB	�dB	�jB	�}B	��B	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�NB	�NB	�NB	�ZB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
B
B
+B
1B
1B
1B
1B
1B
1B
+B
1B
1B
1B
DB
JB
JB
VB
VB
\B
bB
hB
oB
oB
oB
oB
uB
oB
uB
uB
{B
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
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
+B
+B
+B
+B
+B
+B
,B
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
33B
33B
33B
33B
33B
2-B
33B
33B
33B
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
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
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
A�B
B�B
B�B
B�B
B�B
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
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
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
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
K�B
L�B
K�B
K�B
L�B
M�B
M�B
M�B
N�B
N�B
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
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
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
W
B
W
B
W
B
W
B
W
B
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
YB
YB
YB
YB
YB
YB
YB
YB
ZB
YB
ZB
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
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
bNB
bNB
bNB
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
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
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
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�#B�#B�6B�)B�6B�6B�<B�4B�MB��B��B��B�tB�iB!�BH�BU�BZ�B^B_!B`'Bd@Bd@Bd&Bc:Bc Bc BgRBg8BjeBq�Br|Bv�Bz�B}�B�B�B�6B�.B�4B�gB�mB�YB�yB�_B�yB�_B��B��B�qB�kB��B�eB�MB�yB��B��B��B��B��B��B��B�<B��B��B��B��B�B�"B�eB��B�0B{�Bq�B�$B�gBȀBˬBϫB��B��B��B��BѷBϫBϫB͟B̘BňB�<B��Bu�B4B�8B�$B�MB��Bo�BU�BC{B'�BkB	B
��B
�oB
�&B
��B
�*B
��B
�B
v�B
kkB
U�B
EmB
6B
-�B
 �B
.B	��B	�&B	бB	̘B	�gB	�B	��B	��B	��B	�eB	�"B	��B	t�B	fLB	X�B	H�B	(�B	YB	@B	B	�B��B��B�oB�QB�B��B��B��B��BҽBϫBɆB�>B��B�~B�YB�NB�	B��B��B|�Bv�Br|Bm]BiDBf2Bh>BbBW�BCaB6B.�B,�B+�B*�B)�B)�B+�B(�B(�B+�B-�B/�B1�B4B7B7B6B7B9$B;0B>BBB[BB[BB[B>BB<6B;0B:*B7B2�B1�B/�B)�B)�B)�B%�B&�B$�B1�B1�B4B4B5B5B5B4B4B2�B2�B1�B.�B-�B-�B-�B.�B-�B.�B2�BS�Bx�Bx�Bq[Bm]Bm]BoiBoOBnIBkQBf2B^BW�BN�BM�BP�BR�BV�B[�B`B^�BY�BZ�B]�B`BncBw�Bw�Bu�Bq[BoiBiDBf2Be,B`BY�BW�BR�BL�BK�BH�BCaBCGBEmBGzBH�BJ�BK�BK�BN�BM�BQ�BR�BS�BT�BX�BX�BX�BY�B\�B^B_B`�Bc BbBd&Bf2Bj0BnIBr|ButBy�B{�B��B��B��B��B�	B�"B�(B�.B�4B�MB�EB�xB��B��B�vB��B��B��B��B��B��B�B�B�6B�BB�HB�UB�gBʌB̘BбB��B��B��B��B�B�&B�,B�>B�OB�B��B��B��B��B	�B	�B	�B	�B	�B	�B	�B	.B	:B	MB	2B	MB	eB	]B	!�B	%�B	(�B	*�B	-�B	.�B	.�B	1�B	4B	5B	5B	5�B	7B	6�B	:*B	<6B	>BB	@OB	BAB	CGB	EmB	FYB	L�B	N�B	P�B	V�B	\�B	aB	dB	dB	e,B	f2B	gB	kQB	lWB	q[B	utB	vzB	x�B	|�B	}�B	�B	��B	��B	��B	��B	��B	�B	�B	�4B	�4B	� B	�@B	�MB	�_B	�]B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�$B	�B	�B	�.B	�UB	�gB	�YB	�_B	ȀB	ʌB	̘B	̘B	͟B	͟B	ΥB	ϫB	бB	ҽB	ңB	��B	ԯB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	��B	��B	�&B	�2B	�B	�2B	�B	�8B	�8B	�8B	�8B	�$B	�$B	�$B	�DB	�KB	�0B	�QB	�QB	�QB	�WB	�WB	�WB	�]B	�CB	�]B	�CB	�]B	�]B	�IB	�iB	�iB	�UB	�vB	�vB	�[B	�vB	�|B	�hB	�hB	�B	�B	�tB	�tB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
"B
"B
(B
B
4B
 B
 B
 B
:B
@B
 B
@B
@B
FB
,B
@B
MB
SB
9B
?B
_B
EB
_B
_B
eB
KB
kB
kB
WB
WB
xB
~B
~B
dB
�B
�B
�B
pB
pB
pB
 vB
 vB
 �B
 �B
!�B
!�B
"�B
#�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
+�B
+�B
+�B
+�B
+�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
-�B
-�B
.�B
.�B
/�B
0�B
0�B
0�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
1�B
2�B
2�B
2�B
3�B
4B
4B
4�B
6B
6B
5�B
5�B
5�B
6�B
6�B
8B
8B
9	B
9$B
9$B
:*B
:B
:*B
:B
:B
;B
;0B
;0B
;0B
;B
;B
;0B
;0B
<6B
<B
<6B
<B
=<B
>BB
>(B
>BB
>BB
>BB
>BB
>BB
>(B
>(B
?.B
?HB
?HB
?HB
?HB
@4B
?HB
@OB
@4B
@4B
@OB
AUB
AUB
A;B
A;B
BAB
A;B
B[B
B[B
B[B
B[B
CaB
CaB
CaB
CaB
CaB
DMB
DgB
DMB
DgB
DMB
DMB
DMB
EmB
FtB
FtB
FYB
FtB
FtB
GzB
GzB
GzB
G_B
GzB
HfB
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
JrB
J�B
KxB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L~B
L~B
L�B
L~B
K�B
L�B
KxB
K�B
L~B
M�B
M�B
M�B
N�B
N�B
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
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
X�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\�B
\�B
^B
^B
^B
^B
^B
^B
_B
_B
^�B
_B
_B
_B
_B
`B
`B
_�B
`B
`B
_�B
`B
aB
aB
`�B
aB
`�B
`�B
`�B
aB
bB
bB
a�B
bB
bB
a�B
bB
a�B
c B
c B
c B
cB
cB
c B
d&B
dB
dB
dB
d&B
d&B
d&B
dB
e,B
e,B
f2B
f2B
f2B
fB
f2B
f2B
f2B
g8B
g8B
h>B
h>B
h>B
h>B
h$B
h$B
i*B
iDB
iDB
iDB
iDB
iDB
iDB
iDB
i*B
i*B
i*B
jKB
jKB
j0B
jKB
kQB
k6B
kQB
k6B
kQB
l=B
lWB
lWB
lWB
m]B
m]B
mCB
m]B
nIB
ncB
ncB
ncB
ncB
ncB
ncB
ncB
ncB
ncB
nIB
nIB
ncB
ncB
ncB
nIB
oiB
oiB
oiB
oi1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.45(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805110045382018051100453820180511004538201806042357532018060423575320180604235753JA  ARFMdecpA19c                                                                20180505213519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180505123525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180505123526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180505123527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180505123527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180505123527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180505123528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180505123528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180505123528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180505123529                      G�O�G�O�G�O�                JA  ARUP                                                                        20180505125536                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180505154333  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180510154538  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180510154538  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604145753  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                