CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-02T00:35:30Z creation;2018-09-02T00:35:33Z conversion to V3.1;2019-12-23T06:15:52Z update;     
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
resolution        =���   axis      Z        T  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  s    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ڠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180902003530  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               VA   JA  I2_0675_086                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�~4 # 1   @�~4�� @7�~($x�c75�Xy>1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D���D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�ffA33A'33AG33Ag33A���A���A���A���AÙ�Aә�A�ffA�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bj33Bq��By��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�FfC�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�FfC�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�FfC�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6�3D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�fD�NfD���D��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�Q�D���D��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��3D��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�K3D��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfDfD��fD�fD�NfDÎfD��fD�fD�NfDĎfD��fD�fD�NfDŎfD��fD�fD�NfDƎfD��fD�fD�NfDǎfD��fD�fD�NfDȎfD��fD�fD�NfDɎfD��fD�fD�NfDʎfD��fD�fD�NfDˎfD��fD�fD�NfD̎fD��fD�fD�NfD͎fD��fD�fD�NfDΎfD��fD�fD�NfDώfD��fD�fD�NfDЎfD��fD�fD�NfDюfD��fD�fD�NfDҎfD��fD�fD�NfDӎfD��fD�3D�NfDԎfD��fD�fD�NfDՎfD��fD�fD�NfD֎fD��fD�fD�NfD׎fD��fD�fD�NfD؎fD��fD�fD�NfDَfD��fD�fD�NfDڎfD��fD�fD�NfDێfD��fD�fD�NfD܎fD��fD�fD�NfDݎfD��fD�fD�NfDގfD��fD�fD�NfDߎfD��fD�fD�NfD��fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��3D�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD��fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�Q�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��yA���AͰ!A�33A���A�l�A�"�A�=qA���A�  A�XA���A��#A��A���A�ĜA�K�A�jA��A�1'A�l�A��
A�t�A��RA�S�A��mA�ĜA�C�A��9A�7LA�(�A��A���A�E�A��A�VA��A�oA�t�A�%A��9A�l�A�I�A��HA�K�A�t�A��`A��A�A���A�`BA�$�A�
=A���A��yA�M�A�ĜA��A��;A��jA�`BA�K�A��A�5?A���A��A��yA��!A�n�A�?}A��9A�ȴA��!A�A�O�A��+A���A��A�ZA��A�  A��+A��A��FA��^A��FA�7LA�G�A�1'A��#A�~�A���A�JA�33A�G�A�I�A��9A�ƨA��A�`BA�dZA��;A�r�A��A�jA��A�=qA�AhsA}%A{p�Ax�!Av�yAvv�At�jAr�Aq&�Ao?}AoXAm�Ak��AjAgp�Af��Ae�^Ac��Ab�AahsA`�\A_��A\E�A[AY�PAX�AW�AV1AT-AR�!AQx�AQ�AP�AO��ANbNALn�AI;dAEoAA��A>��A;�wA:M�A8VA6ȴA6 �A4v�A2-A0��A/dZA.��A.Q�A-��A-�PA-`BA-+A,n�A, �A+�-A*ȴA)?}A(M�A'�FA'�A&E�A%|�A$�uA$1'A#�A#C�A"JA ��AXAĜA�A�Ax�A��A-A&�A�AI�A�A�`A-A��A�Az�A�A�An�A/A=qA��AffA`BA
�`A
bNA	�A�!A��A�/A��AA�AVAffA�
A;dA �@�|�@��@�G�@���@�-@�z�@���@�C�@���@��j@��@��@�@���@��@�M�@��@��@�1@��@���@�-@�Z@�M�@۝�@��T@�9X@��@�t�@���@��;@�v�@�G�@ˮ@��@�V@��T@��`@���@�^5@ŉ7@ēu@��@\@�v�@�@���@���@�"�@�$�@���@��;@���@��@�"�@�-@��@�/@��@�~�@��^@��`@���@�  @�\)@���@�v�@�5?@���@��7@�hs@���@�Z@�b@���@���@�ȴ@�~�@�E�@�J@���@�?}@�V@�j@��@�t�@���@�M�@�p�@��D@�ƨ@��R@�{@��#@�`B@��`@�r�@�1'@�b@���@�^5@��@�p�@���@���@��#@���@�G�@�bN@��
@�dZ@���@���@�\)@�
=@���@���@�5?@�@�p�@��9@�Q�@� �@���@��@�dZ@��!@�5?@���@��-@�G�@���@�Q�@��-@���@���@��h@���@�hs@��D@��7@��@�Ĝ@�Q�@���@�1'@�  @�33@�n�@�v�@�5?@�G�@�%@��7@��h@�X@��@���@�Z@��;@��@�|�@�o@���@��+@�^5@�V@�M�@�M�@�^5@�-@���@�J@��^@�p�@��@���@�G�@�V@��@�V@���@��D@�j@�A�@��@�9X@��w@�S�@�33@�"�@�o@���@�E�@��#@���@�p�@�/@���@��`@��/@��@��@�&�@�&�@�7L@�/@��@�%@���@��@�j@�I�@�9X@��@��w@���@��@�dZ@���@�n�@���@��\@���@��7@�x�@�`B@�?}@�&�@��/@��9@��u@�z�@�Z@��
@���@�ƨ@��P@�dZ@�\)@�"�@�o@�o@��H@��R@���@��+@�=q@�@��#@���@��h@�p�@�?}@�%@���@�z�@�Z@� �@��@��@�\)@��R@�n�@�=q@��#@�x�@�7L@��@�%@���@���@��u@��D@�bN@�1'@� �@��@�b@�1@��@+@
=@
=@~�y@~��@~@}�@}/@}V@|�@|Z@{�m@{�
@{��@{�@{33@{"�@{@z�!@zM�@zJ@y�#@y�7@x�9@xr�@w�@w�P@wK�@v�@v�+@vv�@vff@vff@v{@u��@t�/@tj@t(�@s��@s�F@s�@r�\@rM�@r-@q�^@q�7@q&�@pr�@o�;@o�P@o�@nȴ@nE�@m?}@l�/@lZ@k�
@k33@j��@j��@j~�@j^5@jM�@i�#@i��@h��@hr�@h  @g�w@gl�@g�@f�+@f$�@ep�@d��@dZ@cƨ@c33@b��@bJ@a�@a�7@a�@`�9@`b@_�;@_�w@_�w@_�P@_;d@^��@^ff@^@]�T@]��@]p�@]V@\�j@\�@\�@\�D@\I�@\1@[�@["�@Z�!@Z-@Y��@Yhs@YG�@X��@X�@XbN@X �@W|�@W;d@W�@V�R@Vv�@V$�@U�T@U�h@U`B@U`B@U/@T��@Tj@S�
@SC�@R�@R��@R�!@R~�@RJ@Q��@Q7L@P��@P�@P1'@O�@O
=@N�+@N5?@M�@M�-@M?}@L�@L�/@L�@K��@K��@K33@J�@J��@J��@J^5@JJ@I��@IG�@H��@H��@HbN@H �@G�w@Gl�@F�y@F�R@E@E��@EO�@D�@D�/@D�j@Dj@DZ@D�@C�@CC�@C@B��@B^5@A��@A�^@AG�@@�`@@�`@@�`@@�9@@r�@@bN@@Q�@@b@?�@?|�@?l�@?\)@?\)@?+@?+@?�@>�R@>�+@>ff@>$�@>@=�T@=p�@=/@=V@<�@<9X@;�m@;��@;dZ@;33@:�@:��@:��@:^5@9��@9�#@9��@9��@9�7@97L@9%@8�`@8��@8��@8�9@8 �@7�@7�@7\)@7+@7+@7+@6ȴ@6�R@6��@6ff@65?@5�-@5/@4�@4�@4Z@4I�@49X@3��@3ƨ@3�@3C�@3"�@2�@2��@2^5@1��@1��@1hs@0�@0 �@/��@/\)@.��@.�y@.�R@.��@-@-`B@-�@,��@,j@,(�@+�F@+t�@+S�@+o@*�H@*��@*�!@*=q@)�@)��@)�7@)X@(��@(��@(A�@(A�@(A�@(  @'��@'�@'+@&��@&�y@&�R@&��@&V@&5?@&{@%�@%��@%V@$�D@$�@#�m@#ƨ@#�F@#�F@#ƨ@#�@#S�@#33@"�@"��@"�!@"��@!�@!�7@!hs@!hs@!hs@!X@!7L@!&�@!&�@!&�@!7L@!&�@ �9@ r�@ bN@ Q�@   @��@|�@
=@ȴ@��@v�@ff@{@�@@�h@p�@p�@`B@V@�@j@9X@9X@(�@(�@(�@1@��@��@�m@�
@��@dZ@o@n�@�@��@hs@7L@&�@%@�`@Q�@1'@ �@ �@ �@  @�;@�w@��@|�@\)@;d@�@�R@E�@5?@�@��@�h@p�@`B@?}@V@�@��@�j@�j@Z@ƨ@��@�@S�@C�@o@�!@n�@M�@��@hs@%@%@%@��@�`@��@��@Ĝ@r�@ �@  @�w@��@l�@\)@
=@�+@E�@5?@$�@{@�@�T@@��@?}@V@�/@z�@9X@(�@�@�@��@�m@ƨ@��@dZ@C�@"�@@
�@
��@
=q@
-@
=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bA�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��yA���AͰ!A�33A���A�l�A�"�A�=qA���A�  A�XA���A��#A��A���A�ĜA�K�A�jA��A�1'A�l�A��
A�t�A��RA�S�A��mA�ĜA�C�A��9A�7LA�(�A��A���A�E�A��A�VA��A�oA�t�A�%A��9A�l�A�I�A��HA�K�A�t�A��`A��A�A���A�`BA�$�A�
=A���A��yA�M�A�ĜA��A��;A��jA�`BA�K�A��A�5?A���A��A��yA��!A�n�A�?}A��9A�ȴA��!A�A�O�A��+A���A��A�ZA��A�  A��+A��A��FA��^A��FA�7LA�G�A�1'A��#A�~�A���A�JA�33A�G�A�I�A��9A�ƨA��A�`BA�dZA��;A�r�A��A�jA��A�=qA�AhsA}%A{p�Ax�!Av�yAvv�At�jAr�Aq&�Ao?}AoXAm�Ak��AjAgp�Af��Ae�^Ac��Ab�AahsA`�\A_��A\E�A[AY�PAX�AW�AV1AT-AR�!AQx�AQ�AP�AO��ANbNALn�AI;dAEoAA��A>��A;�wA:M�A8VA6ȴA6 �A4v�A2-A0��A/dZA.��A.Q�A-��A-�PA-`BA-+A,n�A, �A+�-A*ȴA)?}A(M�A'�FA'�A&E�A%|�A$�uA$1'A#�A#C�A"JA ��AXAĜA�A�Ax�A��A-A&�A�AI�A�A�`A-A��A�Az�A�A�An�A/A=qA��AffA`BA
�`A
bNA	�A�!A��A�/A��AA�AVAffA�
A;dA �@�|�@��@�G�@���@�-@�z�@���@�C�@���@��j@��@��@�@���@��@�M�@��@��@�1@��@���@�-@�Z@�M�@۝�@��T@�9X@��@�t�@���@��;@�v�@�G�@ˮ@��@�V@��T@��`@���@�^5@ŉ7@ēu@��@\@�v�@�@���@���@�"�@�$�@���@��;@���@��@�"�@�-@��@�/@��@�~�@��^@��`@���@�  @�\)@���@�v�@�5?@���@��7@�hs@���@�Z@�b@���@���@�ȴ@�~�@�E�@�J@���@�?}@�V@�j@��@�t�@���@�M�@�p�@��D@�ƨ@��R@�{@��#@�`B@��`@�r�@�1'@�b@���@�^5@��@�p�@���@���@��#@���@�G�@�bN@��
@�dZ@���@���@�\)@�
=@���@���@�5?@�@�p�@��9@�Q�@� �@���@��@�dZ@��!@�5?@���@��-@�G�@���@�Q�@��-@���@���@��h@���@�hs@��D@��7@��@�Ĝ@�Q�@���@�1'@�  @�33@�n�@�v�@�5?@�G�@�%@��7@��h@�X@��@���@�Z@��;@��@�|�@�o@���@��+@�^5@�V@�M�@�M�@�^5@�-@���@�J@��^@�p�@��@���@�G�@�V@��@�V@���@��D@�j@�A�@��@�9X@��w@�S�@�33@�"�@�o@���@�E�@��#@���@�p�@�/@���@��`@��/@��@��@�&�@�&�@�7L@�/@��@�%@���@��@�j@�I�@�9X@��@��w@���@��@�dZ@���@�n�@���@��\@���@��7@�x�@�`B@�?}@�&�@��/@��9@��u@�z�@�Z@��
@���@�ƨ@��P@�dZ@�\)@�"�@�o@�o@��H@��R@���@��+@�=q@�@��#@���@��h@�p�@�?}@�%@���@�z�@�Z@� �@��@��@�\)@��R@�n�@�=q@��#@�x�@�7L@��@�%@���@���@��u@��D@�bN@�1'@� �@��@�b@�1@��@+@
=@
=@~�y@~��@~@}�@}/@}V@|�@|Z@{�m@{�
@{��@{�@{33@{"�@{@z�!@zM�@zJ@y�#@y�7@x�9@xr�@w�@w�P@wK�@v�@v�+@vv�@vff@vff@v{@u��@t�/@tj@t(�@s��@s�F@s�@r�\@rM�@r-@q�^@q�7@q&�@pr�@o�;@o�P@o�@nȴ@nE�@m?}@l�/@lZ@k�
@k33@j��@j��@j~�@j^5@jM�@i�#@i��@h��@hr�@h  @g�w@gl�@g�@f�+@f$�@ep�@d��@dZ@cƨ@c33@b��@bJ@a�@a�7@a�@`�9@`b@_�;@_�w@_�w@_�P@_;d@^��@^ff@^@]�T@]��@]p�@]V@\�j@\�@\�@\�D@\I�@\1@[�@["�@Z�!@Z-@Y��@Yhs@YG�@X��@X�@XbN@X �@W|�@W;d@W�@V�R@Vv�@V$�@U�T@U�h@U`B@U`B@U/@T��@Tj@S�
@SC�@R�@R��@R�!@R~�@RJ@Q��@Q7L@P��@P�@P1'@O�@O
=@N�+@N5?@M�@M�-@M?}@L�@L�/@L�@K��@K��@K33@J�@J��@J��@J^5@JJ@I��@IG�@H��@H��@HbN@H �@G�w@Gl�@F�y@F�R@E@E��@EO�@D�@D�/@D�j@Dj@DZ@D�@C�@CC�@C@B��@B^5@A��@A�^@AG�@@�`@@�`@@�`@@�9@@r�@@bN@@Q�@@b@?�@?|�@?l�@?\)@?\)@?+@?+@?�@>�R@>�+@>ff@>$�@>@=�T@=p�@=/@=V@<�@<9X@;�m@;��@;dZ@;33@:�@:��@:��@:^5@9��@9�#@9��@9��@9�7@97L@9%@8�`@8��@8��@8�9@8 �@7�@7�@7\)@7+@7+@7+@6ȴ@6�R@6��@6ff@65?@5�-@5/@4�@4�@4Z@4I�@49X@3��@3ƨ@3�@3C�@3"�@2�@2��@2^5@1��@1��@1hs@0�@0 �@/��@/\)@.��@.�y@.�R@.��@-@-`B@-�@,��@,j@,(�@+�F@+t�@+S�@+o@*�H@*��@*�!@*=q@)�@)��@)�7@)X@(��@(��@(A�@(A�@(A�@(  @'��@'�@'+@&��@&�y@&�R@&��@&V@&5?@&{@%�@%��@%V@$�D@$�@#�m@#ƨ@#�F@#�F@#ƨ@#�@#S�@#33@"�@"��@"�!@"��@!�@!�7@!hs@!hs@!hs@!X@!7L@!&�@!&�@!&�@!7L@!&�@ �9@ r�@ bN@ Q�@   @��@|�@
=@ȴ@��@v�@ff@{@�@@�h@p�@p�@`B@V@�@j@9X@9X@(�@(�@(�@1@��@��@�m@�
@��@dZ@o@n�@�@��@hs@7L@&�@%@�`@Q�@1'@ �@ �@ �@  @�;@�w@��@|�@\)@;d@�@�R@E�@5?@�@��@�h@p�@`B@?}@V@�@��@�j@�j@Z@ƨ@��@�@S�@C�@o@�!@n�@M�@��@hs@%@%@%@��@�`@��@��@Ĝ@r�@ �@  @�w@��@l�@\)@
=@�+@E�@5?@$�@{@�@�T@@��@?}@V@�/@z�@9X@(�@�@�@��@�m@ƨ@��@dZ@C�@"�@@
�@
��@
=q@
-@
=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BM�BO�BO�BO�BN�BN�BN�BN�BO�BO�BO�BP�BO�BO�BO�BQ�BR�BR�BR�BS�BR�BR�BT�BYBffBiyBjBn�Bz�B��B�B�B��B��Bx�Br�BjBgmBiyBffBo�Bv�Bx�B}�B� B�B�B�B�+B�JB�DB�B|�B}�B�B�+B�PB�hB�oB�B�B�=B~�B�B�B~�B�DB�VB��B�{B�+B{�B}�B}�B}�Bz�Bx�By�B}�Bs�Bm�BgmBiyBw�B�1B�oB�%BhsB_;Bq�B~�B|�B�B|�Bq�B\)BA�B+B"�BbB%BbB��B�B�TB��B�9B�LB�\Bv�Bm�Bv�B�JBo�BO�B/B�BuB"�B33B0!B!�BB
��B
�ZB
�^B
��B
�B
|�B
gmB
H�B
/B
2-B
#�B
\B	��B	�mB	�ZB	�/B	��B	ƨB	�wB	�mB	�`B	�B	��B	�LB	�B	��B	��B	�\B	�B	�B	|�B	cTB	XB	T�B	S�B	I�B	A�B	9XB	0!B	'�B	#�B	 �B	�B	JB�B�)B�3B��Bw�BaHBVBG�B<jB9XB/B&�B �B2-B<jB@�B@�BA�B@�B@�BA�B@�B?}B@�BA�B?}B<jB=qB<jB:^B9XB8RB7LB7LB6FB7LB5?B5?B49B49B33B49B33B49B49B5?B5?B49B6FB5?B5?B5?B6FB5?B49B49B33B5?B5?B49B2-B0!B1'B.B-B/B-B-B-B,B,B+B+B)�B+B(�B'�B(�B(�B(�B(�B'�B'�B+B+B(�B+B,B-B/B.B.B-B,B)�B(�B&�B'�B(�B'�B)�B(�B-B0!B0!B2-B49B8RB:^B;dB;dB<jBB�BB�BD�BE�BK�BM�BN�BO�BO�BO�BO�BO�BR�BR�BQ�BR�BS�BT�BW
BXB[#B`BBbNBgmBhsBm�Bp�Bt�Bv�Bw�By�Bz�B{�B~�B�B� B�B�1B�DB�VB�\B�hB�hB�{B��B��B��B��B��B��B��B��B�B�'B�'B�'B�3B�LB�RB�LB�LB�RB�LB�XB�wB��BƨB��B��B��B��B��B��B�
B�B�B�BB�B�B�B��B��B	%B	
=B	JB	\B	uB	�B	�B	�B	�B	!�B	#�B	%�B	(�B	33B	6FB	:^B	;dB	B�B	C�B	B�B	N�B	Q�B	P�B	P�B	R�B	VB	XB	ZB	YB	YB	YB	ZB	ZB	aHB	dZB	dZB	dZB	e`B	hsB	iyB	iyB	iyB	jB	iyB	jB	n�B	q�B	t�B	t�B	v�B	w�B	x�B	y�B	{�B	}�B	~�B	�B	�B	�B	�+B	�=B	�JB	�VB	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�LB	�RB	�^B	�jB	�qB	�}B	��B	�wB	�}B	��B	B	B	B	B	ŢB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�TB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B

=B

=B
JB
JB
PB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
hB
hB
oB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
 �B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
&�B
'�B
(�B
)�B
)�B
)�B
+B
,B
-B
-B
-B
.B
.B
.B
/B
/B
/B
/B
/B
/B
/B
.B
/B
/B
/B
/B
0!B
0!B
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
49B
49B
5?B
5?B
5?B
5?B
49B
5?B
49B
49B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
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
:^B
:^B
:^B
:^B
;dB
;dB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
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
E�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
G�B
G�B
H�B
I�B
H�B
H�B
H�B
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
K�B
L�B
L�B
L�B
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
P�B
O�B
O�B
O�B
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
S�B
S�B
S�B
S�B
T�B
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
YB
YB
YB
YB
YB
YB
YB
ZB
[#B
ZB
\)B
\)B
]/B
]/B
]/B
]/B
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
^5B
_;B
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
bNB
bNB
cTB
cTB
dZB
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
hsB
hsB
hsB
iyB
iyB
iyB
iyB
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
l�B
l�B
l�B
l�B
l�B
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
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BM�BO�BO�BO�BN�BN�BN�BN�BO�BO�BO�BP�BO�BO�BO�BQ�BR�BR�BR�BS�BR�BR�BT�BX�BfLBi_BjKBn}Bz�B��B��B��B��B��Bx�Br�BjeBgRBi_Bf2BoiBv�Bx�B}�B�B��B��B�B�B�0B�)B��B|�B}�B��B��B�6B�4B�TB��B��B�	B~�B��B��B~�B�)B�"B�YB�aB�B{�B}�B}�B}�Bz�Bx�By�B}�Bs�Bm]Bg8Bi_Bw�B��B�:B�Bh>B_BqvB~�B|�B��B|�BqvB\BAUB*�B"�B.BBHB��B�QB� B�oB�B�B�(Bv�Bm]Bv�B�0BoiBO�B/ B�B@B"�B2�B0B!�B�B
��B
�&B
�*B
��B
��B
|�B
gRB
H�B
.�B
1�B
#�B
BB	��B	�8B	�&B	��B	̘B	ƎB	�BB	�8B	�FB	��B	͟B	�B	��B	��B	�SB	�BB	��B	��B	|�B	c B	W�B	T�B	S�B	I�B	AoB	9$B	/�B	'�B	#�B	 �B	qB	B�B��B��B�gBw�BaBU�BGzB<6B9$B/ B&�B �B2B<6B@OB@OBAUB@OB@OBAUB@OB?HB@OBAUB?HB<6B=<B<6B:*B9$B8B7B7B6B7B5B5B4B4B2�B4B2�B4B4B5B5B4B6B5B5B5B5�B5B4B4B2�B5B5B4B1�B/�B0�B-�B,�B.�B,�B,�B,�B+�B+�B*�B*�B)�B*�B(�B'�B(�B(�B(�B(�B'�B'�B*�B*�B(�B*�B+�B,�B.�B-�B-�B,�B+�B)�B(�B&�B'�B(�B'�B)�B(�B,�B/�B/�B1�B4B8B:*B;0B;0B<6BB[BBABDgBEmBK�BM�BN�BO�BO�BO�BO�BO�BR�BR�BQ�BR�BS�BT�BV�BW�BZ�B`BbBg8Bh>BmCBpUBtnBv�Bw�By�Bz�B{�B~�B��B�B��B��B��B�B�(B�4B�4B�FB�YB�eB�kB�qB�pB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�$B�BB�UB�tBʌB˒B͟BΥB͟BҽBּB��B��B�B�KB�]B�|B��B��B	�B		�B	�B	(B	@B	_B	~B	dB	jB	!|B	#�B	%�B	(�B	2�B	6B	:*B	;0B	B[B	CaB	BAB	N�B	Q�B	P�B	P�B	R�B	U�B	W�B	Y�B	X�B	X�B	X�B	Y�B	Y�B	`�B	d&B	d&B	dB	e,B	h$B	iDB	iDB	i*B	j0B	i*B	jKB	ncB	qvB	t�B	tnB	v�B	w�B	x�B	y�B	{�B	}�B	~�B	��B	��B	��B	��B	�	B	�B	�B	�.B	�.B	�4B	�SB	�MB	�SB	�YB	�?B	�_B	�QB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�6B	�<B	�HB	�OB	�(B	�HB	�UB	�[B	�AB	�[B	�[B	�mB	�YB	�zB	�zB	ɆB	˒B	�~B	̘B	̈́B	̈́B	ΥB	ΥB	ϑB	бB	бB	ѷB	��B	ԯB	��B	��B	ּB	��B	ּB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	� B	�&B	� B	�,B	�B	�>B	�DB	�KB	�CB	�cB	�cB	�cB	�iB	�OB	�iB	�iB	�UB	�[B	�vB	�vB	�vB	�[B	�aB	�|B	�|B	�|B	�B	�B	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	�B

	B
B
�B
B
"B
"B
"B
"B
B
(B
(B
B
B
.B
B
4B
B
:B
 B
:B
:B
&B
FB
2B
MB
MB
2B
9B
9B
?B
_B
_B
_B
EB
EB
eB
KB
KB
QB
QB
QB
QB
kB
qB
WB
QB
qB
qB
qB
qB
xB
xB
xB
xB
xB
xB
]B
dB
�B
�B
jB
�B
jB
�B
�B
�B
�B
�B
�B
�B
 vB
!|B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
&�B
'�B
(�B
)�B
)�B
)�B
*�B
+�B
,�B
,�B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
.�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
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
4B
4B
5B
4�B
5B
5B
4B
5B
4B
4B
4B
4�B
5B
4�B
5B
5�B
6B
5�B
6B
6B
7B
6�B
7B
8B
8B
8B
8B
8B
9	B
9$B
9	B
9	B
9$B
:*B
:B
:*B
:*B
;0B
;B
:*B
;0B
;0B
;0B
<6B
<B
<B
<6B
="B
>BB
?.B
?HB
?.B
@OB
@OB
@OB
@OB
A;B
AUB
AUB
AUB
AUB
BAB
B[B
B[B
CGB
CaB
CaB
CaB
CaB
DMB
DgB
DMB
ESB
DgB
DgB
DgB
DgB
DgB
DgB
DMB
EmB
GzB
G_B
HfB
I�B
H�B
H�B
H�B
H�B
HfB
H�B
I�B
I�B
I�B
I�B
JrB
J�B
J�B
J�B
K�B
L�B
L�B
L~B
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
P�B
O�B
O�B
O�B
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
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
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
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Z�B
Y�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
]�B
^B
^B
_B
^�B
_B
_B
]�B
^�B
`B
`B
`B
_�B
`�B
aB
`�B
aB
aB
aB
`�B
aB
aB
bB
a�B
a�B
bB
bB
bB
cB
c B
d&B
eB
f2B
f2B
f2B
f2B
f2B
f2B
fB
f2B
g8B
g8B
g8B
h>B
h$B
h>B
i*B
i*B
iDB
i*B
jKB
jKB
j0B
jKB
jKB
kQB
k6B
kQB
kQB
k6B
k6B
lWB
l=B
lWB
lWB
l=B
l=B
lWB
l=B
lWB
m]B
m]B
m]B
nIB
ncB
nIB
ncB
oiB
oiG�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.45(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809070054302018090700543020180907005430201809080035152018090800351520180908003515JA  ARFMdecpA19c                                                                20180902093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180902003530  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180902003531  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180902003531  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180902003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180902003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180902003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180902003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180902003532  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180902003533                      G�O�G�O�G�O�                JA  ARUP                                                                        20180902005617                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180902154156  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180903000000  CF  PSAL_ADJUSTED_QCD��3D��3G�O�                JM  ARCAJMQC2.0                                                                 20180906155430  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180906155430  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180907153515  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                