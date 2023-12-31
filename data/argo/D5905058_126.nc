CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-02-19T21:36:38Z creation;2019-02-19T21:36:41Z conversion to V3.1;2019-12-23T06:06:42Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190219213638  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ~A   JA  I2_0675_126                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ب�9D�1   @ب�O���@7��S����c4%��1�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�ffA��A'33AG33Ag33A���A���A���A���AÙ�Aә�A㙚A�B��B	��BffBffB!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fBԳ3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3CY�Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`#3D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�K3D��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��3D�3D�NfD��fD��fD�fD�Q�D��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfDfD��fD�fD�NfDÎfD��fD�fD�NfDĎfD��fD�fD�NfDŎfD��fD�fD�NfDƎfD��fD�fD�NfDǎfD��fD�fD�NfDȎfD��fD�fD�NfDɎfD��fD�fD�NfDʎfD��fD�fD�NfDˎfD��fD�fD�NfD̎fD��fD�fD�NfD͎fD��fD�fD�NfDΎfD��fD�fD�NfDώfD��fD�fD�NfDЎfD��fD�fD�NfDюfD��fD�fD�NfDҎfD��fD�fD�NfDӎfD��fD�fD�NfDԎfD��fD�fD�NfDՎfD��fD�fD�NfD֎fD��fD�fD�NfD׎fD��fD�fD�NfD؎fD��fD�fD�NfDَfD��fD�fD�NfDڎfD��fD�fD�NfDێfD��fD�fD�NfD܎fD��fD�fD�NfDݎfD��fD�fD�NfDގfD��fD�fD�NfDߎfD��fD�fD�NfD��fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD��fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�Q�D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��9A��FA��FA��jA���A�Q�A�K�A�I�A�I�A�M�A�I�A�M�A�bNA�n�A�x�A�|�A�z�A�z�A�v�A�hsA�I�A�"�A�A��HA�ĜA��A���A��hA��+A�dZA�O�A�G�A�;dA�1'A�-A�"�A��A��A��A�bA���A��/A���A���A�ƨA���A��RA���A���A���A��\A��7A�x�A�hsA�\)A�S�A�G�A�;dA�JA�O�A��TA���A�dZA��;A�XA�"�A�VA�p�A�5?A���A�dZA� �A��A�n�A��A��A��wA��A�$�A���A��A��mA�JA�ffA�z�A���A��;A�M�A��A�-A��A��yA���A���A��A���A��+A���A�S�A���A�^5A���A��A�O�A�O�A�E�A��\A��A���A�{A�ƨA��DA�
=A�9XA���A�ffA���A�  A�9XA�^A~�A|z�A{XAyx�Ax��Aw�Av��At��Aq�An��Aj��AhZAf�Aep�Ad��AchsAa�A^��A]A\�\A\�A[��AZ�`AZ1'AY33AW�FAU�AP��AN�/AK
=AH�/AG��AF�yAD��AA�A?/A;�mA8ĜA6�uA5;dA41A3�A2M�A0r�A0{A/A.�uA-hsA-oA,5?A+?}A*ffA(�9A'�7A%�
A#�TA"�9A!�A!�-A!|�A!+A �jA n�A�mA��A�AC�A(�Ap�A��A��A�A��AQ�A��A��A�^A��A(�AA�AhsA�DA��A"�A�uA5?A��A�FAVA
��A
Q�A	��A��A�hAbA?}A�A��AjA��A v�@���@���@�O�@���@�r�@���@��H@���@�b@�hs@�A�@�dZ@��@�bN@�@�K�@���@��@�Z@�@��@��@�bN@㝲@���@��@�X@���@�j@�"�@ܼj@�33@�v�@ڇ+@�=q@؛�@���@�{@�@�&�@ԣ�@�bN@�(�@�ƨ@�^5@�hs@��@���@� �@�"�@�J@ȋD@�o@Ə\@�J@ř�@�O�@ģ�@�z�@�b@���@���@�K�@��-@�/@�Q�@�^5@��-@�`B@��@��
@��@�l�@��y@�E�@���@�hs@�t�@�5?@���@��^@�&�@���@��@��@�@�^5@���@�7L@���@�1@�33@��R@�M�@�J@��-@�O�@���@�bN@�9X@��;@��F@�33@�E�@�J@��h@�X@�&�@���@��@��D@�b@��@���@�S�@�"�@�E�@���@��@��#@��-@�p�@�X@�?}@�&�@��@���@���@��j@��9@��j@��j@���@�r�@�9X@��F@�+@�-@��@���@�?}@��@���@���@��j@��u@�(�@��
@��@�l�@�K�@��@�v�@�E�@��-@�?}@��`@��@�r�@�Z@�b@��@���@�l�@�
=@��@�~�@�{@�J@��@��-@�hs@���@��@��@�z�@�A�@��;@�  @��@���@���@��+@�n�@�E�@�$�@�=q@�-@��T@���@�X@�O�@�`B@�?}@�/@��@��@�V@���@��9@��u@�Q�@�A�@�Z@���@�A�@���@�dZ@��y@�o@�C�@�o@��@���@�v�@�E�@�-@�$�@�~�@��\@�ff@�ff@�E�@�$�@��h@�?}@�/@��@�V@���@���@��@��/@��@�j@�Z@�9X@�1'@�1@��m@�|�@��@��H@��H@��H@��H@�ȴ@���@�ff@�E�@��@��^@�G�@��@�%@��/@��j@��@�I�@� �@���@�t�@�l�@�dZ@�33@��y@���@���@���@�ff@��@��-@�`B@�G�@�7L@�V@���@�1'@�1@�  @���@�  @�1@���@��
@��@���@�@���@�n�@�M�@�5?@���@��@�7L@��9@��@�Z@�9X@� �@�@��@\)@;d@~�y@~�R@~��@~$�@}p�@|��@|�j@|j@|1@{��@{ƨ@{t�@{33@z�H@zn�@y�#@y7L@x�`@xbN@xb@w��@wK�@w+@v�R@v{@u�-@u/@t��@t�j@tZ@t(�@sƨ@s�@sC�@rM�@q�#@q��@qx�@qhs@q7L@p��@p  @o�;@o|�@n�y@nff@n@m�h@l�@l��@l�D@l�@k�@kS�@kC�@ko@j��@i��@ihs@iG�@i7L@h�`@hĜ@h��@hA�@g�;@g��@g|�@g;d@g
=@f��@f�+@fff@f{@e�-@e`B@d��@d��@dj@dI�@d(�@d1@cƨ@c��@c��@cdZ@c@b�!@b~�@bn�@bn�@a��@a��@a��@a&�@`bN@`  @_��@_K�@_
=@^�y@^ȴ@^5?@]��@]�h@]�@]p�@]�@\��@\j@\Z@\1@[t�@[o@Z^5@Y�#@Y��@Y�@X�9@XQ�@W�@WK�@W+@V�y@Vȴ@Vv�@U�@Up�@U?}@U/@T�@T��@Tz�@T9X@S��@St�@S33@R�H@R�\@RM�@RJ@Q�#@Q��@QX@Q%@P�@P1'@O�w@O|�@O;d@Nȴ@Nff@N@M��@M��@MO�@L�@L��@Lj@L(�@K��@KC�@K@J�!@J��@J-@I�@I�7@Ihs@IG�@I%@H��@H��@Hr�@HQ�@Hb@G�;@G�@G+@Fȴ@F�+@F$�@E�T@E��@E�@EO�@D��@Dz�@D9X@C��@C��@CC�@C@B�!@B=q@A�^@Ax�@A&�@@�`@@�9@@r�@@ �@@  @?�w@?K�@?
=@>�@>��@>�+@>$�@=�T@=�-@=�-@=O�@<�@<��@<�j@<z�@<Z@<I�@;��@;t�@;"�@;o@:�!@:^5@:J@9��@9G�@97L@9&�@8Ĝ@81'@8b@7�@7�;@7�w@7�@7l�@7
=@6��@65?@5@5?}@5V@4�j@4��@4z�@4I�@49X@4(�@4�@3��@3�@3t�@3S�@3C�@3o@2�@2��@2�!@2��@2�\@2n�@2�@1�@1hs@0�9@0r�@0r�@0bN@0Q�@0A�@0 �@/;d@/+@/+@/+@/+@/�@.�y@.��@.v�@.ff@.5?@.@-�T@-�-@-��@-`B@-?}@,�@,�j@,z�@,1@+�
@+�F@+��@+t�@+"�@*��@*�\@*M�@)��@)�^@)�7@)X@)�@(��@(�9@(bN@(b@'��@'�@'�P@'\)@'
=@&�R@&ff@&E�@&@%��@%@%p�@%?}@$��@$��@$I�@$�@#ƨ@#C�@#o@#@"�@"�H@"�!@"^5@"M�@"-@!�#@!hs@!�@ �@ 1'@��@\)@��@�R@��@�+@v�@V@�@?}@�/@�@�j@�j@�j@�j@��@z�@I�@9X@�@1@�
@��@33@�@�!@n�@��@�#@�^@x�@G�@7L@Ĝ@bN@A�@b@��@K�@+@�y@ȴ@�R@�R@��@��@ff@@��@?}@��@�@�D@z�@I�@(�@��@�m@��@t�@"�@�!@n�@^5@=q@�@�@J@J@�@�^@x�@hs@X@G�@X@7L@%@��@�@�@�@�@�@r�@bN@  @�w@|�@\)@;d@
=@��@�y@ȴ@v�@5?@{@@��@�@�@`B@V@��@�@�D@j@Z@�m@�@�@�@t�@dZ@S�@33@o@
�@
��@
�!@
n�@
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��9A��FA��FA��jA���A�Q�A�K�A�I�A�I�A�M�A�I�A�M�A�bNA�n�A�x�A�|�A�z�A�z�A�v�A�hsA�I�A�"�A�A��HA�ĜA��A���A��hA��+A�dZA�O�A�G�A�;dA�1'A�-A�"�A��A��A��A�bA���A��/A���A���A�ƨA���A��RA���A���A���A��\A��7A�x�A�hsA�\)A�S�A�G�A�;dA�JA�O�A��TA���A�dZA��;A�XA�"�A�VA�p�A�5?A���A�dZA� �A��A�n�A��A��A��wA��A�$�A���A��A��mA�JA�ffA�z�A���A��;A�M�A��A�-A��A��yA���A���A��A���A��+A���A�S�A���A�^5A���A��A�O�A�O�A�E�A��\A��A���A�{A�ƨA��DA�
=A�9XA���A�ffA���A�  A�9XA�^A~�A|z�A{XAyx�Ax��Aw�Av��At��Aq�An��Aj��AhZAf�Aep�Ad��AchsAa�A^��A]A\�\A\�A[��AZ�`AZ1'AY33AW�FAU�AP��AN�/AK
=AH�/AG��AF�yAD��AA�A?/A;�mA8ĜA6�uA5;dA41A3�A2M�A0r�A0{A/A.�uA-hsA-oA,5?A+?}A*ffA(�9A'�7A%�
A#�TA"�9A!�A!�-A!|�A!+A �jA n�A�mA��A�AC�A(�Ap�A��A��A�A��AQ�A��A��A�^A��A(�AA�AhsA�DA��A"�A�uA5?A��A�FAVA
��A
Q�A	��A��A�hAbA?}A�A��AjA��A v�@���@���@�O�@���@�r�@���@��H@���@�b@�hs@�A�@�dZ@��@�bN@�@�K�@���@��@�Z@�@��@��@�bN@㝲@���@��@�X@���@�j@�"�@ܼj@�33@�v�@ڇ+@�=q@؛�@���@�{@�@�&�@ԣ�@�bN@�(�@�ƨ@�^5@�hs@��@���@� �@�"�@�J@ȋD@�o@Ə\@�J@ř�@�O�@ģ�@�z�@�b@���@���@�K�@��-@�/@�Q�@�^5@��-@�`B@��@��
@��@�l�@��y@�E�@���@�hs@�t�@�5?@���@��^@�&�@���@��@��@�@�^5@���@�7L@���@�1@�33@��R@�M�@�J@��-@�O�@���@�bN@�9X@��;@��F@�33@�E�@�J@��h@�X@�&�@���@��@��D@�b@��@���@�S�@�"�@�E�@���@��@��#@��-@�p�@�X@�?}@�&�@��@���@���@��j@��9@��j@��j@���@�r�@�9X@��F@�+@�-@��@���@�?}@��@���@���@��j@��u@�(�@��
@��@�l�@�K�@��@�v�@�E�@��-@�?}@��`@��@�r�@�Z@�b@��@���@�l�@�
=@��@�~�@�{@�J@��@��-@�hs@���@��@��@�z�@�A�@��;@�  @��@���@���@��+@�n�@�E�@�$�@�=q@�-@��T@���@�X@�O�@�`B@�?}@�/@��@��@�V@���@��9@��u@�Q�@�A�@�Z@���@�A�@���@�dZ@��y@�o@�C�@�o@��@���@�v�@�E�@�-@�$�@�~�@��\@�ff@�ff@�E�@�$�@��h@�?}@�/@��@�V@���@���@��@��/@��@�j@�Z@�9X@�1'@�1@��m@�|�@��@��H@��H@��H@��H@�ȴ@���@�ff@�E�@��@��^@�G�@��@�%@��/@��j@��@�I�@� �@���@�t�@�l�@�dZ@�33@��y@���@���@���@�ff@��@��-@�`B@�G�@�7L@�V@���@�1'@�1@�  @���@�  @�1@���@��
@��@���@�@���@�n�@�M�@�5?@���@��@�7L@��9@��@�Z@�9X@� �@�@��@\)@;d@~�y@~�R@~��@~$�@}p�@|��@|�j@|j@|1@{��@{ƨ@{t�@{33@z�H@zn�@y�#@y7L@x�`@xbN@xb@w��@wK�@w+@v�R@v{@u�-@u/@t��@t�j@tZ@t(�@sƨ@s�@sC�@rM�@q�#@q��@qx�@qhs@q7L@p��@p  @o�;@o|�@n�y@nff@n@m�h@l�@l��@l�D@l�@k�@kS�@kC�@ko@j��@i��@ihs@iG�@i7L@h�`@hĜ@h��@hA�@g�;@g��@g|�@g;d@g
=@f��@f�+@fff@f{@e�-@e`B@d��@d��@dj@dI�@d(�@d1@cƨ@c��@c��@cdZ@c@b�!@b~�@bn�@bn�@a��@a��@a��@a&�@`bN@`  @_��@_K�@_
=@^�y@^ȴ@^5?@]��@]�h@]�@]p�@]�@\��@\j@\Z@\1@[t�@[o@Z^5@Y�#@Y��@Y�@X�9@XQ�@W�@WK�@W+@V�y@Vȴ@Vv�@U�@Up�@U?}@U/@T�@T��@Tz�@T9X@S��@St�@S33@R�H@R�\@RM�@RJ@Q�#@Q��@QX@Q%@P�@P1'@O�w@O|�@O;d@Nȴ@Nff@N@M��@M��@MO�@L�@L��@Lj@L(�@K��@KC�@K@J�!@J��@J-@I�@I�7@Ihs@IG�@I%@H��@H��@Hr�@HQ�@Hb@G�;@G�@G+@Fȴ@F�+@F$�@E�T@E��@E�@EO�@D��@Dz�@D9X@C��@C��@CC�@C@B�!@B=q@A�^@Ax�@A&�@@�`@@�9@@r�@@ �@@  @?�w@?K�@?
=@>�@>��@>�+@>$�@=�T@=�-@=�-@=O�@<�@<��@<�j@<z�@<Z@<I�@;��@;t�@;"�@;o@:�!@:^5@:J@9��@9G�@97L@9&�@8Ĝ@81'@8b@7�@7�;@7�w@7�@7l�@7
=@6��@65?@5@5?}@5V@4�j@4��@4z�@4I�@49X@4(�@4�@3��@3�@3t�@3S�@3C�@3o@2�@2��@2�!@2��@2�\@2n�@2�@1�@1hs@0�9@0r�@0r�@0bN@0Q�@0A�@0 �@/;d@/+@/+@/+@/+@/�@.�y@.��@.v�@.ff@.5?@.@-�T@-�-@-��@-`B@-?}@,�@,�j@,z�@,1@+�
@+�F@+��@+t�@+"�@*��@*�\@*M�@)��@)�^@)�7@)X@)�@(��@(�9@(bN@(b@'��@'�@'�P@'\)@'
=@&�R@&ff@&E�@&@%��@%@%p�@%?}@$��@$��@$I�@$�@#ƨ@#C�@#o@#@"�@"�H@"�!@"^5@"M�@"-@!�#@!hs@!�@ �@ 1'@��@\)@��@�R@��@�+@v�@V@�@?}@�/@�@�j@�j@�j@�j@��@z�@I�@9X@�@1@�
@��@33@�@�!@n�@��@�#@�^@x�@G�@7L@Ĝ@bN@A�@b@��@K�@+@�y@ȴ@�R@�R@��@��@ff@@��@?}@��@�@�D@z�@I�@(�@��@�m@��@t�@"�@�!@n�@^5@=q@�@�@J@J@�@�^@x�@hs@X@G�@X@7L@%@��@�@�@�@�@�@r�@bN@  @�w@|�@\)@;d@
=@��@�y@ȴ@v�@5?@{@@��@�@�@`B@V@��@�@�D@j@Z@�m@�@�@�@t�@dZ@S�@33@o@
�@
��@
�!@
n�@
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bz�B�Bz�B�B��B�B�B�B�B�B�'B�LB�qBBŢBǮBɺBɺB��B��B��B��BɺBɺB��B��B��B��B��B��B�B�)B�BB�NB�TB�ZB�TB�fB�yB�B�B��B��B��B��B��B��BBBBBBDBuB�B�B�B!�B!�B%�BXBffBdZBdZBm�Bm�Bl�Bl�BjBjBjBk�BiyBiyBffBbNB^5BW
BW
BS�BN�BL�B@�B8RB+B!�B{BJB1B��B��B�B�)B��B��B�3B�B��B��B�VB�+B}�Bq�BgmBXBE�B;dB33B-B#�B�B
=B
��B
��B
�B
x�B
YB
7LB
)�B
%�B
�B
hB
B	��B	�B	�mB	�BB	��B	��B	��B	�hB	y�B	p�B	dZB	^5B	S�B	C�B	33B	,B	$�B	!�B	�B	�B	�B	uB		7B	  B�TB��BŢBĜB��B�wB�XB�-B��B��B��B�bB�PB�%B�B�PB�=B�7B�1B�%B�B� B|�Bz�Bu�Bn�BffB`BBT�BO�BL�BK�BJ�BJ�BJ�BI�BJ�BJ�BJ�BJ�BK�BI�BH�BG�BE�BD�BD�BB�BB�B?}B=qB:^B9XB8RB7LB6FB6FB5?B49B2-B49B2-B1'B1'B0!B/B/B-B-B+B)�B)�B'�B)�B'�B'�B&�B&�B'�B&�B&�B&�B&�B&�B(�B(�B(�B(�B(�B)�B,B0!B-B-B+B+B,B.B-B.B/B0!B1'B33B7LB33B33B49B:^B@�BA�B=qB<jBA�BI�BK�BM�BN�BP�BR�BT�BW
BW
BVBT�BN�BJ�BJ�BK�BR�BS�BS�BT�BVBXB]/Be`BffBaHB`BBcTBk�Bl�Bm�Bo�Bq�Br�Bu�Bv�Bx�By�Bz�B|�B~�B�B�B�%B�+B�7B�JB�\B�hB�hB�oB�uB��B��B��B��B��B��B��B��B��B�B�B�B�'B�FB�FB�^B�jB�wB��B��BBǮBɺB��B��B��B�)B�BB�BB�NB�ZB�mB�sB�B�B�B�B��B��B��B��B��B��B��B	%B	PB	PB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	"�B	$�B	&�B	(�B	.B	0!B	2-B	49B	7LB	7LB	9XB	=qB	>wB	@�B	E�B	F�B	H�B	K�B	Q�B	T�B	VB	YB	\)B	]/B	_;B	_;B	aHB	e`B	jB	m�B	m�B	p�B	s�B	s�B	u�B	y�B	{�B	~�B	�B	�B	�B	�B	�%B	�+B	�7B	�PB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�?B	�LB	�LB	�RB	�LB	�RB	�XB	�^B	�jB	�qB	�qB	�qB	�}B	B	ÖB	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
	7B
	7B

=B

=B

=B

=B
DB
DB
DB
JB
PB
PB
PB
VB
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
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
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
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
7LB
8RB
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
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
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
A�B
A�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
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
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
I�B
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
L�B
L�B
L�B
L�B
L�B
M�B
M�B
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
S�B
S�B
S�B
S�B
S�B
T�B
T�B
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
\)B
]/B
]/B
]/B
]/B
]/B
]/B
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
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
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
bNB
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
e`B
e`B
e`B
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
l�B
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
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bz�B��Bz�B��B��B��B��B��B��B��B��B�B�VB�[B�mB�zBɆBɠBʌB˒B̘BʦBɠBɆBʌB˒BˬB͟BΥB��B��B��B�'B�4B� B�&B� B�2B�DB�]B�vB��B��B��B��B��B��B �B �B �B�B�B)B[B�B�B�B!�B!�B%�BW�BfLBd@Bd&Bm]Bm]BlWBlWBjKBjeBjeBkQBi_Bi_Bf2BbB^BV�BV�BS�BN�BL�B@iB8B*�B!�BaBBB��B��B�]B��B͟B�oB��B��B��B�sB�<B��B}�Bq�Bg8BW�BEmB;0B2�B,�B#�BxB
	B
��B
�qB
��B
x�B
X�B
72B
)�B
%�B
�B
4B
�B	��B	�iB	�8B	�B	ѷB	�iB	��B	�4B	y�B	poB	d&B	^B	S�B	CaB	3B	+�B	$�B	!�B	�B	xB	sB	@B		B��B� B͹B�mB�gB�UB�BB�>B��B��B��B�eB�.B�B��B��B�B�	B�B��B��B��B�B|�Bz�Bu�BncBf2B`BT�BO�BL�BK�BJ�BJ�BJ�BI�BJ�BJ�BJ�BJ�BK�BI�BH�BGzBEmBDgBDgBB[BB[B?HB=<B:*B9$B8B7B6B6B5B4B1�B4B1�B0�B0�B/�B.�B.�B,�B,�B*�B)�B)�B'�B)�B'�B'�B&�B&�B'�B&�B&�B&�B&�B&�B(�B(�B(�B(�B(�B)�B+�B/�B,�B,�B*�B*�B+�B-�B,�B-�B.�B/�B0�B2�B7B2�B2�B4B:*B@OBAUB=<B<6BAUBI�BK�BM�BN�BP�BR�BT�BV�BV�BU�BT�BN�BJ�BJ�BKxBR�BS�BS�BT�BU�BW�B\�Be,Bf2BaB`BcBkQBlWBm]BoiBqvBr|ButBvzBx�By�Bz�B|�B~�B��B��B��B��B��B�B�(B�B�4B� B�@B�SB�eB�]B��B�pB��B��B��B��B��B��B��B��B�B�B�B�B�(B�OB�UB�[B�zB�lB̘BϫBҽB��B�B�B�B�&B�B�>B�KB�WB�]B�oB�B�zB��B��B��B��B��B	�B	B	B	B	FB	?B	EB	eB	KB	kB	kB	qB	jB	 �B	!�B	"�B	"�B	$�B	&�B	(�B	-�B	/�B	1�B	4B	6�B	7B	9$B	="B	>BB	@OB	EmB	FtB	HfB	K�B	Q�B	T�B	U�B	X�B	[�B	\�B	^�B	_B	aB	e,B	jKB	m]B	m]B	poB	s�B	s�B	utB	y�B	{�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�4B	�&B	�FB	�YB	�eB	�xB	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�$B	�*B	�B	�<B	�"B	�"B	�.B	�AB	�aB	�GB	�YB	ɆB	ʌB	ʌB	�xB	˒B	̈́B	ΊB	ϫB	ϫB	ϫB	ϑB	ЗB	ҽB	өB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	� B	�&B	�&B	�B	�B	�2B	�2B	�8B	�>B	�$B	�>B	�DB	�KB	�QB	�QB	�QB	�WB	�WB	�CB	�cB	�cB	�OB	�OB	�UB	�oB	�vB	�hB	�B	�B	�B	�B	��B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	�B

	B

	B

	B

�B
B

�B
�B
B
B
B
B
(B
(B
(B
.B
.B
.B
.B
.B
4B
4B
:B
 B
:B
@B
@B
@B
@B
FB
FB
FB
FB
MB
2B
MB
MB
SB
SB
9B
?B
?B
YB
YB
YB
_B
_B
_B
EB
EB
KB
eB
eB
KB
eB
kB
QB
kB
kB
qB
WB
]B
xB
dB
xB
~B
~B
�B
�B
�B
jB
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
#�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
.�B
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
0�B
1�B
1�B
1�B
2�B
2�B
4B
4B
4B
4B
4�B
5B
5B
5B
5B
6B
5�B
5�B
5�B
6B
6�B
7B
6�B
7B
8B
8B
8B
9$B
9	B
9$B
:B
:B
:*B
:*B
:*B
:*B
;B
;B
<6B
<6B
<6B
<B
<B
=<B
=<B
=<B
="B
>BB
>BB
>(B
>BB
>BB
>BB
?HB
?HB
?HB
?.B
?HB
@4B
A;B
AUB
@4B
@OB
AUB
B[B
BAB
B[B
B[B
BAB
B[B
B[B
CaB
CaB
CaB
DgB
DMB
DMB
EmB
EmB
EmB
EmB
EmB
EmB
FYB
FYB
FtB
FtB
FYB
FtB
GzB
GzB
GzB
G_B
GzB
G_B
GzB
G_B
G_B
H�B
I�B
IlB
I�B
I�B
I�B
I�B
I�B
J�B
JrB
IlB
J�B
J�B
J�B
J�B
K�B
K�B
KxB
KxB
KxB
K�B
L~B
L�B
L�B
L�B
L~B
M�B
M�B
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
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
W�B
W�B
W�B
X�B
X�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^B
]�B
_B
_B
^�B
_B
`B
_�B
`B
`B
aB
aB
aB
aB
a�B
bB
a�B
bB
a�B
a�B
bB
a�B
bB
c B
c B
d&B
d&B
d&B
dB
d&B
e,B
e,B
e,B
e,B
e,B
e,B
f2B
f2B
gB
g8B
gB
g8B
g8B
gB
g8B
g8B
g8B
gB
g8B
h$B
h>B
h$B
h>B
h>B
h>B
iDB
iDB
iDB
iDB
iDB
i*B
iDB
i*B
jKB
jKB
jKB
j0B
j0B
jKB
jKB
jKB
k6B
kQB
kQB
lWB
l=B
lWB
lWB
lWB
l=B
lWB
lWB
lWB
m]B
lWB
m]B
m]B
m]B
m]B
m]B
m]B
m]B
ncB
nIB
ncB
ncB
ncB
ncB
nI1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.45(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902250046272019022500462720190225004627201902260053082019022600530820190226005308JA  ARFMdecpA19c                                                                20190220063623  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190219213638  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190219213639  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190219213640  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190219213640  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190219213640  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190219213641  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190219213641  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190219213641  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190219213641                      G�O�G�O�G�O�                JA  ARUP                                                                        20190219215543                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190220153748  CV  JULD            G�O�G�O�F�GU                JM  ARGQJMQC2.0                                                                 20190220153748  CV  JULD_LOCATION   G�O�G�O�F�Gl                JM  ARGQJMQC2.0                                                                 20190220153748  CV  LATITUDE        G�O�G�O�A��R                JM  ARCAJMQC2.0                                                                 20190224154627  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190224154627  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190225155308  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                