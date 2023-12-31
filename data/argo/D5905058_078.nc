CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-29T18:35:29Z creation;2018-07-29T18:35:31Z conversion to V3.1;2019-12-23T06:17:48Z update;     
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݠ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20180729183529  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               NA   JA  I2_0675_078                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�u��}( 1   @�u�����@8�e��O�c*˒:*1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8ffB?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�fD�#31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @P  @�ff@�ffA33A'33AG33Ag33A���A���A���A���AÙ�Aә�A㙚A�B33B	��B��B��B!��B)��B1��B:33BAffBI��BQ��BY��Ba��Bi��Bq��By��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.Y�C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�3D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4�3D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD���D��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfDfD��fD�fD�NfDÎfD��fD�fD�NfDĎfD��fD�fD�NfDŎfD��fD�fD�NfDƎfD��fD�fD�NfDǎfD��fD�fD�NfDȎfD��fD�fD�NfDɎfD��fD�fD�NfDʎfD��fD�fD�NfDˎfD��fD�fD�NfD̎fD��fD�fD�NfD͎fD��fD�fD�NfDΎfD��fD�fD�NfDώfD��fD�fD�NfDЎfD��fD�fD�NfDюfD��fD�fD�NfDҎfD��fD�fD�NfDӎfD��fD�fD�NfDԎfD��fD�fD�Q�DՎfD��fD�fD�NfD֎fD��fD�fD�NfD׎fD��fD�fD�NfD؎fD��fD�fD�NfDَfD��fD�fD�NfDڎfD��fD�fD�NfDێfD��fD�fD�NfD܎fD��fD�fD�NfDݎfD��fD�fD�NfDގfD��fD�fD�NfDߎfD��fD�fD�NfD��fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD��fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�K3D��fD��fD�fD�NfD��fD��fD��D�1�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�/A�33A�7LA�9XA�9XA�;dA�=qA�?}A�A�A�;dA�7LA�5?A�1'A�1'A�1'A�/A��A���A���Aʧ�Aʛ�AʋDA�|�A�33AĮA�dZA�{A�jA�;dA�dZA�ZA�M�A��`A��A���A�ĜA�5?A���A�p�A�jA���A�&�A���A�oA��A�&�A���A�O�A��A��HA�dZA�&�A��TA���A�=qA���A��DA�7LA��HA�ZA��A��A��A�A�A�?}A�l�A�p�A���A�x�A���A� �A�-A�bNA�%A�bA���A���A��`A���A���A��A�E�A��A��FA�l�A���A�I�A�A��DA�\)A�
=A��HA�p�A�
=A��DA�t�A��TA�O�A���A�|�A�A���A��`A��;A��yA��A�n�A�n�A��uA�A�9XA��PA��`A�n�A�A�bA~�A}��A|9XAz^5AwS�AvffAu��At�As��ArffAot�Ajz�Ah�RAg
=Ae��AaK�A_�TA^�RA]�#A\�AYt�AXbAU��AS&�AP(�AM�ALQ�AJA�AFr�AD��AC�;AC/AB�AB��A@��A?A=&�A<jA<JA;7LA9�A9|�A8��A8A7+A6M�A4�RA4A3?}A3G�A2�A1|�A1S�A0n�A/�A.M�A,A�A*E�A)��A)�A)�A(�A(�DA(�\A(�A(��A(z�A(1'A'�A&�\A%A$-A#"�A"�RA"n�A!�
A!%A�A7LA�jA�wAn�AC�A�uA1A`BA�yA^5A/A��AoA�A�AC�A�yA�9AbNA�
A"�A�!An�A��A�A�+A��A��A{A�A
n�A
{A	�wA	hsA�A(�A\)AbNA$�A�AA��A1'AG�A bN@��;@�t�@���@��@�j@�J@��9@�Q�@���@�ȴ@��7@�Z@�33@�J@�?}@��D@@��@@�$�@���@���@�O�@���@�S�@���@�v�@�/@�dZ@��#@�&�@�Z@��@��@�A�@�\)@ڸR@� �@ְ!@�&�@�l�@�ȴ@��@�X@�j@�dZ@Χ�@�@�r�@�ƨ@�V@�r�@ǅ@�v�@�`B@�1@°!@�J@��@�p�@�G�@��;@��@�^5@���@��^@�ff@�ȴ@��^@�(�@�+@���@�G�@�j@�1@��F@�+@�ff@�-@��@�`B@�7L@���@��@���@��H@�?}@�  @���@�K�@��H@�^5@���@�A�@���@�dZ@�o@�5?@��^@�%@�1@��y@��R@��!@��\@�V@�E�@��T@���@�b@�dZ@�C�@��R@���@�&�@���@���@��m@��
@��@�;d@��!@��H@�@���@���@�~�@�^5@�{@�?}@��`@���@��9@�I�@�b@��;@���@�K�@�ff@��T@���@�p�@�hs@�/@��@��/@��j@��@���@��u@�I�@�b@��w@�t�@�33@�
=@��y@��@�ȴ@��\@��+@�^5@�-@�$�@��@���@�G�@�?}@�r�@�Z@�A�@��w@���@�dZ@�o@���@��R@�ff@�-@��@��#@��#@��^@�p�@�/@���@��D@�bN@� �@�  @��@��m@��@��@�l�@�S�@��@��@��@���@���@�ff@�V@�-@�{@��T@���@�X@��@���@��/@��`@��/@��@�Ĝ@���@��@�bN@�A�@�  @��;@�K�@�ȴ@��R@���@���@�~�@�^5@�$�@�{@�@���@���@�@��-@���@���@��7@�p�@�`B@�X@�/@���@��@�Z@�b@��m@���@���@�\)@�;d@�+@�
=@�
=@�
=@��@��H@��@��+@�V@�=q@��@���@�J@��^@���@��7@�`B@�O�@�&�@���@��j@��j@��j@��u@�j@�Z@�A�@�1@��@�ƨ@��@�K�@�"�@��y@���@��\@�v�@�^5@�=q@��@��#@��#@���@�`B@��@��@��/@��j@��@�Q�@�@��@��@+@~ff@}��@}@}�-@}�-@}��@}/@|��@|�j@|��@|z�@|I�@{��@{��@{C�@z�H@zn�@zJ@y�^@y7L@x�u@x  @w�P@w\)@wK�@w�@v��@v�+@vv�@vv�@vff@v5?@u�T@u@u��@u?}@t��@t�D@s��@sƨ@s�@sS�@r�H@r��@r^5@r=q@qhs@p��@p�9@pbN@o�w@o+@nȴ@nV@n@m�T@mO�@l�@l�@lj@l�@k��@j��@j^5@i��@i�#@i��@ix�@i�@h�u@hbN@g�@f��@f�+@f$�@e�T@e�T@ep�@d��@d(�@ct�@b�!@b-@a��@a��@a��@ax�@aX@a�@`�u@`1'@_�P@^�y@^�y@^ȴ@^�+@^{@]��@]�h@]�@]O�@\�j@[�F@["�@Z�!@ZM�@Y�@Y�^@Y%@XbN@X  @W��@V�y@Vff@U��@Up�@U`B@U`B@UO�@UV@T�@T�@T��@Tz�@T(�@S��@Sƨ@S�F@S�F@S�@S"�@S@R�@R��@R��@R~�@RM�@R-@QX@P�@P1'@Pb@O��@O�P@O\)@O
=@Nȴ@Nff@N{@M@MO�@L�@L�D@Lz�@L9X@Kƨ@K33@K"�@Ko@J�@J��@Jn�@JM�@J=q@J-@J�@I�@I�^@IX@H�`@HĜ@HQ�@H �@H  @G�@G�;@G�;@G��@G+@G
=@F�y@F5?@E?}@D�/@D��@Dz�@D9X@C�
@CdZ@B�@B�H@B�!@B~�@B^5@B-@A��@Ahs@AX@AG�@@�u@@A�@@A�@?�w@?|�@?;d@>��@>$�@=@=�@=O�@<�/@<�@<z�@<Z@;��@;�F@;�@;"�@:��@:M�@:=q@9�^@9�7@97L@8Q�@7�;@7��@7�w@7��@7K�@6��@6V@6@5@5�@5/@4�@4�/@4�@4Z@4�D@4Z@4I�@3��@3C�@333@333@3"�@3@2�H@2��@2�\@2^5@2-@2-@1�@1�7@1hs@1G�@1&�@0��@0�9@0Q�@0 �@0b@/��@/\)@/K�@/;d@/+@/+@/�@.��@.$�@-�T@-@-`B@,��@,��@,z�@,Z@,(�@,�@,1@+�
@+��@+33@*�H@*��@*^5@*=q@*�@)�^@)��@)��@)G�@)�@(��@(�u@(A�@'�;@'��@';d@'
=@&�@&�R@&v�@&5?@%��@%�h@%p�@%/@$�@$��@$�@$��@$z�@$I�@$(�@#�m@#��@#C�@#C�@#o@"��@"�\@"n�@"M�@"J@!�@!��@!x�@!X@!7L@ ��@ ��@ ��@ �@ bN@ A�@  �@ b@�@\)@ȴ@�+@V@5?@5?@@��@p�@?}@�@��@�@�/@�j@�D@�@�
@ƨ@��@�@dZ@S�@"�@��@�\@=q@�@�^@�7@�7@7L@�@�`@r�@bN@A�@b@��@�@|�@\)@K�@+@��@�@�R@��@�+@ff@{@�-@�h@`B@O�@�@��@�@Z@9X@��@��@S�@"�@�@��@�\@n�@M�@�@�@�^@X@&�@�@��@��@��@�u@r�@ �@�@�w@�@l�@K�@;d@�@��@�y@�@�R@��@ff@$�@�@��@��@�h@�h@p�@?}@�@�@�/@�@��@z�@Z@Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�/A�33A�7LA�9XA�9XA�;dA�=qA�?}A�A�A�;dA�7LA�5?A�1'A�1'A�1'A�/A��A���A���Aʧ�Aʛ�AʋDA�|�A�33AĮA�dZA�{A�jA�;dA�dZA�ZA�M�A��`A��A���A�ĜA�5?A���A�p�A�jA���A�&�A���A�oA��A�&�A���A�O�A��A��HA�dZA�&�A��TA���A�=qA���A��DA�7LA��HA�ZA��A��A��A�A�A�?}A�l�A�p�A���A�x�A���A� �A�-A�bNA�%A�bA���A���A��`A���A���A��A�E�A��A��FA�l�A���A�I�A�A��DA�\)A�
=A��HA�p�A�
=A��DA�t�A��TA�O�A���A�|�A�A���A��`A��;A��yA��A�n�A�n�A��uA�A�9XA��PA��`A�n�A�A�bA~�A}��A|9XAz^5AwS�AvffAu��At�As��ArffAot�Ajz�Ah�RAg
=Ae��AaK�A_�TA^�RA]�#A\�AYt�AXbAU��AS&�AP(�AM�ALQ�AJA�AFr�AD��AC�;AC/AB�AB��A@��A?A=&�A<jA<JA;7LA9�A9|�A8��A8A7+A6M�A4�RA4A3?}A3G�A2�A1|�A1S�A0n�A/�A.M�A,A�A*E�A)��A)�A)�A(�A(�DA(�\A(�A(��A(z�A(1'A'�A&�\A%A$-A#"�A"�RA"n�A!�
A!%A�A7LA�jA�wAn�AC�A�uA1A`BA�yA^5A/A��AoA�A�AC�A�yA�9AbNA�
A"�A�!An�A��A�A�+A��A��A{A�A
n�A
{A	�wA	hsA�A(�A\)AbNA$�A�AA��A1'AG�A bN@��;@�t�@���@��@�j@�J@��9@�Q�@���@�ȴ@��7@�Z@�33@�J@�?}@��D@@��@@�$�@���@���@�O�@���@�S�@���@�v�@�/@�dZ@��#@�&�@�Z@��@��@�A�@�\)@ڸR@� �@ְ!@�&�@�l�@�ȴ@��@�X@�j@�dZ@Χ�@�@�r�@�ƨ@�V@�r�@ǅ@�v�@�`B@�1@°!@�J@��@�p�@�G�@��;@��@�^5@���@��^@�ff@�ȴ@��^@�(�@�+@���@�G�@�j@�1@��F@�+@�ff@�-@��@�`B@�7L@���@��@���@��H@�?}@�  @���@�K�@��H@�^5@���@�A�@���@�dZ@�o@�5?@��^@�%@�1@��y@��R@��!@��\@�V@�E�@��T@���@�b@�dZ@�C�@��R@���@�&�@���@���@��m@��
@��@�;d@��!@��H@�@���@���@�~�@�^5@�{@�?}@��`@���@��9@�I�@�b@��;@���@�K�@�ff@��T@���@�p�@�hs@�/@��@��/@��j@��@���@��u@�I�@�b@��w@�t�@�33@�
=@��y@��@�ȴ@��\@��+@�^5@�-@�$�@��@���@�G�@�?}@�r�@�Z@�A�@��w@���@�dZ@�o@���@��R@�ff@�-@��@��#@��#@��^@�p�@�/@���@��D@�bN@� �@�  @��@��m@��@��@�l�@�S�@��@��@��@���@���@�ff@�V@�-@�{@��T@���@�X@��@���@��/@��`@��/@��@�Ĝ@���@��@�bN@�A�@�  @��;@�K�@�ȴ@��R@���@���@�~�@�^5@�$�@�{@�@���@���@�@��-@���@���@��7@�p�@�`B@�X@�/@���@��@�Z@�b@��m@���@���@�\)@�;d@�+@�
=@�
=@�
=@��@��H@��@��+@�V@�=q@��@���@�J@��^@���@��7@�`B@�O�@�&�@���@��j@��j@��j@��u@�j@�Z@�A�@�1@��@�ƨ@��@�K�@�"�@��y@���@��\@�v�@�^5@�=q@��@��#@��#@���@�`B@��@��@��/@��j@��@�Q�@�@��@��@+@~ff@}��@}@}�-@}�-@}��@}/@|��@|�j@|��@|z�@|I�@{��@{��@{C�@z�H@zn�@zJ@y�^@y7L@x�u@x  @w�P@w\)@wK�@w�@v��@v�+@vv�@vv�@vff@v5?@u�T@u@u��@u?}@t��@t�D@s��@sƨ@s�@sS�@r�H@r��@r^5@r=q@qhs@p��@p�9@pbN@o�w@o+@nȴ@nV@n@m�T@mO�@l�@l�@lj@l�@k��@j��@j^5@i��@i�#@i��@ix�@i�@h�u@hbN@g�@f��@f�+@f$�@e�T@e�T@ep�@d��@d(�@ct�@b�!@b-@a��@a��@a��@ax�@aX@a�@`�u@`1'@_�P@^�y@^�y@^ȴ@^�+@^{@]��@]�h@]�@]O�@\�j@[�F@["�@Z�!@ZM�@Y�@Y�^@Y%@XbN@X  @W��@V�y@Vff@U��@Up�@U`B@U`B@UO�@UV@T�@T�@T��@Tz�@T(�@S��@Sƨ@S�F@S�F@S�@S"�@S@R�@R��@R��@R~�@RM�@R-@QX@P�@P1'@Pb@O��@O�P@O\)@O
=@Nȴ@Nff@N{@M@MO�@L�@L�D@Lz�@L9X@Kƨ@K33@K"�@Ko@J�@J��@Jn�@JM�@J=q@J-@J�@I�@I�^@IX@H�`@HĜ@HQ�@H �@H  @G�@G�;@G�;@G��@G+@G
=@F�y@F5?@E?}@D�/@D��@Dz�@D9X@C�
@CdZ@B�@B�H@B�!@B~�@B^5@B-@A��@Ahs@AX@AG�@@�u@@A�@@A�@?�w@?|�@?;d@>��@>$�@=@=�@=O�@<�/@<�@<z�@<Z@;��@;�F@;�@;"�@:��@:M�@:=q@9�^@9�7@97L@8Q�@7�;@7��@7�w@7��@7K�@6��@6V@6@5@5�@5/@4�@4�/@4�@4Z@4�D@4Z@4I�@3��@3C�@333@333@3"�@3@2�H@2��@2�\@2^5@2-@2-@1�@1�7@1hs@1G�@1&�@0��@0�9@0Q�@0 �@0b@/��@/\)@/K�@/;d@/+@/+@/�@.��@.$�@-�T@-@-`B@,��@,��@,z�@,Z@,(�@,�@,1@+�
@+��@+33@*�H@*��@*^5@*=q@*�@)�^@)��@)��@)G�@)�@(��@(�u@(A�@'�;@'��@';d@'
=@&�@&�R@&v�@&5?@%��@%�h@%p�@%/@$�@$��@$�@$��@$z�@$I�@$(�@#�m@#��@#C�@#C�@#o@"��@"�\@"n�@"M�@"J@!�@!��@!x�@!X@!7L@ ��@ ��@ ��@ �@ bN@ A�@  �@ b@�@\)@ȴ@�+@V@5?@5?@@��@p�@?}@�@��@�@�/@�j@�D@�@�
@ƨ@��@�@dZ@S�@"�@��@�\@=q@�@�^@�7@�7@7L@�@�`@r�@bN@A�@b@��@�@|�@\)@K�@+@��@�@�R@��@�+@ff@{@�-@�h@`B@O�@�@��@�@Z@9X@��@��@S�@"�@�@��@�\@n�@M�@�@�@�^@X@&�@�@��@��@��@�u@r�@ �@�@�w@�@l�@K�@;d@�@��@�y@�@�R@��@ff@$�@�@��@��@�h@�h@p�@?}@�@�@�/@�@��@z�@Z@Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�uB�oB�oB�uB�uB�uB�uB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BVB!�B�B�B �B(�B1'B<jBQ�BZBO�BQ�BXB_;BhsBq�BYBVB]/BaHBffBjBjBl�Bm�Bo�Bm�Bo�Bw�B�B�%B�7B�=B�7B�7B�B�B� B~�Bx�Bv�Br�B}�B�%B�Bo�BVBO�BP�BXBXB[#Bu�BaHB6FB)�B$�B�B�B,B!�B#�B�BuB�BhB��B�B�TBŢB��B��Bv�BN�B9XB�BB
�TB
��B
B
�'B
��B
�1B
z�B
]/B
7LB
+B
 �B
�B
\B
  B	�B	�B	�mB	�/B	��B	�?B	�'B	��B	��B	�oB	�B	e`B	]/B	\)B	r�B	YB	K�B	B�B	;dB	.B	�B	oB	%B��B�mB�B��BŢB�'B��B��B��B�{B�hB�JB�By�Bt�Br�Bs�Bo�Bl�Bk�BiyBm�Bk�BffBdZBbNBe`Bo�Bo�Bo�BjBcTBaHBYBO�BN�BR�BW
BYB^5BaHBgmBu�B~�B~�B|�Bz�Bv�Bp�Bk�Bk�BjBk�BjBk�Bl�BjBjBgmBbNB_;B]/BZBW
BS�BO�BM�BM�BL�BJ�BI�BH�BG�BF�BF�BD�BC�BB�BA�B?}B>wB<jB;dB7LB6FB6FB5?B33B2-B0!B/B/B)�B)�B(�B&�B&�B#�B"�B"�B!�B!�B �B"�B$�B$�B$�B$�B$�B%�B&�B&�B'�B'�B&�B&�B&�B'�B'�B(�B)�B(�B.B.B.B/B.B/B/B-B-B-B,B,B-B+B+B+B,B-B/B.B/B/B2-B49B5?B6FB7LB9XB=qB=qB?}B@�BA�BC�BF�BF�BG�BG�BH�BL�BN�BP�BR�BT�B\)BdZBcTBk�Bn�Bp�Bp�Bo�Bp�Bq�Bt�Bv�Bx�By�B{�B� B�B�B�+B�1B�DB�bB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�3B�3B�RB�qB��BŢBǮB��B��B��B��B�B�#B�BB�TB�`B�fB�yB�B�B�B�B��B��B��B��B��B��B	B	B	B	B	%B	DB	\B	oB	{B	�B	�B	�B	�B	 �B	!�B	!�B	"�B	$�B	%�B	(�B	-B	.B	/B	2-B	9XB	<jB	=qB	>wB	A�B	D�B	G�B	I�B	J�B	L�B	M�B	O�B	R�B	T�B	[#B	^5B	aHB	dZB	ffB	hsB	jB	l�B	n�B	o�B	r�B	t�B	v�B	x�B	|�B	}�B	� B	�B	�B	�%B	�%B	�7B	�DB	�JB	�PB	�VB	�\B	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�3B	�?B	�FB	�LB	�RB	�XB	�XB	�^B	�^B	�dB	�qB	�qB	�qB	�wB	�wB	�}B	�}B	��B	��B	��B	��B	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
B
%B
%B
+B
	7B

=B
DB
DB
JB
JB
JB
PB
VB
VB
PB
PB
PB
VB
VB
\B
bB
bB
bB
bB
bB
hB
hB
uB
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
'�B
'�B
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
,B
,B
,B
,B
,B
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
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
2-B
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
7LB
7LB
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
9XB
9XB
:^B
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
=qB
=qB
>wB
>wB
?}B
@�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
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
E�B
F�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
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
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
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
Q�B
Q�B
Q�B
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
S�B
S�B
S�B
S�B
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
[#B
\)B
\)B
\)B
\)B
\)B
\)B
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
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
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
iyB
iyB
iyB
iyB
iyB
iyB
iyB
j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�[B�:B�TB�@B�[B�[B�[B�:B�[B�aB�gB�gB�SB�YB�sB�YB��B��B��B��B��B��B��B��B��B<B!�B~B~B �B(�B1B<6BQ�BZBO�BQ�BW�B_!Bh>Bq�BX�BU�B\�BaBf2BjKBjKBlqBm]Bo�Bm]BoiBw�B��B�B�B�#B�B�B��B��B�B~�Bx�Bv�Br|B}�B��B��BoiBU�BO�BP�BW�BW�B[	Bu�BaB6B)�B$�BYByB+�B!�B#�BeB@BMB4B��B�oB� BňB��B��Bv�BN�B9$B�B �B
� B
ѷB
�[B
��B
��B
��B
z�B
]B
7B
*�B
 �B
SB
BB	��B	�|B	�KB	�8B	��B	�UB	�%B	�B	��B	��B	�:B	��B	e,B	]B	[�B	r�B	X�B	K�B	B[B	;JB	-�B	qB	:B	�B�B�8B��BΥB�mB��B��B�~B�YB�FB�4B�0B��By�Bt�Br|Bs�BoiBlWBkQBiDBmwBkQBf2Bd&BbBe,BoiBoiBoiBjKBc BaBX�BO�BN�BR�BV�BX�B^BaBg8Bu�B~�B~�B|�Bz�Bv�BpoBkQBkQBjKBkQBjKBkQBlWBjKBjKBg8BbB_B\�BY�BV�BS�BO�BM�BM�BL�BJ�BI�BH�BGzBFtBFtBDgBCaBB[BAUB?HB>BB<6B;0B7B6B6B5B2�B1�B/�B.�B.�B)�B)�B(�B&�B&�B#�B"�B"�B!�B!�B �B"�B$�B$�B$�B$�B$�B%�B&�B&�B'�B'�B&�B&�B&�B'�B'�B(�B)�B(�B-�B-�B-�B.�B-�B.�B.�B,�B,�B,�B+�B+�B,�B*�B*�B*�B+�B,�B.�B-�B.�B.�B1�B4B5B5�B7B9$B="B=<B?.B@4BAUBCaBFtBFtBGzBGzBH�BL�BN�BP�BR�BT�B[�Bd&Bc BkQBnIBpoBpoBoOBpoBq[Bt�Bv�Bx�By�B{�B�B��B��B��B��B��B�.B�4B�:B�@B�SB�eB�qB�]B�xB��B�vB�|B��B��B��B��B��B��B��B��B�B�<B�OB�mB�_B̈́BѝBѷB��B��B��B��B� B�B�B�DB�WB�IB�oB�aB�B�tB�tB��B��B��B	�B	�B	�B	�B	�B	B	(B	:B	FB	SB	kB	kB	�B	 �B	!�B	!�B	"�B	$�B	%�B	(�B	,�B	-�B	.�B	1�B	9$B	<6B	=<B	>BB	A;B	DgB	G_B	I�B	J�B	L�B	M�B	O�B	R�B	T�B	Z�B	^B	aB	dB	f2B	h>B	jKB	l=B	ncB	oiB	raB	t�B	v�B	x�B	|�B	}�B	�B	��B	��B	��B	��B	��B	�B	��B	�B	�"B	�(B	�.B	�.B	�:B	�FB	�SB	�SB	�?B	�EB	�qB	�jB	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�$B	�	B	�*B	�B	�0B	�<B	�<B	�<B	�BB	�BB	�.B	�.B	�OB	�4B	�OB	�UB	�GB	�MB	�mB	�YB	�YB	�_B	ɆB	ʌB	�xB	̘B	ΥB	ϫB	ϫB	ϑB	бB	бB	ѷB	ҽB	ңB	ѷB	ҽB	��B	յB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�B	�B	�,B	�,B	�2B	�B	�2B	�2B	�2B	�8B	�>B	�KB	�KB	�WB	�]B	�]B	�cB	�IB	�cB	�IB	�cB	�iB	�oB	�[B	�vB	�aB	�aB	�B	�B	�nB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	�B
B
B
B
B
B
B
"B
"B
B
B
B
"B
"B
B
.B
.B
B
.B
B
B
B
@B
FB
FB
,B
FB
,B
,B
MB
YB
_B
_B
eB
eB
eB
eB
eB
_B
eB
KB
eB
KB
eB
eB
KB
KB
_B
_B
eB
eB
kB
kB
kB
WB
qB
WB
qB
qB
qB
xB
xB
]B
xB
~B
~B
~B
�B
jB
�B
�B
 �B
�B
�B
 �B
!|B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
'�B
'�B
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
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
1�B
1�B
1�B
1�B
2�B
4B
4B
4B
3�B
5B
5B
4�B
5B
4�B
6B
7B
7B
8B
8B
9$B
9$B
9$B
9$B
9$B
9	B
9$B
:B
9$B
9	B
:B
;0B
;0B
;0B
;B
;0B
<6B
<B
<6B
<6B
="B
=<B
=<B
>BB
>BB
?HB
@OB
@OB
A;B
BAB
B[B
BAB
CaB
CGB
CGB
CaB
DgB
DgB
DgB
DgB
DgB
EmB
ESB
EmB
EmB
EmB
FtB
EmB
FtB
FtB
FtB
FYB
FYB
FtB
FYB
FtB
FtB
FtB
GzB
HfB
HfB
H�B
H�B
I�B
I�B
IlB
I�B
JrB
J�B
JrB
JrB
J�B
J�B
K�B
K�B
KxB
K�B
L�B
L�B
L~B
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
O�B
O�B
O�B
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
Q�B
Q�B
Q�B
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
S�B
S�B
S�B
S�B
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
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
]�B
^B
]�B
]�B
^B
^B
_B
_B
^�B
_B
`B
`B
_�B
`B
`B
_�B
aB
aB
`�B
`�B
bB
bB
bB
bB
c B
c B
c B
c B
c B
d&B
d&B
d&B
e,B
e,B
e,B
eB
eB
e,B
eB
f2B
f2B
f2B
f2B
fB
f2B
fB
g8B
g8B
g8B
gB
g8B
g8B
g8B
h>B
h>B
h$B
h$B
h>B
h>B
h$B
h>B
iDB
iDB
i*B
iDB
iDB
iDB
iDB
j01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.45(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808040040192018080400401920180804004019201808050037312018080500373120180805003731JA  ARFMdecpA19c                                                                20180730033508  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180729183529  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180729183530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180729183530  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180729183531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180729183531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180729183531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180729183531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180729183531  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180729183531                      G�O�G�O�G�O�                JA  ARUP                                                                        20180729185618                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180730153854  CV  JULD            G�O�G�O�FíG                JM  ARGQJMQC2.0                                                                 20180730153854  CV  JULD_LOCATION   G�O�G�O�Fí^                JM  ARGQJMQC2.0                                                                 20180730153854  CV  LATITUDE        G�O�G�O�A�ȴ                JM  ARGQJMQC2.0                                                                 20180730153854  CV  LONGITUDE       G�O�G�O��U?                JM  ARCAJMQC2.0                                                                 20180803154019  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180803154019  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180804153731  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                