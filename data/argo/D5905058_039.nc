CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-13T06:35:17Z creation;2018-02-13T06:35:21Z conversion to V3.1;2019-12-23T06:27:05Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180213063517  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               'A   JA  I2_0675_039                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�L��� 1   @�L����@6��J�M�b�Q���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D��3D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̃3D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�C3Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�fD�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�ffA33A'33AG33Ag33A���A���A���A���AÙ�Aә�A㙚A�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��3B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fC s3Cs3Cs3Cs3Cs3C
s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C s3C"s3C$s3C&s3C(s3C*s3C,s3C.s3C0s3C2s3C4s3C6s3C8s3C:s3C<s3C>s3C@s3CBs3CDs3CFs3CHs3CJs3CLs3CNs3CPs3CRs3CTs3CVs3CXs3CZs3C\s3C^s3C`s3Cbs3Cds3Cfs3Chs3Cjs3Cls3Cns3Cps3Crs3Cts3Cvs3Cxs3Czs3C|s3C~s3C�9�C�9�C�9�C�FfC�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�FfC�FfC�9�C�9�C�9�C�9�C�9�C�,�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�C�9�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D#3D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6fD6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd#3Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�Q�D��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD�fD�NfDfD��fD�fD�NfDÎfD�њD�fD�NfDĎfD��fD�fD�NfDŎfD��fD�fD�NfDƎfD��fD�fD�NfDǎfD��fD�fD�NfDȎfD��fD�fD�NfDɎfD��fD�fD�NfDʎfD��fD�fD�NfDˎfD��fD�fD�NfD̑�D��fD�fD�NfD͎fD��fD�fD�NfDΎfD��fD�fD�NfDώfD��fD�fD�NfDЎfD��fD�fD�Q�DюfD��fD�fD�NfDҎfD��fD�fD�NfDӎfD��fD�fD�NfDԎfD��fD�fD�NfDՎfD��fD�fD�NfD֎fD��fD�fD�NfD׎fD��fD�fD�NfD؎fD��fD�fD�NfDَfD��fD�fD�NfDڎfD��fD�fD�NfDێfD��fD�fD�NfD܎fD��fD�fD�NfDݎfD��fD�fD�NfDގfD��fD�fD�NfDߎfD��fD�fD�NfD��fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�NfD��fD��fD�fD�NfD�fD��fD�fD�NfD�fD��fD�fD�Q�D�fD��fD�fD�NfD�fD��fD�fD�NfD��fD��fD�fD�NfD��fD��fD��D�8 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�"�A��A�bA�VA��TA���A�^5A�;dA�/A�$�A��A�JA���A��A��mA��TA��/A��
A���A�ƨA��^A���A��hA�~�A�hsA�S�A�C�A�=qA�$�A�
=A���A��A��HA���A���A�ȴA��HA��A��FA�+A��HA�-A�A��A�?}A��A�9XA��7A��PA���A��;A�+A���A��\A��/A�C�A���A�{A�t�A�n�A���A��#A��A���A���A��A�JA��TA���A��A��A��A�ĜA�VA���A��/A��9A��A�&�A��`A���A��#A��RA��HA��A�dZA��RA���A��A���A��A�"�A���A�C�A�I�A��A��FA���A�C�A��A�
=A���A�VA�1A��A���A���A�I�A�"�A�ffA��jA7LAzM�AxAu�;As
=Ao�Al(�Ae�#AdE�Ac��AbbA]t�A[��AZv�AY�;AX��AVn�AUXATffAR�jAN��AL��AF�AC�PABE�A@5?A<ffA:VA9K�A9A7dZA69XA5XA4��A45?A3�PA2��A1�A0�\A/�mA.��A+�7A'7LA%�TA%dZA%/A$$�A!��A!\)A!"�A �A�DA�AZA"�AXA~�A��A�;A��AA�A�mAA��A�/A��A=qAZA��A
�`A	��A	��A	hsAbNA�
Ap�A�yAQ�A��AA�A��AhsA7LA�HAz�A �A��A Ĝ@��
@�-@��;@���@���@�;d@�M�@���@��@��
@�v�@�|�@���@�b@�@�o@�`B@�dZ@���@�@�o@��@�%@�S�@�v�@ݡ�@��`@�  @�;d@�@ؼj@�bN@�A�@�1'@��@׾w@�V@ԓu@�^5@�hs@�&�@��/@��;@ϕ�@�|�@�"�@θR@���@�V@˾w@�K�@�o@��@�M�@�1'@�J@Ĵ9@�ƨ@�o@���@�l�@��T@���@�Q�@�+@�ȴ@�~�@�5?@�@��7@�x�@�hs@� �@�-@�?}@�j@�I�@�9X@�  @��F@�t�@�+@���@�J@�`B@�V@�V@��`@�I�@�b@�1@���@���@�t�@�33@��\@�$�@��@���@��-@�`B@���@�Ĝ@�1'@��P@�S�@��@�~�@�$�@�{@�@��-@��h@�&�@���@�A�@��@��y@��!@�@�O�@��@�Q�@�I�@�(�@���@�dZ@��@���@�M�@��T@���@���@���@�p�@�/@�V@���@�Ĝ@�Q�@���@��y@���@�=q@�J@�@�7L@��9@���@��@��@�r�@�j@�9X@�  @���@�;d@�@��y@�ȴ@�~�@�=q@�$�@��@���@�`B@��`@�bN@��
@���@���@�dZ@��H@���@��+@�V@��@���@��h@��@��@�x�@�p�@�hs@�hs@�hs@��@�&�@�p�@��`@�Ĝ@��9@�z�@��@��;@���@��w@��P@�t�@�l�@�l�@�S�@���@��!@���@���@���@��+@�=q@�J@���@���@�p�@�X@�/@�V@��9@�9X@�1@��
@��w@��F@���@�l�@�K�@�"�@�ȴ@��\@�^5@�M�@�@���@�p�@��@���@��9@��u@��@�j@�1'@��w@�t�@�K�@�
=@��!@�E�@�{@��@���@���@�hs@��`@��u@��u@��D@�bN@�A�@��@��;@���@�33@��H@���@��!@�M�@�5?@��T@��-@��h@�x�@�O�@�V@�Ĝ@��9@��@��D@�1'@�(�@�  @��@�dZ@�l�@�\)@�C�@�+@�o@�o@��y@�ȴ@��R@���@�n�@�^5@�=q@�-@�@���@��@�`B@�O�@�/@�%@��9@���@�bN@� �@��@�b@�  @�;@��@K�@~�+@}�-@}�h@}O�@}/@|�@|��@|I�@|I�@|(�@|1@{��@{�@{C�@{33@z��@y��@yG�@y�@x�`@x��@y%@xĜ@x  @w;d@vȴ@vE�@u�@u�@t�/@t�j@t�j@t�D@t1@s�m@s��@so@r^5@q7L@q�@q�@q%@pbN@o�@o�w@o�@o\)@n��@m@m��@mp�@l�@lI�@l�@kt�@k@j�!@jn�@j-@i�@i��@i7L@h��@h�u@h�@hbN@hQ�@h1'@hb@g�@g�w@g��@g+@g
=@fȴ@f�+@fE�@e�T@e��@e�h@eO�@eV@d��@d�@dZ@d9X@cdZ@b��@b�@a�^@aX@a7L@a%@`��@`Ĝ@`�u@`r�@`A�@`b@_�@_�P@_\)@^�R@^E�@]�T@]?}@]V@\��@\�@\��@\I�@[�m@[��@[33@Zn�@Y��@Y��@Yhs@YG�@XĜ@X�@W��@V�@V�R@Vv�@VE�@V5?@U��@U�h@U`B@T�/@Tj@S�m@SdZ@SC�@S@R��@Q�#@Q��@Q7L@P�9@P�@P �@O�;@O��@O|�@O;d@Nȴ@N{@M�-@M�@Mp�@M`B@M�@MV@L��@L�j@L(�@K��@KS�@K"�@J��@JJ@I�^@I��@I�7@IG�@H�`@H�u@Hr�@HbN@HQ�@H �@H  @G��@G�w@G�P@G+@F�+@F$�@E@EO�@E�@EV@D�@D��@D��@DI�@D(�@C��@C�@CS�@C"�@C@B��@B^5@B=q@A�@A��@A��@A��@A�7@AG�@@�`@@Ĝ@@ �@?�;@?��@?\)@?�@>�y@>��@>5?@=�h@<�/@<�j@<��@<�D@<j@<(�@;�
@;S�@:�\@9��@9��@9hs@8�`@8�9@8�9@8�u@8�@8bN@8  @7�P@7;d@6�@6ȴ@6ȴ@6��@5�@5`B@5V@4��@4�j@4j@49X@41@3�
@3��@3C�@2��@2��@2�@1x�@0��@0�@0A�@01'@0b@/��@/l�@.��@.ȴ@.��@.v�@.V@.$�@.@-�T@-�T@-�T@-��@-�-@-`B@-V@,�j@,�D@,j@,(�@+ƨ@+��@+S�@+o@*��@*n�@*-@)��@)�^@)��@)�7@)hs@)&�@(��@(�u@(r�@(Q�@(  @'��@'|�@'+@'
=@&��@&�R@&��@&v�@&V@&5?@%��@%��@%?}@$��@$�/@$�j@$��@$I�@#ƨ@#��@#��@#dZ@#33@#o@#o@"�@"��@"~�@"n�@"J@!��@!�#@!�^@!�7@!G�@!&�@!7L@!7L@!&�@ ��@ ��@ ��@ �@ bN@ Q�@ Q�@ b@�@�P@l�@�@��@�@ȴ@ȴ@��@5?@$�@�T@��@�h@/@�@V@��@�@�/@�j@��@j@�@�
@��@��@��@�@dZ@C�@@�@�H@�H@��@��@�!@~�@^5@=q@��@�^@��@�7@x�@X@X@X@X@G�@%@Ĝ@�@bN@Q�@Q�@1'@�@l�@;d@�@
=@
=@�y@ff@V@E�@$�@�-@�@?}@��@�@��@z�@I�@��@�F@t�@C�@o@�@�H@��@��@�\@~�@^5@=q@�@�#@��@hs@&�@&�@�@��@�@Q�@1'@ �@ �@b@�@�;@�;@��@�w@��@l�@K�@
=@�@�+@ff@E�@{@��@�h@O�@O�@/@V@�@�j@z�@j@I�@�m@��@t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�"�A��A�bA�VA��TA���A�^5A�;dA�/A�$�A��A�JA���A��A��mA��TA��/A��
A���A�ƨA��^A���A��hA�~�A�hsA�S�A�C�A�=qA�$�A�
=A���A��A��HA���A���A�ȴA��HA��A��FA�+A��HA�-A�A��A�?}A��A�9XA��7A��PA���A��;A�+A���A��\A��/A�C�A���A�{A�t�A�n�A���A��#A��A���A���A��A�JA��TA���A��A��A��A�ĜA�VA���A��/A��9A��A�&�A��`A���A��#A��RA��HA��A�dZA��RA���A��A���A��A�"�A���A�C�A�I�A��A��FA���A�C�A��A�
=A���A�VA�1A��A���A���A�I�A�"�A�ffA��jA7LAzM�AxAu�;As
=Ao�Al(�Ae�#AdE�Ac��AbbA]t�A[��AZv�AY�;AX��AVn�AUXATffAR�jAN��AL��AF�AC�PABE�A@5?A<ffA:VA9K�A9A7dZA69XA5XA4��A45?A3�PA2��A1�A0�\A/�mA.��A+�7A'7LA%�TA%dZA%/A$$�A!��A!\)A!"�A �A�DA�AZA"�AXA~�A��A�;A��AA�A�mAA��A�/A��A=qAZA��A
�`A	��A	��A	hsAbNA�
Ap�A�yAQ�A��AA�A��AhsA7LA�HAz�A �A��A Ĝ@��
@�-@��;@���@���@�;d@�M�@���@��@��
@�v�@�|�@���@�b@�@�o@�`B@�dZ@���@�@�o@��@�%@�S�@�v�@ݡ�@��`@�  @�;d@�@ؼj@�bN@�A�@�1'@��@׾w@�V@ԓu@�^5@�hs@�&�@��/@��;@ϕ�@�|�@�"�@θR@���@�V@˾w@�K�@�o@��@�M�@�1'@�J@Ĵ9@�ƨ@�o@���@�l�@��T@���@�Q�@�+@�ȴ@�~�@�5?@�@��7@�x�@�hs@� �@�-@�?}@�j@�I�@�9X@�  @��F@�t�@�+@���@�J@�`B@�V@�V@��`@�I�@�b@�1@���@���@�t�@�33@��\@�$�@��@���@��-@�`B@���@�Ĝ@�1'@��P@�S�@��@�~�@�$�@�{@�@��-@��h@�&�@���@�A�@��@��y@��!@�@�O�@��@�Q�@�I�@�(�@���@�dZ@��@���@�M�@��T@���@���@���@�p�@�/@�V@���@�Ĝ@�Q�@���@��y@���@�=q@�J@�@�7L@��9@���@��@��@�r�@�j@�9X@�  @���@�;d@�@��y@�ȴ@�~�@�=q@�$�@��@���@�`B@��`@�bN@��
@���@���@�dZ@��H@���@��+@�V@��@���@��h@��@��@�x�@�p�@�hs@�hs@�hs@��@�&�@�p�@��`@�Ĝ@��9@�z�@��@��;@���@��w@��P@�t�@�l�@�l�@�S�@���@��!@���@���@���@��+@�=q@�J@���@���@�p�@�X@�/@�V@��9@�9X@�1@��
@��w@��F@���@�l�@�K�@�"�@�ȴ@��\@�^5@�M�@�@���@�p�@��@���@��9@��u@��@�j@�1'@��w@�t�@�K�@�
=@��!@�E�@�{@��@���@���@�hs@��`@��u@��u@��D@�bN@�A�@��@��;@���@�33@��H@���@��!@�M�@�5?@��T@��-@��h@�x�@�O�@�V@�Ĝ@��9@��@��D@�1'@�(�@�  @��@�dZ@�l�@�\)@�C�@�+@�o@�o@��y@�ȴ@��R@���@�n�@�^5@�=q@�-@�@���@��@�`B@�O�@�/@�%@��9@���@�bN@� �@��@�b@�  @�;@��@K�@~�+@}�-@}�h@}O�@}/@|�@|��@|I�@|I�@|(�@|1@{��@{�@{C�@{33@z��@y��@yG�@y�@x�`@x��@y%@xĜ@x  @w;d@vȴ@vE�@u�@u�@t�/@t�j@t�j@t�D@t1@s�m@s��@so@r^5@q7L@q�@q�@q%@pbN@o�@o�w@o�@o\)@n��@m@m��@mp�@l�@lI�@l�@kt�@k@j�!@jn�@j-@i�@i��@i7L@h��@h�u@h�@hbN@hQ�@h1'@hb@g�@g�w@g��@g+@g
=@fȴ@f�+@fE�@e�T@e��@e�h@eO�@eV@d��@d�@dZ@d9X@cdZ@b��@b�@a�^@aX@a7L@a%@`��@`Ĝ@`�u@`r�@`A�@`b@_�@_�P@_\)@^�R@^E�@]�T@]?}@]V@\��@\�@\��@\I�@[�m@[��@[33@Zn�@Y��@Y��@Yhs@YG�@XĜ@X�@W��@V�@V�R@Vv�@VE�@V5?@U��@U�h@U`B@T�/@Tj@S�m@SdZ@SC�@S@R��@Q�#@Q��@Q7L@P�9@P�@P �@O�;@O��@O|�@O;d@Nȴ@N{@M�-@M�@Mp�@M`B@M�@MV@L��@L�j@L(�@K��@KS�@K"�@J��@JJ@I�^@I��@I�7@IG�@H�`@H�u@Hr�@HbN@HQ�@H �@H  @G��@G�w@G�P@G+@F�+@F$�@E@EO�@E�@EV@D�@D��@D��@DI�@D(�@C��@C�@CS�@C"�@C@B��@B^5@B=q@A�@A��@A��@A��@A�7@AG�@@�`@@Ĝ@@ �@?�;@?��@?\)@?�@>�y@>��@>5?@=�h@<�/@<�j@<��@<�D@<j@<(�@;�
@;S�@:�\@9��@9��@9hs@8�`@8�9@8�9@8�u@8�@8bN@8  @7�P@7;d@6�@6ȴ@6ȴ@6��@5�@5`B@5V@4��@4�j@4j@49X@41@3�
@3��@3C�@2��@2��@2�@1x�@0��@0�@0A�@01'@0b@/��@/l�@.��@.ȴ@.��@.v�@.V@.$�@.@-�T@-�T@-�T@-��@-�-@-`B@-V@,�j@,�D@,j@,(�@+ƨ@+��@+S�@+o@*��@*n�@*-@)��@)�^@)��@)�7@)hs@)&�@(��@(�u@(r�@(Q�@(  @'��@'|�@'+@'
=@&��@&�R@&��@&v�@&V@&5?@%��@%��@%?}@$��@$�/@$�j@$��@$I�@#ƨ@#��@#��@#dZ@#33@#o@#o@"�@"��@"~�@"n�@"J@!��@!�#@!�^@!�7@!G�@!&�@!7L@!7L@!&�@ ��@ ��@ ��@ �@ bN@ Q�@ Q�@ b@�@�P@l�@�@��@�@ȴ@ȴ@��@5?@$�@�T@��@�h@/@�@V@��@�@�/@�j@��@j@�@�
@��@��@��@�@dZ@C�@@�@�H@�H@��@��@�!@~�@^5@=q@��@�^@��@�7@x�@X@X@X@X@G�@%@Ĝ@�@bN@Q�@Q�@1'@�@l�@;d@�@
=@
=@�y@ff@V@E�@$�@�-@�@?}@��@�@��@z�@I�@��@�F@t�@C�@o@�@�H@��@��@�\@~�@^5@=q@�@�#@��@hs@&�@&�@�@��@�@Q�@1'@ �@ �@b@�@�;@�;@��@�w@��@l�@K�@
=@�@�+@ff@E�@{@��@�h@O�@O�@/@V@�@�j@z�@j@I�@�m@��@t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�-B�3B�9B�?B�LB�RB�^B�wB��B�fB�BB�BG�B]/Bp�Bx�B� B�7B�%Bz�Bv�Bm�BffBcTB^5BXBH�BD�B?}B<jB8RB)�B,B=qBB�BF�BK�BQ�BO�BL�BI�BE�B>wB0!B"�B�B�BhBB��B�B�fB�HB�BȴB�XB�B��B�\B�Bv�Bt�BiyB]/BT�BN�BD�B(�B\B
��B
�B
�TB
�B
�)B
�mB
�HB
�5B
�
B
ǮB
��B
|�B
p�B
9XB
B	�B	�qB	��B	�JB	p�B	R�B	,B	�B	�B	uB��B�B�HB�HB�ZB�B��B��BĜB��B�7BT�B6FB1'B;dB,B#�B�B�B�B"�B#�B!�B�B�B�B�B�B{BuBoBoBbB\B\BbB�B�B�B�B �B �B"�B"�B%�B$�B'�B%�B&�B$�B$�B%�B#�B#�B!�B$�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B&�B(�B,B/B33B6FB9XB<jB=qB=qB<jB<jB<jB<jB=qBA�BB�BB�BC�BF�BF�BF�BH�BJ�BQ�BW
BZBaHBffBffBgmBiyBjBjBhsBjBjBl�Bk�Bl�Bk�Bl�Bl�Bm�Bn�Bq�Br�Br�Br�Bv�B{�B~�B�B�B�B�B�%B�1B�7B�PB�hB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�3B�FB�RB�qB��BBƨB��B��B��B��B��B��B�B�#B�BB�fB�B�B�B��B��B��B��B��B	%B	JB	oB	�B	�B	�B	 �B	 �B	 �B	 �B	!�B	#�B	"�B	#�B	#�B	"�B	"�B	!�B	"�B	#�B	$�B	&�B	'�B	'�B	+B	,B	-B	-B	0!B	33B	8RB	:^B	=qB	>wB	?}B	B�B	E�B	H�B	L�B	N�B	O�B	Q�B	S�B	ZB	\)B	`BB	dZB	hsB	iyB	iyB	k�B	m�B	o�B	s�B	s�B	t�B	t�B	t�B	t�B	u�B	u�B	x�B	{�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�=B	�JB	�\B	�hB	�uB	�{B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�?B	�?B	�FB	�LB	�LB	�XB	�dB	�qB	�wB	�wB	�wB	��B	ÖB	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�/B	�5B	�;B	�;B	�HB	�HB	�TB	�`B	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B

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
VB
\B
bB
bB
bB
bB
hB
hB
hB
hB
hB
oB
uB
uB
uB
{B
{B
{B
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
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
-B
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
6FB
7LB
7LB
7LB
7LB
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
:^B
:^B
:^B
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
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
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
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
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
R�B
R�B
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
XB
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
_;B
_;B
_;B
_;B
`BB
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
aHB
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
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
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
k�B
k�B
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
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B��B�B��B�B�%B�2B�8B�*B�]B��B�2BgBBuBGzB]BpoBx�B�B�B�Bz�Bv�Bm]BfLBc:B^BW�BH�BDgB?cB<PB88B)�B+�B=<BBuBF�BK�BQ�BO�BL�BI�BEmB>BB/�B"�B�B_B4B �B��B�kB�LB�-B��BȀB�>B��B�eB�(B��Bv�Bt�BiDB]BT�BN�BDgB(�B(B
��B
�kB
�:B
��B
��B
�8B
�B
�B
��B
�zB
�kB
|�B
poB
9$B
�B	��B	�<B	��B	�0B	poB	R�B	+�B	~B	qB	@B��B�KB�B�B�&B��BΥB˒B�gB��B�BT�B6B0�B;0B+�B#�B�B�B�B"�B#�B!�B�BqBeB_BSBFB@B:B:B.B(B(B.BYBqBkBkB �B �B"�B"�B%�B$�B'�B%�B&�B$�B$�B%�B#�B#�B!�B$�B �B �B �B�B�B~B~BxBxBxBqBkBkBkBkBQBeBeBeBeBKBWBqB~B�B�B�B�B�B~BxBxBkBkBeBeBeBeBqBkB_B]B~B �B!�B&�B(�B+�B.�B2�B6B9$B<6B=<B="B<6B<6B<6B<6B=<BAUBB[BB[BCaBFtBFtBFtBH�BJ�BQ�BV�BY�BaBf2Bf2Bg8BiDBjKBjKBh>BjKBj0Bl=Bk6Bl=Bk6BlWBl=Bm]BncBq[Br|Br|BraBv�B{�B~�B��B��B��B��B��B��B�B�B�B�SB�EB�_B�eB��B��B��B�vB�|B��B��B��B��B��B��B��B��B��B�B�<B�;B�[B�tBʌBΥBϑBбB��B��B��B��B�B�2B�QB�CB�[B��B��B��B��B��B	�B	B	:B	eB	QB	�B	 �B	 vB	 �B	 �B	!|B	#�B	"�B	#�B	#�B	"�B	"�B	!�B	"�B	#�B	$�B	&�B	'�B	'�B	*�B	+�B	,�B	,�B	/�B	2�B	8B	:*B	="B	>BB	?HB	B[B	ESB	H�B	L~B	N�B	O�B	Q�B	S�B	Y�B	[�B	`B	d&B	h>B	i*B	i*B	kQB	m]B	oOB	s�B	s�B	t�B	t�B	t�B	t�B	u�B	utB	x�B	{�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�4B	�&B	�FB	�@B	�@B	�,B	�MB	�YB	�eB	�WB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�	B	�0B	�"B	�BB	�BB	�BB	�OB	�aB	�mB	�mB	�zB	�fB	�xB	�~B	ϫB	бB	ѷB	ҽB	��B	��B	ּB	ּB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�B	�8B	�8B	�$B	�>B	�>B	�*B	�KB	�KB	�KB	�WB	�cB	�iB	�OB	�UB	�UB	�vB	�aB	�B	�B	�B	�nB	�B	�B	��B	��B	��B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	�B
B
B
B
B
B
B
B
B
B
B
(B
.B
.B
B
.B
4B
B
B
4B
B
 B
@B
&B
&B
,B
FB
FB
MB
2B
SB
9B
SB
SB
?B
YB
_B
EB
_B
_B
EB
_B
_B
KB
KB
eB
eB
eB
QB
QB
kB
kB
qB
kB
qB
qB
qB
qB
WB
qB
~B
dB
�B
jB
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
 vB
 vB
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
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
-�B
-�B
-�B
.�B
.�B
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
1�B
1�B
1�B
1�B
2�B
2�B
4B
4B
4B
4B
5B
5B
5B
5B
6B
6B
5�B
6B
6B
5�B
7B
6�B
6�B
7B
7B
7B
8B
8B
9$B
9$B
9$B
9$B
9	B
9	B
:*B
:*B
:*B
:*B
;0B
;0B
;0B
;0B
;0B
<6B
<6B
<B
<6B
<6B
<6B
<6B
=<B
="B
=<B
>BB
>(B
>(B
>BB
?.B
?.B
?HB
?.B
@4B
AUB
A;B
AUB
AUB
A;B
AUB
AUB
B[B
B[B
CaB
CaB
CaB
DgB
DgB
DgB
DgB
DMB
DgB
EmB
EmB
EmB
FtB
FtB
FYB
FYB
FtB
GzB
G_B
GzB
GzB
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
JrB
K�B
K�B
KxB
K�B
KxB
L~B
L~B
M�B
M�B
M�B
M�B
M�B
M�B
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
R�B
R�B
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
T�B
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
Y�B
Y�B
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
Z�B
Z�B
Z�B
[�B
[�B
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
\�B
\�B
^B
^B
]�B
]�B
^B
_B
_B
^�B
_B
_B
_B
_B
_B
_B
`B
_B
_�B
`B
_�B
`B
`B
`B
`B
`�B
aB
`�B
aB
`�B
aB
aB
aB
aB
`�B
bB
bB
bB
bB
a�B
bB
bB
c B
c B
cB
c B
c B
cB
d&B
dB
d&B
dB
e,B
e,B
e,B
e,B
e,B
e,B
f2B
f2B
fB
g8B
g8B
g8B
g8B
gB
g8B
h$B
h>B
h>B
h>B
h>B
h$B
h>B
i*B
iDB
iDB
iDB
iDB
i*B
jKB
jKB
jKB
jKB
jKB
jKB
j0B
k6B
kQB
kQB
kQB
k6B
kQB
kQB
kQB
lWB
lWB
l=B
l=B
lWB
m]B
m]B
m]B
ncB
ncB
nIB
ncB
ncB
nIB
oiB
oiB
oiB
oiB
oiB
po1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.45(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802180042292018021800422920180218004229201804060310412018040603104120180406031041JA  ARFMdecpA19c                                                                20180213153514  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180213063517  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180213063518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180213063519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180213063520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180213063520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180213063520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180213063520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180213063521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180213063521                      G�O�G�O�G�O�                JA  ARUP                                                                        20180213065951                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180213153809  CV  JULD            G�O�G�O�F�`                 JM  ARCAJMQC2.0                                                                 20180217154229  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180217154229  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180405181041  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                