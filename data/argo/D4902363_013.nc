CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-05T00:35:37Z creation;2016-07-05T00:35:39Z conversion to V3.1;2019-12-19T08:36:30Z update;     
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
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20160705003537  20200115101517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_013                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @׸�6��1   @׸��� @<������dx�zxl"1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�<�D D�� D�  D�<�DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @333@s33@���@���A��A;33A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B��B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_�3Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��fD�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�|�D���D���D�6fD�y�D¹�D���D�6fD�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�6fD�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D��fD�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�<�D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�bA�VA�JA�VA��A�oA�bA��A�oA�VA�bA�VA�VA�A��TAĺ^A��A��A�~�A�&�A�v�A�hsA�XA��7A�z�A�I�A�1A���A��`A��-A�ZA��A���A��7A�z�A��FA�E�A��uA�7LA�A�ȴA�$�A���A��9A��TA���A�bNA��wA��A�%A�ZA���A�VA�t�A���A��A��#A��RA���A��#A�9XA���A�7LA��#A��A�A�9XA��
A��PA�I�A�ƨA��;A��;A�VA�A��#A���AoA}��A|��A|�\A|z�A|jA{\)Axz�Aw��Aw�Av~�Au��Au&�At~�AshsAr��Aq�#Aq33Ap��Ap9XApAo��AoG�AnE�Al-AkS�Aj��AjJAi��AiO�Ah��Ah�RAh��Ah�Af�DAdbNAbȴAbI�Aa�TAa��A`�9A_��A_oA^��A^5?A]�A]K�A[��A[G�AY�PAX�DAX1'AW;dAVM�AU%AT{AR�yAQ`BAP�!AO�ANȴAN��ANv�AM�-ALȴAK�#AKK�AJ�HAIC�AG;dAF��AFA�AEO�AD�\AC��AB��ABQ�AA��A@ȴA@1'A?�A>�`A>VA>$�A>$�A>  A=7LA<1A;�hA:�RA9�wA8�HA8{A7K�A6��A5p�A3��A2VA1VA0ZA/�A/l�A/%A.M�A,�yA,jA+S�A*{A)/A(VA'|�A';dA&�A%�;A%�A$ZA#�FA"��A"�A!�-A ȴA �A��AXA�A1'A
=A��A��A�^A��AA�A�#A�A?}A�yA^5A�PA�AZAC�A�A�A$�A��A�
A�7A��A�A~�A�mA�A��A`BA�RA$�A�A
z�A	;dAbAA�A�#A��A�9A�AE�AbA�PA�jA�;A V@�+@���@���@��-@��F@�
=@�/@�@���@��@�F@�@�K�@���@���@@��@�ƨ@�{@��@䛦@���@��@��@�bN@�v�@�j@�S�@�@�Z@׾w@�t�@�K�@���@�(�@�|�@��@�1'@�1@ϥ�@��T@���@�Ĝ@�j@ˮ@�C�@ʰ!@�V@Ȭ@�Q�@��@ǅ@���@ă@�S�@+@�M�@�E�@�-@���@�&�@��
@��H@�E�@�G�@���@���@�$�@��@��F@�33@���@��!@�ff@��7@���@�C�@���@�5?@�X@���@�1@��P@�+@�-@�X@�  @�S�@��@�V@�&�@��@�9X@��m@��P@�t�@�S�@�33@��H@��T@���@�O�@�I�@�b@��w@�C�@�ȴ@�ff@�5?@���@�O�@�7L@���@�A�@���@�^5@���@��@��D@�z�@�bN@���@�33@���@�n�@�M�@���@��9@�bN@�b@���@�l�@�ȴ@�$�@���@�&�@�z�@�Q�@�1@���@�K�@��@�ȴ@�=q@���@��7@�hs@�O�@�&�@�(�@��;@��@�C�@�o@��@�ff@��@��@��/@��D@� �@��@��@�o@���@��!@�$�@���@�O�@���@�r�@�A�@��
@�|�@�+@�+@�o@��@��y@�v�@��@�V@���@��`@���@��D@�ƨ@�K�@��@��@�V@��T@�x�@�X@�/@�&�@��j@�9X@�@K�@~ȴ@~ff@}�@}V@|�@|I�@{��@{��@z��@z�\@zJ@y�7@yG�@x��@x  @w�P@wl�@w;d@v�R@v@u`B@u�@uV@tj@t1@sS�@s@r�\@r-@q��@q�#@q��@q�#@q�@q��@rJ@rJ@q��@q�#@q�^@rJ@rJ@q&�@p��@p�@pQ�@p �@ol�@n�+@n$�@n@m��@m�-@m�@mV@l��@lZ@l(�@l1@k�m@k�@k@j~�@j-@j�@j-@j=q@jJ@i�@i�^@i��@ihs@i&�@h�`@h�`@h�`@h��@hĜ@h��@h��@h�u@hbN@hQ�@g�w@gl�@f�y@fff@f5?@f{@e�@e�-@ep�@eV@dI�@c�
@c�F@c�F@c��@c��@c��@c�@co@b��@b^5@b^5@b^5@b=q@a��@a�^@a&�@`Q�@_��@_��@_��@_�@`1'@_\)@_+@^��@^�@^ȴ@^ȴ@^�R@^�+@^V@^{@^@]�-@]?}@\�@\z�@\z�@\j@\9X@[�
@[��@[�@[�@[33@Z��@Z��@Z�!@Z��@Z=q@Y��@YX@Y&�@Y%@X��@X�u@X�@W�w@W
=@V�R@V��@VV@V@U�@UO�@UV@T�@T��@Tz�@Tj@TI�@T�@S��@S�@SS�@S33@R�@R��@R��@RM�@Q�^@QX@Q7L@Q&�@Q7L@Q&�@Q�@P��@P��@PbN@O�P@Ol�@N�y@NV@M@M`B@MV@L��@LI�@K��@K��@KdZ@KC�@K33@K"�@K@J�@J�H@Jn�@J=q@J=q@J=q@JJ@I�@I��@Ihs@H��@H�9@H�u@H �@G��@G\)@F�y@F�+@Fff@E@E/@D�D@D9X@D�@D1@C�m@C��@C�@CS�@B�@B��@B�!@B�!@B�!@B��@Bn�@A�@AG�@A%@@��@@�@@bN@@Q�@@Q�@@A�@@A�@@1'@@  @?�P@?�@>�y@>�R@>V@>$�@=@=p�@=?}@=V@<��@<��@<�j@<�@<��@<Z@;�
@;t�@;dZ@;o@:�@:�H@:��@:��@:�\@:~�@:^5@:J@9�7@9&�@9%@8��@8�@8A�@8 �@8b@8b@8  @7�;@7��@7�@7l�@7K�@6�y@6�+@6V@5�h@5O�@5�@4��@4�@4��@4I�@3�m@3t�@3C�@3C�@333@3"�@3o@2��@2~�@2�@1�^@1��@1x�@17L@0��@0Ĝ@0�9@0�9@0�u@0  @/�w@/��@/�P@/l�@/;d@/+@.��@.�R@.v�@.ff@-��@-�h@-�h@-p�@-`B@-?}@-/@,�j@,Z@+��@+�F@+t�@+S�@*��@*-@)��@)��@)X@)7L@)7L@)�@(��@(�9@(�@(Q�@(b@(  @'�@'�@'�P@'l�@';d@&��@&ȴ@&ȴ@&�R@&�+@&E�@&5?@&$�@&{@%@%/@$�j@$I�@$�@#�
@#"�@"�@"�!@"~�@"=q@"-@"-@"-@"-@"-@"�@"J@!�#@!�7@!X@!7L@!�@ ��@ Ĝ@ �@ A�@   @�P@K�@ȴ@V@E�@$�@�@�T@�-@�h@`B@V@�@��@�@�D@z�@Z@�
@�H@�!@��@�\@~�@~�@n�@-@�#@��@�7@�7@x�@hs@G�@�@�`@Ĝ@�9@�u@�u@�@bN@��@+@��@v�@E�@5?@$�@@�-@p�@`B@?}@V@��@�/@�j@z�@Z@I�@I�@I�@�@�
@��@dZ@o@�H@�!@�\@�\@~�@n�@�#@G�@&�@�@�`@�u@1'@��@�w@�@�@��@|�@;d@�@ȴ@5?@{@�@�@�@��@�-@�-@�-@��@�h@O�@��@�@�/@��@�j@��@��@��@�@�@��@�D@Z@(�@�m@ƨ@��@S�@o@
�H@
�!@
�\@
�\@
n�@
M�@
-@
J@	�#@	��@	��@	X@��@�`@��@Ĝ@�@Q�@ �@��@�P@�P@\)@+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�bA�VA�JA�VA��A�oA�bA��A�oA�VA�bA�VA�VA�A��TAĺ^A��A��A�~�A�&�A�v�A�hsA�XA��7A�z�A�I�A�1A���A��`A��-A�ZA��A���A��7A�z�A��FA�E�A��uA�7LA�A�ȴA�$�A���A��9A��TA���A�bNA��wA��A�%A�ZA���A�VA�t�A���A��A��#A��RA���A��#A�9XA���A�7LA��#A��A�A�9XA��
A��PA�I�A�ƨA��;A��;A�VA�A��#A���AoA}��A|��A|�\A|z�A|jA{\)Axz�Aw��Aw�Av~�Au��Au&�At~�AshsAr��Aq�#Aq33Ap��Ap9XApAo��AoG�AnE�Al-AkS�Aj��AjJAi��AiO�Ah��Ah�RAh��Ah�Af�DAdbNAbȴAbI�Aa�TAa��A`�9A_��A_oA^��A^5?A]�A]K�A[��A[G�AY�PAX�DAX1'AW;dAVM�AU%AT{AR�yAQ`BAP�!AO�ANȴAN��ANv�AM�-ALȴAK�#AKK�AJ�HAIC�AG;dAF��AFA�AEO�AD�\AC��AB��ABQ�AA��A@ȴA@1'A?�A>�`A>VA>$�A>$�A>  A=7LA<1A;�hA:�RA9�wA8�HA8{A7K�A6��A5p�A3��A2VA1VA0ZA/�A/l�A/%A.M�A,�yA,jA+S�A*{A)/A(VA'|�A';dA&�A%�;A%�A$ZA#�FA"��A"�A!�-A ȴA �A��AXA�A1'A
=A��A��A�^A��AA�A�#A�A?}A�yA^5A�PA�AZAC�A�A�A$�A��A�
A�7A��A�A~�A�mA�A��A`BA�RA$�A�A
z�A	;dAbAA�A�#A��A�9A�AE�AbA�PA�jA�;A V@�+@���@���@��-@��F@�
=@�/@�@���@��@�F@�@�K�@���@���@@��@�ƨ@�{@��@䛦@���@��@��@�bN@�v�@�j@�S�@�@�Z@׾w@�t�@�K�@���@�(�@�|�@��@�1'@�1@ϥ�@��T@���@�Ĝ@�j@ˮ@�C�@ʰ!@�V@Ȭ@�Q�@��@ǅ@���@ă@�S�@+@�M�@�E�@�-@���@�&�@��
@��H@�E�@�G�@���@���@�$�@��@��F@�33@���@��!@�ff@��7@���@�C�@���@�5?@�X@���@�1@��P@�+@�-@�X@�  @�S�@��@�V@�&�@��@�9X@��m@��P@�t�@�S�@�33@��H@��T@���@�O�@�I�@�b@��w@�C�@�ȴ@�ff@�5?@���@�O�@�7L@���@�A�@���@�^5@���@��@��D@�z�@�bN@���@�33@���@�n�@�M�@���@��9@�bN@�b@���@�l�@�ȴ@�$�@���@�&�@�z�@�Q�@�1@���@�K�@��@�ȴ@�=q@���@��7@�hs@�O�@�&�@�(�@��;@��@�C�@�o@��@�ff@��@��@��/@��D@� �@��@��@�o@���@��!@�$�@���@�O�@���@�r�@�A�@��
@�|�@�+@�+@�o@��@��y@�v�@��@�V@���@��`@���@��D@�ƨ@�K�@��@��@�V@��T@�x�@�X@�/@�&�@��j@�9X@�@K�@~ȴ@~ff@}�@}V@|�@|I�@{��@{��@z��@z�\@zJ@y�7@yG�@x��@x  @w�P@wl�@w;d@v�R@v@u`B@u�@uV@tj@t1@sS�@s@r�\@r-@q��@q�#@q��@q�#@q�@q��@rJ@rJ@q��@q�#@q�^@rJ@rJ@q&�@p��@p�@pQ�@p �@ol�@n�+@n$�@n@m��@m�-@m�@mV@l��@lZ@l(�@l1@k�m@k�@k@j~�@j-@j�@j-@j=q@jJ@i�@i�^@i��@ihs@i&�@h�`@h�`@h�`@h��@hĜ@h��@h��@h�u@hbN@hQ�@g�w@gl�@f�y@fff@f5?@f{@e�@e�-@ep�@eV@dI�@c�
@c�F@c�F@c��@c��@c��@c�@co@b��@b^5@b^5@b^5@b=q@a��@a�^@a&�@`Q�@_��@_��@_��@_�@`1'@_\)@_+@^��@^�@^ȴ@^ȴ@^�R@^�+@^V@^{@^@]�-@]?}@\�@\z�@\z�@\j@\9X@[�
@[��@[�@[�@[33@Z��@Z��@Z�!@Z��@Z=q@Y��@YX@Y&�@Y%@X��@X�u@X�@W�w@W
=@V�R@V��@VV@V@U�@UO�@UV@T�@T��@Tz�@Tj@TI�@T�@S��@S�@SS�@S33@R�@R��@R��@RM�@Q�^@QX@Q7L@Q&�@Q7L@Q&�@Q�@P��@P��@PbN@O�P@Ol�@N�y@NV@M@M`B@MV@L��@LI�@K��@K��@KdZ@KC�@K33@K"�@K@J�@J�H@Jn�@J=q@J=q@J=q@JJ@I�@I��@Ihs@H��@H�9@H�u@H �@G��@G\)@F�y@F�+@Fff@E@E/@D�D@D9X@D�@D1@C�m@C��@C�@CS�@B�@B��@B�!@B�!@B�!@B��@Bn�@A�@AG�@A%@@��@@�@@bN@@Q�@@Q�@@A�@@A�@@1'@@  @?�P@?�@>�y@>�R@>V@>$�@=@=p�@=?}@=V@<��@<��@<�j@<�@<��@<Z@;�
@;t�@;dZ@;o@:�@:�H@:��@:��@:�\@:~�@:^5@:J@9�7@9&�@9%@8��@8�@8A�@8 �@8b@8b@8  @7�;@7��@7�@7l�@7K�@6�y@6�+@6V@5�h@5O�@5�@4��@4�@4��@4I�@3�m@3t�@3C�@3C�@333@3"�@3o@2��@2~�@2�@1�^@1��@1x�@17L@0��@0Ĝ@0�9@0�9@0�u@0  @/�w@/��@/�P@/l�@/;d@/+@.��@.�R@.v�@.ff@-��@-�h@-�h@-p�@-`B@-?}@-/@,�j@,Z@+��@+�F@+t�@+S�@*��@*-@)��@)��@)X@)7L@)7L@)�@(��@(�9@(�@(Q�@(b@(  @'�@'�@'�P@'l�@';d@&��@&ȴ@&ȴ@&�R@&�+@&E�@&5?@&$�@&{@%@%/@$�j@$I�@$�@#�
@#"�@"�@"�!@"~�@"=q@"-@"-@"-@"-@"-@"�@"J@!�#@!�7@!X@!7L@!�@ ��@ Ĝ@ �@ A�@   @�P@K�@ȴ@V@E�@$�@�@�T@�-@�h@`B@V@�@��@�@�D@z�@Z@�
@�H@�!@��@�\@~�@~�@n�@-@�#@��@�7@�7@x�@hs@G�@�@�`@Ĝ@�9@�u@�u@�@bN@��@+@��@v�@E�@5?@$�@@�-@p�@`B@?}@V@��@�/@�j@z�@Z@I�@I�@I�@�@�
@��@dZ@o@�H@�!@�\@�\@~�@n�@�#@G�@&�@�@�`@�u@1'@��@�w@�@�@��@|�@;d@�@ȴ@5?@{@�@�@�@��@�-@�-@�-@��@�h@O�@��@�@�/@��@�j@��@��@��@�@�@��@�D@Z@(�@�m@ƨ@��@S�@o@
�H@
�!@
�\@
�\@
n�@
M�@
-@
J@	�#@	��@	��@	X@��@�`@��@Ĝ@�@Q�@ �@��@�P@�P@\)@+1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�=B�bB�!B��B��B��B��B��BB�{B[#B(�B�B��B�9B�uB}�By�Bw�Bt�Bm�B^5BM�BA�B0!B�BVB+BBB��B�B�#B�
B��B�}B�?B�B��B��B�JB~�Br�B_;B\)BQ�BF�B=qB6FB0!B"�B{BJBBB
��B
�B
�ZB
�B
��B
ÖB
��B
�dB
�B
��B
��B
�{B
�uB
�hB
�DB
s�B
l�B
gmB
aHB
\)B
VB
Q�B
J�B
E�B
?}B
;dB
9XB
9XB
9XB
8RB
5?B
.B
!�B
�B
�B
VB
DB
1B
B
B
B	��B	��B	�sB	�#B	��B	��B	��B	ɺB	B	�jB	�^B	�RB	�FB	�3B	��B	��B	��B	�\B	�JB	�%B	�B	� B	x�B	s�B	n�B	jB	gmB	`BB	_;B	]/B	[#B	XB	T�B	O�B	L�B	E�B	9XB	5?B	2-B	-B	&�B	"�B	�B	�B	�B	{B	oB	hB	bB	\B	\B	VB	VB	JB	1B	+B	%B��B��B��B�B�B�fB�/B�B��B��B��B��B��B��BĜB��B�wB�LB�9B�'B�B�B�B��B��B��B��B��B��B�{B�hB�PB�DB�=B�1B�%B�B� B� B~�B|�Bz�Bx�Bw�Bv�Bt�Bs�Bp�Bn�Bm�BjBiyBhsBgmBffBe`BdZBcTBaHB^5B\)B[#BZBYBVBT�BS�BR�BP�BI�BK�BL�BL�BI�BH�BG�BH�BG�BF�BC�BA�B<jB8RB33B1'B0!B/B/B.B+B'�B&�B%�B%�B$�B$�B#�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B#�B �B�B�B�B�B�B#�B$�B$�B$�B"�B!�B!�B'�B(�B)�B+B+B-B-B/B/B/B/B/B/B0!B1'B0!B/B0!B2-B2-B2-B6FB7LB9XB;dB<jB<jB>wBA�BA�BB�BB�BC�BD�BD�BF�BL�BM�BN�BQ�BQ�BT�BT�BW
BZB[#B]/B^5B^5B^5B^5B_;BbNBbNBe`BhsBhsBhsBiyBk�Bn�Bo�Bq�Br�Br�Br�Bt�By�B{�B}�B� B�B�B�B�B�+B�=B�DB�DB�VB�\B�oB�{B��B��B��B��B��B��B��B��B��B�B�!B�!B�-B�?B�LB�RB�XB�XB�^B�}B��BBĜBƨBǮB��B��B��B��B�B�#B�/B�BB�TB�`B�fB�mB�yB�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	B	+B	1B	DB	hB	{B	�B	�B	�B	�B	"�B	$�B	&�B	'�B	,B	0!B	2-B	33B	5?B	7LB	9XB	<jB	@�B	B�B	C�B	E�B	G�B	H�B	J�B	L�B	M�B	N�B	R�B	S�B	S�B	T�B	W
B	[#B	^5B	`BB	`BB	bNB	dZB	gmB	hsB	k�B	m�B	o�B	p�B	q�B	r�B	t�B	t�B	u�B	u�B	x�B	z�B	|�B	�B	�%B	�+B	�%B	�%B	�+B	�7B	�=B	�=B	�=B	�=B	�=B	�=B	�DB	�JB	�PB	�VB	�VB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�XB	�jB	�jB	�jB	�qB	�wB	�}B	�}B	��B	B	ĜB	ŢB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�BB	�NB	�TB	�ZB	�ZB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
1B
	7B

=B

=B
DB
JB
PB
VB
VB
\B
\B
\B
\B
bB
hB
hB
hB
hB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
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
+B
+B
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
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
5?B
5?B
5?B
5?B
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
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
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
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
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
O�B
O�B
O�B
P�B
P�B
P�B
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
T�B
T�B
T�B
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
aHB
aHB
bNB
bNB
bNB
bNB
cTB
bNB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
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
l�B
l�B
l�B
m�B
m�B
m�B
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�-B�-B�3B�-B�MB�B�3B�3B�MB�gB��B�B��B�B̘B�B� B��B�BB�^B��Bf�B3�B��B�NB��B��B~�Bz�By>Bw�BqvBabBPBD�B2�B;B�B�BSB-B�^B��B��B�B��B� B��B��B�B��B�VB�UBt�B`'B]�BS�BHB>�B7�B2|B%FB�BPB�BB
��B
��B
�B
�YB
ˬB
�MB
��B
��B
��B
��B
�7B
��B
�,B
�@B
��B
t�B
m]B
hXB
bNB
]/B
W$B
S@B
K�B
F�B
@iB
<B
9�B
9�B
:B
9$B
6�B
0UB
"�B
eB
sB
�B
�B
�B
�B
�B
B
�B	�*B	�0B	��B	՛B	ңB	�.B	��B	�aB	�"B	�B	�	B	�fB	��B	�QB	��B	��B	�HB	��B	��B	��B	�oB	z�B	u�B	o�B	k�B	hsB	`�B	_�B	^jB	\xB	Y1B	U�B	QB	OB	G�B	:^B	6B	3�B	./B	'�B	$@B	�B	�B	�B	gB	uB	TB	B	�B	�B	�B	�B	�B		7B	�B	zB�BB�B�B�B�B�B��BںB�B��B��BҽB�.B�~BŢB�-B�B��B�tB�-B��B�B�=B�B��B��B��B�KB��B��B�oB�B��B�)B�lB�zB��B��B�UB�4B}�B{�ByrBxlBw�Bu�Bt�Bq�Bo�Bn�Bk6Bj0BiBg�Bf�Bf2Be�Bd�BcB_;B]B\xB[�BZBW$BV9BU�BT�BRTBJ�BMBNVBM�BJ=BI7BHKBIRBH�BG�BEBCaB=�B9�B4�B2�B1vB0!B0�B/�B,"B(�B'RB&LB&LB%,B%`B$�B$&B$&B!�BpBOB�B�B~BB;B!B�B�B�B!HB!-B!bB!�B$�B!�B!HB vBBpB�B$�B%,B%`B%zB#nB"�B"�B(XB)yB*B+�B,=B.B-�B/�B/iB/iB/iB/�B0B1'B1�B0�B0B1[B2�B2�B3hB6�B7�B9�B;�B<�B="B?cBB�BB'BC-BCGBD3BEmBE9BGEBM�BN�BO�BR�BR�BU�BU�BW�BZ�B[�B]�B^�B^�B^jB^�B_�Bb�Bb�Be�Bh�Bh�Bh�Bi�Bk�Bo Bp!BrBr�BsBs�Bu�BzxB|�B~�B�iB�AB�[B��B��B��B��B��B��B�B��B��B��B��B�KB�CB�/B�;B�bB�FB�RB�B��B�oB��B��B��B��B��B��B��B�B��B��B��B��B�B�KB�xB�HB�@B�gB�yB�qBݲB�B�B�B��B��B��B�"B��B��B�3B�8B�0B�"B�"B�<B�BB��B	 �B	uB	aB	aB	zB	�B	�B	�B	�B	�B	B	)B	 'B	#B	%,B	'B	(XB	,�B	0oB	2�B	3�B	5tB	7�B	9�B	<�B	@�B	B�B	C�B	FB	G�B	IB	KB	MB	N"B	OBB	S@B	T,B	TaB	UgB	WsB	[qB	^jB	`vB	`�B	b�B	d�B	g�B	h�B	k�B	m�B	o�B	p�B	q�B	r�B	t�B	t�B	u�B	u�B	y	B	{0B	}B	�AB	��B	�zB	�YB	�YB	��B	��B	��B	��B	�rB	�rB	�rB	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�&B	�8B	�DB	�WB	�cB	�OB	�UB	�[B	�aB	��B	��B	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	żB	��B	��B	��B	�B	�#B	�B	��B	��B	��B	�B	�FB	�,B	�2B	�9B	�9B	�$B	�EB	�EB	�KB	�WB	�]B	�xB	�~B	��B	�B	�nB	�tB	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	��B	�B	�B	�B	�B	�<B	�B	�(B	�(B	�.B	�.B	�.B	�.B
 OB
 4B
;B
'B
'B
AB
AB
GB
GB
{B
aB
MB
gB
mB
tB
fB
	lB

rB

�B
xB
~B
�B
�B
�B
vB
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
 B
!B
!�B
#B
#B
$B
#�B
#�B
#�B
#�B
$B
$B
$&B
%,B
&2B
&2B
'B
'B
'8B
($B
)*B
)*B
)*B
)*B
*B
*0B
*0B
*0B
*KB
+6B
+6B
+B
,=B
,=B
,=B
,"B
,=B
-CB
-CB
-CB
-]B
.cB
/iB
/OB
/OB
/OB
0;B
0;B
0;B
0;B
0UB
1AB
1[B
1[B
1[B
2aB
3�B
3hB
3�B
5tB
5tB
5tB
5tB
6zB
6�B
6�B
7�B
7�B
7fB
8lB
8�B
8�B
8�B
8�B
9�B
9�B
:�B
:�B
:�B
:�B
;�B
;�B
;B
;�B
;�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?}B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
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
H�B
H�B
H�B
H�B
H�B
IB
IB
J#B
J	B
J�B
J�B
KB
LB
LB
MB
MB
L�B
L�B
L�B
L�B
L�B
M�B
NB
NB
NB
O(B
O(B
OB
PB
PB
PB
Q B
QB
Q4B
R B
RTB
S&B
S&B
S&B
S&B
T,B
T,B
T,B
T,B
T,B
U2B
U2B
U2B
U2B
U2B
U2B
U2B
VmB
WYB
W?B
W$B
W
B
W$B
W?B
W$B
W?B
X+B
X+B
X+B
X+B
X+B
XEB
XEB
YKB
YKB
Y1B
YKB
Y1B
YKB
Y1B
YeB
ZkB
[WB
[qB
[qB
\CB
\]B
\xB
\]B
\]B
\]B
]dB
]dB
]IB
]dB
]IB
]dB
^jB
^OB
^OB
^OB
^jB
^jB
_pB
_�B
_�B
_pB
`vB
`vB
`\B
`vB
`vB
`�B
a�B
a|B
a|B
a|B
a|B
a�B
b�B
bhB
bhB
b�B
cnB
b�B
c�B
c�B
d�B
d�B
ezB
ezB
ezB
ezB
e�B
e�B
ezB
ezB
ezB
e�B
e�B
f�B
f�B
ffB
f�B
g�B
g�B
gmB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<:��<,
0<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.2(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607090035342016070900353420160709003534201806221210332018062212103320180622121033201804050402512018040504025120180405040251  JA  ARFMdecpA19c                                                                20160705093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160705003537  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160705003537  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160705003538  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160705003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160705003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160705003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160705003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160705003539  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160705003539                      G�O�G�O�G�O�                JA  ARUP                                                                        20160705012009                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160705153618  CV  JULD            G�O�G�O�F�ǲ                JM  ARCAJMQC2.0                                                                 20160708153534  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160708153534  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190251  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031033  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101517                      G�O�G�O�G�O�                