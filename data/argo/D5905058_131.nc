CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-03-13T03:37:11Z creation;2019-03-13T03:37:14Z conversion to V3.1;2019-12-23T06:05:34Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190313033711  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_131                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @خA�+��1   @خB>�� @8x�p:��c41&�y1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D+��D,� D-  D-� D.  D.� D/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�<�Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ffA��A%��AE��Ae��A���A���A���A���A���A���A���A���BffB	ffBffBffB!ffB)ffB1ffB9ffBAffBIffBQffBYffBaffBiffBqffByffB��3B��3B��3B��3B��3B��3B��fB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bĳ3Bȳ3B̳3Bг3BԳ3Bس3Bܳ3B�3B�3B�3B�3B�3B��3B��3B��3C Y�CY�CY�CY�CY�C
Y�CY�CY�CY�CY�CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(Y�C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:Y�C<Y�C>Y�C@Y�CBY�CDY�CFY�CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXY�CZY�C\Y�C^Y�C`Y�CbY�CdY�CfY�ChY�CjY�ClY�CnY�CpY�CrY�CtY�CvY�CxY�CzY�C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�D fD �fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)��D*fD*�fD+fD+�fD, D,�fD-fD-�fD.fD.�fD/fD/�fD0�D0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDofDo�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDufDu�fDvfDv�fDwfDw�fDxfDx�fDyfDy�fDzfDz�fD{fD{�fD|fD|�fD}fD}�fD~fD~�fDfD�fD�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��fD��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�NfD��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D3D��3D�3D�K3DË3D��3D�3D�K3Dċ3D��3D�3D�K3Dŋ3D��3D�3D�K3DƋ3D��3D�3D�K3Dǋ3D��3D�3D�K3Dȋ3D��3D�3D�K3Dɋ3D��3D�3D�K3Dʋ3D��3D�3D�K3Dˋ3D��3D�3D�K3D̋3D��3D�3D�K3D͋3D��3D�3D�K3D΋3D��3D�3D�K3Dϋ3D��3D�3D�K3DЋ3D��3D�3D�K3Dы3D��3D�3D�K3Dҋ3D��3D�3D�K3DӋ3D��3D�3D�K3Dԋ3D��3D�3D�K3DՋ3D��3D�3D�K3D֋3D��3D�3D�K3D׋3D��3D�3D�K3D؋3D��3D�3D�H Dً3D��3D�3D�K3Dڋ3D��3D�3D�K3Dۋ3D��3D�3D�K3D܋3D��3D�3D�K3D݋3D��3D�3D�K3Dދ3D��3D�3D�K3Dߋ3D��3D�3D�K3D��3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�fD��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D��3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��fD�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A��TA��RA�|�A�Q�A�/A�"�A��A�VA�1A���A��A��TA��#A���A���A�ȴA�A���A��wA��9A��^A��!A�bNA��
A��A�=qA��HA���A��uA�Q�A��`A�33A���A���A��mA��DA�p�A�^5A�?}A��A���A��A��;A���A��jA���A�~�A�5?A�A���A�E�A��A���A�`BA��A���A�M�A�?}A�?}A�33A���A�;dA��RA�hsA�(�A�ĜA�C�A�A���A���A��9A���A��A��A�G�A�A�A���A���A���A�A��#A�p�A��7A���A�bNA�XA�|�A�A�l�A��A��A�VA��yA��\A�VA��-A��A�9XA��yA���A�dZA�^5A��;A��uA��7A���A��A�1'A��uA�/A�33A�~�A��A���A~�A|bNAz��Ax��Aw;dAv(�At�Ap=qAh-Ae�
AdbNAb��A`��A_�A^�yA]AZ��AXVAT�DASAP{AOVAN1'AL��AI��AH��AH��AH�DAG��AEp�ACt�ABI�AA�hA@Q�A>�yA>z�A=��A;\)A:A9%A8(�A6�/A6^5A5�#A4�HA3�wA2JA0�RA/��A.��A-�A,n�A+�-A+O�A)�hA'��A&�uA%oA${A"��A!AA�AXA�TA��A�A�wAA�AK�Av�AhsA�jAƨAK�A�uA��A�7A+AȴA��A��AE�Ap�A^5A��A
�HA	A��A��A`BA�DAhsA7LA9XA�AA�A�;A�#Ax�A ��A =q@���@�/@���@��/@���@�p�@�l�@��@���@�%@�Q�@�F@�ȴ@��T@�t�@�@�b@�|�@�\@�l�@�z�@ߥ�@�@�S�@�v�@�E�@���@���@�@Չ7@�X@�V@ԋD@Ӿw@�;d@�ff@�V@���@���@Ь@�Q�@ϝ�@ΰ!@��@��@�z�@��;@�33@�5?@���@��@��
@�"�@�J@��/@Ý�@��-@�(�@�;d@��R@�{@��T@�x�@�z�@��@��w@�
=@��-@���@�b@��F@�\)@���@��#@��@�1@�K�@��R@��@��@�1'@��y@��\@�$�@��-@�O�@��j@�1@�|�@�
=@�M�@�@��#@��^@�x�@��`@�Z@�ƨ@�\)@���@���@�~�@�J@��#@�x�@��@��@��@��9@�1'@���@�t�@�;d@�
=@��@��@���@�ff@�=q@�{@��-@��h@�?}@��@���@�z�@�9X@�b@�ƨ@�S�@�"�@�@��H@���@�5?@��@���@���@���@�p�@�&�@��@��9@��D@�I�@�1@��F@�t�@�S�@�"�@��@���@�ff@�^5@�@���@�`B@���@���@�bN@�(�@���@�\)@�"�@�
=@���@���@�=q@�{@��@��^@���@�x�@��@��`@��j@��D@�A�@��m@��@��P@�;d@��\@�@���@��T@�$�@��#@��@�p�@�`B@�/@�&�@���@�V@�hs@�%@��D@���@��@�A�@�9X@�(�@�t�@��@�@��@���@���@��\@��\@�ff@�=q@��@���@��@�O�@�G�@�G�@�G�@�G�@�X@�X@�7L@��@��@��D@��D@�bN@�1'@�  @���@�t�@�t�@�t�@�l�@�+@���@�v�@�5?@�$�@��@��@�`B@�7L@��@��/@��@��u@�Z@���@��m@���@��w@��@���@�l�@�\)@�K�@�C�@�;d@�33@�+@�+@��@��y@��@���@�^5@�J@�@���@���@��@�`B@�G�@��@���@��/@���@��9@��D@�Q�@�1'@��@���@��w@��F@���@�;d@�
=@���@��+@�E�@��#@��-@��@�/@��/@���@�bN@�(�@�1@|�@~�+@~$�@~{@}��@}/@|�@|j@|�@|1@|1@{��@{ƨ@{��@{S�@z��@z^5@y��@yX@yG�@y7L@y7L@y&�@x��@x �@w��@w�@w|�@w\)@w\)@w+@v�R@v$�@u@u�@u/@uV@t9X@s�
@sdZ@r�!@r~�@rM�@q�#@qX@q�@p�`@p��@p��@p�u@p �@o�;@o
=@n�R@nE�@n@m�-@mO�@m/@mV@l�@l�j@lz�@l9X@k��@kdZ@k@j~�@i��@ix�@iX@i�@hĜ@h�u@hQ�@g�;@g\)@fȴ@fv�@e�T@e�@dz�@d�@c��@c�@cdZ@b�@b=q@a�#@a�7@a7L@`�`@`��@`r�@_�@_K�@_
=@^ȴ@]�T@]O�@]V@\�@\I�@\(�@[�m@[�@[S�@[C�@Z�@Z�\@Z-@Y�^@Yx�@YG�@Y%@X�`@X��@X�@W��@W;d@W�@W
=@V��@V��@V�+@U@UO�@U/@T�@T�@Tj@TI�@T9X@T(�@T�@Sƨ@SC�@So@R�@R��@R��@R�!@R�\@RM�@Q�@Qx�@Q7L@P��@P�@PA�@P  @O��@O�P@Ol�@O;d@N�R@Nff@NE�@N{@M�T@M@M�h@M�@M?}@L��@L�@L9X@K�m@Ko@J~�@JM�@J=q@J�@I�#@IX@I&�@I�@I�@I%@H��@H��@HbN@Hb@G�;@G�w@G�P@Gl�@G
=@F��@F�@F�R@F��@F5?@E@E�h@E�@E�@Ep�@E`B@E?}@D�@DZ@D(�@D1@C��@C�
@Cƨ@C��@C��@CS�@B��@B�\@B=q@BJ@A��@A��@Ahs@A&�@@Ĝ@@r�@@A�@@ �@?�;@?|�@?\)@?K�@?+@?�@?
=@>�R@>V@>{@=�T@=��@=p�@<�/@<�@<�D@<(�@;�m@;��@;dZ@:�@:~�@:-@9�^@9�^@9hs@9%@8�u@8A�@7�@7;d@6�@6�+@6@5�-@5p�@5?}@5V@4��@4�@4z�@4(�@3�m@3��@3�@3o@2��@2��@2M�@2J@1��@1�7@17L@0��@0�@0  @/�@/|�@/\)@/;d@/
=@.�@.ȴ@.��@.v�@.v�@.v�@.ff@.ff@.ff@.ff@.ff@.ff@.5?@.@-��@-�@-O�@-/@,�@,�j@,(�@,1@,1@+�
@+��@+C�@*�@*��@*M�@*=q@*=q@*-@*�@*�@*J@)�@)��@)X@)7L@)%@(��@(�9@(�@(Q�@'�;@'��@'��@'|�@'|�@'l�@'K�@'
=@&�R@&ff@%�@%��@%�-@%�@%`B@%/@$�@$�D@$(�@$1@#�
@#��@#dZ@#33@#33@#o@"�@"��@"�!@"~�@"^5@"-@!��@!��@!X@!G�@!G�@!%@ �9@ �@ bN@ bN@ bN@ Q�@  �@��@;d@��@�R@��@��@��@��@�+@ff@�T@�-@��@O�@V@�/@�j@�@Z@�@�
@�F@t�@o@~�@�@�^@��@hs@�@�9@Q�@Q�@Q�@  @�;@��@�w@�@�@��@|�@K�@�@�@�R@��@v�@E�@{@@`B@��@�/@��@�@I�@��@�m@�
@t�@o@�H@�\@^5@=q@�@��@&�@�@%@��@��@�9@r�@Q�@ �@�@�P@K�@�y@��@�+@5?@{@@@p�@O�@�@�/@�@z�@I�@9X@�@1@�F@�@dZ@C�@
��@
�\@
n�@
^5@
M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�M�A��TA��RA�|�A�Q�A�/A�"�A��A�VA�1A���A��A��TA��#A���A���A�ȴA�A���A��wA��9A��^A��!A�bNA��
A��A�=qA��HA���A��uA�Q�A��`A�33A���A���A��mA��DA�p�A�^5A�?}A��A���A��A��;A���A��jA���A�~�A�5?A�A���A�E�A��A���A�`BA��A���A�M�A�?}A�?}A�33A���A�;dA��RA�hsA�(�A�ĜA�C�A�A���A���A��9A���A��A��A�G�A�A�A���A���A���A�A��#A�p�A��7A���A�bNA�XA�|�A�A�l�A��A��A�VA��yA��\A�VA��-A��A�9XA��yA���A�dZA�^5A��;A��uA��7A���A��A�1'A��uA�/A�33A�~�A��A���A~�A|bNAz��Ax��Aw;dAv(�At�Ap=qAh-Ae�
AdbNAb��A`��A_�A^�yA]AZ��AXVAT�DASAP{AOVAN1'AL��AI��AH��AH��AH�DAG��AEp�ACt�ABI�AA�hA@Q�A>�yA>z�A=��A;\)A:A9%A8(�A6�/A6^5A5�#A4�HA3�wA2JA0�RA/��A.��A-�A,n�A+�-A+O�A)�hA'��A&�uA%oA${A"��A!AA�AXA�TA��A�A�wAA�AK�Av�AhsA�jAƨAK�A�uA��A�7A+AȴA��A��AE�Ap�A^5A��A
�HA	A��A��A`BA�DAhsA7LA9XA�AA�A�;A�#Ax�A ��A =q@���@�/@���@��/@���@�p�@�l�@��@���@�%@�Q�@�F@�ȴ@��T@�t�@�@�b@�|�@�\@�l�@�z�@ߥ�@�@�S�@�v�@�E�@���@���@�@Չ7@�X@�V@ԋD@Ӿw@�;d@�ff@�V@���@���@Ь@�Q�@ϝ�@ΰ!@��@��@�z�@��;@�33@�5?@���@��@��
@�"�@�J@��/@Ý�@��-@�(�@�;d@��R@�{@��T@�x�@�z�@��@��w@�
=@��-@���@�b@��F@�\)@���@��#@��@�1@�K�@��R@��@��@�1'@��y@��\@�$�@��-@�O�@��j@�1@�|�@�
=@�M�@�@��#@��^@�x�@��`@�Z@�ƨ@�\)@���@���@�~�@�J@��#@�x�@��@��@��@��9@�1'@���@�t�@�;d@�
=@��@��@���@�ff@�=q@�{@��-@��h@�?}@��@���@�z�@�9X@�b@�ƨ@�S�@�"�@�@��H@���@�5?@��@���@���@���@�p�@�&�@��@��9@��D@�I�@�1@��F@�t�@�S�@�"�@��@���@�ff@�^5@�@���@�`B@���@���@�bN@�(�@���@�\)@�"�@�
=@���@���@�=q@�{@��@��^@���@�x�@��@��`@��j@��D@�A�@��m@��@��P@�;d@��\@�@���@��T@�$�@��#@��@�p�@�`B@�/@�&�@���@�V@�hs@�%@��D@���@��@�A�@�9X@�(�@�t�@��@�@��@���@���@��\@��\@�ff@�=q@��@���@��@�O�@�G�@�G�@�G�@�G�@�X@�X@�7L@��@��@��D@��D@�bN@�1'@�  @���@�t�@�t�@�t�@�l�@�+@���@�v�@�5?@�$�@��@��@�`B@�7L@��@��/@��@��u@�Z@���@��m@���@��w@��@���@�l�@�\)@�K�@�C�@�;d@�33@�+@�+@��@��y@��@���@�^5@�J@�@���@���@��@�`B@�G�@��@���@��/@���@��9@��D@�Q�@�1'@��@���@��w@��F@���@�;d@�
=@���@��+@�E�@��#@��-@��@�/@��/@���@�bN@�(�@�1@|�@~�+@~$�@~{@}��@}/@|�@|j@|�@|1@|1@{��@{ƨ@{��@{S�@z��@z^5@y��@yX@yG�@y7L@y7L@y&�@x��@x �@w��@w�@w|�@w\)@w\)@w+@v�R@v$�@u@u�@u/@uV@t9X@s�
@sdZ@r�!@r~�@rM�@q�#@qX@q�@p�`@p��@p��@p�u@p �@o�;@o
=@n�R@nE�@n@m�-@mO�@m/@mV@l�@l�j@lz�@l9X@k��@kdZ@k@j~�@i��@ix�@iX@i�@hĜ@h�u@hQ�@g�;@g\)@fȴ@fv�@e�T@e�@dz�@d�@c��@c�@cdZ@b�@b=q@a�#@a�7@a7L@`�`@`��@`r�@_�@_K�@_
=@^ȴ@]�T@]O�@]V@\�@\I�@\(�@[�m@[�@[S�@[C�@Z�@Z�\@Z-@Y�^@Yx�@YG�@Y%@X�`@X��@X�@W��@W;d@W�@W
=@V��@V��@V�+@U@UO�@U/@T�@T�@Tj@TI�@T9X@T(�@T�@Sƨ@SC�@So@R�@R��@R��@R�!@R�\@RM�@Q�@Qx�@Q7L@P��@P�@PA�@P  @O��@O�P@Ol�@O;d@N�R@Nff@NE�@N{@M�T@M@M�h@M�@M?}@L��@L�@L9X@K�m@Ko@J~�@JM�@J=q@J�@I�#@IX@I&�@I�@I�@I%@H��@H��@HbN@Hb@G�;@G�w@G�P@Gl�@G
=@F��@F�@F�R@F��@F5?@E@E�h@E�@E�@Ep�@E`B@E?}@D�@DZ@D(�@D1@C��@C�
@Cƨ@C��@C��@CS�@B��@B�\@B=q@BJ@A��@A��@Ahs@A&�@@Ĝ@@r�@@A�@@ �@?�;@?|�@?\)@?K�@?+@?�@?
=@>�R@>V@>{@=�T@=��@=p�@<�/@<�@<�D@<(�@;�m@;��@;dZ@:�@:~�@:-@9�^@9�^@9hs@9%@8�u@8A�@7�@7;d@6�@6�+@6@5�-@5p�@5?}@5V@4��@4�@4z�@4(�@3�m@3��@3�@3o@2��@2��@2M�@2J@1��@1�7@17L@0��@0�@0  @/�@/|�@/\)@/;d@/
=@.�@.ȴ@.��@.v�@.v�@.v�@.ff@.ff@.ff@.ff@.ff@.ff@.5?@.@-��@-�@-O�@-/@,�@,�j@,(�@,1@,1@+�
@+��@+C�@*�@*��@*M�@*=q@*=q@*-@*�@*�@*J@)�@)��@)X@)7L@)%@(��@(�9@(�@(Q�@'�;@'��@'��@'|�@'|�@'l�@'K�@'
=@&�R@&ff@%�@%��@%�-@%�@%`B@%/@$�@$�D@$(�@$1@#�
@#��@#dZ@#33@#33@#o@"�@"��@"�!@"~�@"^5@"-@!��@!��@!X@!G�@!G�@!%@ �9@ �@ bN@ bN@ bN@ Q�@  �@��@;d@��@�R@��@��@��@��@�+@ff@�T@�-@��@O�@V@�/@�j@�@Z@�@�
@�F@t�@o@~�@�@�^@��@hs@�@�9@Q�@Q�@Q�@  @�;@��@�w@�@�@��@|�@K�@�@�@�R@��@v�@E�@{@@`B@��@�/@��@�@I�@��@�m@�
@t�@o@�H@�\@^5@=q@�@��@&�@�@%@��@��@�9@r�@Q�@ �@�@�P@K�@�y@��@�+@5?@{@@@p�@O�@�@�/@�@z�@I�@9X@�@1@�F@�@dZ@C�@
��@
�\@
n�@
^5@
M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�bB	�hB	�hB	�\B	�bB	�bB	�bB	�\B	�bB	�bB	�bB	�\B	�\B	�\B	�bB	�bB	�\B	�bB	�bB	�oB	�uB	��B	�^B	��B	�B
�B
^5B
��B
B
�B-BO�BdZBm�Bq�B|�B��B��B�ZB�BB\BoB�B�B�B$�B+B:^B@�BH�BS�BYBaHBl�Bw�B|�B}�B� B~�B� B~�B� B�B�%B�PB�hB�PB�B�PB��B��B��B|�BB�^B��B�B�DB�B�B$�B�BJB
=B��B�B�B�HB��B�-B��B�JB�B~�Bv�Bo�B`BBO�BK�BF�BB�B6FB�BB
�)B
��B
�qB
�!B
��B
�B
m�B
`BB
J�B
9XB
$�B
hB
B	��B	�B	�mB	�#B	��B	�B	o�B	dZB	N�B	A�B	7LB	0!B	%�B	uB		7B�B�B�B��B�
B��B�wB�RB�XB�wB�dB�?B�9B�9B�FB�9B�B�B��B��B��B��B�oB�\B�DB�7B�B~�Bw�Br�Bm�BiyBcTB^5B[#BZB]/BZBYBW
BW
BW
BYBW
BR�BP�BM�BI�BE�BD�BE�BE�BC�BA�B?}B?}B?}B@�B>wB=qB<jB<jB;dB9XB6FB6FB33B33B1'B.B,B,B+B,B)�B(�B)�B+B)�B)�B)�B(�B(�B'�B'�B'�B%�B%�B"�B�B �B �B�B �B�B�B�B �B �B"�B!�B �B �B �B#�B"�B!�B"�B%�B$�B$�B&�B(�B(�B(�B(�B(�B)�B,B-B/B2-B2-B2-B2-B2-B33B5?B5?B7LB7LB8RB9XB;dB>wB?}B?}B@�BC�BE�BG�BL�BP�BS�BT�BW
BW
BXB\)B]/B]/B_;BcTBgmBhsBiyBjBl�Bo�Bt�Bv�By�B{�B~�B�B�%B�DB�JB�VB�\B�hB�uB��B��B��B��B��B��B��B��B��B��B�B�!B�?B�FB�RB�dB�jB�}BÖBĜBĜBƨB��B��B��B�
B�B�B�B�)B�;B�HB�TB�mB�yB�B�B�B��B��B��B��B	B	B	%B	+B	
=B	VB	\B	hB	uB	�B	�B	�B	�B	�B	�B	!�B	#�B	&�B	(�B	)�B	-B	/B	1'B	33B	5?B	7LB	9XB	:^B	<jB	>wB	@�B	A�B	D�B	H�B	I�B	J�B	L�B	M�B	Q�B	T�B	XB	ZB	[#B	\)B	`BB	bNB	dZB	ffB	iyB	m�B	p�B	q�B	r�B	t�B	u�B	w�B	x�B	z�B	y�B	y�B	{�B	}�B	� B	�B	�B	�B	�1B	�7B	�1B	�7B	�JB	�JB	�VB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�?B	�FB	�LB	�XB	�^B	�^B	�^B	�^B	�^B	�jB	�}B	��B	��B	��B	ÖB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
1B
	7B

=B

=B
DB
DB
DB
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
\B
bB
hB
hB
hB
hB
hB
oB
oB
uB
uB
{B
{B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
-B
,B
-B
-B
-B
-B
-B
-B
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
0!B
0!B
0!B
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
33B
33B
33B
33B
33B
33B
49B
49B
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
7LB
7LB
7LB
7LB
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
:^B
:^B
:^B
:^B
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
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
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
E�B
F�B
F�B
F�B
F�B
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
I�B
I�B
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
M�B
N�B
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
W
B
XB
XB
XB
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
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
]/B
]/B
^5B
^5B
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
_;B
_;B
_;B
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
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�HB	�NB	�NB	�BB	�HB	�HB	�HB	�BB	�HB	�HB	�HB	�BB	�BB	�BB	�HB	�HB	�BB	�HB	�HB	�TB	�[B	��B	�DB	οB	�B
sB
^B
�mB
�uB
�eB,�BO�Bd@BmwBq�B|�B��B�iB�@B�B�BBBTBsB�B�B$�B*�B:*B@iBH�BS�BX�Ba-BlqBw�B|�B}�B�B~�B�B~�B�B��B�B�6B�NB�6B��B�6B��B��B��B|�B�B�DB�yB��B�B�wBgB$�B�B0B
#B��B�B�eB�-B��B�B��B�B��B~�Bv�BoiB`BO�BK�BF�BBuB6BB�B
��B
ʦB
�<B
�B
�gB
��B
mwB
`'B
J�B
9$B
$�B
NB
B	��B	�oB	�RB	��B	�oB	��B	oiB	d@B	N�B	AoB	72B	/�B	%�B	@B		B��B�eB��B��B��BϫB�]B�8B�>B�BB�JB�%B�B�B�+B�B��B��B��B��B�yB�MB�TB�(B�)B�B�B~�Bw�Br|BmwBi_Bc:B^B[	BY�B\�BY�BX�BV�BV�BV�BX�BV�BR�BP�BM�BI�BEmBDgBEmBE�BC{BAUB?HB?cB?HB@OB>BB=<B<PB<6B;JB9$B6B6B2�B2�B0�B-�B+�B+�B*�B+�B)�B(�B)�B*�B)�B)�B)�B(�B(�B'�B'�B'�B%�B%�B"�B�B �B �B�B �B�B�B�B �B �B"�B!�B �B �B �B#�B"�B!�B"�B%�B$�B$�B&�B(�B(�B(�B(�B(�B)�B+�B,�B/ B2B2B1�B1�B1�B2�B5B5B7B7B8B9$B;0B>BB?HB?HB@OBC{BE�BG�BL�BP�BS�BT�BV�BV�BW�B[�B\�B]B_Bc:BgRBhXBiDBjeBlqBo�Bt�Bv�By�B{�B~�B��B�B�B�B�<B�BB�4B�@B�SB�B�xB��B��B��B��B��B��B��B��B��B�B�B�B�JB�PB�HB�aB�gB�gBƎB˒BΥB��B��B��B��B��B��B�!B�B�:B�8B�_B�qB�B�vB��B��B��B��B	�B	�B	B	�B	
	B	"B	(B	4B	@B	SB	_B	kB	qB	~B	�B	!�B	#�B	&�B	(�B	)�B	,�B	.�B	0�B	3B	5B	7B	9>B	:*B	<6B	>BB	@iB	AUB	D�B	H�B	I�B	J�B	L�B	M�B	Q�B	T�B	W�B	Y�B	Z�B	\B	`B	bB	d@B	f2B	iDB	m]B	poB	q�B	r|B	t�B	u�B	w�B	x�B	z�B	y�B	y�B	{�B	}�B	�B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�"B	�.B	�HB	�4B	�[B	�gB	�SB	�eB	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�+B	�2B	�$B	�*B	�*B	�*B	�DB	�*B	�6B	�HB	�UB	�UB	�oB	�aB	�tB	�tB	ǔB	ȀB	ʌB	˒B	̘B	ΥB	��B	��B	бB	бB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�'B	�B	�B	�:B	�:B	�&B	�&B	�,B	�2B	�2B	�>B	�>B	�XB	�>B	�DB	�DB	�KB	�kB	�qB	�]B	�cB	�cB	�iB	��B	��B	�vB	�vB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
B
�B
B
B
	B

#B

	B
B
B
)B
B
B
B
B
B
6B
6B
"B
"B
"B
(B
.B
4B
NB
4B
4B
4B
TB
:B
[B
@B
aB
aB
MB
SB
SB
mB
YB
YB
sB
_B
_B
eB
eB
kB
�B
kB
�B
qB
qB
qB
~B
~B
�B
~B
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
!�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
/ B
.�B
.�B
/ B
.�B
/ B
/�B
/�B
0B
/�B
0B
/�B
0B
0�B
1B
1B
2B
2B
1�B
1�B
2B
1�B
3B
2�B
2�B
2�B
2�B
2�B
4B
4B
4B
4B
4B
4B
5B
5%B
5B
5B
6B
6+B
6B
6B
7B
7B
72B
7B
8B
8B
8B
8B
9$B
9>B
9>B
9$B
9>B
:DB
:*B
:*B
:DB
:*B
:DB
:*B
:DB
;0B
;0B
;0B
;0B
<6B
<6B
<6B
=<B
=<B
=<B
=<B
>]B
>]B
?HB
?HB
?cB
?HB
@OB
@OB
@OB
AUB
AUB
B[B
BuB
BuB
B[B
C{B
C{B
C{B
CaB
C{B
C{B
DgB
D�B
DgB
D�B
D�B
D�B
EmB
EmB
E�B
EmB
FtB
FtB
FtB
FtB
EmB
FtB
FtB
F�B
FtB
FtB
FtB
GzB
GzB
G�B
GzB
GzB
G�B
GzB
GzB
GzB
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
I�B
I�B
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
M�B
N�B
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
T�B
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
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
Y�B
ZB
ZB
Y�B
Y�B
Z�B
Z�B
\B
\B
[�B
[�B
\�B
]B
^B
]B
]B
^B
^B
^B
^B
^B
^B
^B
^B
^B
_B
_!B
_B
_!B
_!B
_!B
_B
`B
`B
aB
a-B
aB
a-B
aB
a-B
aB
aB
bB
bB
c B
c B
c:B
c B
d&B
d@B
d@B
e,B
eFB
e,B
eFB
e,B
e,B
e,B
f2B
f2B
fLB
f2B
g8B
g8B
gRB
h>B
h>B
h>B
hXB
h>B
hXB
iDB
iDB
iDB
iDB
jKB
jKB
jKB
jKB
jeB
jeB
jKB
jKB
kQB
kQB
kQB
lWB
lW111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.35(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903180033522019031800335220190318003352201903190025112019031900251120190319002511JA  ARFMdecpA19c                                                                20190313123654  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190313033711  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190313033712  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190313033713  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190313033713  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190313033713  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190313033713  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190313033713  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190313033714  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190313033714                      G�O�G�O�G�O�                JA  ARUP                                                                        20190313035648                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190313153603  CV  JULD            G�O�G�O�F�r                JM  ARCAJMQC2.0                                                                 20190317153352  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190317153352  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190318152511  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                