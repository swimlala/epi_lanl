CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-27T15:36:03Z creation;2018-10-27T15:36:06Z conversion to V3.1;2019-12-23T06:12:59Z update;     
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
resolution        =���   axis      Z        P  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  s   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �,   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �P   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     P  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181027153603  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               cA   JA  I2_0675_099                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @،t��1   @،-�� @6�u%F
��cWᰉ�'1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu�fDv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�fD�)�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�33A��A%��AD  Ae��A���A���A���A���A���A���A���A���BffB	ffBffBffB!ffB)ffB1ffB9ffBAffBIffBQffBYffBaffBiffBqffBy��B��fB��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3B��3Bĳ3Bȳ3B̳3Bг3BԳ3Bس3Bܳ3B�3B�3B�3B�3B�3B��3B��3B��3C Y�CY�CY�CY�CY�C
Y�CY�CY�CY�CY�CY�CY�CY�CY�CY�CY�C Y�C"Y�C$Y�C&Y�C(Y�C*Y�C,Y�C.Y�C0Y�C2Y�C4Y�C6Y�C8Y�C:Y�C<Y�C>Y�C@Y�CBY�CDs3CFY�CHY�CJY�CLY�CNY�CPY�CRY�CTY�CVY�CXY�CZY�C\Y�C^Y�C`Y�CbY�CdY�CfY�ChY�CjY�ClY�CnY�CpY�CrY�CtY�CvY�CxY�CzY�C|Y�C~Y�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�9�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�9�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�  C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�C�,�D fD �fD D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��D	fD	�fD
fD
�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fD fD �fD!fD!�fD"fD"�fD#fD#�fD$fD$�fD%fD%�fD&fD&�fD'fD'�fD(fD(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-�fD.fD.�fD/fD/�fD0fD0�fD1fD1�fD2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<�fD=fD=�fD>fD>�fD?fD?�fD@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF�fDGfDG�fDHfDH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL�fDMfDM�fDNfDN�fDOfDO�fDPfDP�fDQfDQ�fDRfDR�fDSfDS�fDTfDT�fDUfDU�fDVfDV�fDWfDW�fDXfDX�fDYfDY�fDZfDZ�fD[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_�fD`fD`�fDafDa�fDbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDmfDm�fDnfDn�fDo�Do�fDpfDp�fDqfDq�fDrfDr�fDsfDs�fDtfDt�fDu�Du��DvfDv�fDwfDw�fDxfDx�fDyfDy�fDzfDz�fD{fD{�fD|fD|�fD}fD}�fD~fD~�fDfD�fD�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�NfD��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�fD�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D��3D��3D�3D�K3D3D��3D�3D�K3DË3D��3D�3D�K3Dċ3D��3D�3D�K3Dŋ3D��3D�3D�K3DƋ3D��3D�3D�K3Dǋ3D��3D�3D�K3Dȋ3D��3D�3D�K3Dɋ3D��3D�3D�K3Dʋ3D��3D�3D�K3Dˋ3D��3D�3D�K3D̋3D��3D�3D�K3D͋3D��3D�3D�K3D΋3D��3D�3D�K3Dϋ3D��3D�3D�K3DЋ3D��3D�3D�K3Dы3D��3D�3D�K3Dҋ3D��3D�3D�K3DӋ3D��3D�3D�K3Dԋ3D��3D�3D�K3DՋ3D��3D�3D�K3D֋3D��3D�3D�K3D׋3D��3D�3D�K3D؋3D��3D�3D�K3Dً3D��3D�3D�K3Dڋ3D��fD�3D�K3Dۋ3D��3D�3D�K3D܋3D��3D�3D�K3D݋3D��3D�3D�K3Dދ3D��3D�3D�K3Dߋ3D��3D�3D�K3D��3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D��3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��3D�3D�K3D�3D��fD��D�4�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�+A�9XA�7LA�9XA�33A�33A�7LA�=qA�A�A�A�A�C�A�?}A�5?A�+A�1A���A��#Aв-A�l�A�9XA�(�A�VA�A���A��A�ȴAϗ�AϋDA�|�A�$�A�ZA�ƨAǡ�A��A��A���A���A�7LA���A��A��A��A�~�A��A�oA��+A���A�
=A�A�1'A��A���A��yA��A��uA��A��7A�C�A��;A�^5A�"�A�"�A���A��+A�Q�A��mA��/A��wA�t�A�|�A���A��9A�dZA�K�A�A�ȴA�JA���A�l�A�+A���A�ȴA�(�A���A�x�A���A��hA�A��A��A��yA� �A���A���A�?}A��uA�S�A�VA�jA��A�A�&�A��+A�A�  A�?}A�l�A�oA���A�|�A�bA���A�ZA�(�A�A}�A}�#A|A�Ay\)AvZAt��Asl�Ap1Am��Ak
=Ah�+Ag�AfE�Ae��Ae�wAd��Abn�A`�A]`BAZVAYO�AU�ARbNAO��AN5?AMt�AM�AL�HALA�AI��AH��AF�jAEVADbNABbNAA�AA��A@{A=
=A;C�A9��A9K�A7�TA6�jA5�A5%A4�`A4jA3�;A3��A3�A2��A1ƨA0�!A/�A/"�A.�A-x�A,9XA+l�A*��A*E�A*�!A(��A'XA$��A$(�A#&�A �jA��A$�A��A��A�-AVA�/A�TA��A��A��AK�Av�AA;dA �A�+A�yAJA
�\A	oA�AE�A;dA��Az�A1AA�A��A��A
=A n�A 1@�;d@��@���@���@�o@�E�@�X@�Ĝ@�+@�h@�dZ@�-@�Q�@�+@�~�@�-@�-@���@�\)@�hs@�Ĝ@���@◍@��@߮@ޏ\@�V@��@ۅ@���@�33@�5?@�%@Ӿw@�+@Ѻ^@мj@�ƨ@Ώ\@�M�@�x�@� �@˅@�-@�z�@�9X@�33@ư!@�=q@��@ř�@��@�Q�@�S�@���@���@�=q@�1@���@��@���@��@��R@�=q@��T@��`@���@�-@���@��@���@�ȴ@�Q�@�dZ@�ȴ@�^5@�%@�A�@��
@�o@�~�@��^@��u@���@��D@��@�"�@�
=@���@�ȴ@���@��+@�5?@��@��T@��/@��@�bN@�9X@��
@��@���@���@�l�@�dZ@�\)@�+@�o@��@��!@�J@���@��7@���@�Q�@��F@��P@�t�@��;@��@�1@��@��@��;@��w@��@�C�@���@��@��@��+@��T@�@���@�x�@�`B@�?}@�/@�&�@�&�@���@���@�Q�@��F@��@��+@�M�@�{@��@��h@�&�@��`@��D@�z�@�A�@��m@�\)@��@��@��R@�v�@���@���@�x�@��-@���@���@�@��@�G�@�&�@��`@���@�1@�ƨ@�C�@��H@��\@�v�@�$�@���@���@�@���@�G�@��@���@�bN@�  @��
@��@��m@��m@���@��@���@�\)@�33@�@�
=@�"�@��@�@�@���@��@��@�X@�hs@�p�@�x�@�x�@��/@�j@�b@��
@��F@��@��@�|�@�t�@�o@�@��@�ȴ@���@�n�@�5?@���@���@���@�&�@��@��@���@���@�r�@�j@�Z@�9X@���@�S�@��H@��!@���@���@��\@�~�@�^5@�E�@�-@�J@��T@�`B@�7L@�/@��@��`@��@��u@��D@�r�@�bN@�A�@���@���@��@���@�t�@�;d@��@���@��@�J@��@���@�x�@�/@�&�@��@�V@���@���@��`@��@�j@�Q�@�(�@�@��@�@~ȴ@~��@~v�@~ff@~V@}�T@}�h@}`B@}/@}V@|�@|I�@{�F@{C�@z~�@z^5@z^5@z^5@zM�@zJ@y��@y�^@yx�@x�`@xb@x  @w��@wK�@w
=@v�+@v{@u�-@uO�@t�@t��@s��@sC�@r�H@r��@rn�@rJ@q��@qx�@p��@pQ�@p1'@o|�@n�R@nV@m�T@mO�@l�/@l�D@k�m@j�H@jJ@i��@ihs@iG�@h�u@g��@g�@f��@fff@e��@eV@dz�@dZ@d1@c��@cdZ@c33@b�@b�@a�#@a�#@a��@a�7@ax�@ahs@aX@a&�@`Ĝ@`�9@`�u@`Q�@`b@_�P@_+@^��@^v�@]�T@]p�@]?}@]/@\�@\9X@\1@[�
@[dZ@Z��@Z=q@Z�@Y��@Y��@Y�7@YX@X��@XbN@X �@W�;@W�P@Wl�@W;d@W+@W
=@V�R@VV@U�@U�h@Up�@U/@UV@T�j@Tz�@Tj@Tj@S��@S�F@S�F@S��@S@R~�@Q�#@P��@PbN@Pb@O�P@O;d@N��@N�+@N{@M�@M�T@M�h@M`B@MO�@M/@M�@M�@L�@Lz�@LI�@L1@K��@K"�@J�\@J=q@I�@Ihs@I&�@H�9@Hr�@HA�@G�@G\)@G+@G
=@F�@F��@F�+@Fv�@FV@F5?@F$�@F{@F@E�h@EV@Dj@D�@D�@C��@C��@C33@B��@B��@B~�@B�@A�^@A��@AG�@A%@@��@@Q�@@ �@?;d@>��@>�+@>V@>{@>@=��@=p�@<��@<�D@<9X@<�@;��@;dZ@:�!@:n�@:M�@:�@9��@9��@9�7@9G�@9�@8��@8��@8r�@8  @7�@7�P@7\)@7;d@6��@6�y@6��@6ff@5�@5@5�h@5/@5V@4�@4I�@3�m@3�@3C�@3"�@2�H@2~�@2J@1��@1X@1G�@17L@1%@0�@0b@/��@/��@/\)@/+@/
=@.�y@.ȴ@.�R@.��@.��@.5?@.{@-�@-�T@-��@-�@-?}@,�@,�/@,��@,�D@,I�@+�m@+��@+dZ@+@*��@*��@*^5@*�@)�@)��@)hs@)G�@)7L@)�@(��@(�`@(�u@(A�@(1'@'�@'��@'�w@'�@'
=@&�R@&��@&�+@&ff@&$�@%�T@%@%�h@%`B@%O�@%O�@%V@$��@$z�@$9X@$1@#�
@#��@#S�@#C�@#"�@"�@"�H@"��@"��@"^5@!��@!�7@!G�@ ��@ ��@ �@  �@  �@��@l�@K�@ȴ@ff@V@5?@$�@@�@@p�@V@��@�@��@(�@�@�m@�@"�@@��@��@~�@=q@��@��@x�@G�@&�@�@%@��@�9@�@1'@ �@�@��@�w@�@|�@;d@;d@
=@�@ȴ@�R@�R@��@�+@v�@v�@��@��@V@�T@�h@?}@�@��@�/@�@z�@(�@��@�@33@�H@��@�\@�\@n�@n�@M�@J@�@��@�^@��@x�@hs@X@7L@��@��@��@�@A�@  @  @�;@��@��@�y@�y@�y@��@v�@E�@5?@{@�T@`B@�@/@�@�@��@�@�D@j@I�@9X@�@�
@��@dZ@"�@o@
�!@
��@
^5@	��@	��@	x�@	X@	&�@	�@	%@�`@Ĝ@��@bN@  @�@�;@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�+A�9XA�7LA�9XA�33A�33A�7LA�=qA�A�A�A�A�C�A�?}A�5?A�+A�1A���A��#Aв-A�l�A�9XA�(�A�VA�A���A��A�ȴAϗ�AϋDA�|�A�$�A�ZA�ƨAǡ�A��A��A���A���A�7LA���A��A��A��A�~�A��A�oA��+A���A�
=A�A�1'A��A���A��yA��A��uA��A��7A�C�A��;A�^5A�"�A�"�A���A��+A�Q�A��mA��/A��wA�t�A�|�A���A��9A�dZA�K�A�A�ȴA�JA���A�l�A�+A���A�ȴA�(�A���A�x�A���A��hA�A��A��A��yA� �A���A���A�?}A��uA�S�A�VA�jA��A�A�&�A��+A�A�  A�?}A�l�A�oA���A�|�A�bA���A�ZA�(�A�A}�A}�#A|A�Ay\)AvZAt��Asl�Ap1Am��Ak
=Ah�+Ag�AfE�Ae��Ae�wAd��Abn�A`�A]`BAZVAYO�AU�ARbNAO��AN5?AMt�AM�AL�HALA�AI��AH��AF�jAEVADbNABbNAA�AA��A@{A=
=A;C�A9��A9K�A7�TA6�jA5�A5%A4�`A4jA3�;A3��A3�A2��A1ƨA0�!A/�A/"�A.�A-x�A,9XA+l�A*��A*E�A*�!A(��A'XA$��A$(�A#&�A �jA��A$�A��A��A�-AVA�/A�TA��A��A��AK�Av�AA;dA �A�+A�yAJA
�\A	oA�AE�A;dA��Az�A1AA�A��A��A
=A n�A 1@�;d@��@���@���@�o@�E�@�X@�Ĝ@�+@�h@�dZ@�-@�Q�@�+@�~�@�-@�-@���@�\)@�hs@�Ĝ@���@◍@��@߮@ޏ\@�V@��@ۅ@���@�33@�5?@�%@Ӿw@�+@Ѻ^@мj@�ƨ@Ώ\@�M�@�x�@� �@˅@�-@�z�@�9X@�33@ư!@�=q@��@ř�@��@�Q�@�S�@���@���@�=q@�1@���@��@���@��@��R@�=q@��T@��`@���@�-@���@��@���@�ȴ@�Q�@�dZ@�ȴ@�^5@�%@�A�@��
@�o@�~�@��^@��u@���@��D@��@�"�@�
=@���@�ȴ@���@��+@�5?@��@��T@��/@��@�bN@�9X@��
@��@���@���@�l�@�dZ@�\)@�+@�o@��@��!@�J@���@��7@���@�Q�@��F@��P@�t�@��;@��@�1@��@��@��;@��w@��@�C�@���@��@��@��+@��T@�@���@�x�@�`B@�?}@�/@�&�@�&�@���@���@�Q�@��F@��@��+@�M�@�{@��@��h@�&�@��`@��D@�z�@�A�@��m@�\)@��@��@��R@�v�@���@���@�x�@��-@���@���@�@��@�G�@�&�@��`@���@�1@�ƨ@�C�@��H@��\@�v�@�$�@���@���@�@���@�G�@��@���@�bN@�  @��
@��@��m@��m@���@��@���@�\)@�33@�@�
=@�"�@��@�@�@���@��@��@�X@�hs@�p�@�x�@�x�@��/@�j@�b@��
@��F@��@��@�|�@�t�@�o@�@��@�ȴ@���@�n�@�5?@���@���@���@�&�@��@��@���@���@�r�@�j@�Z@�9X@���@�S�@��H@��!@���@���@��\@�~�@�^5@�E�@�-@�J@��T@�`B@�7L@�/@��@��`@��@��u@��D@�r�@�bN@�A�@���@���@��@���@�t�@�;d@��@���@��@�J@��@���@�x�@�/@�&�@��@�V@���@���@��`@��@�j@�Q�@�(�@�@��@�@~ȴ@~��@~v�@~ff@~V@}�T@}�h@}`B@}/@}V@|�@|I�@{�F@{C�@z~�@z^5@z^5@z^5@zM�@zJ@y��@y�^@yx�@x�`@xb@x  @w��@wK�@w
=@v�+@v{@u�-@uO�@t�@t��@s��@sC�@r�H@r��@rn�@rJ@q��@qx�@p��@pQ�@p1'@o|�@n�R@nV@m�T@mO�@l�/@l�D@k�m@j�H@jJ@i��@ihs@iG�@h�u@g��@g�@f��@fff@e��@eV@dz�@dZ@d1@c��@cdZ@c33@b�@b�@a�#@a�#@a��@a�7@ax�@ahs@aX@a&�@`Ĝ@`�9@`�u@`Q�@`b@_�P@_+@^��@^v�@]�T@]p�@]?}@]/@\�@\9X@\1@[�
@[dZ@Z��@Z=q@Z�@Y��@Y��@Y�7@YX@X��@XbN@X �@W�;@W�P@Wl�@W;d@W+@W
=@V�R@VV@U�@U�h@Up�@U/@UV@T�j@Tz�@Tj@Tj@S��@S�F@S�F@S��@S@R~�@Q�#@P��@PbN@Pb@O�P@O;d@N��@N�+@N{@M�@M�T@M�h@M`B@MO�@M/@M�@M�@L�@Lz�@LI�@L1@K��@K"�@J�\@J=q@I�@Ihs@I&�@H�9@Hr�@HA�@G�@G\)@G+@G
=@F�@F��@F�+@Fv�@FV@F5?@F$�@F{@F@E�h@EV@Dj@D�@D�@C��@C��@C33@B��@B��@B~�@B�@A�^@A��@AG�@A%@@��@@Q�@@ �@?;d@>��@>�+@>V@>{@>@=��@=p�@<��@<�D@<9X@<�@;��@;dZ@:�!@:n�@:M�@:�@9��@9��@9�7@9G�@9�@8��@8��@8r�@8  @7�@7�P@7\)@7;d@6��@6�y@6��@6ff@5�@5@5�h@5/@5V@4�@4I�@3�m@3�@3C�@3"�@2�H@2~�@2J@1��@1X@1G�@17L@1%@0�@0b@/��@/��@/\)@/+@/
=@.�y@.ȴ@.�R@.��@.��@.5?@.{@-�@-�T@-��@-�@-?}@,�@,�/@,��@,�D@,I�@+�m@+��@+dZ@+@*��@*��@*^5@*�@)�@)��@)hs@)G�@)7L@)�@(��@(�`@(�u@(A�@(1'@'�@'��@'�w@'�@'
=@&�R@&��@&�+@&ff@&$�@%�T@%@%�h@%`B@%O�@%O�@%V@$��@$z�@$9X@$1@#�
@#��@#S�@#C�@#"�@"�@"�H@"��@"��@"^5@!��@!�7@!G�@ ��@ ��@ �@  �@  �@��@l�@K�@ȴ@ff@V@5?@$�@@�@@p�@V@��@�@��@(�@�@�m@�@"�@@��@��@~�@=q@��@��@x�@G�@&�@�@%@��@�9@�@1'@ �@�@��@�w@�@|�@;d@;d@
=@�@ȴ@�R@�R@��@�+@v�@v�@��@��@V@�T@�h@?}@�@��@�/@�@z�@(�@��@�@33@�H@��@�\@�\@n�@n�@M�@J@�@��@�^@��@x�@hs@X@7L@��@��@��@�@A�@  @  @�;@��@��@�y@�y@�y@��@v�@E�@5?@{@�T@`B@�@/@�@�@��@�@�D@j@I�@9X@�@�
@��@dZ@"�@o@
�!@
��@
^5@	��@	��@	x�@	X@	&�@	�@	%@�`@Ĝ@��@bN@  @�@�;@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B`BB_;B_;B_;B_;B_;B_;B`BB`BB`BBaHBbNBdZBffBk�Bm�Bq�Br�Bp�Bn�Bn�Bm�Bm�Bn�Bo�Bm�BjBk�Bk�BjBl�Bu�B�+B��B��B��B�'B�!B��B��B��B�DB�7B�+B�=B�=B�oB�{B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�7Br�BXB@�B1'B(�B\B��B��B��B�B�B�B�B�B�fB�/B�B��B��B�wB�LB��B��B�{B�7B~�Bs�B`BBR�BL�BH�B?}B2-B&�B�BhBDB
��B
�B
�BB
��B
�^B
�!B
��B
��B
��B
�JB
� B
k�B
hsB
_;B
I�B
33B
!�B
�B	��B	�HB	��B	�9B	��B	��B	��B	��B	��B	�B	y�B	e`B	G�B	;dB	 �B	B�B�HB�#B�B�)B�/BǮB�jB�?B�B��B��B�RB��B�jB��B�{B�1B�B~�B� B{�B{�B�B�1B�B�B� B� B{�Bq�Bn�Bl�Bt�Bq�BjBe`Be`Bo�B�+B�B}�Bv�Br�Bn�BgmB`BB]/BW
BW
BO�BJ�BD�BB�BA�B?}B@�B@�B@�B=qB=qB<jB8RB8RB5?B33B0!B-B+B(�B&�B&�B%�B%�B$�B%�B$�B"�B �B�B�B �B�B�B �B!�B �B"�B#�B"�B!�B$�B&�B%�B%�B%�B%�B%�B&�B&�B%�B'�B&�B(�B(�B(�B(�B(�B(�B)�B)�B(�B+B+B,B0!B0!B0!B2-B1'B2-B49B5?B8RB;dB;dB>wB?}BA�BA�BA�BC�BE�BH�BK�BQ�BQ�BVBXBZB]/B`BBbNBdZBgmBjBiyBp�Bu�Bu�Bu�Bx�B|�B�B�B�B�JB�bB�oB��B��B��B��B��B��B��B�B�'B�'B�-B�-B�3B�?B�FB�LB�qB�wB�wB��BÖBƨBƨBƨB��B��B��B��B��B�B�#B�;B�TB�yB�B�B��B��B��B	1B	JB	bB	oB	uB	{B	�B	�B	%�B	(�B	)�B	)�B	1'B	6FB	7LB	8RB	9XB	9XB	:^B	:^B	:^B	;dB	;dB	=qB	?}B	B�B	E�B	F�B	H�B	K�B	M�B	Q�B	T�B	VB	XB	XB	ZB	\)B	aHB	dZB	dZB	gmB	iyB	k�B	l�B	o�B	s�B	t�B	u�B	z�B	}�B	�B	�B	�%B	�+B	�+B	�1B	�7B	�=B	�DB	�PB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�FB	�LB	�RB	�RB	�RB	�XB	�dB	�jB	�qB	�}B	�}B	��B	��B	ŢB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�5B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
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
+B
1B
	7B

=B
DB
DB
DB
JB
PB
VB
VB
VB
\B
bB
hB
oB
oB
uB
{B
�B
{B
{B
�B
�B
�B
�B
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
!�B
"�B
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
$�B
%�B
%�B
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
-B
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
0!B
1'B
1'B
1'B
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
33B
33B
33B
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
9XB
9XB
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
>wB
>wB
>wB
?}B
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
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
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
H�B
H�B
I�B
H�B
I�B
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
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
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
P�B
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
P�B
Q�B
Q�B
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
YB
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
[#B
[#B
[#B
[#B
\)B
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
dZB
dZB
dZB
dZB
dZB
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
gmB
gmB
gmB
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
iyB
jB
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
k�B
l�B
l�B
l�B
l�B
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
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B`'B_!B_!B_!B_!B_!B_!B`'B`'B`'Ba-Bb4Bd@BfLBkkBmwBq�Br�Bp�Bn}Bn}BmwBmwBn}Bo�BmwBjeBkkBkkBjeBlqBu�B�B��B��B��B�B�B��B��B�gB�)B�B�B�#B�	B�TB�aB�NB�yB�yB�yB��B��B��B��B��B��B��B�B�sB��B��B��B��B�B�Br�BW�B@iB1B(�BBB��B��B��B�]B�kB�}B�eB�KB�2B�B��B��B͹B�BB�2B��B�_B�FB�B~�Bs�B`'BR�BL�BH�B?cB1�B&�BxBNB)B
��B
�wB
�B
ˬB
�*B
��B
��B
��B
��B
�B
�B
kkB
hXB
_!B
I�B
2�B
!�B
mB	��B	�B	ʦB	�B	��B	�qB	�eB	��B	�YB	��B	y�B	e,B	G�B	;JB	 �B	�B�wB�-B�	B��B�B��B�zB�6B�B��B��B��B�8B�iB�PB��B�aB��B��B~�B�B{�B{�B��B�B��B��B�B�B{�Bq�BncBlqBt�BqvBjKBeFBeFBoiB��B��B}�Bv�Br|BncBgRB`'B]BV�BV�BO�BJ�BDgBB[BAUB?cB@iB@OB@OB=<B=<B<PB8B8B5%B2�B/�B,�B*�B(�B&�B&�B%�B%�B$�B%�B$�B"�B �B�B�B �B�B�B �B!�B �B"�B#�B"�B!�B$�B&�B%�B%�B%�B%�B%�B&�B&�B%�B'�B&�B(�B(�B(�B(�B(�B(�B)�B)�B(�B*�B*�B+�B/�B/�B/�B2B0�B1�B4B5B8B;0B;0B>BB?HBAUBAUBAoBCaBEmBH�BK�BQ�BQ�BU�BW�BY�B\�B`BbBd&Bg8BjKBiDBpoBu�Bu�Bu�Bx�B|�B��B��B��B�B�.B�:B�gB�YB��B��B��B��B��B� B��B��B��B��B��B�%B�B�2B�VB�BB�BB�OB�{B�tB�tBƎBˬB˒B̘B��BҽB��B��B�B� B�_B�WB�vB��B��B��B	�B	0B	.B	:B	@B	FB	gB	qB	%�B	(�B	)�B	)�B	1B	6B	7B	88B	9>B	9>B	:*B	:*B	:*B	;0B	;0B	=<B	?HB	B[B	E�B	FtB	H�B	K�B	M�B	Q�B	T�B	U�B	W�B	W�B	Y�B	[�B	aB	d&B	d&B	g8B	iDB	kQB	lqB	oiB	s�B	t�B	u�B	z�B	}�B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�(B	�NB	�:B	�SB	�eB	�eB	�_B	�eB	�kB	�eB	�kB	�qB	��B	�xB	�xB	�xB	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	�B	�B	�B	�8B	�$B	�0B	�PB	�VB	�cB	�HB	�OB	�UB	ňB	�tB	ɆB	ɆB	ʌB	ʌB	ʌB	͟B	ΥB	οB	͟B	οB	ѷB	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	�	B	�B	�B	�B	�B	�B	�B	�-B	�4B	�B	�B	�B	� B	�&B	�@B	�,B	�,B	�,B	�LB	�8B	�>B	�KB	�KB	�KB	�QB	�qB	�]B	�]B	�wB	�]B	�]B	�]B	�]B	�cB	�iB	�iB	�oB	�vB	�vB	�B	�|B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
B
	B

	B
)B
B
B
B
B
"B
"B
"B
(B
HB
4B
:B
:B
@B
FB
MB
FB
FB
MB
gB
gB
gB
YB
sB
YB
_B
_B
_B
yB
_B
eB
eB
eB
eB
�B
�B
qB
�B
xB
xB
�B
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
 �B
!�B
!�B
!�B
"�B
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
$�B
%�B
%�B
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
)�B
)�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
/ B
/�B
/�B
/�B
0�B
0�B
1B
1�B
1�B
1�B
1�B
2B
2�B
2�B
2�B
2�B
2�B
2�B
3B
2�B
2�B
4B
4B
5B
5%B
5B
5%B
6+B
6B
6B
6B
6B
7B
72B
72B
8B
88B
88B
9$B
9$B
:*B
:DB
:*B
;0B
;JB
;0B
;0B
<6B
<6B
<6B
=<B
=<B
=VB
=<B
>BB
>]B
>BB
>BB
>BB
?HB
?cB
?HB
?HB
@iB
@iB
@iB
@iB
@OB
AUB
AUB
AUB
AUB
AUB
B[B
BuB
BuB
BuB
BuB
C{B
B[B
CaB
CaB
C{B
CaB
C{B
C{B
CaB
DgB
DgB
DgB
EmB
EmB
EmB
F�B
FtB
GzB
G�B
G�B
GzB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
I�B
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
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
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
P�B
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
P�B
Q�B
Q�B
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
X�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
ZB
Y�B
Y�B
Y�B
Y�B
Z�B
[	B
[	B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
^B
^B
^B
^B
^B
^B
^B
^B
_B
_B
_B
_B
_!B
_B
_B
_B
_!B
_!B
_!B
_!B
_B
`'B
`B
`'B
`B
a-B
aB
aB
b4B
bB
c B
c:B
c B
c B
c:B
d@B
d@B
d@B
d&B
d&B
d&B
d@B
d&B
eFB
e,B
e,B
f2B
f2B
f2B
g8B
g8B
gRB
g8B
gRB
g8B
g8B
g8B
g8B
h>B
h>B
h>B
hXB
iDB
iDB
iDB
iDB
iDB
jKB
jKB
jKB
iDB
jeB
jKB
jKB
jeB
jKB
jKB
jeB
jKB
jeB
kQB
kQB
kkB
kQB
lWB
lWB
lqB
lWB
lqB
lWB
m]B
m]B
m]B
mwB
mwB
m]B
m]B
m]B
ncB
n}B
ncB
ncB
n}B
ncB
n}B
ncB
ncB
oiB
oiB
oiB
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.35(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811020040412018110200404120181102004041201811030038302018110300383020181103003830JA  ARFMdecpA19c                                                                20181028003518  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181027153603  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181027153604  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181027153605  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181027153605  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181027153605  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181027153606  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181027153606  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181027153606  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181027153606                      G�O�G�O�G�O�                JA  ARUP                                                                        20181027155641                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181027153927  CV  JULD            G�O�G�O�F�`�                JM  ARCAJMQC2.0                                                                 20181101154041  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181101154041  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181102153830  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                