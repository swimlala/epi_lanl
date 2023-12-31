CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-04-29T06:36:39Z creation;2019-04-29T06:36:43Z conversion to V3.1;2019-12-23T06:03:07Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190429063639  20200120031517  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_142                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @غm˩�1   @غ'�} @7�Xy=��c�f�A�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@���A
ffA*ffAJffAjffA�33A�33A�33A�33A�33A�33A�33A�33B��B
��B��B��B"��B*��B2��B:��BB��BJ��BR��BZ��Bb��Bj��Br��Bz��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C �fC�fC�fC�fC�fC
�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC �fC"�fC$�fC&�fC(�fC*�fC,�fC.�fC0�fC2�fC4�fC6�fC8�fC:�fC<�fC>�fC@�fCB�fCD�fCF�fCH�fCJ�fCL�fCN�fCP�fCR�fCT�fCV�fCX�fCZ�fC\�fC^�fC`�fCb�fCd�fCf�fCh�fCj�fCl�fCn�fCp�fCr�fCt�fCv�fCx�fCz�fC|�fC~�fC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�` C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�FfC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3D )�D ��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D	)�D	��D
)�D
��D)�D��D)�D��D0 D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D )�D ��D!)�D!��D")�D"��D#)�D#��D$)�D$��D%)�D%��D&)�D&��D')�D'��D()�D(��D))�D)��D*)�D*��D+)�D+��D,)�D,��D-)�D-��D.)�D.��D/)�D/��D0)�D0��D1)�D1��D2)�D2��D3#3D3��D4)�D4��D5)�D5��D6)�D6��D7)�D7��D8)�D8��D9)�D9��D:)�D:��D;)�D;��D<)�D<��D=)�D=��D>)�D>��D?)�D?��D@)�D@��DA)�DA��DB)�DB��DC)�DC��DD)�DD��DE)�DE��DF)�DF��DG)�DG��DH)�DH��DI)�DI��DJ)�DJ��DK)�DK��DL)�DL��DM)�DM��DN)�DN��DO)�DO��DP)�DP��DQ)�DQ��DR#3DR��DS)�DS��DT)�DT��DU)�DU��DV)�DV��DW)�DW��DX)�DX��DY)�DY��DZ)�DZ��D[)�D[��D\)�D\��D])�D]��D^)�D^��D_)�D_��D`)�D`��Da)�Da��Db)�Db��Dc)�Dc��Dd)�Dd��De)�De��Df)�Df��Dg)�Dg��Dh)�Dh��Di)�Di��Dj)�Dj��Dk)�Dk��Dl)�Dl��Dm)�Dm��Dn)�Dn��Do)�Do��Dp)�Dp��Dq)�Dq��Dr)�Dr��Ds)�Ds��Dt)�Dt��Du)�Du��Dv)�Dv��Dw)�Dw��Dx)�Dx��Dy)�Dy��Dz)�Dz��D{)�D{��D|)�D|��D})�D}��D~)�D~��D)�D��D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D�� D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D�њD��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D�D���D��D�T�DÔ�D���D��D�T�DĔ�D���D��D�T�DŔ�D���D��D�T�DƔ�D���D��D�T�Dǔ�D���D��D�T�DȔ�D���D��D�T�Dɔ�D���D��D�T�Dʔ�D���D��D�T�D˔�D���D��D�T�D̔�D���D��D�T�D͔�D���D��D�T�DΔ�D���D��D�T�Dϔ�D���D��D�T�DД�D���D��D�T�Dє�D���D��D�T�DҔ�D���D��D�T�DӔ�D���D��D�T�DԔ�D���D��D�T�DՔ�D���D��D�T�D֔�D���D��D�T�Dה�D���D��D�T�Dؔ�D���D��D�T�Dٔ�D���D��D�T�Dڔ�D���D��D�T�D۔�D���D��D�T�Dܔ�D���D��D�T�Dݔ�D���D��D�T�Dޔ�D���D��D�T�Dߔ�D���D��D�T�D���D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bNA�{A���A�bNA�7LA��A��HA��A�(�A��\A�=qA��A�{A�bA�JA�%A�A���A��TA��^A���A�\)A�G�A�/A�A��mA��
A��^A��^A��A��PA��A�jA�dZA�I�A��A��A�G�A�M�A��
A�"�A�A�dZA��A��-A�bNA�1'A�%A�l�A���A�jA��TA�dZA�(�A���A�&�A��A�?}A�ȴA�ffA�  A�S�A���A�=qA���A��A��DA�ZA�7LA��
A��A��A��`A���A��A��A�x�A�bA��A�hsA�  A�
=A�t�A���A�O�A�VA��/A�XA�A�oA�M�A�A��A���A�G�A�
=A��HA���A�|�A���A��A���A��\A�ZA���A�A�ƨA��A���A��DA�\)A��
A�E�A��!A��A�r�A���A�ZA��yA��yA��A~=qA{?}Az^5Aw�Au�PAsS�Arn�Ao�Am�-Aj��AiXAhz�Ae�PAd�uAb��Aa+A_�^A^�DA]G�A\�RAZ�AZ  AYS�AX��AW�AV�yAT�AS�#ASK�AQl�AOC�ANM�AL5?AJ�AJM�AI"�AGdZAE��AD��AD$�AC��AB�9AA�
AA;dA@�A?l�A>�jA=��A=C�A<��A:�HA9XA733A5�A5S�A41'A1�A0�9A/�-A.jA.A-�A++A)��A)&�A(VA&A$ȴA$1A#�A"VA �A ��At�Ar�A��AoA��A�AoA��AbA�`AVA�A�FA&�A�A�`A��A��A��A�^A�\A33A
5?A	�TA	��A	\)A	�A�A�DAn�A�TA��A�A �A{AA�AS�A��A �`A �HA ��A 1@��@���@�I�@�
=@��@�X@�1'@�{@�"�@�E�@�@�/@�@@�E�@�O�@�w@��y@�n�@�ff@�{@� �@�n�@��#@�X@�z�@��@�`B@��@�~�@݉7@�7L@���@�ff@��`@�ƨ@Ձ@���@�~�@��@�9X@��@��T@̣�@�V@��m@�
=@�~�@�hs@�/@��`@Ĵ9@�j@�+@�@��@�~�@��@���@��R@���@�/@��9@�9X@��w@�l�@�"�@��H@�v�@��-@�7L@��@��@��F@���@��y@��@�ȴ@���@�M�@���@���@��@�G�@��@��j@��@�b@��@���@�t�@��@��!@��\@��#@���@���@�bN@��;@�t�@�dZ@�\)@��@�v�@�V@�@�p�@��@�Ĝ@���@��R@��@�@�@�@���@���@��@�I�@��;@��@�33@���@��@��@��@��/@�j@��@��F@��y@��@�Z@�A�@�b@���@��P@��@���@�v�@�=q@�$�@��T@���@��7@�x�@��@���@��@���@���@���@���@��9@��9@��j@�1'@�Q�@�A�@���@�K�@���@�~�@�~�@�n�@�=q@���@���@�x�@�`B@�G�@�/@��j@��@�1'@��;@��w@��m@��;@��P@�\)@���@�M�@�J@���@���@��h@��7@��@�x�@�p�@�hs@�`B@�&�@�V@��@��u@�j@�j@�(�@���@�t�@��!@�V@�5?@��h@�G�@�/@�V@��/@��@���@��@�Z@�1@���@��w@��w@�ƨ@���@��F@���@��@�+@��H@��R@�~�@�ff@��@���@��#@���@���@��#@��#@���@��h@�`B@�O�@�G�@��@���@��j@��D@�j@�(�@�b@�  @��;@��P@�\)@�K�@�"�@���@���@�n�@�$�@��@���@�`B@�/@�/@��@�V@��`@��9@���@��u@��@�z�@�r�@� �@��w@��P@�dZ@�S�@�S�@�;d@�+@���@��H@��@��R@�M�@�5?@�{@��@�@���@��h@��7@��@���@�Z@�Q�@�;@�P@|�@�w@�P@~ff@}@|��@{33@z�\@z-@y��@y�^@x��@x�u@xr�@xQ�@w�@w|�@w;d@vff@u��@u`B@t��@t(�@sƨ@so@r=q@qhs@q�^@q�@o�@n�R@nff@n{@nv�@o+@ol�@o+@o�@n�y@nff@m�-@m�h@m?}@l9X@k��@kS�@ko@jM�@i&�@i�@i%@h1'@g�@f�R@f@e?}@d�D@d(�@c��@c�
@c�
@c��@cdZ@c@b~�@bJ@a�#@a�7@a%@`Ĝ@`Ĝ@`Ĝ@`��@`bN@_�P@^ff@^@]�-@]?}@]�@]/@]/@\�@\�@\�D@\j@\9X@\�@[t�@Z�\@Z=q@Z-@ZJ@Y��@Y�#@Y��@Y��@YX@Y&�@Y�@X�`@XĜ@X��@XQ�@X �@X  @W��@W�@W�P@W�P@Wl�@W+@W�@V�R@VE�@V{@U�T@U�@T�/@TI�@T1@S��@S�@SdZ@S33@S@R�!@R~�@R^5@RJ@RJ@Q��@Q��@Q�@Q��@Q��@Q��@Q�@Q%@P�`@P�@O�@Ol�@OK�@O+@N��@Nȴ@NE�@N$�@M��@L�@L�j@L�D@LZ@L(�@L1@K�
@K�F@Kt�@Ko@J��@J~�@J�@I�@I��@IG�@H�`@HĜ@HbN@G�@G�P@GK�@GK�@GK�@G;d@G+@F��@F$�@F@E�@E�@D��@D�D@D�D@DI�@Cƨ@Ct�@CS�@B��@B�!@B��@B�\@BM�@B-@A�@A�^@A��@Ax�@AX@A%@@Q�@?��@?;d@>�y@>v�@>E�@>$�@>@=�h@=�@<z�@<9X@;�m@;��@;33@:��@:^5@:-@9��@9��@9��@9x�@9X@9%@8�9@8�@8 �@7��@7��@7�@6�R@65?@5�h@5O�@4�@4Z@3�
@3�@2�H@2^5@2^5@2~�@2��@2��@1��@1&�@0��@0A�@/�;@/�w@/l�@/K�@/K�@/+@/+@/
=@.ȴ@.��@.�+@.E�@-�@-��@-�-@-�h@-�-@-�@-��@-�@-O�@,�/@,�@,��@,z�@,�@+�m@+�
@+��@+t�@+dZ@+dZ@+33@*�\@)��@)&�@(��@(�`@(��@(��@(Ĝ@(��@(�@(A�@(b@'��@'�w@'|�@';d@'
=@&ȴ@&V@&$�@%��@%p�@$��@$��@$z�@$I�@$9X@$(�@#�m@#�@#C�@#o@"�H@"-@"�@"J@!x�@!%@ Ĝ@ Ĝ@ Q�@  �@ b@�w@
=@�R@��@�+@E�@{@�@�T@@�h@O�@V@�@��@��@��@�@j@I�@(�@�
@��@�@o@��@-@�@�@�7@��@�9@��@�u@r�@Q�@1'@b@�w@��@|�@;d@+@
=@�y@��@�+@ff@$�@�@��@��@p�@?}@�@�@j@9X@��@�F@�@t�@"�@@��@~�@=q@�@��@�#@��@�7@X@�@Ĝ@��@�@Q�@1'@  @�;@�w@l�@;d@�@�@��@ȴ@��@v�@V@E�@{@�T@�h@�@`B@��@��@�@��@��@�@�D@I�@�m@�
@��@S�@"�@
�@
�@
�@
�H@
�!@
�\@
n�@
^5@
-@
J@	��@	x�@	G�@	&�@	%@	%@��@��@�`@�9@�@r�@bN@1'@ �@b@  @��@��@K�@+@
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bNA�{A���A�bNA�7LA��A��HA��A�(�A��\A�=qA��A�{A�bA�JA�%A�A���A��TA��^A���A�\)A�G�A�/A�A��mA��
A��^A��^A��A��PA��A�jA�dZA�I�A��A��A�G�A�M�A��
A�"�A�A�dZA��A��-A�bNA�1'A�%A�l�A���A�jA��TA�dZA�(�A���A�&�A��A�?}A�ȴA�ffA�  A�S�A���A�=qA���A��A��DA�ZA�7LA��
A��A��A��`A���A��A��A�x�A�bA��A�hsA�  A�
=A�t�A���A�O�A�VA��/A�XA�A�oA�M�A�A��A���A�G�A�
=A��HA���A�|�A���A��A���A��\A�ZA���A�A�ƨA��A���A��DA�\)A��
A�E�A��!A��A�r�A���A�ZA��yA��yA��A~=qA{?}Az^5Aw�Au�PAsS�Arn�Ao�Am�-Aj��AiXAhz�Ae�PAd�uAb��Aa+A_�^A^�DA]G�A\�RAZ�AZ  AYS�AX��AW�AV�yAT�AS�#ASK�AQl�AOC�ANM�AL5?AJ�AJM�AI"�AGdZAE��AD��AD$�AC��AB�9AA�
AA;dA@�A?l�A>�jA=��A=C�A<��A:�HA9XA733A5�A5S�A41'A1�A0�9A/�-A.jA.A-�A++A)��A)&�A(VA&A$ȴA$1A#�A"VA �A ��At�Ar�A��AoA��A�AoA��AbA�`AVA�A�FA&�A�A�`A��A��A��A�^A�\A33A
5?A	�TA	��A	\)A	�A�A�DAn�A�TA��A�A �A{AA�AS�A��A �`A �HA ��A 1@��@���@�I�@�
=@��@�X@�1'@�{@�"�@�E�@�@�/@�@@�E�@�O�@�w@��y@�n�@�ff@�{@� �@�n�@��#@�X@�z�@��@�`B@��@�~�@݉7@�7L@���@�ff@��`@�ƨ@Ձ@���@�~�@��@�9X@��@��T@̣�@�V@��m@�
=@�~�@�hs@�/@��`@Ĵ9@�j@�+@�@��@�~�@��@���@��R@���@�/@��9@�9X@��w@�l�@�"�@��H@�v�@��-@�7L@��@��@��F@���@��y@��@�ȴ@���@�M�@���@���@��@�G�@��@��j@��@�b@��@���@�t�@��@��!@��\@��#@���@���@�bN@��;@�t�@�dZ@�\)@��@�v�@�V@�@�p�@��@�Ĝ@���@��R@��@�@�@�@���@���@��@�I�@��;@��@�33@���@��@��@��@��/@�j@��@��F@��y@��@�Z@�A�@�b@���@��P@��@���@�v�@�=q@�$�@��T@���@��7@�x�@��@���@��@���@���@���@���@��9@��9@��j@�1'@�Q�@�A�@���@�K�@���@�~�@�~�@�n�@�=q@���@���@�x�@�`B@�G�@�/@��j@��@�1'@��;@��w@��m@��;@��P@�\)@���@�M�@�J@���@���@��h@��7@��@�x�@�p�@�hs@�`B@�&�@�V@��@��u@�j@�j@�(�@���@�t�@��!@�V@�5?@��h@�G�@�/@�V@��/@��@���@��@�Z@�1@���@��w@��w@�ƨ@���@��F@���@��@�+@��H@��R@�~�@�ff@��@���@��#@���@���@��#@��#@���@��h@�`B@�O�@�G�@��@���@��j@��D@�j@�(�@�b@�  @��;@��P@�\)@�K�@�"�@���@���@�n�@�$�@��@���@�`B@�/@�/@��@�V@��`@��9@���@��u@��@�z�@�r�@� �@��w@��P@�dZ@�S�@�S�@�;d@�+@���@��H@��@��R@�M�@�5?@�{@��@�@���@��h@��7@��@���@�Z@�Q�@�;@�P@|�@�w@�P@~ff@}@|��@{33@z�\@z-@y��@y�^@x��@x�u@xr�@xQ�@w�@w|�@w;d@vff@u��@u`B@t��@t(�@sƨ@so@r=q@qhs@q�^@q�@o�@n�R@nff@n{@nv�@o+@ol�@o+@o�@n�y@nff@m�-@m�h@m?}@l9X@k��@kS�@ko@jM�@i&�@i�@i%@h1'@g�@f�R@f@e?}@d�D@d(�@c��@c�
@c�
@c��@cdZ@c@b~�@bJ@a�#@a�7@a%@`Ĝ@`Ĝ@`Ĝ@`��@`bN@_�P@^ff@^@]�-@]?}@]�@]/@]/@\�@\�@\�D@\j@\9X@\�@[t�@Z�\@Z=q@Z-@ZJ@Y��@Y�#@Y��@Y��@YX@Y&�@Y�@X�`@XĜ@X��@XQ�@X �@X  @W��@W�@W�P@W�P@Wl�@W+@W�@V�R@VE�@V{@U�T@U�@T�/@TI�@T1@S��@S�@SdZ@S33@S@R�!@R~�@R^5@RJ@RJ@Q��@Q��@Q�@Q��@Q��@Q��@Q�@Q%@P�`@P�@O�@Ol�@OK�@O+@N��@Nȴ@NE�@N$�@M��@L�@L�j@L�D@LZ@L(�@L1@K�
@K�F@Kt�@Ko@J��@J~�@J�@I�@I��@IG�@H�`@HĜ@HbN@G�@G�P@GK�@GK�@GK�@G;d@G+@F��@F$�@F@E�@E�@D��@D�D@D�D@DI�@Cƨ@Ct�@CS�@B��@B�!@B��@B�\@BM�@B-@A�@A�^@A��@Ax�@AX@A%@@Q�@?��@?;d@>�y@>v�@>E�@>$�@>@=�h@=�@<z�@<9X@;�m@;��@;33@:��@:^5@:-@9��@9��@9��@9x�@9X@9%@8�9@8�@8 �@7��@7��@7�@6�R@65?@5�h@5O�@4�@4Z@3�
@3�@2�H@2^5@2^5@2~�@2��@2��@1��@1&�@0��@0A�@/�;@/�w@/l�@/K�@/K�@/+@/+@/
=@.ȴ@.��@.�+@.E�@-�@-��@-�-@-�h@-�-@-�@-��@-�@-O�@,�/@,�@,��@,z�@,�@+�m@+�
@+��@+t�@+dZ@+dZ@+33@*�\@)��@)&�@(��@(�`@(��@(��@(Ĝ@(��@(�@(A�@(b@'��@'�w@'|�@';d@'
=@&ȴ@&V@&$�@%��@%p�@$��@$��@$z�@$I�@$9X@$(�@#�m@#�@#C�@#o@"�H@"-@"�@"J@!x�@!%@ Ĝ@ Ĝ@ Q�@  �@ b@�w@
=@�R@��@�+@E�@{@�@�T@@�h@O�@V@�@��@��@��@�@j@I�@(�@�
@��@�@o@��@-@�@�@�7@��@�9@��@�u@r�@Q�@1'@b@�w@��@|�@;d@+@
=@�y@��@�+@ff@$�@�@��@��@p�@?}@�@�@j@9X@��@�F@�@t�@"�@@��@~�@=q@�@��@�#@��@�7@X@�@Ĝ@��@�@Q�@1'@  @�;@�w@l�@;d@�@�@��@ȴ@��@v�@V@E�@{@�T@�h@�@`B@��@��@�@��@��@�@�D@I�@�m@�
@��@S�@"�@
�@
�@
�@
�H@
�!@
�\@
n�@
^5@
-@
J@	��@	x�@	G�@	&�@	%@	%@��@��@�`@�9@�@r�@bN@1'@ �@b@  @��@��@K�@+@
=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
bNB
bNB
aHB
gmB
p�B
y�B
y�B
{�B
�B
�JB
�\B
�hB
�hB
�oB
�oB
�oB
�oB
�uB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
ƨB
�HB
��B1BJB�B&�B6FBE�BVB�bB��B�BB�B�5B�B�BVB�B�B�B-B5?B;dBA�BI�BQ�B]/BbNBffBl�Bp�Bq�Bv�By�B|�B}�B~�B� B�B�B}�B�Bz�B{�B�DB�=B�B�DB�{B�{B�bB�DB� Br�B]/B �B��B�B�yB�TB�BB�ZB�TB��B�B��B��B�PBw�Be`BK�B33BbB
�HB
ĜB
�^B
�!B
�B
��B
�bB
�DB
{�B
e`B
W
B
C�B
-B
%�B
�B
VB	��B	��B	�yB	�#B	��B	B	�qB	�'B	��B	��B	��B	�bB	�DB	�B	~�B	y�B	r�B	n�B	l�B	gmB	cTB	XB	Q�B	M�B	D�B	8RB	0!B	%�B	�B	�B	{B		7B��B��B�B�B�B�B�fB�HB�)B�B��B��B��BĜB�XB�B��B��B��B��B�oB�PB�B~�Bx�Bq�Br�Bt�Bs�B|�Bz�Bz�Bx�Bw�Bq�Bp�Bq�Bn�Bm�BhsBbNBXBT�BL�BG�BE�BC�BB�BA�BA�B?}B<jB9XB8RB6FB6FB8RB8RB49B5?B49B6FB5?B5?B6FB8RB;dB=qB33B49B9XB>wB>wB8RB49B33B33B33B/B/B0!B49B49B5?B7LB8RB7LB6FB6FB7LB8RB7LB7LB8RB:^B;dB<jB;dB;dB?}B@�B@�B@�BB�BD�BE�BH�BI�BJ�BK�BM�BL�BM�BM�BM�BO�BP�BO�BO�BO�BP�BP�BT�BZB\)B^5B`BB`BBaHBbNBdZBhsBhsBk�Bo�Br�Bt�Bs�Bs�Bw�Bx�Bz�B{�B|�B}�B~�B� B�B�+B�PB�hB�uB��B��B��B��B��B��B��B�B�B�!B�9B�FB�XB�qB�wB�wB�}BŢBɺB��B��B�B�B�B�;B�NB�TB�TB�mB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	  B	B	B	%B	VB	bB	\B	bB	oB	�B	�B	�B	�B	!�B	"�B	%�B	)�B	+B	,B	0!B	49B	8RB	9XB	<jB	C�B	E�B	I�B	K�B	N�B	R�B	YB	\)B	]/B	^5B	_;B	dZB	ffB	gmB	hsB	iyB	jB	jB	iyB	iyB	iyB	iyB	iyB	iyB	iyB	k�B	n�B	r�B	s�B	r�B	s�B	s�B	t�B	y�B	z�B	z�B	{�B	{�B	{�B	|�B	|�B	}�B	�B	�B	�B	�7B	�PB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�?B	�FB	�LB	�XB	�^B	�jB	�qB	��B	��B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B

=B
DB
JB
PB
JB
DB

=B

=B

=B
DB
PB
VB
\B
bB
bB
hB
oB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
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
+B
+B
,B
,B
,B
-B
.B
.B
.B
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
49B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
:^B
:^B
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
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
>wB
>wB
>wB
>wB
=qB
>wB
>wB
?}B
?}B
?}B
>wB
>wB
=qB
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
@�B
@�B
@�B
A�B
B�B
B�B
B�B
D�B
E�B
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
G�B
G�B
F�B
F�B
E�B
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
E�B
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
J�B
J�B
J�B
J�B
K�B
L�B
L�B
K�B
L�B
M�B
M�B
M�B
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
P�B
P�B
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
T�B
T�B
T�B
T�B
VB
VB
VB
VB
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
`BB
`BB
`BB
`BB
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
k�B
l�B
k�B
l�B
l�B
l�B
m�B
m�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
bB
bB
aB
g8B
poB
y�B
y�B
{�B
��B
�B
�(B
�4B
�4B
�:B
�:B
�:B
�:B
�@B
�@B
�MB
�SB
�_B
�_B
�qB
�~B
�~B
�~B
�~B
��B
��B
��B
��B
��B
��B
��B
�tB
�B
��B�BBeB&�B6BEmBU�B�.B��B��B�[B��B�B�KB�B"B_BSBkB,�B5B;0BAUBI�BQ�B\�BbBfBlWBpoBqvBv�By�B|�B}�B~�B�B��B��B}�B��Bz�B{�B�B�	B��B�B�FB�,B�.B�B�Br|B\�B vB��B�oB�*B� B�B�&B�B��B��B��B�|B�Bw�Be,BKxB2�B.B
��B
�MB
�B
��B
��B
�jB
�.B
��B
{�B
eB
V�B
CGB
,�B
%�B
kB
B	��B	�zB	�DB	��B	�~B	�[B	�"B	��B	��B	��B	�_B	�.B	�B	��B	~�B	y�B	raB	ncB	l=B	gB	c B	W�B	Q�B	M�B	DMB	8B	/�B	%�B	jB	qB	FB	�B��B��B�[B�[B�[B�6B�2B��B��B��BԯBѝBΊB�MB�	B��B��B��B��B�2B� B�B��B~�Bx�Bq[BraBtnBshB|�Bz�Bz�Bx�Bw�Bq[BpUBq[BnIBmCBh$Ba�BW�BT�BL~BG_BEmBCGBBABA;BA;B?HB<B9	B8B5�B5�B8B8B3�B4�B3�B5�B4�B4�B5�B8B;B="B2�B3�B9	B>(B>(B8B3�B2�B2�B2�B.�B.�B/�B3�B3�B4�B6�B8B6�B5�B5�B6�B8B6�B6�B8B:B;B<B;B;B?.B@4B@4B@4BBABDMBESBHfBIlBJrBKxBM�BL~BM�BM�BMjBO�BP�BO�BO�BO�BP�BP�BT�BY�B[�B]�B_�B_�B`�Ba�BdBh$Bh$Bk6BoOBraBtnBshBshBw�Bx�Bz�B{�B|�B}�B~�B�B��B��B�B�B�&B�?B�KB�]B�vB�hB��B��B��B��B��B��B��B�	B�"B�(B�(B�.B�SB�lB�xBуBյB��B��B��B��B�B�B�B�*B�0B�=B�IB�OB�IB�UB�UB�OB�IB�/B�OB�AB�aB�tB�zB�zB��B�rB��B��B��B��B��B��B	 �B	�B	�B	B	B	B	B	 B	9B	EB	=B	pB	!|B	"�B	%zB	)�B	*�B	+�B	/�B	3�B	8B	8�B	<B	C-B	ESB	IlB	KxB	N�B	R�B	X�B	[�B	\�B	]�B	^�B	dB	fB	gB	h
B	iB	jB	j0B	i*B	i*B	i*B	iB	i*B	i*B	i*B	k6B	nIB	raB	shB	rGB	shB	shB	tnB	y�B	zxB	z�B	{�B	{B	{�B	|�B	|�B	}�B	��B	��B	��B	��B	��B	��B	�B	�2B	�9B	�KB	�7B	�]B	�pB	�vB	�|B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�"B	�4B	�;B	�GB	�3B	�MB	�3B	�9B	�?B	�?B	�RB	�^B	ΊB	ЗB	ѝB	өB	ԯB	՛B	֡B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�$B	�0B	�B	�6B	�6B	�6B	�=B	�=B	�"B	�CB	�)B	�=B	�CB	�IB	�OB	�5B	�UB	�UB	�UB	�UB	�;B	�[B	�UB	�UB	�UB	�;B	�UB	�MB	�MB	�hB	�aB	�aB	�hB	�tB	�tB	�tB	�zB	�zB	��B	�fB	��B	��B	�rB	�rB	��B	��B	��B	��B	��B	�B	�xB	��B	��B	��B	��B	��B	��B	�xB	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B
�B
B
�B

�B
	�B
	�B
	�B

�B
B
B
B
�B
�B
 B
 B
 B
 B
&B
B
,B
,B
,B
2B
B
B
9B
$B
?B
?B
+B
EB
KB
QB
QB
QB
=B
WB
=B
=B
]B
CB
IB
jB
jB
jB
pB
pB
 vB
 vB
!|B
!|B
"�B
"�B
"�B
"hB
"hB
"�B
#�B
#nB
#�B
#nB
#�B
#�B
#nB
$�B
$tB
#�B
$�B
$tB
%�B
%�B
%�B
%�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
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
*�B
*�B
+�B
+�B
+�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
0�B
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
3�B
3�B
3�B
4�B
4�B
4�B
5�B
6�B
6�B
6�B
8B
7�B
7�B
8B
8B
8B
8B
8B
7�B
7�B
:B
9�B
;B
;B
;B
;B
<B
<B
<B
<B
="B
="B
="B
="B
>B
>(B
>B
?.B
?.B
?.B
?.B
>(B
>(B
>B
>(B
="B
>(B
>B
?B
?.B
?B
>(B
>(B
=B
>B
>B
?.B
?.B
?.B
@4B
@B
@4B
@B
@4B
@4B
@4B
@4B
A;B
BAB
B'B
BAB
DMB
ESB
E9B
E9B
E9B
E9B
ESB
ESB
FYB
FYB
FYB
FYB
GEB
G_B
FYB
FYB
E9B
DMB
DMB
DMB
DMB
DMB
ESB
ESB
ESB
E9B
ESB
ESB
FYB
FYB
FYB
GEB
GEB
G_B
G_B
G_B
G_B
G_B
G_B
G_B
HfB
IlB
IRB
IlB
JXB
JrB
JXB
JXB
KxB
L~B
LdB
KxB
LdB
M�B
M�B
M�B
NpB
NpB
N�B
NpB
O�B
OvB
O�B
OvB
P�B
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
U�B
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
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
cB
cB
cB
b�B
cB
cB
cB
dB
c�B
dB
dB
dB
d�B
eB
d�B
eB
e�B
fB
e�B
e�B
fB
fB
fB
gB
gB
gB
gB
h$B
h$B
h$B
h$B
h
B
h
B
h$B
i*B
i*B
i*B
i*B
i*B
j0B
j0B
j0B
j0B
j0B
j0B
jB
j0B
k6B
k6B
k6B
kB
k6B
l=B
k6B
l"B
l"B
l=B
mCB
mCB
nI111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.65(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905040034522019050400345220190504003452201905050031102019050500311020190505003110JA  ARFMdecpA19c                                                                20190429153636  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190429063639  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190429063641  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190429063641  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190429063642  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190429063642  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190429063642  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190429063642  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190429063643  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190429063643                      G�O�G�O�G�O�                JA  ARUP                                                                        20190429065946                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190429153356  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190503153452  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190503153452  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190504153110  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031517                      G�O�G�O�G�O�                