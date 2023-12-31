CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-19T00:35:41Z creation;2018-10-19T00:35:44Z conversion to V3.1;2019-12-23T06:13:25Z update;     
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
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181019003541  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               aA   JA  I2_0675_097                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @؉����1   @؉��l @6���+j��c[dZ�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�<�Dր D�� D�  D�@ D�|�D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D��fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\��@���@���A
ffA*ffAJffAjffA�33A�33A�33A�33A�33A�33A�33A�33B��B
��B��B��B"��B*��B2��B:��BB��BJ��BR��BZ��Bb��Bj��Br��Bz��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C �fC�fC�fC�fC�fC
�fC�fC�fC�fC��C�fC�fC�fC�fC�fC�fC �fC"�fC$�fC&�fC(�fC*�fC,�fC.�fC0�fC2�fC4�fC6�fC8�fC:�fC<�fC>�fC@�fCB�fCD�fCF�fCH�fCJ�fCL�fCN�fCP�fCR�fCT�fCV�fCX��CZ�fC\�fC^�fC`�fCb�fCd�fCf�fCh�fCj�fCl�fCn�fCp�fCr�fCt�fCv�fCx�fCz�fC|�fC~�fC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�FfC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�FfC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�` C�` C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3D )�D ��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D	)�D	��D
)�D
��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D )�D ��D!)�D!��D")�D"��D#)�D#��D$)�D$��D%)�D%��D&)�D&��D')�D'��D()�D(��D))�D)��D*)�D*��D+)�D+��D,)�D,�3D-)�D-��D.)�D.��D/)�D/��D0)�D0�3D1)�D1��D2)�D2��D3)�D3��D4)�D4��D5)�D5��D6)�D6��D7)�D7��D8)�D8��D9)�D9��D:)�D:��D;)�D;��D<)�D<��D=)�D=��D>)�D>��D?)�D?��D@)�D@��DA)�DA��DB)�DB��DC)�DC��DD)�DD��DE)�DE��DF)�DF��DG)�DG��DH)�DH��DI)�DI��DJ#3DJ��DK)�DK��DL)�DL��DM)�DM��DN)�DN��DO)�DO��DP)�DP��DQ)�DQ��DR)�DR��DS)�DS��DT)�DT��DU)�DU��DV)�DV��DW)�DW��DX)�DX��DY0 DY��DZ)�DZ��D[)�D[��D\)�D\��D])�D]��D^)�D^��D_)�D_��D`)�D`��Da)�Da��Db)�Db��Dc)�Dc��Dd)�Dd��De)�De��Df)�Df��Dg)�Dg��Dh)�Dh��Di)�Di��Dj)�Dj��Dk)�Dk��Dl)�Dl��Dm)�Dm��Dn)�Dn��Do)�Do��Dp)�Dp�3Dq)�Dq��Dr)�Dr��Ds)�Ds��Dt)�Dt��Du)�Du��Dv)�Dv��Dw)�Dw��Dx)�Dx��Dy)�Dy��Dz)�Dz��D{)�D{��D|)�D|��D})�D}��D~)�D~��D)�D��D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D�� D���D��D�T�D�D���D��D�T�DÔ�D���D��D�T�DĔ�D���D��D�T�DŔ�D���D��D�T�DƔ�D���D��D�T�Dǔ�D���D��D�T�DȔ�D���D��D�T�Dɔ�D���D��D�T�Dʔ�D���D��D�T�D˔�D���D��D�T�D̔�D���D��D�T�D͔�D���D��D�T�DΔ�D���D��D�T�Dϔ�D���D��D�T�DД�D���D��D�T�Dє�D���D��D�T�DҔ�D���D��D�T�DӔ�D���D��D�T�DԔ�D���D��D�T�DՔ�D���D��D�Q�D֔�D���D��D�T�Dב�D���D��D�T�Dؔ�D���D��D�T�Dٔ�D���D��D�T�Dڔ�D���D��D�T�D۔�D���D��D�T�Dܔ�D���D��D�T�Dݔ�D���D��D�T�Dޔ�D���D��D�T�Dߔ�D���D��D�T�D���D���D��D�T�D��D���D��D�T�D��D���D��D�Q�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D���D���D��D�T�D���D���D��D�X D��3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v�A�bNA�I�A�=qA�5?A�/A�+A�&�A�"�A� �A� �A��A��A��A��A��A��A�oA�bA�VA�
=A�%A���A���A���A���A��AϓuA��A��`A�(�A�1A�v�A�ĜA�5?A��RA�z�A���A�A��^A�  A�ĜA��A��A�7LA��hA�JA�n�A��^A���A�K�A��TA��
A�^5A�O�A�ĜA�A�;dA���A�7LA���A�ZA��#A���A�K�A��9A�Q�A�ĜA�  A�v�A���A�;dA�S�A��mA�"�A�9XA�|�A���A�A�A���A��mA��A�l�A��A�M�A���A�ZA��RA���A��A��-A�E�A�A�O�A�oA�9XA��A�C�A��A�
=A�(�A�/A�A���A���A��A�%A~  A}"�A|z�Ay\)AwhsAvbNAu�-AtjAs��Ar�Ap�RAo�-Am"�Aj�yAj{AhM�Af�yAeC�Ac�wAa�A_7LA]�TA\9XA[`BAYAW�mAWO�ASC�ARAQ��AQS�AO&�AL9XAI�FAHbAG�-AG�AE�AC�;ABv�AAXA@E�A?�A<��A;7LA8r�A7��A6��A6{A5K�A4�A3%A21'A1\)A0A.A-S�A,r�A,M�A,5?A,A+K�A)�A(ĜA(bA&��A%�A$v�A#\)A!��A!A!��A �/A �A�RA��A�AbNA�TA�A1'A%Al�A-A�#A�FAC�AVAp�A~�A�AƨA�AA�A`BA�yA�A��Al�AK�A
�A	��A	VAv�A1A��A
=AȴA�DA��A�`A��At�A �j@�ƨ@�@�{@�b@���@���@���@�t�@�-@�hs@�O�@�Ĝ@���@�o@���@� �@��@�o@�X@�1'@���@�5?@۝�@���@ם�@�@֧�@�@�%@���@���@���@�1'@�l�@�M�@�7L@��
@�o@���@�{@�X@�9X@Ə\@ũ�@�G�@ř�@�G�@��@�V@���@��P@�V@�{@�7L@�j@��w@�t�@�{@�V@�r�@���@�n�@�`B@���@�r�@��m@�K�@��@��R@��#@��7@�G�@���@�Q�@��@�33@���@��@���@�bN@�1'@��@��@�=q@�%@�(�@���@��H@�E�@��T@�X@��@�Ĝ@��@�A�@�  @��m@��w@�K�@��@��m@�1@�  @��;@��@�"�@�~�@��y@���@�@��!@��R@�ȴ@���@�M�@�J@���@��h@��@�j@��@���@��@��w@�C�@�33@�33@�@��R@�v�@���@��h@��@��9@�I�@�(�@��@�(�@� �@���@��w@��;@���@��m@�|�@��@��H@���@���@���@��\@�^5@��@��-@�?}@���@�Z@�(�@��w@���@�C�@��H@���@��\@�v�@�~�@�=q@�5?@�5?@�{@���@�@�7L@�G�@�O�@��u@�I�@�I�@�j@�r�@�r�@�z�@��@��;@��@��@��R@��@��T@���@��-@���@��9@��m@���@��F@��@�t�@�l�@�dZ@�;d@��@��+@��@��@���@�%@�&�@���@�Ĝ@��u@���@��H@���@�~�@�ff@�E�@�-@��@���@�@��7@�G�@�G�@���@�j@�z�@�9X@�(�@���@��;@��
@�ƨ@��@���@�dZ@�+@���@���@�~�@�^5@�5?@�$�@�$�@�{@���@�p�@�X@�7L@��@�Ĝ@��@�Q�@��m@���@��@�t�@�t�@�t�@�dZ@�dZ@�K�@��@��H@�ff@�@��#@���@���@�x�@�?}@�&�@��@���@��/@�Ĝ@��u@�j@�A�@� �@�@\)@~�@~��@~v�@~V@~{@}�h@|�@{�m@{"�@z��@z=q@y��@yx�@y&�@x��@xQ�@xb@x  @w�w@w|�@v��@vV@u�-@u�@tz�@t(�@s�m@sƨ@sS�@s"�@r�\@r-@q��@q%@p�`@p�9@p  @ol�@o+@n�@n��@nff@m�@m�T@m��@m�@l�@l�D@lI�@l1@kƨ@k��@k�@k"�@j��@j=q@i��@i�#@i��@i%@hQ�@hb@g�w@g��@g\)@g�@g
=@f��@fȴ@fff@f{@e�T@e�h@d��@dz�@d9X@c�m@c�F@c��@ct�@c"�@b��@b=q@a�@a%@`��@`�`@`��@`�9@` �@_l�@^��@^5?@^@]��@]`B@\��@\�D@\9X@[��@[dZ@Z�@Z�!@Z�\@ZM�@Z�@Y�@Y��@Y�7@Y�7@Y�@XQ�@X1'@X  @W��@W|�@WK�@W�@V�@V�+@Vv�@V$�@V{@U��@U?}@U/@U�@T��@T��@T�/@T��@T�@Tz�@TI�@S�m@S33@R^5@Q��@Q�@P�u@PbN@P1'@P �@O�@O��@Ol�@OK�@O�@N��@M��@M/@L��@L�j@L��@Lj@L(�@Kƨ@J�H@JM�@I��@IX@IG�@IG�@I7L@I&�@H�u@G��@G
=@F�@Fȴ@F��@F��@F�+@FE�@F@E@E�@E`B@E?}@E/@E/@E�@D�@D�@D�@Dz�@DZ@D�@C�m@C��@CC�@B�@B�!@B^5@B-@A�@Ax�@@��@@�u@@bN@?�w@>��@>v�@>$�@>{@>@=�T@=?}@=V@<��@<��@<��@<��@<�D@<j@<�@;C�@:�H@:�!@:�\@:M�@9��@97L@9&�@9�@9%@8�`@8bN@7�@7|�@7�@6ȴ@6�R@6��@6�+@6v�@6ff@65?@5�T@5��@5@5�-@5�-@5��@5p�@5?}@4�@4z�@41@3o@2��@2�\@2=q@1�#@1��@1G�@0Ĝ@0�u@0�@0Q�@0 �@/�@/\)@.��@.�y@.��@.�y@.�@.v�@.E�@.$�@-��@-`B@-/@-�@-V@,�j@,Z@,�@+�@+C�@*�H@*M�@*J@)�#@)hs@)�@(�u@(A�@(b@'�;@'��@'\)@'+@&�R@&v�@&$�@%�T@%��@%O�@$��@$�D@$Z@$(�@$�@$1@#�m@#��@#dZ@#C�@"��@"�\@"n�@"n�@"^5@"�@!�#@!�7@!G�@!G�@!7L@!7L@!&�@!&�@ ��@ 1'@ b@�@��@�P@K�@�R@E�@{@�-@O�@�@�@��@�@j@I�@�@1@��@dZ@@�@�H@�H@��@~�@�@�@��@X@%@�`@Ĝ@�@A�@1'@b@�;@�@�P@|�@+@�R@�+@E�@5?@�T@�h@?}@�@I�@��@dZ@C�@33@"�@o@o@@�H@��@��@~�@M�@M�@�@J@��@�#@��@X@G�@7L@��@�9@�@�@�@r�@Q�@1'@ �@  @�w@|�@K�@;d@;d@;d@+@�@��@��@�y@�@�@��@v�@ff@E�@5?@$�@{@@@�@�T@�T@@�@`B@`B@O�@O�@?}@V@�@�/@��@�j@��@��@��@�D@�D@�D@�D@Z@��@��@�@33@
�!@
=q@	�@	�^@	��@	X@	G�@	�@��@�@r�@A�@1'@b@�@�@|�@K�@+@��@��@V@{@�@@�T111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�v�A�bNA�I�A�=qA�5?A�/A�+A�&�A�"�A� �A� �A��A��A��A��A��A��A�oA�bA�VA�
=A�%A���A���A���A���A��AϓuA��A��`A�(�A�1A�v�A�ĜA�5?A��RA�z�A���A�A��^A�  A�ĜA��A��A�7LA��hA�JA�n�A��^A���A�K�A��TA��
A�^5A�O�A�ĜA�A�;dA���A�7LA���A�ZA��#A���A�K�A��9A�Q�A�ĜA�  A�v�A���A�;dA�S�A��mA�"�A�9XA�|�A���A�A�A���A��mA��A�l�A��A�M�A���A�ZA��RA���A��A��-A�E�A�A�O�A�oA�9XA��A�C�A��A�
=A�(�A�/A�A���A���A��A�%A~  A}"�A|z�Ay\)AwhsAvbNAu�-AtjAs��Ar�Ap�RAo�-Am"�Aj�yAj{AhM�Af�yAeC�Ac�wAa�A_7LA]�TA\9XA[`BAYAW�mAWO�ASC�ARAQ��AQS�AO&�AL9XAI�FAHbAG�-AG�AE�AC�;ABv�AAXA@E�A?�A<��A;7LA8r�A7��A6��A6{A5K�A4�A3%A21'A1\)A0A.A-S�A,r�A,M�A,5?A,A+K�A)�A(ĜA(bA&��A%�A$v�A#\)A!��A!A!��A �/A �A�RA��A�AbNA�TA�A1'A%Al�A-A�#A�FAC�AVAp�A~�A�AƨA�AA�A`BA�yA�A��Al�AK�A
�A	��A	VAv�A1A��A
=AȴA�DA��A�`A��At�A �j@�ƨ@�@�{@�b@���@���@���@�t�@�-@�hs@�O�@�Ĝ@���@�o@���@� �@��@�o@�X@�1'@���@�5?@۝�@���@ם�@�@֧�@�@�%@���@���@���@�1'@�l�@�M�@�7L@��
@�o@���@�{@�X@�9X@Ə\@ũ�@�G�@ř�@�G�@��@�V@���@��P@�V@�{@�7L@�j@��w@�t�@�{@�V@�r�@���@�n�@�`B@���@�r�@��m@�K�@��@��R@��#@��7@�G�@���@�Q�@��@�33@���@��@���@�bN@�1'@��@��@�=q@�%@�(�@���@��H@�E�@��T@�X@��@�Ĝ@��@�A�@�  @��m@��w@�K�@��@��m@�1@�  @��;@��@�"�@�~�@��y@���@�@��!@��R@�ȴ@���@�M�@�J@���@��h@��@�j@��@���@��@��w@�C�@�33@�33@�@��R@�v�@���@��h@��@��9@�I�@�(�@��@�(�@� �@���@��w@��;@���@��m@�|�@��@��H@���@���@���@��\@�^5@��@��-@�?}@���@�Z@�(�@��w@���@�C�@��H@���@��\@�v�@�~�@�=q@�5?@�5?@�{@���@�@�7L@�G�@�O�@��u@�I�@�I�@�j@�r�@�r�@�z�@��@��;@��@��@��R@��@��T@���@��-@���@��9@��m@���@��F@��@�t�@�l�@�dZ@�;d@��@��+@��@��@���@�%@�&�@���@�Ĝ@��u@���@��H@���@�~�@�ff@�E�@�-@��@���@�@��7@�G�@�G�@���@�j@�z�@�9X@�(�@���@��;@��
@�ƨ@��@���@�dZ@�+@���@���@�~�@�^5@�5?@�$�@�$�@�{@���@�p�@�X@�7L@��@�Ĝ@��@�Q�@��m@���@��@�t�@�t�@�t�@�dZ@�dZ@�K�@��@��H@�ff@�@��#@���@���@�x�@�?}@�&�@��@���@��/@�Ĝ@��u@�j@�A�@� �@�@\)@~�@~��@~v�@~V@~{@}�h@|�@{�m@{"�@z��@z=q@y��@yx�@y&�@x��@xQ�@xb@x  @w�w@w|�@v��@vV@u�-@u�@tz�@t(�@s�m@sƨ@sS�@s"�@r�\@r-@q��@q%@p�`@p�9@p  @ol�@o+@n�@n��@nff@m�@m�T@m��@m�@l�@l�D@lI�@l1@kƨ@k��@k�@k"�@j��@j=q@i��@i�#@i��@i%@hQ�@hb@g�w@g��@g\)@g�@g
=@f��@fȴ@fff@f{@e�T@e�h@d��@dz�@d9X@c�m@c�F@c��@ct�@c"�@b��@b=q@a�@a%@`��@`�`@`��@`�9@` �@_l�@^��@^5?@^@]��@]`B@\��@\�D@\9X@[��@[dZ@Z�@Z�!@Z�\@ZM�@Z�@Y�@Y��@Y�7@Y�7@Y�@XQ�@X1'@X  @W��@W|�@WK�@W�@V�@V�+@Vv�@V$�@V{@U��@U?}@U/@U�@T��@T��@T�/@T��@T�@Tz�@TI�@S�m@S33@R^5@Q��@Q�@P�u@PbN@P1'@P �@O�@O��@Ol�@OK�@O�@N��@M��@M/@L��@L�j@L��@Lj@L(�@Kƨ@J�H@JM�@I��@IX@IG�@IG�@I7L@I&�@H�u@G��@G
=@F�@Fȴ@F��@F��@F�+@FE�@F@E@E�@E`B@E?}@E/@E/@E�@D�@D�@D�@Dz�@DZ@D�@C�m@C��@CC�@B�@B�!@B^5@B-@A�@Ax�@@��@@�u@@bN@?�w@>��@>v�@>$�@>{@>@=�T@=?}@=V@<��@<��@<��@<��@<�D@<j@<�@;C�@:�H@:�!@:�\@:M�@9��@97L@9&�@9�@9%@8�`@8bN@7�@7|�@7�@6ȴ@6�R@6��@6�+@6v�@6ff@65?@5�T@5��@5@5�-@5�-@5��@5p�@5?}@4�@4z�@41@3o@2��@2�\@2=q@1�#@1��@1G�@0Ĝ@0�u@0�@0Q�@0 �@/�@/\)@.��@.�y@.��@.�y@.�@.v�@.E�@.$�@-��@-`B@-/@-�@-V@,�j@,Z@,�@+�@+C�@*�H@*M�@*J@)�#@)hs@)�@(�u@(A�@(b@'�;@'��@'\)@'+@&�R@&v�@&$�@%�T@%��@%O�@$��@$�D@$Z@$(�@$�@$1@#�m@#��@#dZ@#C�@"��@"�\@"n�@"n�@"^5@"�@!�#@!�7@!G�@!G�@!7L@!7L@!&�@!&�@ ��@ 1'@ b@�@��@�P@K�@�R@E�@{@�-@O�@�@�@��@�@j@I�@�@1@��@dZ@@�@�H@�H@��@~�@�@�@��@X@%@�`@Ĝ@�@A�@1'@b@�;@�@�P@|�@+@�R@�+@E�@5?@�T@�h@?}@�@I�@��@dZ@C�@33@"�@o@o@@�H@��@��@~�@M�@M�@�@J@��@�#@��@X@G�@7L@��@�9@�@�@�@r�@Q�@1'@ �@  @�w@|�@K�@;d@;d@;d@+@�@��@��@�y@�@�@��@v�@ff@E�@5?@$�@{@@@�@�T@�T@@�@`B@`B@O�@O�@?}@V@�@�/@��@�j@��@��@��@�D@�D@�D@�D@Z@��@��@�@33@
�!@
=q@	�@	�^@	��@	X@	G�@	�@��@�@r�@A�@1'@b@�@�@|�@K�@+@��@��@V@{@�@@�T111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB;dB:^B:^B:^B:^B:^B:^B:^B9XB:^BP�Bk�B�B�VB�PB��B��B��B�B�!B�B�B�B�FB�9B�9B�-B��B��B�uB�JB�Bv�Bq�Bp�Bn�BiyBaHB�B��B��B�hB�1B� B� BjBgmB_;BYBP�B:^B)�B �B�BoBDBB��B�B�NB�/B��BɺB�dB�!B��B��B�1B{�Bs�BaHB[#BE�B=qB7LB.B+B�BuBPBB
�B
�HB
��B
ǮB
�LB
��B
�hB
}�B
l�B
dZB
ZB
M�B
:^B
1'B
-B
"�B
�B
oB
+B
B	�B	�B	��B	B	�FB	��B	��B	{�B	l�B	^5B	XB	T�B	N�B	@�B	<jB	�B	PB	DB	+B��B�B�
BɺBƨBɺB�jB�B��B��B��B�hB�JB�+B}�B}�B�B�B� B|�Bz�Bz�By�Bv�Bo�Bm�Bk�BjBjBiyBiyBcTB_;B[#BZBW
BVBS�BR�BQ�BQ�BP�BN�BN�BK�BI�BG�BG�BG�BD�BD�BC�BC�BD�BC�BD�BB�B@�B>wB=qB=qB<jB:^B8RB7LB6FB5?B33B2-B33B1'B1'B1'B/B/B/B-B-B-B+B)�B)�B&�B$�B#�B"�B �B�B�B �B!�B!�B!�B"�B!�B#�B!�B"�B#�B%�B(�B(�B)�B)�B)�B)�B)�B+B+B+B+B,B.B-B.B0!B1'B2-B49B6FB6FB6FB9XB9XB:^B>wB@�BA�BF�BK�BM�BR�BVBXBYBZB\)B]/B^5B_;BcTBgmBjBk�Bm�Bl�Bl�Bk�Bl�Bn�Bo�Bp�Bt�Bv�Bw�By�B|�B�B�B�B�7B�VB�\B�bB�oB�{B��B��B��B��B��B�B�B�-B�?B�LB�dB��BǮB��B��B��B�B�;B�TB�`B�mB�B�B�B��B��B��B��B��B	  B	B	B	+B		7B	
=B	PB	{B	�B	�B	�B	!�B	(�B	+B	+B	-B	.B	1'B	33B	2-B	49B	49B	49B	7LB	9XB	;dB	=qB	B�B	D�B	F�B	I�B	J�B	M�B	N�B	O�B	P�B	R�B	S�B	S�B	T�B	VB	W
B	YB	[#B	\)B	\)B	]/B	cTB	e`B	ffB	gmB	gmB	jB	l�B	s�B	v�B	w�B	z�B	{�B	~�B	�B	�B	�%B	�%B	�+B	�1B	�JB	�PB	�PB	�VB	�\B	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�?B	�LB	�dB	�dB	�qB	�wB	�}B	��B	ĜB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B
DB
DB
DB
JB
JB
PB
VB
VB
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
hB
hB
hB
oB
oB
oB
uB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
!�B
"�B
!�B
!�B
!�B
"�B
"�B
#�B
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
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
,B
-B
-B
-B
-B
-B
.B
.B
/B
0!B
0!B
1'B
1'B
0!B
0!B
0!B
1'B
2-B
33B
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
5?B
5?B
5?B
5?B
5?B
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
<jB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
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
C�B
C�B
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
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
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
J�B
J�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
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
P�B
P�B
P�B
P�B
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
S�B
S�B
S�B
T�B
T�B
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
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
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
cTB
cTB
cTB
cTB
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
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B;0B;0B;0B;0B;0B;0B;0B;0B;0B;0B;0B;0B;0B;0B;0B;0B;0B;0B;0B:*B:*B:*B:*B:*B:*B:*B9$B:*BP�BkQB��B�"B�B�eB��B��B��B��B��B��B��B�B�B�B��B��B��B�@B��B��Bv�BqvBpoBncBiDBaB��B�KB�EB�B��B�B�BjKBg8B_BX�BP�B:*B)�B vB~B:B
�B�B��B�]B��B��B��B�lB�0B��B��B�QB��B{�BshBaBZ�BEmB=<B7B-�B*�B�B&BB�B
�UB
�B
ΊB
�zB
�B
��B
�4B
}�B
l=B
dB
Y�B
M�B
:*B
0�B
,�B
"�B
dB
:B
�B
 �B	�oB	��B	ѝB	�AB	��B	��B	�EB	{�B	l=B	^B	W�B	T�B	N�B	@4B	<6B	xB	B	B	�B��B�CB��B�lB�YB�lB�B��B��B�]B�9B�B��B��B}�B}�B��B��B�B|�Bz�Bz�By�Bv�BoOBmCBkQBj0Bj0Bi*Bi*Bc B_BZ�BY�BV�BU�BS�BR�BQ�BQ�BP�BN�BN�BKxBIlBG_BG_BG_BDMBDMBCGBCGBDMBCGBDMBBAB@4B>(B=<B="B<B:B8B6�B5�B4�B2�B1�B2�B0�B0�B0�B.�B.�B.�B,�B,�B,�B*�B)�B)�B&�B$tB#�B"�B vBjBjB vB!bB!|B!|B"�B!|B#�B!|B"�B#�B%�B(�B(�B)�B)�B)�B)�B)�B*�B*�B*�B*�B+�B-�B,�B-�B/�B0�B1�B3�B5�B5�B5�B9	B9	B:B>(B@4BA;BFYBK^BM�BR�BU�BW�BX�BY�B[�B\�B]�B^�BcBgBj0Bk6BmCBl=Bl"BkBl=BnIBoOBpUBtnBvzBw�By�B|�B��B��B��B��B�B�B�B� B�B�$B�=B�jB��B��B��B��B��B��B��B��B� B�EB�^BΊBуBյB��B�B��B�B�0B�;B�OB�nB��B�lB�B��B��B	�B	�B	�B	�B		�B	�B	,B	=B	]B	OB	!bB	(�B	*�B	*�B	,�B	-�B	0�B	2�B	1�B	3�B	3�B	3�B	6�B	9	B	;B	="B	B'B	DMB	FYB	IRB	JrB	M�B	N�B	O�B	P�B	R�B	S�B	S�B	T�B	U�B	V�B	X�B	Z�B	[�B	[�B	\�B	cB	eB	fB	gB	gB	j0B	l=B	shB	v`B	w�B	z�B	{B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�&B	� B	�&B	�B	�$B	�EB	�EB	�1B	�1B	�WB	�]B	�|B	��B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�(B	�.B	�B	�MB	�lB	�lB	�RB	�rB	�XB	�xB	�xB	�xB	�rB	ЗB	уB	ңB	өB	өB	өB	ԯB	ԯB	ԯB	յB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	��B	��B	�B	��B	�B	�*B	�0B	�6B	�6B	�=B	�)B	�CB	�CB	�CB	�/B	�IB	�OB	�UB	�;B	�[B	�AB	�[B	�aB	�aB	�MB	�MB	�MB	�MB	�nB	�zB	��B	��B	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
B
�B
B
�B
B
B
B
 B
B
B
B
 B
B
B
 B
&B
&B
B
&B
&B
B
,B
B
B
B
2B
?B
$B
?B
?B
$B
EB
KB
QB
QB
QB
WB
WB
=B
=B
WB
WB
]B
dB
dB
dB
IB
IB
OB
jB
jB
OB
VB
 vB
 \B
!|B
!bB
!bB
!bB
"�B
!|B
"hB
!|B
!|B
!|B
"�B
"�B
#�B
#nB
$tB
$�B
%�B
%�B
%�B
%�B
%�B
%zB
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
.�B
/�B
/�B
0�B
0�B
/�B
/�B
/�B
0�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
8B
8B
8B
8B
9	B
9	B
:B
;B
;B
;B
:�B
;B
<B
<B
<B
<B
<B
<B
<B
<B
<B
="B
>(B
>(B
>B
>(B
?.B
?.B
?.B
?.B
?B
?.B
@4B
@4B
A;B
A;B
B'B
BAB
BAB
B'B
BAB
B'B
B'B
BAB
B'B
BAB
B'B
BAB
B'B
CGB
CGB
CGB
CGB
CGB
D3B
DMB
DMB
D3B
E9B
E9B
E9B
E9B
ESB
ESB
ESB
ESB
ESB
FYB
FYB
F?B
F?B
FYB
F?B
FYB
FYB
G_B
G_B
G_B
HfB
HKB
HfB
HfB
HKB
IlB
IRB
IlB
JrB
JXB
JrB
JrB
KxB
K^B
L~B
LdB
LdB
L~B
M�B
MjB
MjB
N�B
N�B
NpB
O�B
OvB
O�B
P}B
P�B
P�B
P�B
P}B
P�B
P}B
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
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
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
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
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
a�B
a�B
a�B
cB
b�B
cB
cB
cB
cB
b�B
cB
cB
b�B
cB
c�B
c�B
c�B
dB
c�B
dB
c�B
dB
dB
eB
eB
eB
eB
eB
d�B
d�B
e�B
e�B
fB
e�B
fB
fB
fB
gB
gB
gB
gB
gB
gB
gB
gB
gB
gB
gB
gB
h$B
h$B
h$B
h
B
h$B
h$B
h$B
h$B
h$B
h$B
h$B
i*B
i*B
i*B
iB
iB
i*B
i*B
iB
j0B
jB
j0B
jB
jB
jB
j0B
j0B
j0B
j0B
j0B
k6B
k6B
k6B
k6B
k6B
l"B
l=B
l=B
l=B
mCB
m)B
m)B
nIB
n/B
oOB
oOB
oOB
oOB
o5B
oOB
oOB
o5B
oOB
oOB
pUB
p;B
pUB
q[B
q[B
rG111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.65(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810240041472018102400414720181024004147201810250037112018102500371120181025003711JA  ARFMdecpA19c                                                                20181019093515  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181019003541  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181019003543  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181019003543  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181019003544  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181019003544  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181019003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181019003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181019003544  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181019003544                      G�O�G�O�G�O�                JA  ARUP                                                                        20181019005543                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181019153707  CV  JULD            G�O�G�O�F�O�                JM  ARCAJMQC2.0                                                                 20181023154147  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181023154147  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181024153711  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                