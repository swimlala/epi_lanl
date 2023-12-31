CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-01T15:37:15Z creation;2020-01-01T15:37:19Z conversion to V3.1;2023-06-29T05:50:20Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Ml   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pX   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݐ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �`   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �t   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200101153715  20230705031507  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_200                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @���π1   @����}( @7����)�b�($x1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�C3D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D��3D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@���A
ffA*ffAJffAjffA�33A�33A�33A�33A�33A�33A�33A�33B��B
��B��B��B#  B*��B2��B:��BB��BJ��BR��BZ��Bb��Bj��Br��Bz��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C �fC�fC�fC�fC�fC
�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC �fC"�fC$�fC&�fC(�fC*�fC,�fC.�fC0�fC2�fC4�fC6�fC8�fC:�fC<�fC>�fC@�fCB�fCD�fCF�fCH�fCJ�fCL�fCN�fCP�fCR�fCT�fCV�fCX�fCZ�fC\�fC^�fC`�fCb�fCd�fCf�fCh�fCj�fCl�fCn�fCp�fCr�fCt�fCv�fCx�fCz�fC|�fC~�fC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�FfC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�` C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3D )�D ��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D	)�D	��D
)�D
��D)�D��D)�D�3D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D )�D ��D!)�D!��D")�D"��D#)�D#��D$)�D$��D%)�D%��D&)�D&��D')�D'��D()�D(� D))�D)��D*)�D*��D+)�D+��D,)�D,��D-)�D-��D.)�D.��D/)�D/��D0)�D0��D1)�D1��D2)�D2��D3)�D3��D4)�D4��D5)�D5��D6)�D6��D7)�D7��D8)�D8��D9)�D9��D:)�D:��D;)�D;��D<)�D<��D=)�D=��D>)�D>��D?)�D?��D@)�D@��DA)�DA��DB)�DB��DC)�DC��DD)�DD��DE)�DE��DF)�DF��DG)�DG��DH)�DH��DI)�DI��DJ)�DJ��DK)�DK��DL)�DL��DM)�DM��DN)�DN��DO)�DO��DP)�DP��DQ)�DQ��DR)�DR��DS)�DS��DT)�DT��DU)�DU��DV)�DV��DW)�DW��DX)�DX��DY)�DY��DZ)�DZ��D[)�D[��D\)�D\��D])�D]��D^)�D^��D_)�D_��D`)�D`��Da)�Da��Db)�Db��Dc)�Dc��Dd)�Dd��De)�De��Df)�Df��Dg)�Dg��Dh)�Dh�3Di)�Di��Dj)�Dj��Dk)�Dk��Dl)�Dl��Dm)�Dm��Dn)�Dn��Do)�Do��Dp)�Dp��Dq)�Dq��Dr)�Dr��Ds)�Ds��Dt)�Dt��Du)�Du��Dv)�Dv��Dw)�Dw��Dx)�Dx��Dy)�Dy��Dz)�Dz��D{)�D{��D|)�D|��D})�D}��D~)�D~��D)�D��D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D�њD��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D�D���D��D�T�DÔ�D���D��D�T�DĔ�D���D��D�T�DŔ�D���D��D�T�DƔ�D���D��D�T�Dǔ�D���D��D�T�DȔ�D���D��D�T�Dɔ�D���D��D�T�Dʔ�D���D��D�T�D˔�D���D��D�X D̔�D���D��D�T�D͔�D���D��D�T�DΔ�D���D��D�T�Dϔ�D���D��D�T�DД�D���D��D�T�Dє�D���D��D�T�DҔ�D���D��D�T�DӔ�D���D��D�T�DԔ�D���D��D�T�DՔ�D���D��D�T�D֔�D���D��D�T�Dה�D���D��D�T�Dؔ�D���D��D�T�Dٔ�D���D��D�T�Dڔ�D���D��D�T�D۔�D���D��D�T�Dܔ�D���D��D�T�Dݔ�D���D��D�T�Dޔ�D���D��D�T�Dߔ�D�� D��D�T�D���D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D�њD��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�X D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%A�
=A�%A�A�  A���A�bA�"�A��A�"�A�(�A�&�A�(�A�+A�+A�+A�-A�-A�-A�-A�-A�/A�/A�1'A�1'A�/A�/A�1'A�33A�33A�33A�33A�5?A�5?A�5?A�7LA�7LA�9XA�;dA�;dA�=qA�33A�7LA�;dA�A�A�C�A�C�A�A�A�1'A�&�A�{A�bA�JA���A�ZA���A�
=A���A��A�VA���A�I�A� �A�dZA��A�E�A��hA��hA�
=A�`BA�JA�p�A�oA�r�A��TA�1'A��A��;A�r�A���A�&�A��A�{A���A�VA��^A���A���A�VA���A��A�l�A�VA���A���A�bA���A�dZA�ȴA�z�A�&�A�^5A��A��DA�jA���A��A���A��7A�oA�r�A���A�l�A�ĜA���A�K�A��A{�Awl�Au��As�
Ap�jAn�Am�wAi��Ae�TAd��Ac�FAb�A`�RA^n�A\9XAX�AX9XAV�jAS��ARffAQ��AQ"�AP$�AN^5AKl�AI�;AH�AG�wAG/AFI�AE�#AD�AA�hAA;dA@=qA>�9A=ƨA<�jA:~�A9�A8��A7�
A6v�A4v�A2�uA1�A0M�A/�A.v�A,VA+�A*E�A)`BA(�A'�A&��A%��A%7LA#�TA"r�A!%A7LA1AM�A��A�AA-A��A��A7LA�A�yAjA�wAS�A^5AXA�uA��Av�Ax�A"�A�AbAXA
��A
n�A
M�A
A	��A	;dAE�AA|�A��An�A�A��A&�A�DA�A ȴA I�@�n�@��^@�&�@��@�|�@���@�t�@�ff@��`@�bN@��@�@��/@�ƨ@���@�&�@��@��@�V@�
=@��;@��@�&�@���@�n�@�bN@���@�E�@���@�`B@���@�(�@׍P@ם�@�?}@�1@�33@��@��@�n�@�@�&�@��@�~�@Ο�@́@̓u@̴9@�1@��@�K�@�5?@�J@��#@Ɨ�@���@��@�-@�v�@�p�@� �@�l�@+@�M�@��^@�x�@��j@� �@�ȴ@�ff@��@�`B@��@���@�t�@�
=@��T@�|�@��+@�v�@��;@��@�|�@�dZ@�ff@��h@��/@�I�@��j@�p�@��@���@�x�@��u@��j@���@�@���@�t�@���@���@�%@��@���@��@��m@�+@�-@�v�@�5?@��j@� �@�V@��@�~�@���@�@�^5@��@��@�l�@�33@�\)@��w@��
@���@�|�@�o@�"�@�{@�G�@���@�~�@�v�@���@�@���@�^5@��@��+@�v�@�=q@��^@��h@�Ĝ@�I�@��F@��@�;d@��w@��@�b@��y@�X@�7L@��j@��@�|�@�\)@���@���@���@�J@���@��@��9@��@�7L@�Q�@���@���@�M�@��R@��@�@�dZ@�C�@��\@��7@���@��@��@��/@��@�p�@��h@�@�/@��/@��D@��@�;d@�~�@���@���@�M�@�`B@�/@���@�Ĝ@��D@�1'@�(�@��
@���@�C�@��@���@��+@�ff@�^5@�E�@�@���@��7@�hs@�G�@�?}@�V@��/@��9@���@�r�@��@���@��m@��w@���@��P@�|�@�S�@�@��H@���@��\@�E�@�-@��@��#@��h@�p�@�O�@��@��`@���@�r�@�Z@�9X@�  @���@�dZ@�
=@���@�ȴ@��R@���@���@�v�@�M�@�{@��#@���@�O�@���@��`@��@�Z@�1'@�b@��m@���@��F@���@�l�@�"�@���@���@��+@�=q@�5?@�J@��-@���@��h@��@�`B@�7L@��j@��@���@��D@�z�@�r�@�r�@�j@�9X@�;@|�@�@~��@~V@~@}��@}?}@|�D@{ƨ@{�@{33@z�@z�!@z~�@z�@y��@yhs@x�u@xA�@xb@w�@w�@wK�@v�R@v�+@vE�@v{@u�T@u�-@u�h@uO�@t��@t�D@tZ@t9X@t1@s�
@sC�@r�@r��@r=q@r-@q��@qx�@q7L@p�`@p�u@p�@o�;@o|�@oK�@o�@n�y@n��@n$�@m��@m�@m/@l��@l9X@l1@k�
@k��@kS�@ko@j�@j~�@i��@ix�@i7L@i%@h�`@hĜ@hA�@g+@f�@f��@fE�@e@d��@dz�@dj@d9X@d1@c33@b��@b��@b�\@b-@a��@a�7@a�@`��@`r�@`bN@`A�@`b@_�;@_�w@_K�@^�y@^ff@^E�@^5?@^5?@^@]`B@\�@\9X@[ƨ@[t�@[S�@[S�@[33@[@Z��@Zn�@Z-@Y�#@Y7L@X��@X��@XbN@W��@W�w@W�P@WK�@Vȴ@V��@V�+@Vv�@VE�@U�@U�h@UO�@T�@T�j@TZ@T1@S�
@St�@SC�@So@S@R��@R~�@R=q@RJ@Q��@Q�@Q�#@Q��@Q%@P��@P�@PbN@P �@O��@O��@Ol�@O;d@N��@N��@NE�@N@M��@M`B@M?}@L��@L�j@LZ@L(�@Kƨ@KdZ@KS�@KC�@Ko@Jn�@JJ@I�^@Ihs@I�@H��@Hr�@H �@G�w@G|�@GK�@G
=@Fȴ@F�+@Fv�@F@E`B@D�/@D��@D�@C�m@Ct�@C33@B�H@B~�@B�@BJ@A��@A�^@A��@A�7@Ahs@Ahs@Ahs@A�@@�u@@bN@@1'@?�;@?�@?l�@?;d@>��@>��@>$�@=@=?}@<j@;��@;��@;�@;C�@;o@:��@:^5@9�@9�^@9��@9&�@9�@9%@8�`@8�u@8Q�@81'@7��@7�P@7;d@6�R@6v�@6{@5�h@5p�@5`B@5V@4��@4�@4(�@3�
@3�@3S�@2�@2��@2��@2M�@2J@1�^@1�7@1&�@0��@0�9@0bN@0 �@/�;@/��@.��@.��@.v�@.{@-��@-��@-/@,��@,(�@,9X@+�m@+��@+��@+dZ@+C�@+"�@*�@*��@*~�@*^5@*^5@*�@)��@)�@)��@)x�@)7L@(��@(Ĝ@(��@(�@(r�@(bN@(A�@(b@'��@'l�@'�@&�@&�R@&��@&ff@&V@&$�@%��@%�h@%`B@%�@$��@$�j@$�D@$Z@$1@#��@#��@#�@#C�@#o@"��@"��@"~�@"~�@"~�@"n�@"=q@"J@!��@!�7@!X@!&�@!%@ Ĝ@ �@ Q�@   @�;@�@�P@;d@;d@�@ff@V@�T@�-@��@p�@?}@��@��@j@I�@�m@t�@C�@33@"�@o@�@�!@��@~�@^5@=q@J@�#@��@�^@�7@�7@��@��@7L@��@&�@Ĝ@�@A�@�@�@�P@�P@|�@\)@\)@K�@;d@
=@ȴ@��@��@��@E�@5?@{@�@@��@p�@`B@`B@/@��@�/@�@I�@�@�m@ƨ@��@C�@o@o@@~�@^5@M�@=q@-@-@�@��@�#@�7@G�@&�@%@��@��@��@r�@ �@�;@��@�@��@l�@+@�@
=@�y@ȴ@ȴ@��@�+@5?@@�@�@V@�j@��@j@I�@(�@��@�
@ƨ@��@S�@33@33@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%A�
=A�%A�A�  A���A�bA�"�A��A�"�A�(�A�&�A�(�A�+A�+A�+A�-A�-A�-A�-A�-A�/A�/A�1'A�1'A�/A�/A�1'A�33A�33A�33A�33A�5?A�5?A�5?A�7LA�7LA�9XA�;dA�;dA�=qA�33A�7LA�;dA�A�A�C�A�C�A�A�A�1'A�&�A�{A�bA�JA���A�ZA���A�
=A���A��A�VA���A�I�A� �A�dZA��A�E�A��hA��hA�
=A�`BA�JA�p�A�oA�r�A��TA�1'A��A��;A�r�A���A�&�A��A�{A���A�VA��^A���A���A�VA���A��A�l�A�VA���A���A�bA���A�dZA�ȴA�z�A�&�A�^5A��A��DA�jA���A��A���A��7A�oA�r�A���A�l�A�ĜA���A�K�A��A{�Awl�Au��As�
Ap�jAn�Am�wAi��Ae�TAd��Ac�FAb�A`�RA^n�A\9XAX�AX9XAV�jAS��ARffAQ��AQ"�AP$�AN^5AKl�AI�;AH�AG�wAG/AFI�AE�#AD�AA�hAA;dA@=qA>�9A=ƨA<�jA:~�A9�A8��A7�
A6v�A4v�A2�uA1�A0M�A/�A.v�A,VA+�A*E�A)`BA(�A'�A&��A%��A%7LA#�TA"r�A!%A7LA1AM�A��A�AA-A��A��A7LA�A�yAjA�wAS�A^5AXA�uA��Av�Ax�A"�A�AbAXA
��A
n�A
M�A
A	��A	;dAE�AA|�A��An�A�A��A&�A�DA�A ȴA I�@�n�@��^@�&�@��@�|�@���@�t�@�ff@��`@�bN@��@�@��/@�ƨ@���@�&�@��@��@�V@�
=@��;@��@�&�@���@�n�@�bN@���@�E�@���@�`B@���@�(�@׍P@ם�@�?}@�1@�33@��@��@�n�@�@�&�@��@�~�@Ο�@́@̓u@̴9@�1@��@�K�@�5?@�J@��#@Ɨ�@���@��@�-@�v�@�p�@� �@�l�@+@�M�@��^@�x�@��j@� �@�ȴ@�ff@��@�`B@��@���@�t�@�
=@��T@�|�@��+@�v�@��;@��@�|�@�dZ@�ff@��h@��/@�I�@��j@�p�@��@���@�x�@��u@��j@���@�@���@�t�@���@���@�%@��@���@��@��m@�+@�-@�v�@�5?@��j@� �@�V@��@�~�@���@�@�^5@��@��@�l�@�33@�\)@��w@��
@���@�|�@�o@�"�@�{@�G�@���@�~�@�v�@���@�@���@�^5@��@��+@�v�@�=q@��^@��h@�Ĝ@�I�@��F@��@�;d@��w@��@�b@��y@�X@�7L@��j@��@�|�@�\)@���@���@���@�J@���@��@��9@��@�7L@�Q�@���@���@�M�@��R@��@�@�dZ@�C�@��\@��7@���@��@��@��/@��@�p�@��h@�@�/@��/@��D@��@�;d@�~�@���@���@�M�@�`B@�/@���@�Ĝ@��D@�1'@�(�@��
@���@�C�@��@���@��+@�ff@�^5@�E�@�@���@��7@�hs@�G�@�?}@�V@��/@��9@���@�r�@��@���@��m@��w@���@��P@�|�@�S�@�@��H@���@��\@�E�@�-@��@��#@��h@�p�@�O�@��@��`@���@�r�@�Z@�9X@�  @���@�dZ@�
=@���@�ȴ@��R@���@���@�v�@�M�@�{@��#@���@�O�@���@��`@��@�Z@�1'@�b@��m@���@��F@���@�l�@�"�@���@���@��+@�=q@�5?@�J@��-@���@��h@��@�`B@�7L@��j@��@���@��D@�z�@�r�@�r�@�j@�9X@�;@|�@�@~��@~V@~@}��@}?}@|�D@{ƨ@{�@{33@z�@z�!@z~�@z�@y��@yhs@x�u@xA�@xb@w�@w�@wK�@v�R@v�+@vE�@v{@u�T@u�-@u�h@uO�@t��@t�D@tZ@t9X@t1@s�
@sC�@r�@r��@r=q@r-@q��@qx�@q7L@p�`@p�u@p�@o�;@o|�@oK�@o�@n�y@n��@n$�@m��@m�@m/@l��@l9X@l1@k�
@k��@kS�@ko@j�@j~�@i��@ix�@i7L@i%@h�`@hĜ@hA�@g+@f�@f��@fE�@e@d��@dz�@dj@d9X@d1@c33@b��@b��@b�\@b-@a��@a�7@a�@`��@`r�@`bN@`A�@`b@_�;@_�w@_K�@^�y@^ff@^E�@^5?@^5?@^@]`B@\�@\9X@[ƨ@[t�@[S�@[S�@[33@[@Z��@Zn�@Z-@Y�#@Y7L@X��@X��@XbN@W��@W�w@W�P@WK�@Vȴ@V��@V�+@Vv�@VE�@U�@U�h@UO�@T�@T�j@TZ@T1@S�
@St�@SC�@So@S@R��@R~�@R=q@RJ@Q��@Q�@Q�#@Q��@Q%@P��@P�@PbN@P �@O��@O��@Ol�@O;d@N��@N��@NE�@N@M��@M`B@M?}@L��@L�j@LZ@L(�@Kƨ@KdZ@KS�@KC�@Ko@Jn�@JJ@I�^@Ihs@I�@H��@Hr�@H �@G�w@G|�@GK�@G
=@Fȴ@F�+@Fv�@F@E`B@D�/@D��@D�@C�m@Ct�@C33@B�H@B~�@B�@BJ@A��@A�^@A��@A�7@Ahs@Ahs@Ahs@A�@@�u@@bN@@1'@?�;@?�@?l�@?;d@>��@>��@>$�@=@=?}@<j@;��@;��@;�@;C�@;o@:��@:^5@9�@9�^@9��@9&�@9�@9%@8�`@8�u@8Q�@81'@7��@7�P@7;d@6�R@6v�@6{@5�h@5p�@5`B@5V@4��@4�@4(�@3�
@3�@3S�@2�@2��@2��@2M�@2J@1�^@1�7@1&�@0��@0�9@0bN@0 �@/�;@/��@.��@.��@.v�@.{@-��@-��@-/@,��@,(�@,9X@+�m@+��@+��@+dZ@+C�@+"�@*�@*��@*~�@*^5@*^5@*�@)��@)�@)��@)x�@)7L@(��@(Ĝ@(��@(�@(r�@(bN@(A�@(b@'��@'l�@'�@&�@&�R@&��@&ff@&V@&$�@%��@%�h@%`B@%�@$��@$�j@$�D@$Z@$1@#��@#��@#�@#C�@#o@"��@"��@"~�@"~�@"~�@"n�@"=q@"J@!��@!�7@!X@!&�@!%@ Ĝ@ �@ Q�@   @�;@�@�P@;d@;d@�@ff@V@�T@�-@��@p�@?}@��@��@j@I�@�m@t�@C�@33@"�@o@�@�!@��@~�@^5@=q@J@�#@��@�^@�7@�7@��@��@7L@��@&�@Ĝ@�@A�@�@�@�P@�P@|�@\)@\)@K�@;d@
=@ȴ@��@��@��@E�@5?@{@�@@��@p�@`B@`B@/@��@�/@�@I�@�@�m@ƨ@��@C�@o@o@@~�@^5@M�@=q@-@-@�@��@�#@�7@G�@&�@%@��@��@��@r�@ �@�;@��@�@��@l�@+@�@
=@�y@ȴ@ȴ@��@�+@5?@@�@�@V@�j@��@j@I�@(�@��@�
@ƨ@��@S�@33@33@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
B
B
��B
��B
��B
B
B
��B
��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
�qB
��B
��B
��B
B
B
B
B
B
��B
��B
��B
B
B
B
B
B
��B
��B
��B
B
B
B
B
��B
�}B
�}B
�}B
�wB
�qB%�BI�BYBcTB[#B_;Bx�B��B�;B�fB�TB�ZB��B{B�B�B�B�B&�B,B:^BH�BT�BS�BS�BR�BR�BI�B33BPB��B+BDB
=BPBhB��B�`BB��B�B�mB��BŢB��B�\Bo�B_;B@�B�B%B
�NB
�wB
��B
�=B
��B
�'B
�7B
jB
l�B
VB
@�B
�B	��B	��B	�XB	��B	�\B	|�B	y�B	e`B	F�B	F�B	C�B	:^B	2-B	#�B	�B	B	
=B	oB	B��B��B��B�B�mB�#B��B��BȴBĜB�}B�jB�^B�B��B��B��B��B��B�{B�VB�JB�7B�%B� By�Bw�Bw�Bt�Bo�Bk�BgmBe`BbNB`BB]/B[#BYBXBS�BP�BO�BM�BS�BM�BJ�BJ�BJ�BH�BD�BE�BF�BH�BH�BI�BK�BJ�BM�BO�BN�BL�BL�BI�BH�BG�BG�BE�BJ�BO�BW
B[#BbNBbNB`BBdZBe`B_;B]/B_;B_;BYBW
BYBZB\)BZBW
BXBVB^5BYBS�BP�BQ�BW
B^5B]/B]/B_;B_;B]/BcTBcTBgmBbNB[#B\)B]/B\)BXBN�BI�BI�BM�BM�BM�BO�BQ�B\)B[#BXBW
BXB]/B`BB_;B^5B[#B\)BaHBe`BdZBk�Bo�Bm�BffBk�Bn�Bq�B}�B�B�+B�+B�JB�DB�+B�+B�=B�VB�hB�oB��B��B��B�B�^B�RB�3B�RB�}BŢBĜB��B��B��B�HB�fB�yB�B�B�B�B�yB�5B�;B�mB�fB�B�mB�B�B�fB�sB��B��B��B�B�`B�;B�fB�B�B�B�B�B	B	JB	uB	{B	oB	JB	VB	�B	�B	�B	�B	"�B	%�B	)�B	/B	5?B	8RB	:^B	A�B	A�B	@�B	C�B	O�B	Q�B	P�B	P�B	Q�B	W
B	aHB	cTB	cTB	e`B	e`B	ffB	cTB	cTB	aHB	ZB	T�B	ZB	^5B	^5B	]/B	YB	YB	ZB	`BB	^5B	_;B	cTB	ffB	ffB	ffB	ffB	hsB	iyB	k�B	p�B	m�B	m�B	m�B	n�B	q�B	t�B	x�B	|�B	}�B	|�B	{�B	{�B	|�B	� B	�B	�+B	�DB	�\B	�hB	�oB	�uB	�uB	�hB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�qB	�qB	�wB	�wB	��B	��B	��B	B	ÖB	ĜB	ŢB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�)B	�/B	�;B	�;B	�BB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
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
1B
	7B
	7B
	7B

=B

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
\B
\B
bB
bB
bB
hB
hB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
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
+B
,B
,B
,B
,B
-B
-B
-B
-B
.B
.B
/B
/B
0!B
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
2-B
33B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
=qB
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
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
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
K�B
L�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
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
P�B
Q�B
Q�B
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
T�B
VB
VB
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
ZB
[#B
[#B
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
^5B
^5B
]/B
]/B
]/B
^5B
^5B
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
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
hsB
hsB
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
n�B
o�B
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
o�B
p�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�UB
�[B
�[B
�UB
�UB
�;B
�AB
�[B
�UB
�UB
�[B
�[B
�[B
�[B
�[B
�[B
�[B
�[B
�[B
�[B
�[B
�[B
�[B
�[B
�VB
�UB
�UB
�UB
�[B
�[B
�[B
�[B
�[B
�UB
�UB
�UB
�[B
�[B
�[B
�[B
�uB
�UB
�UB
�UB
�[B
�[B
�uB
�uB
�iB
�}B
��B
�OB
�oB
ǔB(>BL~B]�Bg�B]�Bb4B{JB��B�B��B��B�2B�0B�B�BKB�BB)�B-]B<BKDBV�BT�BU2BT{BT�BM6B7�B�B�qBfBBDB�B@B��B��BSB�dB��B�eB��B��B��B� Bq�BbNBC�B"�B	�B
�B
��B
��B
��B
��B
�TB
��B
l�B
oOB
Y1B
D3B
$B	�$B	��B	��B	�"B	��B	HB	~BB	h�B	H1B	G�B	EmB	<6B	4�B	&�B	�B	?B	JB	B	SB��B��B�LB��B�BܬB��B�B�RBňB�iB�BB�B��B�6B�sB��B�5B��B��B�(B�jB��B��B��Bz�By�Bx�Bv�Bq�Bl�BhXBffBc Ba-B^OB\CBZBY�BU�BR�BQ�BOvBU�BN�BLBK�BL�BJ=BESBF%BF�BH�BI7BJrBLJBK�BN�BP�BO�BNBM�BJ	BI7BHKBHfBF?BJ�BO�BW?B[�Bb�Bc:B`�Bd�Be�B_VB]�BaHB`�BY�BXBY�BZ�B]BZkBWYBX�BV�B_�BY�BT�BQ�BR:BW�B^�B]�B]�B_�B`B]�Bc�Bd&BiDBc�B[�B\�B]~B]�BYBO�BI�BI�BM�BNBM�BO�BR B]/B[�BX_BW
BW�B]IB`vB_�B_!B[WB\)Ba�Be�BdZBlBp�Bn�Bf�BkQBncBq'B}�B��B�EB�B��B��B�zB�zB�=B�pB�hB��B��B�fB��B��B��B��B��B�RB��BŢB��B� B�uB�,B��B�fB�yB��B��B��B�B��BݘB��B�RB�LB�B�mB�B�B��B�B��B�XB�zB�B��B��B�fB�B�B��B�B�AB	�B	�B	�B	�B	�B	B	"B	$B	QB	?B	�B	"NB	%zB	)�B	.�B	5?B	8RB	:DB	A�B	A�B	?�B	B�B	O�B	Q�B	P�B	PbB	QNB	V�B	aB	c B	c:B	ezB	e`B	f�B	cnB	c�B	b�B	ZkB	T�B	Y�B	^5B	^�B	]�B	X�B	Y1B	Z7B	`\B	^B	^�B	c:B	f�B	f�B	ffB	f�B	hXB	h�B	kkB	p�B	m�B	m�B	m]B	m�B	q'B	t9B	xlB	|�B	~BB	}<B	{�B	{�B	|�B	�B	��B	��B	��B	�B	��B	�oB	�[B	��B	��B	�oB	��B	�B	�IB	��B	�B	�eB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�2B	�B	�$B	�*B	�0B	�6B	�<B	�<B	�]B	�]B	�4B	�OB	�oB	�uB	�GB	āB	�mB	ňB	�zB	ȀB	ɆB	ˬB	̳B	̈́B	͟B	οB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�)B	�B	��B	�B	�B	��B	�B	�B	� B	�&B	�&B	�@B	�FB	�LB	�$B	�>B	�XB	�*B	�_B	�eB	�QB	�6B	�6B	�WB	�qB	�B	�5B	�OB	�iB	�iB	�iB	�UB	�oB	�oB	��B	�B	�B	�|B	�B	�nB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
�B
	B
	B
	�B

	B
B
)B
)B
B
B
0B
B
B
B
"B
(B
(B
BB
HB
.B
.B
4B
4B
4B
hB
oB
:B
&B
@B
[B
aB
aB
FB
,B
FB
�B
SB
SB
SB
SB
?B
YB
yB
eB
KB
KB
eB
eB
eB
eB
�B
�B
kB
QB
QB
QB
QB
�B
�B
�B
�B
xB
dB
dB
~B
dB
dB
~B
jB
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
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
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
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
-�B
.�B
.�B
/�B
/�B
0B
/�B
0�B
0�B
0�B
0�B
1�B
1�B
1�B
1�B
3B
3B
4B
4B
4B
5B
5B
4�B
6B
6B
6�B
6�B
7B
7B
7B
8B
8B
8B
9$B
:*B
:B
:B
:B
;0B
;0B
<6B
<6B
=<B
=VB
=VB
>]B
=qB
=VB
=<B
="B
=<B
>BB
>]B
?HB
?cB
?HB
?HB
?HB
@4B
A;B
A;B
AUB
AUB
AUB
B[B
CaB
C{B
C{B
CaB
C{B
C{B
DMB
DgB
EmB
ESB
E�B
F�B
FtB
G_B
GzB
GzB
GzB
GzB
H�B
H�B
HfB
H�B
H�B
I�B
I�B
I�B
J�B
J�B
JrB
J�B
K�B
KxB
K�B
L�B
L~B
L�B
L�B
M�B
L~B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
OvB
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
S�B
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
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
Y�B
Z�B
Z�B
Y�B
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
\�B
\�B
\�B
]�B
]�B
_B
^�B
_B
_B
^�B
^�B
`B
_�B
_�B
`B
_�B
_�B
aB
aB
aB
`�B
bB
a�B
bB
bB
a�B
bB
a�B
cB
c B
cB
cB
cB
d&B
d&B
e,B
eB
d�B
eB
fB
fB
gB
g8B
h>B
h>B
i*B
iB
i*B
i*B
i*B
iDB
iDB
jKB
jKB
jKB
j0B
j0B
jKB
j0B
jKB
kQB
kQB
k6B
k6B
l"B
l=B
l=B
lWB
lWB
l=B
lWB
lWB
lWB
l=B
m]B
m]B
mCB
ncB
ncB
n/B
ncB
nIB
ncB
ncB
oOB
nIB
nIB
oOB
oOB
oiB
oiB
oiB
oiB
oiB
o�B
n}B
ncB
nIB
ncB
ncB
oiB
oOB
oOB
oiB
oiB
oiB
pUB
pUB
poB
pU111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.65(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001070034392020010700343920200107003439202306231719542023062317195420230623171954202001080027332020010800273320200108002733  JA  ARFMdecpA19c                                                                20200102003713  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200101153715  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200101153717  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200101153717  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200101153718  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200101153718  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200101153718  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200101153718  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200101153719  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200101153719                      G�O�G�O�G�O�                JA  ARUP                                                                        20200101155411                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20200101153345  CV  JULD            G�O�G�O�Fǿ                JM  ARCAJMQC2.0                                                                 20200106153439  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200106153439  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200107152733  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623081954  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705031507                      G�O�G�O�G�O�                