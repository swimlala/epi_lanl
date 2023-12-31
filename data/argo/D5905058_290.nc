CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2021-01-20T09:41:03Z creation;2021-01-20T09:41:05Z conversion to V3.1;2023-06-29T05:47:27Z update;     
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
resolution        =���   axis      Z        L  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IP   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \p   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  sd   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     L  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ڨ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20210120094103  20230705041505  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              "A   JA  I2_0675_290                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�X� 1   @�X��� @6~��O�;�b���#�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�3D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�C3D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@љ�A
ffA*ffAJffAjffA�33A�33A�33A�33A�33A�33A�33A�33B��B
��B��B��B"��B*��B2��B:��BB��BJ��BR��BZ��Bb��Bj��Br��Bz��B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B� B�L�B�L�B�L�B�L�B�L�C �fC�fC�fC�fC�fC
�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC �fC"�fC$�fC&�fC(�fC*�fC,�fC.�fC0�fC2�fC4�fC6�fC8�fC:�fC<�fC>�fC@�fCB�fCD�fCF�fCH�fCJ�fCL�fCN�fCP�fCR�fCT�fCV�fCX��CZ�fC\�fC^�fC`�fCb�fCd�fCf� Ch�fCj�fCl�fCn�fCp�fCr�fCt�fCv�fCx�fCz�fC|�fC~�fC�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�` C�` C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3C�S3D )�D ��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D	)�D	��D
)�D
��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D#3D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D)�D��D )�D ��D!)�D!��D")�D"��D#)�D#��D$)�D$��D%)�D%��D&)�D&��D')�D'��D()�D(��D))�D)��D*)�D*��D+)�D+��D,)�D,��D-)�D-��D.)�D.��D/)�D/��D0)�D0�3D1)�D1��D2)�D2��D3)�D3��D4)�D4��D5)�D5��D6)�D6��D7)�D7��D8)�D8��D9)�D9��D:)�D:��D;)�D;��D<)�D<��D=)�D=��D>)�D>��D?)�D?��D@)�D@��DA)�DA��DB)�DB��DC)�DC��DD)�DD��DE)�DE��DF)�DF��DG)�DG��DH)�DH��DI)�DI��DJ)�DJ��DK)�DK��DL)�DL��DM)�DM��DN)�DN��DO)�DO��DP)�DP��DQ)�DQ��DR)�DR��DS)�DS��DT)�DT��DU)�DU��DV)�DV��DW)�DW��DX)�DX��DY)�DY��DZ)�DZ��D[)�D[��D\)�D\��D])�D]��D^)�D^��D_)�D_��D`)�D`��Da)�Da��Db)�Db��Dc)�Dc��Dd)�Dd��De)�De��Df)�Df��Dg)�Dg��Dh)�Dh��Di)�Di��Dj)�Dj��Dk)�Dk��Dl)�Dl��Dm)�Dm��Dn)�Dn��Do)�Do��Dp)�Dp��Dq)�Dq��Dr)�Dr��Ds)�Ds��Dt)�Dt��Du)�Du��Dv)�Dv��Dw)�Dw��Dx)�Dx��Dy)�Dy��Dz)�Dz��D{)�D{��D|)�D|��D})�D}��D~)�D~��D)�D��D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�X D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�Q�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D�њD��D�T�D���D���D��D�T�D���D���D��D�Q�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D�њD��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D�њD��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D���D���D��D�T�D�D���D��D�T�DÔ�D���D��D�T�DĔ�D���D��D�T�DŔ�D���D��D�T�DƔ�D���D��D�T�Dǔ�D���D��D�T�DȔ�D���D��D�T�Dɔ�D���D��D�T�Dʔ�D���D��D�T�D˔�D���D��D�T�D̔�D���D� D�T�D͔�D���D��D�T�DΔ�D���D��D�T�Dϔ�D���D��D�T�DД�D���D��D�T�Dє�D���D��D�T�DҔ�D���D��D�T�DӔ�D���D��D�T�DԔ�D���D��D�T�DՔ�D���D��D�T�D֔�D���D��D�T�Dה�D���D��D�T�Dؔ�D���D��D�T�Dٔ�D���D��D�T�Dڔ�D���D��D�T�D۔�D���D��D�T�Dܔ�D���D��D�T�Dݔ�D���D��D�T�Dޔ�D���D��D�T�Dߔ�D���D��D�X D�� D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D��D���D��D�T�D���D�� D�!�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��yA��A��A��A��`A��yA��A���A��A��mA��`A��mA��TA��HA��TA��yA��A�A�%A�%A�%A�%A�JA�VA�oA�oA� �A�1'A�9XA�C�A�I�A�I�A�?}A�5?A�5?A�/A�&�A��A�
=A��A���Aĩ�A�bNA���A��A���A��^A�ffA�/A��7A�I�A���A�ĜA�XA��^A��A���A��A�=qA�1'A�p�A��A���A�?}A�oA�ĜA���A�1'A��7A�7LA�`BA��RA�z�A��A��A�oA��FA��A���A�z�A���A���A��mA�ffA���A���A��A�VA�7LA�n�A��A��A��A��\A�bA���A��DA��A��9A�1A�t�A�
=A��uA�33A�O�A�1'A�z�A���A��`A�{A�Q�A���A~�AyC�Avr�At-Ap�uAlM�Ah�jAe�;Ac|�A_C�A]�^AZM�AW��AW33AVn�AU��AS�APbAM�AI/AF�HAD�DAC/ABM�A@z�A?�-A>�A<�A;t�A:�jA9�FA8ȴA5�A37LA2�`A2ȴA2�A2  A0�A/��A.�DA-33A,r�A+��A+G�A+A*ȴA)hsA(A&E�A%��A%G�A$�A#�A#`BA"z�A!��A!p�A ��A ^5A �A�wAoAA�A�FAoA�DA��A��A��A��A  AbA��A&�A1Ax�A��A�+AJA�7A�AbNAO�A��A�A33A	��AȴA�A��AdZA�AffA1AS�A$�AƨA��AhsAG�A/A ��A ��@��@�E�@�j@��!@���@�r�@�~�@�x�@�P@�M�@��@�{@�h@�1'@��@�/@�@�`B@���@�;d@��H@�+@�O�@�bN@ߝ�@��@�ȴ@�V@�(�@۶F@�dZ@ڸR@��@�%@�dZ@�V@Ձ@��@ԋD@�@�@�  @θR@�J@˥�@ə�@Ȭ@�b@�+@Ɵ�@�?}@þw@�@�7L@���@�A�@��
@�33@��R@�-@���@�G�@���@��j@���@�A�@���@���@�p�@���@���@�j@� �@���@��m@��
@�S�@��u@��@��T@���@��@��@�p�@��@���@�bN@�1'@�  @���@�"�@��+@�@��-@���@�hs@�O�@�&�@�&�@�V@��9@�A�@�I�@�  @���@�t�@��@���@�Z@��;@���@�o@��@���@���@���@��\@�E�@���@�x�@�?}@�V@��`@�j@��@��m@��@�S�@���@��+@��@��^@���@�x�@�/@��@�V@��/@��9@�r�@�(�@��P@��H@���@�v�@�5?@��#@��@��@��`@���@�Ĝ@��D@�(�@��@�  @��m@��w@���@�dZ@�C�@�o@��@��@��!@��\@�ff@�-@��@�@���@��7@�O�@�%@��/@���@��D@�(�@�ƨ@�K�@�33@��@���@��!@���@�v�@�E�@�J@��#@��-@��@�?}@�/@���@��/@��j@�r�@�1'@�1@��@��@�t�@�C�@�"�@���@�ff@�^5@�{@���@��@��T@�@���@��h@��7@��h@��h@�x�@�G�@�V@��j@��u@�r�@�j@�I�@�b@��w@��P@�l�@�S�@�;d@��y@���@�E�@�-@��@��@�J@���@��^@�G�@��@��j@�z�@�j@��@���@��m@��
@��w@���@��@�dZ@�o@���@���@���@�V@�{@��@��h@�x�@�O�@���@�Ĝ@��9@���@�bN@�Q�@�A�@�9X@� �@��;@���@�l�@�dZ@�\)@�\)@�S�@�@���@���@��+@�-@��#@��7@�X@��@���@��D@�j@�Q�@� �@�w@|�@;d@
=@~�y@~�+@}�-@}p�@|�/@|Z@|9X@|1@{��@{"�@{@zn�@z�@y��@x�`@x �@w�P@w�@v�R@v�+@v@u��@u�h@uV@t��@t�@sƨ@s�@sC�@s"�@so@r�H@r�!@q�@qx�@q�@p��@pr�@p �@o|�@n�@n�+@n{@m��@m�@m`B@m�@l�j@lj@l(�@kƨ@k�@kdZ@k33@j�@jM�@i�^@iG�@i&�@h��@h�u@h �@g�@g�w@g��@g�P@gl�@g+@g
=@f��@f@e��@e`B@e?}@d��@dj@dZ@d9X@c�m@c�F@c��@c��@c"�@b�@b�@b�H@b��@b�!@bn�@a��@a�^@a��@ax�@a%@`��@` �@_l�@^�+@^V@^@]��@\�/@\j@\(�@[�
@[�@[@Zn�@ZJ@Y��@Yx�@X�`@X�9@X�u@XbN@X  @W�P@W+@W�@W
=@W
=@V�@V��@VV@V@U�-@U��@U`B@UV@T�@T�@S�m@S��@SS�@S33@S"�@So@R��@Q��@Q��@Q��@Q�7@Qx�@Qhs@P�`@P��@P�u@PQ�@O�;@Ol�@O
=@N�+@NV@N@M�-@MO�@L��@Lz�@L1@K�m@KdZ@J�!@JM�@I&�@H�@HQ�@H  @G�;@G�;@G�w@G��@G|�@G;d@G�@Fȴ@F��@Fff@FV@F$�@F@E�T@E��@E/@D�/@Dz�@C�m@C�F@C��@C��@C�@Ct�@B��@B�\@B~�@B^5@B=q@B�@A��@A�@@�`@@Ĝ@@Q�@?�P@?
=@>��@>�y@>ȴ@>5?@=O�@<�/@<��@<z�@<9X@<(�@;��@;�m@;�@;33@:�@:�\@:�@9��@9�@9��@9�@9�@9�#@97L@8�`@8Ĝ@8bN@8A�@8b@7��@7��@7|�@7+@6ȴ@6�+@6ff@6V@6$�@5��@5@5��@5`B@5O�@5?}@5/@4��@4�j@4(�@3��@3ƨ@3�F@3��@3t�@3o@2�!@2M�@2J@1�#@1��@1X@1�@0�`@0�u@0r�@0r�@0bN@0Q�@01'@0b@0  @/�;@/|�@/+@.�@.��@.v�@.5?@.5?@.{@.@-�@-�T@-�@-O�@-�@,�/@,z�@+��@+�
@+�F@+��@+��@+C�@+@*�@*�H@*��@*�!@*��@*M�@*-@)��@)��@)G�@)7L@)�@)%@(�`@(Ĝ@(�9@(�9@(Q�@( �@(  @'�@'��@'|�@'|�@'\)@'
=@&�y@&�@&ȴ@&�R@&��@&��@&v�@&@%@%O�@%?}@%O�@%?}@%�@$�/@$��@$�@$j@#�m@#�
@#��@#o@"�!@"��@"�\@"M�@"-@!�^@!�7@!x�@!x�@!X@!&�@ ��@ Ĝ@ �9@ �u@ r�@ bN@ A�@ b@�@�P@l�@�@ff@$�@@@�-@��@�-@��@��@O�@�@�j@��@�D@Z@(�@�@�@1@�
@�@C�@33@"�@o@�@�H@��@~�@~�@n�@n�@^5@M�@=q@-@��@��@�7@G�@Ĝ@�u@r�@Q�@b@�P@|�@l�@\)@K�@+@
=@�@E�@@�T@�-@p�@`B@O�@��@��@9X@1@��@�m@�m@�
@��@dZ@"�@@��@�\@~�@-@��@�@%@��@��@�9@A�@b@�@�@�P@|�@K�@�@��@�@��@�+@ff@V@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��yA��A��A��A��`A��yA��A���A��A��mA��`A��mA��TA��HA��TA��yA��A�A�%A�%A�%A�%A�JA�VA�oA�oA� �A�1'A�9XA�C�A�I�A�I�A�?}A�5?A�5?A�/A�&�A��A�
=A��A���Aĩ�A�bNA���A��A���A��^A�ffA�/A��7A�I�A���A�ĜA�XA��^A��A���A��A�=qA�1'A�p�A��A���A�?}A�oA�ĜA���A�1'A��7A�7LA�`BA��RA�z�A��A��A�oA��FA��A���A�z�A���A���A��mA�ffA���A���A��A�VA�7LA�n�A��A��A��A��\A�bA���A��DA��A��9A�1A�t�A�
=A��uA�33A�O�A�1'A�z�A���A��`A�{A�Q�A���A~�AyC�Avr�At-Ap�uAlM�Ah�jAe�;Ac|�A_C�A]�^AZM�AW��AW33AVn�AU��AS�APbAM�AI/AF�HAD�DAC/ABM�A@z�A?�-A>�A<�A;t�A:�jA9�FA8ȴA5�A37LA2�`A2ȴA2�A2  A0�A/��A.�DA-33A,r�A+��A+G�A+A*ȴA)hsA(A&E�A%��A%G�A$�A#�A#`BA"z�A!��A!p�A ��A ^5A �A�wAoAA�A�FAoA�DA��A��A��A��A  AbA��A&�A1Ax�A��A�+AJA�7A�AbNAO�A��A�A33A	��AȴA�A��AdZA�AffA1AS�A$�AƨA��AhsAG�A/A ��A ��@��@�E�@�j@��!@���@�r�@�~�@�x�@�P@�M�@��@�{@�h@�1'@��@�/@�@�`B@���@�;d@��H@�+@�O�@�bN@ߝ�@��@�ȴ@�V@�(�@۶F@�dZ@ڸR@��@�%@�dZ@�V@Ձ@��@ԋD@�@�@�  @θR@�J@˥�@ə�@Ȭ@�b@�+@Ɵ�@�?}@þw@�@�7L@���@�A�@��
@�33@��R@�-@���@�G�@���@��j@���@�A�@���@���@�p�@���@���@�j@� �@���@��m@��
@�S�@��u@��@��T@���@��@��@�p�@��@���@�bN@�1'@�  @���@�"�@��+@�@��-@���@�hs@�O�@�&�@�&�@�V@��9@�A�@�I�@�  @���@�t�@��@���@�Z@��;@���@�o@��@���@���@���@��\@�E�@���@�x�@�?}@�V@��`@�j@��@��m@��@�S�@���@��+@��@��^@���@�x�@�/@��@�V@��/@��9@�r�@�(�@��P@��H@���@�v�@�5?@��#@��@��@��`@���@�Ĝ@��D@�(�@��@�  @��m@��w@���@�dZ@�C�@�o@��@��@��!@��\@�ff@�-@��@�@���@��7@�O�@�%@��/@���@��D@�(�@�ƨ@�K�@�33@��@���@��!@���@�v�@�E�@�J@��#@��-@��@�?}@�/@���@��/@��j@�r�@�1'@�1@��@��@�t�@�C�@�"�@���@�ff@�^5@�{@���@��@��T@�@���@��h@��7@��h@��h@�x�@�G�@�V@��j@��u@�r�@�j@�I�@�b@��w@��P@�l�@�S�@�;d@��y@���@�E�@�-@��@��@�J@���@��^@�G�@��@��j@�z�@�j@��@���@��m@��
@��w@���@��@�dZ@�o@���@���@���@�V@�{@��@��h@�x�@�O�@���@�Ĝ@��9@���@�bN@�Q�@�A�@�9X@� �@��;@���@�l�@�dZ@�\)@�\)@�S�@�@���@���@��+@�-@��#@��7@�X@��@���@��D@�j@�Q�@� �@�w@|�@;d@
=@~�y@~�+@}�-@}p�@|�/@|Z@|9X@|1@{��@{"�@{@zn�@z�@y��@x�`@x �@w�P@w�@v�R@v�+@v@u��@u�h@uV@t��@t�@sƨ@s�@sC�@s"�@so@r�H@r�!@q�@qx�@q�@p��@pr�@p �@o|�@n�@n�+@n{@m��@m�@m`B@m�@l�j@lj@l(�@kƨ@k�@kdZ@k33@j�@jM�@i�^@iG�@i&�@h��@h�u@h �@g�@g�w@g��@g�P@gl�@g+@g
=@f��@f@e��@e`B@e?}@d��@dj@dZ@d9X@c�m@c�F@c��@c��@c"�@b�@b�@b�H@b��@b�!@bn�@a��@a�^@a��@ax�@a%@`��@` �@_l�@^�+@^V@^@]��@\�/@\j@\(�@[�
@[�@[@Zn�@ZJ@Y��@Yx�@X�`@X�9@X�u@XbN@X  @W�P@W+@W�@W
=@W
=@V�@V��@VV@V@U�-@U��@U`B@UV@T�@T�@S�m@S��@SS�@S33@S"�@So@R��@Q��@Q��@Q��@Q�7@Qx�@Qhs@P�`@P��@P�u@PQ�@O�;@Ol�@O
=@N�+@NV@N@M�-@MO�@L��@Lz�@L1@K�m@KdZ@J�!@JM�@I&�@H�@HQ�@H  @G�;@G�;@G�w@G��@G|�@G;d@G�@Fȴ@F��@Fff@FV@F$�@F@E�T@E��@E/@D�/@Dz�@C�m@C�F@C��@C��@C�@Ct�@B��@B�\@B~�@B^5@B=q@B�@A��@A�@@�`@@Ĝ@@Q�@?�P@?
=@>��@>�y@>ȴ@>5?@=O�@<�/@<��@<z�@<9X@<(�@;��@;�m@;�@;33@:�@:�\@:�@9��@9�@9��@9�@9�@9�#@97L@8�`@8Ĝ@8bN@8A�@8b@7��@7��@7|�@7+@6ȴ@6�+@6ff@6V@6$�@5��@5@5��@5`B@5O�@5?}@5/@4��@4�j@4(�@3��@3ƨ@3�F@3��@3t�@3o@2�!@2M�@2J@1�#@1��@1X@1�@0�`@0�u@0r�@0r�@0bN@0Q�@01'@0b@0  @/�;@/|�@/+@.�@.��@.v�@.5?@.5?@.{@.@-�@-�T@-�@-O�@-�@,�/@,z�@+��@+�
@+�F@+��@+��@+C�@+@*�@*�H@*��@*�!@*��@*M�@*-@)��@)��@)G�@)7L@)�@)%@(�`@(Ĝ@(�9@(�9@(Q�@( �@(  @'�@'��@'|�@'|�@'\)@'
=@&�y@&�@&ȴ@&�R@&��@&��@&v�@&@%@%O�@%?}@%O�@%?}@%�@$�/@$��@$�@$j@#�m@#�
@#��@#o@"�!@"��@"�\@"M�@"-@!�^@!�7@!x�@!x�@!X@!&�@ ��@ Ĝ@ �9@ �u@ r�@ bN@ A�@ b@�@�P@l�@�@ff@$�@@@�-@��@�-@��@��@O�@�@�j@��@�D@Z@(�@�@�@1@�
@�@C�@33@"�@o@�@�H@��@~�@~�@n�@n�@^5@M�@=q@-@��@��@�7@G�@Ĝ@�u@r�@Q�@b@�P@|�@l�@\)@K�@+@
=@�@E�@@�T@�-@p�@`B@O�@��@��@9X@1@��@�m@�m@�
@��@dZ@"�@@��@�\@~�@-@��@�@%@��@��@�9@A�@b@�@�@�P@|�@K�@�@��@�@��@�+@ff@V@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bu�Bu�Bu�Bv�Bv�Bv�Bw�Bw�Bw�Bu�Bu�Bu�Bu�Bt�Bu�Bu�Bv�Bz�B{�B|�B|�B{�B~�B� B�B�B�7B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�RB�RB�BBuB�B �B �B �B"�B$�B(�B,B-B.B2-B49B:^B>wB?}BA�B<jB<jB9XB/B�B{BoBVB	7BBBB��B�B�B�BB�
B��BɺBƨBȴBĜB�RB��B��B��B�bB}�BiyB`BBP�BF�B<jB+BVBB
��B
�B
�`B
�
B
ȴB
�XB
��B
�oB
�B
v�B
dZB
5?B
{B	��B	�`B	��B	�B	��B	�B	p�B	R�B	E�B	5?B	#�B	�B	�B	hB	1B�B�sB��BÖB�RB�-B�B��B��B��B��B��B�oB�PB�=B�Bz�Bw�Bx�Bx�Bw�Bt�Bq�Bm�BjBhsBffBe`BdZBbNBbNB^5B\)BZBXBXBW
BVBS�BR�BP�BP�BP�BO�BN�BL�BJ�BI�BG�BE�BC�BA�BB�BA�B?}B?}B<jB;dB8RB7LB5?B49B33B2-B1'B2-B6FB6FB5?B49B1'B.B/B/B/B-B(�B%�B'�B'�B&�B&�B&�B&�B&�B&�B&�B'�B+B.B/B/B1'B2-B5?B6FB49B1'B1'B0!B5?B33B49B49B49B5?B7LB7LB:^B@�B@�BB�BB�BB�BF�BF�BG�BG�BH�BI�BJ�BM�BO�BP�BQ�BQ�BT�BVBYB\)B]/BcTBffBhsBiyBk�Bl�Bo�Bs�Bw�Bx�Bz�Bz�B{�B}�B~�B�B�B�B�B�B�B�%B�DB�JB�PB�VB�\B�\B�\B�\B�\B�\B�\B��B��B��B��B�B�'B�-B�3B�?B�FB�LB�RB�XB�jB��BĜBǮBȴB��B��B��B��B�B�)B�5B�5B�HB�TB�ZB�fB�B��B��B��B	B	B	1B	
=B	
=B	
=B	PB	bB	{B	�B	�B	�B	�B	�B	!�B	$�B	'�B	+B	-B	1'B	49B	5?B	7LB	:^B	:^B	:^B	<jB	=qB	?}B	A�B	G�B	N�B	P�B	R�B	VB	[#B	cTB	e`B	e`B	e`B	ffB	hsB	k�B	l�B	m�B	m�B	o�B	p�B	r�B	s�B	u�B	v�B	x�B	y�B	{�B	}�B	� B	�B	�B	�B	�%B	�1B	�=B	�=B	�=B	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�9B	�?B	�FB	�LB	�^B	�dB	�jB	�qB	�qB	�qB	�wB	�wB	�}B	�}B	�}B	�}B	�}B	��B	��B	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�BB	�HB	�HB	�HB	�TB	�TB	�ZB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
JB
JB
PB
PB
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
uB
uB
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
#�B
#�B
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
'�B
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
+B
,B
,B
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
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
8RB
9XB
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
=qB
=qB
=qB
=qB
=qB
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
D�B
D�B
D�B
D�B
D�B
E�B
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
H�B
H�B
H�B
I�B
I�B
I�B
J�B
I�B
J�B
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
R�B
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
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
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
_;B
_;B
_;B
_;B
_;B
`BB
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
e`B
e`B
ffB
ffB
ffB
ffB
ffB
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
hsB
hsB
iyB
iyB
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
p�B
p�B
p�B
p�B
p�B
p�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bu�Bu�Bu�Bv�Bv�Bv�Bw�Bw�Bw�Bu�Bu�Bu�Bu�Bt�ButButBvzBz�B{�B|�B|�B{�B~�B�B��B��B��B�,B�QB��B��B��B��B��B��B��B��B��B��B��B� B��B�eB��B��B��B�B_BB�B!-B!HB!�B$&B&�B*eB,�B.IB0;B4B7B=BA�BB'BD3B?.B?�B<�B1�BkB�B&BHBBB�BAB�fB�iB�WB�B�yB��B�XB�B��B�B�xB��B��B�WB��B� Bj�Bb4BRBHB?}B-�B�B�B
��B
�B
�mB
�B
��B
�jB
��B
�aB
�tB
z�B
j�B
:*B
�B	�0B	�B	�}B	�B	��B	�SB	t�B	UMB	I7B	7�B	$�B	�B	B	aB	�B��B�CBԯB�B��B��B��B��B�&B��B��B�mB��B��B��B�tB{0Bw�By$By�By	BvFBr�Bn�Bk6BiBf�Be�Bd�Bc�Bc�B_�B\�BZ�BX�BX�BW�BV�BT{BSuBQNBQ�BQBPHBO�BM�BKxBJXBHfBF�BD�BB�BC�BB�BA�BA B>B<jB8�B7�B5�B4�B3�B2�B1�B3MB6�B72B6FB5�B1�B.�B/iB/�B0�B.�B)yB&�B(�B($B&�B&�B&�B&�B'B'mB'�B(�B+�B.�B/�B/�B2-B2�B6FB7B5tB2B1�B1B6FB3�B5ZB5B4�B5tB7fB7�B:�B@�B@�BB�BB�BCGBF�BF�BG�BG�BIBJ=BK�BNVBPBQBR:BR�BU�BW
BY�B\�B^�BdZBf�Bh�Bi�Bk�Bm]BpoBt�BxBx�Bz�Bz�B|B~B~�B� B�B�B��B�B�9B��B��B�dB�PB�VB�(B�\B�BB�BB�\B��B��B�_B�CB��B�B�kB�B�B�MB�%B�+B�2B�RB��B��B��B�gB�zBȚB̘BϫBѷB��B�B�)B��B�5B�HB�:B�B�B�!B��B��B�B	�B	B	�B	
	B		�B	
#B	PB	HB	aB	sB	eB	�B	�B	�B	!�B	$�B	'�B	*�B	-B	1'B	4B	5%B	72B	:*B	:B	:DB	<6B	=VB	?cB	A�B	G�B	N�B	P�B	R�B	VB	[WB	c B	eB	e,B	e,B	fLB	hXB	k6B	lWB	m]B	mwB	oiB	p�B	r|B	s�B	u�B	v�B	x�B	y�B	{�B	}�B	�B	��B	��B	�B	��B	�B	�	B	��B	�=B	�JB	�<B	�NB	�2B	�2B	�mB	�sB	�eB	�kB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�%B	�FB	�LB	�*B	�0B	�6B	�"B	�"B	�<B	�BB	�BB	�B	�.B	�HB	�HB	�cB	�iB	�oB	�gB	�gB	�mB	�tB	ƎB	�zB	�lB	�rB	˒B	ˬB	��B	οB	��B	ѷB	ѷB	҉B	ҽB	ҽB	��B	��B	��B	�
B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�5B	�'B	��B	�-B	�B	�:B	�:B	�@B	�2B	�LB	�RB	�$B	�*B	�DB	�_B	�KB	�KB	�KB	�QB	�kB	�WB	�]B	�]B	�CB	�CB	�]B	�wB	�B	�oB	��B	�B	�B	�|B	�B	�B	��B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
B
B
�B
�B
B
�B
B
�B
�B
B
B
B
�B
	B
	B
	B
	B
	B
	B

=B
)B
)B
B
0B
6B
PB
<B
B
BB
.B
.B
B
B
4B
B
 B
 B
:B
@B
&B
@B
[B
FB
MB
MB
2B
MB
9B
SB
SB
YB
YB
YB
YB
YB
sB
yB
eB
eB
kB
B
kB
kB
kB
qB
qB
qB
qB
qB
xB
]B
dB
~B
~B
~B
�B
�B
�B
pB
�B
�B
�B
 �B
 �B
!�B
#�B
#�B
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
'�B
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
*�B
+�B
+�B
,�B
,�B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
0B
0!B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
2B
1�B
2�B
2�B
4B
4B
5%B
5%B
6B
6B
5�B
6+B
7LB
88B
9rB
:DB
:*B
:*B
;B
:�B
;0B
;B
;B
<B
<6B
=<B
=<B
="B
="B
=<B
=<B
=<B
="B
>]B
>BB
>BB
>]B
?HB
?.B
?B
?HB
@OB
@OB
@OB
A B
AUB
AUB
AUB
AUB
BuB
B[B
B[B
BuB
B�B
C{B
D3B
DgB
DMB
DgB
D�B
EmB
FtB
FtB
FtB
FtB
FtB
FtB
F�B
GzB
GzB
G�B
H�B
H�B
HfB
HfB
HfB
HfB
H�B
H�B
I�B
I�B
I�B
J�B
IlB
J�B
J�B
J�B
J�B
J�B
K�B
KxB
K�B
K�B
K�B
K�B
K�B
L�B
L~B
LdB
L�B
L~B
L~B
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
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P}B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
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
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
]B
]�B
^B
^B
^B
]�B
]�B
^B
^B
^B
_B
^�B
^�B
_B
_B
_B
^�B
^�B
_B
_B
^�B
^�B
_�B
`�B
aB
aB
a-B
bB
a�B
c B
cB
cB
cB
cB
b�B
c B
c B
cB
dB
dB
d&B
d&B
d&B
eB
eB
e,B
e,B
eB
e,B
e�B
fB
fB
f2B
e�B
f2B
f2B
fB
fB
fB
fB
fB
f2B
gB
g8B
g8B
g8B
g8B
gRB
h>B
h>B
h$B
h>B
hXB
h
B
h$B
h$B
h$B
h>B
h>B
h$B
hXB
iDB
iDB
jKB
jKB
j0B
jKB
jKB
jKB
k6B
kQB
k6B
kB
k6B
kQB
l=B
lWB
lWB
lWB
m]B
m]B
m]B
mwB
mwB
mwB
nIB
nIB
nIB
ncB
n}B
oiB
oiB
oOB
oiB
oOB
oiB
poB
poB
poB
poB
pUB
pUB
pUB
pUB
p;1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.65(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202101260032012021012600320120210126003201202306231727142023062317271420230623172714202101270016022021012700160220210127001602  JA  ARFMdecpA19c                                                                20210120184042  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210120094103  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210120094104  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210120094104  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210120094104  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210120094104  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210120094104  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210120094104  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210120094105  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210120094105                      G�O�G�O�G�O�                JA  ARUP                                                                        20210120095210                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20210120153330  CV  JULD            G�O�G�O�F��p                JM  ARGQJMQC2.0                                                                 20210120153330  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20210120153330  CV  LATITUDE        G�O�G�O�A���                JM  ARCAJMQC2.0                                                                 20210125153201  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20210125153201  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2019V1                                                       20210126151602  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20230623082714  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20230705041505                      G�O�G�O�G�O�                