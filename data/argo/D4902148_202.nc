CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-02-27T18:37:51Z creation;2020-02-27T18:37:55Z conversion to V3.1;2022-11-21T05:27:29Z update;     
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
resolution        =���   axis      Z        `  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Id   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  M<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200227183751  20221123114511  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_202                     2C  D   NAVIS_A                         0397                            ARGO 011514                     863 @�"\ 1   @�#��� @;<PH��dwJ���E1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�P 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B��B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B�ffC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`s3D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfy�Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�9�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D깚D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�9�D�y�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�ZA�^5A�n�A�n�A�l�A�n�A�jA�hsA�\)A�G�A�5?A�$�A� �A��A� �A� �A�"�A�"�A�$�A�&�A�(�A�&�A��A�
=A�A���A�A��!A���A�|�A�bNA�$�A��A��jA�;dA��A��A��A�ȴA�x�A�bNA�=qA�$�A�VA��A�XA��wA�A�A���A���A��uA��A�33A��`A�ffA��mA�+A��/A��\A�G�A��A���A��A���A�=qA�{A���A�dZA�~�A�1A�=qA�ȴA��A���A��TA�G�A���A�33A���A�  A�~�A���A�p�A�n�A�=qA�A��!A�A��A�A���A��A���A���A�33A���A}�A{�7AyAx�/Axz�Ax=qAw��AvbAs"�Aqx�ApQ�Aot�AnAl��Ak�PAj�Aj  Ai��Ai��AiXAh�Ah�9Ag�PAe�FAc��AchsAb��Aa�;Aa7LA`��A`VA^VA]oA\n�A[�A[XAZAYp�AX��AXAW
=AV(�ATffAT1AS�AS�-AR��ARz�AQ\)AP��APjAN��AMK�AL��AK�AK|�AJ��AJI�AI�AI��AIK�AH  AE�AD{AC��AC/ABffA@�RA?G�A>JA=C�A;l�A:ffA9`BA8ZA7?}A5%A4�!A4(�A3x�A3A2�/A2��A2-A1��A1C�A0�9A/��A.v�A-�A-��A-S�A-
=A,ZA+`BA* �A)|�A)�A'�A& �A%�A$JA#/A!A r�A?}A��A�A$�AbA�A�yA-A�FAhsA�/A�FAVA�+AƨA|�A"�A��A�PA�AO�A�yA�RAz�AI�A�A�wA�PA��Az�AA�hA%A��A|�AȴA�\AjAZAI�A$�A��A\)A"�A�yA��A�+A�DAA ff@��#@��@��#@���@�I�@���@�ff@���@��@�w@���@���@�b@���@�K�@�n�@��@�r�@�ȴ@���@�Ĝ@◍@�-@�Ĝ@��u@�Z@�?}@ڸR@���@�7L@��@�r�@ו�@���@�x�@���@�9X@�ƨ@��H@�@�r�@θR@���@�1'@���@�r�@�ƨ@Ǿw@ǥ�@��H@�G�@���@+@��`@�Q�@�b@�ƨ@�S�@�E�@���@�  @���@�Ĝ@�ƨ@�ȴ@��#@���@���@�A�@���@�K�@��H@���@��@���@��
@�+@�{@���@��@�1@�l�@�
=@��y@���@�~�@�@��7@��@���@�Q�@�t�@�o@��\@�M�@�@��@���@���@�b@��@�t�@���@�X@�Ĝ@�9X@�|�@���@�ff@�J@���@��`@�
=@�E�@��7@��@��@���@�j@�b@� �@��
@�dZ@��@�v�@���@�p�@��`@��;@�C�@�ȴ@�~�@�ff@���@��7@�G�@��@�V@���@��@��
@���@�K�@�ȴ@�^5@�$�@�x�@��j@�Q�@� �@���@�|�@�33@��@���@�V@�-@��#@�x�@�?}@�%@��@���@�j@�  @��
@��@��P@�;d@�o@��@��@��!@�5?@��-@�p�@�O�@�7L@���@��/@���@�j@��F@��@�+@��@��+@�V@�5?@�-@�{@��@��-@��T@�-@�-@��@�=q@�E�@���@��-@�x�@�O�@�&�@�%@��9@�9X@�  @�;@��@}�@}`B@}V@|�@|�/@|��@|�@|��@|��@|j@|I�@{�
@{"�@z�!@x�`@w�;@w��@wK�@v$�@u��@uO�@u/@uV@t�/@tz�@s�m@s��@s��@s��@s��@s��@s�@sC�@s"�@r�\@q�#@qG�@p�`@p1'@o��@ol�@n�y@n�+@nE�@n{@m��@m�@mV@lI�@k��@k"�@j�\@j~�@j�!@j��@j~�@j~�@jn�@ihs@i&�@h��@hr�@h  @f�R@fE�@e/@dI�@c�
@c��@cC�@b��@b-@a��@a��@a�^@b-@b=q@b-@bJ@a��@aX@`Ĝ@`Q�@_l�@]�@]�@]?}@\�/@\9X@[��@[33@Z�H@ZM�@Y�#@Y7L@X��@X�@Xb@W�@W��@W+@V�y@V��@VE�@U�@U�@U?}@T�/@Tz�@T(�@S��@S�F@St�@SC�@So@S@R�@R�@R�H@R��@R�!@RM�@Q��@Q��@Qx�@QG�@P��@P��@P�9@P�@PbN@O�;@O|�@OK�@N��@N�R@N��@Nv�@NV@N@Mp�@M�@L��@Lj@K��@K��@KC�@J�\@I��@I��@J-@J-@J-@I�@I��@I��@I��@Ihs@IX@I&�@H�u@H1'@G�;@G�@G|�@G;d@F�@F�+@FV@E�@EO�@E�@D�/@D��@Dj@D9X@D�@Cƨ@C�@CdZ@C@B�!@B~�@B^5@B-@A��@Ax�@A7L@A�@@�`@@��@@Q�@@b@?�@?��@?�P@?\)@?K�@>�@>�R@>�+@>E�@>5?@=�T@=�h@=O�@=?}@=V@<�/@<�@<�@<��@<z�@<(�@;�m@;�m@;�
@;��@;�@;dZ@;33@:�@:��@:M�@9��@9��@9�7@9x�@9X@9%@8�`@8��@8��@8�9@8�@8b@7��@7��@7�P@7l�@7+@6��@6v�@6{@5��@5O�@5V@4��@4Z@4�@3�F@3�@3t�@3t�@3t�@3t�@3t�@3S�@2�H@2�!@2^5@1�^@1�7@1x�@17L@0��@0�@0r�@0  @/�P@/|�@/;d@.��@.V@.{@-�@-�@,��@,��@,Z@+��@+�@+@*��@*��@*�!@*~�@*-@)�7@)�7@)x�@)x�@)X@)G�@)�@)�@)�@)�@(��@(�@(Q�@(1'@( �@'�;@'�@'��@'�P@'|�@'l�@'+@'
=@&��@&�R@%�-@%`B@%?}@%V@$�/@$j@$I�@$9X@$�@$1@#�
@#��@#33@#o@#@"��@"�!@"�\@"-@!�#@!�^@!��@!x�@!hs@!7L@!�@ ��@ ��@ r�@ r�@ r�@ r�@ r�@ r�@ r�@ Q�@ 1'@   @�w@l�@�@�y@�@ȴ@��@ff@$�@{@@O�@O�@?}@/@�/@9X@(�@(�@�@1@��@�
@dZ@�@�!@~�@~�@~�@^5@�@�@��@hs@X@�@%@��@�`@�`@��@bN@Q�@ �@��@\)@;d@��@ȴ@��@�+@E�@{@�T@��@�@O�@/@�@��@�j@��@j@I�@I�@�@1@��@��@dZ@"�@�!@��@~�@n�@^5@^5@M�@-@�@�^@��@��@&�@%@��@Q�@b@�w@�P@\)@+@�@�@�R@V@{@{@�T@�-@�h@�@p�@O�@/@�@�@��@z�@��@��@��@33@
��@
��@
�\@
^5@
�@
J@	��@	�@	�#@	��@	�^@	��@	x�@	X@	G�@	7L@	&�@	&�@	%@Ĝ@r�@Q�@1'@b@�P@|�@|�@|�@l�@\)@K�@+@�@��@��@v�@ff@ff@ff@V@�T@�-@�@`B@O�@?}@�@V@V@�@�/@�/@�/@��@��@�@9X@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�^5A�ZA�^5A�n�A�n�A�l�A�n�A�jA�hsA�\)A�G�A�5?A�$�A� �A��A� �A� �A�"�A�"�A�$�A�&�A�(�A�&�A��A�
=A�A���A�A��!A���A�|�A�bNA�$�A��A��jA�;dA��A��A��A�ȴA�x�A�bNA�=qA�$�A�VA��A�XA��wA�A�A���A���A��uA��A�33A��`A�ffA��mA�+A��/A��\A�G�A��A���A��A���A�=qA�{A���A�dZA�~�A�1A�=qA�ȴA��A���A��TA�G�A���A�33A���A�  A�~�A���A�p�A�n�A�=qA�A��!A�A��A�A���A��A���A���A�33A���A}�A{�7AyAx�/Axz�Ax=qAw��AvbAs"�Aqx�ApQ�Aot�AnAl��Ak�PAj�Aj  Ai��Ai��AiXAh�Ah�9Ag�PAe�FAc��AchsAb��Aa�;Aa7LA`��A`VA^VA]oA\n�A[�A[XAZAYp�AX��AXAW
=AV(�ATffAT1AS�AS�-AR��ARz�AQ\)AP��APjAN��AMK�AL��AK�AK|�AJ��AJI�AI�AI��AIK�AH  AE�AD{AC��AC/ABffA@�RA?G�A>JA=C�A;l�A:ffA9`BA8ZA7?}A5%A4�!A4(�A3x�A3A2�/A2��A2-A1��A1C�A0�9A/��A.v�A-�A-��A-S�A-
=A,ZA+`BA* �A)|�A)�A'�A& �A%�A$JA#/A!A r�A?}A��A�A$�AbA�A�yA-A�FAhsA�/A�FAVA�+AƨA|�A"�A��A�PA�AO�A�yA�RAz�AI�A�A�wA�PA��Az�AA�hA%A��A|�AȴA�\AjAZAI�A$�A��A\)A"�A�yA��A�+A�DAA ff@��#@��@��#@���@�I�@���@�ff@���@��@�w@���@���@�b@���@�K�@�n�@��@�r�@�ȴ@���@�Ĝ@◍@�-@�Ĝ@��u@�Z@�?}@ڸR@���@�7L@��@�r�@ו�@���@�x�@���@�9X@�ƨ@��H@�@�r�@θR@���@�1'@���@�r�@�ƨ@Ǿw@ǥ�@��H@�G�@���@+@��`@�Q�@�b@�ƨ@�S�@�E�@���@�  @���@�Ĝ@�ƨ@�ȴ@��#@���@���@�A�@���@�K�@��H@���@��@���@��
@�+@�{@���@��@�1@�l�@�
=@��y@���@�~�@�@��7@��@���@�Q�@�t�@�o@��\@�M�@�@��@���@���@�b@��@�t�@���@�X@�Ĝ@�9X@�|�@���@�ff@�J@���@��`@�
=@�E�@��7@��@��@���@�j@�b@� �@��
@�dZ@��@�v�@���@�p�@��`@��;@�C�@�ȴ@�~�@�ff@���@��7@�G�@��@�V@���@��@��
@���@�K�@�ȴ@�^5@�$�@�x�@��j@�Q�@� �@���@�|�@�33@��@���@�V@�-@��#@�x�@�?}@�%@��@���@�j@�  @��
@��@��P@�;d@�o@��@��@��!@�5?@��-@�p�@�O�@�7L@���@��/@���@�j@��F@��@�+@��@��+@�V@�5?@�-@�{@��@��-@��T@�-@�-@��@�=q@�E�@���@��-@�x�@�O�@�&�@�%@��9@�9X@�  @�;@��@}�@}`B@}V@|�@|�/@|��@|�@|��@|��@|j@|I�@{�
@{"�@z�!@x�`@w�;@w��@wK�@v$�@u��@uO�@u/@uV@t�/@tz�@s�m@s��@s��@s��@s��@s��@s�@sC�@s"�@r�\@q�#@qG�@p�`@p1'@o��@ol�@n�y@n�+@nE�@n{@m��@m�@mV@lI�@k��@k"�@j�\@j~�@j�!@j��@j~�@j~�@jn�@ihs@i&�@h��@hr�@h  @f�R@fE�@e/@dI�@c�
@c��@cC�@b��@b-@a��@a��@a�^@b-@b=q@b-@bJ@a��@aX@`Ĝ@`Q�@_l�@]�@]�@]?}@\�/@\9X@[��@[33@Z�H@ZM�@Y�#@Y7L@X��@X�@Xb@W�@W��@W+@V�y@V��@VE�@U�@U�@U?}@T�/@Tz�@T(�@S��@S�F@St�@SC�@So@S@R�@R�@R�H@R��@R�!@RM�@Q��@Q��@Qx�@QG�@P��@P��@P�9@P�@PbN@O�;@O|�@OK�@N��@N�R@N��@Nv�@NV@N@Mp�@M�@L��@Lj@K��@K��@KC�@J�\@I��@I��@J-@J-@J-@I�@I��@I��@I��@Ihs@IX@I&�@H�u@H1'@G�;@G�@G|�@G;d@F�@F�+@FV@E�@EO�@E�@D�/@D��@Dj@D9X@D�@Cƨ@C�@CdZ@C@B�!@B~�@B^5@B-@A��@Ax�@A7L@A�@@�`@@��@@Q�@@b@?�@?��@?�P@?\)@?K�@>�@>�R@>�+@>E�@>5?@=�T@=�h@=O�@=?}@=V@<�/@<�@<�@<��@<z�@<(�@;�m@;�m@;�
@;��@;�@;dZ@;33@:�@:��@:M�@9��@9��@9�7@9x�@9X@9%@8�`@8��@8��@8�9@8�@8b@7��@7��@7�P@7l�@7+@6��@6v�@6{@5��@5O�@5V@4��@4Z@4�@3�F@3�@3t�@3t�@3t�@3t�@3t�@3S�@2�H@2�!@2^5@1�^@1�7@1x�@17L@0��@0�@0r�@0  @/�P@/|�@/;d@.��@.V@.{@-�@-�@,��@,��@,Z@+��@+�@+@*��@*��@*�!@*~�@*-@)�7@)�7@)x�@)x�@)X@)G�@)�@)�@)�@)�@(��@(�@(Q�@(1'@( �@'�;@'�@'��@'�P@'|�@'l�@'+@'
=@&��@&�R@%�-@%`B@%?}@%V@$�/@$j@$I�@$9X@$�@$1@#�
@#��@#33@#o@#@"��@"�!@"�\@"-@!�#@!�^@!��@!x�@!hs@!7L@!�@ ��@ ��@ r�@ r�@ r�@ r�@ r�@ r�@ r�@ Q�@ 1'@   @�w@l�@�@�y@�@ȴ@��@ff@$�@{@@O�@O�@?}@/@�/@9X@(�@(�@�@1@��@�
@dZ@�@�!@~�@~�@~�@^5@�@�@��@hs@X@�@%@��@�`@�`@��@bN@Q�@ �@��@\)@;d@��@ȴ@��@�+@E�@{@�T@��@�@O�@/@�@��@�j@��@j@I�@I�@�@1@��@��@dZ@"�@�!@��@~�@n�@^5@^5@M�@-@�@�^@��@��@&�@%@��@Q�@b@�w@�P@\)@+@�@�@�R@V@{@{@�T@�-@�h@�@p�@O�@/@�@�@��@z�@��@��@��@33@
��@
��@
�\@
^5@
�@
J@	��@	�@	�#@	��@	�^@	��@	x�@	X@	G�@	7L@	&�@	&�@	%@Ĝ@r�@Q�@1'@b@�P@|�@|�@|�@l�@\)@K�@+@�@��@��@v�@ff@ff@ff@V@�T@�-@�@`B@O�@?}@�@V@V@�@�/@�/@�/@��@��@�@9X@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B�B�!B�-B�9B�?B�?B�?B�FB�FB�FB�LB�LB�LB�LB�RB�RB�LB�LB�?B�9B�9B�3B�-B�B��B��B��B��B�B�-B�9B�LB�LB�FB�qB�}B�}B�?B�B��B��B��B�hB�Bw�Bt�Bq�Bq�Br�Bu�Bv�Bw�Br�B]/B5?B�BJBDB	7B��B�sB�ZB�B��BB�B��B��B�B|�Bx�Br�Bl�BXBJ�B;dB,B$�B�B1B
��B
�B
�fB
�)B
ǮB
�B
��B
��B
� B
n�B
aHB
[#B
YB
VB
O�B
D�B
2-B
'�B
�B
�B
bB
1B
B	��B	��B	��B	��B	��B	�B	�B	�yB	�;B	��B	��B	��B	��B	ǮB	ŢB	B	�XB	�-B	�B	�B	��B	��B	��B	��B	�oB	�DB	�+B	�B	� B	~�B	}�B	z�B	w�B	r�B	p�B	m�B	ffB	`BB	\)B	XB	VB	R�B	O�B	M�B	K�B	F�B	>wB	8RB	2-B	0!B	/B	+B	#�B	�B	�B	uB	JB	+B	B��B��B�B�sB�fB�`B�TB�NB�HB�;B�/B�B�
B��B��BȴBȴBƨBĜB��B�dB�9B�!B�B��B��B��B��B��B�uB�VB�=B�1B�B� B}�Bz�Bt�Br�Bp�Bo�Bl�BiyBgmBffBdZBcTBbNB`BB]/BYBXBW
BVBVBT�BT�BS�BR�BQ�BO�BN�BL�BJ�BF�BD�BC�BB�BB�BB�BB�BA�B@�B@�B?}B>wB=qB;dB8RB6FB49B2-B0!B0!B/B/B.B-B-B,B+B)�B(�B&�B&�B%�B%�B$�B$�B$�B#�B#�B$�B$�B$�B#�B"�B"�B#�B"�B"�B"�B"�B"�B#�B$�B$�B$�B$�B#�B#�B#�B$�B%�B%�B%�B(�B)�B(�B(�B(�B+B+B,B/B0!B0!B0!B0!B0!B2-B33B33B6FB7LB9XB<jB<jB;dB?}B@�B@�BA�BB�BD�BD�BG�BI�BJ�BL�BM�BM�BN�BP�BP�BP�BP�BQ�BR�BS�BT�BVBYBZB\)B]/B_;B^5B_;BbNBdZBffBffBhsBm�Bn�Bq�Bu�Bw�B{�B�B�1B�7B�PB�\B�bB�hB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B�B�B�!B�!B�3B�?B�FB�LB�LB�RB�^B�}B��BÖBƨBȴBɺB��B��B��B�B�B�B�)B�/B�;B�NB�TB�fB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	%B	1B		7B	
=B	JB	PB	PB	bB	�B	�B	�B	�B	 �B	"�B	$�B	$�B	%�B	(�B	/B	33B	6FB	7LB	8RB	9XB	<jB	?}B	B�B	D�B	E�B	E�B	E�B	E�B	E�B	F�B	G�B	K�B	W
B	ZB	\)B	]/B	^5B	^5B	_;B	_;B	_;B	`BB	aHB	bNB	ffB	ffB	hsB	iyB	iyB	jB	m�B	n�B	r�B	u�B	v�B	z�B	}�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�DB	�PB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�-B	�-B	�-B	�-B	�3B	�3B	�3B	�?B	�LB	�RB	�XB	�dB	�wB	��B	��B	��B	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B

=B

=B
DB
JB
JB
PB
PB
VB
VB
VB
\B
\B
\B
bB
hB
oB
oB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
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
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
,B
,B
-B
.B
.B
/B
/B
0!B
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
49B
49B
49B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
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
R�B
Q�B
R�B
R�B
R�B
S�B
T�B
T�B
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
W
B
W
B
XB
XB
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
aHB
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
ffB
ffB
gmB
gmB
gmB
gmB
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
n�B
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B�0B�]B�UB�GB�TB�ZB�ZB�ZB�`B�FB�`B�fB�fB��B��B�lB��B��B��B��B��B��B��B��B��B��B�
B�XB�DB�CB��B��B��B��B��B��B�4B�B��B�"B��B�QB�sB�&B�ABx�BvBr�Bs3Bs�Bv�Bw�By�BvBa-B8�B$BB0B
�B��B�B�BۦB�$BŢB�B��B��B��B~BBz�Bt�Bo�BY�BMPB>B-�B'mB�B
XB
��B
��B
�sB
ߊB
ˬB
��B
��B
�#B
��B
p�B
bNB
[�B
Y�B
WYB
R:B
G�B
49B
)yB
!-B
qB
 B
	�B
�B	��B	�>B	�B	��B	�ZB	�|B	�AB	�B	�-B	յB	��B	�B	̘B	ȚB	��B	��B	��B	�B	�B	�B	�_B	��B	��B	��B	��B	��B	�B	��B	�iB	}B	~�B	{�B	y$B	shB	q�B	o�B	h
B	aHB	]B	X�B	V�B	S�B	PbB	NpB	L�B	H�B	A;B	:*B	3B	1B	0�B	-B	%�B	VB	B	�B	�B	�B	�B��B�	B�]B�_B�RB�B�B��B��B�B��B�#B�yBӏB˒B�RB�RB�_BżB�B��B�ZB�'B��B�6B�`B�-B�/B��B�MB��B�B��B�B��BcB}�Bu�Bs�BqvBp�BnBjeBh>BgmBd�Bd&BcTBbB_BZ7BX�BW�BV�BV�BU�BU�BT�BS�BR�BP�BO�BN<BMjBH�BE�BC�BB�BB�BB�BB�BB'BAoB@�B@ B?B>�B>B:^B7�B6`B4B1B0�B/�B/�B/ B-�B-�B-CB,WB+B)�B(�B(XB&�B&�B&2B&B%�B$�B%FB%�B%�B%`B$�B$�B$tB$�B#nB#TB#nB#�B#�B$�B%zB%zB%zB%�B$�B%B%,B&B&�B'B'mB)yB*KB)_B)�B*0B,"B,=B-)B/�B0�B0�B0�B1'B1AB33B4nB4�B72B8B:B<�B<�B<jB@BABA BBuBCGBEBEmBHfBJ�BK�BM6BNVBNpBOBBQ4BQ4BQNBQhBRoBSuBTaBU�BV�BYBZ�B\�B]�B_�B^�B`Bb�Bd�Bf�Bg8Bi�Bn/Bo5BraBvFBxlB|PB��B�B�rB�B��B��B��B��B��B��B��B��B�B�B�)B�VB�BB��B��B��B�qB�}B�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�rB�pB�[B�MB�mB�yBڠB�xBݲBߤB�B�B��B��B��B�B��B��B�B�B�B�B�8B�B�B�<B�BB�}B	�B	YB	fB		lB	
�B	~B	�B	�B	 B	�B	B		B	B	!B	# B	%B	%B	&2B	)DB	/B	33B	6`B	7�B	8lB	9�B	<�B	?�B	B�B	EB	FB	E�B	FB	FB	E�B	F�B	G�B	L~B	W?B	ZQB	\]B	]dB	^jB	^jB	_pB	_pB	_pB	`vB	a�B	b�B	f�B	gB	h�B	i�B	i�B	kB	m�B	n�B	r�B	u�B	wB	{0B	~BB	�AB	�-B	�GB	�-B	�GB	�GB	�GB	�GB	�{B	��B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�2B	��B	�B	�$B	�*B	�0B	�0B	�eB	�)B	�iB	��B	��B	��B	��B	��B	��B	�|B	�hB	�hB	��B	��B	��B	��B	�rB	�dB	��B	��B	��B	��B	��B	��B	�B	�1B	�DB	�B	�B	�B	�<B	�BB	� B	�@B	�MB	�?B	�_B	�_B	�eB	�QB	�WB	�WB	�xB	�jB	ބB	�pB	�B	�|B	�B	�B	�B	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�B	�0B	�0B	�6B	�PB	�<B	�B	��B
 4B
 B
 B
;B
'B
AB
GB
GB
GB
aB
SB
YB
YB
YB
_B
zB
fB

rB

�B
�B
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
�B
�B
 �B
 �B
 �B
!�B
!�B
#B
# B
$B
$B
%B
%B
%B
&B
&B
%�B
%�B
'B
'B
'8B
($B
)*B
)*B
)B
)*B
*0B
*KB
+kB
,WB
,=B
-]B
.IB
.IB
/OB
/OB
0UB
1[B
1AB
1'B
1AB
1AB
1AB
1vB
2aB
2|B
3�B
4�B
4nB
4nB
5�B
6zB
6`B
6�B
6�B
7�B
7�B
7�B
9�B
9�B
9�B
:�B
;�B
;�B
;�B
<�B
<�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
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
EB
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
MB
MB
MB
MB
M�B
M�B
M�B
M�B
M�B
M�B
NB
NB
NB
NB
N�B
OB
PB
O�B
PB
PB
QB
QB
Q B
QB
R B
SB
R B
S&B
S@B
S@B
TB
UB
UB
UB
U2B
U2B
UMB
V9B
VB
W?B
W$B
W$B
W$B
W?B
XEB
XEB
Y1B
Y1B
YKB
ZB
Z7B
Z7B
Z7B
ZQB
ZQB
[WB
[WB
[qB
\]B
\]B
\]B
]dB
]IB
]dB
]dB
]dB
^jB
^jB
^jB
^OB
_pB
_pB
_pB
_VB
_pB
`vB
`vB
`BB
`vB
abB
a|B
a|B
a|B
a�B
a|B
bhB
b�B
cTB
cnB
cnB
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
iyB
i�B
i�B
j�B
jB
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
n�B
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
uB
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x�B
z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.2(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202003090034182020030900341820200309003418202211182142122022111821421220221118214212202003100019422020031000194220200310001942  JA  ARFMdecpA19c                                                                20200228033737  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200227183751  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200227183754  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200227183754  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200227183755  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200227183755  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200227183755  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20200227183755  QCF$                G�O�G�O�G�O�            8000JA  ARGQaqcp2.8e                                                                20200227183755  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200227183755  QCF$                G�O�G�O�G�O�            8000JA  ARGQrqcpt16c                                                                20200227183755  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200227183755                      G�O�G�O�G�O�                JA  ARUP                                                                        20200227185357                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20200228153333  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQJMQC2.0                                                                 20200228153308  CV  JULD            G�O�G�O�F�1                JM  ARGQJMQC2.0                                                                 20200228153308  CV  JULD_LOCATION   G�O�G�O�F�1$                JM  ARGQJMQC2.0                                                                 20200228153308  CV  LATITUDE        G�O�G�O�A��#                JM  ARGQJMQC2.0                                                                 20200228153308  CV  LONGITUDE       G�O�G�O��#�d                JM  ARCAJMQC2.0                                                                 20200308153418  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200308153418  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20200309151942  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118124212  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123114511                      G�O�G�O�G�O�                