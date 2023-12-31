CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-04-14T09:01:05Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20220414090105  20220414090105  4903032 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @��U	J�1   @��U��Zp@;�l�C���c����o1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@���@���A��A<��A\��A|��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B'33B/33B733B?33BG33BO33BW33B_33Bg33Bo33Bw33B33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÙ�BǙ�B˙�Bϙ�Bә�Bי�Bۙ�Bߙ�B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD s3D �3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D	s3D	�3D
s3D
�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"�3D#s3D#�3D$s3D$�3D%s3D%�3D&s3D&�3D's3D'�3D(s3D(�3D)s3D)�3D*s3D*�3D+s3D+�3D,s3D,�3D-s3D-�3D.s3D.�3D/s3D/�3D0s3D0�3D1s3D1�3D2s3D2�3D3s3D3�3D4s3D4�3D5s3D5�3D6s3D6�3D7s3D7�3D8s3D8�3D9s3D9�3D:s3D:�3D;s3D;�3D<s3D<�3D=s3D=�3D>s3D>�3D?s3D?�3D@s3D@�3DAs3DA�3DBs3DB�3DCs3DC�3DDs3DD�3DEs3DE�3DFs3DF�3DGs3DG�3DHs3DH�3DIs3DI�3DJs3DJ�3DKs3DK�3DLs3DL�3DMs3DM�3DNs3DN�3DOs3DO�3DPs3DP�3DQs3DQ�3DRs3DR�3DSs3DS�3DTs3DT�3DUs3DU�3DVs3DV�3DWs3DW�3DXs3DX�3DYs3DY�3DZs3DZ�3D[s3D[�3D\s3D\�3D]s3D]�3D^s3D^�3D_s3D_�3D`y�D`�3Das3Da�3Dbs3Db�3Dcs3Dc�3Dds3Dd�3Des3De�3Dfs3Df�3Dgs3Dg�3Dhs3Dh�3Dis3Di�3Djs3Dj�3Dks3Dk�3Dls3Dl�3Dms3Dm�3Dns3Dn�3Dos3Do�3Dps3Dp�3Dqs3Dq�3Drs3Dr�3Dss3Ds�3Dts3Dt�3Dus3Du�3Dvs3Dv�3Dws3Dw�3Dxs3Dx�3Dys3Dy�3Dzs3Dz�3D{s3D{�3D|s3D|�3D}s3D}�3D~s3D~�3Ds3D�3D�9�D�y�D���D���D�<�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D๚D���D�9�D�y�DṚD���D�9�D�y�D⹚D���D�9�D�y�D㹚D���D�9�D�y�D乚D���D�9�D�y�D幚D���D�9�D�y�D湚D���D�<�D�y�D繚D���D�9�D�y�D蹚D���D�9�D�y�D鹚D���D�9�D�y�D��D���D�9�D�y�D빚D���D�9�D�y�D칚D���D�9�D�y�D���D���D�<�D�|�DD���D�9�D�y�D﹚D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D�D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�C41111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��DA��\A�bNA���A��
A���A���A��FA��A���A���A��PA�~�A�p�A�XA�?}A�+A��#A�9XA�
=A���A��DA�ffA�"�A�1A���A��A��TA��#A��A�+A�=qA���A��A��A���A��#A�JA�;dA��A�K�A���A��-A��A��;A���A��jA��-A���A���A���A��uA��DA�v�A�ZA�I�A�A�A� �A���A��A�A��A�&�A�z�A��HA�-A�p�A�+A�t�A��`A��A��A��A��A��A�&�A���A��A���A��wA�ZA���A��!A�ĜA�?}A�=qA�-A�C�A�A��A���A�x�A��HA��wA��RA�;dA��A��\A��A��A�{A��jA�Q�A���A��wA��TA�{A��+A���A�{A�bA}�PAyp�Av�DAu�mAtĜAs/Aq%AoƨAl  AgƨAe�hAdVAb�DAa�A_S�A^~�A]ƨA]�A\��A[��AYVAU�AS�
ARVAQ"�AP�AN�AN-AM�
AM�AL�yAL�AI�FAG;dAF��AFr�AE��AChsAC�AB�yAB�+AB�AA��AA?}A@n�A@1A>��A=33A<ȴA;G�A8VA7t�A5�wA4�uA4�A3�A3��A3oA2JA1+A0(�A/dZA.�uA-O�A,��A+�-A*��A)��A)&�A&n�A%/A$��A$r�A$A"�/A"bA!�;A!��A!�PA!dZA �A M�A��AȴA5?A�
A;dAQ�A�uAx�A�`A��AbNA��A�!AhsAp�A�\AbA�A�\A1'A�`A1'A�
A��A�#A
�HA
jA
$�A	"�AI�AA=qA��A\)A"�A�A%A�TA^5A�A�AdZAO�AoA @�@�5?@���@��@�S�@�E�@��@���@��@�P@�@��@�(�@웦@�ff@���@��@�D@��@�E�@�S�@�h@��@�t�@��y@�p�@�V@��`@���@���@�&�@�dZ@�~�@�7L@�J@�7L@̛�@�ƨ@��@Ȭ@�|�@���@�v�@�v�@�ȴ@Ə\@�G�@���@���@���@��^@�z�@���@��@���@��^@�O�@�(�@�@��@��y@��y@��y@��H@�ȴ@���@�V@�bN@��m@�ff@�`B@�(�@��@�5?@�V@�r�@��@���@�J@��7@��j@�1'@���@�33@���@�n�@��@��@���@���@�r�@��m@���@�$�@���@��7@�x�@�V@�Q�@��@�t�@��R@��-@��@�X@�O�@�/@��@�r�@��;@��@�+@�o@��@�ff@��@���@�1'@��m@��@�C�@���@�n�@�{@��@��@��@��@���@�x�@�&�@���@���@�Q�@��;@�dZ@���@�^5@��T@��^@��7@�X@�/@���@�j@�  @�ƨ@���@�;d@�"�@�o@���@�~�@��@���@��^@���@�G�@��@��@��@���@��@��@�ƨ@�+@���@��!@���@�v�@�^5@�V@�E�@�-@���@���@�7L@���@��@�Z@��@��@�\)@�K�@�+@��@�~�@�V@�{@�@��T@��^@�hs@�/@���@�z�@�Z@��m@���@�"�@���@��@��y@���@�v�@�v�@�ff@�^5@�-@��@���@��^@�G�@���@��9@�I�@�1@��@+@~��@~E�@}O�@|�@|�D@{�m@{@z^5@z=q@z�@y��@y�7@yX@x�`@xQ�@x  @w��@w
=@v�y@v�@vȴ@v��@v@u/@t�@t�/@t�D@t�@s��@sƨ@s��@s�@st�@sS�@sS�@sS�@s33@r�\@rM�@q��@qX@p�`@p�9@p�u@pbN@o�@o�@ol�@n��@nv�@m��@m/@l��@l9X@k��@k��@k"�@j�@j�H@j��@j�\@j^5@i��@iG�@h��@hr�@hb@h  @g;d@f�+@fV@f5?@e�T@e�-@e`B@d�/@d��@d1@c�m@c�m@c�
@cƨ@c��@c�@cS�@b��@b~�@b=q@b�@b�@a��@ax�@`��@`�u@`Q�@`1'@_�@_\)@_;d@^��@^ȴ@^��@^��@^��@^�+@^V@]�@]@]�h@]�h@]p�@]O�@]?}@]/@]V@\��@\9X@\�@[��@[ƨ@[��@[dZ@[o@Z�H@Z�!@Z~�@Z=q@Y��@Y��@YG�@X��@X��@XQ�@W�@W��@W|�@WK�@V�R@VE�@U@U`B@UV@T��@T�j@T�@T��@T�D@Tz�@S��@S"�@R�H@R��@R��@RM�@Q�^@QX@QX@QG�@P�`@P��@Pr�@P �@O�;@Ol�@O;d@N��@Nȴ@N��@NE�@M�T@MV@L�j@Lj@Kƨ@K33@J��@J�@I�^@Ihs@IX@I�@H��@H��@HQ�@Hb@G�@G�;@G��@G��@G\)@G
=@F�y@Fȴ@FE�@F@E�h@E/@D�@Dj@D1@Cƨ@C�@Ct�@Ct�@C"�@Co@B��@B=q@B�@B�@A�@A7L@@��@@�u@@A�@?�@?�w@?��@?|�@?\)@?;d@>��@>@=p�@=/@=/@<��@<�@<j@<Z@<Z@<I�@<9X@<(�@<1@;�
@;S�@;o@;@:�@:�H@:��@:-@9��@9��@9��@9��@9x�@9&�@8A�@8b@7�@7�;@7��@7�@6ȴ@6�R@6�R@6�R@6��@6��@6v�@65?@5�@5�-@5�-@5�h@5�h@5p�@5/@4�@4��@49X@3�@3"�@3@2�H@2n�@2M�@2�@2J@1��@1X@1�@0��@0�@0b@/K�@/
=@/
=@.��@.�y@.�@.�R@.v�@.@-��@-��@-`B@-�@,�/@,��@,z�@,1@+��@+��@+��@+t�@+"�@+@*�H@*�H@*��@*~�@*^5@*^5@*^5@*^5@*M�@*J@)�^@)��@)x�@)hs@)G�@)�@(�`@(Ĝ@(�@(Q�@(1'@'��@'K�@'+@'�@&��@&�y@&�@&ȴ@&ȴ@&��@&v�@&ff@&5?@&{@%��@%��@%V@$�@$�D@$I�@$9X@$1@#��@#S�@#@"~�@"^5@"=q@"-@"�@"J@!��@!�@!�#@!��@!�^@!��@!G�@!&�@ ��@ Ĝ@ Q�@   @�@l�@+@v�@V@E�@$�@@��@�h@p�@O�@/@V@�@��@�@z�@(�@�@�@��@�
@��@��@S�@C�@o@�@�!@^5@M�@=q@-@X@%@�`@��@��@�u@�@r�@Q�@ �@  @�@�P@l�@;d@+@�y@�R@�+@5?@�T@�@/@�/@�@�D@j@I�@9X@(�@�@1@��@��@�m@ƨ@��@t�@33@@=q@�#@�7@%@r�@Q�@ �@��@�@;d@��@�y@��@�y@�y@�y@�@�@ȴ@ȴ@��@V@�T@��@/@�@�j@�D@Z@�
@�F@��@�@S�@@
�H@
�H@
��@
�\@
n�@
=q@	��@	��@	��@	��@	hs@	7L@	&�@��@�9@�u@�@Q�@1'@  @�;@�w@�@|�@+@
=@
=@��@�y@�@��@v�@ff@v�@ff@ff@V@$�@{@��@�-@��@O�@��@��@��@�D@z�@z�@z�@j@Z@Z@Z@I�@9X@(�@1@ƨ@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��DA��\A�bNA���A��
A���A���A��FA��A���A���A��PA�~�A�p�A�XA�?}A�+A��#A�9XA�
=A���A��DA�ffA�"�A�1A���A��A��TA��#A��A�+A�=qA���A��A��A���A��#A�JA�;dA��A�K�A���A��-A��A��;A���A��jA��-A���A���A���A��uA��DA�v�A�ZA�I�A�A�A� �A���A��A�A��A�&�A�z�A��HA�-A�p�A�+A�t�A��`A��A��A��A��A��A�&�A���A��A���A��wA�ZA���A��!A�ĜA�?}A�=qA�-A�C�A�A��A���A�x�A��HA��wA��RA�;dA��A��\A��A��A�{A��jA�Q�A���A��wA��TA�{A��+A���A�{A�bA}�PAyp�Av�DAu�mAtĜAs/Aq%AoƨAl  AgƨAe�hAdVAb�DAa�A_S�A^~�A]ƨA]�A\��A[��AYVAU�AS�
ARVAQ"�AP�AN�AN-AM�
AM�AL�yAL�AI�FAG;dAF��AFr�AE��AChsAC�AB�yAB�+AB�AA��AA?}A@n�A@1A>��A=33A<ȴA;G�A8VA7t�A5�wA4�uA4�A3�A3��A3oA2JA1+A0(�A/dZA.�uA-O�A,��A+�-A*��A)��A)&�A&n�A%/A$��A$r�A$A"�/A"bA!�;A!��A!�PA!dZA �A M�A��AȴA5?A�
A;dAQ�A�uAx�A�`A��AbNA��A�!AhsAp�A�\AbA�A�\A1'A�`A1'A�
A��A�#A
�HA
jA
$�A	"�AI�AA=qA��A\)A"�A�A%A�TA^5A�A�AdZAO�AoA @�@�5?@���@��@�S�@�E�@��@���@��@�P@�@��@�(�@웦@�ff@���@��@�D@��@�E�@�S�@�h@��@�t�@��y@�p�@�V@��`@���@���@�&�@�dZ@�~�@�7L@�J@�7L@̛�@�ƨ@��@Ȭ@�|�@���@�v�@�v�@�ȴ@Ə\@�G�@���@���@���@��^@�z�@���@��@���@��^@�O�@�(�@�@��@��y@��y@��y@��H@�ȴ@���@�V@�bN@��m@�ff@�`B@�(�@��@�5?@�V@�r�@��@���@�J@��7@��j@�1'@���@�33@���@�n�@��@��@���@���@�r�@��m@���@�$�@���@��7@�x�@�V@�Q�@��@�t�@��R@��-@��@�X@�O�@�/@��@�r�@��;@��@�+@�o@��@�ff@��@���@�1'@��m@��@�C�@���@�n�@�{@��@��@��@��@���@�x�@�&�@���@���@�Q�@��;@�dZ@���@�^5@��T@��^@��7@�X@�/@���@�j@�  @�ƨ@���@�;d@�"�@�o@���@�~�@��@���@��^@���@�G�@��@��@��@���@��@��@�ƨ@�+@���@��!@���@�v�@�^5@�V@�E�@�-@���@���@�7L@���@��@�Z@��@��@�\)@�K�@�+@��@�~�@�V@�{@�@��T@��^@�hs@�/@���@�z�@�Z@��m@���@�"�@���@��@��y@���@�v�@�v�@�ff@�^5@�-@��@���@��^@�G�@���@��9@�I�@�1@��@+@~��@~E�@}O�@|�@|�D@{�m@{@z^5@z=q@z�@y��@y�7@yX@x�`@xQ�@x  @w��@w
=@v�y@v�@vȴ@v��@v@u/@t�@t�/@t�D@t�@s��@sƨ@s��@s�@st�@sS�@sS�@sS�@s33@r�\@rM�@q��@qX@p�`@p�9@p�u@pbN@o�@o�@ol�@n��@nv�@m��@m/@l��@l9X@k��@k��@k"�@j�@j�H@j��@j�\@j^5@i��@iG�@h��@hr�@hb@h  @g;d@f�+@fV@f5?@e�T@e�-@e`B@d�/@d��@d1@c�m@c�m@c�
@cƨ@c��@c�@cS�@b��@b~�@b=q@b�@b�@a��@ax�@`��@`�u@`Q�@`1'@_�@_\)@_;d@^��@^ȴ@^��@^��@^��@^�+@^V@]�@]@]�h@]�h@]p�@]O�@]?}@]/@]V@\��@\9X@\�@[��@[ƨ@[��@[dZ@[o@Z�H@Z�!@Z~�@Z=q@Y��@Y��@YG�@X��@X��@XQ�@W�@W��@W|�@WK�@V�R@VE�@U@U`B@UV@T��@T�j@T�@T��@T�D@Tz�@S��@S"�@R�H@R��@R��@RM�@Q�^@QX@QX@QG�@P�`@P��@Pr�@P �@O�;@Ol�@O;d@N��@Nȴ@N��@NE�@M�T@MV@L�j@Lj@Kƨ@K33@J��@J�@I�^@Ihs@IX@I�@H��@H��@HQ�@Hb@G�@G�;@G��@G��@G\)@G
=@F�y@Fȴ@FE�@F@E�h@E/@D�@Dj@D1@Cƨ@C�@Ct�@Ct�@C"�@Co@B��@B=q@B�@B�@A�@A7L@@��@@�u@@A�@?�@?�w@?��@?|�@?\)@?;d@>��@>@=p�@=/@=/@<��@<�@<j@<Z@<Z@<I�@<9X@<(�@<1@;�
@;S�@;o@;@:�@:�H@:��@:-@9��@9��@9��@9��@9x�@9&�@8A�@8b@7�@7�;@7��@7�@6ȴ@6�R@6�R@6�R@6��@6��@6v�@65?@5�@5�-@5�-@5�h@5�h@5p�@5/@4�@4��@49X@3�@3"�@3@2�H@2n�@2M�@2�@2J@1��@1X@1�@0��@0�@0b@/K�@/
=@/
=@.��@.�y@.�@.�R@.v�@.@-��@-��@-`B@-�@,�/@,��@,z�@,1@+��@+��@+��@+t�@+"�@+@*�H@*�H@*��@*~�@*^5@*^5@*^5@*^5@*M�@*J@)�^@)��@)x�@)hs@)G�@)�@(�`@(Ĝ@(�@(Q�@(1'@'��@'K�@'+@'�@&��@&�y@&�@&ȴ@&ȴ@&��@&v�@&ff@&5?@&{@%��@%��@%V@$�@$�D@$I�@$9X@$1@#��@#S�@#@"~�@"^5@"=q@"-@"�@"J@!��@!�@!�#@!��@!�^@!��@!G�@!&�@ ��@ Ĝ@ Q�@   @�@l�@+@v�@V@E�@$�@@��@�h@p�@O�@/@V@�@��@�@z�@(�@�@�@��@�
@��@��@S�@C�@o@�@�!@^5@M�@=q@-@X@%@�`@��@��@�u@�@r�@Q�@ �@  @�@�P@l�@;d@+@�y@�R@�+@5?@�T@�@/@�/@�@�D@j@I�@9X@(�@�@1@��@��@�m@ƨ@��@t�@33@@=q@�#@�7@%@r�@Q�@ �@��@�@;d@��@�y@��@�y@�y@�y@�@�@ȴ@ȴ@��@V@�T@��@/@�@�j@�D@Z@�
@�F@��@�@S�@@
�H@
�H@
��@
�\@
n�@
=q@	��@	��@	��@	��@	hs@	7L@	&�@��@�9@�u@�@Q�@1'@  @�;@�w@�@|�@+@
=@
=@��@�y@�@��@v�@ff@v�@ff@ff@V@$�@{@��@�-@��@O�@��@��@��@�D@z�@z�@z�@j@Z@Z@Z@I�@9X@(�@1@ƨ@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B~�B}�B}�B|�B|�B|�B|�B{�B{�B{�B{�B{�B{�By�By�Bz�Bx�By�By�Bx�By�By�By�Bx�Bx�Bt�Bw�Bu�Bq�Br�Bv�By�Bz�Bw�Bm�BcTB_;BQ�BO�BM�BL�BK�BK�BJ�BI�BJ�BJ�BJ�BJ�BI�BH�BF�BF�BE�BA�B8RB,B$�B�BB��B�B�HB��B��B�}B�B��B�B{�Bu�Bp�BhsBe`B]/B]/B[#BT�BN�BI�B;dB2-B"�B\B��B�yB�/B��B�RB�B��B�DB�Bs�B\)BP�BH�BA�B9XB1'B!�BVB��B�B�yB�)B��BB�B��B� B{�By�Bs�BdZBW
B@�B(�B�B{BVB+BB  B  BBB��B��B�B�sB�ZB�BB�)B��B��BǮBĜB�}B�LB�-B��B��B��B��B��B��B�{B�uB�oB�bB�\B�=B�+B�B|�By�Bt�Bl�BgmBbNB^5B\)B[#BYBXBS�BO�BL�BI�BF�BC�BA�B>wB<jB8RB6FB0!B+B)�B(�B&�B%�B"�B"�B"�B"�B!�B�B�B�B�B�B�B�B{BbB\BPBJBDB
=B%BBB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�sB
�sB
�mB
�fB
�mB
�ZB
�TB
�TB
�NB
�NB
�HB
�NB
�BB
�BB
�BB
�;B
�5B
�/B
�/B
�)B
�)B
�)B
�#B
�#B
�B
�)B
�#B
�#B
�#B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�/B
�)B
�)B
�/B
�B
�B
�
B
�B
�B
�
B
�
B
�B
�B
�#B
�#B
�)B
�)B
�/B
�;B
�ZB
�`B
�fB
�sB
�B
�B
�B
�B
�B
�B
�B
�B
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
��BBB1BDBJB\BbBuB�B�B�B�B�B�B!�B"�B$�B%�B&�B'�B,B/B1'B6FB8RB9XB9XB9XB<jB?}BA�BD�BJ�BO�BP�BQ�BR�BR�BS�BW
BYB[#B]/B]/B^5B`BBe`BhsBo�Bq�Br�Bt�Bw�Bz�B|�B}�B~�B~�B~�B~�B�B�B�%B�+B�7B�PB�bB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�9B�XB�XB�^B�dB�wB�}B�}B�}B�}BĜBȴBɺB��B��B��B��B��B��B�B�B�
B�B�)B�;B�HB�ZB�fB�sB�B�B�B�B�B��B��B��B��B��B��B��B  BB%B+B
=BJBhBoBoBoB�B�B�B�B�B�B�B�B�B �B"�B$�B(�B+B,B/B1'B2-B6FB7LB9XB;dB?}BB�BB�BC�BD�BE�BE�BG�BJ�BK�BM�BQ�BR�BS�BS�BT�BXB\)B^5B^5B_;BaHBaHBbNBbNBdZBdZBe`Be`Be`BffBjBk�Bm�Bo�Bq�Br�Bs�Bt�Bu�Bv�Bw�By�B|�B� B�B�B�%B�%B�1B�=B�DB�JB�JB�PB�VB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�!B�'B�9B�?B�?B�FB�RB�XB�XB�^B�dB�jB�jB�jB�jB�qB�wB�}B�}B�}B��B��B��B��B��BBĜBĜBĜBŢBƨBƨBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�)B�#B�/B�/B�/B�/B�/B�;B�NB�NB�NB�TB�NB�ZB�`B�ZB�ZB�`B�`B�fB�mB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBB%B+B+B1B	7B	7B	7B
=BDBJBJBPBPBVBPBVBVBbBhBoBuBoBoBuB{B{B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B!�B!�B!�B"�B"�B#�B#�B$�B%�B%�B&�B&�B'�B'�B(�B'�B)�B+B+B+B+B,B.B.B.B/B/B/B/B/B0!B1'B1'B1'B2-B33B2-B33B49B5?B49B49B49B5?B6FB6FB6FB6FB6FB7LB7LB7LB7LB7LB8RB8RB8RB8RB9XB9XB9XB:^B:^B:^B;dB;dB<jB<jB<jB=qB=qB=qB=qB>wB=qB>wB>wB>wB>wB?}B?}B?}B@�B@�BA�BB�BB�BB�BC�BC�BD�BD�BE�BE�BE�BE�BE�BF�BE�BF�BF�BF�BF�BG�BG�BG�BG�BH�BI�BI�BI�BJ�BK�BK�BK�BK�BK�BL�BL�BL�BL�BM�BM�BM�BM�BN�BM�BN�BO�BO�BO�BO�BP�BO�BP�BP�BP�BP�BQ�BQ�BR�BQ�BR�BS�BS�BT�BT�BT�BT�BVBVBVBVBVBW
BW
BW
BW
BW
BXBXBYBYBZBZBZB[#B[#B[#B\)B\)B\)B\)B]/B\)B]/B]/B]/B]/B]/B]/B^5B^5B_;B`BB`BBaHBbNBbNBbNBcTBcTBdZBdZBdZBdZBe`BdZBe`BdZBe`Be`Be`Be`Be`BffBffBgmBgmBhsBhsBhsBiyBiyBjBjBiyBjBk�Bk�BjBk�Bl�Bl�Bl�Bm�Bl�Bm�Bm�Bn�Bn�Bn�Bn�Bo�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bq�Bq�Br�Bq�Bq�Br�Br�Bs�Br�Br�Bs�Br�Br�Bs�Bs�Bs�Bt�Bt�Bt�Bu�Bu�Bu�Bu�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bw�Bw�Bw�Bx�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B�B�B�B~�B}�B}�B|�B|�B|�B|�B{�B{�B{�B{�B{�B{�By�By�Bz�Bx�By�By�Bx�By�By�By�Bx�Bx�Bt�Bw�Bu�Bq�Br�Bv�By�Bz�Bw�Bm�BcTB_;BQ�BO�BM�BL�BK�BK�BJ�BI�BJ�BJ�BJ�BJ�BI�BH�BF�BF�BE�BA�B8RB,B$�B�BB��B�B�HB��B��B�}B�B��B�B{�Bu�Bp�BhsBe`B]/B]/B[#BT�BN�BI�B;dB2-B"�B\B��B�yB�/B��B�RB�B��B�DB�Bs�B\)BP�BH�BA�B9XB1'B!�BVB��B�B�yB�)B��BB�B��B� B{�By�Bs�BdZBW
B@�B(�B�B{BVB+BB  B  BBB��B��B�B�sB�ZB�BB�)B��B��BǮBĜB�}B�LB�-B��B��B��B��B��B��B�{B�uB�oB�bB�\B�=B�+B�B|�By�Bt�Bl�BgmBbNB^5B\)B[#BYBXBS�BO�BL�BI�BF�BC�BA�B>wB<jB8RB6FB0!B+B)�B(�B&�B%�B"�B"�B"�B"�B!�B�B�B�B�B�B�B�B{BbB\BPBJBDB
=B%BBB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�sB
�sB
�mB
�fB
�mB
�ZB
�TB
�TB
�NB
�NB
�HB
�NB
�BB
�BB
�BB
�;B
�5B
�/B
�/B
�)B
�)B
�)B
�#B
�#B
�B
�)B
�#B
�#B
�#B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�/B
�)B
�)B
�/B
�B
�B
�
B
�B
�B
�
B
�
B
�B
�B
�#B
�#B
�)B
�)B
�/B
�;B
�ZB
�`B
�fB
�sB
�B
�B
�B
�B
�B
�B
�B
�B
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
��BBB1BDBJB\BbBuB�B�B�B�B�B�B!�B"�B$�B%�B&�B'�B,B/B1'B6FB8RB9XB9XB9XB<jB?}BA�BD�BJ�BO�BP�BQ�BR�BR�BS�BW
BYB[#B]/B]/B^5B`BBe`BhsBo�Bq�Br�Bt�Bw�Bz�B|�B}�B~�B~�B~�B~�B�B�B�%B�+B�7B�PB�bB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�9B�XB�XB�^B�dB�wB�}B�}B�}B�}BĜBȴBɺB��B��B��B��B��B��B�B�B�
B�B�)B�;B�HB�ZB�fB�sB�B�B�B�B�B��B��B��B��B��B��B��B  BB%B+B
=BJBhBoBoBoB�B�B�B�B�B�B�B�B�B �B"�B$�B(�B+B,B/B1'B2-B6FB7LB9XB;dB?}BB�BB�BC�BD�BE�BE�BG�BJ�BK�BM�BQ�BR�BS�BS�BT�BXB\)B^5B^5B_;BaHBaHBbNBbNBdZBdZBe`Be`Be`BffBjBk�Bm�Bo�Bq�Br�Bs�Bt�Bu�Bv�Bw�By�B|�B� B�B�B�%B�%B�1B�=B�DB�JB�JB�PB�VB�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�!B�'B�9B�?B�?B�FB�RB�XB�XB�^B�dB�jB�jB�jB�jB�qB�wB�}B�}B�}B��B��B��B��B��BBĜBĜBĜBŢBƨBƨBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�)B�#B�/B�/B�/B�/B�/B�;B�NB�NB�NB�TB�NB�ZB�`B�ZB�ZB�`B�`B�fB�mB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBB%B+B+B1B	7B	7B	7B
=BDBJBJBPBPBVBPBVBVBbBhBoBuBoBoBuB{B{B�B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B!�B!�B!�B!�B"�B"�B#�B#�B$�B%�B%�B&�B&�B'�B'�B(�B'�B)�B+B+B+B+B,B.B.B.B/B/B/B/B/B0!B1'B1'B1'B2-B33B2-B33B49B5?B49B49B49B5?B6FB6FB6FB6FB6FB7LB7LB7LB7LB7LB8RB8RB8RB8RB9XB9XB9XB:^B:^B:^B;dB;dB<jB<jB<jB=qB=qB=qB=qB>wB=qB>wB>wB>wB>wB?}B?}B?}B@�B@�BA�BB�BB�BB�BC�BC�BD�BD�BE�BE�BE�BE�BE�BF�BE�BF�BF�BF�BF�BG�BG�BG�BG�BH�BI�BI�BI�BJ�BK�BK�BK�BK�BK�BL�BL�BL�BL�BM�BM�BM�BM�BN�BM�BN�BO�BO�BO�BO�BP�BO�BP�BP�BP�BP�BQ�BQ�BR�BQ�BR�BS�BS�BT�BT�BT�BT�BVBVBVBVBVBW
BW
BW
BW
BW
BXBXBYBYBZBZBZB[#B[#B[#B\)B\)B\)B\)B]/B\)B]/B]/B]/B]/B]/B]/B^5B^5B_;B`BB`BBaHBbNBbNBbNBcTBcTBdZBdZBdZBdZBe`BdZBe`BdZBe`Be`Be`Be`Be`BffBffBgmBgmBhsBhsBhsBiyBiyBjBjBiyBjBk�Bk�BjBk�Bl�Bl�Bl�Bm�Bl�Bm�Bm�Bn�Bn�Bn�Bn�Bo�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bq�Bq�Br�Bq�Bq�Br�Br�Bs�Br�Br�Bs�Br�Br�Bs�Bs�Bs�Bt�Bt�Bt�Bu�Bu�Bu�Bu�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bw�Bw�Bw�Bx�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.20 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220414090105                              AO  ARCAADJP                                                                    20220414090105    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220414090105  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220414090105  QCF$                G�O�G�O�G�O�8000            